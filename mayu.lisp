(in-package :mayu)

(defvar *log-pathname* #p"/tmp/cl-mayu.log")

(defvar *log-stream* nil)
;;(setf *log-stream* *standard-output*)

(defconstant +posix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun input-event-to-string (event)
  (cffi:with-foreign-slots ((time type code value) event (:struct input-event))
    (cffi:with-foreign-slots ((tv-sec tv-usec) time (:struct timeval))
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time (+ tv-sec +posix-epoch+))
        (if (= type +ev-key+)
            (format nil "~04,'0d/~02,'0d/~02,'0d ~02,'0d:~02,'0d:~02,'0d.~06,'0d ~a ~[release~;press~;repeat~]"
                    year month day hour min sec tv-usec
                    (key-code-to-symbol code)
                    value)
            (format nil "~04,'0d/~02,'0d/~02,'0d ~02,'0d:~02,'0d:~02,'0d.~06,'0d ~a ~a ~a"
                    year month day hour min sec tv-usec
                    (event-type-to-symbol type)
                    code
                    value))))))

(defgeneric write-log (format &rest args))

#+nil
(defmethod write-log :before (format &rest args)
  (declare (ignore args))
  (unless *log-stream*
    (setf *log-stream* (open *log-pathname*
                             :direction :output
                             :external-format :utf-8
                             :if-does-not-exist :create
                             :if-exists :append))))

#+nil
(defmethod write-log :after (format &rest args)
  (declare (ignore format args))
  (terpri *log-stream*)
  (force-output *log-stream*))

#+nil
(defmethod write-log ((format string) &rest args)
  (apply #'format *log-stream* format args))

(defmethod write-log ((format string) &rest args)
  (when *log-stream*
    (apply #'format *log-stream* format args)))

(defmethod write-log (format &rest args)
  (apply #'write-log (princ-to-string format) args))

(defun close-log-stream ()
  (close *log-stream*)
  (setf *log-stream* nil))



(defconstant +evdev-minors+ 32)
(defvar *envdev-key-fds* ())
(defvar *uinput-fd* nil)

(defun ioc (dir type nr size)
  (logior (ash dir +ioc_dirshift+)
          (ash type +ioc_typeshift+)
          (ash nr +ioc_nrshift+)
          (ash size +ioc_sizeshift+)))

(defun eviocgbit (ev len)
  (ioc +ioc-read+ #.(char-code #\E) (+ #x20 ev) len))

(defun keyboard-device-p (fd)
  (cffi:with-foreign-objects ((devinfo '(:struct input-id)))
    (sb-posix:ioctl fd
                    +eviocgid+
                    (sb-alien:sap-alien devinfo (* t)))
    (cffi:with-foreign-slots ((bustype) devinfo (:struct input-id))
      ;; allow USB, PS/2, ADB
      (unless (member bustype (list +bus-usb+ +bus-i8042+ +bus-adb+) :test #'=)
        (return-from keyboard-device-p nil))))
  (let ((size (1+ (truncate (/ +ev-max+ 8)))))
    (cffi:with-foreign-objects ((evtype-bitmask :uint8 size))
      (sb-posix:ioctl fd
                      (eviocgbit 0 (* 8 size))
                      (sb-alien:sap-alien evtype-bitmask (* t)))
      ;; EV_SYN, EV_KEY, EV_REP ならおｋ
      (let ((value (cffi:mem-ref evtype-bitmask :uint32)))
        (and (logbitp +ev-syn+ value)
             (logbitp +ev-key+ value)
             (logbitp +ev-rep+ value))))))


(defun open-key-device (dev-number)
  (handler-case
      (let* ((dev-path (format nil "/dev/input/event~d" dev-number))
             (fd (sb-posix:open dev-path
                                (logior sb-posix:o-rdonly sb-posix:o-ndelay))))
        (cffi:with-foreign-objects ((version :int))
          (sb-posix:ioctl fd
                          +eviocgversion+
                          (sb-alien:sap-alien version (* t)))
          (if (= (cffi:mem-ref version :int) +ev-version+)
              fd
              (progn
                (sb-posix:close fd)
                nil))))
    (sb-posix:syscall-error () nil)))

(defun open-keyboard ()
  (setf *envdev-key-fds* nil)
  (loop for i from 0 below +evdev-minors+
        for fd = (open-key-device i)
        if (and fd (keyboard-device-p fd))
          do (push fd *envdev-key-fds*))
  (length *envdev-key-fds*))

(defun close-keyboard ()
  (loop for fd in *envdev-key-fds*
        do (ignore-errors (sb-posix:close fd)))
  (setf *envdev-key-fds* nil))
;; (open-keyboard)
;; *envdev-key-fds*
;; (close-keyboard)


(defun destroy-uinput-keyboard ()
  "キーコード出力用のキーボードを破棄"
  (when *uinput-fd*
    (ignore-errors (sb-posix:ioctl *uinput-fd* +ui-dev-destroy+))
    (ignore-errors (sb-posix:close *uinput-fd*))
    (setf *uinput-fd* nil)))

(defun create-uinput-keyboard ()
  "キーコード出力用のキーボードを作成"
  (destroy-uinput-keyboard)
  (setf *uinput-fd* (or (ignore-errors (sb-posix:open "/dev/input/uinput" sb-posix:o-rdwr))
                        (sb-posix:open "/dev/uinput" sb-posix:o-rdwr)))
  (cffi:with-foreign-objects ((uinput-user-dev '(:struct uinput-user-dev)))
    (cffi:with-foreign-slots ((name id) uinput-user-dev (:struct uinput-user-dev))
      (loop for i from 0
            for c across (format nil "mayu uinpt~c" #\Nul)
            do (setf (cffi:mem-aref name :char i) (char-code c)))
      (cffi:with-foreign-slots ((vendor bustype product version) id (:struct input-id))
        (setf vendor 1)
        (setf bustype +bus-i8042+)
        (setf product 1)
        (setf version 4))
      ;; uinput deviceを作成
      (sb-posix:ioctl *uinput-fd* +ui-set-evbit+ +ev-key+)
      (sb-posix:ioctl *uinput-fd* +ui-set-evbit+ +ev-syn+)
      (sb-posix:ioctl *uinput-fd* +ui-set-evbit+ +ev-rep+)
      (sb-posix:ioctl *uinput-fd* +ui-set-evbit+ +ev-rel+)
      (sb-posix:ioctl *uinput-fd* +ui-set-relbit+ +rel-x+)
      (sb-posix:ioctl *uinput-fd* +ui-set-relbit+ +rel-y+)
      (loop for i from 0 below KEY_MAX
            do (sb-posix:ioctl *uinput-fd* +ui-set-keybit+ i))
      (sb-posix:ioctl *uinput-fd* +ui-set-keybit+ +btn-mouse+)
      (sb-posix:ioctl *uinput-fd* +ui-set-keybit+ +btn-left+)
      (sb-posix:ioctl *uinput-fd* +ui-set-keybit+ +btn-middle+)
      (sb-posix:ioctl *uinput-fd* +ui-set-keybit+ +btn-right+)
      (sb-posix:ioctl *uinput-fd* +ui-set-keybit+ +btn-forward+)
      (sb-posix:ioctl *uinput-fd* +ui-set-keybit+ +btn-back+)

      (sb-unix:unix-write *uinput-fd*
                          uinput-user-dev
                          0
                          (cffi:foreign-type-size '(:struct uinput_user_dev)))
      (sb-posix:ioctl *uinput-fd* +ui-dev-create+)
      *uinput-fd*)))
;; (create-uinput-keyboard)
;; (destroy-uinput-keyboard)


(defun open-keyboard-device ()
  (open-keyboard)
  (create-uinput-keyboard))

(defun close-keyboard-device ()
  (keyboard-grab-onoff nil)
  (destroy-uinput-keyboard)
  (close-keyboard))

(defmacro when-open (&body body)
  `(when (and *envdev-key-fds* *uinput-fd*)
     ,@body))

(defun write-input-event (fd input-event)
  (sb-unix:unix-write fd input-event 0
                      (cffi:foreign-type-size '(:struct input_event))))

(defun send-input-event (_type _code _value)
  (when *uinput-fd*
    (cffi:with-foreign-objects ((event '(:struct input-event)))
      (cffi:with-foreign-slots ((time type code value) event (:struct input-event))
        (cffi:with-foreign-slots ((tv-sec tv-usec) time (:struct timeval))
          (setf (values tv-sec tv-usec) (sb-ext:get-time-of-day)))
        (setf type _type
              code _code
              value _value)
        (when (= _type +ev-key+)
          (write-log "SND ~a" (input-event-to-string event)))
        (let ((event-size (cffi:foreign-type-size '(:struct input_event))))
          (let ((write-size (sb-unix:unix-write *uinput-fd*     ; TODO error
                                                event
                                                0
                                                event-size)))
            (when (/= event-size write-size)
              (write-log "send-input-event failed ~d" (sb-alien:get-errno)))))))))


(defvar *mayu-enabled-p* t)

(defun receive-keyboard-event (fd input-event)
  (sb-posix:read fd input-event (cffi:foreign-type-size '(:struct input-event)))
  (cffi:with-foreign-slots ((type code value) input-event (:struct input-event))
    (if *mayu-enabled-p*
        (cond ((= type +ev-key+)
               (write-log "rev ~a" (string-downcase (input-event-to-string input-event)))
               t)
              ((or (= type +ev-syn+) ; 無視
                   (= type +ev-msc+))
               nil)
              (t
               ;; キーボードイベント以外は、そのまま出力
               (write-input-event *uinput-fd* input-event)
               nil))
        (progn
          (write-log "raw ~a" (input-event-to-string input-event))
          (when (and (= type +ev-key+) (= code KEY_F11)
                     (= value +press+))
            (setf *mayu-enabled-p* t))
          (write-input-event *uinput-fd* input-event)
          nil))))

(defun keyboard-grab-onoff (onoff)
  (loop for fd in *envdev-key-fds*
        do (handler-case
               (sb-posix:ioctl fd +eviocgrab+ (if onoff -1 0))
             (sb-posix:syscall-error () nil))))




(defvar *key-stat* (make-array KEY_CNT :initial-element +release+))

(defun sym-to-mod (sym)
  (case sym
    (+shift+ *shift*)
    (+ctrl+  *ctrl*)
    (+alt+   *alt*)
    (+meta+  *meta*)
    (+super+ *super*)
    (+hyper+ *hyper*)))

(defun mod-sym-to-key (mod-sym)
  (car (sym-to-mod mod-sym)))

(defun mod-key-to-sym (mod-key-code)
  (cond ((member mod-key-code *shift*)
         +shift+)
        ((member mod-key-code *ctrl*)
         +ctrl+)
        ((member mod-key-code *alt*)
         +alt+)
        ((member mod-key-code *meta*)
         +meta+)
        ((member mod-key-code *super*)
         +super+)
        ((member mod-key-code *hyper*)
         +hyper+)))

(defun ensure-mod-sym (sym-or-code)
  (typecase sym-or-code
    (symbol sym-or-code)
    (t (mod-key-to-sym sym-or-code))))

(defgeneric send-keyboard-event (code action))

(defmethod send-keyboard-event (code action)
  (when *uinput-fd*
    (send-input-event +ev-key+ code action) ; TODO error
    (send-input-event +ev-syn+ +syn-report+ 0)
    t))

(defmethod send-keyboard-event :after ((code integer) action)
  (setf (aref *key-stat* code) action))

(defmethod send-keyboard-event ((code symbol) action)
  (send-keyboard-event (mod-sym-to-key code) action))

(macrolet ((m (sym keys)
             `(defmethod send-keyboard-event ((code (eql ,sym)) (action (eql +release+)))
                (loop for key in ,keys
                      if (on-p key)
                        do (send-keyboard-event key +release+)))))
  (m +shift+    *shift*)
  (m +ctrl+     *ctrl*)
  (m +alt+      *alt*)
  (m +meta+     *meta*)
  (m +super+    *super*)
  (m +meta+     *meta*))



(defun press-p (code)
  (= (aref *key-stat* code) +press+))

(defun release-p (code)
  (= (aref *key-stat* code) +release+))

(defun repeat-p (code)
  (= (aref *key-stat* code) +repeat+))

(defun on-p (code)
  (not (release-p code)))

(defun shift-press-p ()
  (some #'on-p *shift*))

(defun ctrl-press-p ()
  (some #'on-p *ctrl*))

(defun alt-press-p ()
  (some #'on-p *alt*))

(defun meta-press-p ()
  (some #'on-p *meta*))

(defun super-press-p ()
  (some #'on-p *super*))

(defun hyper-press-p ()
  (some #'on-p *hyper*))



(defun event-type-to-symbol (event-type)
  (case event-type
    (#.+ev-syn+ '+ev-syn+)
    (#.+ev-key+ '+ev-key+)
    (#.+ev-rel+ '+ev-rel+)
    (#.+ev-abs+ '+ev-abs+)
    (#.+ev-msc+ '+ev-msc+)
    (#.+ev-sw+  '+ev-sw+)
    (#.+ev-led+ '+ev-led+)
    (#.+ev-snd+ '+ev-snd+)
    (#.+ev-rep+ '+ev-rep+)
    (#.+ev-ff+  '+ev-ff+)
    (#.+ev-pwr+ '+ev-pwr+)
    (#.+ev-ff-status+ '+ev-qff-status+)
    (t event-type)))

(defvar *key-array*
  (let ((array (make-array KEY_CNT)))
    (loop for i from 0 below KEY_CNT
          do (setf (aref array i) i))
    array))

(defvar *one-shot* nil)
(defvar *one-shot-status* nil)

(defvar *sequence-table* nil)
(defvar *sequence-temp-mod* nil)
(defvar *sequence-temp-mod-for-current-mod* nil)

(defgeneric release-mod (sym-or-code)
  (:method ((sym symbol))
    (release-mod (mod-sym-to-key sym)))
  (:method ((code fixnum))
    (let ((seq (list (mod-key-to-sym code) +release+)))
      (setf *sequence-temp-mod*
            (remove seq *sequence-temp-mod* :test #'equal))
      seq)))

(defun current-mod ()
  (let ((mod `(,@(if (alt-press-p)     '(+alt+))
               ,@(if (ctrl-press-p)  '(+ctrl+))
               ,@(if (meta-press-p)  '(+meta+))
               ,@(if (shift-press-p) '(+shift+))
               ,@(if (super-press-p) '(+super+))
               ,@(if (hyper-press-p) '(+hyper+)))))
    (loop for (c v) in *sequence-temp-mod-for-current-mod*
          for mod-sym = (ensure-mod-sym c)
          do (if (= v +press+)
                 (setf mod (delete mod-sym mod))
                 (pushnew mod-sym mod)))
    mod))

(defgeneric mod-press-p (mod)
  (:method ((mod fixnum))
    (mod-press-p (mod-key-to-sym mod)))
  (:method ((mod symbol))
    (find mod (current-mod))))

(defun find-no-mod (x)
  (find x *modifiers* :key #'cadr))

(defun proc-one-shot (code action)
  (let ((mod (cdr (assoc code *one-shot*))))
    (if mod
        (cond ((and (null *one-shot-status*) (= action +press+))
               (if (mod-press-p mod)
                   nil ; 即に mod キーが有効になっているので、通常動作をする。
                   (progn
                     (setf *one-shot-status* :only)
                     `((,mod ,+press+)))))
              ((and (eq *one-shot-status* :only) (= action +release+)) ; 他に何も押さなかった場合
               (setf *one-shot-status* nil)
               `(,(release-mod mod)
                 (,code ,+press+)
                 (,code ,+release+)))
              ((= action +release+)
               (if *one-shot-status*
                   (progn
                     (setf *one-shot-status* nil)
                     `(,(release-mod mod)))
                   nil)) ; 即に mod キーが有効になっているので、通常動作をする。
              (t (list ())))
        (progn
          (when (and (eq *one-shot-status* :only) (= action +press+))
            (setf *one-shot-status* :other))
          nil))))

(defun compute-sequence-temp-mod (sequence-key sequence-value current-mod)
  (write-log "compute-sequence-temp-mod ~a ~a ~a" sequence-key sequence-value current-mod)
  (flet ((release-mod-seq (mod)
           (loop for key in (sym-to-mod mod)
                 if (on-p key)
                   collect (list key +release+))))
    (let (any)
      (append
       (loop for i in sequence-value
             if (eq i +any+)
               do (setf any t)
             else if (and (symbolp i)
                          (not (member i current-mod)))
                    collect (list i +press+))
       (if any
           ;; ((+any+ +shift+ KEY_COMMA) (+any+ KEY_COMMA)) の場合に shift をリリースするパターン
           (loop for i in sequence-key
                 if (and (symbolp i)
                         (not (eq i +any+))
                         (not (find-no-mod i))
                         (not (member i sequence-value)))
                   append (release-mod-seq i))
           ;; sequence-value にないものは全てリリースする。
           (loop for i in current-mod
                 unless (member i sequence-value)
                   append (release-mod-seq i)))))))

(defun reverse-action-sequnce (sequence)
  (loop for (c v) in sequence
        collect (list c (cond ((= v +press+)
                               +release+)
                              ((= v +release+)
                               +press+)
                              (t v)))))

(defgeneric compute-send-sequence (sequence-key sequence-value current-mod code action)

  (:method (sequence-key sequence-value current-mod code (action (eql +press+)))
    (write-log "compute-send-sequence ~@{~a ~}" sequence-key sequence-value current-mod code action)
    (setf *sequence-temp-mod* (compute-sequence-temp-mod sequence-key sequence-value current-mod))
    (setf *sequence-temp-mod-for-current-mod* *sequence-temp-mod*)
    (let ((key (find-if #'numberp sequence-value)))
      (append *sequence-temp-mod*
              (list (list key +press+))
              (reverse-action-sequnce *sequence-temp-mod*))))

  (:method (sequence-key sequence-value current-mod code (action (eql +repeat+)))
    (let ((key (find-if #'numberp sequence-value)))
      (append *sequence-temp-mod*
              (list (list key +release+)
                    (list key +press+))
              (reverse-action-sequnce *sequence-temp-mod*))))

  (:method (sequence-key sequence-value current-mod code (action (eql +release+)))
    (let ((key (find-if #'numberp sequence-value)))
      (write-log "compute-sequence-temp-mod release ~a ~a" *sequence-temp-mod* *sequence-temp-mod-for-current-mod*)
      (setf *sequence-temp-mod* nil)
      (setf *sequence-temp-mod-for-current-mod* nil)
      (list (list key +release+)))))

(defun sequence-match-p (a b)
  "a is input. b is key in *sequence-table*."
  (let (any)
    (loop for i in b
          for no-mod = (find-no-mod i)
          do (cond (no-mod
                    (when (member (car no-mod) a)
                      (return-from sequence-match-p nil)))
                   ((eq i +any+)
                    (setf any t))
                   ((not (member i a))
                    (return-from sequence-match-p nil))))
    (when (not any)
      (loop for i in a
            unless (member i b)
              do (return-from sequence-match-p nil))))
  t)


(defun proc-sequence (code action)
  (let* ((current-mod (current-mod))
         (current-sequence (append current-mod (list code))))
    (write-log "proce-sequence ~a ~a" current-mod current-sequence)
    (loop for (key . val) in *sequence-table*
          if (sequence-match-p current-sequence key)
            do (return-from proc-sequence
                 (compute-send-sequence key val current-mod code action)))
    nil))


(defun translate-key (code action)
  (let ((code (aref *key-array* code)))
    (or (proc-one-shot code action)
        (proc-sequence code action)
        (list (list code action)))))

(defun proc-key (code action)
  (when (and (= code KEY_F12) (= action +press+))
    (setf *mayu-enabled-p* nil))
  (when (and (= code KEY_F10) (= action +press+)) ; デバッグのためにログにマークを出力する。
    (write-log "-----------------------------------------------------------------------------"))
  (let ((ret (translate-key code action)))
    (loop for (code action) in ret
          if code
            do (send-keyboard-event code action))
    ret))

(defun epoll-ctl (fd epfd op &rest events)
  (cffi:with-foreign-object (ev '(:struct epoll-event))
    (bzero ev (cffi:foreign-type-size '(:struct epoll-event)))
    (setf (cffi:foreign-slot-value ev '(:struct epoll-event) 'events)
          (apply #'logior events))
    (setf (cffi:foreign-slot-value
           (cffi:foreign-slot-value ev '(:struct epoll-event) 'data)
           '(:union epoll-data) 'fd)
          fd)
    (%epoll-ctl epfd op fd ev)))

(defun main-loop ()
  (loop
    (handler-case (progn
                    (open-keyboard-device)
                    (keyboard-grab-onoff t)
                    (unwind-protect
                         (%main-loop)
                      (close-keyboard-device)))
      (error (e)
        (warn "error!!!! ~a" e)))))
;; (sb-thread:make-thread 'main-loop)

(defun %main-loop ()
  (let ((epfd (epoll-create 1))
        (fd-count (length *envdev-key-fds*)))
    (loop for fd in *envdev-key-fds*
          do (epoll-ctl fd epfd epoll-ctl-add epollin))
    (cffi:with-foreign-objects ((input-event '(:struct input-event))
                                (events '(:struct epoll-event) fd-count))
      (bzero events (* (cffi:foreign-type-size '(:struct epoll-event)) fd-count))
      (loop for ready-fds = (epoll-wait epfd events fd-count 1000)
            do (loop for i below ready-fds
                     for event = (cffi:mem-aptr events '(:struct epoll-event) i)
                     for event-fd = (cffi:foreign-slot-value
                                     (cffi:foreign-slot-value event '(:struct epoll-event) 'data)
                                     '(:union epoll-data) 'fd)
                     if (receive-keyboard-event event-fd input-event)
                       do (cffi:with-foreign-slots ((type code value) input-event (:struct input-event))
                            (proc-key code value)))))))

;;; config.lisp 用のマクロ

(defmacro set-key-subst (&rest args)
  `(progn
     ,@(loop for (a b) in args
             collect `(setf (aref *key-array* ,a) ,b))))

(defmacro set-one-shot (&rest args)
  `(setf *one-shot*  (list ,@(loop for (a b) in args
                                   collect `(cons ,a ,b)))))

(defmacro set-sequence (&rest args)
  `(setf *sequence-table* (list ,@(loop for (a b) in args
                                        collect `(cons (list ,@a) (list ,@b))))))
