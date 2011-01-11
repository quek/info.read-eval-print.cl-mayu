(in-package :mayu)

(defvar *log-pathname* #p"/tmp/cl-mayu.log")

(defvar *log-stream* nil)

(defconstant +posix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun input-event-to-string (event)
  (cffi:with-foreign-slots ((time type code value) event input_event)
    (cffi:with-foreign-slots ((tv_sec tv_usec) time timeval)
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time (+ tv_sec +posix-epoch+))
        (if (= type EV_KEY)
            (format nil "~04,'0d/~02,'0d/~02,'0d ~02,'0d:~02,'0d:~02,'0d.~06,'0d ~a ~[release~;press~;repeat~]"
                year month day hour min sec tv_usec
                (key-code-to-symbol code)
                value)
            (format nil "~04,'0d/~02,'0d/~02,'0d ~02,'0d:~02,'0d:~02,'0d.~06,'0d ~a ~a ~a"
                year month day hour min sec tv_usec
                (event-type-to-symbol type)
                code
                value))))))

(defgeneric write-log (format &rest args))

(defmethod write-log :before (format &rest args)
  (declare (ignore args))
  (unless *log-stream*
    (setf *log-stream* (open *log-pathname*
                       :direction :output
                       :external-format :utf-8
                       :if-does-not-exist :create
                       :if-exists :append))))

(defmethod write-log :after (format &rest args)
  (declare (ignore format args))
  (terpri *log-stream*)
  (force-output *log-stream*))

(defmethod write-log ((format string) &rest args)
  (apply #'format *log-stream* format args))

(defmethod write-log (format &rest args)
  (apply #'write-log (princ-to-string format) args))

(defun close-log-stream ()
  (close *log-stream*)
  (setf *log-stream* nil))



(defconstant +evdev-minors+ 32)
(defvar *envdev-key-fds* ())
(defvar *uinput-fd* nil)


(defun keyboard-device-p (fd)
  (cffi:with-foreign-objects ((devinfo 'input_id))
    (sb-posix:ioctl fd
		    EVIOCGID
		    (sb-alien:sap-alien devinfo (* t)))
    (cffi:with-foreign-slots ((bustype) devinfo input_id)
      ;; allow USB, PS/2, ADB
      (unless (member bustype (list BUS_USB BUS_I8042 BUS_ADB) :test #'=)
	(return-from keyboard-device-p nil))))
  (let ((size (1+ (truncate (/ EV_MAX 8)))))
    (cffi:with-foreign-objects ((evtype-bitmask :uint8 size))
      (sb-posix:ioctl fd
		      (EVIOCGBIT 0 (* 8 size))
		      (sb-alien:sap-alien evtype-bitmask (* t)))
      ;; EV_SYN, EV_KEY, EV_REP ならおｋ
      (let ((value (cffi:mem-ref evtype-bitmask :uint32)))
	(and (logbitp EV_SYN value)
	     (logbitp EV_KEY value)
	     (logbitp EV_REP value))))))


(defun open-key-device (dev-number)
  (handler-case
      (let* ((dev-path (format nil "/dev/input/event~d" dev-number))
	     (fd (sb-posix:open dev-path
				(logior sb-posix:o-rdonly sb-posix:o-ndelay))))
	(cffi:with-foreign-objects ((version :int))
	  (sb-posix:ioctl fd
			  EVIOCGVERSION
			  (sb-alien:sap-alien version (* t)))
	  (if (= (cffi:mem-ref version :int) EV_VERSION)
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
	do (sb-posix:close fd))
  (setf *envdev-key-fds* nil))
;; (open-keyboard)
;; *envdev-key-fds*
;; (close-keyboard)


(defun destroy-uinput-keyboard ()
  "キーコード出力用のキーボードを破棄"
  (when *uinput-fd*
    (sb-posix:ioctl *uinput-fd* UI_DEV_DESTROY)
    (setf *uinput-fd* nil)))

(defun create-uinput-keyboard ()
  "キーコード出力用のキーボードを作成"
  (destroy-uinput-keyboard)
  (setf *uinput-fd* (sb-posix:open "/dev/input/uinput" sb-posix:o-rdwr))
  (cffi:with-foreign-objects ((uinput-user-dev 'uinput_user_dev))
    (cffi:with-foreign-slots ((name id ff_effects_max absmax absmin absfuzz absflat)
			      uinput-user-dev uinput_user_dev)
      (loop for i from 0
	    for c across (format nil "mayu uinpt~c" #\Nul)
	    do (setf (cffi:mem-aref name :char i) (char-code c)))
      (cffi:with-foreign-slots ((vendor bustype product version) id input_id)
	(setf vendor 1)
	(setf bustype BUS_I8042)
	(setf product 1)
	(setf version 4))
      ;; uinput deviceを作成
      (sb-posix:ioctl *uinput-fd* UI_SET_EVBIT EV_KEY)
      (sb-posix:ioctl *uinput-fd* UI_SET_EVBIT EV_SYN)
      (sb-posix:ioctl *uinput-fd* UI_SET_EVBIT EV_REP)
      (sb-posix:ioctl *uinput-fd* UI_SET_EVBIT EV_REL)
      (sb-posix:ioctl *uinput-fd* UI_SET_RELBIT REL_X)
      (sb-posix:ioctl *uinput-fd* UI_SET_RELBIT REL_Y)
      (loop for i from 0 below KEY_MAX
	    do (sb-posix:ioctl *uinput-fd* UI_SET_KEYBIT i))
      (sb-posix:ioctl *uinput-fd* UI_SET_KEYBIT BTN_MOUSE)
      (sb-posix:ioctl *uinput-fd* UI_SET_KEYBIT BTN_LEFT)
      (sb-posix:ioctl *uinput-fd* UI_SET_KEYBIT BTN_MIDDLE)
      (sb-posix:ioctl *uinput-fd* UI_SET_KEYBIT BTN_RIGHT)
      (sb-posix:ioctl *uinput-fd* UI_SET_KEYBIT BTN_FORWARD)
      (sb-posix:ioctl *uinput-fd* UI_SET_KEYBIT BTN_BACK)

      (sb-unix:unix-write *uinput-fd*
			  uinput-user-dev
			  0
			  (cffi:foreign-type-size 'uinput_user_dev))
      (sb-posix:ioctl *uinput-fd* UI_DEV_CREATE)
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
                      (cffi:foreign-type-size 'input_event)))

(defun send-input-event (_type _code _value)
  (when *uinput-fd*
    (cffi:with-foreign-objects ((event 'input_event))
      (cffi:with-foreign-slots ((time type code value) event input_event)
	(cffi:with-foreign-slots ((tv_sec tv_usec) time timeval)
	  (setf (values tv_sec tv_usec) (sb-ext:get-time-of-day)))
	(setf type _type
	      code _code
	      value _value)
        (when (= _type EV_KEY)
          (write-log "SND ~a" (input-event-to-string event)))
	(let ((event-size (cffi:foreign-type-size 'input_event)))
	  (let ((write-size (sb-unix:unix-write *uinput-fd*	; TODO error
						event
						0
						event-size)))
	    (when (/= event-size write-size)
	      (write-log "send-input-event failed ~d" (sb-alien:get-errno)))))))))


(defvar *mayu-enabled-p* t)

(defun receive-keyboard-event (event)
  (when-open
    (sb-alien:with-alien ((fd-set (sb-alien:struct sb-unix:fd-set)))
      (loop
	(sb-unix:fd-zero fd-set)
	(loop for fd in *envdev-key-fds*
	      do (sb-unix:fd-set fd fd-set))
	(sb-unix:unix-fast-select (1+ (loop for fd in *envdev-key-fds* maximize fd))
				  (sb-alien:addr fd-set) nil nil nil nil)
	(loop for i from 0
	      and fd in *envdev-key-fds*
	      if (sb-unix:fd-isset fd fd-set)
		do (sb-unix:unix-read fd event (cffi:foreign-type-size 'input_event))
                   (cffi:with-foreign-slots ((type code value) event input_event)
                     (if *mayu-enabled-p*
                         (cond ((= type EV_KEY)
                                (write-log "rev ~a" (string-downcase (input-event-to-string event)))
                                (return-from receive-keyboard-event t))
                               ((or (= type EV_SYN)	; 無視
                                    (= type EV_MSC)))
                               (t
                                ;; キーボードイベント以外は、そのまま出力
                                (sb-unix:unix-write
                                 *uinput-fd* event 0
                                 (cffi:foreign-type-size 'input_event))))
                         (progn
                           (write-log "raw ~a" (input-event-to-string event))
                           (when (and (= type EV_KEY) (= code KEY_F11)
                                      (= value +press+))
                             (setf *mayu-enabled-p* t))
                           (write-input-event *uinput-fd* event)))))))))

(defun keyboard-grab-onoff (onoff)
  (loop for fd in *envdev-key-fds*
	do (handler-case
	       (sb-posix:ioctl fd EVIOCGRAB (if onoff -1 0))
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
    (send-input-event EV_KEY code action) ; TODO error
    (send-input-event EV_SYN SYN_REPORT 0)
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
    (#.EV_SYN 'EV_SYN)
    (#.EV_KEY 'EV_KEY)
    (#.EV_REL 'EV_REL)
    (#.EV_ABS 'EV_ABS)
    (#.EV_MSC 'EV_MSC)
    (#.EV_SW  'EV_SW)
    (#.EV_LED 'EV_LED)
    (#.EV_SND 'EV_SND)
    (#.EV_REP 'EV_REP)
    (#.EV_FF  'EV_FF)
    (#.EV_PWR 'EV_PWR)
    (#.EV_FF_STATUS 'EV_FF_STATUS)
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
  (when (and (= code KEY_F10) (= action +press+)) ; デバッグのためにログにマークを出力する。
    (write-log "-----------------------------------------------------------------------------"))
  (let ((ret (translate-key code action)))
    (loop for (code action) in ret
          if code
            do (send-keyboard-event code action))
    ret))

(defun main-loop ()
  (open-keyboard-device)
  (unwind-protect
       (progn
	 (sleep 0.5)
	 (keyboard-grab-onoff t)
	 (cffi:with-foreign-object (event 'input_event)
	   (loop
	     (receive-keyboard-event event)
	     (cffi:with-foreign-slots ((type code value) event input_event)
	       (proc-key code value)))))
    (close-keyboard-device)))
;; (main-loop)


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
