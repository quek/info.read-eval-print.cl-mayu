(in-package :mayu)

(defmacro p (format &rest args)
  `(progn
     (format *error-output* "~&")
     (format *error-output* ,format ,@args)
     (force-output *error-output*)))


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
	(p "open ~a~%" dev-path)
	(cffi:with-foreign-objects ((version :int))
	  (sb-posix:ioctl fd
			  EVIOCGVERSION
			  (sb-alien:sap-alien version (* t)))
	  (if (= (cffi:mem-ref version :int) EV_VERSION)
	      fd
	      (progn
		(sb-posix:close fd)
		nil))))
    (sb-posix:syscall-error (e) (print e) nil)))

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

(defun send-input-event (_type _code _value)
  (when *uinput-fd*
    (cffi:with-foreign-objects ((event 'input_event))
      (cffi:with-foreign-slots ((time type code value) event input_event)
	(cffi:with-foreign-slots ((tv_sec tv_usec) time timeval)
	  (setf (values tv_sec tv_usec) (sb-ext:get-time-of-day)))
	(setf type _type
	      code _code
	      value _value)
	(let ((event-size (cffi:foreign-type-size 'input_event)))
	  (let ((write-size (sb-unix:unix-write *uinput-fd*	; TODO error
						event
						0
						event-size)))
	    (when (/= event-size write-size)
	      (p "send-input-event failed ~d" (sb-alien:get-errno)))))))))

(defun mod-sym-to-key (mod-sym)
  (case mod-sym
    (+alt+ KEY_LEFTALT)
    (+ctrl+ KEY_LEFTCTRL)
    (+meta+ KEY_LEFTMETA)
    (+shift+ KEY_LEFTSHIFT)))

(defgeneric send-keyboard-event (code action))

(defmethod send-keyboard-event (code action)
  (when *uinput-fd*
    (send-input-event EV_KEY code action) ; TODO error
    (send-input-event EV_SYN SYN_REPORT 0)
    (p "send-keyboard-event ~a ~a" code action)
    t))

(defmethod send-keyboard-event ((code symbol) action)
  (send-keyboard-event (mod-sym-to-key code) action))

(macrolet ((m (key ver)
             `(progn
                (defvar ,ver nil)
                (defmethod send-keyboard-event :after ((code (eql ,key)) (action (eql +press+)))
                  (setf ,ver t))
                (defmethod send-keyboard-event :after ((code (eql ,key)) (action (eql +release+)))
                  (setf ,ver nil)))))
  (m KEY_LEFTSHIFT      *left-shift*)
  (m KEY_RIGHTSHIFT     *right-shift*)
  (m KEY_LEFTCTRL       *left-ctrl*)
  (m KEY_RIGHTCTRL      *right-ctrl*)
  (m KEY_LEFTALT        *left-alt*)
  (m KEY_RIGHTALT       *right-alt*)
  (m KEY_LEFTMETA       *left-meta*)
  (m KEY_RIGHTMETA      *right-meta*))

(defun shift-press-p ()
  (or *left-shift* *right-shift*))

(defun ctrl-press-p ()
  (or *left-ctrl* *right-ctrl*))

(defun alt-press-p ()
  (or *left-alt* *right-alt*))

(defun meta-press-p ()
  (or *left-meta* *right-meta*))


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
		   (cffi:with-foreign-slots ((type) event input_event)
		     (cond ((= type EV_KEY)
			    (print-event event)
			    (return-from receive-keyboard-event t))
			   ((or (= type EV_SYN)	; 無視
				(= type EV_MSC)))
			   (t
			    ;; キーボードイベント以外は、そのまま出力
			    (sb-unix:unix-write
			     *uinput-fd* event 0
			     (cffi:foreign-type-size 'input_event))))))))))

(defun keyboard-grab-onoff (onoff)
  (loop for fd in *envdev-key-fds*
	do (handler-case
	       (sb-posix:ioctl fd EVIOCGRAB (if onoff -1 0))
	     (sb-posix:syscall-error (e) (print e) nil))))

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

(defun print-event (event)
  (cffi:with-foreign-slots ((type code value) event input_event)
    (format t "~&        type: ~a, code: ~d, value: ~d~%"
	    (event-type-to-symbol type)
	    code value)))


(defvar *key-array*
  (let ((array (make-array KEY_CNT :element-type 'fixnum)))
    (loop for i from 0 below KEY_CNT
	  do (setf (aref array i) i))
    array))

(defvar *one-shot* nil)
(defvar *one-shot-status* nil)

(defun proc-one-shot (code action)
  (let ((mod (cdr (assoc code *one-shot*))))
    (if mod
	(cond ((and (not *one-shot-status*) (= action +press+))
	       (setf *one-shot-status* :only)
	       `((,mod ,+press+)))
	      ((and (eq *one-shot-status* :only) (= action +release+)) ; 他に何も押さなかった場合
	       (setf *one-shot-status* nil)
	       `((,mod ,+release+)
		 (,code ,+press+)
		 (,code ,+release+)))
	      ((= action +release+)
	       (setf *one-shot-status* nil)
	       `((,mod ,+release+)))
	      (t (list ())))
	(progn
	  (when (eq *one-shot-status* :only)
	    (setf *one-shot-status* :other))
	  nil))))

(defvar *sequence-table* nil)
(defvar *sequence-temp-mod* nil)


(defun current-mod ()
  (let ((mod `(,@(if (alt-press-p)     '(+alt+))
                 ,@(if (ctrl-press-p)  '(+ctrl+))
                 ,@(if (meta-press-p)  '(+meta+))
                 ,@(if (shift-press-p) '(+shift+)))))
    (loop for (c v) in *sequence-temp-mod*
          do (if (= v +press+)
                 (setf mod (delete c mod))
                 (pushnew c mod)))
    mod))

(defun find-no-mod (x)
  (find x *modifiers* :key #'cadr))

(defun compute-sequence-temp-mod (sequence-key sequence-value current-mod)
  (let (any)
    (append
     (loop for i in sequence-value
           if (eq i +any+)
             do (setf any t)
           else if (and (symbolp i)
                        (not (eq i +any+))
                        (not (member i current-mod)))
                  collect (list i +press+))
     (if any
         (loop for i in sequence-key
               if (and (symbolp i)
                       (not (eq i +any+))
                       (not (find-no-mod i))
                       (not (member i sequence-value)))
                 collect (list i +release+))
         (loop for i in current-mod
               unless (member i sequence-value)
                 collect (list i +release+))))))

(defgeneric compute-send-sequence (sequence-key sequence-value current-mod code action)
  (:method (sequence-key sequence-value current-mod code (action (eql +repeat+)))
    (loop for i in sequence-value
          if (numberp i)
            do (list (list i +repeat+))))
  (:method (sequence-key sequence-value current-mod code (action (eql +press+)))
    (setf *sequence-temp-mod* (compute-sequence-temp-mod sequence-key sequence-value current-mod))
    (let ((key (find-if #'numberp sequence-value)))
      (if key
          (append *sequence-temp-mod*
                  (list (list key +press+)))
          (list ()))))
  (:method (sequence-key sequence-value current-mod code (action (eql +release+)))
    (let ((mod (loop for (c v) in *sequence-temp-mod*
                     collect (list  c (if (= v +press+) +release+ +press+))))
          (key (find-if #'numberp sequence-value)))
      (setf *sequence-temp-mod* nil)
      (if key
          (append (list (list key +release+)) mod)
          (list ())))))

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
;; (sequence-match-p (list KEY_COMMA) (list +any+ +no-shift+ KEY_COMMA))
;; (sequence-match-p (list +shift+ KEY_0) (list +any+ +shift+ KEY_0))
;; (sequence-match-p (list +shift+ KEY_DOT) (list +any+ +no-shift+ KEY_DOT))


(defun proc-sequence (code action)
  (let* ((current-mod (current-mod))
         (current-sequence (append current-mod (list code))))
    (loop for (key . val) in *sequence-table*
          if (sequence-match-p current-sequence key)
            do (return-from proc-sequence
                 (compute-send-sequence key val current-mod code action)))
    nil))
#|
(let ((*sequence-temp-mod* nil))
  (compute-send-sequence (list +any+ +shift+ KEY_DOT) (list +any+ KEY_DOT) (list +shift+) KEY_DOT +press+))
(let ((*sequence-temp-mod* nil))
  (compute-send-sequence (list +any+ +shift+ KEY_DOT) (list +any+ KEY_DOT) (list +shift+ +alt+) KEY_DOT +press+))
(let ((*sequence-temp-mod* nil))
  (compute-send-sequence (list +any+ +shift+ KEY_0) (list +any+ +shift+ KEY_DOT) (list +shift+) KEY_0 +press+))
|#


;;(defun equal-unorder (a b)
;;  (equal (sort a #'< :key #'car)
;;         (sort b #'< :key #'car)))
;;(assert (equal-unorder `((,KEY_LEFTSHIFT 1) (,KEY_LEFTALT 1) (,KEY_LEFTCTRL 0) (,KEY_B 1))
;;                       (let ((*sequence-temp-mod* nil)
;;                             (*left-ctrl* t)
;;                             (*sequence-table* `(((+ctrl+ ,KEY_A) . (+shift+ +alt+ ,KEY_B)))))
;;                         (proc-sequence KEY_A +press+))))


(defun translate-key (code action)
  (let ((code (aref *key-array* code)))
    (or (proc-one-shot code action)
        (proc-sequence code action)
	(list (list code action)))))

(defun proc-key (code action)
  (if (= code KEY_F12)
      nil
      (progn
	(loop for (code action) in (translate-key code action)
	      if code
		do (send-keyboard-event code action))
	t)))

(defun main-loop ()
  (open-keyboard-device)
  (unwind-protect
       (progn
	 (sleep 1)
	 (keyboard-grab-onoff t)
	 (cffi:with-foreign-object (event 'input_event)
	   (loop
	     (receive-keyboard-event event)
	     (cffi:with-foreign-slots ((type code value) event input_event)
	       (unless (proc-key code value)
		 (return))))))
    (close-keyboard-device)))
;; (main-loop)


(defmacro set-key-array (&rest args)
  `(progn
     ,@(loop for (a b) in args
             collect `(setf (aref *key-array* ,a) ,b))))

(defmacro def-one-shot (&rest args)
  `(setf *one-shot*  (list ,@(loop for (a b) in args
                                   collect `(cons ,a ,b)))))

(defmacro def-sequence (&rest args)
  `(setf *sequence-table* (list ,@(loop for (a b) in args
                                        collect `(cons (list ,@a) (list ,@b))))))

#|
(progn
  (sleep 0.5)
  (open-keyboard-device)
  (unwind-protect
       (progn
	 (print *envdev-key-fds*)
	 (print *uinput-fd*)
	 (keyboard-grab-onoff t)
	 (cffi:with-foreign-object (event 'input_event)
	   (loop repeat 50 do
	     (receive-keyboard-event event)
	     (cffi:with-foreign-slots ((type code action) event input_event)
	       (send-keyboard-event (1+ code) action)))))
    (close-keyboard-device)))

(unwind-protect
     (progn
       (create-uinput-keyboard)
       (sleep 0.1)			; sleep が必要みたい
       (send-keyboard-event KEY_CNT 0)
       (send-keyboard-event KEY_A 1)
       (send-keyboard-event KEY_A 0)
       (send-keyboard-event KEY_B 1)
       (send-keyboard-event KEY_B 0)
       (sleep 0.1)) 			; sleep が必要みたい
  (destroy-uinput-keyboard))

(let ((fd (sb-posix:open "/dev/input/event1" (logior sb-posix:o-rdonly sb-posix:o-ndelay))))
  (unwind-protect
       (progn
	 (cffi:with-foreign-objects ((devinfo 'input_id))
	   (sb-posix:ioctl fd EVIOCGID (sb-alien:sap-alien devinfo (* t)))
	   (cffi:with-foreign-slots ((bustype) devinfo input_id)
	     (print bustype)
	     ))
	 )
    (sb-posix:close fd)))


(let ((fd (sb-posix:open "/dev/input/event1" (logior sb-posix:o-rdonly sb-posix:o-ndelay))))
  (unwind-protect
       (progn
	 (let ((size (1+ (truncate (/ EV_MAX 8)))))
	   (cffi:with-foreign-objects ((evtype-bitmask :uint8 size))
	     (sb-posix:ioctl fd
			     (EVIOCGBIT 0 (* 8 size))
			     (sb-alien:sap-alien evtype-bitmask (* t)))
	     ;; EV_SYN, EV_KEY, EV_REP ならおｋ
	     (let ((value (cffi:mem-ref evtype-bitmask :uint32)))
	       (and (logbitp EV_SYN value)
		    (logbitp EV_KEY value)
		    (logbitp EV_REP value)))))
	 )
    (sb-posix:close fd)))

(let ((fd (sb-posix:open "/dev/input/event1" (logior sb-posix:o-rdonly sb-posix:o-ndelay))))
  (unwind-protect
       (progn
	 (keyboard-device-p fd)
	 )
    (sb-posix:close fd)))

(let ((fd (open-key-device 1)))
  (when fd
    (sb-posix:close fd)))



(cffi:with-foreign-objects ((devinfo 'input_id))
  (print (sb-alien:sap-alien devinfo (* t)))
  (print devinfo))
|#