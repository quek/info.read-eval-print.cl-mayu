(in-package :mayu)

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
