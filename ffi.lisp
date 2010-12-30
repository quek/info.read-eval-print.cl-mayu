;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 2.0.1
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.

(in-package :mayu)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun to-signed (n size)
    (if (logbitp (1- size) n)
	(- n (ash 1 size))
	n))

;;;SWIG wrapper code starts here

  (cl:defmacro defanonenum (&body enums)
    "Converts anonymous enums to defconstants."
    `(cl:progn ,@(cl:loop for value in enums
		    for index = 0 then (cl:1+ index)
		    when (cl:listp value) do (cl:setf index (cl:second value)
						value (cl:first value))
		      collect `(cl:defconstant ,value ,index))))

  (cl:eval-when (:compile-toplevel :load-toplevel)
    (cl:unless (cl:fboundp 'swig-lispify)
      (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
	(cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
		      (cl:cond
			((cl:null lst)
			 rest)
			((cl:upper-case-p c)
			 (helper (cl:cdr lst) 'upper
				 (cl:case last
				   ((lower digit) (cl:list* c #\- rest))
				   (cl:t (cl:cons c rest)))))
			((cl:lower-case-p c)
			 (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
			((cl:digit-char-p c)
			 (helper (cl:cdr lst) 'digit 
				 (cl:case last
				   ((upper lower) (cl:list* c #\- rest))
				   (cl:t (cl:cons c rest)))))
			((cl:char-equal c #\_)
			 (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
			(cl:t
			 (cl:error "Invalid character: ~A" c)))))
	  (cl:let ((fix (cl:case flag
			  ((constant enumvalue) "+")
			  (variable "*")
			  (cl:t ""))))
	    (cl:intern
	     (cl:concatenate
	      'cl:string
	      fix
	      (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
	      fix)
	     package))))))

;;;SWIG wrapper code ends here


;;;; /usr/include/asm-generic/ioctl.h
  ;; #define _IOC_NRBITS	8
  (defconstant _IOC_NRBITS	8)
  ;; #define _IOC_TYPEBITS	8
  (defconstant _IOC_TYPEBITS	8)
  ;; 
  ;; /*
  ;;  * Let any architecture override either of the following before
  ;;  * including this file.
  ;;  */
  ;; 
  ;; #ifndef _IOC_SIZEBITS
  ;; # define _IOC_SIZEBITS	14
  ;; #endif
  (defconstant _IOC_SIZEBITS	14)
  ;; 
  ;; #ifndef _IOC_DIRBITS
  ;; # define _IOC_DIRBITS	2
  ;; #endif
  (defconstant _IOC_DIRBITS	2)
  ;; 
  ;; #define _IOC_NRMASK	((1 << _IOC_NRBITS)-1)
  (defconstant _IOC_NRMASK	(1- (ash 1 _IOC_NRBITS)))
  ;; #define _IOC_TYPEMASK	((1 << _IOC_TYPEBITS)-1)
  (defconstant _IOC_TYPEMASK	(1- (ash 1 _IOC_TYPEBITS)))
  ;; #define _IOC_SIZEMASK	((1 << _IOC_SIZEBITS)-1)
  (defconstant _IOC_SIZEMASK	(1- (ash 1 _IOC_SIZEBITS)))
  ;; #define _IOC_DIRMASK	((1 << _IOC_DIRBITS)-1)
  (defconstant _IOC_DIRMASK	(1- (ash 1 _IOC_DIRBITS)))
  ;; 
  ;; #define _IOC_NRSHIFT	0
  (defconstant _IOC_NRSHIFT	0)
  ;; #define _IOC_TYPESHIFT	(_IOC_NRSHIFT+_IOC_NRBITS)
  (defconstant _IOC_TYPESHIFT	(+ _IOC_NRSHIFT _IOC_NRBITS))
  ;; #define _IOC_SIZESHIFT	(_IOC_TYPESHIFT+_IOC_TYPEBITS)
  (defconstant _IOC_SIZESHIFT	(+ _IOC_TYPESHIFT _IOC_TYPEBITS))
  ;; #define _IOC_DIRSHIFT	(_IOC_SIZESHIFT+_IOC_SIZEBITS)
  (defconstant _IOC_DIRSHIFT	(+ _IOC_SIZESHIFT _IOC_SIZEBITS))
  ;; 
  ;; /*
  ;;  * Direction bits, which any architecture can choose to override
  ;;  * before including this file.
  ;;  */
  ;; 
  ;; #ifndef _IOC_NONE
  ;; # define _IOC_NONE	0U
  ;; #endif
  (defconstant _IOC_NONE	0)
  ;; 
  ;; #ifndef _IOC_WRITE
  ;; # define _IOC_WRITE	1U
  ;; #endif
  (defconstant _IOC_WRITE	1)
  ;; 
  ;; #ifndef _IOC_READ
  ;; # define _IOC_READ	2U
  ;; #endif
  (defconstant _IOC_READ	2)
  ;; 
  ;; #define _IOC(dir,type,nr,size) \
  ;; 	(((dir)  << _IOC_DIRSHIFT) | \
  ;; 	 ((type) << _IOC_TYPESHIFT) | \
  ;; 	 ((nr)   << _IOC_NRSHIFT) | \
  ;; 	 ((size) << _IOC_SIZESHIFT))
  (defun _IOC (dir type nr size)
    (to-signed
     (logior (ash dir _IOC_DIRSHIFT)
	     (ash type _IOC_TYPESHIFT)
	     (ash nr _IOC_NRSHIFT)
	     (ash size _IOC_SIZESHIFT))
     32))
  ;; 
  ;; #define _IOC_TYPECHECK(t) (sizeof(t))
  (defun _IOC_TYPECHECK (x)
    (cffi:foreign-type-size x))
  ;; 
  ;; /* used to create numbers */
  ;; #define _IO(type,nr)		_IOC(_IOC_NONE,(type),(nr),0)
  (defun _IO (type nr)
    (_IOC _IOC_NONE type nr 0))
  ;; #define _IOR(type,nr,size)	_IOC(_IOC_READ,(type),(nr),(_IOC_TYPECHECK(size)))
  (defun _IOR (type nr size)
    (_IOC _IOC_READ type nr (_IOC_TYPECHECK size)))
  ;; #define _IOW(type,nr,size)	_IOC(_IOC_WRITE,(type),(nr),(_IOC_TYPECHECK(size)))
  (defun _IOW (type nr size)
    (_IOC _IOC_WRITE type nr (_IOC_TYPECHECK size)))
  ;; #define _IOWR(type,nr,size)	_IOC(_IOC_READ|_IOC_WRITE,(type),(nr),(_IOC_TYPECHECK(size)))
  (defun _IOWR (type nr size)
    (_IOC (logior _IOC_READ _IOC_WRITE) type nr (_IOC_TYPECHECK size)))
  ;; #define _IOR_BAD(type,nr,size)	_IOC(_IOC_READ,(type),(nr),sizeof(size))
  (defun _IOR_BAD (type nr size)
    (_IOC _IOC_READ type nr (cffi:foreign-type-size size)))
  ;; #define _IOW_BAD(type,nr,size)	_IOC(_IOC_WRITE,(type),(nr),sizeof(size))
  (defun _IOW_BAD (type nr size)
    (_IOC _IOC_WRITE type nr (cffi:foreign-type-size size)))
  ;; #define _IOWR_BAD(type,nr,size)	_IOC(_IOC_READ|_IOC_WRITE,(type),(nr),sizeof(size))
  (defun _IOWR_BAD (type nr size)
    (_IOC (logior _IOC_READ _IOC_WRITE) type nr (cffi:foreign-type-size size)))
  ;; 
  ;; /* used to decode ioctl numbers.. */
  ;; #define _IOC_DIR(nr)		(((nr) >> _IOC_DIRSHIFT) & _IOC_DIRMASK)
  (defun _IOC_DIR (nr)
    (logand (ash nr (- _IOC_DIRSHIFT))))
  ;; #define _IOC_TYPE(nr)		(((nr) >> _IOC_TYPESHIFT) & _IOC_TYPEMASK)
  (defun _IOC_TYPE (nr)
    (logand (ash nr (- _IOC_TYPESHIFT))))
  ;; #define _IOC_NR(nr)		(((nr) >> _IOC_NRSHIFT) & _IOC_NRMASK)
  (defun _IOC_NR (nr)
    (logand (ash nr (- _IOC_NRSHIFT))))
  ;; #define _IOC_SIZE(nr)		(((nr) >> _IOC_SIZESHIFT) & _IOC_SIZEMASK)
  (defun _IOC_SIZE (nr)
    (logand (ash nr (- _IOC_SIZESHIFT))))
  ;; 
  ;; /* ...and for the drivers/sound files... */
  ;; 
  ;; #define IOC_IN		(_IOC_WRITE << _IOC_DIRSHIFT)
  (defconstant IOC_IN (ash _IOC_WRITE _IOC_DIRSHIFT))
  ;; #define IOC_OUT		(_IOC_READ << _IOC_DIRSHIFT)
  (defconstant IOC_OUT (ash _IOC_READ _IOC_DIRSHIFT))
  ;; #define IOC_INOUT	((_IOC_WRITE|_IOC_READ) << _IOC_DIRSHIFT)
  (defconstant IOC_INOUT (ash (logior _IOC_WRITE _IOC_READ) _IOC_DIRSHIFT))
  ;; #define IOCSIZE_MASK	(_IOC_SIZEMASK << _IOC_SIZESHIFT)
  (defconstant IOCSIZE_MASK (ash _IOC_SIZEMASK _IOC_SIZESHIFT))
  ;; #define IOCSIZE_SHIFT	(_IOC_SIZESHIFT)
  (defconstant IOCSIZE_SHIFT _IOC_SIZESHIFT)
  ;; 
  ;; #endif /* _ASM_GENERIC_IOCTL_H */



;;;; /usr/include/asm/posix_types_64.h
  (cffi:defcstruct __kernel_fsid_t
    (val :pointer))

  (cffi:defcstruct timespec
    (tv_sec :long)
    (tv_nsec :long))


;;;; /usr/include/linux/time.h
  (cffi:defcstruct timeval
    (tv_sec :long)
    (tv_usec :long))

  (cffi:defcstruct timezone
    (tz_minuteswest :int)
    (tz_dsttime :int))

  (cl:defconstant ITIMER_REAL 0)

  (cl:defconstant ITIMER_VIRTUAL 1)

  (cl:defconstant ITIMER_PROF 2)

  (cffi:defcstruct itimerspec
    (it_interval timespec)
    (it_value timespec))

  (cffi:defcstruct itimerval
    (it_interval timeval)
    (it_value timeval))

  (cl:defconstant CLOCK_REALTIME 0)

  (cl:defconstant CLOCK_MONOTONIC 1)

  (cl:defconstant CLOCK_PROCESS_CPUTIME_ID 2)

  (cl:defconstant CLOCK_THREAD_CPUTIME_ID 3)

  (cl:defconstant CLOCK_MONOTONIC_RAW 4)

  (cl:defconstant CLOCK_REALTIME_COARSE 5)

  (cl:defconstant CLOCK_MONOTONIC_COARSE 6)

  (cl:defconstant CLOCK_SGI_CYCLE 10)

  (cl:defconstant MAX_CLOCKS 16)

  (cl:defconstant CLOCKS_MASK (cl:logior 0 1))

  (cl:defconstant CLOCKS_MONO 1)

  (cl:defconstant TIMER_ABSTIME #x01)


;;;; /usr/include/linux/input.h
  (cffi:defcstruct input_event
    (time timeval)
    (type :unsigned-short)
    (code :unsigned-short)
    (value :int))

  (cl:defconstant EV_VERSION #x010000)

  (cffi:defcstruct input_id
    (bustype :unsigned-short)
    (vendor :unsigned-short)
    (product :unsigned-short)
    (version :unsigned-short))

  (cffi:defcstruct input_absinfo
    (value :int)
    (minimum :int)
    (maximum :int)
    (fuzz :int)
    (flat :int)
    (resolution :int))

;;; /usr/include/linux/input.h
  ;;#define EVIOCGID		_IOR('E', 0x02, struct input_id)	/* get device ID */
  (defconstant EVIOCGID (_IOR (char-code #\E) #x02 'input_id))
  ;;#define EVIOCGBIT(ev,len)	_IOC(_IOC_READ, 'E', 0x20 + ev, len)	/* get event bits */
  (defun EVIOCGBIT (ev len)
    (_IOC _IOC_READ (char-code #\E) (+ #x20 ev) len))
  ;;#define EVIOCGVERSION		_IOR('E', 0x01, int)			/* get driver version */
  (defconstant EVIOCGVERSION (_IOR (char-code #\E) #x01 :int))
  ;;#define EVIOCGRAB		_IOW('E', 0x90, int)			/* Grab/Release device */
  (defconstant EVIOCGRAB (_IOW (char-code #\E) #x90 :int))

  (cl:defconstant EV_SYN #x00)

  (cl:defconstant EV_KEY #x01)

  (cl:defconstant EV_REL #x02)

  (cl:defconstant EV_ABS #x03)

  (cl:defconstant EV_MSC #x04)

  (cl:defconstant EV_SW #x05)

  (cl:defconstant EV_LED #x11)

  (cl:defconstant EV_SND #x12)

  (cl:defconstant EV_REP #x14)

  (cl:defconstant EV_FF #x15)

  (cl:defconstant EV_PWR #x16)

  (cl:defconstant EV_FF_STATUS #x17)

  (cl:defconstant EV_MAX #x1f)

  (cl:defconstant EV_CNT (cl:+ #x1f 1))

  (cl:defconstant SYN_REPORT 0)

  (cl:defconstant SYN_CONFIG 1)

  (cl:defconstant SYN_MT_REPORT 2)


  (cl:defconstant REL_X #x00)

  (cl:defconstant REL_Y #x01)

  (cl:defconstant REL_Z #x02)

  (cl:defconstant REL_RX #x03)

  (cl:defconstant REL_RY #x04)

  (cl:defconstant REL_RZ #x05)

  (cl:defconstant REL_HWHEEL #x06)

  (cl:defconstant REL_DIAL #x07)

  (cl:defconstant REL_WHEEL #x08)

  (cl:defconstant REL_MISC #x09)

  (cl:defconstant REL_MAX #x0f)

  (cl:defconstant REL_CNT (cl:+ #x0f 1))

  (cl:defconstant ABS_X #x00)

  (cl:defconstant ABS_Y #x01)

  (cl:defconstant ABS_Z #x02)

  (cl:defconstant ABS_RX #x03)

  (cl:defconstant ABS_RY #x04)

  (cl:defconstant ABS_RZ #x05)

  (cl:defconstant ABS_THROTTLE #x06)

  (cl:defconstant ABS_RUDDER #x07)

  (cl:defconstant ABS_WHEEL #x08)

  (cl:defconstant ABS_GAS #x09)

  (cl:defconstant ABS_BRAKE #x0a)

  (cl:defconstant ABS_HAT0X #x10)

  (cl:defconstant ABS_HAT0Y #x11)

  (cl:defconstant ABS_HAT1X #x12)

  (cl:defconstant ABS_HAT1Y #x13)

  (cl:defconstant ABS_HAT2X #x14)

  (cl:defconstant ABS_HAT2Y #x15)

  (cl:defconstant ABS_HAT3X #x16)

  (cl:defconstant ABS_HAT3Y #x17)

  (cl:defconstant ABS_PRESSURE #x18)

  (cl:defconstant ABS_DISTANCE #x19)

  (cl:defconstant ABS_TILT_X #x1a)

  (cl:defconstant ABS_TILT_Y #x1b)

  (cl:defconstant ABS_TOOL_WIDTH #x1c)

  (cl:defconstant ABS_VOLUME #x20)

  (cl:defconstant ABS_MISC #x28)

  (cl:defconstant ABS_MT_TOUCH_MAJOR #x30)

  (cl:defconstant ABS_MT_TOUCH_MINOR #x31)

  (cl:defconstant ABS_MT_WIDTH_MAJOR #x32)

  (cl:defconstant ABS_MT_WIDTH_MINOR #x33)

  (cl:defconstant ABS_MT_ORIENTATION #x34)

  (cl:defconstant ABS_MT_POSITION_X #x35)

  (cl:defconstant ABS_MT_POSITION_Y #x36)

  (cl:defconstant ABS_MT_TOOL_TYPE #x37)

  (cl:defconstant ABS_MT_BLOB_ID #x38)

  (cl:defconstant ABS_MT_TRACKING_ID #x39)

  (cl:defconstant ABS_MAX #x3f)

  (cl:defconstant ABS_CNT (cl:+ #x3f 1))

  (cl:defconstant SW_LID #x00)

  (cl:defconstant SW_TABLET_MODE #x01)

  (cl:defconstant SW_HEADPHONE_INSERT #x02)

  (cl:defconstant SW_RFKILL_ALL #x03)

  (cl:defconstant SW_RADIO #x03)

  (cl:defconstant SW_MICROPHONE_INSERT #x04)

  (cl:defconstant SW_DOCK #x05)

  (cl:defconstant SW_LINEOUT_INSERT #x06)

  (cl:defconstant SW_JACK_PHYSICAL_INSERT #x07)

  (cl:defconstant SW_VIDEOOUT_INSERT #x08)

  (cl:defconstant SW_MAX #x0f)

  (cl:defconstant SW_CNT (cl:+ #x0f 1))

  (cl:defconstant MSC_SERIAL #x00)

  (cl:defconstant MSC_PULSELED #x01)

  (cl:defconstant MSC_GESTURE #x02)

  (cl:defconstant MSC_RAW #x03)

  (cl:defconstant MSC_SCAN #x04)

  (cl:defconstant MSC_MAX #x07)

  (cl:defconstant MSC_CNT (cl:+ #x07 1))

  (cl:defconstant LED_NUML #x00)

  (cl:defconstant LED_CAPSL #x01)

  (cl:defconstant LED_SCROLLL #x02)

  (cl:defconstant LED_COMPOSE #x03)

  (cl:defconstant LED_KANA #x04)

  (cl:defconstant LED_SLEEP #x05)

  (cl:defconstant LED_SUSPEND #x06)

  (cl:defconstant LED_MUTE #x07)

  (cl:defconstant LED_MISC #x08)

  (cl:defconstant LED_MAIL #x09)

  (cl:defconstant LED_CHARGING #x0a)

  (cl:defconstant LED_MAX #x0f)

  (cl:defconstant LED_CNT (cl:+ #x0f 1))

  (cl:defconstant REP_DELAY #x00)

  (cl:defconstant REP_PERIOD #x01)

  (cl:defconstant REP_MAX #x01)

  (cl:defconstant SND_CLICK #x00)

  (cl:defconstant SND_BELL #x01)

  (cl:defconstant SND_TONE #x02)

  (cl:defconstant SND_MAX #x07)

  (cl:defconstant SND_CNT (cl:+ #x07 1))

  (cl:defconstant ID_BUS 0)

  (cl:defconstant ID_VENDOR 1)

  (cl:defconstant ID_PRODUCT 2)

  (cl:defconstant ID_VERSION 3)

  (cl:defconstant BUS_PCI #x01)

  (cl:defconstant BUS_ISAPNP #x02)

  (cl:defconstant BUS_USB #x03)

  (cl:defconstant BUS_HIL #x04)

  (cl:defconstant BUS_BLUETOOTH #x05)

  (cl:defconstant BUS_VIRTUAL #x06)

  (cl:defconstant BUS_ISA #x10)

  (cl:defconstant BUS_I8042 #x11)

  (cl:defconstant BUS_XTKBD #x12)

  (cl:defconstant BUS_RS232 #x13)

  (cl:defconstant BUS_GAMEPORT #x14)

  (cl:defconstant BUS_PARPORT #x15)

  (cl:defconstant BUS_AMIGA #x16)

  (cl:defconstant BUS_ADB #x17)

  (cl:defconstant BUS_I2C #x18)

  (cl:defconstant BUS_HOST #x19)

  (cl:defconstant BUS_GSC #x1A)

  (cl:defconstant BUS_ATARI #x1B)

  (cl:defconstant MT_TOOL_FINGER 0)

  (cl:defconstant MT_TOOL_PEN 1)

  (cl:defconstant FF_STATUS_STOPPED #x00)

  (cl:defconstant FF_STATUS_PLAYING #x01)

  (cl:defconstant FF_STATUS_MAX #x01)

  (cffi:defcstruct ff_replay
    (length :unsigned-short)
    (delay :unsigned-short))

  (cffi:defcstruct ff_trigger
    (button :unsigned-short)
    (interval :unsigned-short))

  (cffi:defcstruct ff_envelope
    (attack_length :unsigned-short)
    (attack_level :unsigned-short)
    (fade_length :unsigned-short)
    (fade_level :unsigned-short))

  (cffi:defcstruct ff_constant_effect
    (level :short)
    (envelope ff_envelope))

  (cffi:defcstruct ff_ramp_effect
    (start_level :short)
    (end_level :short)
    (envelope ff_envelope))

  (cffi:defcstruct ff_condition_effect
    (right_saturation :unsigned-short)
    (left_saturation :unsigned-short)
    (right_coeff :short)
    (left_coeff :short)
    (deadband :unsigned-short)
    (center :short))

  (cffi:defcstruct ff_periodic_effect
    (waveform :unsigned-short)
    (period :unsigned-short)
    (magnitude :short)
    (offset :short)
    (phase :unsigned-short)
    (envelope ff_envelope)
    (custom_len :unsigned-int)
    (custom_data :pointer))

  (cffi:defcstruct ff_rumble_effect
    (strong_magnitude :unsigned-short)
    (weak_magnitude :unsigned-short))

  (cffi:defcstruct ff_effect
    (type :unsigned-short)
    (id :short)
    (direction :unsigned-short)
    (trigger ff_trigger)
    (replay ff_replay)
    (u :pointer))

  (cffi:defcunion ff_effect_u
    (constant ff_constant_effect)
    (ramp ff_ramp_effect)
    (periodic ff_periodic_effect)
    (condition :pointer)
    (rumble ff_rumble_effect))

  (cl:defconstant FF_RUMBLE #x50)

  (cl:defconstant FF_PERIODIC #x51)

  (cl:defconstant FF_CONSTANT #x52)

  (cl:defconstant FF_SPRING #x53)

  (cl:defconstant FF_FRICTION #x54)

  (cl:defconstant FF_DAMPER #x55)

  (cl:defconstant FF_INERTIA #x56)

  (cl:defconstant FF_RAMP #x57)

  (cl:defconstant FF_EFFECT_MIN #x50)

  (cl:defconstant FF_EFFECT_MAX #x57)

  (cl:defconstant FF_SQUARE #x58)

  (cl:defconstant FF_TRIANGLE #x59)

  (cl:defconstant FF_SINE #x5a)

  (cl:defconstant FF_SAW_UP #x5b)

  (cl:defconstant FF_SAW_DOWN #x5c)

  (cl:defconstant FF_CUSTOM #x5d)

  (cl:defconstant FF_WAVEFORM_MIN #x58)

  (cl:defconstant FF_WAVEFORM_MAX #x5d)

  (cl:defconstant FF_GAIN #x60)

  (cl:defconstant FF_AUTOCENTER #x61)

  (cl:defconstant FF_MAX #x7f)

  (cl:defconstant FF_CNT (cl:+ #x7f 1))


;;;; /usr/include/linux/uinput.h
  (cl:defconstant UINPUT_VERSION 3)

  (cffi:defcstruct uinput_ff_upload
    (request_id :int)
    (retval :int)
    (effect ff_effect)
    (old ff_effect))

  (cffi:defcstruct uinput_ff_erase
    (request_id :int)
    (retval :int)
    (effect_id :int))

  (cl:defconstant UINPUT_IOCTL_BASE (char-code #\U))
  (defconstant UI_DEV_CREATE		(_IO UINPUT_IOCTL_BASE 1))
  (defconstant UI_DEV_DESTROY		(_IO UINPUT_IOCTL_BASE 2))
  (defconstant UI_SET_EVBIT		(_IOW UINPUT_IOCTL_BASE 100 :int))
  (defconstant UI_SET_KEYBIT		(_IOW UINPUT_IOCTL_BASE 101 :int))
  (defconstant UI_SET_RELBIT		(_IOW UINPUT_IOCTL_BASE 102 :int))
  (defconstant UI_SET_ABSBIT		(_IOW UINPUT_IOCTL_BASE 103 :int))
  (defconstant UI_SET_MSCBIT		(_IOW UINPUT_IOCTL_BASE 104 :int))
  (defconstant UI_SET_LEDBIT		(_IOW UINPUT_IOCTL_BASE 105 :int))
  (defconstant UI_SET_SNDBIT		(_IOW UINPUT_IOCTL_BASE 106 :int))
  (defconstant UI_SET_FFBIT		(_IOW UINPUT_IOCTL_BASE 107 :int))
  (defconstant UI_SET_PHYS		(_IOW UINPUT_IOCTL_BASE 108 '(:pointer :char)))
  (defconstant UI_SET_SWBIT		(_IOW UINPUT_IOCTL_BASE 109 :int))
  (defconstant UI_BEGIN_FF_UPLOAD	(_IOWR UINPUT_IOCTL_BASE 200 'uinput_ff_upload))
  (defconstant UI_END_FF_UPLOAD	(_IOW UINPUT_IOCTL_BASE 201 'uinput_ff_upload))
  (defconstant UI_BEGIN_FF_ERASE	(_IOWR UINPUT_IOCTL_BASE 202 'uinput_ff_erase))
  (defconstant UI_END_FF_ERASE		(_IOW UINPUT_IOCTL_BASE 203 'uinput_ff_erase))


  (cl:defconstant EV_UINPUT #x0101)

  (cl:defconstant UI_FF_UPLOAD 1)

  (cl:defconstant UI_FF_ERASE 2)

  (cl:defconstant UINPUT_MAX_NAME_SIZE 80)
  )

(eval-when (:compile-toplevel :load-toplevel :execute)

  (cffi:defcstruct uinput_user_dev
    (name :char :count #.UINPUT_MAX_NAME_SIZE)
    (id input_id)
    (ff_effects_max :int)
    (absmax :int :count #.(+ ABS_MAX 1))
    (absmin :int :count #.(+ ABS_MAX 1))
    (absfuzz :int :count #.(+ ABS_MAX 1))
    (absflat :int :count #.(+ ABS_MAX 1)))

  )
