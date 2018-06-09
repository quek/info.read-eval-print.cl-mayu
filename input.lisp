(in-package :mayu)


(include "/usr/include/asm-generic/ioctl.h")

(constant (+ioc-none+ "_IOC_NONE"))
(constant (+ioc-write+ "_IOC_WRITE"))
(constant (+ioc-read+ "_IOC_READ"))
(constant (+ioc-nrshift+ "_IOC_NRSHIFT"))
(constant (+ioc-typeshift+ "_IOC_TYPESHIFT"))
(constant (+ioc-sizeshift+ "_IOC_SIZESHIFT"))
(constant (+ioc-dirshift+ "_IOC_DIRSHIFT"))


(include "linux/input.h")

(constant (+eviocgid+ "EVIOCGID"))
(constant (+eviocgversion+ "EVIOCGVERSION"))
(constant (+eviocgrab+ "EVIOCGRAB"))

(constant (+ev-key+ "EV_KEY"))
(constant (+ev-max+ "EV_MAX"))
(constant (+ev-syn+ "EV_SYN"))
(constant (+ev-msc+ "EV_MSC"))
(constant (+ev-rep+ "EV_REP"))
(constant (+ev-rel+ "EV_REL"))
(constant (+ev-abs+ "EV_ABS"))
(constant (+ev-sw+ "EV_SW"))
(constant (+ev-led+ "EV_LED"))
(constant (+ev-snd+ "EV_SND"))
(constant (+ev-ff+ "EV_FF"))
(constant (+ev-pwr+ "EV_PWR"))
(constant (+ev-ff-status+ "EV_FF_STATUS"))
(constant (+syn-report+ "SYN_REPORT"))

(constant (+rel-x+ "REL_X"))
(constant (+rel-y+ "REL_Y"))
(constant (+btn-mouse+ "BTN_MOUSE"))
(constant (+btn-left+ "BTN_LEFT"))
(constant (+btn-middle+ "BTN_MIDDLE"))
(constant (+btn-right+ "BTN_RIGHT"))
(constant (+btn-forward+ "BTN_FORWARD"))
(constant (+btn-back+ "BTN_BACK"))

(constant (+ev-version+ "EV_VERSION"))

;;(constant (+eviocgbit+ "EVIOCGBIT"))

(constant (+bus-usb+ "BUS_USB"))
(constant (+bus-i8042+ "BUS_I8042"))
(constant (+bus-adb+ "BUS_ADB"))


(cstruct timeval "struct timeval"
         (tv-sec "tv_sec" :type :int64)
         (tv-usec "tv_usec" :type :int64))

(cstruct input-event "struct input_event"
         (time "time" :type (:struct timeval))
         (type "type" :type :uint16)
         (code "code" :type :uint16)
         (value "value" :type :int32))

(cstruct input-id "struct input_id"
         (bustype "bustype" :type :uint16)
         (vendor "vendor" :type :uint16)
         (product "product" :type :uint16)
         (version "version" :type :uint16))


(include "linux/uinput.h")

(constant (+ui-dev-create+ "UI_DEV_CREATE"))
(constant (+ui-dev-destroy+ "UI_DEV_DESTROY"))
(constant (+ui-set-evbit+ "UI_SET_EVBIT"))
(constant (+ui-set-keybit+ "UI_SET_KEYBIT"))
(constant (+ui-set-relbit+ "UI_SET_RELBIT"))
(constant (+ui-set-absbit+ "UI_SET_ABSBIT"))
(constant (+ui-set-mscbit+ "UI_SET_MSCBIT"))
(constant (+ui-set-ledbit+ "UI_SET_LEDBIT"))
(constant (+ui-set-sndbit+ "UI_SET_SNDBIT"))
(constant (+ui-set-ffbit+ "UI_SET_FFBIT"))
(constant (+ui-set-phys+ "UI_SET_PHYS"))
(constant (+ui-set-swbit+ "UI_SET_SWBIT"))
(constant (+ui-begin-ff-upload+ "UI_BEGIN_FF_UPLOAD"))
(constant (+ui-end-ff-upload+ "UI_END_FF_UPLOAD"))
(constant (+ui-begin-ff-erase+ "UI_BEGIN_FF_ERASE"))
(constant (+ui-end-ff-erase+ "UI_END_FF_ERASE"))

(constant (+uinput-max-name-size+ "UINPUT_MAX_NAME_SIZE"))

(constant (+abs-cnt+ "ABS_CNT"))

(cstruct uinput-user-dev "struct uinput_user_dev"
         (name "name" :type :char :count :auto)
         (id "id" :type (:struct input-id))
         (ff-effects-max "ff_effects_max" :type :uint32)
         (absmax "absmax" :type :int32 :count :auto)
         (absmin "absmin" :type :int32 :count :auto)
         (absfuzz "absfuzz" :type :int32 :count :auto)
         (absflat "absflat" :type :int32 :count :auto))


