(in-package :mayu)

;; key
(defconstant KEY_CNT (+ #x2ff 1) "キーの数")

(defvar *key-code-symbol-array*
  (let ((array (make-array KEY_CNT)))
    (loop for i from 0 below KEY_CNT
          do (setf (aref array i) i))
    array))

(defun key-code-to-symbol (code)
  (aref *key-code-symbol-array* code))

(defmacro defkey (symbol code)
  `(progn
     (defconstant ,symbol ,code)
     (setf (aref *key-code-symbol-array* ,code) ',symbol)))

;; /usr/include/linux/input.h で定義されているキー
(defkey KEY_RESERVED 0)
(defkey KEY_ESC 1)
(defkey KEY_1 2)
(defkey KEY_2 3)
(defkey KEY_3 4)
(defkey KEY_4 5)
(defkey KEY_5 6)
(defkey KEY_6 7)
(defkey KEY_7 8)
(defkey KEY_8 9)
(defkey KEY_9 10)
(defkey KEY_0 11)
(defkey KEY_MINUS 12)
(defkey KEY_EQUAL 13)
(defkey KEY_BACKSPACE 14)
(defkey KEY_TAB 15)
(defkey KEY_Q 16)
(defkey KEY_W 17)
(defkey KEY_E 18)
(defkey KEY_R 19)
(defkey KEY_T 20)
(defkey KEY_Y 21)
(defkey KEY_U 22)
(defkey KEY_I 23)
(defkey KEY_O 24)
(defkey KEY_P 25)
(defkey KEY_LEFTBRACE 26)
(defkey KEY_RIGHTBRACE 27)
(defkey KEY_ENTER 28)
(defkey KEY_LEFTCTRL 29)
(defkey KEY_A 30)
(defkey KEY_S 31)
(defkey KEY_D 32)
(defkey KEY_F 33)
(defkey KEY_G 34)
(defkey KEY_H 35)
(defkey KEY_J 36)
(defkey KEY_K 37)
(defkey KEY_L 38)
(defkey KEY_SEMICOLON 39)
(defkey KEY_APOSTROPHE 40)
(defkey KEY_GRAVE 41)
(defkey KEY_LEFTSHIFT 42)
(defkey KEY_BACKSLASH 43)
(defkey KEY_Z 44)
(defkey KEY_X 45)
(defkey KEY_C 46)
(defkey KEY_V 47)
(defkey KEY_B 48)
(defkey KEY_N 49)
(defkey KEY_M 50)
(defkey KEY_COMMA 51)
(defkey KEY_DOT 52)
(defkey KEY_SLASH 53)
(defkey KEY_RIGHTSHIFT 54)
(defkey KEY_KPASTERISK 55)
(defkey KEY_LEFTALT 56)
(defkey KEY_SPACE 57)
(defkey KEY_CAPSLOCK 58)
(defkey KEY_F1 59)
(defkey KEY_F2 60)
(defkey KEY_F3 61)
(defkey KEY_F4 62)
(defkey KEY_F5 63)
(defkey KEY_F6 64)
(defkey KEY_F7 65)
(defkey KEY_F8 66)
(defkey KEY_F9 67)
(defkey KEY_F10 68)
(defkey KEY_NUMLOCK 69)
(defkey KEY_SCROLLLOCK 70)
(defkey KEY_KP7 71)
(defkey KEY_KP8 72)
(defkey KEY_KP9 73)
(defkey KEY_KPMINUS 74)
(defkey KEY_KP4 75)
(defkey KEY_KP5 76)
(defkey KEY_KP6 77)
(defkey KEY_KPPLUS 78)
(defkey KEY_KP1 79)
(defkey KEY_KP2 80)
(defkey KEY_KP3 81)
(defkey KEY_KP0 82)
(defkey KEY_KPDOT 83)
(defkey KEY_ZENKAKUHANKAKU 85)
(defkey KEY_102ND 86)
(defkey KEY_F11 87)
(defkey KEY_F12 88)
(defkey KEY_RO 89)
(defkey KEY_KATAKANA 90)
(defkey KEY_HIRAGANA 91)
(defkey KEY_HENKAN 92)
(defkey KEY_KATAKANAHIRAGANA 93)
(defkey KEY_MUHENKAN 94)
(defkey KEY_KPJPCOMMA 95)
(defkey KEY_KPENTER 96)
(defkey KEY_RIGHTCTRL 97)
(defkey KEY_KPSLASH 98)
(defkey KEY_SYSRQ 99)
(defkey KEY_RIGHTALT 100)
(defkey KEY_LINEFEED 101)
(defkey KEY_HOME 102)
(defkey KEY_UP 103)
(defkey KEY_PAGEUP 104)
(defkey KEY_LEFT 105)
(defkey KEY_RIGHT 106)
(defkey KEY_END 107)
(defkey KEY_DOWN 108)
(defkey KEY_PAGEDOWN 109)
(defkey KEY_INSERT 110)
(defkey KEY_DELETE 111)
(defkey KEY_MACRO 112)
(defkey KEY_MUTE 113)
(defkey KEY_VOLUMEDOWN 114)
(defkey KEY_VOLUMEUP 115)
(defkey KEY_POWER 116)
(defkey KEY_KPEQUAL 117)
(defkey KEY_KPPLUSMINUS 118)
(defkey KEY_PAUSE 119)
(defkey KEY_SCALE 120)
(defkey KEY_KPCOMMA 121)
(defkey KEY_HANGEUL 122)
(defkey KEY_HANGUEL 122)
(defkey KEY_HANJA 123)
(defkey KEY_YEN 124)
(defkey KEY_LEFTMETA 125)
(defkey KEY_RIGHTMETA 126)
(defkey KEY_COMPOSE 127)
(defkey KEY_STOP 128)
(defkey KEY_AGAIN 129)
(defkey KEY_PROPS 130)
(defkey KEY_UNDO 131)
(defkey KEY_FRONT 132)
(defkey KEY_COPY 133)
(defkey KEY_OPEN 134)
(defkey KEY_PASTE 135)
(defkey KEY_FIND 136)
(defkey KEY_CUT 137)
(defkey KEY_HELP 138)
(defkey KEY_MENU 139)
(defkey KEY_CALC 140)
(defkey KEY_SETUP 141)
(defkey KEY_SLEEP 142)
(defkey KEY_WAKEUP 143)
(defkey KEY_FILE 144)
(defkey KEY_SENDFILE 145)
(defkey KEY_DELETEFILE 146)
(defkey KEY_XFER 147)
(defkey KEY_PROG1 148)
(defkey KEY_PROG2 149)
(defkey KEY_WWW 150)
(defkey KEY_MSDOS 151)
(defkey KEY_COFFEE 152)
(defkey KEY_SCREENLOCK 152)
(defkey KEY_DIRECTION 153)
(defkey KEY_CYCLEWINDOWS 154)
(defkey KEY_MAIL 155)
(defkey KEY_BOOKMARKS 156)
(defkey KEY_COMPUTER 157)
(defkey KEY_BACK 158)
(defkey KEY_FORWARD 159)
(defkey KEY_CLOSECD 160)
(defkey KEY_EJECTCD 161)
(defkey KEY_EJECTCLOSECD 162)
(defkey KEY_NEXTSONG 163)
(defkey KEY_PLAYPAUSE 164)
(defkey KEY_PREVIOUSSONG 165)
(defkey KEY_STOPCD 166)
(defkey KEY_RECORD 167)
(defkey KEY_REWIND 168)
(defkey KEY_PHONE 169)
(defkey KEY_ISO 170)
(defkey KEY_CONFIG 171)
(defkey KEY_HOMEPAGE 172)
(defkey KEY_REFRESH 173)
(defkey KEY_EXIT 174)
(defkey KEY_MOVE 175)
(defkey KEY_EDIT 176)
(defkey KEY_SCROLLUP 177)
(defkey KEY_SCROLLDOWN 178)
(defkey KEY_KPLEFTPAREN 179)
(defkey KEY_KPRIGHTPAREN 180)
(defkey KEY_NEW 181)
(defkey KEY_REDO 182)
(defkey KEY_F13 183)
(defkey KEY_F14 184)
(defkey KEY_F15 185)
(defkey KEY_F16 186)
(defkey KEY_F17 187)
(defkey KEY_F18 188)
(defkey KEY_F19 189)
(defkey KEY_F20 190)
(defkey KEY_F21 191)
(defkey KEY_F22 192)
(defkey KEY_F23 193)
(defkey KEY_F24 194)
(defkey KEY_LEFTHYPER 199)              ; 勝手に足した。
(defkey KEY_PLAYCD 200)
(defkey KEY_PAUSECD 201)
(defkey KEY_PROG3 202)
(defkey KEY_PROG4 203)
(defkey KEY_DASHBOARD 204)
(defkey KEY_SUSPEND 205)
(defkey KEY_CLOSE 206)
(defkey KEY_PLAY 207)
(defkey KEY_FASTFORWARD 208)
(defkey KEY_BASSBOOST 209)
(defkey KEY_PRINT 210)
(defkey KEY_HP 211)
(defkey KEY_CAMERA 212)
(defkey KEY_SOUND 213)
(defkey KEY_QUESTION 214)
(defkey KEY_EMAIL 215)
(defkey KEY_CHAT 216)
(defkey KEY_SEARCH 217)
(defkey KEY_CONNECT 218)
(defkey KEY_FINANCE 219)
(defkey KEY_SPORT 220)
(defkey KEY_SHOP 221)
(defkey KEY_ALTERASE 222)
(defkey KEY_CANCEL 223)
(defkey KEY_BRIGHTNESSDOWN 224)
(defkey KEY_BRIGHTNESSUP 225)
(defkey KEY_MEDIA 226)
(defkey KEY_SWITCHVIDEOMODE 227)
(defkey KEY_KBDILLUMTOGGLE 228)
(defkey KEY_KBDILLUMDOWN 229)
(defkey KEY_KBDILLUMUP 230)
(defkey KEY_SEND 231)
(defkey KEY_REPLY 232)
(defkey KEY_FORWARDMAIL 233)
(defkey KEY_SAVE 234)
(defkey KEY_DOCUMENTS 235)
(defkey KEY_BATTERY 236)
(defkey KEY_BLUETOOTH 237)
(defkey KEY_WLAN 238)
(defkey KEY_UWB 239)
(defkey KEY_UNKNOWN 240)
(defkey KEY_VIDEO_NEXT 241)
(defkey KEY_VIDEO_PREV 242)
(defkey KEY_BRIGHTNESS_CYCLE 243)
(defkey KEY_BRIGHTNESS_ZERO 244)
(defkey KEY_DISPLAY_OFF 245)
(defkey KEY_WIMAX 246)
(defkey BTN_MISC #x100)
(defkey BTN_0 #x100)
(defkey BTN_1 #x101)
(defkey BTN_2 #x102)
(defkey BTN_3 #x103)
(defkey BTN_4 #x104)
(defkey BTN_5 #x105)
(defkey BTN_6 #x106)
(defkey BTN_7 #x107)
(defkey BTN_8 #x108)
(defkey BTN_9 #x109)
(defkey BTN_MOUSE #x110)
(defkey BTN_LEFT #x110)
(defkey BTN_RIGHT #x111)
(defkey BTN_MIDDLE #x112)
(defkey BTN_SIDE #x113)
(defkey BTN_EXTRA #x114)
(defkey BTN_FORWARD #x115)
(defkey BTN_BACK #x116)
(defkey BTN_TASK #x117)
(defkey BTN_JOYSTICK #x120)
(defkey BTN_TRIGGER #x120)
(defkey BTN_THUMB #x121)
(defkey BTN_THUMB2 #x122)
(defkey BTN_TOP #x123)
(defkey BTN_TOP2 #x124)
(defkey BTN_PINKIE #x125)
(defkey BTN_BASE #x126)
(defkey BTN_BASE2 #x127)
(defkey BTN_BASE3 #x128)
(defkey BTN_BASE4 #x129)
(defkey BTN_BASE5 #x12a)
(defkey BTN_BASE6 #x12b)
(defkey BTN_DEAD #x12f)
(defkey BTN_GAMEPAD #x130)
(defkey BTN_A #x130)
(defkey BTN_B #x131)
(defkey BTN_C #x132)
(defkey BTN_X #x133)
(defkey BTN_Y #x134)
(defkey BTN_Z #x135)
(defkey BTN_TL #x136)
(defkey BTN_TR #x137)
(defkey BTN_TL2 #x138)
(defkey BTN_TR2 #x139)
(defkey BTN_SELECT #x13a)
(defkey BTN_START #x13b)
(defkey BTN_MODE #x13c)
(defkey BTN_THUMBL #x13d)
(defkey BTN_THUMBR #x13e)
(defkey BTN_DIGI #x140)
(defkey BTN_TOOL_PEN #x140)
(defkey BTN_TOOL_RUBBER #x141)
(defkey BTN_TOOL_BRUSH #x142)
(defkey BTN_TOOL_PENCIL #x143)
(defkey BTN_TOOL_AIRBRUSH #x144)
(defkey BTN_TOOL_FINGER #x145)
(defkey BTN_TOOL_MOUSE #x146)
(defkey BTN_TOOL_LENS #x147)
(defkey BTN_TOUCH #x14a)
(defkey BTN_STYLUS #x14b)
(defkey BTN_STYLUS2 #x14c)
(defkey BTN_TOOL_DOUBLETAP #x14d)
(defkey BTN_TOOL_TRIPLETAP #x14e)
(defkey BTN_TOOL_QUADTAP #x14f)
(defkey BTN_WHEEL #x150)
(defkey BTN_GEAR_DOWN #x150)
(defkey BTN_GEAR_UP #x151)
(defkey KEY_OK #x160)
(defkey KEY_SELECT #x161)
(defkey KEY_GOTO #x162)
(defkey KEY_CLEAR #x163)
(defkey KEY_POWER2 #x164)
(defkey KEY_OPTION #x165)
(defkey KEY_INFO #x166)
(defkey KEY_TIME #x167)
(defkey KEY_VENDOR #x168)
(defkey KEY_ARCHIVE #x169)
(defkey KEY_PROGRAM #x16a)
(defkey KEY_CHANNEL #x16b)
(defkey KEY_FAVORITES #x16c)
(defkey KEY_EPG #x16d)
(defkey KEY_PVR #x16e)
(defkey KEY_MHP #x16f)
(defkey KEY_LANGUAGE #x170)
(defkey KEY_TITLE #x171)
(defkey KEY_SUBTITLE #x172)
(defkey KEY_ANGLE #x173)
(defkey KEY_ZOOM #x174)
(defkey KEY_MODE #x175)
(defkey KEY_KEYBOARD #x176)
(defkey KEY_SCREEN #x177)
(defkey KEY_PC #x178)
(defkey KEY_TV #x179)
(defkey KEY_TV2 #x17a)
(defkey KEY_VCR #x17b)
(defkey KEY_VCR2 #x17c)
(defkey KEY_SAT #x17d)
(defkey KEY_SAT2 #x17e)
(defkey KEY_CD #x17f)
(defkey KEY_TAPE #x180)
(defkey KEY_RADIO #x181)
(defkey KEY_TUNER #x182)
(defkey KEY_PLAYER #x183)
(defkey KEY_TEXT #x184)
(defkey KEY_DVD #x185)
(defkey KEY_AUX #x186)
(defkey KEY_MP3 #x187)
(defkey KEY_AUDIO #x188)
(defkey KEY_VIDEO #x189)
(defkey KEY_DIRECTORY #x18a)
(defkey KEY_LIST #x18b)
(defkey KEY_MEMO #x18c)
(defkey KEY_CALENDAR #x18d)
(defkey KEY_RED #x18e)
(defkey KEY_GREEN #x18f)
(defkey KEY_YELLOW #x190)
(defkey KEY_BLUE #x191)
(defkey KEY_CHANNELUP #x192)
(defkey KEY_CHANNELDOWN #x193)
(defkey KEY_FIRST #x194)
(defkey KEY_LAST #x195)
(defkey KEY_AB #x196)
(defkey KEY_NEXT #x197)
(defkey KEY_RESTART #x198)
(defkey KEY_SLOW #x199)
(defkey KEY_SHUFFLE #x19a)
(defkey KEY_BREAK #x19b)
(defkey KEY_PREVIOUS #x19c)
(defkey KEY_DIGITS #x19d)
(defkey KEY_TEEN #x19e)
(defkey KEY_TWEN #x19f)
(defkey KEY_VIDEOPHONE #x1a0)
(defkey KEY_GAMES #x1a1)
(defkey KEY_ZOOMIN #x1a2)
(defkey KEY_ZOOMOUT #x1a3)
(defkey KEY_ZOOMRESET #x1a4)
(defkey KEY_WORDPROCESSOR #x1a5)
(defkey KEY_EDITOR #x1a6)
(defkey KEY_SPREADSHEET #x1a7)
(defkey KEY_GRAPHICSEDITOR #x1a8)
(defkey KEY_PRESENTATION #x1a9)
(defkey KEY_DATABASE #x1aa)
(defkey KEY_NEWS #x1ab)
(defkey KEY_VOICEMAIL #x1ac)
(defkey KEY_ADDRESSBOOK #x1ad)
(defkey KEY_MESSENGER #x1ae)
(defkey KEY_DISPLAYTOGGLE #x1af)
(defkey KEY_SPELLCHECK #x1b0)
(defkey KEY_LOGOFF #x1b1)
(defkey KEY_DOLLAR #x1b2)
(defkey KEY_EURO #x1b3)
(defkey KEY_FRAMEBACK #x1b4)
(defkey KEY_FRAMEFORWARD #x1b5)
(defkey KEY_CONTEXT_MENU #x1b6)
(defkey KEY_MEDIA_REPEAT #x1b7)
(defkey KEY_DEL_EOL #x1c0)
(defkey KEY_DEL_EOS #x1c1)
(defkey KEY_INS_LINE #x1c2)
(defkey KEY_DEL_LINE #x1c3)
(defkey KEY_FN #x1d0)
(defkey KEY_FN_ESC #x1d1)
(defkey KEY_FN_F1 #x1d2)
(defkey KEY_FN_F2 #x1d3)
(defkey KEY_FN_F3 #x1d4)
(defkey KEY_FN_F4 #x1d5)
(defkey KEY_FN_F5 #x1d6)
(defkey KEY_FN_F6 #x1d7)
(defkey KEY_FN_F7 #x1d8)
(defkey KEY_FN_F8 #x1d9)
(defkey KEY_FN_F9 #x1da)
(defkey KEY_FN_F10 #x1db)
(defkey KEY_FN_F11 #x1dc)
(defkey KEY_FN_F12 #x1dd)
(defkey KEY_FN_1 #x1de)
(defkey KEY_FN_2 #x1df)
(defkey KEY_FN_D #x1e0)
(defkey KEY_FN_E #x1e1)
(defkey KEY_FN_F #x1e2)
(defkey KEY_FN_S #x1e3)
(defkey KEY_FN_B #x1e4)
(defkey KEY_BRL_DOT1 #x1f1)
(defkey KEY_BRL_DOT2 #x1f2)
(defkey KEY_BRL_DOT3 #x1f3)
(defkey KEY_BRL_DOT4 #x1f4)
(defkey KEY_BRL_DOT5 #x1f5)
(defkey KEY_BRL_DOT6 #x1f6)
(defkey KEY_BRL_DOT7 #x1f7)
(defkey KEY_BRL_DOT8 #x1f8)
(defkey KEY_BRL_DOT9 #x1f9)
(defkey KEY_BRL_DOT10 #x1fa)
(defkey KEY_NUMERIC_0 #x200)
(defkey KEY_NUMERIC_1 #x201)
(defkey KEY_NUMERIC_2 #x202)
(defkey KEY_NUMERIC_3 #x203)
(defkey KEY_NUMERIC_4 #x204)
(defkey KEY_NUMERIC_5 #x205)
(defkey KEY_NUMERIC_6 #x206)
(defkey KEY_NUMERIC_7 #x207)
(defkey KEY_NUMERIC_8 #x208)
(defkey KEY_NUMERIC_9 #x209)
(defkey KEY_NUMERIC_STAR #x20a)
(defkey KEY_NUMERIC_POUND #x20b)
(defkey KEY_MIN_INTERESTING 113)
(defkey KEY_MAX #x2ff)


;;; modifier
(defvar *shift* (list KEY_LEFTSHIFT KEY_RIGHTSHIFT))
(defvar *ctrl*  (list KEY_LEFTCTRL  KEY_RIGHTCTRL))
(defvar *alt*   (list KEY_LEFTALT   KEY_RIGHTALT))
(defvar *meta*  (list KEY_LEFTMETA  KEY_RIGHTMETA))
(defvar *super* (list))
(defvar *hyper* (list KEY_LEFTHYPER))

(defconstant +any+      '+any+)
(defconstant +alt+      '+alt+)
(defconstant +ctrl+     '+ctrl+)
(defconstant +meta+     '+meta+)
(defconstant +shift+    '+shift+)
(defconstant +super+    '+super+)
(defconstant +hyper+    '+hyper+)
(defconstant +no-alt+   '+no-alt+)
(defconstant +no-ctrl+  '+no-ctrl+)
(defconstant +no-meta+  '+no-meta+)
(defconstant +no-shift+ '+no-shift+)
(defconstant +no-super+ '+no-super+)
(defconstant +no-hyper+ '+no-hyper+)

(defvar *modifiers*
  (list (list +alt+ +no-alt+)
        (list +ctrl+ +no-ctrl+)
        (list +meta+ +no-meta+)
        (list +shift+ +no-shift+)
        (list +super+ +no-super+)
        (list +hyper+ +no-hyper+)))


;;; key action (value)
(defconstant +press+ 1)
(defconstant +release+ 0)
(defconstant +repeat+ 2)
