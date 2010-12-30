(in-package :mayu)

(cl:defconstant KEY_RESERVED 0)
(cl:defconstant KEY_ESC 1)
(cl:defconstant KEY_1 2)
(cl:defconstant KEY_2 3)
(cl:defconstant KEY_3 4)
(cl:defconstant KEY_4 5)
(cl:defconstant KEY_5 6)
(cl:defconstant KEY_6 7)
(cl:defconstant KEY_7 8)
(cl:defconstant KEY_8 9)
(cl:defconstant KEY_9 10)
(cl:defconstant KEY_0 11)
(cl:defconstant KEY_MINUS 12)
(cl:defconstant KEY_EQUAL 13)
(cl:defconstant KEY_BACKSPACE 14)
(cl:defconstant KEY_TAB 15)
(cl:defconstant KEY_Q 16)
(cl:defconstant KEY_W 17)
(cl:defconstant KEY_E 18)
(cl:defconstant KEY_R 19)
(cl:defconstant KEY_T 20)
(cl:defconstant KEY_Y 21)
(cl:defconstant KEY_U 22)
(cl:defconstant KEY_I 23)
(cl:defconstant KEY_O 24)
(cl:defconstant KEY_P 25)
(cl:defconstant KEY_LEFTBRACE 26)
(cl:defconstant KEY_RIGHTBRACE 27)
(cl:defconstant KEY_ENTER 28)
(cl:defconstant KEY_LEFTCTRL 29)
(cl:defconstant KEY_A 30)
(cl:defconstant KEY_S 31)
(cl:defconstant KEY_D 32)
(cl:defconstant KEY_F 33)
(cl:defconstant KEY_G 34)
(cl:defconstant KEY_H 35)
(cl:defconstant KEY_J 36)
(cl:defconstant KEY_K 37)
(cl:defconstant KEY_L 38)
(cl:defconstant KEY_SEMICOLON 39)
(cl:defconstant KEY_APOSTROPHE 40)
(cl:defconstant KEY_GRAVE 41)
(cl:defconstant KEY_LEFTSHIFT 42)
(cl:defconstant KEY_BACKSLASH 43)
(cl:defconstant KEY_Z 44)
(cl:defconstant KEY_X 45)
(cl:defconstant KEY_C 46)
(cl:defconstant KEY_V 47)
(cl:defconstant KEY_B 48)
(cl:defconstant KEY_N 49)
(cl:defconstant KEY_M 50)
(cl:defconstant KEY_COMMA 51)
(cl:defconstant KEY_DOT 52)
(cl:defconstant KEY_SLASH 53)
(cl:defconstant KEY_RIGHTSHIFT 54)
(cl:defconstant KEY_KPASTERISK 55)
(cl:defconstant KEY_LEFTALT 56)
(cl:defconstant KEY_SPACE 57)
(cl:defconstant KEY_CAPSLOCK 58)
(cl:defconstant KEY_F1 59)
(cl:defconstant KEY_F2 60)
(cl:defconstant KEY_F3 61)
(cl:defconstant KEY_F4 62)
(cl:defconstant KEY_F5 63)
(cl:defconstant KEY_F6 64)
(cl:defconstant KEY_F7 65)
(cl:defconstant KEY_F8 66)
(cl:defconstant KEY_F9 67)
(cl:defconstant KEY_F10 68)
(cl:defconstant KEY_NUMLOCK 69)
(cl:defconstant KEY_SCROLLLOCK 70)
(cl:defconstant KEY_KP7 71)
(cl:defconstant KEY_KP8 72)
(cl:defconstant KEY_KP9 73)
(cl:defconstant KEY_KPMINUS 74)
(cl:defconstant KEY_KP4 75)
(cl:defconstant KEY_KP5 76)
(cl:defconstant KEY_KP6 77)
(cl:defconstant KEY_KPPLUS 78)
(cl:defconstant KEY_KP1 79)
(cl:defconstant KEY_KP2 80)
(cl:defconstant KEY_KP3 81)
(cl:defconstant KEY_KP0 82)
(cl:defconstant KEY_KPDOT 83)
(cl:defconstant KEY_ZENKAKUHANKAKU 85)
(cl:defconstant KEY_102ND 86)
(cl:defconstant KEY_F11 87)
(cl:defconstant KEY_F12 88)
(cl:defconstant KEY_RO 89)
(cl:defconstant KEY_KATAKANA 90)
(cl:defconstant KEY_HIRAGANA 91)
(cl:defconstant KEY_HENKAN 92)
(cl:defconstant KEY_KATAKANAHIRAGANA 93)
(cl:defconstant KEY_MUHENKAN 94)
(cl:defconstant KEY_KPJPCOMMA 95)
(cl:defconstant KEY_KPENTER 96)
(cl:defconstant KEY_RIGHTCTRL 97)
(cl:defconstant KEY_KPSLASH 98)
(cl:defconstant KEY_SYSRQ 99)
(cl:defconstant KEY_RIGHTALT 100)
(cl:defconstant KEY_LINEFEED 101)
(cl:defconstant KEY_HOME 102)
(cl:defconstant KEY_UP 103)
(cl:defconstant KEY_PAGEUP 104)
(cl:defconstant KEY_LEFT 105)
(cl:defconstant KEY_RIGHT 106)
(cl:defconstant KEY_END 107)
(cl:defconstant KEY_DOWN 108)
(cl:defconstant KEY_PAGEDOWN 109)
(cl:defconstant KEY_INSERT 110)
(cl:defconstant KEY_DELETE 111)
(cl:defconstant KEY_MACRO 112)
(cl:defconstant KEY_MUTE 113)
(cl:defconstant KEY_VOLUMEDOWN 114)
(cl:defconstant KEY_VOLUMEUP 115)
(cl:defconstant KEY_POWER 116)
(cl:defconstant KEY_KPEQUAL 117)
(cl:defconstant KEY_KPPLUSMINUS 118)
(cl:defconstant KEY_PAUSE 119)
(cl:defconstant KEY_SCALE 120)
(cl:defconstant KEY_KPCOMMA 121)
(cl:defconstant KEY_HANGEUL 122)
(cl:defconstant KEY_HANGUEL 122)
(cl:defconstant KEY_HANJA 123)
(cl:defconstant KEY_YEN 124)
(cl:defconstant KEY_LEFTMETA 125)
(cl:defconstant KEY_RIGHTMETA 126)
(cl:defconstant KEY_COMPOSE 127)
(cl:defconstant KEY_STOP 128)
(cl:defconstant KEY_AGAIN 129)
(cl:defconstant KEY_PROPS 130)
(cl:defconstant KEY_UNDO 131)
(cl:defconstant KEY_FRONT 132)
(cl:defconstant KEY_COPY 133)
(cl:defconstant KEY_OPEN 134)
(cl:defconstant KEY_PASTE 135)
(cl:defconstant KEY_FIND 136)
(cl:defconstant KEY_CUT 137)
(cl:defconstant KEY_HELP 138)
(cl:defconstant KEY_MENU 139)
(cl:defconstant KEY_CALC 140)
(cl:defconstant KEY_SETUP 141)
(cl:defconstant KEY_SLEEP 142)
(cl:defconstant KEY_WAKEUP 143)
(cl:defconstant KEY_FILE 144)
(cl:defconstant KEY_SENDFILE 145)
(cl:defconstant KEY_DELETEFILE 146)
(cl:defconstant KEY_XFER 147)
(cl:defconstant KEY_PROG1 148)
(cl:defconstant KEY_PROG2 149)
(cl:defconstant KEY_WWW 150)
(cl:defconstant KEY_MSDOS 151)
(cl:defconstant KEY_COFFEE 152)
(cl:defconstant KEY_SCREENLOCK 152)
(cl:defconstant KEY_DIRECTION 153)
(cl:defconstant KEY_CYCLEWINDOWS 154)
(cl:defconstant KEY_MAIL 155)
(cl:defconstant KEY_BOOKMARKS 156)
(cl:defconstant KEY_COMPUTER 157)
(cl:defconstant KEY_BACK 158)
(cl:defconstant KEY_FORWARD 159)
(cl:defconstant KEY_CLOSECD 160)
(cl:defconstant KEY_EJECTCD 161)
(cl:defconstant KEY_EJECTCLOSECD 162)
(cl:defconstant KEY_NEXTSONG 163)
(cl:defconstant KEY_PLAYPAUSE 164)
(cl:defconstant KEY_PREVIOUSSONG 165)
(cl:defconstant KEY_STOPCD 166)
(cl:defconstant KEY_RECORD 167)
(cl:defconstant KEY_REWIND 168)
(cl:defconstant KEY_PHONE 169)
(cl:defconstant KEY_ISO 170)
(cl:defconstant KEY_CONFIG 171)
(cl:defconstant KEY_HOMEPAGE 172)
(cl:defconstant KEY_REFRESH 173)
(cl:defconstant KEY_EXIT 174)
(cl:defconstant KEY_MOVE 175)
(cl:defconstant KEY_EDIT 176)
(cl:defconstant KEY_SCROLLUP 177)
(cl:defconstant KEY_SCROLLDOWN 178)
(cl:defconstant KEY_KPLEFTPAREN 179)
(cl:defconstant KEY_KPRIGHTPAREN 180)
(cl:defconstant KEY_NEW 181)
(cl:defconstant KEY_REDO 182)
(cl:defconstant KEY_F13 183)
(cl:defconstant KEY_F14 184)
(cl:defconstant KEY_F15 185)
(cl:defconstant KEY_F16 186)
(cl:defconstant KEY_F17 187)
(cl:defconstant KEY_F18 188)
(cl:defconstant KEY_F19 189)
(cl:defconstant KEY_F20 190)
(cl:defconstant KEY_F21 191)
(cl:defconstant KEY_F22 192)
(cl:defconstant KEY_F23 193)
(cl:defconstant KEY_F24 194)
(cl:defconstant KEY_PLAYCD 200)
(cl:defconstant KEY_PAUSECD 201)
(cl:defconstant KEY_PROG3 202)
(cl:defconstant KEY_PROG4 203)
(cl:defconstant KEY_DASHBOARD 204)
(cl:defconstant KEY_SUSPEND 205)
(cl:defconstant KEY_CLOSE 206)
(cl:defconstant KEY_PLAY 207)
(cl:defconstant KEY_FASTFORWARD 208)
(cl:defconstant KEY_BASSBOOST 209)
(cl:defconstant KEY_PRINT 210)
(cl:defconstant KEY_HP 211)
(cl:defconstant KEY_CAMERA 212)
(cl:defconstant KEY_SOUND 213)
(cl:defconstant KEY_QUESTION 214)
(cl:defconstant KEY_EMAIL 215)
(cl:defconstant KEY_CHAT 216)
(cl:defconstant KEY_SEARCH 217)
(cl:defconstant KEY_CONNECT 218)
(cl:defconstant KEY_FINANCE 219)
(cl:defconstant KEY_SPORT 220)
(cl:defconstant KEY_SHOP 221)
(cl:defconstant KEY_ALTERASE 222)
(cl:defconstant KEY_CANCEL 223)
(cl:defconstant KEY_BRIGHTNESSDOWN 224)
(cl:defconstant KEY_BRIGHTNESSUP 225)
(cl:defconstant KEY_MEDIA 226)
(cl:defconstant KEY_SWITCHVIDEOMODE 227)
(cl:defconstant KEY_KBDILLUMTOGGLE 228)
(cl:defconstant KEY_KBDILLUMDOWN 229)
(cl:defconstant KEY_KBDILLUMUP 230)
(cl:defconstant KEY_SEND 231)
(cl:defconstant KEY_REPLY 232)
(cl:defconstant KEY_FORWARDMAIL 233)
(cl:defconstant KEY_SAVE 234)
(cl:defconstant KEY_DOCUMENTS 235)
(cl:defconstant KEY_BATTERY 236)
(cl:defconstant KEY_BLUETOOTH 237)
(cl:defconstant KEY_WLAN 238)
(cl:defconstant KEY_UWB 239)
(cl:defconstant KEY_UNKNOWN 240)
(cl:defconstant KEY_VIDEO_NEXT 241)
(cl:defconstant KEY_VIDEO_PREV 242)
(cl:defconstant KEY_BRIGHTNESS_CYCLE 243)
(cl:defconstant KEY_BRIGHTNESS_ZERO 244)
(cl:defconstant KEY_DISPLAY_OFF 245)
(cl:defconstant KEY_WIMAX 246)
(cl:defconstant BTN_MISC #x100)
(cl:defconstant BTN_0 #x100)
(cl:defconstant BTN_1 #x101)
(cl:defconstant BTN_2 #x102)
(cl:defconstant BTN_3 #x103)
(cl:defconstant BTN_4 #x104)
(cl:defconstant BTN_5 #x105)
(cl:defconstant BTN_6 #x106)
(cl:defconstant BTN_7 #x107)
(cl:defconstant BTN_8 #x108)
(cl:defconstant BTN_9 #x109)
(cl:defconstant BTN_MOUSE #x110)
(cl:defconstant BTN_LEFT #x110)
(cl:defconstant BTN_RIGHT #x111)
(cl:defconstant BTN_MIDDLE #x112)
(cl:defconstant BTN_SIDE #x113)
(cl:defconstant BTN_EXTRA #x114)
(cl:defconstant BTN_FORWARD #x115)
(cl:defconstant BTN_BACK #x116)
(cl:defconstant BTN_TASK #x117)
(cl:defconstant BTN_JOYSTICK #x120)
(cl:defconstant BTN_TRIGGER #x120)
(cl:defconstant BTN_THUMB #x121)
(cl:defconstant BTN_THUMB2 #x122)
(cl:defconstant BTN_TOP #x123)
(cl:defconstant BTN_TOP2 #x124)
(cl:defconstant BTN_PINKIE #x125)
(cl:defconstant BTN_BASE #x126)
(cl:defconstant BTN_BASE2 #x127)
(cl:defconstant BTN_BASE3 #x128)
(cl:defconstant BTN_BASE4 #x129)
(cl:defconstant BTN_BASE5 #x12a)
(cl:defconstant BTN_BASE6 #x12b)
(cl:defconstant BTN_DEAD #x12f)
(cl:defconstant BTN_GAMEPAD #x130)
(cl:defconstant BTN_A #x130)
(cl:defconstant BTN_B #x131)
(cl:defconstant BTN_C #x132)
(cl:defconstant BTN_X #x133)
(cl:defconstant BTN_Y #x134)
(cl:defconstant BTN_Z #x135)
(cl:defconstant BTN_TL #x136)
(cl:defconstant BTN_TR #x137)
(cl:defconstant BTN_TL2 #x138)
(cl:defconstant BTN_TR2 #x139)
(cl:defconstant BTN_SELECT #x13a)
(cl:defconstant BTN_START #x13b)
(cl:defconstant BTN_MODE #x13c)
(cl:defconstant BTN_THUMBL #x13d)
(cl:defconstant BTN_THUMBR #x13e)
(cl:defconstant BTN_DIGI #x140)
(cl:defconstant BTN_TOOL_PEN #x140)
(cl:defconstant BTN_TOOL_RUBBER #x141)
(cl:defconstant BTN_TOOL_BRUSH #x142)
(cl:defconstant BTN_TOOL_PENCIL #x143)
(cl:defconstant BTN_TOOL_AIRBRUSH #x144)
(cl:defconstant BTN_TOOL_FINGER #x145)
(cl:defconstant BTN_TOOL_MOUSE #x146)
(cl:defconstant BTN_TOOL_LENS #x147)
(cl:defconstant BTN_TOUCH #x14a)
(cl:defconstant BTN_STYLUS #x14b)
(cl:defconstant BTN_STYLUS2 #x14c)
(cl:defconstant BTN_TOOL_DOUBLETAP #x14d)
(cl:defconstant BTN_TOOL_TRIPLETAP #x14e)
(cl:defconstant BTN_TOOL_QUADTAP #x14f)
(cl:defconstant BTN_WHEEL #x150)
(cl:defconstant BTN_GEAR_DOWN #x150)
(cl:defconstant BTN_GEAR_UP #x151)
(cl:defconstant KEY_OK #x160)
(cl:defconstant KEY_SELECT #x161)
(cl:defconstant KEY_GOTO #x162)
(cl:defconstant KEY_CLEAR #x163)
(cl:defconstant KEY_POWER2 #x164)
(cl:defconstant KEY_OPTION #x165)
(cl:defconstant KEY_INFO #x166)
(cl:defconstant KEY_TIME #x167)
(cl:defconstant KEY_VENDOR #x168)
(cl:defconstant KEY_ARCHIVE #x169)
(cl:defconstant KEY_PROGRAM #x16a)
(cl:defconstant KEY_CHANNEL #x16b)
(cl:defconstant KEY_FAVORITES #x16c)
(cl:defconstant KEY_EPG #x16d)
(cl:defconstant KEY_PVR #x16e)
(cl:defconstant KEY_MHP #x16f)
(cl:defconstant KEY_LANGUAGE #x170)
(cl:defconstant KEY_TITLE #x171)
(cl:defconstant KEY_SUBTITLE #x172)
(cl:defconstant KEY_ANGLE #x173)
(cl:defconstant KEY_ZOOM #x174)
(cl:defconstant KEY_MODE #x175)
(cl:defconstant KEY_KEYBOARD #x176)
(cl:defconstant KEY_SCREEN #x177)
(cl:defconstant KEY_PC #x178)
(cl:defconstant KEY_TV #x179)
(cl:defconstant KEY_TV2 #x17a)
(cl:defconstant KEY_VCR #x17b)
(cl:defconstant KEY_VCR2 #x17c)
(cl:defconstant KEY_SAT #x17d)
(cl:defconstant KEY_SAT2 #x17e)
(cl:defconstant KEY_CD #x17f)
(cl:defconstant KEY_TAPE #x180)
(cl:defconstant KEY_RADIO #x181)
(cl:defconstant KEY_TUNER #x182)
(cl:defconstant KEY_PLAYER #x183)
(cl:defconstant KEY_TEXT #x184)
(cl:defconstant KEY_DVD #x185)
(cl:defconstant KEY_AUX #x186)
(cl:defconstant KEY_MP3 #x187)
(cl:defconstant KEY_AUDIO #x188)
(cl:defconstant KEY_VIDEO #x189)
(cl:defconstant KEY_DIRECTORY #x18a)
(cl:defconstant KEY_LIST #x18b)
(cl:defconstant KEY_MEMO #x18c)
(cl:defconstant KEY_CALENDAR #x18d)
(cl:defconstant KEY_RED #x18e)
(cl:defconstant KEY_GREEN #x18f)
(cl:defconstant KEY_YELLOW #x190)
(cl:defconstant KEY_BLUE #x191)
(cl:defconstant KEY_CHANNELUP #x192)
(cl:defconstant KEY_CHANNELDOWN #x193)
(cl:defconstant KEY_FIRST #x194)
(cl:defconstant KEY_LAST #x195)
(cl:defconstant KEY_AB #x196)
(cl:defconstant KEY_NEXT #x197)
(cl:defconstant KEY_RESTART #x198)
(cl:defconstant KEY_SLOW #x199)
(cl:defconstant KEY_SHUFFLE #x19a)
(cl:defconstant KEY_BREAK #x19b)
(cl:defconstant KEY_PREVIOUS #x19c)
(cl:defconstant KEY_DIGITS #x19d)
(cl:defconstant KEY_TEEN #x19e)
(cl:defconstant KEY_TWEN #x19f)
(cl:defconstant KEY_VIDEOPHONE #x1a0)
(cl:defconstant KEY_GAMES #x1a1)
(cl:defconstant KEY_ZOOMIN #x1a2)
(cl:defconstant KEY_ZOOMOUT #x1a3)
(cl:defconstant KEY_ZOOMRESET #x1a4)
(cl:defconstant KEY_WORDPROCESSOR #x1a5)
(cl:defconstant KEY_EDITOR #x1a6)
(cl:defconstant KEY_SPREADSHEET #x1a7)
(cl:defconstant KEY_GRAPHICSEDITOR #x1a8)
(cl:defconstant KEY_PRESENTATION #x1a9)
(cl:defconstant KEY_DATABASE #x1aa)
(cl:defconstant KEY_NEWS #x1ab)
(cl:defconstant KEY_VOICEMAIL #x1ac)
(cl:defconstant KEY_ADDRESSBOOK #x1ad)
(cl:defconstant KEY_MESSENGER #x1ae)
(cl:defconstant KEY_DISPLAYTOGGLE #x1af)
(cl:defconstant KEY_SPELLCHECK #x1b0)
(cl:defconstant KEY_LOGOFF #x1b1)
(cl:defconstant KEY_DOLLAR #x1b2)
(cl:defconstant KEY_EURO #x1b3)
(cl:defconstant KEY_FRAMEBACK #x1b4)
(cl:defconstant KEY_FRAMEFORWARD #x1b5)
(cl:defconstant KEY_CONTEXT_MENU #x1b6)
(cl:defconstant KEY_MEDIA_REPEAT #x1b7)
(cl:defconstant KEY_DEL_EOL #x1c0)
(cl:defconstant KEY_DEL_EOS #x1c1)
(cl:defconstant KEY_INS_LINE #x1c2)
(cl:defconstant KEY_DEL_LINE #x1c3)
(cl:defconstant KEY_FN #x1d0)
(cl:defconstant KEY_FN_ESC #x1d1)
(cl:defconstant KEY_FN_F1 #x1d2)
(cl:defconstant KEY_FN_F2 #x1d3)
(cl:defconstant KEY_FN_F3 #x1d4)
(cl:defconstant KEY_FN_F4 #x1d5)
(cl:defconstant KEY_FN_F5 #x1d6)
(cl:defconstant KEY_FN_F6 #x1d7)
(cl:defconstant KEY_FN_F7 #x1d8)
(cl:defconstant KEY_FN_F8 #x1d9)
(cl:defconstant KEY_FN_F9 #x1da)
(cl:defconstant KEY_FN_F10 #x1db)
(cl:defconstant KEY_FN_F11 #x1dc)
(cl:defconstant KEY_FN_F12 #x1dd)
(cl:defconstant KEY_FN_1 #x1de)
(cl:defconstant KEY_FN_2 #x1df)
(cl:defconstant KEY_FN_D #x1e0)
(cl:defconstant KEY_FN_E #x1e1)
(cl:defconstant KEY_FN_F #x1e2)
(cl:defconstant KEY_FN_S #x1e3)
(cl:defconstant KEY_FN_B #x1e4)
(cl:defconstant KEY_BRL_DOT1 #x1f1)
(cl:defconstant KEY_BRL_DOT2 #x1f2)
(cl:defconstant KEY_BRL_DOT3 #x1f3)
(cl:defconstant KEY_BRL_DOT4 #x1f4)
(cl:defconstant KEY_BRL_DOT5 #x1f5)
(cl:defconstant KEY_BRL_DOT6 #x1f6)
(cl:defconstant KEY_BRL_DOT7 #x1f7)
(cl:defconstant KEY_BRL_DOT8 #x1f8)
(cl:defconstant KEY_BRL_DOT9 #x1f9)
(cl:defconstant KEY_BRL_DOT10 #x1fa)
(cl:defconstant KEY_NUMERIC_0 #x200)
(cl:defconstant KEY_NUMERIC_1 #x201)
(cl:defconstant KEY_NUMERIC_2 #x202)
(cl:defconstant KEY_NUMERIC_3 #x203)
(cl:defconstant KEY_NUMERIC_4 #x204)
(cl:defconstant KEY_NUMERIC_5 #x205)
(cl:defconstant KEY_NUMERIC_6 #x206)
(cl:defconstant KEY_NUMERIC_7 #x207)
(cl:defconstant KEY_NUMERIC_8 #x208)
(cl:defconstant KEY_NUMERIC_9 #x209)
(cl:defconstant KEY_NUMERIC_STAR #x20a)
(cl:defconstant KEY_NUMERIC_POUND #x20b)
(cl:defconstant KEY_MIN_INTERESTING 113)
(cl:defconstant KEY_MAX #x2ff)
(cl:defconstant KEY_CNT (cl:+ #x2ff 1))
