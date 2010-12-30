(in-package :mayu)

(set-key-array (KEY_A KEY_A)
               (KEY_B KEY_B))

(def-one-shot
    (KEY_RIGHTBRACE KEY_RIGHTSHIFT)
    (KEY_RIGHTBRACE KEY_RIGHTSHIFT))

(def-sequence
    ((KEY_3) (:shift KEY_3))
    ((:shift KEY_3) (KEY_3)))



