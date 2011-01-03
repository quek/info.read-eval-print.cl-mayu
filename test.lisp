(in-package :mayu)

(flet ((f (x y)
         (string< (princ-to-string x) (princ-to-string y))))
  (defun seq= (a b)
    (equal (sort a #'f)
           (sort b #'f))))

(defmacro test (&body body)
  `(let ((*uinput-fd* (sb-posix:open "/dev/null" sb-posix:o-wronly))
         (*one-shot-status* nil)
         (*sequence-temp-mod* nil)
         (*sequence-temp-mod-for-current-mod* nil)
         (*right-shift* nil)
         (*left-shift* nil)
         (*right-ctrl* nil)
         (*left-ctrl* nil)
         (*right-alt* nil)
         (*left-alt* nil)
         (*right-meta* nil)
         (*left-meta* nil))
     (unwind-protect
          (progn
            ,@body)
       (sb-posix:close *uinput-fd*))))

(defmacro test-seq (a b)
  `(test (seq= ,a ,b)))

(defmacro assert-seq (a b &optional places datum &rest arguments)
  `(assert (seq= ,a ,b) ,places ,datum ,@arguments))

#+ba
(assert (seq= `((,KEY_LEFTSHIFT 1) (,KEY_LEFTALT 1) (,KEY_LEFTCTRL 0) (,KEY_B 1))
                       (let ((*sequence-temp-mod* nil)
                             (*left-ctrl* t)
                             (*sequence-table* `(((+ctrl+ ,KEY_A) . (+shift+ +alt+ ,KEY_B)))))
                         (proc-sequence KEY_A +press+))))


(assert (sequence-match-p (list KEY_COMMA) (list +any+ +no-shift+ KEY_COMMA)))
(assert (sequence-match-p (list +shift+ KEY_0) (list +any+ +shift+ KEY_0)))
(assert (not (sequence-match-p (list +shift+ KEY_DOT) (list +any+ +no-shift+ KEY_DOT))))

(test-seq
 `((+shift+ ,+release+) (,KEY_DOT ,+press+))
 (compute-send-sequence (list +any+ +shift+ KEY_DOT) (list +any+ KEY_DOT) (list +shift+) KEY_DOT +press+))
(test-seq
 `((+shift+ ,+release+) (,KEY_DOT ,+press+))
 (compute-send-sequence (list +any+ +shift+ KEY_DOT) (list +any+ KEY_DOT) (list +shift+ +alt+) KEY_DOT +press+))
(test-seq
 `((,KEY_DOT ,+press+))
 (compute-send-sequence (list +any+ +shift+ KEY_0) (list +any+ +shift+ KEY_DOT) (list +shift+) KEY_0 +press+))



(test
  (assert-seq `((,KEY_LEFTSHIFT ,+press+))
              (proc-key KEY_SPACE +press+))
  (assert-seq '(+shift+) (current-mod)
              nil "current-mod ~a *sequence-temp-mod* ~a *left-shift* ~a"
              (current-mod) *sequence-temp-mod* *left-shift*)
  (assert-seq `((+shift+ ,+release+) (,KEY_COMMA ,+press+) (+shift+ ,+press+))
              (proc-key KEY_W +press+)
              nil "~a" (proc-key KEY_W +press+))
  (assert-seq `((,KEY_A ,+press+))
              (proc-key KEY_A +press+)
              nil "~a" (proc-key KEY_A +press+))
  )
