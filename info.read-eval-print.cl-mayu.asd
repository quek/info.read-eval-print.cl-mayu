;;;; -*- lisp -*-
(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem #:info.read-eval-print.cl-mayu
  :serial t
  :components ((:file "package")
               (cffi-grovel:grovel-file "grovelling")
               (cffi-grovel:grovel-file "input")
               (:file "key")
               (:file "epoll")
               (:file "mayu")
               (:file "config"))
  :depends-on (:cffi))
