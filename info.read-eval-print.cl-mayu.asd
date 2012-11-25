;;;; -*- lisp -*-

(asdf:defsystem #:info.read-eval-print.cl-mayu
  :serial t
  :components ((:file "package")
	       (:file "key")
	       (:file "ffi")
               (:file "mayu")
	       (:file "config"))
  :depends-on (:cffi :iolib))
