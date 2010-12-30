;;;; mayu.asd

(asdf:defsystem #:mayu
  :serial t
  :components ((:file "package")
	       (:file "key")
	       (:file "ffi")
               (:file "mayu")
	       (:file "config"))
  :depends-on (:cffi))

