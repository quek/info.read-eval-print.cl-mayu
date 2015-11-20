;;;; package.lisp

(defpackage :info.read-eval-print.cl-mayu.epoll
  (:use)
  (:nicknames :mayu.epoll)
  (:export #:data
           #:epoll-ctl-add
           #:epoll-data
           #:epoll-event
           #:epollin
           #:events
           #:fd
           #:size-t))

(defpackage :info.read-eval-print.cl-mayu
  (:use :cl :mayu.epoll)
  (:nicknames :mayu))

