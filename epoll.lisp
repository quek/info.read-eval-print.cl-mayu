(in-package :mayu)

(cffi:defcfun epoll-create :int
  (size :int))

(cffi:defcfun ("epoll_ctl" %epoll-ctl) :int
  (epfd :int)
  (op :int)
  (fd :int)
  (epoll_event (:pointer (:struct epoll-event))))

(cffi:defcfun epoll-wait :int
  (epfd :int)
  (epoll_event (:pointer (:struct epoll-event)))
  (maxevents :int)
  (timeout :int))

(cffi:defcfun bzero :void
  (s :pointer)
  (size size-t))
