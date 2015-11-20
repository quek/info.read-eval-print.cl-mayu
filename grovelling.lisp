(in-package :mayu.epoll)

(include "sys/epoll.h")

(ctype size-t "size_t")

(progn
  (cunion epoll-data "epoll_data_t"
          (ptr "ptr" :type :pointer)
          (fd  "fd"  :type :int)
          (u32 "u32" :type :uint32)
          (u64 "u64" :type :uint64))

  (cstruct epoll-event "struct epoll_event"
           (events "events" :type :uint32)
           (data   "data"   :type (:union epoll-data)))

  (constant (epoll-ctl-add "EPOLL_CTL_ADD"))
  (constant (epoll-ctl-del "EPOLL_CTL_DEL"))
  (constant (epoll-ctl-mod "EPOLL_CTL_MOD"))

  (constant (epollin "EPOLLIN"))
  (constant (epollrdnorm "EPOLLRDNORM"))
  (constant (epollrdband "EPOLLRDBAND"))
  (constant (epollpri "EPOLLPRI"))
  (constant (epollout "EPOLLOUT"))
  (constant (epollwrnorm "EPOLLWRNORM"))
  (constant (epollwrband "EPOLLWRBAND"))
  (constant (epollerr "EPOLLERR"))
  (constant (epollhup "EPOLLHUP"))
  (constant (epollmsg "EPOLLMSG"))
  (constant (epolloneshot "EPOLLONESHOT"))
  (constant (epollet "EPOLLET")))
