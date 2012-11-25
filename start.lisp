(load "~/.sbclrc")

(require :swank)
;(swank:create-server :port 9999 :dont-close t :coding-system "utf-8-unix")

(require :info.read-eval-print.cl-mayu)
(mayu::main-loop)
