(load "~/.sbclrc")

;(require :swank)
;(swank:create-server :port 9999 :dont-close t)

(require :info.read-eval-print.cl-mayu)
(mayu::main-loop)
