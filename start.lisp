(require :swank)
(swank:create-server :port 9999 :dont-close t :coding-system "utf-8-unix")

(require :mayu)
(mayu::main-loop)
