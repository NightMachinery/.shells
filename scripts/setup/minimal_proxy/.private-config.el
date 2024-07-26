;;;
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setq copilot-node-executable (executable-find "node"))

(load "~/.secret-private-config.el" t)
(load "~/.local-private-config.el" t)
;;;
