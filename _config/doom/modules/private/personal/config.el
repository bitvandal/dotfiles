
;;; ~/.config/doom/modules/private/personal/config.el -*- lexical-binding: t; -*-

;; company-mode auto-completion, to turn off use :company-complete

(after! company
  (setq company-idle-delay 0.2
    company-minimum-prefix-length 3))

;; treemacs customizations

(after! treemacs
  (setq treemacs-width 50))
