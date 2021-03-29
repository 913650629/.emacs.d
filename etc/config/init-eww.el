(use-package shrface
  :ensure t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings)
  (setq shrface-href-versatile t))

(use-package eww
  :init
  (add-hook 'eww-after-render-hook #'shrface-mode)
  :config
  (require 'shrface))

(provide 'init-eww)
