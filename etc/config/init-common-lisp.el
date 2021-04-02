(defvar inferior-lisp-program "/usr/bin/sbcl")


;; (use-package slime
;;   :ensure t)

(use-package sly
  :hook (common-lisp-mode . sly-edit-mode) 
  :ensure t)

(use-package sly-macrostep
  :ensure t
  :hook (common-lisp-mode . sly-macrostep-mode))

(use-package sly-repl-ansi-color
  :ensure t
  :hook (common-lisp-mode . sly-repl-ansi-color))

(use-package sly-quicklisp
  :ensure t
  :after common-lisp-mode)

(provide 'init-common-lisp)
