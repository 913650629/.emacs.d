;; Python
(use-package 
  python 
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil) 
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (when (and (executable-find "python3") 
             (string= python-shell-interpreter "python")) 
    (setq python-shell-interpreter "/usr/bin/python3"))
  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell (exec-path-from-shell-copy-env "PYTHONPATH"))
  ;; Live Coding in Python
  (use-package python-black
	:ensure t))

(use-package elpy
  :disabled
  :ensure t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (push "~/.local/bin/" exec-path))

(provide 'init-python)
