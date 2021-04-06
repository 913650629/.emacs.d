;; 图形化插件特殊设置
(if (not (graphic-p))
    (add-hook 'after-make-frame-functions
              (lambda (new-frame)
                (select-frame new-frame)
                (dolist (elisp-code graphic-only-plugins-setting)
                  (eval elisp-code))))
  (dolist (elisp-code graphic-only-plugins-setting)
    (eval elisp-code)))



(provide 'init-graphic)
