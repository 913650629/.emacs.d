;;;===========================================
;;;					模块介绍
;;; 代码自动补全模块，使用company作为基础补全框架
;;; 使用lsp作为全线补全后端
;;; 提供yasnippet，支持代码片段快速插入
;;;===========================================

;; MODULE: CODE COMPLETE

;; AUTHOR: EvanMeek the_lty_mail@foxmail.com

;; CODE:

;; 著名的Emacs补全框架
(use-package company 
  :hook ((prog-mode sly-mrepl-mode) . company-mode)
  :init (setq company-tooltip-align-annotations t company-idle-delay 0.1 company-echo-delay 0
              company-minimum-prefix-length 2 company-require-match nil company-dabbrev-ignore-case
              nil company-dabbrev-downcase nil company-show-numbers t) 
  :config 
  :bind (:map company-active-map
              ("M-n" . nil) 
              ("M-p" . nil) 
              ("C-n" . #'company-select-next) 
              ("C-p" . #'company-select-previous)) 
  (:map leader-key
        ("c s" . #'company-yasnippet))) 

;; 人工智能补全代码
(use-package company-tabnine
  :disabled 
  :ensure t 
  :after 'company-mode 
  'company-tabnine-mode 
  :config (add-to-list 'company-backends #'company-tabnine))

;; 各个语言的Debug工具
;; (use-package dap-mode
;;   :ensure t
;;   :functions dap-hydra/nil
;;   :diminish
;;   :bind (:map lsp-mode-map
;; 			  ("<f5>" . dap-debug)
;; 			  ("M-<f5>" . dap-hydra))
;;   :hook ((after-init . dap-mode)
;; 		 (dap-mode . dap-ui-mode)
;; 		 (python-mode . (lambda () (require 'dap-python)))
;; 		 ((c-mode c++-mode) . (lambda () (require 'dap-lldb)))))


;; 美化company
(use-package company-box 
  :ensure t 
  :hook (company-mode . company-box-mode))

;; 代码片段
(use-package yasnippet 
  :ensure t
  :defer 2
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/etc/snippets")))

(provide 'init-complete)
