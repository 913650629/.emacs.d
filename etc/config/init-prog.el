;;;===========================================
;;;					模块介绍
;;; 编程模块
;;;===========================================

;; MODULE: PROGRAMMING

;; AUTHOR: EvanMeek the_lty_mail@foxmail.com

;; CODE:

;; 微软的python语言服务器-老牌稳定
;; (use-package lsp-python-ms
;;   :disabled
;;   :ensure t
;;   :hook (python-mode . (lambda () 
;;                          (require 'lsp-python-ms) 
;;                          (lsp))) 
;;   :custom (lsp-python-ms-executable
;;            "~/.emacs.d/var/python-language-server/output/bin/Release/linux-x64/publish/Microsoft.Python.LanguageServer"))

;; 大量可用的代码片段
(use-package 
  yasnippet-snippets 
  :ensure t
  :after yasnippet)

;; 编译运行当前文件
(use-package 
  quickrun 
  :ensure t 
  :commands(quickrun) 
  :bind (:map leader-key
              ("c r" . #'quickrun)) 
  :init (setq quickrun-timeout-seconds nil) 
  (setq quickrun-focus-p nil) 
  (setq quickrun-input-file-extension nil) 
  :config
  (quickrun-add-command "python" 
    '((:command .
                "python3") 
      (:exec .
             "%c %s") 
      (:tempfile . 
                 nil))
    :default "python")
  (quickrun-add-command "c++/c1z"
	'((:command . "g++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
				   "%e %a"))
      (:remove  . ("%e")))
	:default "c++"))

;; 将文档插入代码中
(use-package docstr
  :ensure t
  :hook (prog-mode . docstr-mode))

(provide 'init-prog)
