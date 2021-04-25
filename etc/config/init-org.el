(use-package org
  :defer 2
  :ensure t
  :bind
  ("C-c c" . 'org-capture)
  ("C-c a" . 'org-agenda)
  ("M-H" . 'org-shiftmetaleft)
  ("M-L" . 'org-shiftmetaright)
  :custom
  (org-todo-keywords '((sequence "[学习](s!/@)" "[待办](t!/@)" "[等待](w!))" "|" "[完成](d!/@)" "[取消](c!@)")
                       (sequence "[BUG](b!/@)" "[新事件](i/@)" "[已知问题](k!/@)" "[修改中](W!/@)" "|" "[已修复](f!)")))  
  :config
  (setq org-todo-keyword-faces '(("[学习]" . (:foreground "white" :background "#2ECC71" :weight bold))
								 ("[待办]" . (:foreground "white" :background "#F1C40F" :weight bold))
								 ("[等待]" . (:foreground "white" :background "#3498DB" :weight bold))
								 ("[完成]" . (:foreground "black" :background "snow " :weight bold))
								 ("[取消]" . (:foreground "white" :background "#566573" :weight bold))
								 ("[BUG]" . (:foreground "white" :background "#E74C3C" :weight bold))
								 ("[新事件]" . (:foreground "white" :background "#D35400" :weight bold))
								 ("[已知问题]" . (:foreground "white" :background "#17A589" :weight bold))
								 ("[修改中]" . (:foreground "white" :background "#BB8FCE" :weight bold))
								 ("[已修复]" . (:foreground "white" :background "#566573" :weight bold))))
  (defun evan/capture-word ()
	(interactive)
	(setq-local capture-word-item nil)
	(setq evan/capture-word-data nil)
	(let* ((word (youdao-dictionary--request (if (not (thing-at-point 'word))
												 nil
											   (thing-at-point 'word))))
		   (basic (youdao-dictionary--explains word))
		   (eng (assoc-default 'query word)))
	  (dotimes (i (map-length basic))
		(let* ((explain (map-elt basic i)) ;; 所有说明
			   ;; 词性
			   (type (progn (string-match "[a-zA-Z]+" explain)
							(concat (match-string 0 explain) ".")))
			   ;; 中文翻译
			   (chinese (progn (string-match "[\u4e00-\u9fa5；，]+" explain)
							   (match-string 0 explain))))
		  (push (concat "|" eng "|" type "|" chinese "|") capture-word-item))))
	(setq evan/capture-word-data (ivy-read "请选择要插入的词性: " capture-word-item))
	(setq evan/capture-word-data (remove "" (split-string evan/capture-word-data "|")))
	(if (null evan/capture-word-data)
		(message "光标下的单词无法捕获!")
	  (org-capture 1 "f")))

  (defun evan/capture-get-word (number)
	(cond ((eq number 1) (nth 0 evan/capture-word-data))
		  ((eq number 2) (nth 1 evan/capture-word-data))
		  ((eq number 3) (nth 2 evan/capture-word-data))))
  
  ;; 设置org-babel支持运行的代码
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
	 (shell . t))))

(use-package org-capture
  :after org
  :config
    ;; org-capture
  (setq org-capture-templates nil)
  ;; (push "~/Documents/org/capture/task.org" org-agenda-files)
  ;; (setq org-time-stamp-formats '("<%Y-%m-%d 周%u %H:%M>"))
  (add-to-list 'org-capture-templates '("c" "代码"))
  (add-to-list 'org-capture-templates '("b" "博客"))
  (add-to-list 'org-capture-templates '("s" "学习"))
  ;; 代码捕获模板
  (push '("cb" "BUG" entry (file+headline "~/Documents/org/capture/code.org" "BUG")  "* [BUG] %^{BUG} \t%^g\n创建时间: %U\nSCHEDULED: %^T\nDEADLINE: %^T\n %?") org-capture-templates)
  (push '("ci" "新事件" entry (file+headline "~/Documents/org/capture/code.org" "新事件")  "* [新事件] %^{新事件} \t%^g\n创建时间: %U\nSCHEDULED: %^T\nDEADLINE: %^T\n %?") org-capture-templates)
  ;; 学习捕获模板
  (push '("ss" "学习" entry (file+headline "~/Documents/org/capture/study.org" "学习")  "* [学习] %^{学习} \t%^g\n创建时间: %U\nSCHEDULED: %^T\nDEADLINE: %^T\n %?") org-capture-templates)
  (push '("st" "待办" entry (file+headline "~/Documents/org/capture/study.org" "待办") "* [待办] %^{待办} \t%^g\n创建时间: %U\nSCHEDULED: %^T\nDEADLINE: %^T\n %?") org-capture-templates)
  ;; 博客捕获模板
  (push '("bj" "我的日志" entry (file+headline"~/Documents/site/org/diary.org" "日志") "* %U - %^{标题}\n  %?") org-capture-templates)
  (push '("bi" "我的闪念" entry (file+headline "~/Documents/site/org/idea.org" "闪念") "* %U - %^{标题} %^g\n  %?\n") org-capture-templates)
  (push '("bk" "我的百科" entry (file+headline "~/Documents/site/org/wiki.org" "WIKI") "* %^{标题} %t %^g\n  %?\n") org-capture-templates)
  ;; 我的单词捕获模板
  (push '("w" "我的单词" table-line (file+headline "~/Documents/org/capture/word.org" "Words") " | %U | %^{en_US} | %^{词性} | %^{zh_CN} |") org-capture-templates))

(use-package org-agenda
  :after org
  :config
  (defun evan/agenda-icon-material (name)
    "返回一个all-the-icons-material图标"
    (list (all-the-icons-material name)))
  ;; 设置org-agenda分类图标
  (setq org-agenda-category-icon-alist
        `(
          ;; 学习相关
          ("待办" ,(evan/agenda-icon-material "check_box") nil nil :ascent center)
          ("学习" ,(evan/agenda-icon-material "book") nil nil :ascent center)
          ("等待" ,(evan/agenda-icon-material "ac_unit") nil nil :ascent center)
          ("完成" ,(evan/agenda-icon-material "done") nil nil :ascent center)
          ;; 代码相关
          ("取消" ,(evan/agenda-icon-material "cancel") nil nil :ascent)
          ("BUG" ,(evan/agenda-icon-material "bug_report") nil nil :ascent center)
          ("新事件" ,(evan/agenda-icon-material "new_releases") nil nil :ascent center)
          ("已知问题" ,(evan/agenda-icon-material "comment") nil nil :ascent center)
          ("修改中" ,(evan/agenda-icon-material "adjust") nil nil :ascent center)
          ("已修复" ,(evan/agenda-icon-material "thumb_up") nil nil :ascent center))))

;; 美化org
;; (use-package org-bullets
;;   :ensure t
;;   :after org
;;   :hook ('org-mode . 'org-bullets-mode)
;;   :custom
;;   (org-bullets-bullet-list '("☰" "☷" "✿" "☭")))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package gkroam
  :disabled
  :ensure t
  :hook (after-init-hook . gkroam-mode)
  :init
  (setq gkroam-root-dir "~/Documents/org/"
		gkroam-window-margin 4
		org-startup-folded nil)
  :bind
  (("C-c r I" . gkroam-index)
   ("C-c r d" . gkroam-daily)
   ("C-c r f" . gkroam-find)
   ("C-c r i" . gkroam-insert)
   ("C-c r c" . gkroam-capture)
   ("C-c r e" . gkroam-link-edit)
   ("C-c r n" . gkroam-smart-new)
   ("C-c r b" . gkroam-toggle-beautify)
   ("C-c r t" . gkroam-toggle-brackets)
   ("C-c r g" . gkroam-update)
   ("C-c r G" . gkroam-update-all))
  :config
  ;; when this minor mode is on, show and hide brackets dynamically.
  (gkroam-dynamic-brackets-mode -1))

(provide 'init-org)
