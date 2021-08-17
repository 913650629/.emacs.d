(use-package org
  :defer 2
  :ensure t
  :bind
  ("C-c c" . 'org-capture)
  ("C-c a" . 'org-agenda)
  ("M-H" . 'org-shiftmetaleft)
  ("M-L" . 'org-shiftmetaright)
  :custom
  (org-todo-keywords '(                      
                       (sequence "[今日待办](t)" "[急事](1)" "[要事](2)" "[烦事](3)" "[杂事](4)" "[待办](T!)" "|" "[完成](d)")
                       (sequence "[上午](m)" "[下午](a)" "[晚间](n)" "|" "[完成](d)")
                       ))
  :config
  (setq org-todo-keyword-faces '(
								 ("[今日待办]" . (:foreground "black" :background "#D6D5B7" :weight bold))
								 ("[急事]" . (:foreground "black" :background "#F4606C" :weight bold))
								 ("[要事]" . (:foreground "black" :background "#EB7347" :weight bold))
                                 ("[烦事]" . (:foreground "black" :background "#FC9D99" :weight bold))
                                 ("[杂事]" . (:foreground "black" :background "#ECAD9E" :weight bold))
								 ("[待办]" . (:foreground "black" :background "#D1BA74" :weight bold))
								 ("[完成]" . (:foreground "black" :background "#AEDD81" :weight bold))
								 ("[上午]" . (:foreground "black" :background "#EB7347" :weight bold))
								 ("[下午]" . (:foreground "black" :background "#D24D57" :weight bold))
                                 ("[晚间]" . (:foreground "black" :background "#E6CEAC" :weight bold))))
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

;;   (add-to-list 'org-capture-templates '("t" "待办"))   
   ;; 代码捕获模板
     (push '("c" "计划" entry (file+headline "~/Documents/org/day.org" "待办") "* [要事] %^{要事} %?\n  创建时间: %U\n  开始时间: %^t\n  截至时间: %^t") org-capture-templates)
     (push '("t" "待办" entry (file+headline "~/Documents/org/day.org" "待办") "* [要事] %^{要事}%?\n  创建时间: %U") org-capture-templates)
     (push '("d" "今日" entry (file+datetree "~/Documents/org/day.org") "* 今日时间\n** [今日待办] %?\n** [上午] 8:30-11:30\n   上午建议产出时间：150min\n\t- [ ] 计划15x\n\t- [ ] 学习\n\t\t- [ ] 视频40x\n\t\t- [ ] 学习30x\n\t- [ ] 工作30x\n** [下午] 13:00-18:00\n   下午建议产出时间：240min\n\t- [ ] 计划15x\n\t- [ ] 学习\n\t\t- [ ] 视频40x\n\t\t- [ ] 学习30x\n\t- [ ] 工作30x\n** [晚间] 19:00-23:30\n   晚上建议产出时间：120min\n\t- [ ] 学习\n\t\t- [ ] 视频40x\n\t\t- [ ] 学习30x") org-capture-templates))

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
          ("计划" ,(evan/agenda-icon-material "book") nil nil :ascent center)
          ("今日" ,(evan/agenda-icon-material "ac_unit") nil nil :ascent center)
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
