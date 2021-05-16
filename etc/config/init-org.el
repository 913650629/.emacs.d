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
                       (sequence "[今日待办](t)" "[急事](1)" "[要事](2)" "[烦事](3)" "[杂事](4)" "[待办](T!)" "|" "[完成](d@)")
                       (sequence "[待阅读](r)" "[正在阅读](R!)")                                (sequence "[待学习](s)" "[正在学习](S!)")
                       (sequence "[错题](q)" "[好题](g)""|" "[习题集](j!)")
                       (sequence "[正式考试](正)" "[模拟考试](模)" "|" "[考试总结](总@)")
                       (sequence "[考试笔记](记)" "[科目章节](章)" "[读书笔记](记)" "[阅读章节](章)" "[学习笔记](记)" "[学习模块](块)" "|" "[总结](d@)")
                       ))
  :config
  (setq org-todo-keyword-faces '(
								 ("[今日待办]" . (:foreground "white" :background "#D6D5B7" :weight bold))
								 ("[急事]" . (:foreground "white" :background "#F4606C" :weight bold))
								 ("[要事]" . (:foreground "white" :background "#EB7347" :weight bold))
                                 ("[烦事]" . (:foreground "white" :background "#FC9D99" :weight bold))
                                 ("[杂事]" . (:foreground "white" :background "#ECAD9E" :weight bold))
								 ("[待办]" . (:foreground "white" :background "#D1BA74" :weight bold))
								 ("[完成]" . (:foreground "white" :background "#2C3E50" :weight bold))
								 ("[总结]" . (:foreground "white" :background "#D0D0D0" :weight bold))

								 ("[模拟考试]" . (:foreground "white" :background "#EB7347" :weight bold))
								 ("[正式考试]" . (:foreground "white" :background "#D24D57" :weight bold))
                                 ("[待阅读]" . (:foreground "white" :background "#BEE7E9" :weight bold))
                                 ("[正在阅读]" . (:foreground "white" :background "#BEEDC7" :weight bold))
                                 ("[待学习]" . (:foreground "white" :background "#BEE7E9" :weight bold))
                                 ("[正在学习]" . (:foreground "white" :background "#BEEDC7" :weight bold))
								 ("[考试笔记]" . (:foreground "white" :background "#D1BA74" :weight bold))
								 ("[科目章节]" . (:foreground "white" :background "#D6D5B7" :weight bold))
								 ("[考试总结]" . (:foreground "white" :background "#F4606C" :weight bold))
								 ("[错题]" . (:foreground "white" :background "#D24D57" :weight bold))                                 
								 ("[好题]" . (:foreground "white" :background "#19CAAD" :weight bold))
								 ("[习题集]" . (:foreground "white" :background "#39B54A" :weight bold))

								 ("[读书笔记]" . (:foreground "white" :background "#D1BA74" :weight bold))
								 ("[阅读章节]" . (:foreground "white" :background "#D6D5B7" :weight bold))

								 ("[学习笔记]" . (:foreground "white" :background "#D1BA74" :weight bold))
								 ("[学习模块]" . (:foreground "white" :background "#D6D5B7" :weight bold))))
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

   (add-to-list 'org-capture-templates '("e" "考试"))
   (add-to-list 'org-capture-templates '("r" "阅读"))
   (add-to-list 'org-capture-templates '("s" "学习"))
   (add-to-list 'org-capture-templates '("t" "待办"))   
   ;; 代码捕获模板
     (push '("ed" "考试总结" entry (file+headline "~/Documents/org/exam.org" "考试总结") "* [考试总结] %^{试卷名} \t%^g\n创建时间: %U\n** [错题] %?\n") org-capture-templates)        
     (push '("en" "考试笔记" entry (file+headline "~/Documents/org/exam.org" "考试笔记") "* [考试笔记] %^{科目名} \t%^g\n创建时间: %U\n** [科目章节] %^{章节名}\n %?\n") org-capture-templates)
     (push '("et" "模拟考试" entry (file+headline "~/Documents/org/exam.org" "考试列表") "* [模拟考试] %^{试卷名} \t%^g\n创建时间: %U\n开始时间: %^t\n %?\n") org-capture-templates)
     (push '("er" "正式考试" entry (file+headline "~/Documents/org/exam.org" "考试列表") "* [正式考试] %^{试卷名} \t%^g\n创建时间: %U\n开始时间: %^t\n %?\n") org-capture-templates)
     (push '("rn" "读书笔记" entry (file+headline "~/Documents/org/book.org" "读书笔记") "* [读书笔记] %^{书籍名} \t%^g\n创建时间: %U\n** [阅读章节] %^{章节名}\n %?\n") org-capture-templates)
     (push '("ra" "发现好书" entry (file+headline "~/Documents/org//book.org" "读书列表") "* [待阅读] %^{书籍名} \t%^g\n创建时间: %U\n %?\n") org-capture-templates)
     (push '("sn" "学习笔记" entry (file+headline "~/Documents/org/study.org" "学习笔记") "* [学习笔记] %^{技能名} \t%^g\n创建时间: %U\n** [学习模块] %^{模块名}\n %?\n") org-capture-templates)
     (push '("sa" "技能学习" entry (file+headline "~/Documents/org/study.org" "技能列表") "* [待学习] %^{技能名} \t%^g\n创建时间: %U\n %?\n") org-capture-templates)     
     (push '("tc" "完整待办" entry (file+headline "~/Documents/org/day.org" "待办") "* [要事] %^{要事} \t%^g%?\n创建时间: %U\n开始时间: %^t\n截至时间: %^t\n") org-capture-templates)
     (push '("tt" "待办" entry (file+headline "~/Documents/org/day.org" "待办") "* [要事] %^{要事}%?\n创建时间: %U\n") org-capture-templates)
     (push '("td" "今日待办" entry (file+datetree "~/Documents/org/day.org") "* 今日总结\n1. \n* 今日待办\n** [今日待办] %^{今日待办}\n %?\n") org-capture-templates))

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
