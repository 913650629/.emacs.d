;;;===========================================
;;;					模块介绍
;;; 用户交互界面模块
;;;===========================================

;; MODULE: USER INTERFACE

;; AUTHOR: EvanMeek the_lty_mail@foxmail.com

;;; Code:

;; 设置光标颜色
;; (set-cursor-color "green2")
;; 设置光标样式
(setq-default cursor-type 'box)
;; 去除默认启动界面
(setq inhibit-startup-message t)
;; 设置英文/英文字体
(push '(progn
         (if (fontp (font-spec
			         :name evan/en-font-name
			         :style evan/en-font-style
			         :size evan/en-font-size))
	         (progn
	           (set-face-attribute 'default nil
						           :font (font-spec
								          :name evan/en-font-name
								          :style evan/en-font-style
								          :size evan/en-font-size))
               (set-fontset-font t 'han (font-spec
                                         :name evan/zh-font-name
                                         :style evan/zh-font-style
                                         :size evan/zh-font-size))
	           (set-fontset-font t ?中 (font-spec
							            :name evan/zh-font-name
							            :style evan/zh-font-style
							            :size evan/zh-font-size))
               (set-fontset-font "fontset-default" ?༼ (font-spec
                                                       :name "Noto Serif Tibetan"
                                                       :size 0))
               )
           (message "无法找到%s字体，你可以更换其他字体或安装它让这条消息消失." evan/en-font-name)))
      graphic-only-plugins-setting) 

;; 高亮当前行
(global-hl-line-mode 1)

;; 切换buffer焦点时高亮动画
(use-package beacon
  :disabled
  :ensure t
  :hook (after-init . beacon-mode))


;; 主题包									
(use-package
  doom-themes
  :ensure t)

(use-package modus-operandi-theme
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t )

(use-package lab-themes
  :ensure t)

;; 自动切换主题
(use-package
  circadian
  :defer 0
  :ensure t
  :config
  ;; 经纬度，可以在https://www.latlong.net/获取，默认是广州的
  (setq calendar-latitude 23.130280
		calendar-longitude 113.288879)
  ;; sunrise 白天用的主题 sunset 晚上用的主题
  (setq circadian-themes '((:sunrise . doom-one-light)
                           (:sunset . doom-one)))
  (circadian-setup)
  ;; 解决切换主题spaceline色块显示问题
  (add-hook 'circadian-after-load-theme-hook
			#'(lambda (theme)
				;; (spaceline-emacs-theme)
				(when (and
					   (> (car (circadian-now-time)) (car (circadian-sunrise)))
					   (< (car (circadian-now-time)) (car (circadian-sunset))))
				  (progn
					(eaf-setq eaf-pdf-dark-mode "false")
					(eaf-setq eaf-browser-dark-mode "false") 
					(eaf-setq eaf-mindmap-dark-mode "false")))
                (centaur-tabs-mode -1)
                (centaur-tabs-mode +1))))

(progn
  ;; 图标支持
  (use-package
	all-the-icons
	:defer 0
	:ensure t)
  ;; dired模式图标支持
  (use-package
	all-the-icons-dired
	:defer 0
	:ensure t
	:hook ('dired-mode . 'all-the-icons-dired-mode))
  ;; 表情符号
  (use-package
	emojify
	:ensure t
	:custom (emojify-emojis-dir "~/.emacs.d/var/emojis")
	:config
    (add-hook 'telega-root-mode-hook (lambda ()
                                       (emojify-mode +1))))
  ;; 浮动窗口支持
  (use-package
	posframe
	:defer 0
	:ensure t
	:custom
	(posframe-mouse-banish nil)))

;; 竖线
(use-package
  page-break-lines
  :ensure t
  :hook (after-init . page-break-lines-mode))

;; 启动界面
(use-package
  dashboard
  :disabled
  :ensure t
  :init
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text") 
                                    (bookmarks . "book")))
  ;; 设置标题
  (setq dashboard-banner-logo-title
        "欢迎您使用此Emacs配置文件，有任何问题可加QQ群:46689842                  ")
  ;; 设置banner
  (setq dashboard-startup-banner "~/.emacs.d/var/banner/evan-emacs-banner.png")
  (setq dashboard-center-content t) 
  (setq dashboard-set-heading-icons t) 
  ;; (setq dashboard-set-file-icons t) 
  (setq dashboard-set-navigator t)
  :config
  (display-line-numbers-mode -1))


;; modeline样式
(use-package 
  doom-modeline
  :disabled
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 10)
  (doom-modeline-bar-width 3)
  (doom-modeline-buffer-file-name-style 'file-name)
  :config
  (custom-set-faces '(mode-line ((t 
                                  (:family evan/en-font-name
                                           :style evan/en-font-style
                                           :height 125)))) 
                    '(mode-line-inactive ((t 
                                           (:family evan/en-font-name
                                                    :style evan/en-font-size
                                                    :height 125))))))
(use-package mini-modeline
  :disabled
  :ensure t
  :hook (after-init . mini-modeline-mode))


(use-package spaceline
  :disabled
  :defer 0
  :ensure t
  :config
  (spaceline-emacs-theme))

(use-package powerline
  :disabled
  :ensure t
  :config
  (powerline-center-theme))

;; 彩虹括号
(use-package 
  rainbow-delimiters 
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  ;; 设置每一级括号的颜色
  ;; (set-face-foreground 'rainbow-delimiters-depth-1-face "chartreuse3") 
  ;; (set-face-foreground 'rainbow-delimiters-depth-2-face "DodgerBlue1") 
  ;; (set-face-foreground 'rainbow-delimiters-depth-3-face "DarkOrange2")
  ;; (set-face-foreground 'rainbow-delimiters-depth-4-face "deep pink") 
  ;; (set-face-foreground 'rainbow-delimiters-depth-5-face "medium orchid") 
  ;; (set-face-foreground 'rainbow-delimiters-depth-6-face "turquoise") 
  ;; (set-face-foreground 'rainbow-delimiters-depth-7-face "lime green") 
  ;; (set-face-foreground 'rainbow-delimiters-depth-8-face "gold") 
  ;; (set-face-foreground 'rainbow-delimiters-depth-9-face "cyan") 
  ;; (set-face-bold 'rainbow-delimiters-depth-1-face "t") 
  ;; (set-face-bold 'rainbow-delimiters-depth-2-face "t") 
  ;; (set-face-bold 'rainbow-delimiters-depth-3-face "t") 
  ;; (set-face-bold 'rainbow-delimiters-depth-4-face "t") 
  ;; (set-face-bold 'rainbow-delimiters-depth-5-face "t") 
  ;; (set-face-bold 'rainbow-delimiters-depth-6-face "t") 
  ;; (set-face-bold 'rainbow-delimiters-depth-7-face "t") 
  ;; (set-face-bold 'rainbow-delimiters-depth-8-face "t") 
  ;; (set-face-bold 'rainbow-delimiters-depth-9-face "t") 
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))
;; 让info帮助信息中关键字有高亮
(use-package 
  info-colors 
  :ensure t 
  :hook ('Info-selection-hook . 'info-colors-fontify-node))

;; 缩进线-不再使用
(use-package
  indent-guide
  :disabled
  :ensure t
  :hook ((prog-mode . indent-guide-mode)
		 (python-mode . (lambda () (indent-guide-mode -1)))))

;; 彩虹猫进度条
(use-package nyan-mode
  :ensure t
  :hook (after-init . nyan-mode)
  :config
  (setq nyan-wavy-trail t
		nyan-animate-nyancat t))

;; 对齐表格
(use-package valign
  :load-path "~/.emacs.d/site-lisp/valign"
  :hook ((org-mode markdown-mode) . valign-mode)
  :config
  (setq valign-fancy-bar nil))

;; 懒猫的超简洁modeline
(use-package awesome-tray
  :disabled
  :load-path "~/.emacs.d/site-lisp/awesome-tray"
  :hook (after-init . awesome-tray-mode))

;; tab-bar
(use-package tab-bar
  :config
  (setq tab-bar-tab-name-truncated-max 8))


;; 为上层提供 init-ui 模块
(use-package centaur-tabs
  :ensure t
  :hook (after-init . centaur-tabs-mode)
  :config
  (centaur-tabs-headline-match)
  ;; (centaur-tabs-enable-buffer-reordering)
  (setq centaur-tabs-height 35
		centaur-tabs-style "bar"
		centaur-tabs-set-icons t
		centaur-tabs-gray-out-icons 'buffer
		centaur-tabs-set-bar 'under
		x-underline-at-descent-line t
		;; 开启已修改标记
		centaur-tabs-set-modified-marker t
		;; 自动排序
		;; centaur-tabs-adjust-buffer-order t
		;; 默认按键设置为Nil
		centaur-tabs-prefix-map nil
		;; 是否要显示导航按钮
		centaur-tabs-show-navigation-buttons nil)
  (centaur-tabs-change-fonts evan/en-font-name 160)
  ;; centaur show tabs rules
  ;; Centuar-tabs 显示规则
  (defun centaur-tabs-hide-tab (x)
	"Do no to show buffer X in tabs."
	(let ((name (format "%s" x)))
      (or
       ;; Current window is not dedicated window.
       (window-dedicated-p (selected-window))

       ;; Buffer name not match below blacklist.
	   ;; 黑名单
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*company" name)
       (string-prefix-p "*Flycheck" name)
       (string-prefix-p " *Mini" name)
       (string-prefix-p "*help" name)
       (string-prefix-p "*straight" name)
       (string-prefix-p " *temp" name)
       (string-prefix-p "*Help" name)
       (string-prefix-p "*LSP" name)
	   (derived-mode-p 'eaf-mode)
	   (string-prefix-p " *snails" name)
       (string-suffix-p ".pdf" name)
       ;; Is not magit buffer.
       (and (string-prefix-p "magit" name)
			(not (file-name-extension name))))))
  :bind
  (("C-c h" . centaur-tabs-backward-tab)
   ("C-c l" . centaur-tabs-forward-tab)
   ("C-c H" . centaur-tabs-backward-tab-other-window)
   ("C-c L" . centaur-tabs-forward-tab-other-window)
   ("C-c x" . centaur-tabs--kill-this-buffer-dont-ask)
   ("C-c B" . centaur-tabs-counsel-switch-group)))


;; (use-package fira-code-mode
;;   :ensure t
;;   :hook (prog-mode . fira-code-mode))

(provide 'init-ui)
