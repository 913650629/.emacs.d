;;;===========================================
;;;					模块介绍
;;; 用户交互界面模块
;;;===========================================

;; MODULE: USER INTERFACE

;; AUTHOR: EvanMeek the_lty_mail@foxmail.com

;;; Code:
;; 设置窗口之间的分割线
(use-package window-divider-mode
  :hook (after-init . window-divider-mode)
  :custom
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places t))

; 解决Emacs在KDE下最大化出现间隙的问题
(setq frame-resize-pixelwise t)
;; 设置光标颜色
;; (set-cursor-color "green2")
;; 设置光标样式
(setq-default cursor-type 'box)
;; 去除默认启动界面
(setq inhibit-startup-message t)
;; 设置英文/英文字体
(setq font-config '(progn
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
                                                                   :size 0)))
                       (message "无法找到%s字体，你可以更换其他字体或安装它让这条消息消失." evan/en-font-name))))

(push font-config graphic-only-plugins-setting)

;; 高亮当前行
(global-hl-line-mode 1)

;; 切换buffer焦点时高亮动画
(use-package beacon
  :disabled
  :ensure t
  :hook (after-init . beacon-mode))

;; 主题包									
(use-package doom-themes
  :ensure t
  :config)

(use-package modus-operandi-theme
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t )

(use-package lab-themes
  :ensure t)


(use-package lazycat-theme
  :quelpa (lazycat-theme
            :fetcher github
            :repo "manateelazycat/lazycat-theme"))

;; 自动切换主题
(use-package circadian
  :ensure t
  :hook ('after-make-frame-functions . (lambda ()
                                         (circadian-setup)))
  :config
  ;; 经纬度，可以在https://www.latlong.net/获取，默认是广州的
  (setq calendar-latitude 23.130280
		calendar-longitude 113.288879)
  ;; sunrise 白天用的主题 sunset 晚上用的主题
  (setq circadian-themes '((:sunrise . doom-one-light)
                           (:sunset . doom-one)))

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
                (centaur-tabs-mode +1)
                (highlight-indent-guides-mode -1)
                (highlight-indent-guides-mode +1)))
  (circadian-setup))

;; 配置状态栏
(with-eval-after-load 'private-variable
  ;; 隐藏mode-line
  (use-package hide-mode-line
    :if is-need-awesome-tray
    :ensure t
    :hook (after-init . global-hide-mode-line-mode))
  ;; 懒猫的mode-line插件
  (use-package awesome-tray
    :if is-need-awesome-tray
    :after hide-mode-line
    :quelpa (awesome-tray
             :fetcher github
             :repo "manateelazycat/awesome-tray")
    :config
    (awesome-tray-mode 1)))


(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (dashboard-modify-heading-icons '((recents . "file-text") 
                                    (bookmarks . "book")))
  ;; 设置标题
  (setq dashboard-banner-logo-title
        "欢迎您使用此Emacs配置文件，有任何问题可加QQ群:46689842                  ")
  ;; 设置banner
  (setq dashboard-startup-banner "~/.emacs.d/var/banner/evan-emacs-banner.png")
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-navigator t)
  (add-hook 'after-init-hook (lambda () (dashboard-refresh-buffer))))
(progn
  (use-package all-the-icons
	:ensure t)
  ;; dired模式图标支持
  (use-package all-the-icons-dired
	:ensure t
	:hook ('dired-mode . 'all-the-icons-dired-mode))
  ;; 表情符号
  (use-package emojify
	:ensure t
	:custom (emojify-emojis-dir "~/.emacs.d/var/emojis"))
  ;; 浮动窗口支持
  (use-package posframe
	:ensure t
	:custom
	(posframe-mouse-banish nil)))

;; 竖线
(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode)
  :config
  (set-fontset-font "fontset-default"
                  (cons page-break-lines-char page-break-lines-char)
                  (face-attribute 'default :family))
  (let ((table (make-char-table nil)))                   ;; make a new empty table
    (set-char-table-parent table char-width-table)       ;; make it inherit from the current char-width-table
    (set-char-table-range table page-break-lines-char 1) ;; let the width of page-break-lines-char be 1
    (setq char-width-table table)))

;; 让info帮助信息中关键字有高亮
(use-package info-colors 
  :ensure t 
  :hook ('Info-selection-hook . 'info-colors-fontify-node))

;; 缩进线
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'bitmap))

;; 彩虹猫进度条
(use-package nyan-mode
  :if (not (boundp 'awesome-tray-mode))
  :ensure t
  :hook (after-init . nyan-mode)
  :config
  (setq nyan-wavy-trail t
		nyan-animate-nyancat t))

;; 对齐表格
(use-package valign
  :quelpa ((valign :fetcher github :repo "casouri/valign"))
  :hook ((org-mode markdown-mode) . valign-mode)
  :config
  (setq valign-fancy-bar nil))

;; 为上层提供 init-ui 模块
(provide 'init-ui)
