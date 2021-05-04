(define-prefix-command 'leader-key)
(which-key-add-key-based-replacements
  "M-SPC b" "音乐"
  "M-SPC o" "开启"
  "M-SPC t" "切换"
  "M-SPC w" "窗口"
  "M-SPC c" "代码"
  "M-SPC p" "项目"
  "M-SPC w P" "交换窗口-上"
  "M-SPC w N" "交换窗口-下"
  "M-SPC w F" "交换窗口-右"
  "M-SPC w B" "交换窗口-左")
(bind-key "t T" #'+evan/toggle-transparency leader-key)
(bind-key "t p" #'+evan/toggle-proxy leader-key)
(global-set-key (kbd "M-SPC") 'leader-key)
(global-set-key (kbd "C-(") 'backward-sexp) 
(global-set-key (kbd "C-)") 'forward-sexp)
(global-set-key (kbd "<f12>") 'open-init-dired)

(use-package
  hydra
  :defer 0
  :ensure t)

;; (push '(use-package
;; 		 hydra-posframe
;; 		 :defer 0
;; 		 :load-path "~/.emacs.d/site-lisp/hydra-posframe"
;; 		 :hook (after-init . hydra-posframe-mode)) graphic-only-plugins-setting)

(push '(progn
         (use-package
           hydra-posframe
           :quelpa (hydra-posframe
                     :fetcher github
                     :repo "Ladicle/hydra-posframe")
           :hook (after-init . (lambda ()
                                 (hydra-posframe-mode +1)))))
      graphic-only-plugins-setting)


(use-package
  major-mode-hydra
  :defer 0
  :ensure t
  :after hydra)

;; Bongo 音乐播放器
(defhydra evan/hydra-music-menu (:color blue)
  "
									^音乐^
[_RET_] ^播放曲目	[_i_] ^一键播放^		[_x_] ^删除曲目(区域)^	[_d_] ^删除曲目(行内)^
[_\__] ^撤回操作	[_SPC_] ^暂停/播放^		[_TAB_] ^收起专辑^		[_h_] ^回退10s^
[_l_] ^前进10s		[_a_] ^加入列表^		[_n_] ^下一首^			[_p_] ^上一首^
[_r_] ^随机播放 	[_k_] ^关闭播放器^
"
  ("RET" bongo-dwim nil)
  ("i" bongo-init-all nil)
  ("x" bongo-kill-region nil)
  ("d" bongo-kill-line nil) 
  ("_" bongo-undo nil)
  ("SPC" bongo-pause/resume nil) 
  ("TAB" bongo-toggle-collapsed nil) 
  ("h" bongo-seek-backward-10 nil :color red) 
  ("l" bongo-seek-forward-10 nil :color red) 
  ("a" bongo-insert-enqueue nil) 
  ("n" bongo-play-next nil)
  ("p" bongo-play-previous nil) 
  ("r" bongo-play-random nil)
  ("k" bongo-stop nil)
  ("q" nil "QUIT" :color blue))
;; 启动点er啥
(defhydra evan/hydra-open-menu ()
  "
	^启动点er啥^
[_t_] ^vterm^
"
  ("t" vterm nil :color blue)
  ("q" nil "QUIT" :color blue))
;; 开关
(defhydra evan/hydra-toggle-menu ()
  "
				^开关^
[_T_] ^透明^	[_p_] ^代理^	[_f_] ^FlyCheck^
"
  ("T" +evan/toggle-transparency nil)
  ("p" +evan/toggle-proxy nil)
  ("f" global-flycheck-mode nil)
  ("q" nil "QUIT" :color blue))
;; 窗格
(defhydra evan/hydra-window-menu ()
  "
							^窗口管理器^
[_0_] ^关闭窗格^			[_F_] ^全屏模式^		[_K_] ^↑+^		[_k_] ^go ↑^    
[_1_] ^关闭其他窗格^		[_r_] ^旋转交换^		[_J_] ^↓+^		[_j_] ^go ↓^ 
[_2_] ^新建窗格(垂直)^		[_s_] ^选择交换^		[_H_] ^←+^		[_h_] ^go ←^
[_3_] ^新建窗格(水平)^		[_b_] ^平均铺开^		[_L_] ^→+^		[_l_] ^go →^
"
  ("0" delete-window nil)
  ("1" delete-other-windows nil :color blue)
  ("2" split-window-vertically nil)
  ("3" split-window-horizontally nil)
  ("F" toggle-frame-fullscreen nil :color blue)
  ("r" rotate-window nil)
  ("s" ace-swap-window nil :color blue)
  ("b" balance-windows nil :color blue)
  ("H" shrink-window-horizontally nil)
  ("J" enlarge-window nil)
  ("K" shrink-window nil)
  ("L" enlarge-window-horizontally nil)
  ("h" windmove-left nil)
  ("j" windmove-down nil)
  ("k" windmove-up nil)
  ("l" windmove-right nil)
  ("q" nil "QUIT" :color blue))
;; 代码
(defhydra evan/hydra-code-menu ()
  "
				^代码^
[_r_] ^quickrun^	[_s_] company-yasnippet
"
  ("s" company-yasnippet nil :color blue)
  ("r" quickrun nil :color blue)
  ("q" nil "QUIT" :color blue))
;; 项目
(defhydra evan/hydra-project-menu ()
  ""
  ("q" nil "QUIT" :color blue))
;; 各种插件的键绑定
(pretty-hydra-define evan/hydra-app-menu (:color blue)
  (
   "EAF"
   (("e" evan/eaf-hydra/body "EAF"))
   "Telega"
   (("t" telega "启动Telega")
	("c" ivy-telega-chat-with "选择联系人")
	("n" (lambda () (message "占位"))))))

;; EAF键绑定
(pretty-hydra-define evan/eaf-hydra (:color blue)
  (
   "Emacs"
   (("s" eaf-search-it "立即搜索")
	("b" eaf-open-browser "打开网页")
	("h" eaf-open-browser-with-history "历史记录")
	("e" eaf-proxy-open_download_manage_page "下载管理")
	("m" eaf-open-bookmark "打开书签"))
  "Application"
  (("o" eaf-open "智能Open")
   ("c" eaf-open-camera "打开摄像")
   ("p" eaf-open-mindmap "思维导图(O)")
   ("l" eaf-create-mindmap "思维导图(N)")
   ("r" eaf-restart-process "刷新EAF"))
  "Framwork"
  (("t" eaf-open-terminal "打开终端")
   ("f" eaf-file-send-qrcode "隔空投送(F)")
   ("d" eaf-file-browser-qrcode "隔空投送(D)")
   ("i" eaf-open-airshare "隔空投送(S)")
   ("a" eaf-open-rss-reader "RSS阅读器"))))
;; 常用的命令
(defhydra evan/hydra-common-menu ()
  "
						^常用^
[_g_] ^counsel-rg^					[_y_] ^counsel-yank-pop^
[_f_] ^counsel-fzf^					[_d_] ^counsel-dired^	   
[_r_] ^counsel-recentf^				[_m_] ^counsel-bookmark^ 
[_b_] ^counsel-switch-buffer^		[_l_] ^counsel-linux-app^
[_i_] ^find-config-file-by-fzf^     [_=_] ^plus-backlight^
[_w_] ^capture-word^				[_-_] ^less-backlight^
[_c_] ^美女嘤嘤嘤^					[_2_] ^alpha-less^
[_1_] ^alpha-add^					[_I_] ^find-praivte-config-file-by-fzf
[_t_] ^TG选择联系人^
"
  ("g" counsel-rg nil :color blue)
  ("f" counsel-fzf nil :color blue)
  ("r" counsel-recentf nil :color blue)
  ("b" counsel-switch-buffer nil :color blue)
  ("y" counsel-yank-pop nil :color blue)
  ("d" counsel-dired nil :color blue)
  ("m" counsel-bookmark nil :color blue)
  ("l" counsel-linux-app nil :color blue)
  ("w" shengci-capture-word-and-save nil :color blue)
  ("i" +evan/find-config-file-by-fzf :color blue)
  ("I" +evan/find-private-config-file-by-fzf :color blue)
  ("-" +evan/less-backlight :color red)
  ("=" +evan/plus-backlight :color red)
  ("c" +evan/girl-say :color blue)
  ("1" +evan/plus-alpha nil :color red)
  ("2" +evan/less-alpha nil :color red)
  ("t" ivy-telega-chat-with nil :color blue)
  ("q" nil "QUIT" :color blue))

;; Tab bar menu
;; (defhydra evan/hydra-tab-bar-menu (:color blue)
;;   ""
;;   ("n" tab-bar-new-tab "new bar")
;;   ("x" tab-bar-close-tab "close bar")
;;   ("r" tab-bar-rename "rename bar")
;;   ("b" tab-bar-switch-to-tab "switch bar")
;;   ("l" tab-bar-switch-to-next-tab "next bar" :color red)
;;   ("h" tab-bar-switch-to-prev-tab "prev bar" :color red))

(defhydra evan/awesome-fast-switch (:hint nil)
     "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
     ("h" awesome-tab-backward-tab)
     ("j" awesome-tab-forward-group)
     ("k" awesome-tab-backward-group)
     ("l" awesome-tab-forward-tab)
     ("C-a" awesome-tab-select-beg-tab)
     ("C-e" awesome-tab-select-end-tab)
     ("C-j" awesome-tab-ace-jump)
     ("C-h" awesome-tab-move-current-tab-to-left)
     ("C-l" awesome-tab-move-current-tab-to-right)
     ("b" ivy-switch-buffer)
     ("g" awesome-tab-counsel-switch-group)
     ("C-k" kill-current-buffer)
     ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
     ("q" nil "quit"))

;; 主菜单
(defhydra evan/hydra-main-menu ()
  "
					^主菜单^
[_b_] ^音乐^	[_o_] ^开启^ 	[_t_] ^开关^ 	[_w_] ^窗格^
[_c_] ^代码^	[_p_] ^项目^ 	[_a_] ^应用^ 	[_2_] ^常用^
[_r_] ^Awesome-Tab^
"
  ;; [_T_] ^Tab-Bar^
  ("b" evan/hydra-music-menu/body nil :color blue)
  ("o" evan/hydra-open-menu/body nil :color blue)
  ("t" evan/hydra-toggle-menu/body nil :color blue)
  ("w" evan/hydra-window-menu/body nil :color blue)
  ("c" evan/hydra-code-menu/body nil :color blue)
  ("p" evan/hydra-project-menu/body nil :color blue)
  ("2" evan/hydra-common-menu/body nil :color blue)
  ("a" evan/hydra-app-menu/body nil :color blue)
  ("r" evan/awesome-fast-switch/body :color blue)
  ;; ("T" evan/hydra-tab-bar-menu/body nil :color blue)
  ("q" nil "QUIT" :color blue))

(global-set-key (kbd "M-\\") #'evan/hydra-main-menu/body)


(provide 'init-keybind)
