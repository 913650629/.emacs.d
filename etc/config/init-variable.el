;;; 配置内一些可以修改的变量，这些变量请在private模块中重新修改。

(setq
 ;; 字体相关
 ;; 英文字体
 evan/en-font-name "Sarasa Mono SC"
 evan/en-font-style "Regular"
 evan/en-font-size 20
 ;; 中文字体
 evan/zh-font-name "Sarasa Mono SC"
 evan/zh-font-style "Regular"
 evan/zh-font-size 18

 ;; 代理相关
 evan/proxy-type "socks5"
 evan/proxy-host "127.0.0.1"
 evan/proxy-port "1088"

 ;; Emacs原生设置
 
 shell-file-name "/usr/bin/zsh"
 ;; custom-set-variables 的那些数据存放位置
 custom-file "~/.emacs.d/etc/custom.el"
 ;; 最近打开文件最大保存数
 recentf-max-saved-items 150

 ;; EAF相关
 ;; eaf grip 所需token值
 evan/eaf-grip-token nil
 ;; 是否需要启动meow模式编辑模块，如果需要则设置为t
 is-need-meow-module nil
 ;; 是否需要启用awesome-tray
 is-need-awesome-tray nil
 ;; 是否需要启用awesome-tab
 is-need-awesome-tab nil
 ;; 用于恢复字体大小的变量
 original-zh-font-size evan/zh-font-size
 original-en-font-size evan/en-font-size
 )

(provide 'init-variable)
