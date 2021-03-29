(load (expand-file-name "init.el"))

(setq +dump-exclude-packages '(vterm
                               fuz
                               emacs-application-framework))


(dolist (package package-activated-list)
  (unless (member package +dump-exclude-packages)
    (require 'doom-one-theme)
    (require 'doom-one-light-theme)
    (require package)))

(setq +dumped-load-path load-path)

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
