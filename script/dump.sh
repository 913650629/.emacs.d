#!/usr/bin/bash
chmod 644 $HOME/.emacs.d/emacs.pdmp && LC_TYPE=en_US.UTF-8 emacs --batch -q -l $HOME/.emacs.d/dump.el && chmod 444 $HOME/.emacs.d/emacs.pdmp
