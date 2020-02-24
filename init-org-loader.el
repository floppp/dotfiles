;;; package -- Summary

;;; Commentary:

;;; Code:
(require 'org)
(org-babel-load-file
 (if (eq system-type 'darwin)
     (expand-file-name "/Users/nando/workspace/dotfiles/macos-init.org" user-emacs-directory)
   (expand-file-name "/home/nando/workspaces/dotfiles/linux-init.org" user-emacs-directory)))

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)
