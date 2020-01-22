;;; package -- Summary

;;; Commentary:

;;; Code:
(require 'org)
(org-babel-load-file
 (if (eq system-type 'darwin)
     (expand-file-name "macos-init.org" user-emacs-directory)
   (expand-file-name "linux-init.org" user-emacs-directory)))
