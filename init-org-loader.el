;;; package -- Summary

;;; Commentary:

;;; Code:
(require 'org)
(org-babel-load-file
 (if (eq system-type 'darwin)
     (expand-file-name "macos-init.org" user-emacs-directory)
   (expand-file-name "/home/nando/workspaces/dotfiles/linux-init.org" user-emacs-directory)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" default)))
 '(global-company-mode t)
 '(package-selected-packages
   (quote
    (lsp-treemacs zenburn-theme which-key web-mode vue-mode visual-regexp use-package undo-tree typo treemacs-projectile treemacs-icons-dired tide sr-speedbar spacemacs-theme spaceline smex rainbow-delimiters py-autopep8 poet-theme php-mode paredit-everywhere ox-gfm org-bullets nimbus-theme neotree monokai-theme lsp-ui langtool kibit-helper highlight-symbol highlight-parentheses flycheck-tip flycheck-joker flx find-file-in-project expand-region espresso-theme ensime emmet-mode elpy dumb-jump dashboard crux counsel-projectile company-lsp clojure-snippets clj-refactor centered-window ccls auctex all-the-icons aggressive-indent 4clojure))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
