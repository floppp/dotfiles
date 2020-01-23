;;; package -- Summary

;;; Commentary:

;;; Code:
(require 'org)
(org-babel-load-file
 (if (eq system-type 'darwin)
     (expand-file-name "/Users/nando/workspace/dotfiles/macos-init.org" user-emacs-directory)
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
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("7b50dc95a32cadd584bda3f40577e135c392cd7fb286a468ba4236787d295f4b" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" default)))
 '(global-company-mode t)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (doom-modeline doom-themes lsp-treemacs zenburn-theme which-key web-mode vue-mode visual-regexp use-package undo-tree typo treemacs-projectile treemacs-icons-dired tide sr-speedbar spacemacs-theme spaceline smex rainbow-delimiters py-autopep8 poet-theme php-mode paredit-everywhere ox-gfm org-bullets nimbus-theme neotree monokai-theme lsp-ui langtool kibit-helper highlight-symbol highlight-parentheses flycheck-tip flycheck-joker flx find-file-in-project expand-region espresso-theme ensime emmet-mode elpy dumb-jump dashboard crux counsel-projectile company-lsp clojure-snippets clj-refactor centered-window ccls auctex all-the-icons aggressive-indent 4clojure)))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
