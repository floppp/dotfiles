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
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("6973f93f55e4a6ef99aa34e10cd476bc59e2f0c192b46ec00032fe5771afd9ad" "1a1cdd9b407ceb299b73e4afd1b63d01bbf2e056ec47a9d95901f4198a0d2428" "7b50dc95a32cadd584bda3f40577e135c392cd7fb286a468ba4236787d295f4b" "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" default)))
 '(fci-rule-color "#3C3D37")
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
 '(jdee-db-active-breakpoint-face-colors (cons "#171F24" "#237AD3"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#171F24" "#579C4C"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#171F24" "#777778"))
 '(magit-diff-use-overlays nil)
 '(objed-cursor-color "#D16969")
 '(package-selected-packages
   (quote
    (doom-modeline doom-themes lsp-treemacs zenburn-theme which-key web-mode vue-mode visual-regexp use-package undo-tree typo treemacs-projectile treemacs-icons-dired tide sr-speedbar spacemacs-theme spaceline smex rainbow-delimiters py-autopep8 poet-theme php-mode paredit-everywhere ox-gfm org-bullets nimbus-theme neotree monokai-theme lsp-ui langtool kibit-helper highlight-symbol highlight-parentheses flycheck-tip flycheck-joker flx find-file-in-project expand-region espresso-theme ensime emmet-mode elpy dumb-jump dashboard crux counsel-projectile company-lsp clojure-snippets clj-refactor centered-window ccls auctex all-the-icons aggressive-indent 4clojure)))
 '(pdf-view-midnight-colors (cons "#d4d4d4" "#1e1e1e"))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(rustic-ansi-faces
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D" "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
