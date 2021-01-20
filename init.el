;;; package -- Summary
;; Emacs configuration for C/C++, Python, Scala, Clojure, TS/JS, Web (+ React), Java.
;; - LSP for Scala (lsp-metals), C/C++ (ccls), Java (lsp-java).
;; - Elpy for python.
;; - Cider for Clojure.
;; - Tide for TypeScript/JavaScript.
;; - Plus some configurations for Org, Latex, MarkDown.
;; [TODO: Try built-in react support for Emacs 27.]
;; - Rxjs for React.



;;; Commentary:
;; Back to regular init.el file.  Literate programming, even only for small file configurations
;; like this one, is not for me.
;;
;; I'll be using my own configuration plus a lot from bbatsov.
;;
;; https://github.com/bbatsov/emacs.d/blob/master/init.el
;;
;; Some packages need to be installed manually:
;; - emmet
;; - sbt-mode
;; - scala-mode
;; - visual-regexp
;; - py-autopep8
;;
;; They can be installed in some automatic way, with functions that call to install them, but
;; actually i never feel they work (my implementation, modifications made to code found
;; somewhere) fine.  So i prefer installed them manually.  Only needs to be done once obviously.
;;
;; Preferred themes:
;; - light -> dichromacy and Emacs default
;; - dark  -> danneskjold


;;; License:
;; You can make whatever the fuck you want with this configuration file.


;;; Code:
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(defun load-macos-path ()
  "Loading some paths to be able to find executables."
  (setenv "JAVA_HOME" "/usr/local/opt/openjdk")
  (setq exec-path (append exec-path '("/Users/nando/.local/bin"))) ;; Esto me ha hecho que funcione el linting en elpy
  (setq exec-path (append exec-path '("/usr/local/bin")))
  (setq exec-path (append exec-path '("/Users/nando/bin"))))

(defun load-linux-path ()
  "Loading for home/work paths."
  (setq exec-path (append exec-path '("/home/fernando/miniconda3/bin")))
  (setq exec-path (append exec-path '("/home/fernando/.local/bin"))) ; Esto me ha hecho que funcione el linting en elpy
  (setq exec-path (append exec-path '("/home/fernando/.nvm/versions/node/v14.15.4/bin")))
  (setq exec-path (append exec-path '("/home/fernando/bin"))))

(if (eq system-type 'darwin)
    (load-macos-path)
  (load-linux-path))


;; ==== GLOBAL CONFIGURATION ====

(setq user-full-name "Fernando López")
(setq load-prefer-newer t)  ;; Always load newest byte code
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))
(setq gc-cons-threshold 50000000)  ;; reduce the frequency of garbage collection
(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq visible-bell nil)
(setq require-final-newline t)
(setq doc-view-continuous t) ;; Changing docview to move to next page begin from previous page.
;; Para que si tengo dos paneles abiertos, al copiar en uno, se copie directamente en el otro.
(setq dired-dwim-target t)
(setq tab-always-indent 'complete)  ;; smart tab behavior - indent or complete
;; (setq create-lockfiles nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 80)
(setq-default show-trailing-whitespace t)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(set-face-attribute 'default nil :family "Monaco")
(set-face-attribute 'fixed-pitch nil :family "Monaco")
(set-face-attribute 'variable-pitch nil :family "Go Mono")

(global-auto-revert-mode t) ;; To refresh buffer in we change it in other editor.
(global-hl-line-mode +1)
(electric-pair-mode 1) ;; Autocierre de paréntesis, llaves, corchetes, etc
(set-face-attribute 'default nil :height 120) ;; El valor va en 1/10pt, así que 100 será 10pt...
(delete-selection-mode t)
(set-fringe-style 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(add-to-list 'initial-frame-alist '(fullscreen . maximized)) ;; maximize the initial frame automatically

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Enable defer and ensure by default for use-package
;; Keep auto-save/backup files separate from source code:  https://github.com/scalameta/metals/issues/1027
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;;; functions
(defun close-all-buffers ()
  "Para eliminar todos los buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-this-buffer ()
  "Para matar el buffer actual."
  (interactive)
  (kill-buffer (current-buffer)))

;; Those keybindings I couldn't attach in any other place.
(global-set-key (kbd "C-x \\") #'align-regexp)
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)
(global-set-key (kbd "C-q") 'comment-line)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x f") 'flycheck-list-errors)
(global-set-key (kbd "C-x C-g") 'delete-trailing-whitespace)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(define-key global-map [f4] 'toggle-truncate-lines)
(define-key global-map [f5] 'tool-bar-mode)
(define-key global-map [f6] 'menu-bar-mode)
(define-key global-map [f9] 'sort-lines)
(define-key global-map [f11] 'global-linum-mode)
(define-key global-map [f12] 'global-linum-mode)


;;; built-in packages

(use-package paren
  :config
  (show-paren-mode +1))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

;; highlight the current line
(use-package hl-line
  :config
  (global-hl-line-mode +1))

;; To define snippets easily. Nice, try to use.
(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

;; To make buffer names to give more information than default behaviour.
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers ;; (windmove-default-keybindings)
  )
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))


;;; third-party packages
(require 'highlight-symbol)

(require 'visual-regexp)
(define-key global-map (kbd "C-c w") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(use-package ag
  :ensure t)

(unless (require 'ivy nil 'noerror)
  (sleep-for 5))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)     ;; Añade los buffers de bookmarks y de recentf
  (setq ivy-count-format "(%d/%d) ")   ;; Muestra las coincidencias con lo que se escribe y la posicion en estas
  (setq ivy-height 15)                 ;; número de resultados a mostrar
  (setq ivy-on-del-error-function nil) ;; No se sale del minibuffer si se encuentra un error
  (setq ivy-initial-inputs-alist nil)  ;; ivy mete el simbolo ^ al ejecutar algunas ordenes, así se quita
  (setq ivy-wrap t)                    ;; Dar la vuelta a los candidatos
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ;; Que el uso de fuzzy regex se use en todo, no solo en counsel-find-file
  ;; (setq ivi-re-builders-alist '((t . ivi--regex-plus)))
  (setq ivy-re-builders-alist
        '(
          ;; (ivy-switch-buffer . ivy--regex-plus) ; plus por defecto
          ;; (read-file-name-internal . ivy--regex-plus)
          (t . ivy--regex-fuzzy)
          ))
  (setq ivy-virtual-abbreviate 'full) ;; Ver la ruta de los ficheros virtuales
  (setq ivy-use-selectable-prompt t)  ;; Seleccionar el candidato actual (C-m en vez de C-S-m)

  ;; Asegurarse de que están smex, flx
  (use-package smex :ensure t)
  (use-package flx :ensure t)

  :config (ivy-mode 1)
  :config (counsel-mode 1)
  :diminish ivy-mode
  :ensure t)

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))


(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  ;; (setq projectile-project-search-path '("~/workspace/projects/"))
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this))

(use-package aggressive-indent
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package highlight-parentheses
  :ensure t)
(global-highlight-parentheses-mode)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; for moving line or region if selected.
(use-package move-text
  :ensure t
  :bind
  (([(meta up)] . move-text-up)
   ([(meta down)] . move-text-down)))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-c f" . crux-recentf-find-file)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-r" . crux-recentf-find-file)
         ("C-S-j" . crux-top-join-line)
         ("C-S-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
                                        ; Aquí van las opciones. Dejo una para saber dónde ponerlas.
    (setq treemacs-file-event-delay 4000)
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))


(use-package lsp-mode
  :hook
  (sh-mode . lsp) ;; (c-mode . lsp)
  (c++-mode . lsp)
  (java-mode .lsp)
  ;; (scala-mode . lsp)
  (lsp-mode . lsp-lens-mode)
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")))

(use-package lsp-ui
  :requires lsp-mode flycheck
  :commands lsp-ui-mode
  :ensure t
  :config
  (setq lsp-ui-doc-enable t
       lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)) ; flycheck y tips en popups

(use-package lsp-treemacs :commands lsp-treemacs-errors-list :ensure t)

(global-set-key (kbd "C-c C-c u") 'lsp-ui-imenu)
(global-set-key (kbd "C-c C-c d") 'lsp-ui-doc-show)

;;; dap

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  )
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))


;;; clojure

(use-package clojure-mode
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package flycheck-joker
  :ensure t)


;;; python

(use-package elpy
  :ensure t)
(elpy-enable)
(setq ;;elpy-rpc-python-command "/Users/nando/miniconda3/bin/python"
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i --simple-prompt")
(setq elpy-rpc-timeout 10)
(if (eq system-type 'darwin)
        (setenv "WORKON_HOME" "/Users/nando/miniconda3/envs")
        (setenv "WORKON_HOME" "/home/fernando/miniconda3/envs"))
(pyvenv-mode 1)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'elpy-mode-hook 'electric-pair-mode)


;;; java

(use-package lsp-java :ensure t)
(use-package dap-java :after (lsp-java))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  :config
  (dap-mode t)
  (dap-ui-mode t))

;;; scala

;; https://scalameta.org/metals/docs/editors/emacs.html

;; Enable scala-mode for highlighting, indentation and motion commands
(defun load-scala-linux ()
  "Loading lsp-metals for scala development.  I don't know why, I'm not able to make it work in macos."
  (use-package lsp-metals)
  (add-hook 'scala-mode-hook #'lsp)
  (global-set-key (kbd "C-c C-c i") 'lsp-metals-build-import)
  (use-package company-lsp
    :commands company-lsp
    :config (push 'company-lsp company-backends)
    :ensure t))

(defun load-scala-macos ()
  "Loading eglot.  I prefer lsp-metals, but eglot works well too."
  (use-package eglot
    :config
    (add-to-list 'eglot-server-programs '(scala-mode . ("metals-emacs")))
    :hook (scala-mode . eglot-ensure)))

(use-package scala-mode
  :mode "\\.s\\(c\\|cala\\|bt\\)$"
  :interpreter
  ("amm" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(if (eq system-type 'darwin)
    (load-scala-macos)
  (load-scala-linux))

(use-package yasnippet)

;;; c/c++

;; (require 'ccls)
(use-package google-c-style
  :ensure t
  :defer t
  :commands
  (google-set-c-style))

(use-package clang-format
  :ensure t
  :defer t
  :commands
  (clang-format clang-format-buffer))

(use-package ccls
  :ensure t
  :custom
  (indent-tabs-mode nil)
  (tab-width 2)
  (c-basic-offset 2))

(setq ccls-executable (if (eq system-type 'darwin)
                          "/usr/local/bin/ccls"
                        "/usr/bin/ccls"))

;; Rjsx-mode
(use-package rjsx-mode
  :ensure t)
(setq rjsx-indent-level 2)
;;; TypeScript/JavaScript
(defun setup-tide-mode ()
  "Función que nos lanza el modo y lo configura.
No uso use-package, porque si lo hago así,
solamente carga el modo para el primer archivo."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ;;(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  ;; Configuración extra desde ... https://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh
  ;; (run-import-js)  ;; funcíon que no tengo
  ;; Estos setq los meto después (algunos), quito de aquí para no mezclar
  ;; (setq web-mode-enable-auto-quoting nil)
  ;; (setq web-mode-markup-indent-offset 2)
  ;; (setq web-mode-code-indent-offset 2)
  ;; (setq web-mode-attr-indent-offset 2)
  ;; (setq web-mode-attr-value-indent-offset 2)
  ;; (setq lsp-eslint-server-command '("node" (concat (getenv "HOME") "/var/src/vscode-eslint/server/out/eslintServer.js") "--stdio"))
  (set (make-local-variable 'company-backends)
       '((company-tide company-files :with company-yasnippet)
         (company-dabbrev-code company-dabbrev)))
  )

(use-package tide
  :ensure t
  ;; Comento porque ya hago en el add-hook de después
  ;; :after (rjsx-mode company flycheck)
  ;; :hook (rjsx-mode . setup-tide-mode)
  )

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode 'electric-pair-mode)
;; (add-hook 'typescript-mode '(disable-tabs 2))
;; https://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh
;; use rjsx-mode for .js* files except json and use tide with rjsx
;; (add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))  ;; Esto en ppio es automaico para rjsx-mode
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'rjsx-mode-hook 'setup-tide-mode)

;; JavaScript
(add-hook 'js-mode-hook #'setup-tide-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)

;;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'rjsx-hook 'emmet-mode)

;;; Web-mode
;; https://fransiska.github.io/emacs/2017/08/21/web-development-in-emacs
(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (set (make-local-variable 'company-backends)
       '(company-css company-web-html company-yasnippet company-files)))

;; Para hacerlo funcionar con typescritp/react tsx
;; https://dev.to/viglioni/how-i-set-up-my-emacs-for-typescript-3eeh
(flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'web-mode-hook 'company-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'custom-web-mode-hook)


(add-hook 'web-mode-hook 'setup-tide-mode
          (lambda () (pcase (file-name-extension buffer-file-name)
                       ("tsx" ('tide-setup-hook))
                       (_ (my-web-mode-hook)))))

(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s*css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.ts?\\'" . typescript-mode))


;;; Latex, org, markdown
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))
(setq org-hide-emphasis-markers t)
(setq org-bullets-bullet-list '("◉" "○"))
(setq org-fontify-whole-heading-line t)

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (org-indent-mode t)))

(use-package markdown-mode
  :ensure t)

(setq ispell-really-hunspell t)
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "es")
(setq ispell-local-dictionary-alist
      '(("es" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(use-package ispell
  :hook
  ((text-mode . flyspell-mode))
  :config
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t))
  :bind (("C-c s" . spell-buffer-spanish)
         ("C-c e" . spell-buffer-english)))

;; TODO: download langtool
(setq langtool-java-classpath
      (if (eq system-type 'darwin)
          "/usr/local/bin/languagetool:/Users/nando/Downloads/LanguageTool-4.6-stable/*"
        "/home/nando/Software/LanguageTool-4.6-stable/*")
      langtool-mother-tongue "es"
      langtool-default-language "es"
      ;; langtool-disabled-rules '("WHITESPACE_RULE"
      ;; "EN_UNPAIRED_BRACKETS"
      ;; "COMMA_PARENTHESIS_WHITESPACE"
      ;; "EN_QUOTES")
      )


;;; others

(use-package company
  :ensure t
  :config
  (setq company-idle-delay .5)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 10)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))


(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
         ("s-i" . imenu-anywhere)))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

;; Do not show some common modes in the modeline, to save space
;; https://people.gnome.org/~federico/blog/bringing-my-emacs-from-the-past.html
(use-package diminish
  :defer 5
  :config
  (diminish 'org-indent-mode))

(use-package buffer-move
  :config
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right))

;; (setq custom-file "~/.emacs.d/emacs-custom.el")
;; (load custom-file)
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
