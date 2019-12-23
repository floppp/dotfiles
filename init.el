<<<<<<< HEAD
(setq gc-cons-threshold 10000000)

=======
;;; package --- Summary
;; Configuración de Emacs para programar con:
;;   + Clojure    -> Cider
;;   + Python     -> Elpy
;;   + Typescript -> Tide
;;   + Latex      -> Auctex
;;   + Scala      -> Metals

;;; Code:
>>>>>>> 9ca5b2d0918bc490cb70b916e512dd2de34c6781
(setq user-full-name "Fernando López")
(setq user-mail-address "fernandolopezlaso@gmail.com")
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

(setenv "PATH" (concat (getenv "PATH") ":/Users/nando/miniconda3/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/Users/nando/.local/bin"))
(setenv "PAHT" (concat (getenv "PATH") ":/Users/nando/bin"))
(setq exec-path (append exec-path '("/Users/nando/miniconda3/bin")))
(setq exec-path (append exec-path '("/Users/nando/.local/bin"))) ; Esto me ha hecho que funcione el linting en elpy
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq exec-path (append exec-path '("/Users/nando/bin")))

(defvar gnu '("gnu" . "http://mirrors.163.com/elpa/gnu/"))
(defvar melpa '("melpa" . "https://melpa.org/packages/"))
(defvar melpa-stable '("melpa-stable" . "https://stable.melpa.org/packages/"))
(defvar org-elpa '("org" . "http://orgmode.org/elpa/"))
(setq package-archives nil)
(add-to-list 'package-archives melpa-stable t)
(add-to-list 'package-archives melpa t)
(add-to-list 'package-archives gnu t)
(add-to-list 'package-archives org-elpa t)
(package-initialize)
(unless (and (file-exists-p (concat init-dir "elpa/archives/gnu"))
             (file-exists-p (concat init-dir "elpa/archives/melpa"))
             (file-exists-p (concat init-dir "elpa/archives/melpa-stable")))
  (package-refresh-contents))

(defun spell-buffer-spanish ()
  "Buffer in spanish."
  (interactive)
  (ispell-change-dictionary "es")
  (flyspell-buffer))

(defun spell-buffer-english ()
  "Buffer in english."
  (interactive)
  (ispell-change-dictionary "en_US")
  (flyspell-buffer))

(defun close-all-buffers ()
  "Para eliminar todos los buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-other-buffers ()
  "Para eliminar el resto de buffers salvo el activo."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

(defun bjm/kill-this-buffer ()
  "Para matar el buffer actual."
  (interactive)
  (kill-buffer (current-buffer)))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun ido-remove-tramp-from-cache nil
    "Remove any TRAMP entries from `ido-dir-file-cache'.
    This stops tramp from trying to connect to remote hosts on emacs startup,
    which can be very annoying."
    (interactive)
    (setq ido-dir-file-cache
	  (cl-remove-if
	   (lambda (x)
	     (string-match "/\\(rsh\\|ssh\\|telnet\\|su\\|sudo\\|sshx\\|krlogin\\|ksu\\|rcp\\|scp\\|rsync\\|scpx\\|fcp\\|nc\\|ftp\\|smb\\|adb\\):" (car x)))
	   ido-dir-file-cache)))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))
#+END_SRC emacs-lisp

Some common clean setup configuration.
#+BEGIN_SRC emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(global-auto-revert-mode t) ;; To refresh buffer in we change it in other editor.

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))
#+END_SRC emacs-lisp

Some common clean setup configuration.
#+BEGIN_SRC emacs-lisp
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq inhibit-splash-screen t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(global-auto-revert-mode t) ;; To refresh buffer in we change it in other editor.

(electric-pair-mode 1)                        ;; Autocierre de paréntesis, llaves, corchetes, etc
(set-face-attribute 'default nil :height 120) ;; El valor va en 1/10pt, así que 100 será 10pt...
(setq visible-bell nil)
(tool-bar-mode -1)
(menu-bar-mode 1)
(if window-system (scroll-bar-mode -1))

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-j") 'join-line)
(global-set-key (kbd "C-x f") 'flycheck-list-errors)
(global-set-key (kbd "C-x C-g") 'delete-trailing-whitespace)
(global-unset-key (kbd "C-x o")) ;; Desconecto binding original para 'other-window'
(global-set-key (kbd "C-,") #'other-window)
(global-set-key (kbd "C-.") (lambda ()
                              (interactive)
                              (other-window -1)))
(global-set-key (kbd "C-q") 'comment-line)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; mc -> multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
;; undo and redo
(global-set-key (kbd "C-z") 'advertised-undo)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)
(define-key global-map [f4] 'toggle-truncate-lines)
(define-key global-map [f5] 'tool-bar-mode)
(define-key global-map [f6] 'menu-bar-mode)
(define-key global-map [f8] 'align-regexp)
(define-key global-map [f9] 'sort-lines)
(global-set-key (kbd "<f11>") 'global-linum-mode)
;; ivy's shortcuts
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)

(setq ispell-really-hunspell t)
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "es")
(setq ispell-local-dictionary-alist
      '(("es" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(use-package ispell
  :config
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t))
  :bind (("C-c s" . spell-buffer-spanish)
         ("C-c e" . spell-buffer-english)))

(setq langtool-java-classpath "/usr/loca/bin/languagetool:/Users/nando/Downloads/LanguageTool-4.6-stable/*"
      langtool-mother-tongue "es"
      langtool-default-language "es"
      ;; langtool-disabled-rules '("WHITESPACE_RULE"
                                ;; "EN_UNPAIRED_BRACKETS"
                                ;; "COMMA_PARENTHESIS_WHITESPACE"
                                ;; "EN_QUOTES"))

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
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ;; Que el uso de fuzzy regex se use en todo, no solo en counsel-find-file
  ;; (setq ivi-re-builders-alist '((t . ivi--regex-plus)))
  (setq ivy-re-builders-alist
        '((ivy-switch-buffer . ivy--regex-plus) ; plus por defecto
          (read-file-name-internal . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-virtual-abbreviate 'full) ;; Ver la ruta de los ficheros virtuales
  (setq ivy-use-selectable-prompt t)  ;; Seleccionar el candidato actual (C-m en vez de C-S-m)

  ;; Asegurarse de que están smex, flx
  (use-package smex :ensure t)
  (use-package flx :ensure t)
  :config (ivy-mode 1)
  :config (counsel-mode 1)
  :diminish ivy-mode
  :ensure t)

(use-package counsel
  :config
  (setq counsel-find-file-at-point t)
  :ensure t)

(use-package swiper
  :ensure t)

(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

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

(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region)
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key [(control shift return)] #'crux-smart-open-line-above)
(global-set-key (kbd "C-x 4 t") #'crux-transpose-windows)
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c i") #'crux-find-user-init-file)
(global-set-key (kbd "s-r") #'crux-recentf-find-file)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)

(require 'sr-speedbar)

(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
;; if you use multiple-cursors, this is for you:
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

(use-package highlight-parentheses
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(global-highlight-parentheses-mode)

(use-package company
  :defer 0.5
  :delight
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :commands lsp
  ;;:hook (sh-mode . lsp)) ;; Configuración para funcionar con BASH
  :init
  (setq lsp-enable-indentation nil)
  (add-hook 'sh-mode #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  :config
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) over flymake.
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
)

;; Integración con otros paquetes
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
(use-package company-lsp
  :commands company-lsp
  :config (push 'company-lsp company-backends))

(defun setup-tide-mode ()
  "Función que nos lanza el modo y lo configura.
No uso use-package, porque si lo hago así,
solamente carga el modo para el primer archivo."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ;;(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode 'electric-pair-mode)
(add-hook 'typescript-mode '(disable-tabs 2))
(add-hook 'js-mode-hook #'setup-tide-mode)

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s*css?\\'" . web-mode))
;; https://fransiska.github.io/emacs/2017/08/21/web-development-in-emacs
(defun custom-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (set (make-local-variable 'company-backends)
       '(company-css company-web-html company-yasnippet company-files)))
(add-hook 'web-mode-hook 'custom-web-mode-hook)
(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(add-to-list 'auto-mode-alist '("\\.vue?\\'" . vue-mode))

(add-hook 'text-mode-hook
               (lambda ()
                 (variable-pitch-mode 1)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(set-face-attribute 'default nil :family "Monaco")
(set-face-attribute 'fixed-pitch nil :family "Monaco")
(set-face-attribute 'variable-pitch nil :family "Go Mono")

(setq org-hide-emphasis-markers t)
(setq org-bullets-bullet-list
      '("◉" "○"))
(setq org-fontify-whole-heading-line t)
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (org-indent-mode t)))

(use-package markdown-mode
  :ensure t)

(use-package clojure-snippets
  :ensure t)
(use-package cider
  :ensure t
  :pin melpa-stable
  :init
  (setq cider-lein-command "/usr/local/bin/lein")
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))

;;(use-package flycheck-clojure) ;; Mejor instalarlo a mano

;; me gustan kibit y eastwood, pero me dan problemas cada cierto tiempo (que no
;; sé arreglar) así que uso joker
(require 'flycheck-joker)
(require 'flycheck-tip)
(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 ;; insert keybinding setup here
                                 ))
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (setq cljr-warn-on-eval nil))

(use-package elpy
  :ensure t)
(elpy-enable)
(setq ;;elpy-rpc-python-command "/Users/nando/miniconda3/bin/python"
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(setq elpy-rpc-timeout 10)
(setenv "WORKON_HOME" "/Users/nando/miniconda3/envs")
(pyvenv-mode 1)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'elpy-mode-hook 'electric-pair-mode)
<<<<<<< HEAD
=======

;; -----
;; Scala
;; -----


;; ------------------------------------
;; Escritura: Org-mode, MarkDown, Latex
;; ------------------------------------

(add-hook 'text-mode-hook 'typo-mode)
(add-hook 'text-mode-hook
               (lambda ()
		 (variable-pitch-mode 1)))

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . light))

(set-face-attribute 'default nil :family "Source Code Pro") ;; :height 100
(set-face-attribute 'fixed-pitch nil :family "Source Code Pro");;  :height 100
(set-face-attribute 'variable-pitch nil :family "Go Mono")

;; --------
;; Org-mode
;; --------
(setq org-hide-emphasis-markers t)
(setq org-bullets-bullet-list '("◉" "○"))
(setq org-fontify-whole-heading-line t)
(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode 1)
            (org-indent-mode t)))

;; --------
;; Markdown
;; --------
(use-package markdown-mode
	     :ensure t)

;; -----
;; LaTex
;; -----
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;; ------------------------------------------
;; Configuración incluida por emacs, no tocar
;; ------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(custom-enabled-themes (quote (espresso)))
 '(custom-safe-themes
   (quote
    ("1a1cdd9b407ceb299b73e4afd1b63d01bbf2e056ec47a9d95901f4198a0d2428" "9129c2759b8ba8e8396fe92535449de3e7ba61fd34569a488dd64e80f5041c9f" "c82d24bfba431e8104219bfd8e90d47f1ad6b80a504a7900cbee002a8f04392f" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "f92f181467b003a06c3aa12047428682ba5abe4b45e0fca9518496b9403cde6f" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(dumb-jump-mode t)
 '(elpy-syntax-check-command "pylint")
 '(fci-rule-color "#383838")
 '(global-company-mode t)
 '(global-linum-mode nil)
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(line-number-mode nil)
 '(linum-format " %7i ")
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (dashboard highlight-symbol visual-regexp crux sr-speedbar typo org-bullets espresso-theme auctex emmet-mode centered-window company-lsp lsp-ui lsp-mode php-mode counsel-projectile swiper counsel undo-tree dumb-jump web-mode ensime tide projectile spacemacs-theme zenburn-theme nimbus-theme flycheck-joker kibit-helper spaceline py-autopep8 4clojure expand-region centered-window-mode flycheck clj-refactor cider clojure-snippets yasnippet rainbow-delimiters highlight-parentheses paredit-everywhere paredit markdown-mode which-key use-package)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(sublimity-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))

(provide 'init)
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight normal :height 98 :width normal)))))
>>>>>>> 9ca5b2d0918bc490cb70b916e512dd2de34c6781
