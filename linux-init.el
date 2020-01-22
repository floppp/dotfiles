(setq gc-cons-threshold 10000000)

(setq user-full-name "Fernando López")
(setq user-mail-address "fernandolopezlaso@gmail.com")
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

(setq exec-path (append exec-path '("~/.nvm/versions/node/v13.6.0/bin")))
(setq exec-path (append exec-path '("/home/nando/miniconda3/bin")))
(setq exec-path (append exec-path '("/home/nando/.local/bin")))

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
  (ispell-change-dictionary "es_ANY")
  (flyspell-buffer))

(defun spell-buffer-english ()
  "Buffer in english."
  (interactive)
  (ispell-change-dictionary "en_US")
  (flyspell-buffer))

(defun bjm/kill-this-buffer ()
  "Para matar el buffer actual."
  (interactive)
  (kill-buffer (current-buffer)))

(defun close-all-buffers ()
  "Para eliminar todos los buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; From https://dougie.io/emacs/indentation/
(defun disable-tabs (n)
  "Tabs desactivation with N spaces indentation."
  (setq indent-tabs-mode nil)
  (setq tab-width n))

(defun enable-tabs  (n)
  "Tabs activation instead of spaces, with N as tab width."
  (local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq indent-tabs-mode t)
  (setq tab-width n))

(defun kill-other-buffers ()
  "Para eliminar el resto de buffers salvo el activo."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))

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

(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-splash-screen t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq visible-bell nil)
(setq global-hl-line-mode t)
(setq-default show-trailing-whitespace t)
(global-auto-revert-mode t) ;; To refresh buffer in we change it in other editor.
(electric-pair-mode 1)                        ;; Autocierre de paréntesis, llaves, corchetes, etc
(set-face-attribute 'default nil :height 100) ;; El valor va en 1/10pt, así que 100 será 10pt...
(tool-bar-mode -1)
(menu-bar-mode -1)
(if window-system (scroll-bar-mode -1))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode 1)))
;; Cambiamos el comportamiento por defecto de la shell.
(remove-hook 'eshell-output-filter-functions
             'eshell-postoutput-scroll-to-bottom)
;; Para evitar problemas con MarkDown

(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil)

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

(setq langtool-java-classpath "/home/nando/Software/LanguageTool-4.6-stable/*"
      langtool-mother-tongue "es"
      langtool-default-language "es"
      ;; langtool-disabled-rules '("WHITESPACE_RULE"
                                ;; "EN_UNPAIRED_BRACKETS"
                                ;; "COMMA_PARENTHESIS_WHITESPACE"
                                ;; "EN_QUOTES")
)

;; Configuración a partir de daemons.it, quitándole algunas cosas que no uso
;; actualmente (ya veré en un futuro) y modificando otras.
(unless (require 'ivy nil 'noerror)
  (sleep-for 5))

(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t) ;; Añade los buffers de bookmarks y de recentf
  (setq ivy-count-format "(%d/%d) ") ;; Muestra las coincidencias con lo que se escribe y la posicion en estas
  (setq ivy-height 15) ;; número de resultados a mostrar
  (setq ivy-on-del-error-function nil) ;; No se sale del minibuffer si se encuentra un error
  (setq ivy-initial-inputs-alist nil) ;; ivy mete el simbolo ^ al ejecutar algunas ordenes, así se quita
  (setq ivy-wrap t) ;; Dar la vuelta a los candidatos
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ;; Que el uso de fuzzy regex se use en todo, no solo en counsel-find-file
  (setq ivy-re-builders-alist
        '(
          ;; (ivy-switch-buffer . ivy--regex-plus)
          ;; (read-file-name-internal . ivy--regex-plus)
          (t . ivy--regex-fuzzy)
          )
        )
  (setq ivy-virtual-abbreviate 'full) ;; Ver la ruta de los ficheros virtuales
  (setq ivy-use-selectable-prompt t) ;; Seleccionar el candidato actual (C-m en vez de C-S-m)

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
  ;; Quito por problema con elementary os.
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  ;; Aunque por defecto es el usado, por si acaso acabo usando también Emacs en Windows,
  ;; donde por defecto no se usa.
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-switch-project-action #'projectile-dired)
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
    ;;(treemacs-resize-icons 44)
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

;; Para que se usen sus iconos en dired.
(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package all-the-icons)
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq-default neo-show-hidden-files t)

;; Visual-regexp, allow to see regexp substitution in real-time when typing
(require 'visual-regexp)

;; highlight symbol. With mode active symbol at cursor is auto highlighted
(require 'highlight-symbol)

(use-package windmove
  :bind
  ("C-c <up>" . windmove-up)
  ("C-c <down>" . windmove-down)
  ("C-c <left>" . windmove-left)
  ("C-c <right>" . windmove-right))

;; Dashboard on emacs startup.
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq dashboard-items '((projects . 5)
                        (recents . 10)
                        (bookmarks . 5)))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :defer t)

(use-package highlight-parentheses
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode))))
(global-highlight-parentheses-mode)

;; original de linux
;;(use-package lsp-ui :commands lsp-ui-mode) ; flycheck y tips en popups
;;(use-package company-lsp :commands company-lsp)
;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; De mac, probar a ver
(use-package lsp-ui
  :requires lsp-mode flycheck
  :commands lsp-ui-mode
  :ensure t
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        ;;lsp-ui-peek-enable t
        ;;lsp-ui-peek-list-width 60
        ;;lsp-ui-peek-peek-height 25
        )
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'c-mode-hook #'lsp)) ; flycheck y tips en popups
(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :ensure t)

(use-package company-lsp
  :commands company-lsp
  :config (push 'company-lsp company-backends))

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

;;(require 'ccls)
(use-package ccls
  :hook ((c-mode c++-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))
(setq ccls-executable "/usr/bin/ccls")

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
  (company-mode +1))

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode 'electric-pair-mode)
(add-hook 'typescript-mode '(disable-tabs 2))
(add-hook 'js-mode-hook #'setup-tide-mode)

(use-package php-mode
  :mode
  (("\\.php\\'" . php-mode))
  :config
  (add-hook 'php-mode-hook
	    '(lambda ()
               ;; auto-complete
               ;; (require 'ac-php)
               ;; (define-key php-mode-map (kbd "M-]")
                 ;; 'ac-php-find-symbol-at-point)
               ;; (define-key php-mode-map (kbd "M-[")
                 ;; 'ac-php-location-stack)

               ;; company
               (require 'company-php)
	       (company-mode t)
	       ;; (add-to-list 'company-backends 'company-ac-php-backend)
               (ac-php-core-eldoc-setup)

               (set (make-local-variable 'company-backends)
                    '((company-ac-php-backend company-dabbrev-code) company-capf company-files))

               ;; Jump to definition (optional)
               (define-key php-mode-map (kbd "M-]") 'ac-php-find-symbol-at-point)

               ;; Return back (optional)
               (define-key php-mode-map (kbd "M-[") 'ac-php-location-stack-back)
               )
            )
  )

(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook 'emmet-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml?\\'" . web-mode))
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

(use-package clojure-snippets
  :ensure t)

(use-package cider
  :ensure t
  :pin melpa-stable
  :init
  (setq cider-lein-command "/home/nando/bin/lein")
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))

;;(use-package flycheck-clojure) ;; Mejor instalarlo a mano

;; me gustan kibit y eastwood, pero me dan problemas cada cierto tiempo (que no
;; sé arreglar) así que uso joker (que ya uso en sublime/vscode).
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

(setq ;;elpy-rpc-python-command "/home/nando/miniconda3/bin/python"
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(setenv "WORKON_HOME" "/home/nando/miniconda3/envs")
(pyvenv-mode 1)

(require 'py-autopep8)
;;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'elpy-mode-hook 'electric-pair-mode)

;;(add-hook 'text-mode-hook 'typo-mode)
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

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-j") 'join-line)
(global-set-key (kbd "C-x f") 'flycheck-list-errors)
(global-set-key (kbd "C-x C-g") 'delete-trailing-whitespace)
(global-unset-key (kbd "C-x o")) ;; Desconecto binding original para 'other-window'
(global-set-key (kbd "C-.") #'other-window)
(global-set-key (kbd "C-,") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one
(global-set-key (kbd "C-q") 'comment-line)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; Multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
;; Undo y Redo
;; (global-unset-key "C-z")
(global-set-key (kbd "C-z") 'advertised-undo)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-S-z") 'redo)
(define-key global-map [f4] 'toggle-truncate-lines)
(define-key global-map [f5] 'tool-bar-mode)
(define-key global-map [f6] 'menu-bar-mode)
(define-key global-map [f7] 'neotree-toggle)
(define-key global-map [f8] 'align-regexp)
(define-key global-map [f9] 'sort-lines)
(define-key global-map [f12] 'global-linum-mode)

;; Atajos para ivy y todo lo relacionado.
(global-set-key "\C-s" 'swiper) ; de búsqueda normal a swiper
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)

;; crux
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key (kbd "C-c n") #'crux-cleanup-buffer-or-region)
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key [(control shift return)] #'crux-smart-open-line-above)
(global-set-key (kbd "C-x 4 t") #'crux-transpose-windows)
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c i") #'crux-find-user-init-file)
(global-set-key (kbd "s-r") #'crux-recentf-find-file)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)

;; visual-regexp
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
