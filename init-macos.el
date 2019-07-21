;;; package --- Summary

;;; Commentary:
;;; Code:
(setq user-full-name "Fernando López")
(setq user-mail-address "fernandolopezlaso@gmail.com")
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Incluyo en el PATH tanto donde está el jdk, como donde miniconda, para que
;; pueda encontrar los paquetes allí instalados.
(setenv "PATH" (concat (getenv "PATH") ":/Users/nando/miniconda3/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/Users/nando/.local/bin"))
;; (setq exec-path (append exec-path '("/usr/local/bin/tern")))
(setq exec-path (append exec-path '("/Users/nando/miniconda3/bin")))
(setq exec-path (append exec-path '("/Users/nando/.local/bin"))) ; Esto me ha hecho que funcione el linting en elpy

(defvar gnu '("gnu" . "https://elpa.gnu.org/packages/"))
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

;; -------------------------------
;; FUNCIONES USADAS POSTERIORMENTE
;; -------------------------------
(defun packages-install (&rest packages)
  "Function to install those PACKAGES which aren't."
  (message "running packages-install")
  (mapc (lambda (package)
          (let ((name (car package))
                (repo (cdr package)))
            (when (not (package-installed-p name))
              (let ((package-archives (list repo)))
                (package-initialize)
                (package-install name)))))
        packages)
  (package-initialize)
  (delete-other-windows))

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

(defun init--install-packages ()
  "Instalación de paquetes no instalados."
  (message "Lets install some packages")
  (packages-install
   ;; Since use-package this is the only entry here
   ;; ALWAYS try to use use-package!
   (cons 'use-package melpa-stable)))

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

;; --------------------
;; CONFIGURACIÓN GLOBAL
;; --------------------
(desktop-save-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)
;; Cambiamos el comportamiento por defecto de la shell.
(remove-hook 'eshell-output-filter-functions
	     'eshell-postoutput-scroll-to-bottom)
(setq make-backup-files nil)
(setq inhibit-splash-screen t)
;; Scroll suave con el ratón
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; -----------
;; KEYBINDINGS
;; -----------
(global-set-key (kbd "C-?") 'er/expand-region)
(global-set-key (kbd "C-x k") 'bjm/kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-S-j") 'join-line)
(global-set-key (kbd "C-x f") 'flycheck-list-errors)
(global-set-key (kbd "C-x O") (lambda ()
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
;; Autocierre de paréntesis, llaves, corchetes, etc
(electric-pair-mode 1)

;; Atajos para ivy y todo lo relacionado.
(global-set-key "\C-s" 'swiper) ; de búsqueda normal a swiper
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)

;; ------------------
;; Modificaciones GUI
;; ------------------
;; El valor va en 1/10pt, así que 100 será 10pt...
(set-face-attribute 'default nil :height 140)
(setq visible-bell 1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(if window-system (scroll-bar-mode -1))
;; Ponemos la apariencia de spacemacs. Tienen que ir en este orden,
;; primero spaceline y luego spacemacs, sino se lían y el aspecto queda
;; horrible.
(use-package spaceline
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))

;; ------
;; Editor
;; ------
;; (setq-default fill-column 80)
;; (require 'whitespace)
;; (setq whitespace-line-column 79)
;; (setq whitespace-style '(face lines-tail))
;; (add-hook 'prog-mode-hook 'whitespace-mode)
(global-hl-line-mode 1)

;; -------------------
;; Variables Globlales
;; -------------------
;; (require 'ido)
;; (ido-mode t)
(global-display-line-numbers-mode 1)

;; Instalamos paquetes que faltan.
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; ----------------------------------------
;; Paquetes Generales (para más de un modo)
;; ---------------------------------------
;; ---
;; Ivy
;; ---
;; Configuración a partir de daemons.it, quitándole algunas cosas que no uso
;; actualmente (ya veré en un futuro) y modificando otras.
(unless (require 'ivy nil 'noerror)
  (sleep-for 5))

;; Ivy está formado por:
;;    - ivy: un mecanismo genérico de completado de emacs
;;    - counsel: varios comandos habituales de emacs mejorados con ivy
;;    - swiper: un isearch mejorado con ivy
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t) ;; Añade los buffers de bookmarks y de recentf
  (setq ivy-count-format "(%d/%d) ") ;; Muestra las coincidencias con lo que se escribe y la posicion en estas
  (setq ivy-height 15) ;; número de resultados a mostrar
  (setq ivy-on-del-error-function nil)   ;; No se sale del minibuffer si se encuentra un error
  (setq ivy-initial-inputs-alist nil)  ;; ivy mete el simbolo ^ al ejecutar algunas ordenes, así se quita
  (setq ivy-wrap t) ;; Dar la vuelta a los candidatos
  ;; (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))) ;; Que el uso de fuzzy regex se use en todo, no solo en counsel-find-file
  ;; (setq ivi-re-builders-alist '((t . ivi--regex-plus)))
  (setq ivy-re-builders-alist
	'((ivy-switch-buffer . ivy--regex-plus) ; plus por defecto
	  (read-file-name-internal . ivy--regex-plus)
	  (t . ivy--regex-fuzzy)))
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

  ;; ----------
  ;; Projectile
  ;; ----------

(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; --------
;; treemacs
;; --------
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

;; --------
;; lsp-mode
;; --------
(use-package lsp-mode
  :commands lsp
  :hook (sh-mode . lsp)) ;; Configuración para funcionar con BASH

;; Integración con otros paquetes
(use-package lsp-ui :commands lsp-ui-mode) ; flycheck y tips en popups
(use-package company-lsp :commands company-lsp)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; -------
;; Company
;; -------

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

;; -----
;; Otros
;; -----
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

(use-package ispell
  :config
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t))
  :bind (("C-c s" . spell-buffer-spanish)
         ("C-c e" . spell-buffer-english)))

(use-package highlight-parentheses
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode))))
(global-highlight-parentheses-mode)

;; -----------------------
;; Configuración lenguajes
;; -----------------------
;; Bash
;; Está definido ya en lsp-mode.

;; ---------------------
;; TypeScript/JavaScript
;; ---------------------
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ;;(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
  ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; Si uso use-package solo me carga tide en el prime archivo que abro
;;(use-package tide
;;  :ensure t
;;  :after (typescript-mode company flycheck)
;;  :bind (("M-." . tide-jump-to-definition)
;;         ("M-," . tide-jump-back))
;;  :config (setup-tide-mode)
;;  :hook ((typescript-mode . tide-setup)
;;         (typescript-mode . tide-hl-identifier-mode)))
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode 'electric-pair-mode)
(add-hook 'typescript-mode '(disable-tabs 2))
;;(add-hook 'js2-mode-hook #'setup-tide-mode)
(add-hook 'js-mode-hook #'setup-tide-mode)

;; --------
;; web-mode
;; --------
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s*css?\\'" . web-mode))
(setq web-mode-css-indent-offset 2)

;; Markdown
(use-package markdown-mode
	     :ensure t)

;; Python
(use-package elpy
  :ensure t)
(elpy-enable)

(setq ;;elpy-rpc-python-command "/Users/nando/miniconda3/bin/python"
      python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(setenv "WORKON_HOME" "/Users/nando/miniconda3/envs")
(pyvenv-mode 1)

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
(add-hook 'elpy-mode-hook 'electric-pair-mode)

;; -----
;; Scala
;; -----
;; Al usar versión estable, se instalan automáticamente tanto sbt-mode como scala-mode.
;; Si queremos versiones no estables, si hay que instalar ambos modos por separado.
(use-package ensime
  :ensure t
  :pin melpa-stable)
(add-hook 'ensime-mode 'electric-pair-mode)

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
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(compilation-message-face (quote default))
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(custom-enabled-themes (quote (leuven)))
 '(custom-safe-themes
   (quote
    ("7e7c9639e7b83c3271e427becc0336b85116cee201b11b7b8e9e9474c812633d" "84890723510d225c45aaff941a7e201606a48b973f0121cb9bcb0b9399be8cba" "5f27195e3f4b85ac50c1e2fac080f0dd6535440891c54fcfa62cdcefedf56b1b" default)))
 '(elpy-syntax-check-command "pylint")
 '(fci-rule-color "#3C3D37")
 '(global-company-mode t)
 '(global-display-line-numbers-mode nil)
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
 '(js-indent-level 2)
 '(line-number-mode nil)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (tide lsp-treemacs company-lsp lsp-ui lsp-mode php-mode treemacs-icons-dired treemacs-projectile treemacs counsel-projectile company-tern js2-mode web-mode labburn-theme zenburn-theme which-key use-package spaceline rainbow-delimiters py-autopep8 projectile paredit multiple-cursors monokai-theme markdown-mode highlight-parentheses flycheck expand-region ensime elpy clojure-snippets aggressive-indent)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#FFFACE")
 '(pos-tip-foreground-color "#272822")
 '(rainbow-identifiers-choose-face-function (quote rainbow-identifiers-cie-l*a*b*-choose-face) t)
 '(rainbow-identifiers-cie-l*a*b*-color-count 1024 t)
 '(rainbow-identifiers-cie-l*a*b*-lightness 80 t)
 '(rainbow-identifiers-cie-l*a*b*-saturation 25 t)
 '(tool-bar-mode nil)
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
 '(default ((t (:inherit nil :stipple nil :background "#FFFFFF" :foreground "#333333" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monaco")))))
