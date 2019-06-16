;;; package --- Summary
;; Configuración de Emacs para programar con:
;;   + Clojure    -> Cider
;;   + Python     -> Elpy
;;   + Typescript -> Tide
;;   + Latex      -> Auctex
;;   + Scala      -> Ensime

;;; Commentary:
(setq user-full-name "Fernando López")
(setq user-mail-address "fernandolopezlaso@gmail.com")

;; Incluyo en el PATH tanto donde está el jdk, como donde miniconda, para que
;; pueda encontrar los paquetes allí instalados.
(setenv "PATH" (concat (getenv "PATH") ":/home/nando/.sdkman/candidates/java/current/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/home/nando/miniconda3/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/home/nando/.sdkman/candidates/scala/current/bin"))
(setenv "PATH" (concat (getenv "PATH") ":/home/nando/.sdkman/candidates/sbt/current/bin"))
(setq exec-path (append exec-path '("/home/nando/miniconda3/bin")))

;;(require 'package)

;;; Code:
(setq init-dir (file-name-directory (or load-file-name (buffer-file-name))))

;; Repositorios donde buscar paquetes.
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

;; (defun spell-buffer-spanish ()
;;   "Buffer in spanish."
;;   (interactive)
;;   (ispell-change-dictionary "es")
;;   (flyspell-buffer))

;; (defun spell-buffer-english ()
;;   "Buffer in english."
;;   (interactive)
;;   (ispell-change-dictionary "en_US")
;;   (flyspell-buffer))

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
(global-set-key (kbd "C-=") 'er/expand-region)
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
(set-face-attribute 'default nil :height 100)
(setq visible-bell 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
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

(use-package spacemacs-theme
  :defer t
  ;; :ensure t
  :init
  (load-theme 'spacemacs-dark t))
(global-prettify-symbols-mode 1)

;; ------
;; Editor
;; ------
(setq-default fill-column 80)
(global-hl-line-mode 1)
(require 'whitespace)
(setq whitespace-line-column 79)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; -------------------
;; variables Globlales
;; -------------------
(require 'ido)
(ido-mode t)
(global-display-line-numbers-mode 1)

;; Instalamos paquetes que faltan.
(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; =======================================----------------------------------------
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
  ;; Añade los buffers de bookmarks y de recentf
  (setq ivy-use-virtual-buffers t)
  ;; Muestra las coincidencias con lo que se escribe y la posicion en estas
  (setq ivy-count-format "(%d/%d) ")
  ;; Un mejor buscador
  (setq ivy-re-builders-alist
	'((read-file-name-internal . ivy--regex-fuzzy)
	  (t . ivy--regex-plus)))
  ;; número de resultados a mostrar
  (setq ivy-height 15)
  ;; No se sale del minibuffer si se encuentra un error
  (setq ivy-on-del-error-function nil)
  ;; ivy mete el simbolo ^ al ejecutar algunas ordenes, así se quita
  (setq ivy-initial-inputs-alist nil)
  ;; Dar la vuelta a los candidatos
  (setq ivy-wrap t)
  ;; Que el uso de fuzzy regex se use en todo, no solo en counsel-find-file
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
  ;; Ver la ruta de los ficheros virtuales
  (setq ivy-virtual-abbreviate 'full)
  ;; Seleccionar el candidato actual (C-m en vez de C-S-m)
  (setq ivy-use-selectable-prompt t)

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

;; -----
;; Otros
;; -----
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
  :ensure t)
(add-hook 'after-init-hook #'global-flycheck-mode)

(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :defer t)

;; (use-package ispell
;;   :config
;;   (setq-default ispell-program-name "hunspell")
;;   (setq ispell-really-hunspell t)
;;   :bind (("C-c s" . spell-buffer-spanish)
;;          ("C-c e" . spell-buffer-english)))

(use-package highlight-parentheses
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook
            (lambda()
              (rainbow-delimiters-mode))))
(global-highlight-parentheses-mode)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs (concat init-dir "snippets")))

(use-package company
  :ensure t
  ;;  :bind (("C-c /". company-complete)) ;; con esto solo se muestran de forma manual, no automática
  :config
  (global-company-mode))


;; ====================================
;; DEFINICIONES ESPECÍFICAS PARA CÓDIGO
;; ====================================

;; -----
;; C CPP
;; -----
;; TODO: C me está usando un completado y errores muy buenos... y no recuero qué paquete instalé. Investigarlo.
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode t)

;; -----
;; LaTex
;; -----
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t))

;; ----------
;; TypeScript
;; ----------
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))
(add-hook 'typescript-mode 'electric-pair-mode)

;; --------
;; Markdown
;; --------
(use-package markdown-mode
	     :ensure t)

;; ------------------------
;; Clojure (y algo de Lisp)
;; ------------------------
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

;; --------------------
;; Configuración Python
;; --------------------
(use-package elpy
  :ensure t)
(elpy-enable)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(setenv "WORKON_HOME" "/home/nando/miniconda3/envs")
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
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   (quote
	("bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "f92f181467b003a06c3aa12047428682ba5abe4b45e0fca9518496b9403cde6f" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(dumb-jump-mode t)
 '(fci-rule-color "#383838")
 '(global-display-line-numbers-mode t)
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
	("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
	(counsel-projectile swiper counsel undo-tree dumb-jump web-mode ensime tide projectile spacemacs-theme aggressive-indent zenburn-theme nimbus-theme flycheck-joker kibit-helper spaceline py-autopep8 4clojure expand-region centered-window-mode flycheck clj-refactor cider clojure-snippets yasnippet rainbow-delimiters highlight-parentheses paredit-everywhere paredit markdown-mode which-key use-package)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
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
 '(default ((t (:family "Fira Code" :foundry "CTDB" :slant normal :weight normal :height 98 :width normal)))))
