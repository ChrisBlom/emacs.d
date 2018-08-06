(defconst init-start (current-time))

;;
(defmacro section (name &rest a) `(progn ,@a))
(put 'section 'lisp-indent-function 'defun)

;; add extra package repositories
(setq package-archives '(("gnu"          . "http://elpa.gnu.org/packages/")
                         ;("marmalade"    . "https://marmalade-repo.org/packages/")
                         ("melpa"        . "http://melpa.org/packages/")
			 ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

;; make sure we can use packages
(or (package-installed-p 'use-package)
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))



;; only use use-package.el at compile-time
(eval-when-compile (require 'use-package))

(require 'bind-key)

;; put libs that cannot be loaded as packages on the load path
(add-to-list 'load-path (concat user-emacs-directory "lib"))

(section libs

  (use-package diminish
    :ensure t)

  ;; modern list library
  (use-package dash
    :ensure t
    :config (dash-enable-font-lock))

  ;; modern string library
  (use-package s :ensure t)

  ;; modern file handling library
  (use-package f :ensure t))

;; load basic setup
(require 'synth-setup)

;; load personal stuff
(use-package synth-utils)

(setq bookmark-file (f-join user-emacs-directory "var" "bookmarks"))

;; osx specific stuff
(use-package platform-osx
  :if (eq system-type 'darwin))

;; create required dir
(f-mkdir user-emacs-directory "backups")
(f-mkdir user-emacs-directory "tmp")

(defun f-tmp-file (&rest files)
  (let ((tmp-file (apply #'f-join user-emacs-directory "tmp" files)))
    (f-mkdir (f-parent tmp-file))
    tmp-file))

;; do not save backup files in same dir
(setq backup-by-copying t      ; don't clobber symlinks
      vc-make-backup-files t   ; backup versioned files as well
      backup-directory-alist
      `(("." . ,(f-join user-emacs-directory "backups")))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; were to find themes
(add-to-list 'custom-theme-load-path (f-join user-emacs-directory "themes"))
(load-theme 'synth t)

(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-mode +1)
    (setq key-chord-two-keys-delay 0.05)))

(use-package hydra
  :ensure t)

  ;; jump to char/line by key
(use-package avy
  :ensure t
  :bind (("C-o" . avy-goto-char)
	 ("M-g g" . avy-goto-line)))

  ;; undo/redo window configurations
(use-package winner
  :demand t
  :defer 1
  :config (winner-mode +1)
  :bind (("C-c b" . winner-undo)
	 ("C-c f" . winner-redo)))

(use-package zoom-window
  :ensure t
  :bind (("s-w" . zoom-window-zoom)
	 ("s-q" . zoom-window-next)))

;; Move buffers around

(use-package buffer-move
  :ensure t
  :bind
  (("C-c w <right>" . buf-move-right)
   ("C-c w <left>"  . buf-move-left)
   ("C-c w <up>"    . buf-move-up)
   ("C-c w <down>"  . buf-move-down)))

(section ido
  ;; better M-x
  (use-package smex
    :ensure t
    :bind (("M-x" . smex)
	   ("M-X" . smex-major-mode-commands)
	   ("C-x C-m" . smex)
	   ("C-x C-S-M" . smex-major-mode-commands))
    :config
    (setq smex-save-file (f-tmp-file "smex-items")))

  ;; efficient buffer/file switching
  (use-package ido
    :ensure t
    :bind ("C-x C-r" . ido-recentf-open)
    :config
    (progn
      (use-package idomenu :ensure t)
      ;; use fuzzy matching for ido completion
      (use-package flx-ido :ensure t)
      (use-package ido-completing-read+ :ensure t)
      (add-to-list 'ido-ignore-files "\\.DS_Store")
      (ido-mode t)
      (ido-ubiquitous-mode +1)
      (flx-ido-mode +1)
      (icomplete-mode +1)
      (bind-keys :map ido-completion-map)
      (setq ido-enable-flex-matching t
	    ;; disable ido faces to see flx highlights.
	    ido-use-faces nil
	    ido-enable-prefix nil
	    ido-create-new-buffer 'always
	    ido-max-prospects 5
	    ido-default-file-method 'selected-window
	    ido-everywhere 1)))

  (use-package ido-vertical-mode
    :ensure t
    :config
    (progn
      (ido-vertical-mode +1)
      (setq ido-vertical-show-count t)
      (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)))

  (use-package ido-completing-read+
    :ensure t))

(use-package recentf
  :init (setq recentf-save-file (f-tmp-file "recentf.el"))
  :config
  (progn
    (setq recentf-max-saved-items 300)
    (recentf-mode t)
    (defun ido-recentf-open ()
      "Use `ido-completing-read' to \\[find-file] a recent file"
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
	  (message "Opening file...")
        (message "Aborting")))))

;; Expand region increases the selected region by semantic units.
(use-package expand-region
  :ensure t
  :bind
  (("M-]" . er/expand-region)
   ("M-[" . er/contract-region)))

(use-package saveplace
  :init  (setq-default save-place t)
  :config
  (setq save-place-file (f-join user-emacs-directory "var" "places")))

(save-place-mode)

;; (defhydra hydra/mc (:color pink)
;;   ("a" mc/mark-previous-like-this "^")
;;   ("b" mc/mark-previous-like-this "v")
;;   ("q" nil "cancel")
;; )

(use-package multiple-cursors
  :ensure t
  :init (setq mc/list-file (f-join user-emacs-directory "etc" "multiple-cursors-prefs.el"))
  :bind
  (("C-c m h" . mc-hide-unmatched-lines-mode)
   ("C-c m a" . mc/mark-all-like-this)
   ("C-c m v" . mc/vertical-align)
   ("C-c m SPC" . mc/vertical-align-with-space)
   ("C-c m d" . mc/mark-all-symbols-like-this-in-defun)
   ("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m l" . mc/edit-lines)
   ("C-c m a" . mc/insert-characters)
   ("C-c m i" . mc/insert-numbers)
   ("C-c m s" . mc/sort-regions)
   ("C-c m r" . mc/reverse-regions)
   ("s-]" . mc/mark-next-like-this)
   ("s-[" . mc/mark-previous-like-this)
   ("s-{" . mc/unmark-previous-like-this)
   ("s-}" . mc/unmark-next-like-this)
   )
  :config
  (defvar mc--insert-character-char ?a)

  (defun mc--insert-character-and-increase ()
    (interactive)
    (insert (char-to-string mc--insert-character-char))
    (setq mc--insert-character-char (1+ mc--insert-character-char)))

  (defun mc/insert-characters (arg)
    "Insert increasing characters for each cursor, starting at 'a'"
    (interactive "Pc")
    (setq mc--insert-character-char (or arg ?a))
    (mc/for-each-cursor-ordered
     (mc/execute-command-for-fake-cursor #'mc--insert-character-and-increase cursor)))

  (key-chord-define global-map "[]" #'mc/mark-all-like-this-dwim))

(use-package change-inner
  :ensure t
  :bind
  (("M-i" . change-inner)
   ("M-o" . change-outer)))

(use-package paredit
  :ensure t
  :init
  (progn
    (add-hook 'lisp-mode-hook       (lambda () (paredit-mode +1)))
    (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
    (add-hook 'scheme-mode-hook     (lambda () (paredit-mode +1))))
  :bind
  (:map paredit-mode-map
	("M-d"       . live-paredit-forward-kill-sexp)
	("M-h"       . live-paredit-backward-kill-sexp)

	("s-<left>"  . backward-sexp)
	("s-<right>" . my/paredit-right)
	("C-)"       . paredit-forward-slurp-sexp)
	("C-("       . paredit-backward-slurp-sexp)
	("M-("       . paredit-forward-barf-sexp)
	("M-)"       . paredit-backward-barf-sexp)
	("s-("       . paredit-forward)
	("s-)"       . paredit-backward)
	("C-c ( a"   . align-cljlet)
					;     ("C-c p"     . nil)
	("C-c ) s"   . paredit-splice-sexp-killing-forward)
	("C-c ( s"   . paredit-splice-sexp-killing-backward)
	("C-c ( f"   . fill-paragraph)
	("C-M-z"     . align-cljlet)
	("M-S"       . paredit-split-sexp)
	("M-s"       . paredit-splice-sexp)
	("M-j"       . paredit-join-sexps)
	("M-T"       . transpose-sexps)
	("M-P"       . live-paredit-previous-top-level-form)
	("M-N"       . live-paredit-next-top-level-form)
	("C-M-f"     . nil)
	("C-M-b"     . nil)
	("M-q"       . live-paredit-reindent-defun)
	("M-\\"      . live-paredit-delete-horizontal-space))
  :commands paredit-mode
  :diminish "()"
  :config
  (progn
    ;; TODO extract useful stuff
    (load-file (f-join user-emacs-directory "lib/live-paredit.el"))

    (add-hook 'paredit-mode-hook (lambda () (hs-minor-mode 1)))

    (defun my/paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))

    (defun my/paredit-forward-down ()
      (interactive)
      (unwind-protect
	  (let (retval)
	    (condition-case ex
		(paredit-forward-down)
	      ('error (paredit-forward-up)))
	    retval)))



    (defun my/paredit-right ()
      (interactive)
      (condition-case ex
	  (paredit-up/down +1 -1)
	((scan-error) (paredit-forward))
	((error) (progn (forward-word)
			(setq a ex)))))


    ))

(use-package paxedit
  :ensure t
  :init
  (progn
    ;(add-hook 'lisp-mode-hook       (lambda () (paredit-mode +1)))
    ;(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
    ;(add-hook 'scheme-mode-hook     (lambda () (paredit-mode +1)))
    )
  :bind
  ("C-M-f" . paxedit-next-symbol)
  ("C-M-n" . paxedit-previous-symbol )
  :commands paxedit-mode
  :diminish "()")

(setq paxedit-sexp-delimiters
      '(?#
	?,
	?@
	?~
	?'
	?`))

(use-package aggressive-indent
  :ensure t
  :commands (aggressive-indent-mode)
  :diminish ""
  :config
  (setq aggressive-indent-sit-for-time 0.1)
  )

;; better grep, requires ag to be installed
(use-package ag :ensure t)

;; (use-package perspective
;;   :ensure t
;;   :config
;;   (persp-mode +1))

;; project oriented commands
(use-package projectile
  :ensure t
  :commands (projectile-find-file-dwim projectile-switch-project projectile-global-mode)
  :defer t
  :bind
  (("C-x p f" . projectile-find-file-dwim))
  :init
  (setq projectile-known-projects-file (f-tmp-file "projectile" "projectile-bookmarks.eld")
	projectile-cache-file (f-tmp-file "projectile" "projectile.cache"))
  :config
  (progn

    (defun my/projectile-test-suffix (project-type)
      (if (eq project-type 'lein-midje)
	  "_test"
	(projectile-test-suffix project-type)))

    (defun my/projectile-test-prefix (project-type)
      (if (eq project-type 'lein-midje)
	  nil
	(projectile-test-prefix project-type)))

    (custom-set-variables
     '(projectile-test-prefix-function  #'my/projectile-test-prefix)
     '(projectile-test-suffix-function  #'my/projectile-test-suffix))

    ;; to avoid make taking precedence over lein-midje
    (remhash 'make projectile-project-types)

    (projectile-global-mode +1)
    (setq projectile-mode-line-lighter "")
    (setq projectile-mode-line "") ;; smart modeline already shows the current projectile project
    (setq projectile-completion-system 'ido)

    (bind-keys :map projectile-command-map
     ("r" .   projectile-replace))

    (key-chord-define projectile-mode-map "jk" #'projectile-switch-project)
    (key-chord-define projectile-mode-map "df" #'projectile-find-file-dwim)
    (key-chord-define projectile-mode-map "pt" #'projectile-toggle-between-implementation-and-test)))

;; (use-package persp-projectile
;;   :config
;;   (bind-keys
;;    :map persp-mode-map
;;    ("C-c w q" . persp-switch-quick)
;;    ("C-c w k" . persp-kill)))

(use-package helm-ag :ensure t)

;; selection framework
(use-package helm
  :ensure t
  :bind (("M-y" . helm-show-kill-ring)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-c h o" . helm-occur)
	 ("C-c h f" . helm-find-files)
	 ("C-c h r" . helm-regexp)
	 ("C-c h m" . helm-mark-ring)
	 ("C-c h x" . helm-M-x)
	 ("C-c h a" . helm-ag)
	 ("C-c h b" . helm-buffers-list)
	 ("C-c h i" . helm-imenu))
  :init
  (setq helm-adaptive-history-file (f-tmp-file "helm" "adaptive-history"))
  :config
  (progn
    (require 'helm-config)
    (require 'helm-ring)
    (require 'helm-source)
    (require 'helm-adaptive)



    (setq helm-split-window-default-side 'below
	  helm-split-window-in-side-p t
	  helm-move-to-line-cycle-in-source t
	  helm-display-header-line nil
	  helm-candidate-separator "ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ")
    (helm-autoresize-mode +1)))


;; project oriented helm commands
(use-package helm-projectile
  :ensure t
;  :defer t
  :bind (("s-f" . helm-projectile-find-file-dwim)
	 ("s-s" . helm-projectile-ag)
	 ("s-g" . helm-projectile-grep)
	 ("s-p" . projectile-switch-project)
	 ("s-a" . helm-projectile-ack)
	 ("s-t" . projectile-toggle-between-implementation-and-test))
  :bind (:map projectile-command-map
	      ("f" .   helm-projectile-find-file-dwim)
	      ("s s" . helm-projectile-ag)
	      ("s g" . helm-projectile-grep)
	      ("s a" . helm-projectile-ack)))

;(use-package ack-and-a-half :ensure t :pin melpa-stable)


(defun my/company-transformer (candidates)
  (-distinct candidates))

;; completion framework
(use-package company
  :ensure t
  :defer t
  :diminish ""
  :bind
  (:map company-active-map
	("C-n" . company-select-next-or-abort)
	("C-p" . company-select-previous-or-abort))
  :config
  (progn
    (setq company-minimum-prefix-length 2
	  company-idle-delay 0.1
	  company-selection-wrap-around t
	  company-async-timeout 0.05
	  company-async-timeout 2
	  company-auto-complete nil
	  company-auto-complete-chars '(?\ ?\) ?. ?/)
	  ;; select using super+number
	  company-show-numbers t
	  company-tooltip-align-annotations t
	  ;; dont downcase completions
	  company-dabbrev-downcase nil
	  ;; company-backends '(company-bbdb
;; 			     company-nxml
;; 			     company-css
;; 			     company-eclim
;; 			     company-semantic
;; 			     company-clang
;; 			     company-xcode
;; 			     company-cmake
;; 			     (company-capf company-dabbrev-code)
;; ;			     (company-dabbrev-code company-gtags company-etags company-keywords)
;; 			     company-oddmuse
;; 			     company-files
;; 			     company-yasnippet
;; 			     company-dabbrev-code
;; 			     company-dabbrev
;; 			     )
;	  company-transformers '(company-sort-by-occurrence my/company-transformer company-sort-by-backend-importance)
	  )
    (global-company-mode +1)

    (dotimes (i 10)
      (define-key company-mode-map (read-kbd-macro (format "s-%d" i)) 'company-complete-number))))

;; (use-package company-flx
;;   :ensure t
;;   :config
;;   (company-flx-mode -1))

;; (use-package helm-flx
;;   :ensure t
;;   :config
;;   (helm-flx-mode +1))

;; use helm to select company completions
(use-package helm-company
  :ensure t
  :after company
  :bind (("C-:" . helm-company)
	 ("C-;" . helm-company))
  :bind (:map company-active-map ; also available in completion menu
	      ("C-:" . helm-company)
	      ("C-;" . helm-company)))

;; show docs for selected completion in popup window
(use-package company-quickhelp
  :ensure t
  :commands (company-quickhelp-mode)
  :config
  (progn
    (company-quickhelp-mode +1)
    (setq company-quickhelp-delay 0.3)))

;; prettier and smarter mode line
(use-package smart-mode-line
  :ensure t
  :config (progn
	    (message "starting smart-mode-line")
	    (add-to-list 'sml/replacer-regexp-list '("^~/git/" ":Git:") t)
	    ;;   (sml/apply-theme 'synth)
	    (sml/setup)
	    (setq sml/use-projectile-p 'before-prefixes)))

;; show changes in fringe
(use-package git-gutter
  :ensure t
  :defer t
  :diminish ""
  :config
  (progn
    (global-git-gutter-mode +1)
    (setq git-gutter:unchanged-sign " ")))

;; undo/redo history tree navigation
(use-package undo-tree
  :ensure t
  :diminish ""
  :bind (("C-x u" . undo-tree-visualize))
  :bind (:map undo-tree-visualizer-mode-map
	      ("C-g" . kill-buffer-and-window))
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-auto-save-history nil)))

;; show argument lists in echo area
(use-package eldoc
  :diminish ""
  :commands eldoc-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook 'eldoc-mode))
  :config
  (progn
    (setq eldoc-current-idle-delay 0.1)))

;; navigation for elisp
(use-package elisp-slime-nav
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook
	    (lambda () (elisp-slime-nav-mode t)))
  :diminish ""
  :commands elisp-slime-nav-mode)

;; Show documentation/information with M-RET
(define-key lisp-mode-shared-map (kbd "M-RET") 'live-lisp-describe-thing-at-point)

(use-package goto-last-change
  :ensure t
  :bind
  (("s-l" . goto-last-change)
   ("M-g M-l" . goto-last-change)
   ("M-g l" . goto-last-change)))

;; highlight, navigate and edit symbols
(use-package auto-highlight-symbol
  :ensure t
  :diminish ""
  :init
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode)
  :config
  (progn
    (global-auto-highlight-symbol-mode +1)
    (set-face-attribute 'ahs-face nil
			:bold nil
			:underline t
			:background nil)
    (set-face-attribute 'ahs-definition-face nil
			:underline t
			:bold t
			:background nil)
    (setq ahs-default-range 'ahs-range-whole-buffer
	  ahs-include "^[0-9A-Za-z/_.,:;*+=&%|$#@!^?>-]+$"
	  ahs-select-invisible 'temporary
	  ahs-idle-interval 0.25)
    (bind-keys
     :map auto-highlight-symbol-mode-map
     ("M-<left>" . nil)
     ("M-<right>" . nil)
     ("M-F" . ahs-forward)
     ("M-B" . ahs-backward)
     ("s-e" . ahs-edit-mode)
     ("s-f" . ahs-forward)
     ("s-F" . ahs-forward-definition)
     ("s-b" . ahs-backward)
     ("s-B" . ahs-backward-definition)
     ("M-E" . ahs-edit-mode))))

;; snippets
(use-package yasnippet
  :ensure t
  :diminish ""
  :commands (yas-minor-mode yas-global-mode)
  :bind (:map yas-minor-mode-map
	      ("C-<tab>" . yas-expand-from-trigger-key))
  :init
  (progn
    (setq yas-snippet-dirs
	  (list
	   (f-join user-emacs-directory "etc" "snippets")
	   ;(package-desc-dir (cadr (assoc 'yasnippet package-alist)))
	   ;(package-desc-dir (cadr (assoc 'clojure-snippets package-alist)))
	   )))
  :config
  (progn
    (setq yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-no-prompt) )
))

(use-package dockerfile-mode
  :ensure t
  :commands (dockerfile-mode))

(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :config
  (use-package company-restclient :ensure t))

(use-package eval-pulse
  :init
  (setq eval-pulse-depth 4)
  :diminish ""
  :config
  (progn
    (eval-pulse-mode +1)))

(use-package window-number
  :ensure t)

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(use-package tramp
  :config
  (progn
    (setq tramp-default-method "sshx")
    ;; Workaround for problem with TRAMP mode: override tmp dir
    ;; Control Path too long error
    ;; TMPDIR variable is really large
    ;; http://lists.macosforge.org/pipermail/macports-tickets/2011-June/084295.html
    (setenv "TMPDIR" "/tmp")
    (setq tramp-auto-save-directory (f-tmp-file "tramp" "autosaves/" ))))


(font-lock-add-keywords 'scheme-mode
			`((,(concat "(\\(?:\.*/\\)?"
				    (regexp-opt '("when") t)
				    "\\>")
			   1 font-lock-keyword-face)))

(font-lock-add-keywords 'scheme-mode
			`((,(concat "\\<"
				    (regexp-opt '("#f" "#t") t)
				    "\\>")
			   1 font-lock-builtin-face)))

(font-lock-add-keywords 'scheme-mode
			`(("\\<\\sw+\\>: "
			   1 font-lock-builtin-face)))


(font-lock-add-keywords 'scheme-mode
			`((,(concat "\\<"
				    (regexp-opt '("box" "unbox"
						  "box-set!") t)
				    "\\>")
			   1 font-lock-preprocessor-face)))

(defun geiser-repl-clear-buffer ()
  "Delete the output generated by the scheme process."
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (geiser-repl--last-prompt-start))
    (when (< (point) (geiser-repl--last-prompt-end))
      (goto-char (geiser-repl--last-prompt-end)))
;;    (recenter t)
    ))

(add-hook 'scheme-mode-hook #'geiser-mode)

(use-package geiser
  :ensure t
  :pin melpa-stable
 ; :commands geiser-mode
;  :mode "\\.scm\\" . geiser-mode
  :config
  (progn

    (defun my/geiser-eval-to-point ()
      (interactive)
      (geiser-eval-region (point-min) (point)))

    (defun my/geiser-eval-imports (args)
      "evals al (use ...) and (import ...) expression in the buffer"
      (interactive "P")
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward (rx
				   line-start
				   (syntax \()
				   (or "use" "import" "require-extension")
				   " "
				   (*? anything)
				   (syntax \))
				   ) nil t)
	  (condition-case nil
	      (up-list)
	    (error nil))
	  (condition-case nil
	      (geiser-eval-last-sexp args)
	    (error nil)))))

    (defun geiser-syntax--extra-keywords ()
      `((,(format "[[(]%s\\>" (regexp-opt '("#f" "#t" "box" "unbox" "doto"
					    "define-external"
					    ) 1)) . 1)))

    (defun geiser-syntax--keywords ()
      (append
       (geiser-syntax--extra-keywords)
       `(("\\[\\(else\\)\\>" . 1)
	 (,(rx "(" (group "define-syntax-rule") eow (* space)
	       (? "(") (? (group (1+ word))))
	  (1 font-lock-keyword-face)
	  (2 font-lock-function-name-face nil t)))))

    (defun geiser-save-and-load-buffer ()
      (interactive)
      (save-buffer)
      (geiser-load-current-buffer))

    (geiser-syntax--keywords)

    (setq geiser-active-implementations '(chicken)
	  geiser-debug-jump-to-debug-p nil ; don't jump to error buffer
	  geiser-debug-show-debug-p nil ; don't jump to error buffer
	  geiser-autodoc-delay 2
	  geiser-autodoc-mode nil)

    (require 'geiser-repl)
    ;; (eval-after-load "init.el"
    ;;   (bind-keys
    ;;    :map geiser-repl-mode-map
    ;;    ("C-c C-o" . geiser-repl-clear-buffer)))


    (push '("lambda"  . ?λ) prettify-symbols-alist)

    (defun add-geiser-bindings ()
      (setq-local eldoc-documentation-function #'geiser-autodoc--eldoc-function)
      (bind-keys :map geiser-mode-map
		 ("C-c C-j" . run-chicken)
		 ("C-c m b" . geiser-load-current-buffer)
		 ("C-<return>" . geiser-eval-definition)
		 ("C-c C-c" . geiser-repl-interrupt)
		 ("C-c M-c" . geiser-connect)
		 ("C-c C-n" . my/geiser-eval-imports)
		 ("C-x M-h" . my/geiser-eval-to-point)))
    (add-hook 'geiser-mode-hook #'add-geiser-bindings)
    (add-hook 'geiser-mode-hook #'paredit-mode)

    (font-lock-add-keywords
     'geiser-mode
     `((,(concat "\\<"
		 (rx "./")
		 "[a-z0-9-]+"
		 "\\>")
	0
	font-lock-warning-face)))))

(use-package hideshow
  :diminish ""
  :config
  (progn
    (bind-keys
     :map hs-minor-mode-map
     ("M-±"       . hs-toggle-hiding)
     ("<backtab>" . hs-toggle-hiding))))

;(use-package hideshow-fringe :ensure t)

;(hideshow-fringe-enable)

;;;; Git

(defun gitx ()
  (interactive)
  (shell-command "gitx ."))



(defvar magit-buffer-lock-functions '())
(bind-key "C-c g x" 'gitx)

(use-package magit
  :ensure t
  :defer 1
  :pin melpa-stable
  :bind (("C-x m" . magit-status)
	 ("C-c g b" . magit-blame)
	 ("C-c g s" . magit-branch-popup)
	 ("C-c g v" . my/magit-visit-github-pull-request)
	 ("C-c g 0" . my/magit-visit-circleci-url))
  :config
  (progn
    (setq magit-completing-read-function 'magit-ido-completing-read
	  magit-status-expand-stashes nil
	  magit-status-show-hashes-in-headers 'magit-insert-head-branch-header)

    (remove-hook 'pre-command-hook #'magit-pre-command-hook)

    (defun my/magit-visit-github-pull-request ()
      "Visit the current branch's PR on Github."
      (interactive)
      (browse-url
       (format "https://github.com/%s/pull/new/%s"
	       (replace-regexp-in-string
		"\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
		(magit-get "remote"
			   (magit-get-push-remote)
			   "url"))
	       (magit-get-current-branch))))

    (defun my/magit-visit-circleci-url ()
      "Visit the current branch build status on CircleCI"
      (interactive)
      (browse-url
       (format "https://circleci.com/gh/%s/tree/%s"
	       (replace-regexp-in-string
		"\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
		(magit-get "remote"
			   (magit-get-push-remote)
			   "url"))
	       (magit-get-current-branch))))

    (defun my/magit-display-buffer (buffer)
      (display-buffer
       buffer (if (and (derived-mode-p 'magit-mode)
		       (not (memq (with-current-buffer buffer major-mode)
				  '(magit-process-mode
				    magit-stash-mode
				    magit-revision-mode
				    magit-diff-mode
				    magit-status-mode))))
		  '(display-buffer-same-window)
		nil)))

    (define-key magit-mode-map "v" #'my/magit-visit-github-pull-request)
    (define-key magit-mode-map "§" #'gitx)
    (define-key magit-mode-map "`" #'gitx)
    (define-key magit-mode-map "0" #'my/magit-visit-circleci-url)

    (setq magit-display-buffer-function 'my/magit-display-buffer)))

;; (use-package magithub
;;   :ensure t
;;   :after magit
;;   :config (magithub-feature-autoinject t))

(defun magithub--api-available-p ()
  t)


(use-package git-timemachine
  :ensure t
  :commands git-timemachine
  :bind (("C-c g t" . git-timemachine)))

(use-package browse-at-remote
  :ensure t
  :commands browse-at-remote
  :bind (("C-c g r" . browse-at-remote)))

(use-package git-link
  :ensure t
  :commands git-link
  :bind (("C-c g l" . git-link))
  :config (setq git-link-open-in-browser nil))

(use-package ediff
  :config
  (progn
    (setq ediff-window-setup-function 'ediff-setup-windows-plain
	  ediff-split-window-function 'split-window-horizontally
	  ediff-diff-options "-w")
    (add-hook 'ediff-after-quit-hook-internal 'winner-undo)))

(use-package markdown-mode
  :ensure t
  :init (progn
	  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
	  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode)))
  :commands (markdown-mode))

;; (use-package js2-mode
;;   :ensure t
;;   :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;;   :commands js2-mode)

(use-package json-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  :commands json-mode
  :config
  (add-hook 'json-mode-hook #'paredit-mode))

(use-package skewer-mode
  :ensure t
  :commands skewer-mode)

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :bind
  (("C-c c t"     . flycheck-mode))
  :config
  ;(global-flycheck-mode)
  (bind-keys
   :map flycheck-mode-map
   ("C-c c c"     . flycheck-buffer)
   ("C-c c o"     . flycheck-clear)
   ("C-c c l"     . flycheck-list-errors)
   ("C-c c n"     . flycheck-next-error)
   ("C-c c p"     . flycheck-previous-error)))

(use-package evil
  :ensure t
  :commands evil-mode
  :bind
  (("C-c w e" . evil-mode))
  :config
  (progn
    (bind-keys
     ("<f13>" . evil-mode))
    (evil-define-key 'normal global-map " " nil)))

(use-package evil-lisp-state
  :ensure t
  :commands evil-lisp-state
  :config
  (setq evil-lisp-state-major-modes
	'(emacs-lisp-mode clojure-mode racket-mode scheme-)))

(use-package org
  :ensure t
  :commands org-mode
  :config
  (progn
    (bind-keys
     :map org-mode-map
     ("M-." . org-open-at-point))))

(defface clojure-ns-face
  '((t (:inherit default :bold nil)))
  "Face used to font-lock namespaces"
  :group 'clojure
  :package-version '(clojure-mode . "3.0.0"))

(defun bindings-jit-highlighter (beg end)
  "The jit highlighter of highlight-stages."
  (setq beg (progn (goto-char beg)
                   (beginning-of-defun)
                   (skip-syntax-backward "'-") ; skip newlines?
                   (point))
        end (progn (goto-char end)
                   (end-of-defun)
                   (skip-syntax-forward "'-") ; skip newlines?
                   (point)))
  (remove-overlays beg end 'category 'highlight-stages)
  (highlight-stages--jit-highlighter-1 beg end 0))

;; (re-search-forward (rx "#_"
;; 		       (or
;; 			(+ (syntax \w))
;; 			(+ (syntax \s))
;; 			(sequence
;; 			 (syntax \( )
;; 			 (minimal-match (regexp ".*"))
;; 			 (syntax \))))))

;; (fact "fields with the same path get the same id"
;;       (let [c (sc/setup-db)
;; 	      _ #_(aadsad  adsdasade)
;; 	      tx  @(d/transact c [(persist-field (d/db c) ["abc"])] )
;; 	      before   (d/entid (d/db c) [:field/path-edn (pr-str ["abc"])])
;; 	      tx2  @(d/transact c [(persist-field (d/db c) ["abc"])] )]
;;         (d/entid (d/db c) [:field/path-edn (pr-str ["abc"])])
;;         => before
;;         (:tempids tx2) => {}))

(defun my/display-image-inline (buffer-name file-name)
  "Use `BUFFER-NAME' to display the image in `FILE-NAME'.
  Checks weather `BUFFER-NAME' already exists, and if not create
  as needed."
  (save-excursion
    (switch-to-buffer-other-window buffer-name)
    (iimage-mode t)
    (read-only-mode -1)
    (kill-region (point-min) (point-max))
    ;; unless we clear the cache, the same cached image will
    ;; always get re-displayed.
    (clear-image-cache nil)
    (insert-image (create-image file-name))
    (read-only-mode t)))

(use-package image+
  :ensure t)

(use-package esup
  :ensure t
  :commands esup)

(section clojure

  (use-package clojure-mode
    :ensure t
    :commands clojure-mode
    :init
    (defface clojure-lvar-face
      '((t (:inherit font-lock-keyword-face)))
      "Face used to font-lock logic variables (for datomic and cascalog)"
      :group 'clojure
      :package-version '(clojure-mode . "3.0.0"))

    (defface note-face
      '((t (:font-lock-comment-face
	    :background "DarkOrange")))
      "Face used to highligt notes in comments"
      :group 'clojure
      :package-version '(clojure-mode . "3.0.0"))

    (defvar note-face 'note-face)

    (set-face-attribute note-face nil
			:background "DarkOrange"
			:foreground "Black")

    :config

    (progn
      (add-hook 'clojure-mode-hook (lambda ()
				     (yas-minor-mode +1)
				     (yas-load-directory
				      (f-join user-emacs-directory "etc" "snippets" "clojure-mode" ".")
				      t
				      )))


      (put-clojure-indent 'time 2)
      (put-clojure-indent 'section 'defun)
      (put-clojure-indent 'letk 1)
      (put-clojure-indent 'fact 'defun)
      (put-clojure-indent 'defmodule '(1 nil nil (:defn)))
      (put-clojure-indent 'for-map 'defun)
      (put-clojure-indent 'match 'defun)
      (put-clojure-indent 'facts 'defun)
      (put-clojure-indent 'future-fact 'defun)
					;(put-clojure-indent 'facts 'defun)
      ;; fontlock logic vars
      (font-lock-add-keywords
       'clojure-mode
       `((,(concat "\\<"
		   "\\?[a-z0-9-\\+]+"
		   "\\>")
	  0
	  font-lock-keyword-face)))

      (font-lock-add-keywords
       'cider-mode
       `((,(concat "\\<"
		   "\\?[a-z0-9-]+"
		   "\\>")
	  0
	  font-lock-keyword-face)))

      (font-lock-add-keywords
       'clojure-mode
       `((,(regexp-opt (list "GET" "PUT" "POST" "ANY" "OPTIONS" "DELETE") 'words)
	  0
	  font-lock-preprocessor-face)))

      (font-lock-add-keywords
       'clojure-mode
       `((,(concat "\\<"
		   (rx "./")
		   "[a-z0-9-_]+"
		   "\\>")
	  0
	  font-lock-warning-face)))


      ;; invocations
      (font-lock-add-keywords 'clojure-mode
			      `((,(concat "(\\(?:\.*/\\)?"
					  (regexp-opt '("facts?") t)
					  "\\>")
				 1 font-lock-builtin-face)))

      ;; anywhere
      (font-lock-add-keywords 'clojure-mode
			      `((,(concat "\\<"
					  (regexp-opt '("=>" "=not=>" "=contains=>" "run!" "getx" "getx-in") t)
					  "\\>")
				 1 font-lock-builtin-face)))

      ;; (font-lock-add-keywords 'clojure-mode
      ;; 			       `((,(rx "#_"
      ;; 				       (or
      ;; 					(+ (syntax \w))
      ;; 					(+ (syntax \s))
      ;; 					(sequence
      ;; 					 (syntax \( )
      ;; 					 (minimal-match (regexp ".*"))
      ;; 					 (syntax \)))))
      ;; 				  1 font-lock-doc-face 'set)))

      ;; (font-lock-add-keywords
      ;;  'clojure-mode
      ;;  `((,(rx "#_"
      ;; 	     (or
      ;; 	      (+ (syntax \w))
      ;; 	      (+ (syntax \s))
      ;; 	      (sequence "(" (regexp ".*") ")")
      ;; 	      (sequence "[" (regexp ".*") "]")
      ;; 	      (sequence "{" (regexp ".*") "}")))
      ;; 	0
      ;; 	font-lock-warning-face)))

      (font-lock-add-keywords
       'clojure-mode
       `((,(concat
	    "\\<"
	    (regexp-opt '("TODO" "REVIEW" "XXX" "QUESTION" "BUG" "NOTE"))
	    "\\>")
	  0
	  note-face t)))

      (font-lock-add-keywords
       'clojure-mode
       `((,(concat "\\<" (regexp-opt '("@" "$")) "\\>")
	  0
	  font-lock-preprocessor-face)))

      (use-package clojure-mode-extra-font-locking :ensure t)

      (require 'clojure-mode-extra-font-locking)
      (use-package align-cljlet :ensure t)
      (defun synth-toggle-clojure-ignore-next-form ()
	"toggle #_"
	(interactive)
	(save-excursion
	  (goto-char (1- (paredit-point-at-sexp-start)))
	  (if (char-equal ?_  (preceding-char))
	      (delete-backward-char 2)
	    (insert-string "#_"))))
      (defun my/no-cider-connection ()
	(interactive)
	(message "Cider is not connected"))
      (bind-keys
       :map clojure-mode-map
       ("C-c p"   . nil) ;; conflict with projectile
       ("C-c C-j" . cider-jack-in)
       ("C-c C-k" . (lambda () (interactive)
		      (cider-jack-in)
		      (cider-eval-buffer)))
       ("C-c M-c" . cider-connect)
       ("C-x C-e" . my/no-cider-connection)
       ("C-c C-p" . my/no-cider-connection)
       ("C-c C-z" . cider-connect)
       ("C-c C-3" . synth-toggle-clojure-ignore-next-form))
      ;; compjure route formatting
      (define-clojure-indent
	(defroutes 'defun)
	(GET 2)
	(POST 2)
	(PUT 2)
	(DELETE 2)
	(HEAD 2)
	(ANY 2)
	(OPTIONS 2)
	(context 2))))

  (defhydra
    hydra-refactor (:color pink)
    ("t" clojure-thread "thread")
    ("f" clojure-thread-first-all "thread-first-all")
    ("l" clojure-thread-last-all "thread-last-all")
    ("c" cljr-clean-ns "clean-ns")
    ("u" cljr-unwind-all "unwind")
    ("U" clojure-unwind-all "unwind-all")
    ("l" cljr-move-to-let "to-let")
    ("m" cljr-magic-require-namespaces "requires-namespaces" )
    ("s" clojure-sort-ns "sort ns")
    ("e" cljr-extract-function "extract function")
    ("d" cljr-extract-def "extract def")
    ("r" cljr-remove-unused-requires  "r")
    ("q" nil "cancel"))

  (use-package clojure-snippets
    :ensure t
    :after clojure-mode)

  (bind-keys
   :map clojure-mode-map
   )

  (use-package cider
    :pin melpa-stable
    :ensure t
    :commands (cider-mode)
    :bind (:map cider-mode-map
		("M-RET"     . cider-doc)
		("C-<return>"     . cider-eval-defun-at-point)
		("C-c s"   .   my/cider-source)
		("C-c j"   .   cider-jump-to-compilation-error)
		("C-x M-e"   . cider-eval-last-sexp-and-replace)

		("C-c l p"   . my/cider-pull-dependency)
		("C-c C-r" . cider-eval-region)

		("C-x c c"   . my/cider-eval-last-sexp-comment)
		("C-c ."     . my/cider-dot)
		("s-."       . cider-eval-expression-at-point)
		("C-x M-r"   . cider-eval-last-sexp-to-repl)
		("C-x C-k"   . my/cider-eval-buffer)
		("C-x M-h"   . cider-eval-to-point)
		("C-x C-n"   . cider-eval-ns-form)
		("C-c r r"   . my/cider-require-symbol)

		("C-c C-j"   . cider-jack-in)
		("C-c C-q"   . cider-quit)
		("C-c r c"   . cider-connection-browser)
		("C-c p-p"   . nil) ;; conflict with projectile
		("C-c C-o"   . my/cider-clear-repl)
		("C-c C-p"   . my/cider-pp)
		("C-c d"     . cider-debug-defun-at-point)
		("C-c e"   .   my/cider-throw-last-exception)

		;; Testing
		("C-c t d"   . cider-test-run-test)
		("C-c t t"   . cider-test-run-test)
		("C-c t n"   . cider-test-run-ns-tests)
		("C-c t p"   . cider-test-run-project-tests)
		("C-c t r"   . cider-test-show-report)
		("C-c C-t"   . cider-test-rerun-tests)

		("C-c w t"   . my/cider-show-impl-and-test)
		("C-c t m"   .   my/cider-midje-load-facts)
		("C-c t e"   .   my/cider-midje-autotest-error)
		("C-c t <up>"   .   my/cider-midje-autotest-start)
		("C-c t <down>"   .   my/cider-midje-autotest-stop))
    :bind (:map cider-repl-mode-map
		("C-c p t" . cider-repl-toggle-pretty-printing)
		("C-c C-o" . cider-repl-clear-buffer)
		("C-c t m"   .   my/cider-midje-load-facts)
		("C-c t <up>"   .   my/cider-midje-autotest-start)
		("C-c t <down>"   .   my/cider-midje-autotest-stop))
    :bind (:map clojure-mode-map
		("C-c c f" . cider-figwheel-repl)
		("C-c C-j" . cider-jack-in))
    :bind (:map clojurescript-mode-map
		("C-c j r" . my/cider-cljs-refresh)
		("C-c c r" . my/cider-cljs-refresh)
		("C-c j o" . my/cider-cljs-clear))
    :init
    (add-hook 'cider-repl-mode-hook
	      (lambda ()
		(eldoc-mode +1)
		(paredit-mode -1)
		(rainbow-delimiters-mode -1)
		(auto-highlight-symbol-mode -1)
		(aggressive-indent-mode -1)
		(yas-minor-mode -1)
		(turn-off-hideshow)
		(when (eq system-type 'windows-nt)
		  (live-windows-hide-eol))
		(undo-tree-mode -1)))
    (add-hook 'clojure-mode-hook
	      (lambda ()
		(eldoc-mode +1)
		(paredit-mode +1)
		(aggressive-indent-mode -1)
		(auto-highlight-symbol-mode +1)
		(yas-minor-mode +1)
		(setq buffer-save-without-query t)
		(clj-refactor-mode +1)
		(prettify-symbols-mode +1)))
    :config
    (setq cider-prefer-local-resources t
	  cider-mode-line-show-connection nil

	  cider-prompt-for-symbol nil
	  cider-interactive-eval-output-destination 'repl-buffer

	  cider-use-fringe-indicators t
	  cider-use-overlays t

	  cider-show-error-buffer t
	  cider-auto-select-error-buffer nil
	  cider-auto-jump-to-error nil
	  cider-annotate-completion-candidates t
	  cider-test-show-report-on-success nil
	  cider-save-file-on-load nil
	  ;; lein
	  cider-lein-command "lein"
	  cider-lein-parameters "with-profile +power repl"
	  ;; repl
	  cider-repl-pop-to-buffer-on-connect nil
	  cider-repl-wrap-history t
	  cider-repl-use-clojure-font-lock nil
	  cider-repl-display-help-banner nil
	  cider-repl-pop-to-buffer-on-connect nil
	  ;; nrepl
	  nrepl-buffer-name-show-port t
	  nrepl-log-messages nil
	  nrepl-message-buffer-max-size 10000)
    (require 'cider-utils)
    ;; jump to repl buffer on connect cider-repl-pop-to-buffer-on-connect t
    (add-to-list 'same-window-buffer-names "*cider*")

    (define-key cider-mode-map (kbd "C-x C-q")
      (lambda ()
	(interactive)
	(cider-switch-to-relevant-repl-buffer)
	(cider-repl-clear-buffer)
	(cider-switch-to-last-clojure-buffer)
	(cider-eval-last-sexp)
	(my/cider-pp)))

    (define-key clojure-mode-map (kbd "C-x s-p")
      (lambda ()
	(interactive)
	(cider-eval-last-sexp)
	(cider-repl-clear-buffer)
	(my/cider-pp)))

    (defun cider-eval-and-move-next ()
      (interactive)
      (outline-next-visible-heading 1)
      (cider-eval-defun-at-point))

    (defun cider-eval-and-move-previous ()
      (interactive)
      (outline-previous-visible-heading 2)
      (cider-eval-defun-at-point))

    (defun cider-figwheel-repl ()
      (interactive)
      (cider-connect "localhost" 7888)
      (with-current-buffer (cider-current-repl-buffer)
	(goto-char (point-max))
	(insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
	(cider-repl-return))))

  (use-package cider-eval-sexp-fu
    :ensure t)

  (setq eval-sexp-fu-flash-duration 0.3)
  (defun cider-esf--bounds-of-last-sexp ()
    "Return the bounds of the defun around point.
Copies semantics directly from `cider-last-sexp' to ensure highlighted
area is identical to that which is evaluated."
    (cons (save-excursion
            (backward-sexp)
            (point))
          (save-excursion
            (backward-sexp)
	    (forward-sexp)
            (point))))


  ;;(use-package flycheck-clojure
  ;;  :ensure t
  ;;  :pin melpa-stable
  ;;  :after flycheck
  ;;  :config
  ;;  (progn
  ;;    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)XRxf
  ;;    ;;(flycheck-clojure-setup)
  ;;
  ;;    ))

  (use-package flycheck-joker
    :ensure t
    :pin melpa
    :after flycheck)

  ;; (use-package  flycheck-pos-tip :ensure t)


  ;;(use-package sayid
  ;;  :after cider
  ;;  :ensure t
  ;;  :commands sayid-mode
  ;;  :config
  ;;  (sayid-setup-package))


  (use-package cider-eval-sexp-fu)

  ;; refactoring

  (use-package clj-refactor
    :ensure t
    :pin melpa-stable
					; :after cider
    :diminish ""
    :bind (:map clojure-mode-map
		("C->"     . cljr-cycle-coll)
		("C-."     . cljr-thread)
		("C-,"     . cljr-unwind))
    :config
    (progn
      (bind-keys :map clj-refactor-map
		 :prefix "C-c r"
		 :prefix-map cljr
		 ("m"   . cljr-move-to-let)
		 ("l"   . cljr-introduce-let)
		 ("e f" . cljr-extract-function)
		 ("e d" . cljr-extract-def)
		 ("e c" . cljr-extract-constant)
		 ("d k" . cljr-destructure-keys)
		 ("t"   . cljr-thread)

		 ("u"   . cljr-unwind)
		 ("n s" . cljr-sort-ns)
		 ("n c" . cljr-clean-ns)
		 ("s"   . cljr-rename-symbol)
		 ("r"   . cljr-add-require-to-ns)
		 ("i"   . cljr-add-import-to-ns)

		 ("d a"   . cljr-add-project-dependency)
		 ("d h"   . cljr-hotload-dependencies))

      (setq cljr-midje-test-declaration "[midje.sweet :refer :all]")
      ;; monkey patch to refer all
      (defun cljr--add-test-declarations ()
	(save-excursion
	  (let* ((ns (clojure-find-ns))
		 (source-ns (cljr--find-source-ns-of-test-ns ns (buffer-file-name))))
	    (cljr--insert-in-ns ":require")
	    (when source-ns
	      (insert "[" source-ns " :refer :all]"))
	    (cljr--insert-in-ns ":require")
	    (insert (cond
		     ((cljr--project-depends-on-p "midje")
		      cljr-midje-test-declaration)
		     ((cljr--project-depends-on-p "expectations")
		      cljr-expectations-test-declaration)
		     ((cljr--cljc-file-p)
		      cljr-cljc-clojure-test-declaration)
		     (t cljr-clojure-test-declaration))))
	  (indent-region (point-min) (point-max))))

      (setq cljr-magic-require-namespaces
	    (-distinct (-concat cljr-magic-require-namespaces
				'(("component" . "com.stuartsierra.component")
				  ("s" . "schema.core")
				  ("z" . "clojure.zip")
				  ("d" . "datomic.api")
				  ("edn" . "clojure.edn")
				  ("aero" . "aero.core")
				  ("a" . "clojure.core.async")
				  ("sh" . "clojure.java.shell")
				  ("log" . "taoensso.timbre")
				  ("json" . "cheshire.core")
				  ("p" . "plumbing.core")
				  ("s3" . "amazonica.aws.s3")
				  ("rum" . "rum.core")
				  ))))
      ;; override to use my preferred format
      (defun cljr--insert-in-ns (type)
	(cljr--goto-ns)
	(if (cljr--search-forward-within-sexp (concat "(" type))
	    (if (looking-at " *)")
		(progn
		  (search-backward "(")
		  (forward-list 1)
		  (forward-char -1)
		  (insert "\n")) ;; <- newline instead of space
	      (search-backward "(")
	      (forward-list 1)
	      (forward-char -1)
	      (newline-and-indent))
	  (forward-list 1)
	  (forward-char -1)
	  (newline-and-indent)
	  (insert "(" type " )")
	  (forward-char -1)))

      (bind-keys
       :map clj-refactor-map
       ("C-'" . hydra-refactor/body))



      (cljr-add-keybindings-with-modifier "C-s-")))

  (defun my/cider-pprint-eval-last-sexp ()
    "Evaluate the sexp preceding point and pprint its value in a popup buffer."
    (interactive)
    (cider--pprint-eval-form (cider-last-sexp))))

(setq server-socket-dir "/tmp/emacs-socket")
;; start the server so clients can connect to it
(require 'server)
;(server-force-stop)
(if (not (server-running-p))
    (progn
      (message "Starting server")
      (server-start)
      (message "Started server"))
  (message "Server already running"))

(section cosmetic
  ;; highlight current line
  (global-hl-line-mode -1)

  (defun set-pulse ()
    (interactive)
    (setq eval-pulse-depth 1))

  (set-pulse)

  (defun pulse-current-line ()
    (interactive)
    (pulse-momentary-highlight-one-line (point)))

  ;; use a pretty font
  (defun use-meslo ()
    (interactive)

    (set-face-attribute 'default nil
			:family "Meslo LG S DZ" :height 110 :weight 'normal :width 'condensed))

  (defun use-cousine ()
    (interactive)
    (set-face-attribute 'default nil
			:family "Cousine" :height 110 :weight 'normal :width 'condensed))

  (defun use-monofur ()
    (interactive)
    (set-face-attribute 'default nil
			:family "Monofur" :height 120 :weight 'normal :width 'extra-condensed))

  ;; (set-face-attribute 'default nil
  ;; 		      :family "PT Mono" :height 110 :weight 'normal :width 'extra-condensed)

  (use-package rainbow-delimiters
    :ensure t
    :commands rainbow-delimiters-mode-enable
    :init
    (add-hook 'paredit-mode-hook #'rainbow-delimiters-mode-enable)
    (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode-enable)
    (add-hook 'clojure-mode #'rainbow-delimiters-mode-enable)
    (add-hook 'json-mode #'rainbow-delimiters-mode-enable))

  (use-package smooth-scrolling
    :ensure t
    :config
    (setq smooth-scroll-margin 5)
    (smooth-scrolling-mode))

  ;; shows current indent level, slow....
  (use-package indent-guide
    :ensure t
    :commands (indent-guide-mode)
    :config
    (progn
      (setq indent-guide-recursive nil)
      (setq indent-guide-char "")))

  ;; show mark
  (use-package visible-mark
    :ensure t
    :commands (visible-mark-mode))

  ;; Display narrowing by graying out the rest
  (use-package fancy-narrow
    :ensure t
    :defer t
    :diminish ""
    :init (add-hook 'prog-mode-hook #'fancy-narrow-mode)
    :commands (fancy-narrow-mode))

  ;; (use-package highlight-sexp
  ;;   :ensure t
  ;;   ;;:init (add-hook 'paredit-mode-hook 'highlight-sexp-mode)
  ;;   :commands (highlight-sexp-mode)
  ;;   :diminish ""
  ;;   :config
  ;;   (setq hl-sexp-background-color "#2b2b2ba"))

  ;; color parens of current expression by nesting depth
  (use-package highlight-parentheses
    :ensure t
    :diminish ""
    :config
    (global-highlight-parentheses-mode +1)
    (progn

      (setq hl-paren-background-colors
	    (list
	     (hsl 0.1 0.0 0.25)
	     (hsl 0.1 0.0 0.2)
	     (hsl 0.1 0.0 0.15)
	     (hsl 0.1 0.0 0.15)))

      (setq hl-paren-colors
	    (list
	     (hsl 0.14 0.9 0.7)
	     (hsl 0.10 0.7 0.6)
	     (hsl 0.08 0.7 0.5)
	     (hsl 0.07 0.7 0.4)))


      (set-face-attribute
       'hl-paren-face nil
       :underline t
       :overline nil
       :inverse-video nil
       :bold t
       :background nil)

      (setq hl-paren-delay 0.05)))

  ;; color current expression by nesting depth
  (use-package highlight-blocks
    :ensure t
    :commands highlight-blocks-mode
    :config
    (progn
      (eval-when-compile
	(defmacro highlight-blocks--define-faces ()
	  (let ((faces '()))
	    (dotimes (i 9)
	      (push `(defface ,(intern (format "highlight-blocks-depth-%d-face" (1+ i)))
		       '((((class color) (background dark)) :background ,(format "gray%d" (+ 10  (* (mod  i 2) 40) (mod i 2))))
			 (((class color) (background light)) :background ,(format "gray%d" (- 90 (* i 3)))))
		       ,(format "Current nested block face, depth %d." (1+ i))
		       :group 'highlight-blocks-faces)
		    faces))
	    `(progn ,@faces))))

      (highlight-blocks--define-faces)))

  ;; Fontlocking for numbers
  (use-package highlight-numbers
    :ensure t
    :init (add-hook 'prog-mode-hook #'highlight-numbers-mode)
    :defer t
    :commands (highlight-numbers-mode)
    :config
    (setq highlight-numbers-modelist
	  (copy-hash-table
	   (let ((table (make-hash-table :test 'eq)))
	     (puthash 'fasm-mode 'do-not-use table)
	     (puthash 'c-mode
		      (rx (and
			   symbol-start
			   (or (and (+ digit)
				    (opt (and (any "eE")
					      (opt (any "-+"))
					      (+ digit))))
			       (and "0"
				    (any "xX")
				    (+ hex-digit)))
			   (opt (or "f" "F"
				    "u" "U"
				    "l" "L"
				    "ll" "lL" "Ll" "LL"
				    "ul" "uL" "Ul" "UL"
				    "lu" "lU" "Lu" "LU"
				    "ull" "ulL" "uLl" "uLL" "Ull" "UlL" "ULl" "ULL"
				    "llu" "llU" "lLu" "lLU" "Llu" "LlU" "LLu" "LLU"))
			   symbol-end))
		      table)
	     (puthash 'c++-mode
		      (rx (and
			   symbol-start
			   (or (and (+ digit)
				    (opt (and (any "eE")
					      (opt (any "-+"))
					      (+ digit))))
			       (and "0"
				    (any "xX")
				    (+ hex-digit)))
			   (opt (and (any "_" "A-Z" "a-z")
				     (* (any "_" "A-Z" "a-z" "0-9"))))
			   symbol-end))
		      table)
	     (puthash 'emacs-lisp-mode
		      (rx (and
			   (or (and symbol-start
				    (opt (any "-+"))
				    (+ digit)
				    (opt (or (and (any "eE")
						  (opt (any "-+"))
						  (+ digit))
					     (and "."
						  (opt (and (+ digit)
							    (opt (and
								  (any "eE")
								  (opt (any "-+"))
								  (+ digit)))))))))
			       (and "#"
				    symbol-start
				    (or (and (any "bB")
					     (opt (any "-+"))
					     (+ (any "01")))
					(and (any "oO")
					     (opt (any "-+"))
					     (+ (any "0-7")))
					(and (any "xX")
					     (opt (any "-+"))
					     (+ hex-digit)))))
			   symbol-end))
		      table)
	     (puthash 'scheme-mode
		      (rx (and
			   (or (and symbol-start
				    (opt (any "-+"))
				    (+ digit)
				    (opt (or (and (any "eE")
						  (opt (any "-+"))
						  (+ digit))
					     (and "."
						  (opt (and (+ digit)
							    (opt (and
								  (any "eE")
								  (opt (any "-+"))
								  (+ digit)))))))))
			       (and "#"
				    symbol-start
				    (or (and (any "bB")
					     (opt (any "-+"))
					     (+ (any "01")))
					(and (any "oO")
					     (opt (any "-+"))
					     (+ (any "0-7")))
					(and (any "xX")
					     (opt (any "-+"))
					     (+ hex-digit)))))
			   symbol-end))
		      table)
	     table))))

  (defface highlight-quoted-symbol
    '((t :inherit font-lock-keyword-face :foreground "Deepskyblue3"))
    "Face to highlight Lisp quotes."
    :group 'highlight-quoted)

  (use-package highlight-quoted
    :ensure t
    :init (add-hook 'prog-mode-hook #'highlight-quoted-mode)
    :commands (highlight-quoted-mode))

  ;; highlight quoted blocks
  (use-package highlight-stages
    :ensure t
    :diminish ""
    :commands highlight-stages-mode
    :config
    (progn

      (setq highlight-stages-highlight-priority 0)

      (defun highlight-stages-clojure-escape-matcher (&optional limit)
	(when (highlight-stages--search-forward-regexp "~@?" limit)
	  (set-match-data
	   (list (point)
		 (progn (ignore-errors (forward-sexp 1)) (point))))
	  t))

      (add-to-list
       'highlight-stages-matcher-alist
       '(clojure-mode
	 highlight-stages-lisp-quote-matcher . highlight-stages-clojure-escape-matcher))

      (highlight-stages-global-mode -1)))

  (use-package highlight-defined
    :ensure t
    :commands highlight-defined-mode))

(use-package haskell-mode
  :ensure t
  ;:mode "\\.hs$" haskell-mode
  :config
  (progn
    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t)
     '(haskell-interactive-popup-error))
    (bind-keys
     :map haskell-mode-map
     ("C-c C-l" . haskell-process-load-file)
     ("C-c C-j"     . haskell-interactive-bring)
     ("C-c C-t" . haskell-process-do-type)
     ("C-c C-i" . haskell-process-do-info)
     ("C-c C-c" . haskell-process-cabal-build)
     ("C-c C-o" . haskell-interactive-mode-clear)
     ("C-c C-k" . haskell-process-load-file
      )
     ("C-c C-z" . haskell-interactive-switch)
     ("C-c c"   . haskell-process-cabal)
     ("SPC"     . self-insert-command))

    (add-hook 'haskell-mode-hook
	      (lambda ()
		(bind-keys
		 :map interactive-haskell-mode-map
		 ("C-c C-z" . haskell-interactive-switch-back))))))

(defun column (point)
  (save-excursion (goto-char point) (current-column)))

(defun align-to (col)
  (fixup-whitespace)
  (let ((current-col (column (point))))
    (message (concat "align-to " col " " current-col))
    (cond
     ((= col current-col)
      nil)
     ((> col current-col)
      (insert (format (format "%%%ss" (- col current-col)) " ")))
     ((< col current-col)
      (delete-region (- (point) (- current-col col))
		     (point))))))

(defun mc/align-min ()
  (interactive)
  (setq mc--min-col (column (point)))
  (mc/for-each-fake-cursor
   (message "%s" cursor)
   (setq mc--min-col (min mc--min-col (column (second cursor)))))
  (message (format "%s" mc--min-col))
  (mc/execute-command-for-all-cursors (align-to mc--min-col)))

(use-package synth-alias)

(use-package circe :ensure t)


(use-package synth-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Post init

(setq eval-pulse-depth 1)

(setq debug-on-error nil)

(defconst init-duration (time-to-seconds (time-since init-start)))




(setq comint-redirect-verbose nil)




(defun dendrite-connect ()
  (interactive)
  (geiser-connect 'chicken "localhost" 6060))

(setq geiser-repl-startup-time 2000)
(setq geiser-connection-timeout 2000)

(defun clear-messages ()
  (interactive)
  (with-current-buffer "*Messages*"
    (read-only-mode -1)
    (kill-region (point-min)
		 (point-max))
    (read-only-mode +1)))



(autoload 'cider--make-result-overlay "cider-overlays")

(defun endless/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

;; (advice-add 'eval-region :around
;;             (lambda (f beg end &rest r)
;;               (endless/eval-overlay
;;                (apply f beg end r)
;;                end)))

;; (advice-add 'eval-last-sexp :filter-return
;;             (lambda (r)
;;               (endless/eval-overlay r (point))))

;; (advice-add 'eval-defun :filter-return
;;             (lambda (r)
;;               (endless/eval-overlay
;;                r
;;                (save-excursion
;;                  (end-of-defun)
;;                  (point)))))

;; (use-package flycheck-tip
;;   :config
;;   (progn
;;     (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
;;     (flycheck-pos-tip-mode 1)))

(use-package paradox
  :ensure t
  :commands (paradox-list-packages)
  :bind (("C-c a p" . paradox-list-packages)
	 ("C-c a P" . paradox-upgrade-packages)))

(defun my/dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive
   (list
    (let ((print-level nil)
          (minibuffer-history-position 0)
          (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
      (unwind-protect
          (read-from-minibuffer
           "Command: " (prin1-to-string (nth 0 command-history))
           read-expression-map t
           (cons 'command-history 0))

        ;; If command was added to command-history as a
        ;; string, get rid of that.  We want only
        ;; evaluable expressions there.
        (if (stringp (car command-history))
            (setq command-history (cdr command-history)))))))
  (dolist (filename (dired-get-marked-files))
    (with-current-buffer (find-file-noselect filename)
      (if (symbolp command)
          (call-interactively command)
        (eval command)))))

(use-package dired
  :config
  (bind-keys
   :map dired-mode-map
   ("C-c M-x" . my/dired-do-command)))

;; (use-package auto-revert
;;   :diminish "")

(use-package swiper
  :ensure t
  :bind ("s-o" . swiper)
  :diminish "")

(use-package browse-url-dwim
  :ensure t
  :diminish ""
  :bind
  (("C-c g g" . browse-url-dwim-guess)
   ("C-c g u" . browse-url-dwim)
   ("C-c g s" . browse-url-dwim-search)))

(use-package sr-speedbar
  :ensure t)

(use-package slime
  :ensure t
  :commands slime-mode
  :config (setq inferior-lisp-program "sbcl"))

(use-package jdee
  :ensure t
  ;;:mode "\\.java$" jdee-mode
  :commands jdee-mode
  :config
  (setq
   jdee-server-dir (concat user-emacs-directory "lib/jdee")))

(use-package which-key :ensure t)

(which-key-setup-side-window-right)


(defun my/drag-stuff-left (arg)
  (interactive "p")
  (let ((p (paredit-mode)))
    (paredit-mode -1)
    (drag-stuff-left arg)
    (when p (paredit-mode +1))))


(defun drag-sexp-left (&optional arg)
  "Drag sexp under point left ARG sexps."
  (interactive "p")
  (transpose-sexps (- arg)))

(defun drag-sexp-right (&optional arg)
  "Drag sexp under point left ARG sexps."
  (interactive "p")
  (transpose-sexps arg))

(use-package drag-stuff
  :ensure t
  :diminish ""
  :commands drag-stuff-mode
  :bind
  (:map drag-stuff-mode-map
	("<s-up>" . drag-stuff-up)
	("<s-left>" . drag-stuff-left)
	("<s-right>" . drag-stuff-right)
	("<s-down>" . drag-stuff-down)))

(drag-stuff-global-mode t)

;; (defun hub-status (hash)
;;   (let* ((out (s-trim (shell-command-to-string (concat "hub ci-status " hash))))
;; 	 (s (pcase out
;; 	     ("success"   "OK  ")
;; 	     ("error"     "ERR ")
;; 	     ("failure"   "FAIL")
;; 	     ("pending"   "... ")
;; 	     ("no status" "    ")
;; 	     (_ (concat "? " out)))))
;;     (message "%s %s" hash s)
;;     s
;;     ))


;; (defconst a (start-process "1" "*scratch*" "hub"  "ci-status" "1234" ))



;; (shell-command (concat "hub ci-status " "1234" " &"))

;; (hub-status "12345")

;; (cl-defun magit-log-wash-rev (style abbrev)
;;   (when (derived-mode-p 'magit-log-mode)
;;     (cl-incf magit-log-count))
;;   (looking-at (pcase style
;;                 (`log        magit-log-heading-re)
;;                 (`cherry     magit-log-cherry-re)
;;                 (`module     magit-log-module-re)
;;                 (`reflog     magit-log-reflog-re)
;;                 (`stash      magit-log-stash-re)
;;                 (`bisect-vis magit-log-bisect-vis-re)
;;                 (`bisect-log magit-log-bisect-log-re)))
;;   (magit-bind-match-strings
;;       (hash msg refs graph author date gpg cherry _ refsub side) nil
;;     (let ((align (not (member "--stat" (cadr magit-refresh-args))))
;;           (non-graph-re (if (eq style 'bisect-vis)
;;                             magit-log-bisect-vis-re
;;                           magit-log-heading-re)))
;;       (magit-delete-line)
;;       ;; `git reflog show' output sometimes ends with an incomplete
;;       ;; element (which has no basis in the data stored in the file).
;;       (when (and (eq style 'reflog) (not date))
;;         (cl-return-from magit-log-wash-rev t))
;;       (magit-insert-section section
;;         (commit hash)
;;         (pcase style
;;           (`stash      (setf (magit-section-type section) 'stash))
;;           (`module     (setf (magit-section-type section) 'module-commit))
;;           (`bisect-log (setq hash (magit-rev-parse "--short" hash))))
;;         (when cherry
;;           (when (and (derived-mode-p 'magit-refs-mode)
;;                      magit-refs-show-commit-count)
;;             (insert (make-string magit-refs-indent-cherry-lines ?\s)))
;;           (insert (propertize cherry 'face (if (string= cherry "-")
;;                                                'magit-cherry-equivalent
;;                                              'magit-cherry-unmatched)))
;;           (insert ?\s))
;;         (when side
;;           (insert (propertize side 'face (if (string= side "<")
;;                                              'magit-cherry-equivalent
;;                                            'magit-cherry-unmatched)))
;;           (insert ?\s))
;;         (when align
;;           (insert (propertize hash 'face 'magit-hash) ?\s))
;;         (when graph
;;           (insert graph))
;;         (unless align
;;           (insert (propertize hash 'face 'magit-hash) ?\s))
;;         (insert
;;          (propertize (hub-status hash) 'face 'magit-hash))
;;         (when (and refs (not magit-log-show-refname-after-summary))
;;           (insert (magit-format-ref-labels refs) ?\s))
;;         (when refsub
;;           (insert (format "%-2s " (1- magit-log-count)))
;;           (insert (magit-reflog-format-subject
;;                    (substring refsub 0 (if (string-match-p ":" refsub) -2 -1)))))
;;         (when msg
;;           (insert (propertize msg 'face
;;                               (pcase (and gpg (aref gpg 0))
;;                                 (?G 'magit-signature-good)
;;                                 (?B 'magit-signature-bad)
;;                                 (?U 'magit-signature-untrusted)
;;                                 (?X 'magit-signature-expired)
;;                                 (?Y 'magit-signature-expired-key)
;;                                 (?R 'magit-signature-revoked)
;;                                 (?E 'magit-signature-error)))))
;;         (when (and refs magit-log-show-refname-after-summary)
;;           (insert ?\s)
;;           (insert (magit-format-ref-labels refs)))
;;         (insert ?\n)
;;         (when (memq style '(log reflog stash))
;;           (goto-char (line-beginning-position))
;;           (when (and refsub
;;                      (string-match "\\`\\([^ ]\\) \\+\\(..\\)\\(..\\)" date))
;;             (setq date (+ (string-to-number (match-string 1 date))
;;                           (* (string-to-number (match-string 2 date)) 60 60)
;;                           (* (string-to-number (match-string 3 date)) 60))))
;;           (save-excursion
;;             (backward-char)
;;             (magit-log-format-margin author date)))
;;         (when (and (eq style 'cherry)
;;                    (magit-buffer-margin-p))
;;           (save-excursion
;;             (backward-char)
;;             (apply #'magit-log-format-margin
;;                    (split-string (magit-rev-format "%aN%x00%ct") "\0"))))
;;         (when (and graph
;;                    (not (eobp))
;;                    (not (looking-at non-graph-re)))
;;           (when (looking-at "")
;;             (magit-insert-heading)
;;             (delete-char 1)
;;             (magit-insert-section (commit-header)
;;               (forward-line)
;;               (magit-insert-heading)
;;               (re-search-forward "")
;;               (backward-delete-char 1)
;;               (forward-char)
;;               (insert ?\n))
;;             (delete-char 1))
;;           (if (looking-at "^\\(---\\|\n\s\\|\ndiff\\)")
;;               (let ((limit (save-excursion
;;                              (and (re-search-forward non-graph-re nil t)
;;                                   (match-beginning 0)))))
;;                 (unless (magit-section-content magit-insert-section--current)
;;                   (magit-insert-heading))
;;                 (delete-char (if (looking-at "\n") 1 4))
;;                 (magit-diff-wash-diffs (list "--stat") limit))
;;             (when align
;;               (setq align (make-string (1+ abbrev) ? )))
;;             (when (and (not (eobp)) (not (looking-at non-graph-re)))
;;               (when align
;;                 (setq align (make-string (1+ abbrev) ? )))
;;               (while (and (not (eobp)) (not (looking-at non-graph-re)))
;;                 (when align
;;                   (save-excursion (insert align)))
;;                 (magit-make-margin-overlay)
;;                 (forward-line))
;;               ;; When `--format' is used and its value isn't one of the
;;               ;; predefined formats, then `git-log' does not insert a
;;               ;; separator line.
;;               (save-excursion
;;                 (forward-line -1)
;;                 (looking-at "[-_/|\\*o. ]*"))
;;               (setq graph (match-string 0))
;;               (unless (string-match-p "[/\\]" graph)
;;                 (insert graph ?\n))))))))
;;   t)
(provide 'init)

(use-package fish-mode :ensure t)

(use-package glsl-mode :ensure t)

(use-package rust-mode :ensure t)

(use-package string-inflection :ensure t
  :bind
  (("C-c c c" . string-inflection-camelcase)
   ("C-c c k" . string-inflection-kebab-case)
   ("C-c c n" . string-inflection-all-cycle)))

backup-directory-alist

(defun string-inflection-underscore-function (str)
  "FooBar => foo_bar"
  (let ((case-fold-search nil))
    (setq str (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1_\\2" str))
    (setq str (replace-regexp-in-string "-" "_" str)) ; FOO-BAR => FOO_BAR
    (setq str (replace-regexp-in-string (rx ".") "_" str)) ; FOO-BAR => FOO_BAR
    (setq str (replace-regexp-in-string "_+" "_" str))
    (downcase str)))


(defun string-inflection-dot-case-function (str)
  "foo_bar => foo-bar"
  (let ((case-fold-search nil))
    (setq str (string-inflection-underscore-function str))
    (setq str (replace-regexp-in-string "_" "." str))))

(defun string-inflection-dot-case ()
  "foo-bar format"
  (interactive)
  (insert (string-inflection-dot-case-function (string-inflection-get-current-word t))))



(defhydra my/string-inflection (:color pink)
  ("SPC" string-inflection-all-cycle "cycle")
  ("." string-inflection-dot-case "dot.case")
  ("c" string-inflection-camelcase "CamelCase")
  ("-" string-inflection-kebab-case "kebab-case")
  ("_" string-inflection-underscore "UNDER_SCORE")
  ("j" string-inflection-java-style-cycle "cycle java style")
  ("q" nil "cancel" ))

(bind-key "C-c c SPC" 'my/string-inflection/body)

;; (use-package scala-mode
;;   :ensure t
;;   :interpreter
;;   ("scala" . scala-mode))


(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package go-mode
  :ensure t
  :pin melpa-stable)


;;;; monkey patches:

(defun helm-do-ag--helm ()
  (let ((search-dir (if (not (helm-ag--windows-p))
                        helm-ag--default-directory
                      (if (helm-do-ag--target-one-directory-p helm-ag--default-target)
                          (car helm-ag--default-target)
                        helm-ag--default-directory))))
    (helm-attrset 'name (helm-ag--helm-header search-dir)
                  helm-source-do-ag)
    (helm :sources '(helm-source-do-ag) :buffer "*helm-ag*" :keymap helm-do-ag-map
          :input (or
		  (helm-ag--marked-input t)
		  (thing-at-point 'symbol) ; <-
					;(helm-ag--insert-thing-at-point helm-ag-insert-at-point)
		  )
          :history 'helm-ag--helm-history)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("083545312c43a01f9b7e8ecf4e1ae067e907ae79f3e4b385679579ca9e78f5ab" "ba9823e6a937d326822bea7a00c3952c6ae872c39d311eede41a041037549ac7" "4980e5ddaae985e4bae004280bd343721271ebb28f22b3e3b2427443e748cd3f" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "50598275d5ba41f59b9591203fdbf84c20deed67d5aa64ef93dd761c453f0e98" "91aecf8e42f1174c029f585d3a42420392479f824e325bf62184aa3b783e3564" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(haskell-interactive-popup-error nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(package-selected-packages
   (quote
    (go-mode company-restclient ack-and-a-half diminish ensime string-inflection rust-mode glsl-mode fish-mode drag-stuff which-key jdee slime sr-speedbar browse-url-dwim swiper paradox circe haskell-mode highlight-defined highlight-stages highlight-quoted highlight-numbers highlight-blocks highlight-parentheses highlight-sexp fancy-narrow visible-mark indent-guide smooth-scrolling rainbow-delimiters clj-refactor flycheck-joker cider-eval-sexp-fu align-cljlet clojure-mode-extra-font-locking cider clojure-snippets clojure-mode esup image+ evil-lisp-state evil-leader flycheck skewer-mode json-mode git-link browse-at-remote git-timemachine magithub magit geiser yaml-mode window-number restclient dockerfile-mode yasnippet auto-highlight-symbol goto-last-change elisp-slime-nav undo-tree git-gutter smart-mode-line company-quickhelp helm-company company helm-projectile helm-ag projectile perspective ag aggressive-indent paxedit paredit change-inner multiple-cursors expand-region ido-vertical-mode ido-ubiquitous flx-ido idomenu smex buffer-move zoom-window avy hydra key-chord f s dash use-package)))
 '(paradox-github-token t)
 '(projectile-test-prefix-function (function my/projectile-test-prefix))
 '(projectile-test-suffix-function (function my/projectile-test-suffix)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun my/small-font ()
  (interactive)
  (set-face-attribute 'default nil :family "Fira Code Light" :height 100 :weight 'light :width 'extra-condensed)
  (set-frame-parameter nil 'fullscreen 'maximized)
  )

(defun my/large-font ()
  (interactive)
  (set-face-attribute 'default nil :family "Fira Code Light" :height 200 :weight 'light :width 'extra-condensed)
  (set-frame-parameter nil 'fullscreen 'maximized)
  )

(defun my/huge-font ()
  (interactive)
  (set-face-attribute 'default nil :family "Fira Code Light" :height 300 :weight 'light :width 'extra-condensed)
  (set-frame-parameter nil 'fullscreen 'maximized))

(defun hide ()
  (interactive)
  (hs-hide-all))

(defun show ()
  (interactive)
  (hs-show-all))



(defun insert-random-uuid ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d
`
WARNING: this is a simple implementation. The chance of generating the same UUID is much higher than a robust algorithm.."
  (interactive)
  (insert
   (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 6))
           (random (expt 16 6)) ) ) )

(defun add-clj-format-before-save ()
  (interactive)
  (add-hook 'before-save-hook
            'cider-format-buffer
            t
	    t))
