(defconst init-start (current-time))

(package-initialize)

;; make sure we can use packages
(or (package-installed-p 'use-package)
    (package-install 'use-package))

;; only use use-package.el at compile-time
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

; (package-refresh-contents)

;; put libs that cannot be loaded as packages on the load path
(add-to-list 'load-path (concat user-emacs-directory "lib"))

;; load basic setup
(require 'synth-setup)

;; load personal stuff
(use-package synth-utils)

;; osx specific stuff
(use-package platform-osx)

;; modern list library
(use-package dash
  :ensure t
  :config (dash-enable-font-lock))

;; modern string library
(use-package s :ensure t)

;; modern file handling library
(use-package f :ensure t)

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

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; were to find themes
(add-to-list 'custom-theme-load-path (f-join user-emacs-directory "themes"))
(load-theme 'synth t)


(defun open-in-iterm ()
  (interactive)
  (let* ((f (buffer-file-name))
	(d (when f
	     (or (and (f-dir? f) f)
		 (f-parent f)))))
    (when d
      (shell-command
       (format "open -b com.googlecode.iterm2 %s" d)))))

(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-mode +1)
    (setq key-chord-two-keys-delay 0.05)))

(use-package hydra
  :ensure t)

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defhydra hydra-scale (global-map "<f2>")
  "zoom"
  ("+" text-scale-increase "increase")
  ("-" text-scale-decrease "decrease")
  ("1" text-scale-reset "reset")
  ("q" nil "cancel"))

(defun pulse-current-line ()
  (interactive)
  (pulse-momentary-highlight-one-line (point)))

;; undo/redo window configurations
(use-package winner
  :demand t
  :config (winner-mode +1)
  :bind (("C-c b" . winner-undo)
	 ("C-c f" . winner-redo)))

(defhydra hydra-win (global-map "C-c w")
  "zoom"
  ("f" winner-undo "undo")
  ("j" winner-redo "redo")
  ("q" nil "cancel"))

(use-package smooth-scrolling
  :ensure t
  :config
  (setq smooth-scroll-margin 5))

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
    ;; use fuzze matching for ido
    (use-package flx-ido :ensure t)
    (use-package ido-ubiquitous :ensure t)
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
    (setq ido-vertical-define-keys 'C-n-and-C-p-only)))

(use-package ido-completing-read+
  :ensure t)

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

;; use a pretty font
(defun use-meslo ()
  (set-face-attribute 'default nil
                      :family "Meslo LG S DZ" :height 110 :weight 'normal :width 'condensed))

(defun use-cousine ()
  (set-face-attribute 'default nil
                      :family "Cousine" :height 110 :weight 'normal :width 'condensed))

(defun use-monofur ()
  (set-face-attribute 'default nil
                      :family "Monofur" :height 120 :weight 'normal :width 'extra-condensed))

					;(use-cousine) ; ;
					;(use-monofur) ; ;

(set-face-attribute 'default nil
		    :family "PT Mono" :height 110 :weight 'normal :width 'extra-condensed)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'paredit-mode-hook #'rainbow-delimiters-mode-enable)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode-enable)
  (add-hook 'clojure-mode #'rainbow-delimiters-mode-enable))

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

(defvar mc--insert-character-char ?a)

(defun mc--insert-character-and-increase ()
  (interactive)
  (insert (char-to-string mc--insert-character-char))
  (setq mc--insert-character-char (1+ mc--insert-character-char)))

(defun mc/insert-character (arg)
  "Insert increasing numbers for each cursor, starting at 0 or ARG."
  (interactive "P")
  (setq mc--insert-character-char (or arg ?a))
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor #'mc--insert-character-and-increase cursor)))




(use-package multiple-cursors
  :ensure t
  :init (setq mc/list-file (f-join user-emacs-directory "etc" "multiple-cursors-prefs.el"))
  :bind
  (("C-c m h" . mc-hide-unmatched-lines-mode)
   ("C-c m a" . mc/mark-all-like-this-dwim)
   ("C-c m d" . mc/mark-all-symbols-like-this-in-defun)
   ("s-]" . mc/mark-next-like-this)
   ("s-[" . mc/mark-previous-like-this)
   ("s-{" . mc/unmark-previous-like-this)
   ("s-}" . mc/unmark-next-like-this)
   ("s-i" . mc/insert-numbers)
   ("C-c m a" . mc/insert-character)
   ("C-c m i" . mc/insert-numbers))
  :config
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
  :commands paredit-mode
  :diminish "()"
  :config
  (progn
    ;; TODO extract useful stuff
    (load-file (f-join user-emacs-directory "lib/live-paredit.el"))
    (add-hook 'paredit-mode-hook
	      (lambda () (hs-minor-mode 1)))
    ;; Inverse M-(
    (defun paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))
    (key-chord-define paredit-mode-map "()" #'paredit-backward-up)

    (defun synth-paredit-forward-down ()
      (interactive)
      (unwind-protect
	  (let (retval)
	    (condition-case ex
		(paredit-forward-down)
	      ('error (paredit-forward-up)))
	    retval)))

    (bind-keys
     :map paredit-mode-map
     ("M-h"       . live-paredit-backward-kill-sexp)
     ("M-p"       . backward-paragraph)
     ("M-n"       . forward-paragraph)
     ("M-<left>"  . backward-word)
     ("M-<right>" . forward-word)
     ("s-<left>"  . backward-sexp)
     ("s-<right>" . live-paredit-forward-down)
     ("C-)"       . paredit-forward-slurp-sexp)
     ("C-("       . paredit-backward-slurp-sexp)

     ("M-("       . paredit-forward-barf-sexp)
     ("M-)"       . paredit-backward-barf-sexp)

     ("s-("       . paredit-forward)
     ("s-)"       . paredit-backward)

     ("C-c l l"   . align-cljlet)
     ("C-c p"     . nil)
     ("C-c l k"   . paredit-splice-sexp-killing-forward)
     ("C-c l w"   . paredit-splice-sexp-killing-backward)
     ("C-c l t"   . fill-paragraph)
     ("C-c l j"   . live-paredit-forward-slurp-sexp-neatly)
     ("C-M-j"     . live-paredit-forward-slurp-sexp-neatly)
     ("C-M-z"     . align-cljlet)
     ("M-S"       . paredit-split-sexp)
     ("M-s"       . paredit-splice-sexp)
     ("M-j"       . paredit-join-sexps)
     ("M-T"       . transpose-sexps)
     ("M-P"       . live-paredit-previous-top-level-form)
     ("M-N"       . live-paredit-next-top-level-form)
     ("C-M-f"     . live-paredit-forward)
     ("M-q"       . live-paredit-reindent-defun)
     ("M-d"       . live-paredit-forward-kill-sexp)
     ("M-k"       . live-paredit-backward-kill-s)
     ("M-\\"      . live-paredit-delete-horizontal-space))))

(use-package aggressive-indent
  :ensure t
  :diminish "=>")

;; better grep, requires ag to be installed
(use-package ag :ensure t)

(use-package perspective
  :ensure t
  :config
  (persp-mode +1))

;; project oriented commands
(use-package projectile
  :ensure t
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
    (setq projectile-mode-line-lighter "ⓟ")
    (setq projectile-mode-line "ⓟ") ;; smart modeline already shows the current projectile project
    (setq projectile-completion-system 'ido)

    (bind-keys :map projectile-command-map
     ("r" .   projectile-replace))

    (key-chord-define projectile-mode-map "jk" #'projectile-switch-project)
    (key-chord-define projectile-mode-map "df" #'projectile-find-file-dwim)
    (key-chord-define projectile-mode-map "pt" #'projectile-toggle-between-implementation-and-test)))

(use-package persp-projectile
  :config
  (bind-keys
   :map persp-mode-map
   ("C-c w q" . persp-switch-quick)
   ("C-c w k" . persp-kill)))

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
    (use-package helm-ag :ensure t)
    (setq helm-split-window-default-side 'below
	  helm-split-window-in-side-p t
	  helm-move-to-line-cycle-in-source t
	  helm-display-header-line nil
	  helm-candidate-separator "ㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡㅡ")
    (helm-autoresize-mode +1)))

;; project oriented helm commands
(use-package helm-projectile
  :ensure t
  :config
  (progn
    (use-package ack-and-a-half :ensure t)
    (bind-keys
     :map projectile-command-map
     ("f" .   helm-projectile-find-file-dwim)
     ("s s" . helm-projectile-ag)
     ("s g" . helm-projectile-grep)
     ("s a" . helm-projectile-ack))))


(defun my/company-transformer (candidates)
  (-distinct candidates))

;; completion framework
(use-package company
  :ensure t
  :diminish "Ⓒ "
  :config
  (progn
    (setq company-minimum-prefix-length 2
	  company-idle-delay 0.3
	  company-selection-wrap-around t
	  company-async-timeout 0.05
	  company-async-timeout 2
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

    (bind-keys
     :map company-mode-map
     ("C-:" . helm-company))
    (bind-keys
     :map company-active-map
     ("C-n" . company-select-next-or-abort)
     ("C-p" . company-select-previous-or-abort)
     ("C-:" . helm-company)
     ("C-;" . helm-company))
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
  :bind (("C-:" . helm-company)
	 ("C-;" . helm-company)))

;; show docs for selected completion in popup window
(use-package company-quickhelp
  :ensure t
  :commands (company-quickhelp-mode)
  :config
  (progn
    (company-quickhelp-mode -1)
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
  :diminish "± "
  :config
  (progn
    (global-git-gutter-mode +1)
    (setq git-gutter:unchanged-sign " ")
    (setq git-gutter: " ")))

;; awesome undo/redo
(use-package undo-tree
  :ensure t
  :diminish "↶ "
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-auto-save-history nil)
    (bind-keys
     :map undo-tree-visualizer-mode-map
     ("C-g" . kill-buffer-and-window))))

;; show argument lists in echo area
(use-package eldoc
  :diminish "ⓛ "
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

(use-package avy
  :ensure t
  :bind
  (("C-o" . avy-goto-char)
   ("M-g g" . avy-goto-line)))

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
;  :commands (yas-minor-mode yas-global-mode)
  :init
  (setq yas-snippet-dirs
	(list
	 (f-join user-emacs-directory "etc" "snippets")
	 (package-desc-dir (cadr (assoc 'yasnippet package-alist)))
	 (package-desc-dir (cadr (assoc 'clojure-snippets package-alist)))))
  :config
  (progn
    (yas-global-mode +1)
    (setq yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-no-prompt) )
    (add-hook 'clojure-mode-hook
	      (lambda ()
		(yas-minor-mode +1)
		;(yas-activate-extra-mode 'clojure-mode)
		))
    (bind-keys
     :map yas-minor-mode-map
     ("C-<tab>" . yas-expand-from-trigger-key))))

(use-package dockerfile-mode
  :ensure t
  :commands (dockerfile-mode))

(use-package restclient
  :ensure t
  :commands (restclient-mode)
  :config
  (use-package company-restclient :ensure t))

(use-package hardcore-mode
  :ensure t)


(use-package eval-pulse
  :init
  (setq eval-pulse-depth 1)
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



(use-package geiser
  :ensure t
  :config
  (progn

    (defun geiser-save-and-load-buffer ()
      (interactive)
      (save-buffer)
      (geiser-load-current-buffer))

    (geiser-syntax--keywords)

    (setq geiser-active-implementations '(chicken racket)
	  geiser-debug-jump-to-debug-p nil ; don't jump to error buffer
	  geiser-debug-show-debug-p nil ; don't jump to error buffer

	  )

    (require 'geiser-repl)
    (eval-after-load "init.el"
      (bind-keys
       :map geiser-repl-mode-map
       ("C-c C-o" . geiser-repl-clear-buffer)))

    (add-hook 'geiser-mode-hook (lambda ()
				  (auto-highlight-symbol-mode +1)
				  (prettify-symbols-mode +1)
				  (bind-keys
				   :map geiser-mode-map
				   ("C-c C-j" . run-chicken)
				   ("C-c m b" . geiser-load-current-buffer)
				   ("C-c C-c" . geiser-repl-interrupt)
				   ("C-c M-c" . geiser-connect)
				   ("C-c C-n" . my/geiser-eval-imports)
				   ("C-x M-h" . my/geiser-eval-to-point))))

    (push '("lambda"  . ?λ) prettify-symbols-alist)))




(font-lock-add-keywords
 'geiser-mode
 `((,(concat "\\<"
	     (rx "./")
	     "[a-z0-9-]+"
	     "\\>")
    0
    font-lock-warning-face)))


(use-package racket-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  :commands (racket-mode)
  :config
  (progn
    (add-hook 'racket-mode-hook
	      (lambda ()
		(paredit-mode +1)
		(aggressive-indent-mode +1)
		(auto-highlight-symbol-mode +1)
					;		(eldoc-mode -1)
		(prettify-symbols-mode +1)
		(push '("lambda"  . ?λ) prettify-symbols-alist)))
    (bind-keys
     :map racket-mode-map
     ("C-c C-k" . racket-run))))

(use-package hideshow
  ;:diminish ""
  :config
  (progn
    (bind-keys
     :map hs-minor-mode-map
     ("M-±"       . hs-toggle-hiding)
     ("<backtab>" . hs-toggle-hiding))))

(use-package hideshow-fringe)

(hideshow-fringe-enable)


(use-package magit
  :ensure t
  :pin melpa-stable
  :commands magit-status
  :bind (("C-x m" . magit-status)
	 ("C-c g b" . magit-blame)
	 ("C-c g s" . magit-branch-popup)
	 ("C-c g v" . my/magit-visit-pull-request-url))
  :config
  (progn
    (setq magit-completing-read-function 'magit-ido-completing-read)
    (remove-hook 'pre-command-hook #'magit-pre-command-hook)

    (defun my/magit-visit-pull-request-url ()
      "Visit the current branch's PR on Github."
      (interactive)
      (browse-url
       (format "https://github.com/%s/pull/new/%s"
	       (replace-regexp-in-string
		"\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
		(magit-get "remote"
			   (magit-get-remote)
			   "url"))
	       (or (magit-get-push-branch)
		   (user-error "No remote branch")))))


    (define-key magit-mode-map "v" #'my/magit-visit-pull-request-url)

    (defun my/magit-display-buffer (buffer)
      (display-buffer
       buffer (if (and (derived-mode-p 'magit-mode)
		       (not (memq (with-current-buffer buffer major-mode)
                              '(magit-process-mode
                                magit-stash-mode
                                magit-revision-mode
                                magit-diff-mode
                                magit-status-mode
				))))
              '(display-buffer-same-window)
	      nil)))

    (setq magit-display-buffer-function 'my/magit-display-buffer))

)

(use-package magit-gitflow
  :ensure t
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))


;; (use-package magit-filenotify
;;   :ensure t
;;   :config
;;   (remove-hook 'magit-mode-hook (lambda () (magit-filenotify-mode +1))))

(use-package browse-at-remote
  :ensure t
  :commands browse-at-remote
  :init (bind-key "C-c g r" 'browse-at-remote/browse))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine
  :init (bind-key "C-c g t" 'git-timemachine))

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

(use-package js2-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :commands js2-mode)

(use-package skewer-mode
  :ensure t
  :commands skewer-mode)

(use-package flycheck
  :ensure t
  :commands flycheck-mode)

(use-package evil-leader
  :ensure t
  :commands evil-leader
  :config
  (progn
    (setq echo-keystrokes 0.02)
    (global-evil-leader-mode +1)
    (evil-leader/set-leader "<SPC>")

    ;(evil-leader/set-key "<SPC>" #'ace-jump-mode)

    (evil-leader/set-key "b" #'helm-buffers-list)
    (evil-leader/set-key "u" 'universal-argument)
    ;; eval
    (evil-leader/set-key-for-mode 'emacs-lisp-mode
      "eE" #'eval-defun-at-point
      "ee" #'eval-last-sexp
      "ef" #'eval-buffer
      "er" #'eval-region)

    ;; cider
    (evil-leader/set-key
      "ep" #'cider-pprint
      "eP" #'cider-pprint-eval-defun-at-point
      "eE" #'cider-eval-defun-at-point
      "ee" #'cider-eval-last-sexp
      "ef" #'cider-load-file
      "er" #'cider-eval-last-sexp-and-replace
      "en" #'cider-eval-ns-form
      "er" #'cider-eval-region)

    ;; ahs
    (evil-leader/set-key-for-mode auto-highlight-symbol-mode
      "ae" #'ahs-edit-mode
      "aj" #'ahs-forward
      "aJ" #'ahs-forward-definition
      "ak" #'ahs-backward
      "aK" #'ahs-backward-definition)

    ;; projectile
    (evil-leader/set-key
      "pt" #'projectile-toggle-between-implementation-and-test
      "ps" #'helm-projectile-ag
      "pp" #'projectile-switch-project
      "pf" #'projectile-find-file-dwim
      "pa" #'projectile-ag
      "pg" #'projectile-grep
      "ph" #'helm-projectile
      "pr" #'helm-projectile-recentf)

    ;; helm
    (evil-leader/set-key
      "hc" #'helm-company
      "ho" #'helm-occur
      "hi" #'helm-imenu
      "hb" #'helm-buffers-list)

    ;; file
    (evil-leader/set-key
      "fs" #'save-buffer
      "fv" #'revert-buffer
      "fr" #'rename-file-and-buffer
      "fk" #'delete-current-buffer-file)

    (evil-leader/set-key
      "fk" #'delete-current-buffer-file)
    ;; paredit
    (evil-leader/set-key
      ")s" #'paredit-forward-slurp-sexp)

    (evil-leader/set-key
      "(s" #'paredit-splice-sexp-killing-forward)
    (evil-leader/set-key
      "tf" #'toggle-frame-fullscreen
      "tl" #'linum-mode
      "ti" #'aggressive-indent-mode
      "tp" #'paredit-mode
      "tr" #'auto-revert-mode)
    ;; narrowing
    (evil-leader/set-key
      "nn" #'narrow-to-region
      "nd" #'narrow-to-defun
      "nw" #'widen)
    ;;
    (evil-leader/set-key "c1" (lambda ()
				(interactive)
				(company-complete-number 1)))))

(use-package evil
  :ensure t
  :commands evil-mode
  :bind
  (("C-c w e" . evil-mode))
  :config
  (progn
    (global-evil-leader-mode +1)
    (bind-keys
     ("<f13>" . evil-mode))
    (evil-define-key 'normal global-map " " nil)))

(use-package evil-lisp-state
  :ensure t
  :commands evil-lisp-state
  :config
  (setq evil-lisp-state-major-modes
	'(emacs-lisp-mode clojure-mode racket-mode scheme-mode)))

(use-package org
  :ensure t
  :config
  (progn
    (bind-keys
     :map org-mode-map
     ("M-." . org-open-at-point))))

(color-theme-synth)

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
    (put-clojure-indent 'time 2)
    (put-clojure-indent 'section 'defun)
    (put-clojure-indent 'letk 1)
    (put-clojure-indent 'fact 'defun)
    (put-clojure-indent 'assoc nil)
    (put-clojure-indent 'match 'defun)
    (put-clojure-indent 'facts 'defun)
    (put-clojure-indent 'future-fact 'defun)
    ;(put-clojure-indent 'facts 'defun)
    ;; fontlock logic vars
    (font-lock-add-keywords
     'clojure-mode
     `((,(concat "\\<"
		 "\\?[a-z0-9-]+"
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
     `((,(regexp-opt (list "GET" "PUT" "POST" "ANY" "OPTIONS") 'words)
	0
	font-lock-preprocessor-face)))

    (font-lock-add-keywords
     'clojure-mode
     `((,(concat "\\<"
		 (rx "./")
		 "[a-z0-9-]+"
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
					(regexp-opt '("=>" "=not=>" "=contains=>") t)
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
    (defun no-cider ()
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
     ("C-x C-e" . no-cider)

     ("C-c C-p" . no-cider)
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
  ("t" cljr-thread "thread")
  ("f" cljr-thread-first-all "thread-first-all")
  ("l" cljr-thread-last-all "thread-last-all")
  ("c" cljr-clean-ns "clean-ns")
  ("u" cljr-unwind-all "unwind")
  ("U" cljr-unwind-all "unwind-all")
  ("l" cljr-move-to-let "to-let")
  ("m" cljr-magic-require-namespaces "requires-namespaces" )
  ("s" cljr-sort-ns "sort ns")
  ("e" cljr-extract-function "extract function")
  ("d" cljr-extract-def "extract def")
  ("r" cljr-remove-unused-requires  "r")
  ("q" nil "cancel"))

(use-package flycheck-clojure :ensure t
  :config
  (flycheck-clojure-setup))

(use-package clojure-snippets
  :ensure t)

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

(use-package cider
  :pin melpa-stable
  :ensure t
  :commands (cider-jack-in cider-mode)
  :init
  (add-hook 'cider-repl-mode-hook
	    (lambda ()
	      (eldoc-mode +1)
	      (paredit-mode -1)
	      (rainbow-delimiters-mode-disable)
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
	      (company-mode +1)
	      (push '(">=" . ?≥) prettify-symbols-alist)
	      ;(push '(alpha . ?α) prettify-symbols-alist)
	      ;(push '(beta . ?β) prettify-symbols-alist)

					;(push '("comp" . ?○) prettify-symbols-alist)
	      (prettify-symbols-mode +1)))
  :config
  (progn
    (require 'clojure-mode)
    (require 'company)
    (require 'cider-utils)

    (add-hook 'cider-repl-mode-hook
	      (lambda ()
		;(cider-turn-on-eldoc-mode)
		;(paredit-mode 1)
		))
    (add-hook 'cider-mode-hook
	      (lambda ()
		;(cider-turn-on-eldoc-mode)
;		(paredit-mode 1)
		))

    (setq cider-prefer-local-resources t
	  cider-mode-line-show-connection nil

	  cider-popup-stacktraces nil
	  cider-popup-stacktraces-in-repl nil

	  cider-show-error-buffer t
	  cider-auto-select-error-buffer nil
	  cider-auto-jump-to-error nil
	  cider-annotate-completion-candidates t
 	  cider-test-show-report-on-success nil
	  cider-prompt-save-file-on-load nil
	  cider-lein-command "lein"
	  cider-lein-parameters "with-profile +power repl"
	  cider-repl-wrap-history t
	  cider-repl-use-clojure-font-lock nil
	  cider-repl-display-help-banner nil
	  nrepl-buffer-name-show-port t
	  nrepl-log-messages nil
	  nrepl-message-buffer-max-size 10000
	  nrepl-port "4555")
    ;; jump to repl buffer on connect cider-repl-pop-to-buffer-on-connect t
    (add-to-list 'same-window-buffer-names "*cider*")

    (bind-keys
     :map cider-mode-map

     ("M-RET"     . cider-doc)
     ("C-c s"   .   my/cider-source)

     ("C-x M-e"   . cider-eval-last-sexp-and-replace)
     ("C-x c c"   . my/cider-eval-last-sexp-comment)
     ("C-c ."     . my/cider-dot)
     ("s-."       . cider-eval-expression-at-point)
     ("C-x M-r"   . cider-eval-last-sexp-to-repl)
     ("C-x C-k"   . cider-eval-buffer)
     ("C-x M-h"   . cider-eval-to-point)
     ("C-c r r"   . my/cider-require-symbol)
     ("C-c C-j"   . cider-jack-in)
     ("C-c C-q"   . cider-quit)
     ("C-c r c"   . cider-connection-browser)
     ("C-c p-p"   . nil) ;; conflict with projectile
     ("C-c C-o"   . my/cider-clear-repl)
     ("C-c C-p"   . cider-pp)
     ("C-c d"     . cider-debug-defun-at-point)
     ("C-c e"   .   my/cider-throw-last-exception)

     ;; Testing
     ("C-c t d"   . cider-test-run-test)
     ("C-c t t"   . cider-test-run-test)
     ("C-c t n"   . cider-test-run-ns-tests)
     ("C-c w t"   . my/cider-show-impl-and-test)
     ("C-c t m"   .   my/cider-midje-load-facts)
     ("C-c t <up>"   .   my/cider-midje-autotest-start)
     ("C-c t <down>"   .   my/cider-midje-autotest-stop))

    (bind-keys
     :map clojure-mode-map
     ("C->"     . cljr-cycle-coll)
     ("s-;"     . my/clojure-toggle-keyword-string)
     ("C-c C-j" . cider-jack-in)
     ("C-,"     . cljr-unwind)
     ("C-."     . cljr-thread)
     ("<f19>"     . cider-eval-last-sexp) ;; as C-x C-e
     ("s-p s-p"   . cider-pp))

    (bind-keys
     :map cider-repl-mode-map
     ("C-c C-o" . cider-repl-clear-buffer))

    (bind-keys
     :map clojurescript-mode-map
     ("C-c j r" . my/cider-cljs-refresh)
     ("C-c j o" . my/cider-cljs-clear))


    (define-key cider-mode-map (kbd "C-x C-q")
      (lambda ()
	(interactive)
	(cider-switch-to-relevant-repl-buffer)
	(cider-repl-clear-buffer)
	(cider-switch-to-last-clojure-buffer)
	(cider-eval-last-sexp)
	(cider-pp)))

    (define-key clojure-mode-map (kbd "C-x s-p")
      (lambda ()
	(interactive)
	(cider-eval-last-sexp)
	(cider-repl-clear-buffer)
	(cider-pp)))

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
	(cider-repl-return)))


    (bind-keys
     :map clojure-mode-map
     ("C-c c f" . cider-figwheel-repl))))


(use-package cider-eval-sexp-fu)

;; refactoring
(use-package clj-refactor
  :ensure t
  :config
  (progn
    (use-package cljr-helm :ensure t)
    ;; (add-hook 'clj-refactor-mode-hook
    ;; 	  (lambda ()
    ;; 	    (add-hook before-save-hook
    ;; 		      (lambda ()
    ;; 			(when cider-mode
    ;; 			  (cljr-remove-unused-requires)
    ;; 			  (cljr-sort-ns)
    ;; 			  (cljr-clean-ns))))))

    (setq cljr-magic-require-namespaces
	  (-distinct (-concat cljr-magic-require-namespaces
			      '(("component" . "com.stuartsierra.component")
				("s" . "schema.core")
				("z" . "clojure.zip")
				("d" . "datomic.api")
				("edn" . "clojure.edn")
				("a" . "clojure.core.async")
				("sh" . "clojure.java.shell")
				("log" . "taoensso.timbre")
				("json" . "cheshire.core")
				("p" . "plumbing.core")))))
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

    (bind-keys
     :map clj-refactor-map
     :prefix "C-c r"
     :prefix-map cljr
     ("m"   . cljr-move-to-let)
     ("l"   . cljr-introduce-let)
     ("e f" . cljr-extract-function)
     ("e d" . cljr-extract-def)
     ("e c" . cljr-extract-constant)
     ("t"   . cljr-thread)
     ("u"   . cljr-unwind)
     ("n s" . cljr-sort-ns)
     ("n c" . cljr-clean-ns)
     ("s"   . cljr-rename-symbol)
     ("r"   . cljr-add-require-to-ns)
     ("i"   . cljr-add-import-to-ns))

    (cljr-add-keybindings-with-modifier "C-s-")))

(defun my/cider-pprint-eval-last-sexp ()
  "Evaluate the sexp preceding point and pprint its value in a popup buffer."
  (interactive)
  (cider--pprint-eval-form (cider-last-sexp)))





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



(defun set-pulse ()
  (interactive)
  (setq eval-pulse-depth 1))

(set-pulse)

;; Display narrowing by graying out the rest
(use-package fancy-narrow
  :ensure t
  :init (add-hook 'prog-mode-hook #'fancy-narrow-mode)
  :commands (fancy-narrow-mode))

(use-package highlight-sexp
  :ensure t
  :commands highlight-sexp-mode
  :diminish ""
  :config
  (progn (add-hook 'paredit-mode-hook 'highlight-sexp-mode)
	 (setq hl-sexp-background-color   "#2b2b2b")

	 ))

(use-package highlight-parentheses
  :ensure t
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

(use-package highlight-blocks
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
       table)))

  )

(defface highlight-quoted-symbol
  '((t :inherit font-lock-keyword-face :foreground "Deepskyblue3"))
  "Face to highlight Lisp quotes."
  :group 'highlight-quoted)

(use-package highlight-quoted
  :ensure t
  :init (add-hook 'prog-mode-hook #'highlight-quoted-mode)
  :commands (highlight-quoted-mode))

(use-package highlight-stages
  :ensure t
  :diminish ""
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
  :ensure t)

;; Move buffers around
(use-package buffer-move
  :ensure t
  :bind
  (("C-c w <right>" . buf-move-right)
   ("C-c w <left>"  . buf-move-left)
   ("C-c w <up>"    . buf-move-up)
   ("C-c w <down>"  . buf-move-down)))

(use-package visible-mark
  :ensure t
  ;;:diminish ""
  :config
  (visible-mark-mode +1))

(use-package indent-guide
  :ensure t
  :config
  (progn
    (setq indent-guide-recursive nil)
    (setq indent-guide-char "¦")))

(use-package travis :ensure t)

(use-package haskell-mode
  :ensure t
  :config
  (progn
    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t)
     '(haskell-interactive-popup-error)
     )



    (bind-keys
     :map haskell-mode-map
     ("C-c C-l" . haskell-process-load-or-reload)
     ("C-c C-j"     . haskell-interactive-bring)
     ("C-c C-t" . haskell-process-do-type)
     ("C-c C-i" . haskell-process-do-info)
     ("C-c C-c" . haskell-process-cabal-build)
     ("C-c C-o" . haskell-interactive-mode-clear)
     ("C-c C-k" . haskell-process-load-or-reload)
     ("C-c C-z" . haskell-interactive-switch)
     ("C-c c"   . haskell-process-cabal)
     ("SPC"     . haskell-mode-contextual-space))

    (add-hook 'haskell-mode-hook
	      (lambda ()
		(bind-keys
		 :map interactive-haskell-mode-map
		 ("C-c C-z" . haskell-interactive-switch-back)))))



  )

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
  (--each (mc/get-all-fake-cursors-state)
    (setq mc--min-col (min mc--min-col (column (second it)))))
  (message (format "%s" mc--min-col))
  (mc/execute-command-for-all-cursors (align-to mc--min-col)))

(use-package synth-alias)
(use-package synth-bindings)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Post init

(setq eval-pulse-depth 1)

(setq debug-on-error nil)

(defconst init-duration (time-to-seconds (time-since init-start)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("50598275d5ba41f59b9591203fdbf84c20deed67d5aa64ef93dd761c453f0e98" "91aecf8e42f1174c029f585d3a42420392479f824e325bf62184aa3b783e3564" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(global-hl-line-mode t)
 '(safe-local-variable-values
   (quote
    ((Geiser-scheme-implementation . gambit)
     (TeX-master . "geiser")))))


; (load-file "/Users/chris/git/geiser/elisp/geiser.el")

 (setq comint-redirect-verbose nil)

(add-hook 'geiser-mode-hook
	  (lambda ()
	    (auto-highlight-symbol-mode +1)
	    (push '("lambda"  . ?λ) prettify-symbols-alist)))


(setq geiser-repl-startup-time 2000)
(setq geiser-connection-timeout 2000)

;; (geiser-connect 'gambit "localhost" 1111 )

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

;; (require 'tcp)

;; (with-current-buffer "* Gambit REPL *"
;;   (process-send-string nil "(+ 1 1)"))


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



(use-package flycheck-tip

  :config
  (progn
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
    (flycheck-pos-tip-mode 1)


    )


  )
