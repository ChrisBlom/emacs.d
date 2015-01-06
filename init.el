;; put libs that cannot be loaded as packages on the load path
(add-to-list 'load-path (concat user-emacs-directory "lib"))

;; load basic setup
(require 'synth-setup)

;; make sure we can use packages
(package-initialize)
(package-install 'use-package)
(package-initialize)

;; organized package loading framework
(require 'use-package)

;; modern list library
(use-package dash
  :ensure t
  :config
  (dash-enable-font-lock))

;; modern string library
(use-package s :ensure t)

;; modern file handling library
(use-package f :ensure t)

(f-mkdir user-emacs-directory "backups")
(f-mkdir user-emacs-directory "tmp")

(defun f-tmp-file ( &rest files)
  (let ((tmp-file (apply #'f-join user-emacs-directory "tmp" files)))
    (f-mkdir (f-parent tmp-file))
    tmp-file))

;; do not save backup files in same dir
(setq
 backup-by-copying t      ; don't clobber symlinks
 vc-make-backup-files t   ; backup versioned files as well
 backup-directory-alist
 `(("." . ,(f-join user-emacs-directory "backups")))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; the default emacs theme is nicknamed 'angry fruit salad'
;; it makes my eyes bleed, i prefer dark themes
(add-to-list 'custom-theme-load-path (f-join user-emacs-directory "themes"))
(load-theme 'synth t)

;; undo/redo window configurations
(use-package winner
  :config (winner-mode 1)
  :bind (("C-c b" . winner-undo)
	 ("C-c f" . winner-redo)))

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
    (use-package flx-ido :ensure t)
    (use-package ido-vertical-mode :ensure t)
    (use-package ido-ubiquitous :ensure t)

    (ido-mode t)
    (ido-vertical-mode +1)
    (ido-ubiquitous-mode +1)
    (flx-ido-mode +1)
    (icomplete-mode +1)

    (setq ido-enable-flex-matching t
	  ;; disable ido faces to see flx highlights.
	  ido-use-faces nil
	  ido-enable-prefix nil
	  ido-create-new-buffer 'always
	  ido-max-prospects 10

	  ido-default-file-method 'selected-window
	  ido-everywhere 1)))

(use-package recentf
  :config
  (progn
    (setq recentf-save-file (f-tmp-file "recentf.el")
	  recentf-max-saved-items 50)
    (recentf-mode t)
    (defun ido-recentf-open ()
      "Use `ido-completing-read' to \\[find-file] a recent file"
      (interactive)
      (if (find-file (ido-completing-read "Find recent file: " recentf-list))
	  (message "Opening file...")
        (message "Aborting")))))

(use-package key-chord
  :ensure t
  :config
  (progn
    (key-chord-mode +1)
    (setq key-chord-two-keys-delay 0.05)))

;; From http://groups.google.com/group/gnu.emacs.help/browse_thread/thread/44728fda08f1ec8f?hl=en&tvc=2
(defun make-repeatable-command (cmd)
  "Returns a new command that is a repeatable version of CMD.
The new command is named CMD-repeat.  CMD should be a quoted
command.

This allows you to bind the command to a compound keystroke and
repeat it with just the final key.  For example:

  (global-set-key (kbd \"C-c a\") (make-repeatable-command 'foo))

will create a new command called foo-repeat.  Typing C-c a will
just invoke foo.  Typing C-c a a a will invoke foo three times,
and so on."
  (fset (intern (concat (symbol-name cmd) "-repeat"))
        `(lambda ,(help-function-arglist cmd) ;; arg list
           ,(format "A repeatable version of `%s'." (symbol-name cmd)) ;; doc string
           ,(interactive-form cmd) ;; interactive form
           ;; see also repeat-message-function
           (setq last-repeatable-command ',cmd)
           (repeat nil)))
  (intern (concat (symbol-name cmd) "-repeat")))


;; use a pretty font
(defun use-meslo ()
  (set-face-attribute 'default nil
                      :family "Meslo LG S DZ" :height 110 :weight 'normal :width 'condensed))

(use-meslo)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'paredit-mode-hook #'rainbow-delimiters-mode-enable))

;; Expand region increases the selected region by semantic units.
(use-package expand-region
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-]") 'er/expand-region)
    (global-set-key (kbd "M-[") 'er/contract-region)))

(use-package multiple-cursors
  :ensure t
  :config
  (progn
    (setq mc/list-file (f-join user-emacs-directory "etc" "multiple-cursors-prefs.el"))
    (global-set-key (kbd "s-]") 'mc/mark-next-like-this)
    (global-set-key (kbd "s-[") 'mc/mark-previous-like-this)
    (global-set-key (kbd "s-{") 'mc/unmark-previous-like-this)
    (global-set-key (kbd "s-}") 'mc/unmark-next-like-this)))

(use-package paredit
  :ensure t
  :init
  (progn
    (add-hook 'lisp-mode-hook #'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook #'paredit-mode))
  :commands paredit-mode
  :diminish "() "
  :config
  (progn
    ;; TODO extract useful stuff
    (load-file (f-join user-emacs-directory "lib/live-paredit.el"))

    ;; Inverse M-(
    (defun paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))

    (key-chord-define paredit-mode-map "()" #'paredit-backward-up)
    (bind-keys
     :map paredit-mode-map
     ("M-<left>"  . backward-word)
     ("M-<right>" . forward-word)
     ("M-w"       . live-paredit-backward-kill-sexp)

     ("C-("       . paredit-forward-slurp-sexp)
     ("C-)"       . paredit-backward-slurp-sexp)

     ("s-)"       . paredit-backward-barf-sexp)
     ("s-("       . paredit-forward-barf-sexp)

     ("M-("       . paredit-wrap-round)
     ("M-)"       . paredit-wrap-round-from-behind)

     ("C-c l l"   . align-cljlet)
     ("C-c l k"   . paredit-splice-sexp-killing-forward)
     ("C-c l w"   . paredit-splice-sexp-killing-backward)
     ("C-c l t"   . fill-paragraph)
     ("C-c l j"   . live-paredit-forward-slurp-sexp-neatly)
     ("C-M-j"     . live-paredit-forward-slurp-sexp-neatly)
     ("C-M-z"     . align-cljlet)
     ("M-S"       . paredit-split-sexp)
     ("M-s"       . paredit-splice-sexp)
     ("M-j"       . paredit-join-sexps)
     ("M-P"       . live-paredit-previous-top-level-form)
     ("M-N"       . live-paredit-next-top-level-form)
     ("C-M-f"     . live-paredit-forward)
     ("M-q"       . live-paredit-reindent-defun)
     ("M-d"       . live-paredit-forward-kill-sexp)
     ("M-k"       . live-paredit-backward-kill)
     ("M-\\"      . live-paredit-delete-horizontal-space)
     ("C-M-i"     . paredit-forward-down)
     ("C-M-n"     . paredit-forward-up)
     ("C-M-p"     . paredit-backward-down)
     ("C-M-u"     . paredit-backward-up)
     ("M-T"       . transpose-sexps)
     ("C-M-k"     . live-paredit-copy-sexp-at-point))

    (use-package projectile
      :ensure t
      :config
      (progn
	(use-package ack-and-a-half :ensure t)
	(projectile-global-mode +1)
	(setq projectile-cache-file (f-tmp-file "projectile" "projectile.cache"))
	(setq projectile-known-projects-file (f-tmp-file "projectile" "projectile-bookmarks.eld"))
	(setq projectile-mode-line-lighter "P")
	(setq projectile-mode-line "P") ;; smart modeline already shows the current projectile project
	(setq projectile-completion-system 'ido)

	(bind-keys
	 :map projectile-mode-map
	 ("C-c p p" . projectile-switch-project))

	)))

  ;; selection framework
  (use-package helm
    :ensure t
    :bind (("M-y" . helm-show-kill-ring)
	   ("C-x C-b" . helm-buffers-list))
    :config
    (progn
      (require 'helm-ring)
      (require 'helm-source)
      (require 'helm-config)
      (require 'helm-adaptive)
      (setq helm-adaptive-history-file (f-tmp-file "helm" "adaptive-history")))))

;; completion framework
(use-package company
  :ensure t
  :diminish "c "
  :config
  (progn
    (define-key company-mode-map (kbd "C-:") 'helm-company)
    (define-key company-active-map (kbd "C-:") 'helm-company)
    (global-company-mode)
    (setq company-minimum-prefix-length 1
	  company-idle-delay 0.01
	  company-selection-wrap-around t
	  company-async-timeout 0.5
	  ;; select using super+number
	  company-show-numbers t
	  ;; dont downcase completions
	  company-dabbrev-downcase nil
	  company-transformers '(company-sort-by-occurrence company-sort-by-backend-importance))
    (dotimes (i 10)
      (define-key company-mode-map (read-kbd-macro (format "s-%d" i)) 'company-complete-number))))

;; use helm to select company completions
(use-package helm-company
  :ensure t
  :bind ("C-:" . helm-company))

;; prettier and smarter mode line
(use-package smart-mode-line
  :ensure t
  :config (progn
	    (message "starting smart-mode-line")
	    (add-to-list 'sml/replacer-regexp-list '("^~/git/" ":Git:") t)
					;	    (sml/apply-theme 'synth)
	    (sml/setup)
	    (setq sml/use-projectile-p 'before-prefixes)))

;; disable debugging when loading is done
(setq debug-on-error nil)

(load-theme 'synth t)

(use-package git-gutter
  :ensure t
  :diminish "+-"
  :config
  (progn
    (git-gutter-mode +1)
    (global-git-gutter-mode)
    (setq git-gutter:unchanged-sign " ")
    (setq git-gutter: " ")))

;; awesome undo/redo
(use-package undo-tree
  :ensure t
  :diminish "↶ "
  :config (global-undo-tree-mode))

;; show argument lists in echo area
(use-package eldoc
  :diminish " "
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
  :diminish ""
  :commands elisp-slime-nav-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
	    (lambda () (elisp-slime-nav-mode t))))

(use-package lisp-mode
  :config
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (paredit-mode +1)
	      (yas-minor-mode +1)
	      (elisp-slime-nav-mode +1))))

(use-package clojure-mode
  :ensure t
  :commands clojure-mode
  :config
  (progn
    (define-key clojure-mode-map (kbd "C-c C-j") 'cider-jack-in)
    (add-hook 'clojure-mode-hook
	      (lambda ()
		(eldoc-mode +1)
		(paredit-mode +1)
		(prettify-symbols-mode +1)
		(yas-minor-mode +1)
		(use-package clojure-mode-extra-font-locking :ensure t)
		(use-package align-cljlet :ensure t)
		(setq buffer-save-without-query t)
		(auto-highlight-symbol-mode +1)
		(push '(">=" . ?≥) prettify-symbols-alist)
		(push '("comp" . ?○) prettify-symbols-alist)))))

(use-package cider
  :ensure t
  :commands (cider-jack-in cider-mode)
  :config
  (progn
    (require 'clojure-mode)
    (require 'cider)
    (require 'cider-interaction)
    (require 'cider-inspector)
    (require 'cider-test)
    (require 'cider-grimoire)
    (use-package clj-refactor :ensure t)

    (when (eq system-type 'windows-nt)
      (add-hook 'nrepl-mode-hook 'live-windows-hide-eol ))

    (add-hook 'cider-repl-mode-hook
	      (lambda ()
		(cider-turn-on-eldoc-mode)
		(paredit-mode 1)))

    (add-hook 'cider-mode-hook
	      (lambda ()
		(cider-turn-on-eldoc-mode)
		(paredit-mode 1)
		(hs-minor-mode 1)))

    (defun live-windows-hide-eol ()
      "Do not show ^M in files containing mixed UNIX and DOS line endings."
      (interactive)
      (setq buffer-display-table (make-display-table))
      (aset buffer-display-table ?\^M []))

    (setq cider-popup-stacktraces t
	  cider-popup-stacktraces-in-repl t
	  cider-show-error-buffer nil
	  cider-auto-select-error-buffer nil
	  cider-auto-jump-to-error nil
	  cider-test-show-report-on-success nil
	  cider-repl-use-clojure-font-lock t
	  nrepl-log-messages nil
	  cider-prompt-save-file-on-load nil
	  cider-lein-command "lein"
	  cider-lein-parameters "with-profile power repl"
	  nrepl-port "4555")

    ;; jump to repl buffer on connect cider-repl-pop-to-buffer-on-connect t
    (add-to-list 'same-window-buffer-names "*cider*")

    (defun cider-clean-restart (&optional prompt-project)
      "Quit CIDER, run `lein clean` and restart CIDER.
If PROMPT-PROJECT is t, then prompt for the project in which to
restart the server."
      (interactive "P")
      (cider-quit)
      (message "Waiting for CIDER to quit...")
      (sleep-for 2)
      (message "Running lein clean...")
      (let ((exit (shell-command "lein clean")))
	(if (zerop exit) (cider-jack-in prompt-project)
	  (message "Could not run lein clean"))))

    ;; Enable company-mode
    (require 'company)
    (add-hook 'cider-repl-mode-hook 'company-mode)
    (add-hook 'cider-mode-hook 'company-mode)

    (require 'paredit)

    (defun synth-toggle-clojure-ignore-next-form ()
      "toggle #_"
      (interactive)
      (save-excursion
	(goto-char (1- (paredit-point-at-sexp-start)))
	(if (char-equal ?_  (preceding-char))
	    (delete-backward 2)
	  (insert-string "#_"))))

    (define-key clojure-mode-map
      (kbd "C-c C-3")
      'synth-toggle-clojure-ignore-next-form)

    (define-key cider-mode-map
      (kbd "C-c p p")
      'synth-toggle-clojure-ignore-next-form)

    (defun cider-namespace-refresh ()
      (interactive)
      (save-buffer)
      (cider-interactive-eval
       "(require 'clojure.tools.namespace.repl)
    (if (resolve 'reload)
        (clojure.tools.namespace.repl/refresh :after `reload)
        (clojure.tools.namespace.repl/refresh))"))

    (defun clear-message-buffer ()
      "Find the messages buffer and clear it.
  Returns to the buffer in which the command was invoked."
      (interactive)
      (let ((origin-buffer (current-buffer)))
	(switch-to-buffer "*Messages*")
	(erase-buffer)
	(switch-to-buffer origin-buffer)))

    (defun cider-clear-and-eval-defun-at-point ()
      (interactive)
      (cider-find-and-clear-repl-buffer)
      (clear-message-buffer)
      (cider-eval-defun-at-point))


    (defun cider-pp (&optional prefix)
      "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
      (interactive "P")
      (cider-interactive-eval
       (format "(let [res %s] (clojure.pprint/pprint res) res)" (cider-last-sexp))
					;       (cider-last-sexp-start-pos)
       ))

    (defun cider-set-validate ()
      (interactive)
      (cider-interactive-eval
       "(do (require 'schema.core)
        (schema.core/set-fn-validation! true))"))

    (define-key cider-repl-mode-map (kbd "M-RET") 'cider-doc)

    (defun cider-clear-repl-buffer-2 ()
      (interactive)
      (cider-switch-to-relevant-repl-buffer)
      (cider-repl-clear-buffer)
      (cider-switch-to-last-clojure-buffer))


    (bind-keys
     :map cider-mode-map
     ("C-x M-e"   . cider-eval-last-sexp-and-replace)
     ("C-x M-r"   . cider-eval-last-sexp-to-repl)
     ("M-RET"     . cider-doc)
     ("C-c t t"   . cider-test-run-tests)
     ("C-c C-j"   . cider-jack-in)
     ("C-c C-p"   . nil)
     ("C-c n e b" . cider-eval-buffer)
     ("C-c m b"   . cider-eval-buffer)
     ;;     ("M-?"       . ac-nrepl-popup-doc) ;; TODO  find alternative
     ("C-`"       . cider-clear-and-eval-defun-at-point)
     ("<f19>"     . cider-eval-last-sexp) ;; as C-x C-e
     ("s-p s-p"   . cider-pp))

    (define-key cider-repl-mode-map (kbd "C-c C-o") 'cider-repl-clear-buffer)
    (define-key cider-repl-mode-map (kbd "M-?") 'ac-nrepl-popup-doc)
    (define-key clojure-mode-map (kbd "C->") 'cljr-cycle-coll)




    (define-key clojure-mode-map (kbd "C-c C-j") 'cider-jack-in)

    (setq cider-repl-wrap-history t)

    (define-key cider-mode-map (kbd "C-c C-o")
      (lambda ()
	(interactive)
	(cider-switch-to-relevant-repl-buffer)
	(cider-repl-clear-buffer)
	(cider-switch-to-last-clojure-buffer)))

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

    (define-key cider-mode-map (kbd "C-c n n") 'cider-eval-and-move-next)
    (define-key cider-mode-map (kbd "C-c p p") (make-repeatable-command 'cider-eval-and-move-previous))))


(defun live-lisp-describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
   This checks in turn:
     -- for a function name where point is
     -- for a variable name where point is
     -- for a surrounding function call"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
		       (with-syntax-table emacs-lisp-mode-syntax-table
			 (save-excursion
			   (or (not (zerop (skip-syntax-backward "_w")))
			       (eq (char-syntax (char-after (point))) ?w)
			       (eq (char-syntax (char-after (point))) ?_)
			       (forward-sexp -1))
			   (skip-chars-forward "`'")
			   (let ((obj (read (current-buffer))))
			     (and (symbolp obj) (fboundp obj) obj))))))
	   (describe-function sym))
	  ((setq sym (variable-at-point)) (describe-variable sym)))))

;; Show documentation/information with M-RET
(define-key lisp-mode-shared-map (kbd "M-RET") 'live-lisp-describe-thing-at-point)

(use-package ace-jump-mode
  :ensure t
  :bind
  (("C-o" . ace-jump-word-mode)))

(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window)))

;; highlight, navigate and edit symbols
(use-package auto-highlight-symbol
  :ensure t
  :commands auto-highlight-symbol-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'auto-highlight-symbol-mode)
    (add-hook 'clojure-mode 'auto-highlight-symbol-mode))
  :config
  (bind-keys
   :map auto-highlight-symbol-mode-map
   ("M-F" . ahs-forward)
   ("M-B" . ahs-backward)
   ("M-E" . ahs-edit-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9f2d4c071cb97f7c40f6cbc917183adab0cb631f28c540c07da411514bbb3eb7" "b611e26d8b8db41b2859a002f4be1899969a65c0f498de0350eb3957b7471def" "6308a012ffc059bf91f7c1f36d51901d84a032d94f6fc70c2b6bd630ce136502" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(display-buffer-base-action (quote (display-buffer-reuse-window (reusable-frames . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :config
  (progn
    (add-to-list 'yas-snippet-dirs
		 (f-join user-emacs-directory "etc" "snippets"))))

(load-theme 'synth t)

(use-package platform-osx)

(use-package dockerfile-mode
  :ensure t)

(use-package restclient
  :ensure t
  :commands restclient-mode)

;; load personal stuff
(use-package synth-utils)
(use-package synth-alias)
(use-package synth-bindings)

(use-package eval-pulse
  :init
  (setq eval-pulse-depth 1)
  :config
  (setq eval-pulse-depth 1))

(use-package window-number
  :ensure t )

(use-package yaml-mode
  :ensure t
  :commands yaml-mode)

(use-package tramp
  :config
  (setq tramp-auto-save-directory (f-tmp-file "tramp" "autosaves/" ))
  )

;; (mapcar
;;  (lambda (x)
;;    `( ,(format "M-%s" x) .  ,(intern (format "window-number-select-%s" x) )))
;;  (number-sequence 0 9))
(setq initial-scratch-message ";; Loaded emacs")

;; start the server so clients can connect to it
(require 'server)
(if (not (server-running-p)) (server-start))


(use-package geiser
  :ensure t
  :commands (run-racket run-geiser geiser-mode)
  :config
  (progn

    (defun geiser-save-and-load-buffer ()
      (interactive)
      (save-buffer)
      (geiser-load-current-buffer))

    (require 'geiser)
    ;;
    ;; (bind-keys
    ;;  :map 'geiser-mode-map
    ;;  ("C-c m b" . geiser-save-and-load-buffer))

    ))

(use-package hideshow
  :config
  (progn
    (define-key hs-minor-mode-map (kbd "M-±") #'hs-toggle-hiding)
    (use-package hideshowvis)
    (hideshowvis-enable)

    ))
