(provide 'synth-utils)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))


(defun live-delete-whitespace-except-one ()
  (interactive)
  (just-one-space -1))

(defun one-newline ()
  (interactive)
  (join-line -1))

(defun synth-two-windows ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally))

(defun kill-current-unmodified-buffer ()
  (interactive)
  (kill-buffer-if-not-modified (current-buffer)))

;; Push mark when using ido-imenu
(defvar push-mark-before-goto-char nil)

(defadvice goto-char (before push-mark-first activate)
  (when push-mark-before-goto-char
    (push-mark)))

(defun ido-imenu-push-mark ()
  (interactive)
  (let ((push-mark-before-goto-char t))
    (ido)))

;; set mark before jumping to imenu entry
(defadvice ido-imenu (before push-mark activate) (push-mark))

;; show line numbers during goto-line
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: "))
	(pulse-momentary-highlight-one-line (point)))
    (linum-mode -1)))

(bind-keys
 ("M-g g"   . goto-line-with-feedback)
 ("M-g M-g" . goto-line-with-feedback))

;; todo : support regions and sexps
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line 1)
    (move-to-column () col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -1)
    (move-to-column col)))

(bind-keys
 ("<s-up>"   .  move-line-up)
 ("<s-down>"   .  move-line-down))

(defmacro ignore-errors (&rest body)
  (declare (debug t) (indent 0))
  `(condition-case nil (progn ,@body) (error nil)))

(defmacro try-times (f n)
  `(lambda ()
    (interactive)
    (ignore-errors (,f ,n))))

(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defalias 'tws 'toggle-window-split)

(defun live-windows-hide-eol ()
      "Do not show ^M in files containing mixed UNIX and DOS line endings."
      (interactive)
      (setq buffer-display-table (make-display-table))
      (aset buffer-display-table ?\^M []))

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

(defun synth-delete-window (n)
  "With numeric prefix: kill the window with number n, otherwise kills the current window"
  (interactive "P")
  (if (integerp n)
      (delete-window (nth (- n 1) (butlast (window-number-list))) )
    (delete-window)
    (balance-windows)))

(defun synth-delete-other-windows (n)
  "With numeric prefix: kill the window with number n, otherwise kills the current window"
  (interactive "P")
  (when (integerp n)
    (select-window (nth (- n 1) (butlast (window-number-list))) ))
  (delete-other-windows)
  (balance-windows))

(bind-keys
 ("C-x 0" . synth-delete-window)
 ("C-x 1" . synth-delete-other-windows))

(require 'color)

(defvar srd
      (rx
       (group
        (optional (in "+-"))
        (one-or-more digit)
        (optional
         (char ".")
         (one-or-more digit)))))

(defun hsl (h s l)
  (let ((rgb (color-hsl-to-rgb h s l)))
    (apply 'color-rgb-to-hex rgb)))

(defun lab (h s l)
  (let ((rgb (color-lab-to-srgb h s l)))
    (apply 'color-rgb-to-hex rgb)))

(defun live-fontify-hsl-colors (limit)
  (remove-overlays (point) limit 'fontify-hsl-colors t)
  (while (re-search-forward (concat "\\((hsl " srd " " srd  " "srd ")\\)") limit t)
    (let ((ov (make-overlay (match-beginning 0)
                            (match-end 0)))
          (color (hsl (string-to-number (match-string 2))
                      (string-to-number (match-string 3))
                      (string-to-number (match-string 4))))
          (contrast (if (< 0.3 (string-to-number (match-string 4)))
                        "black" "white")))
      (overlay-put ov 'face  (list :background color :foreground contrast
                                   :box '(:line-width 1 :color contrast)))
      (overlay-put ov 'fontify-hsl-colors t)
      (overlay-put ov 'evaporate t)))
  ;; return nil telling font-lock not to fontify anything from this
  ;; function
  nil)

(defun live-fontify-hsl-colours-in-current-buffer ()
  (interactive)
  (font-lock-add-keywords nil
                          '((live-fontify-hsl-colors)
			    (live-fontify-hex-colors))))


(defun live-fontify-hex-colors (limit)
  (remove-overlays (point) limit 'fontify-hex-colors t)
  (while (re-search-forward "\\(#[[:xdigit:]]\\{6\\}\\)" limit t)
    (let ((ov (make-overlay (match-beginning 0)
                            (match-end 0))))
      (overlay-put ov 'face  (list :background (match-string 1) :foreground "black"))
      (overlay-put ov 'fontify-hex-colors t)
      (overlay-put ov 'evaporate t)))
  ;; return nil telling font-lock not to fontify anything from this
  ;; function
  nil)

(defun live-fontify-hex-colours-in-current-buffer ()
  (interactive)
  (font-lock-add-keywords nil
			  '((live-fontify-hex-colors))))

(defun copy-relative-file-name-to-clipboard ()
  (let ((filename (if (equal major-mode 'dired-mode)
		      default-directory
		    (buffer-file-name)))
	(project-dir (condition-case nil (projectile-project-root) (error nil))))
    (cond
     ((and filename project-dir)
      (kill-new (s-chop-prefix project-dir filename)))

     (filename
      (kill-new filename)))))

;; from prelude
(defun copy-file-name-to-clipboard (&optional arg)
  "Copy the current buffer file name to the clipboard."
  (interactive "P")
  (if arg
      (copy-relative-file-name-to-clipboard)
      (let ((filename (if (equal major-mode 'dired-mode)
			  default-directory
			(buffer-file-name))))
	(when filename
	  (kill-new filename)
	  (message "Copied buffer file name '%s' to the clipboard." filename)))))

(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


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

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
		  (when (y-or-n-p (format "Create directory?: %s " dir))
		    (make-directory dir t)))))))

(defun endless/isearch-symbol-with-prefix (p)
  "Like isearch, unless prefix argument is provided.
With a prefix argument P, isearch for the symbol at point."
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'isearch-forward-symbol-at-point
       #'isearch-forward))))

(global-set-key [remap isearch-forward]
                #'endless/isearch-symbol-with-prefix)

(defun open-in-iterm ()
  (interactive)
  (let* ((f (buffer-file-name))
	(d (when f
	     (or (and (f-dir? f) f)
		 (f-parent f)))))
    (when d
      (shell-command
       (format "open -b com.googlecode.iterm2 %s" d)))))
