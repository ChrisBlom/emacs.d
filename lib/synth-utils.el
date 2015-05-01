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

(bind-keys
 ("C-x M-k" . delete-current-buffer-file)
 ("C-x M-r" . rename-file-and-buffer))

(defun gitx ()
  (interactive)
  (shell-command "gitx ."))

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
  (delete-other-windows))



(bind-keys
 ("C-x 0" . synth-delete-window)
 ("C-x 1" . synth-delete-other-windows))
