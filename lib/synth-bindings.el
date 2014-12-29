(provide 'synth-bindings)

(defun live-delete-whitespace-except-one ()
  (interactive)
  (just-one-space -1))

(defun one-newline ()
  (interactive)
  (join-line -1))

;; global bindings
(bind-keys
 ("C-h"       . backward-delete-char)
 ("s-w"       . backward-kill-word) ; like in terminals
 ("M-<left>"  . backward-word)
 ("M-<right>" . forward-word)
 ("M-SPC"     . live-delete-whitespace-except-one)
 ("C-S-j"     . one-newline)
 ("M-'"       . repeat)
 ("C-x C-c"   . nil)
 ("C-x r q"   . kill-emacs))

(bind-keys
 :map emacs-lisp-mode-map
 ("C-c m s" . eval-and-replace)
 ("C-c m b" . eval-buffer)
 ("C-c m e" . eval-last-sexp)
 ("C-c m i" . eval-expression)
 ("C-c m d" . eval-defun)
 ("C-c m n" . eval-print-last-sexp)
 ("C-c m r" . eval-region))
