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
 ("s-w"       . backward-kill-word)
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


(key-chord-define-global "1q" (lambda () (interactive) (window-number-select 1)))
(key-chord-define-global "2w" (lambda () (interactive) (window-number-select 2)))
(key-chord-define-global "3e" (lambda () (interactive) (window-number-select 3)))
(key-chord-define-global "4r" (lambda () (interactive) (window-number-select 4)))
(key-chord-define-global "5t" (lambda () (interactive) (window-number-select 5)))
(key-chord-define-global "6y" (lambda () (interactive) (window-number-select 6)))
(key-chord-define-global "7u" (lambda () (interactive) (window-number-select 7)))
(key-chord-define-global "8i" (lambda () (interactive) (window-number-select 8)))
(key-chord-define-global "9o" (lambda () (interactive) (window-number-select 9)))
(key-chord-define-global "0p" (lambda () (interactive) (window-number-select 10)))
