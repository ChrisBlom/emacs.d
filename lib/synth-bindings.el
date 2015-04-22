(provide 'synth-bindings)

(require 'synth-utils)

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

;; global bindings
(bind-keys
 ("C-h"       . backward-delete-char)
 ("C-z"       . nil)
 ("s-w"       . backward-kill-word)
 ("C-c w 3"   . synth-two-windows)
 ("C-c w w"   . toggle-frame-fullscreen)
 ("C-c w s"   . toggle-window-split)
 ("C-c w b"   . winner-undo)
 ("C-c w f"   . winner-redo)
 ("C-c k 2"   . (lambda () (interactive) (with-current-buffer-window (kill-buffer))))
 ("C-x C-k"   . kill-current-unmodified-buffer)
 ("M-<left>"  . backward-word)
 ("M-<right>" . forward-word)
 ("M-SPC"     . live-delete-whitespace-except-one)
 ("C-S-j"     . one-newline)
 ("M-'"       . repeat)
 ("C-x C-c"   . nil)
 ("C-x r q"   . kill-emacs)
 ("s-t"       . nil))

(bind-keys
 :map emacs-lisp-mode-map
 ("C-x M-e" . eval-and-replace)
 ("C-c C-k" . eval-buffer)
 ("C-c m b" . eval-buffer)
 ("C-c C-p" . eval-print-last-sexp)
 ("C-c C-r" . eval-region))

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
