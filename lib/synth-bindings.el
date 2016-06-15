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
 ("M-h"       . backward-kill-word)
 ("C-z"       . nil)
 ("s-w"       . kill-current-unmodified-buffer)
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
 ("<prior>"   . previous-buffer)
 ("<next>"    . next-buffer)
 ("s-<prior>" . persp-prev)
 ("s-<next>"  . persp-next)

 ("C-<prior>" . winner-undo)
 ("C-<next>"  . winner-redo)

 ("s-t"       . nil))


(define-key global-map (kbd "<s-b>") #'bury-buffer)


(bind-keys
 :map emacs-lisp-mode-map
 ("C-x M-e" . eval-and-replace)
 ("C-c C-k" . eval-buffer)
 ("C-c m b" . eval-buffer)
 ("C-c C-p" . eval-print-last-sexp)
 ("C-c C-r" . eval-region))

(defun my/window-selector (n)
  (lexical-let ((ln n))
    (lambda () (interactive)
      (window-number-select ln))))

(--each (-iterate (lambda (x) (+ x 1)) 0 10)
  (bind-key (format "M-s-%s" it) (my/window-selector it)))

;; Fast window selection
(key-chord-define-global "1q" (my/window-selector 1))
(key-chord-define-global "2w" (my/window-selector 2))
(key-chord-define-global "3e" (my/window-selector 3))
(key-chord-define-global "4r" (my/window-selector 4))
(key-chord-define-global "5t" (my/window-selector 5))
(key-chord-define-global "6y" (my/window-selector 6))
(key-chord-define-global "7u" (my/window-selector 7))
(key-chord-define-global "8i" (my/window-selector 8))
(key-chord-define-global "9o" (my/window-selector 9))
(key-chord-define-global "0p" (my/window-selector 10))
