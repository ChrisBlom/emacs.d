(provide 'synth-bindings)

(require 'synth-utils)

;; global bindings
(bind-keys

 ("C-h"       . backward-delete-char)
 ("M-h"       . backward-kill-word)
 ("C-z"       . nil)

 ("s-w"       . kill-current-unmodified-buffer)
 ("s-r"       . revert-buffer)
 ("s-v"       . yank)
 ("s-c"       . kill-ring-save)
 ("s-x"       . kill-region)

 ;; windows
 ("C-c w 3"   . synth-two-windows)
 ("C-c w w"   . toggle-frame-fullscreen)
 ("C-c w s"   . toggle-window-split)
 ("C-c w b"   . winner-undo)
 ("C-c w f"   . winner-redo)
 ("C-c k 2"   . (lambda () (interactive) (with-current-buffer-window (kill-buffer))))
 ("C-x C-k"   . kill-current-unmodified-buffer)
 ("C-<prior>" . winner-undo)
 ("C-<next>"  . winner-redo)

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

 ;;buffer
 ("C-x M-k" . delete-current-buffer-file)
 ("C-x M-r" . rename-file-and-buffer)
 ("s-t"     . nil)
 ("s-b"     . bury-buffer))

(bind-keys
 :map emacs-lisp-mode-map
 ("C-<return>" . eval-last-sexp)
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
