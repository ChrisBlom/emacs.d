(setq auto-mode-alist (cons '("\\.json\\'" . js-mode) auto-mode-alist))

(custom-set-variables
 '(display-buffer-base-action
   '(display-buffer-reuse-window (reusable-frames . t))))


;(diminish 'clj-refactor-mode)
;(diminish 'auto-revert-mode " ar")
;(diminish 'git-gutter-mode)
;(diminish 'eldoc-mode)
;(diminish 'volatile-highlights-mode)
;(diminish 'projectile-mode " p")
;(diminish 'cider-mode " [c]")
;(diminish 'abbrev-mode "Abv")
;(diminish 'yas-minor-mode)
(diminish 'elisp-slime-nav-mode)


;(define-key hs-minor-mode-map (kbd "M-§") 'hs-toggle-hiding)
;(define-key hs-minor-mode-map (kbd "M-±") 'hs-show-all)



(defun paredit--is-at-start-of-sexp ()
  (and (looking-at "(\\|\\[")
       (not (nth 3 (syntax-ppss))) ;; inside string
       (not (nth 4 (syntax-ppss))))) ;; inside comment

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  ;; skips to start of current sexp
  (while (not (paredit--is-at-start-of-sexp))
    (paredit-backward))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

(defface popup-mouse-face
  '((t (:foreground "#bbffaa" :background "#555555" :underline t)))
  "Face for hidden blocks"
  :group 'custom-faces)

(require 'hideshow)

(define-key hs-minor-mode-map (kbd "M-§") 'hs-toggle-hiding)

(autoload 'zap-up-to-char "misc"
   "Kill up to, but not including ARGth occurrence of CHAR." t)

(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-z") (lambda (char) (interactive "Zap up to char backwards: ") (zap-up-to-char -1 char)))

(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(define-key paredit-mode-map (kbd "M-j") (lambda () (interactive) (join-line -1)))

(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

(defun focus-on-current-window ()
  (interactive)
  (delete-other-windows)
  (read-event)
  (winner-undo))

(global-set-key (kbd "<f6>") 'focus-on-current-window)

;; Make paredit play nice with eldoc
 ;; (eldoc-add-command
 ;;    'paredit-backward-delete
 ;;    'paredit-close-round)

;; Inverse M-(
(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))

(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "M-)")
     'paredit-wrap-round-from-behind))

;; Duplicate sexp
(defun paredit-duplicate-after-point ()
  "Duplicates the content of the line that is after the point."
  (interactive)
  ;; skips to the next sexp
  (while (looking-at " ")
    (forward-char))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (set-mark-command nil)
  (yank)
  (exchange-point-and-mark))

(load-theme 'synth t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))


(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(defun gitx ()
  (interactive)
  (shell-command "gitx"))

(require 'cider)

(defun flash (o hue)
  (let ((flashy-colors (mapcar (lambda (i) (hsl hue 0.5 (/ i 20.0)))
			       (number-sequence 0 10))))
    (dolist (x (-concat flashy-colors (reverse flashy-colors)))
      (overlay-put o 'face `(:background ,x))
      (sit-for 0.001))))

(defun flashy-cider-interactive-eval-handler (ov)
  "Takes an overlay, make an interactive eval handler for BUFFER"
  (lexical-let*
      ((o ov)
       (eval-buffer (current-buffer)))
    (nrepl-make-response-handler
     ;; buffer
     eval-buffer
     ;; value
     (lambda (_buffer value)
       (message (format "succes %s" value))
       (flash o 0.4)
       (delete-overlay o)
       (cider--display-interactive-eval-result value))
     ;; stdout
     (lambda (_buffer out)
       (cider-emit-interactive-eval-output out))
     ;; stderr
     (lambda (_buffer err)
       (cider-emit-interactive-eval-err-output err)
       (cider-handle-compilation-errors err eval-buffer))
     ;; done
     '()
     ;; error
     (lambda (_buffer ex root-ex session)
       (message (format "error %s" ex))
       (flash o 0)
       (delete-overlay o)
       (funcall 'cider-default-err-handler _buffer ex root-ex session))
     ;; interrupt
     (lambda (_buffer)
       (message (format "interrupted"))
       (flash o 0.7)
       (delete-overlay o)))))


(defun make-flashy-eval-callback ()
  (let*
      ((start  (save-excursion
		 (backward-sexp)
		 (point)))
       (end    (point))
       (o      (make-overlay start end )))
    (flash o 0.5)
    (flashy-cider-interactive-eval-handler o)))

(defun flashy-cider-eval-last-sexp ()
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive)
  (cider-interactive-eval (cider-last-sexp)
			  (make-flashy-eval-callback)))
