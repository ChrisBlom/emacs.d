(require 'hideshow)
(require 'fringe-helper)

(fringe-helper-define 'hideshow-fringe-marker-hidden 'top
  "........."
  "........."
  "........."
  ".XXXXXXX."
  "..XXXXX.."
  "...XXX..."
  "....X...."
  ".........")


(define-fringe-bitmap 'hideshow-hideable-marker [0 0 0 126 126 0 0 0]) ; -
(define-fringe-bitmap 'hideshow-folded-marker [0 24 24 1 126 24 24 0]) ; +


(defconst hideshow-fringe-version "v0.1" "Version of hideshow-fringe minor mode")


(defface hideshow-fringe-hidable-face
  '((t (:foreground "Yellow")))
  "Face to highlight foldable regions"
  :group 'hideshow)

(defface hs-fringe-hidden-face
  '((t (:foreground "Yellow")))
  "Face used to highlight the fringe on folded regions"
  :group 'hideshow)

(defface hs-hidden-face
  '((t (:inherit default :box t)))
  "Face to hightlight the ... area of hidden regions"
  :group 'hideshow)

(defcustom hs-hidden-face 'hs-hidden-face
  "*Specify the face to to use for the hidden region indicator"
  :type 'face
  :group 'hideshow)

(defcustom hs-fringe-hidden-face 'hs-fringe-hidden-face
  "*Specify face used to highlight the fringe on hidden regions."
  :type 'face
  :group 'hideshow)

(defcustom hideshow-fringe-ignore-same-line nil
  "Do not display foldable regions in the fringe if the matching
  closing parenthesis is on the same line. Set this to nil if
  enabling the minor mode is slow on your machine"
  :group 'hideshow)

(set-face-attribute 'hs-fringe-hidden-face nil
		    :background nil
		    :foreground "Yellow")

(set-face-attribute 'hs-hidden-face nil
		    :background nil
		    :foreground "Yellow"
		    :box nil )

(defun hideshow-fringe-highlight-hs-regions-in-fringe (&optional start end old-text-length)
  (when hs-minor-mode
    (save-excursion
      (save-restriction
        (when (and start end)
          (narrow-to-region start end))
        (goto-char (point-min))
        (remove-overlays (point-min) (point-max) 'hideshow-fringe-hs t)
        (while (search-forward-regexp hs-block-start-regexp nil t)
          (let* ((ovl (make-overlay (match-beginning 0) (match-end 0)))
                 (marker-string "*hideshow-fringe*")
                 (doit
                  (if hideshow-fringe-ignore-same-line
                      (let (begin-line)
                        (setq begin-line
                              (save-excursion
                                (goto-char (match-beginning 0))
                                (line-number-at-pos (point))))
                        (save-excursion
                          (goto-char (match-beginning 0))
                          (ignore-errors
                            (progn
                              (funcall hs-forward-sexp-func 1)
                              (> (line-number-at-pos (point)) begin-line)))))
                    t)))
            (when doit
              (put-text-property 0
                                 (length marker-string)
                                 'display
                                 (list 'left-fringe 'hideshow-fringe-hideable-marker 'hideshow-fringe-hidable-face)
                                 marker-string)
              (overlay-put ovl 'before-string marker-string)
              (overlay-put ovl 'hideshow-fringe-hs t))))))))

;;;###autoload
(defun hideshow-fringe-click-fringe (event)
  (interactive "e")
  (mouse-set-point event)
  (end-of-line)
  (if (save-excursion
        (end-of-line 1)
        (or (hs-already-hidden-p)
            (progn
              (forward-char 1)
              (hs-already-hidden-p))))
      (hs-show-block)
    (hs-hide-block)
    (beginning-of-line)))

(defvar hideshow-fringe-mode-map
  (let ((hideshow-fringe-mode-map (make-sparse-keymap)))
    (define-key hideshow-fringe-mode-map [left-fringe mouse-1]
      'hideshow-fringe-click-fringe)
    hideshow-fringe-mode-map)
  "Keymap for hideshow-fringe mode")

;;;###autoload
(define-minor-mode hideshow-fringe-minor-mode ()
  "Will indicate regions foldable with hideshow in the fringe."
  :init-value nil
  :require 'hideshow
  :group 'hideshow
  :keymap hideshow-fringe-mode-map
  (condition-case nil
      (if hideshow-fringe-minor-mode
          (progn
            (hs-minor-mode 1)
            (hideshow-fringe-highlight-hs-regions-in-fringe (point-min) (point-max) 0)
            (add-to-list 'after-change-functions 'hideshow-fringe-highlight-hs-regions-in-fringe))
        (remove-overlays (point-min) (point-max) 'hideshow-fringe-hs t)
        (setq after-change-functions
              (remove 'hideshow-fringe-highlight-hs-regions-in-fringe after-change-functions)))
    (error
     (message "Failed to toggle hideshow-fringe-minor-mode"))))

;;;###autoload
(defun hideshow-fringe-enable ()
  "Will enable hideshow-fringe minor mode"
  (interactive)
  (hideshow-fringe-minor-mode 1))


(defcustom hideshow-fringe-format " ▼ %d ..."
  "")

;;;###autoload
(defun display-code-line-counts (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (let* ((marker-string "*fringe-dummy*")
	   (marker-length (length marker-string))
	   (display-string (format " ▼ %d ..." (count-lines (overlay-start ov) (overlay-end ov)))))
      (overlay-put ov 'help-echo (buffer-substring (overlay-start ov) (overlay-end ov)))
      (put-text-property 0 marker-length
			 'display (list 'left-fringe 'hideshow-fringe-marker-hidden 'hs-fringe-hidden-face) marker-string)
      (overlay-put ov 'before-string marker-string)
      (put-text-property 0 (length display-string) 'face 'hs-hidden-face display-string)
      (overlay-put ov 'display display-string))))


(setq hs-set-up-overlay 'display-code-line-counts)

(provide 'hideshow-fringe)

;);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hideshow-fringe.el ends here
