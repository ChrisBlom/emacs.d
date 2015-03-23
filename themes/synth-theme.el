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

(defconst hex-regex
  (rx
   (syntax string-quote)
   (group
    (char ?#)
    (repeat 6 (in "0123456789ABCDEF")))
   (syntax string-quote)))

(defun live-fontify-hex-colors (limit)
  (remove-overlays (point) limit 'fontify-hex-colors t)
  (while (re-search-forward hex-regex limit t)
    (let ((ov (make-overlay (match-beginning 0)
                            (match-end 0)))
          (color (match-string 1))
          (contrast (if (< 0.3 (string-to-number (match-string 4)))
                        "black" "white")))
      (overlay-put ov 'face  (list :background color :foreground contrast
                                   :box '(:line-width 1 :color contrast)))
      (overlay-put ov 'fontify-hex-colors t)
      (overlay-put ov 'evaporate t)))
  ;; return nil telling font-lock not to fontify anything from this
  ;; function
  nil)

(defun live-fontify-hsl-colours-in-current-buffer ()
  (interactive)
  (font-lock-add-keywords nil
                          '((live-fontify-hsl-colors)
			    (live-fontify-hex-colors))))

(defun live-fontify-hex-colours-in-current-buffer ()
  (interactive)
  (font-lock-add-keywords nil
                          '((live-fontify-hex-colors))))

(deftheme synth "Synth color theme")

(defvar synth-colors-alist
  `( (synth-bg          . ,(hsl 0.1 0.1 0.1))
     (synth-fg          . ,(hsl 0.15 0.95 0.95))
     (paren-sat         . 0.9)
     (synth-pale-yellow . ,(hsl 0.14 0.7 0.7))))

(setq synth-colors-alist
      `( (synth-bg          . ,(hsl 1.8 0.01 0.1))
         (synth-fg          . ,(hsl 0.15 0.95 0.95))
         (paren-sat         . 0.15)
	 (paren-lum         . 0.5)
         (synth-pale-yellow . ,(hsl 0.14 0.7 0.7))))

(defmacro with-synth-color-variables (&rest body)
  "`let' bind all colors defined in `zenburn-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (car cons) (cdr cons)))
                   synth-colors-alist))
     ,@body))

(defun color-theme-synth ()
  (with-synth-color-variables
    (custom-theme-set-faces
     'synth
     `(default((t (:foreground ,synth-fg :background ,synth-bg))))
     `(bold    ((t (:weight bold))))
     `(italic ((t (:slant italic))))
     `(bold-italic ((t (:weight bold :slant italic))))
     `(underline ((t (:underline t))))
     `(vertical-border ((t (:foreground ,(hsl 0 0 0.25)))))
     `(custom-face-tag ((t (:foreground "#66D9EF" :weight bold))))
     `(custom-state ((t (:foreground "#A6E22E"))))
     `(region ((t (:background "#304D4D"))))
     `(fringe ((t (:foreground "#405D5D" :background "#232526"))))
     `(highlight ((t (:foreground "#000000" :background "#C4BE89"))))
     `(hl-line ((t (:background "#293739"))))
     `(escape-glyph ((t (:foreground "#E6DB74"))))
     `(minibuffer-prompt ((t (:foreground "#66D9EF"))))
     `(css-selector ((t (:foreground "#F92672"))))
     `(css-property ((t (:foreground "#66D9EF"))))
     `(diff-added ((t (:foreground "#A6E22E" :weight bold))))
     `(diff-context ((t (:foreground ,synth-fg))))
     `(diff-file-header ((t (:foreground "#66D9EF" :background nil))))
     `(diff-indicator-added ((t (:foreground "#A6E22E"))))
     `(diff-indicator-removed ((t (:foreground "#F92672"))))
     `(diff-header ((t (:foreground ,synth-fg :background "#232526"))))
     `(diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
     `(diff-removed ((t (:foreground "#F92672" :weight bold))))
     ;; modeline
     `(mode-line-buffer-id ((t (:foreground "red" :background "#000000" :weight bold))))
     `(mode-line-mousable-minor-mode ((t (:foreground "#BCBCBC" :background "#000000"))))
     `(mode-line          ((t (:foreground ,synth-bg :background "White"))))
     `(mode-line-inactive ((t (:foreground "#3C3C3C" :background "#000000" :box nil))))

     `(sml/global         ((t (:foreground ,(hsl 0 0 0.5)))))
     `(sml/modes          ((t (:inherit sml/global :foreground ,(hsl 0.24 0.4 0.4)))))
     `(sml/prefix         ((t (:inherit sml/global :foreground ,(hsl 0.1 0.8 0.4) ))))
     `(sml/filename       ((t (:inherit sml/global :foreground ,(hsl 0.60 0.8 0.7) :weight bold))))

     `(font-lock-builtin-face ((t (:foreground ,(hsl 0.35 0.5 0.7)))))
     `(font-lock-comment-face ((t (:foreground ,(hsl 0.45 0.05 0.45) :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:foreground "#667477" :slant italic))))
     `(font-lock-constant-face ((t (:foreground ,(hsl 1.7 0.9 0.8)))))
     `(font-lock-string-face ((t (:foreground ,(hsl 0.15 0.8 0.7)))))
     `(font-lock-doc-face ((t (:foreground ,(hsl 0.15 0.7 0.8) :slant italic :slant italic))))
     `(font-lock-variable-name-face ((t (:foreground ,(hsl 0.55 0.7 0.5) :bold t :underline ,(hsl 0.5 0.2 0.2)))))
     `(font-lock-function-name-face ((t (:foreground ,(hsl 0.45 0.7 0.5) :bold t :underline ,(hsl 0.5 0.2 0.2)))))
     `(font-lock-keyword-face ((t (:foreground ,(hsl 0.5 0.8 0.7)))))
     `(font-lock-negation-char-face ((t (:weight bold))))
     `(font-lock-preprocessor-face ((t (:foreground ,(hsl 0.3 0.6 0.75)))))
     `(font-lock-regexp-grouping-backslash ((t (:foreground ,(hsl 0.7 0.8 0.8) :weight bold))))
     `(font-lock-regexp-grouping-construct ((t (:foreground ,(hsl 0.7 0.8 0.8):weight bold))))
     `(font-lock-type-face ((t (:foreground ,(hsl 0.6 0.6 0.8)))))
     `(font-lock-warning-face ((t (:foreground "#FF5555" :background nil))))
     `(icompletep-choices ((t (:foreground "#F92672"))))
     `(icompletep-determined ((t (:foreground "#A6E22E"))))
     `(icompletep-keys ((t (:foreground "#F92672"))))
     `(icompletep-nb-candidates ((t (:foreground "#AE81FF"))))
     `(isearch ((t (:foreground ,(hsl 0.45 0.7 0.9) :background ,(hsl 0.75 0.7 0.2)))))
     `(isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))
     `(lazy-highlight ((t (:foreground "#465457" :background "#000000"))))
     `(markdown-italic-face ((t (:slant italic))))
     `(markdown-bold-face ((t (:weight bold))))
     `(markdown-header-face ((t (:weight normal))))
     `(markdown-header-face-1 ((t (:foreground "#66D9EF"))))
     `(markdown-header-face-2 ((t (:foreground "#F92672"))))
     `(markdown-header-face-3 ((t (:foreground "#A6E22E" :bold t))))
     `(markdown-header-face-4 ((t (:foreground "#AE81FF"))))
     `(markdown-header-face-5 ((t (:foreground ,synth-pale-yellow))))
     `(markdown-header-face-6 ((t (:foreground "#66D9EF"))))
     `(markdown-inline-code-face ((t (:foreground "#66D9EF"))))
     `(markdown-list-face ((t (:foreground "#A6E22E"))))
     `(markdown-blockquote-face ((t (:slant italic))))
     `(markdown-pre-face ((t (:foreground "#AE81FF"))))
     `(markdown-link-face ((t (:foreground "#66D9EF"))))
     `(markdown-reference-face ((t (:foreground "#66D9EF"))))
     `(markdown-url-face ((t (:foreground ,synth-pale-yellow))))
     `(markdown-link-title-face ((t (:foreground "#F92672"))))
     `(markdown-comment-face ((t (:foreground "#465457"))))
     `(markdown-math-face ((t (:foreground "#AE81FF" :slant italic))))
     `(mumamo-background-chunk-major ((t (:background "#272822"))))
     `(mumamo-background-chunk-submode ((t (:background "#1B1D1E"))))
     `(outline-1 ((t (:foreground "#66D9EF"))))
     `(outline-2 ((t (:foreground "#F92672"))))
     `(outline-3 ((t (:foreground "#A6E22E"))))
     `(outline-4 ((t (:foreground "#AE81FF"))))
     `(outline-5 ((t (:foreground ,synth-pale-yellow))))
     `(outline-6 ((t (:foreground "#66D9EF"))))
     `(outline-7 ((t (:foreground "#F92672"))))
     `(outline-8 ((t (:foreground "#A6E22E"))))
     `(secondary-selection ((t (:background "#272822"))))
     `(show-paren-match-face ((t (:foreground "#000000" :background "#FD971F"))))
     `(show-paren-mismatch-face ((t (:foreground "#960050" :background "#1E0010"))))
     `(widget-inactive-face ((t (:background "#ff0000"))))
     `(woman-addition ((t (:foreground "#AE81FF"))))
     `(woman-bold ((t (:foreground "#F92672"))))
     `(woman-italic ((t (:foreground "#A6E22E"))))
     `(woman-unknown ((t (:foreground "#66D9EF"))))
     `(eval-sexp-fu-flash ((t (:inverse-video t :foreground "#6e8b3d" :background "white" ))))
     `(pulse-highlight-start-face ((t (:background ,(hsl 0.35 0.7 0.5)))))
     `(rainbow-delimiters-depth-1-face ((t (:foreground  ,(hsl (/ 6 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground  ,(hsl (/ 1 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground  ,(hsl (/ 10 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground  ,(hsl (/ 3 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground  ,(hsl (/ 8 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground  ,(hsl (/ 5 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground  ,(hsl (/ 0 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground  ,(hsl (/ 7 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground  ,(hsl (/ 4 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-10-face ((t (:foreground ,(hsl (/ 9 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-depth-11-face ((t (:foreground ,(hsl (/ 3 10.0) paren-sat paren-lum)))))
     `(rainbow-delimiters-unmatched-face ((t (:foreground "white" :italic t :bold t :background "red"))))
     `(git-gutter:unchanged ((t (:background nil :inherit 'default))))
     `(git-gutter:added     ((t (:background nil :foreground "#009400"))))
     `(git-gutter:modified  ((t (:foreground "#DD8C00" :inherit 'default))))
     `(company-tooltip           ((t (:inherit default :background "#3A3C3D"))))
     `(company-scrollbar-bg      ((t (:background ,(color-lighten-name "black" 15)))))
     `(company-scrollbar-fg      ((t (:background ,(color-lighten-name "black" 10)))))
     `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
     `(company-tooltip-common    ((t (:inherit font-lock-constant-face))))
     `(ahs-definition-face       ((t (:underline t :inverse-video t))))
     `(ahs-face                  ((t (:underline t ))))
     `(hl-sexp-face              ((t (:underline nil :box nil :inverse-video nil :background ,(hsl 0.3 0.3 0.5)))))
     `(dired-mark ((t (:foreground ,(hsl 0.55 0.7 0.5) :bold t :underline ,(hsl 0.5 0.2 0.2)))))

     `(hs-fringe ((t (:foreground "#0000FF" :box (:line-width 2 :color "grey5" :style released-button)))))
     `(hs-hidden      ((t (:foreground "#bbffaa" :background "#555555" :underline t))))
     ;; smart-mode-line
     `(mode-line-inactive ((t :foreground "gray60" :background ,(hsl 0.3 0.0 0.2) :inverse-video nil)))
     `(mode-line     ((t :foreground "gray100" :background ,(hsl 0.55 0.5 0.29) :inverse-video nil)))
     `(sml/global    ((t :foreground ,(hsl 0.5 0.1 0.5) :inverse-video nil)))
     `(sml/modes     ((t :inherit sml/global :foreground ,(hsl 0.45 0.6 0.5))))
     `(sml/filename  ((t :inherit sml/global :foreground ,(hsl 0.4 0.4 0.9) :weight bold)))
     `(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil)))
     `(sml/prefix    ((t :inherit sml/global :foreground ,(hsl 0.2 0.9 0.9))))
     `(sml/git       ((t :inherit sml/global :foreground ,(hsl 0.5 0.6 0.7))))
     `(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue")))
     `(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
     `(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
		  (file-name-as-directory
		   (file-name-directory load-file-name))))

(color-theme-synth)

  ;; no-byte-compile: t
  ;; End:

(provide-theme 'synth)
