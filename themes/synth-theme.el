(require 'color)

(deftheme synth "Synth color theme")

(defvar synth-colors-alist
  `())

(setq synth-colors-alist
      `( (synth-bg          . ,(hsl 2.4 0.2 0.1))
         (synth-fg          . ,(hsl 0.1 0.2 0.9))
         (paren-sat         . 0.25)
	 (paren-lum         . 0.4)
         (synth-pale-yellow . ,(hsl 0.14 0.7 0.7))))

(defmacro with-synth-color-variables (&rest body)
  "`let' bind all colors defined in `synth-colors-alist' around BODY.
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
     `(default ((t (:foreground ,synth-fg :background ,synth-bg))))
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
     `(secondary-selection ((t (:background ,(hsl 0.1 0.1 0.23)))))

     `(magit-section-title ((t (:bold nil :underline t :background ,(hsl 0.1 0.1 0.3)))))
     `(magit-hash ((t (:bold t :underline nil :foreground ,(hsl 0.1 0.1 0.7)))))

     `(magit-blame-heading ((t (:bold nil :underline nil :background ,(hsl 0.1 0.1 0.2)
				      :foreground ,(hsl 0.1 0.5 0.5)
				      :slant italic
				      ))))

     ;`(magit-item-highlight ((t (:background ,(hsl 0.1 0.1 0.23)))))



     `(hl-line ((t (:background "#293739"))))
     `(escape-glyph ((t (:foreground "#E6DB74"))))
     `(minibuffer-prompt ((t (:foreground "#66D9EF"))))
     `(css-selector ((t (:foreground "#F92672"))))
     `(css-property ((t (:foreground "#66D9EF"))))

     `(magit-diff-hunk-header ((t (:background ,(hsl 0.7 0.1 0.2) :underline nil))))
     `(magit-item-highlight ((t (:background ,(hsl 0.7 0.1 0.2)
				 :foreground ,(hsl 0.15 0.3 0.7)

					     :underline nil))))
     `(diff-changed ((t (:background ,(hsl 0.7 0.9 0.3)))))
     `(diff-added ((t (:foreground "#A6E22E" :weight bold))))
     `(diff-context ((t (:foreground ,synth-fg))))
     `(diff-file-header ((t (:foreground "#66D9EF" :background nil))))
     `(diff-indicator-added ((t (:foreground "#A6E22E"))))
     `(diff-indicator-removed ((t (:foreground "#F92672"))))
     `(diff-header ((t (:foreground ,synth-fg :background "#232526"))))
     `(diff-hunk-header ((t (:foreground "#AE81FF" :background "#232526"))))
     `(diff-removed ((t (:foreground "#F92672" :weight bold))))
     `(ediff-even-diff-A ((t (:foreground nil :background "#000000" :weight bold))))
     `(ediff-odd-diff-A ((t (:foreground nil :background nil :box t :weight bold))))

     ;; modeline
     `(mode-line-buffer-id ((t (:foreground "red" :background "#000000" :weight bold))))
     `(mode-line-mousable-minor-mode ((t (:foreground "#BCBCBC" :background "#000000"))))
     `(mode-line          ((t (:foreground ,synth-bg :background "White"))))
     `(mode-line-inactive ((t (:foreground "#3C3C3C" :background "#000000" :box nil))))

     `(sml/global         ((t (:foreground ,(hsl 0 0 0.5)))))
     `(sml/modes          ((t (:inherit sml/global :foreground ,(hsl 0.24 0.4 0.4)))))
     `(sml/prefix         ((t (:inherit sml/global :foreground ,(hsl 0.1 0.8 0.4) ))))
     `(sml/filename       ((t (:inherit sml/global :foreground ,(hsl 0.60 0.8 0.7) :weight bold))))
     ;; font lock faces
     `(font-lock-builtin-face ((t (:foreground ,(hsl 0.35 0.5 0.7)))))
     `(font-lock-comment-face ((t (:foreground ,(hsl 0.45 0.05 0.55) :slant italic))))
     `(font-lock-comment-delimiter-face ((t (:foreground "#667477" :slant italic))))
     `(font-lock-constant-face ((t (:foreground ,(hsl 1.7 0.9 0.8)))))
     `(font-lock-string-face ((t (:foreground ,(hsl 0.15 0.7 0.8)))))     `(font-lock-doc-face ((t (:foreground ,(hsl 0.45 0.45 0.8) :slant italic :slant italic))))
     `(font-lock-variable-name-face ((t (:foreground ,(hsl 0.55 0.7 0.55) :bold t :underline nil))))
     `(font-lock-function-name-face ((t (:foreground ,(hsl 0.45 0.7 0.5) :bold t :underline nil))))
     `(font-lock-keyword-face       ((t (:foreground ,(hsl 0.5 0.9 0.85)))))
     `(font-lock-negation-char-face ((t (:weight bold))))
     `(font-lock-preprocessor-face  ((t (:foreground ,(hsl 0.3 0.6 0.75)))))
     `(font-lock-regexp-grouping-backslash ((t (:foreground ,(hsl 0.7 0.8 0.8) :weight bold))))
     `(font-lock-regexp-grouping-construct ((t (:foreground ,(hsl 0.7 0.8 0.8):weight bold))))
     `(font-lock-type-face ((t (:foreground ,(hsl 0.6 0.6 0.8)))))
     `(font-lock-warning-face ((t (:foreground "#FF5555" :background nil))))
     `(icompletep-choices ((t (:foreground "#F92672"))))
     `(icompletep-determined ((t (:foreground "#A6E22E"))))
     `(icompletep-keys ((t (:foreground "#F92672"))))
     `(icompletep-nb-candidates ((t (:foreground "#AE81FF"))))
     ;; isearch
     `(isearch ((t (:foreground ,(hsl 0.45 0.7 0.9) :background ,(hsl 0.75 0.7 0.2) :inverse-video t))))
     `(isearch-fail ((t (:foreground "#FFFFFF" :background "#333333"))))
     `(lazy-highlight ((t (:foreground ,(hsl 0.6 0.3 0.6) :background "#000000" :underline nil :inverse-video t) )))
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

     `(show-paren-match-face ((t (:underline nil :bold t :foreground ,(hsl 0.5 1 0.95)))))
     `(show-paren-mismatch-face ((t (:foreground "#960050" :background "#1E0010"))))
     `(widget-inactive-face ((t (:background "#ff0000"))))
     ;;
     `(eval-sexp-fu-flash ((t (:inverse-video t :foreground "#6e8b3d" :background "white" ))))
     `(pulse-highlight-start-face ((t (:background ,(hsl 0.35 0.7 0.5)))))
     ;; Flyscheck
     `(flycheck-warning ((t (:underline (:style line :color "#FFAA20") :weight bold))))

     `(highlight-numbers-number ((t (:inherit font-lock-constant-face :foreground ,(hsl 0.35 0.4 0.8) ))))
     ;; Rainbow delimeters
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
     ;; Git gutter
     `(git-gutter:unchanged ((t (:background nil :inherit 'default))))
     `(git-gutter:added     ((t (:background nil :foreground "#009400"))))
     `(git-gutter:modified  ((t (:foreground "#DD8C00" :inherit 'default))))
     ;; Company mode
     `(company-tooltip           ((t (:inherit default :foreground "gray" :background "#3A4C4D"))))
     `(company-scrollbar-bg      ((t (:background ,(hsl 0.5 0.2 0.3)))))
     `(company-scrollbar-fg      ((t (:background ,(hsl 0.5 0.2 0.5)))))
     `(company-tooltip-common    ((t (:inherit company-tooltip :foreground "gray"))))
     `(company-tooltip-selection ((t (:inherit company-tooltip :foreground ,synth-fg :bold t ))))
     `(company-tooltip-common-selection ((t (:inherit company-tooltip :foreground ,synth-fg :bold nil))))
     `(company-tooltip-annotation ((t (:inherit company-tooltip :foreground ,(hsl 0.45 0.3 0.5)))))
     ;; Auto highlight symbol
     `(ahs-definition-face       ((t (:underline t :bold t :background nil))))
     `(ahs-plugin-whole-buffer-face       ((t (:underline t :bold t :background nil))))
     `(ahs-face                  ((t (:underline t :background nil))))

     `(hl-sexp-face              ((t (:underline nil :box nil :inverse-video nil :background ,(hsl 0.3 0.3 0.5)))))
     `(dired-mark ((t (:foreground ,(hsl 0.55 0.7 0.5) :bold t :underline ,(hsl 0.5 0.2 0.2)))))
     ;; Hide show
     `(hs-fringe ((t (:foreground "#0000FF" :box (:line-width 2 :color "grey5" :style released-button)))))
     `(hs-hidden      ((t (:foreground "#bbffaa" :background "#555555" :underline) t)))
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

     `(persp-selected-face ((t :foreground "white" :inherit sml/filename :underline t)))
     ;; Helm

     `(helm-candidate-number ((t :foreground ,(hsl 0.45 0.6 0.5) :background nil)))
     `(helm-separator ((t :foreground ,(hsl 0.1 0.8 0.5) :background nil)))

     `(highlight-stages-level-1-face ((t :background ,(hsl 0.7 0.25 0.2)
					 :underline nil
					 )))

     `(highlight-stages-negative-level-face ((t :background ,(hsl 0.7 0.25 0.2)
						:underline (:style line :color ,(hsl 0.1 0.5 0.8))
						))))))

(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
		  (file-name-as-directory
		   (file-name-directory load-file-name))))

(color-theme-synth)

  ;; no-byte-compile: t
  ;; End:

(provide-theme 'synth)
