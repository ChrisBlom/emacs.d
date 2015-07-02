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

(deftheme smart-mode-line-synth "Synth color theme")

(defvar synth-colors-alist
  `( (synth-bg          . ,(hsl 1.8 0.01 0.1))
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

(with-synth-color-variables
  (custom-theme-set-faces
   'smart-mode-line-synth
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

   `(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename :underline t)))
   `(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename)))))




(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
		  (file-name-as-directory
		   (file-name-directory load-file-name))))


  ;; no-byte-compile: t
  ;; End:

(provide-theme 'smart-mode-line-synth)
