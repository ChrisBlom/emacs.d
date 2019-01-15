(provide 'platform-osx)

(when (eq system-type 'darwin)

  ;; OS X specific configuration
  ;; ---------------------------
  (setq default-input-method "MacOSX")

  ;; osx specific
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (setq mac-function-modifier 'command)
  (setq mac-control-modifier 'control)

  (defun open-in-iterm ()
    (interactive)
    (let* ((f (buffer-file-name))
	   (d (when f
		(or (and (f-dir? f) f)
		    (f-parent f)))))
      (when d
	(shell-command
	 (format "open -b com.googlecode.iterm2 %s" d)))))

  ;; Make cut and paste work with the OS X clipboard
  (defun live-copy-from-osx ()
    (interactive)
    (shell-command-to-string "pbpaste"))

  (defun live-paste-to-osx (text &optional push)
    (interactive)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  ;; ;; (when (eq 'mac window-system)
  ;;    (setq interprogram-cut-function 'live-paste-to-osx)
  ;;    (setq interprogram-paste-function 'live-copy-from-osx)

  ;; Work around a bug on OS X where system-name is a fully qualified
  ;; domain name
  (setq system-name (car (split-string system-name "\\.")))

  (require 's)

  ;; replace spaces with : on osx;
  (setenv "PATH"
	  (s-join ":"
		  (cons "/usr/local/bin"
			(split-string (getenv "PATH") ":"))))

  ;; copy PATH to exec path
  (setq exec-path (split-string (getenv "PATH") ":"))

;;;; Ligatures
  ;; requires a mac build of emacs and the fira code font
  ;; brew tap railwaycat/emacsmacport
  ;; brew cask install emacs-mac

  ;; enable ligatures
  (let ((alist '(;;(33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
		 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
		 (36 . ".\\(?:>\\)")
		 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
		 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
		 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
		 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
		 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
		 ;;(46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
		 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
		 (48 . ".\\(?:x[a-zA-Z]\\)")
		 (58 . ".\\(?:::\\|[:=]\\)")
		 (59 . ".\\(?:;;\\|;\\)")
		 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
		 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
		 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
		 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
		 (91 . ".\\(?:]\\)")
		 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
		 (94 . ".\\(?:=\\)")
		 (119 . ".\\(?:ww\\)")
		 (123 . ".\\(?:-\\)")
		 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
		 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
		 )
               ))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))



  )
