(provide 'synth-setup)

;; disable gui stuff, it will only slow us down....
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; don't show emacs welcome & splash screen
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; open evertyhing in the same frame
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

;; set all coding systems to utf-8
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq utf-translate-cjk-mode nil) ;disable CJK coding/encoding (Chinese/Japanese/Korean characters)

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; automatically open files in compressed archived
(auto-compression-mode t)

;; show matching parentheses
(show-paren-mode 1)

;; left and right margin
(set-window-fringes nil 4 4)

;; enable debugging in init, it is turned off at the the end
(setq debug-on-error t)

;; (setenv "TERM" "xterm-256color")

;; emacs doesn't work well with fish as its shell
(setq shell-file-name "/bin/bash")
(setq command-line-default-directory "~/")

;; add extra package repositories
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup t)

;; osx specific
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq mac-function-modifier 'command)


(setq ring-bell-function 'ignore)

(setq max-lisp-eval-depth 10000)
(set-variable 'max-specpdl-size 5000)
;; initiate GC every 20 mb allocated
(setq gc-cons-threshold 20000000)

;; disable warning when running these commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
