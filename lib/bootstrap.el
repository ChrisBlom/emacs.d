;; were to find themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; add extra archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(setq backup-by-copying t                                               ; don't clobber symlinks
      backup-directory-alist '(("." . "~/.local/share/emacs-saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups


(if (not  (package-installed-p 'use-package))
    (package-install 'use-package))

(require 'use-package)

;(sort package-activated-list (lambda (x y) (string-less x y)))

(defvar core-packages
  '(auto-highlight-symbol
    bind-key
    company-ycmd
    json-reformat
    pkg-info
    restclient
    smartparens
    use-package
    ycmd))

;; tools
'(ack-and-a-half
  restclient-mode
  json-reformat)

;; core
'(async
  s
  dash
  deferred
  popup
  popwin
  queue)

;; buffer / frame / command selection and navigation
'(ace-jump-buffer
  ace-jump-mode
  ace-window
  smex
  flx
  flx-ido
  flx-isearch
  helm
  ido
  projectile)

;; editing
'(yasnippet
  multiple-cursors
  expand-region
  browse-kill-ring
  company-mode
  helm-company
  undo-tree)

;; powertools
'(key-chord-mode  )

;; feedback
'(eval-pulse
  git-gutter
  wc-mode)

;; usability
'(saveplace
  doremi)

;; lisp
'(paredit
  auto-highlight-symbol)

;; languages
'(ruby-mode
  js2
  docker-mode
  fish-mode
  haskell-mode
  markdown-mode
  yaml-mode
  nxml
  ;; clojure
  clojure-mode
  clj-refactor
  cider
  align-cljlet
  )

;; cosmetic
'(powerline ;; or smart mode line?
  diminish
  )


;; version control
'(magit

  )
