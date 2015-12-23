;; add extra archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

(if (not  (package-installed-p 'use-package))
    (package-install 'use-package))

(require 'use-package)
