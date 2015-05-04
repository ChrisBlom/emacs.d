(require 'cl-lib)
(require 'helm)
(require 'cider)
(require 's)
(require 'dash)

(require 'helm-config)
(require 'helm-locate)
(require 'helm-buffers)
(require 'helm-files)

(require 'projectile)

(defun helm-cider-switch-nrepl-connection-by-name (name)
  (let ((pos (-elem-index name nrepl-connection-list)))
    (when pos
      (setq nrepl-connection-list
	    (append (list (nth pos nrepl-connection-list))
		    (-remove-at pos nrepl-connection-list)))
      (cider-display-current-connection-info))))

(defun helm-cider-connection-info (connection-buffer)
  (with-current-buffer (get-buffer connection-buffer)
    (format "%s\t%s\t%s:%s (Java %s, Clojure %s, nREPL %s)"
	    (or (nrepl--project-name nrepl-project-dir) "<no project>")
	    cider-buffer-ns
	    (car nrepl-endpoint)
	    (cadr nrepl-endpoint)
	    (cider--java-version)
	    (cider--clojure-version)
	    (cider--nrepl-version))))

(defun helm-cider--candidate-transformer (candidates)
  (mapcar #'helm-cider-connection-info candidates))

(defun helm-cider-connect ()
  (interactive)
  (cider-connect))
