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

(defun helm-cider ()
  (helm-build-in-buffer-source "NREPL Connections"
    :data (lambda () nrepl-connection-list)
    :real-to-display #'helm-cider-connection-info
    :action '(("Switch to NREPL connection" . (lambda (connection)
						(helm-cider-switch-nrepl-connection-by-name connection)))
	      ("New connection `M-c'" . helm-cider-connect)
	      ("Close connection `M-k'" . helm-cider-close-connection)
	      ("Remove project(s) `M-D'" . helm-projectile-remove-known-projecth))))

(defun helm-cider-connections ()
  (interactive)
  (helm-other-buffer (helm-cider) "NREPL Connections"))

(defun helm-cider-available-connections ()
  (mapcar
   (lambda (x) (format "%s:%s" (car x) (cadr x)))
   (cider-locate-running-nrepl-ports)))

(defclass helm-source-cider-new-connection (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation "  A function with no arguments to create buffer list.")
   (init :initform (lambda ()
                     "new connection"))
   (candidates :initform helm-input)
   (matchplugin :initform nil)
   ;(nohighlight :initform t)
;   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (resume :initform (lambda ()
                       (run-with-idle-timer
                        0.1 nil (lambda ()
                                  (with-helm-buffer
                                    (helm-force-update))))))
;   (keymap :initform helm-buffer-map)
   (volatile :initform t)
 ;  (mode-line :initform helm-buffer-mode-line-string)
   (persistent-help
    :initform
    "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))


(defclass helm-source-cider-connections (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform #'helm-buffer-list
    :custom function
    :documentation "  A function with no arguments to create buffer list.")
   (init :initform (lambda ()
                     ;; Issue #51 Create the list before `helm-buffer' creation.
                     (setq helm-buffers-list-cache (funcall (helm-attr 'buffer-list)))
                     (let ((result (cl-loop for b in helm-buffers-list-cache
                                            maximize (length b) into len-buf
                                            maximize (length (with-current-buffer b
                                                               (symbol-name major-mode)))
                                            into len-mode
                                            finally return (cons len-buf len-mode))))
                       (unless helm-buffer-max-length
                         (setq helm-buffer-max-length (car result)))
                       (unless helm-buffer-max-len-mode
                         ;; If a new buffer is longer that this value
                         ;; this value will be updated
                         (setq helm-buffer-max-len-mode (cdr result))))))
   (candidates :initform nrepl-connection-list)
   (matchplugin :initform nil)
   ;(nohighlight :initform t)
;   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (resume :initform (lambda ()
                       (run-with-idle-timer
                        0.1 nil (lambda ()
                                  (with-helm-buffer
                                    (helm-force-update))))))
;   (keymap :initform helm-buffer-map)
   (volatile :initform t)
 ;  (mode-line :initform helm-buffer-mode-line-string)
   (persistent-help
    :initform
    "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

(defvar helm-cider-connection-list nil)

(defun hb2 ()
  (unless helm-cider-connection-list
    (setq helm-cider-connection-list
          (helm-make-source "NREPL Connections" 'helm-source-cider-connections)))
  (helm :sources '(helm-cider-connection-list
		   helm-cider-available-ports
		   )
        :buffer "*nrepl connections*"
        :keymap helm-buffer-map
        :truncate-lines t))

(hb2)


(cider-display-current-connection-info)

(setq eval-pulse-depth 1)
