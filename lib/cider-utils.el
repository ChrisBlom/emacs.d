(defun my/cider-dot ()
  (interactive)
  (cider-interactive-eval (format "(spit \"/tmp/cider.dot\" %s)"
				  (apply #'buffer-substring-no-properties (cider-last-sexp 'bounds))))
  (call-process-shell-command "dot -Tsvg -O /tmp/cider.dot")
					;(my/display-image-inline "*dot*" "/tmp/cider.dot.png")
  (call-process-shell-command "open /tmp/cider.dot.svg"))

(defun cider-clean-restart (&optional prompt-project)
  "Quit CIDER, run `lein clean` and restart CIDER.
If PROMPT-PROJECT is t, then prompt for the project in which to
restart the server."
  (interactive "P")
  (cider-quit)
  (message "Waiting for CIDER to quit...")
  (sleep-for 2)
  (message "Running lein clean...")
  (let ((exit (shell-command "lein clean")))
    (if (zerop exit) (cider-jack-in prompt-project)
      (message "Could not run lein clean"))))


(defun cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))
(defun cider-clear-and-eval-defun-at-point ()
  (interactive)
  (cider-find-and-clear-repl-buffer)
  (clear-message-buffer)
  (cider-eval-defun-at-point))

(defun cider-eval-to-point ()
  (interactive)
  (cider-eval-region (point-min) (point)))

;; (format "(let [res %s]
;;          #?(:clj  (clojure.pprint/pprint res)
;;             :cljs (with-out-str (cljs.pprint/pprint res)) )" (cider-last-sexp))

(defun cider-pp (&optional prefix)
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive "P")
					;      (cider-pprint-eval-last-sexp)
  (cider--pprint-eval-form
   (format "(let [res %s]
                   #?(:clj  (clojure.pprint/pprint res)
                      :cljs (with-out-str (cljs.pprint/pprint res))))" (cider-last-sexp))))

(defun cider-pp-clj (&optional prefix)
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive "P")
					;      (cider-pprint-eval-last-sexp)
  (cider--pprint-eval-form
   (format "(let [res %s] (clojure.pprint/pprint res) res)" (cider-last-sexp))))

(defun my/cider-eval-last-sexp-comment (&optional prefix)
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive "P")
  (cider--pprint-eval-form
   (replace-match
    (s-replace "(comment"
	       "(do"
	       (cider-last-sexp)))))

(defun cider-set-validate ()
  (interactive)
  (cider-interactive-eval
   "(do (require 'schema.core)
        (schema.core/set-fn-validation! true))"))

(defun my/cider-cljs-refresh ()
  (interactive)
  (cider-interactive-eval
   "(js/location.reload)"))

(defun my/cider-midje-load-facts ()
  (interactive)
  (cider-interactive-eval
   "(require 'midje.repl)
        (midje.repl/load-facts *ns*)"))

(defun my/cider-midje-autotest-start ()
  (interactive)
  (cider-interactive-eval
   "(do (require 'midje.repl)
            (midje.repl/autotest :all))"))

(defun my/cider-midje-autotest-stop ()
  (interactive)
  (cider-interactive-eval
   "(do (require 'midje.repl)
            (midje.repl/autotest :stop))"))

(defun my/cider-cljs-clear ()
  (interactive)
  (cider-interactive-eval "(js/console.clear)"))

(defun my/projectile-toggle-between-implementation-and-test ()
  "Toggle between an implementation file and its test file."
  (interactive)
  (find-file-other-window
   (projectile-find-implementation-or-test (buffer-file-name))))

(defun my/cider-show-impl-and-test ()
  (interactive)
  (delete-other-windows)
  (my/projectile-toggle-between-implementation-and-test))

(defun my/cider-require-symbol ()
  (interactive)
  (cider-interactive-eval (format "(let [x (symbol (namespace '%s))] (require x) [:required x])" (cider-symbol-at-point))))


(defun my/cider-throw-last-exception ()
  (interactive)
  (cider-interactive-eval "(throw *e)"))


(defun my/cider-source ()
  (interactive)
  (message "Source: %s" (cider-interactive-eval (format "(clojure.repl/source %s)" (cider-symbol-at-point)))))

(defun my/cider-repl-clear-buffer ()
  "clear the relevant REPL buffer"
  (interactive)
  (let ((pm paredit-mode))
    (cider-repl-clear-buffer)
    (paredit-mode pm)))

(defun my/cider-clear-repl ()
  "clear the relevant REPL buffer"
  (interactive)
  (cider-switch-to-repl-buffer)
  (let ((pm paredit-mode))
    (cider-repl-clear-buffer)
    (paredit-mode pm))
  (cider-switch-to-last-clojure-buffer))

(defun cider-eval-buffer-and-set-ns ()
  (interactive)
  (cider-eval-buffer)
  (cider-repl-set-ns))