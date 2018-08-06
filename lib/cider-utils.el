
(provide 'cider-utils)

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

(defun my/cider-pp (&optional prefix)
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

(defun my/cider-eval-last-sexp-comment ()
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive)
  (let* ((expr (cider-last-sexp))
	 (do-expr (s-replace "(comment" "(do" expr)))
    (cider-interactive-eval do-expr)))

(defun my/cider-eval-last-sexp-quoted ()
  "Evaluate the expression preceding point.
If invoked with a PREFIX argument, print the result in the current buffer."
  (interactive)
  (let* ((expr (cider-last-sexp))
	 (do-expr (format "(quote %s)" expr)))
    (cider-interactive-eval do-expr)))

(defun my/cider-debug-symbol-at-point ()
  (interactive)
  (save-excursion
    (cider-find-var)
    (cider-debug-defun-at-point)))

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

(defun my/cider-midje-autotest-error ()
  (interactive)
  (cider-interactive-eval
   "(do (require 'midje.repl)
        (throw midje.repl/*me))"))

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

(defun my/cider-pull-dependency ()
  (interactive)
  (cider-interactive-eval (format "(./distill '%s)" (cider-last-sexp))))

;; (defun my/cider-show-graph ()
;;   (interactive)
;;   (cider-interactive-eval (format "(show-graph (let [s %s] (cond (map? s) s (var? s) (var-get s))))" (cider-last-sexp))))

(defun my/cider-throw-last-exception ()
  (interactive)
  (cider-interactive-eval "(throw *e)"))


(defun my/cider-source ()
  (interactive)
  (message "Source: %s" (cider-interactive-eval (format "(clojure.repl/source %s)" (cider-symbol-at-point)))))

(defun my/cider-clear-namespace ()
  (interactive)
  (message "Cleared: %s" (cider-interactive-eval "(./nc)")))

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

(defun my/cider-eval-buffer (&optional prefix)
  "like cider eval buffer, but with a prefix it first clear the namespace"
  (interactive "P")
  (when prefix (my/cider-clear-namespace))
  (cider-eval-buffer))

(defun cider-eval-buffer-and-set-ns ()
  (interactive)
  (cider-eval-buffer)
  (cider-repl-set-ns))

(defun my/cljr-projectile-clean-ns ()
  (interactive)
  (dolist (filename (-filter (lambda (x) (s-ends-with? ".clj" x ))  (projectile-current-project-files)))
    (with-current-buffer (find-file-noselect filename)
      (condition-case ex
	  (progn
	    (cljr-clean-ns)
	    (save-buffer)
	    (kill-buffer))
	('error (message (format "Caught exception: [%s] %s" ex (buffer-name))))))))



;; like cider-last-sexp, but returns bounds exactly after sexp end
(defun my/cider-last-sexp (&optional bounds)
  "Return the sexp preceding the point.
If BOUNDS is non-nil, return a list of its starting and ending position
instead."
  (apply (if bounds #'list #'buffer-substring-no-properties)
         (save-excursion
           (clojure-backward-logical-sexp 1)
           (list (point)
                 (progn (clojure-forward-logical-sexp 1)
                        (point))))))

(defun my/cider-eval-last-sexp-to-comment ()
  "Evaluate the expression preceding point.
Print its value into the current buffer."
  (interactive)
  (let* ((bounds (my/cider-last-sexp 'bounds))
         (insertion-point (nth 1 bounds)))
    (cider-interactive-eval nil
                            (cider-eval-print-with-comment-handler
                             (current-buffer) insertion-point "\n;; => ")
                            (cider-last-sexp 'bounds))))

(defun my/cider-eval-last-sexp-to-midje ()
  "Evaluate the expression preceding point.
Print its value into the current buffer."
  (interactive)
  (let* ((bounds (my/cider-last-sexp 'bounds))
         (insertion-point (nth 1 bounds)))
    (cider-interactive-eval nil
                            (cider-eval-print-with-comment-handler
                             (current-buffer)
			     insertion-point "\n=> ")
                            (cider-last-sexp 'bounds))))

(defun add-newline-after-require ()
  (interactive)
  (condition-case ex
      (save-excursion
	(goto-char 0)
	(re-search-forward (rx
			    "(:require"
			    (+ (or "\n" space))
			    "["))
	(backward-char)
	(just-one-space -1)
	(newline)
	(indent-region
	 (save-excursion (cljr--goto-ns) (point))
	 (save-excursion (cljr--goto-ns) (forward-char) (paredit-close-parenthesis) (point))))
    ('search-failed nil)))

(advice-add 'cljr-clean-ns :after 'add-newline-after-require)

(define-clojure-indent
  (dom/a nil)
  (dom/abbr nil)
  (dom/address nil)
  (dom/area nil)
  (dom/article nil)
  (dom/aside nil)
  (dom/audio nil)
  (dom/b nil)
  (dom/base nil)
  (dom/bdi nil)
  (dom/bdo nil)
  (dom/big nil)
  (dom/blockquote nil)
  (dom/body nil)
  (dom/br nil)
  (dom/button nil)
  (dom/canvas nil)
  (dom/caption nil)
  (dom/cite nil)
  (dom/code nil)
  (dom/col nil)
  (dom/colgroup nil)
  (dom/data nil)
  (dom/datalist nil)
  (dom/dd nil)
  (dom/del nil)
  (dom/details nil)
  (dom/dfn nil)
  (dom/dialog nil)
  (dom/dl nil)
  (dom/dt nil)
  (dom/em nil)
  (dom/embed nil)
  (dom/g nil)
  (dom/fieldset nil)
  (dom/figcaption nil)
  (dom/figure nil)
  (dom/footer nil)
  (dom/form nil)
  (dom/h1 nil)
  (dom/h2 nil)
  (dom/h3 nil)
  (dom/h4 nil)
  (dom/h5 nil)
  (dom/h6 nil)
  (dom/head nil)
  (dom/header nil)
  (dom/hr nil)
  (dom/html nil)
  (dom/i nil)
  (dom/iframe nil)
  (dom/img nil)
  (dom/ins nil)
  (dom/kbd nil)
  (dom/keygen nil)
  (dom/label nil)
  (dom/legend nil)
  (dom/li nil)
  (dom/link nil)
  (dom/main nil)
  (dom/map nil)
  (dom/mark nil)
  (dom/menu nil)
  (dom/menuitem nil)
  (dom/meta nil)
  (dom/meter nil)
  (dom/nav nil)
  (dom/noscript nil)
  (dom/object nil)
  (dom/ol nil)
  (dom/optgroup nil)
  (dom/output nil)
  (dom/p nil)
  (dom/param nil)
  (dom/picture nil)
  (dom/pre nil)
  (dom/progress nil)
  (dom/q nil)
  (dom/rp nil)
  (dom/rt nil)
  (dom/ruby nil)
  (dom/s nil)
  (dom/svg nil)
  (dom/samp nil)
  (dom/script nil)
  (dom/section nil)
  (dom/small nil)
  (dom/source nil)
  (dom/span nil)
  (dom/strong nil)
  (dom/style nil)
  (dom/sub nil)
  (dom/summary nil)
  (dom/sup nil)
  (dom/table nil)
  (dom/tbody nil)
  (dom/td nil)
  (dom/tfoot nil)
  (dom/th nil)
  (dom/thead nil)
  (dom/time nil)
  (dom/title nil)
  (dom/tr nil)
  (dom/track nil)
  (dom/u nil)
  (dom/ul nil)
  (dom/var nil)
  (dom/video nil)
  (dom/wbr nil)
  (dom/div nil))

(define-clojure-indent
  (bs/panel :defn)
  (bs/panel-heading nil)
  (bs/panel-body nil)
  (bs/container-fluid nil)
  (bs/container nil )
  (bs/col nil )
  (bs/row nil )
  (bs/form-horizontal nil)
  (bs/labeled-input nil))

;; (define-clojure-indent
;;   (b/panel :defn)
;;   (b/panel-heading :defn)
;;   (b/panel-body :defn)
;;   (b/container-fluid :defn)
;;   (b/container :defn )
;;   (b/col :defn )
;;   (b/row :defn )
;;   (b/form-horizontal) :defn
;;   (b/labeled-input :defn))


(define-clojure-indent
  (specification :defn)
  (provided :defn)
  (behavior :defn)
  (assertions :defn))
