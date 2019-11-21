(require 'org)

(when (> emacs-major-version 26)
  (defalias 'ert--print-backtrace 'backtrace-to-string))


(defmacro org-test-with-temp-text (text &rest body)
  "Run body in a temporary buffer with Org mode as the active
mode holding TEXT.  If the string \"<point>\" appears in TEXT
then remove it and place the point there before running BODY,
otherwise place the point at the beginning of the inserted text."
  (declare (indent 1))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text)))
	       (org-mode-hook nil))
     (with-temp-buffer
       (org-mode)
       (let ((point (string-match "<point>" inside-text)))
	       (if point
	           (progn
	             (insert (replace-match "" nil nil inside-text))
	             (goto-char (1+ (match-beginning 0))))
	         (insert inside-text)
	         (goto-char (point-min))))
       ,@body)))

(def-edebug-spec org-test-with-temp-text (form body))
