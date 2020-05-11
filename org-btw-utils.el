;;; org-btw-utils.el --- Customizations and extensions to org-mode
;;
;; Copyright (c) 2019-2020 Brandon T. Willard
;;
;; Author: Brandon T. Willard <brandonwillard@gmail.com>
;; URL: https://github.com/brandonwillard/org-btw
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(require 'cl-lib)
(require 'org-macs)
(require 'f)

(defun org-btw//org-publish-property (prop)
  "Get the publish property PROP (a tag/keyword like `:base-directory') for
the current file's project."
  (ignore-errors
    (org-publish-property prop
                          (org-publish-get-project-from-filename
                           (buffer-file-name (buffer-base-buffer))))))

(defun org-btw//org-export-get-parent-backends (backend)
  (when (symbolp backend) (setq backend (org-export-get-backend backend)))
  (when backend
    (let ((backends))
      (catch 'exit
        (while (org-export-backend-parent backend)
	        (unless backend ;; (memq (org-export-backend-name backend) backends)
	          (throw 'exit t))
	        (setq backend
	              (org-export-get-backend (org-export-backend-parent backend)))
          (setq backends (cons (org-export-backend-name backend) backends)))
        (reverse backends)))))

(defun org-btw//org-babel-get-call-var-value (var-name)
  "Extract the value of a named variable from a CALL statement."
  ;; What about `org-element-context' and `org-babel-parse-header-arguments'?
  (when-let ((el-info (org-babel-lob-get-info)))
    (car-safe
     (seq-filter #'identity
                 (map-values-apply
                  (lambda (x) (if (string-match (format "^%s=\"\\(.*\\)\"$" var-name) x)
                                  (match-string 1 x)))
                  (seq-filter (lambda (x) (eq (car x) :var))
                              (nth 2 el-info)))))))
(defmacro org-btw//org-babel-get-caller-var-value (var)
  `(or (org-with-point-at org-babel-current-src-block-location
         (org-btw//org-babel-get-call-var-value ,(symbol-name var)))
       ,var))

(when (featurep 'projectile)
  ;; TODO: Replace `projectile' dependencies with generic variables that can be
  ;; set to use `projectile'.
  (defun org-btw//org-export-output-project-file-name (orig-fun extension &optional subtreep pub-dir)
    "A wrapper for `org-export-output-file-name' that exports to a project's
corresponding source directory as determined by EXTENSION.

Uses `projectile-src-directory' plus the EXTENSION to determine the exact output
sub-directory."
    (let* ((projectile-require-project-root nil)
           (proj-root (projectile-project-root))
           (lang-src-dir (when proj-root
                           (f-join proj-root
                                   (projectile-src-directory (projectile-project-type))
                                   (s-chop-prefix "." extension))))
           (pub-dir (if (and lang-src-dir
                             (f-exists? lang-src-dir))
                        lang-src-dir
                      pub-dir)))
      (message "%s" pub-dir)
      (funcall orig-fun extension subtreep pub-dir)))
  (defun org-btw//org-compile-file (source process ext &optional err-msg log-buf spec)
    "Same as `org-compile-file' but considers the variable `org-projectile-output-dir'
for the output directory."
    (let* ((projectile-require-project-root nil)
           (proj-root (projectile-project-root))
           (base-name (file-name-base source))
           (full-name (file-truename source))
           (out-dir (or (f-join proj-root org-projectile-output-dir)
                        (file-name-directory source)
                        "./"))
           (output (expand-file-name (concat base-name "." ext)
                                     out-dir))
           (time (current-time))
           (err-msg (if (stringp err-msg)
                        (concat ".  " err-msg)
                      "")))
      (save-window-excursion (pcase process
                               ((pred functionp)
                                (funcall process
                                         (shell-quote-argument source)))
                               ((pred consp)
                                (let ((log-buf (and log-buf
                                                    (get-buffer-create log-buf)))
                                      (spec (append spec
                                                    `((?b . ,(shell-quote-argument base-name))
                                                      (?f . ,(shell-quote-argument source))
                                                      (?F . ,(shell-quote-argument full-name))
                                                      (?o . ,(shell-quote-argument out-dir))
                                                      (?O . ,(shell-quote-argument output))))))
                                  (dolist (command process)
                                    (shell-command (format-spec command spec)
                                                   log-buf))
                                  (when log-buf
                                    (with-current-buffer log-buf
                                      (compilation-mode)))))
                               (_ (error "No valid command to process %S: %s"
                                         source err-msg))))
      ;; Check for process failure.  Output file is expected to be
      ;; located in the same directory as SOURCE.
      (unless (org-file-newer-than-p output time)
        (error (format "File %S wasn't produced: %s" output
                       err-msg)))
      output)))

(defmacro org-btw//session-and-process-name (org-babel-buffer-alist
                                               &optional use-internal)
  (declare (debug (listp "org-babel-buffer-alist" &rest booleanp)))
  (let ((func-name (intern (concat "org-btw//"
                                   (symbol-name org-babel-buffer-alist)
                                   "-process-name"))))
    `(defun ,func-name (orig-func &optional internal)
       "Prepend the org-babel session name to `*-shell-get-process-name'."
       (if (and ,use-internal internal)
           (funcall orig-func internal)
         (let* ((session (cdr (assq :session
                                    (nth 2 (or org-src--babel-info
                                               (ignore-errors (org-babel-get-src-block-info)))))))
                (cached-process-name (when session
                                       (cdr (assoc (intern session) ,org-babel-buffer-alist)))))
           (if cached-process-name
               (string-remove-suffix "*" (string-remove-prefix "*" cached-process-name))
             (let* ((process-name-orig (funcall orig-func (and ,use-internal internal)))
                    (process-name
                     (if (and session (not (eq session :default))
                              (not (s-prefix? (format "[%s]" session) process-name-orig)))
                         (format "[%s]%s" session process-name-orig)
                       process-name-orig)))
               ;; Re-attach earmuffs, if they were present
               process-name)))))))

(defun org-btw//org-babel-execute-from-here (&optional arg)
  "Execute source code blocks from the subtree at the current point upward.
Call `org-babel-execute-src-block' on every source block in
the current subtree upward."
  (interactive "P")
  (save-restriction
    (save-excursion
      (narrow-to-region (point-min)
                        (progn (org-end-of-subtree t t)
	                             (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
	                             (point)))
      (org-babel-execute-buffer arg)
      (widen))))

(defun org-btw//org-remove-headlines (backend)
  "Remove headlines with :no_title: tag.

From https://emacs.stackexchange.com/a/9494/19170"
  ;; (org-map-entries (lambda ()
  ;;                    (let ((beg (point)))
  ;;                      (outline-next-visible-heading 1)
  ;;                      (backward-char)
  ;;                      (delete-region beg
  ;;                                     (point))))
  ;;                  "no_export"
  ;;                  'tree)
  (org-map-entries (lambda ()
                     (delete-region (point-at-bol)
                                    (point-at-eol)))
                   "no_title"))
(defun org-btw//set-nobreak-predicate ()
  (setq fill-nobreak-predicate
        (cl-pushnew #'org-btw//in-org-src-inline fill-nobreak-predicate)))
(defun org-btw//in-org-src-inline ()
  (let ((element (org-element-context)))
    (eq (nth 0 element) 'inline-src-block)))
(defun org-btw//org-make-link-regexps ()
    "Update the link regular expressions.
  This should be called after the variable `org-link-types' has changed."
    (let ((types-re (regexp-opt (org-link-types)
                                        t)))
      (setq org-bracket-link-regexp
            (rx-to-string `(seq "[["
                     (submatch
                      (one-or-more
                       (not
                        (any ?\[ ?\]))))
                     "]"
                     (zero-or-one
                      (submatch "["
                                (submatch
                                 (one-or-more
                                  ;; Simply add an exception for inline babel src statements.
                                  (or
                                   ;; Org inline src block/statement
                                   (zero-or-one ,org-babel-inline-src-rx)
                                   ;; This is the original condition.
                                   (not
                                    (any ?\[ ?\])))))
                                "]"))
                     "]"))
            org-any-link-re
            (concat "\\(" org-bracket-link-regexp "\\)\\|\\("
                    org-angle-link-re "\\)\\|\\("
                    org-plain-link-re "\\)"))))
(defun org-btw//org-element-inline-src-block-parser (limit)
  (-when-let* (((let ((case-fold-search nil))
                  (re-search-forward (rx (seq bow
                                              (submatch "src_"
                                                        (one-or-more (not (any blank ?\[ ?\\ ?\{))))))
                                     limit
                                     t)))
              ;; Start with the matched 'src_<lang>'
              (match-data-res (list (copy-marker (match-beginning 1))
                                    (point-marker)))
              ;; Add the matched block parameters
              (match-data-res (append match-data-res
                                      (list (point-marker))))
              (params-marker (progn
                                (org-element--parse-paired-brackets ?\[)
                                (when (< (point) limit)
                                    (point-marker))))
              (match-data-res (append match-data-res
                                      (list params-marker)))
              ;; Add the matched body
              (match-data-res (append match-data-res
                                      (list (point-marker))))
              (body-marker (progn
                              (org-element--parse-paired-brackets ?\{)
                              (when (< (point) limit)
                                (point-marker))))
              ;; Also push ranges for the entire pattern
              (match-data-res (append (list (car match-data-res))
                                      (list body-marker)
                                      match-data-res
                                      (list body-marker))))
    ;; Set and return the `match-data'
    (set-match-data match-data-res)
    match-data-res))

(defun org-btw//org-latex-src-block (oldfun src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to LaTeX.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information.

This is mostly the standard `ox-latex' with only the following differences:
  1. Float placement options for src blocks (e.g. listings) are now used.
  2. Optional tcolorbox listings environment for minted.
"
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((lang (org-element-property :language src-block))
           (caption (org-export-data (org-export-get-caption src-block) info)
                    ;; TODO: There's also a caption option in attr; prefer that one?
                    ;; (org-element-property :caption src-block)
                    )
           (caption-above-p (org-latex--caption-above-p src-block info))
           (label (org-element-property :name src-block))
           (custom-env (or (and lang
                                (cadr (assq (intern lang) org-latex-custom-lang-environments)))
                           nil
                           ;; TODO: Allow an `all' entry that applies to all languages
                           ;; (assoc-default (intern lang) org-latex-custom-lang-environments
                           ;;                nil (assq 'all org-latex-custom-lang-environments))
                           )
                       )
           (num-start (org-export-get-loc src-block info))
           (retain-labels (org-element-property :retain-labels src-block))
           (attributes (org-export-read-attribute :attr_latex src-block))
           (placement (let ((place (plist-get attributes :placement)))
                        (cond
                         (place (format "%s" place))
                         (t (plist-get info :latex-default-figure-position)))))
           (float (plist-get attributes :float))
           (listings (plist-get info :latex-listings))
           (listings-options (mapconcat #'identity
                                        ;; Only add [default] placement option when float is specified.
                                        (remove nil `(,(and float placement)
                                                      ,(plist-get attributes :listing-options)))
                                        ",")))
      (cond
       ((eq org-latex-listings-wrapper 'tcolorbox)
        (let* ((listings-env-name (or (plist-get attributes :listings-env-name) "oxtcblisting"))
               (body (format "\\begin{%s}{minted language=%s, %s}\n%s\n\\end{%1$s}"
                             ;;
                             listings-env-name
                             ;; Language.
                             (or (cadr (assq (intern lang)
                                             (plist-get info :latex-minted-langs)))
                                 (downcase lang))
                             ;; Options.
                             (mapconcat #'identity
                                        (remove nil
                                                `(,(if (org-string-nw-p caption)
                                                       (format "title={\\lstlistingname\\ \\thetcbcounter: {%s}}" caption))
                                                       ;; (format "listing options={caption={%s}}" caption))
                                                  ,(if (org-string-nw-p label)
                                                       (format "label type=listing, label={%s}" label))
                                                  ,(if (string= "t" float)
                                                       (format "float, floatplacement=%s" placement)
                                                     "nofloat")
                                                  ;; ,org-latex-tcolorbox-default-options
                                                  ,(plist-get attributes :options)))
                                        ",")
                             ;; Source code.
                             (let* ((code-info (org-export-unravel-code src-block))
                                    (max-width (apply 'max
                                                      (mapcar 'length
                                                              (org-split-string (car code-info)
                                                                                "\n")))))
                               (org-export-format-code (car code-info)
                                                       (lambda (loc _num ref)
                                                         (concat loc
                                                                 (when ref
                                                                   ;; Ensure references are flushed to the right,
			                                                             ;; separated with 6 spaces from the widest line
			                                                             ;; of code.
                                                                   (concat (make-string (+ (- max-width
                                                                                              (length loc))
                                                                                           6)
                                                                                        ?\s)
                                                                           (format "(%s)" ref)))))
                                                       nil
                                                       (and retain-labels
                                                            (cdr code-info)))))))
          ;; Return value.
          body))
       ((eq listings 'minted)
        (let* ((caption-str (org-latex--caption/label-string src-block
                                                             info))
               (listings-env-name (or (plist-get attributes :listings-env-name) "listing"))
               (float-env (cond
                           ((string= "multicolumn" float)
                            (format "\\begin{%s*}[%s]\n%s%%s\n%s\\end{%1$s*}"
                                    listings-env-name
                                    listings-options
                                    (if caption-above-p caption-str "")
                                    (if caption-above-p "" caption-str)))
                           (caption (format "\\begin{%s}[%s]\n%s%%s\n%s\\end{%1$s}"
                                            listings-env-name
                                            listings-options
                                            (if caption-above-p caption-str "")
                                            (if caption-above-p "" caption-str)))
                           ((string= "t" float)
                            (format "\\begin{%s}[%s]\n%%s\n\\end{%1$s}" listings-env-name listings-options))
                           (t "%s")))
               (options (plist-get info :latex-minted-options))
               (body (format "\\begin{minted}[%s]{%s}\n%s\\end{minted}"
                             ;; Options.

                             (concat (org-latex--make-option-string (if (or (not num-start)
                                                                            (assoc "linenos" options))
                                                                        options
                                                                      (append `(("linenos")
                                                                                ("firstnumber" ,(number-to-string (1+ num-start))))
                                                                              options)))
                                     (let ((local-options (plist-get attributes :options)))
                                       (and local-options
                                            (concat "," local-options))))
                             ;; Language.
                             (or (cadr (assq (intern lang)
                                             (plist-get info :latex-minted-langs)))
                                 (downcase lang))
                             ;; Source code.
                             (let* ((code-info (org-export-unravel-code src-block))
                                    (max-width (apply 'max
                                                      (mapcar 'length
                                                              (org-split-string (car code-info)
                                                                                "\n")))))
                               (org-export-format-code (car code-info)
                                                       (lambda (loc _num ref)
                                                         (concat loc
                                                                 (when ref
                                                                   ;; Ensure references are flushed to the right,
			                                                             ;; separated with 6 spaces from the widest line
			                                                             ;; of code.
                                                                   (concat (make-string (+ (- max-width
                                                                                              (length loc))
                                                                                           6)
                                                                                        ?\s)
                                                                           (format "(%s)" ref)))))
                                                       nil
                                                       (and retain-labels
                                                            (cdr code-info)))))))
          ;; Return value.
          (format float-env body)))

       ;; Case 4.  Use listings package.
       (t (funcall oldfun src-block _contents info))))))

(defun org-btw//org-babel-hy-session-buffer (orig-func session)
  "Make org-babel's default Hy session buffer naming follow `hy-mode' by forcing
 `org-babel-hy-session-buffer' to return a name for non-initialized sessions."
  (let ((hy-buffer-name (cdr (assoc session org-babel-hy-buffers))))
    (or hy-buffer-name (hy--shell-format-process-name (hy-shell-get-process-name nil)))))


(provide 'org-btw-utils)
