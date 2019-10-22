;;; org-ref+.el --- Customizations and extensions to org-ref
;;
;; Copyright (c) 2012-2019 Brandon T. Willard
;;
;; Author: Brandon T. Willard <brandonwillard@gmail.com>
;; URL: https://github.com/brandonwillard/org-btw
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;; This package provides customizations and extensions to org-ref.
;; For instance, it changes BIBLIOGRAPHY to a proper org export option.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ox)
(require 'org-ref)
(require 's)
(require 'f)
(require 'string-inflection)


(defun org-ref+//org-ref-bibliography-format (old-func keyword desc format)
  (let ((backends (cons format (org-ref+//org-export-get-parent-backends format))))
    (cl-some (lambda (bend)
               (funcall old-func keyword desc bend))
             backends)))

(defun org-ref+//org-ref-enable-bib-as-option ()
  "Add BIBLIOGRAPHY and BIBLIOGRAPHYSTYLE as universal export options and
 include an org export filter that produces a bibliography at the location of
 the the BIBLIOGRAPHY option."

  (advice-add #'org-ref-bibliography-format :around #'org-ref+//org-ref-bibliography-format)

  (add-to-list 'org-export-options-alist
                '(:bibliography "BIBLIOGRAPHY" nil nil split))
  (add-to-list 'org-export-options-alist
                '(:bibliographystyle "BIBLIOGRAPHYSTYLE" nil
                                    nil t))

  (advice-add #'org-ref-find-bibliography :override #'org-ref+//org-ref-find-bibliography)

  (add-to-list 'org-export-filter-parse-tree-functions
                #'org-ref+//org-ref-parse-bib-latex-entries))

(defun org-ref+//org-ref-disable-bib-as-option ()
  (setq org-export-options-alist (assq-delete-all :bibliography org-export-options-alist))
  (setq org-export-options-alist (assq-delete-all :bibliographystyle org-export-options-alist))

  (advice-remove #'org-ref-bibliography-format #'org-ref+//org-ref-bibliography-format)
  (advice-remove #'org-ref-find-bibliography #'org-ref+//org-ref-find-bibliography)

  (remove-hook 'org-export-filter-parse-tree-functions #'org-ref+//org-ref-parse-bib-latex-entries))

(defun* org-ref+//create-md-link (key &optional (format-string "%a (%y)"))
  "A custom Markdown formatting for references."
  (format "<a href=\"#%s\">%s</a>"
          (md5 key)
          (let ((org-ref-bibliography-files (org-ref-find-bibliography))
                (bib-file)
                (author)
                (year)
                (entry)
                (bibtex-entry)
                (format)
                (entry-type))
            (setq bib-file
                  (cl-some
                   (lambda (file)
                     (when (org-ref-key-in-file-p key file)
                       file))
                   (if (listp org-ref-bibliography-files)
                       org-ref-bibliography-files
                     (list org-ref-bibliography-files))))
            (with-temp-buffer
              (insert-file-contents bib-file)
              (bibtex-set-dialect (parsebib-find-bibtex-dialect) t)
              (bibtex-search-entry key nil 0)
              (setq bibtex-entry (bibtex-parse-entry))
              (dolist (cons-cell bibtex-entry)
                (setf (car cons-cell) (downcase (car cons-cell))))
              (setq author (cdr (assoc "author" bibtex-entry)))
              (setq year (cdr (assoc "year" bibtex-entry)))
              (setq entry (org-ref-reftex-format-citation bibtex-entry format-string)))
            (replace-regexp-in-string "[\"\{\}]" "" (htmlize-escape-or-link entry)))))

;; TODO: Make `[[eqref:...]]` output `\(\eqref{...}\)` in Markdown output.
;; Might be related to `org-ref-ref-html', but definetly involves `org-ref-eqref-export'.
(defun org-ref+//org-ref-eqref-export (old-fun keyword desc format)
  (cond
   ((or (eq format 'html) (eq format 'md))
    (format "\\(\\eqref{%s}\\)" keyword))
   (t (funcall old-fun keyword desc format))))

(defmacro create-org-ref-generic-format-function (link-type)
  "A macro that turns `org-ref-format-*' functions into a generic methods, so
 that they're easier to extend."
  (pcase (macroexpand-1 `(org-ref-make-format-function ,link-type))
    (`(defun ,name ,args ,doc . ,body)
     `(cl-defgeneric ,name ,args ,@body))))

(defun org-ref+//org-ref-enable-md-cites ()
  ;; Add a format for "misc" reference types.
  (add-to-list 'org-ref-bibliography-entry-format
               '("misc" . "%a, %t, <i>%j</i>, %p (%y). <a href=\"%U\">link</a>."))
  ;; TODO: Add to `org-ref-formatted-citation-formats', as well?

  (create-org-ref-generic-format-function "citet")
  (create-org-ref-generic-format-function "citep")
  (create-org-ref-generic-format-function "citetitle")

  ;; These versions use `org-ref' for formatting.
  ;; TODO use `org-export-derived-backend-p' as the condition in a `cl-generic-define-generalizer'.
  (cl-defmethod org-ref-format-citet (keyword desc (format (eql md)))
    "A specialized method for citet (textual) links/references with Markdown
 formatting."
    (mapconcat #'org-ref+//create-md-link
               (s-split ",[ \t\n\r]+" keyword)
               ", "))

  (cl-defmethod org-ref-format-citep (keyword desc (format (eql md)))
    "A specialized method for citep (parenthetical) links/references with Markdown
 formatting."
    (mapconcat (lambda (x) (org-ref+//create-md-link x "(%a %y)"))
               (s-split ",[ \t\n\r]+" keyword)
               ", "))

  (cl-defmethod org-ref-format-citetitle (keyword desc (format (eql md)))
    (mapconcat (lambda (x) (org-ref+//create-md-link x "%t"))
               (s-split ",[ \t\n\r]+" keyword)
               ", "))

  ;; Test 'em out.
  ;; (let ((org-ref-default-bibliography (file-truename "test.bib")))
  ;;   (org-ref-format-citet "WillardProgrammingIntelligentCity2018a, WillardProgrammingIntelligentCity2018a" nil 'md)
  ;;   (org-ref-format-citep "WillardProgrammingIntelligentCity2018a" nil 'md)
  ;;   (org-ref-format-citetitle "WillardProgrammingIntelligentCity2018a" nil 'md))

  (advice-add #'org-ref-eqref-export :around #'org-ref+//org-ref-eqref-export))

(defun org-ref+//org-ref-disable-md-cites ()
  (setq org-ref-bibliography-entry-format
        (assoc-delete-all "misc" org-ref-bibliography-entry-format #'string-equal))

  ;; TODO: Remove generic function and reestablish original functions.
  ;; (fmakunbound #'org-ref-format-citet)
  ;; (fmakunbound #'org-ref-format-citep)

  (advice-remove #'org-ref-eqref-export #'org-ref+//org-ref-eqref-export))

(defun org-ref+//org-ref-find-bibliography ()
"Find the bibliography files in the current buffer.

This function sets and returns `org-ref-bibliography-files' obtained from
#+BIBLIOGRAPHY options."
  (prog1
      ;; When called from within a bibtex file, assume we want it; otherwise,
      ;; check the current file for a bibliography source.
      (setq org-ref-bibliography-files (or (and (f-ext? buffer-file-name "bib")
                                                (list buffer-file-name))
                                            (plist-get (org-export-get-environment)
                                                      :bibliography)
                                            org-ref-default-bibliography))

      ;; TODO: Obtain items within latex tokens '\bibliography' and '\addbibresource'.
      ;; Try `org-map-tree' and follow the example of `org-latex-math-block-tree-filter'
      ;; (and/or `org-element-latex-fragment-parser').

      ;; Set reftex-default-bibliography so we can search.
      (setq-local reftex-default-bibliography org-ref-bibliography-files)))

(defun org-ref+//org-export-get-parent-backends (backend)
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

(defun org-ref+//org-ref-parse-bib-latex-entries (tree backend info)
  "Add an export block with the bibliography at the location of the last bibliography
keyword."
  (let ((last-bib-elem))
    (org-element-map tree
        '(keyword)
      (lambda (element)
        (if (string-equal (org-element-property :key element) "BIBLIOGRAPHY")
            (setq last-bib-elem element)))
      info)
    (if last-bib-elem
        (let* ((bib-style (plist-get info :bibliographystyle))
               ;; Truncate paths (i.e. rely on LaTeX build to figure out bib file locations).
               ;; Could use `file-relative-name' against `(plist-get info :publishing-directory)'.
               (bib-locs (mapconcat #'file-name-nondirectory (plist-get info :bibliography) ","))
               ;; XXX: `org-ref-bibliography-format' is very lame and simply
               ;; `cond'itions on a fixed set of formats, so there's no good way
               ;; to make this work with an export format derived from another
               ;; derived format.
               ;; `org-ref' doesn't know about derived modes, so use the parent.
               ;; TODO: would be much better if we could use `org-export-derived-backend-p'.
               (backend-parents (org-ref+//org-export-get-parent-backends backend))
               (backend-and-bib-value (cl-some (lambda (bend)
                                     (cons bend (org-ref-bibliography-format bib-locs nil bend)))
                                   backend-parents))
               (backend (car backend-and-bib-value))
               (backend-parent (org-export-backend-parent (org-export-get-backend backend)))
               (bib-value (cdr backend-and-bib-value))
               (parent (org-element-property :parent last-bib-elem))
               (parent-end (org-element-property :end parent))
               (new-bib-elem))
          (when (and bib-style (eq backend-parent 'latex))
            (setq bib-value (concat (format "\\bibliographystyle{%s}\n" bib-style) bib-value)))
          (setq new-bib-elem (org-element-create 'export-block
                                                 (list :type (string-inflection-upcase-function (symbol-name backend-parent))
                                                       :value bib-value
                                                       :begin parent-end
                                                       :end (+ parent-end (seq-length bib-value)))))
          (org-element-set-element last-bib-elem new-bib-elem)))
    tree))

;;;###autoload
(define-minor-mode org-ref+-mode
  "Toggle org-ref+ mode."
  :require 'org-ref
  :init-value nil
  :global t
  (if org-ref+-mode
      (progn
        (org-ref+//org-ref-enable-md-cites)
        (org-ref+//org-ref-enable-bib-as-option))
    (progn
      (org-ref+//org-ref-disable-md-cites)
      ;; (progn (unload-feature 'org-ref) (load-library "org-ref"))
      (org-ref+//org-ref-disable-bib-as-option))))

(provide 'org-ref+)
;;; org-ref+.el ends here
