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

(defun org-ref+//org-ref-enable-bib-as-option ()
  "Add BIBLIOGRAPHY and BIBLIOGRAPHYSTYLE as universal export options and
 include an org export filter that produces a bibliography at the location of
 the the BIBLIOGRAPHY option."
  (add-to-list 'org-export-options-alist
                '(:bibliography "BIBLIOGRAPHY" nil nil split))
  (add-to-list 'org-export-options-alist
                '(:bibliographystyle "BIBLIOGRAPHYSTYLE" nil
                                    nil t))
  (advice-add 'org-ref-find-bibliography :override 'org-ref+//org-ref-find-bibliography)
  (add-to-list 'org-export-filter-parse-tree-functions
                'org-ref+//org-ref-parse-bib-latex-entries))

(defun org-ref+//org-ref-disable-bib-as-option ()
  (setq org-export-options-alist (assq-delete-all :bibliography org-export-options-alist))
  (setq org-export-options-alist (assq-delete-all :bibliographystyle org-export-options-alist))
  (advice-remove 'org-ref-find-bibliography 'org-ref+//org-ref-find-bibliography)
  (remove-hook 'org-export-filter-parse-tree-functions 'org-ref+//org-ref-parse-bib-latex-entries))

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
                ;; `org-ref' doesn't know about derived modes, so use the parent.
                (backend-parent (or (org-export-backend-parent (org-export-get-backend backend))
                                    backend))
                ;; Truncate paths (i.e. rely on LaTeX build to figure out bib file locations).
                ;; Could use `file-relative-name' against `(plist-get info :publishing-directory)'.
                (bib-locs (mapconcat #'file-name-nondirectory (plist-get info :bibliography) ","))
                (bib-value (org-ref-bibliography-format
                            bib-locs
                            ;; (org-element-property :value last-bib-elem)
                            nil backend-parent))
                (parent (org-element-property :parent last-bib-elem))
                (parent-end (org-element-property :end parent))
                (new-bib-elem))
          (if (and bib-style (eq backend-parent 'latex))
              (setq bib-value (concat (format "\\bibliographystyle{%s}\n" bib-style) bib-value)))
          (setq new-bib-elem (org-element-create 'export-block
                                                  (list :type (string-inflection-upcase-function (symbol-name backend-parent))
                                                        :value bib-value
                                                        :begin parent-end
                                                        :end (+ parent-end (seq-length bib-value)))))
          (org-element-set-element last-bib-elem new-bib-elem)))
    tree))

(define-minor-mode org-ref+-mode
  "Toggle org-ref+ mode."
  :require 'org-ref
  :init-value nil
  :global t
  (if org-ref+-mode
      (org-ref+//org-ref-enable-bib-as-option)
    (org-ref+//org-ref-disable-bib-as-option)))

(provide 'org-ref+)
;;; org-ref+.el ends here
