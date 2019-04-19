;;; ox-pelican.el --- Pelican-friendly Markdown Back-End for Org Export Engine

;; Copyright (C) 2018 Brandon T. Willard

;; Author: Brandon T. Willard
;; Keywords: org, pelican, markdown, github
;; Package-Version: 20180101.0000

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library extends the GitHub-flavored markdown backend
;; with changes for Pelican processing.

;;; Code:

(require 'cl-lib)
(require 'ox-md)
(require 'ox-html)
(require 'ox-gfm)
(require 'ox-publish)
(require 's)
(require 'projectile)
(require 'markdown-mode)

;;; User-Configurable Variables

(defgroup org-export-pelican nil
  "Options specific to Markdown export back-end."
  :tag "Org Pelican Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))


;;; Define Back-End

(org-export-define-derived-backend 'pelican 'gfm
  :menu-entry
  '(?g "Export to Pelican Flavored Markdown"
       ((?G "To temporary buffer"
            (lambda (a s v b) (org-pelican-export-as-markdown a s v)))
        (?g "To file" (lambda (a s v b) (org-pelican-export-to-markdown a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a
                  (org-pelican-export-to-markdown t s v)
                (org-open-file (org-pelican-export-to-markdown nil s v)))))))
  :translate-alist '((template . org-pelican-template)
		                 (section . org-pelican-md-section)
		                 (link . org-pelican-md-link)
                     (src-block . org-pelican-src-block))
  :options-alist '((:html-format-drawer-function nil nil #'org-pelican-html-format-drawer)
                   (:figure-dir "FIGURE_DIR" nil nil t)))


(defun org-pelican-html-format-drawer (name contents)
  "Turn drawers into HTML divs."
  ;; TODO: Add a class for result blocks?
  (format "<div class=\"%s\" markdown=\"\">\n%s\n</div>" (downcase name) contents))


;;; Transcode Functions
(defun org-pelican-md-link (link contents info)
  (if (org-export-inline-image-p link org-html-inline-image-rules)
      (let* ((type (org-element-property :type link))
             (path (let ((raw-path (org-element-property :path link)))
                     (cond
                      ((not (equal "file" type))
                       (concat type ":" raw-path))
                      ((not (file-name-absolute-p raw-path)) raw-path)
                      (t (expand-file-name raw-path)))))
             (caption (org-export-data (org-export-get-caption
                                        (org-export-get-parent-element link))
                                       info))
             ;; TODO: Generate IDs with the following:
             ;; (fig-num
             ;;  (or (org-element-property :CUSTOM_ID destination)
             ;;      (org-export-get-reference destination info)))
             (link-name (org-element-property :name
                                              (org-element-property :parent link)))
             (figure-lines '("<figure id=\"%1$s\">"
                             ;; "<span id=\"%1$s_span\" style=\"display:none;visibility:hidden\">"
                             ;; "$$\begin{equation}"
                             ;; --This one a unique number ID.
                             ;; "\tag{%2$s}"
                             ;; "\label{%1$s}"
                             ;; "\end{equation}$$"
                             ;; "</span>"
                             "![%2$s \\label{%1$s}](%3$s)"
                             "<figcaption>%2$s</figcaption>"
                             "</figure>")))
        (format
         (mapconcat #'identity figure-lines "\n")
         link-name
         ;; fig-num
         ;; (format "<span data-label=\"%s\"></span>" caption)
         caption
         (if (org-export-derived-backend-p org-export-current-backend 'md)
             ;;(not (org-string-nw-p caption))
             ;; Adjust the image filename to work with Pelican.
             (format "{attach}/%s"
                     (file-relative-name path
                                         (f-join (projectile-project-root)
                                                 "content")))
             path
           (format "%s \"%s\"" path caption))))
    (org-md-link link contents info)))

(defun org-pelican-md-section (paragraph contents info)
  (let ((block-type (org-element-property :type paragraph)))
    (if block-type
        (org-html-special-block paragraph contents info)
      (org-md-section paragraph contents info))))

(defun org-pelican-create-yaml (&optional info)
  (let* ((org-env (or info (org-export-get-environment)))
         (res '())
         (author-str
          (format "author: '%s'"
                  (car (plist-get org-env ':author))))
         (res (cons author-str res))
         (date-str
          (format "date: '%s'"
                  (car (plist-get org-env ':date))))
         (res (cons date-str res))
         (title-str
          (format "title: %s"
                  (car (plist-get org-env ':title))))
         (res (cons title-str res))
         (tags-str (s-join "," (plist-get org-env ':filetags)))
         (tags-str (and (not (s-blank? tags-str))
                        (format "tags: '%s'" tags-str)))
         (res (or (and tags-str
                       (cons tags-str res))
                  res))
         (mdate-str (calendar-current-date))
         (mdate-str (format "modified: '%s-%s-%s'"
                            (nth 2 mdate-str)
                            (nth 0 mdate-str)
                            (nth 1 mdate-str)))
         (res (or (and mdate-str
                       (cons mdate-str res))
                  res))
         (bib-str (plist-get org-env ':bibliography))
         (bib-str (and bib-str
                       (format "bibliography:\n- '%s'"
                               (apply #'f-join
                                      (cdr (f-split (car bib-str)))))))
         (res (or (and bib-str
                       (cons bib-str res))
                  res))
         (res (append '("---")
                      res
                      '("figure_dir: '{attach}/articles/figures/'"
                        "figure_ext: png"
                        "---"))))
    (mapconcat #'identity res "\n")))

(defun org-pelican-template (contents _info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  (let ((preamble (org-pelican-create-yaml _info)))
    (concat preamble "\n\n" contents)))

(defun org-pelican-src-block (src-block _contents info)
  "Allow attributes (only name, for now) in fenced code block definitions.

E.g., '```{#block-name .python}'

For Pandoc, use with 'fenced_code_attributes'
"
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (link-id (org-export-get-reference src-block info))
         ;; (name (org-element-property :name src-block))
         (prefix (if link-id
                     ;; TODO: Add a class for result blocks?
                     (concat "```{#" link-id " ." lang "}\n")
                   (concat "```{." lang "}\n")))
         (suffix "```"))
    (concat prefix code suffix)))

;;; Interactive function

(defun org-pelican-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Pelican Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Pelican Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'pelican "*Org Pelican Export*"
    async subtreep visible-only nil nil (lambda () (markdown-mode))))

;;;###autoload
(defun org-pelican-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Pelican Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'pelican outfile async subtreep visible-only)))

;;;###autoload
(defun org-pelican-publish-to-pelican (plist filename pub-dir)
  "Publish an org file to Markdown.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'pelican filename ".md" plist pub-dir))

(provide 'ox-pelican)
;;; ox-pelican.el ends here
