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
(require 'dash)
(require 'projectile)
(require 'markdown-mode)
(require 'org-btw-utils)

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
  '(?p "Export to Pelican Flavored Markdown"
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
             ;; If the given path doesn't have the image (or only an image name
             ;; is given), then check the project's figure directory--if there
             ;; is one.
             (path (if-let* (((not (f-exists? path)))
                             (base-dir (or (org-btw//org-publish-property :base-directory) default-directory))
                             (figure-dir (or (org-btw//org-publish-property :figure-dir) default-directory))
                             (new-path (f-join figure-dir path))
                             ((f-exists? new-path)))
                       (f-relative new-path base-dir)
                     path))
             (caption (org-export-data (org-export-get-caption
                                        (org-export-get-parent-element link))
                                       info))
             ;; TODO: Generate IDs with the following:
             ;; (fig-num
             ;;  (or (org-element-property :CUSTOM_ID destination)
             ;;      (org-export-get-reference destination info)))
             (link-name (org-element-property :name
                                              (org-element-property :parent link)))
             (figure-lines '("<figure id=\"%1$s\" class=\"plot\">"
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
  "Create a YAML block with meta information used by Pelican."
  (let* ((org-env (or info (org-export-get-environment)))
         (res '())
         (res (or (-some--> (car (plist-get org-env ':author))
                    (format "author: '%s'" it)
                    (cons it res))
                  res))
         (res (or (-some--> (plist-get org-env ':date)
                    (format "date: '%s'" (car it))
                    (cons it res))
                  res))
         (res (or (-some--> (car (plist-get org-env ':title))
                    (format "title: %s" it)
                    (cons it res))
                  res))
         ;; Add file tags to the metadata
         (file-tags (plist-get org-env ':filetags))
         (res (or (-some--> file-tags
                    (s-join "," it)
                    (format "tags: '%s'" it)
                    (cons it res))
                  res))
         ;; If "draft" or "published" is a tag, put it in the "status" metadata
         ;; field
         (res (or (-some--> file-tags
                    (cond
                     ((member "draft" it)
                      (format "status: draft"))
                     ((member "published" it)
                      (format "status: published")))
                    (cons it res))
                  res))
         ;; Add a modified date
         (res (or (-some--> (calendar-current-date)
                    (format "modified: '%s-%s-%s'"
                            (nth 2 it)
                            (nth 0 it)
                            (nth 1 it))
                    (cons it res))
                  res))
         (res (or (-some--> (plist-get org-env ':bibliography)
                    (format "bibliography:\n%s"
                            (mapconcat (lambda (x) (format "- '%s'" (f-expand x))) it "\n"))
                    (cons it res))
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

For Pandoc, use with 'backtick_code_blocks' and 'fenced_code_attributes'.
"
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (caption (org-export-data (org-export-get-caption
                                    ;; (org-export-get-parent-element link)
                                    src-block)
                                   info))
         (name (org-element-property :name src-block))
         ;; We only want to add the link name and special CSS class when a
         ;; reference is actually made to this src-block.
         (has-ref (and name
                       (org-element-map (plist-get info :parse-tree)
                           'link
                         (lambda (l)
                           (let ((id (org-element-property :path l)))
                             (string-equal name id)))
                         nil
                         t)))
         (link-id (and name
                       has-ref
                       (org-export-get-reference src-block info)))
         (prefix (if link-id
                     (format "<figure id=\"%s\">\n```{.%s}\n" link-id lang)
                   (concat "<figure>\n```{." lang "}\n")))
         (suffix (if (or link-id (not (string-empty-p caption)))
                     (format "```\n<figcaption>Listing %s%s</figcaption>\n</figure>"
                             (org-export-get-ordinal src-block info)
                             (if (string-empty-p caption)
                                 ""
                               (concat ": " caption)))
                   "```\n</figure>")))
    (concat prefix code suffix)))

;;; Interactive functions

;;;###autoload
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
