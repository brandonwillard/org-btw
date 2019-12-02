;;; ox-sphinx.el --- Advanced reStructuredText Back-End for Org Export Engine

;; Copyright (C) 2019 Brandon T. Willard

;; Author: Brandon T. Willard
;; Keywords: org, rst
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

;; This library that extends the standard reStructuredText backend.

;;; Code:

(require 'cl-lib)
(require 'ox)
(require 'ox-rst)
(require 'ox-publish)
(require 's)

;;; User-Configurable Variables

(defgroup org-export-sphinx nil
  "Options specific to Sphinx export back-end."
  :tag "Org Sphinx"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))


(defcustom org-sphinx-src-block-ref-role ":ref:"
  "The role to use for source block references."
  :group 'org-export-sphinx
  :type 'string)

(defcustom org-sphinx-src-block-always-caption nil
  "Always add a caption to source blocks (default value is the source block name)."
  :group 'org-export-sphinx
  :type 'boolean)



;;; Define Back-End

(org-export-define-derived-backend 'sphinx 'rst
  :menu-entry
  '(?u "Export to Sphinx"
       ((?R "To temporary buffer"
            (lambda (a s v b e) (org-rst-export-as-rst a s v e)))
        (?r "To file" (lambda (a s v b e) (org-rst-export-to-rst a s v e)))))
  :options-alist '((:rst-src-block-always-caption nil nil org-sphinx-src-block-always-caption)
                   (:rst-src-block-ref-role nil nil org-sphinx-src-block-ref-role))
  :translate-alist '((link . org-sphinx-link)
                     (inline-src-block . org-sphinx-inline-src-block)
                     (src-block . org-sphinx-src-block)))


;;; Internal functions
(defun org-sphinx-src-block (src-block _contents info)
  "Transcode a SRC-BLOCK element from Org to reStructuredText.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (when (org-string-nw-p (org-element-property :value src-block))
    (let* ((lang (org-element-property :language src-block))
		       (label (org-element-property :name src-block))
		       (value (org-remove-indentation
				           (org-element-property :value src-block)))
           (num-start (org-export-get-loc src-block info))
           (codeblockd (plist-get info :rst-code-block))
           (caption (or (org-string-nw-p
                         (org-export-data (org-export-get-caption src-block) info))
                        (and (plist-get info :rst-src-block-always-caption)
                             label)))
		       (attributes
			      (org-export-read-attribute :attr_rst src-block))
		       (class (plist-get attributes :class)))
      (cond
       ;; Case 1.
       ((eq codeblockd 'code-block)
		    (let ((lst-lang
			         (or (cadr (assq (intern lang) org-rst-pygments-langs)) lang)))
		      (concat
		       (format ".. code-block:: %s\n" lst-lang)
		       (when num-start (format "    :lineno-start: %s\n" (1+ num-start)))
		       (when caption (format "    :caption: %s\n" caption))
		       (when class (format "    :class: %s\n" class))
		       (when label (format "    :name: %s\n" label))
		       "\n"
		       (org-rst--indent-string value org-rst-quote-margin))))
	     ;; Case 2. code.
	     ((eq codeblockd 'code)
		    (let ((lst-lang
			         (or (cadr (assq (intern lang) org-rst-pygments-langs)) lang)))
		      (concat
		       (format ".. code:: %s\n" lst-lang)
		       (when num-start (format "    :number-lines: %s\n" (1+ num-start)))
		       (when class (format "    :class: %s\n" class))
		       (when caption (format "    :caption: %s\n" caption))
		       (when label (format "    :name: %s\n" label))
		       "\n"
		       (org-rst--indent-string value org-rst-quote-margin))))
       (t
        (concat
         "::\n"
         (when class (format "    :class: %s\n" class))
		     (when caption (format "    :caption: %s\n" caption))
         (when label (format "    :name: %s\n" label))
         "\n"
         (org-rst--indent-string value org-rst-quote-margin)))))))

(defun org-sphinx-link (link desc info)
  "Transcode a LINK object from Org to reStructuredText.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information."
  (let* ((home (when (plist-get info :rst-link-home)
				         (org-trim (plist-get info :rst-link-home))))
		     (use-abs-url (plist-get info :rst-link-use-abs-url))
		     (link-org-files-as-rst-maybe
		      (function
		       (lambda (raw-path info)
			       "Treat links to `file.org' as links to `file.rst', if needed.
           See `org-rst-link-org-files-as-rst'."
			       (cond
			        ((and (plist-get info :rst-link-org-files-as-rst)
					          (string= ".org"
							               (downcase (file-name-extension raw-path "."))))
			         (concat (file-name-sans-extension raw-path) "."
					             (plist-get info :rst-extension)))
			        (t raw-path)))))
		     (type (org-element-property :type link))
		     (search-option (org-element-property :search-option link))
		     (raw-path (org-element-property :path link))
		     ;; Ensure DESC really exists, or set it to nil.
		     (desc (and (not (string= desc "")) desc))
		     (path (cond
				        ((member type '("http" "https" "ftp" "mailto"))
				         (url-encode-url
				          (concat type ":" raw-path)))
				        ((string= type "file")
				         ;; Treat links to ".org" files as ".rst", if needed.
				         (setq raw-path
					             (funcall link-org-files-as-rst-maybe raw-path info))
				         (cond ((and home use-abs-url)
						            (setq raw-path
							                (concat (file-name-as-directory home) raw-path)))
                       (t raw-path)))
				        (t raw-path)))
		     (attributes-plist
		      (org-combine-plists
		       ;; Extract attributes from parent's paragraph.  HACK: Only
		       ;; do this for the first link in parent (inner image link
		       ;; for inline images).  This is needed as long as
		       ;; attributes cannot be set on a per link basis.
		       (let* ((parent (org-export-get-parent-element link))
				          (link (let ((container (org-export-get-parent link)))
						              (if (and (eq 'link (org-element-type container))
								                   (org-rst-inline-image-p link info))
							                container
							              link))))
			       (and (eq link (org-element-map parent 'link #'identity info t))
				          (org-export-read-attribute :attr_rst parent)))
		       ;; Also add attributes from link itself.	 Currently, those
		       ;; need to be added programmatically before `org-rst-link'
		       ;; is invoked, for example, by backends building upon HTML
		       ;; export.
		       (org-export-read-attribute :attr_rst link)))
		     (attributes
		      (let ((attr (org-rst--make-attribute-string attributes-plist)))
			      (if (org-string-nw-p attr) (concat "\n" attr "\n") ""))))
    (cond
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'rst))
     ;; Image file.
     ((and (plist-get info :rst-inline-images)
           (org-export-inline-image-p
            link (plist-get info :rst-inline-image-rules)))
	    (let* ((ipath (if (not (file-name-absolute-p raw-path)) raw-path
					            (expand-file-name raw-path)))
             (caption (org-export-get-caption
					             (org-export-get-parent-element link)))
             (linkname
              (org-element-property :name (org-export-get-parent-element link)))
             (label (if linkname (format ".. _%s:\n\n" linkname) "")))
		    (if caption (format "%s.. figure:: %s%s\n\n    %s\n"
                            label ipath attributes
							              (org-export-data caption info))
		      (format "%s.. image:: %s%s\n" label ipath attributes))))
     ((and (plist-get info :rst-inline-images)
           desc
           (my-org-export-inline-image-p
            desc (plist-get info :rst-inline-image-rules)))
      (format ".. image:: %s\n    :target: %s%s" desc path attributes))
     ;; Radio link: Transcode target's contents and use them as link's
     ;; description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
		    (when destination
			    (format "`%s <%s>`_"
					        path
					        (org-export-data (org-element-contents destination) info)))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
							               (org-export-resolve-fuzzy-link link info)
						               (org-export-resolve-id-link link info))))
		    (cl-case (org-element-type destination)
          ;; Source block
          (src-block
           (let ((ref-role (plist-get info :rst-src-block-ref-role)))
             (if (not desc) (format "%s`%s`" ref-role path)
               (format "%s`%s <%s>`" ref-role desc path))))
		      ;; Id link points to an external file.
		      (plain-text
		       (if desc (format "`%s <%s>`_" desc destination)
			       (format "`%s`_" destination)))
		      ;; Fuzzy link points nowhere.
		      ('nil
		       (let ((rawlink
				          (org-export-data (org-element-property :raw-link link) info)))
			       (if desc (format "`%s <%s>`_" desc rawlink)
			         (format "`%s`_" rawlink))))
		      ;; LINK points to a headline.
		      (headline
           (if (member type '("custom-id" "id"))
               (if (plist-get info :rst-link-use-ref-role)
                   (if desc (format " :ref:`%s <%s>`" desc raw-path)
                     (format " :ref:`%s`" raw-path))
                 (format "`%s`_" raw-path))
             (format "`%s`_" (org-rst--build-title destination info nil))))
          ;; Fuzzy link points to a target.
		      (otherwise
           (if (not desc) (format "`%s`_" path)
             (format "`%s <%s>`_" desc path))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number. It is not supported in ReST.
     ((string= type "coderef")
      (format (org-export-get-coderef-format path desc)
			        (org-export-resolve-coderef path info)))
     ((and (plist-get info :rst-file-link-use-ref-role)
           (string= type "file")
           search-option)
      (let ((ref (replace-regexp-in-string "^#" "" search-option)))
        (if desc
            (format ":ref:`%s <%s>`" desc ref)
          (format ":ref:`%s`" ref))))
     ;; Link type is handled by a special function.
                                        ;((functionp (setq protocol (nth 2 (assoc type org-link-protocols))))
                                        ; (funcall protocol (org-link-unescape path) desc 'latex))
     ;; External link with a description part.
     ((and path desc) (format "`%s <%s>`_" desc path))
     ;; External link without a description part.
     (path (format "`%s <%s>`_"
                   (replace-regexp-in-string "^//" "" path) path))
     ;; No path, only description.  Try to do something useful.
     (t (format "`%s <%s>`_" desc desc)))))

(defun org-sphinx-inline-src-block (inline-src-block _contents info)
  "Transcode an INLINE-SRC-BLOCK element from Org to reStructuredText.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (format "\\ %s\\ "
          (org-rst--text-markup
           (org-element-property :value inline-src-block) 'verbatim info)))


;;; Interactive functions

;;;###autoload
(defun org-sphinx-export-as-rst (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'sphinx "*Org Sphinx Export*"
    async subtreep visible-only body-only ext-plist (lambda () (rst-mode))))

;;;###autoload
(defun org-sphinx-export-to-rst (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let* ((extension (concat "." (or (plist-get ext-plist :rst-extension)
                                    org-rst-extension
                                    "rst")))
         (file (org-export-output-file-name extension subtreep)))
    (org-export-to-file 'sphinx file
      async subtreep visible-only body-only ext-plist)))

;;;###autoload
(defun org-sphinx-publish-to-rst (plist filename pub-dir)
  (org-publish-org-to 'sphinx filename ".rst" plist pub-dir))


(provide 'ox-sphinx)

;;; ox-sphinx.el ends here
