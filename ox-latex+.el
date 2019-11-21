;;; ox-latex+.el --- Advanced LaTeX Back-End for Org Export Engine

;; Copyright (C) 2018 Brandon T. Willard

;; Author: Brandon T. Willard
;; Keywords: org, latex
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

;; This library extends the standard LaTeX backend.

;;; Code:

(require 'cl-lib)
(require 'ox)
(require 'ox-latex)
(require 'ox-publish)
(require 's)

;;; User-Configurable Variables

(defgroup org-export-latex+ nil
  "Options specific to LaTeX+ export back-end."
  :tag "Org LaTeX+ LaTeX"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(defcustom org-latex+-format-non-env-drawers '("results")
  "The drawer names in this list will not be exported as LaTeX environments."
  :group 'org-export-latex+
  :type '(repeat (string :tag "Drawer Name")))

(defcustom org-latex+-pdf-output-dir nil
  "Output directory for PDF files."
  :group 'org-export-latex+
  :type '(string :tag "PDF Output Directory"))

(defcustom org-latex+-listings-wrapper
  'tcolorbox
  "Wrapper for listings (e.g. tcolorbox)"
  :group 'org-export-latex+
  :type '(const :tag "LaTeX Listings Wrapper"))

(defcustom org-latex+-tcolorbox-listing-env
  "\\newtcblisting[auto counter,number within=section]{oxtcblisting}[1]{%
\tframe hidden,
\tlisting only,
\tlisting engine=minted,
\tminted options={fontsize=\\scriptsize},
\tbreakable,
\tenhanced,
\ttitle after break={\\raggedleft\\lstlistingname\\ \\thetcbcounter~ -- continued},
\tlisting remove caption=false,
\tarc=0pt,
\touter arc=0pt,
\tboxrule=0pt,
\tcoltitle=black,
\tcolbacktitle=white,
\tcenter title,
\t#1
}"
  "Default tcolorbox listings environment.

`ox-latex+' uses the listing named \"oxtcblisting\", so the environment that this command
creates should use that name."
  :group 'org-export-latex+
  :type '(string :tag "tcolorbox Listing Definition"))


;;; Define Back-End

(org-export-define-derived-backend 'latex+ 'latex
  :filters-alist '((:filter-body . org-latex+//org-export-latex-add-tcolorbox))
  :menu-entry
  '(?i "Export to LaTeX+"
       ((?L "To temporary buffer"
            (lambda (a s v b) (org-latex+-export-as-latex a s v)))
        (?l "To file" (lambda (a s v b) (org-latex+-export-to-latex a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a
                  (org-latex+-export-to-latex t s v)
                (org-open-file (org-latex+-export-to-latex+ nil s v)))))
        (?p "As PDF file" org-latex+-export-to-pdf)
        (?O "As PDF file and open"
	          (lambda (a s v b)
	            (if a
                  (org-latex+-export-to-pdf t s v b)
                (org-open-file (org-latex+-export-to-pdf nil s v b)))))))
  :translate-alist '((template . org-latex+//org-latex-template)
                     (src-block . org-latex+//org-latex-src-block))
  :options-alist '((:latex-format-drawer-function nil nil #'org-latex+//org-format-drawer-function)
                   (:latex-listings nil nil 'minted)
                   (:figure-dir "FIGURE_DIR" nil nil t)))


;;; Internal functions

;;;###autoload
(defun org-latex+//org-latex-template (contents info)
  (let ((org-latex-listings 'minted)
        (org-latex-prefer-user-labels t)
        (org-latex-packages-alist '(("" "minted")
                                    ("minted, listings, breakable, skins" "tcolorbox"))))
    (org-latex-template contents info)))

(defun org-latex+//org-format-drawer-function (name contents)
  "Turn drawers into custom LaTeX blocks."
  (let ((name (downcase name)))
    (unless (member name org-latex+-format-non-env-drawers)
      (format "\\begin{%s}\n%s\n\\end{%s}" name contents name))))

(defun org-latex+//org-latex-src-block (src-block _contents info)
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
       ((eq org-latex+-listings-wrapper 'tcolorbox)
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
       (t (org-latex-src-block src-block _contents info))))))

(defun org-latex+//org-export-latex-add-tcolorbox (body backend info)
  "Add a custom tcolorbox listing environment to the latex header."
  (concat org-latex+-tcolorbox-listing-env "\n" body))


;;; Interactive functions

;;;###autoload
(defun org-latex+-export-as-latex (&optional async subtreep visible-only)
  "Export current buffer to a LaTeX+ buffer.

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

Export is done in a buffer named \"*Org LaTeX+ Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'latex+ "*Org LaTeX+ Export*"
    async subtreep visible-only nil nil (lambda () (latex-mode))))

;;;###autoload
(defun org-latex+-export-to-latex (&optional async subtreep visible-only)
  "Export current buffer to a LaTeX+ file.

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
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'latex+ outfile async
                        subtreep visible-only)))

;;;###autoload
(defun org-latex+-publish-to-latex (plist filename pub-dir)
  "Publish an org file to LaTeX+.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'latex+ filename ".tex" plist pub-dir))

(defun org-latex+-compile (texfile &optional snippet)
  "Compile a TeX file.

See the original  `org-latex-compile'

TEXFILE is the name of the file being compiled.  Processing is
done through the command specified in `org-latex-pdf-process',
which see.  Output is redirected to \"*Org PDF LaTeX Output*\"
buffer.

When optional argument SNIPPET is non-nil, TEXFILE is a temporary
file used to preview a LaTeX snippet.  In this case, do not
create a log buffer and do not remove log files.

Return PDF file name or raise an error if it couldn't be
produced."
  (unless snippet (message "Processing LaTeX file %s..." texfile))
  (let* ((compiler (or (with-temp-buffer
                         (save-excursion
                           (insert-file-contents texfile))
                         (and (search-forward-regexp (regexp-opt org-latex-compilers)
                                                     (line-end-position 2)
                                                     t)
                              (progn
                                (beginning-of-line)
                                (looking-at-p "%"))
                              (match-string 0)))
                       "pdflatex"))
         (process (if (functionp org-latex-pdf-process)
                      org-latex-pdf-process
                    ;; Replace "%latex" and "%bibtex" with,
		                ;; respectively, "%L" and "%B" so as to adhere to
		                ;; `format-spec' specifications.
                    (mapcar (lambda (command)
                              (replace-regexp-in-string "%\\(?:bib\\|la\\)tex\\>"
                                                        (lambda (m)
                                                          (upcase (substring m 0 2)))
                                                        command))
                            org-latex-pdf-process)))
         (spec `((?B . ,(shell-quote-argument org-latex-bib-compiler))
                 (?L . ,(shell-quote-argument compiler))))
         (log-buf-name "*Org PDF LaTeX Output*")
         (log-buf (and (not snippet)
                       (get-buffer-create log-buf-name)))
         (outfile (org-compile-file texfile
                                    process
                                    "pdf"
                                    (format "See %S for details" log-buf-name)
                                    log-buf
                                    spec)))
    (unless snippet
      (when org-latex-remove-logfiles
        (mapc #'delete-file
              (directory-files (file-name-directory outfile)
                               t
                               (concat (regexp-quote (file-name-base outfile))
                                       "\\(?:\\.[0-9]+\\)?\\."
                                       (regexp-opt org-latex-logfiles-extensions))
                               t)))
      (let ((warnings (org-latex--collect-warnings log-buf)))
        (message (concat "PDF file produced"
                         (cond
                          ((eq warnings 'error) " with errors.")
                          (warnings (concat " with warnings: " warnings))
                          (t "."))))))
    ;; Return output file name.

    outfile))

;;;###autoload
(defun org-latex+-export-to-pdf (&optional async subtreep visible-only body-only
                                           ext-plist)
  "Export current buffer to LaTeX then process through to PDF.

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

When optional argument BODY-ONLY is non-nil, only write code
between \"\\begin{document}\" and \"\\end{document}\".

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return PDF file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".tex" subtreep)))
    (org-export-to-file 'latex+
        outfile
      async
      subtreep
      visible-only
      body-only
      ext-plist
      (lambda (file)
        (org-latex+-compile file)))))

;;;###autoload
(defun org-latex+-publish-to-pdf (plist filename pub-dir)
  "Publish an Org file to PDF (via LaTeX+).

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  ;; Unlike to `org-latex-publish-to-latex', PDF file is generated
  ;; in working directory and then moved to publishing directory.
  (org-publish-attachment plist
                          ;; Default directory could be anywhere when this function is
                          ;; called.  We ensure it is set to source file directory during
                          ;; compilation so as to not break links to external documents.
                          (let ((default-directory (file-name-directory filename)))
                            (org-latex+-compile (org-publish-org-to 'latex+
                                                                   filename
                                                                   ".tex"
                                                                   plist
                                                                   (file-name-directory filename))))
                          pub-dir))

(provide 'ox-latex+)
;;; ox-latex+.el ends here
