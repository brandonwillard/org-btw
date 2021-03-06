;;; test-ox-pelican.el --- Tests for Pelican-friendly Markdown Back-End for Org Export Engine

;; Copyright (C) 2019 Brandon T. Willard

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

;;; Commentary

;;; Code:
(require 'ox-pelican)

(defun ox-pelican-test-transcode-body (str1 str2)
  (should (equal (org-test-with-temp-text str1
		                                      (org-export-as 'pelican nil nil t))
		             str2)))


(ert-deftest test-format-drawer ()
  (ox-pelican-test-transcode-body
    "
:EXAMPLE:
hi
:END:
"
    "<div class=\"example\" markdown=\"\">
hi

</div>"))

(ert-deftest test-bibliography ()
  (should (equal (org-test-with-temp-text
                  "See [[citep:WillardProgrammingIntelligentCity2018a]].

#+BIBLIOGRAPHYSTYLE: plainnat
#+BIBLIOGRAPHY: test/test.bib
"
                  (require 'org-ref+)
                  (org-ref+-mode +1)
		              (org-export-as 'pelican nil nil t))
                 "See <a id=\"c26e94c1b0b80ac3545371089d4f9936\"><a href=\"#WillardProgrammingIntelligentCity2018a\">(Willard &amp; Some 2018)</a></a>.

# Bibliography
<a id=\"WillardProgrammingIntelligentCity2018a\"></a> Willard & Some, Programming an Intelligent City: The Role of Data Science, <i>CityBase</i>, (2018). <a href=\"https://thecitybase.com/programming-an-intelligent-city-the-role-of-data-science/\">link</a>. [↩](#c26e94c1b0b80ac3545371089d4f9936)")))

(ert-deftest test-yaml-metadata-basic ()
  (should
   (equal
    (org-test-with-temp-text
     "#+TITLE: Testing
#+AUTHOR: Brandon T. Willard
#+DATE: 2020-03-18
#+EMAIL: some@email.com
#+FILETAGS: :draft:blah:
"
     (org-export-as 'pelican))

    (format "---
modified: '%s'
status: draft
tags: 'draft,blah'
title: Testing
date: '2020-03-18'
author: 'Brandon T. Willard'
figure_dir: '{attach}/articles/figures/'
figure_ext: png
---

" (format-time-string "%Y-%-m-%-d")))))

(ert-deftest test-yaml-metadata-bib-and-missing ()
  (let ((user-full-name "test-user"))
    (should
     (equal
      (org-test-with-temp-text
       "#+BIBLIOGRAPHYSTYLE: plainnat
#+BIBLIOGRAPHY: test/test.bib
"
       (require 'org-ref+)
       (org-ref+-mode +1)
       (org-export-as 'pelican))

      (format "---
bibliography:
- '%s'
modified: '%s'
author: 'test-user'
figure_dir: '{attach}/articles/figures/'
figure_ext: png
---

" (f-expand "test/test.bib") (format-time-string "%Y-%-m-%-d"))))))


(provide 'test-ox-pelican)
;;; test-ox-pelican.el end here
