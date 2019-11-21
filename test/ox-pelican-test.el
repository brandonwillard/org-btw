;;; test-ox-pelican.el --- Tests for Pelican-friendly Markdown Back-End for Org Export Engine

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


(provide 'test-ox-pelican)
;;; test-ox-pelican.el end here
