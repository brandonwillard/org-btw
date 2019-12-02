;;; test-ox-sphinx.el --- Tests for Sphinx-friendly Markdown Back-End for Org Export Engine

;; Copyright (C) 2019 Brandon T. Willard

;; Author: Brandon T. Willard
;; Keywords: org, sphinx, markdown, github
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
(require 'ox-sphinx)

(defun ox-sphinx-test-transcode-body (str1 str2)
  (should (equal (org-test-with-temp-text str1
		                                      (org-export-as 'sphinx nil nil t))
		             str2)))


(ert-deftest test-src-block-caption ()
  (let ((org-rst-code-block 'code-block))
    (ox-sphinx-test-transcode-body
     "
#+caption: A caption
#+name: python-block
#+begin_src python
print('hi')
#+end_src
"
     ".. code-block:: python
    :caption: A caption
    :name: python-block

    print('hi')
")))

(ert-deftest test-src-block-link-no-caption ()
  (let ((org-rst-code-block 'code-block)
        (org-sphinx-src-block-always-caption nil))
    (ox-sphinx-test-transcode-body
     "[[python-block]]

#+name: python-block
#+begin_src python
print('hi')
#+end_src
"
     ":ref:`python-block`

.. code-block:: python
    :name: python-block

    print('hi')
")))

(ert-deftest test-src-block-link-default-caption ()
  (let ((org-rst-code-block 'code-block)
        (org-sphinx-src-block-always-caption t))
    (ox-sphinx-test-transcode-body
     "[[python-block]]

#+name: python-block
#+begin_src python
print('hi')
#+end_src
"
     ":ref:`python-block`

.. code-block:: python
    :caption: python-block
    :name: python-block

    print('hi')
")))


(provide 'test-ox-sphinx)
;;; test-ox-sphinx.el end here
