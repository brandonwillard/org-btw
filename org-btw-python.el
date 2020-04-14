;;; org-btw-python.el --- Customizations and extensions to python-related org/babel functions
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
(require 'org-btw-utils)

(defun org-btw//org-babel-load-session:python (session body params)
  "Load BODY into SESSION using python-shell-send-string-echo."
  (declare-function python-shell-send-string "python.el")
  (save-window-excursion
    (let ((buffer (org-babel-prep-session:python session params))
          (python-shell-send (if (and ob-python-execute-echo
                                      (fboundp 'python-shell-send-string-echo))
                                 'python-shell-send-string-echo
                               'python-shell-send-string)))
      (with-current-buffer buffer
        (funcall python-shell-send
                 (org-babel-chomp body)
                 (get-buffer-process (current-buffer))))
      buffer)))
(defun org-btw//org-babel-python-session-buffer (orig-func session)
  "Make org-babel's default Python session buffer naming follow `python-mode' by
 forcing `org-babel-python-session-buffer' to return a name for non-initialized
 sessions."
  (let ((py-buffer-name (cdr (assoc session org-babel-python-buffers))))
    (or py-buffer-name (format "*%s*" (python-shell-get-process-name nil)))))

(defun org-btw//ob-ipython--create-repl (name)
  "Use `python-shell-get-process-name' for buffer processes."
  (let ((python-shell-completion-native-enable nil)
        (cmd (s-join " "
                     (ob-ipython--kernel-repl-cmd name)))
        (process-name (if (string= "default" name)
                          (python-shell-get-process-name nil)
                        (format "%s:%s"
                                (python-shell-get-process-name nil)
                                name))))
    (get-buffer-process (python-shell-make-comint cmd process-name
                                                  nil))
    (format "*%s*" process-name)))
(defun org-btw//ob-ipython--process-response (ret file result-type)
  "Don't append 'Out[...]:\n' junk to value-type output!"
  (let ((result (cdr (assoc :result ret)))
        (output (cdr (assoc :output ret))))
    (if (eq result-type 'output)
        output
      (car (->> (-map (-partial 'ob-ipython--render file)
                      (list (cdr (assoc :value result))
                            (cdr (assoc :display result))))
                (cl-remove-if-not nil))))))
(defun org-btw//ob-jupyter-console-repl-refresh ()
  " Manually--and hackishly--'refresh' a Jupyter console session with a
      remote kernel (opening one if not present) and display results echoed from
      a remote kernel.

      XXX: Requires 'c.ZMQTerminalInteractiveShell.include_other_output = True' in
      the jupyter console config.  Could add this to `ob-ipython' console initiation
      just to be sure."
  (with-demoted-errors "Error: %S"
    ;; FIXME: Does not seem to find the correct/any session!
    (let ((session-buffer (org-babel-initiate-session)))
      ;; (let* ((info (or info (org-babel-get-src-block-info)))
      ;;        (lang (nth 0 info))
      ;;        (params (nth 2 info))
      ;;        (session (cdr (assq :session params))))
      (when session-buffer
        (save-mark-and-excursion (with-current-buffer session-buffer
                                   (python-shell-send-string "pass"
                                                             (python-shell-get-process))))))))
(defun org-btw//ob-ipython--render (file-or-nil values)
  "Display `value' output without prepended prompt."
  (let ((org (lambda (value)
               value))
        (png (lambda (value)
               (let ((file (or file-or-nil
                               (ob-ipython--generate-file-name ".png"))))
                 (ob-ipython--write-base64-string file value)
                 (format "[[file:%s]]" file))))
        (svg (lambda (value)
               (let ((file (or file-or-nil
                               (ob-ipython--generate-file-name ".svg"))))
                 (ob-ipython--write-string-to-file file value)
                 (format "[[file:%s]]" file))))
        (html (lambda (value)))
        (txt (lambda (value)
               (when (s-present? value)
                 (s-replace "'" "" value)))))
    (or (-when-let (val (cdr (assoc 'text/org values)))
          (funcall org val))
        (-when-let (val (cdr (assoc 'image/png values)))
          (funcall png val))
        (-when-let (val (cdr (assoc 'image/svg+xml values)))
          (funcall svg val))
        (-when-let (val (cdr (assoc 'text/plain values)))
          (funcall txt val)))))
(defun org-btw//ob-ipython--dump-error (err-msg)
  "No-op used to get rid of the separate trace buffer"
  ;; Drop into console instead?
  ;; (with-demoted-errors "Error: %S" (org-btw//ob-jupyter-console-repl-refresh))
  (error "There was a fatal error trying to process the request."))

;;;###autoload
(defun org-btw//ob-python-generate-plots (orig-func body params)
  "Advice for `org-babel-execute:python' that handles :results graphics.

Example usage: \":results output graphics :file plot.png\"
"
  (let* ((graphics-file (and (member "graphics" (assq :result-params params))
			                       (org-babel-graphical-output-file params))))
    (when graphics-file
      (let* ((root-dir (or (ignore-errors (org-btw//org-publish-property :base-directory)) default-directory))
             (output-dir (or (ignore-errors (org-btw//org-publish-property :figure-dir)) default-directory))
             ;; TODO: Get a list of extensions
             ;; (file-ext (file-name-extension out-file))
             (new-body
              (format "
plt.ioff()

%s

import os

output_dir = '%s'
# fig_filenames = [os.path.join(output_dir, '...')
#                  + os.path.extsep + out_ext
#                  for out_ext in ['pdf', 'png']]
fig_filenames = [os.path.join(output_dir, '%s')]

# try:
#     plt.switch_backend('Agg')
# except ModuleNotFoundError:
#     pass

for fname in fig_filenames:
   plt.savefig(fname)

_ = os.path.relpath(fig_filenames[-1], '%s')

plt.ion()

" body output-dir graphics-file root-dir)))
        ;; TODO: Output a figure link?
        (setq body new-body)))
    (let ((res (funcall orig-func body params)))
      (if graphics-file nil res))))

;; Create a function that can wrap the process-name function and
;; prepend a session (if present).
(spacemacs//session-and-process-name org-babel-python-buffers nil)

;;;###autoload
(define-minor-mode org-btw-python-mode
  "Enables `org-btw' python extensions."
  :require 'org
  :init-value nil
  :global t
  (if org-btw-python-mode
      (progn

        ;; Enable projectile/layout-aware process and buffer naming
        (advice-add #'python-shell-get-process-name :around
                    #'org-btw//org-babel-python-buffers-process-name)

        (advice-add #'org-babel-python-session-buffer :around
                    #'org-btw//org-babel-python-session-buffer)

        (advice-add #'org-babel-load-session:python :override
                    #'org-btw//org-babel-load-session:python)

        ;; Enable graphics output
        (advice-add #'org-babel-execute:python :around
                    #'org-btw//ob-python-generate-plots)

        ;; projectile-aware org file compilation/exporting
        (when (configuration-layer/package-used-p 'projectile)
          (advice-add #'org-compile-file :override #'org-btw//org-compile-file)
          (advice-add #'org-export-output-file-name :around
                      #'org-btw//org-export-output-project-file-name))

        ;; Add our custom babel execution functions
        (spacemacs/set-leader-keys-for-major-mode 'org-mode
          "bh" #'org-btw/org-babel-execute-from-here)
        (spacemacs/set-leader-keys-for-major-mode 'org-mode
          "bD" #'org-babel-remove-result-one-or-many))
    (progn
      (advice-remove #'python-shell-get-process-name #'org-btw//org-babel-python-buffers-process-name)
      (advice-remove #'org-babel-python-session-buffer #'org-btw//org-babel-python-session-buffer)
      (advice-remove #'org-babel-load-session:python #'org-btw//org-babel-load-session:python)
      (advice-remove #'org-babel-execute:python #'org-btw//ob-python-generate-plots)

      (when (configuration-layer/package-used-p 'projectile)
        (advice-remove #'org-compile-file #'spacemacs//org-compile-file)
        (advice-remove #'org-export-output-file-name
                    #'org-btw//org-export-output-project-file-name)))))

(provide 'org-btw-python)
