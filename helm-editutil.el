;;; helm-editutil.el --- My own editing utilities with helm

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Version: 0.01

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

;;; Code:

(require 'helm)

(defgroup helm-editutil nil
  "My own editing utilities with helm"
  :group 'editutil)

(defun helm-editutil--git-ls-files-source (pwd)
  (cl-loop for (description . option) in
           '(("Modified Files" . "--modified")
             ("Untracked Files" . "--others --exclude-standard")
             ("All Files" . ""))
           for cmd = (concat "git ls-files " option)
           collect
           `((name . ,(format "%s (%s)" description pwd))
             (init . (lambda ()
                       (with-current-buffer (helm-candidate-buffer 'global)
                         (call-process-shell-command ,cmd nil t))))
             (candidates-in-buffer)
             (action . (("Open File" . find-file)
                        ("Open Directory" . (lambda (file)
                                              (dired (file-name-directory file))))
                        ("Open File other window" . find-file-other-window)
                        ("Insert buffer" . insert-file))))))

;;;###autoload
(defun helm-editutil-git-ls-files ()
  (interactive)
  (let ((topdir (locate-dominating-file default-directory ".git/")))
    (unless topdir
      (error "I'm not in Git Repository!!"))
    (let ((default-directory topdir)
          (sources (helm-editutil--git-ls-files-source topdir)))
      (helm-other-buffer sources "*Helm Git Project*"))))

;;;###autoload
(defun helm-editutil-etags-select (arg)
  (interactive "P")
  (let ((tag  (helm-etags-get-tag-file))
        (helm-execute-action-at-once-if-one t))
    (when (or (equal arg '(4))
              (and helm-etags-mtime-alist
                   (helm-etags-file-modified-p tag)))
      (remhash tag helm-etags-cache))
    (if (and tag (file-exists-p tag))
        (helm :sources 'helm-source-etags-select :keymap helm-etags-map
              :input (concat (thing-at-point 'symbol) " ")
              :buffer "*helm etags*"
              :default (concat "\\_<" (thing-at-point 'symbol) "\\_>"))
      (message "Error: No tag file found, please create one with etags shell command."))))

(provide 'helm-editutil)

;;; helm-editutil.el ends here
