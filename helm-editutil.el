;;; helm-editutil.el --- My own editing utilities with helm -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Version: 0.01
;; Package-Requires: ((helm "1.0") (cl-lib "0.5"))

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

(require 'cl-lib)
(require 'helm)
(require 'helm-tags)

(defgroup helm-editutil nil
  "My own editing utilities with helm"
  :group 'editutil)

(defun helm-editutil--open-dired (file)
  (dired (file-name-directory file)))

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
                        ("Open Directory" . helm-editutil--open-dired)
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

;;;###autoload
(defun helm-editutil-yas-prompt (prompt choices &optional display-fn)
  (let* ((names (cl-loop for choice in choices
                         collect (or (and display-fn (funcall display-fn choice))
                                     choice)))
         (selected (helm-other-buffer
                    `(((name . ,(format "%s" prompt))
                       (candidates . names)
                       (action . (("Insert snippet" . (lambda (arg) arg))))))
                    "*helm yas/prompt*")))
    (if selected
        (nth (cl-position selected names :test 'equal) choices)
      (signal 'quit "user quit!"))))

(defmacro helm-editutil--line-string ()
  `(buffer-substring-no-properties
    (line-beginning-position) (line-end-position)))

(defun helm-editutil--ghq-root ()
  (with-temp-buffer
    (unless (zerop (call-process "git" nil t nil "config" "ghq.root"))
      (error "Failed: Can't find ghq.root"))
    (goto-char (point-min))
    (expand-file-name (helm-editutil--line-string))))

(defun helm-editutil--ghq-list-candidates ()
  (with-temp-buffer
    (unless (zerop (call-process "ghq" nil t nil "list" "--full-path"))
      (error "Failed: ghq list --full-path'"))
    (let ((ghq-root (helm-editutil--ghq-root))
          paths)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((path (helm-editutil--line-string)))
          (push (cons (file-relative-name path ghq-root) path) paths))
        (forward-line 1))
      (reverse paths))))

(defun helm-editutil--ghq-list-ls-files ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (zerop (call-process "git" nil t nil "ls-files"))
      (error "Failed: 'git ls-files'"))))

(defun helm-editutil--ghq-source (repo)
  (let ((name (file-name-nondirectory (directory-file-name repo))))
    `((name . ,name)
      (init . helm-editutil--ghq-list-ls-files)
      (candidates-in-buffer)
      (action . (("Open File" . find-file)
                 ("Open File other window" . find-file-other-window)
                 ("Open Directory" . helm-editutil--open-dired))))))

;;;###autoload
(defun helm-editutil-ghq-list ()
  (interactive)
  (let ((repo (helm-comp-read "ghq-list: "
                              (helm-editutil--ghq-list-candidates)
                              :name "ghq list"
                              :must-match t)))
    (let ((default-directory (file-name-as-directory repo)))
      (helm :sources (helm-editutil--ghq-source default-directory)
            :buffer "*helm-ghq-list*"))))

(provide 'helm-editutil)

;;; helm-editutil.el ends here
