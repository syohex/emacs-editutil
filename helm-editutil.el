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
    (unless (zerop (call-process "ghq" nil t nil "list"))
      (error "Failed: ghq list"))
    (let (paths)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((path (helm-editutil--line-string)))
          (push path paths))
        (forward-line 1))
      (reverse paths))))

(defun helm-editutil--ghq-list-ls-files ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (zerop (call-process "git" nil t nil "ls-files"))
      (error "Failed: 'git ls-files'"))))

(defun helm-editutil--ghq-format (repo)
  (cond ((string-match "\\`github.com/\\(.+\\)" repo)
         (match-string-no-properties 1 repo))
        ((string-match "\\`code.google.com/\\(.+\\)" repo)
         (match-string-no-properties 1 repo))))

(defun helm-editutil--ghq-update-repository (repo)
  (async-shell-command (concat "ghq get -u "
                               (helm-editutil--ghq-format repo))))

(defun helm-editutil--ghq-source-open (repo-path)
  (let ((name (file-name-nondirectory (directory-file-name repo-path))))
    `((name . ,name)
      (init . helm-editutil--ghq-list-ls-files)
      (candidates-in-buffer)
      (action . (("Open File" . find-file)
                 ("Open File other window" . find-file-other-window)
                 ("Open Directory" . helm-editutil--open-dired))))))

(defun helm-editutil--ghq-source-update (repo)
  `((name . "Update Repository")
    (candidates . (" ")) ;; dummy
    (action . (lambda (candidate)
                (helm-editutil--ghq-update-repository ,repo)))))

;;;###autoload
(defun helm-editutil-ghq-list ()
  (interactive)
  (let ((root (file-name-as-directory (helm-editutil--ghq-root)))
        (repo (helm-comp-read "ghq-list: "
                              (helm-editutil--ghq-list-candidates)
                              :name "ghq list"
                              :must-match t)))
    (let ((default-directory (file-name-as-directory (concat root repo))))
      (helm :sources (list (helm-editutil--ghq-source-open default-directory)
                           (helm-editutil--ghq-source-update repo))
            :buffer "*helm-ghq-list*"))))

(provide 'helm-editutil)

;;; helm-editutil.el ends here
