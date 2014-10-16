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
(require 'helm-files)

(defun helm-editutil--open-dired (file)
  (dired (file-name-directory file)))

(defun helm-editutil--git-ls-files-source (pwd)
  (cl-loop for (description . options) in '(("Modified Files" . ("--modified"))
                                            ("Untracked Files" . ("--others" "--exclude-standard"))
                                            ("All Files" . ("--")))
           for is-first = t then nil
           for header = (if is-first
                            (format "%s (%s)" description pwd)
                          description)
           collect
           `((name . ,header)
             (init . (lambda ()
                       (with-current-buffer (helm-candidate-buffer 'global)
                         (process-file "git" nil t nil "ls-files" ,@options))))
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
      (error "Here is not Git Repository!!"))
    (let ((default-directory topdir)
          (sources (helm-editutil--git-ls-files-source topdir)))
      (helm :sources sources :buffer "*Helm Git Project*"
            :keymap helm-find-files-map))))

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

(defun helm-editutil--ghq-root ()
  (with-temp-buffer
    (unless (zerop (process-file "ghq" nil t nil "root"))
      (error "Failed: 'ghq root'"))
    (goto-char (point-min))
    (expand-file-name (helm-current-line-contents))))

(defun helm-editutil--ghq-list-candidates ()
  (with-temp-buffer
    (unless (zerop (process-file "ghq" nil t nil "list"))
      (error "Failed: 'ghq list'"))
    (let (paths)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((path (helm-current-line-contents)))
          (push path paths))
        (forward-line 1))
      (reverse paths))))

(defun helm-editutil--ghq-list-ls-files ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (zerop (process-file "git" nil t nil "ls-files"))
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

;;;###autoload
(defun helm-editutil-select-2nd-action ()
  (interactive)
  (helm-select-nth-action 1))

;;;###autoload
(defun helm-editutil-select-3rd-action ()
  (interactive)
  (helm-select-nth-action 2))

(defvar helm-editutil--grep-root nil)
(defvar helm-editutil--grep-history nil)

(defun helm-editutil--grep-init ()
  (let ((cmd (read-string "> "
                          (concat "git grep -n -- "
                                  (substring-no-properties
                                   (or (thing-at-point 'symbol)
                                       "")))
                          'helm-editutil--grep-history)))
    (helm-attrset 'recenter t)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let ((default-directory helm-editutil--grep-root))
        (unless (zerop (process-file-shell-command cmd nil t))
          (error "Failed: '%s'" cmd))
        (when (zerop (length (buffer-string)))
          (error "No output: '%s'" cmd))))))

(defun helm-editutil--project-top ()
  (with-temp-buffer
    (unless (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
      (error "Failed: 'git rev-parse --show-toplevel'"))
    (goto-char (point-min))
    (file-name-as-directory
     (buffer-substring-no-properties (point) (line-end-position)))))

(defvar helm-editutil--grep-source
  '((name . "Git Grep")
    (init . helm-editutil--grep-init)
    (candidates-in-buffer)
    (type . file-line)
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-editutil-grep ()
  (interactive)
  (setq helm-editutil--grep-root (helm-editutil--project-top))
  (let ((default-directory helm-editutil--grep-root))
    (helm :sources '(helm-editutil--grep-source) :buffer "*sgit-grep*")))

(defvar helm-editutil--hyperspec-source
  `((name . "Lookup Hyperspec")
    (candidates . ,(lambda ()
                     (hash-table-keys common-lisp-hyperspec--symbols)))
    (action . (("Show Hyperspec" . hyperspec-lookup)))))

;;;###autoload
(defun helm-editutil-hyperspec ()
  (interactive)
  (helm :sources '(helm-editutil--hyperspec-source)
        :default (thing-at-point 'symbol)
        :buffer "*Helm HyperSpec*"))

(defvar helm-editutil--ghc-mod-source
  '((name . "GHC Browse Documennt")
    (init . helm-editutil--ghc-mod-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-editutil--ghc-mod-action-display-document)))

(defun helm-editutil--ghc-mod-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (process-file "ghc-mod" nil t nil "list")
      (error "Failed 'ghc-mod list'"))))

(defun helm-editutil--ghc-mod-action-display-document (candidate)
  (let ((pkg (ghc-resolve-package-name candidate)))
    (if (and pkg candidate)
        (ghc-display-document pkg candidate nil)
      (error (format "Not found %s(Package %s)" candidate pkg)))))

;;;###autoload
(defun helm-editutil-ghc-browse-document ()
  (interactive)
  (helm :sources '(helm-editutil--ghc-mod-source) :buffer "*helm-ghc-document*"))

(provide 'helm-editutil)

;;; helm-editutil.el ends here
