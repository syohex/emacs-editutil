;;; helm-editutil.el --- My own editing utilities with helm -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Version: 0.01
;; Package-Requires: ((helm "1.77") (cl-lib "0.5"))

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
(require 'helm-files)
(require 'helm-mode)
(require 'subr-x)
(require 'recentf)

(defun helm-editutil--open-dired (file)
  (dired (file-name-directory file)))

(defun helm-editutil--make-git-ls-function (options)
  (lambda ()
    (with-current-buffer (helm-candidate-buffer 'global)
      (apply 'process-file "git" nil t nil "ls-files" options))))

(defvar helm-editutil--git-ls-actions
  '(("Open File" . find-file)
    ("Open Directory" . helm-editutil--open-dired)
    ("Open File other window" . find-file-other-window)
    ("Insert buffer" . insert-file)))

(defun helm-editutil--git-ls-files-source (pwd)
  (cl-loop with is-first = t
           for (description . options) in '(("Modified Files" . ("--modified"))
                                            ("Untracked Files" . ("--others" "--exclude-standard"))
                                            ("All Files" . ("--")))
           for header = (if is-first
                            (progn
                              (setq is-first nil)
                              (format "%s (%s)" description pwd))
                          description)
           collect
           (helm-build-in-buffer-source header
             :init (helm-editutil--make-git-ls-function options)
             :action helm-editutil--git-ls-actions)))

;;;###autoload
(defun helm-editutil-git-ls-files ()
  (interactive)
  (let ((topdir (locate-dominating-file default-directory ".git/")))
    (unless topdir
      (error "Here is not Git Repository!!"))
    (let ((default-directory (if current-prefix-arg
                                 default-directory
                               topdir))
          (sources (helm-editutil--git-ls-files-source topdir)))
      (helm :sources sources :buffer "*Helm Git Project*"
            :keymap helm-find-files-map))))

;;;###autoload
(defun helm-editutil-select-2nd-action ()
  (interactive)
  (helm-select-nth-action 1))

;;;###autoload
(defun helm-editutil-select-3rd-action ()
  (interactive)
  (helm-select-nth-action 2))

(defun helm-editutil--recentf-transform (candidates _source)
  (cl-loop for i in candidates
           if helm-ff-transformer-show-only-basename
           collect (cons (helm-basename i) i)
           else collect i))

(defun helm-editutil--file-in-dired (file)
  (dired (file-name-directory file))
  (dired-goto-file file))

(defvar helm-editutil-source-recentf
  (helm-build-sync-source "Recently open files"
    :candidates recentf-list
    :filtered-candidate-transformer 'helm-editutil--recentf-transform
    :candidate-number-limit 9999
    :volatile t
    :action '(("Find File" . find-file)
              ("Find Files in dired" . helm-editutil--file-in-dired)
              ("Find File other window" . find-file-other-window)
              ("Insert File" . insert-file))))

;;;###autoload
(defun helm-editutil-recentf-and-bookmark ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources '(helm-editutil-source-recentf helm-source-bookmarks)
          :buffer "*helm recentf+bookmark*")))

;;;###autoload
(defun helm-editutil-yas-prompt (_prompt choices &optional display-fn)
  (let* ((names (cl-loop for choice in choices
                         collect (or (and display-fn (funcall display-fn choice))
                                     choice)))
         (selected (helm-other-buffer
                    `((name . "Choose a snippet")
                      (candidates . ,names)
                      (action . (("Insert snippet" . identity))))
                    "*helm yas-prompt*")))
    (if selected
        (nth (cl-position selected names :test 'equal) choices)
      (signal 'quit "user quit!"))))

(defun helm-editutil--find-files-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (zerop (process-file "perl" nil t nil "-wE" "say for grep {-T $_} glob('* .*')"))
      (error "Failed: collect files"))))

(defvar helm-editutil-source-find-files
  (helm-build-in-buffer-source "Find Files"
    :init 'helm-editutil--find-files-init
    :action '(("Find File" . find-file)
              ("Find File other window" . find-file-other-window)
              ("Insert File" . insert-file))))

;;;###autoload
(defun helm-editutil-find-files ()
  (interactive)
  (helm :sources '(helm-editutil-source-find-files) :buffer "*Helm Find Files*"))

;;;###autoload
(defun helm-editutil-search-buffer ()
  (interactive)
  (if (buffer-file-name)
      (call-interactively 'helm-do-ag-this-file)
    (call-interactively 'helm-occur)))

;;;###autoload
(defun helm-editutil-switch-buffer ()
  (interactive)
  (let ((bufs (cl-loop with normals = nil
                       with stars = nil

                       for buf in (buffer-list)
                       for name = (buffer-name buf)
                       do
                       (cond ((string-prefix-p "*" name)
                              (push name stars))
                             ((not (string-prefix-p " " name))
                              (push name normals)))
                       finally
                       return (cons (reverse normals) (reverse stars)))))
    (helm :sources (list
                    (helm-build-sync-source "Buffers"
                      :candidates (car bufs)
                      :action 'switch-to-buffer)
                    (helm-build-sync-source "Other Buffers"
                      :candidates (cdr bufs)
                      :action 'switch-to-buffer))
          :buffer "*Helm Switch Buffer*")))

(provide 'helm-editutil)

;;; helm-editutil.el ends here
