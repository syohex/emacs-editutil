;;; helm-editutil.el --- My own editing utilities with helm -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Version: 0.01
;; Package-Requires: ((helm "1.56") (cl-lib "0.5"))

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
(require 'helm-mode)
(require 'subr-x)

(eval-when-compile
  (defvar common-lisp-hyperspec--symbols))

(declare-function ghc-resolve-package-name "ghc")
(declare-function ghc-display-document "ghc")

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
(defun helm-editutil-etags-select (arg)
  (interactive "P")
  (let ((tag  (helm-etags-get-tag-file))
        (helm-execute-action-at-once-if-one t))
    (when (or (= (prefix-numeric-value arg) 4)
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
(defun helm-editutil-select-2nd-action ()
  (interactive)
  (helm-select-nth-action 1))

;;;###autoload
(defun helm-editutil-select-3rd-action ()
  (interactive)
  (helm-select-nth-action 2))

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

(defun helm-editutil--recentf-transform (candidates _source)
  (cl-loop for i in candidates
           if helm-ff-transformer-show-only-basename
           collect (cons (helm-basename i) i)
           else collect i))

(defun helm-editutil--file-in-dired (file)
  (dired (file-name-directory file))
  (dired-goto-file file))

(defvar helm-editutil-source-recentf
  `((name . "Recentf")
    (init . (lambda ()
              (require 'recentf)
              (recentf-mode 1)))
    (candidates . recentf-list)
    (filtered-candidate-transformer . helm-editutil--recentf-transform)
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (action . (("Find File" . find-file)
               ("Find Files in dired" . helm-editutil--file-in-dired)
               ("Find File other window" . find-file-other-window)
               ("Insert File" . insert-file)))))

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

(provide 'helm-editutil)

;;; helm-editutil.el ends here
