;;; org-editutil.el --- editutil for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Version: 0.01
;; Package-Requires: ((emacs "25"))

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

;;; Code:

(require 'org)
(require 'editutil)
(require 'cl-lib)

(defgroup org-editutil nil
  "edit utilities for org-mode"
  :group 'editutil)

(defface org-editutil-check
  '((t (:foreground "green" :weight semi-bold)))
  "Check sign")

(defvar org-editutil--current-task nil)

(defun org-editutil--timer-start-hook ()
  (setq org-editutil--current-task (nth 4 (org-heading-components)))
  (cl-loop for buf in (buffer-list)
           unless (string-match-p "\\`[[:space:]*]" (buffer-name buf))
           do
           (with-current-buffer buf
             (setq-local header-line-format '((" " org-editutil--current-task " ")))))
  (setq-default header-line-format '((" " org-editutil--current-task " "))))

(defun org-editutil--timer-end-hook ()
  (cl-loop for buf in (buffer-list)
           do
           (with-current-buffer buf
             (setq header-line-format nil)))
  (setq-default header-line-format nil)
  (let ((find-fn (if (featurep 'elscreen)
                     'elscreen-find-file
                   'find-file)))
    (funcall find-fn (concat editutil-task-directory "pomodoro.org"))
    (outline-show-all)))

(defun org-editutil-insert-check (arg)
  (interactive "p")
  (dotimes (_ arg)
    (insert "✔")))

(defun org-editutil--add-keywords ()
  (font-lock-add-keywords
   nil '(("\\(✔\\)" 1 'org-editutil-check))))

;;;###autoload
(defun org-editutil-setup ()
  (interactive)

  (dolist (hook '(org-timer-set-hook))
    (add-hook hook #'org-editutil--timer-start-hook))

  (dolist (hook '(org-timer-done-hook org-timer-cancel-hook))
    (add-hook hook #'org-editutil--timer-end-hook))

  (define-key org-mode-map (kbd "C-c d") #'org-editutil-insert-check)

  (add-hook 'org-mode-hook #'org-editutil--add-keywords))

(provide 'org-editutil)

;;; org-editutil.el ends here
