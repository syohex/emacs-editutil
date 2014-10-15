;;; test-helper.el --- helper

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

(defmacro with-editutil-temp-buffer (mode text &rest body)
  "Insert `text' in temporary buffer"
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     (funcall ,mode)
     ,@body))

(defun forward-cursor-on (pattern &optional count)
  (let ((case-fold-search nil))
    (re-search-forward pattern nil nil (or count 1)))
  (goto-char (match-beginning 0)))

(defun backward-cursor-on (pattern &optional count)
  (let ((case-fold-search nil))
    (re-search-backward pattern nil nil (or count 1)))
  (goto-char (match-beginning 0)))

(defun face-at-cursor-p (face)
  (eq (face-at-point) face))

;;; test-helper.el ends here
