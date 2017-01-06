;;; elscreen-editutil.el --- My own elscreen editing utilities

;; Copyright (C) 2017 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Package-Requires: ((emacs "25"))
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

(require 'elscreen)
(require 'cl-lib)

;;;###autoload
(defun elscreen-editutil-update-frame-title ()
  (interactive)
  (when (elscreen-screen-modified-p 'elscreen-frame-title-update)
    (cl-loop with sort-func = (lambda (a b) (< (car a) (car b)))
             with screen-list = (cl-copy-list (elscreen-get-screen-to-name-alist))
             for (index . name) in (sort screen-list sort-func)
             for status = (elscreen-status-label index)
             collect (format "%d%s %s" index status name) into screen-names
             finally
             (set-frame-name (mapconcat #'identity screen-names " ")))))

;;;###autoload
(defun elscreen-editutil-clone-only-this-window ()
  (interactive)
  (call-interactively 'elscreen-clone)
  (delete-other-windows))

;; `cde' command utilities

;;;###autoload
(defun elscreen-editutil-current-directory ()
  (let* ((current-screen (elscreen-get-current-screen))
         (property (elscreen-get-screen-property current-screen))
         (curbuf (marker-buffer (cadr (assoc-default 'window-configuration property))))
         (bufname (buffer-file-name curbuf))
         (curdir (if bufname
                     (file-name-directory bufname)
                   (with-current-buffer curbuf
                     default-directory))))
    (expand-file-name curdir)))

(provide 'elscreen-editutil)

;;; elscreen-editutil.el ends here
