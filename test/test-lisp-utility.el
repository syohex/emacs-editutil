;;; test-lisp-utility --- test for lisp utility

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

(require 'ert)
(require 'editutil)

(ert-deftest toggle-let ()
  "editutil-toggle-let"
  (with-editutil-temp-buffer 'emacs-lisp-mode
    "
    (let ((name 'orange))
      (do-something name))
"
    (save-excursion
      (forward-cursor-on "name")
      (editutil-toggle-let)
      (goto-char (point-min))
      (should (search-forward "let*"))
      (editutil-toggle-let)
      (goto-char (point-min))
      (should-not (search-forward "let*" nil t))
      (goto-char (point-min))
      (should (search-forward "let ")))

    (save-excursion
      (forward-cursor-on "do-something")
      (editutil-toggle-let)
      (goto-char (point-min))
      (should (search-forward "let*"))
      (editutil-toggle-let)
      (goto-char (point-min))
      (should-not (search-forward "let*" nil t))
      (goto-char (point-min))
      (should (search-forward "let ")))

    ;; toggle on let
    (forward-cursor-on "let")
    (editutil-toggle-let)
    (should (string= (thing-at-point 'symbol) "let*"))))

;;; test-lisp-utility ends here
