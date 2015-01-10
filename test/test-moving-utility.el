;;; test-moving-utility.el --- test for moving utilities

;; Copyright (C) 2015 by Syohei YOSHIDA

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

(ert-deftest move-beginning-of-line ()
  "editutil-move-beginning-of-line"
  (with-editutil-temp-buffer 'fundamental-mode
    "  foo bar baz"
    (forward-cursor-on "baz")
    (call-interactively 'editutil-move-beginning-of-line)
    (should (= (current-column) 0))
    (call-interactively 'editutil-move-beginning-of-line)
    (should (= (current-column) (current-indentation)))
    (call-interactively 'editutil-move-beginning-of-line)
    (should (= (current-column) 0))))

;;; test-moving-utility.el ends here
