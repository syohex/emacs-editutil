;;; test-moving-utility.el --- test for moving utilities

;; Copyright (C) 2016 by Syohei YOSHIDA

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

(ert-deftest down-list ()
  "editutil-down-list"
  (with-editutil-temp-buffer 'fundamental-mode
    "  (foo)"
    (call-interactively 'editutil-down-list)
    (should (string= (thing-at-point 'word) "foo"))))

(ert-deftest down-list-fallback ()
  "editutil-down-list in fallback case"
  (with-editutil-temp-buffer 'fundamental-mode
    "  (foo) (bar)"
    (call-interactively 'editutil-down-list)
    (call-interactively 'editutil-down-list)
    (should (= (char-after) ?b))
    (should (string= (thing-at-point 'word) "bar"))))

(ert-deftest same-as-previous-line-indent ()
  "editutil-indent-same-as-previous-line"
  (with-editutil-temp-buffer 'fundamental-mode
    "    foo bar

"
    (let ((expected (current-indentation)))
      (goto-char (point-max))
      (call-interactively 'editutil-indent-same-as-previous-line)
      (should (= (current-indentation) expected))))

  (with-editutil-temp-buffer 'fundamental-mode
    "    foo bar

             baz bar"
    (let ((expected (current-indentation)))
      (goto-char (point-max))
      (call-interactively 'editutil-indent-same-as-previous-line)
      (should (= (current-indentation) expected)))))

;;; test-moving-utility.el ends here
