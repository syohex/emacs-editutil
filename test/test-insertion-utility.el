;;; test-insertion-utility.el --- test for insertion utilities

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

(ert-deftest number-rectangle ()
  "insert sequencial number by rectangle"
  (with-editutil-temp-buffer 'fundamental-mode
    "foo
bar
baz
"
    (let ((start (point)))
      (forward-cursor-on "baz")
      (editutil-number-rectangle start (point) "%d." 1)
      (should (string= (buffer-string) "1.foo\n2.bar\n3.baz\n")))))

(ert-deftest number-rectangle-with-number ()
  "insert sequencial number by rectangle with number"
  (with-editutil-temp-buffer 'fundamental-mode
    "foo
bar
baz
fom
"
    (let ((start (point)))
      (forward-cursor-on "fom")
      (let ((current-prefix-arg 2))
        (editutil-number-rectangle start (point) "%d." 1)
        (should (string= (buffer-string) "1.foo\n1.bar\n2.baz\n2.fom\n"))))))

;;; test-insertion-utility.el ends here
