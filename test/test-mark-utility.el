;;; test-mark-utility.el --- test for mark utilities

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

(ert-deftest mark-inside-paired ()
  "editutil-mark-inside-paried"
  (dolist (input '("(apple)" "[apple]" "{apple}" "<apple>"))
    (with-editutil-temp-buffer 'fundamental-mode
      input
      (forward-cursor-on "apple")
      (let ((char (string-to-char (substring input 0))))
        (editutil-mark-inside-paired char)
        (let ((marked (buffer-substring-no-properties (region-beginning) (region-end))))
          (should (string= marked "apple")))))))

(ert-deftest mark-inside-string ()
  "editutil-mark-inside-paried for quoted"
  (with-editutil-temp-buffer 'fundamental-mode
    " \"orange\" "
    (forward-cursor-on "orange")
    (editutil-mark-inside-paired ?\" )
    (let ((marked (buffer-substring-no-properties (region-beginning) (region-end))))
      (should (string= marked "orange")))))

(ert-deftest mark-inside-paired-nested ()
  "editutil-mark-inside-paried"
  (with-editutil-temp-buffer 'fundamental-mode
    "(apple (orange))"
    (forward-cursor-on "apple")
    (editutil-mark-inside-paired ?\( )
    (let ((marked (buffer-substring-no-properties (region-beginning) (region-end))))
      (should (string= marked "apple (orange)")))))

;;; test-mark-utility.el ends here
