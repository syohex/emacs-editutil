;;; test-editing-utility.el --- editing utilities test

;; Copyright (C) 2017 by Syohei YOSHIDA

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

(ert-deftest edit-next-line ()
  "Edit next line like Vim's 'o'."
  (with-editutil-temp-buffer 'fundamental-mode
    "foo"
    (call-interactively 'editutil-edit-next-line)
    (should (string= (buffer-string) "foo\n"))))

(ert-deftest edit-previous-line ()
  "Edit previous line like Vim's 'O'."
  (with-editutil-temp-buffer 'fundamental-mode
    "foo\nbar"
    (forward-cursor-on "bar")
    (call-interactively 'editutil-edit-previous-line)
    (should (string= (buffer-string) "foo\n\nbar"))))

(ert-deftest edit-previous-line-first-line ()
  "Edit previous line at first line"
  (with-editutil-temp-buffer 'fundamental-mode
    "foo"
    (call-interactively 'editutil-edit-previous-line)
    (should (= (point) 1))
    (should (string= (buffer-string) "\nfoo"))))

(ert-deftest edit-upcase ()
  "Upcase both word and region"
  (with-editutil-temp-buffer 'fundamental-mode
    "foo bar"
    (call-interactively 'editutil-upcase)
    (should (string= (buffer-string) "FOO bar")))

  (with-editutil-temp-buffer 'fundamental-mode
    "foo bar"
    (transient-mark-mode +1)
    (set-mark (point))
    (goto-char (point-max))
    (call-interactively 'editutil-upcase)
    (should (string= (buffer-string) "FOO BAR"))))

(ert-deftest edit-downcase ()
  "Downcase both word and region"
  (with-editutil-temp-buffer 'fundamental-mode
    "FOO BAR"
    (goto-char (point-max))
    (editutil-downcase -1)
    (should (string= (buffer-string) "FOO bar")))

  (with-editutil-temp-buffer 'fundamental-mode
    "FOO BAR"
    (transient-mark-mode +1)
    (set-mark (point))
    (goto-char (point-max))
    (call-interactively 'editutil-downcase)
    (should (string= (buffer-string) "foo bar"))))

(ert-deftest edit-capitalize ()
  "Capitalize both word and region"
  (with-editutil-temp-buffer 'fundamental-mode
    "foo bar"
    (call-interactively 'capitalize-word)
    (should (string= (buffer-string) "Foo bar")))

  (with-editutil-temp-buffer 'fundamental-mode
    "foo bar"
    (transient-mark-mode +1)
    (set-mark (point))
    (goto-char (point-max))
    (call-interactively 'editutil-capitalize)
    (should (string= (buffer-string) "Foo Bar"))))

;;; test-editing-utility.el ends here
