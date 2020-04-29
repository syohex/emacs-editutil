;;; test-kill-utility.el --- test kill utility commands

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
(require 'subr-x)

(ert-deftest kill-region ()
  "My own kill-region."
  (with-editutil-temp-buffer 'text-mode
    "foo bar baz"
    (transient-mark-mode +1)
    (forward-cursor-on "bar")
    (backward-char 1)
    (set-mark (point))
    (goto-char (point-max))
    (editutil-kill-region nil)
    (should (string= (buffer-string) "foo"))))

(ert-deftest kill-region-multiple-lines ()
  "My own kill-region for multiple lines."
  (with-editutil-temp-buffer 'text-mode
    "foo
 bar
 baz"
    (transient-mark-mode +1)
    (forward-cursor-on "bar")
    (editutil-kill-region 2)
    (should (string= (buffer-string) "foo\n"))))

(ert-deftest kill-region-no-region ()
  "My own kill-region."
  (with-editutil-temp-buffer 'text-mode
    "foo bar"
    (editutil-kill-region nil)
    (should (string= (buffer-string) " bar"))))

(ert-deftest zap-to-char ()
  "My own zap-to-char."
  (with-editutil-temp-buffer 'text-mode
    "foo bar"
    (editutil-zap-to-char 1 ?b)
    (should (string= (buffer-string) "bar"))))

(ert-deftest delete-line ()
  "delete-current-line."
  (with-editutil-temp-buffer 'text-mode
    "foo
bar
baz"
    (editutil-delete-line 3)
    (should (string-empty-p (buffer-string)))))

(ert-deftest delete-horizontal-space ()
  "delete spaces around cursor."
  (with-editutil-temp-buffer 'text-mode
    "foo                              bar"
    (forward-cursor-on "bar")
    (let ((current-prefix-arg '(4)))
      (editutil-delete-horizontal-space))
    (should (string= (buffer-string) "foobar"))))

(ert-deftest delete-horizontal-space-inserting-a-space ()
  "Insert a space if there is no spaces around cursor."
  (with-editutil-temp-buffer 'text-mode
    "foobar"
    (forward-cursor-on "bar")
    (call-interactively #'editutil-delete-horizontal-space)
    (should (string= (buffer-string) "foo bar"))))

;;; test-kill-utility.el ends here
