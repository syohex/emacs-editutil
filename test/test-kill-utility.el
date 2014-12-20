;;; test-kill-utility.el --- test kill utility commands

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

(ert-deftest kill-thing-word ()
  "kill-thing for word"
  (with-editutil-temp-buffer 'text-mode
    "foo bar baz"
    (forward-cursor-on "foo")
    (editutil-kill-thing ?w)
    (should (string= (buffer-string) " bar baz"))))

(ert-deftest kill-thing-symbol ()
  "kill-thing for line"
  (with-editutil-temp-buffer 'emacs-lisp-mode
    "foo-bar-baz"
    (forward-cursor-on "foo")
    (editutil-kill-thing ?W)
    (should (string-empty-p (buffer-string)))))

(ert-deftest kill-thing-line ()
  "kill-thing for line"
  (with-editutil-temp-buffer 'emacs-lisp-mode
    "apple
orange
melon"
    (forward-cursor-on "apple")
    (editutil-kill-thing ?l)
    (should (string= (buffer-string) "orange\nmelon"))))

;;; test-kill-utility.el ends here
