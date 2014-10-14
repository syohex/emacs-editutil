;;; test-kill-utility.el --- test for killing utility

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

(ert-deftest forward-kill ()
  "editutil-forward-kill"
  (with-editutil-temp-buffer
    "apple orange melon"
    (forward-cursor-on "apple")
    (editutil-forward-kill 1 ?l)
    (should (string= (thing-at-point 'word) "le"))
    (should (string= (car kill-ring) "app"))))

(ert-deftest backward-kill ()
  "editutil-backward-kill"
  (with-editutil-temp-buffer
    "apple orange melon"
    (forward-cursor-on "melon")
    (editutil-backward-kill 2 (string-to-char " "))
    (should (string= (car kill-ring) "orange "))))

(ert-deftest forward-copy ()
  "editutil-forward-copy"
  (with-editutil-temp-buffer
    "apple orange melon"
    (forward-cursor-on "apple")
    (editutil-forward-copy 1 ?m)
    (should (string= (car kill-ring) "apple orange "))))

(ert-deftest backward-copy ()
  "editutil-backward-copy"
  (with-editutil-temp-buffer
    "apple orange melon"
    (forward-cursor-on "orange")
    (editutil-backward-kill 1 ?l)
    (should (string= (car kill-ring) "e "))))

;;; test-kill-utility.el ends here
