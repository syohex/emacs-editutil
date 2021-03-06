;;; editutil.el --- My own Edit Utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2020 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-editutil
;; Version: 0.01
;; Package-Requires: ((emacs "27.1"))

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

;; My utility collections for using Emacs

;;; Code:

(eval-when-compile
  (defvar my/ctrl-q-map)
  (defvar paredit-mode-map)
  (defvar helm-map)
  (defvar ibuffer-mode-map))

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)
(require 'which-func)
(require 'dired)
(require 'vc-git)

(declare-function subword-forward "subword")
(declare-function subword-backward "subword")
(declare-function elscreen-editutil-current-directory "elscreen-editutil")
(declare-function recentf-save-list "recentf")
(declare-function ibuffer-mark-on-buffer "ibuffer")

(defgroup editutil nil
  "My own editing utilities"
  :group 'editing)

(defsubst editutil--current-line ()
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun editutil-forward-symbol-at-point ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
        (case-fold-search nil))
    (isearch-forward-symbol-at-point)
    (when symbol
      (setq regexp-search-ring
            (cons (substring-no-properties symbol) regexp-search-ring)))))

(defvar editutil--pair-characters
  '(("(" . ")") ("[" . "]") ("{" . "}") ("'" . "'") ("\"" . "\"")
    ("<" . ">") ("|" . "|") ("`" . "`")))

(defun editutil--unwrap-counterpart (sign)
  (let ((pair (assoc-default sign editutil--pair-characters)))
    (unless pair
      (error "Not found: pair string of '%s'" sign))
    pair))

(defun editutil--find-pair-start (arg matched limit)
  (let ((count 0)
        (pair (editutil--unwrap-counterpart matched)))
    (save-excursion
      (forward-char 1)
      (while (re-search-forward (regexp-quote pair) limit t)
        (cl-incf count)))
    (save-excursion
      (let ((search-count (+ arg count))
            (re (regexp-quote matched)))
        (when (> count 0)
          (backward-char 1)
          (unless (re-search-backward re (point-min) t (1- search-count))
            (error "This point is not wrapped")))
        (point)))))

(defun editutil-unwrap-at-point (arg &optional replaced)
  (interactive "p")
  (let ((paren-level (car (syntax-ppss))))
    (when (zerop paren-level)
      (error "Here is top level!!")))
  (save-excursion
    (let ((curpoint (point))
          (count 0)
          (replace-pair (and replaced (editutil--unwrap-counterpart replaced))))
      (when (re-search-backward "\\([(\[{'\"`|<]\\)" (point-min) t arg)
        (let* ((matched (match-string-no-properties 1))
               (pair (editutil--unwrap-counterpart matched))
               start)
          (setq start (editutil--find-pair-start arg matched curpoint))
          (goto-char start)
          (if (string-match-p "[(\[{]" matched)
              (forward-list 1)
            (save-excursion
              (forward-char 1)
              (while (re-search-forward (regexp-quote matched) curpoint t)
                (cl-incf count)))
            (goto-char start)
            (forward-char 1)
            (when (re-search-forward (regexp-quote pair) nil t (1+ count))
              (when (< (point) curpoint)
                (error "This point is not wrapped!!"))))
          (backward-char)
          (delete-char 1)
          (when replaced
            (insert replace-pair))
          (goto-char start)
          (delete-char 1)
          (when replaced
            (insert replaced)))))))

(defsubst editutil--convert-open-string (char)
  (cl-case char
    (?\) "(")
    (?\] "[")
    (?> "<")
    (?} "{")
    (otherwise (char-to-string char))))

(defsubst editutil--in-string-p ()
  (nth 3 (syntax-ppss)))

(defsubst editutil--goto-beginning-of-string-or-comment ()
  (goto-char (nth 8 (syntax-ppss))))

(defun editutil--mark-paired (char inner-p)
  (interactive)
  (let* ((current-prefix-arg nil)
         (open-str (editutil--convert-open-string char))
         (close-str (ignore-errors (editutil--unwrap-counterpart open-str))))
    (if (memq char '(?' ?\"))
        (while (editutil--in-string-p)
          (editutil--goto-beginning-of-string-or-comment))
      (if (= (char-syntax (string-to-char open-str)) ?\()
          (progn
            (when (editutil--in-string-p)
              (editutil--goto-beginning-of-string-or-comment))
            (backward-up-list 1))
        (unless (re-search-backward (regexp-quote open-str) nil t)
          (error "Can't find '%s'" open-str))))
    (if inner-p
        (set-mark (1+ (point)))
      (set-mark (point)))
    (if close-str
        (if (member (char-syntax (string-to-char close-str)) '(?\" ?\)))
            (forward-sexp 1)
          (re-search-forward (regexp-quote close-str) nil t))
      (forward-char 1)
      (re-search-forward (regexp-quote open-str) nil t))
    (and inner-p (backward-char 1))))

(defun editutil-mark-inside-paired (char)
  (interactive
   (list (read-char)))
  (editutil--mark-paired char t))

(defun editutil-mark-around-paired (char)
  (interactive
   (list (read-char)))
  (editutil--mark-paired char nil))

(defun editutil-replace-wrapped-string (arg)
  (interactive "p")
  (let ((replaced (char-to-string (read-char))))
    (editutil-unwrap-at-point arg replaced)))

(defun editutil-edit-previous-line (arg)
  (interactive "p")
  (if (< arg 0)
      (editutil-edit-next-line (- arg))
    (dotimes (_ arg)
      (if (= (line-number-at-pos) 1)
          (progn
            (goto-char (line-beginning-position))
            (open-line 1))
        (forward-line -1)
        (end-of-line)
        (newline-and-indent)))))

(defun editutil-mark-line (arg)
  (interactive "p")
  (set-mark (line-beginning-position))
  (goto-char (line-end-position))
  (when (> arg 1)
    (forward-line (1- arg))
    (goto-char (line-end-position))))

(defun editutil-edit-next-line (arg)
  (interactive "p")
  (if (>= arg 0)
      (dotimes (_ arg)
        (end-of-line)
        (newline-and-indent))
    (editutil-edit-previous-line (- arg))))

(defun editutil-edit-next-line-no-indent (arg)
  (interactive "p")
  (dotimes (_ arg)
    (end-of-line)
    (newline)))

(defun editutil-edit-next-line-same-column (arg)
  (interactive "p")
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (dotimes (_ arg)
      (end-of-line)
      (newline)
      (move-to-column col t))))

(defun editutil--zap-to-char-common (arg char more)
  (with-no-warnings
    (when (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char))))
  (delete-region (point)
                 (let ((case-fold-search nil))
                   (when (>= arg 0)
                     (forward-char (1+ more)))
                   (search-forward (char-to-string char) nil nil arg)
                   (if (>= arg 0)
                       (when (= more 0)
                         (backward-char 1))
                     (forward-char 1))
                   (point))))

(defun editutil-zap-to-char (arg char)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char nil t)))
  (editutil--zap-to-char-common arg char 0))

(defun editutil--char-to-thing (char)
  (cl-case char
    (?w 'word)
    (?q 'symbol)
    (?l 'line)
    (?s 'string)
    (otherwise (error "'%s' is not supported" char))))

(defun editutil--thing-bounds (char)
  (let ((thing (editutil--char-to-thing char)))
    (cl-case thing
      (string (save-excursion
                (unless (editutil--in-string-p)
                  (error "Here is not in `string'."))
                (editutil--goto-beginning-of-string-or-comment)
                (let ((start (point)))
                  (forward-sexp)
                  (cons start (point)))))
      (otherwise (bounds-of-thing-at-point thing)))))

(defun editutil--thing-common (char callback)
  (let ((bound (editutil--thing-bounds char)))
    (unless bound
      (error "Error: '%s' is not found" (editutil--char-to-thing char)))
    (funcall callback bound)))

(defun editutil-copy-thing (char)
  (interactive
   (list (read-char)))
  (editutil--thing-common
   char
   (lambda (bound)
     (kill-ring-save (car bound) (cdr bound)))))

(defun editutil-forward-char (arg char)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char)))
  (when (>= arg 0)
    (forward-char 1))
  (let ((case-fold-search nil))
    (search-forward (char-to-string char) nil t arg))
  (when (>= arg 0)
    (backward-char 1))
  (set-transient-map
   (let ((m (make-sparse-keymap)))
     (define-key m (kbd "M-e") (lambda () (interactive) (editutil-forward-char 1 char)))
     (define-key m (kbd "M-a") (lambda () (interactive) (editutil-forward-char -1 char)))
     m)))

(defun editutil-backward-char (arg char)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char)))
  (editutil-forward-char (- arg) char))

(defun editutil--repeat-move-line-command ()
  (message "'n': Move up, 'p': Move Down")
  (set-transient-map
   (let ((m (make-sparse-keymap)))
     (define-key m (kbd "n") 'editutil-move-line-down)
     (define-key m (kbd "p") 'editutil-move-line-up)
     m)))

(defun editutil-move-line-up ()
  (interactive)
  (let ((curindent (current-column)))
    (transpose-lines 1)
    (indent-according-to-mode)
    (forward-line -2)
    (move-to-column curindent)
    (editutil--repeat-move-line-command)))

(defun editutil-move-line-down ()
  (interactive)
  (let ((curindent (current-column)))
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)
    (indent-according-to-mode)
    (move-to-column curindent)
    (editutil--repeat-move-line-command)))

(defun editutil-delete-following-spaces (arg)
  (interactive "p")
  (let ((orig-point (point)))
    (save-excursion
      (if (<= arg 0)
          (forward-whitespace -1)
        (forward-whitespace +1))
      (delete-region orig-point (point)))))

(defun editutil-kill-whole-line (arg)
  (interactive "p")
  (let ((curcolumn (current-column)))
    (kill-whole-line arg)
    (move-to-column curcolumn)
    (set-transient-map
     (let ((m (make-sparse-keymap)))
       (define-key m (kbd "C-d") 'editutil-kill-whole-line)
       m))))

(defun editutil-yank (arg)
  (interactive "P")
  (setq yank-window-start (window-start))
  (setq this-command t)
  (push-mark (point))
  (let ((str (current-kill 0)))
    (dotimes (_ (or arg 1))
      (insert-for-yank str)))
  (when (eq this-command t)
    (setq this-command 'yank)))

(defun editutil-yank-pop-next (arg)
  (interactive "p")
  (yank-pop (- arg)))

(defsubst editutil--enable-subword-mode-p ()
  (and (boundp 'subword-mode) subword-mode))

(defun editutil--forward-next-space ()
  (save-excursion
    (forward-whitespace +1)
    (skip-chars-forward "^ \t")
    (point)))

(defun editutil-delete-word (arg)
  (interactive "p")
  (let ((next-not-space (editutil--forward-next-space)))
    (save-excursion
      (delete-region (point) (progn
                               (if (editutil--enable-subword-mode-p)
                                   (subword-forward arg)
                                 (forward-word arg))
                               (min next-not-space (point)))))))

(defun editutil-backward-delete-word (arg)
  (interactive "p")
  (let ((bol (line-beginning-position)))
    (when (= (point) bol)
      (backward-char 1))
    (when (looking-back "\\s-+" nil)
      (forward-whitespace -1))
    (let ((start (save-excursion
                   (if (editutil--enable-subword-mode-p)
                       (subword-backward arg)
                     (forward-word (- arg)))
                   (point)))
          (non-space (save-excursion
                       (skip-chars-backward "^ \t")
                       (point))))
      (delete-region (max start non-space (line-beginning-position)) (point)))))

(defun editutil--rectangle-format ()
  (let ((arg (prefix-numeric-value current-prefix-arg)))
    (if (< arg 0)
        (read-string "Number rectangle: " (if (looking-back "^ *" (line-beginning-position)) "%d. " "%d"))
      "%d")))

(defun editutil-number-rectangle (start end format-string start-num)
  "Delete (don't save) text in the region-rectangle, then number it."
  (interactive
   (list (region-beginning) (region-end)
         (editutil--rectangle-format)
         (read-number "From: " 1)))
  (save-excursion
    (goto-char start)
    (setq start (point-marker))
    (goto-char end)
    (setq end (point-marker))
    (delete-rectangle start end)
    (goto-char start)
    (cl-loop with arg = (abs (prefix-numeric-value current-prefix-arg))
             with count = arg
             with column = (current-column)
             while (and (<= (point) end) (not (eobp)))
             for i = start-num then (if (zerop count)
                                        (progn
                                          (setq count arg)
                                          (1+ i))
                                      i)
             do
             (move-to-column column t)
             (insert (format format-string i))
             (cl-decf count)
             (forward-line 1)))
  (goto-char start))

(defun editutil-mark-sexp ()
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (goto-char (car bounds))
      (set-mark (point))
      (goto-char (cdr bounds))
      (exchange-point-and-mark))))

(defun editutil-paredit-backward-delete ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'paredit-backward-delete)))

(defun editutil-duplicate-thing (n)
  (interactive "p")
  (let ((orig-column (current-column)))
    (save-excursion
      (let ((orig-line (line-number-at-pos))
            (str (if (use-region-p)
                     (buffer-substring (region-beginning) (region-end))
                   (buffer-substring (line-beginning-position)
                                     (line-end-position)))))
        (when (or (not (use-region-p)) (not (bolp)))
          (forward-line 1)
          ;; maybe lastline
          (when (= orig-line (line-number-at-pos))
            (insert "\n")))
        (dotimes (_ (or n 1))
          (insert str "\n"))))
    (move-to-column orig-column)))

(defun editutil-indent-same-as-previous-line ()
  (interactive)
  (let ((cur-indent (current-indentation))
        (prev-indent (save-excursion
                       (forward-line -1)
                       (let (finish column)
                         (while (not finish)
                           (when (bobp)
                             (setq finish t))
                           (let ((line (editutil--current-line)))
                             (unless (string-match-p "\\`\\s-*\\'" line)
                               (setq finish t column (current-indentation)))
                             (forward-line -1)))
                         column))))
    (if (< cur-indent prev-indent)
        (progn
          (back-to-indentation)
          (insert-char (string-to-char " ") (- prev-indent cur-indent)))
      (save-excursion
        (goto-char (line-beginning-position))
        (delete-horizontal-space)
        (insert-char (string-to-char " ") prev-indent)))))

(defun editutil-copy-line (arg)
  (interactive "p")
  (let ((start (line-beginning-position)))
    (save-excursion
      (forward-line (1- arg))
      (kill-ring-save start (line-end-position)))))

(defun editutil-backward-up (arg)
  (interactive "p")
  (if (editutil--in-string-p)
      (progn
        (skip-syntax-backward "^\"|")
        (backward-char 1))
    (unless (ignore-errors
              (backward-up-list arg)
              t)
      (skip-syntax-backward "^(")
      (backward-char 1))))

(defun editutil-down-list (arg)
  (interactive "p")
  (unless (ignore-errors
            (down-list arg)
            t)
    (skip-syntax-forward "^(")
    (forward-char 1)))

(defun editutil-forward-list (arg)
  (interactive "p")
  (unless (ignore-errors
            (forward-list arg)
            t)
    (editutil-backward-up arg)
    (forward-sexp arg)))

(defun editutil-insert-parentheses (arg)
  (interactive "P")
  (insert-parentheses (or arg 1)))

(defun editutil-other-window (arg)
  (interactive "p")
  (when (one-window-p)
    (if (> (window-width) 120)
        (split-window-right)
      (split-window-below)))
  (unless (>= (prefix-numeric-value current-prefix-arg) 16)
    (other-window arg)))

(defface editutils-highlight
  '((((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "yellow" :foreground "black"))
  "highlight symbol")

(defun editutil-toggle-let ()
  (interactive)
  (save-excursion
    (let ((limit (save-excursion (beginning-of-defun) (point)))
          (at-point (thing-at-point 'symbol)))
      (when (and at-point (string-match "\\`let\\*?" at-point))
        (forward-char (length (match-string-no-properties 0 at-point))))
      (when (re-search-backward "(\\(let\\)\\(\\*\\)?" limit t)
        (goto-char (match-end 1))
        (if (match-string 2)
            (delete-char 1)
          (insert "*"))
        (backward-up-list)
        (indent-pp-sexp))))
  (when (looking-at-p "^")
    (back-to-indentation)))

(defun editutil-newline-after-sexp (arg)
  (interactive "p")
  (when (< arg 0)
    (setq arg (- arg))
    (editutil-backward-up arg))
  (forward-sexp arg)
  (newline-and-indent))

(defun editutil-kill-line (arg)
  (interactive "P")
  (let ((num (prefix-numeric-value arg)))
    (if (and arg (>= num 1))
        (kill-whole-line num)
      (let ((current-prefix-arg nil))
        (call-interactively 'kill-line)))))

(defun editutil--add-watchwords ()
  (unless (memq major-mode '(org-mode))
    (font-lock-add-keywords
     nil '(("\\(?:^\\|\\s-\\)\\(FIXME\\|TODO\\|XXX\\|@@@\\)\\(?:\\s-\\|$\\)"
            1 '((:foreground "pink") (:weight bold)) t)))))

;; for `cde' command
(defun editutil-current-buffer-directory (&optional in-emacs)
  (if in-emacs
      (with-current-buffer editutil--previous-buffer
        default-directory)
    (if (featurep 'elscreen)
        (elscreen-editutil-current-directory)
      (let* ((bufsinfo (cadr (cadr (current-frame-configuration))))
             (bufname-list (assoc-default 'buffer-list bufsinfo)))
        (cl-loop for buf in bufname-list
                 for file = (or (buffer-file-name buf)
                                (with-current-buffer buf
                                  (when (eq major-mode 'dired-mode)
                                    dired-directory)))
                 when file
                 return (file-name-directory it))))))

(defun editutil--kill-command-common (arg func thing)
  (if (not arg)
      (if (use-region-p)
          (call-interactively func)
        (let* ((bound (bounds-of-thing-at-point thing))
               (kill-p (eq func 'kill-region))
               (del-func (if kill-p 'delete-region 'kill-ring-save)))
          (when (and (not bound) (bolp))
            (setq bound (cons (line-beginning-position) (line-end-position))))
          (when bound
            (funcall del-func (car bound) (cdr bound))
            (unless kill-p
              (message "%s" (buffer-substring-no-properties
                             (car bound) (cdr bound)))))))
    (let ((prefix-arg (prefix-numeric-value arg)))
      (save-excursion
        (if (>= prefix-arg 0)
            (let ((start (line-beginning-position)))
              (forward-line prefix-arg)
              (funcall func start (point)))
          (let ((end (line-end-position)))
            (forward-line (1+ arg))
            (funcall func (point) end)))))))

(defun editutil-kill-ring-save (arg)
  (interactive "P")
  (editutil--kill-command-common arg 'kill-ring-save 'sexp))

(defun editutil-kill-region (arg)
  (interactive "P")
  (editutil--kill-command-common arg 'kill-region 'symbol))

(defsubst editutil--github-url ()
  (car (process-lines "hub" "browse" "-u")))

(defun editutil-browse-github ()
  (interactive)
  (browse-url (editutil--github-url)))

(defun editutil-browse-github-file ()
  (interactive)
  (let ((url (editutil--github-url))
        (file (file-relative-name (buffer-file-name) (vc-root-dir)))
        (branch (editutil--vc-branch))
        (line (if current-prefix-arg
                  (format "#L%d" (line-number-at-pos))
                "")))
    (browse-url (concat url "/blob/" branch "/" file line))))

(defun editutil--latest-commit-id-of-current-line ()
  (let ((current-line (line-number-at-pos))
        (filename (buffer-file-name)))
    (with-temp-buffer
      (let ((status (process-file "git" nil t nil
                                  "blame" "-l" "-L"
                                  (format "%s,+1" current-line) filename)))
        (unless (zerop status)
          (error "Failed: git blame"))
        (goto-char (point-min))
        (looking-at "\\(\\S-+\\)")
        (let ((commit-id (match-string-no-properties 1)))
          (when (string-match-p "\\`0+\\'" commit-id)
            (error "This line is not committed yet"))
          commit-id)))))

(defun editutil-browse-github-commit ()
  (interactive)
  (let ((commit-id (editutil--latest-commit-id-of-current-line)))
    (process-file "hub" nil nil nil
                  "browse" "--" (concat "commit/" commit-id))))

(defun editutil-browse-weblio-sentence (sentence)
  (interactive
   (list (read-string "Sentence: ")))
  (let ((query (string-join (split-string sentence) "+")))
    (browse-url (format "http://ejje.weblio.jp/sentence/content/\"%s\"" query))))

(defun editutil-toggle-cleanup-spaces ()
  (interactive)
  (cond ((memq 'delete-trailing-whitespace before-save-hook)
         (remove-hook 'before-save-hook 'delete-trailing-whitespace))
        (t
         (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  (force-mode-line-update))

(defface editutils-clean-space
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Clean spaces statement in mode-line.")

(defvar editutil-cleanup-space-mode-line
  '(:eval (if (memq 'delete-trailing-whitespace before-save-hook)
              ""
            (propertize "[DT-]" 'face 'editutils-clean-space))))
(put 'editutil-cleanup-space-mode-line 'risky-local-variable t)

(defface editutils-vc-branch
  '((t (:inherit font-lock-constant-face :weight bold)))
  "Branch information in mode-line")

(defun editutil--vc-branch ()
  (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
    (substring-no-properties vc-mode (+ (length backend) 2))))

(defvar editutil-vc-mode-line
  '(:propertize
    (:eval (let ((branch (editutil--vc-branch))
                 (state (if (bound-and-true-p git-gutter2-mode)
                            (cl-case (vc-state (buffer-file-name))
                              (edited
                               (let ((hunks (git-gutter2-buffer-hunks)))
                                 (if (zerop hunks)
                                     ""
                                   (format ":%d" hunks))))
                              (otherwise ""))
                          "")))
             (concat "(" branch state ")")))
    face editutils-vc-branch)
  "Mode line format for VC Mode.")
(put 'editutil-vc-mode-line 'risky-local-variable t)

(defun editutil--init-mode-line ()
  (setq mode-line-misc-info (list (car mode-line-misc-info)))
  (setq-default mode-line-format
                `("%e"
                  editutil-cleanup-space-mode-line
                  ((global-mode-string ("" global-mode-string " ")))
                  mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification " " mode-line-position
                  (vc-mode editutil-vc-mode-line)
                  " "
                  mode-line-modes mode-line-misc-info mode-line-end-spaces)))

(defvar editutil-mode-line-cleaner-alist
  '(;; For minor-mode, first char is 'space'
    (yas-minor-mode . " Ys")
    (paredit-mode . " Pe")
    (company-mode . " Co")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (undo-tree-mode . "")
    (elisp-slime-nav-mode . "")
    (helm-gtags2-mode . " HG")
    (flymake-mode . " Fm")
    (git-gutter2-mode . " GG")
    (isearch-describe-mode . "")
    (flyspell-mode . " FS")
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (git-commit-mode . " Commit")
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (emacs-lisp-mode . "El")
    (js-mode . "JS")
    (markdown-mode . "Md")))

(defun editutil-clear-mode-line ()
  (interactive)
  (cl-loop for (mode . mode-str) in editutil-mode-line-cleaner-alist
           do
           (let ((old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
               (setcar old-mode-str mode-str))
             ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))

(defvar editutil--dictionary-history nil)

(defun editutil-dictionary-search (word)
  (interactive
   (list
    (let ((word (thing-at-point 'word)))
      (or (and word (string-match-p "^[a-zA-Z]" word) word)
          (read-string "Word: " nil 'editutil--dictionary-history)))))
  (setq word (downcase (substring-no-properties word)))
  (if (eq system-type 'darwin)
      (process-file "open" nil nil nil (concat "dict://" word))
    (with-current-buffer (get-buffer-create "*sdic*")
      (read-only-mode -1)
      (erase-buffer)
      (unless current-prefix-arg
        (unless (zerop (process-file "dict" nil t nil word))
          (error "Failed: 'dict %s'" word)))
      (when (or current-prefix-arg (string-empty-p (buffer-string)))
        (unless (zerop (process-file "dict" nil t nil "-s" word))
          (error "Failed: 'dict -s %s'" word))
        (when (string-empty-p (buffer-string))
          (error "Can't find any entries of '%s'" word)))
      (goto-char (point-min))
      (ansi-color-apply-on-region (point-min) (point-max))
      (view-mode +1)
      (read-only-mode +1)
      (pop-to-buffer (current-buffer)))))

(defvar editutil--compile-history nil)
(defvar editutil--last-command nil)

(defsubst editutil--compile-root-directory ()
  (cl-loop for (file . command) in '(("Makefile" . "make")
                                     ("Cargo.lock" . "cargo")
                                     ("yarn.lock" . "yarn")
                                     ("package-lock.json" "npm")
                                     (".git" . ""))
           when (locate-dominating-file default-directory file)
           return (list it command)
           finally return (list default-directory "")))

(defun editutil-compile (command dir)
  (interactive
   (let* ((dir-cmd (editutil--compile-root-directory))
          (dir (cl-first dir-cmd))
          (cmd (cl-second dir-cmd)))
     (list (read-string "Compile command: "
                        cmd 'editutil--compile-history)
           dir)))
  (setq editutil--last-command command)
  (let ((default-directory dir)
        (compilation-scroll-output t))
    (compile command)))

(defun editutil-auto-save-buffers ()
  (save-window-excursion
    (save-excursion
      (cl-loop for buf in (buffer-list)
               unless (string-match-p "\\`\\(?:\\s-+\\|[\\*#]\\)" (buffer-name buf))
               do
               (progn
                 (set-buffer buf)
                 (let ((buffile (buffer-file-name)))
                   (when (and buffile (buffer-modified-p) (not buffer-read-only)
                              (file-writable-p buffile))
                     (save-buffer))))))))

(defun editutil-join-line (arg)
  (interactive "p")
  (save-excursion
    (when (< arg 0)
      (setq arg (1- (abs arg)))
      (forward-line (- arg)))
    (dotimes (_i (abs arg))
      (delete-indentation -1))))

(defun editutil-hippie-expand ()
  (interactive)
  (let ((case-fold-search nil))
    (hippie-expand 1)))

(defun editutil-newline-common (newline-fn)
  (if (not electric-pair-mode)
      (funcall newline-fn)
    (if (and (looking-at-p "[])}]") (looking-back "[\[({]" (1- (point))))
        (progn
          (funcall #'newline-and-indent)
          (save-excursion
            (forward-line 1)
            (indent-for-tab-command)))
      (funcall newline-fn))))

(defun editutil-newline ()
  (interactive)
  (editutil-newline-common #'newline))

(defun editutil-newline-and-maybe-indent ()
  (interactive)
  (editutil-newline-common #'newline-and-indent))

(defun editutil-delete-line (arg)
  (interactive "p")
  (dotimes (_i arg)
    (delete-region (line-beginning-position)
                   (min (1+ (line-end-position)) (point-max)))))

;;;###autoload
(defun editutil-recentf-save-list ()
  (interactive)
  (recentf-save-list)
  (message nil))

(defun editutil-comment-line ()
  (interactive)
  (save-excursion
    (call-interactively #'comment-line)))

(cl-defun editutil-pop-to-mark-advice (orig-fun &rest args)
  (let ((orig (point)))
    (dotimes (_i 10)
      (apply orig-fun args)
      (unless (= orig (point))
        (cl-return-from editutil-pop-to-mark-advice)))))

(defun editutil-case-func-common (word-fn region-fn arg)
  (interactive)
  (if (use-region-p)
      (call-interactively region-fn)
    (funcall word-fn arg)))

(defun editutil-upcase (arg)
  (interactive "p")
  (editutil-case-func-common #'upcase-word #'upcase-region arg))

(defun editutil-downcase (arg)
  (interactive "p")
  (editutil-case-func-common #'downcase-word #'downcase-region arg))

(defun editutil-capitalize (arg)
  (interactive "p")
  (editutil-case-func-common #'capitalize-word #'capitalize-region arg))

(defun editutil-delete-horizontal-space ()
  (interactive)
  (let ((has-spaces
         (save-match-data
           (or (looking-back "\\s-+" nil) (looking-at-p "\\s-+")))))
    (if has-spaces
        (call-interactively #'delete-horizontal-space)
      (insert " "))))

(defun editutil-forward-word-end (arg)
  (interactive "p")
  (forward-char +1)
  (unless (looking-at-p "\\>")
    (backward-char +1))
  (forward-word arg)
  (backward-char +1))

(defun editutil-forward-WORD-end (arg)
  (interactive "p")
  (if (looking-at-p "[[:space:]\r\n]")
      (progn
        (skip-syntax-forward "-")
        (skip-syntax-forward "^-")
        (backward-char 1))
    (forward-char +1)
    (if (looking-at-p "[[:space:]\r\n]")
        (cl-incf arg)
      (backward-char +1))
    (editutil-forward-WORD arg)
    (let ((eob-p (eobp)))
      (backward-char +1)
      (unless eob-p
        (skip-chars-backward "\r\n\t ")
        (backward-char +1)))))

(defun editutil-forward-WORD (arg)
  (interactive "p")
  (dotimes (_i arg)
    (skip-syntax-forward "^-")
    (skip-syntax-forward "-")))

(defun editutil-backward-WORD (arg)
  (interactive "p")
  (let ((pos (point)))
    (skip-syntax-backward "^\\s-")
    (unless (= pos (point))
      (cl-decf arg))
    (dotimes (_i arg)
      (backward-char 1)
      (skip-syntax-backward "\\s-")
      (skip-syntax-backward "^\\s-"))))

(defun editutil-ibuffer-mark-delete-by-filename (regexp)
  "Mark delete all buffers whose filename matches REGEXP."
  (interactive
   (list (read-string "Mark by file name (regexp): ")))
  (ibuffer-mark-on-buffer
   (lambda (buf)
     (with-current-buffer buf
       (let ((filename (buffer-file-name buf))
             (case-fold-search nil))
         (string-match-p
          regexp
          (cond (filename filename)
                ((eq major-mode 'dired-mode) dired-directory)
                (t default-directory))))))
   ?D))

(defun editutil--cycle-buffer-common ()
  (set-transient-map
   (let ((m (make-sparse-keymap)))
     (define-key m (kbd "[") #'editutil-cycle-next-buffer)
     (define-key m (kbd "]") #'editutil-cycle-previous-buffer)
     m)))

(defun editutil-cycle-next-buffer ()
  (interactive)
  (bs-cycle-next)
  (editutil--cycle-buffer-common))

(defun editutil-cycle-previous-buffer ()
  (interactive)
  (bs-cycle-previous)
  (editutil--cycle-buffer-common))

(defun editutil-delete-indent ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (delete-region (line-beginning-position) (point))))

(defvar editutil--previous-buffer nil)

(defun editutil--save-current-windows ()
  (setq editutil--previous-buffer (current-buffer))
  (window-configuration-to-register :editutil-ansiterm))

(defun editutil--buffer-visible-p (bufname)
  (cl-loop for win in (window-list)
           for winbuf = (window-buffer win)
           thereis (string= bufname (buffer-name winbuf))))

(defun editutil-ansi-term ()
  (interactive)
  (if (editutil--buffer-visible-p "*ansi-term*")
      (other-window 1)
    (editutil--save-current-windows)
    (when (>= (length (window-list)) 3)
      (delete-other-windows))
    (when (one-window-p)
      (if (> (window-width) 120)
          (split-window-right)
        (split-window-below)))
    (other-window 1)
    (let ((shell-buf (get-buffer "*ansi-term*")))
      (if (buffer-live-p shell-buf)
          (progn
            (switch-to-buffer shell-buf)
            (goto-char (point-max)))
        (ansi-term shell-file-name)))))

(defun editutil-restore-ansi-term ()
  (interactive)
  (unless (string= (buffer-name) "*ansi-term*")
    (error "This buffer is not ansi-term buffer"))
  (jump-to-register :editutil-ansiterm))

(defvar editutil--last-killed-buffer nil)

(defun editutil-kill-this-buffer ()
  (interactive)
  (when-let (file (buffer-file-name))
    (setq editutil--last-killed-buffer file))
  (call-interactively #'kill-this-buffer))

(defun editutil-restore-last-killed-buffer ()
  (interactive)
  (unless editutil--last-killed-buffer
    (user-error "No killed buffer"))
  (find-file editutil--last-killed-buffer))

;; fixed line position after scrollup, scrolldown
(defun editutil-scroll-move-around (orig-fn &rest args)
  (let ((orig-line (count-lines (window-start) (point))))
    (apply orig-fn args)
    (move-to-window-line orig-line)))

(defun editutil-dired-find-file-other-window ()
  (interactive)
  (save-selected-window
    (call-interactively #'dired-find-file-other-window)))

(define-minor-mode editutil-global-minor-mode
  "Most superior minir mode"
  t
  ""
  `((,(kbd "C-M-j") . editutil-hippie-expand)
    (,(kbd "M-q") . editutil-zap-to-char)
    (,(kbd "C-M-o") . editutil-other-window)))

;;
;; Setup
;;

;;;###autoload
(defun editutil-default-setup ()
  (interactive)

  (editutil--init-mode-line)

  (global-unset-key (kbd "C-x c"))
  (global-unset-key (kbd "C-x z"))
  (global-unset-key (kbd "C-x t"))
  (global-unset-key (kbd "C-x w"))
  (global-unset-key (kbd "C-x s"))
  (global-unset-key (kbd "C-x a"))

  (global-set-key (kbd "RET") #'editutil-newline)
  (global-set-key (kbd "C-j") #'editutil-newline-and-maybe-indent)

  (global-set-key (kbd "C-M-s") #'editutil-forward-symbol-at-point)

  (global-set-key (kbd "C-w") #'editutil-kill-region)
  (global-set-key (kbd "M-w") #'editutil-kill-ring-save)

  (global-set-key (kbd "M-q") #'editutil-zap-to-char)

  (global-set-key (kbd "C-M-o") #'editutil-other-window)
  (global-set-key (kbd "C-M-u") #'editutil-backward-up)

  (global-set-key (kbd "C-x f") #'editutil-forward-char)
  (global-set-key (kbd "C-x a") #'editutil-backward-char)
  (global-set-key (kbd "C-M-)") #'editutil-forward-char)
  (global-set-key (kbd "C-M-(") #'editutil-backward-char)

  (global-set-key (kbd "C-k") #'editutil-kill-line)
  (global-set-key (kbd "C-M-n") #'editutil-forward-list)
  (global-set-key (kbd "C-M-d") #'editutil-down-list)
  (global-set-key (kbd "M-o") #'editutil-edit-next-line)
  (global-set-key (kbd "M-O") #'editutil-edit-previous-line)

  (global-set-key (kbd "C-x k") #'editutil-kill-this-buffer)
  (global-set-key (kbd "C-x K") #'editutil-restore-last-killed-buffer)

  (global-set-key (kbd "M-k") #'editutil-delete-following-spaces)

  (global-set-key (kbd "C-y") #'editutil-yank)
  (global-set-key (kbd "M-Y") #'editutil-yank-pop-next)

  (global-set-key (kbd "M-e") #'editutil-forward-word-end)
  (global-set-key (kbd "M-E") #'editutil-forward-WORD-end)

  (global-set-key (kbd "M-F") #'editutil-forward-WORD)
  (global-set-key (kbd "M-B") #'editutil-backward-WORD)

  (global-set-key (kbd "M-d") #'editutil-delete-word)
  (global-set-key (kbd "M-D") #'editutil-delete-line)

  (global-set-key (kbd "M-u") #'editutil-upcase)
  (global-set-key (kbd "M-l") #'editutil-downcase)
  (global-set-key (kbd "M-c") #'editutil-capitalize)

  (global-set-key (kbd "M-\\") #'editutil-delete-horizontal-space)

  (global-set-key [remap backward-kill-word] #'editutil-backward-delete-word)
  (global-set-key (kbd "C-M-c") #'editutil-duplicate-thing)

  (global-set-key (kbd "C-x C-d") #'editutil-kill-whole-line)

  (global-set-key (kbd "M-I") #'editutil-indent-same-as-previous-line)
  (global-set-key (kbd "M-(") #'editutil-insert-parentheses)

  (global-set-key (kbd "C-x l") #'editutil-mark-line)

  (global-set-key (kbd "C-x m") #'editutil-mark-inside-paired)
  (global-set-key (kbd "C-x M") #'editutil-mark-around-paired)
  (global-set-key (kbd "C-M-w") #'editutil-mark-sexp)

  (global-set-key (kbd "C-c w") #'editutil-dictionary-search)
  (global-set-key (kbd "C-c W") #'editutil-browse-weblio-sentence)

  (global-set-key (kbd "C-x y") #'editutil-copy-line)
  (global-set-key (kbd "C-x J") #'editutil-join-line)
  (global-set-key (kbd "C-x \\") #'editutil-ansi-term)

  (global-set-key (kbd "C-x ;") #'editutil-comment-line)

  (global-set-key (kbd "C-x <") #'editutil-delete-indent)

  ;; 'C-x r' prefix
  (global-set-key (kbd "C-x r N") #'editutil-number-rectangle)

  ;; 'C-x t' prefix
  (global-set-key (kbd "C-x t n") #'editutil-move-line-down)
  (global-set-key (kbd "C-x t p") #'editutil-move-line-up)

  ;; 'C-x s' prefix
  (global-set-key (kbd "C-x s s") #'editutil-unwrap-at-point)
  (global-set-key (kbd "C-x s r") #'editutil-replace-wrapped-string)
  (global-set-key (kbd "M-s s") #'editutil-unwrap-at-point)
  (global-set-key (kbd "M-s r") #'editutil-replace-wrapped-string)

  ;; 'C-x w' prefix
  (global-set-key (kbd "C-x w w") #'editutil-browse-github)
  (global-set-key (kbd "C-x w f") #'editutil-browse-github-file)
  (global-set-key (kbd "C-x w c") #'editutil-browse-github-commit)

  ;; 'M-g' prefix
  (global-set-key (kbd "M-g [") #'editutil-cycle-next-buffer)
  (global-set-key (kbd "M-g ]") #'editutil-cycle-previous-buffer)
  (global-set-key (kbd "M-g c") #'editutil-compile)

  (define-key my/ctrl-q-map (kbd "C-t") #'editutil-toggle-cleanup-spaces)

  (add-hook 'after-change-major-mode-hook #'editutil-clear-mode-line)

  ;; helm-editutil
  (global-set-key (kbd "C-x C-p") 'helm-editutil-git-ls-files)
  (global-set-key (kbd "C-x C-r") 'helm-editutil-recentf-and-bookmark)
  (global-set-key (kbd "C-x C-x") 'helm-editutil-find-files)
  (global-set-key (kbd "C-x b") 'helm-editutil-switch-buffer)
  (global-set-key (kbd "C-M-r") 'helm-editutil-search-buffer)

  (with-eval-after-load 'helm
    (define-key helm-map (kbd "C-e") 'helm-editutil-select-2nd-action)
    (define-key helm-map (kbd "C-j") 'helm-editutil-select-3rd-action))

  (dolist (hook '(prog-mode-hook text-mode-hook markdown-mode-hook))
    (add-hook hook #'editutil--add-watchwords))

  (run-with-idle-timer 10 t #'editutil-auto-save-buffers)

  (advice-add 'scroll-up :around 'editutil-scroll-move-around)
  (advice-add 'scroll-down :around 'editutil-scroll-move-around)

  (with-eval-after-load 'ibuffer
    (define-key ibuffer-mode-map (kbd "C-c C-d") #'editutil-ibuffer-mark-delete-by-filename))

  (with-eval-after-load 'paredit
    (define-key paredit-mode-map (kbd "C-c l") #'editutil-toggle-let)
    (define-key paredit-mode-map (kbd "C-c j") #'editutil-newline-after-sexp)
    (define-key paredit-mode-map (kbd "DEL") #'editutil-paredit-backward-delete))

  (with-eval-after-load 'term-mode
    (define-key term-mode-map (kbd "C-x \\") #'editutil-restore-ansi-term)
    (define-key term-raw-map (kbd "C-x \\") #'editutil-restore-ansi-term))

  (with-eval-after-load 'dired-mode
    (define-key dired-mode-map (kbd "o") #'editutil-dired-find-file-other-window))

  ;; yasnippet
  (custom-set-variables
   '(yas-prompt-functions '(helm-editutil-yas-prompt)))

  ;; pop-to-mark-command
  (advice-add 'pop-to-mark-command :around #'editutil-pop-to-mark-advice)
  (custom-set-variables
   '(set-mark-command-repeat-pop t))

  ;;(makunbound 'editutil-global-minor-mode-map)
  (editutil-global-minor-mode +1)

  t)

(provide 'editutil)

;;; editutil.el ends here
