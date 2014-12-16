;;; editutil.el --- My own Edit Utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; Version: 0.01

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

(eval-when-compile
  (defvar my/ctrl-q-map))

(require 'cl-lib)
(require 'thingatpt)
(require 'which-func)
(require 'vc-git)

(declare-function copy-sexp "thingopt")
(declare-function smartrep-define-key "smartrep")
(declare-function subword-forward "subword")
(declare-function subword-backward "subword")
(declare-function elscreen-editutil-current-directory "elscreen-editutil")
(declare-function ace-jump-word-mode "ace-jump-mode")
(declare-function show-all "outline")

(defgroup editutil nil
  "My own editing utilities"
  :group 'editing)

(defun editutil-forward-symbol-at-point ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
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
          (goto-char (line-beginning-position))
        (forward-line -1)
        (end-of-line))
      (newline-and-indent))))

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

(defun editutil-zap-to-char (arg char)
  (interactive "p\ncZap to char: ")
  (with-no-warnings
    (when (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char))))
  (delete-region (point)
                 (let ((case-fold-search nil))
                   (when (>= arg 0)
                     (forward-char 1))
                   (search-forward (char-to-string char) nil nil arg)
                   (if (>= arg 0)
                       (backward-char 1)
                     (forward-char 1))
                   (point))))

(defun editutil-zap-to-char-backward (arg)
  (interactive "p")
  (let ((input (char-to-string (read-char "Zap to char: " t)))
        (curpoint (point))
        (case-fold-search nil))
    (save-excursion
      (when (search-backward input nil t arg)
        (delete-region (1+ (point)) curpoint)))))

(defun editutil-next-symbol (arg)
  (interactive "p")
  (let ((symbol (thing-at-point 'symbol)))
    (unless symbol
      (error "No symbol at cursor!!"))
    (message "%s" (substring-no-properties symbol))
    (let ((bound (bounds-of-thing-at-point 'symbol)))
      (if (>= arg 0)
          (goto-char (cdr bound))
        (goto-char (car bound))))
    (let ((case-fold-search nil))
      (let ((regexp (concat "\\_<" (regexp-quote symbol) "\\_>"))
            finish)
        (while (not finish)
          (if (re-search-forward regexp nil t arg)
              (progn
                (setq finish t)
                (goto-char (match-beginning 0)))
            (message "Overwrapping")
            (if (>= arg 0)
                (goto-char (point-min))
              (goto-char (point-max)))))))))

(defun editutil-previous-symbol (arg)
  (interactive "p")
  (editutil-next-symbol (- arg)))

(defvar editutil--last-search-char nil)

(defsubst editutil--last-command-move-char-p ()
  (memq last-command '(editutil-forward-char editutil-backward-char)))

(defsubst editutil--use-last-key-p (char key)
  (if window-system
      (= char (aref (kbd key) 0))
    (when (and (= char 27) ;; ESC/Meta
               (string-match "\\`M-\\([a-zA-Z]\\)" key))
      (let ((meta-prefixed (string-to-char (match-string-no-properties 1 key)))
            (second-char (read-event)))
        (= second-char meta-prefixed)))))

(defun editutil-forward-char (arg &optional char)
  (interactive "p\n")
  (unless char
    (if (editutil--last-command-move-char-p)
        (setq char editutil--last-search-char)
      (setq char (read-event))
      (when (editutil--use-last-key-p char "M-e")
        (setq char editutil--last-search-char))))
  (unless (char-or-string-p char)
    (error "Error: Input Invalid Char %d" char))
  (setq editutil--last-search-char char)
  (when (>= arg 0)
    (forward-char 1))
  (let ((case-fold-search nil))
    (search-forward (char-to-string char) nil t arg))
  (when (>= arg 0)
    (backward-char 1)))

(defun editutil-backward-char (arg &optional char)
  (interactive "p\n")
  (unless char
    (if (editutil--last-command-move-char-p)
        (setq char editutil--last-search-char)
      (setq char (read-event))
      (when (editutil--use-last-key-p char "M-a")
        (setq char editutil--last-search-char))))
  (editutil-forward-char (- arg) char))

(defun editutil-kill-common (arg char way copy)
  (unless char
    (setq char (read-event)))
  (save-excursion
    (let ((start (point))
          (func (if copy 'kill-ring-save 'kill-region)))
      (if (eq way 'forward)
          (editutil-forward-char arg char)
        (editutil-backward-char arg char)
        (forward-char +1))
      (let ((end (point)))
        (if (<= start end)
            (funcall func start end)
          (funcall func end start))))))

(defun editutil-forward-kill (arg &optional char)
  (interactive "p\n")
  (editutil-kill-common arg char 'forward nil))

(defun editutil-backward-kill (arg &optional char)
  (interactive "p\n")
  (editutil-kill-common arg char 'backward nil))

(defun editutil-forward-copy (arg &optional char)
  (interactive "p\n")
  (editutil-kill-common arg char 'forward t))

(defun editutil-backward-copy (arg &optional char)
  (interactive "p\n")
  (editutil-kill-common arg char 'backward t))

(defun editutil-move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun editutil-move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(defun editutil-delete-following-spaces (arg)
  (interactive "p")
  (let ((orig-point (point)))
    (save-excursion
      (if (<= arg 0)
          (skip-chars-backward " \t")
        (skip-chars-forward " \t"))
      (delete-region orig-point (point)))))

(defun editutil-yank (arg)
  (interactive "P")
  (setq yank-window-start (window-start))
  (setq this-command t)
  (push-mark (point))
  (let ((str (current-kill 0)))
    (dotimes (_ (or arg 1))
      (insert-for-yank str)))
  (when (eq this-command t)
    (setq this-command 'yank))
  nil)

(defun editutil-insert-newline-without-moving ()
  (interactive)
  (save-excursion
    (newline)))

(defsubst editutil--enable-subword-mode-p ()
  (and (boundp 'subword-mode) subword-mode))

(defun editutil--forward-next-space ()
  (save-excursion
    (skip-chars-forward " \t")
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
  (when (= (point) (line-beginning-position))
    (backward-char 1))
  (when (looking-back "\\s-+")
    (skip-chars-backward " \t"))
  (let ((start (save-excursion
                 (if (editutil--enable-subword-mode-p)
                     (subword-backward arg)
                   (forward-word (- arg)))
                 (point)))
        (non-space (save-excursion
                     (skip-chars-backward "^ \t")
                     (point))))
    (delete-region (max start non-space (line-beginning-position)) (point))))

(defun editutil-number-rectangle (start end format-string start-num)
  "Delete (don't save) text in the region-rectangle, then number it."
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Number rectangle: " (if (looking-back "^ *") "%d. " "%d"))
         (read-number "From: " 1)))
  (save-excursion
    (goto-char start)
    (setq start (point-marker))
    (goto-char end)
    (setq end (point-marker))
    (delete-rectangle start end)
    (goto-char start)
    (cl-loop with column = (current-column)
             while (and (<= (point) end) (not (eobp)))
             for i = start-num then (1+ i)
             do
             (move-to-column column t)
             (insert (format format-string i))
             (forward-line 1)))
  (goto-char start))

(defun editutil-copy-sexp ()
  (interactive)
  (copy-sexp)
  (message "%s" (substring-no-properties (thing-at-point 'sexp))))

(defun editutil-duplicate-thing (n)
  (interactive "p")
  (let ((orig-column (current-column)))
    (save-excursion
      (let ((orig-line (line-number-at-pos))
            (str (if mark-active
                     (buffer-substring (region-beginning) (region-end))
                   (buffer-substring (line-beginning-position)
                                     (line-end-position)))))
        (forward-line 1)
        ;; maybe last line
        (when (= orig-line (line-number-at-pos))
          (insert "\n"))
        (dotimes (_ (or n 1))
          (insert str "\n"))))
    (move-to-column orig-column)))

(defun editutil-view-word-end (arg)
  (interactive "p")
  (forward-char 1)
  (forward-word arg)
  (backward-char 1))

(defun editutil-view-insert ()
  (interactive)
  (read-only-mode -1))

(defun editutil-view-insert-at-next ()
  (interactive)
  (read-only-mode -1)
  (forward-char 1))

(defun editutil-view-insert-at-bol ()
  (interactive)
  (read-only-mode -1)
  (back-to-indentation))

(defun editutil-view-insert-at-eol ()
  (interactive)
  (when view-mode
    (view-mode -1))
  (goto-char (line-end-position)))

(defun editutil-goto-last-line ()
  (interactive)
  (goto-char (point-max))
  (goto-char (line-beginning-position)))

(defun editutil-backward-symbol ()
  (interactive)
  (forward-symbol -1))

(defun editutil-indent-same-as-previous-line ()
  (interactive)
  (let ((cur-indent (current-indentation))
        (prev-indent (save-excursion
                       (forward-line -1)
                       (back-to-indentation)
                       (current-indentation))))
    (if (< cur-indent prev-indent)
        (progn
          (back-to-indentation)
          (insert-char (string-to-char " ") (- prev-indent cur-indent)))
      (goto-char (line-beginning-position))
      (delete-horizontal-space)
      (insert-char (string-to-char " ") prev-indent))))

(defun editutil-copy-line (arg)
  (interactive "p")
  (let ((start (line-beginning-position)))
    (save-excursion
      (forward-line (1- arg))
      (kill-ring-save start (line-end-position)))))

(defun editutil-isearch-ace-jump ()
  (interactive)
  (let ((input (substring isearch-string 0 1)))
    (isearch-exit)
    (ace-jump-word-mode (string-to-char input))))

(defun editutil-isearch-yank-symbol ()
  (interactive)
  (isearch-yank-internal (lambda () (forward-symbol 1) (point))))

(defun editutil-isearch-match-begin ()
  (interactive)
  (isearch-exit)
  (when (and isearch-forward isearch-success)
    (backward-char (length isearch-string))))

(defun editutil-isearch-match-end ()
  (interactive)
  (isearch-exit)
  (when (and (not isearch-forward) isearch-success)
    (forward-char (1- (length isearch-string)))))

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
    (forward-char 2)))

(defun editutil-forward-list (arg)
  (interactive "p")
  (unless (ignore-errors
            (forward-list arg)
            t)
    (editutil-backward-up arg)
    (forward-sexp arg)))

(defun editutil-minibuffer-up-dir ()
  (interactive)
  (backward-char 1)
  (when (search-backward "/" nil t)
    (delete-region (1+ (point)) (line-end-position))
    (forward-char 1)))

(defun editutil-insert-parentheses (arg)
  (interactive "P")
  (insert-parentheses (or arg 1)))

(defun editutil-other-window ()
  (interactive)
  (when (one-window-p)
    (split-window-right))
  (unless current-prefix-arg
    (other-window 1)))

(defface editutils-highlight
  '((((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "yellow" :foreground "black"))
  "highlight symbol"
  :group 'editutil)

(defun editutil-highlight-clear-overlays ()
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'editutils-highlight)
      (delete-overlay ov))))

(defun editutil-highlight-symbol-in-defun ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol)))
    (unless symbol
      (error "Here is not on symbol!!"))
    (let ((bounds (bounds-of-thing-at-point 'defun)))
      (let ((begin (or (car bounds) (point-min)))
            (end (or (cdr bounds) (point-max))))
        (editutil-highlight-clear-overlays)
        (save-excursion
          (goto-char begin)
          (let ((regexp (concat "\\_<" symbol "\\_>")))
            (while (re-search-forward regexp end t)
              (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put ov 'face 'editutils-highlight)
                (overlay-put ov 'editutils-highlight t)))))))))

(defun editutil-show-here-function ()
  (interactive)
  (if (not which-func-mode)
      (message "`which-func-mode' is not enabled")
    (message "%s" (gethash (selected-window) which-func-table))))

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
        (indent-pp-sexp)))))

(defun editutil-toggle-defun ()
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (forward-char 1)
    (unless (looking-at "\\(defun\\|defsubst\\)\\>")
      (error "Here is not defun"))
    (let ((current (match-string-no-properties 1)))
      (delete-region (match-beginning 1) (match-end 1))
      (insert (if (string= current "defun") "defsubst" "defun")))))

(defun editutil-kill-line (arg)
  (interactive "P")
  (if (and arg (>= (prefix-numeric-value arg) 1))
      (kill-whole-line arg)
    (let ((current-prefix-arg nil))
      (call-interactively 'kill-line))))

(defun editutil-view-quit ()
  (interactive)
  (if (buffer-file-name)
      (call-interactively 'read-only-mode)
    (call-interactively 'View-quit)))

(defun editutil--add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\_<\\(FIXME\\|TODO\\|XXX\\|@@@\\)\\_>"
          1 '((:foreground "pink") (:weight bold)) t))))

;; for `cde' command
(defun editutil-current-buffer-directory ()
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
               return (file-name-directory it)))))

(defun editutil--kill-command-common (arg func)
  (if (not arg)
      (call-interactively func)
    (let ((prefix-arg (prefix-numeric-value arg)))
      (save-excursion
        (if (>= prefix-arg 0)
            (let ((start (line-beginning-position)))
              (forward-line (1- prefix-arg))
              (funcall func start (line-end-position)))
          (let ((end (line-end-position)))
            (forward-line (1+ arg))
            (funcall func (point) end)))))))

(defun editutil-kill-ring-save (arg)
  (interactive "P")
  (editutil--kill-command-common arg 'kill-ring-save))

(defun editutil-kill-region (arg)
  (interactive "P")
  (editutil--kill-command-common arg 'kill-region))

(defun editutil--git-github-url (remote branch)
  (with-temp-buffer
    (unless (zerop (process-file "git" nil t nil "remote" "-v"))
      (error "Failed: git remote -v"))
    (goto-char (point-min))
    (when (re-search-forward (concat "\\<" remote "\\>") nil t)
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (when (string-match "github.com[:/]?\\(\\S-+?\\)?\\(?:\\.git\\)" line)
          (let ((path (match-string-no-properties 1 line)))
            (if (string= branch "master")
                (concat "https://github.com/" path)
              (concat "https://github.com/" path "/tree/" branch))))))))

(defun editutil-git-browse (remote)
  (interactive
   (list
    (let ((remotes (process-lines "git" "remote")))
      (if (= (length remotes) 1)
          (car remotes)
        (completing-read "Remote(default: origin): " remotes nil t nil nil
                         "origin")))))
  (let ((current-branch (car (vc-git-branches))))
    (let ((url (editutil--git-github-url remote current-branch)))
      (unless url
        (error "Error: URL not found"))
      (browse-url url))))

(defun editutil-jump-to-vcs-top ()
  (interactive)
  (let ((root (ignore-errors (vc-root-dir))))
    (unless root
      (error "Here is not version controled"))
    (find-file root)))

(defvar editutil--toggle-cleanup-state "")

(defun editutil--setup-toggle-cleanup ()
  (setq-default mode-line-format
                (cons '(:eval editutil--toggle-cleanup-state)
                      mode-line-format)))

(defun editutil-toggle-cleanup-spaces ()
  (interactive)
  (cond ((memq 'delete-trailing-whitespace before-save-hook)
         (setq editutil--toggle-cleanup-state
               (propertize "[DT-]" 'face '((:foreground "turquoise1" :weight bold))))
         (remove-hook 'before-save-hook 'delete-trailing-whitespace))
        (t
         (setq editutil--toggle-cleanup-state "")
         (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  (force-mode-line-update))

(defvar editutil-mode-line-cleaner-alist
  '( ;; For minor-mode, first char is 'space'
    (yas-minor-mode . " Ys")
    (paredit-mode . " Pe")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (autopair-mode . " Ap")
    (undo-tree-mode . "")
    (elisp-slime-nav-mode . "")
    (helm-gtags-mode . " HG")
    (flymake-mode . " Fm")
    (git-gutter-mode . " GG")
    ;; Major modes
    (lisp-interaction-mode . "Li")
    (git-commit-mode . "Commit")
    (python-mode . "Py")
    (ruby-mode   . "Rb")
    (emacs-lisp-mode . "El")
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

;;;###autoload
(defun editutil-default-setup ()
  (interactive)

  (editutil--setup-toggle-cleanup)

  (global-set-key (kbd "C-M-s") 'editutil-forward-symbol-at-point)

  (global-set-key [(control shift up)] 'editutil-move-line-up)
  (global-set-key [(control shift down)] 'editutil-move-line-down)

  (global-set-key (kbd "C-w") 'editutil-kill-region)
  (global-set-key (kbd "M-w") 'editutil-kill-ring-save)

  (global-set-key (kbd "C-M-o") 'editutil-other-window)
  (global-set-key (kbd "C-M-u") 'editutil-backward-up)

  (global-set-key (kbd "C-k") 'editutil-kill-line)
  (global-set-key (kbd "C-M-n") 'editutil-forward-list)
  (global-set-key (kbd "C-M-d") 'editutil-down-list)
  (global-set-key (kbd "M-o") 'editutil-edit-next-line)
  (global-set-key (kbd "M-O") 'editutil-edit-previous-line)
  (global-set-key (kbd "M-s") 'editutil-unwrap-at-point)
  (global-set-key (kbd "M-r") 'editutil-replace-wrapped-string)
  (global-set-key (kbd "M-n") 'editutil-next-symbol)
  (global-set-key (kbd "M-p") 'editutil-previous-symbol)
  (global-set-key (kbd "M-k") 'editutil-delete-following-spaces)
  (global-set-key (kbd "C-y") 'editutil-yank)
  (global-set-key (kbd "M-d") 'editutil-delete-word)
  (global-set-key [remap backward-kill-word] 'editutil-backward-delete-word)
  (global-set-key (kbd "C-x r N") 'editutil-number-rectangle)
  (global-set-key (kbd "C-M-c") 'editutil-duplicate-thing)

  (global-set-key (kbd "M-I") 'editutil-indent-same-as-previous-line)
  (global-set-key (kbd "M-(") 'editutil-insert-parentheses)

  (global-set-key (kbd "C-x .") 'editutil-highlight-symbol-in-defun)
  (global-set-key (kbd "C-x ,") 'editutil-highlight-clear-overlays)

  (global-set-key (kbd "C-x a a") 'editutil-mark-around-paired)
  (global-set-key (kbd "C-x a i") 'editutil-mark-inside-paired)

  ;; C-q map
  (define-key my/ctrl-q-map (kbd "l") 'editutil-copy-line)

  (define-key my/ctrl-q-map (kbd "s") 'editutil-unwrap-at-point)
  (define-key my/ctrl-q-map (kbd "r") 'editutil-replace-wrapped-string)

  (define-key my/ctrl-q-map (kbd "?") 'editutil-show-here-function)

  (define-key my/ctrl-q-map (kbd "w") 'editutil-forward-kill)
  (define-key my/ctrl-q-map (kbd "W") 'editutil-backward-kill)
  (define-key my/ctrl-q-map (kbd "c") 'editutil-forward-copy)
  (define-key my/ctrl-q-map (kbd "C") 'editutil-backward-copy)

  (define-key my/ctrl-q-map (kbd "C-t") 'editutil-toggle-cleanup-spaces)

  (when window-system
    (global-set-key (kbd "C-M-SPC") 'editutil-copy-sexp))

  (define-key isearch-mode-map (kbd "C-j") 'editutil-isearch-ace-jump)
  (define-key isearch-mode-map (kbd "C-M-w") 'editutil-isearch-yank-symbol)
  (define-key isearch-mode-map [remap isearch-exit] 'editutil-isearch-match-begin)
  (define-key isearch-mode-map (kbd "C-M-a") 'editutil-isearch-match-begin)
  (define-key isearch-mode-map (kbd "C-M-e") 'editutil-isearch-match-end)

  (define-key minibuffer-local-map (kbd "C-M-u") 'editutil-minibuffer-up-dir)

  (define-key dired-mode-map (kbd "P") 'editutil-jump-to-vcs-top)

  (add-hook 'after-change-major-mode-hook 'editutil-clear-mode-line)

  (smartrep-define-key
      global-map "C-x" '(("j" . 'editutil-insert-newline-without-moving)))

  ;; helm-editutil
  (global-set-key (kbd "M-.") 'helm-editutil-etags-select)
  (global-set-key (kbd "C-x C-p") 'helm-editutil-git-ls-files)
  (global-set-key (kbd "C-x C-r") 'helm-editutil-recentf-and-bookmark)

  (dolist (hook '(prog-mode-hook org-mode-hook text-mode-hook markdown-mode-hook))
    (add-hook hook 'editutil--add-watchwords))
  t)

(provide 'editutil)

;;; editutil.el ends here
