;;; editutil.el --- My own Edit Utilities

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
  (defvar my/ctrl-q-map)
  (defvar git-gutter-mode))

(require 'cl-lib)
(require 'thingatpt)
(require 'which-func)

(declare-function copy-sexp "thingopt")
(declare-function smartrep-define-key "smartrep")
(declare-function subword-forward "subword")
(declare-function subword-backward "subword")
(declare-function git-gutter "git-gutter")

(defgroup editutil nil
  "My own editing utilities"
  :group 'editing)

(defvar editutil--unwrap-pair
  '(("(" . ")") ("[" . "]") ("{" . "}") ("'" . "'") ("\"" . "\"")
    ("<" . ">") ("|" . "|") ("`" . "`")))

(defun editutil--unwrap-counterpart (sign)
  (let ((pair (assoc-default sign editutil--unwrap-pair)))
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

;;;###autoload
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

(defun editutil--mark-paired (char inner-p)
  (interactive)
  (let* ((current-prefix-arg nil)
         (curpoint (point))
         (open-str (char-to-string char))
         (close-str (ignore-errors (editutil--unwrap-counterpart open-str))))
    (if (memq char '(?' ?\"))
        (while (nth 3 (syntax-ppss))
          (skip-syntax-backward "^\"|")
          (backward-char 1))
      (unless (re-search-backward (regexp-quote open-str) nil t)
        (error "Can't find '%s'" open-str)))
    (if inner-p
        (set-mark (1+ (point)))
      (set-mark (point)))
    (if close-str
        (forward-sexp 1)
      (forward-char 1)
      (re-search-forward (regexp-quote open-str) nil t))
    (and inner-p (backward-char 1))))

;;;###autoload
(defun editutil-mark-inside-paired (char)
  (interactive
   (list (read-char)))
  (editutil--mark-paired char t))

;;;###autoload
(defun editutil-mark-around-paired (char)
  (interactive
   (list (read-char)))
  (editutil--mark-paired char nil))

;;;###autoload
(defun editutil-replace-wrapped-string (arg)
  (interactive "p")
  (let ((replaced (char-to-string (read-char))))
    (editutil-unwrap-at-point arg replaced)))

;;;###autoload
(defun editutil-edit-previous-line (arg)
  (interactive "p")
  (if (< arg 0)
      (editutil-edit-next-line (- arg))
    (dotimes (i arg)
      (if (= (line-number-at-pos) 1)
          (goto-char (line-beginning-position))
        (forward-line -1)
        (end-of-line))
      (newline-and-indent))))

;;;###autoload
(defun editutil-edit-next-line (arg)
  (interactive "p")
  (if (>= arg 0)
      (dotimes (i arg)
        (end-of-line)
        (newline-and-indent))
    (editutil-edit-previous-line (- arg))))

;;;###autoload
(defun editutil-edit-next-line-no-indent (arg)
  (interactive "p")
  (dotimes (i arg)
    (end-of-line)
    (newline)))

;;;###autoload
(defun editutil-edit-next-line-same-column (arg)
  (interactive "p")
  (let ((col (save-excursion
               (back-to-indentation)
               (current-column))))
    (dotimes (i arg)
      (end-of-line)
      (newline)
      (move-to-column col t))))

;;;###autoload
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

;;;###autoload
(defun editutil-zap-to-char-backward ()
  (interactive)
  (let ((arg (prefix-numeric-value current-prefix-arg))
        (char (read-char "Zap to char: " t))
        (curpoint (point))
        (case-fold-search nil))
    (save-excursion
      (when (search-backward (char-to-string char) nil t)
        (delete-region (1+ (point)) curpoint)))))

;;;###autoload
(defun editutil-next-symbol (arg)
  (interactive "p")
  (let ((symbol (thing-at-point 'symbol))
        (curpoint (point)))
    (unless symbol
      (error "No symbol at cursor!!"))
    (let ((bound (bounds-of-thing-at-point 'symbol)))
      (if (>= arg 0)
          (goto-char (cdr bound))
        (goto-char (car bound))))
    (let ((regexp (concat "\\_<" (regexp-quote symbol) "\\_>")))
      (if (re-search-forward regexp nil t arg)
          (goto-char (match-beginning 0))
        (goto-char curpoint)
        (error "No more found('%s')" symbol)))))

;;;###autoload
(defun editutil-previous-symbol (arg)
  (interactive "p")
  (editutil-next-symbol (- arg)))

(defvar editutil--last-search-char nil)

(defsubst editutil--last-command-move-char-p ()
  (memq last-command '(editutil-forward-char editutil-backward-char)))

;;;###autoload
(defun editutil-forward-char (arg &optional char)
  (interactive "p\n")
  (unless char
    (if (editutil--last-command-move-char-p)
        (setq char editutil--last-search-char)
      (setq char (read-event))))
  (unless (char-or-string-p char)
    (error "Error: Input Invalid Char %d" char))
  (setq editutil--last-search-char char)
  (when (>= arg 0)
    (forward-char 1))
  (let ((case-fold-search nil))
    (search-forward (char-to-string char) nil t arg))
  (when (>= arg 0)
    (backward-char 1)))

;;;###autoload
(defun editutil-backward-char (arg &optional char)
  (interactive "p\n")
  (unless char
    (if (editutil--last-command-move-char-p)
        (setq char editutil--last-search-char)
      (setq char (read-event))))
  (backward-char 1)
  (editutil-forward-char (- arg) char))

;;;###autoload
(defun editutil-move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

;;;###autoload
(defun editutil-move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

;;;###autoload
(defun editutil-delete-following-spaces (arg)
  (interactive "p")
  (let ((orig-point (point)))
    (save-excursion
      (if (<= arg 0)
          (skip-chars-backward " \t")
        (skip-chars-forward " \t"))
      (delete-region orig-point (point)))))

;;;###autoload
(defun editutil-yank (arg)
  (interactive "P")
  (setq yank-window-start (window-start))
  (setq this-command t)
  (push-mark (point))
  (let ((str (current-kill 0)))
    (dotimes (i (or arg 1))
      (insert-for-yank str)))
  (when (eq this-command t)
    (setq this-command 'yank))
  nil)

;;;###autoload
(defun editutil-insert-newline-without-moving ()
  (interactive)
  (save-excursion
    (newline)))

(defsubst editutil--enable-subword-mode-p ()
  (and (boundp 'subword-mode) subword-mode))

(defun editutil--forward-next-space ()
  (save-excursion
    (let ((space-p (looking-at-p "[ \t\r\n]")))
      (skip-chars-forward " \t")
      (unless space-p
        (skip-chars-forward "^ \t"))
      (point))))

;;;###autoload
(defun editutil-delete-word (arg)
  (interactive "p")
  (let ((next-not-space (editutil--forward-next-space)))
    (save-excursion
      (delete-region (point) (progn
                               (if (editutil--enable-subword-mode-p)
                                   (subword-forward arg)
                                 (forward-word arg))
                               (min next-not-space (point)))))))

;;;###autoload
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

;;;###autoload
(defun editutil-number-rectangle (start end format-string from)
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
             for i from from   do
             (move-to-column column t)
             (insert (format format-string i))
             (forward-line 1)))
  (goto-char start))

;;;###autoload
(defun editutil-copy-sexp ()
  (interactive)
  (copy-sexp)
  (message "%s" (thing-at-point 'sexp)))

;;;###autoload
(defun editutil-duplicate-thing (n)
  (interactive "p")
  (let ((orig-column (current-column))
        (lines (if mark-active
                   (1+ (- (line-number-at-pos (region-end))
                          (line-number-at-pos (region-beginning))))
                 1)))
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
        (dotimes (i (or n 1))
          (insert str "\n"))))
    (forward-line lines)
    (move-to-column orig-column)))

;;;###autoload
(defun editutil-view-insert ()
  (interactive)
  (when view-mode
    (view-mode -1)))

;;;###autoload
(defun editutil-view-insert-at-next ()
  (interactive)
  (when view-mode
    (view-mode -1))
  (forward-char 1))

;;;###autoload
(defun editutil-view-insert-at-bol ()
  (interactive)
  (when view-mode
    (view-mode -1))
  (back-to-indentation))

;;;###autoload
(defun editutil-view-insert-at-eol ()
  (interactive)
  (when view-mode
    (view-mode -1))
  (goto-char (line-end-position)))

;;;###autoload
(defun editutil-goto-last-line ()
  (interactive)
  (goto-char (point-max))
  (goto-char (line-beginning-position)))

;;;###autoload
(defun editutil-backward-symbol ()
  (interactive)
  (forward-symbol -1))

;;;###autoload
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

;;;###autoload
(defun editutil-copy-line (arg)
  (interactive "p")
  (let ((start (line-beginning-position)))
    (save-excursion
      (forward-line (1- arg))
      (kill-ring-save start (line-end-position)))))

;;;###autoload
(defun editutil-isearch-match-begin ()
  (interactive)
  (isearch-exit)
  (when (and isearch-forward isearch-success)
    (backward-char (length isearch-string))))

;;;###autoload
(defun editutil-isearch-match-end ()
  (interactive)
  (isearch-exit)
  (when (and (not isearch-forward) isearch-success)
    (forward-char (1- (length isearch-string)))))

;;;###autoload
(defun editutil-backward-up (arg)
  (interactive "p")
  (if (nth 3 (syntax-ppss))
      (progn
        (skip-syntax-backward "^\"|")
        (backward-char 1))
    (unless (ignore-errors
              (backward-up-list arg)
              t)
      (skip-syntax-backward "^(")
      (backward-char 1))))

;;;###autoload
(defun editutil-escape-parentheses ()
  (interactive)
  (if (nth 3 (syntax-ppss))
      (progn
        (skip-syntax-forward "^\"|")
        (forward-char 1))
    (backward-up-list 1)
    (forward-sexp 1)))

;;;###autoload
(defun editutil-down-list (arg)
  (interactive "p")
  (unless (ignore-errors
            (down-list arg)
            t)
    (skip-syntax-forward "^(")
    (forward-char 2)))

;;;###autoload
(defun editutil-forward-list (arg)
  (interactive "p")
  (unless (ignore-errors
            (forward-list arg)
            t)
    (editutil-backward-up arg)
    (forward-sexp arg)))

;;;###autoload
(defun editutil-minibuffer-up-dir ()
  (interactive)
  (backward-char 1)
  (when (search-backward "/" nil t)
    (delete-region (1+ (point)) (line-end-position))
    (forward-char 1)))

;;;###autoload
(defun editutil-insert-parentheses (arg)
  (interactive "P")
  (insert-parentheses (or arg 1)))

;;;###autoload
(defun editutil-other-window ()
  (interactive)
  (when (one-window-p)
    (split-window-right))
  (other-window 1))

(defface editutils-highlight
  '((((class color) (background light))
     :background "yellow")
    (((class color) (background dark))
     :background "yellow" :foreground "black"))
  "highlight symbol"
  :group 'editutil)

;;;###autoload
(defun editutil-highlight-clear-overlays ()
  (interactive)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'editutils-highlight)
      (delete-overlay ov))))

;;;###autoload
(defun editutil-highlight-symbol-in-defun ()
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (bounds (bounds-of-thing-at-point 'defun))
         (begin (car bounds))
         (end (cdr bounds)))
    (unless symbol
      (error "Here is not on symbol!!"))
    (editutil-highlight-clear-overlays)
    (save-excursion
      (goto-char begin)
      (let ((regexp (concat "\\_<" symbol "\\_>")))
        (while (re-search-forward regexp end t)
          (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put ov 'face 'editutils-highlight)
            (overlay-put ov 'editutils-highlight t)))))))

;;;###autoload
(defun editutil-move-left-hand-side ()
  (interactive)
  (let ((bound (line-beginning-position)))
    (if (not (re-search-backward "\\s-+:?=\\s-+" bound t))
        (message "Here is left hand side.")
      (goto-char (match-beginning 0)))))

;;;###autoload
(defun editutil-move-right-hand-side ()
  (interactive)
  (let ((bound (line-end-position)))
    (if (not (re-search-forward "\\s-+:?=\\s-+" bound t))
        (message "Here is righht hand side.")
      (goto-char (match-end 0)))))

;;;###autoload
(defun editutil-show-here-function ()
  (interactive)
  (if (not which-func-mode)
      (message "`which-func-mode' is not enabled")
    (message "%s" (gethash (selected-window) which-func-table))))

;;;###autoload
(defun editutil-toggle-let ()
  (interactive)
  (save-excursion
    (let ((limit (save-excursion (beginning-of-defun) (point))))
      (when (re-search-backward "(\\(let\\)\\(\\*\\)?" limit t)
        (goto-char (match-end 1))
        (if (match-string 2)
            (delete-char 1)
          (insert "*"))
        (backward-up-list)
        (indent-pp-sexp)))))

;;;###autoload
(defun editutil-yank-from-clipboard ()
  (interactive)
  (insert (x-get-clipboard)))

;;;###autoload
(defun editutil-git-intent-to-add ()
  (interactive)
  (save-buffer)
  (let* ((file (file-name-nondirectory (buffer-file-name)))
         (cmd (format "git add -N %s" file)))
    (unless (zerop (call-process-shell-command cmd))
      (error "Failed: %s" cmd))
    (message "Success: %s" cmd))
  (when git-gutter-mode
    (git-gutter)))

;;;###autoload
(defun editutil-default-setup ()
  (interactive)

  (global-set-key [(control shift up)] 'editutil-move-line-up)
  (global-set-key [(control shift down)] 'editutil-move-line-down)

  (global-set-key (kbd "C-M-o") 'editutil-other-window)
  (global-set-key (kbd "C-M-l") 'editutil-escape-parentheses)
  (global-set-key (kbd "C-M-u") 'editutil-backward-up)

  (global-set-key (kbd "C-M-r") 'editutil-mark-around-paired)
  (global-set-key (kbd "C-M-c") 'editutil-mark-inside-paired)

  (global-set-key (kbd "C-M-n") 'editutil-forward-list)
  (global-set-key (kbd "C-M-d") 'editutil-down-list)
  (global-set-key (kbd "M-o") 'editutil-edit-next-line)
  (global-set-key (kbd "M-O") 'editutil-edit-previous-line)
  (global-set-key (kbd "M-s") 'editutil-unwrap-at-point)
  (global-set-key (kbd "M-r") 'editutil-replace-wrapped-string)
  (global-set-key (kbd "M-z") 'editutil-zap-to-char)
  (global-set-key (kbd "M-n") 'editutil-next-symbol)
  (global-set-key (kbd "M-p") 'editutil-previous-symbol)
  (global-set-key (kbd "M-k") 'editutil-delete-following-spaces)
  (global-set-key (kbd "C-y") 'editutil-yank)
  (global-set-key (kbd "M-d") 'editutil-delete-word)
  (global-set-key [remap backward-kill-word] 'editutil-backward-delete-word)
  (global-set-key (kbd "C-x r N") 'editutil-number-rectangle)
  (global-set-key (kbd "C-M-SPC") 'editutil-copy-sexp)
  (global-set-key (kbd "M-I") 'editutil-indent-same-as-previous-line)
  (global-set-key (kbd "M-(") 'editutil-insert-parentheses)
  (global-set-key (kbd "C-x l") 'editutil-copy-line)

  (global-set-key (kbd "C-x v N") 'editutil-git-intent-to-add)

  ;; C-q map
  (define-key my/ctrl-q-map (kbd ".") 'editutil-highlight-symbol-in-defun)
  (define-key my/ctrl-q-map (kbd ",") 'editutil-highlight-clear-overlays)
  (define-key my/ctrl-q-map (kbd "s") 'editutil-unwrap-at-point)
  (define-key my/ctrl-q-map (kbd "r") 'editutil-replace-wrapped-string)

  (define-key my/ctrl-q-map (kbd "C-h") 'editutil-move-left-hand-side)
  (define-key my/ctrl-q-map (kbd "C-l") 'editutil-move-right-hand-side)
  (define-key my/ctrl-q-map (kbd "?") 'editutil-show-here-function)

  (when window-system
    ;; This command should be used from `emacsclient -t'
    (define-key my/ctrl-q-map (kbd "y") 'editutil-yank-from-clipboard))

  (define-key isearch-mode-map [remap isearch-exit] 'editutil-isearch-match-begin)
  (define-key isearch-mode-map (kbd "M-a") 'editutil-isearch-match-begin)
  (define-key isearch-mode-map (kbd "M-e") 'editutil-isearch-match-end)

  (define-key minibuffer-local-map (kbd "C-M-u") 'editutil-minibuffer-up-dir)

  (smartrep-define-key
      global-map "C-x" '(("j" . 'editutil-insert-newline-without-moving)))

  (smartrep-define-key
      global-map "M-g" '(("c" . 'editutil-duplicate-thing)))
  t)

(provide 'editutil)

;;; editutil.el ends here
