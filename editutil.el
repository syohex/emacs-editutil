;;; editutil.el --- My own Edit Utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2022 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-editutil
;; Version: 0.01
;; Package-Requires: ((emacs "28.2") (helm "3.9.0"))

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
  (defvar paredit-mode-map)
  (defvar helm-map)
  (defvar fsharp-mode-map)
  (defvar term-mode-map)
  (defvar term-raw-map))

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)
(require 'which-func)
(require 'dired)

(require 'xref)
(require 'recentf)
(require 'helm)
(require 'helm-bookmark)
(require 'helm-files)
(require 'helm-mode)

(declare-function subword-forward "subword")
(declare-function subword-backward "subword")
(declare-function recentf-save-list "recentf")

(defgroup editutil nil
  "My own editing utilities"
  :group 'editing)

(defun editutil-forward-symbol-at-point ()
  (interactive)
  (let ((symbol (thing-at-point 'symbol))
        (case-fold-search nil))
    (isearch-forward-symbol-at-point)
    (when symbol
      (setq regexp-search-ring
            (cons (substring-no-properties symbol) regexp-search-ring)))))

(defsubst editutil--in-string-p ()
  (nth 3 (syntax-ppss)))

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

(defun editutil--do-to-char (arg char func)
  (let* ((bound (if (>= arg 0) (line-end-position) (line-beginning-position)))
         (step (if (>= arg 0) -1 1))
         (start (point))
         end-pos)
    (let ((case-fold-search nil))
      (when (>= arg 0)
        (forward-char 1))
      (when (search-forward (char-to-string char) bound t arg)
        (forward-char step)
        (setq end-pos (point))))
    (when end-pos
      (funcall func start end-pos))))

(defun editutil-zap-to-char (arg char)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char nil t)))
  (editutil--do-to-char arg char #'kill-region))

(defun editutil-copy-to-char (arg char)
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char nil t)))
  (save-excursion
    (editutil--do-to-char arg char #'kill-ring-save)))

(defvar editutil--last-search-char nil)

(defun editutil-forward-to-char (arg char)
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (read-char nil t)))
  (setq editutil--last-search-char char)
  (forward-char 1)
  (let ((case-fold-search nil))
    (search-forward (char-to-string char) (line-end-position) t arg))
  (backward-char 1))

(defun editutil-backward-to-char (arg char)
  (interactive
   (list
    (prefix-numeric-value current-prefix-arg)
    (read-char nil t)))
  (setq editutil--last-search-char char)
  (let ((case-fold-search nil))
    (search-backward (char-to-string char) (line-beginning-position) t arg)))

(defun editutil-forward-last-char ()
  (interactive)
  (if editutil--last-search-char
      (editutil-forward-to-char 1 editutil--last-search-char)
    (call-interactively #'editutil-forward-to-char)))

(defun editutil-backward-last-char ()
  (interactive)
  (if editutil--last-search-char
      (editutil-backward-to-char 1 editutil--last-search-char)
    (call-interactively #'editutil-backward-to-char)))

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

(defun editutil-yank-next-line ()
  (interactive)
  (goto-char (line-end-position))
  (open-line 1)
  (forward-line 1)
  (editutil-yank 1))

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

(defun editutil-other-window-backward ()
  (interactive)
  (other-window -1))

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

(defvar editutil--previous-buffer nil)

;; for `cde' command
(defun editutil-current-buffer-directory ()
  (let* ((bufsinfo (cadr (cadr (current-frame-configuration))))
         (bufname-list (assoc-default 'buffer-list bufsinfo)))
    (cl-loop for buf in bufname-list
             for file = (or (buffer-file-name buf)
                            (with-current-buffer buf
                              (when (eq major-mode 'dired-mode)
                                dired-directory)))
             when file
             return (file-name-directory it))))

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

(defun editutil-toggle-cleanup-spaces ()
  (interactive)
  (cond ((memq 'delete-trailing-whitespace before-save-hook)
         (remove-hook 'before-save-hook 'delete-trailing-whitespace))
        (t
         (add-hook 'before-save-hook 'delete-trailing-whitespace)))
  (force-mode-line-update))

(defface editutil-clean-space
  '((t (:inherit font-lock-keyword-face :weight bold)))
  "Clean spaces statement in mode-line.")

(defvar editutil-cleanup-space-mode-line
  '(:eval (if (or (memq 'delete-trailing-whitespace before-save-hook)
                  (bound-and-true-p eglot--managed-mode))
              ""
            (propertize "[DT-]" 'face 'editutil-clean-space))))
(put 'editutil-cleanup-space-mode-line 'risky-local-variable t)

(defface editutil-vc-branch
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
    face editutil-vc-branch)
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

;;;###autoload
(defun editutil-recentf-save-list ()
  (interactive)
  (recentf-save-list)
  (message nil))

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

(defun editutil-delete-following-spaces (arg)
  (interactive "p")
  (when (member (char-after) '(?  ?\t))
    (let ((orig-point (point)))
      (save-excursion
        (if (<= arg 0)
            (forward-whitespace -1)
          (forward-whitespace +1))
        (delete-region orig-point (point))))))

(defun editutil-forward-word-end (arg)
  (interactive "p")
  (forward-char +1)
  (unless (looking-at-p "\\>")
    (backward-char +1))
  (forward-word arg)
  (backward-char +1))

(defun editutil-point-to-register (register)
  (interactive
   (list (register-read-with-preview "")))
  (set-register register (point-marker)))

(defun editutil-jump-to-register (register)
  (interactive
   (list (register-read-with-preview "")))
  (let ((val (get-register register)))
    (register-val-jump-to val nil)))

;;
;; Buffer utilities
;;

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

(defun editutil--save-current-windows ()
  (setq editutil--previous-buffer (current-buffer))
  (window-configuration-to-register :editutil-ansiterm))

(defun editutil--buffer-visible-p (bufname)
  (cl-loop for win in (window-list)
           for winbuf = (window-buffer win)
           thereis (string= bufname (buffer-name winbuf))))

;;
;; shell utilities
;;

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

(defun editutil-ansi-term-kill-buffer (&optional process _msg)
  (kill-buffer (process-buffer process)))

(defun editutil-restore-ansi-term ()
  (interactive)
  (unless (string= (buffer-name) "*ansi-term*")
    (error "This buffer is not ansi-term buffer"))
  (jump-to-register :editutil-ansiterm))

(defun editutil-kill-this-buffer ()
  (interactive)
  (call-interactively #'kill-this-buffer))

;; fixed line position after scrollup, scrolldown
(defun editutil-scroll-move-around (orig-fn &rest args)
  (let ((orig-line (count-lines (window-start) (point))))
    (apply orig-fn args)
    (move-to-window-line orig-line)))

;;
;; Programming utilities
;;

(defun editutil-comment-line ()
  (interactive)
  (save-excursion
    (if (use-region-p)
        (comment-or-uncomment-region (region-beginning) (line-end-position))
      (call-interactively #'comment-line))))

(defun editutil-find-rust-project-root (dir)
  (when-let ((root (locate-dominating-file dir "Cargo.toml")))
    (list 'vc 'Git root)))

(defun editutil-rust-mode-hook ()
  (setq-local project-find-functions (list #'editutil-find-rust-project-root)))

(defun editutil-fsharp-format ()
  (interactive)
  (when (buffer-modified-p)
    (save-buffer))
  (unless (process-file "fantomas" nil nil nil (buffer-file-name))
    (error "failed to format file"))
  (revert-buffer t t))

(define-minor-mode editutil-global-minor-mode
  "Most superior minir mode"
  :global t
  :lighter ""
  :keymap
  `((,(kbd "C-M-j") . editutil-hippie-expand)
    (,(kbd "M-q") . editutil-zap-to-char)
    (,(kbd "C-M-o") . editutil-other-window)))

(defvar editutil-ctrl-q-map (make-sparse-keymap)
  "keymap binded to C-q")

;;
;; helm
;;

(defun helm-editutil--open-dired (file)
  (dired (file-name-directory file)))

(defun helm-editutil--make-git-ls-function (options)
  (lambda ()
    (with-current-buffer (helm-candidate-buffer 'global)
      (apply 'process-file "git" nil t nil "ls-files" options))))

(defvar helm-editutil--git-ls-actions
  (helm-make-actions
   "Open File" #'find-file
   "Open File other window" #'find-file-other-window
   "Find File alternate" #'find-alternate-file
   "Open Directory" #'helm-editutil--open-dired
   "Insert buffer" #'insert-file))

(defun helm-editutil--git-ls-files-source (pwd)
  (cl-loop with is-first = t
           for (description . options) in '(("Modified Files" . ("--modified"))
                                            ("Untracked Files" . ("--others" "--exclude-standard"))
                                            ("All Files" . ("--")))
           for header = (if is-first
                            (progn
                              (setq is-first nil)
                              (format "%s (%s)" description pwd))
                          description)
           collect
           (helm-build-in-buffer-source header
             :init (helm-editutil--make-git-ls-function options)
             :action helm-editutil--git-ls-actions
             :keymap helm-find-files-map)))

(defun helm-editutil--git-ls-files-common (project-search)
  (let ((topdir (locate-dominating-file default-directory ".git")))
    (unless topdir
      (error "Here is not Git Repository!!"))
    (let* ((default-directory (if project-search
                                  topdir
                                default-directory))
           (current-prefix-arg nil)
           (sources (helm-editutil--git-ls-files-source topdir)))
      (helm :sources sources :buffer "*Helm Git Project*"))))

(defun helm-editutil-git-ls-files-project ()
  (interactive)
  (helm-editutil--git-ls-files-common t))

(defun helm-editutil-git-ls-files ()
  (interactive)
  (helm-editutil--git-ls-files-common nil))

(defun helm-editutil--recentf-transform (candidates _source)
  (cl-loop for i in candidates
           if helm-ff-transformer-show-only-basename
           collect (cons (helm-basename i) i)
           else collect i))

(defun helm-editutil--file-in-dired (file)
  (dired (file-name-directory file))
  (dired-goto-file file))

(defun helm-editutil--recentf-candidates ()
  (mapcar #'abbreviate-file-name recentf-list))

(defvar helm-editutil-source-recentf
  (helm-build-sync-source "Recently open files"
    :candidates #'helm-editutil--recentf-candidates
    :filtered-candidate-transformer #'helm-editutil--recentf-transform
    :candidate-number-limit 9999
    :volatile t
    :action (helm-make-actions
             "Open File" #'find-file
             "Open File other window" #'find-file-other-window
             "Open File alternate" #'find-alternate-file
             "Open Files in dired" #'helm-editutil--file-in-dired
             "Insert File" #'insert-file)))

(defun helm-editutil-recentf-and-bookmark ()
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources '(helm-editutil-source-recentf helm-source-bookmarks)
          :buffer "*helm recentf+bookmark*")))

(defun helm-editutil--find-files-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (zerop (process-file "perl" nil t nil "-E" "say for grep {-T $_} glob('* .*')"))
      (error "Failed: collect files"))))

(defvar helm-editutil-source-find-files
  (helm-build-in-buffer-source "Find Files"
    :init #'helm-editutil--find-files-init
    :action (helm-make-actions
             "Open File" #'find-file
             "Open File other window" #'find-file-other-window
             "Open File alternate" #'find-alternate-file
             "Insert File" #'insert-file)))

(defvar helm-editutil-source-find-directories
  (helm-build-sync-source "Find directories"
    :candidates (lambda ()
                  (cl-loop for f in (directory-files default-directory)
                           when (and (file-directory-p f)
                                     (not (string-prefix-p "." f)))
                           collect (file-name-as-directory f)))
    :action (helm-make-actions
             "Open File" #'find-file
             "Open File other window" #'find-file-other-window
             "Open File alternate" #'find-alternate-file)))

(defun helm-editutil-find-files ()
  (interactive)
  (helm :sources '(helm-editutil-source-find-files helm-editutil-source-find-directories)
        :buffer "*Helm Find Files*"))

(defun helm-editutil--buffer-display (bufname)
  (with-current-buffer bufname
    (let ((path (helm-aif (buffer-file-name)
                    (abbreviate-file-name it)
                  default-directory)))
      (format "%-25s %s" bufname path))))

(defun helm-editutil-switch-buffer ()
  (interactive)
  (let ((bufs (cl-loop with files = nil
                       with directories = nil
                       with others = nil

                       for buf in (buffer-list)
                       for name = (buffer-name buf)
                       do
                       (cond ((string-prefix-p "*" name)
                              (unless (string-prefix-p "*helm" name t)
                                (push name others)))
                             ((not (string-prefix-p " " name))
                              (if (buffer-file-name buf)
                                  (push name files)
                                (push name directories))))
                       finally
                       return (list :files (reverse files)
                                    :directories (reverse directories)
                                    :others (reverse others)))))
    (helm :sources (cl-loop for (prop . name) in '((:files . "File Buffers")
                                                   (:directories . "Directory Buffers")
                                                   (:others . "Other Buffers"))
                            for display-fn = (unless (eq prop :others)
                                               #'helm-editutil--buffer-display)
                            when (plist-get bufs prop)
                            collect
                            (helm-build-sync-source name
                              :candidates it
                              :real-to-display display-fn
                              :action (helm-make-actions
                                       "Switch buffer" #'switch-to-buffer
                                       "Switch buffer other window" #'switch-to-buffer-other-window
                                       "Insert buffer" #'insert-buffer
                                       "Kill buffer" #'kill-buffer)))
          :buffer "*Helm Switch Buffer*")))

(defun helm-editutil-select-2nd-action ()
  (interactive)
  (helm-select-nth-action 1))

(defun helm-editutil-select-3rd-action ()
  (interactive)
  (helm-select-nth-action 2))

;;
;; xref
;;

(defun helm-editutil--project-root ()
  (cl-loop for path in '("Cargo.toml" "go.mod" ".git")
           when (locate-dominating-file default-directory path)
           return it))

(defun helm-editutil--xref-format-candidate (file line summary)
  (concat
   (propertize file 'face 'helm-grep-file)
   (and (integerp line)
        (concat ":" (propertize (int-to-string line) 'font-lock-face 'helm-grep-lineno)))
   ":" summary))

(defun helm-editutil--xref-candidates (fetcher alist)
  (let ((xrefs (or (assoc-default 'fetched-xrefs alist)
                   (funcall fetcher)))
        (project-root (helm-editutil--project-root)))
    (cl-loop for xref in xrefs
             for summary = (xref-item-summary xref)
             for location = (xref-item-location xref)
             for line = (xref-location-line location)
             for file = (file-relative-name (xref-location-group location) project-root)
             collect
             (cons (helm-editutil--xref-format-candidate file line summary) xref))))

(defun helm-editutil--xref-highlight-line (xref)
  (let* ((location (xref-item-location xref))
         (marker (xref-location-marker location))
         (buf (marker-buffer marker))
         (offset (marker-position marker)))
    (switch-to-buffer buf)
    (goto-char offset)
    (helm-highlight-current-line)))

(defun helm-editutil--find-file-common (xref func)
  (let* ((location (xref-item-location xref))
         (marker (xref-location-marker location))
         (buf (marker-buffer marker))
         (offset (marker-position marker)))
    (funcall func buf)
    (goto-char offset)))

(defun helm-editutil--xref-find-file (xref)
  (helm-editutil--find-file-common xref #'switch-to-buffer))

(defun helm-editutil--xref-find-file-other-window (xref)
  (helm-editutil--find-file-common xref #'switch-to-buffer-other-window))

(defun helm-editutil-source-xref (fetcher alist)
  (helm-build-sync-source "Xref"
    :candidates (helm-editutil--xref-candidates fetcher alist)
    :persistent-action #'helm-editutil--xref-highlight-line
    :action (helm-make-actions
             "Open file" #'helm-editutil--xref-find-file
             "Open file other window" #'helm-editutil--xref-find-file-other-window)
    :candidate-number-limit 9999))

(defun helm-editutil-show-xrefs (fetcher alist)
  (helm :sources (helm-editutil-source-xref fetcher alist)
        :truncate-lines t
        :buffer "*Helm Xrefs*"))

(defun helm-editutil-xref-show-defs (fetcher alist)
  (let ((xrefs (funcall fetcher)))
    (cond
     ((not (cdr xrefs))
      (xref-pop-to-location (car xrefs)
                            (assoc-default 'display-action alist)))
     (t
      (helm-editutil-show-xrefs fetcher
                                (cons (cons 'fetched-xrefs xrefs)
                                      alist))))))

;;;###autoload
(defun editutil-default-setup ()
  (interactive)

  (editutil--init-mode-line)

  (global-unset-key (kbd "C-x z"))

  (global-set-key (kbd "RET") #'editutil-newline)
  (global-set-key (kbd "C-j") #'editutil-newline-and-maybe-indent)

  (global-set-key (kbd "C-M-s") #'editutil-forward-symbol-at-point)

  (global-set-key (kbd "C-w") #'editutil-kill-region)
  (global-set-key (kbd "M-w") #'editutil-kill-ring-save)

  (global-set-key (kbd "M-q") #'editutil-zap-to-char)
  (global-set-key (kbd "M-z") #'editutil-copy-to-char)

  (global-set-key (kbd "C-M-o") #'editutil-other-window)
  (global-set-key (kbd "C-M-l") #'editutil-other-window-backward)
  (global-set-key (kbd "C-M-u") #'editutil-backward-up)

  (global-set-key (kbd "C-k") #'editutil-kill-line)
  (global-set-key (kbd "C-M-n") #'editutil-forward-list)
  (global-set-key (kbd "C-M-d") #'editutil-down-list)
  (global-set-key (kbd "M-o") #'editutil-edit-next-line)
  (global-set-key (kbd "M-O") #'editutil-edit-previous-line)

  (global-set-key (kbd "C-x k") #'editutil-kill-this-buffer)

  (global-set-key (kbd "C-y") #'editutil-yank)
  (global-set-key (kbd "C-x y") #'editutil-yank-next-line)

  (global-set-key (kbd "M-SPC") #'editutil-point-to-register)
  (global-set-key (kbd "M-j") #'editutil-jump-to-register)

  (global-set-key (kbd "M-e") #'editutil-forward-word-end)
  (global-set-key (kbd "M-d") #'editutil-delete-word)

  (global-set-key (kbd "M-u") #'editutil-upcase)

  (global-set-key (kbd "M-;") #'editutil-comment-line)
  (global-set-key (kbd "M-\\") #'editutil-delete-following-spaces)

  (global-set-key [remap backward-kill-word] #'editutil-backward-delete-word)

  (global-set-key (kbd "M-(") #'editutil-insert-parentheses)

  (global-set-key (kbd "C-x l") #'editutil-mark-line)
  (global-set-key (kbd "C-M-w") #'editutil-mark-sexp)

  (global-set-key (kbd "C-x L") #'editutil-copy-line)
  (global-set-key (kbd "C-x \\") #'editutil-ansi-term)

  (global-set-key (kbd "M-l") #'editutil-forward-to-char)
  (global-set-key (kbd "M-h") #'editutil-backward-to-char)
  (global-set-key (kbd "M-'") #'editutil-forward-last-char)
  (global-set-key (kbd "M--") #'editutil-backward-last-char)

  ;; 'C-x r' prefix
  (global-set-key (kbd "C-x r N") #'editutil-number-rectangle)

  ;; 'M-g' prefix
  (global-set-key (kbd "M-g [") #'editutil-cycle-next-buffer)
  (global-set-key (kbd "M-g ]") #'editutil-cycle-previous-buffer)

  (define-key global-map (kbd "C-q") editutil-ctrl-q-map)
  (define-key editutil-ctrl-q-map (kbd "C-q") 'quoted-insert)
  (define-key editutil-ctrl-q-map (kbd "C-t") 'editutil-toggle-cleanup-spaces)

  (add-hook 'after-change-major-mode-hook #'editutil-clear-mode-line)

  ;; helm-editutil
  (global-set-key (kbd "C-x C-p") 'helm-editutil-git-ls-files)
  (global-set-key (kbd "C-x C-a") 'helm-editutil-git-ls-files-project)
  (global-set-key (kbd "C-x C-r") 'helm-editutil-recentf-and-bookmark)
  (global-set-key (kbd "C-x C-x") 'helm-editutil-find-files)
  (global-set-key (kbd "C-x b") 'helm-editutil-switch-buffer)
  (global-set-key (kbd "C-M-r") 'helm-editutil-search-buffer)

 (with-eval-after-load 'helm
   (define-key helm-map (kbd "C-e") 'helm-editutil-select-2nd-action)
   (define-key helm-map (kbd "C-j") 'helm-editutil-select-3rd-action))

  (setq xref-show-xrefs-function 'helm-editutil-show-xrefs)
  (setq xref-show-definitions-function 'helm-editutil-xref-show-defs)

  (dolist (hook '(prog-mode-hook text-mode-hook markdown-mode-hook))
    (add-hook hook #'editutil--add-watchwords))

  (run-with-idle-timer 10 t #'editutil-auto-save-buffers)

  (advice-add 'scroll-up :around 'editutil-scroll-move-around)
  (advice-add 'scroll-down :around 'editutil-scroll-move-around)

  (with-eval-after-load 'paredit
    (define-key paredit-mode-map (kbd "M-q") #'editutil-zap-to-char)
    (define-key paredit-mode-map (kbd "C-c l") #'editutil-toggle-let)
    (define-key paredit-mode-map (kbd "C-c j") #'editutil-newline-after-sexp)
    (define-key paredit-mode-map (kbd "DEL") #'editutil-paredit-backward-delete))

  (with-eval-after-load 'term
    (advice-add 'term-sentinel :after #'editutil-ansi-term-kill-buffer)

    (define-key term-mode-map (kbd "C-x") nil)
    (define-key term-raw-map (kbd "C-x") nil)

    (define-key term-mode-map (kbd "C-x \\") #'editutil-restore-ansi-term)
    (define-key term-raw-map (kbd "C-x \\") #'editutil-restore-ansi-term))

  (add-hook 'rust-mode-hook #'editutil-rust-mode-hook)

  (with-eval-after-load 'fsharp-mode
    (define-key fsharp-mode-map (kbd "C-c C-f") #'editutil-fsharp-format))

  ;; pop-to-mark-command
  (advice-add 'pop-to-mark-command :around #'editutil-pop-to-mark-advice)
  (custom-set-variables
   '(set-mark-command-repeat-pop t))

  ;;(makunbound 'editutil-global-minor-mode-map)
  (editutil-global-minor-mode +1)

  t)

(provide 'editutil)

;;; editutil.el ends here
