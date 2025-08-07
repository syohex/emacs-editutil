;;; editutil.el --- My own Edit Utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2024 by Shohei YOSHIDA

;; Author: Shohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-editutil
;; Version: 0.01
;; Package-Requires: ((emacs "30"))

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
  (defvar utop-command))

(require 'cl-lib)
(require 'subr-x)
(require 'pcase)
(require 'thingatpt)
(require 'flymake)
(require 'dired)
(require 'vc)

(require 'xref)
(require 'recentf)

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
                               (forward-word arg)
                               (min next-not-space (point)))))))

(defun editutil-backward-delete-word (arg)
  (interactive "p")
  (let ((bol (line-beginning-position)))
    (when (= (point) bol)
      (backward-char 1))
    (when (looking-back "\\s-+" nil)
      (forward-whitespace -1))
    (let ((start (save-excursion
                   (forward-word (- arg))
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

(defun editutil-paredit-backward-delete ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (call-interactively 'paredit-backward-delete)))

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

(defun editutil-other-window (arg)
  (interactive "p")
  (when (one-window-p)
    (if (> (window-width) 120)
        (split-window-right)
      (split-window-below)))
  (unless (>= (prefix-numeric-value current-prefix-arg) 16)
    (other-window arg)))

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

(defun editutil-kill-line (arg)
  (interactive "P")
  (let ((num (prefix-numeric-value arg)))
    (if (and arg (>= num 1))
        (kill-whole-line num)
      (let ((current-prefix-arg nil))
        (call-interactively 'kill-line)))))

(defun editutil--wrap-with (s)
  (interactive)
  (save-excursion
    (let ((bol (line-beginning-position)))
      (skip-chars-backward "^ \t")
      (when (< (point) bol)
        (goto-char bol))
      (insert s)
      (skip-chars-forward "^ \t\n")
      (insert s))))

(defun editutil-wrap-double-quote ()
  (interactive)
  (editutil--wrap-with "\""))

(defun editutil--add-watchwords ()
  (unless (memq major-mode '(org-mode))
    (font-lock-add-keywords
     nil '(("\\(?:^\\|\\s-\\)\\(FIXME\\|TODO\\|XXX\\)\\(?:\\s-\\|$\\)"
            1 '((:foreground "pink") (:weight bold)) t)))))

(defun editutil--prog-mode-hook ()
  (cl-case major-mode
    ((rust-mode rust-ts-mode) (setq-local compile-command "cargo "))
    ((go-mode go-ts-mode) (setq-local compile-command "go "))
    (emacs-lisp-mode (setq-local mode-name "Emacs-Lisp"))
    (tuareg-mode (setq-local mode-name "Ocaml"
                             compile-command "dune "))))

(defvar editutil--previous-buffer nil)

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

(defun editutil-copy-region-to-clipboard ()
  (interactive)
  (unless (use-region-p)
    (user-error "not specified region"))
  ;; only support linux GUI and WSL
  (let* ((is-wsl (getenv "WSLENV"))
         (args (if is-wsl
                   (list "/mnt/c/Windows/System32/clip.exe" nil nil nil)
                 (list "xsel" nil nil nil "--input" "--clipboard"))))
    (unless (zerop (apply #'call-process-region (region-beginning) (region-end) args))
      (error "failed to copy region to clipboard"))
    (deactivate-mark)))

(defun editutil-next-error ()
  (interactive)
  (if flymake-mode
      (call-interactively #'flymake-goto-next-error)
    (call-interactively #'next-error)))

(defun editutil-previous-error ()
  (interactive)
  (if flymake-mode
      (call-interactively #'flymake-goto-prev-error)
    (call-interactively #'previous-error)))

(defvar editutil-vc-mode-line
  '(:propertize
    (:eval
     (when-let* ((branch (and vc-mode (substring-no-properties vc-mode 5))))
       (let ((change-hunks (if (bound-and-true-p git-gutter2-mode)
                               (let ((hunks (git-gutter2-buffer-hunks)))
                                 (if (zerop hunks)
                                     ""
                                   (format ":%d" hunks)))
                             "")))
         (concat "(" branch change-hunks ")"))))
    face (:foreground "color-202" :weight bold))
  "Mode line format for `vc-mode'.")
(put 'editutil-vc-mode-line 'risky-local-variable t)

(defun editutil--evil-mode-line-color (state)
  (cl-case state
    (normal "color-40")
    (insert "color-198")
    (visual "color-141")
    (otherwise "color-227")))

(defvar editutil-evil-mode-line
  '(:propertize
    (:eval
     (if-let* ((state (bound-and-true-p evil-state)))
         (let ((foreground (editutil--evil-mode-line-color state)))
           (propertize (concat "[" (upcase (symbol-name state)) "]")
                       'face `(:foreground ,foreground :weight bold)))
       "")))
  "Mode line format for `evil-mode'.")
(put 'editutil-evil-mode-line 'risky-local-variable t)

(defvar editutil-encoding-mode-line
  '(:propertize
    (:eval (let* ((end-line (cl-case (coding-system-eol-type buffer-file-coding-system)
                              (0 "LF")
                              (1 "CRLF")
                              (2 "CR")))
                  (coding-plist (coding-system-plist buffer-file-coding-system))
                  (encoding (if (memq (plist-get coding-plist :category)
                                      '(coding-category-undecided coding-category-utf8))
                                'utf-8
                              (plist-get coding-plist :name)))
                  (encoding-str (upcase (symbol-name encoding))))
             (concat encoding-str " " end-line)))))
(put 'editutil-encoding-mode-line 'risky-local-variable t)

(defun editutil-show-current-line-diagnostic ()
  (interactive)
  (let ((line (line-number-at-pos)))
    (when-let* ((diag (cl-loop for diag in (flymake-diagnostics)
                               for beg = (flymake--diag-beg diag)
                               when (= line (line-number-at-pos beg))
                               return diag)))
      (let* ((message (flymake--diag-message diag))
             (type (flymake--diag-type diag))
             (face (if (memq type '(eglot-error error))
                       'flymake-error-echo
                     'flymake-warning-echo)))
        (message "%s" (propertize message 'face face))))))

;; copy from flymake
(defun editutil--flymake-mode-line (type)
  (let ((probe (alist-get type flymake--mode-line-counter-cache 'none)))
    (if (eq probe 'none)
        (setf (alist-get type flymake--mode-line-counter-cache)
              (editutil--flymake-mode-line1 type))
      probe)))

(defun editutil--flymake-mode-line1 (type)
  (let ((count 0)
        (face (flymake--lookup-type-property type 'mode-line-face 'compilation-error)))
    (dolist (d (flymake-diagnostics))
      (when (= (flymake--severity type)
               (flymake--severity (flymake-diagnostic-type d)))
        (cl-incf count)))
    (when (or (cl-plusp count)
              (cond ((eq flymake-suppress-zero-counters t)
                     nil)
                    (flymake-suppress-zero-counters
                     (>= (flymake--severity type)
                         (warning-numeric-level
                          flymake-suppress-zero-counters)))
                    (t t)))
      (propertize (format "%d" count) 'face face))))

(defvar editutil-flymake-mode-line
  '(:propertize
    (:eval
     (let ((errors (editutil--flymake-mode-line :error))
           (warnings (editutil--flymake-mode-line :warning)))
       (concat "err:" errors " warn:" warnings)))))
(put 'editutil-flymake-mode-line 'risky-local-variable t)

(defun editutil--init-mode-line ()
  (setq-default
   mode-line-buffer-identification (propertized-buffer-identification "%12b")
   mode-line-mule-info  `("" (current-input-method (:propertize ("" current-input-method-title))))
   ;; only show major-mode
   mode-line-modes '((:propertize (""  mode-name) face (:foreground "color-81")))
   mode-line-position `((:propertize "%l:%C " display (min-width (8.0)))
                        (:propertize ("" (-3 "%p")) display (min-width (4.0)))))

  (setq-default mode-line-format
                `("%e"
                  ((global-mode-string ("" global-mode-string " ")))
                  editutil-evil-mode-line
                  mode-line-mule-info
                  " "
                  mode-line-modified
                  " "
                  mode-line-buffer-identification
                  " "
                  editutil-encoding-mode-line
                  " "
                  editutil-vc-mode-line
                  " "
                  mode-line-modes
                  " "
                  (flymake-mode editutil-flymake-mode-line)
                  " "
                  mode-line-format-right-align
                  ("" mode-line-process)
                  " "
                  (which-function-mode
                   (which-func-mode
                    (which-func--use-mode-line ("" which-func-format " "))))
                  " "
                  mode-line-position
                  " ")))

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

(defun editutil-delete-following-spaces ()
  (interactive)
  (if current-prefix-arg
      (let ((current-prefix-arg nil))
        (delete-horizontal-space))
    (when (member (char-after) '(?  ?\t))
      (let ((orig-point (point)))
        (save-excursion
          (forward-whitespace +1)
          (delete-region orig-point (point)))))))

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

(defun editutil-kill-this-buffer ()
  (interactive)
  (kill-current-buffer))

;;
;; grep utilities
;;

(defun editutil-grep ()
  (interactive)
  (let* ((initial (thing-at-point 'symbol))
         (pattern (read-string "Pattern: " initial))
         (buf (get-buffer-create "*editutil-grep*"))
         (args (if (string-prefix-p "-" pattern)
                   (split-string pattern " " t)
                 (list pattern))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (unless (zerop (apply #'process-file "rg" nil t nil
                            "--vimgrep" "--color=always" args))
        (user-error "failed to execute 'rg'"))
      (ansi-color-apply-on-region (point-min) (point-max))
      (goto-char (point-min))
      (grep-mode)
      (pop-to-buffer (current-buffer)))))

;;
;; Programming utilities
;;

(defvar editutil-prog-prefix (make-sparse-keymap)
  "Keymap for editutil programming utility")

(defun editutil--rust-project-root (dir)
  (let ((git-root (locate-dominating-file dir ".git")))
    ;; check using cargo workspace first
    (if (and git-root (file-exists-p (file-name-concat git-root "Cargo.toml")))
        (list 'vc 'Git git-root)
      (when-let* ((root (locate-dominating-file dir "Cargo.toml")))
        (list 'vc 'Git root)))))

(defun editutil--project-root ()
  (cl-case major-mode
    ((rust-mode rust-ts-mode) (nth 2 (editutil--rust-project-root default-directory)))
    (otherwise (or (locate-dominating-file default-directory ".git")
                   default-directory))))

(defun editutil-rust-mode-hook ()
  (setq-local project-find-functions (list #'editutil--rust-project-root)))

(defsubst editutil--dune-project-p ()
  (cl-loop for file in '("dune" "dune-project")
           thereis (locate-dominating-file default-directory file)))

(defun editutil-utop-minor-hook ()
  (if (editutil--dune-project-p)
      (setq-local utop-command "opam exec -- dune utop . -- -emacs")
    (setq-local utop-command "utop -emacs")))

(defun editutil--format-buffer (cmd &rest args)
  (when (buffer-modified-p)
    (save-buffer))
  (unless (executable-find cmd)
    (user-error "%s is not installed" cmd))
  (let ((display-line-mode-p (bound-and-true-p display-line-numbers-mode)))
    (unless (zerop (apply #'process-file cmd nil nil nil args))
      (error "failed to format file(%s %s)" cmd args))
    (revert-buffer t t)
    (when display-line-mode-p
      (display-line-numbers-mode +1))))

(defun editutil-format-buffer ()
  (interactive)
  (let ((args (cl-case major-mode
                ((c-mode c++-mode) '("clang-format" "-i"))
                (python-mode '("ruff" "format"))
                (go-ts-mode '("go" "fmt"))
                ((rust-mode rust-ts-mode) '("rustfmt"))
                (haskell-mode '("fourmolu" "-i"))
                (tuareg-mode (if (editutil--dune-project-p)
                                 '("ocamlformat" "-i")
                               '("ocamlformat" "-i" "--enable-outside-detected-project")))
                ((js-mode js-ts-mode typescript-ts-mode) '("deno" "fmt"))
                (fsharp-mode '("fantomas"))
                ((yaml-mode yaml-ts-mode) '("yamlfmt"))
                (otherwise (user-error "unsupport formatting for %s" major-mode)))))
    (pcase args
      (`(,cmd . ,options)
       (let ((options (append options (list (buffer-file-name)))))
         (apply #'editutil--format-buffer cmd options)))
      (_ (error "please check arguments: %s"args)))))

(defun editutil-lint-buffer ()
  (interactive)
  (let ((cmd (cl-case major-mode
               ((rust-mode rust-ts-mode) (cons "cargo clippy" (editutil--project-root)))
               ((c-mode c++-mode c-ts-mode c++-ts-mode)
                (concat "clang-tidy " (buffer-file-name)))
               (python-mode (concat "ruff check " (buffer-file-name)))
               (go-ts-mode (concat "staticcheck " (buffer-file-name)))
               ((js-mode js-ts-mode typescript-ts-mode) (concat "deno lint " (buffer-file-name)))
               (otherwise (user-error "unsupport linting for %s" major-mode)))))
    (when (buffer-modified-p)
      (save-buffer))
    (if (consp cmd)
        (let ((default-directory (cdr cmd)))
          (compile (car cmd)))
      (compile cmd))))

(defun editutil-run-test ()
  (interactive)
  (let ((cmd (cl-case major-mode
               ((rust-mode rust-ts-mode) (cons "cargo test" (editutil--project-root)))
               (go-ts-mode "go test")
               ((js-mode js-ts-mode typescript-ts-mode) "deno test")
               (otherwise (user-error "unsupport testing for %s" major-mode)))))
    (when (buffer-modified-p)
      (save-buffer))
    (if (consp cmd)
        (let ((default-directory (cdr cmd)))
          (compile (car cmd)))
      (compile cmd))))

(defun editutil-comment-dwim ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'comment-dwim)
    (save-excursion
      (call-interactively #'comment-line))))

;;
;; eshell
;;

(defun eshell/d (&rest args)
  (let ((arg (car-safe args)))
    (save-selected-window
      (if (null arg)
          (progn
            (call-interactively 'vc-root-diff)
            nil)
        (let ((dir (cond ((string= arg ".") default-directory)
                         ((file-name-absolute-p arg) arg)
                         (t (concat default-directory arg)))))
          (let ((default-directory dir))
            (vc-diff-internal t (list 'Git (list dir)) nil nil)
            nil))))))

;;
;; Ctrl-q
;;

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

;;;###autoload
(defun editutil-default-setup ()
  (interactive)

  (editutil--init-mode-line)

  (global-set-key [remap backward-kill-word] #'editutil-backward-delete-word)
  (global-set-key (kbd "RET") #'editutil-newline)

  (global-set-key (kbd "C-j") #'editutil-newline-and-maybe-indent)
  (global-set-key (kbd "C-k") #'editutil-kill-line)
  (global-set-key (kbd "C-y") #'editutil-yank)
  (global-set-key (kbd "C-w") #'editutil-kill-region)

  (global-set-key (kbd "M-o") #'editutil-edit-next-line)
  (global-set-key (kbd "M-O") #'editutil-edit-previous-line)
  (global-set-key (kbd "M-w") #'editutil-kill-ring-save)
  (global-set-key (kbd "M-q") #'editutil-zap-to-char)
  (global-set-key (kbd "M-d") #'editutil-delete-word)
  (global-set-key (kbd "M-u") #'editutil-upcase)
  (global-set-key (kbd "M-\\") #'editutil-delete-following-spaces)
  (global-set-key (kbd "M-/") #'editutil-comment-dwim)

  (global-set-key (kbd "C-M-o") #'editutil-other-window)
  (global-set-key (kbd "C-M-u") #'editutil-backward-up)
  (global-set-key (kbd "C-M-n") #'editutil-forward-list)
  (global-set-key (kbd "C-M-d") #'editutil-down-list)
  (global-set-key (kbd "C-M-s") #'editutil-forward-symbol-at-point)

  (global-set-key (kbd "C-x $") 'server-edit)
  (global-set-key (kbd "C-x M-w") #'editutil-copy-region-to-clipboard)
  (global-set-key (kbd "C-x k") #'editutil-kill-this-buffer)
  (global-set-key (kbd "C-x \"") #'editutil-wrap-double-quote)

  (global-set-key (kbd "C-x r N") #'editutil-number-rectangle)

  (global-set-key (kbd "M-g [") #'editutil-cycle-next-buffer)
  (global-set-key (kbd "M-g ]") #'editutil-cycle-previous-buffer)
  (global-set-key (kbd "M-g .") #'editutil-grep)

  ;; programming utilities
  (global-set-key (kbd "M-e") editutil-prog-prefix)
  (define-key editutil-prog-prefix "f" #'editutil-format-buffer)
  (define-key editutil-prog-prefix "l" #'editutil-lint-buffer)
  (define-key editutil-prog-prefix "t" #'editutil-run-test)
  (define-key editutil-prog-prefix "c" #'compile)
  (define-key editutil-prog-prefix "r" #'recompile)
  (define-key editutil-prog-prefix "e" #'editutil-show-current-line-diagnostic)

  ;; flymake
  (global-set-key (kbd "M-g M-n") #'editutil-next-error)
  (global-set-key (kbd "M-g M-p") #'editutil-previous-error)
  (global-set-key (kbd "M-g l") #'flymake-show-buffer-diagnostics)

  ;; ctrl-q
  (define-key global-map (kbd "C-q") editutil-ctrl-q-map)
  (define-key editutil-ctrl-q-map (kbd "C-q") 'quoted-insert)
  (define-key editutil-ctrl-q-map "l" 'display-line-numbers-mode)
  (define-key editutil-ctrl-q-map "s" 'scratch-buffer)

  (dolist (hook '(prog-mode-hook text-mode-hook markdown-mode-hook))
    (add-hook hook #'editutil--add-watchwords))
  (add-hook 'prog-mode-hook #'editutil--prog-mode-hook)

  (run-with-idle-timer 20 t #'editutil-auto-save-buffers)

  (with-eval-after-load 'paredit
    (define-key paredit-mode-map (kbd "M-q") #'editutil-zap-to-char)
    (define-key paredit-mode-map (kbd "C-c l") #'editutil-toggle-let)
    (define-key paredit-mode-map (kbd "DEL") #'editutil-paredit-backward-delete))

  (add-hook 'rust-ts-mode-hook #'editutil-rust-mode-hook)
  (add-hook 'utop-minor-mode-hook #'editutil-utop-minor-hook)

  ;; eshell
  (setenv "GIT_EDITOR" "emacsclient")
  (add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . diff-mode))

  (run-at-time t 600 #'editutil-recentf-save-list)

  (editutil-global-minor-mode +1)

  t)

(provide 'editutil)

;;; editutil.el ends here
