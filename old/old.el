(eval-when-compile
  (defvar common-lisp-hyperspec--symbols))

(declare-function ghc-resolve-package-name "ghc")
(declare-function ghc-display-document "ghc")

(defun helm-editutil-etags-select (arg)
  (interactive "P")
  (let ((tag  (helm-etags-get-tag-file))
        (helm-execute-action-at-once-if-one t))
    (when (or (= (prefix-numeric-value arg) 4)
              (and helm-etags-mtime-alist
                   (helm-etags-file-modified-p tag)))
      (remhash tag helm-etags-cache))
    (if (and tag (file-exists-p tag))
        (helm :sources 'helm-source-etags-select :keymap helm-etags-map
              :input (concat (thing-at-point 'symbol) " ")
              :buffer "*helm etags*"
              :default (concat "\\_<" (thing-at-point 'symbol) "\\_>"))
      (message "Error: No tag file found, please create one with etags shell command."))))

(defvar helm-editutil--hyperspec-source
  `((name . "Lookup Hyperspec")
    (candidates . ,(lambda ()
                     (hash-table-keys common-lisp-hyperspec--symbols)))
    (action . (("Show Hyperspec" . hyperspec-lookup)))))

;;;###autoload
(defun helm-editutil-hyperspec ()
  (interactive)
  (helm :sources '(helm-editutil--hyperspec-source)
        :default (thing-at-point 'symbol)
        :buffer "*Helm HyperSpec*"))

(defvar helm-editutil--ghc-mod-source
  '((name . "GHC Browse Documennt")
    (init . helm-editutil--ghc-mod-init)
    (candidates-in-buffer)
    (candidate-number-limit . 9999)
    (action . helm-editutil--ghc-mod-action-display-document)))

(defun helm-editutil--ghc-mod-init ()
  (with-current-buffer (helm-candidate-buffer 'global)
    (unless (process-file "ghc-mod" nil t nil "list")
      (error "Failed 'ghc-mod list'"))))

(defun helm-editutil--ghc-mod-action-display-document (candidate)
  (let ((pkg (ghc-resolve-package-name candidate)))
    (if (and pkg candidate)
        (ghc-display-document pkg candidate nil)
      (error (format "Not found %s(Package %s)" candidate pkg)))))

(defun helm-editutil-ghc-browse-document ()
  (interactive)
  (helm :sources '(helm-editutil--ghc-mod-source) :buffer "*helm-ghc-document*"))
