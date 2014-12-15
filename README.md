# My Own Editing Utilities [![Build Status](https://travis-ci.org/syohex/emacs-editutil.png)](https://travis-ci.org/syohex/emacs-editutil)

## editutil.el

My configuration is:

```lisp
(require 'editutil)
(editutil-default-setup) ;; my defult key bindings
```

### editutil for view-mode

```lisp
(with-eval-after-load 'view
  (define-key view-mode-map (kbd "i") 'editutil-view-insert)
  (define-key view-mode-map (kbd "a") 'editutil-view-insert-at-next)
  (define-key view-mode-map (kbd "I") 'editutil-view-insert-at-bol)
  (define-key view-mode-map (kbd "A") 'editutil-view-insert-at-eol))
```

## helm-editutil.el

#### `helm-editutil-git-ls-files`

`git ls-files` with helm interface

#### `helm-editutil-etags-select`

Select `etags` candidates with helm interface

#### `helm-editutil-yas-prompt`

`yasnippet` candidates with helm interface

#### `helm-editutil-hyperspec`

Show `hyperspec` documents

#### `helm-editutil-ghc-browse-document`

Show `ghc` documents with helm interface

#### `helm-editutil-recentf-and-bookmark`

My own `helm-recentf` + `helm-books`

#### `helm-editutil-select-2nd-action`, `helm-editutil-select-3rd-action`

Select 2nd or 3rd candidate. These functions were deleted but I like them.
