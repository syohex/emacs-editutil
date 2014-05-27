# My Own Editing Utilities

## editutil.el

My configuration is:

```lisp
(require 'editutil)
(editutil-default-setup) ;; my defult key bindings
```

### editutil for view-mode

```lisp
(eval-after-load "view"
  '(progn
     (define-key view-mode-map (kbd "i") 'editutil-view-insert)
     (define-key view-mode-map (kbd "a") 'editutil-view-insert-at-next)
     (define-key view-mode-map (kbd "I") 'editutil-view-insert-at-bol)
     (define-key view-mode-map (kbd "A") 'editutil-view-insert-at-eol)))
```

## helm-editutil.el

#### `helm-editutil-git-ls-files`

`git ls-files` with helm interface

#### `helm-editutil-etags-select`

Select `etags` candidates with helm interface

#### `helm-editutil-yas-prompt`

Select `yasnippet` candidates with helm interface
