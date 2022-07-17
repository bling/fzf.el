# fzf.el [![MELPA](https://melpa.org/packages/fzf-badge.svg)](https://melpa.org/#/fzf)

An Emacs front-end for [fzf][1].

![demo](https://cloud.githubusercontent.com/assets/306502/12380684/ca0a6648-bd46-11e5-9091-841b282874e4.gif)

# installation

`fzf.el` is available on [MELPA][2].

Below is an illustrative `use-package` configuration of `fzf.el` showing all
available customizations and their default values.

> **Note**: This package does not set default keybindings.

```lisp
(use-package fzf
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))
```

# usage

fzf.el comes with some example commands to try out

- `M-x fzf`
- `M-x fzf-directory`
- `M-x fzf-switch-buffer`
- `M-x fzf-find-file`
- `M-x fzf-find-file-in-dir`
- `M-x fzf-git`
- `M-x fzf-git-files`
- `M-x fzf-hg`
- `M-x fzf-projectile`
- `M-x fzf-git-grep`
- `M-x fzf-recentf`
- `M-x fzf-grep`
- `M-x fzf-grep-dwim`

But the real action is writing your own.

fzf.el exposes three functions:

- `fzf-with-entries (entries action &optional directory)`: run fzf, passing in an elisp list and running the function action with the user's selected results
- `fzf-with-command (command action &optional directory)`: run a shell command and directly pass to fzf. An optimization on top of `fzf-with-entries` so that the output does not have to be stored in emacs before sending to fzf anyway.

Using these functions, it's easy to define your own commands that use fzf:

```lisp
(defun fzf-example ()
  (fzf-with-entries
   (list "a" "b" "c")
   'print))
```

Or more exciting:

```lisp
(defun fzf-find-file (&optional directory)
  (interactive)
  (let ((d (fzf/resolve-directory directory)))
    (fzf
    (lambda (x)
        (let ((f (expand-file-name x d)))
        (when (file-exists-p f)
            (find-file f))))
    d)))
```

# license

GPL3

[1]: https://github.com/junegunn/fzf
[2]: https://melpa.org
