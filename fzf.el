;;; fzf.el --- A front-end for fzf. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2015 by Bailey Ling
;; Author: Bailey Ling
;; URL: https://github.com/bling/fzf.el
;; Filename: fzf.el
;; Description: A front-end for fzf
;; Created: 2015-09-18
;; Version: 0.0.2
;; Package-Requires: ((emacs "24.4"))
;; Keywords: fzf fuzzy search
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install:
;;
;; Autoloads will be set up automatically if you use package.el.
;;
;; Usage:
;;
;; - Generic:
;;    M-x fzf
;;    M-x fzf-directory
;;
;; - Buffer Navigation support:
;;    M-x fzf-switch-buffer
;;
;; - Find support:
;;    M-x fzf-find-file
;;    M-x fzf-find-file-in-dir
;;
;; - Recentf support
;;    M-x fzf-recentf
;;
;; - Grep support:
;;    M-x fzf-grep
;;    M-x fzf-grep-in-dir
;;    M-x fzf-grep-with-narrowing
;;    M-x fzf-grep-in-dir-with-narrowing
;;    M-x fzf-grep-dwim
;;    M-x fzf-grep-dwim-with-narrowing
;;
;; - VCS Support:
;;    M-x fzf-git
;;    M-x fzf-git-files
;;    M-x fzf-hg
;;    M-x fzf-hg-files
;;
;; - VCS/grep support:
;;    M-x fzf-git-grep
;;
;; - Projectile Support
;;    M-x fzf-projectile

;;
;; Naming conventions:
;;
;; - All symbols have a name that starts with 'fzf'
;; - All user options: `fzf/XXXX'
;; - All interactive and publicly available functions and variables: `fzf', `fzf-XXXX'
;; - All internal functions and variables: `fzf--XXXX'

;;; Code:

(require 'subr-x)

;; ---------------------------------------------------------------------------
;; Customization support

(defgroup fzf nil
  "Configuration options for fzf.el"
  :group 'convenience)

(defcustom fzf/window-height 15
  "The window height of the fzf buffer"
  :type 'integer
  :group 'fzf)

(defcustom fzf/executable "fzf"
  "The path to the fzf executable."
  :type 'string
  :group 'fzf)

(defcustom fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
  "Additional arguments to pass into fzf."
  :type 'string
  :group 'fzf)

(defcustom fzf/grep-command "grep -nrH"
  "Command used for `fzf-grep-*` functions.

Output of this command must be in the form <FILE>:<LINE NUMBER>:<LINE>.
See `fzf--action-find-file-with-line` for details on how output is parsed."
  :type 'string
  :group 'fzf)

(defcustom fzf/git-grep-args "-i --line-number %s"
  "Arguments to pass into git grep. %s is the search term placeholder"
  :type 'string
  :group 'fzf)

(defcustom fzf/position-bottom t
  "Set the position of the fzf window. Set to nil to position on top."
  :type 'boolean
  :group 'fzf)

(defconst fzf/buffer-name "*fzf*"
  "The name of the fzf buffer")

(defconst fzf/window-register :fzf-windows
  "A single character for fzf to save/restore the window
configuration.")

(defcustom fzf/directory-start nil
  "The path of the default start directory for fzf-directory."
  :type 'string
  :group 'fzf)

;; ---------------------------------------------------------------------------

;; Internal helper function
(defun fzf--grep-cmd (cmd args)
  (format (concat cmd " " args)
          (shell-quote-argument
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (read-from-minibuffer (concat cmd ": "))))))

;; Internal helper function
(defun fzf--exit-code-from-event (msg)
  "Return 0 if msg is finished, 1 if can parse, \"unknown\" if unknown"
  (cond
   ((string-match-p "finished" msg) "0")
   ((string-match-p "exited abnormally" msg) (car (last (split-string msg))))
   (t "unknown")))

;; Awkward internal, global variable to save the reference to the
;; 'term-handle-exit hook so it can be deleted
(defvar fzf--hook nil
  "Remembers 'term-handle-exit hook to allow its later deletion.")

;; Internal helper function
(defun fzf--close()
  "Cleanup hooks and process."
  ; Remove hook first so it doesn't trigger when process is killed
  (when fzf--hook (advice-remove 'term-handle-exit fzf--hook))
  (setq fzf--hook nil)

  ; Kill process so user isn't prompted
  (when (get-process fzf/executable)
    (delete-process (get-process fzf/executable)))

  ; Kill buffer and restore window
  (when (get-buffer fzf/buffer-name)
    (kill-buffer fzf/buffer-name)
    (jump-to-register fzf/window-register)))

;; Internal helper function
(defun fzf--pass-through (target _text _msg _process-name)
  "Pass-through validator: returns TARGET.
Ignores the other 3 arguments: _TEXT _MSG _PROCESS-NAME."
  target)

;; Internal helper function
(defun fzf--validate-filename (target text msg process-name)
  "Validate and return validated TARGET as valid file name.

Extra arguments used to provide contextual information in case of
error:
- TEXT:         string: complete text returned by FZF.
- MSG:          string: FZF termination message.
- PROCESS_NAME: string: name of used executable (fzf/executable)."
  (let ((orig-target target))
    ;; Sometimes the string returned by fzf has extraneous characters at the
    ;; end of the real/correct file name. Attempt to extract the correct
    ;; file name by stripping 1 character at a time from the end.
    (unless (file-exists-p target)
      (while (and  (not (string= "" target))
                   (not (file-exists-p target)))
        (setq target (substring target 0 -1))))
    ;; report any remaining error by message instead of exception since
    ;; we're in a handler we can't interrupt and provides a better trace.
    (when (or (string= "" target)
              (not (file-exists-p target)))
      (message "TERMINATING: process:[%s], msg:[%s]" process-name msg )
      (message "FZF PROBLEM: non existing file identified [%s]" orig-target)
      (message "FZF returned text: [%s]" text))
    ;; return potentially adjusted file name
    target))

(defvar fzf-target-validator  (function fzf--validate-filename)
  "FZF found target validator & filter function.

- Takes 4 arguments: (target text msg process-name)
- Returns target (a string).  If that target is valid,
  the function must return it unchanged.  If it was not valid
  and needed to be modified the function must return the modified
  target string.

The default is the file validator `fzf--validate-filename' used
as an example. When requiring something different let-bind the
variable to your own validator in your function that will
eventually execute the `fzf--start' and the
`fzf--after-term-handle-exit' which uses the validator.")

;; Internal helper function
(defun fzf--after-term-handle-exit (directory action target-validator)
  "Create and return lambda that handles the result of fzf.

The lambda must conform to `term-handle-exit':  i.e. accept 2 arguments:
1) a process name, 2) an output msg.

The lambda will call ACTION on the result of fzf if fzf exited successfully.
DIRECTORY, if non-nil, is prepended to the result of fzf."
  (lambda (process-name msg)
    (let* ((exit-code (fzf--exit-code-from-event msg))
           (text (buffer-substring-no-properties (point-min) (point-max)))
           (lines (split-string text "\n" t "\s*>\s+"))
           (target (string-trim
                    (concat
                     (when directory
                       (file-name-as-directory directory))
                     (car (last (butlast lines)))))))
      (setq target (funcall target-validator target text msg process-name))
      ;; Kill the fzf buffer and restore the previous window configuration.
      (kill-buffer fzf/buffer-name)
      (jump-to-register fzf/window-register)
      (message (format "FZF exited with code %s" exit-code))
      ;; Only do something with the result if fzf was successful.
      (when (string= "0" exit-code) (funcall action target)))
    ;; Remove this advice so as to not interfere with other usages of `term`.
    ;; This gets added back in `fzf--start`
    (advice-remove 'term-handle-exit
                   (fzf--after-term-handle-exit directory action target-validator))))

(defvar term-exec-hook)               ; prevent byte-compiler warning
(defvar term-suppress-hard-newline)   ; prevent byte-compiler warning

;; Internal helper function
(defun fzf--start (directory action &optional custom-args)
  "Launch `fzf/executable' in terminal, extract and act on selected item."
  (require 'term)

  ;; Clean up existing fzf, allowing multiple action types.
  (fzf--close)

  (unless (executable-find fzf/executable)
    (user-error "Can't find fzf/executable '%s'. Is it in your OS PATH?"
                fzf/executable))
  ;; launch process in an inferior terminal mapped in current window
  (window-configuration-to-register fzf/window-register)
  (advice-add 'term-handle-exit
              :after (fzf--after-term-handle-exit directory
                                                  action
                                                  fzf-target-validator))
  (let* ((term-exec-hook nil)
         (buf (get-buffer-create fzf/buffer-name))
         (min-height (min fzf/window-height (/ (window-height) 2)))
         (window-height (if fzf/position-bottom (- min-height) min-height))
         (args (or custom-args fzf/args))
         (sh-cmd (concat fzf/executable " " args)))
    (with-current-buffer buf
      (setq default-directory (or directory "")))
    (split-window-vertically window-height)
    (when fzf/position-bottom (other-window 1))
    (make-term fzf/executable "sh" nil "-c" sh-cmd)
    (switch-to-buffer buf)

    ;; Disable minor modes that interfere with rendering while fzf is running
    ;; TODO: provide ability to modify the set of actions in the user-option
    ;;       to allow compatibility with more minor modes instead of using
    ;;       this hard coded set.
    (and (fboundp 'turn-off-evil-mode) (turn-off-evil-mode))
    (when (bound-and-true-p linum-mode)
      (linum-mode 0))
    (when (bound-and-true-p visual-line-mode)
      (visual-line-mode 0))
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode 0))
    ;; disable various settings known to cause artifacts, see #1 for more details
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t)
    (setq-local show-trailing-whitespace nil)
    (setq-local display-line-numbers nil)
    (setq-local truncate-lines t)
    (face-remap-add-relative 'mode-line '(:box nil))

    (and (fboundp 'term-char-mode) (term-char-mode))
    ;; Remember the used terminal exit handler to allow its later removal.
    (setq fzf--hook (fzf--after-term-handle-exit directory action fzf-target-validator)
          mode-line-format (format "   FZF  %s" (or directory "")))))

;; Internal helper function
(defun fzf--action-find-file (target)
  (when (file-exists-p target)
    (find-file target)))

;; Internal helper function
(defun fzf--action-find-file-with-line (target)
  (fzf--action-find-file target)
  (let* ((parts (split-string target ":"))
         (f (expand-file-name (nth 0 parts))))
    (when (file-exists-p f)
      (find-file f)
      (goto-char (point-min))
      (forward-line (string-to-number (nth 1 parts))))))

;;;###autoload
(defun fzf ()
  "Starts a fzf session in the appropriate directory.

The selected directory is projectile's root directory if projectile
is used, otherwise the current working directory is used."
  (interactive)
  (let ((fzf-target-validator (function fzf--validate-filename)))
    (fzf--start (fzf--resolve-directory) #'fzf--action-find-file)))

;; Public utility
(defun fzf-with-command (command action &optional directory as-filter initq)
  "Run `fzf` on the output of COMMAND.

If COMMAND is nil, use the default `FZF_DEFAULT_COMMAND`.
Otherwise set `FZF_DEFAULT_COMMAND` to COMMAND.
COMMAND can be a sequence of piped commands to input to FZF.

ACTION is a function that takes a single argument, which is the
selected result from `fzf`.

DIRECTORY is the directory to start in.

If AS-FILTER is non-nil, use command as the narrowing filter instead of fzf,
with INITQ as the initial query, as explained here:
https://github.com/junegunn/fzf/blob/master/ADVANCED.md#using-fzf-as-interactive-ripgrep-launcher
E.g. If COMMAND is grep, use grep as a narrowing filter to interactively
reduce the search space, instead of using fzf to filter (but not narrow)."
  (if command
      (let
          ((process-environment (cons
                                 (concat "FZF_DEFAULT_COMMAND=" command "")
                                 process-environment))
           (args (if as-filter
                     (concat fzf/args
                             " --disabled"
                             " --query " initq
                             " --bind \"change:reload:sleep 0.1; "
                             fzf/grep-command
                             " {q} || true\"")
                   fzf/args)))
        (fzf--start directory action args))
    (fzf--start directory action)))

;; Public utility
(defun fzf-with-entries (entries action &optional directory)
  "Run `fzf` with the list ENTRIES as input.

ACTION is a function that takes a single argument, which is the
selected result from `fzf`.
If DIRECTORY is specified, fzf is run from that directory."
  (if entries
      (fzf-with-command
       (concat "echo \""
               (mapconcat (lambda (x) x) entries "\n") "\"")
       action directory)
    (user-error "No input entries specified")))

;;;###autoload
(defun fzf-directory ()
  "Starts a fzf session at the specified directory."
  (interactive)
  (let ((fzf-target-validator (function fzf--validate-filename))
        (d (read-directory-name "Directory: " fzf/directory-start)))
    (fzf--start d
                (lambda (x)
                  (let ((f (expand-file-name x d)))
                    (when (file-exists-p f)
                      (find-file f)))))))

;; Internal helper function
(defun fzf--resolve-directory (&optional directory)
  "Identify and return directory to perform fzf search.

Return DIRECTORY if specified, the projectile project root if
projectile is used otherwise return the current working directory.

Example usage:

  (defun fzf-example ()
    (fzf  (lambda (x) (print x))
          (fzf--resolve-directory directory)))"
  (cond
   (directory directory)
   ((fboundp 'projectile-project-root)
    (condition-case nil
        (projectile-project-root)
      (error default-directory)))
   (t default-directory)))

;;;###autoload
(defun fzf-switch-buffer ()
  "Switch buffer selecting them with fzf."
  (interactive)
  (let ((fzf-target-validator (function fzf--pass-through)))
    (fzf-with-entries
     (seq-filter
      (lambda (x) (not (string-prefix-p " " x)))
      (mapcar (function buffer-name) (buffer-list)))
     (lambda (x) (set-window-buffer nil x)))))

;;;###autoload
(defun fzf-find-file (&optional directory)
  "Find file in projectile project (if used), current or specified DIRECTORY."
  (interactive)
  (let ((fzf-target-validator (function fzf--validate-filename))
        (d (fzf--resolve-directory directory)))
    (fzf--start d
               (lambda (x)
                 (let ((f (expand-file-name x d)))
                   (when (file-exists-p f)
                     (find-file f)))))))

;;;###autoload
(defun fzf-find-file-in-dir (&optional directory)
  "Find file in specified DIRECTORY or prompt for it."
  (interactive)
  (let ((dir (or directory
                 (read-directory-name "Directory: " fzf/directory-start))))
    (fzf-find-file dir)))

;;;###autoload
(defun fzf-git-grep ()
  "Starts a fzf session based on git grep result. The input comes
   from the prompt or the selected region."
  (interactive)
  (let ((fzf-target-validator (function fzf--pass-through)))
    (fzf-with-command (fzf--grep-cmd "git grep" fzf/git-grep-args)
                      #'fzf--action-find-file-with-line
                      (locate-dominating-file default-directory ".git"))))

;;;###autoload
(defun fzf-recentf ()
  "Start a fzf session with the list of recently opened files."
  (interactive)
  (let ((fzf-target-validator (function fzf--pass-through)))
    (if (bound-and-true-p recentf-list)
        (fzf-with-entries recentf-list #'fzf--action-find-file)
      (user-error "No recently opened files.%s"
                  (if (boundp 'recentf-list)
                      ""
                    " recentf-mode is not active!")))))

;;;###autoload
(defun fzf-grep (&optional search directory as-filter)
  "Call `fzf/grep-command` on SEARCH.

If SEARCH is nil, read input interactively.
Grep in `fzf--resolve-directory` using DIRECTORY if provided.
If AS-FILTER is non-nil, use grep as the narrowing filter instead of fzf."
  (interactive)
  (let* ((fzf-target-validator (function fzf--pass-through))
         (dir (fzf--resolve-directory directory))
         (action #'fzf--action-find-file-with-line)
         (pattern (or search
                      (read-from-minibuffer (concat fzf/grep-command ": "))))
         (cmd (concat fzf/grep-command " " pattern)))
    (fzf-with-command cmd action dir as-filter pattern)))

;;;###autoload
(defun fzf-grep-in-dir (&optional directory as-filter)
  "Call `fzf-grep` in DIRECTORY.

If DIRECTORY is nil, read input interactively.
If AS-FILTER is non-nil, use grep as the narrowing filter instead of fzf."
  (interactive)
  (let ((dir (or directory
                 (read-directory-name "Directory: " fzf/directory-start))))
    (fzf-grep nil dir as-filter)))

;;;###autoload
(defun fzf-grep-with-narrowing ()
  "Call `fzf-grep` with grep as the narrowing filter."
  (interactive)
  (fzf-grep nil nil t))

;;;###autoload
(defun fzf-grep-in-dir-with-narrowing ()
  "Call `fzf-grep-in-dir` with grep as the narrowing filter."
  (interactive)
  (fzf-grep-in-dir nil t))

;;;###autoload
(defun fzf-grep-dwim ()
  "Call `fzf-grep` on `symbol-at-point`.

If `thing-at-point` is not a symbol, read input interactively."
  (interactive)
  (if (symbol-at-point)
      (fzf-grep (thing-at-point 'symbol))
    (fzf-grep)))

;;;###autoload
(defun fzf-grep-dwim-with-narrowing ()
  "Call `fzf-grep` on `symbol-at-point`, with grep as the narrowing filter.

If `thing-at-point` is not a symbol, read input interactively."
  (interactive)
  (if (symbol-at-point)
      (fzf-grep (thing-at-point 'symbol) nil t)
    (fzf-grep nil nil t)))

;; ---------------------------------------------------------------------------
;; VCS Support

;; Internal helper function
(defun fzf--vcs (vcs-name root-filename)
  "Run FZF in the VCS-NAME directory holding ROOT-FILENAME."
  (let ((fzf-target-validator (function fzf--validate-filename))
        (path (locate-dominating-file default-directory root-filename)))
    (if path
        (fzf--start path (function fzf--action-find-file))
      (user-error "Not inside a %s repository" vcs-name))))

(defun fzf--vcs-command (vcs-name root-filename command)
  "Run FZF specific COMMAND in the VCS-NAME directory holding ROOT-FILENAME."
  (let ((fzf-target-validator (function fzf--validate-filename))
        (path (locate-dominating-file default-directory root-filename)))
    (if path
        (fzf-with-command command (function fzf--action-find-file) path)
      (user-error "Not inside a %s repository" vcs-name))))

;;;###autoload
(defun fzf-git ()
  "Starts an fzf session at the root of the current git repo.

Search *all* files in the repository directory tree."
  (interactive)
  (fzf--vcs "Git" ".git"))

;;;###autoload
(defun fzf-git-files ()
  "Starts an fzf session for tracked files in the current Git repo.

Only search files that have been committed."
  (interactive)
  (fzf--vcs-command "Git" ".git" "git ls-files"))

;;;###autoload
(defun fzf-hg ()
  "Starts an fzf session at the root of the current hg repo.

Search *all* files in the repository directory tree."
  (interactive)
  (fzf--vcs "Mercurial" ".hg"))

;;;###autoload
(defun fzf-hg-files ()
  "Starts an fzf session for tracked files in the current Mercurial repo.

Only search files that have been committed."
  (interactive)
  (fzf--vcs-command "Mercurial" ".hg" "hg manifest"))

;; ---------------------------------------------------------------------------
;;;###autoload
(defun fzf-projectile ()
  "Starts an fzf session at the root of the current projectile project."
  (interactive)
  (require 'projectile)
  (if (fboundp 'projectile-project-root)
      (let ((fzf-target-validator (function fzf--validate-filename)))
        (fzf--start (or (projectile-project-root) default-directory)
                    #'fzf--action-find-file))
    (error "projectile-project-root is not bound")))

;; ---------------------------------------------------------------------------

;; test function
(defun fzf/test ()
  "Test ability to handle simple strings."
  (let ((fzf-target-validator (function fzf--pass-through)))
    (fzf-with-entries
     (list "a" "b" "c")
     (lambda (x) (print x)))))

;; ---------------------------------------------------------------------------
(provide 'fzf)

;;; fzf.el ends here
