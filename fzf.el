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
;; M-x fzf
;; M-x fzf-with-command
;; M-x fzf-with-entries
;; M-x fzf-directory
;; M-x fzf-switch-buffer
;; M-x fzf-find-file
;; M-x fzf-find-file-in-dir
;; M-x fzf-git
;; M-x fzf-git-files
;; M-x fzf-hg
;; M-x fzf-projectile
;; M-x fzf-git-grep
;; M-x fzf-recentf
;; M-x fzf-grep
;; M-x fzf-grep-dwim
;;
;;; Code:

(require 'subr-x)

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
See `fzf/action-find-file-with-line` for details on how output is parsed."
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

(defun fzf/grep-cmd (cmd args)
  (format (concat cmd " " args)
          (shell-quote-argument
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (read-from-minibuffer (concat cmd ": "))))))

(defun fzf/exit-code-from-event (msg)
  "Return 0 if msg is finished, 1 if can parse, \"unknown\" if unknown"
  (cond
   ((string-match-p "finished" msg) "0")
   ((string-match-p "exited abnormally" msg) (car (last (split-string msg))))
   (t "unknown")
  )
)

; Awkward internal, global variable to save the reference to the 'term-handle-exit hook so it can be
; deleted
(defvar fzf-hook nil)

(defun fzf-close()
  (interactive)

  ; Remove hook first so it doesn't trigger when process is killed
  (when fzf-hook (advice-remove 'term-handle-exit fzf-hook))
  (setq fzf-hook nil)

  ; Kill process so user isn't prompted
  (when (get-process fzf/executable)
    (delete-process (get-process fzf/executable)))

  ; Kill buffer and restore window
  (when (get-buffer fzf/buffer-name)
    (kill-buffer fzf/buffer-name)
    (jump-to-register fzf/window-register))
)

(defun fzf/after-term-handle-exit (directory action)
  "Create a lambda to handle the result of fzf.

The lambda must conform to `term-handle-exit`, i.e. accept two arguments -
a process name, and output msg.
The lambda will call ACTION on the result of fzf if fzf exited successfully.
If DIRECTORY is provided, it is prepended to the result of fzf."
  (lambda (_ msg)
    (let* ((exit-code (fzf/exit-code-from-event msg))
           (text (buffer-substring-no-properties (point-min) (point-max)))
           (lines (split-string text "\n" t "\s*>\s+"))
           (target (concat
                    (if directory
                        (file-name-as-directory directory))
                    (car (last (butlast lines))))))
      ;; Kill the fzf buffer and restore the previous window configuration.
      (kill-buffer fzf/buffer-name)
      (jump-to-register fzf/window-register)
      (message (format "FZF exited with code %s" exit-code))
      ;; Only do something with the result if fzf was successful.
      (when (string= "0" exit-code) (funcall action target)))
    ;; Remove this advice so as to not interfere with other usages of `term`.
    ;; This gets added back in `fzf/start`
    (advice-remove 'term-handle-exit (fzf/after-term-handle-exit directory action))))

(defun fzf/start (directory action &optional custom-args)
  (require 'term)

  ; Clean up existing fzf
  (fzf-close)

  (window-configuration-to-register fzf/window-register)
  (advice-add 'term-handle-exit :after (fzf/after-term-handle-exit directory action))
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
    (and (fboundp #'turn-off-evil-mode) (turn-off-evil-mode))
    (linum-mode 0)
    (visual-line-mode 0)

    ;; disable various settings known to cause artifacts, see #1 for more details
    (setq-local scroll-margin 0
                scroll-conservatively 0
                term-suppress-hard-newline t
                show-trailing-whitespace nil
                display-line-numbers nil
                truncate-lines t)
    (face-remap-add-relative 'mode-line '(:box nil))

    (term-char-mode)
    (setq fzf-hook (fzf/after-term-handle-exit directory action)
          mode-line-format (format "   FZF  %s" (or directory "")))))


(defun fzf/action-find-file (target)
  (when (file-exists-p target)
    (find-file target))
)

(defun fzf/action-find-file-with-line (target)
  (fzf/action-find-file target)
  (let* ((parts (split-string target ":"))
         (f (expand-file-name (nth 0 parts))))
    (when (file-exists-p f)
      (find-file f)
      (goto-line (string-to-number (nth 1 parts))))
  )
)

;;;###autoload
(defun fzf ()
  "Starts a fzf session."
  (interactive)
  (fzf/start (fzf/resolve-directory) #'fzf/action-find-file)
)

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
https://github.com/junegunn/fzf/blob/master/ADVANCED.md#using-fzf-as-interative-ripgrep-launcher
E.g. If COMMAND is grep, use grep as a narrowing filter to interactively
reduce the search space, instead of using fzf to filter (but not narrow)."
  (interactive)
  (if command
      (let
          ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=" command "") process-environment))
           (args (if as-filter
                     (concat fzf/args
                             " --disabled"
                             " --query " initq
                             " --bind \"change:reload:sleep 0.1; "
                             fzf/grep-command
                             " {q} || true\"")
                   fzf/args)))
        (fzf/start directory action args))
    (fzf/start directory action)))

;;;###autoload
(defun fzf-with-entries (entries action &optional directory)
  "Run `fzf` with the list ENTRIES as input.

ACTION is a function that takes a single argument, which is the
selected result from `fzf`. DIRECTORY is the directory to start in"
  (interactive)
  (if entries
    (fzf-with-command (concat "echo \"" (mapconcat (lambda (x) x) entries "\n") "\"") action directory)
    (message "FZF not started because contents nil")
  )
)

;;;###autoload
(defun fzf-directory ()
  "Starts a fzf session at the specified directory."
  (interactive)
  (let ((d (read-directory-name "Directory: " fzf/directory-start)))
    (fzf/start d
               (lambda (x)
                 (let ((f (expand-file-name x d)))
                   (when (file-exists-p f)
                     (find-file f)))))
  )
)

(defun fzf/resolve-directory (&optional directory)
  ; An example function to resolve a directory in a user command, before passing it to fzf. Here if
  ; directory is undefined, attempt to use the projectile root. Users can define their own as
  ; desired
  ;
  ; Example usage:
  ; (defun fzf-example ()
  ;   (fzf
  ;    (lambda (x) (print x))
  ;    (fzf/resolve-directory directory)))
  (cond
   (directory directory)
   ((fboundp #'projectile-project-root) (condition-case err (projectile-project-root) (error default-directory)))
   (t default-directory)
  )
)


;;;###autoload
(defun fzf-switch-buffer ()
  (interactive)
  (fzf-with-entries
   (seq-filter
    (lambda (x) (not (string-prefix-p " " x)))
    (mapcar (function buffer-name) (buffer-list))
   )
    (lambda (x) (set-window-buffer nil x))
  )
)

;;;###autoload
(defun fzf-find-file (&optional directory)
  (interactive)
  (let ((d (fzf/resolve-directory directory)))
    (fzf/start d
               (lambda (x)
                 (let ((f (expand-file-name x d)))
                   (when (file-exists-p f)
                     (find-file f)))))
  )
)

;;;###autoload
(defun fzf-find-file-in-dir (directory)
  (interactive "sDirectory: ")
  (fzf-find-file directory)
)

;;;###autoload
(defun fzf-git-grep ()
  "Starts a fzf session based on git grep result. The input comes
   from the prompt or the selected region."
  (interactive)
  (fzf-with-command (fzf/grep-cmd "git grep" fzf/git-grep-args)
                    #'fzf/action-find-file-with-line
                    (locate-dominating-file default-directory ".git")))

;;;###autoload
(defun fzf-recentf ()
  (interactive)
  (fzf-with-entries recentf-list #'fzf/action-find-file)
)

;;;###autoload
(defun fzf-grep (&optional search directory as-filter)
  "Call `fzf/grep-command` on SEARCH.

If SEARCH is nil, read input interactively.
Grep in `fzf/resolve-directory` using DIRECTORY if provided.
If AS-FILTER is non-nil, use grep as the narrowing filter instead of fzf."
  (interactive)
  (let* ((dir (fzf/resolve-directory directory))
         (action #'fzf/action-find-file-with-line)
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

;;;###autoload
(defun fzf-git ()
  "Starts an fzf session at the root of the current git project."
  (interactive)
  (let ((path (locate-dominating-file default-directory ".git")))
    (if path
        (fzf/start path #'fzf/action-find-file)
      (user-error "Not inside a Git repository"))))

;;;###autoload
(defun fzf-hg ()
  "Starts an fzf session at the root of the current hg project."
  (interactive)
  (let ((path (locate-dominating-file default-directory ".hg")))
    (if path
        (fzf/start path #'fzf/action-find-file)
      (user-error "Not inside a .hg repository"))))

;;;###autoload
(defun fzf-git-files ()
  "Starts an fzf session for tracked files in the current git project."
  (interactive)
  (let ((path (locate-dominating-file default-directory ".git")))
    (if path
        (fzf-with-command "git ls-files" #'fzf/action-find-file path)
      (user-error "Not inside a Git repository"))))

;;;###autoload
(defun fzf-projectile ()
  "Starts an fzf session at the root of the current projectile project."
  (interactive)
  (require 'projectile)
  (fzf/start (or (projectile-project-root) default-directory) #'fzf/action-find-file))

(defun fzf/test ()
  (fzf-with-entries
   (list "a" "b" "c")
   (lambda (x) (print x))))

(provide 'fzf)

;;; fzf.el ends here
