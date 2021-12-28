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

(defcustom fzf/git-grep-args "-i --line-number %s"
  "Arguments to pass into git grep. %s is the search term placeholder"
  :type 'string
  :group 'fzf)

(defcustom fzf/position-bottom t
  "Set the position of the fzf window. Set to nil to position on top."
  :type 'bool
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
(setq fzf-hook nil)

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
  "Construct function to run after term exits"
  (lambda (process-name msg)
    (let ((exit-code (fzf/exit-code-from-event msg)))
      (message (format "exit code %s" exit-code))
      (if (string= "0" exit-code)
        ; Run action on result of fzf if exit code is 0
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                (lines (split-string text "\n" t "\s*>\s+"))
                (target (car (last (butlast lines 1))))
                (target-full (concat
                              (if directory
                                  (file-name-as-directory directory))
                              target))
            )
            ; Kill fzf and restore windows
            ; Killing has to happen before applying the action so functions like swaping the buffer
            ; will apply to the right window
            (kill-buffer fzf/buffer-name)
            (jump-to-register fzf/window-register)

            (message (format "target %s" target-full))
            (funcall action target-full)
        )
        ; Kill fzf and restore windows
        (kill-buffer fzf/buffer-name)
        (jump-to-register fzf/window-register)
        (message (format "FZF exited with code %s" exit-code))
      )
    )

    ; Clean up advice handler by calling remove with same lambda
    (advice-remove 'term-handle-exit (fzf/after-term-handle-exit directory action))
  )
)

(defun fzf/start (directory action)
  (require 'term)

  ; Clean up existing fzf
  (fzf-close)

  (window-configuration-to-register fzf/window-register)
  (advice-add 'term-handle-exit :after (fzf/after-term-handle-exit directory action))
  (let* ((term-exec-hook nil)
         (buf (get-buffer-create fzf/buffer-name))
         (min-height (min fzf/window-height (/ (window-height) 2)))
         (window-height (if fzf/position-bottom (- min-height) min-height))
         (sh-cmd (concat fzf/executable " " fzf/args)))
    (with-current-buffer buf
      (setq default-directory (if directory directory "")))
    (split-window-vertically window-height)
    (when fzf/position-bottom (other-window 1))
    (make-term fzf/executable "sh" nil "-c" sh-cmd)
    (switch-to-buffer buf)
    (and (fboundp #'turn-off-evil-mode) (turn-off-evil-mode))
    (linum-mode 0)
    (visual-line-mode 0)

    (setq fzf-hook (fzf/after-term-handle-exit directory action))

    ;; disable various settings known to cause artifacts, see #1 for more details
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t) ;for paths wider than the window
    (setq-local show-trailing-whitespace nil)
    (setq-local display-line-numbers nil)
    (setq-local truncate-lines t)
    (face-remap-add-relative 'mode-line '(:box nil))

    (term-char-mode)
    (setq mode-line-format (format "   FZF  %s" directory))))

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

(defun fzf-with-command (command action &optional directory)
  "FZF_DEFAULT_COMMAND is set to `command'. `action' is a
function that takes a single argument which is the selected
result from `fzf'. `directory' is the directory to start in."
  ; Set FZF_DEFAULT_COMMAND and then call fzf/start. If command is nil, leave FZF_DEFAULT_COMMAND
  ; alone and use the users normal command
  ;
  ; For some inputs it would be much more efficient to directly pass the output to FZF rather than
  ; capture in emacs, then pass to FZF. This function takes a command and uses/abuses
  ; FZF_DEFAULT_COMMAND to run and pass the output to FZF.
  (interactive)
  (if command
    (let
      ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=" command "") process-environment)))
      (fzf/start directory action))
    (fzf/start directory action)
  )
)

;;;###autoload
(defun fzf-with-entries (entries action &optional directory)
  "`entries' is a list of strings that is piped into `fzf' as a
source. `action' is a function that takes a single argument which
is the selected result from `fzf'. `directory' is the directory
to start in."
  ; FZF will read from stdin only if it detects stdin is not a tty, which amounts to something being
  ; piped in. Unfortunately the emacs term-exec code runs /bin/sh -c exec "command", so it cannot
  ; take in a pipeline of shell commands. Like bling/fzf.el/pull/20, abuse the FZF_DEFAULT_COMMAND,
  ; environment var
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
(defun fzf-grep (search &optional directory)
  (interactive "sGrep: ")
  (fzf-with-command (format "grep -rHn %s ." search)
                    #'fzf/action-find-file-with-line
                    (or directory default-directory))
)

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
