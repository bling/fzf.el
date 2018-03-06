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
;; M-x fzf-directory
;; M-x fzf-git
;; M-x fzf-hg
;; M-x fzf-projectile
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

(defcustom fzf/args "-x --color bw --print-query"
  "Additional arguments to pass into fzf."
  :type 'string
  :group 'fzf)

(defcustom fzf/position-bottom t
  "Set the position of the fzf window. Set to nil to position on top."
  :type 'bool
  :group 'fzf)

(defcustom fzf/directory-start nil
  "The path of the default start directory for fzf-directory."
  :type 'string
  :group 'fzf)

(defun fzf/exit-code-from-event (msg)
  "Return 0 if msg is finished, 1 if can parse, -1 if unknown"
  (cond
   ((string-match-p "finished" msg) "0")
   ((string-match-p "exited abnormally" msg) (car (last (split-string msg))))
   "-1"
   )
)

; Awkward internal, global variable to save the reference to the 'term-handle-exit hook so it can be
; deleted
(setq fzf-hook nil)

(defun fzf-close()
  (interactive)

  ; Remove hook first so it doesn't trigger when process is killed
  (when fzf-hook (advice-remove 'term-handle-exit fzf-hook))
  ; Delete all hooks on 'term-handle-exit. Potentially unnecessary
  (advice-mapc (lambda (advice _props) (advice-remove 'term-handle-exit advice)) 'term-handle-exit)
  (setq fzf-hook nil)

  ; Kill process so user isn't prompted
  (when (get-process "fzf")
    (delete-process (get-process "fzf")))

  ; Kill buffer and restore window
  (when (get-buffer "*fzf*")
    (kill-buffer "*fzf*")
    (jump-to-register :fzf-windows))
)


(defun fzf/after-term-handle-exit (action)
  "Construct function to run after term exits"
  (lambda (process-name msg)
    (let ((exit-code (fzf/exit-code-from-event msg)))
      (message (format "exit code %s" exit-code))
      (if (string= "0" exit-code)
        ; Run action on result of fzf if exit code is 0
        (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                (lines (split-string text "\n" t "\s*>\s+"))
                (target (car (last (butlast lines 1))))
            )
            ; Kill fzf and restore windows
            ; Killing has to happen before applying the action so functions like swaping the buffer
            ; will apply to the right window
            (kill-buffer "*fzf*")
            (jump-to-register :fzf-windows)

            (message (format "target %s" target))
            (funcall action target)
        )
        ; Kill fzf and restore windows
        (kill-buffer "*fzf*")
        (jump-to-register :fzf-windows)
        (message (format "FZF exited with code %s" exit-code))
      )
    )

    ; Clean up advice handler by calling remove with same lambda
    (advice-remove 'term-handle-exit (fzf/after-term-handle-exit action))
  )
)

(defun fzf/start (directory action)
  (require 'term)

  ; Clean up existing fzf
  (fzf-close)

  (window-configuration-to-register :fzf-windows)
  (advice-add
   'term-handle-exit
   :after
   (fzf/after-term-handle-exit action))
  (let* ((buf (get-buffer-create "*fzf*"))
         (min-height (min fzf/window-height (/ (window-height) 2)))
         (window-height (if fzf/position-bottom (- min-height) min-height))
         (window-system-args (when window-system " --margin=1,0"))
         (fzf-args (concat fzf/args window-system-args)))
    (with-current-buffer buf
      (setq default-directory (if directory directory "")))
    (split-window-vertically window-height)
    (when fzf/position-bottom (other-window 1))
    (apply 'make-term "fzf" fzf/executable nil (split-string fzf-args))
    (switch-to-buffer buf)
    (linum-mode 0)
    (visual-line-mode 0)

    (setq fzf-hook (fzf/after-term-handle-exit action))

    ;; disable various settings known to cause artifacts, see #1 for more details
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t) ;for paths wider than the window
    (setq-local show-trailing-whitespace nil)
    (setq-local truncate-lines t)
    (face-remap-add-relative 'mode-line '(:box nil))

    (term-char-mode)
    (setq mode-line-format (format "   FZF  %s" directory))))

(defun fzf-with-command (command action &optional directory)
  ; Set FZF_DEFAULT_COMMAND and then call fzf/start. If command is nil, leave FZF_DEFAULT_COMMAND
  ; alone and use the users normal command
  ;
  ; For some inputs it would be much more efficient to directly pass the output to FZF rather than
  ; capture in emacs, then pass to FZF. This function takes a command and uses/abuses
  ; FZF_DEFAULT_COMMAND to run and pass the output to FZF
  (interactive)
  (if command
    (let
      ((process-environment (cons (concat "FZF_DEFAULT_COMMAND=" command "") process-environment)))
      (fzf/start directory action))
    (fzf/start directory action)
  )
)

(defun fzf-with-entries (entries action &optional directory)
  "`entries' is a list of strings that is piped into `fzf' as a source."
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

(defun fzf-base (action &optional directory)
  "Run FZF without setting default command"
  (interactive)
  (fzf-with-command "" action directory)
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
   (t "")
  )
)

;; Prebuilt user commands
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

(defun fzf-find-file (&optional directory)
  (interactive)
  (let ((d (fzf/resolve-directory directory)))
    (fzf
    (lambda (x)
        (let ((f (expand-file-name x d)))
        (when (file-exists-p f)
            (find-file f))))
    d
    )
  )
)

(defun fzf-find-file-in-dir (directory)
  (interactive "sDirectory: ")
  (fzf-find-file directory)
)

(defun fzf-recentf ()
  (interactive)
  (fzf-with-entries recentf-list
    (lambda (f) (when (file-exists-p f) (find-file f))))
)

(defun fzf-grep (search &optional directory)
  (interactive "sGrep: ")
  (let ((d (fzf/resolve-directory directory)))
    (fzf-with-command
    (format "grep -rHn %s ." search)
    (lambda (x)
      (let* ((parts (split-string x ":"))
             (f (expand-file-name (nth 0 parts) d)))
        (when (file-exists-p f)
          (find-file f)
          (goto-line (string-to-number (nth 1 parts))))))
    d)))

(defun fzf-test ()
  (fzf-with-entries
   (list "a" "b" "c")
   (lambda (x) (print x))))

(provide 'fzf)

;;; fzf.el ends here
