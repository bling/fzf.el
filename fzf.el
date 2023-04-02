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
;; Usage
;; ------
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


;; File name and line number extraction logic
;; ------------------------------------------
;;
;; The extraction logic uses Emacs Lisp regular expressions that identify
;; group matches for the file name and the line number. By default all
;; commands that use grep (`fzf-grep' and all others that call it) use the
;; `fzf--file-lnum-regexp' and the `fzf-hg' uses `fzf--file-rnum-lnum-regexp'.
;;
;; The extraction code is operation is performed by the
;; `fzf--action-find-file-with-line' internal function, which gets the regular
;; expression *and* the numbers of the file name and line number extraction
;; groups from a let-bound internal variable named `fzf--extractor-list'
;; holding a list described by the variable docstring.
;;
;; All is fine for the current grep command settings.
;;
;; However if you need to use another grep program or sets of arguments that
;; produce output in a format that differ from the two supported regexps
;; listed above, you *can* override the extraction list by providing your own
;; 3-elements (regexp file-group line-group) list in a let-bound variable
;; named `fzf-extractor-list'.  Notice that the name has only one '-' after
;; 'fzf', not two.  The `fzf-grep', `fzf-git-grep' and `fzf-hg-grep' use that
;; variable instead of the default one if it is bound.


;;
;; Naming conventions
;; -------------------
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
  "The name of the fzf executable.

If the executable file is NOT accessible through your PATH,
write the absolute path of the executable to use."
  :type 'string
  :group 'fzf)

(defcustom fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
  "Additional arguments to pass into fzf."
  :type 'string
  :group 'fzf)

(defcustom fzf/args-for-preview "--preview 'cat {}'"
  "Extra arguments to pass to fzf to preview selected file.

The extra arguments identified by this user-option are appended
to the fzf command line used by the `fzf', `fzf-directory',
`fzf-projectile` and `fzf-recentf' Emacs commands when those are
invoked with prefix arguments, for example by typing 'C-u M-x fzf'.

Several choices are available:

- 1: Show file's content with \"--preview 'cat {}'\" by default.
- 2: Show information about the file with \"--preview 'file {}'\" by default.
- 3: Show ls output for the file.
- 4: Show word count output on the file.
- 5: any other argument.

Notes:

- You could add extra arguments here that would do more than only
  adding preview capability to fzf.  These will be applied to `fzf' and
  `fzf-directory' when you invoke them with a prefix argument (eg. C-u).

- You can change the string of any of the available choices,
  allowing you to quickly select from one or another using Emacs
  customization mechanism.n"
  :type '(choice
          (string :tag "Show content of file" "--preview 'cat {}'")
          (string :tag "Show type of file"    "--preview 'file {}'")
          (string :tag "Show ls on file"      "--preview 'ls -l {}'")
          (string :tag "Show wc on file"      "--preview 'wc {}'")
          (string :tag "other" ""))
  :group 'fzf)

(defcustom fzf/grep-command "grep -nrH"
  "Recursive grep-like command line used for `fzf-grep-*` functions.

Identifies a command line that searches in a directory tree and
produces a set of matching line that must all follow the format
identified by the `fzf--file-lnum-regexp' regular expression,
which is: <FILE>:<LINE NUMBER>:<text>.

The following options are available:

- 1: use grep with 'grep -rnH' by default,
- 2: use ripgrep with 'rg --no-heading --color never --line-number' by
    default,
- 3: something else, entirely specified in the option.
     Nothing is specified here; You must fill the command line.

The command line for each option is maintained independently and
can be modified.

The first one, grep, is the default.

- When using grep the user must identify the search pattern and the glob pattern
  for the files to search inside the tree.
- When using ripgrep, the user does not need to describe the files searched
  but can identify them in several ways:

  - using the -g, --glob GLOB option to specify the directories.
  - using the -t, --type TYPE option to specify the type of files to
    search.
  - There are many other options, consult rg man page for more information.
  - You can also append the trailing '--' to ensure that the entered string
    is not interpreted as a ripgrep command line switch but that will prevent
    the user to add more ripgrep options.

See `fzf--action-find-file-with-line' for details on how output is parsed."
  :type '(choice
          (string :tag "Use grep with" "grep -nrH")
          (string :tag "Use ripgrep with" "rg --no-heading --color never -n")
          (string :tag "Something else" "" ))
  :link '(url-link :tag "ripgrep @ GitHub"
                   "https://github.com/BurntSushi/ripgrep")
  :group 'fzf)

(defcustom fzf/git-grep-args "-i --line-number %s"
  "Arguments to pass into git grep. %s is the search term placeholder."
  :type 'string
  :group 'fzf)

(defcustom fzf/hg-grep-args "-i --line-number %s"
  "Arguments to pass into hg grep. %s is the search term placeholder."
  :type 'string
  :group 'fzf)

(defcustom fzf/grep-file-pattern "*"
  "Default file pattern used for fzf-grep operations.

Used only when `fzf/grep-command' is using grep."
  ;; TO-DO make this more flexible allowing the user to specify a regexp
  ;; that search into `fzf/grep-command' to determine if a default file
  ;; pattern should be used, or some other mechanism.
  :type 'string
  :group 'fzf)

(defcustom fzf/prompt-history-per-major-mode t
  "If non-nil fzf maintain prompt history for each major mode.

If nil, fzf prompts have the same history in all modes."
  :type 'boolean
  :safe #'booleanp
  :group 'fzf)

(defcustom fzf/position-bottom t
  "Set the position of the fzf window. Set to nil to position on top."
  :type 'boolean
  :safe #'booleanp
  :group 'fzf)

(defconst fzf/buffer-name "*fzf*"
  "The name of the fzf buffer")

(defcustom fzf/directory-start nil
  "The path of the default start directory for fzf-directory."
  :type 'string
  :group 'fzf)

;; ---------------------------------------------------------------------------
;; Public variables
;; ----------------

;; The `fzf-extractor-list' variable is not bound by default.  If you need it
;; let-bind it inside your code.  See the extraction logic comments in the
;; commentary section above.

;; The `fzf-target-validator' variable is not bound by default.  Bind it
;; inside your function calling on of the fzf commands only if you need to use
;; a different validator function than what the fzf command use.  The fzf
;; commands use either `fzf--pass-through' or `fzf--validate-filename'.  You
;; can override them by let-binding the `fzf-target-validator' variable to the
;; function you want to use.

;; ---------------------------------------------------------------------------
;; Internal variables
;; ------------------

(defconst fzf--window-register :fzf-windows
  "Internal register used by fzf to save/restore the window configuration.")

(defconst fzf--file-lnum-regexp "^\\(.*\\):\\([0-9]+\\):"
  "Regular expression to extract line & line number from FILE:LINE:text.

Match:
- group 1: file name,
- group 2: line number.

Used for the following search commands:

- grep -rnH
- git grep
- rg --no-heading --color never")

(defconst fzf--file-rnum-lnum-regexp "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):"
  "Regular expression to extract line & line number from FILE:REV:LINE:text.

Used for the following search commands:

- hg grep --line-number --all
- hg grep --line-number --all
- hg grep --rev N1 --rev N2

Match:
- group 1: file name,
- group 2: revision number,
- group 3: line number.")

(defvar fzf--target-validator  (function fzf--validate-filename)
  "Internal FZF found target validator & filter function.

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

(defvar fzf--extractor-list nil
  "Internal list of (regexp file-group line-group) for data extraction.

The list members are:

- regexp:     string:  regular expression to extract file name & line number.
- file-group: integer: match group number in regexp to extract file name.
- line-group: integer: match group number in regexp to extract line number.

Both integers are base-1 integer: the first group of a match is 1 (not 0).

Used by `fzf--action-find-file-with-line' top-level callers only:
`fzf-grep', `fzf-git-grep', `fzf-hg-grep' or any other functions
that users might write.

Users of that function should let-bind it to a list holding the
values `fzf--action-find-file-with-line' must use to perform data
extraction for their operation.

Please ensure that the regexp supports all Unix file names,
including file names with embedded colons.

See `fzf--file-lnum-regexp' and `fzf--file-rnum-lnum-regexp' as examples.")

;; ---------------------------------------------------------------------------
;; Internal helper function
(defun fzf--read-for (operation prompt)
  "Prompt in minibuffer for OPERATION with PROMPT and history. Return entry.

Each major mode has it's own history"
  (let ((history-symbol (intern (format
                                 "fzf--%s-prompt-history-for-%s"
                                 operation
                                 (if fzf/prompt-history-per-major-mode
                                     major-mode
                                   'all)))))
    (read-from-minibuffer prompt nil nil nil history-symbol)))

;; Internal helper function
(defun fzf---grep-file-pattern-for (&optional file-pattern forced)
  "Return (potentially replaced/removed) FILE-PATTERN for tool.

Return nil when currently not using grep (as identified by `fzf/grep-command'
user-option unless FORCED is non-nil."
  (when (or forced (string-match-p "^grep " fzf/grep-command))
    (concat  " " (or file-pattern fzf/grep-file-pattern))))

;; Internal helper function
(defun fzf--grep-file-pattern (prompt-user)
  "Return the grep file pattern to use.

Prompt if WITH-PROMPT is non nil otherwise use the default
file pattern specified by `fzf/grep-file-pattern'."
  (if prompt-user
      (fzf--read-for 'grep
                     (format "%sile pattern: "
                             (cond
                              ((string-match-p "^grep " fzf/grep-command)
                               "grep f")
                              ((string-match-p "^rg " fzf/grep-command)
                               "rg f")
                              (t "F"))))
    (fzf---grep-file-pattern-for)))

;; Internal helper function
(defun fzf--text-only-for (region-begin region-end)
  "Filter away display buffer text attributes and ANSI color escape sequences.

The ANSI color sequences are filtered when Emacs runs in termcap mode."
  (unless (display-graphic-p)
    (require 'ansi-color)
    (ansi-color-filter-region region-begin region-end))
  (buffer-substring-no-properties region-begin region-end))

;; Internal helper function
(defun fzf--grep-cmd (cmd args)
  (format (concat cmd " " args)
          (shell-quote-argument
           (if (region-active-p)
               (fzf--text-only-for (region-beginning)
                                   (region-end))
             (fzf--read-for (concat "-" cmd) ; prevent name clash with symbols
                            (concat cmd ": "))))))

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
  ;; Remove hook first so it doesn't trigger when process is killed
  (when fzf--hook (advice-remove 'term-handle-exit fzf--hook))
  (setq fzf--hook nil)
  ;; Kill process so user isn't prompted
  (let ((process-name (file-name-nondirectory fzf/executable)))
    (when (get-process process-name)
      (delete-process (get-process process-name))))

  ;; Kill buffer and restore window
  (when (get-buffer fzf/buffer-name)
    (kill-buffer fzf/buffer-name)
    (jump-to-register fzf--window-register)))

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

;; Internal helper function
(defun fzf--after-term-handle-exit (directory action target-validator extractor-list)
  "Create and return lambda that handles the result of fzf.

The lambda must conform to `term-handle-exit':  i.e. accept 2 arguments:
1) a process name, 2) an output msg.

The lambda will call ACTION on the result of fzf if fzf exited successfully.
DIRECTORY, if non-nil, is pre-pended to the result of fzf.

The returned lambda requires extra context information:

- TARGET-VALIDATOR: a function that validates the file name extracted from fzf
  output.  See `fzf--validate-filename' for an example.
- EXTRACTOR-LIST: a (regexp, file-group line-group) list similar to
  `fzf--extractor-list'.  This identifies the regular expression to extract
  the file name and line number from the grep-like program output, when used."
  (lambda (process-name msg)
    (let* ((exit-code (fzf--exit-code-from-event msg))
           (text (fzf--text-only-for (point-min) (point-max)))
           (lines (split-string text "\n" t "\s*>\s+"))
           (target (string-trim
                    (concat
                     (when directory
                       (file-name-as-directory directory))
                     (car (last (butlast lines)))))))
      (setq target (funcall target-validator target text msg process-name))
      ;; Kill the fzf buffer and restore the previous window configuration.
      (kill-buffer fzf/buffer-name)
      (jump-to-register fzf--window-register)
      (if (string= exit-code "0")
          (message "FZF selection: %s" target)
        (message "FZF error: %s" exit-code))
      ;; Extract file/line from fzf only if fzf was successful.
      (when (string= "0" exit-code)
        ;; Re-Establish the fzf--extractor-list required by original caller
        ;; command.
        (let ((fzf--extractor-list extractor-list))
          (funcall action target))))
    ;; Remove this advice so as to not interfere with other usages of `term`.
    ;; This gets added back in `fzf--start`
    (advice-remove 'term-handle-exit
                   (fzf--after-term-handle-exit directory
                                                action
                                                target-validator
                                                extractor-list))))

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
  (window-configuration-to-register fzf--window-register)
  (advice-add 'term-handle-exit
              :after (fzf--after-term-handle-exit directory
                                                  action
                                                  fzf--target-validator
                                                  fzf--extractor-list))
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
    (make-term (file-name-nondirectory fzf/executable)
               "sh" nil "-c" sh-cmd)
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
    (setq fzf--hook (fzf--after-term-handle-exit directory
                                                 action
                                                 fzf--target-validator
                                                 fzf--extractor-list)
          mode-line-format (format "   FZF  %s" (or directory "")))))

;; Internal helper function
(defun fzf--action-find-file (target)
  (when (file-exists-p target)
    (find-file target)))

;; Internal helper function
(defun fzf--action-find-file-with-line (target)
  (let ((regexp     (nth 0 fzf--extractor-list))
        (file-group (nth 1 fzf--extractor-list))
        (line-group (nth 2 fzf--extractor-list)))
    (if (string-match regexp target)
        (let ((fname (expand-file-name (match-string file-group target)))
              (line  (string-to-number (match-string line-group target))))
          (if (file-exists-p fname)
              (progn
                (find-file fname)
                (goto-char (point-min))
                (forward-line (1- line)))
            (error "Found non-existing file: '%s'" fname)))
      (error "Nothing matching! Is regexp ok?: '%s'" regexp))))

;; Internal helper function
(defun fzf--use-validator (validator)
  "Return  fzf-target-validator if bound, otherwise VALIDATOR.

The fzf-target-validator variable must be bound to a function that performs
the file name validation.  See `fzf--validate-filename' and
`fzf--pass-through' as examples of valid validators."
  (if (bound-and-true-p fzf-target-validator)
      fzf-target-validator
    validator))

;; Internal helper function
(defun fzf--use-extractor (extractor)
  "Return fzf-extractor-list if bound, otherwise EXTRACTOR."
  (if (bound-and-true-p fzf-extractor-list)
      fzf-extractor-list
    extractor))

;; ---------------------------------------------------------------------------

;;;###autoload
(defun fzf (&optional with-preview)
  "Starts a fzf session in the appropriate directory.

By default this process the current working directory unless this is inside a
Projectile project in which case the root directory of the Projectile project
is used.

With optional prefix WITH-PREVIEW the currently selected file
content or attribute is shown using the preview command
identified by the `fzf/args-for-preview' user-option.  By default
that shows the file content with cat, but that can be customized
to use other mechanisms."
  (interactive "P")
  (let ((fzf/args (if with-preview
                      (concat fzf/args " " fzf/args-for-preview)
                    fzf/args)))
    (let ((fzf--target-validator (fzf--use-validator
                                  (function fzf--validate-filename))))
      (fzf--start (fzf--resolve-directory) #'fzf--action-find-file))))

;; Public utility
(defun fzf-with-command (command action
                                 &optional directory as-filter initq
                                 file-pattern validator)
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
reduce the search space, instead of using fzf to filter (but not narrow).

Use the FILE-PATTERN to specify a grep file pattern different than what is
specified by the fzf/grep-file-pattern  user-option.

The default validator is `fzf--pass-through', specify another one by passing
it into the VALIDATOR argument."
  (let ((fzf--target-validator (or validator
                                   (function fzf--pass-through))))
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
                               (format " {q} %s || true\""
                                       (or (fzf---grep-file-pattern-for
                                            file-pattern :forced)
                                           "")))
                     fzf/args)))
          (fzf--start directory action args))
      (fzf--start directory action))))

;; Internal helper function
(defun fzf--with-command-and-args (command action
                                           &optional fzf-append-args
                                           directory)
  "Execute FZF on the output of COMMAND."
  ;; TO-DO clarify docstring
  (if command
      (let
          ((process-environment (cons
                                 (concat "FZF_DEFAULT_COMMAND=" command "")
                                 process-environment))
           (args (if fzf-append-args
                     (concat fzf/args " " fzf-append-args)
                   fzf/args)))
        (fzf--start directory action args))
    (fzf--start directory action)))

;; Public utility
(defun fzf-with-entries (entries action &optional directory validator)
  "Run `fzf` with the list ENTRIES as input.

ACTION is a function that takes a single argument, which is the
selected result from `fzf`.
If DIRECTORY is specified, fzf is run from that directory.
If VALIDATOR is specified it must be a function with the same signature as
`fzf--validate-filename' and it will be used as a item validator. If VALIDATOR
is nil, the default, then the `fzf--pass-through' validator is used (doing
no validation."
  (let ((fzf--target-validator (or validator
                                   (function fzf--pass-through))))
    (if entries
        (fzf-with-command
         (concat "echo \""
                 (mapconcat (lambda (x) x) entries "\n") "\"")
         action directory)
      (user-error "No input entries specified"))))

;;;###autoload
(defun fzf-directory (&optional with-preview)
  "Starts a fzf session at the specified directory.

With optional prefix WITH-PREVIEW the currently selected file
content or attribute is shown using the preview command
identified by the `fzf/args-for-preview' user-option.  By default
that shows the file content with cat, but that can be customized
to use other mechanisms."
  (interactive "P")
  (let ((fzf/args (if with-preview
                      (concat fzf/args " " fzf/args-for-preview)
                    fzf/args))
        (fzf--target-validator (fzf--use-validator
                                (function fzf--validate-filename)))
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

;; ---------------------------------------------------------------------------
;; Operations on Buffers

;;;###autoload
(defun fzf-switch-buffer ()
  "Switch buffer selecting them with fzf."
  (interactive)
  (let ((fzf--target-validator (fzf--use-validator
                                (function fzf--pass-through))))
    (fzf-with-entries
     (seq-filter
      (lambda (x) (not (string-prefix-p " " x)))
      (mapcar (function buffer-name) (buffer-list)))
     (lambda (x) (set-window-buffer nil x)))))


;; Internal helper function
(defun fzf--action-goto-line (target)
  "Extract line number from TARGET then jump to that line.

TARGET is a line produced by 'cat -n'."
  (let ((parts (split-string (string-trim-left target) " ")))
    (goto-char (point-min))
    (forward-line (1- (string-to-number (nth 0 parts))))))

;;;###autoload
(defun fzf-find-in-buffer ()
  "Fuzzy search the current buffer visiting a file."
  (interactive)
  (if (buffer-file-name)
      (progn
        (when (buffer-modified-p)
          (when (y-or-n-p (format "Save modified %S first? " (current-buffer)))
            (save-buffer)))
        (let ((fzf--target-validator (fzf--use-validator
                                      (function fzf--pass-through))))
          (fzf--with-command-and-args (concat "cat -n " buffer-file-name " | tac")
		                      (function fzf--action-goto-line)
                                      "--exact")))
    (user-error "Buffer %S is not visiting a file!" (current-buffer))))

;; ---------------------------------------------------------------------------

;;;###autoload
(defun fzf-find-file (&optional directory)
  "Find file in projectile project (if used), current or specified DIRECTORY."
  (interactive)
  (let ((fzf--target-validator (fzf--use-validator
                                (function fzf--validate-filename)))
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
(defun fzf-recentf (&optional with-preview)
  "Start a fzf session with the list of recently opened files.

With optional prefix WITH-PREVIEW the currently selected file
content or attribute is shown using the preview command
identified by the `fzf/args-for-preview' user-option.  By default
that shows the file content with cat, but that can be customized
to use other mechanisms."
  (interactive "P")
  (let ((fzf/args (if with-preview
                      (concat fzf/args " " fzf/args-for-preview)
                    fzf/args))
        (fzf--target-validator (fzf--use-validator
                                (function fzf--pass-through))))
    (if (bound-and-true-p recentf-list)
        (fzf-with-entries recentf-list #'fzf--action-find-file)
      (user-error "No recently opened files.%s"
                  (if (boundp 'recentf-list)
                      ""
                    " recentf-mode is not active!")))))

;;;###autoload
(defun fzf-grep (&optional search directory as-filter file-pattern)
  "FZF search filtered on a grep search result.

- SEARCH is the end of the grep command line;  typically holding the regexp
  identifying what to search and the glob pattern to identify the file that
  must be searched.  If SEARCH is nil, read input interactively.
- Grep in `fzf--resolve-directory` using DIRECTORY if provided.
- If AS-FILTER is non-nil, use grep as the narrowing filter instead of fzf.

File name & Line extraction:

- By default this function extracts file name and line number
  using the '(fzf--file-lnum-regexp 1 2) extraction list.

  If the grep command you use requires a different extraction
  rule, then let bind a `fzf-extractor-list' variable to a list
  of the form (regexp file-group line-group) over the call
  context of `fzf-grep' (or the function that calls it).

  - IMPORTANT: the name of that let-bound variable must have only
    one dash after 'fzf'!  It's not the same as the internal
    `fzf--extractor-list' variable!"
  (interactive)
  (let* ((fzf--target-validator (fzf--use-validator
                                 (function fzf--pass-through)))
         (dir (fzf--resolve-directory directory))
         (action #'fzf--action-find-file-with-line)
         (pattern (or search
                      (fzf--read-for 'grep-cmd
                                     (concat fzf/grep-command ": "))))
         (cmd (concat fzf/grep-command
                      " "
                      pattern
                      " "
                      file-pattern))
         (fzf--extractor-list (fzf--use-extractor
                               (list fzf--file-lnum-regexp 1 2))))
    (fzf-with-command cmd action dir as-filter pattern file-pattern)))

;;;###autoload
(defun fzf-grep-in-dir (&optional directory as-filter file-pattern)
  "Call `fzf-grep` in DIRECTORY.

If DIRECTORY is nil, read input interactively.
If AS-FILTER is non-nil, use grep as the narrowing filter instead of fzf.
If FILE-PATTERN is non-nil it is used to restrict the scope further. If nil,
it's not specified.

See note about file & line extraction in `fzf-grep'.
The same note applies here."
  (interactive)
  (let ((dir (or directory
                 (read-directory-name "Directory: " fzf/directory-start))))
    (fzf-grep nil dir as-filter file-pattern)))

;;;###autoload
(defun fzf-grep-with-narrowing (&optional with-file-pattern)
  "Call `fzf-grep` with grep as the narrowing filter.

By default the grep command searches in the files identified by
the `fzf/grep-file-pattern' user-option unless a
WITH-FILE_PATTERN prefix argument argument is used; in that case
it prompts for a file pattern to use. The prompt identifies the
tool used (grep or rg) if it recognizes the on specified in
`fzf/grep-command'.  Remember to use the file pattern appropriate
for the tool; grep and Ripgrep use different ones."
  (interactive "P")
  (let ((file-pattern (fzf--grep-file-pattern with-file-pattern)))
    (fzf-grep nil nil t file-pattern)))

;;;###autoload
(defun fzf-grep-in-dir-with-narrowing (&optional with-file-pattern)
  "Call `fzf-grep-in-dir` with grep as the narrowing filter.


By default the grep command searches in the files identified by
the `fzf/grep-file-pattern' user-option unless a
WITH-FILE_PATTERN prefix argument argument is used; in that case
it prompts for a file pattern to use. The prompt identifies the
tool used (grep or rg) if it recognizes the on specified in
`fzf/grep-command'.  Remember to use the file pattern appropriate
for the tool; grep and Ripgrep use different ones."
  (interactive "P")
  (let ((file-pattern (fzf--grep-file-pattern with-file-pattern)))
    (fzf-grep-in-dir nil t file-pattern)))

;;;###autoload
(defun fzf-grep-dwim (&optional with-file-pattern)
  "Call `fzf-grep` on `symbol-at-point`.

If there's no symbol at point (as identified by
`thing-at-point'), prompt for one.

By default the grep command searches in the files identified by
the `fzf/grep-file-pattern' user-option unless a
WITH-FILE_PATTERN prefix argument argument is used; in that case
it prompts for a file pattern to use. The prompt identifies the
tool used (grep or rg) if it recognizes the on specified in
`fzf/grep-command'.  Remember to use the file pattern appropriate
for the tool; grep and Ripgrep use different ones."
  (interactive "P")
  (let ((file-pattern (fzf--grep-file-pattern with-file-pattern)))
    (if (symbol-at-point)
        (fzf-grep (thing-at-point 'symbol) nil nil file-pattern)
      (fzf-grep nil nil nil file-pattern))))

;;;###autoload
(defun fzf-grep-dwim-with-narrowing (&optional with-file-pattern)
  "Call `fzf-grep` on `symbol-at-point`, with grep as the narrowing filter.

If there's no symbol at point (as identified by
`thing-at-point'), prompt for one.

By default the grep command searches in the files identified by
the `fzf/grep-file-pattern' user-option unless WITH-FILE_PATTERN
prefix argument is used; in that case it prompts for a file
pattern to use.

See note about file & line extraction in `fzf-grep'.  The same
note applies here."
  (interactive "P")
  (let ((file-pattern (fzf--grep-file-pattern with-file-pattern)))
    (if (symbol-at-point)
        (fzf-grep (thing-at-point 'symbol) nil t file-pattern)
      (fzf-grep nil nil t file-pattern))))

;; ---------------------------------------------------------------------------
;; VCS Support

;; Internal helper function
(defun fzf--vcs (vcs-name root-filename)
  "Run FZF in the VCS-NAME directory holding ROOT-FILENAME."
  (let ((fzf--target-validator (fzf--use-validator
                                (function fzf--validate-filename)))
        (path (locate-dominating-file default-directory root-filename)))
    (if path
        (fzf--start path (function fzf--action-find-file))
      (user-error "Not inside a %s repository" vcs-name))))

(defun fzf--vcs-command (vcs-name root-filename command)
  "Run FZF specific COMMAND in the VCS-NAME directory holding ROOT-FILENAME."
  (let ((fzf--target-validator (fzf--use-validator
                                (function fzf--validate-filename)))
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
(defun fzf-git-grep ()
  "Grep files committed in Git repo, fzf search result.

Use 'git grep' in the current Git repository to grep into the
files that have been committed into Git. Then execute fzf to
fuzzy search into the files/lines found.  Open the selected file
at the specific line.

Note that git grep *does not* grep into all past revisions of a
Git repo committed files (the way Mercurial 'hg grep' does).
This command only greps in the *current* version of the files.

- With Git, looking into the history is more involved and requires
  using several commands: 'git log -S' to identify the
  'version' (commit-sha) and then a grep on each of these.

See note about file & line extraction in `fzf-grep'.
The same note applies here."
  (interactive)
  (let ((fzf--target-validator (fzf--use-validator
                                (function fzf--pass-through)))
        (fzf--extractor-list (fzf--use-extractor
                              (list fzf--file-lnum-regexp 1 2))))
    (fzf-with-command (fzf--grep-cmd "git grep" fzf/git-grep-args)
                      #'fzf--action-find-file-with-line
                      (locate-dominating-file default-directory ".git"))))
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

;;;###autoload
(defun fzf-hg-grep (&optional all-revs)
  "Grep specified versions of files committed in Mercurial repo, fzf result.

Grep files committed in Mercurial repo and perform a fzf search on the output.
By default only grep in the current revision of the files, but with C-u prefix
grep inside all revisions of the files.

Perform fzf search on the result and open selection.

File name & Line extraction:

- By default this function extracts file name and line number
  using the '(fzf--file-rnum-lnum-regexp 1 3) extraction list.

  If the grep command you use requires a different extraction
  rule, then let bind a `fzf-extractor-list' variable to a list
  of the form (regexp file-group line-group) over the call
  context of `fzf-grep' (or the function that calls it).

  - IMPORTANT: the name of that let-bound variable must have only
    one dash after 'fzf'!  It's not the same as the internal
    `fzf--extractor-list' variable!"
  (interactive "P")
  ;; TODO : add ability to select revision range interactively
  (let ((fzf--target-validator (fzf--use-validator
                                (function fzf--pass-through)))
        (fzf--extractor-list (fzf--use-extractor
                              (list fzf--file-rnum-lnum-regexp 1 3))))
    (fzf-with-command (fzf--grep-cmd
                       (if all-revs
                           "hg grep --all"
                         "hg grep")
                       fzf/hg-grep-args)
                      #'fzf--action-find-file-with-line
                      (locate-dominating-file default-directory ".hg"))))

;; ---------------------------------------------------------------------------
;;;###autoload
(defun fzf-projectile (&optional with-preview)
  "Starts an fzf session at the root of the current projectile project."
  (interactive "P")
  (require 'projectile)
  (if (fboundp 'projectile-project-root)
      (let ((fzf/args (if with-preview
                          (concat fzf/args " " fzf/args-for-preview)
                        fzf/args))
            (fzf--target-validator (fzf--use-validator
                                    (function fzf--validate-filename))))
        (fzf--start (or (projectile-project-root) default-directory)
                    #'fzf--action-find-file))
    (error "projectile-project-root is not bound")))

;; ---------------------------------------------------------------------------

;; test function
(defun fzf/test ()
  "Test ability to handle simple strings."
  (let ((fzf--target-validator (fzf--use-validator
                                (function fzf--pass-through))))
    (fzf-with-entries
     (list "a" "b" "c")
     (lambda (x) (print x)))))

;; ---------------------------------------------------------------------------
(provide 'fzf)

;;; fzf.el ends here
