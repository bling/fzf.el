;;; fzf.el --- A front-end for fzf.
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

(defun fzf/after-term-handle-exit (process-name msg)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (lines (split-string text "\n" t "\s*>\s+"))
         (target (car (last (butlast lines 1))))
         (file (expand-file-name (string-trim target))))
    (kill-buffer "*fzf*")
    (jump-to-register :fzf-windows)
    (when (file-exists-p file)
      (find-file file)))
  (advice-remove 'term-handle-exit #'fzf/after-term-handle-exit))

(defun fzf/start (directory)
  (require 'term)
  (window-configuration-to-register :fzf-windows)
  (advice-add 'term-handle-exit :after #'fzf/after-term-handle-exit)
  (let* ((buf (get-buffer-create "*fzf*"))
         (min-height (min fzf/window-height (/ (window-height) 2)))
         (window-height (if fzf/position-bottom (- min-height) min-height))
         (window-system-args (when window-system " --margin=1,0"))
         (fzf-args (concat fzf/args window-system-args)))
    (with-current-buffer buf
      (setq default-directory directory))
    (split-window-vertically window-height)
    (when fzf/position-bottom (other-window 1))
    (apply 'make-term "fzf" fzf/executable nil (split-string fzf-args))
    (switch-to-buffer buf)
    (linum-mode 0)
    (visual-line-mode 0)

    ;; disable various settings known to cause artifacts, see #1 for more details
    (setq-local scroll-margin 0)
    (setq-local scroll-conservatively 0)
    (setq-local term-suppress-hard-newline t) ;for paths wider than the window
    (setq-local show-trailing-whitespace nil)
    (setq-local display-line-numbers nil)
    (face-remap-add-relative 'mode-line '(:box nil))

    (term-char-mode)
    (setq mode-line-format (format "   FZF  %s" directory))))

(defun fzf/vcs (match)
  (let ((path (locate-dominating-file default-directory match)))
    (if path
        (fzf/start path)
      (fzf-directory))))

(defun fzf/git-files ()
  (let ((process-environment
         (cons (concat "FZF_DEFAULT_COMMAND=git ls-files")
               process-environment))
        (path (locate-dominating-file default-directory ".git")))
    (if path
        (fzf/start path)
      (user-error "Not inside a Git repository"))))

;;;###autoload
(defun fzf ()
  "Starts a fzf session."
  (interactive)
  (if (fboundp #'projectile-project-root)
      (fzf/start (condition-case err
                     (projectile-project-root)
                   (error
                    default-directory)))
    (fzf/start default-directory)))

;;;###autoload
(defun fzf-directory ()
  "Starts a fzf session at the specified directory."
  (interactive)
  (fzf/start (ido-read-directory-name "Directory: " fzf/directory-start)))

;;;###autoload
(defun fzf-git ()
  "Starts a fzf session at the root of the current git."
  (interactive)
  (fzf/vcs ".git"))

;;;###autoload
(defun fzf-git-files ()
  "Starts a fzf session only searching for git tracked files."
  (interactive)
  (fzf/git-files))

;;;###autoload
(defun fzf-hg ()
  "Starts a fzf session at the root of the curreng hg."
  (interactive)
  (fzf/vcs ".hg"))

;;;###autoload
(defun fzf-projectile ()
  "Starts a fzf session at the root of the projectile project."
  (interactive)
  (require 'projectile)
  (fzf/start (projectile-project-root)))

(provide 'fzf)
;;; fzf.el ends here
