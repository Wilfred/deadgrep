;;; deadgrep-transient.el --- Transient interface for deadgrep  -*- lexical-binding: t; -*-

;; URL: https://github.com/Wilfred/deadgrep
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (transient "0.6"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Perform deadgrep searches with transient interface.

;; Install from MELPA, then `M-x deadgrep-transient' will do a search!

;;; Code:

(require 'deadgrep)
(require 'transient)

(defvar-local deadgrep-transient-directory nil
  "Directory where `deadgrep-transisent' searches.")

(transient-define-infix deadgrep-transient:--after-context ()
  :description "After context"
  :class 'transient-option
  :key "-A"
  :argument "--after-context="
  :reader 'transient-read-number-N+)

(transient-define-infix deadgrep-transient:--before-context ()
  :description "Before context"
  :class 'transient-option
  :key "-B"
  :argument "--before-context="
  :reader 'transient-read-number-N+)

(transient-define-infix deadgrep-transient:--fixed-strings ()
  :description "String match"
  :class 'transient-switch
  :key "-F"
  :argument "--fixed-strings")

(transient-define-infix deadgrep-transient:--word-regexp ()
  :description "Word match"
  :class 'transient-switch
  :key "-w"
  :argument "--word-regexp")

(transient-define-infix deadgrep-transient:--*-case ()
  :description "Search case"
  :class 'transient-switches
  :key "=c"
  :argument-format "--%s"
  :argument-regexp "\\(--\\(smart-case\\|case-sensitive\\|ignore-case\\)\\)"
  :choices '("smart-case" "case-sensitive" "ignore-case"))

(transient-define-infix deadgrep-transient:--type ()
  :description "File type"
  :class 'transient-option
  :key "-t"
  :argument "--type="
  :reader 'deadgrep-transient--read-file-type)

(transient-define-infix deadgrep-transient:--glob ()
  :description "File glob"
  :class 'transient-option
  :key "-g"
  :argument "--glob="
  :reader 'deadgrep-transient--read-glob
  :multi-value 'repeat)

(transient-define-infix deadgrep-transient:--hidden ()
  :description "Search hidden"
  :class 'transient-switch
  :key "-."
  :argument "--hidden")

(transient-define-infix deadgrep-transient:--no-ignore-vcs ()
  :description "No VCS ignore"
  :class 'transient-switch
  :key "=v"
  :argument "--no-ignore-vcs")

(defclass deadgrep-transient-directory-variable (transient-lisp-variable)
  ())

(cl-defmethod transient-init-value ((obj deadgrep-transient-directory-variable))
  "Initialize `deadgrep-transient' directory OBJ by `deadgrep-project-root-function'."
  (funcall (oref obj set-value)
           (oref obj variable)
           (oset obj value (if deadgrep--search-term
                               default-directory
                             (funcall deadgrep-project-root-function)))))

(cl-defmethod transient-format-value ((obj deadgrep-transient-directory-variable))
  "Format `deadgrep-transient' directory OBJ with abbreviation."
  (propertize (prin1-to-string (abbreviate-file-name (oref obj value)))
              'face 'transient-value))

(transient-define-infix deadgrep-transient:directory ()
  :description "Directory"
  :class 'deadgrep-transient-directory-variable
  :key "=d"
  :variable 'deadgrep-transient-directory
  :reader (lambda (prompt initial-input _history)
            (read-directory-name prompt nil nil nil initial-input)))

(defun deadgrep-transient-restart ()
  "Restart deadgrep search with new options."
  (interactive)
  (let ((deadgrep--arguments-function #'deadgrep-transient--arguments))
    (setq default-directory deadgrep-transient-directory)
    (deadgrep-transient--set-options transient-current-suffixes)
    (rename-buffer
     (deadgrep--buffer-name deadgrep--search-term default-directory)
     t)
    (deadgrep-restart)))

(defun deadgrep-transient-restart-with-new-term ()
  "Restart deadgrep search with new options and new term."
  (interactive)
  (setq deadgrep--search-term
        (read-from-minibuffer (deadgrep--search-prompt) deadgrep--search-term))
  (deadgrep-transient-restart))

(defun deadgrep-transient-search ()
  "Search in the current directory using transient arguments."
  (interactive)
  (let* ((deadgrep--arguments-function #'deadgrep-transient--arguments)
         (deadgrep--write-heading-function
          (lambda ()
            (deadgrep-transient--set-options transient-current-suffixes)
            (deadgrep--write-heading)))
         (deadgrep-project-root-function (lambda () deadgrep-transient-directory)))
    (call-interactively #'deadgrep)))

(defun deadgrep-transient--set-options (suffixes)
  "Set deadgrep options from transient SUFFIXES."
  (setq deadgrep--context nil)
  (setq deadgrep--search-case 'sensitive)
  (setq deadgrep--search-type 'regexp)
  (setq deadgrep--file-type 'all)

  (let ((after-context 0) (before-context 0) word-regexp)
    (dolist (suffix suffixes)
      (pcase (oref suffix :command)
        ('deadgrep-transient:--after-context
         (-when-let (value (oref suffix value))
           (setq after-context (string-to-number value))))
        ('deadgrep-transient:--before-context
         (-when-let (value (oref suffix value))
           (setq before-context (string-to-number value))))

        ('deadgrep-transient:--fixed-strings
         (when (transient-infix-value suffix)
           (if word-regexp
               (setq deadgrep--search-type 'words)
             (setq deadgrep--search-type 'string))))

        ('deadgrep-transient:--word-regexp
         (when (transient-infix-value suffix)
           (setq word-regexp t)
           (when (eq deadgrep--search-type 'string)
             (setq deadgrep--search-type 'words))))

        ('deadgrep-transient:--*-case
         (-when-let (value (cdr (assoc (transient-infix-value suffix)
                                       '(("--case-sensitive" . sensitive)
                                         ("--ignore-case" . ignore)
                                         ("--smart-case" . smart)))))
           (setq deadgrep--search-case value)))

        ('deadgrep-transient:--glob
         ;; Skip "!/.git"
         (-when-let (glob (--first (not (string= it "!/.git")) (oref suffix value)))
           (setq deadgrep--file-type (cons 'glob glob))))

        ('deadgrep-transient:--type
         (-when-let (value (oref suffix value))
           (setq deadgrep--file-type (cons 'type value))))

        ('deadgrep-transient:--hidden
         (setq deadgrep--skip-if-hidden (not (oref suffix value))))

        ('deadgrep-transient:--no-ignore-vcs
         (if (oref suffix value)
             (progn
               (setq deadgrep--skip-if-vcs-ignore nil)
               (when (eq deadgrep--file-type 'all)
                 (setq deadgrep--file-type '(glob . "!/.git"))))
           (setq deadgrep--skip-if-vcs-ignore t)))))

    (when (or (/= before-context 0) (/= after-context 0))
      (setq deadgrep--context (cons before-context after-context)))))

(defun deadgrep-transient--arguments (search-term &optional _search-type _case _context)
  "Format ripgrep command using SEARCH-TERM."
  (let ((args (copy-sequence deadgrep-extra-arguments)))
    (push "--color=ansi" args)
    (push "--line-number" args)
    (push "--no-heading" args)
    (push "--no-column" args)
    (push "--with-filename" args)
    (setq args (nconc (transient-args 'deadgrep-transient-menu) args))
    (push "--" args)
    (push search-term args)
    (push "." args)
    (nreverse args)))

(defun deadgrep-transient--read-file-type (_prompt _initial-input _history)
  "Read file type for `deadgrep-transient:--file-type'."
  (deadgrep--read-file-type (buffer-file-name)))

(defun deadgrep-transient--read-glob (prompt initial-input history)
  "Read glob pattern for `deadgrep-transient:--glob'.
PROMPT, INITIAL-INPUT and HISTORY are passed to `completing-read-multiple'."
  (completing-read-multiple prompt nil nil nil initial-input history))

(defclass deadgrep-transient-prefix (transient-prefix)
  ())

(transient-define-prefix deadgrep-transient-menu ()
  "Deadgrep transient menu."
  :class 'deadgrep-transient-prefix
  ["Search Options"
   (deadgrep-transient:--fixed-strings)
   (deadgrep-transient:--word-regexp)
   (deadgrep-transient:--*-case)
   ]
  ["Filter Options"
   (deadgrep-transient:--type)
   (deadgrep-transient:--glob)
   (deadgrep-transient:--hidden)
   (deadgrep-transient:--no-ignore-vcs)
   ]
  ["Output Options"
   (deadgrep-transient:--before-context)
   (deadgrep-transient:--after-context)
   ]
  ["Environment"
   (deadgrep-transient:directory)]
  ["Search"
   ("r" "Restart search" deadgrep-transient-restart :if (lambda () deadgrep--search-term))
   ("s" "Restart search with new term" deadgrep-transient-restart-with-new-term :if (lambda () deadgrep--search-term))
   ("n" "New search" deadgrep-transient-search)])

(cl-defmethod transient-init-value ((obj deadgrep-transient-prefix))
  "Initialize `deadgrep-transient' OBJ from `deadgrep--arguments'."
  (let ((orig-args (deadgrep--arguments
                    deadgrep--search-term
                    deadgrep--search-type
                    deadgrep--search-case
                    deadgrep--context))
        (args '())
        (globs '()))
    (oset obj value
          (catch 'break
            (dolist (arg orig-args)
              (cond
               ((equal arg "--")
                (throw 'break (append globs args)))
               ((string-prefix-p "--glob=" arg)
                (unless (member arg globs)
                  (push arg globs)))
               (t (push arg args))))))))

(define-key deadgrep-mode-map "t" #'deadgrep-transient-menu)

(provide 'deadgrep-transient)
;;; deadgrep-transient.el ends here
