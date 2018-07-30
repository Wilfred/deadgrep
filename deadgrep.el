;;; deadgrep.el --- fast, friendly searching with ripgrep  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; URL: https://github.com/Wilfred/deadgrep
;; Keywords: tools
;; Version: 0.5
;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (s "1.11.0") (spinner "1.7.3") (projectile "0.14.0"))

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

;; Perform text searches with the speed of ripgrep and the comfort of
;; Emacs.  This is a bespoke mode that does not rely on
;; compilation-mode, but tries to be a perfect fit for ripgrep.

;; Install from MELPA, then `M-x ripgrep' will do a search!

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'spinner)
(require 'projectile)

(defgroup deadgrep nil
  "A powerful text search UI using ripgrep."
  :group 'tools
  :group 'matching)

(defvar deadgrep-executable
  (executable-find "rg"))

(defvar deadgrep-max-buffers
  4
  "Deadgrep will kill the least recently used results buffer
if there are more than this many.

To disable cleanup entirely, set this variable to nil.")

(defvar deadgrep-project-root-function
  #'deadgrep--project-root
  "Function called by `deadgrep' to work out the root directory
to search from.")

(defvar deadgrep-history
  nil
  "A list of the previous search terms.")

(defvar deadgrep-max-line-length
  500
  "Truncate lines if they are longer than this.

Emacs performance can be really poor long lines, so this ensures
that searching minified files does not slow down movement in
results buffers.

In extreme cases (100KiB+ single-line files), we can get a stack
overflow on our regexp matchers if we don't apply this.")

(defface deadgrep-meta-face
  '((t :inherit font-lock-comment-face))
  "Face used for deadgrep UI text."
  :group 'deadgrep)

(defface deadgrep-filename-face
  '((t :inherit bold))
  "Face used for filename headings in results buffers."
  :group 'deadgrep)

(defface deadgrep-regexp-metachar-face
  '((t :inherit
       ;; TODO: I've seen a more appropriate face in some themes,
       ;; find out what to use instead here.
       font-lock-constant-face))
  "Face used for regexp metacharacters in search terms."
  :group 'deadgrep)

(defface deadgrep-match-face
  '((t :inherit match))
  "Face used for the portion of a line that matches the search term."
  :group 'deadgrep)

(defvar-local deadgrep--search-term nil)
(defvar-local deadgrep--search-type 'string)
(defvar-local deadgrep--search-case 'smart)
(defvar-local deadgrep--file-type 'all)
(defvar-local deadgrep--context nil
  "When set, also show context of results.
This is stored as a cons cell of integers (lines-before . lines-after).")
(defvar-local deadgrep--initial-filename nil)

(defvar-local deadgrep--current-file nil)
(defvar-local deadgrep--spinner nil)
(defvar-local deadgrep--remaining-output nil
  "We can't guarantee that our process filter will always receive whole lines.
We save the last line here, in case we need to append more text to it.")

(defvar-local deadgrep--debug-command nil)
(defvar-local deadgrep--debug-first-output nil)

(defconst deadgrep--position-column-width 5)

(defconst deadgrep--color-code
  (rx "\x1b[" (+ digit) "m")
  "Regular expression for an ANSI color code.")

(defun deadgrep--insert-output (output &optional finished)
  "Propertize OUTPUT from rigrep and write to the current buffer."
  ;; If we had an unfinished line from our last call, include that.
  (when deadgrep--remaining-output
    (setq output (concat deadgrep--remaining-output output))
    (setq deadgrep--remaining-output nil))

  (let ((inhibit-read-only t)
        (lines (s-lines output)))
    ;; Process filters run asynchronously, and don't guarantee that
    ;; OUTPUT ends with a complete line. Save the last line for
    ;; later processing.
    (unless finished
      (setq deadgrep--remaining-output (-last-item lines))
      (setq lines (butlast lines)))

    (save-excursion
      (goto-char (point-max))
      (dolist (line lines)
        (cond
         ;; Ignore blank lines.
         ((s-blank? line))
         ;; Ignore -- lines, which are used as a context separator
         ;; when calling ripgrep with context flags.
         ((string= line "--"))
         ;; If we don't have a color code, ripgrep must be complaining
         ;; about something (e.g. zero matches for a
         ;; glob, or permission denied on some directories).
         ((not (s-matches-p deadgrep--color-code line))
          (when deadgrep--current-file
            (setq deadgrep--current-file nil)
            (insert "\n"))
          (insert line "\n\n"))
         (t
          (-let* ((truncate-p (> (length line) deadgrep-max-line-length))
                  (line
                   (if truncate-p
                       (substring line 0 deadgrep-max-line-length)
                     line))
                  ((filename line-num content) (deadgrep--split-line line))
                  (formatted-line-num
                   (s-pad-right deadgrep--position-column-width " "
                                (number-to-string line-num)))
                  (pretty-line-num
                   (propertize formatted-line-num
                               'face 'deadgrep-meta-face
                               'deadgrep-filename filename
                               'deadgrep-line-number line-num))
                  (pretty-filename
                   (propertize filename
                               'face 'deadgrep-filename-face
                               'deadgrep-filename filename)))
            (cond
             ;; This is the first file we've seen, print the heading.
             ((null deadgrep--current-file)
              (insert pretty-filename "\n"))
             ;; This is a new file, print the heading with a spacer.
             ((not (equal deadgrep--current-file filename))
              (insert "\n" pretty-filename "\n")))
            (setq deadgrep--current-file filename)

            ;; TODO: apply the invisible property if the user decided
            ;; to hide this filename before we finished finding
            ;; results in it.
            (insert pretty-line-num content)
            (when truncate-p
              (insert
               (propertize " ... (truncated)"
                           'face 'deadgrep-meta-face)))
            (insert "\n"))))))))

(defun deadgrep--process-sentinel (process output)
  "Update the deadgrep buffer associated with PROCESS as complete."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; rg has terminated, so stop the spinner.
        (spinner-stop deadgrep--spinner)

        (deadgrep--insert-output "" t)

        ;; Report any errors that occurred.
        (unless (member output
                        (list
                         "exited abnormally with code 1\n"
                         "finished\n"))
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert output))))

        (message "Deadgrep finished")))))

(defun deadgrep--process-filter (process output)
  ;; Searches may see a lot of output, but it's really useful to have
  ;; a snippet of output when debugging. Store the first output received.
  (unless deadgrep--debug-first-output
    (setq deadgrep--debug-first-output output))

  ;; If we had an unfinished line from our last call, include that.
  (when deadgrep--remaining-output
    (setq output (concat deadgrep--remaining-output output))
    (setq deadgrep--remaining-output nil))

  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (deadgrep--insert-output output))))

(defun deadgrep--extract-regexp (pattern s)
  "Search for PATTERN in S, and return the content of the first group."
  (string-match pattern s)
  (match-string 1 s))

(defconst deadgrep--filename-regexp
  (rx bos "\x1b[0m\x1b[3" (or "5" "6") "m" (group (+? anything)) "\x1b[")
  "Extracts the filename from a ripgrep line with ANSI color sequences.
We use the color sequences to extract the filename exactly, even
if the path contains colons.")

(defconst deadgrep--line-num-regexp
  (rx "\x1b[32m" (group (+ digit)))
  "Extracts the line number from a ripgrep line with ANSI color sequences.
Ripgrep uses a unique color for line numbers, so we use that to
extract the linue number exactly.")

(defconst deadgrep--line-contents-regexp
  (rx "\x1b[32m" (+ digit) "\x1b[0m" (or ":" "-") (group (* anything)))
  "Extract the line contents from a ripgrep line with ANSI color sequences.
Use the unique color for line numbers to ensure we start at the
correct colon.

Note that the text in the group will still contain color codes
highlighting which parts matched the user's search term.")

(defconst deadgrep--hit-regexp
  (rx-to-string
   `(seq
     ;; A reset color code.
     "\x1b[0m"
     ;; Two color codes, bold and color (any order).
     (regexp ,deadgrep--color-code)
     (regexp ,deadgrep--color-code)
     ;; The actual text.
     (group (+? anything))
     ;; A reset color code again.
     "\x1b[0m"))
  "Extract the portion of a line found by ripgrep that matches the user's input.
This may occur multiple times in one line.")

(defun deadgrep--split-line (line)
  "Split out the components of a raw LINE of output from rg.
Return the filename, line number, and the line content with ANSI
color codes replaced with string properties."
  (list
   (deadgrep--extract-regexp deadgrep--filename-regexp line)
   (string-to-number
    (deadgrep--extract-regexp deadgrep--line-num-regexp line))
   (deadgrep--propertize-hits
    (deadgrep--extract-regexp deadgrep--line-contents-regexp line))))

(defun deadgrep--propertize-hits (line-contents)
  "Given LINE-CONTENTS from ripgrep, replace ANSI color codes
with Emacs text properties."
  (replace-regexp-in-string
   deadgrep--hit-regexp
   (lambda (s)
     (propertize
      (match-string 1 s)
      'face 'deadgrep-match-face))
   line-contents))

(define-button-type 'deadgrep-search-term
  'action #'deadgrep--search-term
  'help-echo "Change search term")

(defun deadgrep--search-term (_button)
  (setq deadgrep--search-term
        ;; TODO: say string or regexp
        (read-from-minibuffer
         "Search term: "
         deadgrep--search-term))
  (rename-buffer
   (deadgrep--buffer-name deadgrep--search-term default-directory) t)
  (deadgrep-restart))

(define-button-type 'deadgrep-type
  'action #'deadgrep--search-type
  'search-type nil
  'help-echo "Change search type")

(defun deadgrep--search-type (button)
  (setq deadgrep--search-type (button-get button 'search-type))
  (deadgrep-restart))

(define-button-type 'deadgrep-case
  'action #'deadgrep--case
  'case nil
  'help-echo "Change case sensitivity")

(defun deadgrep--case (button)
  (setq deadgrep--search-case (button-get button 'case))
  (deadgrep-restart))

(define-button-type 'deadgrep-context
  'action #'deadgrep--context
  'context nil
  'help-echo "Show/hide context around match")

(defun deadgrep--context (button)
  ;; deadgrep--context takes the value of (before . after) when set.
  (setq deadgrep--context
        (cl-case (button-get button 'context)
          ((nil)
           nil)
          (before
           (cons
            (read-number "Show N lines before: ")
            (or (cdr-safe deadgrep--context) 0)))
          (after
           (cons
            (or (car-safe deadgrep--context) 0)
            (read-number "Show N lines after: ")))
          (t
           (error "Unknown context type"))))

  (deadgrep-restart))

(defun deadgrep--type-list ()
  "Query the rg executable for available file types."
  (let* ((output (shell-command-to-string (format "%s --type-list" deadgrep-executable)))
         (lines (s-lines (s-trim output)))
         (types (--map
                 (s-split (rx ": ") it)
                 lines)))
    types))

(define-button-type 'deadgrep-file-type
  'action #'deadgrep--file-type
  'case nil
  'help-echo "Change file type")

(defun deadgrep--read-file-type (filename)
  "Read a ripgrep file type, defaulting to the type that matches FILENAME."
  (let* ((types-and-exts (deadgrep--type-list))
         (types (-map #'-first-item types-and-exts))
         matching-type-and-ext)
    ;; If we've been given a filename with an extension.
    (when (and filename (file-name-extension filename))
      ;; Get the first type whose list of extensions contains this extension.
      (setq matching-type-and-ext
            (-find
             (-lambda ((_ extensions))
               (s-contains-p
                (format "*.%s" (file-name-extension filename))
                extensions))
             types-and-exts)))
    (completing-read "File type: "
                     types
                     nil t nil nil
                     (car-safe matching-type-and-ext))))

(defun deadgrep--file-type (button)
  (let ((button-type (button-get button 'file-type)))
    (cond
     ((eq button-type 'all)
      (setq deadgrep--file-type 'all))
     ((eq button-type 'type)
      (let ((new-file-type
             (deadgrep--read-file-type deadgrep--initial-filename)))
        (setq deadgrep--file-type (cons 'type new-file-type))))
     ((eq button-type 'glob)
      (let ((glob
             (read-from-minibuffer
              "Glob: "
              (cond
               ;; If we already have a glob pattern, edit it.
               ((eq (car-safe deadgrep--file-type) 'glob)
                (cdr deadgrep--file-type))
               ;; If the initial file had a file name of the form
               ;; foo.bar, offer *.bar as the initial glob.
               ((and deadgrep--initial-filename
                     (file-name-extension deadgrep--initial-filename))
                (format "*.%s"
                        (file-name-extension deadgrep--initial-filename)))
               (t
                "*")))))
        (setq deadgrep--file-type (cons 'glob glob))))
     (t
      (error "Unknown button type: %S" button-type))))
  (deadgrep-restart))

(define-button-type 'deadgrep-directory
  'action #'deadgrep--directory
  'help-echo "Change base directory")

(defun deadgrep--directory (_button)
  (setq default-directory
        (expand-file-name
         (read-directory-name "Search files in: ")))
  (rename-buffer
   (deadgrep--buffer-name deadgrep--search-term default-directory))
  (deadgrep-restart))

(defun deadgrep--button (text type &rest properties)
  ;; `make-text-button' mutates the string to add properties, so copy
  ;; TEXT first.
  (setq text (substring-no-properties text))
  (apply #'make-text-button text nil :type type properties))

(defun deadgrep--format-command (search-term search-type case context)
  "Return a command string that we can execute in a shell
to obtain ripgrep results."
  (format
   "%s --color=ansi --line-number --no-heading --with-filename %s %s %s %s -- %s ."
   deadgrep-executable
   (cond
    ((eq search-type 'string)
     "--fixed-strings")
    ((eq search-type 'words)
     "--fixed-strings --word-regexp")
    ((eq search-type 'regexp)
     "")
    (t
     (error "Unknown search type: %s" search-type)))
   (cond
    ((eq case 'smart)
     "--smart-case")
    ((eq case 'sensitive)
     "--case-sensitive")
    ((eq case 'ignore)
     "--ignore-case")
    (t
     (error "Unknown case: %s" case)))
   ;; TODO: pass this as an argument.
   (cond
    ((eq deadgrep--file-type 'all)
     "")
    ((eq (car-safe deadgrep--file-type) 'type)
     (format "--type %s" (cdr deadgrep--file-type)))
    ((eq (car-safe deadgrep--file-type) 'glob)
     (format "--type-add 'custom:%s' --type custom"
             (cdr deadgrep--file-type)))
    (t
     (error "Unknown file-type: %S" deadgrep--file-type)))
   (if context
       (format "--before-context %s --after-context %s"
               (car context) (cdr context))
     "")
   (shell-quote-argument search-term)))

(defun deadgrep--write-heading ()
  "Write the deadgrep heading with buttons reflecting the current
search settings."
  (let ((inhibit-read-only t))
    (insert (propertize "Search term: "
                        'face 'deadgrep-meta-face)
            (if (eq deadgrep--search-type 'regexp)
                (deadgrep--propertize-regexp deadgrep--search-term)
              deadgrep--search-term)
            " "
            (deadgrep--button "change" 'deadgrep-search-term)
            "\n"
            (propertize "Search type: "
                        'face 'deadgrep-meta-face)

            (if (eq deadgrep--search-type 'string)
                "string"
              (deadgrep--button "string" 'deadgrep-type
                                'search-type 'string))
            " "
            (if (eq deadgrep--search-type 'words)
                "words"
              (deadgrep--button "words" 'deadgrep-type
                                'search-type 'words))
            " "
            (if (eq deadgrep--search-type 'regexp)
                "regexp"
              (deadgrep--button "regexp" 'deadgrep-type
                                'search-type 'regexp))
            "\n"
            (propertize "Case: "
                        'face 'deadgrep-meta-face)
            (if (eq deadgrep--search-case 'smart)
                "smart"
              (deadgrep--button "smart" 'deadgrep-case
                                'case 'smart))
            " "
            (if (eq deadgrep--search-case 'sensitive)
                "sensitive"
              (deadgrep--button "sensitive" 'deadgrep-case
                                'case 'sensitive))
            " "
            (if (eq deadgrep--search-case 'ignore)
                "ignore"
              (deadgrep--button "ignore" 'deadgrep-case
                                'case 'ignore))
            "\n"
            (propertize "Context: "
                        'face 'deadgrep-meta-face)
            (if deadgrep--context
                (deadgrep--button "none" 'deadgrep-context
                                  'context nil)
              "none")
            " "
            (deadgrep--button "before" 'deadgrep-context
                              'context 'before)
            (if deadgrep--context
                (format ":%d" (car deadgrep--context))
              "")
            " "
            (deadgrep--button "after" 'deadgrep-context
                              'context 'after)
            (if deadgrep--context
                (format ":%d" (cdr deadgrep--context))
              "")

            "\n\n"
            (propertize "Directory: "
                        'face 'deadgrep-meta-face)
            (deadgrep--button
             (abbreviate-file-name default-directory)
             'deadgrep-directory)
            "\n"
            (propertize "Files: "
                        'face 'deadgrep-meta-face)
            (if (eq deadgrep--file-type 'all)
                "all"
              (deadgrep--button "all" 'deadgrep-file-type
                                'file-type 'all))
            " "
            (deadgrep--button "type" 'deadgrep-file-type
                              'file-type 'type)
            (if (eq (car-safe deadgrep--file-type) 'type)
                (format ":%s" (cdr deadgrep--file-type))
              "")
            " "
            (deadgrep--button "glob" 'deadgrep-file-type
                              'file-type 'glob)
            (if (eq (car-safe deadgrep--file-type) 'glob)
                (format ":%s" (cdr deadgrep--file-type))
              "")
            "\n\n")))

;; TODO: could we do this in the minibuffer too?
(defun deadgrep--propertize-regexp (regexp)
  "Given a string REGEXP representing a search term with regular
expression syntax, highlight the metacharacters.
Returns a copy of REGEXP with properties set."
  (setq regexp (copy-sequence regexp))

  ;; See https://docs.rs/regex/1.0.0/regex/#syntax
  (let ((metachars
         ;; Characters that don't match themselves.
         '(?\( ?\) ?\[ ?\] ?\{ ?\} ?| ?. ?+ ?* ?? ?^ ?$))
        ;; Characters that have special regexp meaning when preceded
        ;; with a backslash. This includes things like \b but not
        ;; things like \n.
        (escape-metachars
         '(?A ?b ?B ?d ?D ?p ?s ?S ?w ?W ?z))
        (prev-char nil))
    (--each-indexed (string-to-list regexp)
      (cond
       ((and (memq it metachars) (not (equal prev-char ?\\)))
        (put-text-property
         it-index (1+ it-index)
         'face
         'deadgrep-regexp-metachar-face
         regexp))
       ((and (memq it escape-metachars) (equal prev-char ?\\))
        (put-text-property
         (1- it-index) (1+ it-index)
         'face 'deadgrep-regexp-metachar-face
         regexp)))

      (setq prev-char it)))
  regexp)

(defun deadgrep--buffer-name (search-term directory)
  (format "*deadgrep %s %s*"
          search-term
          (abbreviate-file-name directory)))

(defun deadgrep--buffers ()
  "All the current deadgrep results buffers.
Returns a list ordered by the most recently accessed."
  (--filter (with-current-buffer it
              (eq major-mode 'deadgrep-mode))
            ;; `buffer-list' seems to be ordered by most recently
            ;; visited first.
            (buffer-list)))

(defun deadgrep--buffer (search-term directory initial-filename)
  "Create and initialise a search results buffer."
  (let* ((buf-name (deadgrep--buffer-name search-term directory))
         (buf (get-buffer buf-name)))
    (unless buf
      ;; If we need to create the buffer, ensure we don't exceed
      ;; `deadgrep-max-buffers' by killing the least recently used.
      (when (numberp deadgrep-max-buffers)
        (let* ((excess-buffers (-drop (1- deadgrep-max-buffers)
                                      (deadgrep--buffers))))
          ;; Kill buffers so we have one buffer less than the maximum
          ;; before we create a new one.
          (-each excess-buffers #'kill-buffer)))

      (setq buf (get-buffer-create buf-name)))

    (with-current-buffer buf
      (setq default-directory directory)
      (let ((inhibit-read-only t))
        ;; This needs to happen first, as it clobbers all buffer-local
        ;; variables.
        (deadgrep-mode)
        (erase-buffer)

        (setq deadgrep--search-term search-term)
        (setq deadgrep--current-file nil)
        (setq deadgrep--initial-filename initial-filename))
      (setq buffer-read-only t))
    buf))

(define-derived-mode deadgrep-mode special-mode
  '("Deadgrep" (:eval (spinner-print deadgrep--spinner))))

(defun deadgrep--current-column ()
  "Get the current column position in char terms.
This treats tabs as 1 and ignores the line numbers in the results
buffer."
  (let* ((line-start (line-beginning-position))
         (line-number
          (get-text-property line-start 'deadgrep-line-number))
         (line-number-width
          (max deadgrep--position-column-width
               (length (number-to-string line-number))))
         (char-count 0))
    (save-excursion
      (while (not (equal (point) line-start))
        (cl-incf char-count)
        (backward-char 1)))
    (max
     (- char-count line-number-width)
     0)))

(defun deadgrep--flash-column-offsets (start end)
  "Temporarily highlight column offset from START to END."
  (let* ((line-start (line-beginning-position))
         (overlay (make-overlay
                   (+ line-start start)
                   (+ line-start end))))
    (overlay-put overlay 'face 'highlight)
    (run-with-timer 1.0 nil 'delete-overlay overlay)))

(defun deadgrep--match-face-p (pos)
  "Is there a match face at POS?"
  (eq (get-text-property pos 'face) 'deadgrep-match-face))

(defun deadgrep--match-positions ()
  "Return a list of indexes of the current line's matches."
  (let (positions)
    (save-excursion
      (beginning-of-line)

      (let* ((line-number
              (get-text-property (point) 'deadgrep-line-number))
             (line-number-width
              (max deadgrep--position-column-width
                   (length (number-to-string line-number))))
             (i 0)
             (start-pos 0)
             (line-end-pos (line-end-position)))

        (forward-char line-number-width)

        (while (<= (point) line-end-pos)
          ;; If we've just entered a match, record the start position.
          (when (and (deadgrep--match-face-p (point))
                     (not (deadgrep--match-face-p (1- (point)))))
            (setq start-pos i))
          ;; If we've just left a match, record the match range.
          (when (and (not (deadgrep--match-face-p (point)))
                     (deadgrep--match-face-p (1- (point))))
            (push (list start-pos i) positions))

          (setq i (1+ i))
          (forward-char 1))))

    (nreverse positions)))

(defun deadgrep-visit-result (&optional other-window)
  "Goto the search result at point.  View result in other window if OTHER-WINDOW is non-nil."
  (interactive)
  (let* ((pos (line-beginning-position))
         (file-name (get-text-property pos 'deadgrep-filename))
         (line-number (get-text-property pos 'deadgrep-line-number))
         (column-offset (when line-number (deadgrep--current-column)))
         (match-positions (when line-number (deadgrep--match-positions))))
    (when file-name
      (if other-window
          (find-file-other-window file-name)
        (find-file file-name))
      (goto-char (point-min))
      (when line-number
        (forward-line (1- line-number))
        (forward-char column-offset)
        (-each match-positions
          (-lambda ((start end))
            (deadgrep--flash-column-offsets start end)))))))

(define-key deadgrep-mode-map (kbd "RET") #'deadgrep-visit-result)
(define-key deadgrep-mode-map (kbd "o") (lambda() (interactive) (deadgrep-visit-result t)))
;; TODO: we should still be able to click on buttons.

(define-key deadgrep-mode-map (kbd "g") #'deadgrep-restart)

(defvar-local deadgrep--hidden-files nil
  "An alist recording which files currently have their lines
hidden in this deadgrep results buffer.

Keys are interned filenames, so they compare with `eq'.")

(defun deadgrep-toggle-file-results ()
  "Show/hide the results of the file at point."
  (interactive)
  (let* ((pos (line-beginning-position))
         (file-name (get-text-property pos 'deadgrep-filename))
         (line-number (get-text-property pos 'deadgrep-line-number)))
    (when (and file-name (not line-number))
      ;; We're on a file heading.
      (if (alist-get (intern file-name) deadgrep--hidden-files)
          (deadgrep--show)
        (deadgrep--hide)))))

(defun deadgrep--show ()
  (-let* ((pos (line-beginning-position))
          (file-name (get-text-property pos 'deadgrep-filename))
          ((start-pos end-pos) (alist-get (intern file-name) deadgrep--hidden-files)))
    (remove-overlays start-pos end-pos 'invisible t)
    (setf (alist-get (intern file-name) deadgrep--hidden-files)
          nil)))

(defun deadgrep--hide ()
  "Hide the file results immediately after point."
  (save-excursion
    (let* ((pos (line-beginning-position))
           (file-name (get-text-property pos 'deadgrep-filename))
           (start-pos
            (progn
              (forward-line)
              (point)))
           (end-pos
            (progn
              (while (and
                      (get-text-property (point) 'deadgrep-line-number)
                      (not (bobp)))
                (forward-line))
              ;; Step over the newline.
              (1+ (point))))
           (o (make-overlay start-pos end-pos)))
      (overlay-put o 'invisible t)
      (setf (alist-get (intern file-name) deadgrep--hidden-files)
            (list start-pos end-pos)))))

(define-key deadgrep-mode-map (kbd "TAB") #'deadgrep-toggle-file-results)

(defun deadgrep-kill-process ()
  "Kill the deadgrep process associated with the current buffer."
  (interactive)
  (if (get-buffer-process (current-buffer))
      (interrupt-process)
    (message "No process running.")))

;; Keybinding chosen to match `kill-compilation'.
(define-key deadgrep-mode-map (kbd "C-c C-k") #'deadgrep-kill-process)

(defun deadgrep--item-p (pos)
  "Is there something at POS that we can interact with?"
  (or (button-at pos)
      (get-text-property pos 'deadgrep-filename)))

(defun deadgrep--move (forward-p)
  "Move to the next item.
This will either be a button, a filename, or a search result."
  (interactive)
  (let ((pos (point)))
    ;; If point is initially on an item, move past it.
    (while (and (deadgrep--item-p pos)
                (if forward-p
                    (< pos (point-max))
                  (> pos (point-min))))
      (if forward-p
          (cl-incf pos)
        (cl-decf pos)))
    ;; Find the next item.
    (while (and (not (deadgrep--item-p pos))
                (if forward-p
                    (< pos (point-max))
                  (> pos (point-min))))
      (if forward-p
          (cl-incf pos)
        (cl-decf pos)))
    ;; Regardless of direction, ensure point is at the beginning of
    ;; the item.
    (while (and (if forward-p
                    (< pos (point-max))
                  (> pos (point-min)))
                (deadgrep--item-p (1- pos)))
      (cl-decf pos))
    ;; If we reached an item (we aren't at the first/last item), then
    ;; go to it.
    (when (deadgrep--item-p pos)
      (goto-char pos))))

(defun deadgrep-forward ()
  "Move forward to the next item.
This will either be a button, a filename, or a search result."
  (interactive)
  (deadgrep--move t))

(defun deadgrep-backward ()
  "Move backward to the previous item.
This will either be a button, a filename, or a search result."
  (interactive)
  (deadgrep--move nil))

;; TODO: add the ability to fold results for a given file, just like
;; magit.
(define-key deadgrep-mode-map (kbd "n") #'deadgrep-forward)
(define-key deadgrep-mode-map (kbd "p") #'deadgrep-backward)

(defun deadgrep--start (search-term search-type case)
  "Start a ripgrep search."
  (setq deadgrep--spinner (spinner-create 'progress-bar t))
  (spinner-start deadgrep--spinner)
  (let* ((command (deadgrep--format-command
                   search-term search-type case
                   deadgrep--context))
         (process
          (start-process-shell-command
           (format "rg %s" search-term)
           (current-buffer)
           command)))
    (setq deadgrep--debug-command command)
    (set-process-filter process #'deadgrep--process-filter)
    (set-process-sentinel process #'deadgrep--process-sentinel)))

(defun deadgrep-restart ()
  "Re-run ripgrep with the current search settings."
  (interactive)
  (let ((start-point (point))
        (inhibit-read-only t))
    (erase-buffer)
    (setq deadgrep--current-file nil)
    (setq deadgrep--hidden-files nil)

    (deadgrep--write-heading)
    ;; If the point was in the heading, ensure that we restore its
    ;; position.
    (goto-char (min (point-max) start-point))

    (deadgrep--start
     deadgrep--search-term
     deadgrep--search-type
     deadgrep--search-case)))

(defun deadgrep--read-search-term ()
  "Read a search term from the minibuffer.
If region is active, return that immediately.  Otherwise, prompt
for a string, offering the current word as a default."
  (let (search-term)
    (if (use-region-p)
        (progn
          (setq search-term
                (buffer-substring-no-properties (region-beginning) (region-end)))
          (deactivate-mark))
      (let* ((sym (symbol-at-point))
             (sym-name (when sym
                         (substring-no-properties (symbol-name sym))))
             ;; TODO: prompt should say search string or search regexp
             ;; as appropriate.
             (prompt
              (if sym
                  (format "Search term (default %s): " sym-name)
                "Search term: ")))
        (setq search-term
              (read-from-minibuffer
               prompt nil nil nil 'deadgrep-history sym-name))
        (when (equal search-term "")
          (setq search-term sym-name))))
    (unless (equal (car deadgrep-history) search-term)
      (push search-term deadgrep-history))
    search-term))

(defun deadgrep--project-root ()
  "Guess the project root of the given FILE-PATH."
  (let ((projectile-require-project-root nil))
    (projectile-project-root)))

;;;###autoload
(defun deadgrep ()
  "Start a ripgrep search for SEARCH-TERM."
  (interactive)
  (let* ((search-term (deadgrep--read-search-term))
         (dir (funcall deadgrep-project-root-function))
         (buf (deadgrep--buffer
               search-term
               dir
               (or deadgrep--initial-filename
                   (buffer-file-name))))
         (last-results-buf (car-safe (deadgrep--buffers)))
         prev-search-type
         prev-search-case)
    ;; Find out what search settings were used last time.
    (when last-results-buf
      (with-current-buffer last-results-buf
        (setq prev-search-type deadgrep--search-type)
        (setq prev-search-case deadgrep--search-case)))

    (switch-to-buffer buf)

    ;; If we have previous search settings, apply them to our new
    ;; search results buffer.
    (when last-results-buf
      (setq deadgrep--search-type prev-search-type)
      (setq deadgrep--search-case prev-search-case))

    (deadgrep--write-heading)
    (deadgrep--start
     search-term
     deadgrep--search-type
     deadgrep--search-case)))

(defun deadgrep-debug ()
  "Show a buffer with some debug information about the current search."
  (interactive)
  (let ((command deadgrep--debug-command)
        (output deadgrep--debug-first-output)
        (buf (get-buffer-create "*deadgrep debug*"))
        (inhibit-read-only t))
    (pop-to-buffer buf)
    (erase-buffer)
    (special-mode)
    (setq buffer-read-only t)

    (insert
     "About your environment:\n"
     (format "Platform: %s\n" system-type)
     (format "Emacs version: %s\n" emacs-version)
     (format "Command: %s\n" command)
     (format "\nInitial output from ripgrep:\n%S" output)
     (format "\n\nPlease file bugs at https://github.com/Wilfred/deadgrep/issues/new"))))

(provide 'deadgrep)
;;; deadgrep.el ends here
