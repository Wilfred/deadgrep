;;; deadgrep.el --- fast, friendly searching with ripgrep  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; URL: https://github.com/Wilfred/deadgrep
;; Keywords: tools
;; Version: 0.2
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
(autoload 'projectile-project-root "projectile")

(defvar deadgrep-executable
  (executable-find "rg"))

(defvar deadgrep-max-buffers
  4
  "Deadgrep will kill the least recently used results buffer
if there are more than this many.

To disable cleanup entirely, set this variable to nil.")

(defvar deadgrep-max-line-length
  500
  "Truncate lines if they are longer than this.

Emacs performance can be really poor long lines, so this ensures
that searching minified files does not slow down movement in
results buffers.

In extreme cases (100KiB+ single-line files), we can get a stack
overflow on our regexp matchers if we don't apply this.")

(defvar-local deadgrep--search-term nil)
(defvar-local deadgrep--search-type 'string)
(defvar-local deadgrep--search-case 'smart)
(defvar-local deadgrep--file-type 'all)
(defvar-local deadgrep--initial-filename nil)

(defvar-local deadgrep--current-file nil)
(defvar-local deadgrep--spinner nil)
(defvar-local deadgrep--remaining-output nil
  "We can't guarantee that our process filter will always receive whole lines.
We save the last line here, in case we need to append more text to it.")

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
                               'face 'font-lock-comment-face
                               'deadgrep-filename filename
                               'deadgrep-line-number line-num))
                  (pretty-filename
                   (propertize filename
                               'face 'bold
                               'deadgrep-filename filename)))
            (cond
             ;; This is the first file we've seen, print the heading.
             ((null deadgrep--current-file)
              (insert pretty-filename "\n"))
             ;; This is a new file, print the heading with a spacer.
             ((not (equal deadgrep--current-file filename))
              (insert "\n" pretty-filename "\n")))
            (setq deadgrep--current-file filename)

            (insert pretty-line-num content)
            (when truncate-p
              (insert
               (propertize " ... (truncated)"
                           'face 'font-lock-comment-face)))
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
        (unless (equal output "finished\n")
          (save-excursion
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert output))))))))

(defun deadgrep--process-filter (process output)
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
  (rx bos "\x1b[0m\x1b[35m" (group (+? anything)) "\x1b[")
  "Extracts the filename from a ripgrep line with ANSI color sequences.
We use the color sequences to extract the filename exactly, even
if the path contains colons.")

(defconst deadgrep--line-num-regexp
  (rx "\x1b[32m" (group (+ digit)))
  "Extracts the line number from a ripgrep line with ANSI color sequences.
Ripgrep uses a unique color for line numbers, so we use that to
extract the linue number exactly.")

(defconst deadgrep--line-contents-regexp
  (rx "\x1b[32m" (+ digit) "\x1b[0m" ":" (group (+ anything)))
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
      'face 'match))
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

(defun deadgrep--type-list ()
  "Query the rg executable for available file types."
  (let* ((output (shell-command-to-string (format "%s --type-list" deadgrep-executable)))
         (lines (s-lines (s-trim output)))
         (types (--map
                 (-first-item (s-split (rx ":") it))
                 lines)))
    types))

(define-button-type 'deadgrep-file-type
  'action #'deadgrep--file-type
  'case nil
  'help-echo "Change case sensitivity")

(defun deadgrep--file-type (button)
  (let ((button-type (button-get button 'file-type)))
    (cond
     ((eq button-type 'all)
      (setq deadgrep--file-type 'all))
     ((eq button-type 'type)
      (let ((new-file-type
             (completing-read "File type: " (deadgrep--type-list))))
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

(defun deadgrep--format-command (search-term search-type case)
  (format
   "%s --color=ansi --no-heading --with-filename %s %s %s -- %s"
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
   (shell-quote-argument search-term)))

(defun deadgrep--write-heading ()
  "Write the deadgrep heading with buttons reflecting the current
search settings."
  (insert (propertize "Search term: "
                      'face 'font-lock-comment-face)
          (if (eq deadgrep--search-type 'regexp)
              (deadgrep--propertize-regexp deadgrep--search-term)
            deadgrep--search-term)
          " "
          (deadgrep--button "change" 'deadgrep-search-term)
          "\n"
          (propertize "Search type: "
                      'face 'font-lock-comment-face)

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
                      'face 'font-lock-comment-face)
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

          "\n\n"
          (propertize "Directory: "
                      'face 'font-lock-comment-face)
          (deadgrep--button
           (abbreviate-file-name default-directory)
           'deadgrep-directory)
          "\n"
          (propertize "Files: "
                      'face 'font-lock-comment-face)
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
          "\n\n"))

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
         ;; TODO: I've seen a more appropriate face in some themes,
         ;; find out what to use instead here.
         'font-lock-constant-face
         regexp))
       ((and (memq it escape-metachars) (equal prev-char ?\\))
        (put-text-property
         (1- it-index) (1+ it-index)
         'face 'font-lock-constant-face
         regexp)))

      (setq prev-char it)))
  regexp)

(defun deadgrep--buffer-name (search-term directory)
  (format "*deadgrep %s %s*"
          search-term
          (abbreviate-file-name directory)))

(defun deadgrep--buffer (search-term directory)
  (let* ((initial-filename (buffer-file-name))
         (buf-name (deadgrep--buffer-name search-term directory))
         (buf (get-buffer buf-name)))
    (unless buf
      ;; If we need to create the buffer, ensure we don't exceed
      ;; `deadgrep-max-buffers' by killing the least recently used.
      (when (numberp deadgrep-max-buffers)
        (let* ((buffers (buffer-list))
               (helpful-bufs (--filter (with-current-buffer it
                                         (eq major-mode 'deadgrep-mode))
                                       buffers))
               ;; `buffer-list' seems to be ordered by most recently
               ;; visited first, so keep those.
               (excess-buffers (-drop (1- deadgrep-max-buffers) helpful-bufs)))
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
        (setq deadgrep--initial-filename initial-filename)
        (deadgrep--write-heading))
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

(defun deadgrep-visit-result ()
  "Goto the search result at point."
  (interactive)
  (let* ((pos (line-beginning-position))
         (file-name (get-text-property pos 'deadgrep-filename))
         (line-number (get-text-property pos 'deadgrep-line-number))
         (column-offset (when line-number (deadgrep--current-column))))
    (when file-name
      (find-file file-name)
      (goto-char (point-min))
      (when line-number
        (forward-line (1- line-number))
        (forward-char column-offset)))))

(define-key deadgrep-mode-map (kbd "RET") #'deadgrep-visit-result)
;; TODO: we should still be able to click on buttons.

(define-key deadgrep-mode-map (kbd "g") #'deadgrep-restart)

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
  (let ((process
         (start-process-shell-command
          (format "rg %s" search-term)
          (current-buffer)
          (deadgrep--format-command search-term search-type case))))
    (set-process-filter process #'deadgrep--process-filter)
    (set-process-sentinel process #'deadgrep--process-sentinel)))

(defun deadgrep-restart ()
  "Re-run ripgrep with the current search settings."
  (interactive)
  (let ((start-point (point))
        (inhibit-read-only t))
    (erase-buffer)
    (setq deadgrep--current-file nil)

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
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let* ((sym (symbol-at-point))
           (sym-name (when sym
                       (substring-no-properties (symbol-name sym))))
           ;; TODO: prompt should say search string or search regexp
           ;; as appropriate.
           (prompt
            (if sym
                (format "Search term (default %s): " sym-name)
              "Search term: "))
           (user-input
            (read-from-minibuffer
             prompt nil nil nil nil sym-name)))
      (if (equal user-input "")
          sym-name
        user-input))))

(defun deadgrep--project-root (file-path)
  "Guess the project root of the given FILE-PATH."
  (or
   (ignore-errors
     ;; This raises an error if we're not in a project.
     (projectile-project-root))
   file-path))

;;;###autoload
(defun deadgrep ()
  "Start a ripgrep search for SEARCH-TERM."
  (interactive)
  (let* ((search-term (deadgrep--read-search-term))
         (dir (deadgrep--project-root default-directory))
         (buf (deadgrep--buffer search-term dir)))
    (switch-to-buffer buf)
    (deadgrep--start
     search-term
     deadgrep--search-type
     deadgrep--search-case)))

(provide 'deadgrep)
;;; deadgrep.el ends here
