;;; deadgrep.el --- fast, friendly searching with ripgrep  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: tools
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12.0") (s "1.11.0") (spinner "1.7.3"))

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
;; emacs. This is a bespoke mode that does not rely on
;; compilation-mode, but tries to be a perfect fit for ripgrep.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'spinner)

(defvar-local deadgrep--search-term nil)
(defvar-local deadgrep--search-type 'literal)
(defvar-local deadgrep--search-case 'smart)
(defvar-local deadgrep--file-type 'all)

(defvar-local deadgrep--current-file nil)
(defvar-local deadgrep--spinner nil)
(defvar-local deadgrep--remaining-output nil
  "We can't guarantee that our process filter will always receive whole lines.
We save the last line here, in case we need to append more text to it.")

(defconst deadgrep--position-column-width 5)

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
        (unless (s-blank? line)
          (-let* (((filename line-num content) (deadgrep--split-line line))
                  (formatted-line-num
                   (s-pad-right deadgrep--position-column-width " " line-num))
                  (pretty-line-num
                   (propertize formatted-line-num
                               'face 'font-lock-comment-face
                               'deadgrep-filename filename
                               'deadgrep-line-number (string-to-number line-num))))
            (setq filename (propertize filename 'face 'bold))
            (cond
             ;; This is the first file we've seen, print the heading.
             ((null deadgrep--current-file)
              (insert filename "\n"))
             ;; This is a new file, print the heading with a spacer.
             ((not (equal deadgrep--current-file filename))
              (insert "\n" filename "\n")))
            (setq deadgrep--current-file filename)

            (insert pretty-line-num content "\n")))))))

(defun deadgrep--process-sentinel (process output)
  "Update the ag buffer associated with PROCESS as complete."
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

(defun deadgrep--split-line (line)
  "Given a raw LINE of output from rg, apply properties."
  (let* ((parts (s-split (rx (1+ "\x1b[" (+ digit) "m")) line))
         (filename (nth 1 parts))
         (line-num (nth 3 parts))
         (line-content-parts (-drop 4 parts))
         ;; The very first part includes a colon, remove that.
         (line-content-start
          (substring (car line-content-parts) 1)))
    (setq line-content-parts
          (cons line-content-start
                (-drop 1 line-content-parts)))

    (list filename line-num
          (deadgrep--propertize-hits line-content-parts))))

;; TODO: this doesn't work with \d when there are consecutive digits.
(defun deadgrep--propertize-hits (parts)
  "Given a list of PARTS, where every other part is a hit,
join the parts into one string with hit highlighting."
  (let* ((propertized-parts
          (--map-indexed
           (if (cl-evenp it-index)
               it
             (propertize it 'face 'match))
           parts))
         (joined (apply #'concat propertized-parts)))
    joined))

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
   (deadgrep--buffer-name deadgrep--search-term default-directory))
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

(define-button-type 'deadgrep-file-type
  'action #'deadgrep--file-type
  'case nil
  'help-echo "Change case sensitivity")

(defun deadgrep--file-type (button)
  (let ((file-type (button-get button 'file-type)))
    (cond
     ((eq file-type 'all)
      (setq deadgrep--file-type file-type))
     ((eq file-type 'type)
      (setq deadgrep--file-type (cons file-type "elisp")))
     (t
      (error "unknown file type: %S" file-type))))
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
   "rg --color=ansi --no-heading --with-filename %s %s %s -- %s"
   (cond
    ((eq search-type 'literal)
     "--fixed-strings")
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
    (t
     (error "Unknown file-type: %S" deadgrep--file-type)))
   (shell-quote-argument search-term)))

(defun deadgrep--write-heading ()
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

          (if (eq deadgrep--search-type 'literal)
              "literal"
            (deadgrep--button "literal" 'deadgrep-type
                              'search-type 'literal))
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
          (if (eq (car-safe deadgrep--file-type) 'type)
              (format "type:%s" (cdr deadgrep--file-type))
            (deadgrep--button "type" 'deadgrep-file-type
                              'file-type 'type))
          " extension"

          "\n\n"))

;; TODO: could we do this in the minibuffer too?
(defun deadgrep--propertize-regexp (regexp)
  "Given a string REGEXP representing a search term with regular
expression syntax, highlight the metacharacters.
Returns a copy of REGEXP with properties set."
  (setq regexp (copy-sequence regexp))

  ;; TODO: see https://docs.rs/regex/1.0.0/regex/#syntax
  (let ((metachars
         '(?\( ?\) ?\[ ?\] ?\{ ?\} ?| ?. ?+ ?*))
        (prev-char nil))
    (--each-indexed (string-to-list regexp)
      (when (and (memq it metachars) (not (equal prev-char ?\\)))
        (put-text-property
         it-index (1+ it-index)
         'face
         ;; TODO: I've seen a more appropriate face in some themes,
         ;; find out what to use instead here.
         'font-lock-constant-face
         regexp))
      (setq prev-char it)))
  regexp)

(defun deadgrep--buffer-name (search-term directory)
  (format "*deadgrep %s %s*"
          search-term
          (abbreviate-file-name directory)))

(defun deadgrep--buffer (search-term directory)
  (let* ((buf (get-buffer-create
               (deadgrep--buffer-name search-term directory))))
    (with-current-buffer buf
      (setq default-directory directory)
      (let ((inhibit-read-only t))
        ;; This needs to happen first, as it clobbers all buffer-local
        ;; variables.
        (deadgrep-mode)
        (erase-buffer)

        (setq deadgrep--search-term search-term)
        (setq deadgrep--current-file nil)
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
         (column-offset (deadgrep--current-column)))
    (when file-name
      (find-file file-name)
      (goto-char (point-min))
      (forward-line (1- line-number))
      (forward-char column-offset))))

(define-key deadgrep-mode-map (kbd "RET") #'deadgrep-visit-result)
;; TODO: we should still be able to click on buttons.
(define-key deadgrep-mode-map (kbd "<mouse-2>") #'deadgrep-visit-result)

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
    (while (deadgrep--item-p (1- pos))
      (cl-decf pos))
    (goto-char pos)))

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

(define-key deadgrep-mode-map (kbd "TAB") #'deadgrep-forward)
(define-key deadgrep-mode-map (kbd "<backtab>") #'deadgrep-backward)

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
If region is active, return that immediately. Otherwise, offer
the current word as a default."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let* ((sym (symbol-at-point))
           ;; TODO: this can lead to unwanted syntax highlighting. We
           ;; should always highlight in a consistent face.
           (sym-name (when sym (symbol-name sym)))
           ;; TODO: prompt should say search string or search regexp
           ;; as appropriate.
           (prompt
            (if sym
                (format "Search term (default %s): " sym)
              "Search term: "))
           (user-input
            (read-from-minibuffer
             prompt nil nil nil nil sym-name)))
      (if (equal user-input "")
          sym-name
        user-input))))

;;;###autoload
(defun deadgrep ()
  "Start a ripgrep search for SEARCH-TERM.

TODO: If called with a prefix, create the results buffer without
starting the search."
  (interactive)
  (let* ((search-term (deadgrep--read-search-term))
         (buf (deadgrep--buffer search-term default-directory)))
    (switch-to-buffer buf)
    (deadgrep--start
     search-term
     deadgrep--search-type
     deadgrep--search-case)))

(provide 'deadgrep)
;;; deadgrep.el ends here
