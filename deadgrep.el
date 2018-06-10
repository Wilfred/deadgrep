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

;; 

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'spinner)

(defvar-local deadgrep--current-file nil)
(defvar-local deadgrep--spinner nil)
(defvar-local deadgrep--remaining-output nil
  "We can't guarantee that our process filter will always receive whole lines.
We save the last line here, in case we need to append more text to it.")

(defun deadgrep--process-sentinel (process string)
  "Update the ag buffer associated with PROCESS as complete."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; The remaining output must now be a completed line.
        (let ((inhibit-read-only t))
          nil)))))

(defun deadgrep--process-filter (process output)
  ;; If we had an unfinished line from our last call, include that.
  (when deadgrep--remaining-output
    (setq output (concat deadgrep--remaining-output output))
    (setq deadgrep--remaining-output nil))

  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t)
          (lines (s-lines output)))
      ;; Process filters run asynchronously, and don't guarantee that
      ;; OUTPUT ends with a complete line. Save the last line for
      ;; later processing.
      (setq deadgrep--remaining-output (-last-item lines))
      (setq lines (butlast lines))

      (save-excursion
        (goto-char (point-max))
        (dolist (line lines)
          (unless (s-blank? line)
            (-let [(filename line-num content) (deadgrep--split-line line)]
              (cond
               ;; This is the first file we've seen, print the heading.
               ((null deadgrep--current-file)
                (insert filename "\n"))
               ;; This is a new file, print the heading with a spacer.
               ((not (equal deadgrep--current-file filename))
                (insert "\n" filename "\n")))
              (setq deadgrep--current-file filename)

              (insert
               (propertize
                (s-pad-right 5 " " line-num)
                'face 'font-lock-comment-face)
               content
               "\n"))))))))

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

(defun deadgrep--format-command (search-term)
  (format
   "rg --color=ansi --no-heading --with-filename --fixed-strings -- \"%s\""
   (shell-quote-argument search-term)))

(defun deadgrep--buffer (search-term directory)
  (let* ((buf (get-buffer-create "*deadgrep*")))
    (with-current-buffer buf
      (setq default-directory directory)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Search term: " search-term "\n"
                "Directory: "
                (abbreviate-file-name default-directory)
                "\n\n"))
      (setq buffer-read-only t)
      (setq deadgrep--current-file nil)
      (setq deadgrep--spinner (spinner-create 'progress-bar t))
      (spinner-start deadgrep--spinner)
      (deadgrep-mode))
    buf))

(define-derived-mode deadgrep-mode special-mode
  '("Deadgrep" (:eval (spinner-print deadgrep--spinner))))

(defun deadgrep (search-term)
  "Start a ripgrep search for SEARCH-TERM.

If called with a prefix, create the results buffer without
starting the search."
  (interactive "sSearch term: ")
  (let* ((buf (deadgrep--buffer search-term default-directory)))
    (switch-to-buffer buf)
    (let ((process
           (start-process-shell-command
            (format "rg %s" search-term)
            buf
            (deadgrep--format-command search-term))))
      (set-process-filter process #'deadgrep--process-filter)
      (set-process-sentinel process #'deadgrep--process-sentinel))))

(provide 'deadgrep)
;;; deadgrep.el ends here
