;;; deadgrep.el --- fast, friendly searching with ripgrep  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Wilfred Hughes

;; Author: Wilfred Hughes <me@wilfred.me.uk>
;; Keywords: tools

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

(require 's)

(defun deadgrep--process-filter (process output)
  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t)
          (lines (s-lines output)))
      (setq wh/l (car lines))
      (save-excursion
        (goto-char (point-max))
        (dolist (line lines)
          (unless (s-blank? line)
            (setq line (deadgrep--propertize-line line))
            (insert
             (nth 0 line)
             "_"
             (nth 1 line)
             "_"
             (nth 2 line)
             "\n")))))))

(defun deadgrep--propertize-line (line)
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
           (if (evenp it-index)
               it
             (propertize it 'face 'match))
           parts))
         (joined (apply #'concat propertized-parts)))
    joined))

(defun deadgrep--format-command (search-term)
  (format
   "rg --color=ansi --no-heading --with-filename --fixed-strings -- \"%s\""
   (shell-quote-argument search-term)))

(defun deadgrep (search-term)
  "Start a ripgrep search for SEARCH-TERM.

If called with a prefix, create the results buffer without
starting the search."
  (interactive "sSearch term: ")
  (let* ((buf (get-buffer-create "*deadgrep*")))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert "Search term: " search-term "\n\n")
    (let ((process
           (start-process-shell-command
            (format "rg %s" search-term)
            buf
            (deadgrep--format-command search-term))))
      (set-process-filter process #'deadgrep--process-filter))))

(provide 'deadgrep)
;;; deadgrep.el ends here
