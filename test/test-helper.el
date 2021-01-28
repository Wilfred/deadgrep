;;; test-helper.el --- Helper for tests              -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Wilfred Hughes

;; Author:  <me@wilfred.me.uk>

;;; Code:

(require 'ert)
(require 'f)

(let ((deadgrep-dir (f-parent (f-dirname (f-this-file)))))
  (add-to-list 'load-path deadgrep-dir))

(require 'undercover)
(undercover "deadgrep.el"
	    (:exclude "*-test.el")
            (:report-format 'codecov))

;;; test-helper.el ends here
