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
	    (:report-file "/tmp/undercover-report.json"))

(defun deadgrep--button-label ()
  "Get button label at point, or nil."
  (let ((button (button-at (point))))
    (when button
      (button-label button))))

;;; test-helper.el ends here
