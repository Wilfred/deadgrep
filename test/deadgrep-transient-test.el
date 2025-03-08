(require 'deadgrep-transient)

(defconst deadgrep-transient-test-base-args
  '("--no-config" "--color=ansi" "--line-number" "--no-heading" "--no-column" "--with-filename")
  "Base arguments for `deadgrep-transient' test.")

(defun deadgrep-transient-test-create-option (command &optional value)
  (let ((infix (transient-option :command command)))
    (when value (oset infix value value))
    infix))

(defun deadgrep-transient-test-create-switch (command &optional value)
  (let ((infix (transient-switch :command command)))
    (when value (oset infix value value))
    infix))

(defun deadgrep-transient-test-create-switches (command &optional value)
  (let ((infix (transient-switches :command command)))
    (when value (oset infix value value))
    infix))

(ert-deftest deadgrep-transient--set-options-with-default ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--after-context)
      (deadgrep-transient-test-create-option 'deadgrep-transient:--before-context)
      (deadgrep-transient-test-create-switch 'deadgrep-transient:--fixed-strings)
      (deadgrep-transient-test-create-switch 'deadgrep-transient:--word-regexp)
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--*-case)
      (deadgrep-transient-test-create-option 'deadgrep-transient:--glob)
      (deadgrep-transient-test-create-option 'deadgrep-transient:--type)
      (deadgrep-transient-test-create-switch 'deadgrep-transient:--hidden)
      (deadgrep-transient-test-create-switch 'deadgrep-transient:--no-ignore-vcs)))

    (should (null deadgrep--context))
    (should (equal deadgrep--search-type 'regexp))
    (should (equal deadgrep--search-case 'sensitive))
    (should (eq deadgrep--file-type 'all))
    (should (eq deadgrep--skip-if-hidden t))
    (should (eq deadgrep--skip-if-vcs-ignore t))))

(ert-deftest deadgrep-transient--set-options-with-context ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--before-context "2")))
    (should (equal deadgrep--context '(2 . 0)))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--after-context "1")))
    (should (equal deadgrep--context '(0 . 1)))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--before-context "4")
      (deadgrep-transient-test-create-option 'deadgrep-transient:--after-context "3")))
    (should (equal deadgrep--context '(4 . 3)))))

(ert-deftest deadgrep-transient--set-options-with-search-type ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--fixed-strings
                                               "--fixed-strings")))
    (should (equal deadgrep--search-type 'string))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--fixed-strings
                                               "--fixed-strings")
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--word-regexp
                                               "--word-regexp")))
    (should (equal deadgrep--search-type 'words))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--word-regexp
                                               "--word-regexp")))
    ;; no equivalent value in `deadgrep--search-type'.
    (should (equal deadgrep--search-type 'regexp))))

(ert-deftest deadgrep-transient--set-options-with-case ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--*-case
                                               "--case-sensitive")))
    (should (equal deadgrep--search-case 'sensitive))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--*-case
                                               "--ignore-case")))
    (should (equal deadgrep--search-case 'ignore))

    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switches 'deadgrep-transient:--*-case
                                               "--smart-case")))
    (should (equal deadgrep--search-case 'smart))))

(ert-deftest deadgrep-transient--set-options-with-glob ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--glob '("*.el"))))
    (should (equal deadgrep--file-type '(glob . "*.el")))))

(ert-deftest deadgrep-transient--set-options-with-type ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-option 'deadgrep-transient:--type "elisp")))
    (should (equal deadgrep--file-type '(type . "elisp")))))

(ert-deftest deadgrep-transient--set-options-with-hidden ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switch 'deadgrep-transient:--hidden t)))
    (should (not deadgrep--skip-if-hidden))))

(ert-deftest deadgrep-transient--set-options-with-no-ignore-vcs ()
  (with-temp-buffer
    (deadgrep-transient--set-options
     (list
      (deadgrep-transient-test-create-switch 'deadgrep-transient:--no-ignore-vcs t)))
    (should (not deadgrep--skip-if-vcs-ignore))))

(ert-deftest deadgrep-transient--arguments-with-default ()
  (with-temp-buffer
    (let ((deadgrep-extra-arguments '("--no-config")))
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--hidden"
                        "--glob=!/.git"
                        "--smart-case"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))

(ert-deftest deadgrep-transient--arguments-with-after-context ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--after-context)
                            transient-current-suffixes)))
      (oset suffix value "3")
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--after-context=3"
                        "--hidden"
                        "--glob=!/.git"
                        "--smart-case"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))
(ert-deftest deadgrep-transient--arguments-with-before-context ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--before-context)
                            transient-current-suffixes)))
      (oset suffix value "10")
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--before-context=10"
                        "--hidden"
                        "--glob=!/.git"
                        "--smart-case"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))

(ert-deftest deadgrep-transient--arguments-without-fixed-strings ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--fixed-strings)
                            transient-current-suffixes)))
      (oset suffix value nil)
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--hidden"
                        "--glob=!/.git"
                        "--smart-case"
                        "--"
                        "foo"
                        ".")))))))

(ert-deftest deadgrep-transient--arguments-with-word-regexp ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--word-regexp)
                            transient-current-suffixes)))
      (oset suffix value "--word-regexp")
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--hidden"
                        "--glob=!/.git"
                        "--smart-case"
                        "--word-regexp"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))

(ert-deftest deadgrep-transient--arguments-with-type ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--type)
                            transient-current-suffixes)))
      (oset suffix value "org")
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--hidden"
                        "--glob=!/.git"
                        "--type=org"
                        "--smart-case"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))

(ert-deftest deadgrep-transient--arguments-with-case-sensitive ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--*-case)
                            transient-current-suffixes)))
      (oset suffix value "--case-sensitive")
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--hidden"
                        "--glob=!/.git"
                        "--case-sensitive"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))

(ert-deftest deadgrep-transient--arguments-with-ignore-case ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--*-case)
                            transient-current-suffixes)))
      (oset suffix value "--ignore-case")
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--hidden"
                        "--glob=!/.git"
                        "--ignore-case"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))

(ert-deftest deadgrep-transient--arguments-with-glob ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--glob)
                            transient-current-suffixes)))
      (oset suffix value (list "*.org"))
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--hidden"
                        "--glob=*.org"
                        "--smart-case"
                        "--fixed-strings"
                        "--"
                        "foo"
                        "."))))

      (oset suffix value (list "*.el" "*.org"))
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--hidden"
                        "--glob=*.org"
                        "--glob=*.el"
                        "--smart-case"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))

(ert-deftest deadgrep-transient--arguments-without-hidden ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--hidden)
                            transient-current-suffixes)))
      (oset suffix value nil)
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--glob=!/.git"
                        "--smart-case"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))

(ert-deftest deadgrep-transient--arguments-with-no-ignore-vcs ()
  (with-temp-buffer
    (let* ((transient-current-suffixes (transient-suffixes 'deadgrep-transient-menu))
           (transient-current-command 'deadgrep-transient-menu)
           (deadgrep-extra-arguments '("--no-config"))
           (suffix (--first (eq (oref it command) 'deadgrep-transient:--no-ignore-vcs)
                            transient-current-suffixes)))
      (oset suffix value "--no-ignore-vcs")
      (should (equal (deadgrep-transient--arguments "foo")
                     (append
                      deadgrep-transient-test-base-args
                      '("--no-ignore-vcs"
                        "--hidden"
                        "--glob=!/.git"
                        "--smart-case"
                        "--fixed-strings"
                        "--"
                        "foo"
                        ".")))))))
