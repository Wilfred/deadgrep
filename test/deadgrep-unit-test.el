(require 'deadgrep)

(ert-deftest deadgrep--propertize-regexp ()
  ;; Plain text.
  (let ((result (deadgrep--propertize-regexp "foo")))
    (should
     (not
      (eq (get-text-property 0 'face result)
          'deadgrep-regexp-metachar-face))))
  ;; Regexp metacharacters
  (let ((result (deadgrep--propertize-regexp "^.?$")))
    (dotimes (i (length result))
      (should
       (eq (get-text-property i 'face result)
           'deadgrep-regexp-metachar-face))))
  ;; Escaped metacharacter.
  (let ((result (deadgrep--propertize-regexp "\\.")))
    (should
     (not
      (eq (get-text-property 1 'face result)
          'deadgrep-regexp-metachar-face)))))

(ert-deftest deadgrep--propertize-regexp--backslash ()
  (let ((result (deadgrep--propertize-regexp "\\b")))
    (should
     (eq (get-text-property 0 'face result)
         'deadgrep-regexp-metachar-face))
    (should
     (eq (get-text-property 1 'face result)
         'deadgrep-regexp-metachar-face))))

(ert-deftest deadgrep-smoke-test ()
  (deadgrep "foo"))

(ert-deftest deadgrep-forward ()
  (let ((current-prefix-arg t))
    (deadgrep "foo"))

  ;; Smoke test.
  (deadgrep-forward)

  ;; Moving forward, when point is already on the last item, should
  ;; not error.
  (goto-char (point-max))
  (deadgrep-forward)

  ;; We should end up with point on an item.
  (goto-char (point-min))
  (deadgrep-forward)
  (should
   (deadgrep--item-p (point))))

(ert-deftest deadgrep-forward-filename ()
  (deadgrep "foo")

  (sleep-for 0.5)

  ;; Smoke test.
  (deadgrep-forward-filename)

  ;; Moving forward, when point is already on the last item should signal.
  (goto-char (point-max))
  (should-error
   (deadgrep-forward-filename)
   :type 'end-of-buffer)

  ;; We should end up with point on an item.
  (goto-char (point-min))
  (deadgrep-forward-filename)

  (should
   (deadgrep--filename-p (point))))

(ert-deftest deadgrep-backward ()
  (let ((current-prefix-arg t))
    (deadgrep "foo"))

  ;; Smoke test.
  (goto-char (point-max))
  (deadgrep-backward)

  ;; Moving backward, when point is already on the first item, should
  ;; not error.
  (goto-char (point-min))
  (deadgrep-backward)

  ;; We should end up with point on an item.
  (goto-char (point-max))
  (deadgrep-backward)
  (should
   (deadgrep--item-p (point))))

(ert-deftest deadgrep-backward-filename ()
  (deadgrep "foo")

  (sleep-for 0.5)

  ;; Smoke test.
  (goto-char (point-max))
  (deadgrep-backward-filename)

  ;; Moving backward, when point is already on the first item should signal.
  (goto-char (point-min))
  (should-error
   (deadgrep-backward-filename)
   :type 'beginning-of-buffer)

  ;; We should end up with point on an item.
  (goto-char (point-max))
  (deadgrep-backward-filename)
  (should
   (deadgrep--filename-p (point))))

(ert-deftest deadgrep-forward-match ()
  (let ((current-prefix-arg t))
    (deadgrep "foo"))

  (deadgrep--insert-output "[0m[35m./foo.txt[0m:[0m[32m1[0m:[0m[1m[31mfoo[0mbaz[0m[1m[31mfoo[0mbaz\n")

  (goto-char (point-min))
  (deadgrep-forward-match)

  (should
   (eq (get-text-property (point) 'face)
       'deadgrep-match-face)))

(ert-deftest deadgrep--split-line ()
  (-let* ((raw-line
           "[0m[35mdeadgrep.el[0m:[0m[32m123[0m:    (when ([0m[31m[1mbuffer-live[0m-p buffer)")
          ((filename line-num _) (deadgrep--split-line raw-line)))
    (should
     (equal filename "deadgrep.el"))
    (should
     (equal line-num 123)))
  (-let* ((raw-line
           "[0m[35m./deadgrep.el[0m:[0m[32m123[0m:    (when ([0m[31m[1mbuffer-live[0m-p buffer)")
          ((filename line-num _) (deadgrep--split-line raw-line)))
    (should
     (equal filename "deadgrep.el"))
    (should
     (equal line-num 123))))

(ert-deftest deadgrep--split-line--context ()
  "Ensure we split a line correctly when using -A, -B or -C
context arguments to ripgrep."
  (-let* ((raw-line
           "[0m[35mdeadgrep.el[0m-[0m[32m123[0m-    (when (buffer-live-p buffer)")
          ((filename line-num line) (deadgrep--split-line raw-line)))
    (should
     (equal filename "deadgrep.el"))
    (should
     (equal line-num 123))
    (should
     (equal line "    (when (buffer-live-p buffer)")))
  ;; Context lines can even be empty.
  (-let* ((raw-line
           "[0m[35memr.el[0m-[0m[32m67[0m-")
          ((filename line-num line) (deadgrep--split-line raw-line)))
    (should
     (equal filename "emr.el"))
    (should
     (equal line-num 67))
    (should
     (equal line ""))))

(ert-deftest deadgrep--split-line--windows ()
  (-let* ((raw-line
           "[0m[36mtest\\deadgrep.el[0m:[0m[32m456[0m:    (when ([0m[31m[1mbuffer-live[0m-p buffer)")
          ((filename line-num _) (deadgrep--split-line raw-line)))
    (should
     (equal filename "test\\deadgrep.el"))
    (should
     (equal line-num 456))))

(ert-deftest deadgrep--split-line--propertize ()
  (let* ((raw-line "[0m[31m[1mfoo[0m bar")
         (line (deadgrep--propertize-hits raw-line)))
    (should
     (eq (get-text-property 0 'face line) 'deadgrep-match-face)))
  ;; Some users are seeing color codes in a different order. Ensure we
  ;; handle that too.
  (let* ((raw-line "[0m[1m[31mfoo[0m bar")
         (line (deadgrep--propertize-hits raw-line)))
    (should
     (eq (get-text-property 0 'face line) 'deadgrep-match-face))))

(ert-deftest deadgrep--split-line--consecutive ()
  "Ensure we correctly handle immediately consecutive results."
  (-let* ((raw-line
           "[0m[35mdeadgrep.el[0m:[0m[32m379[0m:  ;; see https://docs.rs/regex/[0m[31m[1m1.[0m[0m[31m[1m0.[0m0/regex/#syntax")
          ((_ _ line) (deadgrep--split-line raw-line)))
    (should
     (eq (get-text-property 31 'face line) 'deadgrep-match-face))
    (should
     (eq (get-text-property 33 'face line) 'deadgrep-match-face))))

(ert-deftest deadgrep--insert-output ()
  "Ensure we can split raw output and insert in a buffer."
  (with-temp-buffer
    (deadgrep--insert-output
     "[0m[35mdeadgrep.el[0m:[0m[32m379[0m:foobar"
     t)
    (should
     (equal
      (buffer-substring-no-properties (point-min) (point-max))
      "deadgrep.el\n379  foobar\n"))))

(ert-deftest deadgrep-debug ()
  "Smoke test."
  (deadgrep-debug))

(ert-deftest deadgrep--type-list ()
  "Smoke test."
  (should
   (member
    '("yaml" ("*.yaml" "*.yml"))
    (deadgrep--type-list))))

(ert-deftest deadgrep-restart ()
  "Smoke test."
  (deadgrep "foo")
  (deadgrep-restart))

(ert-deftest deadgrep--relevant-file-type ()
  ;; Match on extension.
  (should
   (equal
    (deadgrep--relevant-file-type
     "foo.clj"
     '(("clojure" ("*.cljs" "*.clj"))
       ("py" ("*.py"))))
    '("clojure" ("*.cljs" "*.clj"))))
  ;; If there are multiple matches, take the match with the largest
  ;; number of extensions.
  (should
   (equal
    (deadgrep--relevant-file-type
     "foo.ml"
     '(("ml" ("*.ml"))
       ("ocaml" ("*.ml" "*.mli"))))
    '("ocaml" ("*.ml" "*.mli"))))
  ;; If there are duplicates with different names, prefer the longer
  ;; name.
  (should
   (equal
    (deadgrep--relevant-file-type
     "foo.md"
     '(("md" ("*.md"))
       ("markdown" ("*.md"))))
    '("markdown" ("*.md"))))
  ;; Return nil if we have no match or no file.
  (should
   (null
    (deadgrep--relevant-file-type
     "foo.bar"
     '(("clojure" ("*.cljs" "*.clj"))
       ("py" ("*.py"))))))
  (should
   (null
    (deadgrep--relevant-file-type
     nil
     '(("clojure" ("*.cljs" "*.clj"))
       ("py" ("*.py")))))))

(ert-deftest deadgrep--relevant-file-type-elisp ()
  "We should prefer elisp over lisp for .el files."
  (should
   (equal
    (deadgrep--relevant-file-type
     "foo.el"
     '(("elisp" ("*.el"))
       ("lisp" ("*.el" "*.lisp"))))
    '("elisp" ("*.el")))))

(ert-deftest deadgrep--glob-regexp ()
  (should
   (string=
    (deadgrep--glob-regexp "abc")
    "^abc$"))
  (should
   (string=
    (deadgrep--glob-regexp "foo?")
    "^foo.$"))
  (should
   (string=
    (deadgrep--glob-regexp "foo*")
    "^foo.*$"))
  (should
   (string=
    (deadgrep--glob-regexp "[ab]")
    "^[ab]$"))
  (should
   (string=
    (deadgrep--glob-regexp "[a-b]")
    "^[a-b]$"))
  (should
   (string=
    (deadgrep--glob-regexp "[?]")
    "^[?]$")))

(ert-deftest deadgrep--create-imenu-index ()
  (with-temp-buffer
    (deadgrep--insert-output "\
[0m[35mtest/test-helper.el[0m:[0m[32m17[0m:	    (:exclude \"*-[0m[1m[31mtest[0m.el\")
[0m[35mdocs/ALTERNATIVES.md[0m:[0m[32m45[0m:ag.el has a few [0m[1m[31mtest[0ms, but coverage is significantly lower than
[0m[35mdocs/ALTERNATIVES.md[0m:[0m[32m62[0m:**Great for**: if you want a ripgrep tool with excellent [0m[1m[31mtest[0m
[0m[35mtest/deadgrep-unit-test.el[0m:[0m[32m3[0m:(ert-def[0m[1m[31mtest[0m deadgrep--propertize-regexp ()
")
    (should (equal (deadgrep--create-imenu-index)
                   '(("Files" . (("test/test-helper.el" . 1)
                                 ("docs/ALTERNATIVES.md" . 55)
                                 ("test/deadgrep-unit-test.el" . 213))))))))

(ert-deftest deadgrep--lookup-override ()
  (let ((deadgrep-project-root-overrides nil))
    (should
     (equal
      (deadgrep--lookup-override "/foo/bar")
      "/foo/bar")))
  (let ((deadgrep-project-root-overrides
         '(("/foo/bar" . "/overridden"))))
    (should
     (equal
      (deadgrep--lookup-override "/foo/bar")
      "/overridden")))
  (let ((deadgrep-project-root-overrides
         '(("~/foo" . "/overridden"))))
    (should
     (equal
      (deadgrep--lookup-override (expand-file-name "~/foo"))
      "/overridden")))
  (let ((deadgrep-project-root-overrides
         '(("~/foo" . "/overridden"))))
    (should
     (equal
      (deadgrep--lookup-override "~/bar")
      "~/bar"))))

(ert-deftest deadgrep--buffer-position ()
  (with-temp-buffer
    (insert "foo\nbar\n")
    (should
     (equal
      (deadgrep--buffer-position 1 0)
      1))
    ;; We should ignore any narrowing in effect.
    (narrow-to-region (point-min) (1+ (point-min)))
    (should
     (equal
      (deadgrep--buffer-position 2 1)
      6))))

(ert-deftest deadgrep--normalise-dirname--local-paths ()
  (if (eq system-type 'windows-nt)
      (progn
        (should (equal (deadgrep--normalise-dirname "c:/foo/bar") "c:/foo/bar"))
        (should (equal (deadgrep--normalise-dirname "c:/foo/bar/") "c:/foo/bar"))
        (should (equal (deadgrep--normalise-dirname "c:/foo/bar/../baz") "c:/foo/baz")))
    (should (equal (deadgrep--normalise-dirname "/foo/bar") "/foo/bar"))
    (should (equal (deadgrep--normalise-dirname "/foo/bar/") "/foo/bar"))
    (should (equal (deadgrep--normalise-dirname "/foo/bar/../baz") "/foo/baz"))))

(ert-deftest deadgrep--normalise-dirname--remote-paths ()
  (should (equal (deadgrep--normalise-dirname "/pscp:localhost:") "/pscp:localhost:"))
  (should (equal (deadgrep--normalise-dirname "/pscp:localhost:/") "/pscp:localhost:/"))
  (should (equal (deadgrep--normalise-dirname "/pscp:localhost:/foo/bar") "/pscp:localhost:/foo/bar"))
  (should (equal (deadgrep--normalise-dirname "/pscp:localhost:/foo/bar/") "/pscp:localhost:/foo/bar")))

(ert-deftest deadgrep--write-heading--read-only ()
  "Ensure that the heading is read only, so we can't
accidentally edit it."
  (let ((buf (deadgrep--buffer "foo" "/" "blah.el")))
    (with-current-buffer buf
      (deadgrep--write-heading)
      (should
       (get-text-property (point-min) 'read-only)))))

(ert-deftest deadgrep-edit-mode--preserve-variables ()
  "Ensure that we don't clobber local variables when switching to
edit mode."
  (let ((buf (deadgrep--buffer "foo" "/" "blah.el")))
    (with-current-buffer buf
      (deadgrep-edit-mode)
      (should
       (equal deadgrep--search-term "foo")))))
