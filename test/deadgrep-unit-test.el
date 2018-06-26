(require 'deadgrep)

(ert-deftest deadgrep--propertize-regexp ()
  ;; Plain text.
  (let ((result (deadgrep--propertize-regexp "foo")))
    (should
     (not
      (eq (get-text-property 0 'face result)
          'font-lock-constant-face))))
  ;; Regexp metacharacters
  (let ((result (deadgrep--propertize-regexp "^.?$")))
    (dotimes (i (length result))
      (should
       (eq (get-text-property i 'face result)
           'font-lock-constant-face))))
  ;; Escaped metacharacter.
  (let ((result (deadgrep--propertize-regexp "\\.")))
    (should
     (not
      (eq (get-text-property 1 'face result)
          'font-lock-constant-face))))
  ;; Backslash escape sequence.
  (let ((result (deadgrep--propertize-regexp "\\b")))
    (should
     (eq (get-text-property 0 'face result)
         'font-lock-constant-face))
    (should
     (eq (get-text-property 1 'face result)
         'font-lock-constant-face))))

(ert-deftest deadgrep-smoke-test ()
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (&rest _args) "foo")))
    (deadgrep)))

(ert-deftest deadgrep-forward ()
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (&rest _args) "a-unique-string")))
    (deadgrep)

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
     (deadgrep--item-p (point)))))

(ert-deftest deadgrep-backward ()
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (&rest _args) "a-unique-string")))
    (deadgrep)

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
     (deadgrep--item-p (point)))))

(ert-deftest deadgrep--split-line ()
  (-let* ((raw-line
           "[0m[35mdeadgrep.el[0m:[0m[32m123[0m:    (when ([0m[31m[1mbuffer-live[0m-p buffer)")
          ((filename line-num _) (deadgrep--split-line raw-line)))
    (should
     (equal filename "deadgrep.el"))
    (should
     (equal line-num 123))))

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
     (eq (get-text-property 0 'face line) 'match)))
  ;; Some users are seeing color codes in a different order. Ensure we
  ;; handle that too.
  (let* ((raw-line "[0m[1m[31mfoo[0m bar")
         (line (deadgrep--propertize-hits raw-line)))
    (should
     (eq (get-text-property 0 'face line) 'match))))

(ert-deftest deadgrep--split-line--consecutive ()
  "Ensure we correctly handle immediately consecutive results."
  (-let* ((raw-line
           "[0m[35mdeadgrep.el[0m:[0m[32m379[0m:  ;; see https://docs.rs/regex/[0m[31m[1m1.[0m[0m[31m[1m0.[0m0/regex/#syntax")
          ((_ _ line) (deadgrep--split-line raw-line)))
    (should
     (eq (get-text-property 31 'face line) 'match))
    (should
     (eq (get-text-property 33 'face line) 'match))))
