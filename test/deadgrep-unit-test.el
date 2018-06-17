(require 'deadgrep)

(ert-deftest deadgrep--propertize-regexp ()
  ;; Plain text.
  (let ((result (deadgrep--propertize-regexp "foo")))
    (should
     (not
      (eq (get-text-property 0 'face result)
          'font-lock-constant-face))))
  ;; Regexp metacharacter.
  (let ((result (deadgrep--propertize-regexp ".")))
    (should
     (eq (get-text-property 0 'face result)
         'font-lock-constant-face)))
  ;; Escaped metacharacter.
  (let ((result (deadgrep--propertize-regexp "\\.")))
    (should
     (not
      (eq (get-text-property 1 'face result)
          'font-lock-constant-face)))))

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
