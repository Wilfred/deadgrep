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
