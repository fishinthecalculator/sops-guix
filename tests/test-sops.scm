;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

;; Test runner. This has been copied from guile-sparql.

(define-module (test-sops)
  #:use-module (sops services sops)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:export (with-test-group))

(define-syntax-rule (with-test-group name . expr)
    (begin (test-begin name) expr (test-end name)))

(with-test-group "key->file-name"
  (lambda _
    (let ((results '(("[\"private\"]" "private")
                     ("[632]" "632")
                     ("[3][\"private\"]" "3-private")
                     ("[\"wireguard\"][18][\"private\"]" "wireguard-18-private"))))
      (for-each
       (match-lambda
         ((input expected)
          (begin
            (test-assert (sanitize-sops-key input))
            (test-equal expected (key->file-name input)))))
       results))

    (let ((failures '("[]"
                      "[\"ciao\""
                      "48]"
                      "[24][\"test\"[89]")))
      (for-each
       (lambda (input)
         (test-error #t (sanitize-sops-key input)))
       failures))))
