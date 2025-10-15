;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <therewasa@fishinthecalculator.me>

;; Test runner. This has been partly copied from guile-sparql.

(define-module (test-sops)
  #:use-module (sops secrets)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match)
  #:export (with-test-group))

(define (sops:test-runner)
  (let ((runner (test-runner-null))
        (num-passed 0)
        (num-failed 0))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (format #t "[~a] line:~a, test: ~a\n"
                (test-result-ref runner 'result-kind)
                (test-result-ref runner 'source-line)
                (test-runner-test-name runner))
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (1+ num-passed)))
          ((fail xfail)
           (if (test-result-ref runner 'expected-value)
               (format #t "~a\n -> expected: ~s\n -> obtained: ~s\n"
                       (string-join (test-runner-group-path runner) "/")
                       (test-result-ref runner 'expected-value)
                       (test-result-ref runner 'actual-value)))
           (set! num-failed (1+ num-failed)))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format #t "Source: ~a\npass = ~a, fail = ~a\n"
                (test-result-ref runner 'source-file) num-passed num-failed)
        (zero? num-failed)))
    runner))

(define-syntax-rule (with-test-group name . expr)
    (begin (test-begin name) expr (test-end name)))

(test-runner-factory sops:test-runner)

(with-test-group "key->file-name"
  (lambda _
    (let ((results '(("[\"private\"]" "private")
                     ("[632]" "632")
                     ("[3][\"private\"]" "3/private")
                     ("[\"wireguard\"][18][\"private\"]" "wireguard/18/private"))))
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
