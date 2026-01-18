;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops tests)
  #:use-module (guix discovery)
  #:use-module ((guix ui) #:select (warn-about-load-error))
  #:use-module ((gnu tests) #:select (system-test?))
  #:export (all-system-tests))

(define (test-modules)
  "Return the list of modules that define system tests."
  (scheme-modules (dirname (dirname (dirname (search-path %load-path "sops/services/sops.scm"))))
                  "sops/tests"
                  #:warn warn-about-load-error))

(define (fold-system-tests proc seed)
  "Invoke PROC on each system test, passing it the test and the previous
result."
  (fold-module-public-variables (lambda (obj result)
                                  (if (system-test? obj)
                                      (cons obj result)
                                      result))
                                '()
                                (test-modules)))

(define (all-system-tests)
  "Return the list of system tests."
  (reverse (fold-system-tests cons '())))
