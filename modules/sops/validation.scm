;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops validation)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-35)
  #:export (gexp-or-file-like?
            string-or-file-like?))

(define (gexp-or-file-like? value)
  (if (or (file-like? value) (gexp? value))
      value
      (raise
       (formatted-message
        (G_ "config field must contain only gexps or file-like objects,
but ~a was found")
        value))))

(define (string-or-file-like? value)
  (if (or (file-like? value)
          (gexp? value)
          (string? value))
      value
      (raise
       (formatted-message
        (G_ "file field must contain only gexps, strings or file-like
objects, but ~a was found")
        value))))
