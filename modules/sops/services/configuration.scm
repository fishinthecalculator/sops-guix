;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops services configuration)
  #:use-module (guix gexp)
  #:export (gexp-or-string?
            sanitize-gexp-or-string))

(define (gexp-or-string? value)
  (or (file-like? value) (gexp? value) (string? value)))

(define (sanitize-gexp-or-string value)
  (if (gexp-or-string? value)
      value
      (raise
       (formatted-message
        (G_ "command lines can only be Guix gexp records or strings but ~a
was found...") value))))
