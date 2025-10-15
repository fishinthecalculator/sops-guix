;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops validation)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:export (gexp-or-file-like?))

(define (gexp-or-file-like? value)
  (if (or (file-like? value) (gexp? value))
      value
      (raise
       (formatted-message
        (G_ "file field value must contain only gexps or file-like objects,
but ~a was found")
        value))))
