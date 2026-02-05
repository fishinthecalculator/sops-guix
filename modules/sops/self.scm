;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops self)
  #:use-module (ice-9 match))

(define-public (sops-module-name? name)
  "Return true if NAME (a list of symbols) denotes a Guix or sops-guix module."
  (match name
    (('guix _ ...) #t)
    (('gnu _ ...) #t)
    (('sops _ ...) #t)
    (_ #f)))
