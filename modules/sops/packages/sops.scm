;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops packages sops)
  #:use-module (gnu packages password-utils)
  ;; For backward compatibility.
  #:re-export (sops))
