;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (sops-secrets-sh-command-wrapper))

(define (sops-secrets-sh-command-wrapper secrets-specs command)
  "Returns a POSIX sh command exporting each one of the SECRETS-SPECS, a list of
lists or pairs whose first element is the name of an environment variable secret
and whose second as an environment variable, before executing COMMAND, a list of
strings representing a POSIX shell command."
  (string-join
   `("set -e"
     ,@(map (match-lambda
              ((variable secret)
               (string-append
                "export " variable "=\"$(cat " secret ")\"")))
            secrets-specs)
     ,(string-append "exec -a " (first command) " "
                     (string-join command " ")))
   "; "))
