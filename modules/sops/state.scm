;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops state)
  #:use-module (guix records)
  #:export (<sops-runtime-state>
            sops-runtime-state
            sops-runtime-state?
            sops-runtime-state-age-key-file
            sops-runtime-state-gnupg-home
            sops-runtime-state-secrets
            sops-runtime-state-sops
            sops-runtime-state-gpg-command
            sops-runtime-state-host-ssh-key
            sops-runtime-state-secrets-directory
            sops-runtime-state-generate-key?
            sops-runtime-state-verbose?))

;; This is an intermediate representation necessary because of bad design
;; decisions on the public API. The home service and the system service should
;; have the same configuration record. This record type addresses this
;; shortcoming.
(define-record-type* <sops-runtime-state>
  sops-runtime-state
  make-sops-runtime-state
  sops-runtime-state?
  this-sops-runtime-state

  (age-key-file             sops-runtime-state-age-key-file)
  (gnupg-home               sops-runtime-state-gnupg-home)
  (secrets                  sops-runtime-state-secrets)
  (sops                     sops-runtime-state-sops)
  (gpg-command              sops-runtime-state-gpg-command)
  (host-ssh-key             sops-runtime-state-host-ssh-key
                            (default "/etc/ssh/ssh_host_rsa_key"))
  (secrets-directory        sops-runtime-state-secrets-directory
                            (default #f))
  (generate-key?            sops-runtime-state-generate-key?
                            (default #f))
  (verbose?                 sops-runtime-state-verbose?
                            (default #f)))
