;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024-2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops activation)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (sops derive)
  #:use-module (sops secrets)
  #:use-module (sops self)
  #:use-module (sops state)
  #:export (activate-secrets
            generate-ssh-key))

(define (generate-ssh-key runtime-state)
  "Return an activation gexp for generating the host key used by the SOPS
service."
  (match-record runtime-state <sops-runtime-state>
                (age-key-file gnupg-home gpg-command host-ssh-key
                 generate-key? verbose?)
    (with-imported-modules (source-module-closure
                            '((sops build activation))
                            #:select? sops-module-name?)
        #~(begin
            (use-modules (guix build utils)
                         (sops build activation))

            (if #$generate-key?
                (if (file-exists? #$host-ssh-key)
                    (invoke #$(generate-host-key age-key-file
                                                 gnupg-home
                                                 gpg-command
                                                 #:host-ssh-key host-ssh-key
                                                 #:verbose? verbose?))
                    (format
                     (current-error-port)
                     "'~a' does not exist so no host key can be generated...~%"
                     #$host-ssh-key))
                (format
                 (current-error-port) "no host key will be generated...~%"))))))

(define (activate-secrets runtime-state)
  "Return an activation gexp for provided secrets."
  (match-record runtime-state <sops-runtime-state>
                (age-key-file gnupg-home secrets sops gpg-command
                 secrets-directory verbose?)
    (let ((sops-secrets
           (map lower-sops-secret secrets))
          (sops-command
           (file-append sops "/bin/sops")))

      (with-imported-modules (source-module-closure
                              '((sops build activation))
                              #:select? sops-module-name?)
        #~(begin
            (use-modules (guix build utils)
                         (sops build activation))

            (define-values (secrets-directory extra-links-directory)
              (sops-secrets-directories #$secrets-directory))

            (sops-secrets-setenv #$age-key-file #$gnupg-home #$gpg-command
                                 #:verbose? #$verbose?)

            (format
             (current-error-port)
             "Setting up secrets in '~a'...~%" secrets-directory)

            ;; Cleanup old secrets.
            (sops-secrets-cleanup secrets-directory extra-links-directory)

            ;; Create new secrets.
            (sops-secrets-create
             #$sops-command (list #$@sops-secrets)
             secrets-directory extra-links-directory
             #:verbose? #$verbose?))))))
