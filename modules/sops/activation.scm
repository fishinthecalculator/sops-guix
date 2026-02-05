;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops activation)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (sops derive)
  #:use-module (sops secrets)
  #:use-module (sops self)
  #:export (activate-secrets))

(define* (activate-secrets config-file
                           age-key-file
                           gnupg-home
                           sops-secrets
                           sops-package
                           gpg-command
                           #:key (host-ssh-key "/etc/ssh/ssh_host_rsa_key")
                           secrets-directory
                           generate-key?
                           verbose?)
  "Return an activation gexp for provided secrets."
  (let ((bash (file-append bash-minimal "/bin/bash"))
        (secrets
         (map lower-sops-secret sops-secrets))
        (sops
         (file-append sops-package "/bin/sops")))

    (with-imported-modules (source-module-closure
                            '((sops build activation))
                            #:select? sops-module-name?)
      #~(begin
          (use-modules (guix build utils)
                       (sops build activation))

          (define age-key-file #$age-key-file)
          (define gnupg-home #$gnupg-home)

          (define-values (secrets-directory extra-links-directory)
            (sops-secrets-directories #$secrets-directory))

          (sops-secrets-setenv age-key-file gnupg-home #$gpg-command
                               #:verbose? #$verbose?)

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
               (current-error-port) "no host key will be generated...~%"))

          (format
           (current-error-port)
           "setting up secrets in '~a'...~%" secrets-directory)
          (unless (file-exists? secrets-directory)
            (mkdir-p secrets-directory))

          ;; Cleanup old secrets.
          (sops-secrets-cleanup secrets-directory extra-links-directory)

          ;; Create SOPS access rules.
          (chdir secrets-directory)
          (symlink #$config-file (string-append secrets-directory "/.sops.yaml"))

          ;; Create new secrets.
          (sops-secrets-create
           #$sops (list #$@secrets)
           secrets-directory extra-links-directory
           #:verbose? #$verbose?)))))
