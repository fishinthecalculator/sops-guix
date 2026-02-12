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
  #:export (activate-secret
            generate-ssh-key
            wait-for-secrets))

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

(define (activate-secret secret runtime-state)
  "Return an activation gexp for provided secrets."
  (match-record runtime-state <sops-runtime-state>
                (age-key-file gnupg-home sops gpg-command
                 secrets-directory verbose?)
    (let* ((sops-secret
            (lower-sops-secret secret))
           ;; See lower-sops-secret from sops/secrets.scm
           (secret-file-name #~(eighth #$sops-secret))
           (secret-link #~(seventh #$sops-secret))
           (sops-command
            (file-append sops "/bin/sops")))

      (with-imported-modules (source-module-closure
                              '((sops build activation))
                              #:select? sops-module-name?)
        #~(begin
            (use-modules (guix build utils)
                         (sops build activation)
                         (srfi srfi-1))

            (define-values (secrets-directory extra-links-directory)
              (sops-secrets-directories #$secrets-directory))

            (sops-secrets-setenv #$age-key-file #$gnupg-home #$gpg-command
                                 #:verbose? #$verbose?)

            (format
             (current-error-port)
             "Setting up secret in '~a'...~%" secrets-directory)

            ;; Cleanup old secrets.
            (sops-secret-cleanup
             (string-append secrets-directory "/" #$secret-file-name)
             #$secret-link
             extra-links-directory)

            ;; Create new secrets.
            (sops-secret-create
             #$sops-command #$sops-secret
             secrets-directory extra-links-directory
             #:verbose? #$verbose?))))))

(define (wait-for-secrets runtime-state)
  "Return a gexp waiting for provided secrets to be created."
  (match-record runtime-state <sops-runtime-state>
                (secrets secrets-directory verbose?)
    (let ((sops-secrets-names
           (map sops-secret->file-name secrets)))

      (with-imported-modules (source-module-closure
                              '((sops build activation)
                                (sops build utils))
                              #:select? sops-module-name?)
        #~(begin
            (use-modules (sops build activation)
                         (sops build utils)
                         (ice-9 format))

            (define-values (secrets-directory extra-links-directory)
              (sops-secrets-directories #$secrets-directory))

            (for-each
             (lambda (secret)
               (define secret-path
                 (string-append secrets-directory "/" secret))
               (wait-for-file secret-path)
               (when #$verbose?
                 (format (current-output-port) "~a successfully created.~%"
                         secret-path)))
             (list #$@sops-secrets-names))

            (format
             (current-error-port)
             "Secrets correctly setup in '~a'.~%" secrets-directory))))))
