;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024-2026 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;; Copyright © 2026 Hilton Chain <hako@ultrarare.space>

(define-module (sops home services sops)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-crypto)
  #:use-module (sops services configuration)
  #:use-module (sops services sops)
  #:use-module (sops packages sops)
  #:use-module (sops activation)
  #:use-module (sops secrets)
  #:use-module (sops self)
  #:use-module (sops state)
  #:use-module (sops validation)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (home-sops-secrets-service-type

            home-sops-service-configuration
            home-sops-service-configuration?
            home-sops-service-configuration-fields
            home-sops-service-configuration-gnupg
            home-sops-service-configuration-sops
            home-sops-service-configuration-config
            home-sops-service-configuration-gnupg-home
            home-sops-service-configuration-age-key-file
            home-sops-service-configuration-verbose?
            home-sops-service-configuration-secrets
            home-sops-service-configuration-shepherd-requirement))

(define list-of-sops-secrets?
  (list-of sops-secret?))

(define list-of-sops-symbols?
  (list-of symbol?))

(define (string-or-gexp? value)
  (or (string? value)
      (gexp? value)))

(define-maybe/no-serialization string)
(define-maybe/no-serialization string-or-gexp)
(define-maybe/no-serialization gexp-or-file-like)

(define (home-sops-service-gnupg-home config)
  (if (maybe-value-set? config)
      config
      #~(string-append (getenv "HOME")
                       "/.gnupg")))

(define (home-sops-service-age-key-file config)
  (if (maybe-value-set? config)
      config
      #~(string-append (getenv "HOME")
                       "/.config/sops/age/keys.txt")))

(define-configuration/no-serialization home-sops-service-configuration
  (gnupg
   (gexp-or-string (file-append gnupg "/bin/gpg"))
   "The @code{GnuPG} command line used to perform decryption."
   (sanitizer sanitize-gexp-or-string))
  (sops
   (package sops)
   "The @code{SOPS} package used to perform decryption.")
  (config
   (maybe-gexp-or-file-like)
   "A gexp or file-like object evaluating to the SOPS config file.  This field
is deprecated and will be removed in the future.")
  (gnupg-home
   (maybe-string-or-gexp)
   "The homedir of GnuPG, i.e. where keys used to decrypt SOPS secrets will be
looked for.
It defaults to @code{~/.gnupg}"
   (sanitizer home-sops-service-gnupg-home))
  (age-key-file
   (maybe-string-or-gexp)
   "The file containing the corresponding @code{age} identities where SOPS will
look for when decrypting a secret.  It defaults to
@code{~/.config/sops/age/keys.txt}"
   (sanitizer home-sops-service-age-key-file))
  (verbose?
   (boolean #f)
   "When true the service will print extensive information about its execution
state.")
  (secrets
   (list-of-sops-secrets '())
   "The @code{sops-secret} records managed by the
@code{home-sops-secrets-service-type}.")
  (shepherd-requirement
   (list-of-symbols '())
   "List of Home shepherd services that must be started before decrypting
SOPS secrets."))

(define (home-sops-service-configuration->sops-runtime-state config)
  (match-record config <home-sops-service-configuration>
                (gnupg sops verbose? secrets)
    (sops-runtime-state
     (age-key-file (home-sops-service-configuration-age-key-file config))
     (gnupg-home (home-sops-service-configuration-gnupg-home config))
     (secrets secrets)
     (sops sops)
     (gpg-command gnupg)
     (verbose? verbose?))))

(define (home-sops-secrets-shepherd-services config)
  (when config
    (let ((config-file
           (home-sops-service-configuration-config config))
          (runtime-state
           (home-sops-service-configuration->sops-runtime-state config))
          (requirement
           (home-sops-service-configuration-shepherd-requirement config)))
      (when (maybe-value-set? config-file)
        (warning
         (G_
          "the 'config' field of 'home-sops-service-configuration' is\
 deprecated, you can delete it from your configuration.~%")))
      (append
       (map
        (cut sops-secret-decrypt-shepherd-service <> runtime-state
             #:home-service? #t #:sops-requirement requirement)
        (home-sops-service-configuration-secrets config))
       (list
        (sops-secrets-shepherd-service
         runtime-state
         #:sops-provision '(home-sops-secrets)
         #:home-service? #t
         #:sops-requirement requirement))))))

(define (secrets->home-sops-service-configuration config secrets)
  (home-sops-service-configuration
   (inherit config)
   (secrets
    (append
     (home-sops-service-configuration-secrets config)
     secrets))))

(define (home-sops-secrets-activation config)
  (with-imported-modules (source-module-closure
                            '((sops build activation))
                            #:select? sops-module-name?)
    #~(begin
        (use-modules (guix build utils)
                     (sops build activation))

        (define-values (secrets-directory extra-links-directory)
          (sops-secrets-directories))

        (unless (file-exists? secrets-directory)
          (mkdir-p secrets-directory)))))

(define home-sops-secrets-service-type
  (service-type (name 'home-sops-secrets)
                (extensions (list (service-extension home-profile-service-type
                                                     (lambda (config)
                                                       (list (home-sops-service-configuration-sops config))))
                                  (service-extension home-activation-service-type
                                                     home-sops-secrets-activation)
                                  (service-extension home-shepherd-service-type
                                                     home-sops-secrets-shepherd-services)))
                (compose concatenate)
                (extend secrets->home-sops-service-configuration)
                (description
                 "This service runs at herd's startup, its duty is to
decrypt @code{SOPS} secrets and place them at their place on tmpfs.")))
