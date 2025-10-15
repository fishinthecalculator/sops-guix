;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024, 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops home services sops)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-crypto)
  #:use-module (sops services configuration)
  #:use-module (sops packages sops)
  #:use-module (sops activation)
  #:use-module (sops secrets)
  #:use-module (sops validation)
  #:use-module (srfi srfi-1)
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

(define-maybe/no-serialization string)

(define-configuration/no-serialization home-sops-service-configuration
  (gnupg
   (gexp-or-string (file-append gnupg "/bin/gpg"))
   "The @code{GnuPG} command line used to perform decryption."
   (sanitizer sanitize-gexp-or-string))
  (sops
   (package sops)
   "The @code{SOPS} package used to perform decryption.")
  (config
   (gexp-or-file-like)
   "A gexp or file-like object evaluating to the SOPS config file.")
  (gnupg-home
   (maybe-string)
   "The homedir of GnuPG, i.e. where keys used to decrypt SOPS secrets will be looked for.
It defaults to @code{~/.gnupg}")
  (age-key-file
   (maybe-string)
   "The file containing the corresponding @code{age} identities where SOPS will look for
when decrypting a secret.  It defaults to @code{~/.config/sops/age/keys.txt}")
  (verbose?
   (boolean #f)
   "When true the service will print extensive information about its execution state.")
  (secrets
   (list-of-sops-secrets '())
   "The @code{sops-secret} records managed by the @code{home-sops-secrets-service-type}.")
  (shepherd-requirement
   (list-of-symbols '())
   "List of Home shepherd services that must be started before decrypting SOPS secrets."))

(define (home-sops-service-age-key-file config)
  (define age-key-file
    (home-sops-service-configuration-age-key-file config))
  (if (maybe-value-set? age-key-file)
      age-key-file
      #~(string-append (getenv "HOME")
                       "/.config/sops/age/keys.txt")))

(define (home-sops-service-gnupg-home config)
  (define gnupg-home
    (home-sops-service-configuration-gnupg-home config))
  (if (maybe-value-set? gnupg-home)
      gnupg-home
      #~(string-append (getenv "HOME")
                       "/.gnupg")))

(define (home-sops-secrets-shepherd-service config)
  (when config
    (let* ((config-file
            (home-sops-service-configuration-config config))
           (age-key-file
            (home-sops-service-age-key-file config))
           (gnupg-home
            (home-sops-service-gnupg-home config))
           (shepherd-req (home-sops-service-configuration-shepherd-requirement config))
           (secrets (home-sops-service-configuration-secrets config))
           (gnupg (home-sops-service-configuration-gnupg config))
           (sops (home-sops-service-configuration-sops config))
           (verbose? (home-sops-service-configuration-sops config)))
      (list
       (shepherd-service (provision '(home-sops-secrets))
                         (requirement shepherd-req)
                         (one-shot? #t)
                         (documentation
                          "SOPS secrets decrypting home service.")
                         (start
                          #~(make-forkexec-constructor
                             (list
                              #$(program-file "home-sops-secrets-entrypoint"
                                              (activate-secrets config-file
                                                                age-key-file
                                                                gnupg-home
                                                                secrets
                                                                sops gnupg
                                                                #:verbose?
                                                                verbose?)))))
                         (stop
                          #~(make-kill-destructor)))))))

(define (secrets->home-sops-service-configuration config secrets)
  (home-sops-service-configuration
   (inherit config)
   (secrets
    (append
     (home-sops-service-configuration-secrets config)
     secrets))))

(define home-sops-secrets-service-type
  (service-type (name 'home-sops-secrets)
                (extensions (list (service-extension home-profile-service-type
                                                     (lambda (config)
                                                       (list (home-sops-service-configuration-sops config))))
                                  (service-extension home-shepherd-service-type
                                                     home-sops-secrets-shepherd-service)))
                (compose concatenate)
                (extend secrets->home-sops-service-configuration)
                (description
                 "This service runs at herd's startup, its duty is to
decrypt @code{SOPS} secrets and place them at their place on tmpfs.")))
