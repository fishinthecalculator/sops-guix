;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops home services sops)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (sops packages sops)
  #:use-module (sops activation)
  #:use-module (sops secrets)
  #:use-module (sops validation)
  #:use-module (srfi srfi-1)
  #:export (home-sops-secrets-service-type

            home-sops-service-configuration
            home-sops-service-configuration?
            home-sops-service-configuration-fields
            home-sops-service-configuration-sops
            home-sops-service-configuration-config
            home-sops-service-configuration-gnupg-home
            home-sops-service-configuration-secrets))

(define list-of-sops-secrets?
  (list-of sops-secret?))

(define-configuration/no-serialization home-sops-service-configuration
  (sops
   (package sops)
   "The @code{SOPS} package used to perform decryption.")
  (config
   (gexp-or-file-like)
   "A gexp or file-like object evaluating to the SOPS config file.")
  (gnupg-home
   (string "~/.gnupg")
   "The homedir of GnuPG, i.e. where keys used to decrypt SOPS secrets will be looked for.")
  (secrets
   (list-of-sops-secrets '())
   "The @code{sops-secret} records managed by the @code{home-sops-secrets-service-type}."))

(define (home-sops-secrets-shepherd-service config)
  (when config
    (let* ((config-file
            (home-sops-service-configuration-config config))(gnupg-home
            (home-sops-service-configuration-gnupg-home config))
           (secrets (home-sops-service-configuration-secrets config))
           (sops (home-sops-service-configuration-sops config)))
      (list
       (shepherd-service (provision '(home-sops-secrets))
                         (requirement '())
                         (one-shot? #t)
                         (documentation
                          "SOPS secrets decrypting home service.")
                         (start
                          #~(make-forkexec-constructor
                             (list
                              #$(program-file "home-sops-secrets-entrypoint"
                                              (activate-secrets config-file
                                                                gnupg-home
                                                                secrets
                                                                sops)))))
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
                                                       (list age gnupg
                                                             (home-sops-service-configuration-sops config))))
                                  (service-extension home-activation-service-type
                                                     (lambda _
                                                       #~(begin
                                                           (define secrets-directory (string-append "/run/user/" (number->string (getuid)) "/secrets"))
                                                           (unless (file-exists? secrets-directory)
                                                             (mkdir-p secrets-directory)))))
                                  (service-extension home-shepherd-service-type
                                                     home-sops-secrets-shepherd-service)))
                (default-value #f)
                (compose concatenate)
                (extend secrets->home-sops-service-configuration)
                (description
                 "This service runs at herd's startup, its duty is to
decrypt @code{SOPS} secrets and place them at their place on tmpfs.")))
