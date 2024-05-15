;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops services sops)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-crypto)
  #:use-module (sops packages sops)
  #:use-module (sops activation)
  #:use-module (sops secrets)
  #:use-module (sops validation)
  #:use-module (srfi srfi-1)
  #:export (sops-secrets-service-type

            sops-service-configuration
            sops-service-configuration?
            sops-service-configuration-fields
            sops-service-configuration-sops
            sops-service-configuration-config
            sops-service-configuration-generate-key?
            sops-service-configuration-gnupg-home
            sops-service-configuration-secrets-directory
            sops-service-configuration-secrets))

(define list-of-sops-secrets?
  (list-of sops-secret?))

(define-configuration/no-serialization sops-service-configuration
  (sops
   (package sops)
   "The @code{SOPS} package used to perform decryption.")
  (config
   (gexp-or-file-like)
   "A gexp or file-like object evaluating to the SOPS config file.")
  (generate-key?
   (boolean #f)
   "When true a GPG key will be derived from the host SSH RSA key with
@code{ssh-to-pgp} and added to the keyring located at
@code{gnupg-home} field value. It is discouraged and you are
more than welcome to provide your own key in the keyring.")
  (gnupg-home
   (string "/root/.gnupg")
   "The homedir of GnuPG, i.e. where keys used to decrypt SOPS secrets will be looked for.")
  (secrets-directory
   (string "/run/secrets")
   "The path on the filesystem where the secrets will be decrypted.")
  (secrets
   (list-of-sops-secrets '())
   "The @code{sops-secret} records managed by the @code{sops-secrets-service-type}."))

(define (sops-secrets-shepherd-service config)
  (when config
    (let* ((config-file
            (sops-service-configuration-config config))
           (generate-key?
            (sops-service-configuration-generate-key? config))
           (gnupg-home
            (sops-service-configuration-gnupg-home config))
           (secrets (sops-service-configuration-secrets config))
           (secrets-directory
            (sops-service-configuration-secrets-directory config))
           (sops (sops-service-configuration-sops config)))
      (list
       (shepherd-service (provision '(sops-secrets))
                         (requirement '(file-systems))
                         (one-shot? #t)
                         (documentation
                          "SOPS secrets decrypting service.")
                         (start
                          #~(make-forkexec-constructor
                             (list
                              #$(program-file "sops-secrets-entrypoint"
                                              (activate-secrets config-file
                                                                gnupg-home
                                                                secrets
                                                                sops
                                                                #:secrets-directory secrets-directory
                                                                #:generate-key? generate-key?)))))
                         (stop
                          #~(make-kill-destructor)))))))

(define (%sops-secrets-file-system config)
  (list
   (file-system
     (device "none")
     (mount-point
      (sops-service-configuration-secrets-directory config))
     (type "ramfs")
     (check? #f))))

(define (secrets->sops-service-configuration config secrets)
  (sops-service-configuration
   (inherit config)
   (secrets
    (append
     (sops-service-configuration-secrets config)
     secrets))))

(define sops-secrets-service-type
  (service-type (name 'sops-secrets)
                (extensions (list (service-extension profile-service-type
                                                     (lambda (config)
                                                       (list age gnupg
                                                             (sops-service-configuration-sops config))))
                                  (service-extension file-system-service-type
                                                     %sops-secrets-file-system)
                                  (service-extension activation-service-type
                                                     (lambda (config)
                                                       #~(begin
                                                           (define secrets-directory
                                                             #$(sops-service-configuration-secrets-directory config))
                                                           (unless (file-exists? secrets-directory)
                                                                   (mkdir-p secrets-directory)))))
                                  (service-extension shepherd-root-service-type
                                                     sops-secrets-shepherd-service)))
                (default-value #f)
                (compose concatenate)
                (extend secrets->sops-service-configuration)
                (description
                 "This service runs at system activation, its duty is to
decrypt @code{SOPS} secrets and place them at their place with the right
permissions.")))
