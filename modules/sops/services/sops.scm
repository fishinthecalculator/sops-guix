;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops services sops)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages gnupg)
  #:use-module (sops packages sops)
  #:use-module (sops packages utils)
  #:use-module (srfi srfi-1)
  #:export (sops-secrets-service-type

            sops-secret
            sops-secret?
            sops-secret-key
            sops-secret-user
            sops-secret-group
            sops-secret-permissions
            sops-secret-path

            sops-service-configuration
            sops-service-configuration?
            sops-service-configuration-sops
            sops-service-configuration-file
            sops-service-configuration-generate-key?
            sops-service-configuration-secrets-directory
            sops-service-configuration-secrets

            %secrets-activation))

(define (string-or-gexp? value)
  (or (string? value) (gexp? value)))

(define-configuration/no-serialization sops-secret
  (key
   (string-or-gexp)
   "A string or a gexp evaluating to a key in the secrets file.")
  (user
   (string "root")
   "The user owner of the secret.")
  (group
   (string "root")
   "The group owner of the secret.")
  (permissions
   (number #o440)
   "@code{chmod} permissions that will be applied to the secret.")
  (path
   (string)
   "The path on the root filesystem where the secret will be placed."))

(define (lower-sops-secret secret)
  #~'(#$(sops-secret-key secret)
      #$(sops-secret-user secret)
      #$(sops-secret-group secret)
      #$(sops-secret-permissions secret)
      #$(sops-secret-path secret)))

(define list-of-sops-secrets?
  (list-of sops-secret?))

(define-configuration/no-serialization sops-service-configuration
  (sops
   (package sops)
   "The @code{SOPS} package used to perform decryption.")
  (file
   (gexp)
   "A gexp or file-like object evaluating to the secrets file.")
  (generate-key?
   (boolean #f)
   "When true a GPG key will be derived from the host SSH RSA key with
@code{ssh-to-pgp} and added to the root keyring. It is discouraged and you are
more than welcome to provide your own key in the root keyring.")
  (secrets-directory
   (string "/run/secrets")
   "The path on the root filesystem where the secrets will be decrypted.")
  (secrets
   (list-of-sops-secrets '())
   "The @code{sops-secret} records managed by the @code{sops-secrets-service-type}."))

(define (%secrets-activation config)
  "Return an activation gexp for system secrets."
  (when config
    (let* ((bash (file-append bash-minimal "/bin/bash"))
           (extract-secret.sh
            (file-append sops-guix-utils "/bin/extract-secret.sh"))
           (file
            (sops-service-configuration-file config))
           (generate-key?
            (sops-service-configuration-generate-key? config))
           (generate-host-key.sh
            (file-append sops-guix-utils "/bin/generate-host-key.sh"))
           (gpg (file-append gnupg "/bin/gpg"))
           (secrets
            (map lower-sops-secret (sops-service-configuration-secrets config)))
           (secrets-directory
            (sops-service-configuration-secrets-directory config)))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))

          (setenv "SOPS_GPG_EXEC" #$gpg)

          (if generate-key?
              (invoke #$generate-host-key.sh)
              (format #t "no host key will be generated...~%"))

          (format #t "setting up secrets in '~a'...~%" #$secrets-directory)
          (if (file-exists? #$secrets-directory)
              (for-each (compose delete-file
                                 (cut string-append #$secrets-directory "/" <>))
                        (scandir secrets-directory
                                 (lambda (file)
                                   (not (member file '("." ".."))))
                                 string<?))
              (mkdir-p #$secrets-directory))

          ;; Actually decrypt secrets
          (for-each
           (match-lambda
             ((key user group permissions path)
              (let ((uid (passwd:uid user))
                    (gid (passwd:gid group)))
                (invoke #$extract-secret.sh key path file)
                (chown path uid gid)
                (chmod path permissions))))
           #$secrets)))))

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
                                                     (lambda _ (list gnupg sops-guix-utils)))
                                  (service-extension activation-service-type
                                                     %secrets-activation)))
                (default-value #f)
                (compose concatenate)
                (extend secrets->sops-service-configuration)
                (description
                 "This service runs at system activation, its duty is to
decrypt @code{SOPS} secrets and place them at their place with the right
permissions.")))
