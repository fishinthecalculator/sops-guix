;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops services sops)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (sops packages sops)
  #:use-module (sops packages utils)
  #:use-module (sops secrets)
  #:use-module (sops validation)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
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

(define* (activate-secrets config-file
                           gnupg-home
                           sops-secrets
                           secrets-directory
                           sops-package
                           #:key (generate-key? #f))
  "Return an activation gexp for provided secrets."
  (let* ((bash (file-append bash-minimal "/bin/bash"))
         (generate-host-key.sh
          (file-append sops-guix-utils "/bin/generate-host-key.sh"))
         (gpg (file-append gnupg "/bin/gpg"))
         (secrets
          (map lower-sops-secret sops-secrets))
         (sops
          (file-append sops-package "/bin/sops"))
         (extra-links-directory
          (string-append secrets-directory "/extra")))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 ftw)
                     (ice-9 match))
        (define* (list-content directory #:key (exclude '()))
          (scandir directory
                   (lambda (file)
                     (not (member file `("." ".." ,@exclude))))
                   string<?))

        (setenv "GNUPGHOME" #$gnupg-home)
        (setenv "SOPS_GPG_EXEC" #$gpg)

        (if #$generate-key?
            (invoke #$generate-host-key.sh)
            (format #t "no host key will be generated...~%"))

        (format #t "setting up secrets in '~a'...~%" #$secrets-directory)
        (if (file-exists? #$secrets-directory)
            (begin
              ;; Cleanup secrets symlink
              (when (file-exists? #$extra-links-directory)
                (for-each
                 (lambda (link)
                   (define link-path (string-append #$extra-links-directory "/" link))
                   (define link-target (readlink link-path))
                   ;; The user may have manually deleted the target.
                   (when (file-exists? link-target)
                     (format #t "Deleting ~a -> ~a...~%" link-path link-target)
                     (delete-file-recursively link-target)))
                 (list-content #$extra-links-directory)))
              ;; Cleanup secrets
              (for-each (compose delete-file-recursively
                                 (cut string-append #$secrets-directory "/" <>))
                        (list-content #$secrets-directory)))
            (mkdir-p #$secrets-directory))

        (chdir #$secrets-directory)
        (symlink #$config-file (string-append #$secrets-directory "/.sops.yaml"))

        ;; Actually decrypt secrets
        (for-each
         (match-lambda
           ((key secrets-file user group permissions output-type path derived-name)
            (let ((output
                   (string-append #$secrets-directory "/" derived-name))
                  (gc-link
                   (string-append #$extra-links-directory "/" derived-name))
                  (uid (passwd:uid
                        (getpwnam user)))
                  (gid (passwd:uid
                        (getgrnam group))))

              (apply invoke `(#$sops "-d"
                              "--extract" ,key
                              "--output" ,output
                              ,@(if output-type
                                    `("--output-type" ,output-type)
                                    '())
                              ,secrets-file))

              ;; Setting owner is supported only in the system service
              (when (= (getuid) 0)
                (chown output uid gid))
              ;; Permissions are supported regardless
              (chmod output permissions)

              (when path
                ;; First try to setup the symlink
                (symlink output path)

                ;; If everything goes well, setup symlink for
                ;; cleaning up
                (mkdir-p #$extra-links-directory)
                (symlink path gc-link)))))
         (list #$@secrets)))))

(define (%system-secrets-activation config)
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
      (activate-secrets config-file
                        gnupg-home
                        secrets
                        secrets-directory
                        sops
                        #:generate-key? generate-key?))))

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
                                  (service-extension activation-service-type
                                                     %system-secrets-activation)))
                (default-value #f)
                (compose concatenate)
                (extend secrets->sops-service-configuration)
                (description
                 "This service runs at system activation, its duty is to
decrypt @code{SOPS} secrets and place them at their place with the right
permissions.")))
