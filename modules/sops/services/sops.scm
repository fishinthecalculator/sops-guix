;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops services sops)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (sops packages sops)
  #:use-module (sops packages utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (sops-secrets-service-type

            sanitize-sops-key
            key->file-name

            sops-secret
            sops-secret?
            sops-secret-file
            sops-secret-key
            sops-secret-user
            sops-secret-group
            sops-secret-permissions
            sops-secret-path

            sops-service-configuration
            sops-service-configuration?
            sops-service-configuration-sops
            sops-service-configuration-config
            sops-service-configuration-generate-key?
            sops-service-configuration-gnupg-home
            sops-service-configuration-secrets-directory
            sops-service-configuration-secrets

            %secrets-activation))

(define (gexp-or-file-like? value)
  (if (or (file-like? value) (gexp? value))
      value
      (raise
       (formatted-message
        (G_ "file field value must contain only gexps or file-like objects,
but ~a was found")
        value))))

(define (serialize-string name value)
  value)

(define (sanitize-sops-string-key value)
  (if (and (string? value)
           (string-match "^(\\[(\".*\"|[0-9]+)\\])+$" value))
      value
      (raise
       (formatted-message
        (G_ "key field value must follow Python's dictionary syntax, but ~a was found.~%~%Please refer to the SOPS documentation to make sure of the actual syntax,
or if you are really it's a bug in SOPS Guix make sure to report it at https://todo.sr.ht/~fishinthecalculator/sops-guix .")
        value))))

(define (sanitize-sops-list-key value)
  (if (every (lambda (key) (or string?
                               (and (integer? value)
                                    (>= value 0))))
             value)
      (apply string-append
             (map (lambda (key)
                    (format #f "[~a]" (if (number? key)
                                          key
                                          (format #f "\"~a\"" key))))
                  value))
      (raise
       (formatted-message
        (G_ "key field value must be a list of strings or positive integers, but ~a was found.~%")
        value))))

(define (sanitize-sops-key value)
  (match value
    ((? string? value)
     (sanitize-sops-string-key value))
    ((? list? value)
     (sanitize-sops-list-key value))
    (_
      (raise
       (formatted-message
        (G_ "key field value must be either a string or a list, but ~a was found.~%")
        value)))))

(define (string-or-list? value)
  (or (string? value)
      (list? value)))

(define (sanitize-output-type value)
  (if (not (maybe-value-set? value))
      value
      (if (and (string? value)
               (member value '("json"
                               "dotenv"
                               "binary"
                               "yaml")))
          value
          (raise
           (formatted-message
            (G_ "output-type field value must one of json, dotenv, binary or yaml but ~a was found.~%")
            value)))))

(define (key->file-name key)
  (string-join
   (filter-map
    (lambda (sub-key)
      (and (not (string-null? sub-key))
           (string-replace-substring sub-key "[" "")))
    (string-split
     (string-replace-substring key "\"" "") #\]))
   "-"))

(define-maybe string)

(define-configuration/no-serialization sops-secret
  (key
   (string-or-list)
   "A key representing a value in the secrets file. Its value can be a string,
which will be directly passed to @command{sops -d --extract} or a list of strings
representing the path of the value you want to reference in the secrets file."
   (sanitizer sanitize-sops-key))
  (file
   (gexp-or-file-like)
   "A gexp or file-like object evaluating to the secrets file.")
  (user
   (string "root")
   "The user owner of the secret.")
  (group
   (string "root")
   "The group owner of the secret.")
  (output-type
   (maybe-string)
   "Currently json, yaml, dotenv and binary are supported. If not set, sops will
use the secrets file's extension to determine the output format."
   (sanitizer sanitize-output-type))
  (permissions
   (number #o440)
   "@code{chmod} permissions that will be applied to the secret.")
  (path
   (maybe-string)
   "An optional path on the root filesystem where the secret will be placed."))

;; FIXME: This way of lowering secrets is not pretty.
(define (lower-sops-secret secret)
  (let* ((key (sops-secret-key secret))
         (file-name
          (key->file-name key))
         (output-type
          (sops-secret-output-type secret))
         (path (sops-secret-path secret)))
    #~'(#$(sops-secret-key secret)
        #$(sops-secret-file secret)
        #$(sops-secret-user secret)
        #$(sops-secret-group secret)
        #$(sops-secret-permissions secret)
        #$(and (maybe-value-set? output-type)
               output-type)
        #$(and (maybe-value-set? path)
               (not (string=? path file-name))
               path)
        #$file-name)))

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
   "The path on the root filesystem where the secrets will be decrypted.")
  (secrets
   (list-of-sops-secrets '())
   "The @code{sops-secret} records managed by the @code{sops-secrets-service-type}."))

(define (%secrets-activation config)
  "Return an activation gexp for system secrets."
  (when config
    (let* ((bash (file-append bash-minimal "/bin/bash"))
           (config-file
            (sops-service-configuration-config config))
           (generate-key?
            (sops-service-configuration-generate-key? config))
           (generate-host-key.sh
            (file-append sops-guix-utils "/bin/generate-host-key.sh"))
           (gpg (file-append gnupg "/bin/gpg"))
           (gnupg-home
            (sops-service-configuration-gnupg-home config))
           (secrets
            (map lower-sops-secret (sops-service-configuration-secrets config)))
           (secrets-directory
            (sops-service-configuration-secrets-directory config))
           (sops
            (file-append
              (sops-service-configuration-sops config)
              "/bin/sops"))
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
                (chown output uid gid)
                (chmod output permissions)

                (when path
                  ;; First try to setup the symlink
                  (symlink output path)

                  ;; If everything goes well, setup symlink for
                  ;; cleaning up
                  (mkdir-p #$extra-links-directory)
                  (symlink path gc-link)))))
           (list #$@secrets))))))

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
                                                     %secrets-activation)))
                (default-value #f)
                (compose concatenate)
                (extend secrets->sops-service-configuration)
                (description
                 "This service runs at system activation, its duty is to
decrypt @code{SOPS} secrets and place them at their place with the right
permissions.")))
