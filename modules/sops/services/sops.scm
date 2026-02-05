;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops services sops)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-crypto)
  #:use-module (sops packages sops)
  #:use-module (sops services configuration)
  #:use-module (sops activation)
  #:use-module (sops secrets)
  #:use-module (sops self)
  #:use-module (sops state)
  #:use-module (sops validation)
  #:use-module (srfi srfi-1)
  #:export (sops-secrets-service-type

            sops-secrets-shepherd-service

            %default-sops-secrets-directory
            sops-secret->secret-file

            sops-public-key
            sops-public-key?
            sops-public-key-fields
            sops-public-key-name
            sops-public-key-value
            sops-public-key-type

            sops-service-configuration
            sops-service-configuration?
            sops-service-configuration-fields
            sops-service-configuration-gnupg
            sops-service-configuration-sops
            sops-service-configuration-log-directory
            sops-service-configuration-config
            sops-service-configuration-generate-key?
            sops-service-configuration-host-ssh-key
            sops-service-configuration-gnupg-home
            sops-service-configuration-age-key-file
            sops-service-configuration-verbose?
            sops-service-configuration-secrets-directory
            sops-service-configuration-secrets))

(define list-of-sops-secrets?
  (list-of sops-secret?))

(define %default-sops-secrets-directory
  "/run/secrets")

(define* (sops-secret->secret-file secret #:key
                                   (directory %default-sops-secrets-directory))
  "Return the actual file name of SECRET on the filesystem.  The keyword
argument DIRECTORY allows overriding the default directory where secrets are
stored."
  (string-append directory "/" (sops-secret->file-name secret)))

(define-configuration/no-serialization sops-public-key
  (name
   (string)
   "The name of the SOPS key.")
  (value
   (string)
   "The value of the public key.")
  (type
   (symbol)
   (string-append "A symbol denoting the type of public key, supported types
are:

@itemize
@item @code{'gpg}
@item @code{'age}
@end itemize")))

(define-maybe/no-serialization gexp-or-file-like)
(define-maybe/no-serialization string)

(define-configuration/no-serialization sops-service-configuration
  (gnupg
   (gexp-or-string (file-append gnupg "/bin/gpg"))
   "The @code{GnuPG} command line used to perform decryption.")
  (sops
   (package sops)
   "The @code{SOPS} package used to perform decryption.")
  (config
   (maybe-gexp-or-file-like)
   "A gexp or file-like object evaluating to the SOPS config file.  This field
is deprecated and will be removed in the future.")
  (log-directory
   (maybe-string)
   "The name of a directory where the sops service will create its log files.")
  (generate-key?
   (boolean #f)
   "When true, a SOPS supported key will be derived from the host SSH private
key.  For RSA keys @code{ssh-to-pgp} is used and the generated key is added to
the keyring located at @code{gnupg-home} field value.  For ed25519 keys
@code{ssh-to-age} is used and the generated key is appended to the keyring file
located at @code{age-key-file} field value.  It is discouraged to generate key
this way, unless for bootstrapping.  You are more than welcome to provision (and
rotate) your own SOPS compatible keys.")
  (host-ssh-key
   (string "/etc/ssh/ssh_host_rsa_key")
   "The file system path of the SSH private key used for automatic derivation of
a SOPS compatible key.  If the @code{generate-key?} field is false, this field is
ignored.")
  (gnupg-home
   (string "/root/.gnupg")
   "The homedir of GnuPG, i.e. where keys used to decrypt SOPS secrets will be looked for.")
  (age-key-file
   (string "/root/.config/sops/age/keys.txt")
   "The absolute path of the file containing the corresponding @code{age}
identities where SOPS should look for when decrypting a secret.")
  (secrets-directory
   (string %default-sops-secrets-directory)
   "The path on the filesystem where the secrets will be decrypted.")
  (verbose?
   (boolean #f)
   "When true the service will print extensive information about its execution state.")
  (secrets
   (list-of-sops-secrets '())
   "The @code{sops-secret} records managed by the @code{sops-secrets-service-type}."))

(define (sops-service-configuration->sops-runtime-state config)
  (match-record config <sops-service-configuration>
                (gnupg sops generate-key? host-ssh-key gnupg-home age-key-file
                 log-directory secrets-directory verbose? secrets)
    (sops-runtime-state
     (age-key-file age-key-file)
     (gnupg-home gnupg-home)
     (secrets secrets)
     (sops sops)
     (gpg-command gnupg)
     (host-ssh-key host-ssh-key)
     (log-directory (and (maybe-value-set? log-directory) log-directory))
     (secrets-directory secrets-directory)
     (generate-key? generate-key?)
     (verbose? verbose?))))

(define* (sops-secrets-shepherd-service runtime-state
                                        #:key (sops-provision '(sops-secrets))
                                        sops-requirement)
  (define log-directory
    (sops-runtime-state-log-directory runtime-state))
  (define secrets-directory
    (sops-runtime-state-secrets-directory runtime-state))
  (define requirement
    (or sops-requirement
        `(user-processes
          ,(string->symbol
            (string-append "file-system-" secrets-directory)))))
  (shepherd-service (provision sops-provision)
                    (requirement requirement)
                    (one-shot? #t)
                    (documentation
                     "SOPS secrets decrypting service.")
                    (start
                     #~(make-forkexec-constructor
                        (list
                         #$(program-file "sops-secrets-entrypoint"
                                         (activate-secrets runtime-state)))
                        #$@(if log-directory
                               (list #:log-file (string-append log-directory
                                                               "/sops-secrets.log"))
                               '())))
                    (stop
                     #~(make-kill-destructor))))

(define (sops-secrets-shepherd-services config)
  (when config
    (let ((config-file
           (sops-service-configuration-config config)))
      (when (maybe-value-set? config-file)
        (warning
         (G_
          "the 'config' field of 'sops-service-configuration' is\
 deprecated, you can delete it from your configuration.~%")))
      (list
       (sops-secrets-shepherd-service
        (sops-service-configuration->sops-runtime-state config))))))

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

(define (sops-secrets-activation config)
  (define secrets-directory
    (sops-service-configuration-secrets-directory config))
  (with-imported-modules (source-module-closure
                            '((sops build activation))
                            #:select? sops-module-name?)
    #~(begin
        (use-modules (guix build utils)
                     (sops build activation))

        (define-values (secrets-directory extra-links-directory)
          (sops-secrets-directories #$secrets-directory))

        (unless (file-exists? secrets-directory)
          (mkdir-p secrets-directory)))))

(define sops-secrets-service-type
  (service-type (name 'sops-secrets)
                (extensions (list (service-extension profile-service-type
                                                     (lambda (config)
                                                       (list (sops-service-configuration-sops config))))
                                  (service-extension file-system-service-type
                                                     %sops-secrets-file-system)
                                  (service-extension activation-service-type
                                                     sops-secrets-activation)
                                  (service-extension shepherd-root-service-type
                                                     sops-secrets-shepherd-services)))
                (compose concatenate)
                (extend secrets->sops-service-configuration)
                (description
                 "This service runs at system activation, its duty is to
decrypt @code{SOPS} secrets and place them at their place with the right
permissions.")))
