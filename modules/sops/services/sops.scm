;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops services sops)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages gnupg)
  #:use-module (sops packages sops)
  #:use-module (srfi srfi-1)
  #:export (sops-secrets-service-type

            sops-secret
            sops-secret?
            sops-secret-value
            sops-secret-user
            sops-secret-group
            sops-secret-permissions
            sops-secret-path

            sops-service-configuration
            sops-service-configuration?
            sops-service-configuration-sops
            sops-service-configuration-generate-key?
            sops-service-configuration-decrypt-path
            sops-service-configuration-secrets

            sops-service-configuration->shepherd-services))

(define-configuration/no-serialization sops-secret
  (value
   (gexp)
   "A gexp evaluating to the SOPS secret file.")
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
  #~'(#$(sops-secret-value secret)
      #$(sops-secret-user secret)
      #$(sops-secret-group secret)
      #$(sops-secret-permissions secret)
      #$(sops-secret-path secret)))

(define (list-of-sops-secrets? l)
  (every package? l))

(define-configuration/no-serialization sops-service-configuration
  (sops
   (package sops)
   "The @code{SOPS} package used to perform decryption.")
  (generate-key?
   (boolean #f)
   "When true a GPG key will be derived from the host SSH RSA key with
@code{ssh-to-pgp} and added to the root keyring. It is discouraged and you are
more than welcome to provide your own key in the root keyring.")
  (decrypt-path
   (string "/run/secrets")
   "The path on the root filesystem where the secrets will be decrypted.")
  (secrets
   (list-of-sops-secrets '())
   "The secrets file managed by the @code{sops-secrets-service-type}"))

(define (decrypt-program config)
  (let* ((decrypt-path (sops-service-configuration-decrypt-path config))
         (gpg (file-append gnupg "/bin/gpg"))
         (grep (file-append grep "/bin/grep"))
         (secrets (sops-service-configuration-secrets config))
         (sops-command
          (file-append (sops-service-configuration-sops config)
                       "/bin/sops"))
         (ssh-to-pgp (file-append ssh-to-pgp "/bin/ssh-to-pgp"))
         (tail (file-append coreutils "/bin/tail")))
    (program-file
     "install-secrets.scm"
     (with-imported-modules (source-module-closure
                             '((guix build utils)))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 popen)
                        (ice-9 textual-ports)
                        (srfi srfi-1))

           (define secret-value first)
           (define secret-user second)
           (define secret-group third)
           (define secret-permissions fourth)
           (define secret-path fifth)

           (unless #$(> (length secrets) 0)
                   (format #t "No secrets file configured, exiting.")
                   (exit))

           (define sh-options '("-x" "-l"))

           (define (sh-run command)
             (apply invoke `("sh" ,@sh-options "-c" ,command)))

           (when #$(sops-service-configuration-generate-key? config)
                 (define (read-pipe command)
                   (get-string-all
                    (apply open-pipe* `(,OPEN_READ "sh" ,@sh-options "-c" ,command))))

                 (define ssh-to-pgp-private-key
                   (string-append
                    #$ssh-to-pgp " "
                    #$(string-join '("-comment" "'Imported from SSH'"
                                     "-email" "root@localhost"
                                     "-format" "armor"
                                     "-name" "root")
                                   " ")
                    " -i /etc/ssh/ssh_host_rsa_key -private-key"))

                 (define key-id
                   (string-trim-both
                    (read-pipe
                     (string-append
                      ssh-to-pgp-private-key " | "
                      #$gpg " --import-options show-only --import |"
                      #$grep " 'sec#' -A 1 | " #$tail " -1"))))

                 (when (string-null?
                        (read-pipe (string-append #$gpg " --list-keys | " #$grep " " key-id)))
                   (sh-run ssh-to-pgp-private-key " | "
                           #$gpg " --import")))

           (mkdir-p #$decrypt-path)

           (for-each
            (lambda (secret)
              (define secret-file
                (string-append #$decrypt-path "/"))
              (sh-run
               (string-append "export SOPS_GPG_EXEC=" #$gpg "; "
                              #$sops-command " -d " (secret-value secret) " > ")))

            #$secrets))))))

(define (sops-service-configuration->shepherd-services config)
  (list
   (shepherd-service (provision `(sops-secrets))
                     (one-shot? #t)
                     (documentation
                      "This service decrypts and symlinks @code{SOPS} secrets.")
                     (start
                      #~(make-forkexec-constructor
                         (list #$(decrypt-program config))
                         #:user "root"
                         #:group "root"))
                     (stop
                      #~(make-kill-destructor)))))

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
                                                     (lambda _ (list gnupg sops)))
                                  (service-extension shepherd-root-service-type
                                                     sops-service-configuration->shepherd-services)))
                (default-value (sops-service-configuration))
                (compose concatenate)
                (extend secrets->sops-service-configuration)
                (description
                 "This service runs at system activation, it's duty is to
decrypt @code{SOPS} secrets and place them at their place with the right
permissions.")))
