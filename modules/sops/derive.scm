;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025, 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops derive)
  #:use-module (gnu packages base)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages password-utils)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (sops self)
  #:export (generate-host-key))

;; GnuPG versions after 2.5.21 refuse keys serialized with ProtonMail/go-crypto < 1.5.2.
;; See https://github.com/fishinthecalculator/sops-guix/issues/42
;; TODO: This package can be dropped once https://codeberg.org/guix/guix/pulls/12545
;; gets merged.
(use-modules (guix packages) (guix git-download))
(define ssh-to-pgp/fixed
  (package
    (inherit ssh-to-pgp)
    (native-inputs
     (modify-inputs (package-native-inputs ssh-to-pgp)
       (replace "go-github-com-protonmail-go-crypto"
         (package
           (inherit go-github-com-protonmail-go-crypto)
           (version "1.5.2")
           (source
            (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ProtonMail/go-crypto")
                     (commit (string-append "v" version))))
              (file-name
               (git-file-name
                (package-name go-github-com-protonmail-go-crypto) version))
              (sha256
               (base32 "1z0x3hmxqc5s97pbgsjfdwpl9cmhr1hw86ks5vlgv3cw75qxkk80"))))))))))

(define* (generate-host-key age-key-file
                            gnupg-home
                            gpg-command
                            #:key (host-ssh-key "/etc/ssh/ssh_host_rsa_key")
                            verbose?)
      (with-imported-modules (source-module-closure
                              '((sops build age)
                                (sops build gnupg))
                              #:select? sops-module-name?)
        (program-file
         "generate-host-key"
         #~(begin
             (use-modules (sops build age)
                          (sops build gnupg)
                          (ice-9 format)
                          (srfi srfi-11))

             (define age-key-file #$age-key-file)
             (define gnupg-home #$gnupg-home)

             (setenv "GNUPGHOME" gnupg-home)

             (define-values (age-key-derivation-status age-key)
               (generate-age-key #$(file-append ssh-to-age "/bin/ssh-to-age")
                                 #$host-ssh-key
                                 #:verbose? #$verbose?))

             (if (zero? age-key-derivation-status)
                 (if (age-key-exists? age-key age-key-file)
                     (format
                      (current-error-port)
                      "Derived age key already exists at ~a.~%"
                      age-key-file)
                     (age-store age-key age-key-file #:verbose? #$verbose?))
                 (let-values (((gpg-key-derivation-status gpg-key gpg-key-id)
                               (generate-gpg-key
                                #$host-ssh-key
                                #$(file-append ssh-to-pgp/fixed "/bin/ssh-to-pgp")
                                #$gpg-command
                                #$(file-append grep "/bin/grep")
                                #$(file-append sed "/bin/sed")
                                #:verbose? #$verbose?)))
                   (if (zero? gpg-key-derivation-status)
                       (if (gpg-key-exists?
                            gpg-key-id #$gpg-command #:verbose? #$verbose?)
                           (format
                            (current-error-port)
                            "Derived GnuPG key already exists at ~a.~%"
                            gnupg-home)
                           (gpg-import gpg-key #$gpg-command))
                       (format
                        (current-error-port)
                        "No SOPS compatible key could be generated from ~a.~%"
                        #$host-ssh-key))))))))
