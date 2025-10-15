;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops packages sops)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:))

(define (sops-source version suffix checksum)
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/getsops/sops/releases/"
                        "download/v" version "/sops-v" version ".linux." suffix))
    (sha256 (base32 checksum))))

(define-public sops
  (package
    (name "sops")
    (version "3.9.4")
    (source #f)
    (native-inputs (cond ((target-aarch64?)
                          `(("binary-source"
                             ,(sops-source version "arm64" "0jqpxsg8ahx8n7cq8n6ybkc96hl9f4kzdzhdkrfm120x31mlqmhn"))))

                         ((target-x86-64?)
                          `(("binary-source"
                             ,(sops-source version "amd64" "11afdrifjla52ck884bs84fbjfmbpdad0pc9mn17kpkiqhmy722l"))))

                         (else '())))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let* ((source #+(this-package-native-input "binary-source"))
                 (dest-dir (string-append #$output "/bin"))
                 (dest-file (string-append dest-dir "/sops")))
            (mkdir-p dest-dir)
            (copy-file source dest-file)
            (chmod dest-file #o555)))))
    (synopsis "Simple and flexible tool for managing secrets ")
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (description "sops is an editor of encrypted files that supports YAML, JSON,
ENV, INI and BINARY formats and encrypts with AWS KMS, GCP KMS, Azure Key Vault,
age, and PGP.")
    (home-page "https://github.com/getsops/sops")
    (license license:mpl2.0)))
