;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops packages sops)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match))

(define %sops-version
  "3.8.0")

(define (sops-origin-file-name system)
  (string-append "sops-" %sops-version "-" system "64"))

(define (sops-origin-url system)
  (string-append "https://github.com/getsops/sops/releases/"
                 "download/v" %sops-version "/sops-v"
                 %sops-version ".linux." system "64"))

(define* (sops-origin-values #:key amd64-hash aarch64-hash)
  (define system (%current-system))
  (match (%current-system)
    ("x86_64-linux"
     (values (sops-origin-url "amd")
             (sops-origin-file-name "amd")
             amd64-hash))
    ("aarch64-linux"
     (values (sops-origin-url "arm")
             (sops-origin-file-name "arm")
             aarch64-hash))))

(define-public sops
  (define-values (url file-name hash)
    (sops-origin-values
     #:amd64-hash "1h1503lh5f87rpdsinv4kc5h8v9664crkd0mpq9r4jh1c9jlmys8"
     #:aarch64-hash "0qrs3v401nzrqacqi9hq92yidrmn26rwkdzlkysm9q9mssp1xhsy"))
  (package
    (name "sops")
    (version %sops-version)
    (source
     (origin
       (method url-fetch)
       (uri url)
       (file-name file-name)
       (sha256 (base32 hash))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; There's no point in substitutes.
      #:substitutable? #f
      #:install-plan
      #~'((#$file-name "/bin/sops"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'chmod
            (lambda _
              (chmod #$file-name #o555))))))
    (synopsis "Simple and flexible tool for managing secrets ")
    (supported-systems '("x86_64-linux" "aarch64-linux"))
    (description "sops is an editor of encrypted files that supports YAML, JSON,
ENV, INI and BINARY formats and encrypts with AWS KMS, GCP KMS, Azure Key Vault,
age, and PGP.")
    (home-page "https://github.com/getsops/sops")
    (license license:mpl2.0)))
