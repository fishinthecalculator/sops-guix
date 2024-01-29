;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023-2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops packages sops-nix)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-crypto)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (ice-9 match))

(define-public ssh-to-age
  (package
    (name "ssh-to-age")
    (version "1.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Mic92/ssh-to-age")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06c7bswd5kkk3l0pbfgg0k0hzcrrij00s7gl4j2ffxn5q2iap13i"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/Mic92/ssh-to-age"))
    (propagated-inputs (list go-golang-org-x-sys
                             go-golang-org-x-crypto
                             go-filippo-io-edwards25519
                             go-filippo-io-age))
    (home-page "https://github.com/Mic92/ssh-to-age")
    (synopsis "ssh-to-age")
    (description "Convert SSH Ed25519 keys to age keys.")
    (license license:expat)))

(define-public ssh-to-pgp
  (package
    (name "ssh-to-pgp")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Mic92/ssh-to-pgp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x224wfgfnywwa5dz06ac0xm34mnnv9l8qhbvp52lxhbyrhgf7yx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/Mic92/ssh-to-pgp"))
    (native-inputs
     (list gnupg))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-golang-org-x-crypto))
    (home-page "https://github.com/Mic92/ssh-to-pgp")
    (synopsis "ssh-to-pgp")
    (description "Convert SSH RSA keys to GPG keys")
    (license license:expat)))
