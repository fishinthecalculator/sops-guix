;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops packages utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (sops packages sops)
  #:use-module (sops packages sops-nix)
  #:use-module (ice-9 match))

(define-public sops-guix-utils
  (let ((version "0.0.0")
        (revision "0")
        (commit "f308b623eac7b89e22cbed7aa7e34f4265ab399f"))
    (package
      (name "sops-guix-utils")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.sr.ht/~fishinthecalculator/sops-guix")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "016jkrvmbhglvay3adjr7wj33bjf66vkavzkkhkw35mpahafzh0i"))))
      (build-system copy-build-system)
      (arguments
       (list
        ;; There's no point in substitutes.
        #:substitutable? #f
        #:install-plan
        #~'(("modules/sops/packages/bin/generate-host-key.sh" "/bin/generate-host-key.sh")
            ("modules/sops/packages/bin/extract-secret.sh" "/bin/extract-secret.sh"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'wrap
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((bin (string-append #$output "/bin"))
                      (bin-directories
                       (search-path-as-list '("bin" "sbin" "libexec")
                                            (map cdr inputs))))

                  (for-each
                   (lambda (entrypoint)
                     (chmod entrypoint #o555)
                     (wrap-program entrypoint
                      `("PATH" ":" prefix
                        (,(string-join
                           (append
                            bin-directories
                            (list bin))
                           ":")))))
                   (find-files bin))))))))
      (inputs
       (list bash-minimal
             coreutils
             grep
             gnupg
             sops
             ssh-to-pgp))
      (synopsis "Utilities for deploying secrets")
      (description "This package bundles a set of scripts required to facilitate
the deploying of SOPS Guix secrets.")
      (home-page "https://sr.ht/~fishinthecalculator/sops-guix/")
      (license license:gpl3))))
