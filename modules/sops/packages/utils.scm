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

(define %source-dir
  (string-append (current-source-directory) "/bin"))

;; From https://guix.gnu.org/en/blog/2023/from-development-environments-to-continuous-integrationthe-ultimate-guide-to-software-development-with-guix/
(define vcs-file?
  ;; Return true if the given file is under version control.
  (or (git-predicate %source-dir)
      (const #t)))                                ;not in a Git checkout

(define-public sops-guix-utils
  (package
    (name "sops-guix-utils")
    (version "0.0.0-0")
    (source (local-file %source-dir "sops-guix-utils-checkout"
                        #:recursive? #t
                        #:select? vcs-file?))
    (build-system copy-build-system)
    (arguments
     (list
      ;; There's no point in substitutes.
      #:substitutable? #f
      #:install-plan
      #~'(("generate-host-key.sh" "/bin/generate-host-key.sh")
          ("extract-secret.sh" "/bin/extract-secret.sh"))
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
    (license license:gpl3)))
