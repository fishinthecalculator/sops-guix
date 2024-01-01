;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops packages utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (sops packages sops-nix)
  #:use-module (ice-9 match))

(define generate-host-key.sh
  (let* ((bash (file-append bash-minimal "/bin/bash"))
         (echo (file-append coreutils "/bin/echo"))
         (gpg (file-append gnupg "/bin/gpg"))
         (grep (file-append grep "/bin/grep"))
         (ssh-to-pgp (file-append ssh-to-pgp "/bin/ssh-to-pgp"))
         (tail (file-append coreutils "/bin/tail")))
    (mixed-text-file
     "generate-host-key.sh"
     "#!/usr/bin/env -S bash -l\n"
     "ssh-to-pgp-private-key () {\n"
     "    " ssh-to-pgp " -comment 'Imported from SSH' "
                "-email root@localhost "
                "-format armor "
                "-name root "
                " -i /etc/ssh/ssh_host_rsa_key -private-key 2>/dev/null\n"
     "}\n"
     "export key_id=$(ssh-to-pgp-private-key | "
     gpg " --import-options show-only --import |"
     grep " -E -A 1 'sec(#)?' | " tail " -1 2>&1)\n"
     "if " gpg " --list-keys | " grep " -i -q $key_id; then\n"
     "    " echo " ${key_id} already known, skipping import...\n"
     "else\n"
     "    ssh-to-pgp-private-key | "
          gpg " --import\nfi\n")))

(define-public sops-guix-utils
  (package
    (name "sops-guix-utils")
    (version "0.0.0-0")
    (source generate-host-key.sh)
    (build-system copy-build-system)
    (arguments
     (list
      ;; There's no point in substitutes.
      #:substitutable? #f
      #:install-plan
      #~'(("generate-host-key.sh" "/bin/generate-host-key.sh"))
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
           ssh-to-pgp))
    (synopsis "Utilities for deploying secrets")
    (description "This package bundles a set of scripts required to facilitate
the deploying of SOPS Guix secrets.")
    (home-page "https://sr.ht/~fishinthecalculator/sops-guix/")
    (license license:gpl3)))
