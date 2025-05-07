;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops derive)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages password-utils)
  #:use-module (guix gexp)
  #:export (generate-host-key))

(define* (generate-host-key age-key-file
                            gnupg-home
                            gpg-command
                            #:key (host-ssh-key "/etc/ssh/ssh_host_rsa_key")
                            (verbose? #f))
  (with-imported-modules '((guix build utils))
    (program-file
     "generate-host-key"
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 format)
                      (ice-9 popen)
                      (ice-9 rdelim)
                      (ice-9 textual-ports)
                      (srfi srfi-1)
                      (srfi srfi-11))

         (define (slurp invocation)
           (when #$verbose?
             (format #t "Running~{ ~a~}~%" invocation))
           (let* ((port (apply open-pipe* OPEN_READ (pk 'invocation invocation)))
                  (output (get-string-all port))
                  (status (close-pipe port)))
             (pk 'status status)
             (pk 'exit-val (status:exit-val status))
             (values (status:exit-val status) output)))

         (define (run-command invocation)
           (when #$verbose?
             (format #t "Running~{ ~a~}~%" invocation))
           (status:exit-val (apply system* invocation)))

         (define (generate-age-key)
           (slurp
            '(#$(file-append ssh-to-age "/bin/ssh-to-age")
              "-i" #$host-ssh-key "-private-key")))

         (define (age-key-exists? key)
           (string-contains
            (call-with-input-file #$age-key-file get-string-all)
            key))

         (define (age-store key)
           (let* ((port (open-file #$age-key-file "a")))
             (format port "~a~%" key)
             (close-port port)))

         (define (generate-gpg-key)
           (slurp '(#$(file-append ssh-to-pgp "/bin/ssh-to-pgp")
                    "-comment" "Imported from SSH"
                    "-email" "root@localhost"
                    "-format" "armor"
                    "-name" "root"
                    "-i" #$host-ssh-key "-private-key")))

         (define (gpg-key-exists? key)
           #t)

         (define (gpg-import) 1)

         (define-values (age-key-derivation-status age-key)
           (generate-age-key))

         (if (zero? age-key-derivation-status)
             (if (age-key-exists? age-key)
                 (format #t "Derived age key already exists at ~a.~%"
                         #$age-key-file)
                 (age-store age-key))
             (let-values (((gpg-key-derivation-status gpg-key)
                           (generate-gpg-key)))
               (if (zero? gpg-key-derivation-status)
                   (if (gpg-key-exists? gpg-key)
                       (format #t "Derived GnuPG key already exists at ~a.~%"
                               #$gnupg-home)
                       (gpg-import))
                   (format #t "No SOPS compatible key could be generated from ~a.~%"
                           #$host-ssh-key))))))))
