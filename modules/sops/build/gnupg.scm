;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops build gnupg)
  #:use-module (sops build utils)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (generate-gpg-key
            gpg-key-exists?
            gpg-import))

(define (compute-gpg-key-id key gpg grep sed)
  (define commands
    `((,gpg "--list-packets")
      (,grep "keyid: ")
      (,sed "-E" "s/^.*keyid: ([^[:space:]]+)[[:space:]]*$/\\1/")))

  (receive (from to pids)
      (pipeline commands)
    (format to "~a" key)
    (close to)
    (let ((output (read-line from)))
      (close from)
      (waitpid (first pids))
      (and (string? output)
           (not (string=? "" output))
           output))))

(define* (generate-gpg-key host-ssh-key ssh-to-pgp gpg grep sed #:key verbose?)
  (with-error-to-file "/dev/null"
    (lambda _
      (let*-values (((status key)
                     (slurp `(,ssh-to-pgp
                              "-comment" "Imported from SSH"
                              "-email" "root@localhost"
                              "-format" "armor"
                              "-name" "root"
                              "-i" ,host-ssh-key "-private-key")
                            #:verbose? verbose?))
                    ((id-status key-id)
                     (let ((id
                            (and (zero? status)
                                 (compute-gpg-key-id key gpg grep sed))))
                       (values (if id status 256) id))))

        (values (+ status id-status) key key-id)))))

(define* (gpg-key-exists? gpg-key-id gpg #:key verbose?)
  (let-values (((status keys)
                (slurp
                 `(,gpg "--list-secret-keys")
                 #:verbose? verbose?)))
    (and (zero? status)
         (string-contains-ci keys gpg-key-id))))

(define (gpg-import key gpg)
  (let* ((port (open-output-pipe
                (string-append gpg " --import")))
         (input (display key port))
         (status (close-pipe port)))
    input))
