;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops build age)
  #:use-module (guix build utils)
  #:use-module (sops build utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:export (age-key-exists?
            age-store
            generate-age-key))

(define (age-key-exists? key age-key-file)
  (and
   (file-exists? age-key-file)
   (integer?
    (string-contains
     (call-with-input-file age-key-file get-string-all)
     (string-trim-both key)))))

(define* (age-store key age-key-file #:key verbose?)
  (when verbose?
    (format
     (current-error-port) "Appending age key to ~a.~%" age-key-file))
  (mkdir-p (dirname age-key-file))
  (let ((port (open-file age-key-file "a")))
    (format port "~a" key)
    (close-port port)
    (chmod age-key-file #o400)))

(define* (generate-age-key ssh-to-age host-ssh-key #:key verbose?)
  (with-error-to-file "/dev/null"
    (lambda _
      (slurp
       `(,ssh-to-age "-i" ,host-ssh-key "-private-key")
       #:verbose? verbose?))))
