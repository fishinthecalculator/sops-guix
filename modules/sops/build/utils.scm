;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops build utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:export (run-command slurp wait-for-file))

(define* (run-command invocation #:key verbose?)
  (when verbose?
    (format (current-error-port) "Running~{ ~a~}~%" invocation))
  (status:exit-val (apply system* invocation)))

(define* (slurp invocation #:key verbose?)
  (when verbose?
    (format (current-error-port) "Running~{ ~a~}~%" invocation))
  (let* ((port (apply open-pipe* OPEN_READ invocation))
         (output (get-string-all port))
         (status (close-pipe port)))
    (values status output)))

(define* (wait-for-file file #:optional (timeout 10))
  ;; Wait until FILE shows up.
  (let loop ((i timeout))
    (cond ((file-exists? file)
           #t)
          ((zero? i)
           (error "file didn't show up" file))
          (else
           (sleep 1)
           (loop (- i 1))))))
