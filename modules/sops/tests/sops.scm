;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops tests sops)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:use-module ((guix utils) #:select (current-source-directory))
  #:export (%test-sops
            %test-sops-age))


;;;
;;; The SOPS service.
;;;

(define %channel-root
  (string-append (current-source-directory) "/data"))

(define %secrets-dir
  (local-file (string-append %channel-root "/secrets")
              "secrets-dir"
              #:recursive? #t))

(define rsa-ssh-key
  "-----BEGIN OPENSSH PRIVATE KEY-----
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
-----END OPENSSH PRIVATE KEY-----")

(define ed25519-ssh-key
  "-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACD5fwRTlcvbopXNlesUvQ3aMYZdNErqVM8Ma6la8dLaigAAAJDg0TQs4NE0
LAAAAAtzc2gtZWQyNTUxOQAAACD5fwRTlcvbopXNlesUvQ3aMYZdNErqVM8Ma6la8dLaig
AAAEDFQJ/AQWZPP1EaqOCwbMFm05NE1RXNeWmKS4RT45E0H/l/BFOVy9uilc2V6xS9Ddox
hl00SupUzwxrqVrx0tqKAAAADHBhdWxAc2tpYmlkaQE=
-----END OPENSSH PRIVATE KEY-----")

(define-public (secrets-file file-name)
  (file-append %secrets-dir "/" file-name))

;; To add secrets to this file follow these steps:
;;
;; 1. Import the test vm OpenPGP public key with
;;   gpg --import < modules/sops/tests/data/komputilo_pub.gpg
;;
;; 2. cd modules/sops/tests/data
;;
;; 3. SOPS_AGE_KEY_FILE="$(pwd)/age-keys.txt" sops secrets/komputilo.yaml
;;
(define-public komputilo.yaml
  (secrets-file "komputilo.yaml"))

;; TODO: Test extra symlink
(define-public restic-secret
  (sops-secret
   (key '("restic"))
   (file komputilo.yaml)
   (user "alice")
   (group "users")
   (permissions #o400)))
(define-public bonfire-secret
  (sops-secret
   (key '("bonfire" "postgresql_password"))
   (file komputilo.yaml)
   (user "alice")
   (group "users")
   (permissions #o440)))

(define %sops-os
  (simple-operating-system
   (extra-special-file "/etc/ssh/ssh_host_rsa_key"
                       (plain-file "ssh_host_rsa_key" rsa-ssh-key))
   (simple-service 'profile-with-gnupg profile-service-type
                    (specifications->packages '("gawk" "gnupg")))
   (service sops-secrets-service-type
            (sops-service-configuration
             (generate-key? #t)
             (verbose? #t)
             (log-directory "/var/log")
             (secrets
              (list
               bonfire-secret
               restic-secret))))))

(define (run-sops-test)
  "Run tests in %SOPS-OS."
  (define os
    (marionette-operating-system
     %sops-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 512)))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (gnu services herd))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "sops")

          (sleep 60)

          (test-equal "GPG key has been generated"
            "0C59A513D163688D280F9AFA5EB8AAF08C879499\n"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen) (ice-9 textual-ports))
                (define port
                  (open-input-pipe
                   "gpg --fingerprint --with-colons root@localhost | awk -F: '$1 == \"fpr\" {print $10;}'"))
                (define key (with-input-from-port port
                              (lambda ()
                                (get-string-all (current-input-port)))))
                (close-port port)
                key)
             marionette))

          (test-assert "secret created"
            (wait-for-file "/run/secrets/restic" marionette))

          (sleep 5)

          (test-equal "restic secret content is sound"
            "hello world"
            (marionette-eval
             '(begin
                (use-modules (ice-9 textual-ports))
                (call-with-input-file "/run/secrets/restic" get-string-all))
             marionette))

          (test-equal "bonfire secret content is sound"
            "super-secret"
            (marionette-eval
             '(begin
                (use-modules (ice-9 textual-ports))
                (call-with-input-file "/run/secrets/bonfire/postgresql_password" get-string-all))
             marionette))

          (test-equal "restic secret permissions are correct"
            #o400
            (marionette-eval
             '(begin
                (stat:perms (stat "/run/secrets/restic")))
             marionette))

          (test-equal "restic user owner is correct"
            "alice"
            (marionette-eval
             '(begin
                (passwd:name
                 (getpwuid
                  (stat:uid
                   (stat "/run/secrets/restic")))))
             marionette))

          (test-equal "restic user group is correct"
            "users"
            (marionette-eval
             '(begin
                (group:name
                 (getgrgid
                  (stat:gid
                   (stat "/run/secrets/restic")))))
             marionette))

          (test-equal "bonfire secret permissions are correct"
            #o440
            (marionette-eval
             '(begin
                (stat:perms (stat "/run/secrets/bonfire/postgresql_password")))
             marionette))

          (test-equal "bonfire user owner is correct"
            "alice"
            (marionette-eval
             '(begin
                (passwd:name
                 (getpwuid
                  (stat:uid
                   (stat "/run/secrets/bonfire/postgresql_password")))))
             marionette))

          (test-equal "bonfire user group is correct"
            "users"
            (marionette-eval
             '(begin
                (group:name
                 (getgrgid
                  (stat:gid
                   (stat "/run/secrets/bonfire/postgresql_password")))))
             marionette))

          (test-assert "sops-secrets.log created"
            (wait-for-file "/var/log/sops-secrets.log" marionette))

          (test-assert "sops-secrets-host-key.log created"
            (wait-for-file "/var/log/sops-secrets-host-key.log" marionette))

          (test-assert "sops-secret-restic.log created"
            (wait-for-file "/var/log/sops-secret-restic.log" marionette))

          (test-end))))

  (gexp->derivation "sops-test" test))

(define %test-sops
  (system-test
   (name "sops")
   (description "Test the SOPS service with default settings.")
   (value (run-sops-test))))

(define %sops-age-os
  (simple-operating-system
   (extra-special-file "/etc/ssh/ssh_host_ed25519_key"
                       (plain-file "ssh_host_ed25519_key" ed25519-ssh-key))
   (simple-service 'profile-with-gnupg profile-service-type
                   (specifications->packages '("age")))
   (service sops-secrets-service-type
            (sops-service-configuration
             (generate-key? #t)
             (verbose? #t)
             (host-ssh-key "/etc/ssh/ssh_host_ed25519_key")
             (log-directory "/var/log")
             (secrets
              (list
               bonfire-secret
               restic-secret))))))

(define (run-age-sops-test)
  "Run tests in %SOPS-AGE-OS."
  (define os
    (marionette-operating-system
     %sops-age-os
     #:imported-modules '((gnu services herd))))

  (define vm
    (virtual-machine
     (operating-system os)
     (memory-size 512)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "sops-age")

          (sleep 60)

          (test-equal "age key has been generated"
            "age16mhhz8lsra7h0z60h89xwhsdhphxqgr88hqu0hky4kt67qpp0vrs0xp556\n"
            (marionette-eval
             '(begin
                (use-modules (ice-9 popen) (ice-9 textual-ports))
                (define port
                  (open-input-pipe
                   "age-keygen -y /root/.config/sops/age/keys.txt"))
                (define key (with-input-from-port port
                              (lambda ()
                                (get-string-all (current-input-port)))))
                (close-port port)
                key)
             marionette))

          (test-assert "secret created"
            (wait-for-file "/run/secrets/restic" marionette))

          (sleep 5)

          (test-equal "restic secret content is sound"
            "hello world"
            (marionette-eval
             '(begin
                (use-modules (ice-9 textual-ports))
                (call-with-input-file "/run/secrets/restic" get-string-all))
             marionette))

          (test-equal "bonfire secret content is sound"
            "super-secret"
            (marionette-eval
             '(begin
                (use-modules (ice-9 textual-ports))
                (call-with-input-file "/run/secrets/bonfire/postgresql_password" get-string-all))
             marionette))

          (test-equal "restic secret permissions are correct"
            #o400
            (marionette-eval
             '(begin
                (stat:perms (stat "/run/secrets/restic")))
             marionette))

          (test-equal "restic user owner is correct"
            "alice"
            (marionette-eval
             '(begin
                (passwd:name
                 (getpwuid
                  (stat:uid
                   (stat "/run/secrets/restic")))))
             marionette))

          (test-equal "restic user group is correct"
            "users"
            (marionette-eval
             '(begin
                (group:name
                 (getgrgid
                  (stat:gid
                   (stat "/run/secrets/restic")))))
             marionette))

          (test-equal "bonfire secret permissions are correct"
            #o440
            (marionette-eval
             '(begin
                (stat:perms (stat "/run/secrets/bonfire/postgresql_password")))
             marionette))

          (test-equal "bonfire user owner is correct"
            "alice"
            (marionette-eval
             '(begin
                (passwd:name
                 (getpwuid
                  (stat:uid
                   (stat "/run/secrets/bonfire/postgresql_password")))))
             marionette))

          (test-equal "bonfire user group is correct"
            "users"
            (marionette-eval
             '(begin
                (group:name
                 (getgrgid
                  (stat:gid
                   (stat "/run/secrets/bonfire/postgresql_password")))))
             marionette))

          (test-assert "sops-secrets.log created"
            (wait-for-file "/var/log/sops-secrets.log" marionette))

          (test-assert "sops-secrets-host-key.log created"
            (wait-for-file "/var/log/sops-secrets-host-key.log" marionette))

          (test-assert "sops-secret-restic.log created"
            (wait-for-file "/var/log/sops-secret-restic.log" marionette))

          (test-end))))

  (gexp->derivation "sops-age-test" test))

(define %test-sops-age
  (system-test
   (name "sops-age")
   (description "Test the SOPS service with age based host key generation.")
   (value (run-age-sops-test))))
