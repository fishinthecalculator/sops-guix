;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops services databases)
  #:use-module (gnu)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services databases)
  #:use-module (gnu services shepherd)
  #:use-module (sops secrets)
  #:use-module (sops services sops)
  #:use-module (srfi srfi-1)
  #:export (sops-secrets-postgres-role
            sops-secrets-postgres-role?
            sops-secrets-postgres-role-fields
            sops-secrets-postgres-role-password
            sops-secrets-postgres-role-value

            sops-secrets-postgresql-set-passwords
            sops-secrets-postgresql-role-shepherd-service
            sops-secrets-postgresql-role-service-type))

(define-configuration/no-serialization sops-secrets-postgresql-role
  (password
   (sops-secret)
   "A sops-secret record representing the role password.")
  (value
   (postgresql-role)
   "The postgres-role record for the password."))

(define (sops-secrets-postgresql-set-passwords roles)
  (define (roles->queries roles)
    (string-join
     (map
      (lambda (role)
        (let ((name (postgresql-role-name
                     (sops-secrets-postgresql-role-value role)))
              (password
               (sops-secrets-postgresql-role-password role)))
          (string-append "ALTER ROLE " name " WITH PASSWORD '$(cat /run/secrets/" (sops-secret->file-name) ")';")))
      roles)
     " "))

  #~(let ((bash #$(file-append bash-minimal "/bin/bash"))
          (psql #$(file-append postgresql "/bin/psql")))
      (list bash "-c" (string-append psql " -c \"" #$(roles->queries roles) "\""))))

(define (sops-secrets-postgresql-role-shepherd-service config)
  (list (shepherd-service
         (requirement '(postgres-roles))
         (provision '(sops-secrets-postgres-roles))
         (one-shot? #t)
         (start
          #~(lambda args
              (let ((pid (fork+exec-command
                          #$(sops-secrets-postgresql-set-passwords config)
                          #:user "postgres"
                          #:group "postgres")))
                (zero? (cdr (waitpid pid))))))
         (documentation "Set PostgreSQL roles passwords."))))

(define sops-secrets-postgresql-role-service-type
  (service-type (name 'sops-secrets-postgresql-role)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          sops-secrets-postgresql-role-shepherd-service)
                       (service-extension sops-secrets-service-type
                                          (lambda (config)
                                            (map sops-secrets-postgresql-role-password config)))
                       (service-extension postgresql-role-service-type
                                          (lambda (config)
                                            (map sops-secrets-postgresql-role-value config)))))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description "Ensure the specified PostgreSQL roles are
created after the PostgreSQL database is started.")))
