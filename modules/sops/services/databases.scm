;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops services databases)
  #:use-module (gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages databases)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (postgresql-role
            postgresql-role?
            postgresql-role-name
            postgresql-role-password-file
            postgresql-role-permissions
            postgresql-role-create-database?
            postgresql-role-configuration
            postgresql-role-configuration?
            postgresql-role-configuration-host
            postgresql-role-configuration-log
            postgresql-role-configuration-requirement
            postgresql-role-configuration-roles

            postgresql-role-service-type))

(define-record-type* <postgresql-role>
  postgresql-role make-postgresql-role
  postgresql-role?
  (name             postgresql-role-name) ;string
  (password-file    postgresql-role-password-file  ;string
                    (default #f))
  (permissions      postgresql-role-permissions
                    (default '(createdb login))) ;list
  (create-database? postgresql-role-create-database?  ;boolean
                    (default #f))
  (encoding postgresql-role-encoding ;string
            (default "UTF8"))
  (collation postgresql-role-collation ;string
             (default "en_US.utf8"))
  (ctype postgresql-role-ctype ;string
         (default "en_US.utf8"))
  (template postgresql-role-template ;string
            (default "template1")))

(define-record-type* <postgresql-role-configuration>
  postgresql-role-configuration make-postgresql-role-configuration
  postgresql-role-configuration?
  (host             postgresql-role-configuration-host ;string
                    (default "/var/run/postgresql"))
  (requirement      postgresql-role-configuration-requirement ;list-of-symbols
                    (default '()))
  (log              postgresql-role-configuration-log ;string
                    (default "/var/log/postgresql_roles.log"))
  (roles            postgresql-role-configuration-roles
                    (default '()))) ;list

(define (postgresql-create-roles config)
  ;; See: https://www.postgresql.org/docs/current/sql-createrole.html for the
  ;; complete permissions list.
  (define (format-permissions permissions)
    (let ((dict '(bypassrls createdb createrole login replication superuser)))
      (string-join (filter-map (lambda (permission)
                                 (and (member permission dict)
                                      (string-upcase
                                       (symbol->string permission))))
                               permissions)
                   " ")))

  (define (password-value role)
    (string-append "password_" (postgresql-role-name role)))

  (define (role->password-variable role)
    (define file-name
      (postgresql-role-password-file role))
    (if (string? file-name)
        ;; This way passwords do not leak to the command line
        #~(string-append "-v \"" #$(password-value role)
                         "=$(" #+coreutils "/bin/cat " #$file-name ")\"")
        ""))

  (define (roles->queries roles)
    (apply mixed-text-file "queries"
           (append-map
            (lambda (role)
              (match-record role <postgresql-role>
                (name permissions create-database? encoding collation ctype
                      template password-file)
                `("SELECT NOT(EXISTS(SELECT 1 FROM pg_catalog.pg_roles WHERE \
rolname = '" ,name "')) as not_exists;\n"
"\\gset\n"
"\\if :not_exists\n"
"CREATE ROLE \"" ,name "\""
" WITH " ,(format-permissions permissions)
,(if (and (string? password-file)
          (not (string-null? password-file)))
     (string-append
      "\nPASSWORD :'" (password-value role) "'")
     "")
";\n"
,@(if create-database?
      `("CREATE DATABASE \"" ,name "\""
        " OWNER \"" ,name "\"\n"
        " ENCODING '" ,encoding "'\n"
        " LC_COLLATE '" ,collation "'\n"
        " LC_CTYPE '" ,ctype "'\n"
        " TEMPLATE " ,template ";")
      '())
"\\endif\n")))
            roles)))

  (let ((host (postgresql-role-configuration-host config))
        (roles (postgresql-role-configuration-roles config)))
    (program-file "run-queries"
      #~(let ((bash #$(file-append bash-minimal "/bin/bash"))
              (psql #$(file-append postgresql "/bin/psql")))
          (define command
            (string-append
             "set -e; exec " psql " -c -a -h " #$host " -f "
             #$(roles->queries roles) " "
             (string-join
              (list
               #$@(map role->password-variable roles))
              " ")))
          (execlp bash bash "-c" command)))))

(define (postgresql-role-shepherd-service config)
  (match-record config <postgresql-role-configuration>
    (log requirement)
    (list (shepherd-service
           (requirement `(user-processes postgres ,@requirement))
           (provision '(sops-postgres-roles))
           (one-shot? #t)
           (start
            #~(lambda args
                (zero? (spawn-command
                        #$(postgresql-create-roles config)
                        #:user "postgres"
                        #:group "postgres"
                        ;; XXX: As of Shepherd 1.0.2, #:log-file is not
                        ;; supported.
                        ;; #:log-file #$log
                        ))))
           (documentation "Create PostgreSQL roles.")))))

(define postgresql-role-service-type
  (service-type (name 'sops-postgres-role)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          postgresql-role-shepherd-service)))
                (compose concatenate)
                (extend (lambda (config extended-roles)
                          (match-record config <postgresql-role-configuration>
                            (host roles)
                            (postgresql-role-configuration
                             (inherit config)
                             (host host)
                             (roles (append roles extended-roles))))))
                (default-value (postgresql-role-configuration))
                (description "Ensure the specified PostgreSQL roles are
created after the PostgreSQL database is started.")))
