;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops build activation)
  #:use-module (guix build utils)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (sops-secrets-directories
            sops-secrets-setenv
            sops-secret-cleanup
            sops-secret-create))

(define* (sops-secrets-directories #:optional maybe-secrets-directory)
  (define secrets-directory
    (or maybe-secrets-directory
        (string-append
         "/run/user/" (number->string (getuid)) "/secrets")))

  (define extra-links-directory
    (string-append secrets-directory "/extra"))

  (values secrets-directory extra-links-directory))

(define* (sops-secrets-setenv age-key-file gnupg-home gpg-command #:key verbose?)
  "Set the environment variables required by SOPS to retreive age and GnuPG
command line entrypoints."
  (setenv "SOPS_AGE_KEY_FILE" age-key-file)
  (setenv "GNUPGHOME" gnupg-home)
  (setenv "SOPS_GPG_EXEC" gpg-command)
  (when verbose?
    (for-each (lambda (var)
                (format
                 (current-error-port) "~a: ~a~%" var (getenv var)))
              '("SOPS_AGE_KEY_FILE"
                "GNUPGHOME"
                "SOPS_GPG_EXEC"))))

(define (sops-secret-cleanup secret link extra-links-directory)
  "sops-guix secrets can be linked to arbitrary locations once they are
provisioned.  The service keeps track of these links into EXTRA-LINKS-DIRECTORY,
to be able to manage their lifecycle.  This procedure's purpose is to cleanup
symlinks and secrets files before provisioning new ones."
  (define* (list-content directory #:key (exclude '()))
    (scandir directory
             (lambda (file)
               (not (member file `("." ".." ,@exclude))))
             string<?))

  ;; Cleanup secret symlink
  (when (and link (file-exists? extra-links-directory))
    (for-each
     (lambda (extra-link)
       (define link-path (string-append extra-links-directory "/" extra-link))
       (define link-target (readlink link-path))
       ;; Identify current's secret link
       (when (string=? link-target link)
         ;; The user may have manually deleted the target.
         (when (file-exists? link-target)
           (format (current-error-port)
                   "Removing ~a -> ~a...~%"
                   link-path link-target)
           (delete-file-recursively link-target))))
     (list-content extra-links-directory)))

  ;; Cleanup secret
  (when (file-exists? secret)
    (format (current-error-port) "Removing ~a...~%" secret)
    (delete-file secret)))

(define* (sops-secret-create sops secret secrets-directory
                             extra-links-directory #:key verbose?)
  "Create SECRET by calling SOPS.  Secrets are created in SECRETS-DIRECTORY
and can be symlinked to EXTRA-LINKS-DIRECTORY, depending on user configuration."
  ;; Actually decrypt secrets
  ((match-lambda
     ((key secrets-file user group permissions output-type path derived-name)
      (let* ((output
              (string-append secrets-directory "/" derived-name))
             (gc-link
              (string-append extra-links-directory "/" derived-name))
             (uid (passwd:uid
                   (getpwnam user)))
             (gid (passwd:uid
                   (getgrnam group)))
             (command
              `(,sops "-d"
                "--extract" ,key
                ,@(if output-type
                      `("--output-type" ,output-type)
                      '())
                ,secrets-file)))

        (when verbose?
          (format (current-error-port) "Running~{ ~a~}~%" command))
        (mkdir-p (dirname output))

        ;; First, create a temporary file
        (let* ((port (mkstemp (string-append (dirname output)
                                             "/secret-XXXXXX")))
               (tmp (port-filename port)))
          ;; Set it read/write only for the current user
          (chmod port #o600)
          ;; Write the secret
          (spawn sops command #:output port)
          (close-port port)
          ;; Rename the temporary file to its actual name
          (rename-file tmp output))

        (when verbose?
          (format (current-error-port) "~a has been created.~%" output))

        ;; Setting owner is supported only in the system service
        (when (= (getuid) 0)
          (for-each
           (lambda (file)
             (when verbose?
               (format
                (current-error-port)
                "Changing owner of ~a to ~a:~a~%" file uid gid))
             (chown file uid gid))
           (find-files
            (string-append
             secrets-directory "/" (first (string-split derived-name #\/)))
            #:directories? #t)))
        ;; Permissions are supported regardless
        (chmod output permissions)
        (when verbose?
          (format
           (current-error-port)
           "Setting ~a's permissions to ~a~%" output permissions))

        (when path
          ;; First try to setup the symlink
          (symlink output path)

          ;; If everything goes well, setup symlink for
          ;; cleaning up
          (mkdir-p (dirname gc-link))
          (symlink path gc-link)
          (when verbose?
            (format (current-error-port)
                    "Setup symlink at ~a and ~a~%" path gc-link))))))
   secret))
