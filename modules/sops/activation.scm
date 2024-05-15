;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>

(define-module (sops activation)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (guix gexp)
  #:use-module (sops secrets)
  #:use-module (sops packages utils)
  #:export (activate-secrets))

(define* (activate-secrets config-file
                           age-key-file
                           gnupg-home
                           sops-secrets
                           sops-package
                           #:key (secrets-directory #f)
                                 (generate-key? #f))
  "Return an activation gexp for provided secrets."
  (let* ((bash (file-append bash-minimal "/bin/bash"))
         (generate-host-key.sh
          (file-append sops-guix-utils "/bin/generate-host-key.sh"))
         (gpg (file-append gnupg "/bin/gpg"))
         (secrets
          (map lower-sops-secret sops-secrets))
         (sops
          (file-append sops-package "/bin/sops")))

    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-1)
                       (srfi srfi-26)
                       (ice-9 ftw)
                       (ice-9 match))
          (define secrets-directory
            (if #$secrets-directory
                #$secrets-directory
                (string-append "/run/user/" (number->string (getuid)) "/secrets")))
          (define extra-links-directory
            (string-append secrets-directory "/extra"))
          (define* (list-content directory #:key (exclude '()))
            (scandir directory
                     (lambda (file)
                       (not (member file `("." ".." ,@exclude))))
                     string<?))

          (setenv "SOPS_AGE_KEY_FILE" #$age-key-file)
          (setenv "GNUPGHOME" #$gnupg-home)
          (setenv "SOPS_GPG_EXEC" #$gpg)

          (if #$generate-key?
              (invoke #$generate-host-key.sh)
              (format #t "no host key will be generated...~%"))

          (format #t "setting up secrets in '~a'...~%" secrets-directory)
          (when (file-exists? secrets-directory)
            (begin
              ;; Cleanup secrets symlink
              (when (file-exists? extra-links-directory)
                (for-each
                 (lambda (link)
                   (define link-path (string-append extra-links-directory "/" link))
                   (define link-target (readlink link-path))
                   ;; The user may have manually deleted the target.
                   (when (file-exists? link-target)
                     (format #t "Deleting ~a -> ~a...~%" link-path link-target)
                     (delete-file-recursively link-target)))
                 (list-content extra-links-directory)))
              ;; Cleanup secrets
              (for-each (compose delete-file-recursively
                                 (cut string-append secrets-directory "/" <>))
                        (list-content secrets-directory))))

          (chdir secrets-directory)
          (symlink #$config-file (string-append secrets-directory "/.sops.yaml"))

          ;; Actually decrypt secrets
          (for-each
           (match-lambda
             ((key secrets-file user group permissions output-type path derived-name)
              (let ((output
                     (string-append secrets-directory "/" derived-name))
                    (gc-link
                     (string-append extra-links-directory "/" derived-name))
                    (uid (passwd:uid
                          (getpwnam user)))
                    (gid (passwd:uid
                          (getgrnam group))))

                (mkdir-p (dirname output))
                (apply invoke `(#$sops "-d"
                                "--extract" ,key
                                "--output" ,output
                                ,@(if output-type
                                      `("--output-type" ,output-type)
                                      '())
                                ,secrets-file))

                ;; Setting owner is supported only in the system service
                (when (= (getuid) 0)
                  (for-each
                   (lambda (file)
                     (chown file uid gid))
                   (find-files (first (string-split derived-name #\/))
                               #:directories? #t)))
                ;; Permissions are supported regardless
                (chmod output permissions)

                (when path
                  ;; First try to setup the symlink
                  (symlink output path)

                  ;; If everything goes well, setup symlink for
                  ;; cleaning up
                  (mkdir-p (dirname gc-link))
                  (symlink path gc-link)))))
           (list #$@secrets))))))
