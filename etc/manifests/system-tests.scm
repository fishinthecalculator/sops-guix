;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2016, 2018-2020, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

;; This manifest has been adapted by the system tests manifest from Guix
;; mainline.

(use-modules (sops tests)
             ((gnu tests) #:select (system-test system-test-name system-test-value))
             (gnu packages package-management)
             (guix monads)
             (guix store)
             ((guix git-download) #:select (git-predicate))
             ((guix utils) #:select (current-source-directory))
             (git)
             (ice-9 match))

(define (source-commit directory)
  "Return the commit of the head of DIRECTORY or #f if it could not be
determined."
  (let ((repository #f))
    (catch 'git-error
      (lambda ()
        (set! repository (repository-open directory))
        (let* ((head   (repository-head repository))
               (target (reference-target head))
               (commit (oid->string target)))
          (repository-close! repository)
          commit))
      (lambda _
        (when repository
          (repository-close! repository))
        #f))))

(define (tests-for-current-sops-guix source commit)
  "Return a list of tests for perform, using sops-guix built from SOURCE, a channel
instance."
  ;; Honor the 'TESTS' environment variable so that one can select a subset
  ;; of tests to run in the usual way:
  ;;
  ;;   TESTS=installed-os guix build -m etc/manifests/system-tests.scm
  (let ((guix (channel-source->package source #:commit commit)))
    (map (lambda (test)
           (system-test
            (inherit test)
            (value (store-parameterize ((current-guix-package guix))
                     (system-test-value test)))))
         (match (getenv "TESTS")
           (#f
            (all-system-tests))
           ((= string-tokenize (tests ...))
            (filter (lambda (test)
                      (member (system-test-name test) tests))
                    (all-system-tests)))))))

(define (system-test->manifest-entry test)
  "Return a manifest entry for TEST, a system test."
  (manifest-entry
    (name (string-append "test." (system-test-name test)))
    (version "0")
    (item test)))

(define (system-test-manifest)
  "Return a manifest containing all the system tests, or all those selected by
the 'TESTS' environment variable."
  (define source
    (string-append (current-source-directory) "/../.."))

  (define commit
    ;; Fetch the current commit ID so we can potentially build the same
    ;; derivation as ci.guix.gnu.org.
    (source-commit source))

  ;; Intern SOURCE so that 'build-from-source' in (guix channels) sees
  ;; "fresh" file names and thus doesn't find itself loading .go files
  ;; from ~/.cache/guile when it loads 'build-aux/build-self.scm'.
  (let* ((source (local-file source
                             (if commit
                                 (string-append "sops-guix-"
                                                (string-take commit 7))
                                 "sops-guix-source")
                             #:recursive? #t
                             #:select?
                             (or (git-predicate source)
                                 (const #t))))
         (tests  (tests-for-current-sops-guix source commit)))
    (format (current-error-port) "Selected ~a system tests...~%"
            (length tests))

    (manifest (map system-test->manifest-entry tests))))

;; Return the manifest.
(system-test-manifest)
