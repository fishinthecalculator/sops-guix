(use-modules (blue subprocess)
             (blue types blueprint)
             (blue types command))

(define-command (check-command arguments)
  ((invoke "check")
   (category 'dispatch))
  (zero?
   (popen "guix"
          `("repl"
            "-L" ,(in-vicinity (getcwd) "modules")
            "--"
            "tests/test-sops.scm"))))

(define-command (system-tests-command arguments)
  ((invoke "system-tests")
   (category 'dispatch))
  (zero?
   (popen "guix"
          `("build"
            "-L" ,(in-vicinity (getcwd) "modules")
            "-m" "etc/manifests/system-tests.scm"))))

(define-command (clean-command arguments)
  ((invoke "clean")
   (category 'dispatch))
  (zero?
   (popen "rm"
          `("-rfv" ,(string-append (getcwd) "/*.log")))))

(blueprint
 (commands
  (list
   check-command
   clean-command
   system-tests-command)))
