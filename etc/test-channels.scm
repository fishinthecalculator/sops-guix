(use-modules (guix utils))

(list (channel
       (name 'guix)
       (url "https://git.guix.gnu.org/guix.git")
       (commit "72ce4088da077b3b2283be23bb6cb0065b8d94b6")
       (introduction
        (make-channel-introduction
         "9edb3f66fd807b096b48283debdcddccfea34bad"
         (openpgp-fingerprint
          "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
      (channel
       (name 'sops-guix)
       (url (string-append "file://" (dirname (current-source-directory))))
       (branch "main")))
