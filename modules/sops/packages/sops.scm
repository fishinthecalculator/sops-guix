;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2023, 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops packages sops)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages prometheus)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public go-cloud-google-com-go-iam
  (package
    (name "go-cloud-google-com-go-iam")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "iam"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j9dm999ls9mw5b06zj46ik3jxn3c8adk8mn25i1n047gmhlds2k"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet #~(begin
                    (define (delete-all-but directory . preserve)
                      (with-directory-excursion directory
                        (let* ((pred (negate (cut member <>
                                                  (cons* "." ".." preserve))))
                               (items (scandir "." pred)))
                          (for-each (cut delete-file-recursively <>) items))))
                    (delete-all-but "." "iam")))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.25
      #:skip-build? #t
      #:import-path "cloud.google.com/go/iam"
      #:unpack-path "cloud.google.com/go"
      #:phases
      #~(modify-phases %standard-phases
          ;; unpack phase fails for some reason.
          ;; since the package is not built the phase
          ;; is useless anyways, so let's disable it.
          (delete 'unpack))))
    (propagated-inputs (list go-cloud-google-com-go
                             go-cloud-google-com-go-longrunning
                             go-github-com-googleapis-gax-go-v2
                             go-google-golang-org-api
                             go-google-golang-org-genproto
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-grpc
                             go-google-golang-org-protobuf))
    (home-page "https://cloud.google.com/go")
    (synopsis "IAM API")
    (description
     "Package iam supports the resource-specific operations of Google Cloud IAM
(Identity and Access Management) for the Google Cloud Libraries.  See
@url{https://cloud.google.com/iam} for more about IAM.")
    (license license:asl2.0)))

(define-public go-cloud-google-com-go-kms
  (package
    (name "go-cloud-google-com-go-kms")
    (version "1.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googleapis/google-cloud-go")
             (commit (go-version->git-ref version
                                          #:subdir "kms"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l3dc1i1qqj8mfvl7xcm8rgvsi49xaxlzlrx3p4vjhc23skx1688"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet #~(begin
                    (define (delete-all-but directory . preserve)
                      (with-directory-excursion directory
                        (let* ((pred (negate (cut member <>
                                                  (cons* "." ".." preserve))))
                               (items (scandir "." pred)))
                          (for-each (cut delete-file-recursively <>) items))))
                    (delete-all-but "." "kms")))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.25
      #:skip-build? #t
      #:import-path "cloud.google.com/go/kms"
      #:unpack-path "cloud.google.com/go"
      #:phases
      #~(modify-phases %standard-phases
          ;; unpack phase fails for some reason.
          ;; since the package is not built the phase
          ;; is useless anyways, so let's disable it.
          (delete 'unpack))))
    (propagated-inputs (list go-cloud-google-com-go-iam
                             go-cloud-google-com-go-longrunning
                             go-github-com-googleapis-gax-go-v2
                             go-google-golang-org-api
                             go-google-golang-org-genproto
                             go-google-golang-org-genproto-googleapis-api
                             go-google-golang-org-grpc
                             go-google-golang-org-protobuf))
    (home-page "https://cloud.google.com/go")
    (synopsis "Cloud Key Management Service (KMS) API")
    (description
     "Go Client Library for Cloud Key Management Service (KMS) API.")
    (license license:asl2.0)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-internal
  (package
    (name
     "go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-internal")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/security/keyvault/internal"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16xb2gyl48xcc1xc58l8c8qhraxsmar7655348ggi4r2jzyc6sik"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet #~(begin
                    (define (delete-all-but directory . preserve)
                      (with-directory-excursion directory
                        (let* ((pred (negate (cut member <>
                                                  (cons* "." ".." preserve))))
                               (items (scandir "." pred)))
                          (for-each (cut delete-file-recursively <>) items))))
                    (delete-all-but "sdk/security/keyvault" "internal")
                    (delete-all-but "sdk/security" "keyvault")
                    (delete-all-but "sdk" "security")
                    (delete-all-but "." "sdk")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/security/keyvault/internal"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-azure-azure-sdk-for-go-sdk-azcore
                             go-github-com-azure-azure-sdk-for-go-sdk-internal
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Key Vault Internal Module for Go")
    (description
     "This module contains shared code for all the Key Vault SDKs, mainly the
challenge authentication policy.")
    (license license:expat)))

(define-public go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-azkeys
  (package
    (name "go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-azkeys")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Azure/azure-sdk-for-go")
             (commit (go-version->git-ref version
                                          #:subdir "sdk/security/keyvault/azkeys"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jxpzd6sg6fp2mim3sr7gg9y0lvlngf23mmij388ywz4zfvpcqhq"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet #~(begin
                    (define (delete-all-but directory . preserve)
                      (with-directory-excursion directory
                        (let* ((pred (negate (cut member <>
                                                  (cons* "." ".." preserve))))
                               (items (scandir "." pred)))
                          (for-each delete-file-recursively items))))
                    (delete-all-but "sdk/security/keyvault" "azkeys")
                    (delete-all-but "sdk/security" "keyvault")
                    (delete-all-but "sdk" "security")
                    (delete-all-but "." "sdk")))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path
      "github.com/Azure/azure-sdk-for-go/sdk/security/keyvault/azkeys"
      #:unpack-path "github.com/Azure/azure-sdk-for-go"))
    (propagated-inputs (list go-github-com-azure-azure-sdk-for-go-sdk-azcore
                             go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                             go-github-com-azure-azure-sdk-for-go-sdk-internal
                             go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-internal
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/Azure/azure-sdk-for-go")
    (synopsis "Azure Key Vault Keys client module for Go")
    (description
     "@@url{https://github.com/Azure/azure-sdk-for-go/tree/main/sdk/security/keyvault/azkeys/client.go,Source
code} | @@url{https://aka.ms/azsdk/go/keyvault-keys/docs,Package (pkg.go.dev)} |
@@url{https://learn.microsoft.com/azure/key-vault/,Product documentation} |
@@url{https://aka.ms/azsdk/go/keyvault-keys/docs#pkg-examples,Samples}.")
    (license license:expat)))

(define-public go-github-com-aws-aws-sdk-go-v2-service-kms
  (package
    (name "go-github-com-aws-aws-sdk-go-v2-service-kms")
    (version "1.50.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aws/aws-sdk-go-v2")
             (commit (go-version->git-ref version
                                          #:subdir "service/kms"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "068yzhxxxdymr1avb1l1pm9m0p7mcd0zlw5an66mcqldgl7hfivg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aws/aws-sdk-go-v2/service/kms"
      #:unpack-path "github.com/aws/aws-sdk-go-v2"))
    (propagated-inputs (list go-github-com-aws-smithy-go))
    (home-page "https://github.com/aws/aws-sdk-go-v2")
    (synopsis #f)
    (description
     "Package kms provides the API client, operations, and parameter types for AWS Key
Management Service.")
    (license license:asl2.0)))

(define-public go-github-com-envoyproxy-go-control-plane
  (package
    (name "go-github-com-envoyproxy-go-control-plane")
    (version "0.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/envoyproxy/go-control-plane")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "153z0jdbyhbcadiipl5631vnsg74m0fx7h1dmak4mqfjgdahxdvk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/envoyproxy/go-control-plane"
      #:unpack-path "github.com/envoyproxy/go-control-plane"))
    (propagated-inputs (list go-github-com-cncf-xds-go
                             go-github-com-google-go-cmp
                             go-github-com-prometheus-client-model
                             go-github-com-stretchr-testify
                             go-go-opentelemetry-io-proto-otlp
                             go-go-uber-org-goleak
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-grpc
                             go-google-golang-org-protobuf))
    (home-page "https://github.com/envoyproxy/go-control-plane")
    (synopsis "control-plane")
    (description
     "This repository contains a Go-based implementation of an API server that
implements the discovery service APIs defined in
@@url{https://github.com/envoyproxy/data-plane-api,data-plane-api}.")
    (license license:asl2.0)))

(define-public go-github-com-getsops-gopgagent
  (package
    (name "go-github-com-getsops-gopgagent")
    (version "0.0.0-20241224165529-7044f28e491e")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/getsops/gopgagent")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n1r7jkb48y06jw2dhkaqs33j6qgwf9fvbisd1zsr6v2k7wgfvf3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/getsops/gopgagent"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-files
            (lambda _
              (for-each
               (lambda (f)
                 (substitute* (string-append "src/github.com/getsops/gopgagent/" f)
                   (("import \"go\\.mozilla\\.org/gopgagent\"") "")))
               '("gpgagent.go" "gpgagent_test.go")))))))
    (propagated-inputs (list go-go-mozilla-org-gopgagent))
    (home-page "https://github.com/getsops/gopgagent")
    (synopsis #f)
    (description "Package gpgagent interacts with the local GPG Agent.")
    (license license:asl2.0)))

(define-public go-github-com-goware-prefixer
  (package
    (name "go-github-com-goware-prefixer")
    (version "0.0.0-20160118172347-395022866408")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/goware/prefixer")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h1ngpyx4pkhh0x6b7njrmx1pk2w175f4cy3bac909drnaxn7yzc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/goware/prefixer"))
    (home-page "https://github.com/goware/prefixer")
    (synopsis "Prefixer")
    (description
     "Package prefixer implements io.Reader wrapper prepending every line with a given
string.")
    (license license:expat)))

(define-public go-github-com-hashicorp-go-rootcerts
  (package
    (name "go-github-com-hashicorp-go-rootcerts")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-rootcerts")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06z1bxcnr0rma02b6r52m6y0q7niikqjs090vm1i8xi3scyaw1qa"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-rootcerts"))
    (propagated-inputs (list go-github-com-mitchellh-go-homedir))
    (home-page "https://github.com/hashicorp/go-rootcerts")
    (synopsis "rootcerts")
    (description
     "Package rootcerts contains functions to aid in loading CA certificates for TLS
connections.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-secure-stdlib-parseutil
  (package
    (name "go-github-com-hashicorp-go-secure-stdlib-parseutil")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-secure-stdlib")
             (commit (go-version->git-ref version
                                          #:subdir "parseutil"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08wp7cx0br7wz0rwz92v0c6jgbv735l04438k74wgqrghsxgl31m"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet #~(begin
                    (define (delete-all-but directory . preserve)
                      (with-directory-excursion directory
                        (let* ((pred (negate (cut member <>
                                                  (cons* "." ".." preserve))))
                               (items (scandir "." pred)))
                          (for-each (cut delete-file-recursively <>) items))))
                    (delete-all-but "." "parseutil")))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Many tests fail with:
      ;;
      ;; Error:          Not equal:
      ;;                 expected: "username@10.10.1.10:8200"
      ;;                 actual  : ""
      ;;
      ;;                 Diff:
      ;;                 --- Expected
      ;;                 +++ Actual
      ;;                 @@ -1 +1 @@
      ;;                 -username@10.10.1.10:8200
      ;;                 +
      #:tests? #f
      #:import-path "github.com/hashicorp/go-secure-stdlib/parseutil"
      #:unpack-path "github.com/hashicorp/go-secure-stdlib"))
    (propagated-inputs (list go-github-com-hashicorp-go-secure-stdlib-strutil
                             go-github-com-hashicorp-go-sockaddr
                             go-github-com-mitchellh-mapstructure
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/hashicorp/go-secure-stdlib")
    (synopsis #f)
    (description #f)
    (license license:mpl2.0)))

(define-public go-github-com-ryanuber-go-glob
  (package
    (name "go-github-com-ryanuber-go-glob")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ryanuber/go-glob")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mhrjy0iba3jr6bsgy7q50zjr42ar1njn1sb2fvihlkhxgb2ahv2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ryanuber/go-glob"))
    (home-page "https://github.com/ryanuber/go-glob")
    (synopsis "String globbing in golang")
    (description
     "@@code{go-glob} is a single-function library implementing basic string glob
support.")
    (license license:expat)))

(define-public go-github-com-hashicorp-go-secure-stdlib-strutil
  (package
    (name "go-github-com-hashicorp-go-secure-stdlib-strutil")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-secure-stdlib")
             (commit (go-version->git-ref version
                                          #:subdir "strutil"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19jgdmw5snwfq4z8ifsqr917aj535x6fjkv7ma08qmibv1rrxs6s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/hashicorp/go-secure-stdlib/strutil"
      #:unpack-path "github.com/hashicorp/go-secure-stdlib"))
    (propagated-inputs (list go-github-com-ryanuber-go-glob))
    (home-page "https://github.com/hashicorp/go-secure-stdlib")
    (synopsis #f)
    (description #f)
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-retryablehttp-0.7.8
  (package
    (inherit go-github-com-hashicorp-go-retryablehttp)
    (name "go-github-com-hashicorp-go-retryablehttp")
    (version "0.7.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/go-retryablehttp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k9pxdypflyc6i30g7cclm9hfmisllpfgajlysi94vj1l4lp1dp0"))))))

(define-public go-github-com-hashicorp-hcl-1.0.1-vault-7
  (package
    (inherit go-github-com-hashicorp-hcl)
    (name "go-github-com-hashicorp-hcl")
    (version "1.0.1-vault-7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/hashicorp/hcl")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0crhhxk1357l1x4qhi83lxgvmf75j0nm7xl8w4schmjh4a62v9n6"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-github-com-hashicorp-hcl)
       ((#:tests? _ #t) #f)))))

(define-public go-github-com-hashicorp-vault-api
  (package
    (name "go-github-com-hashicorp-vault-api")
    (version "1.23.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hashicorp/vault")
             (commit (go-version->git-ref version
                                          #:subdir "api"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zimh4g8bwj9zz7hjrpk1arg02m3lkyaqmyl9jdw08qkc2vq679p"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; TODO: tests require additional dependencies
      #:tests? #f
      #:import-path "github.com/hashicorp/vault/api"
      #:unpack-path "github.com/hashicorp/vault"))
    (propagated-inputs (list go-github-com-cenkalti-backoff-v4
                             go-github-com-go-jose-go-jose-v4
                             go-github-com-go-test-deep
                             go-github-com-hashicorp-errwrap
                             go-github-com-hashicorp-go-cleanhttp
                             go-github-com-hashicorp-go-hclog
                             go-github-com-hashicorp-go-multierror
                             go-github-com-hashicorp-go-retryablehttp-0.7.8
                             go-github-com-hashicorp-go-rootcerts
                             go-github-com-hashicorp-go-secure-stdlib-parseutil
                             go-github-com-hashicorp-go-secure-stdlib-strutil
                             go-github-com-hashicorp-hcl-1.0.1-vault-7
                             go-github-com-mitchellh-go-homedir
                             go-github-com-mitchellh-mapstructure
                             go-github-com-natefinch-atomic
                             go-github-com-stretchr-testify
                             go-golang-org-x-net
                             go-golang-org-x-time))
    (home-page "https://github.com/hashicorp/vault")
    (synopsis "Vault API")
    (description
     "This provides the @@code{github.com/hashicorp/vault/api} package which contains
code useful for interacting with a Vault server.")
    (license license:mpl2.0)))

(define-public go-github-com-huaweicloud-huaweicloud-sdk-go-v3
  (package
    (name "go-github-com-huaweicloud-huaweicloud-sdk-go-v3")
    (version "0.1.191")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/huaweicloud/huaweicloud-sdk-go-v3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dgvaayihhly5ghphdabhv27z04ff8qzfvvbghmcn2da17w066hf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/huaweicloud/huaweicloud-sdk-go-v3"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-files
            (lambda _
              (for-each
               (lambda (f)
                 (substitute* (string-append "src/github.com/huaweicloud/huaweicloud-sdk-go-v3/core/auth/signer/hkdf/" f)
                   (("import \"golang\\.org/x/crypto/hkdf\"") "")))
               '("hkdf.go" "hkdf_test.go")))))))
    (propagated-inputs (list go-github-com-goccy-go-yaml
                             go-github-com-json-iterator-go
                             go-github-com-shopspring-decimal
                             go-github-com-stretchr-testify
                             go-github-com-tjfoc-gmsm
                             go-go-mongodb-org-mongo-driver
                             go-golang-org-x-crypto
                             go-gopkg-in-ini-v1))
    (home-page "https://github.com/huaweicloud/huaweicloud-sdk-go-v3")
    (synopsis "Requirements")
    (description "Package sdk_doc created for dependence ensure.")
    (license #f)))

(define-public go-github-com-microsoft-go-winio
  (package
    (name "go-github-com-microsoft-go-winio")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/go-winio")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09dxp536m15d3l9aibpfgs9ag55n7gqrgp5app4rcb888c6mclxm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/Microsoft/go-winio"))
    (propagated-inputs (list go-github-com-sirupsen-logrus go-golang-org-x-sys
                             go-golang-org-x-tools))
    (home-page "https://github.com/Microsoft/go-winio")
    (synopsis "go-winio")
    (description
     "This package provides utilities for efficiently performing Win32 IO operations
in Go.  Currently, this package is provides support for genreal IO and
management of.")
    (license license:expat)))

(define-public go-github-com-ory-dockertest-v3
  (package
    (name "go-github-com-ory-dockertest")
    (version "3.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ory/dockertest")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15gv7afmq40hgxvk1kv3i0m44n0lp4ffgrwyg4sn0a7yx91sgazc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/ory/dockertest/v3"
      #:unpack-path "github.com/ory/dockertest"))
    (propagated-inputs (list go-github-com-cenkalti-backoff-v4
                             go-github-com-containerd-continuity
                             go-github-com-docker-cli
                             go-github-com-docker-go-connections
                             go-github-com-docker-go-units
                             go-github-com-go-sql-driver-mysql
                             go-github-com-lib-pq
                             go-github-com-microsoft-go-winio
                             go-github-com-moby-term
                             go-github-com-nvveen-gotty
                             go-github-com-opencontainers-image-spec
                             go-github-com-opencontainers-runc
                             go-github-com-sirupsen-logrus
                             go-github-com-stretchr-testify
                             go-golang-org-x-sys))
    (home-page "https://github.com/ory/dockertest")
    (synopsis "Why should I use Dockertest?")
    (description
     "Use Docker to run your Golang integration tests against third party services on
@strong{Microsoft Windows, Mac OSX and Linux}.")
    (license license:asl2.0)))

(define-public go-go-mozilla-org-gopgagent
  (package
    (name "go-go-mozilla-org-gopgagent")
    (version "0.0.0-20170926210634-4d7ea76ff71a")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mozilla-services/gopgagent")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qbc6m83yfzwmrjyr2z0z2psiqf2bwmzpp69bfz1dqzfcgpq28ja"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; src/go.mozilla.org/gopgagent/gpgagent_test.go:40:13: fmt.Sprintf format %d has arg time.Now() of wrong type time.Time
      ;; src/go.mozilla.org/gopgagent/gpgagent_test.go:76:13: fmt.Sprintf format %d has arg time.Now() of wrong type time.Time))))
      ;; FAIL    go.mozilla.org/gopgagent [build failed]))))
      ;; FAIL
      #:tests? #f
      #:import-path "go.mozilla.org/gopgagent"))
    (home-page "https://go.mozilla.org/gopgagent")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public sops
  (package
    (name "sops")
    (version "3.12.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/getsops/sops")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01w67iv0v9hnxgaklixk871dwnhyhllm3zz36iiwqsd19d5rllfm"))))
    (build-system go-build-system)
    (arguments
     (list #:go go-1.26
           #:install-source? #f
           #:unpack-path "github.com/getsops/sops/v3"
           #:import-path "github.com/getsops/sops/v3/cmd/sops"
           #:build-flags
           #~(list (string-append
                    "-ldflags="
                    "-X github.com/getsops/sops/v3/version.Version="
                    #$(package-version this-package)))
           #:modules
           '(((guix build gnu-build-system) #:prefix gnu:)
             (guix build go-build-system)
             (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'install-license-files
                 (assoc-ref gnu:%standard-phases 'install-license-files)))))
    (propagated-inputs (list go-cloud-google-com-go-kms
                             go-cloud-google-com-go-storage
                             go-filippo-io-age
                             go-github-com-aws-aws-sdk-go-v2
                             go-github-com-aws-aws-sdk-go-v2-config
                             go-github-com-aws-aws-sdk-go-v2-credentials
                             go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
                             go-github-com-aws-aws-sdk-go-v2-service-kms
                             go-github-com-aws-aws-sdk-go-v2-service-s3
                             go-github-com-aws-aws-sdk-go-v2-service-sts
                             go-github-com-azure-azure-sdk-for-go-sdk-azcore
                             go-github-com-azure-azure-sdk-for-go-sdk-azidentity
                             go-github-com-azure-azure-sdk-for-go-sdk-security-keyvault-azkeys
                             go-github-com-blang-semver
                             go-github-com-envoyproxy-go-control-plane
                             go-github-com-fatih-color
                             go-github-com-getsops-gopgagent
                             go-github-com-google-go-cmp
                             go-github-com-google-shlex
                             go-github-com-goware-prefixer
                             go-github-com-hashicorp-go-cleanhttp
                             go-github-com-hashicorp-vault-api
                             go-github-com-huaweicloud-huaweicloud-sdk-go-v3
                             go-github-com-lib-pq
                             go-github-com-mitchellh-go-homedir
                             go-github-com-mitchellh-go-wordwrap
                             go-github-com-ory-dockertest-v3
                             go-github-com-pkg-errors
                             go-github-com-protonmail-go-crypto
                             go-github-com-sirupsen-logrus
                             go-github-com-stretchr-testify
                             go-github-com-urfave-cli
                             go-go-yaml-in-yaml-v3
                             go-golang-org-x-crypto
                             go-golang-org-x-net
                             go-golang-org-x-oauth2
                             go-golang-org-x-sys
                             go-golang-org-x-term
                             go-google-golang-org-api
                             go-google-golang-org-genproto-googleapis-rpc
                             go-google-golang-org-grpc
                             go-google-golang-org-protobuf
                             go-gopkg-in-ini-v1))
    (home-page "https://github.com/getsops/sops")
    (synopsis "Tool for managing secrets")
    (description
     "sops is an editor of encrypted files that supports YAML, JSON,
ENV, INI and BINARY formats and encrypts with AWS KMS, GCP KMS, Azure Key Vault,
age, and PGP.")
    (license license:mpl2.0)))
