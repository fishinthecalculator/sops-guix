;;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Copyright © 2024 Giacomo Leidi <therewasa@fishinthecalculator.me>

(define-module (sops secrets)
  #:use-module (gnu services configuration)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (sops validation)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:export (sanitize-sops-key
            key->file-name
            sops-secret->file-name

            sops-secret
            sops-secret?
            sops-secret-fields
            sops-secret-file
            sops-secret-key
            sops-secret-user
            sops-secret-group
            sops-secret-permissions
            sops-secret-path

            lower-sops-secret))

(define (sanitize-sops-string-key value)
  (if (and (string? value)
           (string-match "^(\\[(\".*\"|[0-9]+)\\])+$" value))
      value
      (raise
       (formatted-message
        (G_ "key field value must follow Python's dictionary syntax, but ~a was found.~%~%Please refer to the SOPS documentation to make sure of the actual syntax,
or if you are really it's a bug in SOPS Guix make sure to report it at https://github.com/fishinthecalculator/sops-guix .")
        value))))

(define (sanitize-sops-list-key value)
  (if (every (lambda (key) (or string?
                               (and (integer? value)
                                    (>= value 0))))
             value)
      (apply string-append
             (map (lambda (key)
                    (format #f "[~a]" (if (number? key)
                                          key
                                          (format #f "\"~a\"" key))))
                  value))
      (raise
       (formatted-message
        (G_ "key field value must be a list of strings or positive integers, but ~a was found.~%")
        value))))

(define (sanitize-sops-key value)
  (match value
    ((? string? value)
     (sanitize-sops-string-key value))
    ((? list? value)
     (sanitize-sops-list-key value))
    (_
      (raise
       (formatted-message
        (G_ "key field value must be either a string or a list, but ~a was found.~%")
        value)))))

(define (string-or-list? value)
  (or (string? value)
      (list? value)))

(define (sanitize-output-type value)
  (if (not (maybe-value-set? value))
      value
      (if (and (string? value)
               (member value '("json"
                               "dotenv"
                               "binary"
                               "yaml")))
          value
          (raise
           (formatted-message
            (G_ "output-type field value must one of json, dotenv, binary or yaml but ~a was found.~%")
            value)))))

(define (key->file-name key)
  (string-join
   (filter-map
    (lambda (sub-key)
      (and (not (string-null? sub-key))
           (string-replace-substring sub-key "[" "")))
    (string-split
     (string-replace-substring key "\"" "") #\]))
   "/"))

(define-maybe string)

(define-configuration/no-serialization sops-secret
  (key
   (string-or-list)
   "A key representing a value in the secrets file. Its value can be a string,
which will be directly passed to @command{sops -d --extract} or a list of strings
representing the path of the value you want to reference in the secrets file."
   (sanitizer sanitize-sops-key))
  (file
   (gexp-or-file-like)
   "A gexp or file-like object evaluating to the secrets file.")
  (user
   (string "root")
   "The user owner of the secret.")
  (group
   (string "root")
   "The group owner of the secret.")
  (output-type
   (maybe-string)
   "Currently json, yaml, dotenv and binary are supported. If not set, sops will
use the secrets file's extension to determine the output format."
   (sanitizer sanitize-output-type))
  (permissions
   (number #o440)
   "@code{chmod} permissions that will be applied to the secret.")
  (path
   (maybe-string)
   "An optional path on the filesystem where the secret will be symlinked."))

(define (sops-secret->file-name secret)
  (key->file-name (sops-secret-key secret)))

;; FIXME: This way of lowering secrets is not pretty.
(define (lower-sops-secret secret)
  (let* ((file-name
          (sops-secret->file-name secret))
         (output-type
          (sops-secret-output-type secret))
         (path (sops-secret-path secret)))
    #~'(#$(sops-secret-key secret)
        #$(sops-secret-file secret)
        #$(sops-secret-user secret)
        #$(sops-secret-group secret)
        #$(sops-secret-permissions secret)
        #$(and (maybe-value-set? output-type)
               output-type)
        #$(and (maybe-value-set? path)
               (not (string=? path file-name))
               path)
        #$file-name)))
