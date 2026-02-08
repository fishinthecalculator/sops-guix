# SOPS Guix

[![CI](https://github.com/fishinthecalculator/sops-guix/actions/workflows/main.yml/badge.svg)](https://github.com/fishinthecalculator/sops-guix/actions/workflows/main.yml)

This project aims at implementing secure provisioning of secrets with Guix and [SOPS](https://getsops.io). It was strongly inspired from NixOS' [sops-nix](https://github.com/Mic92/sops-nix).

It works by putting encrypted secrets in the store and by adding a one-shot Shepherd service that decrypts them at startup in a ramfs/tmpfs filesystem. This means that clear text secrets never hit the disk and that you can (and actually are encouraged to) check in your SOPS secrets in the same version control system you use to track you Guix configurations.

## Secure secret provisioning with Guix

This channels exposes the `sops-secrets-service-type` Guix service and the `sops-secret` record to safely handle secrets with Guix.

Assuming that the right private keys are also provided, `sops-secret`s can be included in Guix images, deployed with `guix deploy` and included in Guix System/Home containers.

#### Features

- Support for different ecryption standards (e.g. age and GnuPG), even mixed.
- One Shepherd service for each secret. This way applications can only wait for the secret they are interested in to start. It is also possible to add a dependency to the `sops-secrets` service, which will itself wait for all secrets to be provisioned.
- Derive target host keys (both age and GnuPG keys) from SSH keys.
- By design encrypted secrets can be either included in the Git repository that contains your Guix code, or they can be distributed out of band.

#### How `sops-secret`s Guile records can be used

`sops-secret` records are the Guile API used to reference secrets in your code.  The following example shows how to provision a Postgresql database with a password set via a `sops-guix` secret. You can find a more complete example in the [`sops-guix` tutorial](doc/TUTORIAL.md).

``` scheme
(use-modules ...
             (sops secrets)
             (sops services sops))

;; First let's define the secret
(define postgresql-secret
  (sops-secret
   (key '("postgresql"))
   ;; Here you can pass any absolute file name. It doesn't
   ;; need to be under version control nor in the
   ;; same directory as the Guix code.
   (file (local-file "/home/user/.secrets/secrets.yaml"
                     "secrets.yaml"))))

;; Now we can reference it inside an operating-system record.
;; Or similarly inside a home-environment record.
(operating-system
  ...
  (services
    ...
    ;; Instantiate the sops-secrets service
    (service sops-secrets-service-type
             (sops-service-configuration
              (secrets (list postgresql-secret))))

    ;; Instantiate PostgreSQL process
    (service postgresql-service-type)
    ;; Create a database, a user and set the user password
    (service postgresql-role-service-type
             (postgresql-role-configuration
              ;; Wait for sops secrets to be provisioned before
              ;; creating the database
              (shepherd-requirement
               (append %default-postgresql-role-shepherd-requirement
                       '(sops-secrets)))
              (roles
               (list (postgresql-role
                      (create-database? #t)
                      ;; Create a user and a db named test
                      (name "test")
                      ;; Set the user's password to the content
                      ;; of the secret
                      (password-file
                       (sops-secret->secret-file postgresql-secret)))))))))
```

#### How secret files can be used

Secrets are provisioned as files (usually) under `/run/secrets`. How to use them really depends on which application you need the secret for. As an example, restic is a program to do encrypted backups and it supports a `--password-file` CLI option. Secret files can be to restore a backup like so:

``` bash
restic --repository rclone:onedrive:backup --password-file /run/secrets/restic restore --target ./restored latest
```

Some applications expect secrets to be provided as environment variables, in which case the application entrypoint can be wrapped in a bash script like the following:

``` bash
read -d $'\x04' variable < /run/user/$UID/secrets/super_secret_password
export variable
./application_entrypoint
```

The purpose of `sops-guix` is simply to safely bring secrets from your Guix configuration to one or more machines, after that it is up to applications and users to figure out how to use them.

## Configure

To configure Guix for using this channel you need to create a `.config/guix/channels.scm` file with the following content:

``` scheme
(cons* (channel
        (name 'sops-guix)
        (url "https://github.com/fishinthecalculator/sops-guix.git")
        (branch "main")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
          (openpgp-fingerprint
           "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
       %default-channels)
```

Otherwise, if you already have a `.config/guix/channels.scm` you can simply prepend this channel to the preexisting ones:

``` scheme
(cons* (channel
        (name 'sops-guix)
        (url "https://github.com/fishinthecalculator/sops-guix.git")
        (branch "main")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9"
          (openpgp-fingerprint
           "8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2"))))
       (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       %default-channels)
```

### What is a Guix channel?

A [channel](https://guix.gnu.org/manual/devel/en/guix.html#Channels) is roughly the Guix equivalent of the AUR or container registries. It's a software repository providing Guix package and service definitions.

You can search for package and service definitions from this channel and many others at [toys.whereis.social](https://toys.whereis.social).

## Contributing

All contributions are welcome. If you have commit access please remember to setup the authentication hook with

```bash
git fetch origin keyring:keyring
guix git authenticate --cache-key=channels/sops-guix 0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9 '8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2'
```

## License

Unless otherwise stated all the files in this repository are to be considered under the GPL 3.0 terms. You are more than welcome to open issues or send patches.
