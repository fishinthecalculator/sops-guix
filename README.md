# SOPS Guix

[![CI](https://github.com/fishinthecalculator/sops-guix/actions/workflows/main.yml/badge.svg)](https://github.com/fishinthecalculator/sops-guix/actions/workflows/main.yml)

This project aims at implementing secure provisioning of secrets with Guix and [SOPS](https://getsops.io). It was strongly inspired from NixOS' [sops-nix](https://github.com/Mic92/sops-nix).

## Secure secret provisioning with Guix

This channels exposes the `sops-secrets-service-type` Guix service and the `sops-secret` record to safely handle secrets with Guix. It works by putting encrypted secrets in the store and by adding a one-shot Shepherd service that decrypts them at startup in a ramfs/tmpfs filesystem. This means that clear text secrets never hit the disk and that you can (and actually are encouraged to) check in your SOPS secrets in the same version control system you use to track you Guix configurations.

Assuming that the right private keys are also provided, `sops-secret`s can be included in Guix images, deployed with `guix deploy` and included in Guix System/Home containers.

### Creating secrets with SOPS

First of all you need to create encrypted secrets with SOPS. To do so I'm assuming you already have a GPG key for yourself and the machines you want to deploy secrets to. You should be able to list the private keys you have in your keyring with

``` bash
user1@home:~ $ gpg --list-secret-keys
/home/user1/.gnupg/pubring.kbx
------------------------
sec   ed25519 2023-12-01 [SC] [expires: 2907-11-30]
      8D1060B96BB8B7249AED41CC193B701E2SODIJNS
uid           [ultimate] user1@example.org
ssb   cv25519 2023-12-01 [E]

pub   rsa3072 1970-01-01 [SCE]
      8C3E4F6EB38828939029AE7BE9B6AF0CD39DD935
uid           [ unknown] root (Imported from SSH) <root@localhost>

pub   rsa3072 1970-01-01 [SCE]
      ZZ3E4VREB38800039029AE7BE9B6AF0CD39AALH9
uid           [ unknown] root (Imported from SSH) <root@localhost>
```


If you don't have a suitable set of GPG keys it's pretty simple to find online how generate them. Once you have a suitable set of keys for yourself and your machines you are ready to create the only configuration you need for SOPS: a `.sops.yaml` file that you will place in your project's root directory, or anyway in the same directory where you keep your system configuration. In this file you define which keys will be able to access your secrets files, it may very well be something like:

``` yaml
keys:
    - &user_user1 8D1060B96BB8B7249AED41CC193B701E2SODIJNS
    - &host_host1 8C3E4F6EB38828939029AE7BE9B6AF0CD39DD935
    - &host_host2 ZZ3E4VREB38800039029AE7BE9B6AF0CD39AALH9

creation_rules:
    - path_regex: .*common\.yaml$
      key_groups:
          - pgp:
                - *user_user1
                - *host_host1
                - *host_host2
    - path_regex: .*host1\.yaml$
      key_groups:
          - pgp:
                - *user_user1
                - *host_host1
```

In this file we define three keys called `user_user1`, `host_host1` and `host_host2` . The prefixes `host_` and `user_` are just a convention to indicate that some GPG keys belong to users and some belong to machines.

We now have defined two secrets file names patterns and we declared permissions for each key, it should be possible now to run the following in your projects root directory:

``` bash
sops common.yaml
```

This will open your default editor with an example content to define your secrets value. You can edit it or delete it and your own content for example:

``` yaml
wireguard:
    private: MYPRIVATEKEY
```

after saving and closing the file you can see by `cat`ting the secret file that `sops` encrypted it before saving it, so you are free to check it in your VCS.

### Making sure the right host keys are in the configured GnuPG keyring

For hosts to be able to decrypt secrets you need to provide in the `root` user keyring (or anyway the keyring located at the configured `gnupg-homedir`) the keys you defined in your `.sops.yaml`. So based on the above example you'd need to provide `8C3E4F6EB38828939029AE7BE9B6AF0CD39DD935`'s private key on `host1` and `ZZ3E4VREB38800039029AE7BE9B6AF0CD39AALH9`'s private key on `host2` .

To check that your key is correctly imported into the keyring run:

``` bash
user1@host1:~ $ sudo gpg --list-secret-keys
/root/.gnupg/pubring.kbx
------------------------
pub   rsa3072 1970-01-01 [SCE]
      8C3E4F6EB38828939029AE7BE9B6AF0CD39DD935
uid           [ unknown] root (Imported from SSH) <root@localhost>
```

By setting `generate-key?` to `#t` in `sops-service-configuration` a GPG key will be automatically derived for you from your system's `/etc/ssh/ssh_host_rsa_key` and added to the configured keyring. It is *discouraged* to do so and you are more than encouraged to autonomously provide a key in your configured keyring. While having `sops-guix` generate a keypair on your behalf is easier, you have less control on the key. For example some could have a requirement to rotate the key periodically. You can certainly enable it once, deploy your configuration, turn it off and handle the key from there, but having the key managed by `sops-guix` may not be the best default choice in every case.

### Adding secrets to your `operating-system` record

Now, supposing you have your `operating-system` file in the same directory where you have your `.sops.yaml` and `common.yaml` files, you can simply add the following to your configuration:

``` scheme
(use-modules (sops secrets)
             (sops services sops)
             (guix utils))

(define project-root
  (current-source-directory))

(define sops.yaml
  (local-file (string-append project-root "/.sops.yaml")
              ;; This is because paths on the store
              ;; can not start with dots.
              "sops.yaml"))

(define common.yaml
  (local-file (string-append project-root "/common.yaml")))

(operating-system
  [...]
  (services
    (list
       [...]
       (service sops-secrets-service-type
                (sops-service-configuration
                  (gnupg-homedir "/mnt/.gnupg")
                  (generate-key? #t)
                  (config sops.yaml)
                  (secrets
                    (list
                      (sops-secret
                        (key '("wireguard" "private"))
                        (file common.yaml)
                        (user "user1")
                        (group "users")
                        (permissions #o400)))))))))
```

Upon reconfiguration, this will yield the following content at `/run/secrets`:

``` bash
user1@host1:~ $ sudo ls -la /run/secrets/
total 12
drwxr-xr-x 1 root root    50 Jan  2 12:44 .
drwxr-xr-x 1 root root   254 Jan  2 12:44 ..
lrwxrwxrwx 1 root root    53 Jan  2 12:44 .sops.yaml -> /gnu/store/lyhyh91jw2n2asa1w0fc0zmv93yxkxip-sops.yaml
-r-------- 1 user1 users  44 Jan  2 12:44 wireguard
user1@host1:~ $ cat /run/secrets/wireguard/private
MYPRIVATEKEY
```

### Adding secrets to your `home-environment` record

`sops-guix` also provides a Guix Home service that is able to provide most feature of the system service. Most significant limitations are:

- AFAIK `home-environment`s can't configure ramfs mount points hence the `secrets-directory` option is not available. Secrets are stored in `/run/user/$UID/secrets` which usually is mounted on tmpfs.
- `sops-secret-user` and `sops-secrets-group` are ignored. All secrets belong to the user running the Guix comman line but you can still set permissions.
- There's no option to automatically generate GPG keys since probably users can easily generate one.

Now, supposing you have your `home-environment` file in the same directory where you have your `.sops.yaml` and your secrets files, you can simply add the following to your configuration:

``` scheme
(use-modules (sops secrets)
             (sops home services sops)
             (guix utils))

(define project-root
  (current-source-directory))

(define sops.yaml
  (local-file (string-append project-root "/.sops.yaml")
              ;; This is because paths on the store
              ;; can not start with dots.
              "sops.yaml"))

(define user1.yaml
  (local-file (string-append project-root "/user1.yaml")))

(home-environment
  [...]
  (services
    (list
       [...]
       (service home-sops-secrets-service-type
                (home-sops-service-configuration
                  (gnupg-homedir (string-append (getenv "HOME") "/.gnupg"))
                  (config sops.yaml)
                  (secrets
                    (list
                      (sops-secret
                        (key '("wireguard" "private"))
                        (file user1.yaml)
                        (permissions #o400)))))))))
```

Upon reconfiguration, this will yield the following content at `/run/secrets/$YOUR_UID/secrets`:

``` bash
user1@host1:~ $ ls -la /run/user/$(id -u)/secrets
total 12
drwxr-xr-x 1 user1 users    50 Jan  2 12:44 .
drwxr-xr-x 1 user1 users   254 Jan  2 12:44 ..
lrwxrwxrwx 1 user1 users    53 Jan  2 12:44 .sops.yaml -> /gnu/store/lyhyh91jw2n2asa1w0fc0zmv93yxkxip-sops.yaml
-r-------- 1 user1 users    44 Jan  2 12:44 wireguard
user1@host1:~ $ cat /run/user/$(id -u)/secrets/wireguard/private
MYPRIVATEKEY
```

## Configure

To configure Guix for using this channel you need to create a `.config/guix/channels.scm` file with the following content:

``` scheme
(cons* (channel
        (name 'sops-guix)
        (url "https://github.com/fishinthecalculator/sops-guix")
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
        (url "https://github.com/fishinthecalculator/sops-guix")
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

A [channel](https://guix.gnu.org/en/manual/devel/en/guix.html#Channels) is roughly the Guix equivalent of the AUR or container registries. It's a software repository providing Guix package and service definitions.

You can search for package and service definitions from this channel and many others at [toys.whereis.social](https://toys.whereis.social).

## Contributing

All contributions are welcome. If you have commit access please remember to setup the authentication hook with

```bash
git fetch origin keyring:keyring
guix git authenticate --cache-key=channels/sops-guix 0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9 '8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2'
```

## License

Unless otherwise stated all the files in this repository are to be considered under the GPL 3.0 terms. You are more than welcome to open issues or send patches.
