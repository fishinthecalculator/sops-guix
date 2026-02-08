# How to sops-guix

This guide is supposed to bring you from a system without secrets to a system with `sops-guix` installed and your secrets provisioned under `/run/secrets`.

**Table of contents:**

- [Installing `sops-guix`](#installing-sops-guix)
- [Your keypair](#your-keypair)
- [Host keypair](#host-keypair)
- [Creating secrets with SOPS](#creating-secrets-with-sops)
- [Guix System secrets](#guix-system-secrets)
- [Guix Home secrets](#guix-home-secrets)

### Installing `sops-guix`

`sops-guix` is a Guix channel, which is a fancy way of saying that `sops-guix` adds functionality to your Guix installation. To make your Guix command aware of `sops-guix`, you simply [add the channel](https://github.com/fishinthecalculator/sops-guix/tree/main?tab=readme-ov-file#configure) to your `~/.config/guix/channels.scm` and run `guix pull`.

After Guix has pulled the `sops-guix` channel, you can verify that it was correctly installed by running, **in a new shell**, `guix describe`. It should look something like:

```shell
guix describe
Generation 152  Feb 08 2026 11:16:58    (current)
  guix ae90908
    repository URL: https://git.guix.gnu.org/guix.git
    branch: master
    commit: ae90908399398f3a384b8107bbf9839e57971fce
  sops-guix 2fa6853
    repository URL: https://github.com/fishinthecalculator/sops-guix.git
    branch: main
    commit: 2fa6853677ccb6a3d41cd89b090e6d17d6839f58
```

### Your keypair

SOPS works by encrypting secrets with the key of the user and one or more key for each host that will need to decrypt them. SOPS can also query cloud providers (like [AWS KMS, GCP KMS, Azure Key Vault](https://getsops.io/docs/)) but this has never been tested with `sops-guix`.

The first task you have is to perform is to generate a cryptographic keypair. There are two supported encryption tools supported by SOPS: [age](https://age-encryption.org/) and [GnuPG](https://www.gnupg.org/). Once setup, the choice become transparet to day-to-day usage of SOPS. You are free to either choose one or the other.

There are many guides online on how to generate a new `age` key or a new `gpg` key, you should be able to generate one following any of those.

### Host keypair

For hosts to be able to decrypt secrets you need to provide in the `root` user keyring (or anyway the keyrings located at the configured `gnupg-homedir` and `age-key-file`) the keys you defined in your `.sops.yaml`. So based on the above example you'd need to provide `8C3E4F6EB38828939029AE7BE9B6AF0CD39DD935`'s private key on `host1` and `ZZ3E4VREB38800039029AE7BE9B6AF0CD39AALH9`'s private key on `host2` .

By setting `generate-key?` to `#t` in `sops-service-configuration` a `gpg` or `age` key will be automatically derived for you respectively from your system's `/etc/ssh/ssh_host_rsa_key` (or `/etc/ssh/ssh_host_ed25519_key`, in case it exists) and added to the configured keyring.

It is *discouraged* to do so and you are more than encouraged to autonomously provide a key in your configured keyring. While having `sops-guix` generate a keypair on your behalf is easier, you have less control on the key. For example some could have a requirement to rotate the key periodically. You can certainly enable it once, deploy your configuration, turn it off and handle the key from there, but having the key managed by `sops-guix` may not be the best default choice in every case.

### Creating secrets with SOPS

Once you have a suitable set of keys for yourself and your machines you are ready to create a `.sops.yaml` file, the only configuration you need for SOPS. In this file you define which keys will be able to access your secrets files, it may very well be something like:

``` yaml
keys:
    - &user_user1 age1h2z53yacysrzwlpyqz7z5dg2yt6wzsyqhuhvqh0r4khmj20d5y5q73m2rf
    - &host_host1 8C3E4F6EB38828939029AE7BE9B6AF0CD39DD935
    - &host_host2 ZZ3E4VREB38800039029AE7BE9B6AF0CD39AALH9

creation_rules:
    - path_regex: .*common\.yaml$
      key_groups:
          - pgp:
                - *host_host1
                - *host_host2
            age:
                - *user_user1
    - path_regex: .*host1\.yaml$
      key_groups:
          - pgp:
                - *host_host1
            age:
                - *user_user1
```

In this file we define three keys called `user_user1`, `host_host1` and `host_host2` . The prefixes `host_` and `user_` are just a convention to indicate that some public keys belong to users and some belong to machines.

We now have defined two secrets file names patterns and we declared permissions for each key, it should be possible now to run the following in your projects root directory:

``` bash
sops common.yaml
```

This will open your default editor with an example content to define your secrets value. You can edit it or delete it and your own content for example:

``` yaml
wireguard:
    private: MYPRIVATEKEY
```

after saving and closing the file you can see by `cat`ting the secret file that `sops` encrypted it before saving it.

### Guix System secrets

Now you can simply add the following to your configuration:

``` scheme
(use-modules (sops secrets)
             (sops services sops))

(define common.yaml
  ;; The path to your encrypted secrets.
  ;; It can be anywhere on your system,
  ;; provided that the user running Guix
  ;; can read the files.
  (local-file "/path/to/common.yaml"))

(operating-system
  [...]
  (services
    (list
       [...]
       (service sops-secrets-service-type
                (sops-service-configuration
                  (generate-key? #t)
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
-r-------- 1 user1 users  44 Jan  2 12:44 wireguard
user1@host1:~ $ cat /run/secrets/wireguard/private
MYPRIVATEKEY
```

### Guix Home secrets

`sops-guix` also provides a Guix Home service that is able to provide most feature of the system service. Most significant limitations are:

- `home-environment`s can't configure ramfs mount points hence the `secrets-directory` option is not available. Secrets are stored in `/run/user/$UID/secrets` which usually is mounted on tmpfs.
- The `user` and `group` fields of `sops-secret`s are ignored. All secrets belong to the user running the Guix command line, but you can still set permissions.
- There's no option to automatically generate `gpg`/`age` keys since probably users can easily generate them.

Now you can simply add the following to your configuration:

``` scheme
(use-modules (sops secrets)
             (sops home services sops))

(define user1.yaml
  ;; The path to your encrypted secrets.
  ;; It can be anywhere on your system,
  ;; provided that the user running Guix
  ;; can read the files.
  (local-file "/path/to/user1.yaml"))

(home-environment
  [...]
  (services
    (list
       [...]
       (service home-sops-secrets-service-type
                (home-sops-service-configuration
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
-r-------- 1 user1 users    44 Jan  2 12:44 wireguard
user1@host1:~ $ cat /run/user/$(id -u)/secrets/wireguard/private
MYPRIVATEKEY
```

