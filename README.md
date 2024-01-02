# SOPS Guix

 This project aims at implementing secure deploying of secrets with Guix and SOPS.

## Secure secret management with Guix

This channels exposes the `sops-secrets-service-type` Guix service and the `sops-secret` record from the `(sops service sops)` module to safely handle secrets with Guix.

### Creating secrets with SOPS

First of all you need to create a suitable `.sops.yaml` file that you will place in your project's root directory. In this file you define which keys will be able to access your secrets files, it may very well be something like:

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

We now have defined two secrets file names patterns and we declared permissions for each key, it should be possible now to run

``` bash
sops common.yaml
```

to define our secrets value, for example:

``` yaml
wireguard:
    private: MYPRIVATEKEY
```

after saving and closing the file you can see that `sops` encrypted it before saving it, so you are free to check it in your VCS.

### Making sure the right host keys are in the root GnuPG keyring

For hosts to be able to decrypt secrets you need to provide in the `root` user keyring (or anyway the keyring located at the configured `gnupg-homedir`) the keys you defined in your `.sops.yaml`. So based on the above example you'd need to provide `8C3E4F6EB38828939029AE7BE9B6AF0CD39DD935`'s private key on `host1` and `ZZ3E4VREB38800039029AE7BE9B6AF0CD39AALH9`'s private key on `host2` .

To check that your key is correctly imported into the keyring run:

``` bash
paul@host1:~ $ sudo gpg --list-keys 
/root/.gnupg/pubring.kbx
------------------------
pub   rsa3072 1970-01-01 [SCE]
      8C3E4F6EB38828939029AE7BE9B6AF0CD39DD935
uid           [ unknown] root (Imported from SSH) <root@localhost>
```

By setting `generate-key?` to `#t` in `sops-service-configuration` a GPG key will be automatically derived for you from your system's `/etc/ssh/ssh_host_rsa_key` and added to the configured keyring. It is *discouraged* to do so and you are more than encouraged to autonomally provide a key in your configured keyring.

### Adding secrets to your `operationg-system` record

Now, supposing you have your `operating-system` file in the same directory where you have your `.sops.yaml` and `common.yaml` files, you can simply add the following to your configuration:

``` scheme
(use-modules (sops services sops)
             (guix utils))

(define project-root
  (current-source-directory))

(define sops.yaml
  (local-file (string-append root "/.sops.yaml")
              "sops.yaml"))

(define common.yaml
  (local-file (string-append root "/common.yaml")))

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
                        (key "[\"wireguard\"][\"private\"]")
                        (file common.yaml)
                        (user "paul")
                        (group "users")
                        (permissions #o400)
                        (path "/run/secrets/wireguard")))))))))
```

Upon reconfiguration, this will yield the following content at `/run/secrets`:

``` bash
sudo ls -la /run/secrets/
total 12
drwxr-xr-x 1 root root  50 Jan  2 12:44 .
drwxr-xr-x 1 root root 254 Jan  2 12:44 ..
lrwxrwxrwx 1 root root  53 Jan  2 12:44 .sops.yaml -> /gnu/store/lyhyh91jw2n2asa1w0fc0zmv93yxkxip-sops.yaml
-r-------- 1 paul users  44 Jan  2 12:44 wireguard
```

## Configure

To configure Guix for using this channel you need to create a `.config/guix/channels.scm` file with the following content:

``` scheme
(cons* (channel
        (name 'sops-guix)
        (url "https://git.sr.ht/~fishinthecalculator/sops-guix")
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
        (url "https://git.sr.ht/~fishinthecalculator/sops-guix")
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

A [channel](https://guix.gnu.org/en/manual/devel/en/guix.html#Channels) is roughly the Guix equivalent of Ubuntu's PPAs or container registries. It's a software repository providing Guix package and service definitions.

This channels hosts some packages, services and opinionated Guix `operating-system` definitions, which are supposed to ease the way into Guix for novice developers. You can search for package and service definitions from this channel any many others at [toys.whereis.みんな](https://toys.whereis.みんな).

## Contributing

All contributions are welcome. You can open issues at [todo.sr.ht/~fishinthecalculator/sops-guix](https://todo.sr.ht/~fishinthecalculator/sops-guix) and send contributions at [lists.sr.ht/~fishinthecalculator/sops-guix-devel](https://lists.sr.ht/~fishinthecalculator/sops-guix-devel). If you have commit access please remember to setup the authentication hook with

```bash
cp etc/git/pre-push .git/hooks/pre-push
```

## License

Unless otherwise stated all the files in this repository are to be considered under the GPL 3.0 terms. You are more than welcome to open issues or send patches.
