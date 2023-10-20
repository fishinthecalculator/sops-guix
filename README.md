# SOPS Guix

 This project aims at implementing secure deploying of secrets with Guix and SOPS.

## Configure

To configure Guix for using this channel you need to create a `.config/guix/channels.scm` file with the following content:

``` scheme
(cons* (channel
        (name 'sops-guix)
        (url "https://git.sr.ht/~fishinthecalculator/sops-guix")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "bea63b9b2d07d7a978db8d271130171cdcc410e6"
          (openpgp-fingerprint
           "D088 4467 87F7 CBB2 AE08  BE6D D075 F59A 4805 49C3"))))
       %default-channels)
```

Otherwise, if you already have a `.config/guix/channels.scm` you can simply prepend this channel to the preexisting ones:

``` scheme
(cons* (channel
        (name 'small-guix)
        (url "https://git.sr.ht/~fishinthecalculator/sops-guix")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "bea63b9b2d07d7a978db8d271130171cdcc410e6"
          (openpgp-fingerprint
           "D088 4467 87F7 CBB2 AE08  BE6D D075 F59A 4805 49C3"))))
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

All contributions are welcome. If you have commit access please remember to setup the authentication hook with

```bash
cp etc/git/pre-push .git/hooks/pre-push
```

## License

Unless otherwise stated all the files in this repository are to be considered under the GPL 3.0 terms. You are more than welcome to open issues or send patches.
