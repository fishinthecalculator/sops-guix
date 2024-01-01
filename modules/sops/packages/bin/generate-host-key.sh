#!/usr/bin/env -S bash -l

set -eu

ssh-to-pgp-private-key () {
    ssh-to-pgp -comment 'Imported from SSH' \
               -email root@localhost \
               -format armor \
               -name root \
               -i /etc/ssh/ssh_host_rsa_key -private-key 2>/dev/null
}

key_id="$(ssh-to-pgp-private-key | \
                gpg --import-options show-only --import | \
                grep -E -A 1 'sec(#)?' | tail -1 2>&1)"

export key_id

if gpg --list-keys | grep -i -q "$key_id"; then
    echo "${key_id} already known, skipping import..."
else
    ssh-to-pgp-private-key | gpg --import
fi
