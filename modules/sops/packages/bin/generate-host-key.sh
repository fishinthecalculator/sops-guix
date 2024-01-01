#!/usr/bin/env -S bash -l

set -eu

ssh-to-pgp-private-key () {
    ssh-to-pgp -comment 'Imported from SSH' \
               -email root@localhost \
               -format armor \
               -name root \
               -i /etc/ssh/ssh_host_rsa_key -private-key 2>/dev/null
}

key="$(ssh-to-pgp-private-key)"
key_id="$(echo "${key}" | \
          gpg --import-options show-only --import | \
          grep -E -A 1 'sec(#)?' | tail -1)"

if gpg --list-keys | grep -i -q "$key_id"; then
    echo "${key_id} already known, skipping import..."
else
    echo "${key}" | gpg --import
fi
