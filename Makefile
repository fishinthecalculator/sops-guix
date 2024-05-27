##
# SOPS Guix
#
# @file
# @version 0.1

check:
	set -e; guile -L ${PWD}/modules -s tests/test-sops.scm

clean:
	rm -rfv ${PWD}/*.log

# end
