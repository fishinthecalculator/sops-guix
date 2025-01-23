##
# SOPS Guix
#
# @file
# @version 0.1

check:
	set -e; guix repl -L ${PWD}/modules -- tests/test-sops.scm

clean:
	rm -rfv ${PWD}/*.log

# end
