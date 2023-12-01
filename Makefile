##
# SOPS Guix
#
# @file
# @version 0.1

# Introduction of the 'sops-guix' channel.
channel_intro_commit = 0bbaf1fdd25266c7df790f65640aaa01e6d2dbc9
channel_intro_signer = 8D10 60B9 6BB8 292E 829B  7249 AED4 1CC1 93B7 01E2

authenticate:
	echo "Authenticating Git checkout..." ;	\
	guix git authenticate					\
	    --cache-key=channels/guix --stats			\
	    "$(channel_intro_commit)" "$(channel_intro_signer)"

# end
