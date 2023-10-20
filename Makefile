##
# SOPS Guix
#
# @file
# @version 0.1

# Introduction of the 'small-guix' channel.
channel_intro_commit = bea63b9b2d07d7a978db8d271130171cdcc410e6
channel_intro_signer = D088 4467 87F7 CBB2 AE08  BE6D D075 F59A 4805 49C3

authenticate:
	echo "Authenticating Git checkout..." ;	\
	guix git authenticate					\
	    --cache-key=channels/guix --stats			\
	    "$(channel_intro_commit)" "$(channel_intro_signer)"

# end
