##
# SOPS Guix
#
# @file
# @version 0.1

# Introduction of the 'small-guix' channel.
channel_intro_commit = 4a1aecea90774e14eeb0647d4e7716698de689cb
channel_intro_signer = D088 4467 87F7 CBB2 AE08  BE6D D075 F59A 4805 49C3

authenticate:
	echo "Authenticating Git checkout..." ;	\
	guix git authenticate					\
	    --cache-key=channels/guix --stats			\
	    "$(channel_intro_commit)" "$(channel_intro_signer)"

# end
