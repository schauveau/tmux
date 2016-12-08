#!/bin/bash

# Remark: This scripts requires a read command that accepts the -d option 
#         in 'read' to change the end delimiter.
#         This is a bash extension that won't work with /bin/sh   


#
# Usage: osc-get-selection.sh TARGETS
#
#
# The first and unique argument specifies which buffers to look for:
#
#      s    : SELECT (depend on terminal settings) 
#      p    : PRIMARY 
#      c    : CLIPBOARD 
#      0..7 : CUT_BUFFER0 to CUT_BUFFER7 
# 

finish () {
    # restore tty state
    stty -F "$TTY" "$stty_state"
}

TTY=`tty`
if [ ! -c "$TTY" ] ; then 
    echo "ERROR: STDIN is not a tty" >&2 
    exit 1 
fi

# Save the tty state
stty_state=`stty -F "$TTY" -g `
trap finish EXIT

# We don't want to see the terminal replies to our requests.
# Not sure for -icanon but xtermcontrol does it so...
stty -F "$TTY" -echo -icanon 

targets=`echo -n "$1" | sed 's/[^scp01234567]//g'`

printf '\e]52;%s;?\a' "$targets"

read -d $'\a' MSG < $TTY

buf64=`echo -n "$MSG" | sed "s/.*;$targets;//" `

# Fix for malformed base64 encoding in XTerm-297 
#
# Base64 encodes each block of 3 chars into 4 chars (6bits per char).
# If the last block is incomplete, then one or two characters are used 
# to indicate that the last 6 or 12 bits are non-significant 
#
# For instance, a proper base64 encoding is
#
#   'hello' --> 'aGVsbG8='
#   'ping'  --> 'cGluZw==' 
# 
# However, Xterm 297 produces 
#
#   'hello' --> 'aGVsbG8=='
#   'ping'  --> 'cGluZw====' 
#
# Simply speaking, the '=' are duplicated.
#
# This bug is easy to detect because the size of the base64 encoded
# string should be a multiple of 4.
#
case $(( ${#buf64} % 4)) in 
    1) buf64="${buf64%=}" ;;   # discard one '=' 
    2) buf64="${buf64%==}" ;;  # discard two '=' 
esac

echo -n "$buf64" | base64 -d -i 
