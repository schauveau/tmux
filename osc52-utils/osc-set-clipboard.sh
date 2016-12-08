#!/bin/bash

# Usage: set the selection using OSC52
#   
#  First argument is the selection string.
#
#  Second argument is a string in which each characters describes a selection 
#  target:
#      s : SELECT (depend on terminal settings) 
#      p : PRIMARY 
#      c : CLIPBOARD 
#      0..7 : CUT_BUFFER0 to CUT_BUFFER7 
# 
#  Third argument, if non-empty then use an alternative OSC end marker (for testing)  
#
# 
# Filter out unknown targets 
# Actually, the character we really want to avoid is ';' and characters 
# not allowed in OSC sequences (e.g. \e, \a, ...) 
#

b64=$(echo -n "$1" | base64 -w 0)

targets=$(echo -n "$2" | sed 's/[^scp01234567]//g') 

echo "Using targets '$targets'"

if [ -z "$3" ] ; then 
    printf "\e]52;%s;%s\a" "$targets" "$b64"
else    
    echo "Using alternative end sequence"
    printf "\e]52;%s;%s\e\\" "$targets" "$b64"
fi 

