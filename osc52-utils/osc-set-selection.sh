#!/bin/sh

# Usage: osc-set-selection.sh TEXT TARGETS
# 
# Set the selection using OSC52 escape sequences.
#   
#  TEXT the selection string. 
#
#  TARGETS is a string in which each of the following character 
#  describes a selection target buffer. 
# 
#      s    : SELECT (depend on terminal settings) 
#      p    : PRIMARY 
#      c    : CLIPBOARD 
#      0..7 : CUT_BUFFER0 to CUT_BUFFER7 
# 
#  if a non-empty, third argument is given then use an alternative OSC end marker (for testing)  
# 
#  Unrecognzed characters are filtered out.  
#  Actually, the characters that we really want to avoid are ';' and all 
#  control characters that are scuscptible to end the OSC escape sequence 
#  (e.g. \033, \007, ...) 
#

text="$1"

b64=`echo -n "$text" | base64 -w 0`

targets=`echo -n "$2" | sed 's/[^scp01234567]//g'`

echo "Using targets '$targets'"

if [ -z "$3" ] ; then 
    printf '\033]52;%s;%s\007' "$targets" "$b64"
else    
    echo "Using alternative end sequence"
    printf '\033]52;%s;%s\033\\' "$targets" "$b64"
fi 


