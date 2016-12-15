
OSC52 is an XTerm escape sequence to control the Operating System 
selection (i.e. clipboard).  It is officially documented in 
   
  http://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-Operating-System-Commands

For OSC52 aware terminals, Tmux already provide the ability to set the OS clipboard 
when using its copy mode (see option set-clipboard).

However, OSC52 aware applications cannot be used from within Tmux. It is possible to 
protect the OSC52 sequence within a DSC sequence but this is not satisfactory for several 
reasons:
  - Not all terminal support OSC52.
  - Screen and Tmux are accepting different DSC sequences ('\eP...\e\\' vs '\ePtmux...\e\\') 
    and even application that  are aware of Screen cannot differentiate it from Tmux.
  - Protecting OSC52 with DSC does not give access to the TMux buffers. 
  - If a OSC52 'get' request is protected by DSC and if the session is open 
    multiple times then the application will receive multiple answers thus 
    creating garbage. 
  

This implementation provides OSC-52 support between tmux and the applications.

This solves most of the problems listed previously:
  - Application do not need to protect OSC-52 request with DSC
  - In case of a 'get' request, tmux will reply only once  
  - OSC52 is managed by tmux and do not rely on a specific terminal.
  - OSC52 aware applications (e.g. emacs, vim, ...) can exchange data.
  - OSC52 escape sequences work even through a remote connection. This 
    is not the case for methods that rely on comminicating with the 
    (local) tmux server.
    
An OSC52 escape sequence is 
 
   \033 ] 52 ; p1 ; p2 \007   
or 
   \033 ] 52 ; p1 ; p2 \033 \\    

 The first argument p1 is a list of target buffers in 
 which the following characters are recognized
 
     s  to let the terminal choose a buffer (see below)
     p  for PRIMARY
     c  for CLIPBOARD
     0  for CUT_BUFFER0  (see osc-selection-cut-buffers below) 
     1  for CUT_BUFFER1
     2  for CUT_BUFFER1
     3  for CUT_BUFFER3
     4  for CUT_BUFFER4
     5  for CUT_BUFFER5
     6  for CUT_BUFFER6
     7  for CUT_BUFFER7
 
In regular XTerm, the target 's' is associated to either PRIMARY or CLIPBOARD 
depending of the terminal configuration. 

In Tmux, the target 's' mimics the default selection scheme ; that is set the
selection in a new automatic buffer and get the selection from the last
modified buffer.
 
If p1 can be empty, in which case the default value "s0" is used instead.

The second arguments p2 controls the operation.
 
If p2 is "?" then the operation is a 'get' and terminal shall reply with 
an OSC52 'set' sequence using the same termination sequence ( so "\007" 
or "\033\\") and the same p1 argument.

If p2 is not "?" then p2 is assumed to be base64 encoded. The operation 
is a 'set' and all the targets buffers specified by p1 are filled with 
the decoded text. 

The following options are used to control the OSC52 behaviour:
 
 - osc-selection-mode  [ manual | targets | none ] 
 
    This is a window option. The default is  'manual'
 
    In 'manual' mode, the targets specified in the OSC52 
    request are fully interpreted.
 
    In 'targets' mode, the targets specified in the OSC52 
    request are ignored. The value of the "osc-selection-targets" option
    is used instead. 

    The 'none' mode is equivalent to the 'targets' mode with an 
    empty "osc-selection-targets" string. Beware that the  
    selection may still be copied to the terminal clipboard (see 
    osc-set-clipboard).
  
 - osc-selection-targets  'string'
  
    When osc-selection-mode is 'targets', the value of that string 
    overrides the list of targets specified in all OSC52 requests.  
    Typical values are "s" to mimic the default tmux selection behaviour
    or "c" to always use a separate CLIPBOARD buffer.
 
 - osc-selection-get [ on | off ]
   
    This is a window option. The default is 'off'
 
    Control the ability to get the selection using OSC52 escape 
    sequences. Event if 'off' alls 'get' requests will be honored 
    by an empty reply to prevent dead-locking the application.
   
 -  osc-selection-set [ on | off ]
 
    This is a window option. The default is 'off'
 
    Control the ability to set the selection using OSC52 escape 
    sequences. 

 -  osc-set-clipboard [ on | off ]
 
    This is a window option. The default is 'off'

    When set, any OSC52 set request will attempt to set the clipboard 
    of the controlling terminal as described in option 'set-clipboard'.
    That feature requires both the 'set-clipboard' and the 
    'osc-selection-set' options to be set.
 
  - osc-selection-cut-buffers [ on | off ] 

    This is a window option. The default is 'off'
 
    Enable the use of CUT_BUFFER0 .. CUT_BUFFER7 in OSC-52 requests. 
    Cut buffers are of little practical use so ignoring them 
    can save memory for applications explictly using the official 
    default OSC-52 target list "s0".
     
  - osc-primary-is-select [ on | off ]

    This changes the interpretation of the 'p' target.
    Instead of using a buffer named PRIMARY, the 'p' target is now
    interpreted as 's' (the current selection is in the most recent 
    buffer).

====== OSC-52 aware terminals =====

An OSC-52 aware terminal is not stricly required to use most of the features 
provided by that Tmux branch.

However, the ability to send the selection to the OS (see the osc-set-clipboard option)
can be quite convenient.

Here is a list of terminals applications that support OSC-52: 

  - XTerm  

    Officiall supported since version >= 238 but it is probably best to use 
    a more recent version.  I recently reported a bug in #327 and hopefully 
    it will be fixed in the next release. 

    The feature must be enabled for instance with 
       xterm -xrm '*.allowWindowOps: true' 
     
  - urxvt or rxvt-unicode 

    See https://github.com/parantapa/dotfiles/blob/master/urxvt-perl/52-osc

    Untested!

  - ... 

As far as I known, libvte does not support OSC-52 so most Gnome or GTK
based terminal applications do not either.

Konsole, the KDE terminal, does not support OSC-52 but there is an opened bug report
https://bugs.kde.org/show_bug.cgi?id=372116



====== EMACS 25.1 =====

Emacs is theoretically OSC52 aware since version 25 but it requires a bit of tuning: 

The provided file emacs25-tmux-osc52.el contains my current configuration for using OSC52 in emacs 25.1

It probably won't work with any emacs version < 25.


====== Older EMACS using osc52e package  =====

For older emacs versions, the package osc52e provides the ability 
to set the selection (but not get the selection) 

   https://gist.github.com/AlexCharlton/cc82001c407786f7c1f7

As for emacs 25.1, osc52e looks at the TERM name to figure out 
if screen (or tmux) is running and then encapsulate the OSC-52 
escape sequences in DSC. This is not desirable and can be 
avoided by adding the following lisp commands to your .emacs file:

    (when (require 'osc52e nil 'noerror)
      (osc52-set-cut-function)
      ;; Force using raw OSC-52 sequence without DSC  
      (setq osc52-cut-function 'osc52-select-text)
    )



====== VIM  =====

In theory, VIM is also OSC-52 aware. 

I am not a VIM user. Need feedback please!  

========= Provided tools =============

The provided shell script osc-set-selection.sh can be used to 
set the selection. The first argument is the selection value.
The second argument is the target list (see above).

For instance, assuming that osc-selection-mode is manual, and 
osc-selection-set is on, then the following command should fill
the CLIPBOARD and PRIMARY tmux buffers with a random number.

  ./osc-set-selection.sh "$RANDOM" "cp" 

The following command will do the same in a new automatic buffer.

  ./osc-set-selection.sh "$RANDOM" "s" 

    

