
OSC-52 is an XTerm escape sequence that provides access the OS selection buffers (CLIPBOARD, PRIMARY, ...).

The official Tmux release can already set the OS selection for terminals that support OSC-52 escape sequences (e.g, XTerm, Rxvt, ...) 

This branch adds OSC-52 support between Tmux and the client applications (the programs running in windows and panes).

When using that branch, the OSC-52 escape sequences emited by the application are interpreted by Tmux to provide access to its internal selection buffers.

The goal is to provide seamless copy-paste between OSC-52 aware applications (Vim, Emacs, ...)  and Tmux panes. 

Those new features will work even for people using a terminal application that is not OSC-52 aware.

The directory osc52-utils/ contains several useful files and scripts, including a configuration file for emacs-25.1 

See the file osc52-utils/README.txt for more details.

THIS IS A WORK IN PROGRESS. IT WORKS BUT EXPECT SOME CHANGES IN THE NEAR FUTURES.





