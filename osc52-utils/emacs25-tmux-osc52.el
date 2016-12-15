

(message "in emacs25-tmux-osc52.el")

;; =====  OSC52 support for tmux + emacs 25.1.1 ===========
;;
;; Add the following to your .emacs
;;  
;; (unless window-system
;;   (when (string= emacs-version "25.1.1" )
;;    (load-file "~/git/tmux/osc52-utils/emacs25-tmux-osc52.el")))
;;
;;
;;
;; Emacs uses the TERM value to figure out how to configure the terminal.
;; This is documented in http://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/term/README
;; 
;; For tmux the situation is complicated because most users will use a TERM
;; value such as 'screen' or 'screen-256color' so tmux will actually be recognized
;; as screen and the function  terminal-init-screen (in lisp/term/screen.el) will 
;; be called
;;
;; What is done below is tell emacs to use terminal-init-tmux instead of terminal-init-screen
;; 
;; terminal-init-tmux and xterm-tmux-extra-capabilities are basically copies of
;; terminal-init-screen and xterm-screen-extra-capabilities from lisp/term/screen.el
;;
;; Once this is done, the xterm versions gui-backend-get-selection and gui-backend-get-selection
;; won't recognize anymore the terminal as screen and will stop adding DCS.  
;;
;; At that point, gui-backend-set-selection should be fully functionnal (assuming that 
;; you are using a version of tmux that recognize OSC-52 set requests) 
;; 
;; However, gui-backend-get-selection is still broken even in regular xterm because of 
;; the \a character in the reply that is incorrectly interpreted as an interrupt by emacs.
;; 
;; The gui-backend-get-selection method below is a patched version that uses 
;; \e\\ instead of \a as OSC-52 terminator.
;;
;; This is a cl-defmethod so it does not replace the gui-backend-get-selection in term/xterm.el
;;
;; Instead, the proper implementation is dynamically choosen according to the value of  
;; 'xterm--get-selection in the terminal parameters. 
;; 
;; For the original implementation that value is t 
;; For the patched implementation that value is "PATCHED"
;; 
;; Finally, I am adding a hook that will be called at the end of the xterm 
;; initialization. Be aware that this hook will be called for all xterm variants.
;;
;; The Emacs variables select-enable-clipboard and select-enable-primary control 
;; the use of CLIPBOARD and PRIMARY.  Set them to nil or t to enable or disable 
;; the use of each buffer.
;;
;; It should be noted that prior to emacs 25, those variables were named 
;; x-select-enable-clipboard and select-enable-primary. The old names can still  
;; be used but are officialy obsolete.
;; 
;; When using OSC52, CLIPBOARD requests will use the 'c' target while 
;; PRIMARY requests will use the 'p' target. This is controled by the  
;; xterm--selection-char function in term/xterm.el 
;; 
;; If both targets are enabled, emacs will perform two OSC-52 requests
;; which is not optimal.  
;;
;; OSC52 can introduce some noticeable lag so a good setup could be to disable 
;; both PRIMARY and CLIPBOARD and to explicly use the clipboard functions 
;; when needed (see below)
;;

;; Tell emacs to use our custom terminal-init-tmux initiialization 
;; for those TERM values or prefixes.

(add-to-list 'term-file-aliases '("screen"                 . "tmux")) 
(add-to-list 'term-file-aliases '("xterm-screen-256color"  . "tmux")) 


;; The terminal-init-tmux function is basically equivalent to terminal-init-screen.
;;
;; The symbol 'terminal-init-tmux will be stored in the terminal parameters
;; and will later be used to differentiate screen from tmux.
;;

(require 'term/xterm)

(defcustom xterm-tmux-extra-capabilities '(modifyOtherKeys)
  "Extra capabilities supported under \"tmux\""
  :version "25.1"
  :type xterm--extra-capabilities-type
  :group 'xterm)

(defun terminal-init-tmux ()
  "Terminal initialization function for tmx."   
  (message "In terminal-init-tmux")
  ;; Initialize xterm using the tmux capabilities   
  (let ((xterm-extra-capabilities xterm-tmux-extra-capabilities))
    (tty-run-terminal-initialization (selected-frame) "xterm"))        
  )


;; gui-backend-get-selection is the method that handles the get requests
;; to CLIPBOARD and PRIMARY. 
;;
;; There can be multiple versions of that methods. The proper one is selected
;; according to the terminal parameters   
;;
;; That specifi version will be used when xterm--get-selection is "PATCHED"
;; 
;; Remark: the original version in term/xterm.el is used when xterm--get-selection is t
;;
;; The \a used to terminate the OSC52 get-selection reply is causing
;; some issues in the current term/xterm.el implementation (emacs 25.1).
;;
;; This implementation uses the alternative end sequence \e\\  
;;
(cl-defmethod gui-backend-get-selection
              (type data-type
                    &context (window-system nil)
                    ;; Only applies to terminals which have it enabled.
                    ((string= (terminal-parameter nil 'xterm--get-selection) "PATCHED") (eql t)))
              (message "  --> in patched gui-backend-get-selection")
              (unless (eq data-type 'STRING)
                (error "Unsupported data type %S" data-type))
              (let* ((screen (eq (terminal-parameter nil 'terminal-initted)
                                 'terminal-init-screen))
                     (query (concat "\e]52;" (xterm--selection-char type) ";")))
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (xterm--query
                   (concat (when screen "\eP") query "?\e\\" (when screen "\e\\"))
                   (list (cons query (lambda ()
                                       (while (let ((char (read-char)))
                                                (unless (eq char ?\\)
                                                  (unless (eq char ?\e) 
                                                    (insert char))
                                                  t))))))
                   'no-async)
                  (base64-decode-region (point-min) (point-max))
                  (decode-coding-region (point-min) (point-max) 'utf-8-unix t))))

;; Call this instead of xterm--init-activate-get-selection
;; to enable our patched implementation of gui-backend-get-selection    
(defun xterm--init-activate-get-selection-PATCHED ()
  (set-terminal-parameter nil 'xterm--get-selection "PATCHED" )
  )

;;;
;;; Calling this function will redefined the function xterm--selection-char
;;; from term/xterm.el
;;;
;;; That redefinition shall occur in a tty hook called after term/xterm.el
;;; is automatically loaded by emacs.
;;;
;;; The default version found in term/xterm.el knows PRIMARY=p and CLIPBOARD=c.
;;;
;;; This new implementation adds a few convenient targets for OSC-52
;;; Those new targets won't be used by default but they can be accessed manually
;;; via gui-get-selection and gui-set-selection.
;;;
;;; Changing the strings associated to 'PRIMARY and 'CLIPBOARD could also
;;; make sense to change the yank/kill behavior when select-enable-primary
;;; or select-enable-clipboard are set. That would only affect selection
;;; via OSC-52. The behavior under X11 is not affected.
;;;
;;; When used undertmux
;;;
;;;
(defun redefine-xterm--selection-char () 
  "Redefined xterm--selection-char, the function used to select the OSC-52 target buffers"
  (interactive)
  (message "in redefine-xterm--selection-char")
  (defun xterm--selection-char (type)
    (pcase type
      ('PRIMARY      "p")
      ('CLIPBOARD    "c")
      ('SELECT       "s")
      ('SELECT_CLIPBOARD "sc")
      ('CUT_BUFFER0  "0")
      ('CUT_BUFFER1  "1")
      (_ (error "Invalid selection type: %S" type))))
  )  


;; A hook that will be called at the end of the terminal initialization 
;; This is the right place the enable features according to the terminal
(defun tty-osc52-hook ()
  (let ((term (terminal-parameter nil 'terminal-initted) ))
    (message (concat "in tty-osc52-hook" ))
    (cond
     ;; ===== Xterm  ======
     ((eq term 'terminal-init-xterm)        
           (message "  --> Xterm detected")
           (xterm--init-activate-set-selection)
           (xterm--init-activate-get-selection-PATCHED)
           ;; For xterm, I like my selection to go in both PRIMARY and CLIPBOARD
           (setq select-enable-primary   t)
           (setq select-enable-clipboard t)
           ;;
           (redefine-xterm--selection-char)
           )
     ;; ===== Tmux ======
     ((eq term 'terminal-init-tmux) 
           (message "  --> Tmux detected")
           (xterm--init-activate-set-selection)
           (xterm--init-activate-get-selection-PATCHED)
           ;; For TMux, do not use OSC-52 by default
           (setq select-enable-primary   nil)
           (setq select-enable-clipboard nil)
           ;; Instead, use clipboard-yank and clipboard-kill-ring-save
           ;; to copy paste to tmux via OSC52.
           (global-set-key [M-insert] 'clipboard-yank)
           (global-set-key [M-delete] 'clipboard-kill-ring-save)
           ;;
           (redefine-xterm--selection-char)
           )
     )    
    )
  )


(add-hook 'tty-setup-hook 'tty-osc52-hook)

;;;
;;;  In emacs 25.0 or soon before that version, x-select-enable-clipboard was renamed to select-enable-clipboard. 
;;;  Unfortunately, the renaming was done incorrectly in a few places (gui-select-enable-clipboard vs select-enable-clipboard).
;;;  That breaks the clipboard-yank, clipboard-kill-ring-save and clipboard-kill-region functions
;;;
;;;  due to an incorrect renaming of x-select-enable-clipboard.
;;;  https://debbugs.gnu.org/cgi/bugreport.cgi?bug=25145
;;;

(defun clipboard-yank ()
  "Insert the clipboard contents, or the last stretch of killed text."
  (interactive "*")
  (message "clipboard-yank") ;; for debug, to be removed! 
  (let ((select-enable-clipboard t))
    (yank)))

(defun clipboard-kill-ring-save (beg end)
  "Copy region to kill ring, and save in the X clipboard."
  (interactive "r")
  (message "clipboard-kill-ring-save") ;; for debug, to be removed! 
  (let ((select-enable-clipboard t))
    (kill-ring-save beg end)))

(defun clipboard-kill-region (beg end)
  "Kill the region, and save it in the X clipboard."
  (interactive "r")
  (message "clipboard-kill-region")  ;; for debug, to be removed! 
  (let ((select-enable-clipboard t))
    (kill-region beg end)))

;;;
;;; Below are new functions to manipulate the GUI selections in a more direct.
;;; Unlike yank, clipboard-yank and similar functions those functions do not
;;; try to be clever. between CLIPBOARD, PRIMARY or the kill-ring in a 'clever' way.
;;;

(defun paste-gui-clipboard ()
  "Paste the value of the CLIPBOARD buffer at point"
  (interactive)
  (insert (gui-get-selection 'CLIPBOARD 'STRING))
  )

(defun paste-gui-primary ()
  "Paste the value of the PRIMARY buffer at point"
  (interactive)
  (insert (gui-get-selection 'PRIMARY 'STRING))
  )

;; By default, SELECT is not a known gui selection target. 
;; This function requires a patched version of xterm--selection-char
;; that knows of the 'SELECT target (See above)
(defun paste-gui-select ()
  "Paste the value of the SELECT buffer at point"
  (interactive)
  (insert (gui-get-selection 'SELECT 'STRING))
  )

(defun copy-gui-clipboard (beg end)
  "Copy the current region to the gui CLIPBOARD buffer "
  (interactive "r")
  (gui-set-selection 'CLIPBOARD (buffer-substring-no-properties beg end) )
  )


(defun copy-gui-primary (beg end)
  "Copy the current region to the gui CLIPBOARD buffer "
  (interactive "r")
  (gui-set-selection 'PRIMARY (buffer-substring-no-properties beg end) )
  )


(defun copy-gui-select (beg end)
  "Copy the current region to the gui SELECT buffer "
  (interactive "r")
  (gui-set-selection 'SELECT (buffer-substring-no-properties beg end) )
  )

(defun copy-gui-select-clipboard (beg end)
  "Copy the current region to the gui SELECT and CLIPBOARD buffers"
  (interactive "r")
  (gui-set-selection 'SELECT_CLIPBOARD (buffer-substring-no-properties beg end) )
  )

