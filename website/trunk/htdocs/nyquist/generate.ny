;nyquist plug-in
;version 2
;type generate
;name "Nyquist generate prompt..."
;action "Generating audio..."
;info "by David R. Sky, Steven Jones, Edgar Franke\nReleased under terms of GNU General Public License version 2"

; Idea by David R. Sky
; original code by Steven Jones
; multi-line update by Edgar Franke
; November 30, 2005
; Released under terms of the GNU General Public License version 2 
; http://www.opensource.org/licenses/gpl-license.php

;control code1 "Line 1" string ""  
;control code2 "Line 2" string ""  
;control code3 "Line 3" string ""  
;control code4 "Line 4" string ""  
;control code5 "Line 5" string ""  

; edgar to david: I have noticed that in the control lines there have to be
; at least two blanks at the end of the line otherwise Nyquist refuses to
; use the variable and there occur "unbound variable" errors. I assume this
; is a bug in the Nyquist interface.

(setq nyquist-code (format NIL "~a ~a ~a ~a ~a" code1 code2 code3 code4 code5))

; edgar to david: the number of ~a must match the number of string variables
; this way you can copy as many lines as you like one after the other and
; then send it with Stevens' code to the Nyquist interpreter

(eval (read (make-string-input-stream nyquist-code)))

