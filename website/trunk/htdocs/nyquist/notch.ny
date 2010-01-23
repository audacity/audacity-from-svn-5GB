;nyquist plug-in
;version 1
;type process
;name "Notch Filter..."
;action "Applying Notch Filter..."
;info "by David R. Sky\nReleased under terms of GNU Public License\nlower q gives wider notch"

;control choice "Default choice" int "0=60 1=50 2=alternative" 0 0 2
;control freq "Notch frequency" real "Hz" 60.0 20.0 20000.0
;control q "Notch q" real "Q" 1.00 0.01 5.00

; Notch filter by David R. Sky
; updated January 2, 2006
; Released under terms of the GNU Public License
; http://www.opensource.org/licenses/gpl-license.php

(setf freq (cond
; remove 60 Hz hum, North America
((= choice 0) 60)
; remove 50Hz hum, UK and elsewhere
((= choice 1) 50)
; set other desired notch frequency
((= choice 2) freq)))

; if audio is stereo...
(if (arrayp s)
; apply notch to stereo audio
(vector (notch2 (aref s 0) freq q)
(notch2 (aref s 1) freq q))
; ... otherwise apply to mono
(notch2 s freq q))

