;nyquist plug-in
;version 1
;type process
;name "High Pass Filter..."
;action "Performing High Pass Filter..."
;info "Released under terms of the GNU General Public License Version 2\nAttenuates low frequencies below your specified cutoff frequency\n"

;control f "Cutoff frequency [Hz]" real "" 1000 1 rate
(hp s f)

; arch-tag: 49302eba-9945-43d7-aade-f1c7eded27af

