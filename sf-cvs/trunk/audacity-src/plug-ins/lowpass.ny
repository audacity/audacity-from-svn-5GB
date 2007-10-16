;nyquist plug-in
;version 1
;type process
;name "Low Pass Filter..."
;action "Performing Low Pass Filter..."
;info "Released under terms of the GNU General Public License Version 2\nAttenuates high frequencies above your specified cutoff frequency\n"

;control f "Cutoff frequency [Hz]" real "" 10000 1 rate
(lp s f)

; arch-tag: c2d96e46-b4e2-47c0-9a19-761011418e02

