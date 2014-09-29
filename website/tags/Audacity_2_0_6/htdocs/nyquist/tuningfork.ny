;nyquist plug-in
;version 1
;type generate
;name "Tuning Fork..."
;action "Generating tone..."
;info "Tuning Fork by David R. Sky\nmiddle C=MIDI note 60 A440=69\nC0=0 C1=12 C2=24 C3=36 C4=48 C5=60 C6=72 C7=84 C8=96 C9=108 C10=120"

; Note the above info line should not be word wrapped.

;control dur "Tone duration" real "seconds" 5.00 0.00 120.00
;control cf "Constant or fade out" int "0=constant 1=fade" 0 0 1
;control mf "MIDI or frequency" int "0=MIDI 1=frequency" 0 0 1
;control midin "MIDI note" real "MIDI note" 69.00 0.00 127.00
;control f "frequency" real "Hz" 440.00 20.00 20000.00

(setf midin (if (= mf 0) midin (hz-to-step f)))
(setq envelope (if (= cf 0)
(pwl 0.002 1 (- dur 0.002) 1 dur)
(pwl 0.002 1 dur)))

(mult envelope (osc midin dur))

