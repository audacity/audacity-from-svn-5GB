;nyquist plug-in
;version 1
;type process
;name "Customizable EQ..."
;action "Applying gain to the band..."
;info "Customizable EQ by David Sky"

;control f "Center frequency" real "Hz" 440.0 20.0 20000.0 
;control width "Band width in octaves" real "octaves" 1.0 0.1 5.0 
;control gain "Gain" real "db" 0.0 -24.0 24.0

(eq-band s f gain width)
