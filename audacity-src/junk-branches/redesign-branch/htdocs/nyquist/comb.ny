;nyquist plug-in
;version 1
;type process
;name "Comb filter..."
;action "Applying Comb filter..."
;info "Comb filter by David R. Sky"

;; Comb filter by David R. Sky August 2004
;; the higher decay is, the more the comb filter 
;; resonates.

;control f "Comb frequency" real "Hz" 440.0 20.0 5000.0
;control decay "Comb decay" real "decay" 0.025 0.000 0.100

(comb s decay f)

