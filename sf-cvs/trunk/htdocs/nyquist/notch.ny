;nyquist plug-in
;version 1
;type process
;name "Notch Filter..."
;action "Applying Notch Filter..."
;info "Notch Filter by David R. Sky"

;control freq "Notch frequency" real "Hz" 60 20 10000
;control q "Notch q" real "lower value=wider notch" 1.00 0.01 5.00

;; Notch filter, works on mono and stereo audio

(if (arrayp s)
(vector (notch2 (aref s 0) freq q)
(notch2 (aref s 1) freq q))

(notch2 s freq q))

