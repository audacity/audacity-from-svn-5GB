;nyquist plug-in
;version 2
;type process
;name "Cross Fade In"
;action "Cross-Fading In..."
(mult s (snd-exp (snd-scale 0.5 (snd-log (ramp)))))
