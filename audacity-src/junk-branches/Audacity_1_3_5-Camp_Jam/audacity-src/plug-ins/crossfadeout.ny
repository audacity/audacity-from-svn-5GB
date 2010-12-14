;nyquist plug-in
;version 2
;type process
;name "Cross Fade Out"
;action "Cross-Fading Out..."
(mult s (snd-exp
          (snd-scale 0.5 (snd-log
                          (sum 1 (snd-scale -1 (ramp)))))))
