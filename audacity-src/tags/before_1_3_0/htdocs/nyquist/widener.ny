;nyquist plug-in
;version 1
;type process
;name "Stereo Widener..."
;action "Widening your stereo audio..."
;info "Stereo Widener by David R. Sky"

;control vol "Inverted signal volume" real "db" -20 -36 -6

;; Stereo Widener by David R. Sky, October 18, 2004
;; Thanks to David Walsh of the Audacity-users list for explanation
;; of one process to widen stereo, implemented here.
;; Each channel is inverted, attenuated by (vol), then added to 
;; the opposite channel.
;; 
;; The greater volume is, the wider the stereo effect.

(vector 
(sum (aref s 0)
(mult (aref s 1) -1 (db-to-linear vol)))

(sum (aref s 1)
(mult (aref s 0) -1 (db-to-linear vol))))
