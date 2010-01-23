;nyquist plug-in
;version 2
;type analyze
;name "Selection Duration (V2)"
;action "Getting duration..."
;info "Selection Duration by David R. Sky, Steven Jones, Dominic Mazzoni"

(setf dur (/ len *sound-srate*))

(setf minutes (/ dur 60))
(setf minutes (truncate minutes))
(setf mins2secs (* minutes 60))

(setf seconds (- dur mins2secs))

(cond ((= minutes 0)
(format nil "Selection duration is ~a seconds ~%" seconds))

((= minutes 1)
(format nil "Selection duration is ~a minute ~a seconds ~%" minutes
seconds))

((> minutes 1)
(format nil "Selection duration is ~a minutes ~a seconds ~%"
minutes seconds)))

