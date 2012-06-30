;nyquist plug-in
;version 1
;type process
;name "Extract Audio (Percent)..."
;action "Extracting audio..."
;info "Extract Audio (Percent) by David R. Sky"

;; Extract Audio by David R. Sky October 27, 2004
;; Define start and stop times
;; based on percentages of the duration of the selection

;control start "Start position" real "percent" 0.00 0.00 100.00 
;control end "End position" real "percent" 100.00 0.00 100.00 

(setf start (* start 0.01))
(setf end (* end 0.01))

(if (arrayp s)
(vector (extract start end (aref s 0))
(extract start end (aref s 1)))
(extract start end s))

