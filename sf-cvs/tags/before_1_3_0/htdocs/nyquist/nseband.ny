;nyquist plug-in
;version 1
;type generate
;name "Noise Band"
;action "Generating Noise Band..."
;info "Adapted by Steven Jones pluto@swbell.net GNU 2004\nGenerates narrow band noise.
;control center "Center Frequency" int "Hz" 1000 10 10000
;control bw "Band Width" int "Hz" 50 1 10000
;control wdur "Duration" inc "sec" 1 0 30
;control fdur "Fractional Duration" int "n/100 sec" 0 0 99
 

(setq duration (+ wdur (* 0.01 fdur)))



(defun nseband (center bw duration)
  (mult (sine (hz-to-step center) duration)
	(lowpass4 (noise duration) bw)))




(setq rawsig (nseband center bw duration))
(setq peakval (peak rawsig ny:all))
(scale (* 0.9 (/ 1.0 peakval)) rawsig)
	
