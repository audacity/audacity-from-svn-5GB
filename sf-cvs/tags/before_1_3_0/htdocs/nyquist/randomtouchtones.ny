;nyquist plug-in
;version 1
;type generate
;name "Random Touch Tones (TM)..."
;action "Generating random Touch tones (TM)..."
;info "Random Touch tones (TM) by David R. Sky\nwith help from Dominic Mazzoni, Roger Dannenberg, Steven Jones"

;control tones "Number of Touch Tones" int "number" 10 1 1000  
;control td "Tone duration" real "seconds" 0.050 0.001 1.000  
;control twist "High to low tone ratio" real "db" 4.0 0.0 4.0 
;control sd "Post silence  duration" real "ms" 0.050 0.000 1.000 

;; Random Touch Tones (TM) by David R. Sky, September 15, 2004
;; Thanks for help from Dominic Mazzoni, Roger Dannenberg & Steven Jones

; make tones an integer and greater than 0
(setf tones (max (truncate tones) 1))

(defun randomtt (td twist sd) 

(setf number (random 12))

(setf low 0)

;; row 1
 (setf low (if (or (or (= number 1) (= number 2)) (= number 3)) 697 low))

;; row 2
 (setf low (if (or (or (= number 4) (= number 5)) (= number 6)) 770 low))

;; row 3
 (setf low (if (or (or (= number 7) (= number 8)) (= number 9)) 852 low))

;; row 4
 (setf low (if (or (or (= number 10) (= number 0)) (= number 11)) 941 low))

(setf high 0)

;; column 1
 (setf high (if (or (or (or (= number 1) (= number 4)) (= number 7)) (= number 10)) 1209 high))

;; column 2
 (setf high (if (or (or (or (= number 2) (= number 5)) (= number 8)) (= number 0)) 1336 high))

;; column 3
 (setf high (if (or (or (or (= number 3) (= number 6)) (= number 9)) (= number 11)) 1477 high))

(seq
  (mult 0.3
        (pwl 0.002 1 (- td 0.002) 1 td)
        (sim
          (osc (hz-to-step high) td)
          (loud (- 0 twist) (osc (hz-to-step low) td))))
  (s-rest sd)) )

(seqrep (i tones) (randomtt td twist sd))


