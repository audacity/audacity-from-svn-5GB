;nyquist plug-in
;version 1
;type generate
;name "Touch Tones (TM) ..."
;action "Generating Touch tone (TM) ..."
;info "Touch tones (TM) by David Sky, Dominic Mazzoni, Roger Dannenberg\n*=10, #=11, silence=12"

;control number "Keypad number" int "keypad number" 1 0 12 
;control tl "Tone length" real "seconds" 0.100 0.001 1.000  
;control twist "High to low tone ratio" real "db" 0.0 0.0 4.0 
;control sl "Post silence  length" real "seconds" 0.100 0.000 1.000

; Twist is the ratio of the high to low tone, in db
; used in real telephone applications.

(setf low 0)

;; row 1
 (setf low (if (or (or (= number 1) (= number 2)) (= number 3)) 697
low))

;; row 2
 (setf low (if (or (or (= number 4) (= number 5)) (= number 6)) 770
low))

;; row 3
 (setf low (if (or (or (= number 7) (= number 8)) (= number 9)) 852
low))

;; row 4
 (setf low (if (or (or (= number 10) (= number 0)) (= number 11))
941 low))

(setf high 0)

;; column 1
 (setf high (if (or (or (or (= number 1) (= number 4)) (= number
7)) (= number 10)) 1209 high))

;; column 2
 (setf high (if (or (or (or (= number 2) (= number 5)) (= number
8)) (= number 0)) 1336 high))

;; column 3
 (setf high (if (or (or (or (= number 3) (= number 6)) (= number
9)) (= number 11)) 1477 high))

;; silence low
 (setf low (if (= number 12) 10 low))

;; silence high
 (setf high (if (= number 12) 10 high))

(setf volume (if (= number 12) 0.0 0.3))

(seq
  (mult volume 
        (pwl 0.002 1 (- tl 0.002) 1 tl)
        (sim
          (osc (hz-to-step high) tl)
          (loud (- 0 twist) (osc (hz-to-step low) tl))))
  (s-rest sl))
