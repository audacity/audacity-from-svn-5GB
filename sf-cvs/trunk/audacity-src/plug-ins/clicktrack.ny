;nyquist plug-in
;version 1
;type generate
;name "Click Track..."
;action "Generating click track..."
;info "Generates a click track at the given tempo and time signature\n\nThe sound to use for the click can be chosen below,\n which may or may not resemble the sound of a metronome.\n\nWritten by Dominic Mazzoni, modified by David R. Sky"

;control tempo "Tempo [beats per minute]" int "" 120 30 300
;control sig "Beats per measure (bar)" int "" 4 1 20
;control measures "Number of measures (bars)" int "" 32 10 1000
;control click-type "Click track sound type [0=sinewave 1=noise-based 2=drip sound]" int "" 0 0 2
;control q "Resonance  for noise clicks [q] (higher gives more defined pitch)" int "" 1 1 20
;control high "Strong click MIDI pitch" int "" 92 48 96
;control low "Weak click MIDI pitch" int "" 80 48 96

; original clicktrack.ny, modified by David R. Sky September 12, 2007
; to allow user to select accented and unaccented click MIDI pitches
; and select between sinewave-based or noise-based click sounds.
; The higher the q value for filtered noise-based  clicks, 
; the more the click becomes discernable as having a definite pitch
; [as specified by the user].
; Original code kept 'as is', with additional code 
; for setting of accented and unaccented pitches,
; addition of noise-based clicks, 
; normalization function for noise-based clicks,
; and pwl to make for smoother fade-in and fade-out of clicks.

(setf measures (truncate measures))
(setf tempo (truncate tempo))
(setf sig (truncate sig))
(setf ticklen 0.01) ; duration of 1 click
(setf beatlen (/ 60.0 tempo))

; function to generate drip sound clicks
; code by Paul Beach www.proviewlandscape.com/liss/
(defun drip (p) ; p is pitch in hz
(lp 
(stretch 1
(mult (exp-dec 0 0.015 0.25) 
( sim
(mult (hzosc (*  2.40483  p))  0.5 )
(mult (hzosc (*  5.52008  p))  0.25 )
(mult (hzosc (* 8.653  p))  0.125 )
(mult (hzosc (* 11.8  p))  0.0625 )
)
)
) 
440))



; function to normalize noise-based clicks
; this function is necessary because [resonant] filtering 
; changes amplitude of filtered noise clicks
(defun normalize (sound)
(setf peak-level (peak sound ny:all))
(scale (/ 1.0 peak-level) sound))

; make one measure
(setf measure (stretch-abs ticklen (mult 0.75 
; pwl is used to add fast [5ms] fade-in and fade-out of clicks
(pwl 0 0 0.005 1 0.995 1 1 0 1)
(cond
((= click-type 0) ; tone-based accented clicks
(osc high))
((= click-type 1) ; noise-based accented clicks
(normalize (lowpass2 (noise 1) (step-to-hz high) q)))
((= click-type 2) ; drip sound accented clicks
(normalize (drip (step-to-hz high))))
))))   ;accented
(dotimes (x (- sig 1))
  (setf measure (sim measure
                     (at (* beatlen (+ x 1))                 ;unaccented
                         (stretch-abs ticklen (mult 0.5 
; again, pwl adds fast [5ms] fade-in and fade-out to clicks
(pwl 0 0 0.005 1 0.995 1 1 0 1)
(cond
((= click-type 0) ; tone-based unaccented clicks
(osc low))
((= click-type 1) ; noise-based unaccented clicks
(normalize (lowpass2 (noise 1) (step-to-hz low) q)))
((= click-type 2) ; drip sound unaccented clicks
(normalize (drip (step-to-hz low))))
)))))))
; make the measure exactly the right length
(setf measure (sim measure
                   (stretch-abs (* sig beatlen) (const 0.0))))

; loop measure n [measures-1] times
(setf result measure)
(dotimes (x (- measures 1))
  (setf result (seq result measure)))

; return [click track] result
result

; arch-tag: 73fbc0e9-548b-4143-b8ac-13437b9154a7
