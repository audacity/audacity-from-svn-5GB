;nyquist plug-in
;version 2
;type process
;name "Turntable Warping (V2)..."
;action "Warping audio..."
;info "Turntable Warping by David R. Sky\nbased on warp tutorial by Roger B. Dannenberg\nSteps are number of semitones above or below non-warped audio\nreleased under terms of the GNU Public License"

;control step0 "Initial step" int "semitones" 0 -36 36
;control amp0 "Initial amplitude" int "percent" 100 0 100
;control midtime "Change time" real "percent" 50.0 0.0 100.0
;control step1 "End step" int "semitones" -12 -36 36
;control amp1 "End amplitude" int "percent" 40 0 100

; Turntable Warping by David R. Sky, December 13, 2004
; step values are semitones above or below pitch of unwarped audio
; Based on warp tutorial by Roger B. Dannenberg
; Released under terms of the GNU Public License
; http://www.opensource.org/licenses/gpl-license.php

; determine duration of selection in seconds
(setf dur (/ len *sound-srate*))

; set percentages for amp values
(setf amp0 (* amp0 0.01))
(setf amp1 (* amp1 0.01))

; function to stretch audio to inverse of duration
(defun newstretch (dur s)
(force-srate 44100 (stretch-abs (/ 1.0 dur) (sound s))))

; Variable Resample function by Roger B. Dannenberg
(defun variable-resample (steps snd)
; p1 helps convert steps to a ratio        
  (let ((p1 (/ (log 2.0) 12)) ratio map)
; pitch ratio
    (setf ratio (s-exp (mult steps p1))) 
; map from real-time to sound time     
    (setf map (integrate ratio)) 
(snd-compose snd (force-srate *sound-srate* map))))

; some of the math below may be incorrect
; 
; calculate duration after warp
; this math is an assumption of
; how to calculate duration after warp
; otherwise there's an obvious click at end of audio
; when negative step values are used
(defun warptime (step midtime)
(* midtime (expt 2.0 (/ step -24.0))))

; convert midtime percentage to real time
(setf midtime0 (* midtime 0.01 dur))
(setf midtime1 (* (- 1.0 (* midtime 0.01)) dur))

; approximate finish time for warp pwl
; which is then multiplied by 10
; to alleviate ending click
(setf finish (* 10.0 (+ (warptime step0 midtime0) 
(warptime step1 midtime1))))

; doing the warp
(newstretch dur 
(variable-resample 
(pwl 0.0 step0 midtime0 0 dur step1 finish step1)
(mult (pwl 0.0 amp0 midtime 1.0 dur amp1 dur) 
(sound s))))

