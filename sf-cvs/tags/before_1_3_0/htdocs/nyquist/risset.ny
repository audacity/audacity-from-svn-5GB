;nyquist plug-in
;version 1
;type generate
;name "Risset Drum..."
;action "Generating Risset Drum..."
;info "Risset Drum generator by Steven Jones pluto@swbell.net\n- After Jean Claude Risset."

;control frq "Frequency" real "Hz" 100 50 2000
;control decay "Decay" real "seconds" 2 0.125 10
;control cf "Noise center frequency" real "Hz" 500 100 5000
;control bw "Noise band width" real "Hz" 400 10 1000
;control noise "Noise mix" real "percent" 50 0 100

(if (not (boundp '*rdrum-wtabsize*))
    (progn
      (setq *rdrum-wtabsize*  2048)
      (setq *rdrum-wtab* 
     (list (sum
          (scale 1.00 (build-harmonic 10 *rdrum-wtabsize*))
          (scale 1.50 (build-harmonic 16 *rdrum-wtabsize*))
          (scale 2.00 (build-harmonic 22 *rdrum-wtabsize*))
          (scale 1.50 (build-harmonic 23 *rdrum-wtabsize*)))
             (hz-to-step 1) t))))


(defun log2 (n)
  (/ (log (float n))(log 2.0)))


(defun percussion (decay)
  (let* ((half-life (expt 2.0 (- (log2 decay) 3))))
    (exp-dec 0 half-life decay)))


(defun pink (dur cutoff)
  (lowpass6 (noise dur) cutoff))
  

(defun risset-drum (frq decay cf bw noise)
  (let* ((decay2 (* decay 0.50))
      (pitch1 (hz-to-step frq))
      (pitch2 (hz-to-step (* frq 0.10)))
      (noise-mix  (float (min (max (/ noise 100) 0) 1)))
      (tone-mix   (- 1 noise-mix)))
    (sum (mult 
       (sum (scale noise-mix 
               (mult (sine (hz-to-step cf) decay2)
                     (pink decay2 bw)))
            (scale (* tone-mix 0.17)
                      (osc pitch2 decay2 *rdrum-wtab*)))
       (percussion decay2))
      (mult (scale tone-mix (sine pitch1 decay))
               (percussion decay)))))
      

;; Generate signal and normalize.
;; ISSUE: Is there any way to normalize signal without 
;; generating it twice?
;;
(setf peakval (peak (risset-drum frq decay cf bw (/ noise 100))
ny:all))
(scale (/ 1.0 peakval)(risset-drum frq decay cf bw (/ noise 100)))
