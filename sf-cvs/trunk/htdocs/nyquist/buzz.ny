;nyquist plug-in
;version 1
;type generate
;name "Buzz generator..."
;action "Generating Buzz tone..."
;info "Buzz buzz buzz"

;control n "Number of harmonics" int "harmonics" 12 1 30 
;control note "MIDI note #" int "MIDI note" 60 0 127 
;control dur "Duration" real "seconds" 1.00 0.01 30.00

;; the following .lsp code is taken from nyquist.lsp
;; from a downloaded Nyquist program.

(defun buzz (n pitch modulation)
  (let ((modulation-srate (snd-srate modulation))
        (hz (step-to-hz (+ pitch (get-transpose)))))
    (cond ((< *SOUND-SRATE* modulation-srate)
           (format t "Warning: down-sampling modulation in buzz~%")
           (setf modulation (snd-down *SOUND-SRATE* modulation))))
    (cond ((> hz (/ *SOUND-SRATE* 2))
           (format t "Warning: buzz nominal frequency (~A hz) will alias at current sample rate (~A hz).\n"
                   hz *SOUND-SRATE*)))
    (setf n (max n 1)) ; avoid divide by zero problem
    (scale-db (get-loud)
              (snd-buzz n       ((modulation-srate (snd-srate modulation))
        (hz (step-to-hz (+ pitch (get-transpose)))))
    (cond ((< *SOUND-SRATE* modulation-srate)
           (format t "Warning: down-sampling modulation in buzz~%")
           (setf modulation (snd-down *SOUND-SRATE* modulation))))
    (cond ((> hz (/ *SOUND-SRATE* 2))
           (format t "Warning: buzz nominal frequency (~A hz) will alias at current sample rate (~A hz).\n"
                   hz *SOUND-SRATE*)))
    (setf n (max n 1)) ; avoid divide by zero problem
    (scale-db (get-loud)
              (snd-buzz n                   ; number of harmonics
                        *SOUND-SRATE*       ; output sample rate
                        hz                  ; output hz
                        (local-to-global 0) ; starting time
                        modulation))))      ; freq. modulation
                        

(seq              
(mult 0.8 (pwl 0.002 1 (- td 0.002) 1 td)
(buzz n note (s-rest td)))

(s-rest sd))

