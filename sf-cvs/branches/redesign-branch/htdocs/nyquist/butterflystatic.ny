;nyquist plug-in
;version 1
;type process
;name "Stereo Butterfly (static)..."
;action "Stereo Butterfly is choreographing your audio..."
;info "Stereo Butterfly (static) by David Sky"

;control width "Stereo width" real "width" 1.00 -1.00 1.00 

;; Stereo Butterfly (static)
;; by David Sky, September 10, 2004
;; 
;; like a butterfly's wings at rest 
;; 1.00 is wings wide open (full stereo)
;; 0 is wings fully closed (audio sounds mono)
;; -1.00 butterfly has totally flipped, wings wide open
;; (left channel is fully flipped with right and vice versa)

; make sure width is between -1 and 1 inclusive
(setf width (min (max width -1) 1))

(defun butterfly (sound width) 

(vector 
(sum (mult (aref sound 0) (sum width 1) 0.5)
(mult (aref sound 1) (sum width -1) -0.5))

(sum (mult (aref sound 1) (sum width 1) 0.5) 
(mult (aref sound 0) (sum width -1) -0.5)))) 

(butterfly s width)
