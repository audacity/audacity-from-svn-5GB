;nyquist plug-in
;version 1
;type process
;name "Panning..."
;action "Panning audio..."
;info "Panning by David Sky\n0=left, 1=right"

;control where "Pan position" real "where" 0.50 0.00 1.00 

;; Note: Audacity users must use this plug-in with stereo audio.
;; Best results when audio starts in center, 
;; otherwise drop in volume will occur.

;; 0=pan left
;; 1=pan right

(defun pan3 (s where)
   (vector (mult (aref s 0) (sum 1 (mult -1 where)))
       (mult (aref s 1) where)))

(pan3 (mult 2 s) where)
