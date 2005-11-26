;nyquist plug-in
;version 1
;type process
;name "Audio Sample Sequencer 1.b..."
;action "Sequencing audio..."
;info "Audio Sample Sequencer 1.b by David R. Sky\nReleased under terms of the GNU Public License""

;control tempo "Tempo" real "beats per minute" 120 30 600
;control sig "Beats per measure" int "beats" 4 1 16
;control count "Sequences to generate" int "sequences" 16 1 1000
;control ts1 "Tone shift 1" int "semitone change" 0 -60 60
;control vol1 "Volume 1" real "linear" 1.0 0.0 1.0
;control ts2 "Tone shift 2" int "semitone change" 0 -60 60
;control vol2 "Volume 2" real "linear" 1.0 0.0 1.0
;control ts3 "Tone shift 3" int "semitone change" 0 -60 60
;control vol3 "Volume 3" real "linear" 1.0 0.0 1.0
;control ts4 "Tone shift 4" int "semitone change" 0 -60 60
;control vol4 "Volume 4" real "linear" 1.0 0.0 1.0
;control ts5 "Tone shift 5" int "semitone change" 0 -60 60
;control vol5 "Volume 5" real "linear" 1.0 0.0 1.0
;control ts6 "Tone shift 6" int "semitone change" 0 -60 60
;control vol6 "Volume 6" real "linear" 1.0 0.0 1.0
;control ts7 "Tone shift 7" int "semitone change" 0 -60 60
;control vol7 "Volume 7" real "linear" 1.0 0.0 1.0
;control ts8 "Tone shift 8" int "semitone change" 0 -60 60
;control vol8 "Volume 8" real "linear" 1.0 0.0 1.0
;control ts9 "Tone shift 9" int "semitone change" 0 -60 60
;control vol9 "Volume 9" real "linear" 1.0 0.0 1.0
;control ts10 "Tone shift 10" int "semitone change" 0 -60 60
;control vol10 "Volume 10" real "linear" 1.0 0.0 1.0
;control ts11 "Tone shift 11" int "semitone change" 0 -60 60
;control vol11 "Volume 11" real "linear" 1.0 0.0 1.0
;control ts12 "Tone shift 12" int "semitone change" 0 -60 60
;control vol12 "Volume 12" real "linear" 1.0 0.0 1.0
;control ts13 "Tone shift 13" int "semitone change" 0 -60 60
;control vol13 "Volume 13" real "linear" 1.0 0.0 1.0
;control ts14 "Tone shift 14" int "semitone change" 0 -60 60
;control vol14 "Volume 14" real "linear" 1.0 0.0 1.0
;control ts15 "Tone shift 15" int "semitone change" 0 -60 60
;control vol15 "Volume 15" real "linear" 1.0 0.0 1.0
;control ts16 "Tone shift 16" int "semitone change" 0 -60 60
;control vol16 "Volume 16" real "linear" 1.0 0.0 1.0

; Audio Sample Sequencer 1.b by David R. Sky, December 17, 2004  
; sequences mono and stereo audio already loaded into Audacity
; (Sequencer 1.a simplified to make a version 1 plug-in)

; make sure sig and count are integers
(truncate sig)
(truncate count)

; set duration of 1 beat
(setf beatlen (/ 60.0 tempo))

; set duration of 1 measure
(setf measurelen (* sig beatlen))

; setting list of variables ts (tone shift) and vol
(setf variables (list ts1 vol1 ts2 vol2 ts3 vol3 ts4 vol4 ts5 vol5
ts6 vol6 ts7 vol7 ts8 vol8 ts9 vol9 ts10 vol10 ts11 vol11 ts12
vol12 ts13 vol13 ts14 vol14 ts15 vol15 ts16 vol16)) 

; getting stereo sample
(defun get2sample (s)
(setq sample0 (aref s 0))
(setq sample1 (aref s 1))) 

; taking audio sample
(if (arrayp s)  ; if audio is stereo
(get2sample s)
(setq sample s)) 

; clearing track
(mult 0.0 s)

; initializing sequence as blank audio
(setq sequence (s-rest 0.0))

; function to tone shift stereo sample
(defun changestereo (sample0 sample1  ts vol)
(vector 
(mult vol (force-srate 44100 (stretch-abs ts (sound sample0))))
(mult vol (force-srate 44100 (stretch-abs ts (sound sample1)))))) 

; function to do tone shift with volume adjust
(defun change (sample ts vol)
(setf ts (expt 2.0 (/ ts 12.0)))
(setf ts (/ 1.0 ts))
(if (arrayp s) ; if sample is stereo
(changestereo sample0 sample1 ts vol)
(mult vol (force-srate 44100 (stretch-abs ts (sound sample)))))  )

; making new note with correct time placement in sequence
(defun makenote (i sig sample ts vol s)
(if (>= i sig) (cue sequence) 
(sim (s-rest 0.0) 
(at-abs (* i beatlen) (cue (change sample ts vol))))))

; generating sequence
(dotimes (i sig)
(setf ts (nth (* i 2) variables))
(setf vol (nth (+ 1 (* i 2)) variables))
(setq sequence (sim (cue sequence)
(makenote i sig sample ts vol s))))

; clearing track
(mult 0.0 s)

; repeat sequence count times
(simrep (i count)
(sim (s-rest 0.0)
(at-abs (* i measurelen) (cue sequence))))

