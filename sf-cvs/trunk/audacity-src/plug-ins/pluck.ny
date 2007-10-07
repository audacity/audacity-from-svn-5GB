;nyquist plug-in
;version 3
;type generate
;name "Pluck..."
;action "Generating pluck sound..."
;info "modified by David R. Sky\nReleased under terms of the GNU General Public License version 2 \nMIDI values for C notes: 36, 48, 60 [middle C], 72, 84, 96 \nIf duration is set to 0, pluck is default 1 second, otherwise user-set duration."

;control p "Pluck MIDI pitch" int "" 60 1 127
;control fade "Fade-out" choice "abrupt fade-out,gradual fade-out" 0
;control dur "Duration [seconds, 0=default 1 second]" real "" 0 0 30

; original pluck.ny modified by David R. Sky October 2007
; to give user option  to use [default] abrupt or gradual fade-out,
; and ability to make pluck sound up to 30 seconds in duration
; [0 seconds tells pluck.ny to use default 1 second duration].

(setf fade (+ fade 1))

; if fade is default abrupt fade-out, don't change envelope type,
; otherwise apply a backward ramp to pluck sound
; to make it a gradual fade-out
(mult (if (= fade 1)
1.0 
(if (= dur 0)
(pwl 0 1 1 0 1)
(pwl 0 1 dur 0 dur)))
; if dur is 0, perform default duration of pluck,
; otherwise generate pluck for dur duration
(highpass8 (if (= dur 0)
(pluck p) 
(pluck p dur)) 1.0))

; arch-tag: bebc6cb8-3bb0-42d5-a467-df6bd1a7f1e4
