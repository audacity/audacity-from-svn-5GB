;nyquist plug-in

;version 3

;type generate

;name "Pluck..."

;action "Generating pluck sound..."

;info "modified by David R. Sky\nReleased under terms of the GNU General Public License version 2 \nMIDI values for C notes: 36, 48, 60 [middle C], 72, 84, 96 \nIf duration is set to 0, pluck is default 1 second, otherwise user-set duration."



;control p "Pluck MIDI pitch" int "" 60 1 127

;control fade "Fade-out type" choice "abrupt,gradual" 0

;control dur "Duration [seconds, 0=default 1 second]" real "" 0 0 30



; original pluck.ny modified by David R. Sky October 2007

; [vastly simplified later]

; to give user option  to use [default] abrupt or gradual fade-out,

; and ability to make pluck sound up to 30 seconds in duration

; [0 seconds tells pluck.ny to use default 1 second duration].



; set pluck duration

(setf dur (if (= dur 0) 1.0 dur))



; set final-amp for abrupt or gradual fade

(setf final-amp (if (= fade 0) 0.001 0.000001))



; original pluck has DC offset for high MIDI values,

; so have used a highpass8 filter here

(highpass8 (pluck p dur final-amp) 1.0)

