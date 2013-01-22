;nyquist plug-in
;version 1
;type generate
;name "DTMF Generator"
;action "Generating DTMF ..."
;info "by David R. Sky, Dominic Mazzoni, Roger Dannenberg, W. Borgert\nReleased under terms of the GNU General Public License Version 2\nCreates Dual Tone Multi-Frequency tones also known as Touch Tones.\nEnter a tone string consisting of any of the following characters:\n0...9,  A...D,  *,  #,  a...z\nEach tone is followed by a silence of the specified duration; entering\na space in the string extends the silence by that duration.\nExamples: 1209 1336 1477 1633\n                697   1    2    3    A\n                770   4    5    6  B\n                852   7 8    9    C\n                941   *    0    #     D"

;control keys "Tone string" string "" "180audacity"
;control tl "Tone duration [milliseconds]" int "" 100 1 1000
;control sl "Silence duration after tone [milliseconds]" int "" 100 0 1000
;control twist "Twist [Increased volume of high tone in each tone; dB]" real "" 0.0 0.0 4.0
;control volume "Volume [percent]" int "" 80 1 100

; DTMF (Dual Tone Multi-Frequency) generator
; otherwise known as Touch Tones (TM)
; by David R. Sky, Dominic Mazzoni, Roger B. Dannenberg, W. Borgert
; 2004-09, 2005-04
; January 7, 2006 now includes alphabet
; for example 123audacity 1800recycle 
; includes the 4 military 'numbers'
; A B C D, to the right of the regular number keypad
; Released under terms of the GNU General Public License version 2
; http://www.gnu.org/copyleft/gpl.html

; convert volume percent to flonum
; DTMF uses two tones so we cut volume by further one-half
(setf volume (* volume 0.005))

; convert milliseconds to seconds
(setf tl (* tl 0.001))
(setf sl (* sl 0.001))

; Twist is the ratio of the high to low tone, in db
; used in real telephone applications.

(defun dtmf (key volume tl twist sl)
  (setf low (if (member key 
'(#\1 #\2 #\3 #\A #\a #\b #\c #\d #\e #\f)) 697
              (if (member key 
'(#\4 #\5 #\6 #\B #\g #\h #\i #\j #\k #\l #\m #\n #\o)) 770
                (if (member key 
'(#\7 #\8 #\9 #\C #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)) 852
                  (if (member key 
'(#\* #\0 #\# #\D)) 941 0)))))
  (setf high (if (member key 
'(#\1 #\4 #\7 #\* #\g #\h #\i #\p #\q #\r #\s)) 1209
               (if (member key 
'(#\2 #\5 #\8 #\0 #\a #\b #\c #\j #\k #\l #\t #\u #\v)) 1336
                 (if (member key 
'(#\3 #\6 #\9 #\# #\d #\e #\f #\m #\n #\o #\w #\x #\y #\z)) 1477
                   (if (member key 
'(#\A #\B #\C #\D)) 1633 0)))))
  (setf volume (if (member key '(#\1 #\2 #\3 #\A
                                 #\4 #\5 #\6 #\B
                                 #\7 #\8 #\9 #\C
                                 #\* #\0 #\# #\D
#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
#\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z)) volume 0.0))
  (seq
    (mult volume
          (pwl 0.002 1 (- tl 0.002) 1 tl)
          (sim
            (osc (hz-to-step high) tl)
            (loud (- 0 twist) (osc (hz-to-step low) tl))))
    (s-rest sl)))

(seqrep (i (length keys)) (dtmf (char keys i) volume tl twist sl))

