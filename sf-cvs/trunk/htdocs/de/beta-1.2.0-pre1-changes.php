<h2>Änderungen seit Audacity 1.0</h2>

<h3>Audio in Profiqualität</h3>

<ul>
<li>
Mit Audacity können jetzt 24-Bit und 32-Bit (Floating Point) Dateien aufgezeichnet und bearbeitet werden. Tracks mit unterschiedlichen Sampleraten/Sampleformaten innerhalb eines Projektes konvertiert Audacity -falls notwendig- in Echtzeit. Dithering und Resampling erfolgt bei allen Konvertierungen in hoher Qualität. Fuer das Resampling werden nun die Algorithmen der Bibliothek von <a href="http://www.mega-nerd.com/SRC/">SRC</a>Erik de Castro Lopo.
</li> angewandt.

<li>
Audacity's Audio-Ein/Ausgabe wurde verbessert. Aufnahmen von mehr als zwei Kanälen sind damit möglich.  Zur Vermeidung von Sprüngen und Buffer Underruns wurde die Latenzeit verringert.
</li>
</ul>


<h3>Effekte</h3>

<ul>
<li>
Three new effects change the pitch and Tempo of a track:
 <ul>
  <li>"Pitch" hebt oder senkt die Tonlage innerhalb einer Auswahl ohne die Geschwindigkeit zu beeinflussen.</li>
  <li>"Tempo" wirkt sich anhebend bzw. absenkend auf die Geschwindigkeit der ausgewählten Daten aus, bleibt jedoch ohne Auswirkung auf die Tonhöhe.</li>
  <li>Die Option "Speed" dagegen ändert sowohl die Geschwindigkeit als auch die Tonhöhe, ähnlich der Änderung der Geschwindigkeit beim Plattenspieler oder Tonbandgerät.</li>
 </ul>
</li>

<li>
Die meisten Effekte enthalten jetzt eine Möglichkeit zum "Vorhören". So besteht die Möglichkeit, verschiedene Einstellungen auszuprobieren, ohne das Fenster schliessen zu müssen. Eine neue Funktion erlaubt es außerdem, den zuletzt angewandten Effekt zu wiederholen, ohne das Fenster erneut öffnen zu müssen.
</li>

<li>
Weitere neue Effekte beinhalten:
 <ul>
  <li>Dynamik Kompressor</li>
  <li>Repeat-Funktion (Wiederholung) für Samples in Endlosschleifen</li>
  <li>Normalisierung - Korrektur von Lautstärke und Gleichspannungsabstand</li>
 </ul>
</li>
</ul>


<h3>Neue Editorfunktionen</h3>

<ul>
<li>
Das Hüllkurvenwerkzeug, normalerweise für weiche Ein- und Ausblendungen benutzt, kann jetzt die Lautstaerke ganzer Tracks anheben oder absenken.
</li>

<li>
Das neue "Time Track"-Feature ähnelt in der Funktion dem vorgenannten Hüllkurvenwerkzeug, sorgt jedoch für saubere Wechsel der Geschwindigkeit beim Abspielen der Spur.
</li>

<li>
Jede Spur hat jetzt ihre eigenen Lautstärke- und Balanceregler zum bequemeren Mischen.
</li>

<li>
Audacity kann Überschneidungen finden, die einen weichen Schnitt und saubere Endlosschleifen erlauben. Durch das Drücken von "Z" werden die Enden jeweils an die beste Position verschoben.
</li>
</ul>


<h3>Plugins</h3>

<ul>
<li>
Audacity kann jetzt auch unter Linux <a href="http://www.ladspa.org/">LADSPA-Plugins</a> laden.
</li>

<li>
Audacity 1.2.0 verwendet zur digitalen Signalverarbeitung Nyquist, eine Programmiersprache die es Usern erlaubt, Effekte in einer LISP-ähnlichen Sprache zu programmieren.
</li>
</ul>


<h3>Im- und Export von Dateien</h3>

<ul>
<li>
Audacity 1.2 arbeitet mit Projektdateien in einem neuen XML-Format.  Audacity 1.2 öffnet und konvertiert automatisch Projektdateien der Vorgängerversionen.
</li>

<li>
Audacity 1.2.0 verwendet <a href="http://www.underbit.com/products/mad/">Libmad</a>
für ein schnelleres Umrechnen der MP3-Dateien..  Erik de Castro Lopo's
<a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a> verspricht eine bessere Kompatibilität mit vielen nicht komprimierten Audioformaten.
</li>

<li>
Fuer einen Ogg Vorbis-Export werden die aktuellsten <a href="http://www.vorbis.com/">Vorbis Bibliotheken</a> genutzt.
</li>

<li>
Audacity erlaubt jetzt die Auswahl mehrerer Dateien für das Öffnen/Importieren in ein einzelnes Projekt.  Das neue "LOF" Dateiformat ermöglicht es dem Anwender an Hand einer Textdatei mit Positionsangaben, gleichzeitig eine Vielzahl zu importierender Dateien zu laden und in einem einzigen Audacity-Projekt zu öffnen.
</li>
</ul>


<h3>Verbesserte Oberfläche</h3>

<ul>
<li>
Neue Bearbeitungs- und Mixwerkzeugleisten erlauben schnellen Zugriff auf die wichtigsten Funktionen.
</li>

<li>
Das neue Zeichenwerkzeug erlaubt die eingezoomte Bearbeitung sehr feiner Samples.  Das neue Multifunktionswerkzeug gewährt schnellsten Zugriff auf verschiedenste Bearbeitungsfunktionen ohne lästiges Wechseln der Werkzeuge.
</li>

<li>
Viele neue Tastenfunktionen wurden hinzugefuegt, die Shortcuts können jetzt angepasst werden.
</li>

<li>
Neue Funktionen:
 <ul>
  <li>Endloswiedergabe durch Drücken der Taste "L" oder Gedrückt halten der Shift-Taste beim Mausklick auf Wiedergabe.</li>
  <li>Drücken der Taste "1", um einen Ausschnitt mit einer Länge von einer Sekunde ab Cursor anzuspielen.</li>
 </ul>
</li>

<li>
Das Mausrad kann zum Ein- und Auszoomen benutzt werden.
</li>

<li>
In Tracks kann jetzt Durch Klicken oder Ziehen in die vertikalen Lineale eingezoomt werden. Mit Shift+Mausklick oder einfachen Rechtsklick kann wieder ausgezoomt werden.
</li>

<li>
Lineale und Statusleiste können Zeitangaben in verschiedenen Formaten darstellen, u.B. in Sekunden, Samples oder in Einzelbildern (Video Frames).</li>

<li>
Die Benutzeroberfläche von Audacity kann jetzt anderen Sprachen neben Englisch verfügbar gemacht werden. Unter <a href="translation/">Audacity übersetzen</a> können Sie helfen, Audacity in Ihre Sprache zu übersetzen.
</li>
</ul>