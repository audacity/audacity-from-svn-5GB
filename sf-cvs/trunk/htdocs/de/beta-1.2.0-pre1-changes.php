<h2>Aenderungen seit Audacity 1.0</h2>

<h3>Audio in Profiqualitaet</h3>

<ul>
<li>
Mit Audacity koennen jetzt 24-Bit und 32-Bit (Floating Point) Dateien aufgezeichnet und bearbeitet werden. Tracks mit unterschiedlichen Sampleraten/Sampleformaten innerhalb eines Projektes konvertiert Audacity -falls notwendig- in Echtzeit. Dithering und Resampling erfolgt bei allen Konvertierungen in hoher Qualitaet. Fuer das Resampling werden nun die Algorithmen der Bibliothek von <a href="http://www.mega-nerd.com/SRC/">SRC</a>Erik de Castro Lopo.
</li> angewandt.

<li>
Audacity's Audio-Ein/Ausgabe wurde verbessert. Aufnahmen von mehr als zwei Kanaelen sind damit möglich.  Zur Vermeidung von Spruengen und Buffer Underruns wurde die Latenzeit verringert.
</li>
</ul>


<h3>Effekte</h3>

<ul>
<li>
Three new effects change the pitch and tempo of a track:
 <ul>
  <li>"Pitch" hebt oder senkt die Tonlage innerhalb einer Auswahl ohne die Geschwindigkeit zu beeinflussen.</li>
  <li>"Tempo" wirkt sich anhebend bzw. absenkend auf die Geschwindigkeit der ausgewaehlten Daten aus, jedoch ohne Auswirkung auf die Tonhoehe.</li>
  <li>Die Option "Speed" dagegen ändert sowohl die Geschwindigkeit als auch die Tonhoehe, aehnlich der Aenderung der Geschwindigkeit beim Plattenspieler oder Tonbandgeraet.</li>
 </ul>
</li>

<li>
Die meisten Effekte enthalten jetzt eine Moeglichkeit zum Vorhoeren. So besteht die Moeglichkeit, verschiedene Einstellungen auszuprobieren, ohne das Fenster schliessen zu muessen. Eine neue Funktion erlaubt es ausserdem, den zuletzt angewandten Effekt zu wiederholen, ohne das Fenster erneut oeffnen zu muessen.
</li>

<li>
Weitere neue Effekte beinhalten:
 <ul>
  <li>Dynamik Kompressor</li>
  <li>Repeat-Funktion (Wiederholung) fuer Samples in Endlosschleifen</li>
  <li>Normalisierung - Korrektur von Lautstaerke und Gleichspannungsabstand</li>
 </ul>
</li>
</ul>


<h3>Neue Editorfunktionen</h3>

<ul>
<li>
Das Huellkurvenwerkzeug, normalerweise fuer weiche Ein- und Ausblendungen benutzt, die Lautstaerke ganzer Tracks anzuheben oder abzusenken.
</li>

<li>
The new "Time track" feature is similar to the volume envelope, but
instead changes the playback speed smoothly as a track plays.
</li>

<li>
Jede Spur hat jetzt ihre eigenen Lautstaerke- und Balanceregler zum bequemeren Mischen.
</li>

<li>
Audacity kann Ueberschneidungen finden, die weichen Schnitt und saubere Endlosschleifen ermoeglichen. Durch das Druecken von "Z" werden die Enden an die beste Position verschoben.
</li>
</ul>


<h3>Plugins</h3>

<ul>
<li>
Audacity kann jetzt auch unter Linux <a href="http://www.ladspa.org/">LADSPA-Plugins</a> laden.
</li>

<li>
Audacity 1.2 verwendet zur digitalen Signalverarbeitung Nyquist, eine Programmiersprache die es Usern erlaubt, Effekte in einer LISP-aehnlichen Sprache zu programmieren.
</li>
</ul>


<h3>Im- und Export von Dateien</h3>

<ul>
<li>
Audacity 1.2 arbeitet mit Projektdateien in einem neuen XML-Format.  Audacity 1.2 oeffnet und konvertiert automatisch Projektdateien der Vorgaengerversionen.
</li>

<li>
Audacity 1.2 verwendet <a href="http://www.underbit.com/products/mad/">Libmad</a>
fuer ein schnelleres Umrechnen der MP3-Dateien..  Erik de Castro Lopo's
<a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a> verspricht eine bessere Kompatibilitaet mit vielen nichtkomprimierten Audioformaten.
</li>

<li>
Fuer einen Ogg Vorbis-Export werden die aktuellsten <a href="http://www.vorbis.com/">Vorbis Bibliotheken</a> genutzt.
</li>

<li>
Audacity erlaubt jetzt die Auswahl mehrerer Dateien fuer das Oeffnen/Importieren in ein einzelnes Projekt.  A new "LOF"
file format provides a way for Audacity to open a group of files with
offsets listed in a text file.
</li>
</ul>


<h3>Verbesserte Oberflaeche</h3>

<ul>
<li>
Neue Bearbeitungs- und Mixwerkzeugleisten erlauben schnellen Zugriff auf die wichtigsten Funktionen.
</li>

<li>
Das neue Zeichenwerkzeug erlaubt die eingezoomte Bearbeitung sehr feiner Samples.  Das neue Multifunktionswerkzeug gewaehrt schnellsten Zugriff auf verschiedenste Bearbeitungsfunktionen ohne laestiges Wechseln der Werkzeuge.
</li>

<li>
Viele neue Tastenfunktionen wurden hinzugefuegt, die Tastatur-Shortcuts koennen jetzt angepasst werden.
</li>

<li>
Neue Funktionen:
 <ul>
  <li>Endloswiedergabe durch Druecken der Taste "L" oder Gedrueckthalten der Shift-Taste beim Mausklick auf Wiedergabe.</li>
  <li>Druecken der Taste "1", um einen Ausschnitt mit einer Laenge von einer Sekunde ab Cursor anzuspielen.</li>
 </ul>
</li>

<li>
Das Mausrad kann zum Ein- und Auszoomen benutzt werden.
</li>

<li>
In Tracks kann jetzt Durch Klicken oder Ziehen in die vertikalen Lineale eingezoomt werden. Mit Shift+Mausklick oder einfachen Rechtsklick kann wieder ausgezoomt werden.
</li>

<li>
Lineale und Statusleiste koennen Zeitangaben in verschiedenen formaten darstellen, u.B. in Sekunden, Samples oder in Einzelbildern (Video Frames).</li>

<li>
Die Benutzeroberflaeche von Audacity kann jetzt anderen Sprachen neben Englisch verfuegbar gemacht werden. Unter <a href="translation/">Audacity uebersetzen</a> koennen Sie helfen, Audacity Ihre Sprache zu uebersetzen.
</li>
</ul>