
<?php BoxTop($develNewsStr); ?>

<p>
Diese Seite wird der Platz werden, wo man sehen kann, was die einzelnen Entwickler geradte tun. 
In der zwischenzeit hier einige Informationen was wir gerade machen( Anm.d.Übersetzers: Isr leider etws veraltet)
Dieses betrifft Audacity 1.1
</p>

<p>
<b>Neue Sachen in Version 1.1:</b>
<pre>
  * Core audio Processing:
    - Unterst&uuml;tztung f&uuml;r 24-bit und 32-bit Sample Formate
    - Automatische real-time Resampling (Es wird die lineare Interpolation benutzt)
  * Effects:
    - Unterst&uuml;tztung der LADSPA plugins in Linux / Unix
  * Dateiformate:
    - Neues XML-basiertes Audacity Projekt Format
    - Komplette Ogg Vorbis Unterst&uuml;tztung (Import und Export)
    - Export f&uuml;r jedes command-line Programm in Unix
    - Unterst&uuml;tztung um wesentlich mehr Dateitypen von nicht komprimierten Audiodateien zu lesen
      including ADPCM WAV files.
  * Toolbars
    - Neuer Code in den Toolbars; nun werden die Systemfarben gezogen
    - Neue Buttons (Springe zum Anfang bzw. Ende)
    - Neue &Auml;ndern Toolbar
    - Die Buttons sind nun inaktiv, wenn sie nicht angew&auml;hlt werden k&ouml;nnen
  * User Interface
    - Die Tastaturkommandos können nun komplett selber belegt werden
    - Autoscroll wenn ein Lied spielt bzw. aufgenommen wird
    - Neue Regeln die in der Hauptansicht bzw. bei den FFT Filter Effekten eingesetzt werden
    - Die Wellenform zeigt die durchschnittlichen Wertte innerhalb von Peak's nun in anderen Farben
  * Localization
    - Audacity kann nun in verschiedene Sprachen übersetzt werden.
</pre>
</p>

<p>
<b>Liste der libraries die benutzt werden (Version 1.1.0 und spätere):</b>
<table border=0 cellpadding=8 cellspacing=2>
<tr>
<th>Library</th>
<th>Purpose</th>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://wxwidgets.org/">wxWidgets</a>
<td bgcolor="#ccccff"
>Dieses ist die library die unserer GUI (Menüs, Buttons,
    Windows, drawing, etc.) erlaubt, auf den verschiedenen Betriebssystemen zu laufen
    wie Mac, Windows, und Unix. wxWidgets hat ebenfalls weitere nützliche C++ classes und 
    Audacity basiert zu 100% auf dieser library. Wenn Sie betriebssystemunabhängige Entwicklung 
	betreiben willst, solltest du diese library beutzen.
</tr>

<tr>
<td bgcolor="#ccccff"
><a href="http://www.mars.org/home/rob/proj/mpeg/">libmad</a>
<td bgcolor="#ccccff"
><p>
    MAD ist die Abkürzung für MPEG Audio Decoder. Dies ist einer
    der kostenlosen (GPL) MP3 Decoder, und der einzige den wir kennen
    der integer Mathematik benutzt und somit die Möglichkeit hat
    24-bit Output zu erzeugen (Diese Möglichkeit nutzen wir bisher noch nicht).
    Obwohl MP3 Dateien normalerweise von einem 16-bit Input erzeugt werden,
    kann man mit einem solchen MP3 Decoder, der 24-bit output erzeugt, bessere 
    Qualität erzeugen und man hat grundsätzlich mehr Möglichkeiten und Kontrolle.
    Diese library ist sehr schnell (gegenüber xaudio, welches wir vorher benutzt hatten) und
    vsehr stabil. Der Autor ist Rob Leslie.</p>
    <p>Diese library braucht man nur, wenn man MP3 Dateien lesen willst.</p>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.mars.org/home/rob/proj/mpeg/">libid3tag</a></td>
<td bgcolor="#ccccff"
><p>
    Diese library ist ebenfalls von Rob Leslie und eine kleine und feine library
    um ID3 tags in MP3 zu lesen und zu schreiben.</p>
    <p>Diese library ist optional; wenn Sie vorhanden ist, bekommt der User einen
    ID3 Tag Dialog, wenn er MP3 speichern will.<br>
    libid3tag wird nicht separat vertrieben, sondern ist Teil von MAD.</p>
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.xiph.org/ogg/vorbis/"
>libogg<br>libvorbis<br>libvorbisfile</a>
</td>

<td bgcolor="#ccccff"
>
<p>Ogg Vorbis ist beides, ein En/Decoder, sowie eine library.
Dieses Format sollte ein Ersatz für MP3 werden und viele Leute denken dass es
gleichertig und an manchen Ecken sogar besser, sowohl in der Dateigrösse, als auch
in der Qualität ist. Die letzte (beta) Version von Audacity schreibt und liest dieses Format.
</p>
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
>
<a href="http://www.portaudio.com">portaudio</a>
</td>
<td bgcolor="#ccccff"
>
Diese library erlaubt uns das Audio I/O auf verschiedene Betriebssystemen und dabei nur
eine gemeinsame API zu benutzen. Debei werden die Unterschiede zwischen den systemen
"versteckt" und es hat über alles eine gute Geschinwigkeit. Der reine Audio Code 
ist für Windows (MME und DirectX), Unix/OSS und MacOS 9 stabil und Portierungen für MacOS X, Linux/ALSA und Linux/aRts
sind in Arbeit.
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a></td>

<td bgcolor="#ccccff"
>
Das ist die neue library, die wir benutzen, um die Audiodaten zu lesen und zu schreiben
wie zum Beispiel WAV, AIFF und AU. Einfache Kompression wie ADPCM wird unterstützt, aber keine
starke Kompression.
</td>
</tr>
</table>
</p>


<?php BoxBottom(); ?>

