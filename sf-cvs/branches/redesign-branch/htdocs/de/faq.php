<?php BoxTop("Frequently Asked Questions"); ?>

<!--
<table width=100% cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+3">Frequently Asked Questions</font>
</td></tr></table>
-->

<p>

Allgemein:<br>
<a href="#g1">Ist Audacity wirklich frei und was ist der Grund dafür?</a><br>
<a href="#g2">Wer entwickelt Audacity?</a><br>
<a href="#g3">Wie kann ich helfen?</a><br>
<a href="#g4">Ich habe einen Fehler gefunden! Was jetzt?</a><br>
<p>
Installation:<br>

<a href="#i1">Windows: Was soll ich mit der .exe Datei?</a><br>
<a href="#i2">Windows: Wie entferne ich Audacity?</a><br>
<a href="#i3">Mac: Was soll ich mit der .sit Datei machen?</a><br>
<a href="#i4">Unix: Warum habe ich fehlende Abhängigkeiten, wenn ich das .rpm Paket installieren will?</a><br>
<a href="#i5">Unix: Warum erhalte ich eine Fehlermeldung, wenn die Bibliothek libwx_gtk.so lädt?</a><br>
<p>
Verwendung:<br>
<a href="#a1">Wie kann ich zwei Stücke mixen?</a><br>
<a href="#a2">Kann man Stimmen entfernen?</a><br>

<a href="#a3">Wenn ich versuche Harmonien aufzunehmen sind beide Stücke nicht zeitgleich bzw. nicht synchron ?
             </a><br>
<a href="#a4">Warum wird bei Audacity kein MP3 encoder mitgeliefert?</a><br>
<a href="#a5">Kann man RealAudio oder andere Streaminmedien aufnehmen ?</a><br>
<a href="#a6">Hilfe! Ich drücke Aufnahme, aber alles was ich bekomme ist "Stille"!</a><br>
<a href="#a7">Wie trenne ich ein Lied (Track) in verschiedene Stücke ?</a><br>
<a href="#a8">Wie bekomme ich die Lieder von einer Audio CD ?</a><br>

<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Allgemein</font>
</td></tr></table>

<p>

<a name="g1"></a>
<font size=+1><b>
1. Ist Audacity wirklich frei und was ist der Grund dafür?<br>
</b></font>

<p>
Ja, es ist in jeder Hinsicht frei. Audacity steht nicht nur kostenlos zum Download zur Verfügung,
Sie haben auch alle Freiheiten bei der Benutzung des Programms.

<p>
Audacity wird unter der
<a href="http://www.gnu.org/copyleft/gpl.html"
>General Public License (GPL)</a> vertrieben.
Sie können das Programm sowohl für private als auch für berufliche Zwecke verwenden.
Weitergabe und Verkauf sind erlaubt. Der Quelltext ist frei verfügbar und darf beliebig
verändert werden, sofern die Änderungen unter der GPL veröffentlicht werden.

<p>
Audacity wurde in Verwendung von wxWindows, eine Bibliothek die unter der weniger
restriktiven LGPL vertrieben wird, entwickelt.
Für nähere Informationen besuchen Sie die Projektseite
<a href="http://www.wxwindows.org">wxwindows.org</a>.

<p>
Die Autoren von Audacity entschieden sich aus mehreren Gründen für die GPL.
Einige sind einfach freundliche Menschen, andere sind der Meinung,
dass Software grundsätzlich frei sein sollte. Wieder andere glauben,
dass es gleichermaßen freie <b>und</b> kommerzielle Software geben sollte.
Ein wichtiger Grund ist wohl, dass das Programm so viel schneller bekannt wird.
Viele von uns sehen lieber Millionen zufriedene Menschen,
die Audacity zum Nulltarif einsetzen als vielleicht tausend zahlende Anwender.

<p>
Weiterhin sind für ein freies Projekt mehr Mitarbeiter zu finden.
Wäre Audacity Shareware, wäre es fraglich ob dutzende Entwickler auf der ganzen Welt Code,
Fehlerbereinigungen, Dokus und Grafiken beisteuerten.
Wo wäre das Programm dann jetzt?

<p>
<a name="g2"></a>
<font size=+1><b>
2. Wer entwickelt Audacity?<br>
</b></font>

<p>
Dominic Mazzoni startet die Entwicklung von Audacity im Herbst 1999, während er Student an der Carnegie Mellon University in Pittsburgh, PA, USA war. Er arbeitet an einer Forschungsarbeit mit seinem Professor Roger Dannenberg und sie benötigten ein Programm, das Audioanalysen bildlich sichtbar machte. Nach einiger Zeit entwickelte das Programm sich zu einem, für viele Zwecke einsetzbare, Audiobearbeitungsprogramm und andere Leute beteiligten sich an der Weiterentwicklung.
<p>
Heute wird das Programm mit Hilfe von Sourceforge entwickelt,
wodurch viele Menschen über das Internet an freien Softwareprojekten zusammenarbeiten können.
Für nähere Informationen gibt es die Seite
 <a href="http://www.sourceforge.net"
>sourceforge.net</a>.
Dutzende Entwickler haben zur Entwicklung von Audacity beigetragen und das Programm
wird kontinuierlich verbessert..

<p>
<a name="g3"></a>
<font size=+1><b>
3. Wie kann ich helfen?<br>
</b></font>

<p>
Suchen Sie Fehler und teilen Sie uns die Fehler mit.
Schreiben Sie Code. Übersetzen Sie das Prorgamm in Ihre Sprache. Entwerfen Sie Graphiken.
Tragen Sie sich in die Benutzer-Mailingliste von Audacity ein. Spenden Sie Geld.

<p>
Schauen Sie doch einmal bei unserer <?php print "<a href=donatetime.php?$langLinkStr>"; ?>Spenden-Seite</a>
 vorbei, um näheres zu erfahren.
<p>

<a name="g4"></a>
<font size=+1><b>
4. Ich habe einen Fehler gefunden! Was nun?<br>
</b></font>

<p>
Posten Sie den Fehler bitte im <a href="http://audacity.fuchsi.de/">Audacity-Forum.</a>
Das wichtigste bei einem Fehlerreport ist die Präzision.
Wir brauchen so viele Information wie nur möglich, um den Fehler reproduzieren zu können,
ansonsten können wir ihn schlecht ausbessern.

<p>
Lassen Sie uns wissen, welches Betriebssystem Sie verwenden
(z.B. Windows XP, MacOS X, Fedora Core, etc.) und
welche Hardware vielleicht etwas mit dem Fehler zu tun haben könnte.

<p>
Ist der Fehler reproduzierbar? Wenn er regelmäßig auftritt,
sagen Sie uns Schritt für Schritt, welche Operationen durchzuführen sind,
damit sich der Fehler bemerkbar macht. Wenn Sie eine Fehlermeldung erhalten,
vergessen Sie bitte nicht, uns den genauen Wortlaut der Meldung mitzuteilen.

<p>
Wir wollen alle Fehler bereinigen! Vielen Dank im Voraus für Ihre Mithilfe.

<p>
<br>
<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Installation</font>
</td></tr></table>

<p>
<a name="i1"></a>
<font size=+1><b>
1. Windows: Was fange ich mit der .exe Datei an?<br>
</b></font>

<p>
Wählen Sie im Browser die Option "Speichern" und speichern Sie die Datei in einem Ordner, den Sie
im Explorer wiederfinden. Starten Sie den Explorer, wählen Sie den Ordner aus und doppelklicken
Sie die Datei um das Setup zu starten. Dann werden die Programmdateien installiert und die
Startmenüeintrage erstellt.

<p>
Wenn Sie die .exe bereits heruntergeladen haben, doppelklicken Sie und das Setup wird gestartet.

<p>
Falls Sie nichts von Installationsroutinen halten, können Sie Sie auch das Zip Archiv herunterladen
und die Dateien (mit Winzip, PowerArchiver usw.) in ein beliebiges Verzeichnis entpacken.


<p>
<a name="i2"></a>
<font size=+1><b>
2. Windows: Wie entferne ich Audacity?<br>
</b></font>

<p>
Audacity kann in der Systemsteuerung unter "Software" deinstalliert werden:
Wählen Sie den Eintrag "Audacity" und klicken Sie auf "Entfernen" - die Deinstallationsroutine
wird gestartet.

<p>
Im Fall der Zipdatei löschen Sie das Programmverzeichnis

<p>
<a name="i3"></a> <font size=+1><b> 3. Mac: Was soll ich mit der .sit Datei?<br> </b></font>

<p>
Sie benötigen eine aktuelle Stufflt Expander Version.
Dieses Programm ist auf allen Macintosh-Computern vorhanden und
normalerweise in den Browser integriert. Falls das Archiv nicht automatisch
entpackt wird, ziehen Sie "audacity.sit" zum Stuffit Expander, um es zu entpacken.

<p>
Hier ist keine Installation mehr notwendig. Ziehen Sie lediglich den neuen
Ordner in Ihr Programme Verzeichnis.
Um Audacity zu entfernen, muß nur das Verzeichnis gelöscht werden.

<p>
<a name="i4"></a>
<font size=+1><b>
4. Unix: Warum habe ich fehlende Abhängigkeiten, wenn ich das .rpm Paket installieren will?
<br></b></font>

<p>
Zunächst stellen Sie bitte sicher, dass wxGTK installiert ist.
Dies kann zum Beispiel durch "rpm -qa | grep wxGTK" erfolgen - es sollte dann in etwa folgendes zu sehen
sein:<br>
<code>wxGTK-2.2.9-55<br>
wxGTK-devel-2.2.9-55</code><br>
Ein "rpm -q wxGTK" tut es zur Not auch ;)<br>
Die Links zu den erforderlichen Paketen sind auf auf unserer
<a href="http://audacity.sourceforge.net/unix.php">Linux-Seite</a> zu finden.
Falls beim Installationsversuch des Paketes eine Fehlermeldung erscheint obwohl die angemahnte
Bibliothek einwandfrei installiert ist, fügen Sie einfach die Option "--nodeps" hinzu.

<p>
<a name="i5"></a>
<font size=+1><b>
5. Unix: Warum erhalte ich eine Fehlermeldung, wenn die Bibliothek library libwx_gtk.so geladen wird?<br></b></font>

<p>Das kann zwei Gründe haben:
<ul>
<li>Wenn Sie das wxWindows - RPM installiert haben, teilen Sie Ihrem System (als root)
das Vorhandensein der Bibliotheken mittels "ldconfig" mit.</li>
<li>Wenn Sie wxWindows selbst kompiliert haben, wird es warscheinlich in /usr/local sein.
/usr/local/lib ist aber oft nicht im Pfad (PATH) enthalten. Sie können den "Fehler" durch hinzufügen
des Pfades in der /etc/ld.so.conf beheben. Bei dieser Gelegenheit können Sie auch
gleich /usr/local/bin eintragen, damit das Programm "wx-config" gefunden wird.</li>
</ul>


<p>
<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Arbeiten mit Audacity</font>
</td></tr></table>

<p>
<a name="a1"></a>
<font size=+1><b>
1. Wie kann ich zwei Stücke mixen?
</b></font>

<p>
Audacity mixt automatisch. Sie müssen lediglich zwei Stücke in das selbe Projekt importieren
(Import Audio). Wenn Sie den Aufnahme-Button betätigen, wird auch ein neuer Track erstellt.

<p>
Wenn Sie Ihr Projekt exportieren (wav, ogg, ...) werden alle Tracks zu einem Stück "verschmolzen".
Den gleichen Effekt erzielt man durch "Quick Mix" im Project-Menü.

<p>
<a name="a2"></a>
<font size=+1><b>
2. Wie kann ich Stimmen aus Stücken entfernen?
</b></font>

<p>
Bei einigen Stereo-Aufnahmen ist es möglich, die Stimmen zu entfernen weil die Stimme bei der
Studioaufnahme genau in der Mitte platziert wurde. Bei solchen Stücken kann man den rechten
Kanal vom Linken subtrahieren (oder umgekehrt) und die Stimmen sind entfernt.

<p>
<b>Das geht aber nur mit einigen wenigen Aufnahmen!</b>
<p>
Um das in Audacity zu bewerkstelligen, importieren Sie zunächst einen Stereo - Track.
Klicken Sie das Trackmenü an (kleiner Pfeil, der nach unten zeigt) und wählen Sie "Split Stereo Track"
aus.
Nun haben Sie die beiden Kanäle getrennt und können einen davon auswählen
indem Sie in den grauen Bereich links neben der Visualisierung des Stückes klicken.
Im Effect-Menü finden Sie den Eintrag "Invert".  Durch Auswahl dieser Funktion wird der ausgewählte
Kanal invertiert. Dies führt dazu, dass Signale, die zuvor sowohl im linken als auch im rechten Kanal
vorhanden waren, sich nun gegenseitig aufheben. In einigen Fällen kann mit dieser Methode der Gesang
ausgeblendet werden (Karaoke-Effekt). Das funktioniert aber nur bei manchen Aufnahmen.
Einfach probieren!

<p>
<a name="a3"></a>
<font size=+1><b>
3. Wenn ich versuche eigene Harmonien aufzunehmen, sind diese nicht synchron. Was kann ich tun?</b></font>
<p>
Das ist normal und liegt daran, daß es eine kurze Zeitverzögerung zwischen dem Abspielen der Töne im Programm und dem erreichen deiner Lautsprecher gibt (Also dem Hören). Audacity berichtigt diese kurze Zeitverzögerung nicht automatisch, daß musst du manuell machen.

Um dies zu tun, benutze das "Timeshift" Modul von Audacity und verschiebe eine der beiden Spuren, bis sie synchron sind. Dies kannst du tun, während du das ganze anhörst.

<p>

<a name="a4"></a>
<font size=+1><b>
4. Warum wird Audacity nicht mit einem MP3 Encoder ausgeliefert?
</b></font>

<p>
Leider ist der Algorythmus zur Codierung und Decodierung von MP3's patentiert.
Audacity kann keinen MP3 Algorythmus beinhalten, ohne Gebühren zu bezahlen oder sich
in vielen Ländern strafbar zu machen.

<p>
Als Kompromiss wurde die Möglichkeit, externe Encoder zu verwenden, eingebaut.
Die Encoder müssen separat geladen werden und es liegt an Ihnen,
ob Sie deren Lizenzbestimmungen akzepieren wollen.
<p></p>
<a name="#a5"></a>
<font size=+1><b>
5. Kann man RealAudio oder andere Streaminmedien mit Audacity aufnehmen?<br><p>
</b></font>
Eigentlich nicht. Die meiste Musik , die gestreamt wird, auch die die für den RealOne Player, Windows Media Player oder Quicktime geliefert wird, soll eigentlich verhindern, dass Sie diese aufnehmen können.
<p>
Aber heutzutage ist es immer noch möglich, alles was Sie auf Ihrem Computer abspielen können, auch aufzunehmen. Verbinde dazu nur deinen Outputkanal an der Soundkarte (in der Regel der grüne Ausgang) mit dem "Line In" Eingang deiner Soundkarte. Benutzte dazu ein 1/8 Inch Audio Stereo Kabel.
<p>
Wenn du dies so gemacht hast, kann Audacity alles aufnehmen, was abgespielt wird. Drücke dazu erst den Aufnahmeknopf in Audacity, danach den „Start“ Knopf in deiner Anwendung und Audacity nimmt alles auf ! (vgl. auch die nächste Frage).



<p>
<a name="#a6"></a>
<font size=+1><b>
6. Hilfe! Ich drücke Aufnahme, aber alles was ich bekomme ist "Stille"!</b></font>
<br>
<p>
Audacity benutzt zur Aufnahme den Standard Input, der in deinem Betriebssystem festgelegt ist. Wenn man mehrere Möglichkeiten hat, Aufnahmen zu starten, kann , bei einer falschen Wahl, Audacity nur „Stille“ aufnehmen.
Ein Beispiel: Du hast ein Mikrofon angeschlossen und hast aber im Input den Line In Eingang gewählt. Dann kommt nichts an und Audacity kann auch nichts aufnehmen.
<p>
Wenn du Windows benutzt, kann du mit Rechts-Klick auf das Lautstärke-Symbol (Rechts unten) den Input-Kanal wählen.
<p>
Wenn du Mac OS9 besitzt, geht dies über die Sound Control. Wenn du Mac OSX benutzt, gehe über die Sound Control in den Systemeinstellungen (Bei 10.2 oder höher ist dies ein eigener Reiter).

<p>
<a name="#a7"></a>
<font size=+1><b>
7. Wie trenne ich ein Lied (Track) in verschiedene Stücke?</b></font><br>
<p>
Manchmal nimmst du verschiedene Lieder auf, die aber zum Schlu&suml; in einem Stück gespeichert sind (zum Beispiel du nimmst eine Seite einer LP in einem "Rutsch" auf).  Wenn du diese Lieder dann auf eine CD brennen willst, aber in verschiedenen Liedern, musst du für jedes Lied eine einzelne Datei speichern.
</p><p>
Es gibt verschiedene Wege dies mit Audacity zu tun, hier sind ein paar davon:
 </p><p>
<b>Methode 1: Export Auswahl</b>
</p><ul>
<li>Markiere das erste Lied in deiner Datei (mit der Maus):
 </li><li>Wähle im Dateimenü "Speichere Auswahl als WAV" und speichere das Lied auf der Festplatte. 
</li><li> Mache dies für alle weiteren Lieder.
</li></ul>
<p>
<b>Methode 2: Löschen und Wiederherstellen</b>
</p><ul>
<li>Lösche alles <i>mit Ausnahme vont</i> dem ersten Lied.
</li><li> Wähle im Dateimenü "Speichere Auswahl als WAV" und speichere das Lied auf der Festplatte.
</li><li>Nimm "Undo" (Wiederherstellen) aus dem Ändern Menü.  Mach dieses bis die komßplette Datei wieder da ist. (Audacity kann unlimitiert wiederherstellen.)
</li><li> Mache dies für alle weiteren Lieder.
</li></ul>
<p>
<b>Method 3: Trennen und Exportieren</b>
</p><ul>
<li>Suche den Teil der Datei, der das erste Lied darstellt
</li><li>Wähle  "Split (Teilen)" aus dem Ändern Menü, dies sorgt dafü, dass deine Auswahl in eine zusätzliche Datei innerhalb Audacity verschoben wird.
 </li><li>Mache dies mit allen weiteren Liedern. Du kannst mit "Wiederherstellen" jederzeit den alten Zustand wiederhertsellen, falls du einen Fehler machst.
</li><li>gehe nun wieder zurück in die erste separierte Datei und wähle im Dateimenü "Speichere Auswahl als WAV" und speichere das Lied auf der Festplatte.
Dies musst du mit jeder Datei machen.</li>
<li>  Hinweis: Um eine Datei komplett und schnell zu markieren drücke auf den Label (Linke Seite neben der Waveform, unter dem Titel).
</li></ul>
<p>


<p>
<a name="#a8"></a>
<font size=+1><b>
8. Wie bekomme ich die Lieder von einer Audio CD ?</b></font><br>
<p>
Audacity kann keine Lieder von Audio CD`s extrahieren (Rippen). Du benötigst ein "Ripper" Programm um dieses durchzuführen.
<p>
Wenn du Windows benutzt, empfehlen wir <a href="http://cdexos.sourceforge.net/">CDex</a>.
<p>
Wenn du Mac OS (9 und X) benutzt, solltest du Apple's <a href="http://www.apple.com/itunes/">iTunes</a> benutzen.
<p>
Für Linux, versuche <a href="http://www.xiph.org/paranoia/">Paranoia</a>.
<p>
<p>
<p>
Für nähere Informationen lesen Sie in unserer Online Hilfe nach.


<?php BoxBottom(); ?>
