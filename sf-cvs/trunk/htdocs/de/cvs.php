<?php BoxTop("CVS"); ?>

Wir benutzen <a href="http://www.cvshome.org">CVS</a>, das Concurrent Versions
System um Audacity gemeinsam entwickeln zu können. Schaue <a
href="http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/audacity/audacity-src/"
>hier</a> um den Quelltext und Sourcecode im CVS zu durchsuchen.

<p>
<h3>Achtung: Version 1.2 ist veröffentlicht:</h3>
Wenn du die stabile Version von Audacity, Version 1.2.x haben willst, muss man nun
<tt>-r AUDACITY_1_2</tt> eingeben, wenn du ein Update oder einen Checkout machen willst.
Andernfalls bekommst du den CVS HEAD, der eventuell instabil wird, wenn wir jetzt anfangen an
Version 1.3.0.

<p><h3>Audacity CVS Kurzanleitung:</h3>
  Wenn Sie den Audacity Quellcode^haben möchten, können Sie einen
  CVS Klient benutzen um einen cvs Branch auf Ihren PC zu laden. Einmal gemacht,
  hilft Ihnen die CVS Software, den Code aktuell zu halten. Beachten Sie bitte folgendene 
  Anleitungen:


<h4>Anonymer CVS Zugang mit einem CVS Kommandozeilen-Klienten:</h4>
<p>Schreiben Sie bitte folgendes in die Zeile: :<br>
<ul><tt>cvs -d:pserver:anonymous@cvs.sourceforge.net:/cvsroot/audacity
login </tt><br></ul>
und drücken die ENTER taste wenn er nach einem Passwort fragt.

<p>Dann, <b>um den letzten Code zu bekommen (1.3.0)</b> (wieder in <b>eine</b> Zeile):
<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.sourceforge.net:/cvsroot/audacity co
audacity</tt></ul>
oder <b>für eine stabile Version: (1.2.0)</b> :<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.sourceforge.net:/cvsroot/audacity co -r AUDACITY_1_2 audacity</tt></ul>
oder <b>für die alte Version 1.0 (1.0.0)</b>:<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.sourceforge.net:/cvsroot/audacity co -r audacity-0_9-branch audacity-old</tt></ul>

<p> Alternativ kann man die <tt>CVSROOT</tt> Umgebungsvariable
auf 
<tt>:pserver:anonymous@cvs.sourceforge.net:/cvsroot/audacity</tt>
(siehe unten) setzen<br>
Dann, <b>um die letzte Version (instabil) (1.3.0)</b>, gib folgendes ein<br> 
   <ul><tt>cvs checkout audacity</tt></ul>
oder, <b>für die stabile Version (1.2)</b>, gib ein <br>
   <ul><tt>cvs checkout -r AUDACITY_1_2 audacity</tt></ul>
oder <b>für die alte Version 1.0 </b>, gib folgendes ein: <br>
   <ul><tt>cvs checkout -r audacity-0_9-branch audacity-old</tt></ul>

<p> Um die <tt>CVSROOT</tt> Environment Variable richtig zu setzen, kann man
die command shell's resource file benutzen oder eines der folgenden Kommandos benutzen:
<h5>In bash or bourne shell, as one line:</h5>
<ul><tt>export
CVSROOT=:pserver:anonymous@cvs.sourceforge.net:/cvsroot/audacity</tt><br></ul>
<h5>In csh or its descendents, as one line:</h5>
<ul><tt>setenv CVSROOT
:pserver:anonymous@cvs.sourceforge.net:/cvsroot/audacity</tt><br></ul>

<hr width="80%">

<h4>Anonymous CVS access with a graphical client</h4>
<h2> Anmerkung des Übersetzers (Pennywize): Ab hier keine weitere Übersetzung mehr, wer diese Tools benutzt muss !!
Englisch können und das folgende auch so lesen können, außerdem ist es sehr technisches Englisch, also kann
kaum übersetzt werden.</h2> 

For a graphical client like wincvs, maccvs, or gcvs,
(available at <a href="http://www.wincvs.org">wincvs.org</a>) you must set
the 
<tt>CVSROOT</tt> variable (in the Admin|Preferences submenu) to be 
<tt>:pserver:anonymous@cvs.sourceforge.net:/cvsroot/audacity</tt>
and select "pserver" or "Password" authentication. Then,
under the Globals tab of the Preferences dialog, make sure
you have unchecked "Checkout read-only (CVSREAD)".  Next,
choose login, and hit the enter key for the password (it is
""). Finally, choose "Create|Checkout module", choose a
location that you want to put the checked-out branch, and
follow the directions below depending on which branch you want.
If you get an error, make sure that your <tt>CVSROOT</tt>
variable does not contain any white space at the end--which
can happen if you copied the variables directly from this web page.

<p>
<b>To get the latest unstable branch (1.3.0):</b><br>
Under the "Checkout Settings" dialog, enter
<tt>audacity</tt> as the module name. Hit "OK" and the
branch will be automatically downloaded onto your computer.
<p>
<b>To get the stable (1.2.0) branch: </b><br>
Under the "Checkout Settings" dialog, enter
<tt>audacity</tt> as the module name.  Then, under the
"Sticky options" tab, check the "Retrieve rev./tag/branch
(-r)" box and enter <tt>AUDACITY_1_2</tt> into the
box beside it. Hit "OK" and the branch will be automatically
downloaded onto your computer.
<p>
<b>To get the old version 1.0 branch: </b><br>
Under the "Checkout Settings" dialog, enter
<tt>audacity-old</tt> as the module name.  Then, under the
"Sticky options" tab, check the "Retrieve rev./tag/branch
(-r)" box and enter <tt>audacity-0_9-branch</tt> into the
box beside it. Hit "OK" and the branch will be automatically
downloaded onto your computer.

<hr width="80%">
New to CVS?  Get started by reading Jim Blandy's <a href="">Introduction
to
CVS</a>, Bob Arnson's <a
href="http://www.cvshome.org/new_users.html">CVS for new
users</a>, or visit the cvs webpage at <a
href="http://www.cvshome.org/">www.cvshome.org</a>.
More detailed information is available in the GPLed chapters of Karl
Fogel's
<a href="http://cvsbook.red-bean.com/cvsbook.html">CVS Book
at cvsbook.red-bean.com</a>, or the "Official" <a
href="http://www.cvshome.org/docs/manual">Per
 Cederqvist manual</a>.


<p> Um eine konkrete Hilfe für CVS in sourceforge.net zu erhalten, versuche folgende
sourceforge Dokumentationen: 
<a
href="http://sourceforge.net/docman/display_doc.php?docid=763&group_id=1">Unix</a>,
<a
href="http://sourceforge.net/docman/display_doc.php?docid=766&group_id=1">Microsoft Windows</a> und <a
href="http://sourceforge.net/docman/display_doc.php?docid=2973&group_id=1">MacOS
(prior to OS X)</a> Systeme.

<hr width="80%">

<p>
<b>Detailliertere Informationen:</b>

</p>

<p>Audacity benutzt mehrere libraries die von "Drittanbietern" stammen. Einge dieser libraries benötigen
Änderungen um auf allen Systemen zu laufen. Deswegen haben wir ein Extraverzeichnis all dieser
librariers eingerichtet im CVS. 
</p>
<p>Wir haben zwei Verzeichnisse: 'audacity-src', dieser enthält allen Code den wir geschrieben haben und
'lib-src,' dieser enthält den Code der libraries.<br>
Um garantieren zu können, dass Audacity und diese libraries miteinander arbeiten können, benutzte bitte die
libraries die in dem Ordner'lib-src' liegen. In Unix Systemen kann man diese libraries auch mit vorhandenen libraries 
auf dem eigenen System kompilieren.
Schreibe 'configure --help' um die Optionen zu sehen.
</p>
<p>
Wenn du den kompletten Source-Code haben möchtest, solltest du das Modul
'audacity' mit CVS laden, dieses enthält als Unterordner auch das lib-src Verzeichnis.
</td>
</tr>
</table>
</p>

<?php BoxBottom(); ?>
