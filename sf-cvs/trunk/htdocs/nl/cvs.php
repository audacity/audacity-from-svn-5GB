<?php BoxTop("CVS"); ?>

(opmerking: deze Nederlandse vertaling zou fouten kunnen bevatten gezien mijn geringe (geen) ervaring met CVS)
We gebruiken <a href="http://www.cvshome.org">CVS</a>, de Concurrent Versions
System, om de gebruikers te helpen
Audacity samen te ontwikkelen. Klik
<a
href="http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/audacity/audacity-src/">hier</a> om de broncode te bekijken in onze CVS opslagplaats.

<p>
<h3>Opmerking: Versie 1.2 heeft nu een broncode:</h3>
Als je de stabiele broncode van Audacity wilt gebruiken, versie 1.2.x, dien je nu in te typen <tt>-r AUDACITY_1_2</tt> als je een cvs update of checkout uitvoert. Anders is het mogelijk dat je de CVS HEAD verkrijgt die mogelijk behoorlijk instabiel kan zijn omdat we begonnen zijn met versie 1.3.0.

<p><h3>Snelle Audacity CVS Hoe te:</h3>
  Als je toegang wilt krijgen tot de Audacity broncode, kun je
  een cvs client gebruiken om een cvs branche naar je computer
  te downloaden. Als je eenmaal een branche hebt onderzocht, is je CVS
  client software in staat om je te helpen jouw versie
  up to date te houden met de andere Audacity ontwikkelaars. Volg de
  instructies hieronder om de broncode te verkrijgen.


<h4>Anonieme CVS toegang met een command-line cvs client:</h4>
<p>Tik het volgende in op de command line (merk op dat het een 
enkele zin is zonder entertoets aanslagen):<br>
<ul><tt>cvs -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity
login </tt><br></ul>
en druk de entertoets in als om een wachtwoord wordt gevraagd.

<p>Dan, <b>om de laatste branch te verkrijgen(1.3.0)</b> (als een enkele zin):
<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co
audacity</tt></ul>
of <b>voor de stabiele branch (1.2.0)</b> (als een enkele zin):<br>
   <ul><tt>cvs -z3 -
of <b>voor de oude 1.0 branch (1.0.0)</b> (als een enkele zin):<br>
   <ul><tt>cvs -z3 -
d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co -r audacity-0_9-branch audacity-old</tt></ul>

<p> Als alternatief kun je je <tt>CVSROOT</tt> omgeving
aanapssen naar
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
(zie hieronder).<br>
Dan, <b>om de laatste instabiele branch te verkrijgen (1.3.0)</b>, toets in<br> 
   <ul><tt>cvs checkout audacity</tt></ul>
Of, <b>voor de stabiele branch (1.2)</b>, toets in <br>
   <ul><tt>cvs checkout -r AUDACITY_1_2 audacity</tt></ul>
of <b>voor de oude 1.0 branch<b/>, toets in <br>
   <ul><tt>cvs checkout -r audacity-0_9-branch audacity-old</tt></ul>

<p> Bij het instellen van de  <tt>CVSROOT</tt> omgevings variabel, kun
je je command shell's bronbestand gebruiken, of een van de volgende
commando's:
<h5>In bash of bourne shell, als een enkele zin:</h5>
<ul><tt>export
CVSROOT=:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>
<h5>In csh of een gelijke daarvan, in een enkele zin</h5>
<ul><tt>setenv CVSROOT
:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>

<hr width="80%">

<h4>Anonieme CVS toegang met een grafische client</h4>

Voor een grafische client zoals wincvs, maccvs, of gcvs,
(beschikbaar op <a href="http://cvsgui.org">cvsgui.org</a>) moet je
de 
<tt>CVSROOT</tt> variabel (in het Admin|voorkeuren submenu) instellen als 
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
en selecteer "pserver" of "Password" authenticatie. Dan, moet je je,
onder de Globals tab van de Voorkeurs dialoog, ervan vergewissen
dat je "Checkout read-only (CVSREAD)" niet hebt afgevinkt.  Dan,
login kiezen, en de entertoets indrukken voor het wachtwoord (het is
""). Tenslotte, kies "Create|Checkout module", kies een
locatie waar je de checked-out branch wilt plaatsen, en
volg de instructies hieronder afhankelijk van welke branch dat je wilt.
Als je een foutmelding krijgt, vergewis je er dan van dat je de <tt>CVSROOT</tt>
variabel aan het einde geen spaties bevat--wat
zou kunnen gebeuren als je de variabellen rechtstreeks van deze webpagina hebt gekopieerd.

<p>
<b>Om de laatste instabiele branch te verkrijgen (1.3.0):</b><br>
In de "Checkout Settings" dialoog, toets in
<tt>audacity</tt> als module naam. Druk op de "OK" knop en de
branch zal automatisch naar je computer worden gedownload.
<p>
<b>Om de stabiele (1.2.0) branch te verkrijgen: </b><br>
In de "Checkout Settings" dialoog, toets in
<tt>audacity</tt> als de module naam.  Dan, in de
"Sticky options" tab, check het "Retrieve rev./tag/branch
(-r)" vakje en toets in <tt>AUDACITY-1_2</tt> in het
vakje ernaast. Druk op "OK" en de branch zal automatisch naar je computer
worden gedownload.
<p>
<b>Om de oude vresie 1.0 branch te krijgen: </b><br>
In de "Checkout Settings" dialoog, toets in
<tt>audacity-old</tt> als de module naam.  Dan, in de
"Sticky options" tab, check het "Retrieve rev./tag/branch
(-r)" vakje en toets in <tt>audacity-0_9-branch</tt> in het
vakje ernaast. Druk op "OK" en de branch zal automatisch naar je computer
worden gedownload.

<hr width="80%">
Nieuw met CVS?  Begin met het lezen van Jim Blandy's <a href="">Introductie
met
CVS</a>, Bob Arnson's <a
href="http://www.cvshome.org/new_users.html">CVS voor nieuwe
gebruikers</a>, of bezoek de cvs webpagina bij <a
href="http://www.cvshome.org/">www.cvshome.org</a>.
Meer gedetailleerde informatie is beschikbaar in de GPLed hoofdstukken van Karl
Fogel's
<a href="http://cvsbook.red-bean.com/cvsbook.html">CVS Boek
op cvsbook.red-bean.com</a>, of het "Officiële" <a
href="http://www.cvshome.org/docs/manual">Per
 Cederqvist handboek</a>.


<p> Voor specifieke hulp bij CVS op sourceforge.net, probeer dan de
sourceforge documentatie voor
<a
href="http://sourceforge.net/docman/display_doc.php?docid=763&group_id=1">Unix</a>,
<a
href="http://sourceforge.net/docman/display_doc.php?docid=766&group_id=1">Microsoft Windows</a>, en <a
href="http://sourceforge.net/docman/display_doc.php?docid=2973&group_id=1">MacOS
(voorafgaand aan OS X)</a> systemen.

<hr width="80%">

<p>
<b>Meer details:</b>

</p>

<p>Audacity gebruikt vele andere bibliotheken. Vele van deze dienen te worden getweaked om gebruikt te kunnen worden op de systemen die wij beogen. Om die reden, hebben we een lokale opslagruimte van alle andere bibliotheek broncodes met CVS. Zo werkt het:
</p>
<p>Er zijn twee opslagruimten: 'audacity-src', welke alle codes bevat
die we hebben geschreven, en
'lib-src,' welke de broncode bevat van alle
bibliotheken die we gebruiken.
Om samenwerking met andere systemen te kunnen garanderen tussen Audacity en
onze bibliotheken, raden wij aan dat je de versies van de bibliotheek gebruikt
van de 'lib-src'.  Echter, op een Unix systeem kun je de samenwerking van sommige bibliotheken vermijden door de bibliotheken te gebruiken die je al op je systeem hebt staan.
Type 'configure --help' om de opties te bekijken.
</p>
<p>
Dus, als je alles wilt bekijken, inclusief de bibliotheek broncode,
bekijk dan de module 'audacity' welke audacity-src zal tonen maar ook
de lib-src opslagruimte kun je gebruiken als een subdirectory van 'audacity'.
</td>
</tr>
</table>
</p>

<?php BoxBottom(); ?>