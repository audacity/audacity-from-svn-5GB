<?php BoxTop("Veel en vaak gestelde vragen"); ?>

<!--
<table width=100% cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+3">Veel en vaak gestelde vragen</font>
</td></tr></table>
-->

<p>

Algemeen:<br>
<a href="#g1">Is Audacity echt gratis?  Waarom?</a><br>
<a href="#g2">Wie ontwikkelt Audacity?</a><br>
<a href="#g3">Hoe kan ik helpen om Audacity te verbeteren?</a><br>
<a href="#g4">Ik heb een fout gevonden! Wat nu?</a><br>

<p>

Installatie:<br>

<a href="#i1">Windows: Wat moet ik met het .exe bestand doen?</a><br>
<a href="#i2">Windows: Hoe deïnstalleer ik Audacity?</a><br>
<a href="#i3">Mac: Wat moet ik met het .sit bestand doen?</a><br>
<a href="#i4">Unix: Waarom krijg ik failed dependencies als ik vanaf
              de RPM installeer?</a><br>
<a href="#i5">Unix: Waarom krijg ik een foutmelding tijdens het laden van
              de gedeelde bibliotheek libwx_gtk.so?</a><br>

<p>

Het gebruik van Audacity:<br>

<a href="#a1">Hoe mix ik twee tracks samen?</a><br>
<a href="#a2">Kan ik het vocale gedeelte van een opname verwijderen?</a><br>

<a href="#a3">When I try to record harmonies with myself,
              why are the two tracks out of sync?</a><br>
<a href="#a4">Waarom zit er geen MP3 encoder in Audacity?</a><br>
<a href="#a5">Kan ik RealAudio of andere streaming audio opnemen?</a><br>
<a href="#a6">Help! Ik klik op Opnemen maar ik krijg alleen maar   
              stilte</a><br>
<a href="#a7">Hoe kan ik een enkel bestand opsplitsen in meervoudige
              tracks?</a><br>
<a href="#a8">Hoe haal ik de liedjes van mijn audio CD af?</a><br>

<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Algemeen</font>
</td></tr></table>

<p>

<a name="g1"></a>
<font size=+1><b>
1. Is Audacity echt gratis? Waarom?<br>

</b></font>

<p>

Ja, in bijna alle hoedanigheden.  Audacity wordt niet alleen gratis gedistribueerd, maar je bent zelfs vrij om er mee te doen wat je wilt.
<p>
Audacity wordt gedistribueerd overeenkomstig de
<a href="http://www.gnu.org/copyleft/gpl.html"
>General Public License (GPL)</a>.
Het is toegestaan om dit programma voor persoonlijke en commercieele doeleinden te gebruiken.  Je mag het zelfs weggeven of verkopen.
De broncode van het programma is vrijelijk verkrijgbaar via internet en het staat je vrij om het voor eigen gebruik te modificeren, echter elke verandering die je aanbrengt moet, overeenkomstig de GPL, gedistribueerd worden.
<p>
Audacity is gebouwd met gebruik van wxWidgets, een software bibliotheek welke is vrijgegeven via de minder beperkende LGPL.  
Voor meer informatie, bezoek dan a.u.b.
<a href="http://www.wxwidgets.org">wxwidgets.org</a>.
<p>
De auteurs van Audacity hebben besloten het onder de GPL vrij te geven in verband met diverse redenen.  Sommigen van ons doen het uit vrijgevigheid.
Sommige van ons doen het om morele redenen omdat wij van mening zijn dat alle software gratis zou moeten zijn, terwijl anderen onder ons vinden dat er op de wereld behoefte is aan zowel gratis als commercieele software.
<p>
Een van de redenen waarom Audacity gratis is, is dat het daardoor populairder wordt. Velen van ons hebben liever dat miljoenen mensen plezier hebben aan het gratis gebruik van Audacity dan dat duizenden mensen ons zouden moeten betalen.
De meeste gebruikers zijn vriendelijker als iets gratis is.

<p>
Wat ook een reden is, is dat dit samenwerking bevorderd.  Als Audacity
shareware zou zijn geweest, zou het ondenkbaar zijn dat vele mensen over de hele wereld zouden helpen met de code, fouten te herstellen, opzetten van documentatie,vertalingen en het grafische gedeelte.

<p>
<a name="g2"></a>
<font size=+1><b>
2. Wie ontwikkelt Audacity?<br>
</b></font>

<p>

De ontwikkeling van Audacity is begonnen in de herfst van 1999
door Dominic Mazzoni op het moment dat hij een student was op de
Carnegie Mellon Universiteit in Pittsburgh, PA, USA.
Hij werkte samen met zijn adviseur, Professor Roger Dannenberg, aan een onderzoeksproject en ze hadden een hulpmiddel nodig voor visualisatie van 
audio analysis algoritmen.  Langzamerhand ontwikkelde dit programma zich tot een algemeen audiobewerkingsprogramma en andere mensen begonnen mee te helpen.
<p>
Vandaag de dag wordt Audacity ontwikkeld met gebruik van Sourceforge, een online site dat mensen over de hele werled de mogelijkheid biedt om samen te werken aan gratis sotware projecten. Zie <a href="http://www.sourceforge.net"
>sourceforge.net</a> voor meer informatie.
Vele mensen hebben meegeholpen aan Audacity, en de vooruitgang hiervan blijft constant versnellen.

<p>

<a name="g3"></a>
<font size=+1><b>
3. Hoe kan ik helpen met het verbeteren van Audacity?<br>
</b></font>

Vindt fouten en laat ons dat weten.
Schrijfcode.  Vertaal het in je eigen taal.  Maak grafische bestanden.
Meldt je aan bij de Audacity-Gebruikers mailinglijst.  Doneer geld.
<p>
A.u.b. bekijk eens onze nieuwe <?php print "<a href=donatetime.php?$langLinkStr>"; ?>Donatie</a>
pagina voor meer informatie over hoe je kunt helpen.
<p>

<a name="g4"></a>
<font size=+1><b>
4. Ik heb een fout gevonden!  Wat nu?<br>
</b></font>
<p>

Het belangrijkste bij het rapporteren van een fout is, dat je deze zo specifiek mogelijk meldt.  Geef ons alle informatie, zodat wij de fout zelf kunnen reproduceren, anders is het voor ons onmogelijk om de fout te herstellen.
<p>
Het is belangrijk dat je ons ook mededeelt welk besturingsprogramma je gebruikt(zoals Windows 98, MacOS 9.1, RedHat Linux 7.1, etc.)
en alle andere informatie over je computer waarvan je denkt dat dit wel eens relevant kan zijn.
<p>
Dan, kun je zelf de fout oproepen?  Als deze regelmatig voorkomt,
meldt ons dan de exacte volgorde van handelingen die je deed totdat de foutmelding ontstond.  Als je dan de foutmelding krijgt, is het belangrijk dat je ons de precieze tekst van die foutmelding rapporteert.
<p>

We willen alle fouten uitbannen!  Bedankt dat je de tijd nam om ons te helpen de fouten te vinden.

<p>
<br>
<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Installatie</font>
</td></tr></table>

<p>
<a name="i1"></a>
<font size=+1><b>
1. Windows: Wat moet ik met het .exe bestand doen?<br>
</b></font>

<p>Vanuit je browser kun je bij het downloaden kiezen voor het installeren en opslaan van het bestand. Als je gekozen hebt voor het direct installeren, zal, als de download gereed is, het Audacity setup programma opstarten, welke de installatie van de Audacity programma bestanden zal uitvoeren en een koppeling in je Startmenu zal plaatsen.
<p>
Als je het bestand heb gedownload met de optie Opslaan, hoef je, als de download gereed is, alleen maar te dubbelklikken op het bestand om daarmee de installatie uit te voeren.
<p>
Als je voorkeur uitgaat naar het niet gebruiken van het installatieprogramma, kun je Audacity ook als een ZIP bestand downloaden, die je dan kunt uitpakken met behulp van WinZip.

<p>
<a name="i2"></a>
<font size=+1><b>
2. Windows: Hoe deïnstalleer ik Audacity?<br>
</b></font>

<p><font size=+0>Audacity kan gedeïnstalleerd worden door in het configuratiescherm te kiezen voor Software en daar in de lijst "Audacity" te selecteren. Klik op de knop 'Verwijderen" en de deïnstallatie van Audacity zal uitgevoerd worden.

<p>
<a name="i3"></a> <font size=+1><b> 3. Mac: Wat moet ik met het .sit bestand doen<br> </b></font>

<p>
Je hebt een recente versie van StuffIt Expander nodig.  StuffIt Expander
wordt geleverd met alle Macintoshes en is normaal gesproken als standaard geconfigureerd in alle Mac web browsers.  Als Audacity niet automatisch wordt uitgepakt, sleep dan "audacity.sit" naar StuffIt Expander om het uitpakken te laten beginnen.
<p>
Installeren van Audacity is niet nodig.  Je hoeft alleen maar de Audacity
Map naar je Programma's map te slepen of waar je het dan ook wilt plaatsen.  Voor het deïnstalleren: Sleep de gehele folder naar de prullenbak.

<p>
<a name="i4"></a>
<font size=+1><b>
4. Unix: Waarom krijg ik failed dependencies als ik vanaf de RPM installeer?
<br></b></font>

<p><font size=+0>Allereerst moet je je ervan vergewissen dat de wxGTK geïnstalleerd is; een link kun je vinden op onze Linux pagina
- http://audacity.sourceforge.net/linux.html. Als de RPM blijft weigeren te installeren, is dit een onoplosbare situatie.  Installeer dan de RPM met gebruik van de flag --nodeps.</font>

<p>
<a name="i5"></a>
<font size=+1><b>

5. Unix: Waarom krijg ik een foutmelding bij het laden van de gedeelde bibliotheek libwx_gtk.so?<br></b></font>

<p><font size=+0>Dit zou een van twee problemen kunnen zijn:
<br>- Als je de wxWindows RPM hebt geïnstalleerd, voer dan "ldconfig" (als
root) uit zodat je systeem weet dat een nieuwe bibliotheek aanwezig is.

<br><br>- Als je wxWindows hebt geïnstalleerd
van source, is het waarschijnlijk geïnstallerd in /usr/local, maar /usr/local/lib is misschien
niet in je bibliotheek pad. voeg /usr/local/lib toe aan het bestand  "/etc/ld.so.conf"
en voer dan "ldconfig" uit. Het zou kunnen dat je ook "/usr/local/bin" aan je pad moet toevoegen, zodat programma's de "wx-config" utility kunnen vinden.<br>

<p>
<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Gebruik van Audacity</font>
</td></tr></table>

<p>
<a name="a1"></a>
<font size=+1><b>

1. Hoe mix ik twee tracks samen?
</b></font>
<p>

Audacity mixt automatisch.  Het enige wat je hoeft te doen is twee tracks in één project te importeren, wat je kunt doen door het "Import Audio" commando in het Project menu te gebruiken, of door eenvoudig de twee audio bestanden naar 
Audacity te slepen.  Tevens zal Audacity, als je de Opname knop indrukt, automatisch een nieuwe track genereren.

<p>

Om je nieuwe mix op te slaan, kun je zowel het project Exporteren, waardoor alle tracks automatisch worden gemixt, of je kunt je tracks selecteren en het
"Quick Mix" commando in het Project menu gebruiken.

<p>
<a name="a2"></a>
<font size=+1><b>
2. Kan ik het vocale gedeelte in een opname verwijderen?
</b></font>
<p>

In sommige stereo opnames kun je het vocale gedeelte verwijderen vanwege de manier van mixen van de opname in de studio.
Mestal staat het vocale gedeelte precies in het midden van de opname, terwijl alle andere instrumenten iets uit het midden zijn geplaatst.
Als je het rechterkanaal scheidt van het linkerkanaal, zal het vocale gedeelte volledig uitgeschakeld zijn en blijven de andere instrumenten over.
<p>

<b>Dit werkt niet bij alle opnames!</b>
<p>
Om dit uit te proberen in Audacity, importeer je een stereo opname, klik dan op track pop-up menu (het kleine naar beneden wijzende pijltje naast de naam van de track) en selecteer "Splits Stereo Track".
Selecteer nu de onderste track (het rechterkanaal) en gebruik het
"Invert" effect (van het Effecten menu).  Tenslotte gebruik je
track pop-up menus om van beide kanalen mono kanalen te maken,
en mix ze dan samen met behulp van Quick Mix.  Als je geluk hebt zal het vocale gedeelte zijn verdwenen.

<p>
<a name="a3"></a>
<font size=+1><b>
3. When I try to record harmonies with myself,
              why are the two tracks out of sync?
</b></font>
<p>
Dit is normaal en dit komt doordat er een kleine vertraging zit tussen het starten van het geluid in Audacity en de tijd die nodig is voor het geluid om je speakers te bereiken.  Audacity probeert deze vertraging niet automatisch te corrigeren dus zul je dit handmatig moeten doen.
<p>
Om deze vertraging te corrigeren kun je de Time Shift tool gebruiken om een van de tracks op te schuiven totdat ze op dezelfde lijn zitten.  Merk op dat je deze tool kunt gebruiken tijdens het beluisteren van de opname.

<p>

<a name="a4"></a>
<font size=+1><b>
4. Waarom zit er geen MP3 encoder in Audacity?
</b></font>
<p>

Helaas is de algoritme voor het coderen en maken van MP3 bestanden gepatenteerd, en <b>Audacity</b> kan niet worden voorzien van een MP3 exporteer algoritme zonder dat daar een betaling tegenover staat of dat daardoor een wetgeving in vele landen wordt overtreden.
<p>
Als compromis, is Audacity in staat gebruik te maken van andere MP3 encoders die je afzonderlijk zult moeten downloaden.
Het is aan jou om je te houden aan de licensievoorwaarden welke opgelegd zijn door MP3 encoders.
<p>
Voor meer informatie, bekijk de Exporting MP3 bestanden in onze Online Help.

<p>

<a name="a5"></a>
<font size=+1><b>
5. Kan ik RealAudio of andere streaming audio opnemen?
</b></font>
</p><p>

Niet automatisch.  De meeste streaming audio, waaronder de meeste muziek die geleverd wordt aan de RealOne Player, Windows Media Player, en Quicktime, is specifiek ontwikkeld om het opnemen daarvan tegen te gaan.
</p><p>
Toch is het bijna altijd mogelijk om alles wat je computer kan afspelen ook op te nemen, door de stekker die in de uitgang sound-out van je geluidskaart zit door te lussen naar de ingang sound-in/microphone van je geluidskaart.
</p><p>
Gebruik een 1/8 inch stereo miniplug op een 1/8 inch stereo mini kabel.  Steek die plug in je sound-out van je geluidskaart - de uitgang waarop je anders je speakers of koptelefoon hebt aangesloten (de ingang is meestal groen van kleur). Steek het andere einde in de uitgang van de microphone of line-in (de ingang is meestal rood van kleur).
</p><p>
Daarna kan Audacity alles wat je computer kan afspelen, opnemen.
Druk op de Opnameknop in Audacity, en druk dan op de Playknop in je streaming audio programma.
</p><p>

<a name="a6"></a>
<font size=+1><b>
6. Help! Ik druk op Opname maar ik krijg alleen maar stilte!
</b></font>
</p><p>
Audacity neemt op vanaf de standaard inputbron welke door je besturingssysteem is geselecteerd.  Als je meerdere inputbronnen hebt (zoals een ingebouwde microfoon, een externe microfoon, line-in uitgang, of een audio CD) moet je één van deze selecteren als opnamebron. Als je bijvoorbeeld van een microfoon wilt opnemen, maar je inputbron staat ingesteld op "line-in", zal Audacity alleen stilte opnemen.
</p><p>
Als je Windows gebruikt, kun je de opnamebron selecteren door met de rechtermuisknop te klikken op het luidspreker icoontje aan de rechter onderzijde in de taakbalk van je scherm.
</p><p>
Als je Mac OS 9 gebruikt, gebruik dan het Geluids controle paneel.
In Mac OS X, gebruik het Geluidspaneel in de Systeem Voorkeuren (er is een Input tab als je versie 10.2 of hoger hebt).
</p><p>
In Linux, gebruik een mixer zoals "xmixer" of "kmix".
</p><p>

<a name="a7"></a>
<font size=+1><b>
7. Hoe kan ik een enkel bestand splitsen in meervoudige tracks?
</b></font>
</p><p>
Soms kan het voorkomen dat je een opname hebt van meervoudige songs welke in een enkel bestand zijn opgenomen, bijvoorbeeld een LP ineens opgenomen. Als je deze songs als afzonderlijke tracks op CD wilt branden, zul je een afzonderlijk bestand van elke song moeten maken.
</p><p>
Er zijn in Audacity vele manieren om een opname in meervoudige tracks te splitsen. Hier zijn een paar manieren die je kunt proberen:
</p><p>
Methode 1: Export Selectie
</p><ul>
<li>Selecteer het audiogedeelte dat overeenkomt met de eerste track.
</li><li>Kies "Exporteer Selectie als WAV" van het Bestandsmenu, en sla het op  
    schijf op.
</li><li>Doe hetzelfde met de overgebleven tracks.
</li></ul>
<p>
Methode 2: Verwijder en Undo
</p><ul>
<li>Verwijder alles <i>behalve</i> van de eerste track.
</li><li>Kies "Exporteer als WAV" van het Bestandsmenu, en sla de eerste track 
    op schijf op.
</li><li>Selecteer "Undo" van het Edit menu. Blijf undo-ing totdat je de rest 
    van je audio bestand terug gekregen hebt.(Audacity heeft ongelimiteerde  
    undo.)
</li><li>Doe dit met alle overgebleven tracks.
</li></ul>
<p>
Methode 3: Splitsen en Exporteren
</p><ul>
<li>Selecteer wat je als eerste track zou willen hebben.
</li><li>Kies "Splitsen" in het Edit menu, waardoor de slectie als een 
    afzonderlijke track in Audacity wordt geplaatst.
</li><li>Blijf dit doen totdat alle songs afzonderlijke tracks zijn geworden.
    Vergeet niet dat je wanneer je maar wilt Undo kunt gebruiken als je een 
    fout hebt gemaakt.
</li><li>Nu teruggaan en "Exporter Selectie als WAV" gebruiken om elke track 
    naar een afzonderlijk bestand te exporteren.  Om een track snel te kunnen 
    selecteren kun je op het label ervan (overal in het gebied links van de 
    wavevorm, onder de titel) klikken.
</li></ul>
<p>

<a name="a8"></a>
<font size=+1><b>
8. Hoe krijg ik liedjes van een audio CD?
</b></font>
</p><p>
Audacity kan geen liedjes van een audio CD afhalen.  Daarvoor heb je een zogenaamd "rip-programma" ('Ripper') nodig.
</p><p>
Voor Windows adviseren we <a href="http://cdexos.sourceforge.net/">CDex</a>.
</p><p>
Voor Mac OS (9 and X), adviseren we Apple's <a href="http://www.apple.com/itunes/">iTunes</a>.
</p><p>
Voor Linux, probeer <a href="http://www.xiph.org/paranoia/">Paranoia</a>.

<?php BoxBottom(); ?>
