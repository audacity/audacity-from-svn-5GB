<?php BoxTop("Ofte stillede spørgsmål (FAQ)"); ?>

<!--
<table width=100% cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+3">Ofte stillede spørgsmål (FAQ)</font>
</td></tr></table>
-->

<p>

General:<br>
<a href="#g1">Er Audacity virkelig gratis?  Hvorfor?</a><br>
<a href="#g2">Hvem skrev Audacity?</a><br>
<a href="#g3">Hvordan kan jeg hjælpe med at forbedre Audacity?</a><br>
<a href="#g4">Jeg fandt en fejl. Hvad nu?</a><br>
<p>
Installation:<br>

<a href="#i1">Windows: Hvad gør jeg med .exe filen?</a><br>
<a href="#i2">Windows: Hvordan afinstallerer jeg Audacity?</a><br>
<a href="#i3">Mac: Hvad gør jeg med .sit filen?</a><br>
<a href="#i4">Unix: Hvorfor får jeg ikke-tilfredsstillede afhængigheder, når jeg
installerer fra RPM-filen?</a><br>
<a href="#i5">Unix: Hvorfor får jeg en fejl ved indlæsning af det delte
              bibliotek libwx_gtk.so?</a><br>
<p>
Brug af Audacity:<br>
<a href="#a1">Hvordan mixer jeg to spor?</a><br>
<a href="#a2">Kan jeg fjerne vokalerne fra en indspilning?</a><br>

<a href="#a3">Når jeg prøver at indspille harmonier med mig selv,
              hvorfor taber de to spor da synkroniseringen?</a><br>
<a href="#a4">Hvorfor er der ikke en mp3-koder indbygget i Audacity?</a><br>
<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Alment</font>
</td></tr></table>

<p>

<a name="g1"></a>
<font size=+1><b>
1. Er Audacity virkelig gratis?  Hvorfor?<br>

</b></font>

<p>

Ja, ikke bare gratis, også fri. Audacity er gratis at kopiere,
og du kan gøre stort set hvad du vil med programmet.
<p>
Audacity er udgivet under betingelserne i
<a href="http://www.gnu.org/copyleft/gpl.html"
>General Public License (GPL)</a>.
Du kan bruge programmet ubegrænset til personlige og kommercielle
formål. Du må give det væk, og du må sælge det. Kildeteksten til
programmet er frit tilgængelig over internettet, og du har lov til
at ændre det til dine egne formål. Alle ændringer der distribueres
skal distribueres under GPL.
<p>
Audacity er lavet ved hjælp af wxWindows, et software bibliotek som
er udgivet under den mindre restriktive LGPL. Flere oplysninger om
wxWindows kan du finde på
<a href="http://www.wxwindows.org">wxwindows.org</a>.
<p>
Der er mange grunde til at Audacitys forfattere har valgt at
udgive programmet under GPL. Nogle af os gør det af gavmildhed.
Nogle af idealistiske grunde, fordi vi synes al software bør være
fri. Nogle af os ser et behov for både kommerciel og fri software.
<p>
En hovedgrund er, at det vil gøre Audacity mere populært.
Mange af os vil hellere se en million mennesker bruge programmet
gratis end vi vil have tusinde til at betale os. Og de fleste
brugere er venligere, når de får noget gratis.
<p>
En anden grund er, at det tilskynder til samarbejde. Hvis Audacity
var shareware, ville det ikke motivere mennesker over hele verden
til at bidrage med kode, fejlrettelser, dokumentation, grafik og
oversættelser.
<p>
<a name="g2"></a>
<font size=+1><b>
2. Hvem udviklede Audacity?<br>
</b></font>

<p>

Audacity blev påbegyndt i efteråret 1999
af Dominic Mazzoni, der da studerede på Carnegie Mellon University 
i Pittsburgh, PA, USA.
Han arbejde på et projekt med sin rådgiver Professor Roger 
Dannenberg, og de havde brug for et værktøj der kunne hjælpe dem
med at visualisere audio analyse algoritmer. Hen ad vejen blev dette
program til et alment lydredigeringsprogram, og andre begyndte at 
give en hånd med.
<p>
I dag udvikles Audacity ved hjælp af SourceForge, en online
service der tillader folk fra hele verden at arbejde sammen 
på fri software. Se <a href="http://www.sourceforge.net"
>sourceforge.net</a> for flere oplysninger.
Snesevis af personer har bidraget til Audacity, og 
stadig flere kommer til.

<p>

<a name="g3"></a>
<font size=+1><b>
3. Hvordan kan jeg hjælpe med at forbedre Audacity?<br>
</b></font>

Find fejl - og fortæl os om dem.
Skriv kode. Oversæt Audacity til dit sprog. Lav grafik.
Vær med på vores postlister. Send os penge.
<p>
Se den nye side<?php print "<a href=donatetime.php?$langLinkStr>"; ?>Bidrag</a>
for at se, hvordan du kan give en hånd med.
<p>

<a name="g4"></a>
<font size=+1><b>
4. Jeg fandt en fejl!  Hvad nu?<br>
</b></font>
<p>

Det vigtigste, når du rapporterer en fejl, er at være så
nøjagtig som muligt. Specielt er det essentielt at give os
nok oplysninger til at vi kan reproducere fejlen selv,
ellers er det ret usandsynligt, at vi vil være i stand
til at rette den.
<p>
Det er vigtigt at fortælle os, hvilket operativsystem, 
du bruger (Windows 98, MacOS 9.1, RedHat Linux 7.1 etc.)
og andre oplysninger om din computer, du mener har en
eller anden form for betydning.
<p>
Og dernæst, kan du gentage fejlen? Hvis den optræder systematisk
er det vigtigt at fortælle nøjagtigt hvilken sekvens af 
operationer, der udløser fejlen. Hvis du får en fejlmelding
er det vigtigt at sende den nøjagtige tekst i denne.
<p>

Vi vil udrydde alle fejl! Vi takker på forhånd for indsatsen
med at finde dem.

<p>
<br>
<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Installation</font>
</td></tr></table>

<p>
<a name="i1"></a>
<font size=+1><b>
1. Windows: Hvad stiller jeg op med .exe-filen?<br>
</b></font>

<p>From the browser, select the option to "Run from current
location".  This launches the Audacity setup program, which will install the
Audacity program files and create an entry in the start menu.
<p>
If you already downloaded the file, just double-click on it to
start the installer.
<p>
If you prefer not to use the Installer program, you can also
download Audacity as a ZIP file, which you can decompress
using WinZip.

<p>
<a name="i2"></a>
<font size=+1><b>
2. Windows: How do I uninstall Audacity?<br>
</b></font>

<p><font size=+0>Audacity can be uninstalled by opening
Add/Remove Programs in
the Control Panel.  Select "Audacity" from the list and
click the button
"Add/Remove".
This launches the Audacity uninstall program.

<p>
<a name="i3"></a> <font size=+1><b> 3. Mac: What do I do with
the .sit file?<br> </b></font>

<p>
You need a recent version of StuffIt Expander.  StuffIt Expander
comes with all Macintoshes and is usually configured by default
with all Mac web browsers.  If Audacity does not decompress
automatically, drag "audacity.sit" to StuffIt Expander to decompress
it.
<p>
There is no need to install Audacity.  Just drag the Audacity
folder to your Applications folder, or wherever else you would
like to put it.  To uninstall, just drag the entire folder to
the trash.

<p>
<a name="i4"></a>
<font size=+1><b>
4. Unix: Why do I get failed dependencies when installing from the RPM?
<br></b></font>

<p><font size=+0>First, ensure that the wxGTK is installed; a link can be found
from our Linux page - http://audacity.sourceforge.net/linux.html. If the RPM
still fails to install, this is an unresolved issue.  Simply install the RPM
using the flag --nodeps.</font>

<p>
<a name="i5"></a>
<font size=+1><b>

5. Unix: Why do I get an error while loading
	the shared library libwx_gtk.so?<br></b></font>

<p><font size=+0>This could be one of two problems:
<br>- If you installed the wxWindows RPM, be sure to run "ldconfig" (as
root) so your system knows about the new library.

<br><br>- If you installed wxWindows
from source, it may have been installed in /usr/local, but /usr/local/lib may
not be in your library path. Add /usr/local/lib to the file "/etc/ld.so.conf"
and then run "ldconfig". You may also want to add "/usr/local/bin" to your path,
so that programs can find the "wx-config" utility.<br>

<p>
<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Using Audacity</font>
</td></tr></table>

<p>
<a name="a1"></a>
<font size=+1><b>

1. How do I mix two tracks together?
</b></font>
<p>

Audacity mixes automatically.  All you have to do is
import two tracks into the same project, which you can
do using the "Import Audio" command in the Project
menu, or simply by dragging the two audio files to
Audacity.  When you press the record button, Audacity
generates a new track automatically, also.

<p>

In order to save your mix, you can either Export the
project, which will automatically mix all of your
tracks together, or you can select your tracks and
use the "Quick Mix" command in the Project menu.

<p>
<a name="a2"></a>
<font size=+1><b>
2. Can I remove the vocals from a recording?
</b></font>
<p>

With some stereo recordings, it is possible to remove the vocals
because of the way in which the recording was mixed at the studio.
Often, the vocals are placed in the exact center of the recording,
while all other instruments are slightly off-center.  If you
subtract the right channel from the left channel, the vocals get
completely canceled out, leaving only the other instruments.
<p>

<b>This only works on some recordings!</b>
<p>
To attempt this in Audacity, import a stereo recording, then
click on the track pop-up menu (the little down-arrow next to
the name of the track) and select "Split Stereo Track".
Now select the lower track (the right channel) and use the
"Invert" effect (from the Effect menu).  Finally, use the
track pop-up menus to make both channels Mono channels,
and then mix them together using Quick Mix.  If you're lucky,
the vocals will be gone.

<p>
<a name="a3"></a>
<font size=+1><b>
3. When I try to record harmonies with myself,
              why are the two tracks out of sync?
</b></font>
<p>
This is normal and it happens because there is a small delay
between when Audacity starts playing sound and when it actually
reaches your speaker.  Audacity does not automatically try to
correct for this delay, and you must do it manually.
<p>
To correct this delay, use the
Time Shift tool to slide
one of the tracks over until they line up.  Note that you can
use the tool while you are listening to the recording.

<p>

<a name="a4"></a>
<font size=+1><b>
4. Hvorfor er der ikke en mp3-koder indbygget i Audacity?
</b></font>
<p>

Desværre er der patent på algoritmen til at kode og lave
mp3-filer, og <b>Audacity</b> kan ikke have en mp3 eksport
facilitet uden enten at betale en afgift eller overtræde 
loven i mange lande.
<p>
Som et kompromis kommer Audacity med mulighed for at bruge
andre mp3-kodere, som skal downloades separat. Det er så 
op til dig at sikre dig, at du overholder licensbetingelserne
på den mp3-koder, du bruger.
<p>
Du kan finde mere information på siden 'Exporting MP3 files' 
i online hjælpesystemet.
<?php BoxBottom(); ?>
