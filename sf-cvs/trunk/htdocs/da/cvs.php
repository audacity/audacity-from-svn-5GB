<?php BoxTop("CVS"); ?>

Vi bruger <a href="http://www.cvshome.org">CVS</a>, Concurrent Versions
System, for at koordinere udviklingen af Audacity. Klik
 <a
href="http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/audacity/audacity-src/">her</a> for at kigge gennem kildeteksten direkte fra vores CVS server.

<p><h3>Hurtig start med Audacity CVS:</h3>
  Hvis du vil have fat i kildekoden til Audacity, kan du bruge en CVS klient
  til at hente en bestemt CVS-gren til din computer.
  Når du en gang har hentet grenen, vil CVS klienten kunne holde
  din version synkron med de andre Audacity udvikleres.
  Følg disse instruktioner for at få kildeteksten.


<h4>Anonym CVS adgang med kommando-linje klient:</h4>
<p>Skriv følgende på kommandolinjen (bemærk at det er en lang linje uden linjeskift):<br>
<ul><tt>cvs -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity login </tt><br></ul>
og tryk Enter når du bliver spurgt om en adgangskode.

<p>Derefter <b>får du seneste udgave (1.1.0)</b> (på en linje):
<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co
audacity</tt></ul>
eller <b>for at få den stabile udgave (0.9-1.0)</b> (på en linje):<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co -r audacity-0_9-branch audacity-old</tt></ul>

<p> Alternativ kan du sætte <tt>CVSROOT</tt> environment
variablen til
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
(se nedenfor).<br>
Så <b>kan du få den seneste udgave (1.1.0)</b> ved at indtaste<br> 
   <ul><tt>cvs checkout audacity</tt></ul>
Eller, <b>for den seneste stabile udgave(0.9-1.0)</b>, tast <br>
   <ul><tt>cvs checkout -r audacity-0_9-branch audacity-old</tt></ul>

<p> For at sætte environment variablen <tt>CVSROOT</tt>, kan du
bruge din shells resource fil, eller en af disse kommandoer:
<h5>I bash eller bourne shell, på en linje:</h5>
<ul><tt>export
CVSROOT=:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>
<h5>I csh og dens efterfølgere, på en linje:</h5>
<ul><tt>setenv CVSROOT
:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>

<hr width="80%">

<h4>Anonym CVS adgang med en grafisk klient</h4>

For at bruge en grafisk klient som wincvs, maccvs, eller gcvs
(kan fås her: <a href="http://cvsgui.org">cvsgui.org</a>), skal du sætte
<tt>CVSROOT</tt> variablen (i Admin|Preferences undermenuen) til 
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
og vælge "pserver" eller "Password" authentikering. Så, på
fanebladet "Globals" i Indstillinger-dialogboksen, skal du
sikre dig at punktet "Checkout read-only (CVSREAD)" ikke er markeret.
Dernæst vælger du 'login', og taster Enter for adgangskoden (som er tom).
Endelig vælger du "Create|Checkout module", vælger en placering for 
den kode, du henter, og følger instruktionerne herunder, afhængig af,
hvilken udgave, du vil hente.
Hvis du får en fejlmelding, skal du sikre dig at din <tt>CVSROOT</tt>
variabel ikke har overflødige mellemrum efter bogstaverne - det kan ske ved et uheld.

<p>
<b>For at få den seneste ikke-stabile udgave (1.1.0):</b><br>
I dialogboksen "Checkout Settings" indtaster du 
<tt>audacity</tt> som modulnavn. Klik "OK", og denne gren bliver 
automatisk overført til din computere.
<p>
<b>For at få den stabilee udgave (0.9-1.0): </b><br>
I dialogboksen "Checkout Settings" indtaster du
<tt>audacity-old</tt> som modulnavn. Så indstiller du under
"Sticky options" fanebladet punktet "Retrieve rev./tag/branch
(-r)" og skriver <tt>audacity-0_9-branch</tt> i boksen ved siden af
Klik "OK", og denne gren bliver nu overført til din computer.

<hr width="80%">
CVS-begynder?  Du får en god introduktion ved at læse Jim Blandy's <a href="">Introduction
to CVS</a>, Bob Arnson's <a
href="http://www.cvshome.org/new_users.html">CVS for new
users</a>, eller besøg CVS hjemmesiden på <a
href="http://www.cvshome.org/">www.cvshome.org</a>.
Du kan finde flere detaljer på de GPL-licenserede kapitler i Karl
Fogel's <a href="http://cvsbook.red-bean.com/cvsbook.html">CVS Book
på cvsbook.red-bean.com</a>, eller den "officielle" <a
href="http://www.cvshome.org/docs/manual">Per
 Cederqvist manual</a>.


<p> Søger du specifik hjælp omkring CVS på SourceForge, kan du prøve SourceForge dcokumentationen på
<a
href="http://sourceforge.net/docman/display_doc.php?docid=763&group_id=1">Unix</a>,
<a
href="http://sourceforge.net/docman/display_doc.php?docid=766&group_id=1">Microsoft Windows</a>, og <a
href="http://sourceforge.net/docman/display_doc.php?docid=2973&group_id=1">MacOS
(før OS X)</a> platforme.

<hr width="80%">

<p>
<b>Om biblioteker:</b>

</p>

<p>Audacity bruger mange tredjepartsbiblioteker. En del af dem skal tilpasses
lidt for at kunne bruges på alle de operativsystemer, Audacity kører på. Derfor
har vi en lokal kopi af alle tredjepartsbiblioteker i vort eget CVS-træ. Det
fungerer sådan:
</p>
<p>Der er to kodepuljer: 'audacity-src', som rummer al den kode, vi selv har
skrevet, og 'lib-src', som rummer kildeteksten til alle de biblioteker, vi 
bruger.
For at garantere at Audacity og bibliotekerne fungerer sammen, anbefaler vi,
at du bruger de versioner af bibliotekerne, der findes i 'lib-src'. 
På den anden side kan du undgå at compilere nogle af de biblioteker, du
allerede har i din computer.
Skriv 'configure --help' for at se de mulige indstilliger.
</p>
<p>
Så hvis du vil hente alting, inklusive kildekoden til bibliotekerne, 
skal du hente modulet 'audacity' (via CVS), som vil overføre både 
audacity-src og lib-src kodepuljerne som en undermappe under 'audacity'.
</td>
</tr>
</table>
</p>

<?php BoxBottom(); ?>
