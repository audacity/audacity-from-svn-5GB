<?php BoxTop("Ofte Spurte Spørsmål"); ?>

<!--
<table width=100% cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+3">Ofte Spurte Spørsmål</font>
</td></tr></table>
-->

<p>

General:<br>
<a href="#g1">Er Audacity virkelig gratis? Hvorfor?</a><br>
<a href="#g2">Hvem utviklet Audacity?</a><br>
<a href="#g3">Hvordan kan jeg helpe til for å forbedre Audacity?</a><br>
<a href="#g4">Jeg fant en feil! Hva nå?</a><br>

<p>

Installering:<br>

<a href="#i1">Windows: Hva gjør jeg med .exe-fila?</a><br>
<a href="#i2">Windows: Hvordan avinstallerer jeg Audacity?</a><br>
<a href="#i3">Mac: Hva gjør jeg med .sit-fila?</a><br>
<a href="#i4">Unix: Hvorfor får jeg feil med avhengigheter når jeg installerer fra RPM-pakken?</a><br>
<a href="#i5">Unix: Hvorfor får jeg en feil når jeg laster inn det delte biblioteket libwx_gtk.so?</a><br>

<p>

Bruk av Audacity:<br>

<a href="#a1">Hvordan mikser jeg to spor sammen?</a><br>
<a href="#a2">Kan jeg fjerne vokalen fra et opptak?</a><br>

<a href="#a3">Når jeg prøver å ta opp harmonier med meg selv, hvorfor er de to sporene usynkronisert?</a><br>
<a href="#a4">Hvorfor kommer ikke Audacity med en MP3-koder?</a><br>
<a href="#a5">Kan jeg ta opp RealAudio eller annen "streaming" lyd (lyd for direkteavspilling)?</a><br>
<a href="#a6">Hjelp! Jeg trykker Spill inn, men jeg får bare stillhet!</a><br>
<a href="#a7">Hvordan deler jeg èn enkelt fil inn i flere spor?</a><br>
<a href="#a8">Hvordan får jeg overført sanger fra en lyd-CD?</a><br>

<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Generelt</font>
</td></tr></table>

<p>

<a name="g1"></a>
<font size=+1><b>
1. Er Audacity virkelig gratis? Hvorfor?<br>

</b></font>

<p>

Ja, på nesten alle måter. Audacity er ikke bare distribuert kostnadsfritt, men du er også fri til å gjøre nesten hva du vil med det.
<p>
Audacity er distribuert under vilkårene til 
<a href="http://www.gnu.org/copyleft/gpl.html"
>General Public License (GPL)</a>.
Du kan kostnadsfritt bruke dette programmet for private eller kommersielle formål. Du har også lov til å gi det bort eller selge det. Kildekoden til dette programmet er fritt tilgjengelig på nettet, og du kan gjerne modifisere programmet for egen bruk, men eventuelle forandringer du gjør må også være distribuert under vilkårene til GPL.
<p>
Audacity ble bygd vha. wxWidgets, et programvarebibliotek som er gitt ut under den mindre restriktive beskyttelsen til LGPL.
For mer informasjon, vennligst se
<a href="http://wxwidgets.org/">wxwidgets.org</a>.
<p>
Forfatterne av Audacity bestemte å gi ut programmet under GPL for flere grunner. Noen av oss gjør det av generøsitet. Noen gjør det for etiske grunner, fordi vi føler at all programvare burde være gratis, mens andre av oss mener at det er nødvendig med både gratis og kommersiell programvare i verden.
<p>
En grunn til at Audacity er gratis er så det blir mer populært. Mange av oss vil heller se en million mennesker som bruker Audacity med glede uten å betale noe, enn å ha tusen mennesker betale oss. 
De fleste brukere er mer vennlige når de får noe gratis.

<p>
Enda en grunn er at det oppmuntrer til og fremmer samarbeid. Hvis Audacity var shareware så er det lite sannsynlig at dusinvis av mennesker verden rundt ville ha bidratt med kode, feilrettelser, dokumentasjon og grafikk.

<p>
<a name="g2"></a>
<font size=+1><b>
2. Hvem utviklet Audacity?<br>
</b></font>

<p>

Audacity ble startet i høsten 1999 av Dominic Mazzoni mens han var en 
student ved siste året på Carnegie Mellon University i Pittsburgh, Pennsylvania, USA.
Han jobbet med et forskningsprosjekt sammen med rådgiveren hans, Professor Roger Dannenberg, og de trengte et verktøy som ville la dem visualisere algoritmer for lydanalyse. Over tid utviklet det seg til et generelt programverktøy for analyse av lyd, og andre mennesker begynte å hjelpe til.
<p>
Idag blir Audacity utviklet vha. Sourceforge, en internettside som lar mennesker verden rundt samarbeide med frie programvareprosjekter. Se <a href="http://www.sourceforge.net"
>sourceforge.net</a> for mer informasjon.
Dusinvis av mennesker har bidratt med ressurser til Audacity, og framgangen går kontinuerlig raskere.

<p>

<a name="g3"></a>
<font size=+1><b>
3. Hvordan kan jeg helpe til for å forbedre Audacity?<br>
</b></font>

Finn feil og fortell oss om dem.
Skriv programkode. Oversett det til ditt eget språk. Tegn grafikk. Bli med på e-postlisten for Audacity-brukere. Send oss penger.
<p>
Vennligst se vår nye <?php print "<a href=donatetime.php?$langLinkStr>"; ?>Doneringsside</a> for flere detaljer om hvordan du kan hjelpe til.
<p>

<a name="g4"></a>
<font size=+1><b>
4. Jeg fant en feil! Hva nå?<br>
</b></font>
<p>

Den viktigste tingen når du rapporterer en feil er å være så spesifikk som mulig. Gi oss nok informasjon til at vi kan reprodusere feilen selv, ellers er det lite sannsynlig at vi klarer å rette den opp. Send feilrapporter til <a href="mailto:audacity-help@lists.sourceforge.net">
<audacity-help@lists.sourceforge.net></a>.
<br><font size=-1><?php print "$listPrivacyStr"; ?></font>
<p>
Husk å gi beskjed om hvilket operativsystem du kjører (som f.eks. Windows 98, MacOS 9.1, RedHat Linux 7.1, osv.) og eventuell annen informasjon du finner relevant. 
<p>
Deretter, kan du få feilen til å forekomme igjen? Hvis det skjer konsekvent, fortell oss den eksakte sekvensen av handlinger som får feilen til å forekomme. Hvis du får en feilmelding, skriv den ned og send oss nøyaktig hva det sto. 
<p>

Vi vil gjerne bli kvitt alle feilene! Tusen takk for å hjelpe oss med å finne dem.

<p>
<br>
<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Installering</font>
</td></tr></table>

<p>
<a name="i1"></a>
<font size=+1><b>
1. Windows: Hva gjør jeg med .exe-fila?<br>
</b></font>

<p>Fra nettleseren: velg "Åpne" eller "Kjør fra gjeldende plassering". Dette starter Audacity sitt oppsettprogram, som vil installere programfilene til Audacity og opprette en mappe i startmenyen. 
<p>
Har du allerede lastet ned fila trenger du bare å dobbeltklikke den for å starte installeringen. 
<p>
Hvis du foretrekker å ikke bruke installeringsprogrammet kan du laste ned Audacity som en ZIP-fil, som du kan pakke opp med WinZip.

<p>
<a name="i2"></a>
<font size=+1><b>
2. Windows: Hvordan avinstallerer jeg Audacity?<br>
</b></font>

<p><font size=+0>Audacity kan avinstalleres ved å åpne Legg til/Fjern Programmer i Kontrollpanelet. Velg "Audacity" fra lista og klikk knappen "Legg til/Fjern".
Dette starter Audacity sitt avinstalleringsprogram.

<p>
<a name="i3"></a> <font size=+1><b> 3. Mac: Hva gjør jeg med .sit-fila?<br> </b></font>

<p>
Du trenger en forholdsvis ny versjon av StuffIt Expander. StuffIt Expander kommer med all Macintosh maskiner og er vanligvis konfigurert til å være standardvalget hos alle nettlesere til Mac. Hvis Audacity ikke pakkes opp automatisk, dra "audacity.sit" til StuffIt Expander for å pakke den opp. 
<p>
Det er ikke nødvendig å installere Audacity. Bare dra Audacity-mappen til Programmer-mappen din, eller hvor enn du har lyst til å ha den. For å avinstallere, bare dra hele mappen til papirkurven. 

<p>
<a name="i4"></a>
<font size=+1><b>
4. Unix: Hvorfor får jeg feil med programavhengigheter når jeg installerer fra RPM-pakken?
<br></b></font>

<p><font size=+0>Først, sjekk at du har wxGTK installert; lenken kan du finne fra vår Linux-side - http://audacity.sourceforge.net/linux.html. Hvis RPM-en fortsatt ikke vil installere er dette pga. et fortsatt uløst problem. Bare installer RPM-en med flagget --nodeps.</font>

<p>
<a name="i5"></a>
<font size=+1><b>

5. Unix: Hvorfor får jeg en feil når jeg laster inn det delte biblioteket libwx_gtk.so?<br></b></font>

<p><font size=+0>Dette kan være ett av to problemer:
<br>- Hvis du installerte wxWindows RPM-en, husk å kjøre "ldconfig" (som
root) slik at systemet ditt vet om det nye biblioteket.

<br><br>- Hvis du installerte wxWidgets fra kildene kan det ha blitt installert i /usr/local, men det er mulig at /usr/local/lib ikke er i bibliotekstien din. Legg til stien /usr/local/lib i fila "/etc/ld.so.conf"
og kjør så "ldconfig". Det kan også være greit å legge til stien "/usr/local/bin" så andre programmer kan finne verktøyet "wx-config".<br>

<p>
<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Bruk av Audacity</font>
</td></tr></table>

<p>
<a name="a1"></a>
<font size=+1><b>

1. Hvordan mikser jeg to spor sammen?
</b></font>
<p>

Audacity mikser automatisk. Det eneste du må gjøre er å importere to spor inn i samme prosjekt, noe du kan gjøre vha. kommandoen "Importer lyd" i Prosjekt-menyen, eller ved å ganske enkelt dra og slippe to lydfiler inn i Audacity. Audacity genererer også et nytt lydspor når du trykker "Spill inn"-knappen.

<p>

For å lagre miksen din kan du enten eksportere prosjektet, noe som automatisk mikser alle sporene dine sammen, eller du kan velge spor og bruke "Rask miks"-kommandoen i Prosjekt-menyen.

<p>
<a name="a2"></a>
<font size=+1><b>
2. Kan jeg fjerne vokalen fra et opptak?
</b></font>
<p>

Det er mulig å fjerne all vokal fra enkelte stereo-opptak pga. måten opptaket ble mikset på studioet. Ofte er vokalene plassert eksakt i senteret av opptaket, mens alle andre instrumenter er litt forskjøvet fra midten. Hvis du subtraherer høyre kanal fra venstre vil vokalen bli helt borte, mens instrumentalen står igjen.
<p>

<b>Dette virker bare på noen opptak!</b>
<p>
For å prøve på dette i Audacity, importer et stereo-opptak og klikk på hurtigmenyen til sporet (den lille ned-pila ved siden av navnet til sporet) og velg "Del opp stereospor". Velg så det nederste sporet (høyre kanal) og bruk effekten "Inverter" (finnes i Effekt-menyen). Til slutt, bruk hurtigmenyen til sporene for å gjøre begge kanalene til Mono-kanaler, og miks dem sammen vha. "Rask miks". Hvis du er heldig vil vokalen nå være borte. 

<p>
<a name="a3"></a>
<font size=+1><b>
3. Når jeg prøver å ta opp harmonier med meg selv, hvorfor er de to sporene usynkronisert?
</b></font>
<p>
Dette er normalt og skjer fordi det er en liten forsinkelse mellom når Audacity starter å spille av lyd, og når lyden faktisk når høyttalerne dine. Audacity prøver ikke å automatisk korrigere for denne forsinkelsen, så det må gjøres manuelt.
<p>
For å korrigere på denne forsinkelsen, bruk verktøyet Tidsforskyvning for å skyve det ene sporet til de er parallelle. Merk at du kan bruke verktøyet mens du hører på opptaket. 

<p>

<a name="a4"></a>
<font size=+1><b>
4. Hvorfor kommer ikke Audacity med en MP3-koder?
</b></font>
<p>

Uheldigvis er algoritmen for å kode eller skape MP3-filer under patentbeskyttelse, og <b>Audacity</b> ville ikke være i stand til å inkludere en eksport-algoritme for MP3 uten enten å ta betalt eller å bryte loven i en god del land.
<p>
Som et kompromiss kommer Audacity med muligheten til å bruke andre MP3-kodere, som må lastes ned for seg selv. Det er opp til deg å sjekke at du følger lisensrestriksjonene som er gitt med MP3-koderene.
<p>
For mer informasjon, se siden om å Eksportere MP3 i vår Online Hjelp.

<p>



<a name="a5"></a>
<font size=+1><b>
5. Kan jeg ta opp RealAudio eller annen "streaming" lyd (lyd for direkteavspilling)?
</b></font>
</p><p>

Ikke automatisk. De fleste formatene skapt for direkteavspilling, som inkluderer det meste av musikken levert til RealOne Player, Windows Media Player og Quicktime er spesielt designet for å forhindre at noen skal kunne ta opp lyden.
</p><p>
Men, det er nesten alltid mulig å ta opp hva enn maskinen din spiller ved å kjøre en sløyfe fra lyd-ut til lyd-inn/mikrofon-pluggen på lydkortet til maskinen din. 
</p><p>
Bruk en 3.5mm stereo minijack han-til-han kabel. Koble til den ene enden til lyd-ut-pluggen på datamaskinen eller lydkortet (der hvor du normalt kobler til høyttalere eller høretelefoner - vanligvis grønnfarget). Koble den andre enden til lyd-inn eller mikrofon-pluggen (vanligvis rødfarget).
</p><p>
Nå kan Audacity ta opp hva enn maskinen din spiller. Trykk Spill inn i Audacity, og trykk så Spill av i ditt "streaming"-lydprogram. 
</p><p>

<a name="a6"></a>
<font size=+1><b>
6. Hjelp! Jeg trykker Spill inn, men jeg får bare stillhet!
</b></font>
</p><p>
Audacity tar opp fra innkilden som er valgt til å være standard i ditt operativsystem. Hvis du har flere innkilder for lyd (som f.eks. en innebygd mikrofon, en ekstern mikrofon, lyd-inn-plugg, eller en lyd-CD) må du velge en av disse å ta opp fra. Hvis du prøver å ta opp fra mikrofon, for eksempel, men innkilden din er satt til "line in", vil Audacity bare ta opp stillhet.
</p><p>
Hvis du bruker Windows kan du velge innkilde ved å høyreklikke på volum-ikonet på verktøylinjen (nederst i høyre hjørne av skjermen din, til venstre for klokka).
</p><p>
Hvis du bruker Mac OS 9, bruk kontrollpanelet for lyd. I Mac OS X, bruk Lyd-panelet i Systempreferanser (det er en Innkilde-del der hvis du har 10.2 eller høyere).
</p><p>
Under Linux, bruk en mikser, som f.eks. "xmixer" eller "kmix".
</p><p>

<a name="a7"></a>
<font size=+1><b>
7. Hvordan deler jeg èn enkelt fil inn i flere spor?
</b></font>
</p><p>
Noen ganger kan du ende opp med et opptak av flere sanger i en enkelt fil. Hvis du vil brenne disse sangene til en CD i separate spor må du lage en separat fil for hver sang.
</p><p>
Det finnes mange måter å dele et opptak opp i flere spor i Audacity. Her er noen av metodene du kan prøve:
</p><p>
Metode 1: Eksportere markering
</p><ul>
<li>Velg den delen av lydsporet som korresponderer med det første sporet. 
</li><li>Velg "Eksporter markering som WAV..." fra Fil-menyen, og lagre til disken.
</li><li>Fortsett slik for alle de resterende sporene.
</li></ul>
<p>
Metode 2: Slette og Angre
</p><ul>
<li>Slett alt <i>utenom</i> det første sporet.
</li><li>Velg "Eksporter som WAV..." fra Fil-menyen, og lagre det første sporet til disken.
</li><li>Velg "Angre" fra Rediger-menyen. Fortsett å angre helt til du har resten av lyden igjen. (Audacity kan angre uendelig mange ganger.)
</li><li>Fortsett slik for alle de resterende sporene.
</li></ul>
<p>
Metode 3: Del og Eksporter 
</p><ul>
<li>Marker det du ønsker å ha som første sporet.
</li><li>Velg "Del opp" fra Rediger-menyen, slik at markeringen blir flyttet til et adskilt spor i Audacity.
</li><li>Fortsett slik til alle sangene er i adskilte spor. Glem ikke at du kan Angre når som helst hvis du gjør en feil.
</li><li>Gå så gjennom dem og bruk "Eksporter markering som WAV..." for å eksportere hvert spor til en separat fil. For å raskt velge hvert spor, klikk på etiketten (området til venstre for bølgeformen, rett under tittelen).
</li></ul>
<p>

<a name="a8"></a>
<font size=+1><b>
8. Hvordan får jeg overført sanger fra en lyd-CD?
</b></font>
</p><p>
Audacity kan ikke overføre sanger fra en lyd-CD. Du må bruke et "ripper"-program for dette.
</p><p>
For Windows anbefaler vi <a href="http://cdexos.sourceforge.net/">CDex</a>.
</p><p>
For Mac OS (9 og X) anbefaler vi Apples <a href="http://www.apple.com/itunes/">iTunes</a>.
</p><p>
For Linux, prøv <a href="http://www.xiph.org/paranoia/">Paranoia</a>.

<?php BoxBottom(); ?>
