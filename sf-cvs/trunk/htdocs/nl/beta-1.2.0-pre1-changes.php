<h2>Wijzigingen sinds Audacity 1.0</h2>

In deze sectie, een lijst van wijzigingen na de vrijgave van onze laatste stabiele versie, Audacity 1.0.0. Gebruikers van onze beta versies zouden de vrijgave notities moeten lezen i.v.m. wijzigingen
<a href="<?php print "betanotes.php$langQueryStr"; ?>"> sinds Audacity 1.1.3</a>.

<h3>Professional-Quality Audio</h3>

<ul>
<li>
Audacity kan nu nu 24-bit en 32-bit (floating-point) samples opnemen en bewerken.  Tracks met verschillende sample rates en formaten kunnen nu in hetzelfde project bestaan en Audacity converteert ze in realtime, indien nodig.  High-quality dithering en resampling wordt voor alle conversies gebruikt.  Resampling gebruikt nu de algoritmes van de 
<a href="http://www.mega-nerd.com/SRC/">SRC</a> bibliotheek, gemaakt door
Erik de Castro Lopo.
</li>

<li>
Audacity's geluid input en output is verbeterd.  Audacity kan nu
meer dan twee kanalen tegelijkertijd opnemen.  Latency is verminderd voor he verlagen van de kans op overslaan of buffer underruns.
</li>
</ul>


<h3>Effecten</h3>

<ul>
<li>
Drie nieuwe effecten veranderen de the pitch en tempo van een track:
 <ul>
  <li>Wijziging van Pitch vergoogd of verlaagde de toon van een selectie, zonder
  aantasting van de snelheid.</li>
  <li>Wijziging van Tempo zorgt ervoor dat de selectiesneller of langzamer 
  afspeelt zonder wijziging van de pitch.</li>
  <li>Wijziging van Snelheid wijzigt zowel playback snelheid als pitch, 
  precies zoals wijzigen van snelheid van een platen- of cassettespeler. </ul>
</li>

<li>
De meeste effecten bevatten nu een "Preview" knop waardoor je de verschillende instellingen kunt uitproberen zonder het effectenscherm te hoeven sluiten. Een nieuw commando geeft de mogelijkheid het laatste effect te herhalen zonder heropening van het scherm.
</li>

<li>
Andere nieuwe effecten bevatten:
 <ul>
  <li>Compressor, voor dynamische range compression.</li>
  <li>Herhaal, om samples in de loop te plaatsen.</li>
  <li>Normalisering, voor aanpassing van volume en correctie van DC bias.</li>
 </ul>
</li>
</ul>


<h3>Nieuwe Bewerkingsmogelijkheden</h3>

<ul>
<li>
De Envelope tool, te gebruiken voor in en uit faden van tracks, kan nu ook gebruikt worden om het tracksvolume luider of zachter te maken dan het origineel.
</li>

<li>
De nieuwe "Time track" mogelijkheid is hetzelfde als het volume envelope, maar
in tegenstelling daarvan wijzigt deze de playback snelheid tijdens het afspelen van de track.
</li>

<li>
Elke track bevat nu zijn eigen Gain and Pan controle om makkelijker te mixen.
</li>

<li>
Audacity kan zero-crossings vinden als hulp bij de creatie van cuts en loops.
Druk "Z" voor het slepen van de hoeken van de selectie naar de dichtsbijzijnde  zero-crossings.
</li>
</ul>


<h3>Plugins</h3>

<ul>
<li>
Voor Linux, Audacity kan nu <a href="http://www.ladspa.org/">LADSPA</a>
plugins laden.
</li>

<li>
Audacity 1.2 bevat een digitaal signaalverwerkende taal genaamd
<a href="nyquist.php">Nyquist</a>, die gebruikers de mogelijkheid biedt nieuwe effecten te programmeren in een LISP-achtige taal.
</li>
</ul>


<h3>Bestands Import en Export</h3>

<ul>
<li>
Audacity 1.2 project bestanden gebruiken een nieuw XML bestandsformaat. Audacity 1.2 zal nu projectbestanden van eerdere versies automatisch kunnen openen en converteren.
</li>

<li>
Audacity 1.2 gebruikt <a href="http://www.underbit.com/products/mad/">libmad</a>
voor sneller decoderen van MP3 bestanden.  Erik de Castro Lopo's
<a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a> zorgt voor
verbeterde compabiliteit met vele uncompressed audiobestandsformaten.
</li>

<li>
De laatste versie van de <a href="http://www.vorbis.com/">Vorbis</a>
bibliotheken worden gebruikt voor hoge kwaliteits Ogg Vorbis encoding.
</li>

<li>
Via de import en open dialoovensters kunnen Audacity gebruikers nu meerdere audiobestanden ineens swlecteren en deze als een enkel project openen.  Een nieuw "LOF" bestandsformaat geeft Audacity de mogelijkheid om een groep bestanden met offsets te openen als een text bestand.
</li>
</ul>


<h3>Verbeterde gebruikersinterface</h3>

<ul>
<li>
Nieuwe Bewerkings- en Mixer knoppenbalk voor snelle toegang tot veel voorkomende functies.
</li>

<li>
Het nieuwe Drawing tool staat aanpassingen van individuele smaples toe indien volledig is ingezoomd.  De nieuwe Multi-Tool mode geeft snelle toegang tot verschillende bewerkingsfuncties zonder te hoeven switchen tussen tools.
</li>

<li>
Vele nieuwe toestenbordcommandos zijn toegevoegd en sneltoetscombinaties kunnen aangepast worden.
</li>

<li>
Nieuwe commandos:
 <ul>
  <li>Looped play.  Typ "L" of houdt shift ingdrukt bij klikken op Play.</li>
  <li>Typ "1" om een 1-seconde preview af te spelen van de audio.</li>
 </ul>
</li>

<li>
De muiswiel kan gebruikt worden voor in- en uitzoomen.
</li>

<li>
Tracks kunnen nu verticaal ingezoomed worden door klikken in of slepen naar de
verticale linealen.  Shift-klik of rechts-klik om uit te zoomen.  
</li>

<li>
De lineaal en de statusbalk kunnen nu de displaytijd aangeven in verschillende 
formaten inclusief seconden, samples of video frames.
</li>

<li>
Audacity's interface kan nu in verschillende talen vertaald worden.  Je kunt je hiervoor als vrijwilliger aanbieden <a href="translation/">vertaal
Audacity</a> om het in je eigen taal te vertalen.
</li>
</ul>