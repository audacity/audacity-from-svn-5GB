<h2>Forandringer i Audacity 1.2</h2>

Større forandringer siden vår forrige stabile utgave, Audacity 1.0.0:

<h3>Lyd med profesjonell kvalitet</h3>

<ul>
<li>
Audacity kan nå ta opp og redigere 24-bits og 32-bits (flyttall)
datapunkt. Samme prosjekt kan ha spor med forskjellige datapunktrater, og 
Audacity vil konvertere dem i sanntid når enn det er nødvendig. 
Dithering og omsampling med høy kvalitet blir brukt for alle konverteringer.
</li>

<li>
Audacitys innlyd og utlyd er bedret. Audacity kan nå ta opp flere enn to 
kanaler samtidig. Buffering er forbedret, for å redusere sjansen for at
sporet hakker eller at bufferen går tom.
</li>
</ul>

<a href="screenshots.php<?php print $langQueryStr; ?>">
<img alt="screenshots" border="0" src="images/screen/linux/change-pitch.png" align="right"></a>
<h3>Effekter</h3>

<ul>
<li>
Tre nye effekter kan nå forandre tonehøyde og tempo på et spor:
 <ul>
  <li>Forandre tonehøyde gjør tonen på et utvalg høyere eller lavere,
  uten å forandre hastighet.</li>
  <li>Forandre tempo gjør at utvalget spiller raskere eller saktere, 
  uten å forandre tonehøyde.</li>
  <li>Forandre hastighet forandrer både avspillingshastighet og tonehøyde,
  akkurat som når du forandrer hastighet på en plate- eller kassettspiller.</li>
 </ul>
</li>

<li>
De fleste effekter vil nå ha en "Forhåndsvisning"-knapp, som lar deg prøve 
ut forskjellige instillinger uten å lukke effektvinduet. En ny kommando lar
deg repetere den sist brukte effekten uten å gjenåpne vinduet.
</li>

<li>
Andre nye effekter er bl.a.:
 <ul>
  <li>Komprimering, for dynamisk områdekomprimering.</li>
  <li>Repeter, for å spille markeringer i løkke.</li>
  <li>Normaliser, for å justere volum og rette "DC-bias".</li>
 </ul>
</li>
</ul>


<h3>Nye redigeringsfunksjoner</h3>

<ul>
<li>
Omhyllingsverktøyet, brukt for å jevnt tone inn og ut et spor, kan nå bli 
brukt til å gi spor høyere volum enn det de originalt hadde deres, i 
tillegg til lavere volum. 
</li>

<li>
Den nye "Tidsspor"-funksjonen ligner på volumomhyllingskurven, men
isteden forandrer den jevnt avspillingshastighet mens et spor spiller.
</li>

<li>
Hvert spor har nå sine eget Forsterknings- og Panoreringskontroller,
for enklere miksing. 
<a href="screenshots.php<?php print $langQueryStr; ?>"><img alt="screenshots" border="0" src="images/screen/linux/track-controls.png" align="right"></a>
</li>

<li>
Audacity can finne nullpunkt, for å hjelpe til med å lage jevne kutt
og løkker. Tast "Z" for å flytte markeringskanten til nærmeste 
nullpunkt.
</li>
</ul>


<h3>Programtillegg</h3>

<ul>
<li>
På Linux kan Audacity nå laste inn programtillegg av typen <a href="http://www.ladspa.org/">LADSPA</a>.
</li>

<li>
Audacity 1.2 bruker et digitalt signalbehandlingsspråk kalt
<a href="nyquist.php">Nyquist</a>, som lar brukere programmere nye 
effekter i et LISP-liknende språk.
</li>
</ul>


<h3>Filimportering og -eksportering</h3>

<ul>
<li>
Audacity 1.2 sine prosjektfiler bruker et nytt XML-filformat. 
Audacity 1.2 vil automatisk åpne og konvertere prosjektfiler 
fra tidligere utgaver.
</li>

<li>
Audacity 1.2 bruker <a href="http://www.underbit.com/products/mad/">libmad</a>
for mye raskere dekoding av MP3-filer. Erik de Castro Lopo sin
<a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a> gir 
bedret samspill med mange ukomprimerte lydfilformat.
</li>

<li>
Import- og åpne-dialogene vil nå la Audacity-brukere velge mellom flere
lydfiler på en gang og åpne dem inn i ett enkelt prosjekt. Et nytt 
"LOF"-filformat gir en måte for Audacity å åpne en gruppe filer
som har forskyvninger gitt på en liste i en tekstfil.
</li>
</ul>


<h3>Forbredet brukergrensesnitt</h3>

<a href="screenshots.php<?php print $langQueryStr; ?>"><img alt="screenshots" border="0" src="images/screen/macosx/main-toolbar.png" align="right"></a>
<ul>
<li>
Nye Rediger- og Mikserverktøy for hurtig tilgang til vanlige funksjoner.
</li>

<li>
Det nye Tegneverktøyet tillater justering av individuelle datapunkt, når 
du har zoomet inn fullstendig. Den nye Multi-verktøy-modusen gir hurtig 
tilgang til forskjellige redigeringsfunksjoner, uten at du trenger å bytte
verktøy.
</li>

<li>
Mange nye tastaturkommandoer er lagt til og mange tastatursnarveier kan
nå skreddersys.
</li>

<li>
Nye kommandoer:
 <ul>
  <li>Spill av i løkke. Tast "L" eller hold nede skift-knappen når du trykker Spill av.</li>
  <li>Tast "1" for å spille en 1-sekund lang forhåndsvisning av lyden rundt markøren.</li>
 </ul>
</li>

<li>
Musehjulet kan nå brukes for å zoome inn og ut.
</li>

<li>
Du kan nå zoome vertikalt på spor ved å klikke eller dra i de vertikale
linjalene. Skift-klikk eller høyreklikk for å zoome ut.
</li>

<li>
Linjalen og statuslinja kan nå vise tiden i flere forskjellige format,
inkludert sekunder, datapunkt eller videorammer ("frames").
</li>

<li>
Audacitys brukergrensesnitt kan nå oversettes til andre språk enn engelsk.
Du kan hjelpe til med å <a href="translation/">oversette Audacity</a> 
til ditt eget morsmål.
</li>
</ul>

