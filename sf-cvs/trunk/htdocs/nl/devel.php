<?php BoxTop($develNewsStr); ?>

<p>
Deze webpagina zal zich ontwikkelen tot een plek waar je te weten kunt komen wat alle ontwikkelaars aan het doen zijn.  In de tussentijd hier wat informatie over Audacity 1.1, wat op dit moment de beta-versie is.
</p>

<p>
<b>Nieuwe mogelijkheden in versie 1.1:</b>
<pre>
  * Kern audio bewerking:
    - Ondersteuning voor 24-bit en 32-bit sample formaten
    - Automatische real-time resampling (gebruik makend van lineaire
      interpolatie)
  * Effecten:
    - Ondersteuning van LADSPA plugins voor Linux / Unix
  * Bestandsformaten:
    - Nieuwe op XML-gebaseerde Audacity project formaten
    - Volledige Ogg Vorbis ondersteuning (zowel impoteren als exporteren)
    - Export naar iedere command-line programma's in Unix
    - Ondersteuning voor het lezen en schrijven van veel meer types
      niet gecomprimeerde audio bestanden, waaronder ADPCM WAV bestanden.
  * Werkbalken
    - Nieuwe werkbalk teken code; past zich automatisch aan aan de kleuren van 
      je besturingssysteem
    - Nieuwe werkbalk knoppen (Spring naar Start, Spring naar Einde)
    - Nieuwe Bewerkings werkbalk
    - Werkbalk knoppen uitschakelen als ze niet beschikbaar zijn
  * Gebruikers Interface
    - Volledig aanpasbare toetsenbord commando's
    - Autoscroll tijdens afspelen of opnemen
    - Nieuwe liniaal, wordt gebruikt in het hoofdvenster en in het
      FFT Filter effect
    - De wavevorm toont nu de gemiddelde waarden in een lichtere kleur
      binnen de piekwaarden
  * Toewijzing
    - Audacity kan nu aan verschillende buitenlandse talen worden toegewezen.
</pre>
</p>

<p>
<b>Tabel van bibliotheken waar wij op vertrouwen (versie 1.1.0 en verder):</b>
<table border=0 cellpadding=8 cellspacing=2>
<tr>
<th>Bibliotheek</th>
<th>Doel</th>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.wxwindows.org">wxWindows</a>
<td bgcolor="#ccccff"
>Dit is een cross-platform bibliotheek welke onze GUI menus, buttons,
    windows, drawing, etc.) toestaat op Mac, Windows, and Unix systemen te 
    draaien.  wxWindows verstrekt ook andere nuttige C++ classes, en
    Audacity is 100% afhankelijk van deze bibliotheek.  We adviseren het 
    gebruik van deze bibliotheek ten zeerste als je cross-platform
    ontwikkeling wilt uitvoeren.
</tr>

<tr>
<td bgcolor="#ccccff"
><a href="http://www.mars.org/home/rob/proj/mpeg/">libmad</a>
<td bgcolor="#ccccff"
><p>
    MAD betekent MPEG Audio Decoder.  Dit is één van de weinige
    MP3 decoders die gratis is (GPL), en de enige, zover wij weten, die
    integer math gebruikt en in staat is om 24-bit output te genereren (waar
    we op dit momnet nog geen gebruik van maken).
    Hoewel MP3 bestanden normaal gesproken gecreeërd worden via 16-bit input,
    geeft de MP3 decoder die 24-bit output kan produceren ons de mogelijkheid 
    om onze eigen dither te gebruiken, wat leidt tot een in potentie hogere 
    audio kwaliteit, of in elk geval een betere gebruikers beheersing. Deze 
    bibliotheek is erg snel (zelfs vergeleken met xaudio, wat we hiervoor 
    gebruikten) en erg stabiel. Het is geschreven door Rob Leslie.</p>
    <p>Deze bibliotheek heb je alleen nodig als je MP3 bestanden wilt 
    importeren</p>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.mars.org/home/rob/proj/mpeg/">libid3tag</a></td>
<td bgcolor="#ccccff"
><p>
    Deze bibliotheek is ook geschreven door Rob Leslie (de auteur van libmad,
    zie hierboven) en het is een fijne en simpele bibliotheek voor het lezen en 
    schrijven van ID3 tags in MP3 bestanden.</p>
    <p>Deze bibliotheek is optioneel; als het wordt gekoppeld binnen Audacity, 
    zal aan de gebruiker een Tags dialoog worden gepresenteerd tijdens het 
    exporteren van MP3 bestanden, en de tags zullen worden vastgelegd tijdens 
    het importeren van de MP3 bestanden.
    Merk op dat de libid3tag niet apart wordt gedistribueerd; het is een 
    onderdeel van MAD.</p>
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.xiph.org/ogg/vorbis/"
>libogg<br>libvorbis<br>libvorbisfile</a>
</td>

<td bgcolor="#ccccff"
>
<p>Ogg Vorbis is zowel een gratis audio compressie formaat als een bibliotheek
dat bestanden kan coderen en decoderen in dit formaat.  Het is bedoeld als vervanger van MP3, en vele mensen vinden dat het net zo goed zo niet beter is dan het MP3 formaat in zowel kwaliteit als grootte.
De laatste (beta) versie van Audacity kan Ogg Vorbis bestanden zowel importeren als exporteren.
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
Deze bibliotheek geeft ons de mogelijkheid om audio I/O te gebruiken op meervoudige systemen, gebruik makend van normale API.  Het verbergt de verschillen in audio I/O verwezelijkingen tussen de verschillende systemen
en presteert normaal gesproken erg goed.  De standaard audio code is stabiel voor Windows (MME en DirectX),
Unix/OSS, en MacOS 9, en uitgangen voor MacOS X, Linux/ALSA, en Linux/aRts
zijn in voorbereiding.
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a></td>

<td bgcolor="#ccccff"
>
Dit is de nieuwe bibliotheek die we gebruiken om audio files te lezen en te schrijven zoals WAV, AIFF en AU.  Het kan overweg met simpele compressies zoals ADPCM, maar niet met lossy-gecomprimeerde bestanden.
</td>
</tr>
</tabel>
</p>


<?php BoxBottom(); ?>
