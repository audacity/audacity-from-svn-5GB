<pre>
Wijzigingen in 1.2.0-pre3:

  * Fout waardoor de Exportknop grijs bleef als niets geselceteerd was, is
    hersteld.

  * Fout hersteld die zorgde voor het crashen van de computer met een high-end
    geluidskaart zonder mixer ondersteuning, bij het openen van Audacity.

  * Fout die een crash in Import Raw veroorzaakte is hersteld.

  * Nieuwe Stereo Track hersteld.

  * Uiterlijke veranderingen in de Mac OS X versie.

  * Ondersteuning voor de VST Enabler in Windows toegevoegd.

  * Crach hersteld die optrad bij sluiting van Audacity terwijl het voorkeur-
    menu nog actief was.

  * Duplicate-character fout in Mac OS X Label Tracks hersteld.

  * Het opname level control in Linux past nu de IGAIN aan, i.p.v. het
    playthrough level van de opnamebron.

  * Fout die corruptie aan 16-bit stereo opnames veroorzaakte is hersteld.

  * Fout hersteld welke dataverlies veroorzaakte bij het vewijderen van tracks
    in een opgeslagen project, terwijl een nieuw bestand in hetzelfde venster
    werd geopend.

  * Toegevoegde ondersteuning voor alternatieve audio button order (in Interface
    voorkeuren)

  * Voorlopige ondersteuning voor for wxX11 toegevoegd

  * Volledig transparante Windows XP icoon toegevoegd

  * Crashfout hersteld als je probeerde op te nemen (of af te spelen)en er geen
    audio-apparaat aanwezig was, of als het audio-apparaat de geselecteerde mode
    niet ondersteunde.

  * Audacity zal tijdens opnames in Windows niet langer de process priority op
    Hoog instellen. Gebruikers kunnen dit nog wel handmatig instellen m.g.v.
    de Task Manager.

  * Fout hersteld die ervoor zorgde dat de laatste ~100 ms van de selectie in
    Windows werd afgekapt.

  * Herstelde FFT Filter en Equalisatie effecten menu.

  * Herstelde fouten in Unix gebouwde systemen (DESTDIR in lokale directory,
    welke libsamplerate kiest in plaats van libresample)

  * Ondersteuning voor LADSPA plug-ins in Windows toegevoegd, en drie open 
    source LADSPA plug-ins geport naar Windows
    (GVerb reverb, SC4 compressor en Hard Limiter)

Wijzigingen in 1.2.0-pre2:

  * Online help is nu compleet. De complete handleiding is bijna gereed en zal
    zeer binnenkort op de website worden geplaatst.

  * Audacity zal je niet langer onveilige bewerkingen laten uitvoeren
    tijdens opnames of het afspelen.  Hierdoor worden vele potentieële
    crashes voorkomen.

  * Herstelde optie om Quit button te kunnen afbreken.

  * Nieuwe resampling library, zonder restricties v.w.b. de maximum of
    minimum rate van resampelen.

  * Audacity bevat nu ondersteuning voor LADSPA plug-ins voor alle platformen
    en ondersteund VST plug-ins via een optionele LADSPA plug-in die
    de "VST Enabler" wordt genoemd, welke apart kan worden gedownload.
    I.v.m. licentie problemen kan Audacity niet verspreid worden met ingebouwde
    VST ondersteuning.

  * Mac OS X keyboard snelkoppeling problemen zijn verholpen.

  * Mac OS X audio muting problemen zijn verholpen.

  * Mac OS X playback/recording cursor sync problemen zijn verholpen.

  * Stilte wordt nu weer via een rechte lijn getoond i.p.v. niets.

  * Een verticale lineaal is aan de Waveform dB display toegevoegd.

  * Crashfout in Change Pitch hersteld.

  * Je kunt nu plakken terwijl er niets is geselecteerd.

  * Afbreken van een Import handeling zal niet meer leiden tot een getoonde 
    extra foutmelding.

  * Audacity kan nu overweg met bestandsnamen die internationale characters
    bevatten.

  * ID3v2.3 tags (in plaats van ID3v2.4), om daarmee compatible te zijn met
    meer MP3 spelers.

  * Kleine verbeteringen in build system voor Unix systemen.

Nieuwe mogelijkheden in Audacity 1.2.0-pre1:

  * Gebruikers Interface
    - Verticale zooming van tracks.
    - Verbeterd uiterlijk en plaatsing van de werkbaken.
    - Nieuwe standaard muis cursors.
    - Complete implementatie van bewerkbare keyboard sneltoetsen.
    - Vinden van zero-crossings.
    - Mouse wheel kan gebruikt worden voor in- en uitzoomen.
    - Multi-Tool mode.
    - Versterk door gebruik van envelope.
    - Labels kan selecties opslaan (zoals in Audacity 1.0.0).

  * Effecten
    - Herhaal laatst gegeven Effecten opdracht.
    - Verbeterde VST plug-in ondersteuning.
    - Meeste effecten hebben nu een Preview knop.
    - Compressor (Dynamic Range Compressor).
    - Verander Pitch (zonder tempo te veranderen).
    - Verander Tempo (zonder pitch te veranderen).
    - Verander snelheid (verandering van zowel pitch als tempo).
    - Herhaal (bruikbaar bij het maken van loops).
    - Normalisatie (aanpassen van volume en DC bias).

  * Audio I/O
    - 1-seconde preview opdracht.
    - Looped play.

  * Bestand I/O
    - Audacity 1.2.0 opent project bestanden van alle vorige versies
      van Audacity van 0.98 tot 1.1.3.
    - Open veelvoudige bestanden van dezelfde dialog.
    - Gebruik een text bestand omeen lijst met audiobestanden te specificeren
      te openen met offsets.

  * Geupdate gebruikershandleiding.


Fout herstel in Audacity 1.2.0-pre1

  * Projectbestanden met speciale characters zijn niet langer ongeldig.

  * "Kras" geluiden veroorzaakt door slecht knippen zijn verholpen.

  * Audacity exporteert niet langer ongeldige Ogg bestanden, en verwijderd niet
    langer de laatste paar seconden van een geëxporteerd Ogg bestand.

  * Mono MP3 bestanden worden nu met de correcte snelheid geëxporteerd.

  * Vele onjuuiste resultaten van de Envelope tool zijn hersteld.

  * De "Export Labels" opdracht overschrijft bestaande bestanden nu correct.
  
  * Het "Plot Spectrum" scherm toont nu correcte octave nummers voor notities.

  * Verschillende geheugenlekken zijn verholpen.

  * Vele andere kleine fouten zijn hersteld.
</pre>
