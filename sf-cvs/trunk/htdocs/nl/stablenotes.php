<?php BoxTop("$releaseNotesStr $stableVersion"); ?>

<pre>
Bekende onderwerpen/problemen:

* Windows: Het floating tool palette minimaliseert niet als
  een project venster wordt geminimaliseerd.  Als tijdelijke oplossing,
  kun je de tool palette samen laten gaan met het project venster.

* Linux: Full duplex (afspelen van een track terwijl je een andere aan het 
         opnemen bent) schijnt niet te werken, terwijl je geluidskaart deze 
         optie wel ondersteunt.
         Dit probleem ligt waarschijnlijk gelegen in de manier hoe wij OSS 
         gebruiken.

* Linux: Geluidskaarten die alleen kunnen werken met meer dan twee kanalen, 
         worden nog niet ondersteund.

-------------------------------------------------------------

Veranderingen in 1.0:

  * Mac OS:

    - Audacity wordt niet direct afgesloten als je het laatste venser sluit.
      De menubalk blijft actief en daardoor kun je een nieuw venster openen.

    - Fout hersteld die ervoor zorgde dat de Voorkeuren dialoog niet verscheen 
      in Mac OS 9 met het ATM controle paneel

    - Fout hersteld welke het opnemen van lage sample rates in Mac OS 9 
      verhinderde
      
    - Herkenning van MP3 bestanden aan de bestandsextensie en type code

    - Audacity is nu stabieler en functioneler in MacOS X

  * Nieuwe mogelijkheden:

    - "Bewaar Veranderingen" dialoog voor alle platforms toegevoegd

    - Tooltips

    - Nieuwe toestenbord shortcuts en commando's

    - Label tracks kunnen nu worden gebruikt om selection regions te markeren.
      Het toevoegen van labels maakt gebruik van een nieuwe commando -- zie 
      de gebruikershandleiding.

  * Unix:

    - Fout hersteld welke te snel audio playback veroorzaakte
      (in het bijzonder op Mandrake systemen).

    - Fout hersteld welke bestanden aantastte als je een bestand  
      overtop een andere exporteerde.

  * Anderen:

    - VST Effecten worden sneller uitgevoerd

    - Verschillende fouten hersteld

Veranderingen in 0.98b (alleen in Windows foutherstel vrijgave):

  - Herstelde "Ruisverwijdering" vastloper

  - "Bewaar Veranderingen" dialoog toegevoegd

  - Opnieuw ingevoerde ondersteuning voor ID3 tags in Windows

  - Meer fouten hersteld

Veranderingen in 0.98:

* Effecten

  - Nieuwe omkeer effecten

  - Nieuwe terugkeer effecten

  - Verbeterde Ruisverwijdering effecten

* Fout herstel

  - Je kunt nu veilig kopieer- en plakbewerkingen tussen projecten uitvoeren.

  - Herstelde fouten in laden, opslaan en undoing envelopes.

  - Het is nu niet meer toegestaan hetzelfde project in twee verschillende 
    vensters te openen (zou alleen maar data verlies veroorzaken).

  - Projecten slaan nu ook de kanalen van elke track op, dit wordt niet langer 
    vergeten

  - Betere behandeling van het probleem dat ontstond als tijdens het opstarten 
    van het programma bleek, dat de temp directory ongeldig was

  - Geen overschrijving van eeh bestand waar het project afhankelijk van was
    (het oude bestand wordt nu hernoemd). Je kunt nu zonder problemen naar 
    hetzelfde bestand exporteren als dat je had geïmporteerd.

  - Opslaan Als... van een project, vernietigd het oude project niet meer.

  - Ongedaan maken informatie wordt nu verwijderd bij het sluiten van een 
    project, waardoor diskruimte wordt bespaard wat in vorige versies niet het 
    geval was.

  - Opslaan Als... bevallige herstel bij het proberen van opslaan naar een 
    slechte locatie

  - Dupliceren van een track met gebruik van een offset werkt nu

  - Een vastloper in de Versterker is hersteld... (als geen data in één van
    de tracks is geselecteerd)

  - Windows: Opnemen/weergave apparaten worden niet langer in de Voorkeuren 
    omgedraaid!

  - Windows: VST plug-ins worden nu gevonden ongeacht de manier waarop Audacity 
    wordt opgestart.

  - Windows: Je kunt nu Ogg Vorbis bestanden importeren.

  - Bij het mislukken van het opnemen, wordt nu geen fantoom track meer 
    aangemaakt.

  - Verandering van het exportformaat verandert nu de menubalk.

  - Meer kleine fouten hersteld...

* Unix:

  - Nieuwe OSS code, zou betere playback en opname moeten verzorgen op bijna 
    alle OSS Unix systemen.

  - Voorafgaande ondersteuning voor de KDE/aRts soundserver (een 'compile-time'
    optie; moet geconfigureerd worden om deze in plaats van de OSS code te 
    kunnen gebruiken)

  - Fout herstel in het "make install" script

Veranderingen in 0.97:

* Installatie/configuratie

  - Nieuwe Windows installeerder

  - Meer opties in het configureer script in Unix

* Gebruikers Interface:

  - Fout in  het Effectenmenu hersteld, die bleef hangen als niet alle tracks 
    waren geselecteerd.

  - Ruisverwijdering toegevoegd (Dominic)

  - Verbeterde click-drag zoom.

  - Ondersteuning voor drag-en-drop voor het importeren van audio bestanden 
    (alleen in Windows)

  - Verbeterde Export bestanden dialoog (vraagt naar vreemde bestandsextensies)

  - Ander fouten hersteld

* MacOS:

  - Autoscrolling fout hersteld (verscheen soms bij het loslaten van de
    cursor buiten het venster tijdens een selectie).

* Unix:

  - Fout in de Versterking hersteld (bleef soms hangen bij het proberen te 
    openen van de dialoog).

Veranderingen in 0.96:

* Algemene Gebruikers Interface:

  - Mute/Solo knoppen toegevoegd

* Importeren van Audio:

  - Regressie fout hersteld in 0.95 (importeerde een stereo bestand als twee 
    mono tracks)

  - Importeren van MP3 ID3 tags

* Exporteren van Audio:

  - Exporteren van MP3 werkt nu, bij correcte installatie van de juiste versie 
    van de LAME DLL (Joshua)
  
  - Staat bewerking van MP3 ID3 tags met exporteren toe.

* Voorkeuren:

  - Audio I/O selectors toegevoegd voor Mac (Dominic) en Windows (Joshua)

* Effecten:

  - Progress dialog support voor alle effecten toegevoegd (daardoor is het ook 
    mogelijk om effecten te schrappen)
  
  - Toegevoegde ondersteuning voor stereo effecten en effecten die tracks
    kunnen toevoegen of verwijderen, of die multiple passes benodigen.
  
  - Verbeterde Versterker effect en herstel van alle bekende fouten (Dominic)
  
  - Verbeterde Bass Boost effect
  
  - Filter effect toegevoegd (Dominic)
  
  - Phaser effect toegevoegd (Paul)
  
  - Wahwah effect toegevoegd (Paul)

Veranderingen in 0.95:

* Installatie/Compilatie:

  - Verbeterde configureer script op unix systems (Joshua)

* Algemene Gebruikers Interface:

  - Menu items zijn nu uitgeschakeld als ze niet beschikbaar zijn

  - Online help toegevoegd (Dominic and Logan)

* Importeren van Audio:

  - Lazy import toegevoegd, verdubbeling van snelheid bij het importeren van
    PCM bestanden

  - Toegevoegde ondersteuning voor de gratis libmpeg3 bibliotheek voor unix als 
    vervanging van het gepatenteerde xaudio (Joshua)

  - Importeren van MP3 en Ogg Vorbis bestanden wordt nu automatisch 
    gerealiseerd door de Open en Import commando's.

  - Herstel van de Import Raw Data mogelijkheid, je kunt nu bestanden van 
    bijna alle willekeurige formaten importeren (zolang ze niet gecomprimeerd 
    zijn).

* Hoofd Venster:

  - Nieuwe track labels met een geïntegreerd pop-up menu om alle track opties 
    te kunnen verwerken

  - Verticale liniaal toegevoegd, samen met preliminaire ondersteuning voor
    verticale zooming

  - Stereo tracks kunnen nu gelinked worden zodat veranderingen beide tracks 
    beïnvloeden

  - Point-sample display neemt het over als je zeer ver inzoomt

  - Twee nieuwe wave displays: een dB (logaritmisch) wave display en
    een spectral pitch display (maakt gebruik van versterkte autocorrelation)

* Voorkeuren:

  - Nieuwe spectral display voorkeuren

  - Temp directory kan ingesteld worden in de voorkeuren

* Frequentie display:

  - Vele nieuwe frequentie venster veranderingen, waaronder ondersteuning voor
    cepstrum, autocorrelation, en versterkte autocorrelation.

* Envelope bewerker:

  - Envelopes zijn nu interpolated gebruik makend van decibels, waardoor
    cross-fades veel beter klinken

* Effecten:

  - Fout hersteld die leidde tot onverenigbaarheid met vele VST plug-ins.

  - Maximize Amplitude effect toegevoegd

  - Bass Boost effect toegevoegd (Paul)

* Anderen:

  - Verbeterde geheugenmanagement betreffende lange Ongedaan maken geschiedenis

  - Vele andere fouten hersteld

Veranderingen in 0.94:

* Voorkeuren dialoog (Joshua Haberman)

* OGG Vorbis import (Joshua Haberman)

* Stilte, Invoegen Stilte commando's

* Splits en Dupliceer commando's

* Mac OS X ondersteuning

* Ondersteuning van opname met Mac OS 8 en 9

* Vele fouten hersteld

Veranderingen in 0.93:

* Displays playback/recording positie indicator

* Bijhouden van sommige voorkeuren

* Ondersteuning van willekeurige project sample rate

* Mac: openen van documenten via de Zoeker

* Floating tool palette is nu dockable
  (en gedocked is standaard)

* Fouten hersteld in uitvoering van meervoudige open projecten

* Ondersteuning van Opnemen (Windows, Linux)

* Frequentie Venster toont namen (d.w.z. C4, G#5)

* Vele fouten in de effecten hersteld, waaronder VST plug-in effecten

Veranderingen in 0.92:

* Frequency Plot venster toegevoegd en verbeterde Spectrum display

* Fout hersteld in "Bestand: Open' als het te openen bestand eigenlijk een 
  groot WAV bestand was

Veranderingen in 0.91:

* Gebruikt xaudio bibliotheek voor importeren van mp3 bestanden

* Zoom menu

Veranderingen in 0.9:

* Nieuwe floating tool palette met vier tools (selectie, sliding, zooming, en 
  envelope bewerking) plus Afspeel en stop knoppen

* Playback mixt nu tracks, en je kunt ermee werken terwijl je er naar luistert.
  De stop knop werkt.

* Herschreven bestanden handling functies.  Het hoofdvenster is niet langer 
  afhankelijk van de wxWindows DocView classes, dus kunnen we nu bestanden zelf 
  behandelen. Het project bestandsformaat is op tekst gebaseerd zodat 
  fouten gemakkelijk opgelost kunnen worden. uiteindelijk zal alles op XML 
  worden gebaseerd.

* Verbeterde verwerking van wave tracks: zoals eerder, is de data opgeslagen in 
  blocks, maar nu worden de blocks evenredig verdeeld tussen n and 2n bytes elk 
  (voor sommige n), wat garandeert dat bewerkingshandelingen altijd evenlang 
  zullen duren, terwijl dit ook het fragmenteren van projecten tegengaat.

* Herschreven gebruikers interface code. De grijze achergronden zijn afkomstig 
  van het besturingssyteem, en het project venster is opnieuw ontworpen om 
  consequenter te zijn in de layout van alle systemen.

* Selecteren van "Open" doet nu wat het moet doen, het openen van een project 
  als je een project opent en het importeren van een WAV bestand als je een WAV 
  bestand wilt importeren.

* Knipperende cursor geeft de huidige bewerkingspositie aan

* Verbeterde liniaal - de liniaal ziet er niet alleen beter uit, het 
  toont nu ook de selctie en de cursor.

* De zoom tool centreert op de cursor zodat je waar dan ook kunt inzoomen.

</pre>

<?php BoxBottom(); ?>
