<?php BoxTop("$releaseNotesStr 1.1.3"); ?>

<pre>
Bekende fouten in Audacity 1.1.3:
- Het plaatst een bestand met de naam "Audacity-Commands.xml" in je
  home directory of ergens anders op je computer. Dit bestand is nodig
  om Audacity 1.1.3 te kunnen gebruiken. We verontschuldigen ons voor
  de hierdoor ontstane wanorde op je computer en we beloven om in de
  volgende versie ervoor te zorgen dat het bestand om een geschiktere
  plaats terecht komt.

Nieuwe mogelijkheden in Audacity 1.1.3:
  * Gebruikers Interface
- Nieuwe Mixer werkbalk zorgt ervoor dat je direct vanuit Audacity
  controle kunt uitoefenen over uitgaande volume, inkomende volume
  en de invoerbron
- Elke track heeft nu een eigen 'gain' en 'pan' controle.

  * Bestand I/O
- Gebruikt een verbeterd project bestandsformaat.(Ongelukkigerwijs wordt
  het lezen van vorige bestandsformaten, waaronder versie 1.1.1, niet
  ondersteund.
- Block bestanden (opgeslagen in Audacity project directories) gebruiken
  nu het standaard AU formaat. Hoewel sommige Audacity meta-informatie in
  deze bestanden aanwezig is, kunnen ze nu ook door andere populaire
  audioprogramma's worden gelezen.
- Sommige fouten zijn opgelost betreffende het lezen/schrijven van 
  audiobestanden van meer dan 16 bits per sample.
- Importeren van RAW is weer functioneel, met een simpelere GUI
  maar ondersteuning voor veel meer bestandsformaten. De
  autodetectie algorithmes zijn veel accurater dan die in versie 1.0.
      
  * Audio I/O
    - Compleet herschreven audio I/O, met lagere latency
      en minimale kans op buffer underruns tijdens het opnemen.

  * Resampling
    - Gebruik van hoge kwaliteit resampling algorithmes, met de
      optie voor betere kwaliteit voor mixen dan voor real-time
      playback.

- Ondersteuning voor Time Tracks, voor verandering playback snelheid via tijd.

  * Veel meer fouten herstel en nieuwe mogelijkheden

Nieuwe mogelijkheden in Audacity 1.1.2:
  * Gebruikers Interface
    - Herstelde fout in de Windows versie, voor de track menu commando's 
      "Name..." en "Splits Stereo Track"/"Maak Stereo Track".
  * Effecten
    - Nyquist ondersteuning voor Windows (ondersteund plug-ins geschreven met 
      Nyquist, een geïnterpreteerde functionele taal gebasserd op Lisp).

Bekende fouten in Audacity 1.1.1:

  * Gebruik het Benchmark commando in Windows niet - het zou kunnen vastlopen.
    Het is geen handige mogelijkheid voor de meeste gebruikers - het is bedoeld
    voor ontwikkelaars om de snelheid van het programma op verschillende
    computers en besturingssystemen te kunnen beoordelen.

  * Mac OS X: sommige geluidbestanden kunnen verloren gaan als je de muisknop
    te lang op een menu houdt terwijl je opneemt. Probeer Audacity niet te 
    gebruiken tijdens het opnemen om de beste resultaten te verkrijgen.

Nieuwe mogelijkheden in Audacity 1.1.1:

  * Gebruikers Interface
    - Tooltips verschijnen in de Statusbalk.
    - Verticale cursor volgt afspeel/opnemen
    - Pauze knop
    - Opname hulpmiddel (met drie verschillende modes)
    - Verticale verandering van grootte van stereo tracks is veel leuker.
    - Pas de selectie aan door klik-slepen van de grens selectie
    - Werkbalkknop context-sensitiviteit inschakelen/uitschakelen
    - Verbeterde zooming functionaliteit (middelpunt regio)
    - Meervoudige mogelijkheden om de cursor positie en selectie weer te geven
    - Snap-to selectie mode
    - Sleep tracks op en neer
    - Voorbereiding en groepering van voorbereidingsfuncties
    - Cursor bewaar/herstel
    - Werkend geschiedenis venster
  * Effecten
    - Effecten onderverdeeld in drie menu's: Genereren, Effect en Analyseren
    - Genereer menu biedt de mogelijkheid tot het genereren van stilte, geluid 
      of een toon
    - Nyquist ondersteuning (ondersteund plug-ins geschreven met Nyquist,
      een geïnterpreteerde functionele taal gebaseerd op Lisp)
  * Plaatsbepaling
    - Verbeterde plaatsbepaling ondersteuning
    - Meerdere talen beschikbaar
    - Taal selectie dialoog tijdens opstarten
  * Mac OS X
    - Ondersteuning van meerdere audio hardware
    - Ondersteuning voor full-duplex (afspelen terwijl wordt opgenomen)
    - Ondersteuning voor MP3 exporteren gebruik makend van LameLib Carbon
  * Unix
    - Audacity heeft nu een handleidingsbestand (het beschrijft de command-line
      opties en hoe het zoekpad moet worden ingesteld)
  * Bestandsformaten
    - Gebruikt libsndfile 1.0, dat sommige fouten hersteld en de prestatie
      verbeterd
  * Zoeken naar bestanden:
    - Met Windows en Mac OS, Audacity zoekt nu naar vertalingen in de
      "Languages" folder en alle plug-ins in de "Plug-ins" folder, met 
      betrekking tot het programma.
    - Met Unix, Audacity zoekt naar vertalingen in <prefix>/share/locale en
      zoekt al het andere in <prefix>/share/audacity en ook in elk pad in
      de AUDACITY_PATH omgevings variabellen

Nieuwe mogelijheden in Audacity 1.1.0:

  * Kern audio bewerking:
    - Ondersteuning van 24-bit en 32-bit sample formaten
    - Automatische real-time resampling (gebruik makend van lineaire
      interpolatie)
  * Effecten:
    - Ondersteuning van LADSPA plugins voor Linux / Unix
  * Bestandsformaten:
    - Nieuwe op XML gebaseerde Audacity project formaten
    - Volledige Ogg Vorbis ondersteuning (zowel importeren als exporteren)
    - Exporteren naar elke command-line programma's in Unix
    - Ondersteuning voor lezen en schrijven van vele andere types van
      niet gecomprimeerde audio bestanden, inclusief ADPCM WAV bestanden.
  * Werkbalken
    - Nieuwe werkbalk teken code; neemt automatisch de kleuren van je 
      besturingssysteem over
    - Nieuwe werkbalk knoppen (Overslaan naar Start, Overslaan naar Einde)
    - Nieuwe Bewerkingswerkbalk
    - Werkbalk knoppen uitschakelen als ze niet beschikbaar zijn
  * Gebruikers Interface
    - Volledig aanpasbare toetsenbord commando's
    - Autoscroll tijdens afspelen of opnemen
    - Nieuwe liniaal, te gebruiken in het hoofdvenster en in
      FFT Filter effect
    - De waveform toont nu de gemiddelde waarden aan in een lichtere kleur 
      binnen de piek waarden
  * Plaatsbepaling
    - Audacity kan nu aan verschillende buitenlandse talen worden toegewezen.

Nieuwe bibliotheken in Audacity 1.1:

  * libmad voor snel importeren van MP3
  * libid3tag voor bewerking van MP3 bestandsinformatie
  * libsndfile voor het lezen en schrijven van meerdere audio bestandsformaten
  * PortAudio voor cross-platform audio afspelen en opname
</pre>

<?php BoxBottom(); ?>
