<?php BoxTop("$releaseNotesStr 1.1.3"); ?>

<pre>
Známé problémy v Audacity 1.1.3:
  - Ve vašem domácím adresáøi nebo nìkde jinde v poèítaèi si vytvoøí
    soubor s názvem "Audacity-Commands.xml".  Tento soubor je nezbytný pro bìh
    programu. Omlouváme se za zaneøádìní vašeho poèítaèe a slibujeme, že 
    v další verzi dáme soubor na lepší místo.

Nové vlastnosti v Audacity 1.1.3:
  * Uživatelské rozhraní
    - Nový Mixovací panel umožnuje nastavovat hlasitost vstupu, 
      výstupu a vstupního zdroje pøímo z Audacity.

    - Každá stopa má nyní vlastní nastavení "gain" a panorama (pan).

  * Vstup a výstup souborù (File I/O)
    - Používá se nový vylepšený projektový formát. (Bohužel naèítání
      pøedešlých formátù, vèetnì 1.1.1, není podporováno.)
    - Blokové soubory (uložené vadresáøi projektù) nyní
      používají standardní AU formát.  Aèkoliv jsou v tìchto souborech
      nìkteré meta-informace Audacity, mohou být naèítány i mnoha jinými
      populárními audio programy.
    - Byly opraveny nìkteré chyby týkající se naèítání a zapisování
      audio souborù s více než 16ti bity na vzorek.
    - Import RAW je znovu funkèní a to s jednodušším rozhraním (GUI)
      ale zato s podporou daleko více souborových formátù.
      Autodetekèní algoritmy jsou mnohem pøesnìjší než u verze 1.0.

  * Vstup a výstup audia (Audio I/O)
    - Vstup a výstup audia byl kompletnì pøepsán s nižší latencí
      a minimální šancí na pøeteèení bufferu bìhem nahrávání.

  * Pøevzorkování
    - Používají se vysoce kvalitní pøevzorkovací algoritmy,
      s možností lepší kvality pro mixování než jaká je pøi
      real-timovém pøehrávání.

    - Pøedbìžná podpora Èasových stop, pro zmìny rychlosti pøehrávání 
      over time.

  * Mnoho dalších oprav chyb a nových vlastností.

Nové vlastnosti v Audacity 1.1.2:
  * Uživatelské rozhraní
    - Opravena chyba Windowsové verze v menu stopy
  "Jméno..." a "Rozdìlit stereo stopu"/"Vytvoøit stereo stopu".
  * Efekty
    - Podpora pro Nyquist ve Windows (podporuje pluginy napsané
      v Nyquistu, interpretovaném funkèním jazyku založeném na Lisp).

Známé chyby v Audacity 1.1.1:

  * Nespouštìjte pøíkaz Benchmark na Windowsech - mùže zhavarovat.
    Pro vìtšinu uživatelù se jedná o nepotøebnou funci - je urèena
    vývojáøùm, aby posoudili rychlost programu na rùzných poèítaèích
    a operaèních systémech.

  * Mac OS X: èást nahrávky mùže být ztracena, pokud bìhem nahrávání
    stisknete tlaèítko myši nad menu a podržíte ho pøíliš dlouho.
    Zkuste bìhem nahrávání nechat Audacity napokoji, obdržíte lepší
    výsledky.

Nové vlastnosti v Audacity 1.1.1:

  * Uživatelské rozhraní
    - Tooltipy se objevují ve stavovém øádku
    - Svislý kurzor sleduje pøehrávání i nahrávání
    - Tlaèítko Pause
    - Nástroj Kreslení (s tøemi odlišnými režimy)
    - Svislá zmìna velikosti stereostop je veselejší
    - Pøizpùsobení výbìru pomocí kliknutí a tažení hranice výbìru
    - Kontextovì senzitivní vysvìcování tlaèítek na panelech
    - Lepší funkcionalita zvìtšování (centruje oblast)
    - Více zpùsobù zobrazení pozice kurzoru a výbìru
    - Režim výbìru "Snap-to"
    - Pøetahování stop nahoru a dolù
    - Zarovnávání a skupinové zarovnávání
    - Ukládání a obnovování kurzoru
    - Funkèní okno historie
  * Efekty
    - Efekty jsou rozdìleny do tøí menu: Generovat, Efekt, a
      Analyzovat
    - Menu Generovat vám umožní generovat ticho, šum nebo tón
    - Podpora pro Nyquist (podporuje psaní plug-inù v Nyquistu,
      interpretovaném funkcionálním jazyce založeném na Lisp)
  * Lokalizace
    - Vylepšená lokalizaèní podpora
    - K dispozici je více jazykù
    - Dialog výbìru jazyka pøi startu
  * Mac OS X
    - Podpora vìtšího množství audio hardware
    - Podpora plného duplexu (pøehrávání bìhem nahrávání)
    - Podpora pro export do MP3 s použitím LameLib Carbon
  * Unix
    - Audacity má nyní manuálovou stránku (popisuje øádkové pøíkazy
      a jak nastavit vyhledávací cestu)
  * Formáty souborù
    - Používá se libsndfile 1.0, která opravuje nìkteré chyby
      a zlepšuje výkon
  * Hledání souborù:
    - Na Windowsech a Mac OS, Audacity nyní hledá
      pøeklady v adresáøi "Languages" a všechny plug-iny
      v adresáøi "Plug-ins", relativnì k programu.
    - Na Unixu, Audacity vyhledává pøeklady v
      /share/locale a všechno ostatní v
      /share/audacity a také ve všech cestách 
      v promìnné prostøedí AUDACITY_PATH.

Nové vlastnosti v Audacity 1.1.0:

  * Základní zpracování zvuku:
    - Support for 24-bit and 32-bit sample formats
    - Automatic real-time resampling (using linear
        interpolation)
  * Efekty:
    - Support LADSPA plugins on Linux / Unix
  * Formáty souborù:
    - New XML-based Audacity project format
    - Full Ogg Vorbis support now (importing and exporting)
    - Export to any command-line programs on Unix
    - Support for reading and writing many more types of
        uncompressed audio files, including ADPCM WAV files.
  * Panely
    - New toolbar drawing code; automatically adopts your
        operating system's colors
    - New toolbar buttons (Skip to Start, Skip to End)
    - New Edit toolbar
    - Toolbar buttons disable when they're not available
  * Uživatelské rozhraní
    - Fully customizable keyboard commands
    - Autoscroll while playing or recording
    - New Ruler, used in main view and in
        FFT Filter effect
    - The waveform now displays the average value in a lighter
        color inside the peak values
  * Lokalizace
    - Audacity can now be localized to different foreign
      languages.

Nové vlastnosti v Audacity 1.1:

  * libmad for fast MP3 importing
  * libid3tag for editing MP3 file information
  * libsndfile to read and write more audio file formats
  * PortAudio for cross-platform audio playing and recording
</pre>

<?php BoxBottom(); ?>

