<?php BoxTop("$releaseNotesStr 1.1.3"); ?>

<pre>
Kendte problemer i Audacity 1.1.3:
  - Der bliver placeret en fil ved navn "Audacity-Commands.xml" i din 
    hjemmemappe eller et lignende sted på din computer. Denne fil er nødvendig
    for at audacity 1.1.3 kan køre. Vi beklager at måtte lægge ekstra filer ind
    på din computer og lover at lægge den et mere passende sted i fremtiden.

Nye faciliteter i Audacity 1.1.3:
  * Burgerfladen
    - Ny Mixer værktøjslinje lader dig styre udgangsvolumen, indgangsvolumen 
      og indgangskilde direkte fra Audacity. 
    - Hvert spor har nu individuelle gain og pan kontroller.

  * Fil I/O
    - Bruger et nyt og bedre projektfilformat (Desværre er det dermed umuligt at
      læse de ældre formater, inklusive version 1.1.1.)
    - Blokfiler (gemmes i Audacity projektmapperne) bruger nu det normale AU 
      format. Selvom der findes nogen Audacity meta-information i disse filer, 
      kan de alligevel læses af et bredt udvalg af lydredigeringsprogrammer.
    - Rettede nogle fejl i forbindelse med læsning/skrivning af lydfiler med
      mere end 16 bits per datapunkt.
    - Import af RAW-filer fungerer igen, med en enklere brugerflade og
      understøttelse af flere filformater. Automatisk filtypebestemmelse 
      fungerer langt bedre end i version 1.0.

  * Audio I/O
    - Fuldstændig nyskrevet audio I/O, med hurtigere reaktionstid og 
      minimal risiko for buffer underløb under optagelse.

  * Frekvenskonvertering
    - Bruger algoritmer med bedste lydkvalitet, med mulighed for at mixe i
      bedre kvalitet end under prøvelytning.

    - Nogen understøttelse af tidsspor, så afspilningshastighed kan varieres           løbende.

  * Mange fejlrettelser og nye faciliteter.


Nye faciliteter i Audacity 1.1.2:
  * Brugerflade
    - Rettede fejl i Windowsudgaven, i kommandoerne 
	  "Navn..." og "Opdel stereospor"/"Lav stereospor".
  * Effekter
    - Nyquist understøttes i Windows (plug-ins skrevet i Nyquist,
	  et fortolket funktionelt sprog baseret på LISP).


Known bugs in Audacity 1.1.1:

  * Do not run the Benchmark command on Windows - it may crash.
    It is not a useful feature for most users - it is intended
    for developers to judge the speed of the program on different
    computers and operating systems.

  * Mac OS X: some audio may be lost if you hold the mouse button
    for too long over a menu while recording.  Try to leave Audacity
    alone while recording for best results.

New features in Audacity 1.1.1:

  * User Interface
    - Tooltips appear in Statusbar.
    - Vertical cursor follows play/record
    - Pause button
    - Drawing tool (with three different modes)
    - Vertical Resizing of stereo tracks is more fun.
    - Adjust selection by click-dragging selection boundary
    - Toolbar button context-sensitive enabling/disabling
    - Better zooming functionality (centers region)
    - Multiple ways to display the cursor position and selection
    - Snap-to selection mode
    - Drag tracks up and down
    - Align and group align functions
    - Cursor save/restore
    - Working history window
  * Effects
    - Effects broken down into three menus: Generate, Effect, and
      Analyze
    - Generate menu lets you generate silence, noise, or a tone
    - Nyquist support (supports plug-ins written in Nyquist,
      an interpreted functional language based on Lisp)
  * Localization
    - Improved localization support
    - More languages available
    - Language selection dialog on startup
  * Mac OS X
    - Support for more audio hardware
    - Support for full-duplex (play while recording)
    - Support for MP3 exporting using LameLib Carbon
  * Unix
    - Audacity now has a man page (it describes command-line
      options and how to set the search path)
  * File Formats
    - Uses libsndfile 1.0, which fixes some bugs and
      improves performance
  * Searching for Files:
    - On Windows and Mac OS, Audacity now looks for
      translations in the "Languages" folder and all plug-ins
      in the "Plug-ins" folder, relative to the program.
    - On Unix, Audacity looks for translations in
      <prefix>/share/locale and looks for everything else
      in <prefix>/share/audacity and also in any paths in
      the AUDACITY_PATH environment variable


New features in Audacity 1.1.0:

  * Core audio processing:
    - Support for 24-bit and 32-bit sample formats
    - Automatic real-time resampling (using linear
        interpolation)
  * Effects:
    - Support LADSPA plugins on Linux / Unix
  * File formats:
    - New XML-based Audacity project format
    - Full Ogg Vorbis support now (importing and exporting)
    - Export to any command-line programs on Unix
    - Support for reading and writing many more types of
        uncompressed audio files, including ADPCM WAV files.
  * Toolbars
    - New toolbar drawing code; automatically adopts your
        operating system's colors
    - New toolbar buttons (Skip to Start, Skip to End)
    - New Edit toolbar
    - Toolbar buttons disable when they're not available
  * User Interface
    - Fully customizable keyboard commands
    - Autoscroll while playing or recording
    - New Ruler, used in main view and in
        FFT Filter effect
    - The waveform now displays the average value in a lighter
        color inside the peak values
  * Localization
    - Audacity can now be localized to different foreign
      languages.

New libraries in Audacity 1.1.0:

  * libmad for fast MP3 importing
  * libid3tag for editing MP3 file information
  * libsndfile to read and write more audio file formats
  * PortAudio for cross-platform audio playing and recording
</pre>

<?php BoxBottom(); ?>
