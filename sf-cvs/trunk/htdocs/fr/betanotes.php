<?php BoxTop("$releaseNotesStr $betaVersion"); ?>

<pre>
Problèmes connus avec Audacity 1.1.3:
  - Il place un fichier appelé "Audacity-Commands.xml" dans votre
    répertoire racine ou ailleurs sur votre disque dur. Ce fichier est
    nécessaire à Audacity 1.1.3 pour fonctionner. Nous sommes
    désolés d'emcombrer ainsi votre ordinateur et prometons de placer
    ce fichier à un meilleur endroit dans la prochaine version.

Nouvelles fonctionnalités d'Audacity 1.1.3:
  * Interface Utilisateur
    - Nouvelle barre d'outils de mixage vous permettant de contrôler le
      volume de sortie, le volume d'entrée, et la source entrante
      directement depuis Audacity.
    - Chaque piste a désormais son propre réglage de gain et de pan.

  * Fichiers d'E/S 
    - Utilisation d'un format de fichier amélioré (malheureusement,
      il n'est plus possible de lire l'ancien format de fichiers, y compris
      le 1.1.1) 
    - Les fichier blocs (stockés dans le répertoire du projet Audacity)
      Utilisent maintenant le format AU standard. Comme certaines 
      méta-informations d'Audacity se trouvent dans ces fichiers, il
      peuvent désormais être lus par beaucoup d'autres logiciels audio.
    - Certains bugs relatifs à la lecture et l'écriture de fichier de plus de
      16 bits de résolution ont été corrigés.
    - L'import des fichiers RAW est de nouveau fonctionnel, avec une
      interface graphique améliorée, mais supporte bien plus de formats
      de fichiers. Les algorythmes d'autodétection sont bien plus précis
      que dans la version 1.0

  * E/S Audio
    - E/S audio complétements réécrites, avec une latence plus basse
      et des chances minimum de buffer underruns pendant 
      l'enregistrement.

  * Rééchantillonage
    - Utilise des algorythmes de haute qualité, avec une option de 
      meilleure qualité pour le mixage que la lecture en temps réel.

    - Support préliminaire pour des pistes de temps, pour changer
      La vitesse de lecture sur un temps donné.

  * Plusieurs corrections de bugs et nouvelles fonctionnalités

Nouvelles fonctionnalités d'Audacity 1.1.2:
  * Interface utilisateur
    - Un bug à été corrigé dans la version Windows, pour les commandes
      du menu "Nom..." et "Séparer piste stéréo"/"Créer piste stéréo"
  * Effets
    - Support Nyquist sur Windows (support les plug-ins écrits en Nyquist,
      un language d'interprétation fonctionnel basé sur Lisp).

Bugs connus dans Audacity 1.1.1:

  * Ne lancez pas la commande test de performances sous Windows
    Il peut "planter". Ce n'est pas une commande utile pour la plupart des
    utilisateurs -  elle à été conçue pour les développeurs afin de juger
    des performances sur différentes machines et systèmes.

  * Mac OS X: De l'audio pourrait être perdue si vous tenez le bouton
    de la souris trop longtemps appuyé sur un menu pendant 
    l'enregistrement. Essayez de faire fonctionner Audacity seul pour
    de meilleurs résultats.

Nouvelles fonctionnalités d'Audacity 1.1.1:

  * Interface utilisateur
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

New libraries in Audacity 1.1:

  * libmad for fast MP3 importing
  * libid3tag for editing MP3 file information
  * libsndfile to read and write more audio file formats
  * PortAudio for cross-platform audio playing and recording
</pre>

<?php BoxBottom(); ?>