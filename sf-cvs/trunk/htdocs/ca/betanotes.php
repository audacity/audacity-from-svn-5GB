<?php BoxTop("$releaseNotesStr $betaVersion"); ?>

<pre>
Nous errors de l'Audacity 1.1.0:

  * No engegueu l'ordre Benchmark en el Windows - pot petar.
    No és una característica útil per molts usuaris - els 
    desenvolupadors l'han col·locat per jutjar la velocitat del programa en
    diferents ordinadors i sistemes operatius.

Noves característiques de l'Audacity 1.1.0:

  * Core audio processing:
    - Permet l'ús de formats de 24-bit i 32-bit
    - Automatic real-time resampling (emprant interpolació lineal)
  * Efectes:
    - Pemet l'ús de pedaços LADSPA en Linux / Unix
  * Formats de fitxer:
    - Nou format e XML-based Audacity project format
    - Ara permet l'ús complet del Ogg Vorbis (importar i exportar)
    - Exporta a qualsevol programa de línies d'ordres de l'Unix
    - Permet llegir i escriure diversos tipus d'incompresos formats
        de fitxers d'àudio, incloent-hi els fitxers ADPCM WAV.
  * Barres d'eines
    - Nou codi de dibuix de la barra d'eines; adopta automàticament els colors
        del vostre sistema operatiu
    - Nous botons de la barra d'eines (Skip to Start, Skip to End)
    - Nova barra d'eines
    - Els botons estan inhabilitats quan no estan disponibles
  * Interfície de l'usuari
    - Ordres del teclat totalment personalitzables
    - Desplaçament automàtic mentre es reprodueix o grava
    - New Ruler, used in main view and in
        FFT Filter effect
    - La forma d'ona ara mostra el valor mig en un color lluminós dintre dels
        valors dels pics
  * Localització
    - Ara, l'Audacity pot ser localitzat en diferents llengües estrangeres.

Noves llibreries de l'Audacity 1.1:

  * libmad per una ràpida importació de MP3
  * libid3tag per editar la informació dels MP3
  * libsndfile per llegir i escriure més formats de fitxers d'àudio
  * PortAudio per gravar i reproduir àudio multiplataforma
</pre>

<?php BoxBottom(); ?>
