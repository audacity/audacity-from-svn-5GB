<?php BoxTop("$releaseNotesStr $betaVersion"); ?>

<pre>
Errores(bugs) conocidos en Audacity 1.1.1:

  * No corras el comando Benchmark (test de rendimiento) en
    Windows - puede terminar el programa. No es una opción muy
    útil para la mayoría de usuarios - está pensada para que los
    desarrolladores juzguen la velocidad del programa en distintas
    computadoras y sistemas operativos.

  * Mac OS X: puede perderse audio si mantienes el botón del mouse
    demasiado tiempo sobre un menú mientras grabas. Intenta liberar
    a Audacity mientras grabas para mejores resultados.

Nuevas características en Audacity 1.1.1:

  * Interface del Usuario
    - Aparecen *Tooltips* en la Barra de Estado.
    - Cursor vertical sigue reproducción/grabación
    - Botón de Pausa
    - Herramienta de Dibujo (con tres modos diferentes)
    - Cambio de tamaño vertical de pistas estéreo es más amigable.
    - Ajustar selección cliqueando-arrastrando límite de selección
    - Habilitar/deshabilitar Barra de Herramientas sensible a
      contexto
    - Mejor funcionalidad de Zoom (centra la región)
    - Múltiples formas de mostrar la posición del cursor y
      selección.
    - Modo de selección Saltar-Hacia
    - Arrastrar pistas hacia arriba y abajo
    - Funciones alinear y alinear grupo
    - Guardar/restaurar cursor
    - Ventana de historia de trabajo
  * Efectos
    - Efectos repartidos en tres menúes: Generar, Efectos, y
      Analizar
    - El menú Generar te permite generar silencio, ruido, o un
      tono
    - Soporte para Nyquist (soporta plug-ins escritos en Nyquist,
      un lenguaje funcional interpretado basado en Lisp)
  * Localización
    - Soporte mejorado de localización
    - Más lenguajes disponibles
    - Cuadro de diálogo para selección de lenguaje al comenzar.
  * Mac OS X
    - Soporte para mas hardware de audio.
    - Soporte para full-duplex (reproducir mientras grabas)
    - Soporte para exportación de MP3 utilizando LameLib Carbon
  * Unix
    - Audacity ahora tiene una *man page* (describe opciones de
      línea de comandos y cómo configurar el path de búsqueda)
  * Formatos de Archivo
    - Utiliza libsndfile 1.0, que arregla algunos errores(bugs)
      y mejora el rendimiento
  * Búsqueda de Archivos:
    - En Windows y Mac OS, Audacity ahora busca traducciones en
      la carpeta "Languages" y todos los plug-ins en la carpeta
      "Plug-ins", relativas al programa.
    - En Unix, Audacity busca traducciones en
      <prefijo>/share/locale y busca por todo lo demás
      en <prefijo>/share/audacity y también en cualquier path en
      la variable de entorno AUDACITY_PATH

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