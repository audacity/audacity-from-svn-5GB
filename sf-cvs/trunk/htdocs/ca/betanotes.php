<?php BoxTop("$releaseNotesStr 1.1.3"); ?>

<pre>
Problemes coneguts de l'Audacity 1.1.3:
  - Es crea un fitxer anomenat "Audacity-Commands.xml" en el directori de l'usuari
    o en algun altre lloc de l'ordinador.  Aquest fitxer és necessari per
    tal que l'Audacity 1.1.3 pugui funcionar. Us demanem disculpes per destorbar
    el vostre ordinador i prometem col·locar aquest fitxer en un lloc més adient
    en la propera versió.

Noves funcionalitats de l'Audacity 1.1.3:
  * Interfície d'usuari
    - Nova barra del mesclador, que us permet controlar el volum de sortida
      el volum i la font d'entrada directament des de l'Audacity.
    - Ara cada pista té els seus controls de guany i panoràmica.

  * E/S de fitxers
    - Ús d'un nou format millorat de fitxer de projecte.  (Desafortunadament, la lectura
      dels formats antics, incloent l'1.1.1, no està suportada.)
    - Els fitxers de blocs (emmagatzemats en les carpetes de projecte de l'Audacity) ara
      fan servir el format estàndard AU. Tot i que l'Audacity col·loca algunes meta-dades
      en aquests fitxers, ara es poden llegir també amb altres programes d'àudio.
    - S'han arreglat algunes errades relatives a la lectura i escriptura de fitxers
      d'àudio amb més de 16 bits per mostra.
    - La importació de dades crues (RAW) torna a ser funcional, amb una interfície d'usuari
      senzilla però que suporta molts més formats de fitxer. Els algorismes de detecció
      automàtica són molt més acurats que e la versió 1.0.

  * E/S d'àudio
    - S'ha rescrit totalment l'E/S d'àudio, obtenint una latència mínima i reduint dràsticament
      la possibilitat de provocar un desbordament del magatzem de memòria mentre s'enregistra.

  * Delmatge (resampling)
    - Ús d'algorismes de delmatge d'alta qualitat, amb l'opció
      d'obtenir una qualitat superior en mesclar que reproduint en temps real.

    - Suport preliminar a les pistes de temps, per canviar la velocitat
      de reproducció al llarg del temps.

  * Moltes més correccions i noves funcionalitats


Noves funcionalitats de l'Audacity 1.1.2:
  * Interfície d'usuari
    - S'ha corregit una errada en la versió Windows, relativa a les instruccions del menú "pista"
	  "Nom..." i "Separar una pista estèreo"/"Convertir en pista estèreo".
  * Efectes
    - Suport de Nyquist a Windows (suporta complements escrits en
	  Nyquist, un llenguatge funcional interpretat que es basa en el Lisp).


Errors coneguts de l'Audacity 1.1.1:

  * No engegueu l'ordre Benchmark en la versió per a Windows - podria penjar-se.
    No és una funcionalitat útil per a la majoria d'usuaris - està pensada
    perquè els desenvolupadors puguin valorar la velocitat del programa en diferents
    ordinadors i sistemes operatius.

  * Mac OS X: es pot perdre àudio si premeu el botó del ratolí
    durant molta estona sobre un menú mentre s'enregistra. Per obtenir bons resultats intenteu
    deixar l'Audacity com a única aplicació funcionant mentre s'enregistra.

Noves funcionalitats de l'Audacity 1.1.1:

  * Interfície d'usuari
    - Els consells apareixen a la barra d'estat.
    - El cursor vertical segueix el punt de reproducció o enregistrament
    - Botó de pausa
    - Eina de dibuix (amb tres modalitats diferents)
    - El redimensionament vertical de les pistes estereofòniques és més agradable.
    - Ajustament de la selecció clicant i arrossegant els límits.
    - Activació/Desactivació dels botons de la barra d'eines segons el context.
    - Funció de zoom millorada (centra la regió)
    - Diverses maneres de mostrar la posició del cursor i la selecció
    - Mode de selecció "Enganxat a"
    - Arrossegament de pistes amunt i avall
    - Funcions d'alineament
    - Desar i recuperar el cursor
    - Finestra d'historial de treball
  * Efectes
    - Els efectes s'organitzen en tres menús: Generació, Efecte, i
      Anàlisi
    - El menú de generació permet crear silenci, soroll o un to
    - Suport del Nyquist (se suporten els complements escrits en Nyquist,
      un llenguatge funcional interpretat basat en el Lisp)
  * Localització
    - Suport millorat a la localització
    - Més idiomes disponibles
    - Diàleg de selecció d'idioma en engegar
  * Mac OS X
    - Més maquinari d'àudio suportat
    - Suport del full-duplex (interpretar mentre s'enregistra)
    - Suport de l'exportació a MP3 mitjançant la biblioteca LameLib Carbon
  * Unix
    - L'Audacity té ara una pàgina "man" (s'hi descriuen les opcions de línia d'ordres
      i com indicar els camins de cerca)
  * Formats de fitxers
    - Ús del libsndfile 1.0, que arregla alguns errors i millora
      el rendiment
  * Cerca de fitxers:
    - En Windows i Mac OS, l'Audacity busca ara les traduccions a la
      carpeta "Languages", i tots els complements 
      a la carpeta "Plug-ins", relativa a la ubicació del programa.
    - A Unix, l'Audacity cerca les traduccions a
      <prefix>/share/locale i cerca qualsevol cosa
      a <prefix>/share/audacity així com en tots els camins indicats
      a la variable d'entorn AUDACITY_PATH


Noves funcionalitats de l'Audacity 1.1.0:

  * Procés d'àudio:
    - Permet l'ús de formats de 24-bit i 32-bit
    - Automatic real-time resampling (emprant interpolació lineal)
  * Efectes:
    - Pemet l'ús de pedaços LADSPA en Linux / Unix
  * Formats de fitxer:
    - Nou format basat en XML per als projectes Audacity
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
    - Desplaçament automàtic mentre es reprodueix o s'enregistra
    - Nou Ruler, que s'utilitza en la finestra principal i en el filtre FFT
    - La forma d'ona ara mostra el valor mig en un color lluminós dintre dels
      valors dels pics
  * Localització
    - Ara, l'Audacity pot ser localitzat en diferents idiomes.

Noves biblioteques de l'Audacity 1.1.0:

  * libmad per una ràpida importació de MP3
  * libid3tag per editar la informació dels MP3
  * libsndfile per llegir i escriure més formats de fitxers d'àudio
  * PortAudio per enregistrar i reproduir àudio multiplataforma
</pre>

<?php BoxBottom(); ?>
