<pre>
Canvis a 1.2.0-pre3:

  * S'ha corregit un error pel qual l'exortació estava desactivda quan no
    hi havia res seleccionat.

  * S'han arreglat els problemes que es donaven en obrir l'Audacity en
    ordinadors amb targetes de so sense suport de mesclador.

  * S'ha corregit un error a "Importa dades crues".

  * Corregida la creació de noves pistes estèreo.

  * Canvis cosmètics per al Mac OS X.

  * S'ha afegit el suport a l'activador de filtres VST en Windows.

  * S'ha corregit l'error que es produïa en tancar l'Audacity amb el diàleg
    de preferències obert.

  * S'ha corregit un error de caràcters duplicats en les etiquetes de pistes
    en el Mac OS X.

  * El control de nivell d'enregistrament en Linux ara ajusta l'IGAIN,
    en comptes del nivell global de la font d'enregistrament.

  * S'ha corregit un error que causava corrupcio de dades en els
    enregistraments estèreo de q6 bits.

  * Fixed bug that caused data loss if you deleted all tracks in
    a saved project and then open a new file into the same window.

  * Added support for alternate audio button order (in Interface
    preferences)

  * Added preliminary support for wxX11

  * Added fully transparent Windows XP icon

  * Fixed crash if you try to record (or play) and no audio
    devices exist, or if the audio device doesn't support the
    mode you selected.

  * Audacity no longer sets the process priority to high while
    recording on Windows.  Users can still do this manually
    using the Task Manager.

  * Fixed bug that caused last ~100 ms of the selection to get
    cut off on Windows.

  * Fixed FFT Filter and Equalization effects dialogs.

  * Fixed bugs in Unix build system (DESTDIR in locale directory,
    choosing libsamplerate instead of libresample)

  * Support for LADSPA plug-ins on Windows added, and 
    three open source LADSPA plug-ins ported to Windows
    (GVerb reverb, SC4 compressor, and Hard Limiter)

Changes in 1.2.0-pre2:

  * Online help completed.  The full manual is nearly complete
    and will be posted to the website for online browsing shortly.

  * Audacity will no longer let you do unsafe editing operations
    while playing or recording.  This eliminates many potential
    crashes.

  * Fixed ability to cancel Quit button.

  * New resampling library, with no restrictions on the maximum or
    minimum rate of resampling.

  * Audacity now supports LADSPA plug-ins on all platforms, and
    supports VST plug-ins through an optional LADSPA plug-in
    called the "VST Enabler", which you can download separately.
    Because of licensing issues, Audacity cannot be distributed
    with VST support built-in.

  * Mac OS X keyboard shortcut problems have been fixed.

  * Mac OS X audio muting problems have been fixed.

  * Mac OS X playback/recording cursor sync problems have been fixed.

  * Silence now displays a straight line again, instead of nothing.

  * Added a vertical ruler to the Waveform dB display.

  * Fixed crash in Change Pitch.

  * You can now Paste if nothing is selected.

  * Canceling an Import operation doesn't cause an extra error
    dialog to appear.

  * Audacity now handles filenames with international characters
    correctly.

  * Now outputs ID3v2.3 tags (instead of ID3v2.4), to be
    compatible with more MP3 players.

  * Minor improvements to build system on Unix systems.

Noves característiques de l'Audacity 1.2.0-pre1:

  * Interfície de l'usuari
    - Ampliació vertical de les peces.
    - S'ha millorat l'aparença i localització de les barres d'eines.
    - Més cursors del ratolí.
    - Implementació completa de dreceres de teclat editables.
    - Troba zero-crossings.
    - La roda del ratolí pot ser utilitzada per apropar i allunyar.
    - Mode multi eina.
    - Amplificador utilitzant un entorn.
    - Les etiquetes poden emmagatzemar seleccions (com ara l'Audacity 1.0.0).

  * Efectes
    - Capacitat d'ordenar la repetició de l'últim efecte.
    - S'ha millorat la compatibilitat amb el pedaç VST.
    - Molts efectes ara tenen un botó de previsualització.
    - Compressor (Rang dinàmic de compressió).
    - Canvia el to (sense canviar el temps de compàs).
    - Canvia el temps de compàs (sense canviar el to).
    - Canvia la velocitat (canviant tant el temps com la velocitat).
    - Repeteix (útil per crear bucles).
    - Normalitza (ajusta el volum i DC bias).

  * Àudio Entrada/Sortida
    - ordre de previsualització d'un segon.
    - Reproducció en bucleLooped.

  * Fitxer Entrada/Sortida
    -L' Audacity 1.2.0 obre fitxers de projectes de totes les versions prèvies
      de l'Audacity des de 0.98 fins a 1.1.3.
    - Obre múltiples fitxers des del mateix diàleg.
    - Utilitza un fitxer de text per especificar un llistat de fitxers d'àudio a obrir amb offsets.

  * S'ha actualitzat el manual d'usuari.


Errors fixats en l'Audacity 1.2.0-pre1

  * Els fitxers de projectes amb caràcters especials han deixat de ser
    invàlids.

  * S'han fixat els sorolls de ratllades causats per mals truncaments .

  * L'Audacity ja no exporta fitxers Ogg invàlids i no talla els últims
    pocs segons dels fitxers Ogg exportats.

  * Els fitxers MP3 Mono exporten a la velocitat correcta.

  * S'ha fixat diversos resultats incorrectes des de l'eina d'entorn.

  * L'ordre de "Exportar etiquetes" ara sobreescriu correctament els
    fitxers existents.
  
  * Ara es mostra correctament el número correcte de l'octava de les notes.

  * S'ha fixat diverses pèrdues de memòria.

  * Molts altres petits erros fixats.
</pre>