<pre>
Canvis a 1.2.0-pre3:

  * Arreglat un error que feia que l'ordre "Exporta" es veiés en gris
    quan no hi havia res seleccionat.

  * Arreglat un error que apareixia quan s'obria l'Audacity en un ordinador
    amb targeta de so sense suport de mesclador.

  * Arreglada la importació de dades crues.

  * Arreglada l'ordre "Nova pista estèreo".

  * Canvis cosmètics per al Mac OS X.

  * Afegit el suport a l'habilitador VST en Windows.

  * Arreglat l'error que es produïa si es tancava l'Audacity amb el diàleg
    de preferències obert.

  * Arreglat l'error de lletres duplicades en les etiquetes de pistes 
    amb Mac OS X.

  * El nivell d'enregistrament en Linux ara ajusta l'IGAIN,
    en comptes d'actuar sobre el nivell de la font d'enregistrament.

  * Arreglat un error que causava corrupció de dades en enregistraments
    estèreo de 16 bits.

  * Arreglat l'error que provocava pèrdua de dades si esborràveu totes 
    les pistes d'un projecte desat i obríeu un nou fitxer a la 
    mateixa finestra.

  * Afegit el suport al botò altern d'àudio (a les preferències d'interfície)

  * Afegit el suport preliminar a wxX11

  * Afegida una icona transparent per a Windows XP

  * Arreglat l'error que es produia si intentàveu enregistrar (o reproduir)
    sense tenir deispositius d'àudio, o si els dispositius no suportaven
    el mode seleccionat.

  * L'Audacity ja no es col·loca com a procés d'alta prioritat quan enregistra
    en Windows.  Els usuaris poden continuar fent això des de
    l'administrador de tasques.

  * Arreglat un error que causava la pèrdua dels darrers ~100 ms de la
    selecció quan es retallava, en Windows.

  * Arreglats els diàlegs dels efectes "Filtre FFT" i "Equalització".

  * Arreglats errors en el sistema de muntatge en Unix (DESTDIR al directori
    local, escollint libsamplerate en comptes de libresample)

  * Afegit el suport als complements LADSPA en Windows, i 
    tres complements LADSPA de codi obert portats a Windows
   (reverberador GVerb, compressor SC4, i Hard Limiter)

Canvis a 1.2.0-pre2:

  * Ajuda en línia completada. El manual està gairebé acabat i es
    col·locarà a la web per poder ser consultat en breu.

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