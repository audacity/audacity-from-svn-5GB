<pre>
Canvis a 1.2.2-pre1:

  * S'han afegit mesuradors de nivell (VU Meters) tant per a la reproducció
    com a l'enregistrament. Es pot fer clic al mesurador d'enregistrament per
    ajustar el nivell d'entrada abans de començar a enregistrar.

  * Exportació múltiple - nova funcionalitat que us permet exportar diversos
    fitxers al mateix temps, ja sigui per pista o a partir de 
    fragments delimitats per etiquetes.

  * El programa intentarà corregir automàticament la latència en
    enregistraments de tipus full-duplex. (Això encara no funciona
    perfectament, i no està suportat en tots els sistemes. Ho millorarem en 
    futures versions.)

  * S'ha arreglat un seriós error que podia provocar la pèrdua de dades
    quan es desava i es tornava a obrir un projecte Audacity que contingués
    dades repetides o duplicades.

  * El diàleg d'etiquetes MP3 només es mostra la primera vegada que exporteu 
    a MP3; a partir d'aquí ja no apareix més, sempre i quan hi hagueu omplert
    com a mínim un camp.

  * Ara podeu afegir una etiqueta a la posició actual de reproducció, des del 
    menú de Projecte o amb les tecles Ctrl+M.

  * Quan feu clic en una etiqueta es seleccionen totes les pistes, simplificant
    l'ús de les etiquetes per a recuperar seleccions.

  * Windows: S'ha arreglat un error en la funció "Fixar resolució" de la 
    pista de temps.

  * S'ha arreglat un error que causava problemes amb enregistraments de
    més de 45 minuts en alguns sistemes Windows.

  * Mac OS X: S'ha millorat el suport per a l'iMic de Griffin, arreglant
    un error que feia que sempre enregistrés en mono en comptes d'estèreo.

  * S'ha afegit suport per a "playthrough" mitjançant software (escoltar
    el que s'està enregistrant mentre s'enregistra, o mentre s'ajusta el
    nivell d'entrada). Això fa possible, per exemple, enregistrar fent
    servir un dispositiu d'àudio mentre s'escolta el resultat en un altre
    dispositiu.

  * Unix/Linux: Arreglat el bloqueig del ratolí quan un dispositiu d'àudio
    es penja. Ara l'Audacity pot deixar de respondre, però ja no congela
    tot el sistema X.

  * S'ha arreglat un error cosmètic que feia que les gràfiques d'ona es
    veiessin malament quan obríeu un projecte Audacity fet en una altra
    plataforma (per exemple, copiant un projecte d'un Mac a un PC).

  * S'ha arreglat un error que podia causar inestabilitat quan s'enganxava, 
    es dividia o es duplicava la pista d'etiquetes.

  * Ara es pot canviar el tipus de lletra de la pista d'etiquetes mitjançant
    l'opció "Tipus de lletra..." del seu menú emergent.

  * S'ha afegit un suport bàsic a la impressió. Actualment s'escala tot el
    projecte per encabir-lo en una pàgina. Proveu amb l'orientació de paper
    apaisada per a obtenir millors resultats.

  * Mac OS X i Windows: L'Audacity ve amb la nova versió (1.0.1)
    del codificador Ogg Vorbis.  La compressió Vorbis tindrà ara una millor
    qualitat i permetrà obtenir fitxers més petits.

  * S'ha arreglat un error que ocasionalment podia provocar penjades quan
    s'aplicaven efectes a pistes creades a partir d'una divisió.

  * Altres errors menors i millores de funcionament.

</pre>
