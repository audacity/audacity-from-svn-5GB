<h2>Canvis a l'Audacity 1.2</h2>

Principals canvis des de l'anterior versió estble, Audacity 1.0.0:

<h3>Àudio de qualitat professional</h3>

<ul>
<li>
L'Audacity pot ara enregistrar i eitar mostres de 24 bits i 32 bits (coma flotant).  
En un mateix projecte poden coexisitr pistes amb diferents freqüències i formats, i l'Audacity 
les convertirà n temps real quan sigui necessari.  En totes les conversions es fan servir tècniques 
d'arrodoniment d'alta qualitat.
</li>

<li>
L'entrada i sortida de so de l'Audacity han estat millorades.  L'Audacity pot ara enregistrar més de 
dos canals alhora. L'ús de memòria cau ha estat millorat, per reduir les possibilitats de trobar-se 
amb situacions de desbordament de memòria.
</li>
</ul>

<a href="screenshots.php<?php print $langQueryStr; ?>">
<img alt="screenshots" border="0" src="images/screen/linux/change-pitch.png" align="right"></a>
<h3>Effects</h3>

<ul>
<li>
Tres nous efectes que poden canviar el to i el tempo d'una pista:
 <ul>
  <li><b>Canvia el to</b> puja o baixa el to del fragment seleccionat, sense que es vegi afectada la velocitat.</li>
  <li><b>Canvia el temps</b> fa que el fragment seleccionat s'escolti més de pressa o més poc a poc, sense modificar-ne el to.</li>
  <li><b>Canvia la velocitat</b> afecta tant la velocitat de reproducció com el to, tal com passa si canvieu la velocitat de gir d'un tocadiscs.</li>
 </ul>
</li>

<li>
La major part d'efectes inclouen un botó de vista preliminar, que us permet provar 
diferents configuracions sense tancar la finestra d'efectes.  Una nova funció us permet 
repetir el darrer efecte sense haver de tornar a obrir la finestra.
</li>

<li>
Altres efectes inclouen:
 <ul>
  <li>Compresor, per compressió de rang dinàmic.</li>
  <li>Repeteix, per escoltar les mostres en cicle.</li>
  <li>Normalitza, per ajustar el volum i corregir la desviació del punt central de l'ona.</li>
 </ul>
</li>
</ul>


<h3>Noves funcionalitats d'edició</h3>

<ul>
<li>
L'eina d'envolupant, emprada per produir fàdings d'inici i final, es pot fer servir ara també 
per a pujar o baixar el volum global de les pistes.
</li>

<li>
La nova funcionalitat "Pista de temps" és similar a l'envolupant de volum, però en lloc de canviar la 
intensitat del so srveix per a produir canvis progressius en la velocitat d'interpretació de la pista.
</li>

<li>
Cada pista té ara els seus propis controls de guany i panoràmica, per facilitar les mescles.
<a href="screenshots.php<?php print $langQueryStr; ?>"><img alt="screenshots" border="0" src="images/screen/linux/track-controls.png" align="right"></a>
</li>

<li>
L'Audacity pot trobar punts de creuament amb el zero, per ajudar a crear talls i bucles suaus.
Premeu "Z" per moure els límits de la selecció als punts de creuament amb el zero més propers.
</li>
</ul>


<h3>Endollats</h3>

<ul>
<li>
En Linux, l'Audacity pot carregar ara endollats <a href="http://www.ladspa.org/">LADSPA</a>.
</li>

<li>
L'Audacity 1.2 ofereix un llenguatge de procés de senyals digitals anomenat 
<a href="nyquist.php">Nyquist</a>, que permet als usuaris programar nous efectes 
en un llenguatge similar al LISP.
</li>
</ul>


<h3>Importació i exportació de fitxers</h3>

<ul>
<li>
Els fitxers de projecte de l'Audacity 1.2 fan servir un nou format XML.  L'Audacity 1.2 
obrirà i convertirà automàticament els fitxers de projecte de versions anteriors.
</li>

<li>
L'Audacity 1.2 fa servir la biblioteca <a href="http://www.underbit.com/products/mad/">libmad</a>
per a una descompressió ràpida dels fitxers MP3.  La biblioteca <a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a>,
 d'Erik de Castro Lopo, ofereix una compatibilitat millorada amb la majoria de formats de fitxers d'àudio no comprimit.
</li>

<li>
Les finestres de diàleg d'importar i obrir permeten ara als usuaris de l'Audacity seleccionar 
diversos fitxers cada vegada, i obrir-los users junts en un mateix projecte.  
Un nou format de fitxer "LOF" permet a l'Audacity obrir grups de fitxers, fins i tot indicant punts de desplaçament (offsets), 
mitjançant una llista escrita en un fitxer de text.
</li>
</ul>


<h3>Interfície d'usuari millorada</h3>

<a href="screenshots.php<?php print $langQueryStr; ?>"><img alt="screenshots" border="0" src="images/screen/macosx/main-toolbar.png" align="right"></a>
<ul>
<li>
Noves barres d'edició i mesclador, per un accés més ràpid a les funcions més comunes.
</li>

<li>
La nova eina de dibuix permet ajustar les mostres una per una, quan s'està en la modalitat de zoom total. 
La nova eina d'usos múltiples ofereix un accés ràpid a diverses funcionalitats d'edició sense que calgui 
canviar d'eina.
</li>

<li>
S'hn afegit moltes drecres de teclat, que poden ser personalitzades.
</li>

<li>
Noves funcions:
 <ul>
  <li>Interpretació en bucle.  Premeu "L", o mantingueu premuda la tecla de majúscules mentre feu clic a "Interpreta".</li>
  <li>Premeu "1" per escoltar un fragment d'un segon de durada de l'àudio que hi ha al voltant del cursor.</li>
 </ul>
</li>

<li>
La roda del ratolí es pot fer servir per a fer zoom cap a dins i cap a fora.
</li>

<li>
Les pistes es poden augmentar verticalment fent clic o arrossegant les barres verticals. Feu majúscules-clic 
o clic amb el botó dret per fer zoom enrere.  
</li>

<li>
La barra d'estat pot mostrar ara el temps en diversos formats, incloent 
segons, mostres o fotogrames de vídeo.
</li>

<li>
L'interfície de l'Audacity es pot traduir ara a idiomes diferents de l'anglès.  Podeu fer-vos voluntaris 
per ajudar a <a href="translation/">traduir l'Audacity</a> al vostre idioma.
</li>
</ul>
