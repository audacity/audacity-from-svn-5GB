<h2>Canvis des de l'Audacity 1.0</h2>

Aquesta secció llista els canvis més importants des de la nostra última versió estable que va ser el llançament de l'Audacity 1.0.0.   Els usuaris de les nostres versions beta haurien de visitar les notes de llançament de
<a href="<?php print "betanotes.php$langQueryStr"; ?>">canvis des de la versió 1.1.3 de l'Audacity</a>.

<h3>Qualitat professional d'Àudio</h3>

<ul>
<li>
Audacity can now record and edit 24-bit and 32-bit (floating-point)
samples.  Tracks with different sample rates and formats can exist in
the same project, and Audacity will convert them in realtime whenever
necessary.  High-quality dithering and resampling is used for all
conversions.  Resampling now uses algorithms from the
<a href="http://www.mega-nerd.com/SRC/">SRC</a> library by
Erik de Castro Lopo.
</li>

<li>
S'ha millorat el so d'entrada i de sortida de l'Audacity.  L'Audacity pot ara gravar més de dos canals en un.  Latency is reduced, for lower
chances of skipping and buffer underruns.
</li>
</ul>


<h3>Efectes</h3>

<ul>
<li>
Three new effects change the pitch and tempo of a track:
 <ul>
  <li>Change Pitch raises or lowers the tone of a selection, without
  affecting the speed.</li>
  <li>Change Tempo makes the selection play faster or slower, without
  altering the pitch.</li>
  <li>Change Speed alters both the playback speed and the pitch, just
  like changing the speed of a turntable or tape player.</li>
 </ul>
</li>

<li>
Hi ha molts efectes que ara inclouen el botó de "Previsualització" que permet de triar diferents paràmetres sense tancar la finestra d'efecte.  Una nova ordre us permet de repetir l'últim efecte sense tornar a obrir la finestra.
</li>

<li>
Altres efectes nous inclosos:
 <ul>
  <li>Compressor, per a un grau dinàmic de compressió.</li>
  <li>Repetidor, per mostres en bucle.</li>
  <li>Normalitzador, per ajustar el volum i corregir bias DC.</li>
 </ul>
</li>
</ul>


<h3>Noves característiques d'edició</h3>

<ul>
<li>
L'eina d'entorn, used for smoothly fading tracks in and out, can now be
used to make tracks louder than their original volume as well as quieter.
</li>

<li>
The new "Time track" feature is similar to the volume envelope, but
instead changes the playback speed smoothly as a track plays.
</li>

<li>
Each track now has its own Gain and Pan controls, for easier mixing.
</li>

<li>
Audacity can find zero-crossings, to help create smooth cuts and loops.
Press "Z" to move selection edges to the nearest zero-crossings.
</li>
</ul>


<h3>Pedaços</h3>

<ul>
<li>
En Linux, l'Audacity pot carregar pedaços <a href="http://www.ladspa.org/">LADSPA</a>.
</li>

<li>
Audacity 1.2 features a digital signal processing language called
<a href="nyquist.php">Nyquist</a>, which allows users to program new effects
in a LISP-like language.
</li>
</ul>


<h3>Importació i exportació de fitxer</h3>

<ul>
<li>
Els fitxers de projecte de l'Audacity 1.2 utilitzen un nou format de fitxer XML.  L'Audacity 1.2 obrirà  i convertirà automàticament fitxers de projecte dels llançaments anteriors.
</li>

<li>
L'Audacity 1.2 utilitza <a href="http://www.underbit.com/products/mad/">libmad</a>
per descodificar fitxers MP3 més de pressa.  Erik de Castro Lopo's
<a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a> habilita la compatibilitat amb diversos formats no comprimits de fitxers d'àudio.
</li>

<li>
L'última versió de les llibreries <a href="http://www.vorbis.com/">Vorbis</a>
són utilitzades per a una codificació d'alta qualitat en Ogg Vorbis.
</li>

<li>
The import and open dialogs now allow Audacity users to select several
audio files at once, and open them into a single project.  A new "LOF"
file format provides a way for Audacity to open a group of files with
offsets listed in a text file.
</li>
</ul>


<h3>Interfície d'usuari millorada</h3>

<ul>
<li>
Noves barres d'eines d'edició i barreja per accedir abans a funcions comunes.
</li>

<li>
La nova eina de dibuix us permet l'ajustament de mostres individuals quan estan totalment aproximades.  El nou mode de multi eina permet l'accés ràpid a diferents funcions d'edició sense necesitar les eines de connexió.
</li>

<li>
S'hi han afegit moltes noves ordres de teclat, i dreceres de teclat que poden ser personalitzades.
</li>

<li>
Noves ordres:
 <ul>
  <li>reproducció en bucle.  Teclegeu "L", o pitgeu Maj mentre cliqueu a reproducció.</li>
  <li>Teclegeu "1" per reproduir 1 segon de previsualització d'àudio a on sigui el cursor.</li>
 </ul>
</li>

<li>
La rodeta del ratolí pot ser utilitzada per augmentar i per disminuir.
</li>

<li>
Les peces es poden ampliar verticalment clicant i arrossegant en els regles verticals.  Maj-clic o clic amb la dreta per disminuir.  
</li>

<li>
El regla i la barra d'estat ara poden mostrar el temps en diversos formats diferents, incloent-hi segons, mostres, or marcs de vídeo.
</li>

<li>
La interfície de l'Audacity ara es pot traduir a diferents idiomes a més de l'anglès.  Hi podeu ajudar <a href="translation/">traduint l'Audacity</a> al vostre idioma natiu.
</li>