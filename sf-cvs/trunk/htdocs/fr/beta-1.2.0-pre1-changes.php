<h2>Changes Since Audacity 1.0</h2>

Cette section liste les changements majeurs depuis notre dernière version stable, Audacity 
1.0.0.  Les utilisateurs de notre version bêta devraient consulter les notes
<a href="<?php print "betanotes.php$langQueryStr"; ?>">changements depuis Audacity 1.1.3</a>.

<h3>Qualité audio professionnelle</h3>

<ul>
<li>
Audacity peut maintenant enregistrer et éditer des échantillons en 24-bit et 32-bit (virgule-flottante).  Des pistes avec différent taux d'échantillonage et formats peuvent exister dans le même projet, et Audacity les convertira en temps réel quand c'est nécessaire. Un dithering et un rééchantillonage de haute qualité sont utilisés pour toutes les conversions.
Le rééchantillonage utilise maintenant les algorythmes de la librairie
<a href="http://www.mega-nerd.com/SRC/">SRC</a> par
Erik de Castro Lopo.
</li>

<li>
L'entrée et la sortie son d'Audacity a été améliorée.  Audacity peut maintenant enregistrer plus de deux canaux à la fois. La latence est réduite, pour réduire les risques de saut et de buffer underruns.
</li>
</ul>


<h3>Effets</h3>

<ul>
<li>
Trois nouveaux effets changent la hauteur et le tempo de la piste:
 <ul>
  <li>Changer Hauteur augmente ou diminue la tonalité de la sélection, sans affecter la vitesse.</li>
  <li>Changer Tempo augmente ou diminue la vitesse de lecture, sans altérer la hauteur.</li>
  <li>Changer Vitesse altère et la vitesse et la hauteur, exactement comme en changeant la vitesse d'une platine tourne-disque ou K7.</li>
 </ul>
</li>

<li>
La plupart des effets incluent maintenant un bouton "Prévisualisation", qui permet d'essayer différent réglages sans fermer la fenêtre d'effet. Une nouvelle commande vous permet de répêter le dernier effet sans réouvrir la fenêtre.
</li>

<li>
Autres nouveaux effets inclus:
 <ul>
  <li>Compresseur, pour compression dynamique.</li>
  <li>Repeat, pour faire des boucles.</li>
  <li>Normaliser, pour ajuster le volume et corriger le DC bias.</li>
 </ul>
</li>
</ul>


<h3>Nouvelles fonctions d'édition</h3>

<ul>
<li>
L'outil d'enveloppe, utilisé pour adoucir les transitions des Fades entre pistes, peut maintenant être utilisé pour augmenter ou diminuer le volume des pistes.
</li>

<li>
La nouvelle fonction "Time track" est similaire à l'enveloppe de volume mais change la vitesse de lecture en douceur pendant la lecture.
</li>

<li>
Chaque piste a maintenant son propre volume de Gain et de Panoramique, pour un mixage facilité.
</li>

<li>
Audacity peut trouver les zero-crossings, pour aider à créer des coupures et des boucles sans-à-coups. Pressez "Z" pour déplacer les bords de la sélection aux plus proches zero-crossings.
</li>
</ul>


<h3>Plugins</h3>

<ul>
<li>
Sous Linux, Audacity peut maintenant charger les plugins <a href="http://www.ladspa.org/">LADSPA</a>.
</li>

<li>
Audacity 1.2 comprend un language de traitement du signal numérique appelé 
<a href="nyquist.php">Nyquist</a>, qui permet à l'utilisateur de programmer de nouveaux effets dans un language similaire à LISP.
</li>
</ul>


<h3>Importation et exportation de fichiers</h3>

<ul>
<li>
Audacity 1.2 project files use a new XML file format.  Audacity 1.2 will
automatically open and convert project files from earlier releases.
</li>

<li>
Audacity 1.2 uses <a href="http://www.underbit.com/products/mad/">libmad</a>
for much faster decoding of MP3 files.  Erik de Castro Lopo's
<a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a> provides
improved compatibility with many uncompressed audio file formats.
</li>

<li>
The latest version of the <a href="http://www.vorbis.com/">Vorbis</a>
libraries are used, for higher-quality Ogg Vorbis encoding.
</li>

<li>
The import and open dialogs now allow Audacity users to select several
audio files at once, and open them into a single project.  A new "LOF"
file format provides a way for Audacity to open a group of files with
offsets listed in a text file.
</li>
</ul>


<h3>Improved User Interface</h3>

<ul>
<li>
New Edit and Mixer toolbars for fast access to common functions.
</li>

<li>
The new Drawing tool allows adjustment of individual samples, when fully
zoomed in.  The new Multi-Tool mode gives quick access to different editing
functions without needing to switch tools.
</li>

<li>
Many new keyboard commands have been added, and keyboard shortcuts can
now be customized.
</li>

<li>
New commands:
 <ul>
  <li>Looped play.  Type "L", or hold down shift when clicking Play.</li>
  <li>Type "1" to play a 1-second preview of the audio around the cursor.</li>
 </ul>
</li>

<li>
The mouse wheel can be used to zoom in and out.
</li>

<li>
Tracks can now be zoomed vertically by clicking or dragging in the
vertical rulers.  Shift-click or right-click to zoom out.  
</li>

<li>
The ruler and status bar can now display time in several different
formats, including seconds, samples, or video frames.
</li>

<li>
Audacity's interface can now be translated into languages other than
English.  You can volunteer to help <a href="translation/">translate
Audacity</a> into your native language.
</li>
</ul>