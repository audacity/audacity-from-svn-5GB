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
Les fichiers de projets Audacity 1.2 utilisent un nouveau format XML. Audacity 1.2 ouvrira automatiquement un projet d'une version précédente et le convertira.
</li>

<li>
Audacity 1.2 utilise <a href="http://www.underbit.com/products/mad/">libmad</a>
pour un décodage plus rapide des fichiers MP3.  Les fichiers<a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a> d'Erik de Castro Lopo offre une meilleure compatibilité avec plusieurs format de fichiers non compressés.
</li>

<li>
La dernière version des librairies <a href="http://www.vorbis.com/">Vorbis</a>
est utilisée, pour une meilleure qualité d'encodage Ogg Vorbis.
</li>

<li>
Le dialogue "Importer et Ouvrir" permet maintenant aux utilisateurs d'Audacity d'utiliser plusieurs fichiers son à la fois et de les ouvrir dans un seul projet. Un nouveau format de fichier "LOF" permet à Audacity d'ouvrir un groupe de fichiers avec les offsets listés dans un fichier texte.
</li>
</ul>


<h3>Interface utilisateur améliorée</h3>

<ul>
<li>
Nouvelles barres d'outils Edition et Mixage pour un accès rapide au fonctions communes.
</li>

<li>
Le nouvel outil de dessin permet l'ajustement d'échantillons individuels quand le zoom est au maximum. Le nouveau mode Multi-Outil donne un accès rapide aux différentes fonctions d'édition sans avoir à changer d'outil.
</li>

<li>
Plusieurs nouvelles commandes clavier on été ajoutées, et les raccourcis clavier peuvent maintenant être personnalisés.
</li>

<li>
Nouvelles commandes:
 <ul>
  <li>Lecture en boucle.  Pressez "L", ou maintenez la touche majuscule enfoncée en cliquant sur Lecture.</li>
  <li>Pressez "1" pour lire une prévisualisation d'une seconde autour du curseur.</li>
 </ul>
</li>

<li>
La roulette de la souris peut être utilisée pour zoomer en avant ou en arrière.
</li>

<li>
Les pistes peuvent maintenant être zoomées verticalement en cliquant ou en déplacent les règles verticales.  Shift-click or click-droit pour zoomer en arrière.  
</li>

<li>
La règle et la barre de statut peuvent maintenant afficher le temps dans plusieurs formats différents, incluant les secondes, échantillons ou images vidéo.
</li>

<li>
L'interface d'Audacity peut maintenant être traduite en d'autres langues que l'anglais. Vous pouvez contribuer pour <a href="translation/">traduire
Audacity</a> dans votre propre langue.
</li>
</ul>