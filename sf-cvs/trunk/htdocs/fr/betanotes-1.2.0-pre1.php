<pre>
Changements dans la version 1.2.0-pre3:

  * Résolution d'un bogue grisant Export lorsque rien n'est sélectionné.

  * Résolution de crash causé par Audacity sur ordinateur équipé d'une carte son sans support de mixage.

  * Crash résolu lors de l'importation de données Raw.

  * Stabilisation de Nouvelle piste Stéréo.

  * Amélioration esthétiques pour Mac OS X.

  * Support de VST Enabler ajouté à la version Windows.

  * Résolution d'un crash survenant lors de la fermeture d'Audacity alors que la boîte de dialogue est ouverte.

  * Résolution d'un bogue de double affichage sous Mac OS X dans les pistes d'index.

  * The recording level control on Linux now adjusts the IGAIN,
    rather than the playthrough level of the recording source.

  * Résolution d'un bogue provoquant une corruption de l'enregistrement en 16-bit stéeréos.

  * Résolution d'un bogue causant une perte de données lors de l'effacement de toutes les pistes d'un projet enregistré et de l'ouverture d'un nouveau fichier dans la même fenêtre .

  * Ajout d'une possibilité de disposition ergonomique des boutons (dans Préférences > Interface)

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

Changements dans la version 1.2.0-pre2:

  * Aide en ligne complètée. Le manual complet est proche de la finalisation et sera en ligne bientôt.

  * Audacity ne vous laissera plus faire des opérations d'édition peu sûres pendant la lecture ou l'enregistrement. Cela élimine plusieurs crashes potentiels.

  * Possibilité d'annuler le bouton quitter corrigée.

  * Nouvelle librairie de rééchantillonnage, sans restrictions de taux maximum ou minimum de rééchantillonnage.

  * Audacity supporte maintenant les plug-ins LADSPA sur toutes les plate-formes, et supporte les plug-ins VST à travers un plug-in LADSPA optionnel appelé "VST Enabler", que vous pouvez télécharger séparément. Pour des questions de licences, Audacity ne peu pas être distribuée avec un support VST intégré.

  * Mac OS X: problèmes de raccourcis clavier corrigés.

  * Mac OS X: problèmes de coupures de son corrigés.

  * Mac OS X: les problèmes de synchro du curseur en lecture/enregistrement sont corrigés.

  * Silence affiche de nouveau une ligne droite au lieu de rien.

  * Ajouté une règle verticale à l'affichage Forme d'onde dB.

  * Corrigé un crash dans changer Hauteur.

  * Vous pouvez maintenant coller si rien n'est sélectionné.

  * Annuler une opération d'importation ne fait plus apparaitre une dialogue d'erreur supplémentaire.

  * Audacity prend maintenant correctement en charge les noms de fichiers avec des caractères internationaux.

  * Ecrit maintenant des tags ID3v2.3 (au lieu de ID3v2.4), pour être compatible avec plus de lecteurs MP3.

  * Minor improvements to build system on Unix systems.

Nouvelles fonctionnalités d'Audacity 1.2.0-pre1:

  * Interface utilisateur
    - Zoom vertical dans les pistes.
    - Apparence et emplacement des barres d'outils améliorés.
    - Nouveau curseurs de souris améliorés.
    - Implémentation complète des raccourcis clavier.
    - Recherche des zero-crossings.
    - La roulette de la souris peut-être utilisée pour le zoom avant ou
      arrière.
    - Mode multi-outils.
    - Amplifier en utilisant l'envelope.
    - Labels peuvent stocker les sélections(comme Audacity 1.0.0).

  * Effets
    - Répêter la dernière commande d'effet.
    - Support des plug-in VST amélioré.
    - La plupart des effets on maintenant un bouton prévisualisation.
    - Compresseur (Compresseur Dynamique).
    - Changer la hauteur de note(sans changer le tempo).
    - Changer le Tempo (sans changer la hauteur de note).
    - Changer la vitesse (en changeant et la hauteur de note et le tempo).
    - Repeat (utile pour créer des boucles).
    - Normalisation (ajuste le volume et DC bias).

  * E/S Audio
    - Commande 1-seconde de prévisualisation.
    - Lecture en boucle.

  * E/S Fichier
    - Audacity 1.2.0 ouvre les projets de toutes les versions précédentes
      d'Audacity, de la 0.98 à la 1.1.3.
    - Ouvre plusieurs fichier depuis la même boite de dialogue.
    - Utilise un fichier texte pour spécifier un liste de fichier
      audio à ouvrir avec offsets.

  * Mode d'emploi mis à jour.


Bogues corrigés dans Audacity 1.2.0-pre1

  * Les fichier projet contenant des caractères spéciaux ne sont plus
    invalides.

  * Les bruits de "Scratch" causés par une saturation sont corrigés.

  * Audacity n'exporte plus des fichiers Ogg invalides, et ne coupe plus
    les dernières secondes des fichiers Ogg exportés.

  * Les fichiers MP3 mono sont maintenant exportés à la bonne vitesse.

  * Plusieurs résultats incorrects de l'outil d'enveloppe ont été corrigés.

  * La commande "Exporter Labels" remplace correctement les fichiers
    existants.
  
  * La fenêtre "Plot Spectrum" affiche le nombre correct d'octaves pour les notes.

  * Plusieurs défaillances mémoire corrigées.

  * Plusieurs autres corrections de bugs mineurs.
</pre>

 

    
 
