<?php BoxTop("CVS"); ?>

Nous utilisons <a href="http://www.cvshome.org">CVS</a>, le Concurrent Versions
System, pour nous aider à développer Audacity de manière collaborative. Cliquez
 <a
href="http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/audacity/audacity-src/">ici</a>
pour parcourir le code source dans notre dépôt CVS.

<p>
<h3>Note: La version 1.2 is now branched:</h3>
Si vous voulez la branche stable d'Audacity, version 1.2.x, vous devez taper <tt>-r AUDACITY_1_2</tt> quand vous faites une vérification ou une mise à jour CVS.
Autrement vous obtiendrez le CVS HEAD, qui peut devenir instable
Dès aujourd'hui nous travaillons à la version 1.3.0.

<p><h3>Vue rapide du CVS Audacity :</h3>
  Si vous voulez accéder au code source d'Audacity, vous pouvez
  utiliser un client cvs pour télécharger une branche cvs sur votre ordinateur.
  Une fois que vous avez récupéré une branche une fois, votre logiciel client CVS
  pourra vous aider à garder à jour votre version
  avec les autres développeurs Audacity. Suivez les instructions ci-dessous pour obtenir le code source.


<h4>Accès anonyme à CVS avec un client en ligne de commande :</h4>
<p>Entrez la ligne de commande suivante (notez qu'il s'agit d'une seule ligne sans retour à la ligne) :<br>
<ul><tt>cvs -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity
login </tt><br></ul>
et tapez sur la touche Entrée à la demande d'un mot de passe (password).

<p>Puis, <b>pour récupérer le dernier code (1.3.0)</b> (en une seule ligne) :
<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co
audacity</tt></ul>
ou <b>pour la branche stable (1.2.0)</b> (en une seule ligne) :<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co -r AUDACITY_1_2 audacity</tt></ul>
ou <b>pour l'ancienne branche 1.0 (1.0.0)</b> (en une seule ligne):<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co -r audacity-0_9-branch audacity-old</tt></ul>


<p> Alternativement, vous pouvez faire pointer votre variable d'environnement <tt>CVSROOT</tt> vers
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
(voir ci-dessous).<br>
Puis, <b>pour récupérer la dernière branche instable (1.3.0)</b>, entrez<br> 
   <ul><tt>cvs checkout audacity</tt></ul>
Or, <b>pour la branche stable (1.2)</b>, enter <br>
   <ul><tt>cvs checkout -r AUDACITY_1_2 audacity</tt></ul>
ou, <b>pour l'ancienne branche stable (1.0)</b>, entrez <br>
   <ul><tt>cvs checkout -r audacity-0_9-branch audacity-old</tt></ul>

<p> Pour faire pointer votre variable d'environnement <tt>CVSROOT</tt>,
vous pouvez utiliser votre fichier de commandes des ressources du shell ou l'une des commandes suivantes :
<h5>Dans les shells bash ou bourne, sur une ligne :</h5>
<ul><tt>export
CVSROOT=:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>
<h5>Dans csh ou ses descendants, sur une ligne :</h5>
<ul><tt>setenv CVSROOT
:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>

<hr width="80%">

<h4>Accès anonyme à CVS avec un client graphique</h4>

Pour un client graphique comme wincvs, maccvs, ou gcvs,
(disponibles ici <a href="http://cvsgui.org">cvsgui.org</a>) vous devez faire pointer votre variable
<tt>CVSROOT</tt> (dans le sous menu Admin|Preferences) vers
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
et choisir l'authentification "pserver" ou "Password". Puis,
sous la partie Globals de la boite de dialogue "Preferences",
vérifiez que vous avez décoché "Checkout read-only (CVSREAD)".  Ensuite,
choisissez le login, et tapez la touche Entrée pour le mot de passe (password) (c'est "").
Finalement, choisissez "Create|Checkout module", choisissez un emplacement
pour placer la branche téléchargée, et
suivez les instructions ci-dessous en fonction de la branche que vous voulez.
Si vous obtenez une erreur, vérifiez que votre variable <tt>CVSROOT</tt>
ne contient aucun espace à la fin -- ce qui peut arriver si vous

<p>
<b>Pour obtenir la dernière branche instable (1.3.0) :</b><br>
Dans la boite de dialogue "Checkout Settings", entrez
<tt>audacity</tt> comme nom de module. Cliquez "OK" et la branche
sera automatiquement chargée sur votre ordinateur.
<p>
<b>Pour obtenir la branche stable (1.2.0) : </b><br>
Dans la boite de dialogue "Checkout Settings", entrez
<tt>audacity</tt> comme nom de module.  Puis, sous 
l'onglet "Sticky options", cochez la boite "Retrieve rev./tag/branch
(-r)" et entrez <tt>AUDACITY_1_2</tt> dans la zone en dessous. Cliquez "OK" et la branche sera automatiquement chargée sur votre ordinateur.
<p>
<b>Pour obtenir la vieille version de branche 1.0 : </b><br>
Dans la boite de dialogue "Checkout Settings", entrez
<tt>audacity-old</tt> comme nom de module.  Puis, sous 
l'onglet "Sticky options", cochez la boite "Retrieve rev./tag/branch
(-r)" et entrez <tt>audacity-0_9-branch</tt> dans la zone en dessous. Cliquez "OK" et la branche sera automatiquement chargée sur votre ordinateur.

<hr width="80%">
Si vous débutez avec CVS ? Démarrer par la lecture de Jim Blandy : <a href="">Introduction à CVS</a>, Bob Arnson : <a
href="http://www.cvshome.org/new_users.html">CVS for new
users</a>, ou aller sur les pages cvs à <a
href="http://www.cvshome.org/">www.cvshome.org</a>.
Plus d'informations détaillées sont disponibles dans les chapitres sous GPL du livre de Karl
Fogel <a href="http://cvsbook.red-bean.com/cvsbook.html">CVS Book
at cvsbook.red-bean.com</a>, ou "l'officiel" <a
href="http://www.cvshome.org/docs/manual">Per
 Cederqvist manual</a>.

<p> Pour une aide spécifique sur CVS dans sourceforge.net,
essayez la documentation sourceforge pour les plates-formes 
<a
href="http://sourceforge.net/docman/display_doc.php?docid=763&group_id=1">Unix</a>,
<a
href="http://sourceforge.net/docman/display_doc.php?docid=766&group_id=1">Microsoft Windows</a>, et <a
href="http://sourceforge.net/docman/display_doc.php?docid=2973&group_id=1">MacOS
(avant OS X)</a>.

<hr width="80%">

<p><b>Plus de détails :</b></p>

<p>Audacity utilise de nombreuses librairies provenant de tiers. La plupart nécessitent des améliorations
pour être compilé sur nos plates-formes cibles. Par conséquent, nous conservons un dépôt local de tous les codes sources des tiers dans le CVS.
Voici comment cela marche :
</p>
<p>Il y a deux dépôts : 'audacity-src', qui contient tous les codes sources que nous avons écrits,
et 'lib-src,' qui contient les codes sources de toutes les librairies que nous utilisons.
Pour garantir l'interopérabilité entre Audacity et nos librairies,
nous recommandons que vous utilisiez les versions des librairies contenues dans 'lib-src'.
Toutefois, sur un système Unix, vous pouvez éviter de compiler certaines librairies
en utilisant les librairies qui sont déjà sur votre système.
Tapez 'configure --help' pour voir les options.
</p>
<p>
Comme ça, si vous voulez tout vérifier, y compris le code source des librairies,
faites checkout sur le module 'audacity' qui va vérifier audacity-src mais récupérez aussi le dépôt
lib-src comme sous répertoire de 'audacity'.
</td>
</tr>
</table>
</p>

<?php BoxBottom(); ?>

