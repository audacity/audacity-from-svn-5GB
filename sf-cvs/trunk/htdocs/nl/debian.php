<?php BoxTop("Debian"); ?>

<p>
Audacity is een gedeelte van zowel de <b>sid</b> en <b>woody</b> 
distributies in Debian (in test en
instabiel), dus installeren van Audacity is net zo simpel als:
</p>
<xmp>$ apt-get update && apt-get install audacity</xmp>
<p>
Voor iedereen die nog altijd potato (Debian 2.2) draaiende heeft, je kunt de
deb downloaden (van <a href="http://sourceforge.net/project/showfiles.php?group_id=6235">

De SourceForge bestanden opslagplaats</a>)
of voeg de volgende zin toe aan je /etc/apt/sources.lijst:
</p>
<xmp>
deb http://audacity.sourceforge.net potato/
</xmp>
<p><b>Waarschuwing: </b>de potato debs lopen ver achter. Omdat de meeste mensen
die Debian gebruiken op hun desktop of woody of sid draaien, heb ik geen
deb voor versie 0.96-0.98. geplaatst. Deze nemen behoorlijk wat tijd en werk in beslag om te bouwen, en er is bijna geen vraag
naar.</p>

<?php BoxBottom(); ?>
