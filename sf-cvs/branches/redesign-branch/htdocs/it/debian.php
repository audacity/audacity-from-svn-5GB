<?php BoxTop("Debian"); ?>

<p>
Audacity fà parte delle distribuzioni Debian <b>sid</b> e <b>woody</b>
(versione di test non stabile), con cui l'installazione di Audacity è semplice:
</p>
<xmp>
$ apt-get update && apt-get install audacity
</xmp>
<p>
Per chi usa ancora potato (Debian 2.2) è disponibile per il download il
deb file(da <a href="http://sourceforge.net/project/showfiles.php?group_id=6235">

da SourceForge</a>)
o in alternativa bisogna aggiungere le seguenti linee a /etc/apt/sources.list:
</p>
<xmp>
deb http://audacity.sourceforge.net potato/
</xmp>
<p><b>Attenzione: </b>i deb della potato non sono più recenti. Da quando molte persone
usano Debian come sistema desktop, non ho più creato un deb dalla versione 0.96-0.98.
Sono molto impegnativi dal punto di vista del lavoro, e la domanda è esigua.</p>

<?php BoxBottom(); ?>
