<?php BoxTop("Debian"); ?>

<p>
Audacity er med i både <b>sid</b> og <b>woody</b> distributionerne i Debian (test- og
ustabile udgaver), så det er nemt at installere Audacity:
</p>
<xmp>
$ apt-get update && apt-get install audacity
</xmp>
<p>
For dem, der stadig bruger potato (Debian 2.2) kan downloades en deb
 (fra <a href="http://sourceforge.net/project/showfiles.php?group_id=6235">
 SourceForge filserveren</a>)
eller tilføj dette til din /etc/apt/sources.list:
</p>
<xmp>
deb http://audacity.sourceforge.net potato/
</xmp>
<p><b>Advarsel: </b>potato deb-filerne er gamle. Siden de fleste der kører Debian på arbejsstationer vil bruge enten woody eller sid, har jeg ikke udsendt deb-filer for version 0.96-0.98. De tager en del arbejde at lave, og ingen har bedt om dem.
</p>

<?php BoxBottom(); ?>
