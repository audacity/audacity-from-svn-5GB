<?php BoxTop("Debian"); ?>

<p>
Audacity ist in sid und woody (woody ist inzwischen stabil) enthalten.
Die Installation erfolgt wie folgt:
</p>
<xmp>$ apt-get update && apt-get install audacity</xmp>
<p>
Wenn Sie noch potato nutzen sollten (Debian 2.2), können Sie das Paket in der
 <a href="http://sourceforge.net/project/showfiles.php?group_id=6235">
SourceForge files area</a> herunterladen oder folgende Zeile in die sources.list eintragen:
</p>
<xmp>
deb http://audacity.sourceforge.net potato/
</xmp>
<p><b>Warning: </b>the potato debs are way behind. Since most people
who run Debian on the desktop run either woody or sid, I haven't posted
a deb for version 0.96-0.98. They take a fair amount of work to build, and the demand
isn't there.</p>

<?php BoxBottom(); ?>
