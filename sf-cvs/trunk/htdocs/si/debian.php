<?php BoxTop("Debian"); ?>

<p>
Audacity is a part of both the <b>sid</b> and <b>woody</b> 
distributions in Debian (testing and
unstable), so installing Audacity is as simple as:
</p>
<xmp>$ apt-get update && apt-get install audacity</xmp>
<p>
For anyone still running potato (Debian 2.2) you can download the
deb (from <a href="http://sourceforge.net/project/showfiles.php?group_id=6235">

the SourceForge files area</a>)
or add the following line to your /etc/apt/sources.list:
</p>
<xmp>
deb http://audacity.sourceforge.net potato/
</xmp>
<p><b>Warning: </b>the potato debs are way behind. Since most people
who run Debian on the desktop run either woody or sid, I haven't posted
a deb for version 0.96-0.98. They take a fair amount of work to build, and the demand
isn't there.</p>

<?php BoxBottom(); ?>
