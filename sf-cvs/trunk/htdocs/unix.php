<?php

  require_once 'main.inc.php';
  $title = $unixStr;
  include 'top.inc.php';

  $src = "$stableDownloadDir/audacity-src.php/audacity-src.tgz";
  $rpm = "$stableDownloadDir/audacity-i386-rpm.php/audacity-i386.rpm";
?>


<?php BoxTop($sourceStr); ?>

<center>
  <?php print "$latestStableStr $srcStableVersion"; ?>
</center>

<table border="0">

<tr>
<td valign=center
><?php print "<a href=\"$src\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$src\">audacity-src.tgz";
?></a>
<?php print "($tarballStr, $srcStableSize)"; ?>
</td>
</tr>

</table>

<p>
Having trouble downloading?  Need a previous version?
<a href="http://sourceforge.net/project/showfiles.php?group_id=6235">Click here for direct download links</a>
</p>

<center><hr width=50%></center>
<?php print "<a href=stablenotes.php?$langLinkStr>$releaseNotesStr2</a>"; ?>
<br>
<a href="audacity-manual-1.0.0-A.zip"><?php print $docsStr; ?></a>

<?php BoxBottom(); ?>

<p>

<?php BoxTop("RedHat RPM"); ?>

<center>
  <?php print "$latestStableStr $rpmStableVersion"; ?>
</center>

<table border="0">

<tr>
<td valign=center
><?php print "<a href=\"$rpm\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$rpm\">audacity.i386.rpm";
?></a>
<?php print "($rpmStr, $rpmStableSize)"; ?>
</td>
</tr>

</table>

<p>

<b>Requires:</b>
<ul>
<li><a href="http://telia.dl.sourceforge.net/sourceforge/wxwindows/wxGTK-2.2.9-0.i386.rpm">wxGTK-2.2.9-0.i386.rpm</a> (2 MB)
<li><a href="http://www.vorbis.com/files/rc3/unix/libogg-1.0rc3-1.i386.rpm"
>libogg-1.0rc3-1.i386.rpm</a> (14K)<br>
<li><a href="http://www.vorbis.com/files/rc3/unix/libvorbis-1.0rc3-1.i386.rpm"
>libvorbis-1.0rc3-1.i386.rpm</a> (164K)
</p>
</ul>
</p>

<p>
These RPMs have been reported to work well with Caldera OpenLinux, also.
</p>

<center><hr width=50%></center>
<?php print "<a href=stablenotes.php?$langLinkStr>$releaseNotesStr2</a>"; ?>
<br>
<a href="audacity-manual-1.0.0-A.zip"><?php print $docsStr; ?></a>

<?php

  BoxBottom();

  print "<p>";

  include GetTranslation("debian");

  print "<p>";

  include GetTranslation("sisyphus");

  print "<p>";

  include GetTranslation("mandrake");

  print "<p>";

  include GetTranslation("gentoo");

?>

<?php BoxTop("Unix Tips"); ?>

<p>
Are you using a soundserver like
<a href="http://www.arts-project.org/">aRts</a> (commonly found with KDE)
or
<a href="http://www.gnome.org/softwaremap/projects/esound/">esd</a> (commonly found with GNOME)?
If so, you will need to kill it first before using Audacity!
Example:
</p>
<p>
<tt>
  killall artsd
</tt>
</p>

<?php BoxBottom(); ?>

<?php

  include 'bottom.inc.php';

?>






