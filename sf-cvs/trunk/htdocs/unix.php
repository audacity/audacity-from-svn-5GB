<?php

  require_once 'main.inc.php';
  $title = $unixStr;
  include 'top.inc.php';

  $src = "$stableDownloadDir/audacity-src.php/audacity-src.tar.gz";
  $linux386 = "$stableDownloadDir/audacity-linux-i386.php/audacity-linux-i386.tar.bz2";
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

<tr>
<td></td>
<td>
<p><?php print "$sourceDepStr"; ?></p>
<ul>
<li><a href="http://wxwidgets.org/downld2.htm">wxWidgets 2.4</a> (required)
<li><a href="http://www.mars.org/home/rob/proj/mpeg/">MAD (Mpeg Audio Decoder)</a> (optional)
<li><a href="http://vorbis.com/download.psp">Ogg Vorbis</a> (optional)
<li><a href="http://www.mp3dev.org/mp3/download/download.html">LAME</a> (optional)
</ul>
</td>
</tr>

<tr>
<td></td>
<td>
<a href="http://audacity.sourceforge.net/audacity-manual-1.2.zip">
<?php print "Manual</a> (450k $zipStr, in English)"; ?>
</td>
</tr>

</table>

<center><hr width=50%></center>
<?php print "<a href=stablenotes.php$langQueryStr>$releaseNotesStr2</a>"; ?>
<br>
<?php print "<a href=help.php$langQueryStr>$docsStr</a>\n<br>\n"; ?>

<p>
<?php print "$altDownloadStr"; ?> <a href="http://sourceforge.net/project/showfiles.php?group_id=6235"><?php print "$altDownloadStr2"; ?>
</a>
</p>

<?php BoxBottom(); ?>

<p>

<?php BoxTop($linuxStr); ?>

<center>
  <?php print "$latestStableStr $linux386StableVersion"; ?>
</center>

<table border="0">

<tr>
<td valign=center
><?php print "<a href=\"$linux386\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$linux386\">audacity-linux-i386.tar.bz2";
?></a>
<?php print "($linux386Str, $linux386StableSize)"; ?>
</td>
</tr>
<p><?php print "$linuxStr2"; ?></p>

</table>

<?php BoxBottom(); ?>

<p>

<?php
  print "<p>";

  include GetTranslation("fedora");

  print "<p>";

  include GetTranslation("debian");

  print "<p>";

  include GetTranslation("sisyphus");

  print "<p>";

  include GetTranslation("gentoo");
?>

<p>

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

