<?php

  require_once 'main.inc.php';
  $title = $windowsStr;
  include 'top.inc.php';

  $exe = "$stableDownloadDir/audacity-win.php/audacity-win.exe";
  $zip = "$stableDownloadDir/audacity-win-zip.php/audacity-win.zip";

  BoxTop($windowsStr);
?>

<center>
  <?php print "$latestStableStr $winStableVersion<br>"; ?>
  <?php print "$winReqStr"; ?>

  <br>
  (Windows 98, ME, NT4, 2000, & XP)

</center>

<table border="0">

<tr>
<td valign=center
><?php print "<a href=\"$exe\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$exe\">audacity-win.exe";
?></a>
<?php print "($installerStr, $winStableSize)"; ?>
</td>
</tr>

<tr>
<td valign=center
><?php print "<a href=\"$zip\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$zip\">audacity-win.zip";
?></a>
<?php print "($zipStr, $winStableZipSize)"; ?>
</td>
</tr>

<tr>
<td></td>
<td>
<br>
<a href="http://mitiok.free.fr/">
<?php print "$lameStr</a> $lameStr2"; ?>
</td>
</tr>
                                                                                
<tr>
<td></td>
<td>
<a href="http://audacityteam.org/vst/">
<?php print "$vstEnablerStr</a>"; ?>
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
Known incompatibilities:
<ul>
<li>Audacity has been reported to not work with
    Norton Protected Recycle Bin.
<li>Audacity does not run on Windows 95.  Windows 98 or higher is required.
</ul>

<p>
<?php print "$altDownloadStr"; ?> <a href="http://sourceforge.net/project/showfiles.php?group_id=6235"><?php print "$altDownloadStr2"; ?>
</a>
</p>

<?php

  BoxBottom();

  include 'bottom.inc.php';

?>

