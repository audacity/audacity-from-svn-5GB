<?php

  require_once 'main.inc.php';
  $title = $macStr;
  include 'top.inc.php';

  $os9 = "$stableDownloadDir/audacity-mac.php/audacity-mac.sit";
  $osx = "$stableDownloadDir/audacity-macosx.php/audacity-macosx.dmg";
?>

<?php BoxTop("Mac OS X"); ?>

<center>
  <?php print "$latestStableStr $macOSXStableVersion<br>"; ?>
  <?php print "$macOSXReqStr"; ?>
</center>

<table border="0">

<tr>
<td valign=center
><?php print "<a href=\"$osx\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$osx\">audacity-macosx.dmg";
?></a>
<?php print "($dmgStr, $macOSXStableSize)"; ?>
</td>
</tr>

<tr>
<td></td>
<td>
<a href="http://spaghetticode.org/lame/">
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
<?php print "$altDownloadStr"; ?> <a href="http://sourceforge.net/project/showfiles.php?group_id=6235"><?php print "$altDownloadStr2"; ?>
</a>
</p>

<?php BoxBottom(); ?>

<p>

<?php BoxTop("Mac OS 9"); ?>

<center>
  <?php print "$latestStableStr $macOS9StableVersion<br>"; ?>
  <?php print "$macOS9ReqStr"; ?>
</center>
<?php include GetTranslation("macos9note"); ?>

<table border="0">

<tr>
<td valign=center
><?php print "<a href=\"$os9\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$os9\">audacity-mac.sit";
?></a>
<?php print "($sitStr, $macOS9StableSize)"; ?>
</td>
</tr>

<tr>
<td></td>
<td>
<a href="http://spaghetticode.org/lame/">
<?php print "$lameStr</a> $lameStr2"; ?>
</td>
</tr>
</table>

<center><hr width=50%></center>
<p>
<?php print "$altDownloadStr"; ?> <a href="http://sourceforge.net/project/showfiles.php?group_id=6235"><?php print "$altDownloadStr2"; ?>
</a>
</p>

<p>
Known incompatibilities:
<ul>
<li>Faxstf (causes problems with the Apple menu)
<li>Kaleidoscope (causes problems with the GUI)
<li>Does not work with Mac OS 8.6 anymore - oops!<br>
    We'll try to release an OS 8.6-compatible version later...<br>
    Still, Mac OS 9.2 is highly recommended.
</ul>

</p>

<center><hr width=50%></center>
<?php print "<a href=help.php$langQueryStr>$docsStr</a>\n<br>\n"; ?>

<?php

  BoxBottom();

  include 'bottom.inc.php';

?>

