<?php

  require_once 'main.inc.php';
  $title = $macStr;
  include 'top.inc.php';

  $os9 = "$stableDownloadDir/audacity-mac.php/audacity-mac.sit";
  $osx = "$stableDownloadDir/audacity-macosx.php/audacity-macosx.dmg";
?>


<?php BoxTop("Mac OS 9"); ?>

<center>
  <?php print "$latestStableStr $macOS9StableVersion<br>"; ?>
  <?php print "$macOS9ReqStr"; ?>
</center>

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

</table>

<p>
Having trouble downloading?  Need a previous version?
<a href="http://sourceforge.net/project/showfiles.php?group_id=6235">Click here for direct download links</a>
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
<a href="http://spaghetticode.org/lame/">
<?php print "$lameStr</a> $lameStr2"; ?>
<br>
<?php print "<a href=stablenotes.php?$langLinkStr>$releaseNotesStr2</a>"; ?>
<br>
<a href="audacity-manual-1.0.0-A.zip"><?php print $docsStr; ?></a>

<?php BoxBottom(); ?>

<p>

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

</table>

<p>
<b>Note: We now recommend that all Mac OS X users try version 
<a href="beta.php">Audacity 1.1.3</a> instead of 1.0.0.
It's still in beta, but it has far surpassed version 1.0 in
OS X support.  Both recording and MP3 exporting are broken
   in Audacity 1.0, but they work much better in 1.1.3.
</p>

<p>
<b>Notes:</b> (if you decide to use Audacity 1.0 anyway...)
<ul>
<li>Having problems when Audacity opens the first time?  Errors involving
the temp directory?  This seems to affect some OS X users.
We're investigating the problem, but in the meantime, here are two
  workarounds:
<ul>
<li>Run Audacity from the Terminal (if you're comfortable doing that).
<li>Download this default <a href="http://spaghetticode.org/Audacity%20Preferences">Audacity Preferences</a> file and put it in your home directory,
under Library/Preferences.
</ul>

<li>Having trouble recording?  Make sure you select "Record in Stereo" in the
Preferences, otherwise Audacity won't be able to record.  (Many OS X
devices, including the built-in audio device, only support stereo and not
mono.)
</ul>
</p>

<center><hr width=50%></center>
<?php print "<a href=stablenotes.php?$langLinkStr>$releaseNotesStr2</a>"; ?>
<br>
<a href="audacity-manual-1.0.0-A.zip"><?php print $docsStr; ?></a>

<?php

  BoxBottom();

  include 'bottom.inc.php';

?>






