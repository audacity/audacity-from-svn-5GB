<?php

  require_once 'main.inc.php';
  $title = $betaVersionsStr;
  include 'top.inc.php';

  $exe = "$betaDownloadDir/audacity-win.php/audacity-win.exe";
  $zip = "$betaDownloadDir/audacity-win-zip.php/audacity-win.zip";
  $os9 = "$betaDownloadDir/audacity-mac.php/audacity-mac.sit";
  $osx = "$betaDownloadDir/audacity-macosx.php/audacity-macosx.dmg";
  $src = "$betaDownloadDir/audacity-src.php/audacity-src.tgz";
  $rpm = "$betaDownloadDir/audacity-rpm.php/audacity-i386.rpm";

  BoxTop($betaVersionsStr);
?>

<?php include GetTranslation("beta-1.2.0-pre1"); ?>

<p align="center">
<a href="<?php print "screenshots.php?$langLinkStr"; ?>">
<img src="images/audacity-1.2.0-pre1-small.png" border=0 width=273 height=226></a>
</p>

<?php include GetTranslation("beta-1.2.0-pre1-problems"); ?>
<?php include GetTranslation("beta-1.2.0-pre1-changes"); ?>

<table border="0">

<tr>
<td colspan=2>
<center><b>
<?php print "Windows ($winBetaVersion)" ?>
</b></center>
</td>
</tr>

<tr>
<td valign=center
><?php print "<a href=\"$exe\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$exe\">audacity-win.exe";
?></a>
<?php print "($installerStr, $winBetaSize)"; ?>
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
<?php print "($zipStr, $winBetaZipSize)"; ?>
</td>
</tr>

<tr>
<td></td>
<td>
<a href="http://mitiok.free.fr/">
<?php print "$lameStr</a>"; ?>
</td>
</tr>

<tr>
<td colspan=2>
<center><b>
<?php print "Mac OS X ($macOSXBetaVersion)" ?>
</b></center>
</td>
</tr>

<tr>
<td valign=center
><?php print "<a href=\"$osx\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$osx\">audacity-macosx.dmg";
?></a>
<?php print "($dmgStr, $macOSXBetaSize)"; ?>
</td>
</tr>

<tr>
<td></td>
<td>
<a href="http://spaghetticode.org/lame/">
<?php print "$lameStr</a>"; ?>
</td>
</tr>

<tr>
<td colspan=2>
<center><b>
<?php print "Source Code ($srcBetaVersion)" ?>
</b></center>
</td>
</tr>

<tr>
<td valign=center
><?php print "<a href=\"$src\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$src\">audacity-src.tgz";
?></a>
<?php print "($tarballStr, $srcBetaSize)"; ?>
</td>
</tr>

<tr>
<td></td>
<td>
Source code dependencies:
<ul>
<li><a href="http://www.wxwindows.org/downld2.htm">wxWindows 2.4.0</a> (required)
     <li><a href="http://www.mars.org/home/rob/proj/mpeg/">MAD (Mpeg Audio Decoder)</a> (optional)
     <li><a href="http://www.xiph.org/ogg/vorbis/download.html">Ogg Vorbis</a> (optional)
     <li><a href="http://www.mp3dev.org/mp3/download/download.html">LAME</a> (optional)
</ul>
</td>
</tr>

<!--

<tr>
<td valign=center
><?php print "<a href=\"$rpm\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$rpm\">audacity-i386.rpm";
?></a>
<?php print "($rpmStr, $rpmBetaSize)"; ?>
</td>
</tr>

-->

<tr>
<td colspan=2>
<center><b>
<?php print "Mac OS 9 ($macOS9BetaVersion)" ?>
</b></center>
</td>
</tr>

<!--
<tr>
<td valign=center
><?php print "<a href=\"$os9\">";
?><img src=images/DownloadIcon.gif width=32 height=32 border=0></a>
</td>
<td valign=center
><?php print "<a href=\"$os9\">audacity-mac.sit";
?></a>
<?php print "($sitStr, $macOS9BetaSize)"; ?>
</td>
</tr>
-->

<tr>
<td></td>
<td>
<?php include GetTranslation("macos9note"); ?>
</td>
</tr>

</table>

<center><hr width=50%></center>

<!--
<a href="http://www.maindex.com/lame/pages/beta_Win32_DLL.html">
<?php print "$lameStr</a> $lameStr2"; ?>
<br>
-->

<?php print "<a href=betanotes.php?$langLinkStr>$releaseNotesStr2</a>"; ?>

<?php

  BoxBottom();

  include 'bottom.inc.php';

?>






