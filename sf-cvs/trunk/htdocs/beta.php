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

<p>
  July 30, 2003
</p>

<p>
  Thanks for your interest in beta versions of Audacity.  We have temporarily
  removed the links to beta versions of Audacity while we work out licensing
  issues and prepare for the next stable release, Audacity 1.2.0, in the next
  few weeks.
</p>

<p>
  We will be distributing a prerelease version of Audacity 1.2.0 to a small
  group of users very soon.  If you are interested in helping to test, please
  join one of our <a href="http://sourceforge.net/mail/?group_id=6235">mailing lists</a>.
</p>

<p>
  If you haven't used Audacity before, you may want to try version 1.0 until
  the next version is released.  Download version 1.0 here:
  <a href="windows.php">[Windows]</a> 
  <a href="mac.php">[Mac]</a> 
  <a href="unix.php">[Unix]</a>.
</p>

<?php

  BoxBottom();

  include 'bottom.inc.php';

?>






