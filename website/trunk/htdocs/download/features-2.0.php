<?php
/*
 * Copyright 2005-2012 Dominic Mazzoni, Gale Andrews, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "features-2.0";
  $pageTitle = _("New Features in Audacity 2.0");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p>
<?=_('Audacity 2.0 is our current version. 
      It has dozens of new features, developed in the 1.3 series. 
      It replaces all previous versions, especially 1.2.6 and 1.3.14.
      See <a href="../download/features-1.3-a">New Features in 1.3</a> for changes during the 1.3 series.
      See also <a href="http://wiki.audacityteam.org/wiki/Release_Notes_2.0.0">Release Notes</a> for more details.
')?></p>

<?=_('
Some of the major changes in 2.0.0 over 1.2.6 are: 
<ul> 
  <li>Many effects significantly improved, especially Equalization, Noise
      Removal and Normalize. Vocal Remover now included plus GVerb on 
      Windows and Mac. VAMP analysis plug-ins now supported.  
    </li>
  <li>Improved label tracks with Sync-Lock Tracks feature in the Tracks Menu.
      Multiple clips per track. Tracks and selections can be fully 
      manipulated using the keyboard. Many more keyboard shortcuts. 
    </li>
  <li>New Device Toolbar to manage inputs and outputs. Timer Record feature.
      New Mixer Board view with per-track VU meters. 
    </li>
  <li>Automatic Crash Recovery in the event of abnormal program termination.  
    </li>
  <li>Fast "On-Demand" import of WAV/AIFF files if read directly from source. 
      FLAC now fully supported. Added support for optional FFmpeg library 
      for import/export of AC3/M4A/WMA and import of audio from video files. 
    </li>
</ul>
')?>



<?php
  include "../include/footer.inc.php";
?>