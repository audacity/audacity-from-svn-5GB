<?php
/*
 * Copyright 2004-11 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "links";
  $pageTitle = _("Links");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>
<h3><?=_("Other Things Named “Audacity” (not related to us)")?></h3>
<ul>
  <li><?=_('<a href="http://audacityaudio.com/index.htm">Audacity DVR</a> is a digital voice recorder for smartphones.')?></li>
  <li><?=_('<a href="http://www.audacity.org">Audacity.org</a> is a research company for construction industry professionals.')?></li>
  <li><?=_('<a href="http://www.audacity.com/">Audacity, Inc.</a> provides software consulting services for businesses in Washington, Arizona and California.')?></li>
  <li><?=_('Two different rock bands from <a href="http://www.myspace.com/audacityrocks">Finland</a> and <a href="http://www.unsigned.com/audacity">England</a> are named "Audacity".')?></li>
</ul>

<h3><?=_("Other Open Source Audio Software")?></h3>
<p><?=_('<a href="http://ardour.org/">Ardour</a> is a powerful digital audio workstation that runs on GNU/Linux and Mac OS X.')?></p>
<p><?=_('<a href="http://cdexos.sourceforge.net/">CDex</a> is a free CD ripper for Microsoft Windows.')?></p>
<p><?=_('<a href="http://www.opensourcepartners.nl/~costar/gramofile/">GramoFile</a> is a free tool for capturing and cleaning recordings from vinyl records.')?></p>
<p><?=_('<a href="http://www.mixxx.org/">Mixxx</a> is free cross-platform digital DJ mixing software with features like automatic BPM matching and seamless looping.')?></p>
<p><?=_('More free audio software at SourceForge.net:')?></p>
<ul>
  <li><a href="http://sourceforge.net/search/?&fq[]=trove%3A99&fq[]=trove%3A113&fq[]=trove%3A120"><?=_("Audio Editors")?></a>
  <li><a href="http://sourceforge.net/search/?&fq[]=trove%3A99&fq[]=trove%3A113&fq[]=trove%3A120&fq[]=trove%3A115"><?=_("Audio Capture/Recording")?></a>
  <li><a href="http://sourceforge.net/search/?&fq[]=trove%3A99&fq[]=trove%3A113&fq[]=trove%3A120&fq[]=trove%3A114"><?=_("Audio Analysis")?></a>
</ul>

<h3><?=_("Open Source Audio-Visual Software")?></h3>
<p><?=_('<a href="http://www.blender.org/">Blender</a> is a free, cross-platform 3D content creation suite for animations, interactive applications and video games.')?></p>
<p><?=_('<a href="http://mediainfo.sourceforge.net/">MediaInfo</a> is a free, cross-platform tag and codec analyzer for audio and video files.')?></p>
<p><?=_('<a href="http://www.videolan.org/vlc/">VLC media player</a> is a free, cross-platform media player for most audio and video files as well as DVD, Audio CD and VCD. VLC also can also send, save and convert <a href="http://www.videolan.org/streaming-features.html">audio and video streams<a/>.')?></p>
<p><?=_('CD/DVD burning:')?></p>
<ul>
  <li><?=_('<a href="http://infrarecorder.org/">InfraRecorder</a> for Windows')?>  
  <li><?=_('<a href="http://burn-osx.sourceforge.net/Pages/English/home.html">Burn</a> for Mac OS X')?>  
  <li><?=_('<a href="http://k3b.org/">K3b</a> or <a href="http://www.xfce.org/projects/xfburn">Xfburn</a> for Linux')?>  
</ul>
<p><?=_('Video editing/capture:')?></p>
<ul>
  <li><?=_('<a href="http://www.virtualdub.org/">VirtualDub</a> for Windows')?>  
  <li><?=_('<a href="http://avidemux.sourceforge.net/">Avidemux</a> for Windows, Mac OS X (Intel) and Linux')?>  
  <li><?=_('<a href="http://sourceforge.net/projects/hyperengine/">HyperEngine-AV</a> for Mac OS X')?>  
  <li><?=_('<a href="http://cinelerra.org/about.php">Cinelerra</a> or <a href="http://www.kinodv.org/article/static/2">Kino</a> for Linux')?>  
</ul>

<?php
  include "../include/footer.inc.php";
?>
