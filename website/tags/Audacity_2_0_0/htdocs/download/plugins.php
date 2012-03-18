<?php
/*
 * Copyright 2004 - 12 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  require_once "../latest/versions.inc.php";
  require_once "../latest/mirror.inc.php";
  $pageId = "plugins";
  $pageTitle = _("Plug-Ins and Libraries");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>
<p><?=_('You can download and install plug-ins or libraries to add extra functionality to Audacity. Plug-ins can give you extra effects, or more audio generation and analysis capability. Adding libraries can allow you to import or export additional audio formats.')?></p>

<h3><?=_("Plug-In Installation")?></h3>
<p><?=_('To install new plug-ins, place them in the <b>Plug-Ins</b> folder inside the Audacity installation folder.  On Windows computers, this is usually under "Program Files".  On Mac OS X, it is usually under "Applications". Restart Audacity, then the Plug-ins will appear underneath the divider in the "Effect", "Generate" or "Analyze" menus.')?></p>

<h3><?=_("LADSPA Plug-Ins")?></h3>
<p><?php printf(_('Audacity has built-in support for LADSPA plug-ins.  These plug-ins are mostly built for Linux, but some are available for other operating systems too.  Audacity includes some sample LADSPA effects. Windows users can install an additional <a href="%s">set of over 90 LADSPA plug-ins</a>.  There is a similar set of LADSPA plug-ins for <a href="%s">Mac</a>. More information and many LADSPA plug-ins for Linux can be found on the <a href="%s">LADSPA web site</a>.'), download_url($ladspa_url), 'http://ardour.org/files/releases/swh-plugins-0.4.15.dmg', 'http://www.ladspa.org/')?></p>

<h3><?=_("Nyquist Plug-Ins")?></h3>
<p><?=_('Audacity has built-in support for Nyquist effects on all operating systems. You can download additional <a href="nyquistplugins">Nyquist plug-ins</a>, or create your own using the <a href="../help/nyquist">Nyquist programming language</a>. Nyquist code can be tested using "Nyquist Prompt" under the Effect menu, or code for Nyquist plug-ins that generate audio can be quickly tested with <a href="http://audacity.sourceforge.net/nyquist/generate.zip">Nyquist Generate Prompt</a>.')?>

<h3><?=_("VST Plug-Ins")?></h3>
<p><?=_('Audacity can load VST effects (but not VST instruments) on Windows and Mac. The VST Enabler is no longer required. See <a href="http://manual.audacityteam.org/help/manual/man/faq_installation_and_plug_ins.html#vst_install">"How do I install VST plug-ins?"</a> for more information.')?></p>

<p><?=_('VST effects can be found on many plug-in sites such as:')?></p>
<ul>
<li><a href="http://www.hitsquad.com">Hitsquad</a>: &nbsp;<a href="http://www.hitsquad.com/smm/win95/PLUGINS_VST/">Windows</a>, <a href="http://www.hitsquad.com/smm/mac/PLUGINS_VST/">Mac</a></li>
<li><a href="http://www.kvraudio.com/">KVR Audio</a>: &nbsp;<a href="http://www.kvraudio.com/get.php?mode=results&st=q&s=6">Windows</a>, <a href="http://www.kvraudio.com/get.php?mode=results&st=q&s=8">Mac</a></li>
<li><a href="http://dmoz.org/Computers/Multimedia/Music_and_Audio/Software/Plug-ins/">Open Directory</a> (Windows, Mac).</li>
</ul>

<p><?=_('The <a href="http://wiki.audacityteam.org/wiki/VST_Plug-ins">VST Plug-ins</a> page on our <a href="http://wiki.audacityteam.org/">Wiki<a/> contains further help for VST plug-ins, and lists a large number of VST plug-ins that have been reported to work well in Audacity.')?></p>

<h3><?=_("Libraries")?></h3>
<p><?=_('The <b>LAME MP3 encoding</b> library allows Audacity to export audio in the popular <a href="http://wiki.audacityteam.org/index.php?title=MP3">MP3</a> format. To install the LAME library, please read our <a href="http://manual.audacityteam.org/help/manual/man/faq_installation_and_plug_ins.html#lame">LAME FAQ</a>.')?></p>    

<p><?=_('The <b>FFmpeg import/export</b> library allows Audacity to import and export many additional audio formats such as AC3, AMR(NB), M4A and WMA, and to import audio from video files. To install the FFmpeg library, please read our <a href="http://manual.audacityteam.org/help/manual/man/faq_installation_and_plug_ins.html#ffdown">FFmpeg FAQ</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>