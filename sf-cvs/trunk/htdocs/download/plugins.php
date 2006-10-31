<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "plugins";
  $pageTitle = _("Plug-In Effects");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>
<p><?=_('You can download and install plug-ins to add extra effects to Audacity.  Plug-ins appear at the bottom of the "Effect" menu.  To install new plug-ins, place them in the <b>Plug-Ins</b> folder inside the Audacity installation folder.  On Windows computers, this is usually under "Program Files."  On Mac OS X, it is usually under "Applications."')?></p>

<h3><?=_("LADSPA Plug-Ins")?></h3>
<p><?=_('Audacity has built-in support for LADSPA plug-ins.  These plug-ins are mostly built for Linux, but some are available for other operating systems too.  Audacity comes with sample LADSPA effects for Mac. For Windows, a <a href="http://audacity.sourceforge.net/beta/ladspa/ladspa-0.4.15.exe">set of over 90 LADSPA effects</a> is available.  For more information and downloads, see the <a href="http://www.ladspa.org/">LADSPA web site</a>.')?></p>

<h3><?=_("Nyquist Plug-Ins")?></h3>
<p><?=_('Audacity has built-in support for cross-platform Nyquist effects.  You can download additional <a href="nyquistplugins">Nyquist plug-ins</a>, or create your own using the <a href="../help/nyquist">Nyquist programming language</a>')?>

<h3><?=_("VST Plug-Ins")?></h3>
<p><?=_('Audacity can load VST effects (for Mac OS or Windows) using the optional <a href="../help/faq?s=install&amp;i=vst-enabler">VST Enabler</a>.  You can find VST effects at <a href="http://www.kvraudio.com/">KVR Audio</a> and <a href="http://dmoz.org/Computers/Multimedia/Music_and_Audio/Software/Plug-ins/">other plug-in sites</a>.</p>')?>

<?php
  include "../include/footer.inc.php";
?>
