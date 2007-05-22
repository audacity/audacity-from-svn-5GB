<?php
/*
 * Copyright 2004 Matt Brubeck
 * Richard Ash 2006
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "tutorials";
  $pageTitle = _("Tutorials");
  include "../include/header.inc.php";

	echo "<h2>$pageTitle</h2>";

	// i18n-hint:  Please add a note that the links on this page are in English.
	// If there are tutorials in your language, link to them here.
	echo "<p>"._('These articles give step-by-step instructions for common tasks using the free Audacity sound editor.')."</p>";

  // i18n-hint:  If this string is translated, it will appear above the list of
  // tutorials on the "help/tutorials" page.  If there are extra tutorials
  // available in your language, add them here.  Otherwise, do not translate
  // this string.  (Or make the translation identical to the original string.)
  if (_("localized_tutorials") != "localized_tutorials") {
    echo _("localized_tutorials");
  }
?>
<ul>
  <li>
    <p>
      <a href="http://audacityteam.org/wiki/index.php?title=Tutorials">
        <?=_("Audacity Wiki Tutorials")?>
      </a> -
      <?=_("A collection of tutorials on doing various editing and conversion tasks in Audacity, such as making ringtones, removing vocals, mixing, creating podcasts, transferring tapes and records to computer, and several more. Also includes links to other tutorials on the web. The ")?>
      <a href="http://audacityteam.org/wiki/index.php?title=MultiLingual">MultiLingual</a>
      <?=_("page contains links to tutorials and resources in languages other than English.")?>
    </p>
  </li>
  <li><p><a href="http://audacity.sourceforge.net/manual-1.2/tutorials.html"><?=_("Audacity 1.2 Tutorials")?></a> - <?=_("From the official Audacity user's manual.")?></p></li>
	<li><p><a href="http://www.edhsonline.org/other/audacity/"><?=_("Audacity 1.2 Tutorial by Dan Eliot")?></a> - <?=_("Learn how to open and edit a WAV file, and save the result in an MP3.")?></p></li>
	<li><p><a href="http://quicktoots.linuxaudio.org/toots/audacity/"><?=_("Audacity Tutorial by Daniel James")?></a> - <?=_("Shows how to mix a multi-track project.  Includes sample sound files.")?></p></li>
  <li>
    <p>
      <a href="http://vip.chowo.co.uk/wp-content/uploads/jaws/Audacity-1.3.3-Guide.html">
        <?=_("JAWS&reg; Users' Audacity 1.3.3 Guide")?>
      </a> -
      <?=_("A guide for using Audacity 1.3.3 with JAWS or other screen readers, written by Chorlton Workshop for ")?>
      <a href="http://www.henshaws.org.uk/">
        <?=_("Henshaws Society for Blind People")?> .
      </a>
    </p>
  </li>
  <li>
    <p>
      <a href="http://www.cnet.com.au/mp3players/musicsoftware/0,239029154,339273336,00.htm">
        <?=_("Master your MP3s with Audacity")?>
    </a> -
      <?=_("C|NET article about editing MP3 files for Web publishing, mobile phone ring tones, and podcasts.")?>
    </p>
  </li>
</ul>

<p><?=_('For additional help, see the <a href="documentation">Audacity documentation</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
