<?php
/*
 * Copyright 2004 Matt Brubeck
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
	<li><p><a href="http://audacity.sourceforge.net/manual-1.2/tutorials.html"><?=_("Audacity 1.2 Tutorials")?></a> - <?=_("From the official Audacity user's manual.")?></p></li>
	<li><p><a href="http://www.edhsonline.org/other/audacity/"><?=_("Audacity 1.2 Tutorial by Dan Eliot")?></a> - <?=_("Learn how to open and edit a WAV file, and save the result in an MP3.")?></p></li>
	<li><p><a href="http://www.agnula.org/documentation/dp_tutorials/audacity/"><?=_("Multi-Track Recording Tutorial from AGNULA")?></a> - <?=_("Make a simple multi-track recording.")?></p></li>
	<li><p><a href="http://quicktoots.linuxaudio.org/toots/audacity/"><?=_("Audacity Tutorial by Daniel James")?></a> - <?=_("Shows how to mix a multi-track project.  Includes sample sound files.")?></p></li>
	<li><p><a href="http://audacitybook.org/html/"><?=_("The Audacity Book")?></a> - <?=_("An online book for new Audacity users.  (This is an incomplete work in progress.)")?></p></li>
</ul>

<p><?=_('For additional help, see the <a href="documentation">Audacity documentation</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
