<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("Documentation and Support");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";

  // i18n-hint: If there are any special resources for Audacity users in your
  // language (forums, documentation, mailing lists...), you can add links to
  // them here.
  echo _('<p>If you have a question, check the list of <a href="faq">Frequently Asked Questions</a>.</p>
<p>For instructions and help using Audacity, see:</p>
<ul>
  <li><a href="documentation">Documentation</a> - Read the user’s manual and quick reference.</li>
  <li><a href="tutorials">Tutorials</a> - Step by step articles teach you how to use Audacity.</li>
</ul>
<p>If your question isn’t answered here, please <a href="../contact/">contact us</a>.</p>');
?>

<h3><?=_('Other Resources')?></h3>
<ul>
	<li><?=_('<a href="http://audacityteam.org/wiki/">The Audacity Wiki</a> contains tips, tricks, and documentation that anyone can edit.')?></li>
</ul>

<?php
  include "../include/footer.inc.php";
?>
