<?php
/*
 * Copyright 2003, 2004 Dominic Mazzoni
 * Copyright 2004 - 12 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "users";
  $pageTitle = _("User Community");
  include "../include/header.inc.php";

	echo "<h2>$pageTitle</h2>";

	echo "<p>"._("Audacity was written by volunteers in their spare time.  If you appreciate Audacity and would like to help out, here are some ideas:")."</p>";
?>

<h3><?=_('Say Hello!')?></h3>
<p><?php printf(_('If you enjoy Audacity, <a href="%s">write to us</a> and let us know that you like it.  It makes us happy to know that people enjoy it.  Also feel free to send comments and suggestions.  We\'re always improving Audacity, and we value your input.'), "../contact#feedback")?></p>

<?php
	// i18n-hint:  Note that the wiki is in English.  If you want, you can create
	// a page in your language on the wiki, and link to it instead.
	echo _('<h3>Join the User Community</h3>')?>
<p><ul><li><?php printf(_('Subscribe to the <a href="%s">audacity-users mailing list</a> to share tips, questions and feedback with other Audacity users and developers.'), "../contact/lists#users")?></li> 
<li><?php printf(_('Visit the <a class="ext" target="blank" href="%s">Audacity Wiki</a>, a community resource where users can add or edit information about Audacity and digital audio.'), "http://wiki.audacityteam.org/")?></li>
<li><?php printf(_('Join our <a class="ext" target="blank" href="%s">Audacity Forum</a>, where you can ask and answer questions about Audacity.'), "http://forum.audacityteam.org/")?></li></ul></p>

<h3><?=_('Report bugs')?></h3>
<p><?php printf(_('If you find a bug in Audacity, please <a href="%s">let us know</a>.'), "../contact#feedback")?></p>

<h3><?=_('Help us test our latest versions')?></h3>
<p><?php printf(_('<a href="%s">Subscribe</a> to our announcements list for the latest news, and download our <a href="%s">latest release</a> or <a href="%s">Nightly Build</a>. Nightly Builds are aplha pre-releases that may contain new features or bug fixes that haven\'t been fully tested yet.'), "../#announce", "../download/", "http://wiki.audacityteam.org/wiki/Nightly_Builds")?></p>

<p><?php printf(_('After testing, please <a href="%s">let us know</a>:
<ul><li>Details of any problems you encountered</li>
<li>Any <em>regressions</em> - things that used to work but now don\'t</li>
<li>What you liked</li>
<li>What could be improved.</li></ul>.'), "../contact#feedback")?></p>

<?php
  include "../include/footer.inc.php";
?>
