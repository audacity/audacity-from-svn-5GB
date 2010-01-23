<?php
/*
 * Copyright 2003, 2004 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "users";
  $pageTitle = _("User Community");
  include "../include/header.inc.php";

	echo "<h2>$pageTitle</h2>";

	echo "<p>"._("Audacity was written by volunteers in their spare time.  If you appreciate Audacity and would like to help out, here are some ideas:")."</p>";
?>

<?=_('<h3>Say Hello!</h3>
<p>If you enjoy Audacity, <a href="../contact/">write to us</a> and let us know that you like it.  It makes us happy to know that people enjoy it.  Also feel free to send comments and suggestions.  We\'re always improving Audacity, and we value your input.</p>')?>

<?php
	// i18n-hint:  Note that the wiki is in English.  If you want, you can create
	// a page in your language on the wiki, and link to it instead.
	echo _('<h3>Join the User Community</h3>
<p>Subscribe to the <a href="../contact/lists#users">audacity-users mailing list</a> to share tips, questions, and feedback with other Audacity users and developers.</p> <p>Visit the <a href="http://audacityteam.org/wiki/">Audacity Wiki</a>, a community resource where users can add or edit information about Audacity.</p>')
?>

<?=_('<h3>Report bugs</h3>
<p>If you find a bug in Audacity, <a href="../contact/">let us know</a>.</p>')?>

<?=_('<h3>Be a Beta Tester</h3>
<p>In addition to the latest stable version of Audacity, we often have beta versions available.  These versions usually contain new features that haven\'t been tested yet.  If you want to help out, download this version of Audacity and play with it.  Let us know what you like and don\'t like, and tell us how you think it should be improved.  Tell us if you find any <em>regressions</em> - things that used to work but don\'t work anymore.</p>')?>

<?php
  include "../include/footer.inc.php";
?>
