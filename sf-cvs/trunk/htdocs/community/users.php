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

<h3><?=_('Say Hello!')?></h3>
<p><?=_('If you enjoy Audacity,')?> <a href="../contact#feedback"><?=_('write to us')?></a> <?=_('and let us know that you like it.  It makes us happy to know that people enjoy it.  Also feel free to send comments and suggestions.  We\'re always improving Audacity, and we value your input.')?></p>

<?php
	// i18n-hint:  Note that the wiki is in English.  If you want, you can create
	// a page in your language on the wiki, and link to it instead.
	echo _('<h3>Join the User Community</h3>')?>
<p><?=_('Subscribe to the')?> <a href="../contact/lists#users"><?=_('audacity-users mailing list')?></a> <?=_('to share tips, questions and feedback with other Audacity users and developers.')?></p> 
<p><?=_('Visit the')?> <a class="ext" target="blank" href="http://audacityteam.org/wiki/"> <?=_('Audacity Wiki')?></a>, <?=_('a community resource where users can add or edit information about Audacity and digital audio.')?></p>
<p><?=_('Join our')?> <a class ="ext" target="blank" href="http://audacityteam.org/forum/"> <?=_('Audacity Forum')?></a>, <?=_('where you can ask and answer questions about Audacity.')?></p>

<h3><?=_('Report bugs')?></h3>
<p><?=_('If you find a bug in Audacity,')?> <a href="../contact/"><?=_('let us know')?></a>.</p>

<h3><?=_('Be a Beta Tester')?></h3>
<p><?=_('In addition to the latest stable version of Audacity, we usually have beta versions available.  These versions usually contain new features that haven\'t been fully tested. If you want to help out, ')?><a href="../download/"><?=_('download')?></a><?=_(' the Beta version and play with it.')?> <a href="../contact#feedback"><?=_('Let us know')?></a><?=_(' what you like and don\'t like, and how you think it should be improved.  Tell us if you find any <em>regressions</em> - things that used to work but don\'t work anymore.')?></p>

<?php
  include "../include/footer.inc.php";
?>
