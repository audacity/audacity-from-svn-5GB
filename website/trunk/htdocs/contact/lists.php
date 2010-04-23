<?php
/*
 * Copyright 2005 Matt Brubeck, 2008 - 2010 Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "lists";
  $pageTitle = _("Mailing Lists");
  include "../include/header.inc.php";

  $subscriberOnlyStr = _("You must subscribe to send messages to this list.");
	// i18n-hint: The "%s" will be replaced by the name of a mailing list.
  $archiveStr = _("%s archives and subscription information");

	echo "<h2>$pageTitle</h2>";

	// i18n-hint: Please add a note that these lists are in English.  If there are
	// Audacity mailing lists or forums in your language, please link to them here.
	echo "<p>"._('These lists are for discussion about the Audacity audio editor.  </p>
<p><b>Note:</b> In line with our <a href="../contact/privacy#lists">Privacy Policy</a>, these are public lists - messages are seen by all list subscribers, and posted on several web sites where the messages are archived.</p>');?>

<dl>
  <dt id="users">audacity-users</dt>
  <dd>
    <p><?=_('Discuss Audacity with other <a href="../community/users">users</a> and developers.')?> <?=$subscriberOnlyStr?></p>
    <p><a href="http://lists.sourceforge.net/lists/listinfo/audacity-users"><?php printf($archiveStr, "audacity-users")?></a>
  </dd>

<dt id="devel">audacity-devel</dt>
  <dd>
    <p><?=_('For  <a href="../community/developers">developers</a> working with the Audacity source code and documentation, and others interested in following our development process, or learning about compiling or developing our code.')?>  <?=$subscriberOnlyStr?></p>
    <p><a href="http://lists.sourceforge.net/lists/listinfo/audacity-devel"><?php printf($archiveStr, "audacity-devel")?></a>
    <p><?=_('We now use a <a href="http://www.bugzilla.org/">Bugzilla</a> installation to track bugs and enhancements. To find issues to work on, please view our categorized <a href="http://wiki.audacityteam.org/wiki/Bug_Lists">Bug Lists</a>.')?></p> 
  </dd>

<dt id="svn">audacity-svn</dt>
  <dd>
    <p><?=_('Receive automatic notification of every change made to the Audacity <a href="../community/developers#svn">source code</a>.')?> <?=$subscriberOnlyStr?></p>
	<p><a href="http://groups.google.co.uk/group/audacity-svn">
	<?php printf($archiveStr, "audacity-svn")?></a>
  </dd>

  <dt id="translation">audacity-translation</dt>
  <dd>
    <p><?=_('For <a href="../community/translation">translators</a> localizing the Audacity software, web site and documentation.')?>  <?=$subscriberOnlyStr?></p>
    <p><a href="http://lists.sourceforge.net/lists/listinfo/audacity-translation"><?php printf($archiveStr, "audacity-translation")?></a>
  </dd>

    <dt id="nyquist">audacity-nyquist</dt>
  <dd>
    <p><?=_('Share questions and tips about programming <a href="../help/nyquist">Nyquist plug-ins</a>.')?>  <?=$subscriberOnlyStr?></p>
    <p><a href="http://lists.sourceforge.net/lists/listinfo/audacity-nyquist"><?php printf($archiveStr, "audacity-nyquist")?></a>
  </dd>


  <?php
  include "../include/footer.inc.php";
?>
