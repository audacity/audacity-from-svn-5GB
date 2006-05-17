<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "developers";
  $pageTitle = _("Developers");
  include "../include/header.inc.php";

  $bugzilla_url = "http://limpet.net/audacity/bugzilla";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_('Mailing Lists')?></h3>
<p><?=_('Join the <a href="../contact/lists#devel">audacity-devel</a> mailing list to discuss current and future development, or to get help building and modifying the Audacity source code.')?></p>

<?=_('<h3 id="cvs">CVS</h3>
<p>You can get the latest development code from our CVS repository.</p>
<ul>
  <li><p><a href="http://audacity.cvs.sourceforge.net/audacity">Browse the Audacity source code online.</a></p></li>

  <li><p>Use this command to check out the latest Audacity 1.2 (stable) source code from CVS:</p>
  <p><kbd>cvs -d:pserver:anonymous@audacity.cvs.sourceforge.net:/cvsroot/audacity checkout -r AUDACITY_1_2 audacity</kbd></p></li>

  <li><p>Or use this command to check out the latest Audacity 1.3 (unstable) source code from CVS:</p>
  <p><kbd>cvs -d:pserver:anonymous@audacity.cvs.sourceforge.net:/cvsroot/audacity checkout audacity</kbd></p></li>
</ul>
<p>For more information, read the <a href="http://sourceforge.net/cvs/?group_id=6235">SourceForge CVS instructions</a>.</p>
<p>See the source code page for <a href="../download/source#instructions">compilation instructions</a>.</p>')?>

<?php printf(_('<h3 id="bugzilla">Bugzilla</h3>
<p>The Audacity developers use <a href="%s/">Bugzilla</a> to keep track of confirmed bugs.  Check the list of <a href="%s/buglist.cgi?bug_status=NEW&bug_status=ASSIGNED&bug_status=REOPENED">open bugs</a> if you are looking for something to work on.</p>
<p><strong>Note:</strong> This Bugzilla is for development use only.  If you are a user reporting new bugs, please <a href="../contact/">send us e-mail</a> instead.  Thank you!</p>'), $bugzilla_url, $bugzilla_url)?>

<?php
  include "../include/footer.inc.php";
?>
