<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("Download");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("Select your operating system to download the latest version of the free Audacity sound editor.")?></p>

<div class="downloads">
  <h3><a href="windows"><img alt="" src="../images/win.png">
  <?=_("Windows")?></a></h3>

  <h3><a href="mac"><img alt="" src="../images/mac.png">
  <?=_("Mac OS 9 or X")?></a></h3>

  <h3><a href="linux"><img alt="" src="../images/unix.png">
  <?=_("Linux/Unix")?></a></h3>
</div>

<p><?=_('You can also <a href="buy">purchase</a> a copy of Audacity on CD.')?></p>

<p><?=_('If you wish to compile Audacity yourself, download the <a href="source">source code</a>.')?></p>

<p><?=_('You may modify, distribute, and sell Audacity under the <a href="../about/license">GNU GPL</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
