<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "beta_linux";
  $pageTitle = _("Linux/Unix");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('The beta version of Audacity has not been packaged for very many distributions yet.  You probably want to compile Audacity from <a href="beta_source">source code</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
