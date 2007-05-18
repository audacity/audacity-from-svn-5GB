<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  require_once "../beta/versions.inc.php";
  $pageId = "beta_mac";
  $pageTitle = _("Mac OS X");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<?php include "betawarning.inc.php" ?>
<?php include "betafeatures.inc.php" ?>

<p><?=$downloadTagline?></p>

<h3><?=_("Recommended Download")?></h3>
<?php include "recommended.inc.php"?>
<ul>
  <li><p>Universal Binary: <?php printf(_('<a href="%s">Audacity %s</a> (.dmg file, %.1lf MB)'), "../beta/".$macosx_ub_url, macosx_ub_version, macosx_ub_size)?></p></li>

  <!-- TODO: Installation instructions? -->
</ul>

<h3><?=_("System Requirements")?></h3>
<ul>
  <li><?=_("Audacity 1.3 requires Mac OS X 10.3 or later.")?></li>
  <li><?=_("Audacity runs best with at least 64 MB RAM and a 300 MHz processor.")?></li>
</ul>


<?php
  include "../include/footer.inc.php";
?>
