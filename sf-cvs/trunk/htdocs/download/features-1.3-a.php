<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-a";
  $pageTitle = _("New features in Audacity 1.3");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>
<?php include "betawarning.inc.php" ?>

<h3><?=_("New features in Audacity 1.3.1 and 1.3.2")?></h3>

<ol>
<li><a href="features-1.3-h.php"><?=_("Improved Toolbar Docking")?></a></li>
<li><a href="features-1.3-i.php"><?=_("Track focus for improved keyboard navigation")?></a></li>
<li><a href="features-1.3-j.php"><?=_("Repair and Equalization effects")?></a></li>
<li><a href="features-1.3-k.php"><?=_("Timer Recording")?></a></li>
<li><a href="features-1.3-l.php"><?=_("Project saving and recovery")?></a></li>
<li><a href="features-1.3-d.php"><?=_("Selection Bar")?></a></li>
<li><a href="features-1.3-f.php"><?=_("Mac OS X features")?></a></li>
</ol>

<h3><?=_("New features in Audacity 1.3.0")?></h3>

<ol>
<li><a href="features-1.3-b.php"><?=_("Collapse/Expand Tracks")?></a></li>
<li><a href="features-1.3-c.php"><?=_("Multiple clips per track")?></a></li>
<li><a href="features-1.3-e.php"><?=_("Improved Label Tracks")?></a></li>
<li><a href="features-1.3-g.php"><?=_("Other features")?></a></li>
</ol>

<?php
  include "../include/footer.inc.php";
?>
