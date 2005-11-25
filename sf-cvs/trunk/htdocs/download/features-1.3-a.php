<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3";
  $pageTitle = _("New features in Audacity 1.3");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_("Beta warning")?></h3>
<p><?=_("Audacity 1.3 is an unstable, beta version of Audacity.  Users who want a completely stable, finished program should be using Audacity 1.2.  Audacity 1.3 has hundreds of new features, but some features may be incomplete or confusing, it is more likely to crash, and you are more likely to lose data.  You can install both Audacity 1.2 and 1.3 on the same machine.  When this branch of Audacity is finished, it will be released as version 1.4, which will be stable and complete.")?></p>
<p><?=_("Audacity 1.3.0 is currently only available in English.  Future Audacity 1.3 releases will be translated into many other languages.")?></p>
<h3><?=_("New features")?></h3>
<p><?=_("The new features in Audacity 1.3 have been grouped into the following six major categories.  Click on the links below for more information.")?></p>

<ol>
<li><a href="features-1.3-b.php"><?=_("Collapse/Expand Tracks")?></a></li>
<li><a href="features-1.3-c.php"><?=_("Multiple clips per track")?></a></li>
<li><a href="features-1.3-d.php"><?=_("Selection Bar")?></a></li>
<li><a href="features-1.3-e.php"><?=_("Improved Label Tracks")?></a></li>
<li><a href="features-1.3-f.php"><?=_("QuickTime and Audio Units on Mac OS X")?></a></li>
<li><a href="features-1.3-g.php"><?=_("Other features")?></a></li>
</ol>

<?php
  include "../include/footer.inc.php";
?>
