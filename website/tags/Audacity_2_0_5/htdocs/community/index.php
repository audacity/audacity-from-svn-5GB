<?php
/*
 * Copyright 2004 - 12 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("Get Involved");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("Audacity is free software, developed by volunteers.  Here's how you can contribute your time to the Audacity project:")?></p>

<dl>
  <dt><a href="users"><?=_("Users")?></a></dt>
  <dd><p><?=_("Give us <a href=\"../contact#feedback\">suggestions or bug reports</a>, join the Audacity user community and help other users.")?></p></dd>

  <dt><a href="developers"><?=_("Developers")?></a></dt>
  <dd><p><?=_("If you are a C++ programmer, join the development team and help us improve Audacity.  We also welcome documentation and other contributions, and can help with questions on modifying the Audacity source code. To get started, please join the <a href=\"http://lists.sourceforge.net/lists/listinfo/audacity-devel\">audacity-devel</a> mailing list.")?></p></dd>

  <dt><a href="translation"><?=_("Translators")?></a></dt>
  <dd><p><?=_("If you are fluent in both English and another language, you can help translate the Audacity software, web site and <a href=\"http://manual.audacityteam.org/index.php?title=Main_Page\">Manual</a>. We also need people to help provide support for Audacity in different languages.")?></p></dd>
</dl>

<?=_("We also welcome <a href=\"../donate/\">donations</a> to support Audacity development.")?>

<?php
  include "../include/footer.inc.php";
?>
