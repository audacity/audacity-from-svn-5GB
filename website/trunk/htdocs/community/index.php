<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2008 Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("Get Involved");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("Audacity is free software, developed by volunteers.  Here's how you can contribute to the Audacity project:")?></p>

<dl>
  <dt><a href="users"><?=_("Users")?></a></dt>
  <dd><p><?=_("Give us feedback, report bugs, join the Audacity user community and help other users.")?></p></dd>

  <dt><a href="developers"><?=_("Developers")?></a></dt>
  <dd><p><?=_("If you are a C++ programmer, join the development team and help us improve Audacity.  We also welcome documentation and other contributions, and can help with questions on building and modifying the Audacity source code.")?></p></dd>

  <dt><a href="translation"><?=_("Translators")?></a></dt>
  <dd><p><?=_("If you are fluent in both English and another language, you can help translate the Audacity software, web site and <a href=\"http://manual.audacityteam.org/index.php?title=Main_Page\">Bata Manual</a>. We also need people to help provide support for Audacity in different languages.")?></p></dd>

  <dt><a href="donate"><?=_("Donate")?></a></dt>
  <dd><p><?=_("We welcome donations to support Audacity development.")?></p></dd>
</dl>

<?php
  include "../include/footer.inc.php";
?>
