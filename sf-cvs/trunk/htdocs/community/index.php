<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("Get Involved");
  include "../include/header_adsense.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("Audacity is free software, developed by volunteers.  Here's how you can contribute to the Audacity project:")?></p>

<dl>
  <dt><a href="users"><?=_("Users")?></a></dt>
  <dd><p><?=_("Give us feedback, report bugs, and become part of the Audacity user community.")?></p></dd>

  <dt><a href="translation"><?=_("Translators")?></a></dt>
  <dd><p><?=_("If you are fluent in both English and another language, you can help translate the Audacity software and web site.  We also need people to help provide support for Audacity in different languages.")?></p></dd>

  <dt><a href="developers"><?=_("Developers")?></a></dt>
  <dd><p><?=_("If you are a C++ programmer, you can join the development team and help us improve Audacity.  We welcome documentation and other contributions too.")?></p></dd>
</dl>

<p><?=_("You may also <a href=\"donate\">donate money</a> to support Audacity
development.")?></p>

<?php
  include "../include/footer.inc.php";
?>
