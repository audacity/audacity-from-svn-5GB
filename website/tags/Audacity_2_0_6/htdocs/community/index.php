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

  <dt>
    <a href="developers">
      <?=_("Developers")?>
    </a>
  </dt>
  <dd>
    <p>
      <?=_("Audacity is written in C++. Our website is written in PHP. If you can help in either regard, we welcome you. To get started, join the <a href=\"http://lists.sourceforge.net/lists/listinfo/audacity-devel\">audacity-devel</a> mailing list, then tell us what you're interested in and ask questions.")?>
    </p>
  </dd>

  <dt>
    <?=_("Documenters and Testers")?>
  </dt>
  <dd>
    <p>
      <?=_("We welcome documentation and other contributions. For documentation, see our <a href=\"../help/\">Help</a> page. For testing, join the <a href=\"http://lists.sourceforge.net/lists/listinfo/audacity-quality\">audacity-quality</a> mailing list.")?>
    </p>
  </dd>

  <dt><a href="translation"><?=_("Translators")?></a></dt>
  <dd><p><?=_("If you are fluent in both English and another language, you can help translate the Audacity software, website, and <a href=\"http://manual.audacityteam.org/index.php?title=Main_Page\">Manual</a>. We also need people to help provide support for Audacity in different languages.")?></p></dd>
</dl>

<?=_("We also welcome <a href=\"../donate/\">donations</a> to support Audacity development.")?>

<?php
  include "../include/footer.inc.php";
?>
