<?php
/*
 * Copyright 2005 Dominic Mazzoni
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("File Formats");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_("Audacity uses an open XML file format to encourage interoperability.  Beginning with version 1.3.0, we are publishing the following DTD to aid those who want to develop software that reads and writes Audacity project files:")?></p>

<p>
<a href="audacityproject-1.3.0.dtd">audacityproject-1.3.0.dtd</a>
</p>

<p>
<?=_("More documentation on the Audacity project file format is forthcoming.")?>
</p>

<?php
  include "../include/footer.inc.php";
?>
