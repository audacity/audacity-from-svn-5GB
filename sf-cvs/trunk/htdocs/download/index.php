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

  include "../latest/versions.inc.php";
  include "../beta/versions.inc.php";
  $stable_version = stable_version;
  $beta_version = beta_version;
?>

<h2><?=$pageTitle?></h2>

<p><?=_("Select your operating system to download the latest version of the free Audacity sound editor.")?></p>

<div class="download_container">
  <div class="downloads" id="stable_downloads">
    <div class="downloads_header">
      <h3><?=_("Stable").": ".$stable_version?></h3>
      <p><?=_("For all users")?></p>
    </div>
    <div>
      <h3><a href="windows"><img alt="" src="../images/win.png">
      <?=_("Windows")?></a></h3>
  
      <h3><a href="mac"><img alt="" src="../images/mac.png">
      <?=_("Mac OS 9 or X")?></a></h3>
  
      <h3><a href="linux"><img alt="" src="../images/unix.png">
      <?=_("Linux/Unix")?></a></h3>
    </div>
  </div>
  <div class="downloads" id="beta_downloads">
    <div class="downloads_header">
      <h3><?=_("Beta").": ".$beta_version?></h3>
      <p><?=_("For advanced users")?></p>
    </div>
    <div>
      <h3><a href="beta_windows"><img alt="" src="../images/win.png">
      <?=_("Windows")?></a></h3>
  
      <h3><a href="beta_mac"><img alt="" src="../images/mac.png">
      <?=_("Mac OS 9 or X")?></a></h3>
  
      <h3><a href="beta_linux"><img alt="" src="../images/unix.png">
      <?=_("Linux/Unix")?></a></h3>
  </div>
</div>

<div class="under_downloads">
<p><?=_('You can also <a href="buy">purchase</a> a copy of Audacity on CD.')?></p>

<p><?=_('If you wish to compile Audacity yourself, download the <a href="source">source code</a>.')?></p>

<p><?=_('You may modify, distribute, and sell Audacity under the <a href="../about/license">GNU GPL</a>.')?></p>
</div>

<?php
  include "../include/footer.inc.php";
?>
