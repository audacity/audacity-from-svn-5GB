<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2008 - 2013 Gale Andrews, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("Download");
  include "../include/header.inc.php";

  include "../latest/versions.inc.php";
  // include "../beta/versions.inc.php";
  // include "../legacy/versions.inc.php";
  $stable_version = stable_version;
  $beta_version = beta_version;
?>

<h2><?=$pageTitle?></h2>

<div class="download_container">
  <div class="downloads" id="stable_downloads">
    <div class="downloads_header">
      <h3><?php printf("Audacity %s", src_version)?></h3>
    </div>

    <div>
      <h3>
        <a href="windows">
          <img alt="Windows" src="../images/new_win.png">
          &nbsp;
          <?=_("Audacity for Windows&reg;")?>
          &nbsp;
        </a>
        &nbsp;
        <span class="downloads_smaller">
          <br>
          <img src="../images/download_spacer.png">
          <?=_("(Windows 2000/XP/Vista/Windows 7/Windows 8)")?>
        </span>
      </h3>

      <h3>
        <a href="mac">
          <img alt="Mac" src="../images/mac.png">
          &nbsp;
          <?=_("Audacity for Mac&reg;")?>
          &nbsp;
        </a>
        &nbsp;
        <span class="downloads_smaller">
          <br>
          <img src="../images/download_spacer.png">
          <?=_("(Universal Binary for Mac OS X 10.4 or later)")?>
        </span>
      </h3>

      <h3>
        <a href="linux">
          <img alt="Linux" src="../images/unix.png">
          &nbsp;
          <?=_("Audacity for GNU/Linux&reg;")?>
          &nbsp;
        </a>
        &nbsp;
        <span class="downloads_smaller">
          <br>
          <img src="../images/download_spacer.png">
            <?=_("(source code)")?>
        </span>
      </h3>

      <h3>
        <div class="downloads_notes">
          <?php printf('<a href="http://wiki.audacityteam.org/wiki/Release_Notes_%s">', src_version)?>
          <img src="../images/download_spacer.png">
          <?php printf(_("View Release Notes for Audacity %s"), src_version)?>
        </a>
        </div>
      </h3>

    </div>
  </div>
  <p>&nbsp;</p>
  <p>&nbsp;</p>
</div>

<div class="under_downloads">
  <h3 id="legacy"><?=_("Legacy Downloads")?></h3>
  <ul>
    <li><p><?=_("For users of <b>Windows 98 and ME</b>, a legacy 2.0.0 version of Audacity is available on the <a href=\"legacy_windows\">Legacy Windows downloads page")?></a>.</p></li>
    <li><p><?=_("For users of <b>Mac OS 9</b> and <b>Mac OS 10.0 through 10.3</b>, legacy versions of Audacity are available on the <a href=\"legacy_mac\">Legacy Mac downloads page")?></a>.</p></li>
    <li><p><?=_('<a href="http://sourceforge.net/projects/audacity/files/">All Previous Versions of Audacity</a> (click to enter the required folder under "Name").')?></p></li>
  </ul>

  <h3><?=_("Further Information")?></h3>
<ul>
    <li><p><?=_('You can also obtain Audacity as a <a href="buy">CD image file or purchasable CD</a>. Note that these are often not the latest version of Audacity.')?></p></li>
    <li><p><a href="features-2.0"><?=_("New Features in Audacity 2.0")?></a></p></li>
    <li><p><?=_('To build Audacity yourself, download the <a href="source">source code</a>.')?>
      </p></li>
    <li><p><?=_('You may copy, distribute, modify and/or resell Audacity, under the terms of the  <a href="../about/license">GNU GPL</a>.')?></p></li>
  </ul>
  <p>&nbsp;</p>
</div>

<!-- No longer show either betas or legacy downloads. -->
<!--<div class="under_downloads">
  <h3>
    <?=_("Legacy Downloads")?>
  </h3>
  <p>The following are obsolete, but are maintained for people with older operating systems.</p>

  <div class="under_downloads">
    <div class="download_container">
      <div class="downloads" id="beta_downloads">
        <div class="downloads_header">
          <h3>
            <a href="features-1.3-a">
              <?=_("1.3 Series (Beta)")?>
            </a>
          </h3>
        </div>
        <div>
          <h3>
            <img alt="Windows" src="../images/new_win.png">
              &nbsp;
              <a href="beta_windows">Audacity for Windows</a>
            </h3>

          <h3>
            <img alt="Mac" src="../images/mac.png">
              &nbsp;
              <a href="beta_mac">Audacity for Mac</a>
            </h3>

          <h3>
            <img alt="Linux" src="../images/unix.png">
              &nbsp;
              <a href="beta_linux">Audacity for GNU/Linux</a>
            </h3>
        </div>
      </div>
    </div>
  </div>

  <div class="download_container">
    <div class="downloads" id="legacy_downloads">
      <div class="downloads_header">
        <h3>
          <?=_("Audacity 1.2")?>
        </h3>
      </div>
      <div>
        <h3><img alt="Windows" src="../images/new_win.png">&nbsp;<a href="legacy_windows">Audacity for Windows</a></h3>
        <h3><img alt="Mac" src="../images/mac.png">&nbsp;<a href="legacy_mac">Audacity for Mac</a></h3>
        <h3><img alt="Linux" src="../images/unix.png">&nbsp;<a href="legacy_linux">Audacity for GNU/Linux</a></h3>
        --><!--<h3><a href="legacy_source">Source Code</a></h3>--><!--
      </div>
    </div>
  </div>
</div>-->

<?php
  include "../include/footer.inc.php";
?>