<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2008 - 2012 Gale Andrews, Vaughan Johnson
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
      <h3><?=_("Audacity 2.0")?></h3>
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
          <?=_("(Windows 98/ME/2000/XP/Vista/7)")?>
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
    </div>
  </div>
  <p>&nbsp;</p>
  <p>&nbsp;</p>
</div>

<div class="under_downloads">
  <h3><?=_("Further Information")?></h3>
  <ul>
    <li><p><?=_("For users of <b>Mac OS 9</b> and <b>Mac OS 10.0 through 10.3</b>, legacy versions of Audacity are available on the ")?><a href="legacy_mac"><?=_("Legacy Mac downloads page")?></a>.</p></li>
    <li><p><?=_('You can also obtain Audacity as a <a href="buy">CD image file or purchasable CD</a>. Note that these are often not the latest version of Audacity.')?></p></li>
    <li><p><a href="features-2.0"><?=_("New Features")?></a></p></li>
    
    <!-- Note that release notes link will need to change to "http://wiki.audacityteam.org/wiki/Release_Notes" for next release after 2.0.0, but there's too little there, currently. -->
    <li><p><a href="http://wiki.audacityteam.org/wiki/Release_Notes_2.0.0"><?=_("Release Notes")?></a></p></li>

    <li>
      <p>
        <?=_('<a href="http://sourceforge.net/projects/audacity">Older Versions of Audacity</a>')?>
      </p>
    </li>
    <li><p><?=_('To build Audacity yourself, download the <a href="source-v">source code</a>.')?>
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
              <a href="beta_windows-v">Audacity for Windows</a>
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