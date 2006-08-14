<?php
/*
 * Copyright 2005 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $sectionId = "";
  $pageTitle = _("Free Audio Editor and Recorder");
  include "include/header.inc.php";
  include "include/news.inc.php";
  include "latest/versions.inc.php";
  include "include/detect-os.inc.php";

  $download = which_download();
  if ($download == "windows") {
    $download_version = win_exe_version;
    $download_desc = _("for Microsoft Windows");
    $download_page = "windows";
    $beta_download_page = "beta_windows";

    include "beta/versions.inc.php";
    $beta_version = beta_win_exe_version;
  }
  else if ($download == "mac") {
    $download_version = macosx_version;
    $download_desc = _("for Mac OS X");
    $download_page = "mac";
    $beta_download_page = "beta_mac";

    include "beta/versions.inc.php";
    $beta_version = beta_macosx_version;
  }
  else {
    $download_version = src_version;
    $download_desc = _("For Windows, Mac, or GNU/Linux");
    $download_page = "";
    $beta_download_page = "";

    include "beta/versions.inc.php";
    $beta_version = beta_src_version;
  }
?>
<div id="about">
  <h2><?=_("The Free, Cross-Platform Sound Editor")?></h2>
  <div id="screenshot">
    <!-- TODO: Auto-select or randomly rotate screenshot? -->
    <a title="<?=_("Screenshots")?>" href="about/screenshots"><img alt="<?=_("Screenshots")?>" src="about/images/audacity-linux-small.jpg"></a>
  </div>
  <p><?=_('Audacity&reg; is free, open source software for recording and editing sounds.  It is available for Mac OS X, Microsoft Windows, GNU/Linux, and other operating systems.  <a href="about/">Learn more about Audacity...</a>')?></p>
</div>

<div id="download">
  <div id="download_sub">
  <h3><a href="download/<?=$download_page?>"><?php printf(_("Download Audacity %s"), $download_version)?></a></h3>
  <p><?=$download_desc?></p>
  </div>
  <div id="download_sub">
  <h3><a href="download/<?=$beta_download_page?>"><?php printf(_("Download Audacity %s"), $beta_version)?></a> (<?=_("Beta")?>)</h3>
  <p><?=$download_desc?></p>
  </div>

  <?php
    if ($download_page) {
      echo '<div id="download_sub">';

      if ($download == "mac") {
        echo '<p><a href="http://audacityteam.org/mac">';
        printf(_('Using an Intel Mac?  Click Here!'));
        echo '</a></p>';
      }

      echo '<p><a href="download/">'._("Other downloads").'</a></p>';
      echo '</div>';
    }
  ?>

</div>

<div id="news">
  <?php
    global $news_items;
    $item = array_shift($news_items);
    $dateStr = $item->dateStr();
  ?>
  <div class="newsitem">
    <h3><?="$dateStr: $item->title"?></h3>
    <?=$item->body?>
  </div>
  <h4><a href="about/news"><?=_("More news items...")?></a></h4>
</div>

<form id="notify" method="post" action="http://scripts.dreamhost.com/add_list.cgi">
  <h3><?=_("Get Notified of New Versions")?></h3>
  <p>
  <input type="hidden" name="list" value="audacity-announce">
  <input type="hidden" name="url" value="http://audacity.sourceforge.net/list/subscribed.php">
  <input type="hidden" name="emailconfirmurl" value="http://audacity.sourceforge.net/list/emailconfirm.php">
  <input type="hidden" name="unsuburl" value="http://audacity.sourceforge.net/list/unsubscribed.php">
  <input type="hidden" name="alreadyonurl" value="http://audacity.sourceforge.net/list/alreadyon.php">
  <input type="hidden" name="notonurl" value="http://audacity.sourceforge.net/list/noton.php">
  <input type="hidden" name="invalidurl" value="http://audacity.sourceforge.net/list/invalid.php">
  <input type="hidden" name="domain" value="audacityteam.org">
  <input type="hidden" name="emailit" value="1">

  <label for="address"><?=_("Email address")?></label>: <input name="address" id="address" class="text">
  <input type="submit" name="submit" value="<?=_("Add")?>">
  <input type="submit" name="unsub" value="<?=_("Remove")?>">
  </p>
</form>
<?php
  include "include/footer.inc.php";
?>
