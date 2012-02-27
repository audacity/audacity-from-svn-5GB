<?php
/*
 * Copyright 2005 Matt Brubeck
 * 2007 - 12 Vaughan Johnson, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
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
    $download_desc = _("for Windows&reg; 98/ME/2000/XP");
    $download_page = "windows";
    $beta_download_desc = _("for Windows&reg; 98/ME/2000/XP/Vista/7");
    $beta_download_page = "beta_windows";

    include "beta/versions.inc.php";
    $beta_version = beta_version;
  }
  else if ($download == "mac") {
    $download_version = macosx_version;
    $download_desc = _("for Mac OS X 10.0 to 10.3");
    $download_page = "mac";
    $beta_download_desc = _("for Mac OS X 10.4 or later");
    $beta_download_page = "beta_mac";

    include "beta/versions.inc.php";
    $beta_version = beta_version;
  }
  else {
    $download_version = src_version;
    $download_desc = _("for Windows&reg;, Mac or GNU/Linux");
    $download_page = "";
    $beta_download_desc = _("for Windows&reg;, Mac or GNU/Linux"); 
    $beta_download_page = "";

    include "beta/versions.inc.php";
    $beta_version = beta_version;
  }
?>
<div id="about">
  <h2><?=_("Audacity&reg; is free, open source, cross-platform software for recording & editing sounds.")?></h2>
  <div id="screenshot">
    <!-- TODO: Auto-select or randomly rotate screenshot? -->
    <a title="<?=_("Screenshots")?>" href="about/screenshots"><img alt="<?=_("Screenshots")?>" src="about/images/audacity-linux-small.jpg"></a>
  </div>
  <p>
<?=_('Audacity is available for Mac OS X, Microsoft Windows, GNU/Linux, and other operating systems. <a href="about/">Learn more about Audacity...</a> Also check our <a href="http://audacityteam.org/wiki/">Wiki</a> and <a href="http://audacityteam.org/forum/">Forum</a> for more information.')?></p>
  <p>
<b><?php printf(_('The current release of Audacity is <a href="download/">%s'), beta_version)?></a></b>. <?=_(' Most users should download this version. See <a href="download/features-1.3-a">New Features</a> for more information. We will soon release version 2.0, replacing both this version and 1.2.')?></p>
</div>

<div id="download">
  <div id="download_sub">
  <h3><a href="download/<?=$beta_download_page?>"><?php printf(_("Download Audacity %s"), $beta_version)?></a></h3>
  <p><?=$beta_download_desc?></p>
  </div> 
<!--<div id="download_sub">
  <h3><a href="download/<?=$download_page?>"><?php printf(_("Download Audacity %s"), $download_version)?></a></h3>	
  <p><?=$download_desc?></p>
  </div>-->

  <?php
    if ($download_page) {
      echo '<div id="download_sub">';

      echo '<p><a href="download/">'._("Other downloads").'</a></p>';
      echo '</div>';
    }
  ?>

</div>

<div style="clear: both;"> </div>

<div id="news">
  <?php
    global $news_items;
    /* Revert to showing only one news item on front page.
      for ($i = 0; $i < 2; $i++) 
      */
    for ($i = 0; $i < 1; $i++)
    {
        $item = array_shift($news_items);
        $dateStr = $item->dateStr();
        echo "<div class=\"newsitem\"><h3>";
        echo $dateStr . ": " . $item->title;
        echo "</h3>" . $item->body . "</div>";
    }
  ?>
  <h4>
    <a href="about/news">
      <?=_("More news items...")?>
    </a>
  </h4>
</div>

<form id="notify" method="post" action="http://scripts.dreamhost.com/add_list.cgi">
  <h3 id="announce"><?=_("Get Notified of New Versions")?></h3>
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