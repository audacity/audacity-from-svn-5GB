<?php
/*
 * index.php for audacity home
 *
 * Copyright 2005 Matt Brubeck
 * 2007 - 13 Vaughan Johnson, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $sectionId = "";
  $pageTitle = _("Free Audio Editor and Recorder");
  include "include/header.inc.php";
  include "latest/mirror.inc.php";
  include "include/news.inc.php";
  // include "beta/versions.inc.php";
  include "latest/versions.inc.php";
  include "include/detect-os.inc.php";

  $download = which_download();
  if ($download == "windows") {
    $download_version = win_exe_version;
    $download_OS = _("for Windows");
    $download_OS_versions = "2000/XP/Vista/Windows&nbsp;7/Windows&nbsp;8";
    $download_page = "windows";
    $recommended_download = $win_exe_url;
    /*
      $beta_version = beta_version;
      $beta_download_OS_versions = "2000/XP/Vista/7";
      $beta_download_page = "beta_windows";
      */
  }
  else if ($download == "mac") {
    $download_version = macosx_version;
    $download_OS = _("for Mac");
    $download_OS_versions = _("OS X 10.4 or later");
    $download_page = "mac";
    $recommended_download = $macosx_url;
    /*
      $beta_version = beta_version;
      $beta_download_OS_versions = _("Mac OS X 10.4 or later");
      $beta_download_page = "beta_mac";
      */
  }
  else {
    $download_version = src_version;
    $download_OS = _("for Windows, Mac or GNU/Linux");
    $download_OS_versions = "";
    $download_page = "source";
    $recommended_download = $min_src_url;
    /*
      $beta_version = beta_version;
      $beta_download_OS_versions = _("Windows, Mac or GNU/Linux"); 
      $beta_download_page = "";
      */
  }
?>

<div id="about">
  <h2><?=_("Audacity&reg; is free, open source, cross-platform software for recording and editing sounds.")?></h2>
  <div id="screenshot">
    <!-- TODO: Auto-select or randomly rotate screenshot? -->
    <a title="<?=_("Screenshots")?>" href="about/screenshots"><img alt="<?=_("Screenshots")?>" src="about/images/audacity-windows-small.jpg"></a>
  </div>
  <p>
    <?=_('Audacity is available for Windows&reg;, Mac&reg;, GNU/Linux&reg; and other operating systems. Check our <a href="about/features">feature list</a>, <a href="http://wiki.audacityteam.org/">Wiki</a> and <a href="http://forum.audacityteam.org/">Forum</a> for more information.')?></p>
</div>

<div id="download">
  <div id="download_sub">
    <a href="<?=download_url($recommended_download)?>"><?php printf(_("<h3>Download Audacity %s</h3></a>%s %s"), $download_version, $download_OS, $download_OS_versions)?>
  </div>

  <?php
    if ($download_page) {
      echo '<div id="download_sub">';
        echo '<p><a href="download/';
        printf(_("%s\">Other Audacity Downloads %s</a></p>"), $download_page, $download_OS);
      echo '</div>';
    }
  ?>

  <div id="download_sub">
    <p>
      <a href="download/"><?=_('All Audacity Downloads')?></a>
    </p>
  </div>

  <!--<div id="download_sub">
    <h3><a href="download/<?=$beta_download_page?>"><?php printf(_("Download Audacity %s %s"), $beta_version, $beta_download_OS_versions)?></a></h3>
    <p><?=$beta_download_OS_versions?></p>
  </div>-->
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

</div>
<hr>
<div id="adsense_home_page" style="border:1px solid black">
  <center>
    <!-- Google AdSense -->
    <script type="text/javascript">
    <!--
    google_ad_client = "ca-pub-2386514291404644";
    /* AdSense_home_page */
    google_ad_slot = "7680997128";
    google_ad_width = 728;
    google_ad_height = 90;
    //-->
    </script>
    <script type="text/javascript"
    src="http://pagead2.googlesyndication.com/pagead/show_ads.js">
    </script>
    <!-- end Google AdSense -->

    <p>
      <a href="/contact/privacy#advertising">
        <font size="-2">
          <?=_("Audacity Team's Advertisements Policy")?>
        </font>
      </a>
    </p>
  </center>
<!-- NO </div> here! footer.inc.php has it at top. -->

<?php
  include "include/footer.inc.php";
?>