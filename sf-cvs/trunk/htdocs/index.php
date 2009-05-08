<?php
/*
 * Copyright 2005 Matt Brubeck
 * 2007-9 Vaughan Johnson, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $sectionId = "";
  $pageTitle = _("Free Audio Editor and Recorder");
  include "include/header.inc.php";
  include "include/ninc.php";
  include "latest/versions.inc.php";
  include "include/detect-os.inc.php";

  $download = which_download();
  if ($download == "windows") {
    $download_version = win_exe_version;
    $download_desc = _("for Windows&reg; 98/ME/2000/XP/Vista");
    $download_page = "windows";
    $beta_download_desc = _("for Windows&reg; 98/ME/2000/XP/Vista");
    $beta_download_page = "beta_windows";

    include "beta/versions.inc.php";
    $beta_version = beta_win_exe_version;
  }
  else if ($download == "mac") {
    $download_version = macosx_version;
    $download_desc = _("for Mac OS X 10.1 or later");
    $download_page = "mac";
    $beta_download_desc = _("for Mac OS X 10.4 or later");
    $beta_download_page = "beta_mac";

    include "beta/versions.inc.php";
    $beta_version = beta_macosx_version;
  }
  else {
    $download_version = src_version;
    $download_desc = _("for Windows&reg;, Mac or GNU/Linux");
    $download_page = "";
    $beta_download_desc = _("for Windows&reg;, Mac or GNU/Linux"); 
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
  <p>
    <?=_('Audacity&reg; is free, open source software for recording and editing sounds.  It is available for Mac OS X, Microsoft Windows, GNU/Linux, and other operating systems. <a href="about/">Learn more about Audacity...</a> Also check our <a href="http://audacityteam.org/wiki/">Wiki</a> and <a href="http://audacityteam.org/forum/">Forum</a> for more information.')?></p>
  <p>
    <?=_('The latest release of Audacity is <a href="/download">1.3.7 (Beta)</a>. Because it is a work in progress and does not yet come with complete documentation or translations into foreign languages, it is recommended for more advanced users. See <a href="/download/features-1.3-a">New Features in 1.3</a> for more information about the 1.3.x Beta series.')?></p>
  <p>
    <?=_('For all users, <a href="/download">Audacity 1.2.6</a> is a stable release, complete and fully documented.  You can have Audacity 1.2.6 and 1.3.7 installed on the same machine.')?></p>
</div>

<div id="download">
  <div id="download_sub">
  <h3><a href="download/<?=$download_page?>"><?php printf(_("Download Audacity %s"), $download_version)?></a></h3>	
  <p><?=$download_desc?></p>
  </div>
  <div id="download_sub">
  <h3><a href="download/<?=$beta_download_page?>"><?php printf(_("Download Audacity %s"), $beta_version)?></a> (<?=_("Beta")?>)</h3>
  <p><?=$beta_download_desc?></p>
  </div>

  <?php
    if ($download_page) {
      echo '<div id="download_sub">';

      echo '<p><a href="download/">'._("Other downloads").'</a></p>';
      echo '</div>';
    }
  ?>

</div>

<table border="0" width="100%"><tr><td>&nbsp;</td></tr>
<table border="0" width="100%">
<tr><td><a href="http://sourceforge.net/community/cca09/nominate/?project_name=Audacity&project_url=http://audacity.sourceforge.net/">
<img src="http://sourceforge.net/images/cca/cca_nominate.png" border="0" alt="SourceForge.net Community Choice Awards"></a></td> 
<td>&nbsp;</td><td><p>Audacity is competing for the <b>"Best Project"</b> 
and <b>"Best Project for Multimedia"</b> categories in the 2009
<a href="http://sourceforge.net/community/cca09/">SourceForge.net Community Choice Awards</a>. Please <a href="http://sourceforge.net/community/cca09/nominate/?project_name=Audacity&project_url=http://audacity.sourceforge.net/">
nominate us now</a> for one or both categories and spread the word to others! Nominations close on May 29, 2009 at 4:00 PDT.</p> 
<p>The projects with the highest number of nominations in each category will be selected for the final round of voting commencing June 22, 2009.
</p></td></tr></table>

<div style="clear: both;"> </div>

<div id="news">
  <?php
    global $news_items;
    for ($i = 0; $i < 2; $i++)
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