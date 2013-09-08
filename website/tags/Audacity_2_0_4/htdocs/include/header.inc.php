<?php
/*
 * Copyright 2006 - 13 Vaughan Johnson
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html lang="<?=$lang?>">
<head>
  <title><?=_("Audacity")?>: <?=$pageTitle?></title>
  <meta http-equiv="Content-Type" content="text/html; charset=<?=$encoding?>">
  <meta http-equiv="pragma" content="no-cache" />
  <meta name="keywords" content="Audacity audio software recording editing sounds">
  <link rel="shortcut icon" href="<?=$sitePath?>/favicon.ico">
  <link rel="stylesheet" href="<?=$sitePath?>/style.css" type="text/css">

  <!-- Google+ : This part needs to be within <head>. The badge is shown in the footer. -->
   <link href="https://plus.google.com/108226440145001840649" rel="Audacity Team" />
  <!-- end Google+ -->
</head>
<body>
<p><a class="skip" href="#content"><?=_("Jump to page content")?></a></p>

<!-- Google Custom Search Engine -->
   <!-- search box code -->
      <form action="http://www.google.com" id="cse-search-box" target="blank">
         <table border="0" bgcolor="#ffffff" align="right">
          <tr><td>
            <div>
               <input type="hidden" name="cx" value="partner-pub-2386514291404644:6498816767" />
               <input type="hidden" name="ie" value="UTF-8" />
               <input type="text" name="q" size="45" />
               <input type="submit" name="sa" value="Search" />
            </div>
          </td></tr>
          <tr><td align="center">
             <font size="-1"><i>Search Audacity sites and other selected sites.</i></font>
          </td></tr>
         </table>
      </form>
      <script type="text/javascript" src="http://www.google.com/coop/cse/brand?form=cse-search-box&amp;lang=en"></script>
   <!-- end search box code -->
   <!-- /* Vaughan, 2013-03-22: This optional code from Google is causing display problems against the supplied "search box code" above, 
      that's modified from Google-supplied, to be top-right on our site. Popular queries seem to work, regardless. Revisit later. */
         popular queries code
            <script type="text/javascript" src="http://www.google.com/cse/query_renderer.js"></script>
            <div id="queries"></div>
            <script src="http://www.google.com/cse/api/partner-pub-2386514291404644/cse/6498816767/queries/js?oe=UTF-8&amp;callback=(new+PopularQueryRenderer(document.getElementById(%22queries%22))).render"></script>     
        end popular queries code 
   --> 
<!-- end Google Custom Search Engine -->

<?php
  $titleStr = _("Audacity: Free Sound Editor and Recording Software");
  $titleImg = "<img title=\"$titleStr\" alt=\"$titleStr\" src=\"$sitePath/images/Audacity-logo-r_50pct.jpg\">";

  if ($sectionId == "" && $pageId == "") {
    echo "<h1>$titleImg</h1>";
  }
  else {
    echo "<h1><a href=\"$sitePath/\">$titleImg</a></h1>";
  }
?>

<!--<div align="right">
   <a title="<?=_('Audacity Store, for Audacity-logo shirts, bags and ceramic mugs')?>" href="http://audacitystore.com/">
   <img src="../images/Audacity Store_banner_50pct.jpg" alt="<?=_('Audacity Store, for Audacity-logo shirts, bags and ceramic mugs')?>" />
   </a>
   <br>
</div>-->

<div id="sitenav">
  <h2><?=_("Site Navigation")?></h2>
  <ul>
    <?php
      foreach ($siteNavItems as $navItem) {
        $name = $navItem[0];
        $path = $navItem[1];
        if ($sectionId == $path) {
          $sectionNavItems = $navItem[2];
          ?><li class="selected"><?=$name?></li><?php
        }
        else {
          ?><li><a href="<?=$sitePath?>/<?=$path?>"><?=$name?></a></li><?php
        }
      }
    ?>
  </ul>
</div>

<?php
  if ($sectionNavItems) {
  ?>

  <div id="leftcolumn">

  <div id="subnav">
    <?php
      if ($pageId == "") {
        ?><h3 class="selected"><?=$sectionTitle?></h3><?php
      }
      else {
        ?><h3><a href="<?=$sectionPath?>/"><?=$sectionTitle?></a></h3><?php
      }
    ?>
    <ul>
      <?php
        foreach ($sectionNavItems as $navItem) {
          $name = $navItem[0];
          $path = $navItem[1];
          $indented = "";
          $heading = "";
          $selected = "";
          if (substr($name, 0, 1)=="\t") {
            $name = substr($name, 1);
            $indented = 1;
          }

          if ($path == "") {
            $heading = 1;
            $selected = 1;
          }
          else if ($pageId == $path) {
            $selected = 1;
          }

          if ($selected) {
            if ($heading) {
              $li_class = "class=\"heading\"";
       }
            else if ($indented) {
              $li_class = "class=\"indented_selected\"";
            }
            else {
              $li_class = "class=\"selected\"";
            }

          }
          else {
            if ($indented) {
              $li_class = "class=\"indented\"";
            }
            else {
              $li_class = "";
            }
          }

          if ($selected) {
            ?><li <?=$li_class?>><?=$indent?><?=$name?></li><?php
          }
          else {
            ?><li <?=$li_class?>><a href="<?=$sectionPath?>/<?=$path?>"><?=$name?></a></li><?php
          }
        }
      ?>
    </ul>
  </div>

  <div id="adsense" style="border:2px solid black">
   <?php
   /* 4/30/2007: Ross Turk of SourceForge says please just direct all revenue to 
      Audacity for one month, due to error I made because of their documentation. 
      6/1/2007: The above is the Sourceforge code (http://downloads.sourceforge.net/sitedocs/afc_setup.pdf). 
      This commented out stuff is the code that puts up a very similar type of ad unit, 
      in case we ever need it again.
      6/3/2007: Something's wrong with their script, so we've plummeted from ~85k pageviews per day 
      to ~35. Revert to commenting out the SourceForge scripts and doing the Google script.
      - Vaughan

      <script type="text/javascript">
      var dfp_ord = Math.random()*10000000000000000;
      var partner_clientid = '2386514291404644';
      var unixgroupname = 'audacity';
      </script>
      <script type="text/javascript" src="http://genweb.ostg.com/google/sfproject.js"></script>
   */
   ?>

   <script type="text/javascript"><!--
   google_ad_client = "ca-pub-2386514291404644";
   /* AdSense_under_navbar */
   google_ad_slot = "4949551373";
   google_ad_width = 160;
   google_ad_height = 600;
   //-->
   </script>
   <script type="text/javascript"
     src="http://pagead2.googlesyndication.com/pagead/show_ads.js">
   </script>
   
   <center>
      <a href="/contact/privacy#advertising"><font size="-2"><?=_("Advertisements Policy")?></font></a>
   </center>

  </div>

  </div> <!-- leftcolumn -->

  <?php
  }
?>

<div id="content"<?php if ($sectionNavItems) echo ' class="afternav"';?>>
