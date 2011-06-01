<?php
/*
 * Copyright 2006 - 11 Vaughan Johnson
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
  <link rel="icon" type="image/png" href="<?=$sitePath?>/favicon.png">
  <link rel="stylesheet" href="<?=$sitePath?>/style.css" type="text/css">
</head>
<body>
<p><a class="skip" href="#content"><?=_("Jump to page content")?></a></p>

<!-- SiteSearch Google -->
<form method="get" action="http://www.google.com/custom" target="_top">
<table border="0" bgcolor="#ffffff" align="right">
<tr><td nowrap="nowrap" valign="top" align="left" height="32">
<a href="http://www.google.com/">
<img src="http://www.google.com/logos/Logo_25wht.gif" border="0" alt="Google" align="middle"></img></a>
</td>
<td nowrap="nowrap">
<input type="hidden" name="domains" value="audacity.sourceforge.net;audacityteam.org"></input>
<label for="sbi" style="display: none">Enter your search terms</label>
<input type="text" name="q" size="31" maxlength="255" value="" id="sbi"></input>
<label for="sbb" style="display: none">Submit search form</label>
<input type="submit" name="sa" value="Search" id="sbb"></input>
</td></tr>
<tr>
<td>&nbsp;</td>
<td nowrap="nowrap">
<table>
<tr>
<td>
<input type="radio" name="sitesearch" value="audacity.sourceforge.net" checked id="ss1"></input>
<label for="ss1" title="Search audacity.sourceforge.net"><font size="-1" color="#000000">this site</font></label></td>
<td>
<input type="radio" name="sitesearch" value="audacityteam.org" id="ss2"></input>
<label for="ss2" title="Search audacityteam.org"><font size="-1" color="#000000">Wiki/Forum/Team site</font></label></td>
<td>
<input type="radio" name="sitesearch" value="" id="ss0"></input>
<label for="ss0" title="Search the Web"><font size="-1" color="#000000">Web</font></label></td>
</tr>
</table>
<input type="hidden" name="client" value="pub-2386514291404644"></input>
<input type="hidden" name="forid" value="1"></input>
<input type="hidden" name="ie" value="ISO-8859-1"></input>
<input type="hidden" name="oe" value="ISO-8859-1"></input>
<input type="hidden" name="safe" value="active"></input>
<input type="hidden" name="cof" value="GALT:#008000;GL:1;DIV:#336699;VLC:663399;AH:center;BGC:FFFFFF;LBGC:336699;ALC:0000FF;LC:0000FF;T:000000;GFNT:0000FF;GIMP:0000FF;LH:50;LW:127;L:http://audacity.sourceforge.net/images/Audacity-logo-r_50pct.jpg;S:http://audacity.sourceforge.net;FORID:1"></input>
<input type="hidden" name="hl" value="en"></input>
</td></tr></table>
</form>
<!-- SiteSearch Google -->

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

  <div id="adsense">
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
	google_ad_client = "pub-2386514291404644";
	google_ad_width = 160;
	google_ad_height = 600;
	google_ad_format = "160x600_as";
	google_ad_type = "text";
	google_ad_channel = "";
	google_color_border = "336699";
	google_color_bg = "FFFFFF";
	google_color_link = "0000FF";
	google_color_text = "000000";
	google_color_url = "008000";
	//-->
	</script>
	<script type="text/javascript"
	  src="http://pagead2.googlesyndication.com/pagead/show_ads.js">
	</script>
	
	<center>
		<a href="http://audacity.sourceforge.net/contact/privacy#advertising"><font size="-2"><?=_("Advertisements Policy")?></font></a>  
	</center>
  </div>

  </div> <!-- leftcolumn -->

  <?php
  }
?>

<div id="content"<?php if ($sectionNavItems) echo ' class="afternav"';?>>
