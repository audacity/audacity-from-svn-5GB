<?php
/*
 * Copyright 2006 Vaughan Johnson
 * Copyright 2005 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html lang="<?=$lang?>">
<head>
  <title><?=_("Audacity")?>: <?=$pageTitle?></title>
  <meta http-equiv="Content-Type" content="text/html; charset=<?=$encoding?>">
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
<input type="hidden" name="domains" value="audacity.sourceforge.net"></input>
<input type="text" name="q" size="31" maxlength="255" value=""></input>
<input type="submit" name="sa" value="<?=_('Search')?>"></input>
</td></tr>
<tr>
<td>&nbsp;</td>
<td nowrap="nowrap">
<table>
<tr>
<td>
<input type="radio" name="sitesearch" value=""></input>
<font size="-1" color="#000000"><?=_('Web')?></font>
</td>
<td>
<input type="radio" name="sitesearch" value="audacity.sourceforge.net" checked="checked"></input>
<font size="-1" color="#000000">audacity.sourceforge.net</font>
</td>
</tr>
</table>
<input type="hidden" name="client" value="pub-2386514291404644"></input>
<input type="hidden" name="forid" value="1"></input>
<input type="hidden" name="channel" value="1707102428"></input>
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
  $titleImg = "<img title=\"$titleStr\" alt=\"$titleStr\" src=\"$sitePath/images/Audacity-logo-r_50pct.jpg\" width=\"253\" height=\"100\">";

  if ($sectionId == "" && $pageId == "") {
    echo "<h1>$titleImg</h1>";
  }
  else {
    echo "<h1><a href=\"$sitePath/\">$titleImg</a></h1>";
  }
?>

<div align="right">
	<a href="http://audacitystore.com/">
	<img src="../images/Audacity Store_banner_50pct.jpg" alt="<?=_('Audacity Store')?>"></img>
	</a>
	<br>
</div>

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
  <?php
  }
?>

<div id="content"<?php if ($sectionNavItems) echo ' class="afternav"';?>>
