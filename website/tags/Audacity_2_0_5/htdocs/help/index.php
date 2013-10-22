<?php
/*
 * Copyright 2004 - 2012 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("Documentation and Support");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<?php
echo _('<p><b>Questions?</b> First, check our <a href="faq">Frequently Asked Questions</a> and the following resources:</p>

<p>
 <ul>
   <li><a href="documentation">Manuals</a> - Read the Audacity Manual (with Tutorials). These Tutorials offer step-by-step help with common tasks in Audacity, such as making ringtones, removing vocals, creating podcasts and transferring tapes and records to computer.</li>
   <li><a href="http://wiki.audacityteam.org/">Audacity Wiki</a> is an extensive user-editable help resource including:
    <ul><li><a href="http://wiki.audacityteam.org/wiki/Category:Tutorial">Tutorials</a> - More specialised tutorials, including some <a href="http://wiki.audacityteam.org/wiki/Category:Tutorial#legacy">legacy tutorials</a> for old versions of Audacity, links to <a href="http://wiki.audacityteam.org/wiki/Category:Tutorial#external">other tutorials on the web</a> and some tutorials in <a href="http://wiki.audacityteam.org/wiki/MultiLingual">languages other than English</a>.</li>
     <li><a href="http://wiki.audacityteam.org/wiki/Category:Tips">Tips</a> - these articles will help you work with and understand Audacity and your audio hardware.</li>
    </ul>
  </li>
 </ul>
</p>')?>

<?php
// i18n-hint: If there are any special sources of help (local forums, lists) for
// Audacity users in your language (other than our Russian, French, German
// and Spanish forums), please add links to them at the end of this paragraph
// and at \htdocs\contact\index.php. If there is any translated documentation,
// please add links to it at the bottom of \htdocs\help\documentation.php

echo _('<p><b>Still need help?</b> Please read <a href="../contact/">Contact Us</a> for how to ask your question on the <a href="http://forum.audacityteam.org/">Audacity Forum</a>.</p>')?>

<?php
  include "../include/footer.inc.php";
?>
