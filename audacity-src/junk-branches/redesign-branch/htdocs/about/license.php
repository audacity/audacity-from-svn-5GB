<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "license";
  $pageTitle = _("License");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<?php

// i18n-hint:  These gnu.org web pages are available in several languages.
// Please link to the version in your language, if possible.  For example:
//
//   http://www.gnu.org/licenses/licenses.es.html#GPL
//   http://www.gnu.org/licenses/gpl-faq.cs.html
//
// See the bottom of each page for versions in other languages.
//
echo _('<p>Audacity is free software; you can redistribute it and/or modify it under the terms of the <a href="http://www.gnu.org/licenses/licenses.html#GPL">GNU General Public License</a> as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.</p>

<p>This license gives you the permission to modify, copy, distribute, and sell Audacity, as long as the code you distribute is available under the GPL.  If you have questions, please <a href="../contact/">contact us</a>.

<p>See also: <a href="http://www.gnu.org/licenses/gpl-faq.html">Frequently Asked Questions about the GNU GPL</a>.</p>');

?>

<!-- TODO: Copyright and licenses for libraries. -->

<?php
  include "../include/footer.inc.php";
?>
