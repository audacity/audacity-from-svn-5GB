<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
class NewsItem {
  var $id;
  var $date;
  var $title;
  var $body;

  // Constructor.
  function NewsItem($date, $title, $body) {
    $this->date = $date;
    $this->title = $title;
    $this->body = $body;
  }

  // Returns the date, in the preferred format for the current locale.
  function dateStr() {
    // i18n-hint: Controls how dates are formatted.
    // See http://www.php.net/manual/function.strftime.php for details.
    return locale_to_unicode(strftime(_("%B %d, %Y"), $this->date));
  }
}

$news_items = array();
function add_news_item($dateStr, $id, $title, $body) {
  global $news_items;
  $date = strtotime($dateStr);
  $key = strftime("%Y-%m-%d", $date)."/".$id;
  $news_items[$key] = new NewsItem($date, $title, $body);
}
function most_recent_news_item() {
}

// Add news items below in reverse-chronological order
// (most recent first).

add_news_item(
  "November 28, 2005",
	"1.2.4-release",
  _("Audacity 1.2.4 and 1.3.0 Released"),
  _('<p>Audacity 1.2.4 is a new stable version of Audacity.  It includes a couple of bug fixes and minor improvements and is recommended for all users.  Audacity 1.3.0 is a beta release that contains hundreds of <a href="/download/features-1.3-a">new features</a>, but this version is unfinished and unstable, and is recommended primarily for advanced users.  You can install both Audacity 1.2 and 1.3 simultaneously.</p>
<p>See the <a href="/download/release-notes">1.2.4 Release Notes</a> for a complete list of changes and known problems in Audacity 1.2.4, or see <a href="/download/features-1.3-a">New Features in 1.3</a> for information about the new beta version.</p>
<p>Finally, the best way to get help with Audacity is now the new
<a href="http://audacityteam.org/forum/">Audacity Forum</a>.
</p>')
);

add_news_item(
  "November 19, 2004",
	"1.2.3-release",
  _("Audacity 1.2.3 Released"),
  _('<p>Audacity 1.2.3 is a new stable version of Audacity. This version fixes a bug that interfered with long recordings on some Windows systems, and another bug that causes random crashes on Mac OS X. It also includes several updated translations, and other bug fixes and minor improvements.</p>
<p>See the <a href="/download/release-notes">Release Notes</a> for a complete list of changes and known problems in this version.</p>')
);
?>
