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
  "October 30, 2006",
        "1.3.2-release",
  _("Audacity 1.3.2 and 1.2.5 Released"),
  _("<p>
The Audacity developers have been busy with many new features over  
the past year.  We're pleased to announce
<a href=\"/download/features-1.3-a\">Audacity 1.3.2 (beta)</a>, which
contains dozens of new features and capabilities.  Because it is a work
in progress and does not yet come with complete documentation or translations
into foreign languages, it is recommended for more advanced users.
For all users, <a href=\"/download\">Audacity 1.2.5</a>
is a minor bug-fix update that addresses some problems with Audacity 1.2.4,
but does not add any significant new features.
It is complete and fully documented.  You can have
both Audacity 1.2.5 and 1.3.2 installed simultaneously.
Also, we have just made available a set of 92
<a href=\"/download/plugins\">LADSPA plug-ins for Windows</a>
(for both Audacity 1.2.x and 1.3.x).

</p><p>

See the <a href=\"/download/release-notes\">1.2.5 Release Notes</a> for a complete list of changes and known problems in Audacity 1.2.5, or see
<a href=\"/download/features-1.3-a\">New Features in 1.3</a>
for information about the new beta version.

</p>

<b>Beta version 1.3.2</b>
<dl><dt></dt><dd><!-- indent cheat -->
<dl>
       <dt> Usability improvements </dt>
             <dd> New selection bar </dd>
             <dd> New features for label tracks </dd>
             <dd> Improved toolbar docking flexibility </dd>
             <dd> Menu renaming and reorganization </dd>
             <dd> Selection, ruler, and playback control improvements </dd>
       <dt> Major improvements to some built-in effects </td>
              <dd> New Repair effect </dd>
              <dd> Improved Equalization effect </dd>
              <dd> Many fixes and minor improvements to other effects </dd>
       <dt> Improved accessibility for the visually impaired </dt>
             <dd> Improvements for screen readers, accessibility of tracks, and hot keys </dd>
       <dt> Timer recording </dt>
       <dt> Auto-save and automatic crash recovery </dt>
       <dt> New features and bug fixes for Nyquist </dt>
       <dt> Restructured Preferences dialog </dt>
       <dt> Improved batch processing </dt>
       <dt> File format export improvements </dt>
       <dt> Intel Mac support </dt>
       <dt> Many bug fixes and stability improvements </dt>
</dl>
</dd></dl>

<b>Stable version 1.2.5</b>
<dl><dt></dt><dd><!-- indent cheat -->
<dl>
       <dt> Support for new file formats, including FLAC </dt>
       <dt> Fixes for Mac audio problems </dt>
       <dt> Fix Generate Silence bug, a crash issue, and Linux GCC build issues </dt>
       <dt> Intel Mac support </dt>
</dl>
</dd></dl>")
);

add_news_item(
  "August 14, 2006",
	"AudacityStore",
  _("The Audacity Store Is Open"),
  _('
	<p>You are invited to try out the new <a href="http://audacitystore.com/">Audacity Store</a>, which features  
		Audacity-logo items (T-shirts, embroidered polo shirts, embroidered messenger bags), and consumer electronics.
	</p>
	<p align="center">
		<a href="http://audacitystore.com/">
		  <img src="../images/Audacity Store_banner_50pct.jpg" alt="Audacity Store"></img>
		</a>
	</p>
	<p><a href="http://audacity.sourceforge.net/community/donate">Learn more about how Audacity raises money...
		</a>
	</p>
	<h3>New Releases on the Way</h3>
	<p>Also, we will soon release updates to both the stable 1.2.x and development 1.3.x lines. 
		The new 1.3.x version will be 1.3.2 -- no official 1.3.1 because there are lots of changes 
			and several unofficial 1.3.1 builds have already been posted.
		Major changes in the 1.3.2 release include:
		<dl><dt></dt><dd><!-- indent cheat -->
			<dl>
				<dt>Preliminary Intel Mac Support</dt>
					<dd>pre-release builds at <a href="http://audacityteam.org/mac">http://audacityteam.org/mac</a></dd>
				<dt>Dependencies dialog</dt>
					<dd>lets user see dependencies of project on other files, and 
						copy audio data directly into the project</dd>
				<dt>Crash Recovery</dt><dd>automatically saved data makes recovery fast and easy</dd>
				<dt>New Repair Effect</dt><dd>smooths corrupted waveforms</dd>
				<dt>UI Changes</dt>
					<dd>themes (custom user interfaces), new toolbar docking features, new 
						time-specification controls, increased accessibility support, history 
						window changes, lots more</dd>
				<dt>Selection Bar Improvements</dt><dd>increased control and bug fixes</dd>
				<dt>Equalization Effect Improvements</dt><dd>better layout of Graphic EQ, faster animation of curve, increased control</dd>
				<dt>Many More LADSPA effects for Windows</dt><dd>most LADSPA effects now ported to Windows</dd>
				<dt>Numerous Bug Fixes</dt><dd>fixes to hotkeys, directories, Portable Audacity, crashes, etc.</dd>
				<dt></dt><dd></dd>
			</dl>
		</dd></dl>
	</p>
	')
);

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

?>
