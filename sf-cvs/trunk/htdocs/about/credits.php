<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "credits";
  $pageTitle = _("Credits");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>
<p><?=_("These are some of the people who have <a href=\"../community/\">contributed</a> to the free Audacity audio editor.</p>  
</p>If you have questions or feedback about Audacity, please see the <a href=\"../contact/\">contact page</a>.")?></p>

<h3><?=_("History")?></h3>

<p><?=_("Audacity was started by Dominic Mazzoni and Roger Dannenberg in the fall of 1999 at Carnegie Mellon University.  It was released as open-source software at SourceForge.net in May of 2000.")?></p>

<h3><?=_("Audacity Team Members")?></h3>
<ul>
  <li>Gale Andrews, <?=_("documentation and support")?></li>
  <li>Richard Ash, <?=_("documentation and support")?></li>
  <li>Christian Brochec, <?=_("documentation and support")?>, <?=_("French")?></li>
  <li>James Crook, <?=_("developer")?></li>
  <li>Vaughan Johnson, <?=_("developer")?></li>
  <li>Leland Lucius, <?=_("developer")?></li>
  <li>Dominic Mazzoni, <?=_("developer")?></li>
  <li>Markus Meyer, <?=_("developer")?></li>
  <li>Alexandre Prokoudine, <?=_("documentation and support")?></li>
  <li>Martyn Shaw, <?=_("developer")?></li>
</ul>

<h3><?=_("Technical leadership council")?></h3>
<ul>
  <li><a href="http://dominic-mazzoni.com/">Dominic Mazzoni</a> - (dominic (at) audacityteam.org)</li>
  <li><a href="http://claimid.com/mbrubeck">Matt Brubeck</a> - (mbrubeck (at) audacityteam.org)</li>
  <li>James Crook - (james (at) audacityteam.org)</li>
  <li>Vaughan Johnson - (vaughan (at) audacityteam.org)</li>
  <li>Leland Lucius - (leland (at) audacityteam.org)</li>
  <li>Markus Meyer - (markus (at) audacityteam.org)</li>
</ul>

<h3><?=_("Emeritus: distinguished Audacity team members, not currently active")?></h3>
<ul>
  <li><a href="http://claimid.com/mbrubeck">Matt Brubeck</a>, <?=_("developer")?></li>
  <li><a href="http://www.cs.cmu.edu/~rbd">Roger Dannenberg</a>, <?=_("developer")?></li>
  <li><a href="http://www.reverberate.org/">Joshua Haberman</a>, <?=_("developer")?></li>
  <li>Monty Montgomery, <?=_("developer")?></li>
  <li>Shane Mueller, <?=_("developer")?></li>
  <li>Tony Oetzmann, <?=_("documentation and support")?></li>
</ul>

<h3><?=_("Contributors")?></h3>
<ul>
  <li>William Bland, <?=_("developer")?></li>
  <li>Brian Gunlogson, <?=_("developer")?></li>
  <li>Harvey Lubin, <?=_("graphic artist")?></li>
  <li>Greg Mekkes, <?=_("developer")?></li>
  <li>Abe Milde, <?=_("developer")?></li>
  <li>Paul Nasca, <?=_("developer")?></li>
  <li>Augustus Saunders, <?=_("developer")?></li>
  <li>Tom Woodhams, <?=_("developer")?></li>
</ul>

<h3><?=_("Translators")?></h3>
<?php
?>
<ul>
  <li>Mikhail Balabanov (bg)</li>
  <li>Francesc Busquets (ca)</li>
  <li>Pau Crespo (ca)</li>
  <li>Aleš Tošovský (cs)</li>
  <li>Henrik Clausen (da)</li>
  <li>Christoph Kobe (de)</li>
  <li>Karsten Zeller (de)</li>
  <li>Antonio Paniagua (es)</li>
  <li>Ezequiel Plaza (es)</li>
  <li>Waldo Ramirez (es)</li>
  <li>Petri Vuorio (fi)</li>
  <li>Lionel Allorge (fr)</li>
  <li>Olivier Ballestraz (fr)</li>
  <li>Christian Brochec (fr)</li>
  <li>Fabrice Silva (fr)</li>
  <li>Micil Sheain Mhicil (ga)</li>
  <li>Xosé Antón Vicente Rodríguez (gl)</li>
  <li>Márton Balázs - <a href="http://documan.sourceforge.net/">documan magyarító oldala</a> (hu)</li>
  <li>Jozsef Herczeg (hu)</li>
  <li>Aldo Boccacci (it)</li>
  <li>Ohkubo Kohei (ja)</li>
  <li>Šarūnas Gliebus (lt)</li>
  <li>Ilija Iliev (mk)</li>
  <li><a href="http://folk.uio.no/kevinu">Kevin Brubeck Unhammer</a> (nb)</li>
  <li>Tino Meinen (nl)</li>
  <li>Tomasz Bandura (pl)</li>
  <li>Marek Mularczyk (pl)</li>
  <li>Sebastian Pacholski (pl)</li>
  <li>Cleber Tavano (pt_BR)</li>
  <li>Manuel Ciosici (ro)</li>
  <li>Yuri Ilyin (ru)</li>
  <li>Alexandre Prokoudine (ru)</li>
  <li>Joe Yeti (sk)</li>
  <li>Rok Hecl (sl)</li>
  <li>Martin Srebotnjak (sl)</li>
  <li>Lars Carlsson (sv)</li>
  <li>Maxim Dziumanenko (uk)</li>
  <li>XiaoXi Liu (zh_CN)</li>
  <li><a href="http://chido.idv.st/">Chido</a> (zh_TW)</li>
  <li>潘明忠 - <a href="http://rt.openfoundry.org/Foundry/Project/index.html?Queue=210">Audacity 中文化</a> (zh_TW)</li>
</ul>

<h3><?=_("Libraries")?></h3>
<p><?=_("Audacity is based on code from the following projects:")?></p>
<ul>
  <li><a href="http://www.jclark.com/xml/expat.html">expat</a></li>
  <li><a href="http://lame.sourceforge.net/">LAME</a></li>
  <li><a href="http://www.underbit.com/products/mad/">libmad</a></li>
  <li><a href="http://www.mega-nerd.com/libsndfile/">libsndfile</a></li>
  <li><a href="http://www-2.cs.cmu.edu/~rbd/doc/nyquist/root.html">Nyquist</a></li>
  <li><a href="http://vorbis.com/">Ogg Vorbis</a></li>
  <li><a href="http://www.portaudio.com/">PortAudio</a></li>
  <li><a href="http://www-ccrma.stanford.edu/~jos/resample/">Resample</a></li>
  <li><a href="http://sky.prohosting.com/oparviai/soundtouch/">SoundTouch</a></li>
  <li><a href="http://wxwidgets.org/">wxWidgets</a></li>
</ul>

<h3><?=_("Thanks")?></h3>
<p><?=_("There are many other people and organizations we would like to thank. Many of these have contributed code, patches, feedback, money, time, or other things to the project.  Others simply produce tools that made this whole thing possible.")?></p>
<ul>
  <li>Dave Beydler</li>
  <li>Jason Cohen</li>
  <li>Dave Fancella</li>
  <li>Steve Harris</li>
  <li>Daniel James</li>
  <li>Daniil Kolpakov</li>
  <li>Robert Leidle</li>
  <li>Logan Lewis</li>
  <li>David Luff</li>
  <li>Jason Pepas</li>
  <li>Mark Phillips</li>
  <li>Jonathan Ryshpan</li>
  <li>Patrick Shirkey</li>
  <li>David R. Sky</li>
  <li>Tuomas Suutari</li>
  <li>Mark Tomlinson</li>
  <li>David Topper</li>
  <li>Rudy Trubitt</li>
  <li>UmixIt Technologies, LLC</li>
  <li>Verilogix, Inc.</li>
  <li><?=_("(Let us know if we're forgetting anyone!)")?></li>
</ul>

<?php
  include "../include/footer.inc.php";
?>
