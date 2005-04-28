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

<h3><?=_("Lead Developers")?></h3>
<ul>
  <li><a href="http://dominic-mazzoni.com/">Dominic Mazzoni</a> - (dominic (at) audacityteam.org)
  <p><?=_("Dominic is the project leader and original author of Audacity.")?></p></li>
  <li><a href="http://advogato.org/person/mbrubeck/">Matt Brubeck</a> - (mbrubeck (at) audacityteam.org)</li>
  <li>James Crook - (james (at) audacityteam.org)</li>
  <li>Vaughan Johnson - (vaughan (at) audacityteam.org)</li>
  <li>Markus Meyer - (markus (at) audacityteam.org)</li>
</ul>

<h3><?=_("Active Developers")?></h3>
<ul>
  <li><a href="http://www.reverberate.org/">Joshua Haberman</a> - (joshua (at) audacityteam.org)</li>
  <li>Monty Montgomery</li>
  <li>Shane Mueller</li>
</ul>

<h3><?=_("Contributors")?></h3>
<ul>
  <li>William Bland</li>
  <li>Roger Dannenberg</li>
  <li>Brian Gunlogson</li>
  <li>Harvey Lubin</li>
  <li>Greg Mekkes</li>
  <li>Abe Milde</li>
  <li>Paul Nasca</li>
  <li>Tony Oetzmann</li>
  <li>Augustus Saunders</li>
  <li>Tom Woodhams</li>
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
  <li>Petri Vuorio (fi)</li>
  <li>Lionel Allorge (fr)</li>
  <li>Olivier Ballestraz (fr)</li>
  <li>Christian Brochec (fr)</li>
  <li>Fabrice Silva (fr)</li>
  <li>Micil Sheain Mhicil (ga)</li>
  <li>Márton Balázs - <a href="http://documan.sourceforge.net/">documan magyarító oldala</a> (hu)</li>
  <li>Jozsef Herczeg (hu)</li>
  <li>Aldo Boccacci (it)</li>
  <li>Ohkubo Kohei (ja)</li>
  <li>Ilija Iliev (mk)</li>
  <li><a href="http://folk.uio.no/kevinu">Kevin Brubeck Unhammer</a> (nb)</li>
  <li>Tino Meinen (nl)</li>
  <li>Tomasz Bandura (pl)</li>
  <li>Sebastian Pacholski (pl)</li>
  <li>Cleber Tavano (pt_BR)</li>
  <li>Yuri Ilyin (ru)</li>
  <li>Alexandre Prokoudine (ru)</li>
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
  <li>Richard Ash</li>
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
  <li>Verilogix, Inc.</li>
  <li><?=_("(Let us know if we're forgetting anyone!)")?></li>
</ul>

<?php
  include "../include/footer.inc.php";
?>
