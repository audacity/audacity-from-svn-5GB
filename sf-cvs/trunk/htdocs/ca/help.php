<?php BoxTop($helpStr); ?>

<p>
<a href="docs/contents.html">Llegiu la documentació en línia</a>
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">Anglès</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Búlgar</a>
</ul>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>Tutorials</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Preguntes més freqüents (P.M.F.)</a>
</p>

<p>
<a href="http://www.geocrawler.com/lists/3/SourceForge/14679/0/"
>Arxius del llistat de correu de l'ajuda de l'Audacity</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>
