<?php BoxTop($helpStr); ?>

<p>
<a href="docs/contents.html">Read documentation Online</a>
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">English</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Bulgarian</a>
<li><a href="audacity-manual-1.0.0-es.zip">Spanish</a>
</ul>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>Tutorials</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Frequently Asked Questions (F.A.Q.)</a>
</p>

<p>
<a href="http://www.geocrawler.com/lists/3/SourceForge/14679/0/"
>Audacity-Help Mailing List Archives</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>
