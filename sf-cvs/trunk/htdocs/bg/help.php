<?php BoxTop($helpStr); ?>

<p>
<a href="docs/contents.html">Ръководство за потребителя/документация</a>
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">English</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Bulgarian</a>
</ul>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>Уроци</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Често задавани въпроси (ЧЗВ или FAQ)</a>
</p>

<p>
<a href="<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>Архиви на пощенския списък Audacity-Help</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>
