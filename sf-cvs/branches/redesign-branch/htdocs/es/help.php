<?php BoxTop($helpStr); ?>

<p>
<a href="docs/contents.html">Lee la documentación Online</a>
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">Inglés</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Búlgaro</a>
<li><a href="audacity-manual-1.0.0-es.zip">Español</a>
</ul>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>Tutoriales</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Preguntas Frecuentes</a>
</p>

<p>
<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>Archivos de la lista de correos en cadena Audacity-Help</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<p>
<hr>
</p>

<p>
<a href="docs1.1">(Sin Terminar) documentación para Audacity 1.1</a>
</p>

<?php BoxBottom(); ?>
