<?php BoxTop("Debian"); ?>

<p>
Audacity es una parte tanto de la distribución <b>sid</b>
como de <b>woody</b> en Debian (de testeo e inestable), así que
instalar Audacity es tan simple como:
</p>
<xmp>
$ apt-get update && apt-get install audacity
</xmp>
<p>
Para cualquiera aún utilizando potato (Debian 2.2) puede descargar el
deb (de <a href="http://sourceforge.net/project/showfiles.php?group_id=6235">
el área de archivos de SourceForge</a>)
o agregar la siguiente línea a su /etc/apt/sources.list:
</p>
<xmp>
deb http://audacity.sourceforge.net potato/
</xmp>
<p><b>Advertencia: </b>los debs de potato están muy desactualizados. Debido a que
la mayoría de personas que utiliza Debian en escritorio usa woody o sid, no he
publicado un deb para versiones desde la 0.96 a la 0.98. Lleva su trabajo
construirlos, y no hay mucha demanda para ellos.
</p>

<?php BoxBottom(); ?>
