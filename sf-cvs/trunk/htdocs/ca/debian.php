<?php BoxTop("Debian"); ?>

<p>
L'Audacity és una part de la distribució tant <b>sid</b> com <b>woody</b> 
de Debian (en prova i inestable), per tant, instal·lar l'Audacity é tan simple com:
</p>
<xmp>$ apt-get update && apt-get install audacity</xmp>
<p>
Per qualsevol que encara utilitzi el potato (Debian 2.2) us podeu descarregar el deb (des de l'<a href="http://sourceforge.net/project/showfiles.php?group_id=6235">

àrea de descàrrega de fitxers de SourceForge</a>)
o afegir la següent línia al vostre /etc/apt/sources.list:
</p>
<xmp>
deb http://audacity.sourceforge.net potato/
</xmp>
<p><b>Atenció: </b>els debs de potato es troben molt desactualitzats. Des que la majoria de persones que utilitzen el Debian a l'escriptori fan servir el woody o sid, no s'ha publicat un deb des de la versió 0.96-0.98. El temps que i esforços que costaria construir-los és massa elevat comparat amb la demanda inexistent.</p>

<?php BoxBottom(); ?>