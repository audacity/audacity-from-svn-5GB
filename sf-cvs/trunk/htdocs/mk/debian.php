<?php BoxTop("Debian"); ?>

<p>
Audacity е дел од обете <b>sid</b> и <b>woody</b>
дистрибуции на Debian (тестирана и
нестабилна), па инсталирањето на Audacity е едноставно:
</p>
<xmp>$ apt-get update && apt-get install audacity</xmp>
<p>
Оние кои се се уште со potato (Debian 2.2) можат да симнат
deb (од <a href="http://sourceforge.net/project/showfiles.php?group_id=6235">

SourceForge датотеките</a>)
или додате ја следнава линија во вашиот /etc/apt/sources.list:
</p>
<xmp>
deb http://audacity.sourceforge.net potato/
</xmp>
<p><b>Внимание: </b>potato deb-от е застарен. Бидејќи повеќето кои
користат Debian работат со woody или sid, немам поставено
deb за верзиите 0.96-0.98. Тие бараат многу работа за изградба а
тоа е штета нели?</p>

<?php BoxBottom(); ?>
