<h2>Zmìny v Audacity 1.2</h2>

Tato stránka obsahuje popis nejvýznamnìjších zmìn od vydání naší poslední stabilní verze Audacity 1.0.0. Uživatelé betaverzí by se mìli podívat na changelog <a href="<?php print "betanotes.php$langQueryStr"; ?>">zmìny od vydání Audacity 1.1.0</a>.

<h3>Zvuk v profesionální kvalitì</h3>

<ul>
<li>
Audacity nyní umí nahrávat a upravovat 24 a 32 bitové vzorky (pohyblivá øadová èárka). V jednom projektu mohou být stopy s rozdílnými vzorkovacími frekvencemi i formáty, pøièemž pokud je tøeba, Audacity je zkonvertuje v reálném èase. Pro všechny konverze je použito vysoce kvalitní pøevzorkování a dithering.
</li>

<li>
Vstup a výstup zvuku v Audacity byl vylepšen. Audacity nyní mùže nahrávat více než dva kanály najednou. Aby se pøedešlo pøeskakování a podteèení vyrovnávací pamìti (buffer underruns), byla zlepšena práce s bufferem.
</li>
</ul>

<a href="screenshots.php<?php print $langQueryStr; ?>">
<img alt="screenshots" border="0" src="images/screen/linux/change-pitch.png" align="right"></a>
<h3>Efekty</h3>

<!--
-->

<ul>
<li>
Tøi nové efekty na zmìnu ladìní a rychlosti stopy:
 <ul>
<li>"Zmìna ladìní" zvýší nebo sníží zvuk vybrané èásti bez vlivu na rychlost.</li>
<li>"Zmìna tempa" zrychlí nebo zpomalí vybranou èást bez vlivu na výšku ladìní.</li>
<li>"Zmìna rychlosti" zmìní rychlost pøehrávání i výšku ladìní, jako když zmìníte rychlost gramofonu nebo magnetofonu.</li>
 </ul>
</li>

<li>
Vìtšina efektù nyní obsahuje tlaèítko "Preview", které umožòuje vyzkoušet rùzná nastavení pøed zavøením efektového okna. Nový pøíkaz vám umožní zopakovat poslednì použitý efekt bez opìtovného otevírání okna.
</li>

<li>
Další nové efekty obsahují:
 <ul>
<li>Kompresor, pro dynamic range kompresi.</li>
<li>Opakování, pro vytváøení smyèek ze vzorkù.</li>
<li>Normalizace, pro úpravu hlasitosti a opravu DC bias.</li>
 </ul>
</li>
</ul>


<h3>Nové editaèní vlastnosti</h3>

<ul>
<li>
Nástrojem Obálka, døíve používanému k postupnému zesilování a zeslabování stop, mùžete nyní také dosáhnout toho, aby stopy byly hlasitìjší nebo slabší než je originální hlasitost.
</li>

<li>
Nová vlastnost "Time track" je podobná obálce hlasitosti, ale rychlost se dá mìnit pøímo pøi pøehrávání.
</li>

<li>
Každá stopa má nyní v zájmu jednoduššího mixování vlastní nastavení Gain (zisk) a Pan (panorama).
<a href="screenshots.php<?php print $langQueryStr; ?>"><img alt="screenshots" border="0" src="images/screen/linux/track-controls.png" align="right"></a>
</li>

<li>
Audacity nyní umí najít nulové pøechody, èímž napomáhá snadnìjšímu vytváøení støihù a smyèek. Stisknìte "Z" a hranice výbìru se pøesunou na nejbližší nulový pøechod.
</li>
</ul>


<h3>Pluginy</h3>

<ul>
<li>
V Linuxu umí Audacity naèítat <a href="http://www.ladspa.org/">LADSPA</a> pluginy.
</li>

<li>
Audacity 1.2 pøináší jazyk pro manipulaci s digitálním signálem, zvaný <a href="nyquist.php">Nyquist</a>, který uživatelùm umožòuje naprogramovat nové efekty v jazyce podobném LISPu.
</li>
</ul>


<h3>Import a export souborù</h3>

<ul>
<li>
Projektové soubory Audacity 1.2 používají formát XML. Projektové soubory ze starších verzí se automaticky otevírají a konvertují.
</li>

<li>
Audacity 1.2 používá pro mnohem rychlejší dekódování souborù MP3 <a href="http://www.underbit.com/products/mad/">libmad</a>. <a href="http://www.zip.com.au/~erikd/libsndfile/">Libsndfile</a> od Erika de Castro Lopo poskytuje zlepšenou kompatibilitu s mnoha nekomprimovanými zvukovými souborovými formáty.
</li>

<li>
Dialog pro otevírání a import nyní dovoluje uživatelùm Audacity zvolit nìkolik zvukových souborù najednou a otevøít je v jednom projektu. Nový souborový formát "LOF" umožòuje Audacity otevøít skupinu souborù s offsety popsanými v textovém souboru.
</li>
</ul>


<h3>Vylepšené uživatelské rozhraní</h3>

<a href="screenshots.php<?php print $langQueryStr; ?>"><img alt="screenshots" border="0" src="images/screen/macosx/main-toolbar.png" align="right"></a>
<ul>
<li>
Nové editaèní a mixovací panely umožòující rychlý pøístup k bìžným funkcím.
</li>

<li>
Nový nástroj Kreslení umožòuje úpravy jednotlivých vzorkù pøi plném zvìtšení. 
</li>
<li>
Vícenástrojový režim poskytuje rychlý pøístup k rùzným editaèním funkcím bez pøepínání mezi nástroji.
</li>

<li>
Bylo pøidáno mnoho klávesových pøíkazù a klávesové zkratky jsou nyní nastavitelné.
</li>

<li>
Nové pøíkazy:
 <ul>
  <li>Pøehrávání ve smyèce.  Stisknìte "L", nebo pøi kliknutí na Play podržte shift.</li>
  <li>Stisknutím "1" pøehrajete jednovteøinovou ukázku zvuku kolem kurzoru.</li>
 </ul>
</li>

<li>
Koleèko myši se dá použít ke zvìtšování a zmenšování.
</li>

<li>
Stopy mohou být svisle zvìtšovány kliknutím nebo tažením na svislých pravítkách. Opìtovného zmenšení lze dosáhnout podržením shiftu pøi kliknutí nebo kliknutím pravým tlaèítkem.
</li>

<li>
Pravítko a stavový øádek nyní mohou zobrazovat èas v nìkolika rùzných formátech, vèetnì vteøin, vzorkù nebo video rámù.
</li>

<li>
Uživatelské rozhraní Audacity mùže nyní být pøeloženo do dalších jazykù. S pøekladem do vaší rodné øeèi mùžete dobrovolnì <a href="translation/">pomoci i vy</a>.
</li>
</ul>

<?/*  */ ?>
