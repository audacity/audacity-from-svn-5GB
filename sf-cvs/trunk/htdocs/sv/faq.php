<?php BoxTop("Frequently Asked Questions - Vanliga ställda frågor"); ?>

<!--
<table width=100% cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+3">Vanliga ställda frågor</font>
</td></tr></table>
-->

<p>

General:<br>
<a href="#g1">Är Audacity verkligen gratis? Varför?</a><br>
<a href="#g2">Vem utvecklar Audacity?</a><br>
<a href="#g3">Hur kan jag hjälpa till med att förbättra Audacity?</a><br>
<a href="#g4">Jag har hittat en bug! Vad gör jag nu?</a><br>

<p>

Installation:<br>

<a href="#i1">Windows: Vad gör jag med .exe filen?</a><br>
<a href="#i2">Windows: Hur avinstallerar jag Audacity?</a><br>
<a href="#i3">Mac: Vad gör jag med .sit filen?</a><br>
<a href="#i4">Unix: Varför får jag "failed dependencies" när jag installerar via RPM?</a><br>
<a href="#i5">Unix: Varför får jag ett felmeddelande när den laddar libwx_gtk.so?</a><br>

<p>

Använda Audacity:<br>

<a href="#a1">Hur lägger jag ihop två spår?</a><br>
<a href="#a2">Kan jag ta bort sången från en inspelning?</a><br>

<a href="#a3">När jag försöker spela in stämmor med mig själv,
varför är de två spåren inte synkroniserade?</a><br>
<a href="#a4">Varför är ingen MP3-kodare inkluderad?</a><br>
<a href="#a5">Kan jag spela in RealAudio eller eller annat strömmande ljud?</a><br>
<a href="#a6">Hjälp! Jag har tryckt "spela in" men allt jag får är tystnad!</a><br>
<a href="#a7">Hur delar jag upp en fil i flera spår?</a><br>
<a href="#a8">Hur kan jag få låtar från en ljud-CD?</a><br>

<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Allmänt</font>
</td></tr></table>

<p>

<a name="g1"></a>
<font size=+1><b>
1. Är Audacity verkligen gratis? Varför?<br>

</b></font>

<p>

Ja,  Audacity är inte bara distribuerad
utan kostnad, men du har också möjligheten att göra nästan
vad du vill med det.
<p>
Audacity är distribuerad under termerna för
<a href="http://www.gnu.org/copyleft/gpl.html"
>General Public License (GPL)</a>.
Du har friheten att använda denna programvara för personligt eller kommersiellt
ändamål.  Du har också friheten att fritt dela med dig av programmet eller sälja det.
Källkoden till detta program är fritt tillgänglig på
Internet, och du har friheten att ändra det för eget bruk, men
ändringar måste också bli distribuerade under GPL.
<p>
Audacity är byggt med hjälp av wxWidgets, ett mjukvarubibliotek som
är utgiven under den mindre restriktiva LGPL.  
För mer information, gå till
<a href="http://wxwidgets.org/">wxwidgets.org</a>.
<p>
Utvecklarna av Audacity har valt att ge ut den under GPL
av många anledningar.  Många av oss gör det för att vara generösa.
Några gör det för moralen, därför att vi tycker att
all mjukvara  borde vara gratis, medans andra tror att
det finns ett behov av både gratis och kommersiell mjukvara i världen.
<p>
En anledning till att Audacity är gratis är att den blir mer allmänt förekommande.
Många av oss skulle hellre se en miljon glada människor använda
Audacity gratis än tusen människor som betalar. De flera blir mer nöjda med programmet om det är gratis.

<p>
Ännu en anledning är att det främjar samarbete. Om Audacity
var shareware, skulle det inte vara troligt att dussintals människor i
världen skulle ha bidragit med kod, bugg-fixar, dokumentation,
och grafik.

<p>
<a name="g2"></a>
<font size=+1><b>
2. Vem utvecklar Audacity?<br>
</b></font>

<p>

Audacity-projektet startades hösten 1999
av Dominic Mazzoni medan han var student vid Carnegie Mellon Universitet i Pittsburgh, PA, USA.
Han arbetade på ett forskningsprojekt med hans handledare, Professor
Roger Dannenberg, och dom behövde ett verktyg som gav dem möjligheten att
visualisera ljud-analysalgorithmer.  Tiden gick, och detta program
utvecklades till ett generellt ljudredigeringsprogram, och andra människor började
hjälpa till.
<p>
Idag är Audacity utvecklat med användandet av Sourceforge, en hemsida
som tillåter människor från hela världen att samarbeta med gratis
mjukvaru projekt.  Se <a href="http://www.sourceforge.net"
>sourceforge.net</a> för mer information.
Dussintals människor har bidragit till
Audacity, och utvecklingen fortsätter att accelerera.

<p>

<a name="g3"></a>
<font size=+1><b>
3. Hur kan jag hjälpa till med att förbättra Audacity?<br>
</b></font>

Leta rätt på buggar och meddela oss.
Skriv kod.  Översätta det till ditt eget språk. Rita grafik.
Bli medlem i Audacityqnvändarnas postlista. Skicka pengar till oss.
<p>
Var vänlig se vår nya <?php print "<a href=donatetime.php?$langLinkStr>"; ?>Bidra med pengar</a> 
för mer detaljer om hur du kan hjälpa till.
<p>

<a name="g4"></a>
<font size=+1><b>
4. Jag har hittat en bug! Vad gör jag nu?<br>
</b></font>
<p>

Det viktigaste när du rapporterar en bugg är att vara
så specifik som som möjligt.  Ge oss tillräckligt med information så
vi kan återskapa buggen själva, annars är det inte troligt
att vi kan fixa det. Skicka buggrapporter till
<a href="mailto:audacity-help@lists.sourceforge.net">
<audacity-help@lists.sourceforge.net></a>.
<br><font size=-1><?php print "$listPrivacyStr"; ?></font>
<p>
Kom ihåg att tala om vilket operativ system du kör
(som Windows 98, MacOS 9.1, RedHat Linux 7.1, etc.)
och all annan information om din dator som du
tycker är relevant.
<p>
Sen, kan du återskapa buggen? Om det händer konstant,
berätta den exakta sekvensen av händelser som orsakar buggen.
Om du får ett felmeddelande, se till att skicka med
den exakta texten av felmeddelandet.
<p>

Vi vill bli av med alla buggar!  Tack för att du hjälper oss att finna dem.

<p>
<br>
<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Installation</font>
</td></tr></table>

<p>
<a name="i1"></a>
<font size=+1><b>
1. Windows: Vad gör jag med .exe filen?<br>
</b></font>

<p>Från Internet Explorer, välj att köra filen.
Detta startar Audacitys installationsprogram, som kommer att installera
Audacityprogrammets filer och skapa en genväg från Startmenyn.
<p>
Om du redan har laddat ner filen, dubbelklicka på den för att starta installationen.
<p>
Om du föredrar att inte använda installationsprogrammet, kan du också
ladda ner Audacity i form av en zip-fil, vilken du kan packa upp 
med hjälp av WinZip.

<p>
<a name="i2"></a>
<font size=+1><b>
2. Windows: Hur avinstallerar jag Audacity?<br>
</b></font>

<p><font size=+0>Audacity kan avinstalleras genom att öppna 
Lägga Till/Ta bort Program i
kontrollpanelen.  Välj "Audacity" från listan och
klicka på knappen
"Lägg till/Ta bort".
Detta startar Audacitys avinstallationsprogram.

<p>
<a name="i3"></a> <font size=+1><b> 3. Mac: Vad gör jag med .sit filen?<br> </b></font>

<p>
Du behöver en färsk version av StuffIt Expander, StuffIt Expander
kommer med alla Macintoshar och är oftast konfigurerad 
med alla Mac-webbläsare.  Om Audacity inte packar upp 
automatiskt, dra "audacity.sit" till StuffIt Expander för att packa upp 
filen.
<p>
Det finns inget behov att installera Audacity. Bara dra Audacity
mappen till din Applications mapp, eller vart du vill ha den.
För att avinstallera, bara dra mappen till soptunnan.

<p>
<a name="i4"></a>
<font size=+1><b>
4. Unix: Varför får jag "failed dependencies" när jag installerar via RPM?
<br></b></font>

<p><font size=+0>Först, se till att wxGTK är installerat; en länk finns
på vår Linux sida - http://audacity.sourceforge.net/linux.html. Om Rpm:en
fortfarande inte installeras, är detta en olöst bisak.  Installera bara RPM:en
med flaggan --nodeps.</font>

<p>
<a name="i5"></a>
<font size=+1><b>

5. Unix: Varför får jag ett felmeddelande när den laddar libwx_gtk.so?<br></b></font>

<p><font size=+0>Detta kan vara på grund av två problem:
<br>- Om du installerade wxWindows via RPM, se till att köra "ldconfig" /som root) så att ditt system kan känna igen det nya biblioteket.

<br><br>- Om du installerade wxWidgets
med källkod, kan det ha blivit installerat i /usr/local, men /usr/local/lib/ kanske
inte finns i din biblioteks sökväg. Lägg till /usr/local/lib i filen "/etc/ld.so.conf"
och kör sen "ldconfig". Du kanske cokså vill lägga till "usr/local/bin" till sökvägen så att program kan hitta "wx-config" verktyget.<br>

<p>
<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Använda Audacity</font>
</td></tr></table>

<p>
<a name="a1"></a>
<font size=+1><b>

1. Hur lägger jag ihop två spår?
</b></font>
<p>

Audacity lägger ihop spåren automatiskt.  Allt du behöver göra är att
importera två spår till samma projekt, vilket du kan
göra genom att klicka på "Importera ljudfil" i Projektmenyn, 
eller helt enkelt att dra de två ljudfilerna till 
Audacity. När du trycker på spela-in knappen, skapar
Audacity ett nytt spår automatiskt.

<p>

För att spara din blandning, kan du antingen Exportera projekt,
vilket automatiskt blandar alla dina spår tillsammans, eller
så kan du välja spår och använda "Snabbmix" från Projektmenyn.

<p>
<a name="a2"></a>
<font size=+1><b>
2. Kan jag ta bort sången från en inspelning?
</b></font>
<p>

Med vissa stereoinspelningar, är det möjligt att ta bort sången
på grund av sättet inspelningen var mixad vid studion.
Oftast är sången placerad exakt i mitten av inspelningen,
medan alla andra instrument är utanför. Om du
tar bort höger-kanalen från vänster-kanalen, blir sången 
helt utelämnad, och kvar blir bara instrumenten.
<p>

<b>Detta fungerar bara med vissa inspelingar!</b>
<p>
För att uppnå detta i Audacity, importera en stereoinspelning, klicka sedan på spårets pop-up meny (den lilla nedåt-pilen bredvid spårets namn) och välj "Dela stereospår".
Välj nu det nedersta spåret (den högra kanalen) och använd
"Invertera" effekten (från Effektmenyn).  Slutligen, använd
spårens pop-up menyer för att göra båda kanalerna monokanaler,
och mixa dom tillsammans med hjälp av Snabbmix. Om du har tur
kommer stämmorna att vara borta.

<p>
<a name="a3"></a>
<font size=+1><b>
3. När jag försöker spela in stämmor med mig själv,
varför är de två spåren inte synkroniserade?
</b></font>
<p>
Detta är normalt och det händer därför att det är en liten fördröjning
mellan det att Audacity börjar spela upp ljud och när det
kommer fram till högtalarna. Audacity försöker inte automatiskt
korrigera denna fördröjning, utan du måste göra det manuellt.
<p>
För att korrigera denna fördröjning, använd
Time Shift verktyget för att flytta
en av spåren tills dom är synkroniserade. Kom ihåg att du kan
använda verktyget medan du lyssnar på inspelningen.

<p>

<a name="a4"></a>
<font size=+1><b>
4. Varför är ingen MP3-kodare inkluderad?
</b></font>
<p>

Oturligt nog, så är algoritmen för kodning eller skapande av MP3 filer
patenterad, och <b>Audacity</b> skulle inte kunna
inkludera en MP3 export-algoritm utan att antingen
ta betalt eller inkräkta på lagarna i många länder.
<p>
Som en kompromiss, kommer Audacity med möjligheten att använda
andra MP3-kodare, vilka du måste ladda ner separat.
Det är upp till dig att se till att du är noga med att följa restriktionerna för MP3-kodare.
<p>
För mer information, se sidan om Exportering av MP3 filer
i våran Online Hjälp.

<p>



<a name="a5"></a>
<font size=+1><b>
5. Kan jag spela in RealAudio eller eller annat strömmande ljud?
</b></font>
</p><p>

Inte automatiskt.  Flertaler strömmande ljud, med musik utgiven till
RealOne Player, Windows Media Player, och Quicktime, är uttryckligen
avsett för att du inte ska kunna spela in det.
</p><p>
Det är ändå nästan alltid möjligt att spela in allting som din dator
kan spela genom att koppla din ljudutgång till din ljudingång/mikrofonuttag
på din dators ljudkort.
</p><p>
Använd en 1/8 tum stereo mini till 1/8 tum stereo minikabel.  Koppla en ända
till ljudutgången på din dator eller ljudkort (där du
kopplar in högtalare eller hörlurar - den är oftast färgad grön).  Koppla in den andra änden i mikrofonuttaget eller ljudingången (oftast färgad röd).
</p><p>
Nu kan Audacity spela in allt som datorn spelar upp. Tryck Inspelning
i Audacity, och sen tryck Spela i programmet som spelar upp ditt streamande ljud.
</p><p>

<a name="a6"></a>
<font size=+1><b>
6. Hjälp! Jag har tryckt "spela in" men allt jag får är tystnad!
</b></font>
</p><p>
Audacity spelar in från den förinställda ingångskällan som är vald av
ditt operativsystem. Om du har flera  ingångskällor (som
inbyggd mikrofon, extern mikrofon, line-in uttag, eller en
audio-CD) du måste välja en av dessa att spela in ifrån. Om du
försöker spela in från från mikrofon, som exempel, men din ingångskälla
 är inställd till "line-in", så kommer Audacity bara spela in tystnad.
</p><p>
Om du använder Windows, kan du ställa in ingångskällan genom att högerklicka
på volymikonen i verktygslisten (nedre högra hörnet av skärmen).
</p><p>
Om du använder Mac OS 9, använd Sound control panel.  I MAc OS X, använd
Sound panel i System Preferences (det finns en input-flik om du 10.2 eller högre).
</p><p>
I Linux, använd en mixer som "xmixer" eller "kmix".
</p><p>

<a name="a7"></a>
<font size=+1><b>
7. Hur delar jag upp en fil i flera spår?
</b></font>
</p><p>
Ibland kan du få en inspelning av flera sånger som är
i en enda fil.  Om du vill bränna dessa låtar på en CD som separata
spår, måste du skapa en separat fil för varje sång.
</p><p>Det finns många olika sätt att dela upp inspelningar till flera spår med hjälp av
Audacity.  Här finns några exempel:
</p><p>
Method 1: Exportera Markering
</p><ul>
<li>Välj den region av ljud som korresponderar det första spåret.
</li><li>Välj "Exportera som WAV" från Arkiv-menyn, och spara det första spåret på hårddisken.
</li><li>Fortsätt så med alla resterande spår.
</li></ul>
<p>
Method 2: Radera och Ångra
</p><ul>
<li>Radera allting <i>förutom</i> för det första spåret.
</li><li>Välj "Exportera som WAV" från Arkiv-menyn, och spara det första spåret på hårddisken.
</li><li>Välj "Ångra" från Redigera-menyn.  Fortsätt att ångra ända tills du har resten av ljudet tillbaka. (Audacity har obegränsad ångrafunktion.)
</li><li>Fortsätt så med alla resterande spår.
</li></ul>
<p>
Method 3: Dela och Exportera
</p><ul>
<li>Välj det du vill ha som första spår.
</li><li>Välj "Dela" från Redigera-menyn, vilket flyttar markeringen till ett separat spår i Audacity.
</li><li>Fortsätt med detta tills alla låtar är i separata spår.
    Glöm inte att du ångra om du råkat göra fel.
</li><li>Gå tillbaka och använd "Exportera markering som WAV" för att exportera varje
    sspår till en separat fil.  För att snabbt välja varje spår, klicka på dess
    eticket (vart som helst i området till vänster om vågformen, under
    titeln).
</li></ul>
<p>

<a name="a8"></a>
<font size=+1><b>
8. Hur kan jag få låtar från en ljud-CD?
</b></font>
</p><p>
Audacity kan inte ta sånger från ljud-CD. Du måste använda ett
"ripper" program för detta.
</p><p>
För Windows, rekommenderas <a href="http://cdexos.sourceforge.net/">CDex</a>.
</p><p>
För Mac OS (9 and X), rekommenderas Apple's <a href="http://www.apple.com/itunes/">iTunes</a>.
</p><p>
För Linux, pröva <a href="http://www.xiph.org/paranoia/">Paranoia</a>.

<?php BoxBottom(); ?>
