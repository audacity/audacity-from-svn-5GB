<?php BoxTop("CVS"); ?>

Користиме <a href="http://www.cvshome.org">CVS</a>, за Concurrent Versions
System, како помош за
соработка при Audacity развојот. Кликнете
 <a
href="http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/audacity/audacity-src/">тука</a> за барање на изворниот код од нашиот CVS насочувач.

<p><h3>Кратко Audacity CVS Како да:</h3>
  Ако сакате да пристапите до Audacity изворниот код, можете да
  користите cvs клиент за симнување на cvs податоците на вашиот
  компјутер. Еднаш кога ќе ги симнете податоците, вашиот CVS
  клиент ќе ви помага при одржување на свежината
  на вашиот систем со другите Audacity создавачи. Следете ги
  инструкциите подолу за доаѓање до изворниот код.


<h4>Анонимен CVS пристап со команднолиниски cvs клиент:</h4>
<p>Внесете во командната линија (внимавајте тоа е
една линија без нови редови):<br>
<ul><tt>cvs -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity
login </tt><br></ul>
и притиснете enter кога ќе праша за шифра.

<p>Потоа, <b>за земање на последниот извор (1.1.0)</b> (една линија):
<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co
audacity</tt></ul>
или <b>за стабилниот извор (0.9-1.0)</b> (една линија):<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co -r audacity-0_9-branch audacity-old</tt></ul>

<p> Алтернативно, можете да ја поставите вашата <tt>CVSROOT</tt> околина
на
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
(види подолу).<br>
Потоа, <b>за последниот нестабилен извор (1.1.0)</b>, внесете<br>
   <ul><tt>cvs checkout audacity</tt></ul>
или, <b>за стабилниот извор (0.9-1.0)</b>, внесете <br>
   <ul><tt>cvs checkout -r audacity-0_9-branch audacity-old</tt></ul>

<p> За поставка на  <tt>CVSROOT</tt> околината
можете да користите командна скрипта или една од следните
команди:
<h5>Во bash или bourne школката, како една линија:</h5>
<ul><tt>export
CVSROOT=:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>
<h5>Во csh или нејзе сличните, како една линија:</h5>
<ul><tt>setenv CVSROOT
:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>

<hr width="80%">

<h4>Анонимен CVS пристап со графички клиент</h4>

За графички клиент како wincvs, maccvs или gcvs,
(достапни на <a href="http://cvsgui.org">cvsgui.org</a>) морате да поставите
во
<tt>CVSROOT</tt> околината (во Admin|Preferences подменито) да биде
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
и одберете "pserver" или "Password" автентификација. Потоа,
под Globals во Preferences дијалогот, осигурајте се
дека сте го обезначиле "Checkout read-only (CVSREAD)".  Следно,
одберете login и притиснете ентер за password (таа е
""). Конечно, одберете "Create|Checkout module", одберете
локација каде што сакате да го ставите бараниот извор
и следете ги упатствата кои доаѓаат со изворот кој го барате.
Ако добиете грешка осигурајте се дека <tt>CVSROOT</tt>
околината не содржи ниедно празно место на крај
- што може да ви се случи ако податоците сте ги копирале директно од веб страна.

<p>
<b>За последен нестабилен извор (1.1.0):</b><br>
Под "Checkout Settings" внесете
<tt>audacity</tt> како module name. Кликнете "OK" и
автоматски ќе биде симнат на вашиот компјутер.
<p>
<b>За стабилниот (0.9-1.0) извор: </b><br>
Под "Checkout Settings" внесете
<tt>audacity-old</tt> како module name.  Потоа, под
"Sticky options" одберете "Retrieve rev./tag/branch
(-r)" и внесете <tt>audacity-0_9-branch</tt> во
полето под него. Кликнете "OK" и изворот автоматски
ќе биде симнат на вашиот компјутер.

<hr width="80%">
Нови во CVS?  Почнете со читање на Jim Blandy-евата <a href="">Вовед
во
CVS</a>, Bob Arnson-овата <a
href="http://www.cvshome.org/new_users.html">CVS за нови
корисници</a>, или посетете ја cvs веб страната на <a
href="http://www.cvshome.org/">www.cvshome.org</a>.
Подетални информации се достапни во GPL текстовите од Karl
Fogel-овата
<a href="http://cvsbook.red-bean.com/cvsbook.html">CVS книга
на cvsbook.red-bean.com</a>, или  <a
href="http://www.cvshome.org/docs/manual">Официјално
 Cederqvist упатство</a>.


<p> За специфична помош околу CVS на sourceforge.net побарајте
sourceforge документација за
<a
href="http://sourceforge.net/docman/display_doc.php?docid=763&group_id=1">Unix</a>,
<a
href="http://sourceforge.net/docman/display_doc.php?docid=766&group_id=1">Microsoft Windows</a> и <a
href="http://sourceforge.net/docman/display_doc.php?docid=2973&group_id=1">MacOS
(дел од OS X)</a> платформите.

<hr width="80%">

<p>
<b>Подетално:</b>

</p>

<p>Audacity користи многу third party библиотеки. Многу од нив бараат местење
на сите наши целни платформи. Затоа ги чуваме локалните извори од
сите third party библиотеки во нашиот CVS. Еве како работи тоа:
</p>
<p>Има два извора: 'audacity-src', кој го содржи
целиот код кој го имаме напишано и
'lib-src,' кој го содржи изворниот кд на сите
библиотеки кои ги користиме.
За да гарантираме кооперативност меѓу Audacity и нашите
библиотеки, ви препорачуваме да ги користите верзиите на библиотеките
содржани во 'lib-src'.  Секако, на Unix системите можете да одбегнете преведување
на некоја од библиотеките користејќи библиотека која веќе ја имате во системот.
Внесете 'configure --help' за да ги видите опциите.
</p>
<p>
Значи ако сакате да проверите се, вклучувајќи ги и библиотеките,
обележете го module 'audacity' кое ќе го преземе audacity-src но исто така
земете го и lib-src како подименик на 'audacity'.
</td>
</tr>
</table>
</p>

<?php BoxBottom(); ?>
