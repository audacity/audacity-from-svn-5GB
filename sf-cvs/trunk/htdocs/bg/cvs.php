<?php BoxTop("CVS"); ?>

За съвместното разработване на Audacity се използва системата
<a target=_top href="http://www.cvshome.org">CVS</a>, или Concurrent Versions
System [система за управление на успоредни версии]. Щракнете
<a target=_top href="http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/audacity/audacity-src/">тук</a>, за да
видите изходния код в хранилището на CVS.

<p>
<h3>Забележка: версия 1.2 е разклонена:</h3>
Ако искате стабилния клон на Audacity, версия 1.2.x, трябва да въведете
<tt>-r AUDACITY_1_2</tt> при обновяване или извличане на файлове от CVS. Иначе ще получите основния клон (HEAD), който може да стане доста нестабилен, когато започнем работа по версия 1.3.0.

<p><h3>Бърз курс за работа с Audacity чрез CVS:</h3>
За достъп до изходния код на Audacity използвайте клиент за CVS, за да изтеглите
разклонение (branch) от CVS на вашия компютър. След като сте извлекли
разклонението, клиентският софтуер за CVS ще ви помага да държите версията си
синхронизирана с останалите разработчици на Audacity. За да получите достъп до
изходния код, следвайте долните указания.

<h4>Анонимен достъп до CVS чрез клиент, управляван от командния ред:</h4>
<p>Въведете следното на командния ред (всичко на един ред!):<br>
<ul><tt>cvs -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity
login </tt><br></ul>
и натиснете Enter, когато бъдете запитани за парола.

<p>След това, <b>за да изтеглите най-новото разклонение (1.3.0)</b> (пак на един ред):
<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co
audacity</tt></ul>
или <b>за стабилното разклонение (1.2.0)</b> (на един ред):<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co -r AUDACITY_1_2 audacity</tt></ul>
или <b>за стария клон 1.0 (1.0.0)</b> (на един ред):<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co -r audacity-0_9-branch audacity-old</tt></ul>


<p> Можете също да присвоите на променлива <tt>CVSROOT</tt> от обкръжението стойност
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
(вижте по-долу).<br>
След това, <b>за да изтеглите последното нестабилно разклонение (1.3.0)</b>, въведете<br> 
   <ul><tt>cvs checkout audacity</tt></ul>
или, <b>за стабилното разклонение (1.2)</b>, въведете <br>
   <ul><tt>cvs checkout -r AUDACITY_1_2 audacity</tt></ul>
или, <b>за старото разклонение 1.0 </b>, въведете <br>
   <ul><tt>cvs checkout -r audacity-0_9-branch audacity-old</tt></ul>   <ul><tt>cvs checkout -r audacity-0_9-branch audacity-old</tt></ul>

<p>За да зададете стойност на променливата <tt>CVSROOT</tt> от обкръжението, може да
използвате ресурсния файл на командния интерпретатор или една от следните команди:
<h5>В bash (bourne shell), на един ред:</h5>
<ul><tt>export
CVSROOT=:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>
<h5>В csh и производните му, на един ред:</h5>
<ul><tt>setenv CVSROOT
:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>

<hr width="80%">

<h4>Анонимен достъп до CVS чрез графичен клиент</h4>
За графичен клиент като wincvs, maccvs или gcvs (достъпни от
<a target=_top href="http://cvsgui.org">cvsgui.org</a>), трябва да присвоите на променливата
<tt>CVSROOT</tt> (в подменюто Admin|Preferences) стойност
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
и за режим на достъп (authentication) да изберете "pserver" или "Password".
После се уверете, че в страницата Globals на диалоговия прозорец Preferences
полето "Checkout read-only (CVSREAD)" НЕ е отметнато. След това изберете login и
натиснете Enter на запитването за парола (тя е ""). Накрая, изберете "Create|Checkout module",
изберете местоназначение за изтегляне на разклонението и следвайте указанията по-долу
в зависимост от желаното разклонение. Ако получите съобщение за грешка, проверете
дали променливата <tt>CVSROOT</tt> не завършва с интервали - което може да се случи, ако сте копирали стойността направо от настоящата страница.

<p>
<b>За да получите последното нестабилно разклонение (1.3.0):</b><br>
В диалоговия прозорец "Checkout Settings" въведете като име на модул
<tt>audacity</tt>. Натиснете "OK" и разклонението ще бъде свалено автоматично
на вашия компютър.
<p>
<b>За да получите стабилното разклонение (1.2.0): </b><br>
В диалоговия прозорец "Checkout Settings" въведете като име на модул
<tt>audacity-old</tt>. После, на страницата "Sticky options", отметнете
полето "Retrieve rev./tag/branch (-r)" и въведете <tt>AUDACITY_1_2</tt> в
текстовото поле до него. Натиснете "OK" и разклонението ще бъде свалено автоматично
на вашия компютър.

<p>
<b>За да получите старото разклонение (1.0):</b><br>
В диалоговия прозорец "Checkout Settings" въведете за име на модул <tt>audacity-old</tt>. После, на страницата "Sticky options", отметнете полето "Retrieve rev./tag/branch (-r)" и въведете <tt>audacity-0_9-branch</tt> в текстовото поле до него. Натиснете "OK" и разклонението ще бъде свалено автоматично
на вашия компютър.

<hr width="80%">
Не сте работили с CVS досега? Започнете, прочитайки <a href="">Introduction
to
CVS</a> [Въведение в CVS] на Джим Бленди (Jim Blandy), <a target=_top href="http://www.cvshome.org/new_users.html">CVS for new
users</a> [CVS за нови потребители] на Боб Арнсън (Bob Arnson), или посетете страницата на CVS на адрес <a target=_top href="http://www.cvshome.org/">www.cvshome.org</a>.
По-подробна информация е достъпна в главите, разпространявани по лиценз GPL, от книгата за CVS на Карл Фогел (Karl
Fogel) на <a target=_top href="http://cvsbook.red-bean.com/cvsbook.html">cvsbook.red-bean.com</a>, или "официалното" <a target=_top href="http://www.cvshome.org/docs/manual">ръководство</a> на Пер Седерквист (Per Cederqvist).


<p>За помощ конкретно по работата със sourceforge.net чрез CVS вижте документацията на Source Forge за
платформите <a target=_top href="http://sourceforge.net/docman/display_doc.php?docid=763&group_id=1">Unix</a>,
<a target=_top href="http://sourceforge.net/docman/display_doc.php?docid=766&group_id=1">Microsoft Windows</a> и <a target=_top href="http://sourceforge.net/docman/display_doc.php?docid=2973&group_id=1">MacOS
(преди OS X)</a>.

<hr width="80%">

<p>
<b>Още подробности:</b>

</p>

<p>В Audacity са използвани много библиотеки, писани от трети лица.
Много от тях се нуждаят от леки промени, за да се компилират на всички целеви
платформи. Затова съхраняваме изходния код на всички такива библиотеки в CVS.
Ето какво трябва да знаете:
</p>
<p>Има две хранилища: 'audacity-src', което съдържа целия написан от нас код, и
'lib-src', което съдържа изходния код на използваните чужди библиотеки.
За да се гарантира съвместимост между Audacity и библиотеките, препоръчваме ви да
използвате версиите им от 'lib-src'. На Unix можете да избегнете компилирането
на някои от библиотеките, използвайки вече инсталирани на системата библиотеки.
Въведете 'configure --help', за да видите достъпните възможности.
</p>
<p>
И така, ако искате да свалите всичко, включително изходния код на библиотеките,
извлечете модула 'audacity', което ще свали audacity-src заедно със съдържанието на
lib-src като поддиректория на 'audacity'.
</td>
</tr>
</table>
</p>

<?php BoxBottom(); ?>
