<?php BoxTop("Nyquist"); ?>

<p>
<b>Deel 1: Introductie over Nyquist en Lisp programmering.</b><br>
<?php print "<a href=nyquist2.php?$langLinkStr>";
?>Deel 2: Programmeren met Nyquist.</a><br>
<?php print "<a href=nyquist3.php?$langLinkStr>";
?>Deel 3: Nyquist plug-ins maken.</a><br>
</p>

<p>
Te beginnen met versie 1.1.1, in Audacity kun je nu de Nyquist programmeer
taal gebruiken om je eigen plug-in effecen te maken voor Audacity.
In tegenstelling tot VST en LADSPA plug-ins, kunnen Nyquist plug-ins
geschreven worden met behulp van een normale text editor en behoeven ze
niet samengesteld te worden.
</p>

<p>
Nyquist is geschreven door
<a href="http://www-2.cs.cmu.edu/~rbd/">Roger B. Dannenberg</a>
en is bedoeld om te worden gebruikt als een complete programmeer taal
voor audio synthesis en analysis, met ondersteuning voor MIDI, audio
opname en playback, bestands I/O, object-georiënteerde programmering,
profiling, debugging en meer.  Audacity gebruikt maar een gedeelte van
Nyquist's functionaliteit, waardoor je simpele Nyquist functies kunt maken
en deze kunt gebruiken om audio data te bewerken. Audacity bevat geen ondersteuning voor debugging Nyquist code, dus als je probeert een gecompliceerde plug-in te maken, is het misschien makkelijker om de
volledige versie van Nyquist te halen, daarin te ontwikkelen en dan om
te zetten in een Audacity plug-in.  Nyquist is beschikbaar bij de Carnegie Mellon University Computer Music Project:
<ul>
<a href="http://www-2.cs.cmu.edu/~music/music.software.html"
>CMU Computer Music Project Software</a> - 
download the volledige Nyquist hier</a>
<br>
<a href="http://www-2.cs.cmu.edu/~rbd/doc/nyquist/root.html"
>Complete Nyquist handleiding bij CMU</a>
</ul>
</p>

<p>
Je hoeft dus niet de volledige Nyquist versie te downloaden om simpele
plug-ins voor gebruik in Audacity te maken.  Alle instructies die je nodig
bent, bevinden zich hieronder.
</p>

<p><b>Lisp</b></p>

<p>
Nyquist is gebaseerd op <i>Lisp</i>.  Als je al eens in Lisp hebt
geprogrammeerd, kun je deze sectie overslaan of direct gaan naar de
<?php print "<a href=nyquist2.php?$langLinkStr>";
?><b>volgende pagina</b></a>.
In andere gevallen is hier een korte introductie betreffende Lisp:
</p>

<p>
In Lisp (en dus ook in Nyquist), is alles in S-Expressie,
wat gewoon een lijst tekens (woorden) is, gescheiden door spaties en
samengevoegd in parentheses.  De naam van de functie is altijd
het eerste teken in een S-Expressie en alle andere tekens
zijn argumenten voor die functie.  Hier een simpel voorbeeld:
</p>

<pre>
  (setf area (* 3.14159 (expt radius 2)))
</pre>

<p>
Laten we dit voorbeeld eens nader bekijken.  De buitenste S-expressie heeft
drie leden.  De eerste, <tt>setf</tt>, is de naam van
de functie (betekent hier set-field).  <tt>setf</tt> wordt gebruikt om een waarde aan een variabele toe te kennen.  (Er zijn andere gelijke functies,
zoals <tt>set</tt> en <tt>setq</tt>, maar <tt>setf</tt> is de
meest krachtige, dus gebruiken we deze in ons voorbeeld.)
Na <tt>setf</tt> komt <tt>area</tt>, dat de naam is van de variabele
die we gaan plaatsen.  De volgende stap is de waarde toekennen aan deze variabele, dat hier een andere S-expressie is.
</p>

<p>
Lisp bevat geen speciale operators voor Math functiess - het zijn allemaal
functies zoals alle andere, gebruik makend van <i>prefix</i> notatie,
waar de naam van de functie (of operator) voor zijn argument komt.
Dus in plaats van 3*7 voor het produkt van 3 en 7, zou je in Lisp
moeten schrijven (* 3 7).  In Nyquist, komt de <tt>expt</tt> (exponent)
functie van het eerste argument boven die van het tweede argument.
Daardoor <tt>(* 3.14159 (expt radius 2))</tt> betekent 3.14159 keer
het kwadraat van de <tt>radius</tt>, of de formule van een cirkel.
</p>

<p>
In plaats van het constant iedere keer typen van deze volledige uitleg,
gaan we een functie voor de ruimte van de cirkel definieren, die we elke
keer als we deze nodig zijn, kunnen oproepen:
</p>

<pre>
  (defun circlearea (radius) (* 3.14159 (expt radius 2)))
</pre>

<p>
De <tt>defun</tt> functie wordt gebruikt om een nieuwe functie te definieren.  Het eerste argument is de naam van de functie, in dit geval
<tt>circlearea</tt>.
Het tweede argument is een lijst van argumenten van de functie om gedefinieerd te worden - dit is een van de weinige gevallen waarin je een S-expressien hebt
welke niet als een functie oproep wordt geïnterpreteerd.  De laatste expressie is de waarde van de functie.  Dus als we de ruimte van de radius van een cirkel willen berekenen <tt>r</tt>, hoeven we alleen te berekenen:
</p>

<pre>
  (setf area (circlearea r))
</pre>

<p>
Een S-expressie is alleen maar een representatie van een lijst.  Lisp
gebruikt lijsten om bijna alles te representeren (de naam LISP komt van
LISt Processing language), dus is het handig om te weten hoe je lijsten kunt
manipuleren.  Laten we beginnen met het toekennen van een lijst met nummers aan een variabele.  Het volgende kun je niet doen:
</p>

<pre>
  (setf mylist (1 2 3 4 5))  <font color=#cc0000><--  error!</font>
</pre>

<p>
De reden waarom dit niet werkt is dat iedere keer dat Nyquist een
S-expressie ziet, het deze als een functie wil evalueren tenzij je
meegeeft dat dit niet moet.  Omdat er geen functie "1" is genaamd die de argumenten <tt>(2 3 4 5)</tt> benoemd, zal dit een fout genereren.
Om lisp mee te geven dat deze de S-expressie letterlijk moet nemen en
en deze niet moet evalueren als een functie, moet je deze <i>aanhalen</i>.
In Nyquist kun je een lijst aanhalen door er een enkel aanhalingsteken voor
te zetten, zoals hier:
</p>

<pre>
  (setf mylist '(1 2 3 4 5))
</pre>

<p>
Nyquist bevat ook een <tt>list</tt> functie die je kunt gebruiken om een
lijst te maken - dit is handig indien sommige elementen van de lijst
functies blijken te zijn:
</p>

<pre>
  (setf mylist (list 1 2 3 4 (sqrt 25)))
</pre>

<p>
Om zaken van een lijst af te krijgen, kune je de <tt>first</tt> en
<tt>rest</tt> functies gebruiken.  (Traditioneel gezien worden die
<tt>car</tt> en <tt>cdr</tt> geoemd, respectievelijk, maar <tt>first</tt> en
<tt>rest</tt> zijn makkelijker te onthouden.  Beide benamingen worden in  Nyquist ondersteund.)  De output van <tt>(first mylist)</tt> is 1,
en de output van <tt>(rest mylist)</tt> is de lijst <tt>(2 3 4 5)</tt>.
Dus het tweede element van de lijst is <tt>(first (rest mylist))</tt>.
</p>

<p>
<b>Lisp functie verwijzing</b>
</p>

<p>
Hier een lijst van enkele van de basis lisp functies die je nodig zou
kunnen hebben.
</p>

<table border=0>
<tr><td align=center><i>Math functions</i></td><td></td></tr>
<tr><td><tt>(+ a b)</tt></td><td>addition</td></tr>
<tr><td><tt>(- a b)</tt></td><td>subtraction</td></tr>
<tr><td><tt>(* a b)</tt></td><td>multiplication</td></tr>
<tr><td><tt>(/ a b)</tt></td><td>division</td></tr>
<tr><td><tt>(truncate a b)</tt></td><td>round down to integer (floor)</td></tr>
<tr><td><tt>(float a b)</tt></td><td>integer to floating-point</td></tr>
<tr><td><tt>(rem a b c ...)</tt></td><td>remainder</td></tr>
<tr><td><tt>(min a b c ...)</tt></td><td>minimum</td></tr>
<tr><td><tt>(max a b c ...)</tt></td><td>maximum </td></tr>
<tr><td><tt>(abs a)</tt></td><td>absolute value</td></tr>
<tr><td><tt>(random n)</tt></td><td>random integer between 1 and n-1</td></tr>
<tr><td><tt>(sin a b)</tt></td><td>sine</td></tr>
<tr><td><tt>(cos a b)</tt></td><td>cosine</td></tr>
<tr><td><tt>(tan a b)</tt></td><td>tangent</td></tr>
<tr><td><tt>(expt a b)</tt></td><td>exponent (a to the power of b)</td></tr>
<tr><td><tt>(sqrt a b)</tt></td><td>square root</td></tr>
<tr><td><tt>(< a b)</tt></td><td>test for a less than b</td></tr>
<tr><td><tt>(<= a b)</tt></td><td>test for a less than or equal to b</td></tr>
<tr><td><tt>(> a b)</tt></td><td>test for a greater than b</td></tr>
<tr><td><tt>(>= a b)</tt></td><td>test for a greater than or equal to b</td></tr>
<tr><td><tt>(= a b)</tt></td><td>test for equality</td></tr>
<tr><td><tt>(/= a b)</tt></td><td>test for inequality</td></tr>
<tr><td align=center><i>List functions</i></td><td></td></tr>
<tr><td><tt>(first l)</tt></td><td>first element of a list (car)</td></tr>
<tr><td><tt>(rest l)</tt></td><td>rest of the list (cdr)</td></tr>
<tr><td><tt>(reverse l)</tt></td><td>reverse a list</td></tr>
<tr><td><tt>(list a b ...)</tt></td><td>construct a list</td></tr>
<tr><td><tt>(append l1 l2)</tt></td><td>append two lists</td></tr>
<tr><td><tt>(length l)</tt></td><td>length of a list</td></tr>
<tr><td><tt>(maplist function l)</tt></td><td>apply a function to every element in a list</td></tr>
<tr><td align=center><i>Control</i></td><td></td></tr>
<tr><td><tt>(if expr expr1 expr2)</tt></td><td>if expr is true, evaluates expr1, otherwise evaluates expr2</td></tr> 
</table>

<p>
Voor een complete lijst van Lisp / Nyquist functies, bekijk de
<a href="http://www-2.cs.cmu.edu/~rbd/doc/nyquist/root.html">Nyquist Verwijzingen Handleiding</a>.
</p>

<p>
<b>Opmerking: Symbolen in Nyquist(zoals namen van variabelen of functie namen) zijn niet hoofdletter gevoelig.  Ze worden intern tot hoofdletter geconverteerd.</b>
</p>

<p>
<?php print "<a href=nyquist2.php?$langLinkStr>";
?><b>Volgende pagina: Programmeren in Nyquist</b></a>
</p>

<?php BoxBottom(); ?>
