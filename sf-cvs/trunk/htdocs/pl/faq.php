<?php BoxTop("Frequently Asked Questions"); ?>

<!--
<table width=100% cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+3">Najczęściej zadawane pytania (F.A.Q.)</font>
</td></tr></table>
-->

<p>
Ogólne:<br>
<a href="#g1">Czy rzeczywiście Audacity jest darmowy? Dlaczego?</a><br>
<a href="#g2">Kto rozwija Audacity?</a><br>
<a href="#g3">Jak mogę pomóc w rozwoju Audacity?</a><br>
<a href="#g4">Znalazłem(-am) błąd! No i co teraz?</a><br>

<p>


Instalacja:<br>

<a href="#i1">Windows: Co zrobić z plikiem .exe?</a><br>
<a href="#i2">Windows: Jak odinstalować Audacity?</a><br>
<a href="#i3">Mac: Co zrobić z plikiem .sit?</a><br>
<a href="#i4">Unix: Dlaczego mam niespełnione zależności jeżeli instaluję z pliku
              RPM?</a><br>
<a href="#i5">Unix: Dlaczego pojawia się błąd w czasie wczytywania współdzielonej biblioteki
              libwx_gtk.so?</a><br>

<p>

<p>

Używanie Audacity:<br>

<a href="#a1">Jak mogę zmiksować (zmieszać) razem dwie ścieżki?</a><br>
<a href="#a2">Czy można usunąć wokal z nagrania?</a><br>

<a href="#a3">Podczas próby "dogrywania się" do odtwarzanego dźwięku, dlaczego obie ścieżki są rozsynchronizowane?</a><br>
<a href="#a4">Dlaczego Audacity nie jest rozpowrzechniany razem enkoderem MP3?</a><br>
<a href="#a5">Czy można nagrywać RealAudio lub inne strumienie audio?</a><br>
<a href="#a6">Pomocy! Naciskam Nagrywaj, a otrzymuję tylko ciszę!</a><br>
<a href="#a7">Jak mogę podzielić plik na niezależne ścieżki</a><br>
<a href="#a8">Jak można "wyciągnąć" utwór z płyty audio CD?</a><br>

<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Ogólne</font>
</td></tr></table>

<p>

<a name="g1"></a>
<font size=+1><b>
1. Czy rzeczywiście Audacity jest darmowy? Dlaczego?<br>

</b></font>

<p>

Tak, prawie pod każdym względem. Audacity nie tylko jest rozpowrzechniany
bez żadnych opłat, ale także możesz z nim za darmo zrobić praktycznie wszystko co chcesz.
<p>
Audacity jest rozpowrzechniany na licencji:
<a href="http://www.gnu.org/copyleft/gpl.html"
>General Public License (GPL)</a>.
Jest darmowy zarówno wprzypadku korzystania z niego prywatnie, jak również w celach komercyjnych.
You are also free to give it away or sell it.
Kody źródłowe programu są udostępnione za darmo w internecie,
możesz bez przeszkód modyfikować je na własny użytek, jednak
każdą zmianę musisz także rozpowrzechniać na zasadach GPL.
<p>
Audacity wykorzystuje bibliotekę wxWidgets, która jest rozpowrzechniana 
na mniej restrykcyjnych zasadach LGPL.
Aby uzyskać więcej informacji odwiedź stronę
<a href="http://wxwidgets.org/">wxwidgets.org</a>.
<p>
Autorzy programu Audacity postanowili go rozpowrzechniać na licencji GPL
z wielu powodów. Niektórzy z nas robią go ze zwykłej wspaniałomyślności.
Niektórzy z nas z pobudek moralnych, ponieważ uważamy że oporgramowanie powinno
być darmowe, podczas gdy inni z nas wierzą że jest potrzebne dla obu:
darmowych i komercyjnych programów na świecie.
<p>
Jedną z przyczyn tego że Audacity jest darmowe jest to że dzięki temu
ma szansę być bardziej popularny. Dużo z nas wolałoby raczej widzieć 
milion zadowolonych ludzi korzystających z Audacity niż mieć tysiąc
ludzi którzy nam płacą. Dużo osób jest milszych jeżeli mogą coś wziąc za darmo.
<p>
JEszcze jeden powód jest popieranie współpracy. Jeżeli Audacity byłoby 
shareware, staje się niemożliwe żeby ktokolwiek na świecie chciałby
współtworzyć program, usuwać błedy, tworzyć dokumentację i grafikę.
<p>
<a name="g2"></a>
<font size=+1><b>
2. Kto napisał Audacity?<br>
</b></font>

<p>

Audacity powstało jesienią 1999 roku z rąk Dominic'a Mazzoni w czasie stawania się 
absolwentem Carnegie Mellon University w Pitsburgu (USA).
Pracował wtedy nad projektem razem z profesorem Roger'em Dannenberg'iem
i obaj potrzebowali narzędzia które umożliwiło by im wizualizację
wyników algorytmó do analizy sygnałów audio. 
Z czasem program zaczął stawać się edytorem audio i inni ludze zaczynali
mu pomagać.

<p>
Obecnie Audacity jest rozwijane w Sourceforge, stronę która umożliwia
współprawcować ludziom z całego świata nad darmowymi projektami informatycznymi.
 Zobacz <a href="http://www.sourceforge.net"
>sourceforge.net</a> aby uzyskać więcej informacji.
Tuziny ludzi współpracuje ze sobą tworząc Audacity, a ich liczba ciągle rośnie.

<p>
<a name="g3"></a>
<font size=+1><b>
3. Jak mogę pomóc w rozwoju Audacity??<br>
</b></font>

Znajdź błędy i powiedz nam o nich.
Pisz kod. Tłumacz program na swój język. Twórz grafikę.
Dołącz do listy mailingowej użytkowników Audacity (Audacity-Users).
Prześlij nam pieniądze.

<p>
Proszę, odwiedź naszą nową stronę
<a href=donatetime.php?lang=en>Dotacja</a>
aby uzyskać więcej informacji na temat jak nam możesz pomóc.
<p>

<a name="g4"></a>
<font size=+1><b>
4. Znalazłem(-am) błąd! No i co teraz?<br>
</b></font>
<p>

Najważniejszą rzeczą w czasie zgłąszania błedu jest to żeby
błąd był opisany dak dokładnie jak tylko jest to możliwe.
Przekaż nam informację wystarczającą do odtworzenia tego błędu
u nas, w przeciwnym wypadku może nie być możliwe usunięcie go.
Błędy zgłąszaj do
<a href="mailto:audacity-help@lists.sourceforge.net">
<audacity-help@lists.sourceforge.net></a>.
<p>
Przekaż nam informację na temat tego jakiego systemu opreacyjnego używasz
(jak na przykład Windows 98, MacOS 9.1, RedHat Linux 7.1, itd.)
oraz informację na temat Twojej konfiguracji sprzętowej
jeżeli uważasz że może to być istotne.
<p>
Następnie, czy potrafisz powtórzyć ten błąd? Jeżeli zdarza się tylko w konkretnym przypadku,
opisz nam dokładnie jakie kroki wykonałeś żeby ten błąd wystąpił.
Jeżeli pojawił się komuniakt o błędzie, wyślij nam pełny
tekst tego błędu.
<p>

Chcemy wykosić wszystkie błędy! Dzięki za poświęcenie nam odrobiny czasu
aby pomóc nam w wyśledzeniu go i usunięciu.

<p>
<br>
<p>

<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Instalacja</font>
</td></tr></table>

<p>
<a name="i1"></a>
<font size=+1><b>
1. Windows: Co zrobić z plikiem .exe?<br>
</b></font>

<p>
Z przeglądarki, wybierz opcję "Uruchom z bierzącej lokalizacji". Spowoduje
to uruchomienie programu instalacyjnego, który zainstaluje program Audacity
i utworzy pozycję w menu start.
<p>
Jeżeli zapisałeś ten plik, kliknij na nim dwa razy myszką uruchamiając 
w ten sposób program instalacyjny.
<p>
Jeżeli nie preferujesz takiego sposobu instalacji programu,
możesz także ściągnąć Audacity jako archiwum ZIP i rozpakować
go przy użyciu np WinZip'a.
<p>

<a name="i2"></a>
<font size=+1><b>
2. Windows: Jak odinstalować Audacity?<br>
</b></font>

<p><font size=+0>
Audacity może zostać odinstalowane poprzez otwarcie
"Dodaj/Usuń programy" w panelu sterownia. Wybierz "Audacity"
z listy i naciśnij przycisk "Dodaj/Usuń".
Spowoduje to uruchomienie programu deinstalacyjnego.

<p>
<a name="i3"></a> <font size=+1><b> 3. Mac: What do I do with
the .sit file?<br> </b></font>

<p>
You need a recent version of StuffIt Expander.  StuffIt Expander
comes with all Macintoshes and is usually configured by default
with all Mac web browsers.  If Audacity does not decompress
automatically, drag "audacity.sit" to StuffIt Expander to decompress
it.
<p>
There is no need to install Audacity.  Just drag the Audacity
folder to your Applications folder, or wherever else you would
like to put it.  To uninstall, just drag the entire folder to
the trash.

<p>
<a name="i4"></a>
<font size=+1><b>
4. Unix: Dlaczego mam niespełnione zależności jeżeli instaluję z pliku RPM?
<br></b></font>

<p><font size=+0>Po pierwsze sprawdź czy pakiet wxGTK jest zainstalowany. Link do niej możesz znaleźć na naszej stronie Linuksowej - http://audacity.sourceforge.net/linux.html. 
Jeżeli w dalszym ciągu pojawiają się błędy podczas instalacji, to mógł wystąpić nierozpoznany błąd. Spróbuj zainstalować RPM'a z opcją -nodeps.</font>

<p>
<a name="i5"></a>
<font size=+1><b>

5. Unix: Dlaczego pojawia się błąd w czasie wczytywania współdzielonej biblioteki libwx_gtk.so?<br></b></font>

<p><font size=+0>o może być jeden z dwóch powodów:
<br>- jeżeli instalowałeś wxWindows z RPM'a, miej pewność że uruchomiono
"ldconfig" (jako root) , dzięki czemu Twój system wie o nowej bibliotece.
<br><br>- jeżeli instalowałeś bibliotekę wxWidgets ze źródeł, mogła ona zostać 
zainstalowana w katalogu /usr/local, ale katalog /usr/local/lib może nie być
wpisany w pliku "/etc/ld.so.conf". Dopisz ścieżkę "/usr/local/lib" do pliku
"/etc/ld.so.conf", a następnie uruchom "ldconfig". Możesz także dodać
"/usr/local/bin" do Twojej ścieżki, tak żeby progamy mogły tam znaleźć narzędzie
"wx-config".<br>

<p>
<table cellpadding=6 width=100%><tr><td bgcolor="#ffd0d0" align="center">
<font size="+1">Używanie Audacity</font>
</td></tr></table>

<p>
<a name="a1"></a>
<font size=+1><b>

1. Jak mogę zmiksować (zmieszać) razem dwie ścieżki?
</b></font>
<p>

Audacity miksuje automatycznie. Wszystko co musisz zrobić
to zaimportować dwie ścieżki do swojego projektu, co 
możesz zrobić przy użyciu "Importuj Audio" w menu projektu,
lub po prostu "przeciągnąć" dwa pliki audio do Audacity.
Gdy naciśniesz klawisz nagrywania, Audacity także stworzy nową ścieżkę automatycznie.

<p>

Aby zapisać swój mix możesz albo Eksportować projekt 
co spowoduje utworzenie mixu wszystkich twoich ścieżek 
automatycznie, albo możesz zaznaczyć ścieżki i użyć opcji
"Szybki mix" z menu Projekt.


<p>
<a name="a2"></a>
<font size=+1><b>
2. Czy można usunąć wokal z nagrania?
</b></font>
<p>

W niektórych stereofonicznych rejestratorach dźwięku jest możliwe usunięcie
wokali, w zależności od tego jak został on zmiksowany w studio.
Często wokale umieszczane są dokładnie pośrodku, podczas gdy instrumenty
są nieznacznie porozsuwane. Jeżeli odejmiesz od siebie kanały lewy i prawy,
wokal zostanie całkowicie wycięty,pozostawiając jedynie instrumenty.
<p>

<b>To działa tylko dla niektórych nagrań!</b>
<p>
Aby wykonać tego w Audacity, importuj nagranie stereo, wybierz
na ścieżce menu pop
To attempt this in Audacity, import a stereo recording, then
click on the track pop-up menu (troche poniżej dolnej strzałki
za nazwą ścieżki(i wybierz "Rozdziel ścieżki stereo".
Następnie zaznacz niższą ścieżkę (prawy kanał) i użyj efektu "Odwróć" z menu Efektów.
W kolejnym kroku utwórz z obie ścieżki monofoniczne i połącz je przy użyciu
Szybkiego miksu. Jeżeli małeś szczęście , wokale zostaną usunięte.

<p>
<a name="a3"></a>
<font size=+1><b>
3. Podczas próby "dogrywania się" do odtwarzanego dźwięku, dlaczego obie ścieżki są rozsynchronizowane?
</b></font>
<p>
Jest to normalne zjawisko i dzieje się tak ponieważ jest małe opóźnienie pomiędzy rozpoczęciem odtwarzania przez Audacity, a sygnałem docierającym do głośnika.
Audacity nie koryguje tego opóźnienia automatycznie, musisz tego dokonać manualnie.
<p>
Aby skorygować opóźnienie, użyj narzędzia do przesuwania w czasie i przesuń jedną ze ścieżek aż uda ci się to skorygować.
Zauważ że możesz użyć tego narzędzia podczas odsłuchu nagrania.
<p>

<a name="a4"></a>
<font size=+1><b>
4. Dlaczego Audacity nie jest rozpowrzechniany razem enkoderem MP3?
</b></font>
<p>

Niestety, algorytm służący do enkodowania lub tworzenia plików MP3
jest opatentowany i <b>Audacity</b> nie może zawierać algorytmu exportu
do MP3 bez konieczności uiszczenia honorarium lub naruszenia prawa 
w niektórych państwach.
<p>
Ale tak się składa, że Audacity jest przygotowane do
używania enkoderów MP3, które musisz ściągną niezależnie.
Powinieneś mieć pewność że spełniasz warunki licencji
nakłożone na enkodery MP3.
<p>
Aby uzyskać więcej informacji, zobacz stronę Eksportowanie plików MP3 w
naszej pomocy Online.
<p>

<a name="a5"></a>
<font size=+1><b>
5. Czy można nagrywać RealAudio lub inne strumienie audio?
</b></font>
</p><p>

Nie automatycznie. Wiele strumieni audio, głównie dostarczających muzykę:
RealOne Player, Windows Media Player i Quicktime jest specjalnie zrobionych
tak żeby uniemożliwić zapis.

</p><p>
Mimo to prawie zawsze jest możliwe nagrywanie wszystkiego co twoj komputer 
odtwarza, poprzez zapętlenie Twojego wyjścia audio (jack) z Twoim
wejściem (sound-in/microphone) w twojej karcie dźwiękowej.
</p><p>
Użyj kabla z wtyczkami stereo mini jack. Podłącz jeden koniec do wyjścia
karty dźwiękowej(tam gdzie podłączasz głośniki lub słuchawki - często jest koloru zielonego).
Drugi koniec podłącz do wejścia mikrofonowego lub liniowego (często jest koloru czerwonego).
</p><p>
Teraz Audacity potrafi nagrywać wszystko co komputer odtwarza.
Naciśnij Nagrywaj w Audacity, a następnie Odtwarzaj w Twoim programie do odtwarzania strumienia audio.
</p><p>

<a name="a6"></a>
<font size=+1><b>
6. Pomocy! Naciskam Nagrywaj, a otrzymuję tylko ciszę!
</b></font>
</p><p>
Audacity nagrywa domyślny sygnał wejściowy który jest wybrany w Twoim
systemie operacyjnym. Jeżel masz wiele wejść 
(takich jak np. wbudowany mikrofon,zewnętrzny mikrofon,wejście liniowe jack,
lub audio CD) musisz wybrać to którego będziesz używać do nagrywania.
Jeżeli na przykład będziesz próbować nagrywać mikrofon, a twoje źródło dźwięku jest ustawione
na wejście liniowe - Audacity nagra ciszę.
</p><p>
Jeżeli używasz Windows'a źródło dźwięku możesz ustawić poprzez 
kliknięcie prawym klawiszem na ikonce głłośnika w prawym dolnym rogu
</p><p>
Jeżeli używasz Mac OS 9, użyj panelu kontroli dźwięku. W systemie Mac OS X
użyj panel dźwięku we właściwościach systemu.
</p><p>
W linuksie użyj miksera, np. "xmixer" or "kmix".
</p><p>

<a name="a7"></a>
<font size=+1><b>
7. Jak podzielić pojedynczy plik na wiele ścieżek?
</b></font>
</p><p>
Czasami możesz nagrać wiele utworów w jednym ciągłym pliku. Jeżeli
chcesz wypalić te piosenki na płycie CD jako osobne ścieżki,
musisz utworzyć osobny plik dla każdej piosenki.
</p><p>
JEst wiele sposobów na podział nagrania na wiele ścieżek przy użyciu Audacity.
Tutaj zostały wymienione niektóre z nich, które możesz wypróbować:
</p><p>
Metoda 1: Eksport zaznaczenia
</p><ul>
<li>Zaznacz region odpowiadający pierwszej ścieżce. 
</li><li>Wybierz opcję
"Eksportuj zaznaczenie jako WAV" z menu Plik i zapisz go na dysk.
</li><li>Powtórz dla pozostałych ścieżek.
</li></ul>
<p>
Metoda 2: Usuń i Cofnij
</p><ul>
<li>Usuń wszystko <i>za wyjątkiem</i> pierwszej ścieżki
</li><li>Wybierz opcję
"Eksportuj zaznaczenie jako WAV" z menu Plik i zapisz go na dysk.
</li><li>Wybierz  "Cofnij" z menu Edycja.  Cofaj tak długo aż odzyskasz cały materiał audio zpowrotem.
(Audacity ma nieograniczone cofanie)
</li><li>Powtóz dla pozostałych ścieżek.
</li></ul>
<p>
Metoda 3: Podział i Eksport
</p><ul>
<li>Zaznacz to cochcesz żeby było pierwszą ścieżką.
</li><li>Wybierz "Podziel" zmenu Edycji, co przeniesie zaznaczenie
 do osobnej ścieżki w Audacity.
</li><li>Rób to aż wszystkie piosenki będoą w osobnych ścieżkach.
Nie zapominaj że możesz cofnąć każdą operację jeżeli popełniłeś błąd.
    
</li><li>Teraz cofni się do opcji "Eksportuj zaznaczenie jako WAV" żeby wyeksportować każdą
    ścieżkę do osobnego pliku. Aby szybko zaznaczyć każdą ścieżkę, kliknij na jej
    etykiecie (w dowolnym miejscu panela z lewej strony przebiegu czasowego, poniżej
    tytułu).
</li></ul>
<p>

<a name="a8"></a>
<font size=+1><b>
8. Jak można "wyciągnąć" utwór z płyty audio CD?
</b></font>
</p><p>
Audacity nie "wyciąga" utworów z płyt audio CD. Musisz użyć programu
do "rippowania" ,aby tego dokonać.
</p><p>
Dla Windowsa polecamy<a href="http://cdexos.sourceforge.net/">CDex</a>.
</p><p>
Dla Mac OS (9 i X), polecamy Apple'a <a href="http://www.apple.com/itunes/">iTunes</a>.
</p><p>
Dla Linuksa, wypróbuj <a href="http://www.xiph.org/paranoia/">Paranoia</a>.

<?php BoxBottom(); ?>
