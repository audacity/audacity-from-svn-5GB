<h2>Zmiany w Audacity w wersji 1.2.1</h2>

<ul>
  <li><p>Poniższe tłumaczenia zostały dodane lub uzupełnione: fińskie,
    francuskie, węgierskie, włoskie, japońskie, norweskie, polskie, rosyjskie.sian.

  <li><p>Naprawiono błąd powodujący utratę danych w przypadku wklejania
    danych z jednego projektu do drugiego wtedy gdy ten pierwszy projekt
    był zapisywany i zamykany.

  <li><p>Usunięto możliwe "rozwalenie" programu w momencie otwierania lub
    zmiany rozmiarów okna Korekcji Częstotliwościowej, szczególnie w przypadku 
    używania dużych czcionek ekranowych.

  <li><p>Uniemożliwienie ustawienia wartości procentowej mniejszej niż -100%
     w efekcie Zmiany Tempa/Wysokości (Zapobieżono możliwości uszkodzenia programu).

  <li><p>Usunięto problem "wywracania się" w przypadku gdy 
      katalog tymczasowy jest niedostępny przy starcie programu.

  <li><p>Poprawiono ładowanie tagów ID3 zapisywanych w plikach projektu Audacity.

  <li><p>W systemach Linux i OS X, lockfile przeniesiono do katalogu tymczasowego
     zamiast katalogu domowego. Rozwiązało to problemy w środowisku laboratoryjnym
     gdy użytkownicy mieli pewne ograniczenia lub katalogi domowe były montowane
     przez sieć.

  <li><p>Fix a bug that prevented Nyquist effects from running when certain
    regional settings were activated.

  <li><p>Usunięcie błędu w przypadku używania funkcji "Szybkiego Mixu" (Quick Mix) która mogła  powodować niekasowanie starych plików tymczasowych.

  <li><p>Linux: Fix endianness problems in playback on PowerPC.

  <li><p>Linux: Fix compilation problem in Nyquist on MIPS.

  <li><p>Linux: Dołączono bardziej aktualną wersję PortAudio v19 (rozwiązało to problem
    występujący w czasie budowania programu z opcją --with-portaudio=v19 ).

  <li><p>Dwa nowe pluginy Nyquist'a : "Cross Fade In" i "Cross Fade Out."
  
  <li><p>Usunięcie innych mniej ważnych błędów.
</ul>

