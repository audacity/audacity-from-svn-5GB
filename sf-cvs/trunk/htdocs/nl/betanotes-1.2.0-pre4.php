<pre>
Wijzigingen in 1.2.0-pre4:

  * Problemen hersteld die voor konden komen bij het importeren van
    bepaalde non-seekable PCM audio bestanden, zoals GSM610.

  * Fout hersteld that die veroorzaakte dat de samples horizontaal van het
    scherm verdween bij het ver inzoomen als de track een time-shift offset
    had.

  * Herstelde fouten in de nieuwe resampler die ruis toevoegde aan 
    geresamplde audio op sommige systemen. Als je ruis opmerkte bij het
    exporteren naar een WAV, MP3 of OGG bestand, kan het zijn dat je
    last van deze fouten hebt gehad.
  
  * Fout hersteld die tot een strach leiden kon bij gebruik van de
    time-shift tool in combinatie met high zoom factoren.
    
  * Dithering wordt nu allen bij exporteren toegepast als het echt
    noodzakelijk is (bijv. float samples converteren naar 16-bit).
    
  * Bestanden met alleen mono tracks worden nu automatisch geëxporteerd
    naar stereo bestanden als ze tracks bevatten die naar links or rechts
    zijn 'gepand'.
    
  * De Delete toets kan nu gebruikt worden om de huidige selectie te 
    verwijderen, dat slechts met de Backspace toets mogelijk was.

  * Foute herseld waarin Audacity niet vroeg om wijzigingen op te slaan
    bij het sluiten van projecten of het stoppen tijdens opnames.

  * Mac OS X: Ondersteuning voor Playthrough (luisteren naar hetgeen wat je
    aan het opnemen bent, tijdens diezelfde opname) als je hardware apparaat 
    dit ondersteunt.

  * Mac OS X: Audacity is nu een pakket (je kunt een rechtermuis-klik op
    Audacity.app geven en 'Show Package Contents' selecteren). Opstart tijd 
    is behoorlijk verbeterd.

  * MS Windows: Probleem hersteld die bij Windows XP ertoe leidde dat de 
    verkorte naam van een bestand ("TESTFI~1.AUP") werd gebruikt. Dat 
    veroorzaakte een probleem als het bestand later werd geopend met de lange 
    benaming.
    
  * MS Windows: Fout hersteld die het exporteren van een bestand liet mislukken
    indien het bestand werd opgeslagen in een root folder van een Windows 
    schijf.

  * MS Windows: Audacity's applicatie informatie dat wordt weggeschreven naar 
    het Windows register, bevat nu altijd het volledige pad naar de 
    executable. 

  * MS Windows: problemen hersteld bij het proberen als geen-admin gebruiker 
    het Windows register bestsandstype associaties toe te wijzen.

  * Zorg ervoor dat het "Save" commando is geactiveerd na het wijzigen van de
    gain en pan schuifregelaars.

  * Vertalingen ge-update.  Verwijzing naar de vertaler opgenomen in de 
    betreffende versies van Audacity.

</pre>