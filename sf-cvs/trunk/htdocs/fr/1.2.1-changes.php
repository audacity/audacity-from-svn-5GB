<h2>Changements dans Audacity 1.2.1</h2>

<ul>
  <li><p>Les traductions suivantes ont été ajoutées ou mises à jour: Finnois, Français, Hongrois, Italien, Japonais, Norvégien, Polonais, Russe.

  <li><p>Corrige un bogue qui peut causer la perte de données en copiant de l'audio dans un projet et le collant dans une autre, après que le premier projet ait été sauvé et fermé.

  <li><p>Corrige un crash possible lors de l'ouverture ou du redimensionnement de la fenêtre d'égalisation, spécialement lors de l'utilisation de grandes polices systême.

  <li><p>Don't allow percentages less than -100% in Change Pitch/Speed/Tempo
    effects (fixes a possible crash).

  <li><p>Fix a crash when the temporary directory is not available on startup.

  <li><p>Correctly load ID3 tags saved in Audacity project files.

  <li><p>On Linux and OS X, store lockfiles in the temp directory instead of
    the user's home directory.  This fixes problems in lab environments
    where users have restricted or network-mounted home directories.

  <li><p>Fix a bug that prevented Nyquist effects from running when certain
    regional settings were activated.

  <li><p>Fix a bug in the Quick Mix command that could cause old temporary
    files to not be deleted.

  <li><p>Linux: Fix endianness problems in playback on PowerPC.

  <li><p>Linux: Fix compilation problem in Nyquist on MIPS.

  <li><p>Linux: Include a more recent PortAudio v19 snapshot (fixes compilation
    problems when building with the --with-portaudio=v19 option).

  <li><p>Two new Nyquist plug-ins: "Cross Fade In" and "Cross Fade Out."
  
  <li><p>Other minor bug-fixes.
</ul>