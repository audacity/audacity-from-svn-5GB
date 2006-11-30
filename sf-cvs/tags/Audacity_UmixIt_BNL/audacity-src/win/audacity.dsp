# Microsoft Developer Studio Project File - Name="Audacity" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

CFG=Audacity - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "audacity.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "audacity.mak" CFG="Audacity - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Audacity - Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Audacity - Win32 Debug" (based on "Win32 (x86) Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Audacity - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "$(WXWIN)\lib\msw" /I "." /I "$(WXWIN)\include" /I "..\src\include" /I "..\src\include\win32" /I "..\lib-src\allegro" /I "..\lib-src\expat" /I "..\lib-src\libid3tag" /I "..\lib-src\portaudio\pa_common" /I "..\lib-src\libsndfile\src" /I "..\lib-src\libmad" /I "..\lib-src\libogg\include" /I "..\lib-src\libvorbis\include" /I "..\lib-src\libnyquist\nyx" /I "..\lib-src\portmixer\px_common" /I "..\lib-src\libsamplerate\src" /I "..\lib-src\libflac\include" /I "..\lib-src\soundtouch\include" /I "..\lib-src\libresample\include" /D "NDEBUG" /D "__WX__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D "__WIN95__" /D "__WIN32__" /D WINVER=0x0400 /D "STRICT" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /i "$(WXWIN)\include" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 expat.lib sndfile.lib PAStaticWMME.lib wxmsw.lib png.lib zlib.lib jpeg.lib tiff.lib libci.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /libpath:"..\lib-src\libogg\win32\static_release" /libpath:"..\lib-src\libvorbis\win32\vorbis_static_release" /libpath:"..\lib-src\libvorbis\win32\vorbisfile_static_release" /libpath:"$(WXWIN)\lib" /libpath:"..\lib-src\libid3tag" /libpath:"..\lib-src\expat" /libpath:"..\lib-src\libmad" /libpath:"..\lib-src\libsndfile\Win32" /libpath:"..\lib-src\portaudio\winproj\lib" /libpath:"..\lib-src\allegro" /libpath:"..\lib-src\libnyquist" /libpath:"..\lib-src\portmixer\winproj" /libpath:"..\lib-src\libsamplerate\Win32" /libpath:"..\lib-src\libflac\obj\lib" /libpath:"..\lib-src\soundtouch\source\SoundTouch\Release" /libpath:"..\lib-src\libresample\win"
# SUBTRACT LINK32 /pdb:none /nodefaultlib

!ELSEIF  "$(CFG)" == "Audacity - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MTd /W3 /GX /Zi /Od /I "$(WXWIN)\lib\mswd" /I "." /I "$(WXWIN)\include" /I "..\src\include" /I "..\src\include\win32" /I "..\lib-src\allegro" /I "..\lib-src\expat" /I "..\lib-src\libid3tag" /I "..\lib-src\portaudio\pa_common" /I "..\lib-src\libsndfile\src" /I "..\lib-src\libmad" /I "..\lib-src\libogg\include" /I "..\lib-src\libvorbis\include" /I "..\lib-src\libnyquist\nyx" /I "..\lib-src\portmixer\px_common" /I "..\lib-src\libsamplerate\src" /I "..\lib-src\libflac\include" /I "..\lib-src\soundtouch\include" /I "..\lib-src\libresample\include" /D "_DEBUG" /D DEBUG=1 /D "__WXDEBUG__" /D "__WX__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D "__WIN95__" /D "__WIN32__" /D WINVER=0x0400 /D "STRICT" /YX"wx/wxprec.h" /FD /c
# SUBTRACT CPP /X
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /i "$(WXWIN)\include" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 expatd.lib sndfiled.lib PAStaticWMMED.lib wxmswd.lib pngd.lib zlibd.lib jpegd.lib tiffd.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept /libpath:"..\lib-src\libogg\win32\static_debug" /libpath:"..\lib-src\libvorbis\win32\vorbis_static_debug" /libpath:"..\lib-src\libvorbis\win32\vorbisfile_static_debug" /libpath:"$(WXWIN)\lib" /libpath:"..\lib-src\libid3tag" /libpath:"..\lib-src\expat" /libpath:"..\lib-src\libmad" /libpath:"..\lib-src\libsndfile\Win32" /libpath:"..\lib-src\portaudio\winproj\lib" /libpath:"..\lib-src\allegro" /libpath:"..\lib-src\libnyquist" /libpath:"..\lib-src\portmixer\winproj" /libpath:"..\lib-src\libsamplerate\Win32" /libpath:"..\lib-src\libflac\obj\lib" /libpath:"..\lib-src\soundtouch\source\SoundTouch\Debug" /libpath:"..\lib-src\libresample\win" /IGNORE:4049
# SUBTRACT LINK32 /verbose /pdb:none

!ENDIF 

# Begin Target

# Name "Audacity - Win32 Release"
# Name "Audacity - Win32 Debug"
# Begin Group "src"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\AboutDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\src\AboutDialog.h
# End Source File
# Begin Source File

SOURCE=..\src\AColor.cpp
# End Source File
# Begin Source File

SOURCE=..\src\AColor.h
# End Source File
# Begin Source File

SOURCE=..\src\AStatus.cpp
# End Source File
# Begin Source File

SOURCE=..\src\AStatus.h
# End Source File
# Begin Source File

SOURCE=..\src\AudacityApp.cpp
# End Source File
# Begin Source File

SOURCE=..\src\AudacityApp.h
# End Source File
# Begin Source File

SOURCE=..\src\AudioIO.cpp
# End Source File
# Begin Source File

SOURCE=..\src\AudioIO.h
# End Source File
# Begin Source File

SOURCE=..\src\Benchmark.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Benchmark.h
# End Source File
# Begin Source File

SOURCE=..\src\BlockFile.cpp
# End Source File
# Begin Source File

SOURCE=..\src\BlockFile.h
# End Source File
# Begin Source File

SOURCE=..\src\ControlToolBar.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ControlToolBar.h
# End Source File
# Begin Source File

SOURCE=..\src\DirManager.cpp
# End Source File
# Begin Source File

SOURCE=..\src\DirManager.h
# End Source File
# Begin Source File

SOURCE=..\src\EditToolBar.cpp
# End Source File
# Begin Source File

SOURCE=..\src\EditToolBar.h
# End Source File
# Begin Source File

SOURCE=..\src\Envelope.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Envelope.h
# End Source File
# Begin Source File

SOURCE=..\src\FFT.cpp
# End Source File
# Begin Source File

SOURCE=..\src\FFT.h
# End Source File
# Begin Source File

SOURCE=..\src\FileFormats.cpp
# End Source File
# Begin Source File

SOURCE=..\src\FileFormats.h
# End Source File
# Begin Source File

SOURCE=..\src\FormatSelection.cpp
# End Source File
# Begin Source File

SOURCE=..\src\FormatSelection.h
# End Source File
# Begin Source File

SOURCE=..\src\FreqWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\src\FreqWindow.h
# End Source File
# Begin Source File

SOURCE=..\src\Help.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Help.h
# End Source File
# Begin Source File

SOURCE=..\src\HistoryWindow.cpp
# End Source File
# Begin Source File

SOURCE=..\src\HistoryWindow.h
# End Source File
# Begin Source File

SOURCE=..\src\ImageManipulation.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ImageManipulation.h
# End Source File
# Begin Source File

SOURCE=..\src\Internat.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Internat.h
# End Source File
# Begin Source File

SOURCE=..\src\LabelTrack.cpp
# End Source File
# Begin Source File

SOURCE=..\src\LabelTrack.h
# End Source File
# Begin Source File

SOURCE=..\src\Landmark.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Landmark.h
# End Source File
# Begin Source File

SOURCE=..\src\LangChoice.cpp
# End Source File
# Begin Source File

SOURCE=..\src\LangChoice.h
# End Source File
# Begin Source File

SOURCE=..\src\Languages.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Languages.h
# End Source File
# Begin Source File

SOURCE=..\src\Legacy.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Legacy.h
# End Source File
# Begin Source File

SOURCE=..\src\Menus.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Menus.h
# End Source File
# Begin Source File

SOURCE=..\src\MeterToolBar.cpp
# End Source File
# Begin Source File

SOURCE=..\src\MeterToolBar.h
# End Source File
# Begin Source File

SOURCE=..\src\Mix.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Mix.h
# End Source File
# Begin Source File

SOURCE=..\src\MixerToolBar.cpp
# End Source File
# Begin Source File

SOURCE=..\src\MixerToolBar.h
# End Source File
# Begin Source File

SOURCE=..\src\NoteTrack.cpp
# End Source File
# Begin Source File

SOURCE=..\src\NoteTrack.h
# End Source File
# Begin Source File

SOURCE=..\src\PitchName.cpp
# End Source File
# Begin Source File

SOURCE=..\src\PitchName.h
# End Source File
# Begin Source File

SOURCE=..\src\PlatformCompatibility.cpp
# End Source File
# Begin Source File

SOURCE=..\src\PlatformCompatibility.h
# End Source File
# Begin Source File

SOURCE=..\src\Prefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Prefs.h
# End Source File
# Begin Source File

SOURCE=..\src\Project.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Project.h
# End Source File
# Begin Source File

SOURCE=..\src\Resample.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Resample.h
# End Source File
# Begin Source File

SOURCE=..\src\RingBuffer.cpp
# End Source File
# Begin Source File

SOURCE=..\src\RingBuffer.h
# End Source File
# Begin Source File

SOURCE=..\src\SampleFormat.cpp
# End Source File
# Begin Source File

SOURCE=..\src\SampleFormat.h
# End Source File
# Begin Source File

SOURCE=..\src\Sequence.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Sequence.h
# End Source File
# Begin Source File

SOURCE=..\src\Spectrum.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Spectrum.h
# End Source File
# Begin Source File

SOURCE=..\src\Tags.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Tags.h
# End Source File
# Begin Source File

SOURCE=..\src\TimeTrack.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TimeTrack.h
# End Source File
# Begin Source File

SOURCE=..\src\ToolBar.cpp
# End Source File
# Begin Source File

SOURCE=..\src\ToolBar.h
# End Source File
# Begin Source File

SOURCE=..\src\Track.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Track.h
# End Source File
# Begin Source File

SOURCE=..\src\TrackArtist.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TrackArtist.h
# End Source File
# Begin Source File

SOURCE=..\src\TrackPanel.cpp
# End Source File
# Begin Source File

SOURCE=..\src\TrackPanel.h
# End Source File
# Begin Source File

SOURCE=..\src\UndoManager.cpp
# End Source File
# Begin Source File

SOURCE=..\src\UndoManager.h
# End Source File
# Begin Source File

SOURCE=..\src\ViewInfo.h
# End Source File
# Begin Source File

SOURCE=..\src\WaveTrack.cpp
# End Source File
# Begin Source File

SOURCE=..\src\WaveTrack.h
# End Source File
# End Group
# Begin Group "src/effects"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\effects\Amplify.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Amplify.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\BassBoost.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\BassBoost.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\ChangePitch.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\ChangePitch.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\ChangeSpeed.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\ChangeSpeed.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\ChangeTempo.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\ChangeTempo.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Compressor.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Compressor.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Echo.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Echo.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Effect.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Effect.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Equalization.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Equalization.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Fade.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Fade.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Filter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Filter.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Invert.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Invert.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\LoadEffects.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\LoadEffects.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Noise.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\NoiseRemoval.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\NoiseRemoval.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Normalize.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Normalize.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Phaser.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Phaser.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Repeat.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Repeat.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Reverse.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Reverse.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Silence.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\SimpleMono.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\SimpleMono.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\SimplePairedTwoTrack.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\SimplePairedTwoTrack.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\SoundTouchEffect.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\SoundTouchEffect.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\ToneGen.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\ToneGen.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Wahwah.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Wahwah.h
# End Source File
# End Group
# Begin Group "src/effects/VST"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\effects\VST\LoadVSTWin.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\LoadVSTWin.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\VSTEffect.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\VSTEffect.h
# End Source File
# End Group
# Begin Group "src/export"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\export\Export.cpp
# End Source File
# Begin Source File

SOURCE=..\src\export\Export.h
# End Source File
# Begin Source File

SOURCE=..\src\export\ExportMP3.cpp
# End Source File
# Begin Source File

SOURCE=..\src\export\ExportMP3.h
# End Source File
# Begin Source File

SOURCE=..\src\export\ExportMultiple.cpp
# End Source File
# Begin Source File

SOURCE=..\src\export\ExportMultiple.h
# End Source File
# Begin Source File

SOURCE=..\src\export\ExportOGG.cpp
# End Source File
# Begin Source File

SOURCE=..\src\export\ExportOGG.h
# End Source File
# Begin Source File

SOURCE=..\src\export\ExportPCM.cpp
# End Source File
# Begin Source File

SOURCE=..\src\export\ExportPCM.h
# End Source File
# End Group
# Begin Group "src/import"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\import\Import.cpp
# End Source File
# Begin Source File

SOURCE=..\src\import\Import.h
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportLOF.cpp
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportLOF.h
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportMIDI.cpp
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportMIDI.h
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportMP3.cpp
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportMP3.h
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportOGG.cpp
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportPCM.cpp
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportPCM.h
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportRaw.cpp
# End Source File
# Begin Source File

SOURCE=..\src\import\ImportRaw.h
# End Source File
# Begin Source File

SOURCE=..\src\import\RawAudioGuess.cpp
# End Source File
# Begin Source File

SOURCE=..\src\import\RawAudioGuess.h
# End Source File
# End Group
# Begin Group "src/prefs"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\prefs\AudioIOPrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\AudioIOPrefs.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\DirectoriesPrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\DirectoriesPrefs.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\FileFormatPrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\FileFormatPrefs.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\GUIPrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\GUIPrefs.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\KeyConfigPrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\KeyConfigPrefs.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\PrefsDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\PrefsDialog.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\PrefsPanel.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\QualityPrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\QualityPrefs.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\SpectrumPrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\SpectrumPrefs.h
# End Source File
# End Group
# Begin Group "src/widgets"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\widgets\AButton.cpp
# End Source File
# Begin Source File

SOURCE=..\src\widgets\AButton.h
# End Source File
# Begin Source File

SOURCE=..\src\widgets\ASlider.cpp
# End Source File
# Begin Source File

SOURCE=..\src\widgets\ASlider.h
# End Source File
# Begin Source File

SOURCE=..\src\widgets\Meter.cpp
# End Source File
# Begin Source File

SOURCE=..\src\widgets\Meter.h
# End Source File
# Begin Source File

SOURCE=..\src\widgets\Ruler.cpp
# End Source File
# Begin Source File

SOURCE=..\src\widgets\Ruler.h
# End Source File
# Begin Source File

SOURCE=..\src\widgets\Warning.cpp
# End Source File
# Begin Source File

SOURCE=..\src\widgets\Warning.h
# End Source File
# End Group
# Begin Group "Resources"

# PROP Default_Filter ".bmp, .cur, .rc, .ico"
# Begin Source File

SOURCE=.\audacity.ico
# End Source File
# Begin Source File

SOURCE=.\audacity.rc
# End Source File
# End Group
# Begin Group "src/xml"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\xml\XMLFileReader.cpp
# End Source File
# Begin Source File

SOURCE=..\src\xml\XMLFileReader.h
# End Source File
# Begin Source File

SOURCE=..\src\xml\XMLTagHandler.cpp
# End Source File
# Begin Source File

SOURCE=..\src\xml\XMLTagHandler.h
# End Source File
# End Group
# Begin Group "src/effects/nyquist"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\effects\nyquist\LoadNyquist.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\nyquist\LoadNyquist.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\nyquist\Nyquist.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\nyquist\Nyquist.h
# End Source File
# End Group
# Begin Group "src/commands"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\commands\CommandManager.cpp
# End Source File
# Begin Source File

SOURCE=..\src\commands\CommandManager.h
# End Source File
# Begin Source File

SOURCE=..\src\commands\Keyboard.cpp
# End Source File
# Begin Source File

SOURCE=..\src\commands\Keyboard.h
# End Source File
# End Group
# Begin Group "src/blockfile"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\blockfile\LegacyAliasBlockFile.cpp
# End Source File
# Begin Source File

SOURCE=..\src\blockfile\LegacyAliasBlockFile.h
# End Source File
# Begin Source File

SOURCE=..\src\blockfile\LegacyBlockFile.cpp
# End Source File
# Begin Source File

SOURCE=..\src\blockfile\LegacyBlockFile.h
# End Source File
# Begin Source File

SOURCE=..\src\blockfile\PCMAliasBlockFile.cpp
# End Source File
# Begin Source File

SOURCE=..\src\blockfile\PCMAliasBlockFile.h
# End Source File
# Begin Source File

SOURCE=..\src\blockfile\SilentBlockFile.cpp
# End Source File
# Begin Source File

SOURCE=..\src\blockfile\SilentBlockFile.h
# End Source File
# Begin Source File

SOURCE=..\src\blockfile\SimpleBlockFile.cpp
# End Source File
# Begin Source File

SOURCE=..\src\blockfile\SimpleBlockFile.h
# End Source File
# End Group
# Begin Group "src/effects/ladspa"

# PROP Default_Filter ""
# Begin Source File

SOURCE=..\src\effects\ladspa\ladspa.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\ladspa\LadspaEffect.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\ladspa\LadspaEffect.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\ladspa\LoadLadspa.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\ladspa\LoadLadspa.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\wx\msw\blank.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\bullseye.cur
# End Source File
# Begin Source File

SOURCE=..\src\Dither.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Dither.h
# End Source File
# Begin Source File

SOURCE=.\wx\msw\error.ico
# End Source File
# Begin Source File

SOURCE=.\wx\msw\hand.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\info.ico
# End Source File
# Begin Source File

SOURCE=.\wx\msw\magnif1.cur
# End Source File
# Begin Source File

SOURCE=..\src\prefs\MousePrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\MousePrefs.h
# End Source File
# Begin Source File

SOURCE=.\wx\msw\noentry.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\pbrush.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\pencil.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\pntleft.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\pntright.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\query.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\question.ico
# End Source File
# Begin Source File

SOURCE=.\wx\msw\roller.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\size.cur
# End Source File
# Begin Source File

SOURCE=.\wx\msw\tip.ico
# End Source File
# Begin Source File

SOURCE=.\wx\msw\warning.ico
# End Source File
# Begin Source File

SOURCE=.\wx\msw\watch1.cur
# End Source File
# Begin Source File

SOURCE=.\wx\html\msw\wbook.ico
# End Source File
# Begin Source File

SOURCE=.\wx\html\msw\wfolder.ico
# End Source File
# Begin Source File

SOURCE=.\wx\html\msw\whelp.ico
# End Source File
# Begin Source File

SOURCE=.\wx\html\msw\whlproot.ico
# End Source File
# Begin Source File

SOURCE=.\winlibs.c
# End Source File
# Begin Source File

SOURCE=.\wx\html\msw\wpage.ico
# End Source File
# End Target
# End Project
