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
# ADD CPP /nologo /MT /W3 /GX /O2 /Ob2 /I "." /I "..\src\include" /I "..\src\include\win32" /I "..\lib-src\allegro" /D "NDEBUG" /D "__WX__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D "__WIN95__" /D "__WIN32__" /D WINVER=0x0400 /D "STRICT" /FD /c
# SUBTRACT CPP /YX
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /i "h:\wx2\include" /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 mad.lib sndfile.lib ogg_static.lib vorbis_static.lib vorbisfile_static.lib PAStaticWMME.lib wx.lib xpm.lib png.lib zlib.lib jpeg.lib tiff.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"MSVCRT.LIB" /nodefaultlib:"MSVCIRT.LIB" /libpath:"..\lib-src\libmad" /libpath:"..\lib-src\libsndfile\Win32" /libpath:"..\lib-src\libogg\win32\static_release" /libpath:"..\lib-src\libvorbis\win32\vorbis_static_release" /libpath:"..\lib-src\libvorbis\win32\vorbisfile_static_release" /libpath:"..\lib-src\portaudio\winproj\lib" /libpath:"..\lib-src\allegro"
# SUBTRACT LINK32 /nodefaultlib

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
# ADD CPP /nologo /MTd /W3 /GX /ZI /Od /I "." /I "..\src\include" /I "..\src\include\win32" /I "..\lib-src\allegro" /D "_DEBUG" /D DEBUG=1 /D "__WXDEBUG__" /D "__WX__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D "__WIN95__" /D "__WIN32__" /D WINVER=0x0400 /D "STRICT" /YX"wx/wxprec.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /i "h:\wx2\include" /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 madd.lib ogg_static.lib vorbis_static.lib vorbisfile_static.lib sndfiled.lib PAStaticWMMED.lib wxd.lib xpm.lib png.lib zlib.lib jpeg.lib tiff.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib winmm.lib /nologo /subsystem:windows /incremental:no /debug /machine:I386 /nodefaultlib:"libcd.lib" /nodefaultlib:"libcid.lib" /nodefaultlib:"msvcrt.lib" /nodefaultlib:"msvcrtd.lib" /nodefaultlib:"msvcirt.lib" /nodefaultlib:"msvcirtd.lib" /pdbtype:sept /libpath:"..\lib-src\libmad" /libpath:"..\lib-src\libsndfile\Win32" /libpath:"..\lib-src\libogg\win32\static_release" /libpath:"..\lib-src\libvorbis\win32\vorbis_static_release" /libpath:"..\lib-src\libvorbis\win32\vorbisfile_static_release" /libpath:"..\lib-src\portaudio\winproj\lib" /libpath:"..\lib-src\allegro"
# SUBTRACT LINK32 /pdb:none /nodefaultlib

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

SOURCE=..\src\APalette.cpp
# End Source File
# Begin Source File

SOURCE=..\src\APalette.h
# End Source File
# Begin Source File

SOURCE=..\src\AStatus.cpp
# End Source File
# Begin Source File

SOURCE=..\src\AStatus.h
# End Source File
# Begin Source File

SOURCE=..\src\AudacityApp.cpp

!IF  "$(CFG)" == "Audacity - Win32 Release"

# ADD CPP /MT /FAcs

!ELSEIF  "$(CFG)" == "Audacity - Win32 Debug"

!ENDIF 

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

SOURCE=..\src\configwin.h
# End Source File
# Begin Source File

SOURCE=..\src\DirManager.cpp
# End Source File
# Begin Source File

SOURCE=..\src\DirManager.h
# End Source File
# Begin Source File

SOURCE=..\src\DiskFunctions.cpp
# End Source File
# Begin Source File

SOURCE=..\src\DiskFunctions.h
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

SOURCE=..\src\Menus.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Mix.cpp
# End Source File
# Begin Source File

SOURCE=..\src\Mix.h
# End Source File
# Begin Source File

SOURCE=..\src\NoteTrack.cpp
# End Source File
# Begin Source File

SOURCE=..\src\NoteTrack.h
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

SOURCE=..\src\effects\LoadEffects.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\LoadEffects.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\NoiseRemoval.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\NoiseRemoval.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\Phaser.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\Phaser.h
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

SOURCE=..\src\effects\VST\AEffect.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\aeffectx.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\AudioEffect.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\audioeffectx.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\audioeffectx.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\LoadVSTWin.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\LoadVSTWin.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\vstcontrols.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\VSTEffect.cpp
# End Source File
# Begin Source File

SOURCE=..\src\effects\VST\VSTEffect.h
# End Source File
# Begin Source File

SOURCE=..\src\effects\vstgui.h
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

SOURCE=..\src\prefs\PrefsDialog.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\PrefsDialog.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\PrefsPanel.h
# End Source File
# Begin Source File

SOURCE=..\src\prefs\SampleRatePrefs.cpp
# End Source File
# Begin Source File

SOURCE=..\src\prefs\SampleRatePrefs.h
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
# End Target
# End Project
