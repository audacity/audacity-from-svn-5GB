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
!MESSAGE "Audacity - Win32 Debug DLL" (based on "Win32 (x86) Application")
!MESSAGE "Audacity - Win32 Release DLL" (based on "Win32 (x86) Application")
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
# ADD CPP /nologo /MD /W3 /GX /O2 /Ob2 /I "../../include" /I "../../contrib/include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D "__WIN95__" /D "__WIN32__" /D WINVER=0x0400 /D "STRICT" /D "__WX__" /YX"wx/wxprec.h" /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib winmm.lib wx.lib xpm.lib png.lib zlib.lib jpeg.lib tiff.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libc.lib" /nodefaultlib:"libci.lib" /nodefaultlib:"msvcrtd.lib" /libpath:"../../lib" /libpath:"../../contrib/lib"

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
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "../../include" /I "../../contrib/include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D DEBUG=1 /D "__WXDEBUG__" /D "__WIN95__" /D "__WIN32__" /D WINVER=0x0400 /D "STRICT" /D "__WX__" /YX"wx/wxprec.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib winmm.lib wxd.lib xpmd.lib pngd.lib zlibd.lib jpegd.lib tiffd.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcd.lib" /nodefaultlib:"libcid.lib" /nodefaultlib:"msvcrt.lib" /pdbtype:sept /libpath:"../../lib" /libpath:"../../contrib/lib"

!ELSEIF  "$(CFG)" == "Audacity - Win32 Debug DLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "DebugDLL"
# PROP BASE Intermediate_Dir "DebugDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "DebugDLL"
# PROP Intermediate_Dir "DebugDLL"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MDd /W3 /Gm /GX /ZI /Od /I "../../include" /I "../../contrib/include" /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D DEBUG=1 /D "__WXDEBUG__" /D "__WIN95__" /D "__WIN32__" /D WINVER=0x0400 /D "STRICT" /D WXUSINGDLL=1 /Yu"wx/wxprec.h" /FD /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x809 /d "_DEBUG"
# ADD RSC /l 0x809 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib winmm.lib wxdlld.lib /nologo /subsystem:windows /debug /machine:I386 /nodefaultlib:"libcd.lib" /nodefaultlib:"libcid.lib" /out:"DebugDLL/docview.exe" /pdbtype:sept /libpath:"../../lib" /libpath:"../../contrib/lib"

!ELSEIF  "$(CFG)" == "Audacity - Win32 Release DLL"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "ReleaseDLL"
# PROP BASE Intermediate_Dir "ReleaseDLL"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "ReleaseDLL"
# PROP Intermediate_Dir "ReleaseDLL"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /YX /FD /c
# ADD CPP /nologo /MD /W3 /GX /O1 /Ob2 /I "../../include" /I "../../contrib/include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D "__WIN95__" /D "__WIN32__" /D WINVER=0x0400 /D "STRICT" /D WXUSINGDLL=1 /FD /c
# SUBTRACT CPP /YX
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x809 /d "NDEBUG"
# ADD RSC /l 0x809 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib winmm.lib /nologo /subsystem:windows /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib comctl32.lib rpcrt4.lib wsock32.lib winmm.lib wxdll.lib /nologo /subsystem:windows /machine:I386 /nodefaultlib:"libc.lib" /nodefaultlib:"libci.lib" /out:"ReleaseDLL/docview.exe" /libpath:"../../lib" /libpath:"../../contrib/lib"

!ENDIF 

# Begin Target

# Name "Audacity - Win32 Release"
# Name "Audacity - Win32 Debug"
# Name "Audacity - Win32 Debug DLL"
# Name "Audacity - Win32 Release DLL"
# Begin Group "snd"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\snd\audio.h
# End Source File
# Begin Source File

SOURCE=.\snd\audiowin32.c
# End Source File
# Begin Source File

SOURCE=.\snd\audiowin32.h
# End Source File
# Begin Source File

SOURCE=.\snd\convert.c
# End Source File
# Begin Source File

SOURCE=.\snd\ieeecvt.c
# End Source File
# Begin Source File

SOURCE=.\snd\ieeecvt.h
# End Source File
# Begin Source File

SOURCE=.\snd\snd.c
# End Source File
# Begin Source File

SOURCE=.\snd\snd.h
# End Source File
# Begin Source File

SOURCE=.\snd\sndcvt.c
# End Source File
# Begin Source File

SOURCE=.\snd\sndhead.h
# End Source File
# Begin Source File

SOURCE=.\snd\sndheader.c
# End Source File
# Begin Source File

SOURCE=.\snd\sndio.c
# End Source File
# Begin Source File

SOURCE=.\snd\sndwin32.c
# End Source File
# Begin Source File

SOURCE=.\snd\sndwin32.h
# End Source File
# Begin Source File

SOURCE=.\snd\sndwrite.h
# End Source File
# End Group
# Begin Group "mp3"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\mp3\common.c
# End Source File
# Begin Source File

SOURCE=.\mp3\common.h
# End Source File
# Begin Source File

SOURCE=.\mp3\dct64_i386.c
# End Source File
# Begin Source File

SOURCE=.\mp3\decode_i386.c
# End Source File
# Begin Source File

SOURCE=.\mp3\huffman.h
# End Source File
# Begin Source File

SOURCE=.\mp3\interface.c
# End Source File
# Begin Source File

SOURCE=.\mp3\layer3.c
# End Source File
# Begin Source File

SOURCE=.\mp3\mpg123.h
# End Source File
# Begin Source File

SOURCE=.\mp3\mpglib.h
# End Source File
# Begin Source File

SOURCE=.\mp3\tabinit.c
# End Source File
# End Group
# Begin Group "effects"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\effects\Amplify.cpp
# End Source File
# Begin Source File

SOURCE=.\effects\Amplify.h
# End Source File
# Begin Source File

SOURCE=.\effects\Echo.cpp
# End Source File
# Begin Source File

SOURCE=.\effects\Echo.h
# End Source File
# Begin Source File

SOURCE=.\effects\Effect.cpp
# End Source File
# Begin Source File

SOURCE=.\effects\Effect.h
# End Source File
# Begin Source File

SOURCE=.\effects\Fade.cpp
# End Source File
# Begin Source File

SOURCE=.\effects\Fade.h
# End Source File
# End Group
# Begin Source File

SOURCE=.\AboutDialog.cpp
# End Source File
# Begin Source File

SOURCE=.\AboutDialog.h
# End Source File
# Begin Source File

SOURCE=.\res\audacity.rc
# End Source File
# Begin Source File

SOURCE=.\res\AudacityLogo.bmp
# End Source File
# Begin Source File

SOURCE=.\res\AudacitySmall.bmp
# End Source File
# Begin Source File

SOURCE=.\AudioApp.cpp
# End Source File
# Begin Source File

SOURCE=.\AudioApp.h
# End Source File
# Begin Source File

SOURCE=.\AudioDoc.cpp
# End Source File
# Begin Source File

SOURCE=.\AudioDoc.h
# End Source File
# Begin Source File

SOURCE=.\AudioFrame.cpp
# End Source File
# Begin Source File

SOURCE=.\AudioFrame.h
# End Source File
# Begin Source File

SOURCE=.\AudioView.cpp
# End Source File
# Begin Source File

SOURCE=.\AudioView.h
# End Source File
# Begin Source File

SOURCE=.\AutoCorrelate.cpp
# End Source File
# Begin Source File

SOURCE=.\AutoCorrelate.h
# End Source File
# Begin Source File

SOURCE=.\res\blank.cur
# End Source File
# Begin Source File

SOURCE=.\BlockFile.cpp
# End Source File
# Begin Source File

SOURCE=.\BlockFile.h
# End Source File
# Begin Source File

SOURCE=.\res\bullseye.cur
# End Source File
# Begin Source File

SOURCE=.\res\cross.bmp
# End Source File
# Begin Source File

SOURCE=.\DDC.h
# End Source File
# Begin Source File

SOURCE=.\DDCMath.h
# End Source File
# Begin Source File

SOURCE=.\DirManager.cpp
# End Source File
# Begin Source File

SOURCE=.\DirManager.h
# End Source File
# Begin Source File

SOURCE=.\res\disable.bmp
# End Source File
# Begin Source File

SOURCE=.\res\error.ico
# End Source File
# Begin Source File

SOURCE=.\Export.cpp
# End Source File
# Begin Source File

SOURCE=.\Export.h
# End Source File
# Begin Source File

SOURCE=.\FFTMisc.cpp
# End Source File
# Begin Source File

SOURCE=.\Fourier.h
# End Source File
# Begin Source File

SOURCE=.\FourierD.cpp
# End Source File
# Begin Source File

SOURCE=.\FourierF.cpp
# End Source File
# Begin Source File

SOURCE=.\GenericStream.cpp
# End Source File
# Begin Source File

SOURCE=.\GenericStream.h
# End Source File
# Begin Source File

SOURCE=.\res\hand.cur
# End Source File
# Begin Source File

SOURCE=.\Import.cpp
# End Source File
# Begin Source File

SOURCE=.\Import.h
# End Source File
# Begin Source File

SOURCE=.\ImportMIDI.cpp
# End Source File
# Begin Source File

SOURCE=.\ImportMIDI.h
# End Source File
# Begin Source File

SOURCE=.\ImportMP3.cpp
# End Source File
# Begin Source File

SOURCE=.\ImportMP3.h
# End Source File
# Begin Source File

SOURCE=.\ImportRaw.cpp
# End Source File
# Begin Source File

SOURCE=.\ImportRaw.h
# End Source File
# Begin Source File

SOURCE=.\res\info.ico
# End Source File
# Begin Source File

SOURCE=.\LabelTrack.cpp
# End Source File
# Begin Source File

SOURCE=.\LabelTrack.h
# End Source File
# Begin Source File

SOURCE=.\Landmark.cpp
# End Source File
# Begin Source File

SOURCE=.\Landmark.h
# End Source File
# Begin Source File

SOURCE=.\Mix.cpp
# End Source File
# Begin Source File

SOURCE=.\Mix.h
# End Source File
# Begin Source File

SOURCE=.\res\noentry.cur
# End Source File
# Begin Source File

SOURCE=.\NoteTrack.cpp
# End Source File
# Begin Source File

SOURCE=.\NoteTrack.h
# End Source File
# Begin Source File

SOURCE=.\res\pbrush.cur
# End Source File
# Begin Source File

SOURCE=.\Peak.cpp
# End Source File
# Begin Source File

SOURCE=.\Peak.h
# End Source File
# Begin Source File

SOURCE=.\res\pencil.cur
# End Source File
# Begin Source File

SOURCE=.\Pitch.cpp
# End Source File
# Begin Source File

SOURCE=.\Pitch.h
# End Source File
# Begin Source File

SOURCE=.\Play.h
# End Source File
# Begin Source File

SOURCE=.\PlaySnd.cpp
# End Source File
# Begin Source File

SOURCE=.\PlaySnd.h
# End Source File
# Begin Source File

SOURCE=.\res\plot_dwn.bmp
# End Source File
# Begin Source File

SOURCE=.\res\plot_zot.bmp
# End Source File
# Begin Source File

SOURCE=.\res\pntleft.cur
# End Source File
# Begin Source File

SOURCE=.\res\pntright.cur
# End Source File
# Begin Source File

SOURCE=.\res\query.cur
# End Source File
# Begin Source File

SOURCE=.\res\question.ico
# End Source File
# Begin Source File

SOURCE=.\res\roller.cur
# End Source File
# Begin Source File

SOURCE=.\res\size.cur
# End Source File
# Begin Source File

SOURCE=.\Spectrum.cpp
# End Source File
# Begin Source File

SOURCE=.\Spectrum.h
# End Source File
# Begin Source File

SOURCE=.\res\tick.bmp
# End Source File
# Begin Source File

SOURCE=.\res\tip.ico
# End Source File
# Begin Source File

SOURCE=.\Track.cpp
# End Source File
# Begin Source File

SOURCE=.\Track.h
# End Source File
# Begin Source File

SOURCE=.\UndoManager.cpp
# End Source File
# Begin Source File

SOURCE=.\UndoManager.h
# End Source File
# Begin Source File

SOURCE=.\res\warning.ico
# End Source File
# Begin Source File

SOURCE=.\res\watch1.cur
# End Source File
# Begin Source File

SOURCE=.\WaveTrack.cpp
# End Source File
# Begin Source File

SOURCE=.\WaveTrack.h
# End Source File
# End Target
# End Project
