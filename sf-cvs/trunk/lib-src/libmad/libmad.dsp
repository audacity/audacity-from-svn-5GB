# Microsoft Developer Studio Project File - Name="libmad" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libmad - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libmad.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libmad.mak" CFG="libmad - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libmad - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libmad - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libmad - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "include" /I "..\..\win" /D "NDEBUG" /D "__WX__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D "__WIN95__" /D "__WIN32__" /D "_MBCS" /D "_LIB" /D "HAVE_CONFIG_H" /D "FPM_INTEL" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"mad.lib"

!ELSEIF  "$(CFG)" == "libmad - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "include" /I "..\..\win" /D "_DEBUG" /D "__WXDEBUG__" /D "__WX__" /D "WIN32" /D "_WINDOWS" /D "__WINDOWS__" /D "__WXMSW__" /D "__WIN95__" /D "__WIN32__" /D "_MBCS" /D "_LIB" /D "HAVE_CONFIG_H" /D "FPM_INTEL" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"madd.lib"

!ENDIF 

# Begin Target

# Name "libmad - Win32 Release"
# Name "libmad - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\bit.c
# End Source File
# Begin Source File

SOURCE=.\decoder.c
# End Source File
# Begin Source File

SOURCE=.\fixed.c
# End Source File
# Begin Source File

SOURCE=.\frame.c
# End Source File
# Begin Source File

SOURCE=.\huffman.c
# End Source File
# Begin Source File

SOURCE=.\layer12.c
# End Source File
# Begin Source File

SOURCE=.\layer3.c
# End Source File
# Begin Source File

SOURCE=.\stream.c
# End Source File
# Begin Source File

SOURCE=.\synth.c
# End Source File
# Begin Source File

SOURCE=.\timer.c
# End Source File
# Begin Source File

SOURCE=.\version.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\bit.h
# End Source File
# Begin Source File

SOURCE=.\config.h
# End Source File
# Begin Source File

SOURCE=.\configwin32.h
# End Source File
# Begin Source File

SOURCE=.\decoder.h
# End Source File
# Begin Source File

SOURCE=.\fixed.h
# End Source File
# Begin Source File

SOURCE=.\frame.h
# End Source File
# Begin Source File

SOURCE=.\global.h
# End Source File
# Begin Source File

SOURCE=.\huffman.h
# End Source File
# Begin Source File

SOURCE=.\layer12.h
# End Source File
# Begin Source File

SOURCE=.\layer3.h
# End Source File
# Begin Source File

SOURCE=.\stream.h
# End Source File
# Begin Source File

SOURCE=.\synth.h
# End Source File
# Begin Source File

SOURCE=.\timer.h
# End Source File
# Begin Source File

SOURCE=.\version.h
# End Source File
# End Group
# End Target
# End Project
