# Microsoft Developer Studio Project File - Name="libflac" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libflac - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libflac.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libflac.mak" CFG="libflac - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libflac - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libflac - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libflac - Win32 Release"

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
# ADD CPP /nologo /MT /W3 /GX /O2 /I "..\include" /I "..\..\..\include" /D "NDEBUG" /D "WIN32" /D "_MBCS" /D "_LIB" /D VERSION=\"1.0.4\" /D "FLAC__CPU_IA32" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\..\obj\lib\libFLAC.lib"

!ELSEIF  "$(CFG)" == "libflac - Win32 Debug"

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
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\include" /I "..\..\..\include" /D "_DEBUG" /D "FLAC__OVERFLOW_DETECT" /D "WIN32" /D "_MBCS" /D "_LIB" /D VERSION=\"1.0.4\" /D "FLAC__CPU_IA32" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\..\obj\lib\libFLACd.lib"

!ENDIF 

# Begin Target

# Name "libflac - Win32 Release"
# Name "libflac - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\bitbuffer.c
# End Source File
# Begin Source File

SOURCE=..\bitmath.c
# End Source File
# Begin Source File

SOURCE=..\cpu.c
# End Source File
# Begin Source File

SOURCE=..\ia32\cpu_asm.nasm

!IF  "$(CFG)" == "libflac - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "libflac - Win32 Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\crc.c
# End Source File
# Begin Source File

SOURCE=..\file_decoder.c
# End Source File
# Begin Source File

SOURCE=..\file_encoder.c
# End Source File
# Begin Source File

SOURCE=..\fixed.c
# End Source File
# Begin Source File

SOURCE=..\ia32\fixed_asm.nasm

!IF  "$(CFG)" == "libflac - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "libflac - Win32 Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\format.c
# End Source File
# Begin Source File

SOURCE=..\lpc.c
# End Source File
# Begin Source File

SOURCE=..\ia32\lpc_asm.nasm

!IF  "$(CFG)" == "libflac - Win32 Release"

# PROP Exclude_From_Build 1

!ELSEIF  "$(CFG)" == "libflac - Win32 Debug"

# PROP Exclude_From_Build 1

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\md5.c
# End Source File
# Begin Source File

SOURCE=..\memory.c
# End Source File
# Begin Source File

SOURCE=..\metadata_iterators.c
# End Source File
# Begin Source File

SOURCE=..\metadata_object.c
# End Source File
# Begin Source File

SOURCE=..\seekable_stream_decoder.c
# End Source File
# Begin Source File

SOURCE=..\seekable_stream_encoder.c
# End Source File
# Begin Source File

SOURCE=..\stream_decoder.c
# End Source File
# Begin Source File

SOURCE=..\stream_encoder.c
# End Source File
# Begin Source File

SOURCE=..\stream_encoder_framing.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
