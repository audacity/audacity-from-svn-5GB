# Microsoft Developer Studio Project File - Name="libflac_cpp" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=libflac_cpp - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libflac_cpp.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libflac_cpp.mak" CFG="libflac_cpp - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libflac_cpp - Win32 Release" (based on "Win32 (x86) Static Library")
!MESSAGE "libflac_cpp - Win32 Debug" (based on "Win32 (x86) Static Library")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libflac_cpp - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "."
# PROP Intermediate_Dir "."
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\..\obj\lib\libflac++.lib"

!ELSEIF  "$(CFG)" == "libflac_cpp - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "."
# PROP Intermediate_Dir "."
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo /out:"..\..\..\obj\lib\libflac++d.lib"

!ENDIF 

# Begin Target

# Name "libflac_cpp - Win32 Release"
# Name "libflac_cpp - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;cc;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=..\file_decoder.cc

!IF  "$(CFG)" == "libflac_cpp - Win32 Release"

# Begin Custom Build
IntDir=.\.
InputPath=..\file_decoder.cc
InputName=file_decoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MT /W3 /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /TP $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "libflac_cpp - Win32 Debug"

# Begin Custom Build
IntDir=.\.
InputPath=..\file_decoder.cc
InputName=file_decoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c /TP $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\file_encoder.cc

!IF  "$(CFG)" == "libflac_cpp - Win32 Release"

# Begin Custom Build
IntDir=.\.
InputPath=..\file_encoder.cc
InputName=file_encoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MT /W3 /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /TP $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "libflac_cpp - Win32 Debug"

# Begin Custom Build
IntDir=.\.
InputPath=..\file_encoder.cc
InputName=file_encoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c /TP $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\metadata.cc

!IF  "$(CFG)" == "libflac_cpp - Win32 Release"

# Begin Custom Build
IntDir=.\.
InputPath=..\metadata.cc
InputName=metadata

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MT /W3 /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /TP $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "libflac_cpp - Win32 Debug"

# Begin Custom Build
IntDir=.\.
InputPath=..\metadata.cc
InputName=metadata

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c /TP $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\seekable_stream_decoder.cc

!IF  "$(CFG)" == "libflac_cpp - Win32 Release"

# Begin Custom Build
IntDir=.\.
InputPath=..\seekable_stream_decoder.cc
InputName=seekable_stream_decoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MT /W3 /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /TP $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "libflac_cpp - Win32 Debug"

# Begin Custom Build
IntDir=.\.
InputPath=..\seekable_stream_decoder.cc
InputName=seekable_stream_decoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c /TP $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\seekable_stream_encoder.cc

!IF  "$(CFG)" == "libflac_cpp - Win32 Release"

# Begin Custom Build
IntDir=.\.
InputPath=..\seekable_stream_encoder.cc
InputName=seekable_stream_encoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MT /W3 /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /TP $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "libflac_cpp - Win32 Debug"

# Begin Custom Build
IntDir=.\.
InputPath=..\seekable_stream_encoder.cc
InputName=seekable_stream_encoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c /TP $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\stream_decoder.cc

!IF  "$(CFG)" == "libflac_cpp - Win32 Release"

# Begin Custom Build
IntDir=.\.
InputPath=..\stream_decoder.cc
InputName=stream_decoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MT /W3 /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /TP $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "libflac_cpp - Win32 Debug"

# Begin Custom Build
IntDir=.\.
InputPath=..\stream_decoder.cc
InputName=stream_decoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c /TP $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\stream_encoder.cc

!IF  "$(CFG)" == "libflac_cpp - Win32 Release"

# Begin Custom Build
IntDir=.\.
InputPath=..\stream_encoder.cc
InputName=stream_encoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MT /W3 /GX /O2 /I "..\..\..\include" /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c /TP $(InputPath)

# End Custom Build

!ELSEIF  "$(CFG)" == "libflac_cpp - Win32 Debug"

# Begin Custom Build
IntDir=.\.
InputPath=..\stream_encoder.cc
InputName=stream_encoder

"$(IntDir)\$(InputName).obj" : $(SOURCE) "$(INTDIR)" "$(OUTDIR)"
	cl /nologo /MTd /W3 /Gm /GX /ZI /Od /I "..\..\..\include" /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c /TP $(InputPath)

# End Custom Build

!ENDIF 

# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# End Group
# End Target
# End Project
