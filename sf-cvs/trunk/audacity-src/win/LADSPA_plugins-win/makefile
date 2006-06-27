#
# Builds the real makefile
#

all: ..\*.c
   @echo off
   -rename ..\config.h configold.h
   cd "$(OUTDIR)"
   echo >unistd.h /* dummy file */
   echo !include ..\makefile.inc >submake
   echo dlls: $(?F:.c=.dll) >>submake
   echo objs: $(?F:.c=.obj) >>submake
   for %%a in ($(?B)) do @echo %%a.obj: ..\..\%%a.c >>submake
   $(MAKE) CONFIG=$(CONFIG) submake
   cd ..
   -rename ..\configold.h config.h

clean:
   @echo off
   cd "$(OUTDIR)"
   -del *.dll *.exp *.lib *.obj *.pdb *.ilk unistd.h submake
   cd ..