@echo off

if "%1"=="check" GOTO CHECK

copy /y Win32\sndfile.h src\sndfile.h
nmake -f Win32\Makefile.msvc
goto END


:CHECK
nmake -f Win32\Makefile.msvc check
goto END

:END
