@echo off

if "%1"=="check" GOTO CHECK

copy /y Win32\config.h src\config.h
copy /y Win32\unistd.h examples\unistd.h

nmake -f Win32\Makefile.msvc
goto END


:CHECK
nmake -f Win32\Makefile.msvc check
goto END

:END
