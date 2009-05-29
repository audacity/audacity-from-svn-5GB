python mw2html.py http://audacityteam.org/manual ..\..\help\temp -s
rmdir /S /Q ..\..\help\manual
mkdir ..\..\help\manual
move ..\..\help\temp\audacityteam.org ..\..\help\manual
rmdir /S /Q ..\..\help\temp

