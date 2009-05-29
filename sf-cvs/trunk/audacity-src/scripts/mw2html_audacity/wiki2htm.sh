#!/bin/sh

helpdir="../../help/manual"
tmpdir_="../../help/temp"
srcuri="http://audacityteam.org/manual"

mkdir -p "${tmpdir_}"
python mw2html.py "${srcuri}" "${tmpdir_}" -s
mv "${tmpdir_}/audacityteam.org" "${helpdir}"
\rm -r "${tmpdir_}"
