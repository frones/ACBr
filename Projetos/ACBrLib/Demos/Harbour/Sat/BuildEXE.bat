@echo off
if %1. == . goto MissingParameter
if %2. == . goto MissingParameter
if %2. == debug.   goto GoodParameter
if %2. == release. goto GoodParameter

echo You must send "debug" or "release" as parameter
goto End

:GoodParameter

set PATH=E:\Programacao\xHabour\harbour\bin;%PATH%
set HB_COMPILER=mingw
::set HB_COMPILER=msvc
set HB_PATH=E:\Programacao\xHabour\harbour

E:
md "%1\%2\"
cd "%1\%2\"

if %1 == debug (
		hbmk2 %1\ACBrSat.hbp -b
) else (
		hbmk2 %1\ACBrSat.hbp
)

goto End
:MissingParameter
echo Missing Parameter
:End