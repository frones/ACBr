@echo off
if %1. == . goto MissingParameter
if %1. == debug.   goto GoodParameter
if %1. == release. goto GoodParameter

echo You must send "debug" or "release" as parameter
goto End

:GoodParameter

set PATH=E:\Programacao\xHabour\harbour\bin;%PATH%
set HB_COMPILER=mingw
::set HB_COMPILER=msvc
set HB_PATH=E:\Programacao\xHabour\harbour

E:
md "E:\Programacao\ACBr\ACBr\Projetos\ACBrLib\Demos\Sat\Harbour\%1\"
cd "E:\Programacao\ACBr\ACBr\Projetos\ACBrLib\Demos\Sat\Harbour\%1\"

if %1 == debug (
		hbmk2 E:\Programacao\ACBr\ACBr\Projetos\ACBrLib\Demos\Sat\Harbour\ACBrSat.hbp -b
) else (
		hbmk2 E:\Programacao\ACBr\ACBr\Projetos\ACBrLib\Demos\Sat\Harbour\ACBrSat.hbp
)

goto End
:MissingParameter
echo Missing Parameter
:End