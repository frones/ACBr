; ler a versão do ACBrMonitor do arquivo versao.txt
#define FileHandle = FileOpen(AddBackslash(SourcePath) + "versao.txt")
#define FileLine = FileRead(FileHandle)
#define FileHandle FileClose(FileHandle)
#define MyAppVersion Str( FileLine )
#define MyAppVersion copy( MyAppVersion, pos("'", MyAppVersion) + 1, pos(";", MyAppVersion) )
#define MyAppVersion Copy( MyAppVersion, 0, pos("'", MyAppVersion) - 1 )

#define MyAppName "ACBrMonitor"
#define MyAppVerName MyAppName + '-' + MyAppVersion
#define MyAppPublisher "Projeto ACBr"
#define MyAppURL "http://acbr.sourceforge.net/ACBrMonitor.htm"
#define MyAppUrlName "ACBrMonitor.url"
#define MyAppExeName "ACBrMonitor.exe"
#define ACBrDIR "..\..\..\"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={sd}\{#MyAppName}
DefaultGroupName={#MyAppName}
LicenseFile=LICENCA.TXT
OutputBaseFilename={#MyAppVerName}-Windows-Instalador
Compression=lzma
SolidCompression=yes
AppMuTex=ACBrMonitor

[Types]
Name: compact; Description: Instalação Mínima
Name: full; Description: Instalação Completa
Name: custom; Description: Instalação Customizada; Flags: iscustom

[Components]
Name: programa; Description: Programa ACBrMonitor; Types: full compact custom; Flags: fixed
Name: console; Description: ACBrMonitor modo Console; Types: full custom
Name: help; Description: Arquivos de Ajuda; Types: full custom
Name: exemplos; Description: Exemplos de Uso; Types: full custom

[Languages]
Name: brazilianportuguese; MessagesFile: compiler:Languages\BrazilianPortuguese.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: ACBrMonitor.exe; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: banner_acbrmonitor.gif; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: ACBrMonitorConsole.exe; DestDir: {app}; Flags: ignoreversion; Components: console
Source: {#ACBrDIR}\Exemplos\ACBrECF\Lazarus\ECFTeste.exe; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: PRICETAB.TXT; DestDir: {app}; Flags: onlyifdoesntexist; Components: programa
Source: ..\Exemplos\TesteTXT.BAT; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: ..\Exemplos\Clipper_TXT_xHarbour_Socket.zip; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: ..\Exemplos\Cobol_TXT_Socket.zip; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: ..\Exemplos\Java_socket.txt; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: ..\Exemplos\Oracle.txt; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: ..\Exemplos\php_socket.zip; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: LICENCA.TXT; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: LICENSE.TXT; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: ACBrMonitor.chm; DestDir: {app}; Flags: ignoreversion; Components: help
Source: ACBrMonitor.pdf; DestDir: {app}; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\DLLs\Diversos\inpout32.dll; DestDir: {sys}; Flags: ; Components: programa
Source: {#ACBrDIR}\DLLs\OpenSSL\openssl.exe; DestDir: {app}; Flags: ; Components: programa
Source: {#ACBrDIR}\DLLs\OpenSSL\libeay32.dll; DestDir: {app}; Flags: ; Components: programa
Source: {#ACBrDIR}\DLLs\OpenSSL\ssleay32.dll; DestDir: {app}; Flags: ; Components: programa
Source: {#ACBrDIR}\DLLs\MSVCR\msvcr71.dll; DestDir: {app}; Flags: ; Components: programa
Source: {#ACBrDIR}\Fontes\ACBrBoleto\Logos\Colorido\*.*; DestDir: {app}\Logos; Flags: ; Components: programa




[INI]
Filename: {app}\{#MyAppUrlName}; Section: InternetShortcut; Key: URL; String: {#MyAppURL}; Components: help

[Icons]
Name: {group}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; Components: programa
Name: {group}\Programa para Configurar e Testar ECF; Filename: {app}\ECFTeste.exe; WorkingDir: {app}; Components: programa
Name: {group}\Manual do ACBrMonitor; Filename: {app}\ACBrMonitor.chm; WorkingDir: {app}; Components: help
Name: {group}\Pasta Exemplos; Filename: {app}\Exemplos\; Components: exemplos
Name: {userdesktop}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; Tasks: desktopicon
Name: {userstartup}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}
Name: {group}\{cm:ProgramOnTheWeb,{#MyAppName}}; Filename: {app}\{#MyAppUrlName}; Components: help

[Run]
Filename: {app}\{#MyAppExeName}; Description: {cm:LaunchProgram,{#MyAppName}}; Flags: nowait postinstall skipifsilent
Filename: {app}\ACBrMonitor.chm; Description: Novidades desta Versão; Flags: postinstall shellexec skipifsilent; Components: help

