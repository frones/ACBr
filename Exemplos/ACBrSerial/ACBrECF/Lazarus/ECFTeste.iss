; ler a versão do ACBrMonitor do arquivo versao.txt
#define FileHandle = FileOpen(AddBackslash(SourcePath) + "versao.txt")
#define FileLine = FileRead(FileHandle)
#define FileHandle FileClose(FileHandle)
#define MyAppVersion Str( FileLine )
#define MyAppVersion copy( MyAppVersion, pos("'", MyAppVersion) + 1, pos(";", MyAppVersion) )
#define MyAppVersion Copy( MyAppVersion, 0, pos("'", MyAppVersion) - 1 )

#define MyAppName "ECFTeste"
#define MyAppVerName MyAppName + '-' + MyAppVersion
#define MyAppPublisher "Projeto ACBr"
#define MyAppURL "https://www.projetoacbr.com.br/forum/files/file/65-ecfteste/"
#define MyAppUrlName "ACBrMonitor.url"
#define MyAppExeName "ECFTeste.exe"
#define ACBrDIR "..\..\..\..\"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={sd}\{#MyAppName}
DefaultGroupName={#MyAppName}
LicenseFile={#ACBrDIR}\Projetos\ACBrMonitorPLUS\Lazarus\LICENCA.TXT
OutputBaseFilename={#MyAppVerName}-Windows-Instalador
Compression=lzma
SolidCompression=yes
AppMuTex=ECFTeste
DisableDirPage=no


[Types]
Name: full; Description: Instalação Completa;
Name: compact; Description: Instalação Mínima
Name: custom; Description: Instalação Customizada; Flags: iscustom

[Components]
Name: programa; Description: Programa ECFTeste; Types: full compact custom; Flags: fixed

[Languages]
Name: brazilianportuguese; MessagesFile: compiler:Languages\BrazilianPortuguese.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: firewallopen; Description: "Liberar {#MyAppName}, porta 3434, no Firewall do Windows"; GroupDescription: "Firewall:"

[Files]
; Aplicação
Source: {#MyAppExeName}; DestDir: {app}; Flags: ignoreversion sign; Components: programa

;DLL para acesso direto a portas
Source: {#ACBrDIR}\DLLs\Diversos\inpout32.dll; DestDir: {syswow64}; Flags: ; Components: programa
;OpenSSL
Source: {#ACBrDIR}\DLLs\OpenSSL\0.9.8.14\openssl.exe; DestDir: {app}; Flags: ; Components: programa
Source: {#ACBrDIR}\DLLs\Diversos\msvcr71.dll; DestDir: {app}; Components: programa;  Flags: sharedfile 
;DLLs CAPICOM
Source: {#ACBrDIR}\DLLs\Capicom\capicom.dll; DestDir: {syswow64}; Components: programa; Flags: regserver sharedfile
Source: {#ACBrDIR}\DLLs\Capicom\msxml5.dll; DestDir: {syswow64}; Components: programa; Flags: regserver sharedfile
Source: {#ACBrDIR}\DLLs\Capicom\msxml5r.dll; DestDir: {syswow64}; Components: programa; Flags: sharedfile
;DLLs XMLSec-MINGW
Source: {#ACBrDIR}\DLLs\XMLSec\MinGW\32\*.dll; DestDir: {app}; Flags: ; Components: programa
;Schemas da NFe
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\NFe\*.*; DestDir: {app}\Schemas; Components: programa;
;DLLs de Fabricantes SAT
Source: {#ACBrDIR}\Projetos\ACBrMonitorPLUS\Lazarus\SAT\*.*; DestDir: {app}\SAT; Flags: recursesubdirs; Components: programa;

[INI]
Filename: {app}\{#MyAppUrlName}; Section: InternetShortcut; Key: URL; String: {#MyAppURL}; Components: programa

[Icons]
Name: {group}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; Components: programa
Name: {userdesktop}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; Tasks: desktopicon
Name: {userstartup}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}

[Run]
Filename: "{app}\{#MyAppExeName}"; Flags: nowait postinstall skipifsilent; Description: "{cm:LaunchProgram,{#MyAppName}}"