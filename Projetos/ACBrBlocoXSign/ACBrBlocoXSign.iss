#define MyAppName "ACBrBlocoXSign"
#define MyAppVerName MyAppName
#define MyAppPublisher "Projeto ACBr"
#define MyAppURL "http://www.projetoacbr.com.br/"
#define MyAppUrlName "ACBrBlocoXSign.url"
#define MyAppExeName "ACBrBlocoXSign.exe"
#define ACBrDIR "..\..\"

[Setup]
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={sd}\{#MyAppName}
DefaultGroupName={#MyAppName}
OutputBaseFilename={#MyAppVerName}-Windows-Instalador
Compression=lzma
SolidCompression=yes
AppMuTex=ACBrBlocoXSign

[Types]
Name: full; Description: Instalação Completa;

[Components]
Name: programa; Description: Programa ACBrBlocoXSign; Types: full; Flags: fixed

[Languages]
Name: brazilianportuguese; MessagesFile: compiler:Languages\BrazilianPortuguese.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: firewallopen; Description: "Liberar {#MyAppName}, porta 3434, no Firewall do Windows"; GroupDescription: "Firewall:"

[Files]
; Aplicação
Source: {#MyAppExeName}; DestDir: {app}; Flags: ignoreversion; Components: programa

;Diversos
Source: {#ACBrDIR}\DLLs\Diversos\msvcr71.dll; DestDir: {app}; Components: programa;  Flags: sharedfile 

;DLLs CAPICOM
Source: {#ACBrDIR}\DLLs\Capicom\capicom.dll; DestDir: {syswow64}; Components: programa; Flags: regserver sharedfile
Source: {#ACBrDIR}\DLLs\Capicom\msxml5.dll; DestDir: {syswow64}; Components: programa; Flags: regserver sharedfile
Source: {#ACBrDIR}\DLLs\Capicom\msxml5r.dll; DestDir: {syswow64}; Components: programa; Flags: sharedfile

;DLLs XMLSec
Source: {#ACBrDIR}\DLLs\XMLSec\MinGW\32\*.dll; DestDir: {app}; Flags: ; Components: programa

[Icons]
Name: {group}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; Components: programa
Name: {userdesktop}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; Tasks: desktopicon
Name: {userstartup}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}

[Run]
Filename: "{app}\{#MyAppExeName}"; Flags: nowait postinstall skipifsilent; Description: "{cm:LaunchProgram,{#MyAppName}}"

[UninstallRun]
Filename: "{sys}\netsh.exe"; Parameters: "advfirewall firewall delete rule name=""{#MyAppName}"""; Flags: skipifdoesntexist runhidden; MinVersion: 0,6.0; Tasks: firewallopen
