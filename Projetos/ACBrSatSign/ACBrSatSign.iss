; ler a versão do ACBrMonitor do arquivo versao.txt
#define FileHandle = FileOpen(AddBackslash(SourcePath) + "versao.txt")
#define FileLine = FileRead(FileHandle)
#define FileHandle FileClose(FileHandle)
#define MyAppVersion Str( FileLine )
#define MyAppVersion copy( MyAppVersion, pos("'", MyAppVersion) + 1, pos(";", MyAppVersion) )
#define MyAppVersion Copy( MyAppVersion, 0, pos("'", MyAppVersion) - 1 )

#define MyAppName "ACBrSatSign"
#define MyAppVerName MyAppName + '-' + MyAppVersion
#define MyAppPublisher "Projeto ACBr"
#define MyAppURL "https://www.projetoacbr.com.br/forum/files/file/387-acbrsatsign/"
#define MyAppUrlName "ACBrSatSign.url"
#define MyAppExeName "ACBrSatSign.exe"
#define ACBrDIR "..\..\"

[Setup]
AppName={#MyAppName}
AppVersion={#MyAppVerName}
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
AppMuTex=ACBrSatSign
DisableDirPage=no
AlwaysShowComponentsList=False
ShowComponentSizes=False

[Languages]
Name: brazilianportuguese; MessagesFile: compiler:Languages\BrazilianPortuguese.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
; Aplicação
Source: {#MyAppExeName}; DestDir: {app}; Flags: ignoreversion sign;

;DLL para acesso direto a portas
Source: {#ACBrDIR}\DLLs\Diversos\inpout32.dll; DestDir: {syswow64}; Flags: ; 
;OpenSSL
Source: {#ACBrDIR}\DLLs\OpenSSL\0.9.8.14\openssl.exe; DestDir: {app}; Flags: ;
Source: {#ACBrDIR}\DLLs\Diversos\msvcr71.dll; DestDir: {app}; Flags: sharedfile 
;DLLs XMLSec-MINGW
Source: {#ACBrDIR}\DLLs\XMLSec\MinGW\32\*.dll; DestDir: {app}; Flags: ; 

[INI]
Filename: {app}\{#MyAppUrlName}; Section: InternetShortcut; Key: URL; String: {#MyAppURL};

[Icons]
Name: {group}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; 
Name: {userdesktop}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; Tasks: desktopicon
Name: {userstartup}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}

[Run]
Filename: "{app}\{#MyAppExeName}"; Flags: nowait postinstall skipifsilent; Description: "{cm:LaunchProgram,{#MyAppName}}"