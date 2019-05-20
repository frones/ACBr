; ler a versão do ACBrMonitor do arquivo versao.txt
#define FileHandle = FileOpen(AddBackslash(SourcePath) + "versao.txt")
#define FileLine = FileRead(FileHandle)
#define FileHandle FileClose(FileHandle)
#define MyAppVersion Str( FileLine )
#define MyAppVersion copy( MyAppVersion, pos("'", MyAppVersion) + 1, pos(";", MyAppVersion) )
#define MyAppVersion Copy( MyAppVersion, 0, pos("'", MyAppVersion) - 1 )

#define MyAppName "ACBrMonitorPLUS"
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
DisableDirPage=no
; Comente a linha abaixo caso não deseje gerar instalador assinado.
SignTool=SignTool /d $qACBrMonitorPLUS - SAC ACBr$q /du $qhttp://www.projetoacbr.com.br$q $f

[Types]
Name: full; Description: Instalação Completa;
Name: compact; Description: Instalação Mínima
Name: custom; Description: Instalação Customizada; Flags: iscustom

[Components]
Name: programa; Description: Programa ACBrMonitor; Types: full compact custom; Flags: fixed
;Name: console; Description: ACBrMonitor modo Console; Types: full custom
Name: help; Description: Arquivos de Ajuda; Types: full custom
Name: exemplos; Description: Exemplos de Uso; Types: full custom

[Languages]
Name: brazilianportuguese; MessagesFile: compiler:Languages\BrazilianPortuguese.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked
Name: firewallopen; Description: "Liberar {#MyAppName}, porta 3434, no Firewall do Windows"; GroupDescription: "Firewall:"

[Files]
; Aplicação
Source: {#MyAppExeName}; DestDir: {app}; Flags: ignoreversion sign; Components: programa
Source: banner_acbrmonitor.gif; DestDir: {app}; Flags: ignoreversion; Components: programa
;Source: ACBrMonitorConsole.exe; DestDir: {app}; Flags: ignoreversion; Components: console
Source: {#ACBrDIR}\Exemplos\ACBrSerial\ACBrECF\Lazarus\ECFTeste.exe; DestDir: {app}; Flags: ignoreversion sign; Components: programa
;Arquivos de configuração
Source: PRICETAB.TXT; DestDir: {app}; Flags: onlyifdoesntexist; Components: programa
Source: ..\..\..\Fontes\ACBrDFe\ACBrNFe\ACBrNFeServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: ..\..\..\Fontes\ACBrDFe\ACBrCTe\ACBrCTeServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: ..\..\..\Fontes\ACBrDFe\ACBrMDFe\ACBrMDFeServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: ..\..\..\Fontes\ACBrDFe\ACBreSocial\ACBreSocialServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: ..\..\..\Fontes\ACBrDFe\ACBrReinf\ACBrReinfServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: ..\..\..\Fontes\ACBrDFe\ACBrBPe\ACBrBPeServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
;Arquivos de Change-log.TXT
Source: ACBrMonitor-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: ..\..\..\Fontes\ACBrDFe\ACBrNFe\ACBrNFe-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: ..\..\..\Fontes\ACBrDFe\ACBrDFe-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: ..\..\..\Fontes\ACBrSAT\ACBrSAT-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: ..\..\..\Fontes\ACBrSerial\ACBrSerial-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: ..\..\..\Fontes\PCNComum\PCNComum-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: ..\..\..\Fontes\ACBrComum\ACBrComum-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: ..\..\..\Fontes\ACBrTCP\ACBrTCP-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: ..\..\..\Fontes\ACBrBoleto\ACBrBoleto-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: ..\..\..\Fontes\ACBrDiversos\ACBrDiversos-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: Notas_Lancamento.pdf; DestDir: {app}; Flags: ignoreversion; Components: help
;Arquivo de leitura de CHM para exibição diretamente usando a tecla F1
Source: lhelp\lhelp.exe; DestDir: {app}\lhelp; Flags: ignoreversion sign; Components: programa

;Exemplos
Source: Exemplos\TesteTXT.BAT; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: Exemplos\Clipper_TXT_xHarbour_Socket.zip; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: Exemplos\Cobol_TXT_Socket.zip; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: Exemplos\Java_socket.txt; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: Exemplos\Oracle.txt; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: Exemplos\php_socket.zip; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
;Logotipos de Bancos
Source: {#ACBrDIR}\Fontes\ACBrBoleto\Logos\Colorido\*.*; DestDir: {app}\Logos; Flags: ; Components: programa
;Licença de uso
Source: LICENCA.TXT; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: LICENSE.TXT; DestDir: {app}; Flags: ignoreversion; Components: programa
;Arquivos de Ajuda e documentação
Source: ACBrMonitor.chm; DestDir: {app}; Flags: ignoreversion; Components: help
;Source: ACBrMonitor.pdf; DestDir: {app}; Flags: ignoreversion; Components: help
;DLL para acesso direto a portas
Source: {#ACBrDIR}\DLLs\Diversos\inpout32.dll; DestDir: {syswow64}; Flags: ; Components: programa
;OpenSSL
Source: {#ACBrDIR}\DLLs\OpenSSL\0.9.8.14\openssl.exe; DestDir: {app}; Flags: ; Components: programa
;Source: {#ACBrDIR}\DLLs\OpenSSL\0.9.8.14\libeay32.dll; DestDir: {app}; Flags: ; Components: programa
;Source: {#ACBrDIR}\DLLs\OpenSSL\0.9.8.14\ssleay32.dll; DestDir: {app}; Flags: ; Components: programa
Source: {#ACBrDIR}\DLLs\Diversos\msvcr71.dll; DestDir: {app}; Components: programa;  Flags: sharedfile 
;DLLs CAPICOM
Source: {#ACBrDIR}\DLLs\Capicom\capicom.dll; DestDir: {syswow64}; Components: programa; Flags: regserver sharedfile
Source: {#ACBrDIR}\DLLs\Capicom\msxml5.dll; DestDir: {syswow64}; Components: programa; Flags: regserver sharedfile
Source: {#ACBrDIR}\DLLs\Capicom\msxml5r.dll; DestDir: {syswow64}; Components: programa; Flags: sharedfile
;DLLs XMLSec-MINGW
Source: {#ACBrDIR}\DLLs\XMLSec\MinGW\32\*.dll; DestDir: {app}; Flags: ; Components: programa
;Schemas da NFe
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\CTe\*.*; DestDir: {app}\Schemas; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\MDFe\*.*; DestDir: {app}\Schemas; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\NFe\*.*; DestDir: {app}\Schemas; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\eSocial\*.*; DestDir: {app}\Schemas; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\Reinf\*.*; DestDir: {app}\Schemas; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\BPe\*.*; DestDir: {app}\Schemas; Components: programa;
;Lista de municípios do IBGE
Source: MunIBGE\*.*; DestDir: {app}\MunIBGE; Flags: ; Components: programa
;DLLs de Fabricantes SAT
Source: SAT\*.*; DestDir: {app}\SAT; Flags: recursesubdirs; Components: programa;

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
Filename: "{app}\{#MyAppExeName}"; Flags: nowait postinstall skipifsilent; Description: "{cm:LaunchProgram,{#MyAppName}}"
Filename: "{app}\ACBrMonitor.chm"; Flags: postinstall shellexec skipifsilent; Description: "Manual ACBrMonitor"; Components: help
Filename: "{sys}\netsh.exe"; Parameters: "advfirewall firewall add rule name=""{#MyAppName}"" dir=in action=allow protocol=TCP localport=3434"; Flags: skipifdoesntexist runhidden; MinVersion: 0,6.0; Tasks: firewallopen

[UninstallRun]
Filename: "{sys}\netsh.exe"; Parameters: "advfirewall firewall delete rule name=""{#MyAppName}"""; Flags: skipifdoesntexist runhidden; MinVersion: 0,6.0; Tasks: firewallopen
