#define MyAppName "ACBrMonitorPLUS"
#define MyAppPublisher "Projeto ACBr"
#define MyAppURL "https://www.projetoacbr.com.br/acbr-monitor-plus/"
#define MyAppUrlName "ACBrMonitor.url"
#define ACBrDIR "..\..\.."
;GetEnv("ACBR_HOME")

#define ACBrMonitorPLUSDir ACBrDIR + "\Projetos\ACBrMonitorPLUS\Lazarus"
#ifNDef OutputDir
  #define OutputDir ACBrMonitorPLUSDir
;  #define CHMFile  OutputDir + "\Output\chm\ACBrMonitor.chm"
;  #define PDFFile  OutputDir + "\Output\pdf\ACBrMonitor.pdf"
#else
;  #define CHMFile  OutputDir + "\ACBrMonitor.chm"
;  #define PDFFile  OutputDir + "\ACBrMonitor.pdf"
#endif

; para teste de compilação em 64 bits, descomente a linha abaixo
;#define MyAppTarget "x64"

#IfNDef MyAppTarget
  #define MyAppTarget "x86"
#endif

#if MyAppTarget != "x86"
  #define MyAppTarget "x64"
  #define App64bits
#endif

#pragma warning "ACBrDIR: " + ACBrDIR
#pragma warning "ACBrMonitorPLUSDir: " + ACBrMonitorPLUSDir
#pragma warning "OutputDir: " + OutputDir
#pragma warning "MyAppTarget: " + MyAppTarget

#ifDef App64bits
  #define MyAppExeName "ACBrMonitor64.exe"
  #define ECFTesteExeName "ECFTeste64.exe"
#else
  #define MyAppExeName "ACBrMonitor.exe"
  #define ECFTesteExeName "ECFTeste.exe"
#endif

#define MyAppVersion GetFileVersion(MyAppExeName)

#define MyAppVerName MyAppName + "-" + MyAppVersion + "-" + MyAppTarget
#define OpenSSLDir ACBrDIR + "\DLLs\OpenSSL\1.1.1.10\" + MyAppTarget
#define LibXML2Dir ACBrDIR + "\DLLs\LibXml2\" + MyAppTarget
#define VCRedistInstaller "VC_redist." + MyAppTarget + ".exe"

[Setup]
#ifDef App64bits
  ArchitecturesInstallIn64BitMode=x64
  ArchitecturesAllowed=x64
#endif
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
AppContact={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={sd}\{#MyAppName}
DefaultGroupName={#MyAppName}
LicenseFile=LICENCA.TXT
OutputBaseFilename={#MyAppVerName}-Instalador
Compression=lzma
SolidCompression=yes
AppMuTex=ACBrMonitor
DisableDirPage=no

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
Source: {#ACBrMonitorPLUSDir}\banner_acbrmonitor.gif; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: {#ACBrDIR}\Exemplos\ACBrSerial\ACBrECF\Lazarus\{#ECFTesteExeName}; DestDir: {app}; Flags: ignoreversion sign; Components: programa
Source: {#ACBrMonitorPLUSDir}\PRICETAB.TXT; DestDir: {app}; Flags: onlyifdoesntexist; Components: programa
;Arquivos com URLs
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBrNFe\ACBrNFeServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBrCTe\ACBrCTeServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBrMDFe\ACBrMDFeServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBreSocial\ACBreSocialServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBrReinf\ACBrReinfServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBrBPe\ACBrBPeServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBrGNRE\ACBrGNREServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBrNFSeX\ACBrNFSeXServicos.ini; DestDir: {app}; Flags: ignoreversion; Components: programa
;Logotipos de Bancos
Source: {#ACBrDIR}\Fontes\ACBrBoleto\Logos\Colorido\png\*.*; DestDir: {app}\Logos; Flags: ; Components: programa
;Licença de uso
Source: {#ACBrMonitorPLUSDir}\LICENCA.TXT; DestDir: {app}; Flags: ignoreversion; Components: programa
Source: {#ACBrMonitorPLUSDir}\LICENSE.TXT; DestDir: {app}; Flags: ignoreversion; Components: programa

;Schemas da NFe
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\CTe\*.*; DestDir: {app}\Schemas\CTe; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\MDFe\*.*; DestDir: {app}\Schemas\MDFe; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\NFe\*.*; DestDir: {app}\Schemas\NFe; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\eSocial\*.*; DestDir: {app}\Schemas\eSocial; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\Reinf\*.*; DestDir: {app}\Schemas\Reinf; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\BPe\*.*; DestDir: {app}\Schemas\BPe; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\GNRe\*.*; DestDir: {app}\Schemas\GNRe; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\GTIN\*.*; DestDir: {app}\Schemas\GTIN; Components: programa;
Source: {#ACBrDIR}\Exemplos\ACBrDFe\Schemas\NFSe\*; DestDir: {app}\Schemas\NFSe; Components: programa; Flags: recursesubdirs 



;Arquivos de Change-log.TXT
;Source: {#ACBrMonitorPLUSDir}\Notas_Lancamento.pdf; DestDir: {app}; Flags: ignoreversion; Components: help
Source: {#ACBrMonitorPLUSDir}\ACBrMonitor-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBrNFe\ACBrNFe-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\Fontes\ACBrDFe\ACBrDFe-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\Fontes\ACBrSAT\ACBrSAT-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\Fontes\ACBrSerial\ACBrSerial-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\Fontes\PCNComum\PCNComum-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\Fontes\ACBrComum\ACBrComum-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\Fontes\ACBrTCP\ACBrTCP-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\Fontes\ACBrBoleto\ACBrBoleto-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help
Source: {#ACBrDIR}\Fontes\ACBrDiversos\ACBrDiversos-change-log.txt; DestDir: {app}\ChangeLog; Flags: ignoreversion; Components: help

;Arquivo de leitura de CHM para exibição diretamente usando a tecla F1
;Source: {#ACBrMonitorPLUSDir}\lhelp\lhelp.exe; DestDir: {app}\lhelp; Flags: ignoreversion sign; Components: help
;Source: {#CHMFile}; DestDir: {app}; Flags: ignoreversion; Components: help
;Source: {#PDFFile}; DestDir: {app}; Flags: ignoreversion; Components: help

;Exemplos
Source: {#ACBrMonitorPLUSDir}\Exemplos\TesteTXT.BAT; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: {#ACBrMonitorPLUSDir}\Exemplos\Clipper_TXT_xHarbour_Socket.zip; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: {#ACBrMonitorPLUSDir}\Exemplos\Cobol_TXT_Socket.zip; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: {#ACBrMonitorPLUSDir}\Exemplos\Java_socket.txt; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: {#ACBrMonitorPLUSDir}\Exemplos\Oracle.txt; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos
Source: {#ACBrMonitorPLUSDir}\Exemplos\php_socket.zip; DestDir: {app}\Exemplos; Flags: ignoreversion; Components: exemplos

;Arquivo com Códigos e Cidades IBGE
Source: {#ACBrMonitorPLUSDir}\ACBrIBGE.txt; DestDir: {app}; Flags: ignoreversion; Components: programa

;DLL para acesso direto a portas
#ifNDef App64bits
  Source: {#ACBrDIR}\DLLs\Diversos\x86\inpout32.dll; DestDir: {app}; Flags: ignoreversion ; Components: programa
#else
  Source: {#ACBrDIR}\DLLs\Diversos\x64\inpoutx64.dll; DestDir: {app}; Flags: ignoreversion ; Components: programa
#endif

;Visual C++ 2010 RunTime
Source: {#ACBrDIR}\DLLs\Diversos\{#MyAppTarget}\{#VCRedistInstaller}; DestDir: {tmp}; Flags: deleteafterinstall
Source: {#ACBrDIR}\DLLs\Diversos\{#MyAppTarget}\msvcr120.dll; DestDir: {app}; Flags: ignoreversion ; Components: programa

;OpenSSL
;Source: {#OpenSSLDir}\openssl.exe; DestDir: {app}; Components: programa; Flags: ignoreversion ;
Source: {#OpenSSLDir}\libcrypto-1_1*.dll; DestDir: {app}; Components: programa; Flags: ignoreversion ;
Source: {#OpenSSLDir}\libssl-1_1*.dll; DestDir: {app}; Components: programa; Flags: ignoreversion ;

;LibXML2
Source: {#LibXML2Dir}\libexslt.dll; DestDir: {app}; Components: programa; Flags: ignoreversion ;
Source: {#LibXML2Dir}\libiconv.dll; DestDir: {app}; Components: programa; Flags: ignoreversion ;
Source: {#LibXML2Dir}\libxml2.dll; DestDir: {app}; Components: programa; Flags: ignoreversion ;
Source: {#LibXML2Dir}\libxslt.dll; DestDir: {app}; Components: programa; Flags: ignoreversion ;

#ifNDef App64bits
  ;DLLs CAPICOM  
  Source: {#ACBrDIR}\DLLs\Capicom\capicom.dll; DestDir: {syswow64}; Components: programa; Flags: regserver sharedfile
  Source: {#ACBrDIR}\DLLs\Capicom\msxml5.dll; DestDir: {syswow64}; Components: programa; Flags: regserver sharedfile
  Source: {#ACBrDIR}\DLLs\Capicom\msxml5r.dll; DestDir: {syswow64}; Components: programa; Flags: sharedfile

#endif
;DLLs de Fabricantes SAT
  Source: {#ACBrMonitorPLUSDir}\SAT\*.*; DestDir: {app}\SAT; Flags: recursesubdirs; Components: programa;

[INI]
Filename: {app}\{#MyAppUrlName}; Section: InternetShortcut; Key: URL; String: {#MyAppURL}; Components: help

[Icons]
Name: {group}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; Components: programa
Name: {group}\Programa ECFTeste; Filename: {app}\{#ECFTesteExeName}; WorkingDir: {app}; Components: programa
;Name: {group}\Manual do ACBrMonitor; Filename: {app}\ACBrMonitor.chm; WorkingDir: {app}; Components: help
Name: {group}\Pasta Exemplos; Filename: {app}\Exemplos\; Components: exemplos
Name: {userdesktop}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}; Tasks: desktopicon
Name: {userstartup}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}
Name: {group}\{cm:ProgramOnTheWeb,{#MyAppName}}; Filename: {app}\{#MyAppUrlName}; Components: help

[Run]
Filename: {tmp}\{#VCRedistInstaller}; Parameters: "/install /passive /norestart"; StatusMsg: Installing Visual C++ 2019 RunTime...
    
Filename: "{app}\{#MyAppExeName}"; Flags: nowait postinstall skipifsilent; Description: "{cm:LaunchProgram,{#MyAppName}}"
;Filename: "{app}\ACBrMonitor.chm"; Flags: postinstall shellexec skipifsilent; Description: "Manual ACBrMonitor"; Components: help
Filename: "{sys}\netsh.exe"; Parameters: "advfirewall firewall add rule name=""{#MyAppName}"" dir=in action=allow protocol=TCP localport=3434"; Flags: skipifdoesntexist runhidden; MinVersion: 0,6.0; Tasks: firewallopen

[UninstallRun]
Filename: "{sys}\netsh.exe"; Parameters: "advfirewall firewall delete rule name=""{#MyAppName}"""; Flags: skipifdoesntexist runhidden; MinVersion: 0,6.0; Tasks: firewallopen

