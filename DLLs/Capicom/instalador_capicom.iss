#define MyAppName "Projeto ACBR - CAPICOM"
#define MyAppVersion "1.0"
#define MyAppPublisher "Projeto ACBR."
#define MyAppURL "http://acbr.sourceforge.net/drupal/"

[Setup]
AppID={{9E210EC6-AB32-4524-8303-E737B91A0408}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
CreateAppDir=false
OutputDir=output
OutputBaseFilename=capicom
Compression=lzma/Max
SolidCompression=false
Uninstallable=false

[Languages]
Name: "brazilianportuguese"; MessagesFile: "compiler:Languages\BrazilianPortuguese.isl"

[Files]
Source: capicom.dll; DestDir: {sys}; Flags: regserver; 
Source: msxml5.dll; DestDir: {sys}; Flags: regserver;
Source: msxml5r.dll; DestDir: {sys};  
Source: ..\OpenSSL\libeay32.dll; DestDir: {sys}; Flags: onlyifdoesntexist; 
Source: ..\OpenSSL\ssleay32.dll; DestDir: {sys}; Flags: onlyifdoesntexist; 
