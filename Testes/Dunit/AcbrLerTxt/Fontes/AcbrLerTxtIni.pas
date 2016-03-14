unit AcbrLerTxtIni;


{$I ACBr.inc}
interface

uses
  IniPersist,SysUtils, StrUtils, Classes,  Contnrs,System.IOUtils;

Type
  TIniAcbrLerTxt = Class( Tobject )
  private
    FVersao       : Widestring;
    FStrId        : Widestring;
    fIntnNF       : Integer;
    FDecvUnTrib   : Integer;
    FDecqCom      : Integer;
    FDecvUnCom    : Integer;
    FDecvProd     : Integer;
    FDecqTrib     : Integer;
    FSettingsFile : String;
  Public
    [IniValue('CONFIGUR'      ,'Versao' ,'Beta 1.0')]
    Property Versao       : Widestring  read FVersao      write fVersao;
    [IniValue('CONFIGUR'  ,'StrId'    ,'')]
    Property StrId        : Widestring  read FStrId       write fStrId;
    [IniValue('CONFIGUR'  ,'IntnNF'    ,'6')]
    Property  IntnNF      : Integer     read fIntnNF      write fIntnNF;
    [IniValue('CONFIGUR'  ,'DecqCom'    ,'4')]
    Property DecqCom      : Integer     read FDecqCom     write FDecqCom;
    [IniValue('CONFIGUR'  ,'DecvUnCom'  ,'10')]
    Property DecvUnCom    : Integer     read FDecvUnCom   write FDecvUnCom;
    [IniValue('CONFIGUR'  ,'DecvProd'   ,'2')]
    Property DecvProd     : Integer     read FDecvProd    write FDecvProd;
    [IniValue('CONFIGUR'  ,'DecqTrib'   ,'4')]
    Property DecqTrib     : Integer     read FDecqTrib    write FDecqTrib;
    [IniValue('CONFIGUR'  ,'DecvUnTrib' ,'10')]
    Property DecvUnTrib   : Integer     read FDecvUnTrib  write FDecvUnTrib;
    procedure Save;
    procedure Load;
    constructor Create;
    destructor Destroy; override;
  end;
implementation

{ TIniAcbrLerTxt }

constructor TIniAcbrLerTxt.Create;
begin
  FSettingsFile := TDirectory.GetCurrentDirectory + '\AcbrLerTxt.ini';
  if Not ( FileExists( FSettingsFile  ) ) then
    Save;
  Load;
end;

destructor TIniAcbrLerTxt.Destroy;
begin
  inherited Destroy;
end;

procedure TIniAcbrLerTxt.Load;
begin
// This loads the INI File Values into the properties.
   TIniPersist.Load(FSettingsFile,Self);
end;

procedure TIniAcbrLerTxt.Save;
begin
// This saves the properties to the INI
   TIniPersist.Save(FSettingsFile,Self);
end;

end.
