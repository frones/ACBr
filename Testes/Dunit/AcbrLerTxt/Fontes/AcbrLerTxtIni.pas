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
    fStrtpAmb     : Widestring;
    fIntnNF       : Integer;
    FDecvUnTrib   : Integer;
    FDecqCom      : Integer;
    FDecvUnCom    : Integer;
    FDecvProd     : Integer;
    FDecqTrib     : Integer;
    FDecvFrete    : Integer;
    FDecvSeg      : Integer;
    FDecvDesc     : Integer;
    FDecvOutro    : Integer;
    fIntcNF       : Integer;
    fIntcDV       : Integer;
    fIntCCEP      : Integer;
    fIntECEP      : Integer;

    FSettingsFile : String;
  Public
    [IniValue('CONFIGUR'      ,'Versao' ,'Beta 1.1')]
    Property Versao       : Widestring  read FVersao      write fVersao;
    [IniValue('CONFIGUR'  ,'StrId'      ,'')]
    Property StrId        : Widestring  read FStrId       write fStrId;
    [IniValue('CONFIGUR'  ,'StrtpAmb'    ,'0')]
    Property  StrtpAmb    : Widestring  read fStrtpAmb    write fStrtpAmb;

    [IniValue('CONFIGURiDB' ,'IntcNF' ,'8')]
    Property IntcNF     : Integer     read fIntcNF   write fIntcNF;
    [IniValue('CONFIGURiDB' ,'IntnNF' ,'6')]
    Property  IntnNF      : Integer     read fIntnNF      write fIntnNF;
    [IniValue('CONFIGURiDB' ,'IntcDV' ,'1')]
    Property  IntcDV       : Integer    read fIntcDV      write fIntcDV;

    [IniValue('CONFIGURidC' ,'IntCCEP' ,'8')]
    Property  IntCCEP      : Integer    read fIntCCEP     write fIntCCEP;

    [IniValue('CONFIGURIdE' ,'IntECEP' ,'8')]
    Property  IntECEP      : Integer    read fIntECEP     write fIntECEP;

    [IniValue('CONFIGURIdI'  ,'DecqCom'    ,'4')]
    Property DecqCom      : Integer     read FDecqCom     write FDecqCom;
    [IniValue('CONFIGURIdI'  ,'DecvUnCom'  ,'10')]
    Property DecvUnCom    : Integer     read FDecvUnCom   write FDecvUnCom;
    [IniValue('CONFIGURIdI'  ,'DecvProd'   ,'2')]
    Property DecvProd     : Integer     read FDecvProd    write FDecvProd;
    [IniValue('CONFIGURIdI'  ,'DecqTrib'   ,'4')]
    Property DecqTrib     : Integer     read FDecqTrib    write FDecqTrib;
    [IniValue('CONFIGURIdI'  ,'DecvUnTrib' ,'10')]
    Property DecvUnTrib   : Integer     read FDecvUnTrib  write FDecvUnTrib;
    [IniValue('CONFIGURIdI'  ,'DecvFrete'  ,'2')]
    Property DecvFrete    : Integer     read FDecvFrete   write FDecvFrete;
    [IniValue('CONFIGURIdI'  ,'DecvSeg'    ,'2')]
    Property DecvSeg      : Integer     read FDecvSeg     write FDecvSeg;
    [IniValue('CONFIGURIdI'  ,'DecvDesc'   ,'2')]
    Property DecvDesc     : Integer     read FDecvDesc    write FDecvDesc;
    [IniValue('CONFIGURIdI'  ,'DecvOutro'  ,'2')]
    Property DecvOutro    : Integer     read FDecvOutro   write FDecvOutro;



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
