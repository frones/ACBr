{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Alexandre Rocha Lima e Marcondes                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

//{$DEFINE ThreadEnviaLPT}  { Use // no inicio dessa linha para desabilitar a Thread}

{$IFDEF LINUX}
   { Thread TACBrThreadEnviaLPT não funciona muito bem no Linux }
   { infelizmente, Strings grandes nao funcionam bem no LINUX usando a Thread}
  {$UNDEF ThreadEnviaLPT}
{$ENDIF}

Unit ACBrBase ;

interface
uses
  Classes, SysUtils, syncobjs,
  {$IFDEF COMPILER6_UP}
   Types,
  {$ELSE}
   Windows, ACBrD5,
  {$ENDIF}
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs
  {$Else}
   Contnrs
  {$IfEnd}
  {$IFNDEF NOGUI}
   {$IF DEFINED(VisualCLX)}
    ,QDialogs
   {$ELSEIF DEFINED(FMX)}
    ,FMX.Dialogs, System.UITypes
   {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
    ,Vcl.Dialogs, System.UITypes
   {$ELSE}
    ,Dialogs
   {$IFEND}
  {$ENDIF};

{$IFDEF DELPHIXE2_UP}
const
  { Platform identifiers }
  piacbrWin32          = $00000001; // Windows 32-bit
  piacbrWin64          = $00000002; // Windows 64-bit
  piacbrOSX32          = $00000004; // OS X 32-bit
  piacbrOSX64          = $00001000; // OS X 64-bit
  piacbriOSSimulator32 = $00000008; // iOS Simulator 32-bit (runs on the Mac)
  piacbriOSSimulator64 = $00010000; // iOS Simulator 64-bit (runs on the Mac)
  piacbrAndroid32Arm   = $00000010; // Android device 32-bit
  piacbrAndroid64Arm   = $00008000; // Android device 64-bit
  piacbrLinux32        = $00000020; // Linux 32-bit
  piacbrLinux64        = $00000080; // Linux 64-bit
  piacbrLinux32Arm     = $00002000; // Linux 32-bit ARM processor (raspberry pi)
  piacbrLinux64Arm     = $00004000; // Linux 64-bit ARM processor (raspberry pi)
  piacbriOSDevice32    = $00000040; // iOS Device 32-bit (iPad, iPhone, iPod Touch)
  piacbriOSDevice64    = $00000400; // iOS Device 64-bit (iPad, iPhone, iPod Touch)
  piacbrWinNX32        = $00000100; // Windows ??
  piacbrWinIoT32       = $00000200; // Windows Embedded IoT (Internet of Things) - Intel Galileo
  piacbrWinARM32       = $00000800; // Windows 32-bit ARM processor (raspberry pi)

  piacbrAllDesktopPlatforms = piacbrWin32 or piacbrWin64 or piacbrOSX32
  {$IFDEF DELPHIXE3_UP}
    or piacbrLinux32 or piacbrWinNX32
  {$ENDIF}
  {$IFDEF DELPHIXE8_UP}
    or piacbrLinux64
  {$ENDIF}
  {$IFDEF DELPHIX_BERLIN_UP}
    or piacbrOSX64 or piacbrLinux32Arm or piacbrLinux64Arm
  {$ENDIF};

  piacbrAllAndroidPlatforms = 0
  {$IFDEF DELPHIXE3_UP}
    or piacbrAndroid32Arm
  {$ENDIF}
  {$IFDEF DELPHIX_BERLIN_UP}
    or piacbrAndroid64Arm
  {$ENDIF};

  piacbrAllPlatforms = piacbrAllDesktopPlatforms or piacbrAllAndroidPlatforms
  {$IFDEF DELPHIXE3_UP}
    or piacbriOSSimulator32 or piacbriOSDevice32
  {$ENDIF}
  {$IFDEF DELPHIXE8_UP}
    or piacbriOSDevice64 or piacbrWinIoT32
  {$ENDIF}
  {$IFDEF DELPHIX_SEATTLE_UP}
    or piacbrWinARM32
  {$ENDIF}
  {$IFDEF DELPHIX_RIO_UP}
    or piacbriOSSimulator64
  {$ENDIF};
{$ENDIF}

{$IfNDef FPC}
const
  ANYSIZE_ARRAY = 1;

type
   TLibHandle = THandle;

   // Compatibilidade para compilar nas versões anteriores ao Delphi XE2
   {$IfNDef DELPHIXE2_UP}
    NativeUInt = Cardinal;
   {$EndIf}

   LPVOID = Pointer;
   SizeUInt = {$IFDEF COMPILER16_UP} NativeUInt {$ELSE} Longword {$ENDIF};
   {$IfNDef COMPILER12_UP}
    ULONG_PTR = SizeUInt;
   {$EndIf}

  // Compatibilização de Tipos inexistentes em compiladores NEXTGEN
  {$IfDef NEXTGEN}
    AnsiString = RawByteString;
    AnsiChar = UTF8Char;
    PAnsiChar = MarshaledAString;
    PPAnsiChar = ^MarshaledAString;
    WideString = String;
  {$EndIf}
{$EndIf}

type

  EACBrException = class(Exception);

TACBrAboutInfo = (ACBrAbout);

{ ACBrComponente contém apenas a propriedade ACBrAbout }
{$IFDEF DELPHIXE2_UP}
[ComponentPlatformsAttribute(piacbrAllPlatforms)]
{$ENDIF DELPHIXE2_UP}
TACBrComponent = class( TComponent )
  private
    fsAbout: TACBrAboutInfo;
  published
     property AboutACBr : TACBrAboutInfo read fsAbout write fsAbout
                           stored false ;
  end ;

TACBrGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

TAnsiStringList = class
  private
    FList: array of AnsiString;
    function GetCount: Integer;
    function GetItem(Index: Integer): AnsiString;
    procedure SetItem(Index: Integer; const Value: AnsiString);
    function GetText: AnsiString;

  public
    constructor Create;

    procedure Clear;
    function Add(AAnsiString: AnsiString): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: AnsiString read GetItem write SetItem; default;
    property Text: AnsiString read GetText;
  end;

{ TACBrObjectList }

  TACBrObjectList = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TObject>{$EndIf})
  protected
    fIsSorted: Boolean;
  public
    constructor Create(FreeObjects: boolean = True);

    Function Add(AObject: TObject): Integer;
    Procedure Insert(Index: Integer; AObject: TObject);
    {$IfDef HAS_SYSTEM_GENERICS}
     procedure Sort(const AComparer: IComparer<TObject>);
     function FindObject(Item: TObject; AComparer: IComparer<TObject>; Nearest: Boolean = False): Integer;
     procedure Assign(ObjectListSource: TObjectList<TObject>);
     procedure Clear; virtual;
    {$Else}
     procedure Sort(Compare: TListSortCompare);
     function FindObject(Item: Pointer; AComparer: TListSortCompare; Nearest: Boolean = False): Integer;
    {$EndIf}
  end;

{ TACBrThreadTimer }

{ Essa classe emula um TTimer, porem em uma Thread, evitando sobrecarregar
  o Application. Usada por ACBrLCB e ACBrDIS quando em modo CONSOLE, ou NOGUI }

TACBrThreadTimer = class(TThread)
  private
    fsOnTimer : TNotifyEvent;
    fsEnabled: Boolean;
    fsInterval: Integer;
    fsEvent: TSimpleEvent;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetInterval(const AValue: Integer);
  protected
    procedure DoCallEvent;
    procedure Execute; override;
  public
    constructor Create ;
    destructor Destroy; override;

    property OnTimer  : TNotifyEvent read fsOnTimer write fsOnTimer ;
    property Interval : Integer read fsInterval write SetInterval ;
    property Enabled : Boolean read fsEnabled write SetEnabled ;
  end;

  TACBrInformacaoTipo = ( tiString, tiAnsiString, tiBoolean,
                          tiInteger, tiInt64, tiFloat,
                          tiDate, tiTime, tiDateTime);

{ TACBrInformacao - está classe emula campos TField, permitindo montar listas
  de campos quando necessário}
  TACBrInformacao = class
  private
    fFloatDecimalDigits: Integer;
    fInfo: String;
    fNome: String;
    fTipo: TACBrInformacaoTipo;
    function GetAsBinary: AnsiString;
    function GetAsBoolean: Boolean;
    function GetAsDate: TDateTime;
    function GetAsFloat: Double;
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsTime: TDateTime;
    function GetAsTimeStamp: TDateTime;
    function GetAsTimeStampSQL: TDateTime;
    procedure SetAsBinary(AValue: AnsiString);
    procedure SetAsBoolean(AValue: Boolean);
    procedure SetAsDate(const AValue: TDateTime);
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsInteger(const AValue: Integer);
    procedure SetAsInt64(const AValue: Int64);
    procedure SetAsString(const AValue: String);
    procedure SetAsTime(const AValue: TDateTime);
    procedure SetAsTimeStamp(const AValue: TDateTime);
    procedure SetAsTimeStampSQL(const AValue: TDateTime);
    procedure SetFloatDecimalDigits(AValue: Integer);
  public
    constructor Create;
    property Nome          : String     read fNome             write fNome;
    property Tipo          : TACBrInformacaoTipo read fTipo;
    property AsString      : String     read GetAsString       write SetAsString;
    property AsDate        : TDateTime  read GetAsDate         write SetAsDate;
    property AsTime        : TDateTime  read GetAsTime         write SetAsTime;
    property AsTimeStamp   : TDateTime  read GetAsTimeStamp    write SetAsTimeStamp;
    property AsTimeStampSQL: TDateTime  read GetAsTimeStampSQL write SetAsTimeStampSQL;
    property AsInteger     : Integer    read GetAsInteger      write SetAsInteger;
    property AsInt64       : Int64      read GetAsInt64        write SetAsInt64;
    property AsFloat       : Double     read GetAsFloat        write SetAsFloat;
    property AsBinary      : AnsiString read GetAsBinary       write SetAsBinary;
    property AsBoolean     : Boolean    read GetAsBoolean      write SetAsBoolean;

    property FloatDecimalDigits : Integer read fFloatDecimalDigits write SetFloatDecimalDigits default 2;
  end ;

  { TACBrInformacoes }

  TACBrInformacoes = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrInformacao>{$EndIf})
  private
    function GetItem(Index: Integer): TACBrInformacao;
    procedure SetItem(Index: Integer; const Value: TACBrInformacao);
    function GetFields(Index: String): TAcbrInformacao;
  public
    function Add (Obj: TACBrInformacao): Integer;
    procedure Insert (Index: Integer; Obj: TACBrInformacao);
    function New: TACBrInformacao;

    function AddField(const AName: String): TACBrInformacao; overload;
    function AddField(const AName: String; const AValue: String): TACBrInformacao; overload;
    function FindFieldByName(const AName: String): TACBrInformacao;
    function FieldByName(const AName: String): TACBrInformacao;

    function FieldExists(const AName: String): Boolean;

    procedure SaveToFile(const AFileName : String);
    procedure LoadFromFile(const AFileName : String);

    property Items[Index: Integer]: TACBrInformacao read GetItem write SetItem;
    property Fields[Index: String]: TAcbrInformacao read GetFields; default;
  end;

  { THttpHeader }

  THttpHeader = class(TStringList)
  public
    function GetHeaderIndex(const AHeader: String): Integer;
    function GetHeaderValue(const AHeader: String): String; overload;
    function GetHeaderValue(const AIndex: Integer): String; overload;
    function AddHeader(const AHeader, AValue: string): Integer;
  end;


procedure ACBrAboutDialog ;

implementation

uses
  DateUtils, Math,
  ACBrUtil.Strings,
  ACBrUtil.Math;

procedure ACBrAboutDialog ;
  var Msg : String ;
begin
  {$IFDEF NOGUI}
      Msg := 'Componentes ACBr CONSOLE'+sLineBreak+
             'Automação Comercial Brasil'+sLineBreak+
             'https://projetoacbr.com.br' ;
      Msg := ACBrStr(Msg) ;
      writeln( Msg )
  {$ELSE}
    {$IFDEF VisualCLX}
      Msg := 'Componentes <b>ACBr CLX</b><BR>'+
              'Automação Comercial Brasil<BR><BR>'+
              '<A HREF="https://projetoacbr.com.br">'+
              'https://projetoacbr.com.br</A><BR><BR>' ;
    {$ELSE}
      Msg := 'Componentes ACBr '+
             {$IFDEF FPC}
              'Lazarus/FPC'
             {$ELSE}
              {$IFDEF FMX}
               'FMX'
              {$ELSE}
               'VCL'
              {$ENDIF}
             {$ENDIF}+#10+
             'Automação Comercial Brasil'+#10+#10+
             'https://projetoacbr.com.br' ;
      Msg := ACBrStr( Msg ) ;
    {$ENDIF}

    {$IFDEF FMX}
     MessageDlg(Msg, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK],0);
    {$ELSE}
     MessageDlg(Msg ,mtInformation ,[mbOk],0) ;
    {$ENDIF}
 {$ENDIF}
end;

{ TACBrObjectList }

constructor TACBrObjectList.Create(FreeObjects: boolean);
begin
  inherited Create(FreeObjects);
  fIsSorted := False;
end;

function TACBrObjectList.Add(AObject: TObject): Integer;
begin
  Result := inherited Add(AObject);
  fIsSorted := False;
end;

procedure TACBrObjectList.Insert(Index: Integer; AObject: TObject);
begin
  inherited Insert(Index, AObject);
  fIsSorted := False;
end;

{$IfDef HAS_SYSTEM_GENERICS}
 procedure TACBrObjectList.Sort(const AComparer: IComparer<TObject>);
 begin
   inherited Sort(AComparer);
   fIsSorted := True;
 end;

 procedure TACBrObjectList.Assign(ObjectListSource: TObjectList<TObject>);
 var
   I: Integer;
 begin
   Clear;
   Capacity := ObjectListSource.Capacity;
   for I := 0 to ObjectListSource.Count - 1 do
     Add(ObjectListSource[I]);
 end;

procedure TACBrObjectList.Clear;
begin
  inherited Clear;
end;
{$Else}
 procedure TACBrObjectList.Sort(Compare: TListSortCompare);
 begin
   inherited Sort(Compare);
   fIsSorted := True;
 end;
{$EndIf}

{$IfDef HAS_SYSTEM_GENERICS}
function TACBrObjectList.FindObject(Item: TObject; AComparer: IComparer<TObject>; Nearest: Boolean = False): Integer;
{$Else}
function TACBrObjectList.FindObject(Item: Pointer; AComparer: TListSortCompare; Nearest: Boolean): Integer;
{$EndIf}
var
  nLow, nHigh, nCompare, nCheckPos : Integer;

  function DoCompare(APos: Integer): Integer;
  begin
    {$IfDef HAS_SYSTEM_GENERICS}
     Result := AComparer.Compare(Item,Items[APos]);
    {$Else}
     Result := AComparer(Item,Pointer(Items[APos]));
    {$EndIf}
  end;

begin
  { Inspirado de http://www.avdf.com/mar97/delf_sortlist.html }

  if not fIsSorted then
    raise Exception.Create('Lista de Objetos não foi ordenada por chamada ao método "Sort"');

  nLow := 0;
  nHigh := Count-1;
  Result := -1;
  // keep searching until found or
  // no more items to search
  while (Result = -1) and (nLow <= nHigh) do
  begin
    nCheckPos := (nLow + nHigh) div 2;
    nCompare := DoCompare(nCheckPos);

    if (nCompare = -1) then                // less than
      nHigh := nCheckPos - 1
    else if (nCompare = 1) then            // greater than
      nLow := nCheckPos + 1
    else                                   // equal to
    begin
      Result := nCheckPos;
      // Check if we have another match below
      while (Result > nLow) and (DoCompare(Result-1) = 0) do
        Dec(Result);
    end;
  end;

  if (Result = -1) and Nearest then
    Result := nLow;
end;


{------------------------------ TACBrThreadTimer ------------------------------}
constructor TACBrThreadTimer.Create ;
begin
  fsInterval := 100 ;
  fsEnabled  := False ;
  fsOnTimer  := nil ;
  fsEvent := TSimpleEvent.Create;

  inherited Create( False );
end;

destructor TACBrThreadTimer.Destroy;
begin
  fsEnabled := False;
  Terminate;
  fsEvent.SetEvent;  // libera Event.WaitFor()
  if not Terminated then
    WaitFor;

  fsEvent.Free;
  inherited Destroy;
end;

procedure TACBrThreadTimer.Execute;
begin
  while not Terminated do
  begin
    fsEvent.ResetEvent;

    if fsEnabled then
    begin
      fsEvent.WaitFor( fsInterval );

      if fsEnabled and Assigned( fsOntimer ) then
      begin
        {$IFNDEF NOGUI}
        Synchronize( DoCallEvent )
        {$ELSE}
        DoCallEvent;
        {$ENDIF};
      end;
    end
    else
      fsEvent.WaitFor( Cardinal(-1) );
  end ;
end;

procedure TACBrThreadTimer.DoCallEvent;
begin
  fsOnTimer( self ) ;
end;

procedure TACBrThreadTimer.SetEnabled(const AValue: Boolean);
begin
  if fsEnabled = AValue then
    exit ;

  fsEnabled := AValue;
  fsEvent.SetEvent;
end;

procedure TACBrThreadTimer.SetInterval(const AValue: Integer);
begin
  fsInterval := AValue;
  if AValue = 0 then
     Enabled := False ;
end;

{ TACBrInformacao }

constructor TACBrInformacao.Create;
begin
  inherited Create;

  fFloatDecimalDigits := 2;
  fNome := '';
  fInfo := '';
  fTipo := tiString;
end;

function TACBrInformacao.GetAsDate : TDateTime;
var
   DataStr, AnoStr: String;
begin
  DataStr := OnlyNumber( Trim(fInfo) );

  if (Length(DataStr) = 6) then // DDMMYY, converte para DDMMYYYY
  begin
    AnoStr := IntToStr(YearOf(Today));
    DataStr := copy(DataStr,1,4) + copy(AnoStr,1,2) + copy(DataStr,5,2);
  end;

  try
    Result := EncodeDate( StrToInt(copy(DataStr,5,4)),
                          StrToInt(copy(DataStr,3,2)),
                          StrToInt(copy(DataStr,1,2)) ) ;
  except
    Result := 0 ;
  end;
end;

function TACBrInformacao.GetAsBinary: AnsiString;
begin
  Result := StringToBinaryString(fInfo);
end;

function TACBrInformacao.GetAsBoolean: Boolean;
begin
  Result := (fInfo = '1');
end;

function TACBrInformacao.GetAsFloat : Double;
Var
  Info: String ;
  Pow: Extended;
begin
  Info := StringReplace( Trim(fInfo), ',','',[rfReplaceAll] );
  Info := StringReplace( Info       , '.','',[rfReplaceAll] );
  Pow  := IntPower(10, FloatDecimalDigits);

  Result := StrToFloatDef( Info , 0);
  Result := Result / Pow;
end;

function TACBrInformacao.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Trim(fInfo),0);
end;

function TACBrInformacao.GetAsInt64: Int64;
begin
  Result := StrToInt64Def(Trim(fInfo),0);
end;

function TACBrInformacao.GetAsString: String;
begin
  Result := fInfo ;
end;

function TACBrInformacao.GetAsTime : TDateTime;
var
  TimeStr : String;
begin
  TimeStr := OnlyNumber( Trim(fInfo) );

  try
    Result := EncodeTime( StrToInt(copy(TimeStr,1,2)),
                          StrToInt(copy(TimeStr,3,2)),
                          StrToInt(copy(TimeStr,5,2)), 0) ;
  except
    Result := 0 ;
  end;
end;

function TACBrInformacao.GetAsTimeStamp : TDateTime;
var
  DateTimeStr : String;
begin
  DateTimeStr := OnlyNumber( Trim(fInfo) );

  try
    Result := EncodeDateTime( YearOf(now),
                              StrToInt(copy(DateTimeStr,3,2)),
                              StrToInt(copy(DateTimeStr,1,2)),
                              StrToIntDef(copy(DateTimeStr,5,2),0),
                              StrToIntDef(copy(DateTimeStr,7,2),0),
                              StrToIntDef(copy(DateTimeStr,9,2),0), 0) ;
  except
    Result := 0 ;
  end;
end;

function TACBrInformacao.GetAsTimeStampSQL : TDateTime;
var
  DateTimeStr : String;
begin
  DateTimeStr := OnlyNumber( Trim(fInfo) );

  try
    Result := EncodeDateTime( StrToInt(copy(DateTimeStr,1,4)),
                              StrToInt(copy(DateTimeStr,5,2)),
                              StrToInt(copy(DateTimeStr,7,2)),
                              StrToIntDef(copy(DateTimeStr,9,2),0),
                              StrToIntDef(copy(DateTimeStr,11,2),0),
                              StrToIntDef(copy(DateTimeStr,13,2),0), 0) ;
  except
    Result := 0 ;
  end;
end;

procedure TACBrInformacao.SetAsBinary(AValue: AnsiString);
begin
  fInfo := String(BinaryStringToString(AValue));
  fTipo := tiAnsiString;
end;

procedure TACBrInformacao.SetAsBoolean(AValue: Boolean);
begin
  if AValue then
    fInfo := '1'
  else
    fInfo := '0';

  fTipo := tiBoolean;;
end;

procedure TACBrInformacao.SetAsDate(const AValue : TDateTime);
begin
  if AValue = 0 then
    fInfo := ''
  else
    fInfo := FormatDateTime('DDMMYYYY',AValue);

  fTipo := tiDate;
end;

procedure TACBrInformacao.SetAsFloat(const AValue : Double);
var
  Pow: Extended;
begin
  if AValue = 0 then
    fInfo := ''
  else
  begin
    Pow  := IntPower(10, FloatDecimalDigits);
    fInfo := IntToStr(Trunc(SimpleRoundTo(AValue * Pow, 0)));
    if Length(fInfo) < 3 then
      fInfo := PadLeft(fInfo,3,'0');
  end;

  fTipo := tiFloat;
end;

procedure TACBrInformacao.SetAsInteger(const AValue : Integer);
begin
  if AValue = 0 then
    fInfo := ''
  else
    fInfo := IntToStr( AValue ) ;

  fTipo := tiInteger;
end;

procedure TACBrInformacao.SetAsInt64(const AValue: Int64);
begin
  if AValue = 0 then
    fInfo := ''
  else
    fInfo := IntToStr( AValue ) ;

  fTipo := tiInt64;
end;

procedure TACBrInformacao.SetAsString(const AValue: String);
begin
  fInfo := AValue;
  fTipo := tiString;
end;

procedure TACBrInformacao.SetAsTime(const AValue : TDateTime);
begin
  if AValue = 0 then
    fInfo := ''
  else
    fInfo := FormatDateTime('HHNNSS', AValue);

  fTipo := tiTime;
end;

procedure TACBrInformacao.SetAsTimeStamp(const AValue : TDateTime);
begin
  if AValue = 0 then
    fInfo := ''
  else
    fInfo := FormatDateTime('DDMMHHNNSS', AValue);

  fTipo := tiDateTime;
end;

procedure TACBrInformacao.SetAsTimeStampSQL(const AValue : TDateTime);
begin
  if AValue = 0 then
    fInfo := ''
  else
    fInfo := FormatDateTime('YYYYMMDDHHNNSS', AValue);

  fTipo := tiDateTime;
end;

procedure TACBrInformacao.SetFloatDecimalDigits(AValue: Integer);
begin
  if fFloatDecimalDigits = AValue then
    Exit;

  fFloatDecimalDigits := abs(AValue);
end;

{ TACBrInformacoes }

function TACBrInformacoes.AddField(const AName: String): TACBrInformacao;
begin
  Result := AddField(AName, '');
end;

function TACBrInformacoes.AddField(const AName: String; const AValue: String
  ): TACBrInformacao;
begin
  Result := FindFieldByName(AName);
  if Result <> nil then
    Result.AsString := AValue
  else
  begin
    Result := Self.New;
    with Result do
    begin
      Nome     := AName;
      AsString := AValue;
    end;
  end;
end;

function TACBrInformacoes.FieldByName(const AName: String): TACBrInformacao;
begin
  Result := FindFieldByName( AName );

  if Result = nil then
    raise Exception.CreateFmt('Campo "%s" não encontrado.', [AName]);
end;

function TACBrInformacoes.FieldExists(const AName: String): Boolean;
begin
  Result := FindFieldByName( AName ) <> nil;
end;

function TACBrInformacoes.FindFieldByName(const AName: String): TACBrInformacao;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count - 1 do
  begin
    if AnsiSameText(Self.Items[I].Nome, AName) then
    begin
      Result := Self.Items[I];
      Exit;
    end;
  end;
end;

procedure TACBrInformacoes.SaveToFile(const AFileName : String) ;
var
  I  : Integer ;
  SL : TStringList ;
begin
  SL := TStringList.Create;
  try
    For I := 0 to Count-1 do
       SL.Values[ Items[I].Nome ] := Items[I].AsString;

    SL.SaveToFile( AFileName );
  finally
    SL.Free;
  end ;
end ;

procedure TACBrInformacoes.LoadFromFile(const AFileName : String) ;
var
  I  : Integer ;
  SL : TStringList ;
begin
  SL := TStringList.Create;
  try
    Clear;

    SL.LoadFromFile( AFileName );
    For I := 0 to SL.Count-1 do
{$IFDEF COMPILER7_UP}
       AddField( SL.Names[ I ], SL.ValueFromIndex[ I ] );
{$ELSE}
       AddField( SL.Names[ I ], SL.Values[ SL.Names[ I ] ] );
{$ENDIF}
  finally
    SL.Free;
  end ;
end ;

function TACBrInformacoes.GetFields(Index: String): TAcbrInformacao;
begin
  Result := FieldByName(Index);
end;

function TACBrInformacoes.Add(Obj: TACBrInformacao): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TACBrInformacoes.Insert(Index: Integer; Obj: TACBrInformacao);
begin
  inherited Insert(Index, Obj);
end;

function TACBrInformacoes.GetItem(Index: Integer): TACBrInformacao;
begin
  Result := TACBrInformacao(inherited Items[Index]);
end;

function TACBrInformacoes.New: TACBrInformacao;
begin
  Result := TACBrInformacao.Create;
  inherited Add(Result);
end;

procedure TACBrInformacoes.SetItem(Index: Integer;
  const Value: TACBrInformacao);
begin
  inherited Items[Index] := Value;
end;

{ TAnsiStringList }

constructor TAnsiStringList.Create;
begin
  inherited;
  Clear;
end;

procedure TAnsiStringList.Clear;
begin
  SetLength(FList,0);
end;

function TAnsiStringList.Add(AAnsiString: AnsiString): Integer;
begin
  Result := GetCount;
  SetLength(FList, Result+1);
  FList[Result] := AAnsiString;
end;

function TAnsiStringList.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TAnsiStringList.GetItem(Index: Integer): AnsiString;
begin
  Result := FList[Index]
end;

procedure TAnsiStringList.SetItem(Index: Integer; const Value: AnsiString);
begin
  FList[Index] := Value;
end;

function TAnsiStringList.GetText: AnsiString;
var
  i, l: Integer;
begin
  Result := '';
  l := GetCount-1;
  for i := 0 to l do
    Result := Result + GetItem(i);
end;


{ THttpHeader }

function THttpHeader.GetHeaderIndex(const AHeader: String): Integer;
var
  I: Integer;
  LinhaHeader: String;
begin
  Result := -1;
  I := 0;
  while (Result < 0) and (I < Count) do
  begin
    LinhaHeader := Strings[I];
    if (pos(AHeader, LinhaHeader) = 1) then
      Result := I;

    Inc( I ) ;
  end ;
end;

function THttpHeader.GetHeaderValue(const AHeader: String): String;
var
  I: Integer;
begin
  I := GetHeaderIndex(AHeader);
  Result := GetHeaderValue(I)
end;

function THttpHeader.GetHeaderValue(const AIndex: Integer): String;
var
  LinhaHeader: String;
  P: Integer;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex < Count) then
  begin
    LinhaHeader := Strings[AIndex];
    P := pos(':', LinhaHeader);
    if (P > 0) then
      Result := Trim(copy(LinhaHeader, P+1, Length(LinhaHeader)));
  end;
end;

function THttpHeader.AddHeader(const AHeader, AValue: string): Integer;
var
  LinhaHeader: String;
begin
  LinhaHeader := AHeader + ': ' + AValue;
  Result := GetHeaderIndex(AHeader);
  if Result < 0 then
    Result := Add(LinhaHeader)
  else
    Strings[Result] := LinhaHeader;
end;

end.


