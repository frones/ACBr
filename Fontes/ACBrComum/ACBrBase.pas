{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:    Alexandre Rocha Lima e Marcondes             }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
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
uses Classes, SysUtils, Contnrs, syncobjs,
     {$IFDEF COMPILER6_UP}
        Types
     {$ELSE}
        Windows, ACBrD5
     {$ENDIF}
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
type

TACBrAboutInfo = (ACBrAbout);

{ ACBrComponente contém apenas a propriedade ACBrAbout }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
TACBrComponent = class( TComponent )
  private
    fsAbout: TACBrAboutInfo;
  published
     property AboutACBr : TACBrAboutInfo read fsAbout write fsAbout
                           stored false ;
  end ;

TACBrGravarLog = procedure(const ALogLine: String; var Tratado: Boolean) of object ;

{ TACBrObjectList }

TACBrObjectList = class(TObjectList)
  protected
    fIsSorted: Boolean;
  public
    constructor Create(FreeObjects: boolean);

    Function Add(AObject: TObject): Integer;
    Procedure Insert(Index: Integer; AObject: TObject);
    procedure Sort(Compare: TListSortCompare);

    function FindObject(Item: Pointer; Compare: TListSortCompare; Nearest: Boolean = False): Integer;
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

{ TACBrInformacao - está classe emula campos TField, permitindo montar listas
de campos quando necessário}
  TACBrInformacao = class
  private
    fFloatDecimalDigits: Integer;
    fInfo: String;
    fName: String;
    function GetAsDate : TDateTime;
    function GetAsFloat: Double;
    function GetAsInteger : Integer;
    function GetAsString: String;
    function GetAsTime : TDateTime;
    function GetAsTimeStamp : TDateTime;
    function GetAsTimeStampSQL : TDateTime;
    //procedure SetAsAnsiString(const AValue: AnsiString);
    procedure SetAsDate(const AValue : TDateTime);
    procedure SetAsFloat(const AValue : Double);
    procedure SetAsInteger(const AValue : Integer);
    procedure SetAsString(const AValue: String);
    procedure SetAsTime(const AValue : TDateTime);
    procedure SetAsTimeStamp(const AValue : TDateTime);
    procedure SetAsTimeStampSQL(const AValue : TDateTime);
    procedure SetFloatDecimalDigits(AValue: Integer);
  public
    constructor Create;
    property Nome          : String     read fName             write fName;
    property AsString      : String     read GetAsString       write SetAsString ;
    property AsDate        : TDateTime  read GetAsDate         write SetAsDate ;
    property AsTime        : TDateTime  read GetAsTime         write SetAsTime ;
    property AsTimeStamp   : TDateTime  read GetAsTimeStamp    write SetAsTimeStamp ;
    property AsTimeStampSQL: TDateTime  read GetAsTimeStampSQL write SetAsTimeStampSQL ;
    property AsInteger     : Integer    read GetAsInteger      write SetAsInteger ;
    property AsFloat       : Double     read GetAsFloat        write SetAsFloat ;

    property FloatDecimalDigits : Integer read fFloatDecimalDigits write SetFloatDecimalDigits default 2;
  end ;

  { TACBrInformacoes }

  TACBrInformacoes = class(TObjectList)
  private
    function GetItem(Index: Integer): TACBrInformacao;
    procedure SetItem(Index: Integer; const Value: TACBrInformacao);
    function GetFields(Index: String): TAcbrInformacao;
  public
    function Add: TACBrInformacao;
    function AddField(const AName: String; const AValue: String): TACBrInformacao;
    function FindFieldByName(const AName: String): TACBrInformacao;
    function FieldByName(const AName: String): TACBrInformacao;

    function FieldExists(const AName: String): Boolean;

    procedure SaveToFile(const AFileName : String);
    procedure LoadFromFile(const AFileName : String);

    property Items[Index: Integer]: TACBrInformacao read GetItem write SetItem;
    property Fields[Index: String]: TAcbrInformacao read GetFields; default;
  end;

procedure ACBrAboutDialog ;

implementation

Uses
  ACBrUtil, DateUtils, Math ;

procedure ACBrAboutDialog ;
  var Msg : String ;
begin
  {$IFDEF NOGUI}
      Msg := 'Componentes ACBr CONSOLE'+sLineBreak+
             'Automação Comercial Brasil'+sLineBreak+
             'http://acbr.sourceforge.net' ;
      Msg := ACBrStr(Msg) ;
      writeln( Msg )
  {$ELSE}
    {$IFDEF VisualCLX}
      Msg := 'Componentes <b>ACBr CLX</b><BR>'+
              'Automação Comercial Brasil<BR><BR>'+
              '<A HREF="http://acbr.sourceforge.net">'+
              'http://acbr.sourceforge.net</A><BR><BR>' ;
    {$ELSE}
      Msg := 'Componentes ACBr '+{$IFDEF FPC}'Lazarus/FPC'{$ELSE}'VCL'{$ENDIF}+#10+
             'Automação Comercial Brasil'+#10+#10+
             'http://acbr.sourceforge.net' ;
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

procedure TACBrObjectList.Sort(Compare: TListSortCompare);
begin
  inherited Sort(Compare);
  fIsSorted := True;
end;

{ Inspirado de http://www.avdf.com/mar97/delf_sortlist.html }

function TACBrObjectList.FindObject(Item: Pointer; Compare: TListSortCompare;
  Nearest: Boolean): Integer;
var
  nLow, nHigh, nCompare, nCheckPos : Integer;
begin
  if not fIsSorted then
    raise Exception.Create('Lista de Objetos não foi ordanada por chamada ao método "Sort"');

  nLow := 0;
  nHigh := Count-1;
  Result := -1;
  // keep searching until found or
  // no more items to search
  while (Result = -1) and (nLow <= nHigh) do
  begin
    nCheckPos := (nLow + nHigh) div 2;
    nCompare := Compare(Item,Pointer(Items[nCheckPos]));
    if (nCompare = -1) then                // less than
      nHigh := nCheckPos - 1
    else if (nCompare = 1) then            // greater than
      nLow := nCheckPos + 1
    else                                   // equal to
      Result := nCheckPos;
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

function TACBrInformacao.GetAsDate : TDateTime;
var
   DataStr : String;
begin
  DataStr := OnlyNumber( Trim(fInfo) );

  try
     Result := EncodeDate( StrToInt(copy(DataStr,5,4)),
                           StrToInt(copy(DataStr,3,2)),
                           StrToInt(copy(DataStr,1,2)) ) ;
  except
     Result := 0 ;
  end;
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

function TACBrInformacao.GetAsInteger : Integer;
begin
  Result := StrToIntDef(Trim(fInfo),0);
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

{
procedure TACBrInformacao.SetAsAnsiString(const AValue: AnsiString);
begin
   fInformacao := AValue;
end;
}
procedure TACBrInformacao.SetAsDate(const AValue : TDateTime);
begin
  if AValue = 0 then
     fInfo := ''
  else
     fInfo := FormatDateTime('DDMMYYYY',AValue);
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
      fInfo := PadLeft(fInfo,3,'0') ;
  end ;
end;

procedure TACBrInformacao.SetAsInteger(const AValue : Integer);
begin
  if AValue = 0 then
     fInfo := ''
  else
     fInfo := IntToStr( AValue ) ;
end;

procedure TACBrInformacao.SetAsString(const AValue: String);
begin
   fInfo := AValue;
end;

procedure TACBrInformacao.SetAsTime(const AValue : TDateTime);
begin
  if AValue = 0 then
     fInfo := ''
  else
     fInfo := FormatDateTime('HHNNSS', AValue);
end;

procedure TACBrInformacao.SetAsTimeStamp(const AValue : TDateTime);
begin
  if AValue = 0 then
     fInfo := ''
  else
     fInfo := FormatDateTime('DDMMHHNNSS', AValue);
end;

procedure TACBrInformacao.SetAsTimeStampSQL(const AValue : TDateTime);
begin
  if AValue = 0 then
     fInfo := ''
  else
     fInfo := FormatDateTime('YYYYMMDDHHNNSS', AValue);
end;

procedure TACBrInformacao.SetFloatDecimalDigits(AValue: Integer);
begin
  if fFloatDecimalDigits = AValue then
    Exit;

  fFloatDecimalDigits := abs(AValue);
end;

constructor TACBrInformacao.Create;
begin
  inherited Create;
  fFloatDecimalDigits := 2;
end;

{ TACBrInformacoes }

function TACBrInformacoes.AddField(const AName: String; const AValue: String
  ): TACBrInformacao;
begin
  Result := FindFieldByName(AName);
  if Result <> nil then
    Result.AsString := AValue
  else
  begin
    Result := Self.Add;
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

function TACBrInformacoes.GetItem(
  Index: Integer): TACBrInformacao;
begin
  Result := TACBrInformacao(inherited Items[Index]);
end;

function TACBrInformacoes.Add: TACBrInformacao;
begin
  Result := TACBrInformacao.Create;
  inherited Add(Result);
end;

procedure TACBrInformacoes.SetItem(Index: Integer;
  const Value: TACBrInformacao);
begin
  Put(Index, Value);
end;

end.

