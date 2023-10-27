{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXSchemasLoteCobV;

interface

uses
  Classes, SysUtils,
  ACBrJSON, ACBrPIXBase, ACBrPIXSchemasCobV, ACBrPIXSchemasProblema;

type

  { TACBrPIXLoteCobV }

  TACBrPIXLoteCobV = class(TACBrPIXSchema)
  private
    fcriacao: TDateTime;
    fcriacao_Bias: Integer;
    fproblema: TACBrPIXProblema;
    fstatus: TACBrPIXStatusLoteCobranca;
    ftxId: String;
    procedure SetTxId(AValue: String);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXLoteCobV);

    property txId: String read ftxId write SetTxId;
    property status: TACBrPIXStatusLoteCobranca read fstatus write fstatus;
    property problema: TACBrPIXProblema read fproblema;
    property criacao: TDateTime read fcriacao write fcriacao;
    property criacao_Bias: Integer read fcriacao_Bias write fcriacao_Bias;
  end;

  { TACBrPIXLoteCobVArray }

  TACBrPIXLoteCobVArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXLoteCobV;
    procedure SetItem(Index: Integer; Value: TACBrPIXLoteCobV);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ALoteCobV: TACBrPIXLoteCobV): Integer;
    Procedure Insert(Index: Integer; ALoteCobV: TACBrPIXLoteCobV);
    function New: TACBrPIXLoteCobV;
    property Items[Index: Integer]: TACBrPIXLoteCobV read GetItem write SetItem; default;
  end;

  { TACBrPIXLoteCobVConsultado }

  TACBrPIXLoteCobVConsultado = class(TACBrPIXSchema)
  private
    fcobsv: TACBrPIXLoteCobVArray;
    fcriacao: TDateTime;
    fcriacao_Bias: Integer;
    fdescricao: String;
    fid: Int64;
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXLoteCobVConsultado);

    property id: Int64 read fid write fid;
    property descricao: String read fdescricao write fdescricao;
    property criacao: TDateTime read fcriacao write fcriacao;
    property criacao_Bias: Integer read fcriacao_Bias write fcriacao_Bias;
    property cobsv: TACBrPIXLoteCobVArray read fcobsv;
  end;

  { TACBrPIXLoteCobVConsultadoArray }

  TACBrPIXLoteCobVConsultadoArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXLoteCobVConsultado;
    procedure SetItem(Index: Integer; Value: TACBrPIXLoteCobVConsultado);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ALoteCobV: TACBrPIXLoteCobVConsultado): Integer;
    Procedure Insert(Index: Integer; ALoteCobV: TACBrPIXLoteCobVConsultado);
    function New: TACBrPIXLoteCobVConsultado;
    property Items[Index: Integer]: TACBrPIXLoteCobVConsultado read GetItem write SetItem; default;
  end;

  { TACBrPIXCobVSolicitadaLote }

  TACBrPIXCobVSolicitadaLote = class(TACBrPIXCobVSolicitada)
  private
    ftxId: String;
    procedure SetTxId(AValue: String);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVSolicitadaLote);

    property txId: String read ftxId write SetTxId;
  end;

  { TACBrPIXCobVSolicitadaLoteArray }

  TACBrPIXCobVSolicitadaLoteArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXCobVSolicitadaLote;
    procedure SetItem(Index: Integer; Value: TACBrPIXCobVSolicitadaLote);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ALoteCobV: TACBrPIXCobVSolicitadaLote): Integer;
    Procedure Insert(Index: Integer; ALoteCobV: TACBrPIXCobVSolicitadaLote);
    function New: TACBrPIXCobVSolicitadaLote;
    property Items[Index: Integer]: TACBrPIXCobVSolicitadaLote read GetItem write SetItem; default;
  end;

  { TACBrPIXLoteCobVBody }

  TACBrPIXLoteCobVBody = class(TACBrPIXSchema)
  private
    fcobsv: TACBrPIXCobVSolicitadaLoteArray;
    fdescricao: String;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXLoteCobVBody);

    property descricao: String read fdescricao write fdescricao;
    property cobsv: TACBrPIXCobVSolicitadaLoteArray read fcobsv;
  end;

  { TACBrPIXCobVRevisadaLote }

  TACBrPIXCobVRevisadaLote = class(TACBrPIXCobVRevisada)
  private
    ftxId: String;
    procedure SetTxId(AValue: String);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobVRevisadaLote);

    property txId: String read ftxId write SetTxId;
  end;

  { TACBrPIXCobVRevisadaLoteArray }

  TACBrPIXCobVRevisadaLoteArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXCobVRevisadaLote;
    procedure SetItem(Index: Integer; Value: TACBrPIXCobVRevisadaLote);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(ACobV: TACBrPIXCobVRevisadaLote): Integer;
    Procedure Insert(Index: Integer; ACobV: TACBrPIXCobVRevisadaLote);
    function New: TACBrPIXCobVRevisadaLote;
    property Items[Index: Integer]: TACBrPIXCobVRevisadaLote read GetItem write SetItem; default;
  end;

  { TACBrPIXLoteCobVBodyRevisado }

  TACBrPIXLoteCobVBodyRevisado = class(TACBrPIXSchema)
  private
    fcobsv: TACBrPIXCobVRevisadaLoteArray;
    fdescricao: String;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXLoteCobVBodyRevisado);

    property descricao: String read fdescricao write fdescricao;
    property cobsv: TACBrPIXCobVRevisadaLoteArray read fcobsv;
  end;

implementation

uses
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime;

{ TACBrPIXLoteCobV }

constructor TACBrPIXLoteCobV.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fproblema := TACBrPIXProblema.Create('problema');
end;

destructor TACBrPIXLoteCobV.Destroy;
begin
  fproblema.Free;
  inherited Destroy;
end;

procedure TACBrPIXLoteCobV.Clear;
begin
  fcriacao := 0;
  fcriacao_Bias := 0;
  fproblema.Clear;
  fstatus := stlNENHUM;
  ftxId := '';
end;

function TACBrPIXLoteCobV.IsEmpty: Boolean;
begin
  Result := (fcriacao = 0) and
            (fcriacao_Bias = 0) and
            (fstatus = stlNENHUM) and
            (ftxId = '') and
            fproblema.IsEmpty;
end;

procedure TACBrPIXLoteCobV.Assign(Source: TACBrPIXLoteCobV);
begin
  fcriacao := Source.criacao;
  fcriacao_Bias := Source.criacao_Bias;
  fstatus := Source.status;
  ftxId := Source.txId;
  fproblema.Assign(Source.problema);
end;

procedure TACBrPIXLoteCobV.SetTxId(AValue: String);
var
  s, e: String;
begin
  if ftxid = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') and fIsBacen then
  begin
    e := ValidarTxId(s, 35, 26);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

procedure TACBrPIXLoteCobV.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXLoteCobV) then
    Assign(TACBrPIXLoteCobV(ASource));
end;

procedure TACBrPIXLoteCobV.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('txid', ftxId, False)
    .AddPair('criacao', DateTimeToIso8601(fcriacao, BiasToTimeZone(fcriacao_Bias)));

  if (fstatus <> stlNENHUM) then
    AJSon.AddPair('status', PIXStatusLoteCobrancaToString(fstatus));

  fproblema.WriteToJSon(AJSon);
end;

procedure TACBrPIXLoteCobV.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s, wC: String;
begin
  {$IfDef FPC}
  s := EmptyStr;
  wC := EmptyStr;
  {$EndIf}

  AJSon
    .Value('txid', ftxId)
    .Value('status', s)
    .Value('criacao', wC);

  fstatus := StringToPIXStatusLoteCobranca(s);
  fproblema.ReadFromJSon(AJSon);

  if NaoEstaVazio(wC) then
  begin
    fcriacao := Iso8601ToDateTime(wC);
    fcriacao_Bias := TimeZoneToBias(wC);
  end;
end;

{ TACBrPIXLoteCobVArray }

function TACBrPIXLoteCobVArray.GetItem(Index: Integer): TACBrPIXLoteCobV;
begin
  Result := TACBrPIXLoteCobV(inherited Items[Index]);
end;

procedure TACBrPIXLoteCobVArray.SetItem(Index: Integer; Value: TACBrPIXLoteCobV);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXLoteCobVArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXLoteCobVArray.Add(ALoteCobV: TACBrPIXLoteCobV): Integer;
begin
  Result := inherited Add(ALoteCobV);
end;

procedure TACBrPIXLoteCobVArray.Insert(Index: Integer; ALoteCobV: TACBrPIXLoteCobV);
begin
  inherited Insert(Index, ALoteCobV);
end;

function TACBrPIXLoteCobVArray.New: TACBrPIXLoteCobV;
begin
  Result := TACBrPIXLoteCobV.Create('');
  Self.Add(Result);
end;

{ TACBrPIXLoteCobVConsultado }

constructor TACBrPIXLoteCobVConsultado.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcobsv := TACBrPIXLoteCobVArray.Create('cobsv');
end;

destructor TACBrPIXLoteCobVConsultado.Destroy;
begin
  inherited Destroy;
  fcobsv.Free;
end;

procedure TACBrPIXLoteCobVConsultado.Clear;
begin
  fcobsv.Clear;
  fcriacao := 0;
  fcriacao_Bias := 0;
  fdescricao := '';
  fid := 0;
end;

function TACBrPIXLoteCobVConsultado.IsEmpty: Boolean;
begin
  Result := fcobsv.IsEmpty and
            (fcriacao = 0) and
            (fcriacao_Bias = 0) and
            (fdescricao = '') and
            (fid = 0);
end;

procedure TACBrPIXLoteCobVConsultado.Assign(Source: TACBrPIXLoteCobVConsultado);
begin
  fcobsv.Assign(Source.cobsv);
  fcriacao := Source.criacao;
  fcriacao_Bias := Source.criacao_Bias;
  fdescricao := Source.descricao;
  fid := Source.id;
end;

procedure TACBrPIXLoteCobVConsultado.AssignSchema(ASource: TACBrPIXSchema);
begin
   if (ASource is TACBrPIXLoteCobVConsultado) then
     Assign(TACBrPIXLoteCobVConsultado(ASource));
end;

procedure TACBrPIXLoteCobVConsultado.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('id', fid, False)
    .AddPair('descricao', fdescricao, False)
    .AddPair('criacao', DateTimeToIso8601(fcriacao, BiasToTimeZone(fcriacao_Bias)));
  fcobsv.WriteToJSon(AJSon);
end;

procedure TACBrPIXLoteCobVConsultado.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wC: String;
begin
  {$IfDef FPC}wC := EmptyStr;{$EndIf}

  AJSon
    .Value('id', fid)
    .Value('descricao', fdescricao)
    .Value('criacao', wC);

  fcobsv.ReadFromJSon(AJSon);

  if NaoEstaVazio(wC) then
  begin
    fcriacao := Iso8601ToDateTime(wC);
    fcriacao_Bias := TimeZoneToBias(wC);
  end;
end;

{ TACBrPIXLoteCobVConsultadoArray }

function TACBrPIXLoteCobVConsultadoArray.GetItem(Index: Integer): TACBrPIXLoteCobVConsultado;
begin
  Result := TACBrPIXLoteCobVConsultado(inherited Items[Index]);
end;

procedure TACBrPIXLoteCobVConsultadoArray.SetItem(Index: Integer; Value: TACBrPIXLoteCobVConsultado);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXLoteCobVConsultadoArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXLoteCobVConsultadoArray.Add(ALoteCobV: TACBrPIXLoteCobVConsultado): Integer;
begin
  Result := inherited Add(ALoteCobV);
end;

procedure TACBrPIXLoteCobVConsultadoArray.Insert(Index: Integer; ALoteCobV: TACBrPIXLoteCobVConsultado);
begin
  inherited Insert(Index, ALoteCobV);
end;

function TACBrPIXLoteCobVConsultadoArray.New: TACBrPIXLoteCobVConsultado;
begin
  Result := TACBrPIXLoteCobVConsultado.Create('');
  Self.Add(Result);
end;

{ TACBrPIXCobVSolicitadaLote }

procedure TACBrPIXCobVSolicitadaLote.Clear;
begin
  inherited Clear;
  ftxId := '';
end;

function TACBrPIXCobVSolicitadaLote.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (ftxId = '');
end;

procedure TACBrPIXCobVSolicitadaLote.Assign(Source: TACBrPIXCobVSolicitadaLote);
begin
   inherited Assign(Source);
   ftxId := Source.txId;
end;

procedure TACBrPIXCobVSolicitadaLote.SetTxId(AValue: String);
var
  s, e: String;
begin
  if ftxid = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') and fIsBacen then
  begin
    e := ValidarTxId(s, 35, 26);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

procedure TACBrPIXCobVSolicitadaLote.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXCobVSolicitadaLote) then
    Assign(TACBrPIXCobVSolicitadaLote(ASource));
end;

procedure TACBrPIXCobVSolicitadaLote.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('txid', ftxId);
  inherited DoWriteToJSon(AJSon);
end;

procedure TACBrPIXCobVSolicitadaLote.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.Value('txid', ftxId);
  inherited DoReadFromJSon(AJSon);
end;

{ TACBrPIXCobVSolicitadaLoteArray }

function TACBrPIXCobVSolicitadaLoteArray.GetItem(Index: Integer): TACBrPIXCobVSolicitadaLote;
begin
  Result := TACBrPIXCobVSolicitadaLote(inherited Items[Index]);
end;

procedure TACBrPIXCobVSolicitadaLoteArray.SetItem(Index: Integer; Value: TACBrPIXCobVSolicitadaLote);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXCobVSolicitadaLoteArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXCobVSolicitadaLoteArray.Add(ALoteCobV: TACBrPIXCobVSolicitadaLote): Integer;
begin
  Result := inherited Add(ALoteCobV);
end;

procedure TACBrPIXCobVSolicitadaLoteArray.Insert(Index: Integer; ALoteCobV: TACBrPIXCobVSolicitadaLote);
begin
  inherited Insert(Index, ALoteCobV);
end;

function TACBrPIXCobVSolicitadaLoteArray.New: TACBrPIXCobVSolicitadaLote;
begin
  Result := TACBrPIXCobVSolicitadaLote.Create('');
  Self.Add(Result);
end;

{ TACBrPIXLoteCobVBody }

constructor TACBrPIXLoteCobVBody.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcobsv := TACBrPIXCobVSolicitadaLoteArray.Create('cobsv');
end;

destructor TACBrPIXLoteCobVBody.Destroy;
begin
  fcobsv.Free;
  inherited Destroy;
end;

procedure TACBrPIXLoteCobVBody.Clear;
begin
  fcobsv.Clear;
  fdescricao := '';
end;

function TACBrPIXLoteCobVBody.IsEmpty: Boolean;
begin
  Result := fcobsv.IsEmpty and
            (fdescricao = '');
end;

procedure TACBrPIXLoteCobVBody.Assign(Source: TACBrPIXLoteCobVBody);
begin
  fdescricao := Source.descricao;
  fcobsv.Assign(Source.cobsv);
end;

procedure TACBrPIXLoteCobVBody.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('descricao', fdescricao, False);
  fcobsv.WriteToJSon(AJSon);
end;

procedure TACBrPIXLoteCobVBody.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.Value('descricao', fdescricao);
  fcobsv.ReadFromJSon(AJSon);
end;

{ TACBrPIXCobVRevisadaLote }

procedure TACBrPIXCobVRevisadaLote.Clear;
begin
  inherited Clear;
  ftxId := '';
end;

function TACBrPIXCobVRevisadaLote.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and
            (ftxId = '');
end;

procedure TACBrPIXCobVRevisadaLote.Assign(Source: TACBrPIXCobVRevisadaLote);
begin
  inherited Assign(Source);
  ftxId := Source.txId;
end;

procedure TACBrPIXCobVRevisadaLote.SetTxId(AValue: String);
var
  s, e: String;
begin
  if ftxid = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') and fIsBacen then
  begin
    e := ValidarTxId(s, 35, 26);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

procedure TACBrPIXCobVRevisadaLote.AssignSchema(ASource: TACBrPIXSchema);
begin
  if (ASource is TACBrPIXCobVRevisadaLote) then
    Assign(TACBrPIXCobVRevisadaLote(ASource));
end;

procedure TACBrPIXCobVRevisadaLote.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('txid', ftxId);
  inherited DoWriteToJSon(AJSon);
end;

procedure TACBrPIXCobVRevisadaLote.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.Value('txid', ftxId);
  inherited DoReadFromJSon(AJSon);
end;

{ TACBrPIXCobVRevisadaLoteArray }

function TACBrPIXCobVRevisadaLoteArray.GetItem(Index: Integer): TACBrPIXCobVRevisadaLote;
begin
  Result := TACBrPIXCobVRevisadaLote(inherited Items[Index]);
end;

procedure TACBrPIXCobVRevisadaLoteArray.SetItem(Index: Integer; Value: TACBrPIXCobVRevisadaLote);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXCobVRevisadaLoteArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXCobVRevisadaLoteArray.Add(ACobV: TACBrPIXCobVRevisadaLote): Integer;
begin
  Result := inherited Add(ACobV);
end;

procedure TACBrPIXCobVRevisadaLoteArray.Insert(Index: Integer; ACobV: TACBrPIXCobVRevisadaLote);
begin
  inherited Insert(Index, ACobV);
end;

function TACBrPIXCobVRevisadaLoteArray.New: TACBrPIXCobVRevisadaLote;
begin
  Result := TACBrPIXCobVRevisadaLote.Create('');
  Self.Add(Result);
end;

{ TACBrPIXLoteCobVBodyRevisado }

constructor TACBrPIXLoteCobVBodyRevisado.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  fcobsv := TACBrPIXCobVRevisadaLoteArray.Create('cobsv');
end;

destructor TACBrPIXLoteCobVBodyRevisado.Destroy;
begin
  fcobsv.Free;
  inherited Destroy;
end;

procedure TACBrPIXLoteCobVBodyRevisado.Clear;
begin
  fcobsv.Clear;
  fdescricao := '';
end;

function TACBrPIXLoteCobVBodyRevisado.IsEmpty: Boolean;
begin
  Result := fcobsv.IsEmpty and
            (fdescricao = '');
end;

procedure TACBrPIXLoteCobVBodyRevisado.Assign(Source: TACBrPIXLoteCobVBodyRevisado);
begin
  fdescricao := Source.descricao;
  fcobsv.Assign(Source.cobsv);
end;

procedure TACBrPIXLoteCobVBodyRevisado.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('descricao', fdescricao, False);
  fcobsv.WriteToJSon(AJSon);
end;

procedure TACBrPIXLoteCobVBodyRevisado.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.Value('descricao', fdescricao);
  fcobsv.ReadFromJSon(AJSon);
end;

end.

