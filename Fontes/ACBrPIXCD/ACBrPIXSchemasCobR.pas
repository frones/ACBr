{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César                                                                }
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

unit ACBrPIXSchemasCobR;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXBase, ACBrPIXSchemasCalendario,
  ACBrPIXSchemasPix, ACBrPIXSchemasDevedor, ACBrPIXSchemasLocation,
  ACBrPIXSchemasRecBase, ACBrPIXSchemasRecAtualizacao, ACBrPIXSchemasPaginacao,
  ACBrPIXSchemasRecParticipantes;

type

  { TACBrPIXCobRTentativa }

  TACBrPIXCobRTentativa = class(TACBrPIXSchema)
  private
    frejeicao: TACBrPIXRecRejeicao;
    fatualizacao: TACBrPIXCobRHistoricoTentativaAtualizacao;
    fdataLiquidacao: TDateTime;
    fendToEndId: String;
    fstatus: TACBrPIXStatusTentativaCobranca;
    ftipo: TACBrPIXTipoTentativaCobranca;
    function GetAtualizacao: TACBrPIXCobRHistoricoTentativaAtualizacao;
    function GetRejeicao: TACBrPIXRecRejeicao;
    procedure SetEndToEndId(AValue: String);
  protected
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobRTentativa);
    
    property endToEndId: String read fendToEndId write SetEndToEndId;
    property tipo: TACBrPIXTipoTentativaCobranca read ftipo write ftipo;
    property status: TACBrPIXStatusTentativaCobranca read fstatus write fstatus;
    property dataLiquidacao: TDateTime read fdataLiquidacao write fdataLiquidacao;

    property rejeicao: TACBrPIXRecRejeicao read GetRejeicao;
    property atualizacao: TACBrPIXCobRHistoricoTentativaAtualizacao read GetAtualizacao;
  end;

  { TACBrPIXCobRHistoricoTentativas }

  TACBrPIXCobRHistoricoTentativas = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TACBrPIXCobRTentativa;
    procedure SetItem(Index: Integer; Value: TACBrPIXCobRTentativa);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    function Add(aItem: TACBrPIXCobRTentativa): Integer;
    procedure Insert(Index: Integer; aItem: TACBrPIXCobRTentativa);
    function New: TACBrPIXCobRTentativa;
    property Items[Index: Integer]: TACBrPIXCobRTentativa read GetItem write SetItem; default;
  end;

  { TACBrPIXCobRValor }

  TACBrPIXCobRValor = class(TACBrPIXSchema)
  private
    foriginal: Currency;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobRValor);

    property original: Currency read foriginal write foriginal;
  end;

  { TACBrPIXCobRBase }

  TACBrPIXCobRBase = class(TACBrPIXSchema)
  private
    fPix: TACBrPIXArray;
    fValor: TACBrPIXCobRValor;
    fDevedor: TACBrPIXDadosDevedor;
    fRecebedor: TACBrPIXCobRRecebedor;
    fCalendario: TACBrPIXCalendarioCobR;
    fAtualizacao: TACBrPIXCobRHistoricoAtualizacao;
    fEncerramento: TACBrPIXRecEncerramento;
    fTentativas: TACBrPIXCobRHistoricoTentativas;
    ftxId: String;
    fidRec: String;
    finfoAdicional: String;
    fajusteDiaUtil: Boolean;
    fpoliticaRetentativa: TACBrPIXRetentativa;
    fstatus: TACBrPIXStatusRegistroCobranca;
    function GetAtualizacao: TACBrPIXCobRHistoricoAtualizacao;
    function GetCalendario: TACBrPIXCalendarioCobR;
    function GetDevedor: TACBrPIXDadosDevedor;
    function GetEncerramento: TACBrPIXRecEncerramento;
    function GetPix: TACBrPIXArray;
    function GetRecebedor: TACBrPIXCobRRecebedor;
    function GetTentativas: TACBrPIXCobRHistoricoTentativas;
    function GetValor: TACBrPIXCobRValor;
    procedure SetIdRec(AValue: String);
    procedure SetInfoAdicional(AValue: String);
    procedure SetTxId(AValue: String);
  protected   
    procedure AssignSchema(ASource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

    property idRec: String read fidRec write SetIdRec;
    property txId: String read ftxId write SetTxId;
    property infoAdicional: String read finfoAdicional write SetInfoAdicional;
    property ajusteDiaUtil: Boolean read fajusteDiaUtil write fajusteDiaUtil;
    property status: TACBrPIXStatusRegistroCobranca read fstatus write fstatus;
    property politicaRetentativa: TACBrPIXRetentativa read fpoliticaRetentativa write fpoliticaRetentativa;

    property pix: TACBrPIXArray read GetPix;
    property valor: TACBrPIXCobRValor read GetValor;
    property devedor: TACBrPIXDadosDevedor read GetDevedor;
    property recebedor: TACBrPIXCobRRecebedor read GetRecebedor;
    property calendario: TACBrPIXCalendarioCobR read GetCalendario;
    property encerramento: TACBrPIXRecEncerramento read GetEncerramento;
    property atualizacao: TACBrPIXCobRHistoricoAtualizacao read GetAtualizacao;
    property tentativas: TACBrPIXCobRHistoricoTentativas read GetTentativas;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; reintroduce;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobRBase);
  end;

  { TACBrPIXCobRSolicitada }

  TACBrPIXCobRSolicitada = class(TACBrPIXCobRBase)
  public
    property idRec;
    property infoAdicional;
    property calendario;
    property valor;
    property ajusteDiaUtil;
    property recebedor;
    property devedor;
  end;

  { TACBrPIXCobRGerada }

  TACBrPIXCobRGerada = class(TACBrPIXCobRBase)
  public
    property idRec;
    property txId;
    property infoAdicional;
    property calendario;
    property valor;
    property ajusteDiaUtil;
    property recebedor;
    property status;
    property devedor;
  end;

  { TACBrPIXCobRRevisada }

  TACBrPIXCobRRevisada = class(TACBrPIXCobRBase)
  public
    property status;
  end;

  { TACBrPIXCobRCompleta }

  TACBrPIXCobRCompleta = class(TACBrPIXCobRBase)
  public
    property idRec;
    property txId;
    property infoAdicional;
    property calendario;
    property valor;
    property ajusteDiaUtil;
    property recebedor;
    property status;
    property politicaRetentativa;
    property devedor;
    property pix; 
    property atualizacao;
    property encerramento;
    property tentativas;
  end;

  { TACBrPIXCobRCompletaArray }

  TACBrPIXCobRCompletaArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPIXCobRCompleta;
    procedure SetItem(aIndex: Integer; Value: TACBrPIXCobRCompleta);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aCobR: TACBrPIXCobRCompleta): Integer;
    Procedure Insert(aIndex: Integer; aCobR: TACBrPIXCobRCompleta);
    function New: TACBrPIXCobRCompleta;
    property Items[aIndex: Integer]: TACBrPIXCobRCompleta read GetItem write SetItem; default;
  end;

  { TACBrPIXCobRConsultaParametros }

  TACBrPIXCobRConsultaParametros = class(TACBrPIXSchema)
  private
    fpaginacao: TACBrPIXPaginacao;
    frecebedor: TACBrPIXRecSolicRecebedor;
    fcnpj: String;
    fcpf: String;
    ffim: TDateTime;
    fidRec: String;
    finicio: TDateTime;
    fstatus: TACBrPIXStatusRegistroCobranca;
    function GetPaginacao: TACBrPIXPaginacao;
    function GetRecebedor: TACBrPIXRecSolicRecebedor;
    procedure SetCnpj(AValue: String);
    procedure SetCpf(AValue: String);
    procedure SetIdRec(AValue: String);
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXCobRConsultaParametros);

    property inicio: TDateTime read finicio write finicio;
    property fim: TDateTime read ffim write ffim;
    property idRec: String read fidRec write SetIdRec;
    property cpf: String read fcpf write SetCpf;
    property cnpj: String read fcnpj write SetCnpj;
    property status: TACBrPIXStatusRegistroCobranca read fstatus write fstatus;
    property recebedor: TACBrPIXRecSolicRecebedor read GetRecebedor;
    property paginacao: TACBrPIXPaginacao read GetPaginacao;
  end;

  { TACBrPIXCobsRConsultadas }

  TACBrPIXCobsRConsultadas = class(TACBrPIXSchema)
  private
    fParametros: TACBrPIXCobRConsultaParametros;
    fCobsr: TACBrPIXCobRCompletaArray;
    function GetCobsr: TACBrPIXCobRCompletaArray;
    function GetParametros: TACBrPIXCobRConsultaParametros;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    destructor Destroy; override;
    procedure Assign(Source: TACBrPIXCobsRConsultadas);

    property parametros: TACBrPIXCobRConsultaParametros read GetParametros;
    property cobsr: TACBrPIXCobRCompletaArray read GetCobsr;
  end;

implementation

uses
  DateUtils, Math, IniFiles,
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime;

{ TACBrPIXCobRTentativa }

function TACBrPIXCobRTentativa.GetAtualizacao: TACBrPIXCobRHistoricoTentativaAtualizacao;
begin
  if not Assigned(fatualizacao) then
    fatualizacao := TACBrPIXCobRHistoricoTentativaAtualizacao.Create('atualizacao');
  Result := fatualizacao;
end;

function TACBrPIXCobRTentativa.GetRejeicao: TACBrPIXRecRejeicao;
begin
  if not Assigned(frejeicao) then
    frejeicao := TACBrPIXRecRejeicao.Create('rejeicao');
  Result := frejeicao;
end;

procedure TACBrPIXCobRTentativa.SetEndToEndId(AValue: String);
var
  e: String;
begin
  if (fendToEndId = AValue) then
    Exit;

  if fIsBacen then
  begin
    e := ValidarEndToEndId(AValue);
    if NaoEstaVazio(e) then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fendToEndId := AValue;
end;

procedure TACBrPIXCobRTentativa.AssignSchema(ASource: TACBrPIXSchema);
begin
  if Assigned(ASource) and (ASource is TACBrPIXCobRTentativa) then
    Assign(TACBrPIXCobRTentativa(ASource));
end;

procedure TACBrPIXCobRTentativa.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('endToEndId', fendToEndId, False)
    .AddPair('tipo', PIXTipoTentativaCobrancaToString(ftipo), False)
    .AddPair('status', PIXStatusTentativaCobrancaToString(fstatus), False);
  if NaoEstaZerado(fdataLiquidacao) then
    AJSon.AddPair('dataLiquidacao', Copy(DateTimeToISO8601(fdataLiquidacao), 1, 10));

  rejeicao.WriteToJSon(AJSon);
  atualizacao.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobRTentativa.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wData, wTipo, wStatus: String;
begin
  Clear;
  {$IFDEF FPC}
  wData := EmptyStr;
  wTipo := EmptyStr;
  wStatus := EmptyStr;
  {$ENDIF}

  AJSon
    .Value('endToEndId', fendToEndId)
    .Value('tipo', wTipo)
    .Value('status', wStatus)
    .Value('dataLiquidacao', wData);

  if NaoEstaVazio(wTipo) then
    ftipo := StringToPIXTipoTentativaCobranca(wTipo);
  if NaoEstaVazio(wStatus) then
    fstatus := StringToPIXStatusTentativaCobranca(wStatus);
  if NaoEstaVazio(wData) then
    fdataLiquidacao := Iso8601ToDateTime(wData);

  rejeicao.ReadFromJSon(AJSon);
  atualizacao.ReadFromJSon(AJSon);
end;

constructor TACBrPIXCobRTentativa.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXCobRTentativa.Destroy;
begin
  if Assigned(frejeicao) then
    frejeicao.Free;
  if Assigned(fatualizacao) then
    fatualizacao.Free;
  inherited;
end;

procedure TACBrPIXCobRTentativa.Clear;
begin
  fendToEndId := EmptyStr;
  ftipo := ttcNENHUM;
  fstatus := steNENHUM;
  fdataLiquidacao := 0;
  if Assigned(frejeicao) then
    frejeicao.Clear;
  if Assigned(fatualizacao) then
    fatualizacao.Clear;
end;

function TACBrPIXCobRTentativa.IsEmpty: Boolean;
begin
  Result := (fendToEndId = EmptyStr) and
            (ftipo = ttcNENHUM) and
            (fstatus = steNENHUM) and
            EstaZerado(fdataLiquidacao) and
            ((not Assigned(frejeicao)) or frejeicao.IsEmpty) and
            ((not Assigned(fatualizacao)) or fatualizacao.IsEmpty);
end;

procedure TACBrPIXCobRTentativa.Assign(Source: TACBrPIXCobRTentativa);
begin
  Clear;
  if not Assigned(Source) then Exit;

  endToEndId := Source.endToEndId;
  ftipo := Source.tipo;
  fstatus := Source.status;
  fdataLiquidacao := Source.dataLiquidacao;
  rejeicao.Assign(Source.rejeicao);
  atualizacao.Assign(Source.atualizacao);
end;

{ TACBrPIXCobRHistoricoTentativas }

function TACBrPIXCobRHistoricoTentativas.GetItem(Index: Integer): TACBrPIXCobRTentativa;
begin
  Result := TACBrPIXCobRTentativa(inherited Items[Index]);
end;

procedure TACBrPIXCobRHistoricoTentativas.SetItem(Index: Integer; Value: TACBrPIXCobRTentativa);
begin
  inherited Items[Index] := Value;
end;

function TACBrPIXCobRHistoricoTentativas.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXCobRHistoricoTentativas.Add(aItem: TACBrPIXCobRTentativa): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPIXCobRHistoricoTentativas.Insert(Index: Integer; aItem: TACBrPIXCobRTentativa);
begin
  inherited Insert(Index, aItem);
end;

function TACBrPIXCobRHistoricoTentativas.New: TACBrPIXCobRTentativa;
begin
  Result := TACBrPIXCobRTentativa.Create;
  Add(Result);
end;

{ TACBrPIXCobRValor }

procedure TACBrPIXCobRValor.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon.AddPair('original', foriginal);
end;

procedure TACBrPIXCobRValor.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  AJSon.Value('original', foriginal);
end;

constructor TACBrPIXCobRValor.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXCobRValor.Clear;
begin
  foriginal := 0;
end;

function TACBrPIXCobRValor.IsEmpty: Boolean;
begin
  Result := EstaZerado(foriginal);
end;

procedure TACBrPIXCobRValor.Assign(Source: TACBrPIXCobRValor);
begin
  foriginal := Source.original;
end;

{ TACBrPIXCobRBase }

function TACBrPIXCobRBase.GetAtualizacao: TACBrPIXCobRHistoricoAtualizacao;
begin
  if not Assigned(fAtualizacao) then
    fAtualizacao := TACBrPIXCobRHistoricoAtualizacao.Create('atualizacao');
  Result := fAtualizacao;
end;

function TACBrPIXCobRBase.GetCalendario: TACBrPIXCalendarioCobR;
begin
  if not Assigned(fCalendario) then
    fCalendario := TACBrPIXCalendarioCobR.Create('calendario');
  Result := fCalendario;
end;

function TACBrPIXCobRBase.GetDevedor: TACBrPIXDadosDevedor;
begin
  if not Assigned(fDevedor) then
    fDevedor := TACBrPIXDadosDevedor.Create('devedor');
  Result := fDevedor;
end;

function TACBrPIXCobRBase.GetEncerramento: TACBrPIXRecEncerramento;
begin
  if not Assigned(fEncerramento) then
    fEncerramento := TACBrPIXRecEncerramento.Create('encerramento');
  Result := fEncerramento;
end;

function TACBrPIXCobRBase.GetPix: TACBrPIXArray;
begin
  if not Assigned(fPix) then
    fPix := TACBrPIXArray.Create('pix');
  Result := fPix;
end;

function TACBrPIXCobRBase.GetRecebedor: TACBrPIXCobRRecebedor;
begin
  if not Assigned(fRecebedor) then
    fRecebedor := TACBrPIXCobRRecebedor.Create('recebedor');
  Result := fRecebedor;
end;

function TACBrPIXCobRBase.GetTentativas: TACBrPIXCobRHistoricoTentativas;
begin
  if not Assigned(fTentativas) then
    fTentativas := TACBrPIXCobRHistoricoTentativas.Create('tentativas');
  Result := fTentativas;
end;

function TACBrPIXCobRBase.GetValor: TACBrPIXCobRValor;
begin
  if not Assigned(fValor) then
    fValor := TACBrPIXCobRValor.Create('valor');
  Result := fValor;
end;

procedure TACBrPIXCobRBase.SetIdRec(AValue: String);
var
  s, e: String;
begin
  if (fidRec = AValue) then Exit;

  s := Trim(AValue);
  if NaoEstaVazio(s) and IsBacen then
  begin
    e := ValidarIdRec(s);
    if NaoEstaVazio(e) then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fidRec := s;
end;

procedure TACBrPIXCobRBase.SetInfoAdicional(AValue: String);
begin
  if finfoAdicional = AValue then Exit;
  finfoAdicional := Copy(AValue, 1, 140);
end;

procedure TACBrPIXCobRBase.SetTxId(AValue: String);
var
  s, e: String;
begin
  if ftxid = AValue then Exit;

  s := Trim(AValue);
  if NaoEstaVazio(s) and IsBacen then
  begin
    e := ValidarTxId(s, 35, 26);
    if NaoEstaVazio(e) then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fTxId := s;
end;

procedure TACBrPIXCobRBase.AssignSchema(ASource: TACBrPIXSchema);
begin
  if Assigned(ASource) and (ASource is TACBrPIXCobRBase) then
    Assign(TACBrPIXCobRBase(ASource));
end;

procedure TACBrPIXCobRBase.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('idRec', fidRec, False)
    .AddPair('txid', ftxId, False)
    .AddPair('politicaRetentativa', PIXRetentativaToString(fpoliticaRetentativa), False)
    .AddPair('status', PIXStatusRegistroCobrancaToString(fstatus), False)
    .AddPair('infoAdicional', finfoAdicional, False);
  if fajusteDiaUtil then
    AJSon.AddPair('ajusteDataUtil', fajusteDiaUtil);

  devedor.WriteToJSon(AJSon);
  recebedor.WriteToJSon(AJSon);
  calendario.WriteToJSon(AJSon);
  valor.WriteToJSon(AJSon);
  encerramento.WriteToJSon(AJSon);
  atualizacao.WriteToJSon(AJSon);
  tentativas.WriteToJSon(AJSon);
  pix.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobRBase.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wPolitica, wStatus: String;
begin
  Clear;
  {$IFDEF FPC}
  wPolitica := EmptyStr;
  wStatus := EmptyStr;
  {$ENDIF}

  AJSon
    .Value('idRec', fidRec)
    .Value('txid', ftxId)
    .Value('ajusteDataUtil', fajusteDiaUtil)
    .Value('politicaRetentativa', wPolitica)
    .Value('status', wStatus)
    .Value('infoAdicional', finfoAdicional);

  if NaoEstaVazio(wPolitica) then
    fpoliticaRetentativa := StringToPIXRetentativa(wPolitica);
  if NaoEstaVazio(wStatus) then
    fstatus := StringToPIXStatusRegistroCobranca(wStatus);

  devedor.ReadFromJSon(AJSon);
  recebedor.ReadFromJSon(AJSon);
  calendario.ReadFromJSon(AJSon);
  valor.ReadFromJSon(AJSon);
  encerramento.ReadFromJSon(AJSon);
  atualizacao.ReadFromJSon(AJSon);
  tentativas.ReadFromJSon(AJSon);
  pix.ReadFromJSon(AJSon);
end;

constructor TACBrPIXCobRBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXCobRBase.Destroy;
begin
  if Assigned(fPix) then
    fPix.Free;
  if Assigned(fValor) then
    fValor.Free;
  if Assigned(fDevedor) then
    fDevedor.Free;
  if Assigned(fRecebedor) then
    fRecebedor.Free;
  if Assigned(fCalendario) then
    fCalendario.Free;
  if Assigned(fAtualizacao) then
    fAtualizacao.Free;
  if Assigned(fEncerramento) then
    fEncerramento.Free;
  if Assigned(fTentativas) then
    fTentativas.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobRBase.Clear;
begin
  fidRec := EmptyStr;
  ftxId := EmptyStr;
  finfoAdicional := EmptyStr;
  fajusteDiaUtil := False;
  fpoliticaRetentativa := rttNENHUM;
  fstatus := srcNENHUM;

  if Assigned(fPix) then fPix.Clear;
  if Assigned(fValor) then fValor.Clear;
  if Assigned(fDevedor) then fDevedor.Clear;
  if Assigned(fRecebedor) then fRecebedor.Clear;
  if Assigned(fCalendario) then fCalendario.Clear;
  if Assigned(fAtualizacao) then fAtualizacao.Clear;
  if Assigned(fEncerramento) then fEncerramento.Clear;
  if Assigned(fTentativas) then fTentativas.Clear;
end;

function TACBrPIXCobRBase.IsEmpty: Boolean;
begin
  Result := EstaVazio(fidRec) and
            EstaVazio(ftxId) and
            EstaVazio(finfoAdicional) and
            (not fajusteDiaUtil) and
            (fpoliticaRetentativa = rttNENHUM) and
            (fstatus = srcNENHUM) and
            ((not Assigned(fPix)) or fPix.IsEmpty) and
            ((not Assigned(fValor)) or fValor.IsEmpty) and
            ((not Assigned(fDevedor)) or fDevedor.IsEmpty) and
            ((not Assigned(fRecebedor)) or fRecebedor.IsEmpty) and
            ((not Assigned(fCalendario)) or fCalendario.IsEmpty) and
            ((not Assigned(fAtualizacao)) or fAtualizacao.IsEmpty) and
            ((not Assigned(fEncerramento)) or fEncerramento.IsEmpty) and
            ((not Assigned(fTentativas)) or fTentativas.IsEmpty);
end;

procedure TACBrPIXCobRBase.Assign(Source: TACBrPIXCobRBase);
begin
  Clear;
  if not Assigned(Source) then Exit;

  idRec := Source.idRec;
  txId := Source.txId;
  infoAdicional := Source.infoAdicional;
  fajusteDiaUtil := Source.ajusteDiaUtil;
  fpoliticaRetentativa := Source.politicaRetentativa;
  fstatus := Source.status;

  pix.Assign(Source.pix);
  valor.Assign(Source.valor);
  devedor.Assign(Source.devedor);
  recebedor.Assign(Source.recebedor);
  calendario.Assign(Source.calendario);
  atualizacao.Assign(Source.atualizacao);
  encerramento.Assign(Source.encerramento);
  tentativas.Assign(Source.tentativas);
end;

{ TACBrPIXCobRCompletaArray }

function TACBrPIXCobRCompletaArray.GetItem(aIndex: Integer): TACBrPIXCobRCompleta;
begin
  Result := TACBrPIXCobRCompleta(inherited Items[aIndex]);
end;

procedure TACBrPIXCobRCompletaArray.SetItem(aIndex: Integer; Value: TACBrPIXCobRCompleta);
begin
  inherited Items[aIndex] := Value;
end;

function TACBrPIXCobRCompletaArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXCobRCompletaArray.Add(aCobR: TACBrPIXCobRCompleta): Integer;
begin
  Result := inherited Add(aCobR);
end;

procedure TACBrPIXCobRCompletaArray.Insert(aIndex: Integer; aCobR: TACBrPIXCobRCompleta);
begin
  inherited Insert(aIndex, aCobR);
end;

function TACBrPIXCobRCompletaArray.New: TACBrPIXCobRCompleta;
begin
  Result := TACBrPIXCobRCompleta.Create;
  Add(Result);
end;

{ TACBrPIXCobRConsultaParametros }

function TACBrPIXCobRConsultaParametros.GetPaginacao: TACBrPIXPaginacao;
begin
  if not Assigned(fPaginacao) then
    fPaginacao := TACBrPIXPaginacao.Create('paginacao');
  Result := fPaginacao;
end;

function TACBrPIXCobRConsultaParametros.GetRecebedor: TACBrPIXRecSolicRecebedor;
begin
  if not Assigned(fRecebedor) then
    fRecebedor := TACBrPIXRecSolicRecebedor.Create('recebedor');
  Result := fRecebedor;
end;

procedure TACBrPIXCobRConsultaParametros.SetCnpj(AValue: String);
begin
  if fcnpj = AValue then Exit;
  fcnpj := AValue;
end;

procedure TACBrPIXCobRConsultaParametros.SetCpf(AValue: String);
begin
  if fcpf = AValue then Exit;
  fcpf := AValue;
end;

procedure TACBrPIXCobRConsultaParametros.SetIdRec(AValue: String);
begin
  if fidRec = AValue then Exit;
  fidRec := AValue;
end;

procedure TACBrPIXCobRConsultaParametros.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('inicio', DateTimeToISO8601(finicio))
    .AddPair('fim', DateTimeToISO8601(ffim))
    .AddPair('idRec', fidRec, False)
    .AddPair('cpf', fcpf, False)
    .AddPair('cnpj', fcnpj, False)
    .AddPair('status', PIXStatusRegistroCobrancaToString(fstatus), False);

  recebedor.WriteToJSon(AJSon);
  paginacao.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobRConsultaParametros.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  wInicio, wFim, wStatus: String;
begin
  Clear;
  {$IFDEF FPC}
  wInicio := EmptyStr;
  wFim := EmptyStr;
  wStatus := EmptyStr;
  {$ENDIF}

  AJSon
    .Value('inicio', wInicio)
    .Value('fim', wFim)
    .Value('idRec', fidRec)
    .Value('cpf', fcpf)
    .Value('cnpj', fcnpj)
    .Value('status', wStatus);

  if NaoEstaVazio(wInicio) then
    finicio := ISO8601ToDateTime(wInicio);
  if NaoEstaVazio(wFim) then
    ffim := ISO8601ToDateTime(wFim);
  if NaoEstaVazio(wStatus) then
    fstatus := StringToPIXStatusRegistroCobranca(wStatus);

  recebedor.ReadFromJSon(AJSon);
  paginacao.ReadFromJSon(AJSon);
end;

constructor TACBrPIXCobRConsultaParametros.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXCobRConsultaParametros.Destroy;
begin
  if Assigned(fPaginacao) then
    fPaginacao.Free;
  if Assigned(fRecebedor) then
    fRecebedor.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobRConsultaParametros.Clear;
begin
  finicio := 0;
  ffim := 0;
  fidRec := EmptyStr;
  fcpf := EmptyStr;
  fcnpj := EmptyStr;
  fstatus := srcNENHUM;
  if Assigned(fPaginacao) then
    fPaginacao.Clear;
  if Assigned(fRecebedor) then
    fRecebedor.Clear;
end;

function TACBrPIXCobRConsultaParametros.IsEmpty: Boolean;
begin
  Result := EstaZerado(finicio) and
            EstaZerado(ffim) and
            EstaVazio(fidRec) and
            EstaVazio(fcpf) and
            EstaVazio(fcnpj) and
            (fstatus = srcNENHUM) and
            ((not Assigned(fPaginacao)) or fPaginacao.IsEmpty) and
            ((not Assigned(fRecebedor)) or fRecebedor.IsEmpty);
end;

procedure TACBrPIXCobRConsultaParametros.Assign(Source: TACBrPIXCobRConsultaParametros);
begin
  Clear;
  if not Assigned(Source) then Exit;

  finicio := Source.inicio;
  ffim := Source.fim;
  idRec := Source.idRec;
  cpf := Source.cpf;
  cnpj := Source.cnpj;
  fstatus := Source.status;
  paginacao.Assign(Source.paginacao);
  recebedor.Assign(Source.recebedor);
end;

{ TACBrPIXCobsRConsultadas }

function TACBrPIXCobsRConsultadas.GetCobsr: TACBrPIXCobRCompletaArray;
begin
  if not Assigned(fCobsr) then
    fCobsr := TACBrPIXCobRCompletaArray.Create('cobsr');
  Result := fCobsr;
end;

function TACBrPIXCobsRConsultadas.GetParametros: TACBrPIXCobRConsultaParametros;
begin
  if not Assigned(fParametros) then
    fParametros := TACBrPIXCobRConsultaParametros.Create('parametros');
  Result := fParametros;
end;

procedure TACBrPIXCobsRConsultadas.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  parametros.WriteToJSon(AJSon);
  cobsr.WriteToJSon(AJSon);
end;

procedure TACBrPIXCobsRConsultadas.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  Clear;
  parametros.ReadFromJSon(AJSon);
  cobsr.ReadFromJSon(AJSon);
end;

constructor TACBrPIXCobsRConsultadas.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXCobsRConsultadas.Clear;
begin
  if Assigned(fParametros) then
    fParametros.Clear;
  if Assigned(fCobsr) then
    fCobsr.Clear;
end;

function TACBrPIXCobsRConsultadas.IsEmpty: Boolean;
begin
  Result := ((not Assigned(fParametros)) or fParametros.IsEmpty) and
            ((not Assigned(fCobsr)) or fCobsr.IsEmpty);
end;

destructor TACBrPIXCobsRConsultadas.Destroy;
begin
  if Assigned(fParametros) then
    fParametros.Free;
  if Assigned(fCobsr) then
    fCobsr.Free;
  inherited Destroy;
end;

procedure TACBrPIXCobsRConsultadas.Assign(Source: TACBrPIXCobsRConsultadas);
begin
  Clear;
  if not Assigned(Source) then Exit;

  parametros.Assign(Source.parametros);
  cobsr.Assign(Source.cobsr);
end;

end.

