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

unit ACBrPIXSchemasLocation;

interface

uses
  Classes, SysUtils, ACBrJSON, ACBrPIXBase,
  ACBrPIXSchemasPaginacao,
  ACBrPIXSchemasRecParticipantes;

type

  { TACBrPIXLocationBase }

  TACBrPIXLocationBase = class(TACBrPIXSchema)
  private
    fcriacao: TDateTime;
    fcriacao_Bias: Integer;
    fid: Int64;
    fidRec: String;
    flocation: String;
    ftipoCob: TACBrPIXTipoCobranca;
    ftxId: String;
    procedure SetidRec(AValue: String);
    procedure SetTxId(AValue: String);
  protected
    property id: Int64 read fid write fid;
    property txId: String read ftxId write SetTxId;
    property location: String read flocation write flocation;
    property idRec: String read fidRec write SetidRec;

    property criacao: TDateTime read fcriacao;
    property criacao_Bias: Integer read fcriacao_Bias;

    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXLocationBase);

    property tipoCob: TACBrPIXTipoCobranca read ftipoCob write ftipoCob;
  end;

  { TACBrPIXLocationCobSolicitada }

  TACBrPIXLocationCobSolicitada = class(TACBrPIXLocationBase)
  public
    property id;
  end;

  { TACBrPIXLocation }

  TACBrPIXLocation = class(TACBrPIXLocationBase)
  public
    property id;
    property location;
    property criacao;
    property criacao_Bias;
  end;

  { TACBrPIXLocationCompleta }

  TACBrPIXLocationCompleta = class(TACBrPIXLocationBase)
  public
    property id;
    property txId;
    property location;
    property criacao;
    property criacao_Bias;
  end;

  { TACBrPIXRecLocation }

  TACBrPIXRecLocation = class(TACBrPIXLocationBase)
  public
    property id;
    property location;
    property criacao;
    property criacao_Bias;
    property idRec;
  end;

  { TACBrPIXRecLocationArray }

  TACBrPIXRecLocationArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPIXRecLocation;
    procedure SetItem(aIndex: Integer; Value: TACBrPIXRecLocation);
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(aRecLoc: TACBrPIXRecLocation): Integer;
    Procedure Insert(aIndex: Integer; aRecLoc: TACBrPIXRecLocation);
    function New: TACBrPIXRecLocation;
    property Items[aIndex: Integer]: TACBrPIXRecLocation read GetItem write SetItem; default;
  end;

  { TACBrPIXRecLocationGerada }

  TACBrPIXRecLocationGerada = class(TACBrPIXLocationBase)
  public
    property id;
    property location;
    property criacao;
    property criacao_Bias;
  end;

  { TACBrPIXLocRecConsultaParametros }

  TACBrPIXLocRecConsultaParametros = class(TACBrPIXSchema)
  private
    fidRecPresente: Boolean;
    fpaginacao: TACBrPIXPaginacao;
    frecebedor: TACBrPIXRecSolicRecebedor;
    ffim: TDateTime;
    finicio: TDateTime;
    fstatus: TACBrPIXStatusRegistroCobranca;
    function GetPaginacao: TACBrPIXPaginacao;
    function GetRecebedor: TACBrPIXRecSolicRecebedor;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;

  public
    constructor Create(const ObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXLocRecConsultaParametros);

    property inicio: TDateTime read finicio write finicio;
    property fim: TDateTime read ffim write ffim;
    property idRecPresente: Boolean read fidRecPresente write fidRecPresente;
    property recebedor: TACBrPIXRecSolicRecebedor read GetRecebedor;
    property paginacao: TACBrPIXPaginacao read GetPaginacao;
  end;

  { TACBrPIXRecLocConsultadas }

  TACBrPIXRecLocConsultadas = class(TACBrPIXSchema)
  private
    fParametros: TACBrPIXLocRecConsultaParametros;
    fLoc: TACBrPIXRecLocationArray;
    function GetLoc: TACBrPIXRecLocationArray;
    function GetParametros: TACBrPIXLocRecConsultaParametros;
  protected
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrPIXRecLocConsultadas);

    property parametros: TACBrPIXLocRecConsultaParametros read GetParametros;
    property loc: TACBrPIXRecLocationArray read GetLoc;
  end;

implementation

uses
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrPIXUtil;

{ TACBrPIXLocationBase }

constructor TACBrPIXLocationBase.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TACBrPIXLocationBase.Clear;
begin
  fcriacao := 0;
  fcriacao_Bias := 0;
  fid := 0;
  flocation := '';
  ftipoCob := tcoNenhuma;
  ftxId := '';
  fidRec := '';
end;

function TACBrPIXLocationBase.IsEmpty: Boolean;
begin
  Result := (fcriacao = 0) and
            (fcriacao_Bias = 0) and
            (fid = 0) and
            (flocation = '') and
            (ftipoCob = tcoNenhuma) and
            (ftxId = '') and
            (fidRec = '');
end;

procedure TACBrPIXLocationBase.Assign(Source: TACBrPIXLocationBase);
begin
  fcriacao := Source.criacao;
  fcriacao_Bias := Source.criacao_Bias;
  fid := Source.id;
  flocation := Source.location;
  ftipoCob := Source.tipoCob;
  ftxId := Source.txId;
  fidRec := Source.idRec;
end;

procedure TACBrPIXLocationBase.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  if (fid <> 0) then
    AJSon.AddPair('id', fid, False);
  if (ftipoCob <> tcoNenhuma) then
    AJSon.AddPair('tipoCob', PIXTipoCobrancaToString(ftipoCob));
  if (fcriacao > 0) then
    AJSon.AddPair('criacao', DateTimeToIso8601(fcriacao, BiasToTimeZone(fcriacao_Bias)));

  AJSon
    .AddPair('txid', ftxId, False)
    .AddPair('location', flocation, False)
    .AddPair('idRec', fidRec, False);
end;

procedure TACBrPIXLocationBase.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  s, wC: String;
begin 
  {$IfDef FPC}
  s := EmptyStr;
  wC := EmptyStr;
  {$EndIf}

  AJSon
    .Value('id', fid)
    .Value('txid', ftxId)
    .Value('location', flocation)
    .Value('tipoCob', s)
    .Value('criacao', wC)
    .Value('idRec', fidRec);

  ftipoCob := StringToPIXTipoCobranca(s);

  if NaoEstaVazio(wC) then
  begin
    fcriacao := Iso8601ToDateTime(wC);
    fcriacao_Bias := TimeZoneToBias(wC);
  end;
end;

procedure TACBrPIXLocationBase.SetTxId(AValue: String);
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

procedure TACBrPIXLocationBase.SetidRec(AValue: String);
var
  s, e: String;
begin
  if fidRec = AValue then Exit;

  s := Trim(AValue);
  if NaoEstaVazio(s) and fIsBacen then
  begin
    e := ValidarIdRec(s);
    if NaoEstaVazio(e) then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fidRec := s;
end;

{ TACBrPIXRecLocationArray }

function TACBrPIXRecLocationArray.GetItem(aIndex: Integer): TACBrPIXRecLocation;
begin
  Result := TACBrPIXRecLocation(inherited Items[aIndex]);
end;

procedure TACBrPIXRecLocationArray.SetItem(aIndex: Integer; Value: TACBrPIXRecLocation);
begin
  inherited Items[aIndex] := Value;
end;

function TACBrPIXRecLocationArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TACBrPIXRecLocationArray.Add(aRecLoc: TACBrPIXRecLocation): Integer;
begin
  Result := inherited Add(aRecLoc);
end;

procedure TACBrPIXRecLocationArray.Insert(aIndex: Integer; aRecLoc: TACBrPIXRecLocation);
begin
  inherited Insert(aIndex, aRecLoc);
end;

function TACBrPIXRecLocationArray.New: TACBrPIXRecLocation;
begin
  Result := TACBrPIXRecLocation.Create;
  Add(Result);
end;

{ TACBrPIXLocRecConsultaParametros }

function TACBrPIXLocRecConsultaParametros.GetPaginacao: TACBrPIXPaginacao;
begin
  if not Assigned(fPaginacao) then
    fPaginacao := TACBrPIXPaginacao.Create('paginacao');
  Result := fPaginacao;
end;

function TACBrPIXLocRecConsultaParametros.GetRecebedor: TACBrPIXRecSolicRecebedor;
begin
  if not Assigned(fRecebedor) then
    fRecebedor := TACBrPIXRecSolicRecebedor.Create('recebedor');
  Result := fRecebedor;
end;

procedure TACBrPIXLocRecConsultaParametros.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('inicio', DateTimeToISO8601(finicio))
    .AddPair('fim', DateTimeToISO8601(ffim))
    .AddPair('idRecPresente', fidRecPresente);

  recebedor.WriteToJSon(AJSon);
  paginacao.WriteToJSon(AJSon);
end;

procedure TACBrPIXLocRecConsultaParametros.DoReadFromJSon(AJSon: TACBrJSONObject);
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
    .Value('idRecPresente', fidRecPresente);

  if NaoEstaVazio(wInicio) then
    finicio := ISO8601ToDateTime(wInicio);
  if NaoEstaVazio(wFim) then
    ffim := ISO8601ToDateTime(wFim);

  recebedor.ReadFromJSon(AJSon);
  paginacao.ReadFromJSon(AJSon);
end;

constructor TACBrPIXLocRecConsultaParametros.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXLocRecConsultaParametros.Destroy;
begin
  if Assigned(fPaginacao) then
    fPaginacao.Free;
  if Assigned(fRecebedor) then
    fRecebedor.Free;
  inherited Destroy;
end;

procedure TACBrPIXLocRecConsultaParametros.Clear;
begin
  finicio := 0;
  ffim := 0;
  fidRecPresente := False;
  if Assigned(fPaginacao) then
    fPaginacao.Clear;
  if Assigned(fRecebedor) then
    fRecebedor.Clear;
end;

function TACBrPIXLocRecConsultaParametros.IsEmpty: Boolean;
begin
  Result := EstaZerado(finicio) and
            EstaZerado(ffim) and
            (not fidRecPresente) and
            ((not Assigned(fPaginacao)) or fPaginacao.IsEmpty) and
            ((not Assigned(fRecebedor)) or fRecebedor.IsEmpty);
end;

procedure TACBrPIXLocRecConsultaParametros.Assign(Source: TACBrPIXLocRecConsultaParametros);
begin
  Clear;
  if not Assigned(Source) then Exit;

  finicio := Source.inicio;
  ffim := Source.fim;
  fidRecPresente := Source.idRecPresente;
  paginacao.Assign(Source.paginacao);
  recebedor.Assign(Source.recebedor);
end;

{ TACBrPIXRecLocConsultadas }

function TACBrPIXRecLocConsultadas.GetLoc: TACBrPIXRecLocationArray;
begin
  if not Assigned(fLoc) then
    fLoc := TACBrPIXRecLocationArray.Create('loc');
  Result := fLoc;
end;

function TACBrPIXRecLocConsultadas.GetParametros: TACBrPIXLocRecConsultaParametros;
begin
  if not Assigned(fParametros) then
    fParametros := TACBrPIXLocRecConsultaParametros.Create('parametros');
  Result := fParametros;
end;

procedure TACBrPIXRecLocConsultadas.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  parametros.WriteToJSon(AJSon);
  loc.WriteToJSon(AJSon);
end;

procedure TACBrPIXRecLocConsultadas.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  parametros.ReadFromJSon(AJSon);
  loc.ReadFromJSon(AJSon);
end;

constructor TACBrPIXRecLocConsultadas.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

destructor TACBrPIXRecLocConsultadas.Destroy;
begin
  if Assigned(fParametros) then
    fParametros.Free;
  if Assigned(fLoc) then
    fLoc.Free;
end;

procedure TACBrPIXRecLocConsultadas.Clear;
begin
  if Assigned(fParametros) then
    fParametros.Clear;
  if Assigned(fLoc) then
    fLoc.Clear;
end;

function TACBrPIXRecLocConsultadas.IsEmpty: Boolean;
begin
  Result := ((not Assigned(fParametros)) or fParametros.IsEmpty) and
            ((not Assigned(fLoc)) or fLoc.IsEmpty);
end;

procedure TACBrPIXRecLocConsultadas.Assign(Source: TACBrPIXRecLocConsultadas);
begin
  Clear;
  if not Assigned(Source) then Exit;

  parametros.Assign(Source.parametros);
  loc.Assign(Source.loc);
end;

end.

