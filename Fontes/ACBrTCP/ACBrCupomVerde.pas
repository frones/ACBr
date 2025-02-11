{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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

unit ACBrCupomVerde;

interface

uses
  Classes, SysUtils,
  ACBrBase,
  ACBrJSON,
  ACBrSocket,
  ACBrAPIBase;

const
  cCupomVerdeURLProducao = 'https://api.cupomverde.com.br/api/prod/v2/integracao';
  cCupomVerdeURLHomologacao = 'https://api.cupomverde.com.br/api/hom/v2/integracao';
  cCupomVerdeEndpointCancelamentos = 'cancelamentos';
  cCupomVerdeEndpointUsuarios = 'usuarios';
  cCupomVerdeEndpointUpload = 'upload';

type

  TACBrCupomVerdeAmbiente = (
    cvaNenhum,
    cvaHomologacao,
    cvaProducao);

  TACBrCupomVerdeImpressao = (
    cviNenhum,
    cviNaoImprimir,
    cviReduzido,
    cviCompleto
  );

  { TACBrCupomVerdeErro }

  TACBrCupomVerdeErro = class(TACBrAPISchema)
  private
    fname: String;
    fmessage: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrCupomVerdeErro);

    property name: String read fname write fname;
    property message: String read fmessage write fmessage;
  end;

  { TACBrCupomVerdeComprovante }

  TACBrCupomVerdeComprovante = class(TACBrAPISchema)
  private
    fdescricao: String;
    fparcelas: Integer;
    fvalorTotal: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrCupomVerdeComprovante);

    property descricao: String read fdescricao write fdescricao;
    property parcelas: Integer read fparcelas write fparcelas;
    property valorTotal: Double read fvalorTotal write fvalorTotal;
  end;

  { TACBrCupomVerdeComprovantes }

  TACBrCupomVerdeComprovantes = class(TACBrAPISchemaArray)
  private
    function GetItem(Index: Integer): TACBrCupomVerdeComprovante;
    procedure SetItem(Index: Integer; aValue: TACBrCupomVerdeComprovante);
  public
    Function Add(aItem: TACBrCupomVerdeComprovante): Integer;
    Procedure Insert(Index: Integer; aItem: TACBrCupomVerdeComprovante);
    function New: TACBrCupomVerdeComprovante;
    property Items[Index: Integer]: TACBrCupomVerdeComprovante read GetItem write SetItem; default;
  end;

  { TACBrCupomVerdeXMLRequisicao }

  TACBrCupomVerdeXMLRequisicao = class(TACBrAPISchema)
  private
    fxml: AnsiString;
    fcpf: String;
    fcodigoOperador: String;
    fcodDocumento: String;
    fcomprovantesPagamento: TACBrCupomVerdeComprovantes;
    function GetcomprovantesPagamento: TACBrCupomVerdeComprovantes;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrCupomVerdeXMLRequisicao);

    property xml: AnsiString read fxml write fxml;
    property cpf: String read fcpf write fcpf;
    property codigoOperador: String read fcodigoOperador write fcodigoOperador;
    property codDocumento: String read fcodDocumento write fcodDocumento;
    property comprovantesPagamento: TACBrCupomVerdeComprovantes read GetcomprovantesPagamento;
  end;

  { TACBrCupomVerdeXMLResposta }

  TACBrCupomVerdeXMLResposta = class(TACBrAPISchema)
  private
    fimpressao: TACBrCupomVerdeImpressao;
    fmensagem: String;
    fqrCode: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJson(aJson: TACBrJSONObject); override;
    procedure DoReadFromJson(aJson: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(Source: TACBrCupomVerdeXMLResposta);

    property impressao: TACBrCupomVerdeImpressao read fimpressao write fimpressao;
    property mensagem: String read fmensagem write fmensagem;
    property qrCode: String read fqrCode write fqrCode;
  end;

  { TACBrCupomVerdeConsultaResposta }

  TACBrCupomVerdeConsultaResposta = class(TACBrAPISchema)
  private
    fcupomSempreVerde: Boolean;
    fdocumento: String;
    fimpressao: TACBrCupomVerdeImpressao;
    fmensagem: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrCupomVerdeConsultaResposta);

    property impressao: TACBrCupomVerdeImpressao read fimpressao write fimpressao;
    property mensagem: String read fmensagem write fmensagem;
    property documento: String read fdocumento write fdocumento;
    property cupomSempreVerde: Boolean read fcupomSempreVerde write fcupomSempreVerde;
  end;

  { TACBrCupomVerde }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCupomVerde = class(TACBrHTTP)
  private
    fxApiKey: String;
    fAmbiente: TACBrCupomVerdeAmbiente;
    fRespostaErro: TACBrCupomVerdeErro;
    fXMLEnviado: TACBrCupomVerdeXMLRequisicao;
    fRespostaEnviar: TACBrCupomVerdeXMLResposta;
    fRespostaConsulta: TACBrCupomVerdeConsultaResposta;

    function CalcularURL: String;
    function GetRespostaConsulta: TACBrCupomVerdeConsultaResposta;
    function GetRespostaEnviar: TACBrCupomVerdeXMLResposta;
    function GetRespostaErro: TACBrCupomVerdeErro;
    function GetXMLEnviado: TACBrCupomVerdeXMLRequisicao;

    procedure ValidarConfiguracao;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;

    function EnviarXML: Boolean;
    function ConsultarCPF(const aCPF: String): Boolean;
    function CancelarDocumento(const aChave: String): Boolean;

    property XMLEnviado: TACBrCupomVerdeXMLRequisicao read GetXMLEnviado;
    property RespostaEnviar: TACBrCupomVerdeXMLResposta read GetRespostaEnviar;
    property RespostaConsulta: TACBrCupomVerdeConsultaResposta read GetRespostaConsulta;
    property RespostaErro: TACBrCupomVerdeErro read GetRespostaErro;
  published
    property xApiKey: String read fxApiKey write fxApiKey;
    property Ambiente: TACBrCupomVerdeAmbiente read fAmbiente write fAmbiente;
  end;

  function ImpressaoToString(aValue: TACBrCupomVerdeImpressao): String;
  function StringToImpressao(aStr: String): TACBrCupomVerdeImpressao;

implementation

uses
  synautil, synacode,
  ACBrUtil.Base;

function ImpressaoToString(aValue: TACBrCupomVerdeImpressao): String;
begin
  Result := EmptyStr;
  case aValue of
    cviNaoImprimir: Result := 'NAO_IMPRIMIR';
    cviReduzido: Result := 'REDUZIDO';
    cviCompleto: Result := 'COMPLETO';
  end;
end;

function StringToImpressao(aStr: String): TACBrCupomVerdeImpressao;
begin
  Result := cviNenhum;
  if aStr = 'NAO_IMPRIMIR' then
    Result := cviNaoImprimir
  else if aStr = 'REDUZIDO' then
    Result := cviReduzido
  else if aStr = 'COMPLETO' then
    Result := cviCompleto;
end;

{ TACBrCupomVerdeXMLResposta }

procedure TACBrCupomVerdeXMLResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrCupomVerdeXMLResposta) then
    Assign(TACBrCupomVerdeXMLResposta(aSource));
end;

procedure TACBrCupomVerdeXMLResposta.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .AddPair('impressao', ImpressaoToString(fimpressao))
    .AddPair('mensagem', fmensagem)
    .AddPair('qrCode', fqrCode);
end;

procedure TACBrCupomVerdeXMLResposta.DoReadFromJson(aJson: TACBrJSONObject);
var
  s: String;
begin
  if (not Assigned(aJson)) then
    Exit;

  {$IfDef FPC}s := EmptyStr;{$EndIf}
  aJson
    .Value('impressao', s)
    .Value('mensagem', fmensagem)
    .Value('qrCode', fqrCode);

  if NaoEstaVazio(s) then
    fimpressao := StringToImpressao(s);
end;

procedure TACBrCupomVerdeXMLResposta.Clear;
begin
  fimpressao := cviNenhum;
  fmensagem := EmptyStr;
  fqrCode := EmptyStr;
end;

function TACBrCupomVerdeXMLResposta.IsEmpty: Boolean;
begin
  Result := (fimpressao = cviNenhum) and
    EstaVazio(fmensagem) and
    EstaVazio(fqrCode);
end;

procedure TACBrCupomVerdeXMLResposta.Assign(Source: TACBrCupomVerdeXMLResposta);
begin
  if (not Assigned(Source)) then
    Exit;

  fimpressao := Source.impressao;
  fmensagem := Source.mensagem;
  fqrCode := Source.qrCode;
end;

{ TACBrCupomVerdeXMLRequisicao }

function TACBrCupomVerdeXMLRequisicao.GetcomprovantesPagamento: TACBrCupomVerdeComprovantes;
begin
  if (not Assigned(fcomprovantesPagamento)) then
    fcomprovantesPagamento := TACBrCupomVerdeComprovantes.Create('comprovantesPagamento');
  Result := fcomprovantesPagamento;
end;

procedure TACBrCupomVerdeXMLRequisicao.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrCupomVerdeXMLRequisicao) then
    Assign(TACBrCupomVerdeXMLRequisicao(aSource));
end;

procedure TACBrCupomVerdeXMLRequisicao.DoWriteToJson(aJson: TACBrJSONObject);
var
  b64: AnsiString;
begin
  if (not Assigned(aJson)) then
    Exit;

  b64 := EncodeBase64(fxml);
  aJson
    .AddPair('xml', b64)
    .AddPair('cpf', fcpf)
    .AddPair('codigoOperador', fcodigoOperador)
    .AddPair('codDocumento', fcodDocumento);
  if Assigned(fcomprovantesPagamento) then
    fcomprovantesPagamento.WriteToJSon(aJson);
end;

procedure TACBrCupomVerdeXMLRequisicao.DoReadFromJson(aJson: TACBrJSONObject);
var
  s: String;
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .Value('xml',  s)
    .Value('cpf', fcpf)
    .Value('codigoOperador', fcodigoOperador)
    .Value('codDocumento', fcodDocumento);
  if NaoEstaVazio(s) then
    fxml := DecodeBase64(s);
  if Assigned(fcomprovantesPagamento) then
    fcomprovantesPagamento.ReadFromJson(aJson);
end;

destructor TACBrCupomVerdeXMLRequisicao.Destroy;
begin
  if Assigned(fcomprovantesPagamento) then
    fcomprovantesPagamento.Free;
  inherited Destroy;
end;

procedure TACBrCupomVerdeXMLRequisicao.Clear;
begin
  fxml := EmptyStr;
  fcpf := EmptyStr;
  fcodigoOperador := EmptyStr;
  fcodDocumento := EmptyStr;
  if Assigned(fcomprovantesPagamento) then
    fcomprovantesPagamento.Clear;
end;

function TACBrCupomVerdeXMLRequisicao.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fxml) and
    EstaVazio(fcpf) and
    EstaVazio(fcodigoOperador) and
    EstaVazio(fcodDocumento) and
    (not Assigned(fcomprovantesPagamento) or fcomprovantesPagamento.IsEmpty);
end;

procedure TACBrCupomVerdeXMLRequisicao.Assign(Source: TACBrCupomVerdeXMLRequisicao);
begin
  if (not Assigned(Source)) then
    Exit;

  fxml := Source.xml;
  fcpf := Source.cpf;
  fcodigoOperador := Source.codigoOperador;
  fcodDocumento := Source.codDocumento;
  comprovantesPagamento.Assign(Source.comprovantesPagamento);
end;

{ TACBrCupomVerdeComprovantes }

function TACBrCupomVerdeComprovantes.GetItem(Index: Integer): TACBrCupomVerdeComprovante;
begin
  Result := TACBrCupomVerdeComprovante(inherited Items[Index]);
end;

procedure TACBrCupomVerdeComprovantes.SetItem(Index: Integer; aValue: TACBrCupomVerdeComprovante);
begin
  inherited Items[Index] := aValue;
end;

function TACBrCupomVerdeComprovantes.Add(aItem: TACBrCupomVerdeComprovante): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrCupomVerdeComprovantes.Insert(Index: Integer; aItem: TACBrCupomVerdeComprovante);
begin
  inherited Insert(Index, aItem);
end;

function TACBrCupomVerdeComprovantes.New: TACBrCupomVerdeComprovante;
begin
  Result := TACBrCupomVerdeComprovante.Create;
  Self.Add(Result);
end;

{ TACBrCupomVerdeComprovante }

procedure TACBrCupomVerdeComprovante.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrCupomVerdeComprovante) then
    Assign(TACBrCupomVerdeComprovante(aSource));
end;

procedure TACBrCupomVerdeComprovante.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .AddPair('descricao', fdescricao)
    .AddPair('parcelas', fparcelas)
    .AddPair('valorTotal', fvalorTotal);
end;

procedure TACBrCupomVerdeComprovante.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;

  aJson
    .Value('descricao', fdescricao)
    .Value('parcelas', fparcelas)
    .Value('valorTotal', fvalorTotal);
end;

procedure TACBrCupomVerdeComprovante.Clear;
begin
  fdescricao := EmptyStr;
  fparcelas := 0;
  fvalorTotal := 0;
end;

function TACBrCupomVerdeComprovante.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fdescricao) and
    EstaZerado(fparcelas) and
    EstaZerado(fvalorTotal);
end;

procedure TACBrCupomVerdeComprovante.Assign(Source: TACBrCupomVerdeComprovante);
begin
  if (not Assigned(Source)) then
    Exit;

  fdescricao := Source.descricao;
  fparcelas := Source.parcelas;
  fvalorTotal := Source.valorTotal;
end;

{ TACBrCupomVerdeErro }

procedure TACBrCupomVerdeErro.AssignSchema(aSource: TACBrAPISchema);
begin
  if Assigned(aSource) and (aSource is TACBrCupomVerdeErro) then
    Assign(TACBrCupomVerdeErro(aSource));
end;

procedure TACBrCupomVerdeErro.DoWriteToJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;
  aJson
    .AddPair('name', fname)
    .AddPair('message', fmessage);
end;

procedure TACBrCupomVerdeErro.DoReadFromJson(aJson: TACBrJSONObject);
begin
  if (not Assigned(aJson)) then
    Exit;
  aJson
    .Value('name', fname)
    .Value('message', fmessage);
end;

procedure TACBrCupomVerdeErro.Clear;
begin
  fname := EmptyStr;
  fmessage := EmptyStr;
end;

function TACBrCupomVerdeErro.IsEmpty: Boolean;
begin
  Result := EstaVazio(fname) and EstaVazio(fmessage);
end;

procedure TACBrCupomVerdeErro.Assign(Source: TACBrCupomVerdeErro);
begin
  if (not Assigned(Source)) then
    Exit;
  fname := Source.name;
  fmessage := Source.message;
end;

{ TACBrCupomVerdeConsultaResposta }

procedure TACBrCupomVerdeConsultaResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (ASource is TACBrCupomVerdeConsultaResposta) then
    Assign(TACBrCupomVerdeConsultaResposta(ASource));
end;

procedure TACBrCupomVerdeConsultaResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (not Assigned(aJSon)) then
    Exit;

  aJson
    .AddPair('impressao', ImpressaoToString(fimpressao))
    .AddPair('mensagem', fmensagem)
    .AddPair('documento', fdocumento)
    .AddPair('cupomSempreVerde', fcupomSempreVerde);
end;

procedure TACBrCupomVerdeConsultaResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  if (not Assigned(aJSon)) then
    Exit;

  {$IfDef FPC}s := EmptyStr;{$EndIf}
  aJson
    .Value('impressao', s)
    .Value('mensagem', fmensagem)
    .Value('documento', fdocumento)
    .Value('cupomSempreVerde', fcupomSempreVerde);

  if NaoEstaVazio(s) then
    fimpressao := StringToImpressao(s);
end;

procedure TACBrCupomVerdeConsultaResposta.Clear;
begin
  fimpressao := cviNenhum;
  fmensagem := EmptyStr;
  fdocumento := EmptyStr;
  fcupomSempreVerde := False;
end;

function TACBrCupomVerdeConsultaResposta.IsEmpty: Boolean;
begin
  Result := (fimpressao = cviNenhum) and (not fcupomSempreVerde) and
    EstaVazio(fmensagem) and
    EstaVazio(fdocumento);
end;

procedure TACBrCupomVerdeConsultaResposta.Assign(aSource: TACBrCupomVerdeConsultaResposta);
begin
  if (not Assigned(aSource)) then
    Exit;

  fimpressao := aSource.impressao;
  fmensagem := aSource.mensagem;
  fdocumento := aSource.documento;
  fcupomSempreVerde := aSource.cupomSempreVerde;
end;

{ TACBrCupomVerde }

function TACBrCupomVerde.CalcularURL: String;
begin
  Result := EmptyStr;
  if (Ambiente = cvaProducao) then
    Result := cCupomVerdeURLProducao
  else if (Ambiente = cvaHomologacao) then
    Result := cCupomVerdeURLHomologacao;
end;

function TACBrCupomVerde.GetRespostaConsulta: TACBrCupomVerdeConsultaResposta;
begin
  if (not Assigned(fRespostaConsulta)) then
    fRespostaConsulta := TACBrCupomVerdeConsultaResposta.Create;
  Result := fRespostaConsulta;
end;

function TACBrCupomVerde.GetRespostaEnviar: TACBrCupomVerdeXMLResposta;
begin
  if (not Assigned(fRespostaEnviar)) then
    fRespostaEnviar := TACBrCupomVerdeXMLResposta.Create;
  Result := fRespostaEnviar;
end;

function TACBrCupomVerde.GetRespostaErro: TACBrCupomVerdeErro;
begin
  if (not Assigned(fRespostaErro)) then
    fRespostaErro := TACBrCupomVerdeErro.Create;
  Result := fRespostaErro;
end;

function TACBrCupomVerde.GetXMLEnviado: TACBrCupomVerdeXMLRequisicao;
begin
  if (not Assigned(fXMLEnviado)) then
    fXMLEnviado := TACBrCupomVerdeXMLRequisicao.Create;
  Result := fXMLEnviado;
end;

procedure TACBrCupomVerde.ValidarConfiguracao;
var
  wErro: String;
begin
  {$IfDef FPC}wErro := EmptyStr;{$EndIf}
  if (Ambiente = cvaNenhum) then
    wErro := '- Informe o ambiente' + sLineBreak;
  if EstaVazio(xApiKey) then
    wErro := wErro + '- Informe o x-api-key';
  if NaoEstaVazio(wErro) then
    raise Exception.Create('ERRO: ' + sLineBreak + wErro);
end;

constructor TACBrCupomVerde.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  xApiKey := EmptyStr;
  Ambiente := cvaNenhum;
  Clear;
end;

destructor TACBrCupomVerde.Destroy;
begin
  if Assigned(fRespostaConsulta) then
    fRespostaConsulta.Free;
  if Assigned(fRespostaErro) then
    fRespostaErro.Free;
  if Assigned(fRespostaEnviar) then
    fRespostaEnviar.Free;
  if Assigned(fXMLEnviado) then
    fXMLEnviado.Free;
  inherited Destroy;
end;

procedure TACBrCupomVerde.Clear;
begin
  if Assigned(fRespostaErro) then
    fRespostaErro.Clear;
  if Assigned(fRespostaEnviar) then
    fRespostaEnviar.Clear;
  if Assigned(fRespostaConsulta) then
    fRespostaConsulta.Clear;
  if Assigned(fXMLEnviado) then
    fXMLEnviado.Clear;
end;

function TACBrCupomVerde.ConsultarCPF(const aCPF: String): Boolean;
var
  jo: TACBrJSONObject;
  Body: String;
begin
  Result := False;
  ValidarConfiguracao;
  if EstaVazio(aCPF) then
    Exit;

  LimparHTTP;
  RegistrarLog('  TACBrCupomVerde.ConsultarCPF(CPF: ' + aCPF + ')');

  HttpSend.Protocol := '1.1';
  if NaoEstaVazio(xApiKey) then
    HTTPSend.Headers.Add('x-api-key: ' + xApiKey);
  HTTPSend.Headers.Add('x-sender-id: acbr');

  jo := TACBrJSONObject.Create;
  try
    jo.AddPair('documento', aCPF);
    Body := jo.ToJSON;
    WriteStrToStream(HTTPSend.Document, Body);
    HttpSend.MimeType := cContentTypeApplicationJSon;
    RegistrarLog('Req.Body: ' + Body);
  finally
    jo.Free;
  end;

  URLPathParams.Add(cCupomVerdeEndpointUsuarios);

  try
    HTTPMethod(cHTTPMethodPOST, CalcularURL);
    Result := (HTTPResultCode = HTTP_OK);
    if Result then
      RespostaConsulta.AsJSON := HTTPResponse
    else
      RespostaErro.AsJSON := HTTPResponse;
  except
    RespostaErro.AsJSON := HTTPResponse;
  end;
end;

function TACBrCupomVerde.EnviarXML: Boolean;
var
  Body: String;
begin
  Result := False;
  ValidarConfiguracao;
  if XMLEnviado.IsEmpty then
    Exit;

  LimparHTTP;
  RegistrarLog('  TACBrCupomVerde.EnviarXML');

  HttpSend.Protocol := '1.1';
  if NaoEstaVazio(xApiKey) then
    HTTPSend.Headers.Add('x-api-key: ' + xApiKey);
  HTTPSend.Headers.Add('x-sender-id: acbr');

  Body := XMLEnviado.AsJSON;
  RegistrarLog('Req.Body: ' + Body);
  WriteStrToStream(HTTPSend.Document, Body);
  HttpSend.MimeType := cContentTypeApplicationJSon;
  
  URLPathParams.Add(cCupomVerdeEndpointUpload);
  try
    HTTPMethod(cHTTPMethodPOST, CalcularURL);
    Result := (HTTPResultCode = HTTP_OK);
    if Result then
      RespostaEnviar.AsJSON := HTTPResponse
    else
      RespostaErro.AsJSON := HTTPResponse;
  except
    RespostaErro.AsJSON := HTTPResponse;
  end;
end;

function TACBrCupomVerde.CancelarDocumento(const aChave: String): Boolean;
begin
  Result := False;
  ValidarConfiguracao;
  if EstaVazio(aChave) then
    Exit;

  LimparHTTP;
  RegistrarLog('  TACBrCupomVerde.CancelarDocumento');

  HttpSend.Protocol := '1.1';
  if NaoEstaVazio(xApiKey) then
    HTTPSend.Headers.Add('x-api-key: ' + xApiKey);
  HTTPSend.Headers.Add('x-sender-id: acbr');

  URLPathParams.Add(cCupomVerdeEndpointCancelamentos);
  URLPathParams.Add(aChave);
  try
    HTTPMethod(cHTTPMethodPOST, CalcularURL);
    Result := (HTTPResultCode = HTTP_OK);
    if (not Result) then
      RespostaErro.AsJSON := HTTPResponse;
  except
    RespostaErro.AsJSON := HTTPResponse;
  end;
end;

end.
