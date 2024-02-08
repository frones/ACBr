{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Cristian Carvalho                                                          }
{ - Sidnei Alves                                                               }
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

  Documentação
  https://developers.bancointer.com.br

*)

{$I ACBr.inc}

unit ACBrPIXPSPGate2All;

interface

uses
  Classes, SysUtils,
  {$IFDEF RTL230_UP}ACBrBase,{$ENDIF RTL230_UP}
  ACBrPIXCD, ACBrPIXBase, ACBrSchemasGate2All,
  ACBrPIXSchemasProblema, ACBrOpenSSLUtils;

const
  cGate2AllURLSandbox = 'https://apidemo.gate2all.com.br';
  cGate2AllURLProducao = 'https://api.gate2all.com.br';
  cGate2AllEndpointVoid = 'void';
  cGate2AllEndpointTransactions = '/v1/transactions';
  cGate2AllHeaderAuthenticationApi = 'authenticationApi';
  cGate2AllHeaderAuthenticationKey = 'authenticationKey';

type

  { TACBrPSPGate2All }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPGate2All = class(TACBrPSP)
  private
    FAuthenticationApi: String;
    FAuthenticationKey: String;

    procedure DoQuandoAcessarEndpoint(const aEndPoint: String; var aURL: String;
      var aMethod: String);
    procedure DoQuandoReceberRespostaEndPoint(const aEndPoint, aURL, aMethod: String;
      var aResultCode: Integer; var aRespostaHttp: AnsiString);

    function CobSolicitadaParaPixSolicitado: String;
    function PixGeradoParaCobGerada(aResponseJSON: String): String;
    function PixConsultadoParaCobCompleta(aResponseJSON: String): String;
    function PixConsultadoParaPix(aResponseJSON: String): String;
    
    function CobStatusParaTransactionStatus(aCobStatus: TACBrPIXStatusCobranca): TGate2AllTransactionStatus;
    function TransactionStatusParaCosStatus(aTransactionStatus: TGate2AllTransactionStatus): TACBrPIXStatusCobranca;
  protected
    function ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String; override;
    function CalcularEndPointPath(const Method, EndPoint: String): String; override;
    procedure ConfigurarBody(const Method, EndPoint: String; var aBody: String); override;
    procedure ConfigurarPathParameters(const aMethod, aEndPoint: String); override;
    procedure ConfigurarQueryParameters(const Method, EndPoint: String); override;
    procedure ConfigurarHeaders(const Method, AURL: String); override;
    procedure TratarRetornoComErro(ResultCode: Integer; const RespostaHttp: AnsiString; Problema: TACBrPIXProblema); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Autenticar; override;
  published
    property AuthenticationApi: String read FAuthenticationApi write FAuthenticationApi;
    property AuthenticationKey: String read FAuthenticationKey write FAuthenticationKey;
  end;

implementation

uses
  synautil, DateUtils, StrUtils, ACBrJSON, ACBrPIXSchemasCob, ACBrPIXSchemasPix,
  ACBrPIXBRCode, ACBrUtil.Strings, ACBrUtil.Base;

{ TACBrPSPGate2All }

procedure TACBrPSPGate2All.DoQuandoAcessarEndpoint(const aEndPoint: String;
  var aURL: String; var aMethod: String);
begin
  // Validando os métodos que não existem na API
  if ((aEndPoint = cEndPointPix) and (aMethod <> ChttpMethodGET)) or
      (aEndPoint = cEndPointCobV)  or (aMethod = ChttpMethodDELETE) then
    raise EACBrPSPException.Create(sErroEndpointNaoImplementado);

  if (aEndPoint = cEndPointCob) then
  begin
    if (aMethod = ChttpMethodPUT) then
      aMethod := ChttpMethodPOST
    else if (aMethod = ChttpMethodPATCH) then
      aMethod := ChttpMethodPUT;
  end;
end;

procedure TACBrPSPGate2All.DoQuandoReceberRespostaEndPoint(const aEndPoint,
  aURL, aMethod: String; var aResultCode: Integer; var aRespostaHttp: AnsiString
  );
begin
  if ((aResultCode = HTTP_OK) or (aResultCode = HTTP_CREATED)) then
  begin
    if (aEndPoint = cEndPointCob) then
    begin
      if ((aMethod = ChttpMethodPOST) or (aMethod = ChttpMethodPUT)) then
        aRespostaHttp := PixGeradoParaCobGerada(aRespostaHttp)
      else if (aMethod = ChttpMethodGET) then
        aRespostaHttp := PixConsultadoParaCobCompleta(aRespostaHttp)
    end
    else if (aEndPoint = cEndPointPix) and (aMethod = ChttpMethodGET) then
      aRespostaHttp := PixConsultadoParaPix(aRespostaHttp);
  end;
end;

function TACBrPSPGate2All.CobSolicitadaParaPixSolicitado: String;
var
  wPixSolicitado: TGate2AllTransaction;
begin
  wPixSolicitado := TGate2AllTransaction.Create;
  try
    wPixSolicitado.description := IfThen(NaoEstaVazio(
      epCob.CobSolicitada.solicitacaoPagador),
      epCob.CobSolicitada.solicitacaoPagador, 'Produto/Servico');
    wPixSolicitado.amount := epCob.CobSolicitada.valor.original;
    wPixSolicitado.payment.pix.provider := gppC6Bank;
    wPixSolicitado.payment.pix.key := gpkRandomKey;
    wPixSolicitado.payment.pix.expirationDateTime := IncSecond(Now, epCob.CobSolicitada.calendario.expiracao);
    Result := wPixSolicitado.AsJSON;
  finally
    wPixSolicitado.Free;
  end;
end;

function TACBrPSPGate2All.PixGeradoParaCobGerada(aResponseJSON: String): String;
var
  wCob: TACBrPIXCobGerada;
  wQRCode: TACBrPIXQRCodeDinamico;
  wPixGerado: TGate2AllTransactionResponse;
begin
  wCob := TACBrPIXCobGerada.Create('');
  wPixGerado := TGate2AllTransactionResponse.Create;
  try
    wPixGerado.AsJSON := aResponseJSON;
    wCob.txId := wPixGerado.transactionId;
    wCob.calendario.criacao := wPixGerado.dtTransaction;
    wCob.calendario.expiracao :=
      SecondsBetween(wPixGerado.dtTransaction, wPixGerado.payment.pix.expirationDateTime);
    wCob.status := TransactionStatusParaCosStatus(wPixGerado.status);
    wCob.pixCopiaECola := wPixGerado.payment.pix.qrcode;
    wCob.valor.original := wPixGerado.amount;

    wQRCode := TACBrPIXQRCodeDinamico.Create;
    try
      wQRCode.AsString := wCob.pixCopiaECola;
      wCob.location := wQRCode.URL;
      wCob.loc.tipoCob := tcoCob;
      wCob.loc.location := wQRCode.URL;
    finally
      wQRCode.Free;
    end;

    Result := wCob.AsJSON;
  finally
    wCob.Free;
    wPixGerado.Free;
  end;
end;

function TACBrPSPGate2All.PixConsultadoParaCobCompleta(aResponseJSON: String): String;
var
  wCob: TACBrPIXCobCompleta;
  wQRCode: TACBrPIXQRCodeDinamico;
  wPixConsultado: TGate2AllTransactionResponse;
begin
  wCob := TACBrPIXCobCompleta.Create('');
  wPixConsultado := TGate2AllTransactionResponse.Create;
  try
    wPixConsultado.AsJSON := aResponseJSON;
    wCob.txId := wPixConsultado.transactionId;
    wCob.calendario.criacao := wPixConsultado.dtTransaction;
    wCob.calendario.expiracao :=
      SecondsBetween(wPixConsultado.dtTransaction, wPixConsultado.payment.pix.expirationDateTime);
    wCob.status := TransactionStatusParaCosStatus(wPixConsultado.status);
    wCob.pixCopiaECola := wPixConsultado.payment.pix.qrcode;
    wCob.valor.original := wPixConsultado.amount;

    if (wPixConsultado.payment.pix.paymentAmount > 0) then
    with wCob.pix.New do
    begin
      txid := wPixConsultado.transactionId;
      endToEndId := wPixConsultado.transactionId;
      horario := wPixConsultado.payment.pix.paymentDate;
      valor := wPixConsultado.payment.pix.paymentAmount;
    end;

    wQRCode := TACBrPIXQRCodeDinamico.Create;
    try
      wQRCode.AsString := wCob.pixCopiaECola;
      wCob.location := wQRCode.URL;
      wCob.loc.tipoCob := tcoCob;
      wCob.loc.location := wQRCode.URL;
    finally
      wQRCode.Free;
    end;

    Result := wCob.AsJSON;
  finally
    wCob.Free;
    wPixConsultado.Free;
  end;
end;

function TACBrPSPGate2All.PixConsultadoParaPix(aResponseJSON: String): String;
var
  wPix: TACBrPIX;
  wPixConsultado: TGate2AllTransactionResponse;
begin
  wPix := TACBrPIX.Create('');
  wPixConsultado := TGate2AllTransactionResponse.Create;
  try 
    wPixConsultado.AsJSON := aResponseJSON;
    if (wPixConsultado.payment.pix.paymentAmount > 0) then
    begin
      wPix.txid := wPixConsultado.transactionId;
      wPix.endToEndId := wPixConsultado.transactionId;
      wPix.horario := wPixConsultado.payment.pix.paymentDate;
      wPix.valor := wPixConsultado.payment.pix.paymentAmount;
    end;

    Result := wPix.AsJSON;
  finally
    wPix.Free;
    wPixConsultado.Free;
  end;
end;

function TACBrPSPGate2All.CobStatusParaTransactionStatus(aCobStatus: TACBrPIXStatusCobranca): TGate2AllTransactionStatus;
begin
  case aCobStatus of
    stcREMOVIDA_PELO_PSP, stcREMOVIDA_PELO_USUARIO_RECEBEDOR: Result := gtsCancelada;
    stcATIVA: Result := gtsAguardandoPagamento;
    stcCONCLUIDA: Result := gtsConfirmada;
    else
      Result := gtsNone;
  end;
end;

function TACBrPSPGate2All.TransactionStatusParaCosStatus(aTransactionStatus: TGate2AllTransactionStatus): TACBrPIXStatusCobranca;
begin
  case aTransactionStatus of
    gtsTransacaoIniciada, gtsAguardandoPagamento, gtsCancelamentoEmAndamento,
      gtsPendenteDeConfirmacao, gtsEmAnalise: Result := stcATIVA;
    gtsExpirada, gtsNegada, gtsFalhaNaComunicacao: Result := stcREMOVIDA_PELO_PSP;
    gtsAutorizada, gtsConfirmada: Result := stcCONCLUIDA;
    gtsCancelada, gtsIntencaoCancelada: Result := stcREMOVIDA_PELO_USUARIO_RECEBEDOR;
  else
    Result := stcNENHUM;
  end;
end;

function TACBrPSPGate2All.ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String;
begin
  if (aAmbiente = ambProducao) then
    Result := cGate2AllURLProducao
  else
    Result := cGate2AllURLSandbox;
end;

function TACBrPSPGate2All.CalcularEndPointPath(const Method, EndPoint: String): String;
begin
  if (EndPoint = cEndPointCob) or (EndPoint = cEndPointPix) then
    Result := cGate2AllEndpointTransactions;
end;

procedure TACBrPSPGate2All.ConfigurarBody(const Method, EndPoint: String;
  var aBody: String);
begin
  if (EndPoint = cEndPointCob) then
  begin
    if ((Method = ChttpMethodPUT) or (Method = ChttpMethodPOST)) then
      aBody := CobSolicitadaParaPixSolicitado
    else if (Method = ChttpMethodPATCH) then
      aBody := EmptyStr;
  end
  else if (EndPoint = cEndPointPix) then
    aBody := EmptyStr;
end;

procedure TACBrPSPGate2All.ConfigurarPathParameters(const aMethod, aEndPoint: String);
begin
  if (URLPathParams.Count > 1) then
    raise EACBrPSPException.Create(sErroEndpointNaoImplementado);

  if (aEndPoint = cEndPointCob) then
  begin
    if (aMethod = ChttpMethodPUT) then
      URLPathParams.Clear
    else if (aMethod = ChttpMethodPATCH) then
      URLPathParams.Add(cGate2AllEndpointVoid);
  end;
end;

procedure TACBrPSPGate2All.ConfigurarQueryParameters(const Method,
  EndPoint: String);
begin
  if (URLQueryParams.Count > 0) then
    raise EACBrPSPException.Create(sErroEndpointNaoImplementado);
end;

procedure TACBrPSPGate2All.ConfigurarHeaders(const Method, AURL: String);
begin
  inherited ConfigurarHeaders(Method, AURL);

  if NaoEstaVazio(AuthenticationApi) then
    Http.Headers.Add(cGate2AllHeaderAuthenticationApi + ': ' + FAuthenticationApi);

  if NaoEstaVazio(AuthenticationKey) then
    Http.Headers.Add(cGate2AllHeaderAuthenticationKey + ': ' + FAuthenticationKey);
end;

procedure TACBrPSPGate2All.TratarRetornoComErro(ResultCode: Integer; const RespostaHttp: AnsiString; Problema: TACBrPIXProblema);
var
  wErro: TGate2AllResponseError;
begin
  if (NivelLog > 2) then
    RegistrarLog('TratarRetornoComErro( ' + IntToStr(ResultCode) + ' )');

  if (ResultCode = HTTP_UNAUTHORIZED) then
    fpAutenticado := False;

  Problema.Clear;
  if (Trim(RespostaHttp) = '') then
    AtribuirErroHTTPProblema(Problema)
  else
  begin
    wErro := TGate2AllResponseError.Create;
    try
      wErro.AsJSON := string(RespostaHttp);

      Problema.title := wErro.error;
      Problema.status := wErro.status;
      Problema.detail := wErro.path;
    finally
      wErro.Free;
    end;
  end;
end;

constructor TACBrPSPGate2All.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpIsBacen := False;
  fpQuandoReceberRespostaEndPoint := DoQuandoReceberRespostaEndPoint;
  fpQuandoAcessarEndPoint := DoQuandoAcessarEndpoint;
end;

procedure TACBrPSPGate2All.Autenticar;
begin
  LimparHTTP;
  fpAutenticado := True;
end;

end.

