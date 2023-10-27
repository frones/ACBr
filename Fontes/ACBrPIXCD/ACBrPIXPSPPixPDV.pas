{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elias César Vieira                              }
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
  https://pixpdv.com.br/api/index.html

*)

{$I ACBr.inc}

unit ACBrPIXPSPPixPDV;

interface

uses
  Classes, SysUtils,
  {$IFDEF RTL230_UP}ACBrBase,{$ENDIF RTL230_UP}
  ACBrPIXCD, ACBrPIXSchemasPixPDV, ACBrPIXBase;

const

  cPixPDVURLSandbox = 'https://pixpdv.com.br/api-h/v1';
  cPixPDVURLProducao = 'https://pixpdv.com.br/api/v1';
  cPixPDVHeaderPoweredBy = 'X-PoweredBy: Projeto ACBr';

  cPIXPDVEndPointStatusToken = '/statustoken';
  cPIXPDVEndPointQrDinamico = '/qrdinamico';
  cPIXPDVEndPointQrCobranca = '/qrcobranca';
  cPIXPDVEndPointQrStatus = '/qrstatus';
  cPIXPDVEndPointQrRefund = '/qrrefund';
  cPIXPDVEndPointQrResumo = '/qrresumo';
  cPIXPDVEndPointQrSimulaPagar = '/qrsimulateapproved';
  cPIXPDVEndPointSaldo = '/saldo';
  cPIXPDVEndPointRetirada = '/retirada';
  cPIXPDVEndPointExtrato = '/extrato';

type

  TACBrPSPPIXPDVTipoData = (tpdtEmissao, tpdtRecebimento, tpdtVencimento);

  { TACBrPSPPixPDV }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPPixPDV = class(TACBrPSP)
  private
    fCNPJ: String;
    fError: TPixPDVError;
    fToken: String;
    fStatusTokenDados: TPixPDVStatusTokenDados;
    fQrDinamico: TPixPDVQrDinamico;
    fQrCobranca: TPixPDVQrCobranca;
    fQrGerado: TPixPDVQrGerado;
    fQrStatus: TPixPDVQrStatus;
    fQrRefund: TPixPDVQrRefund;
    fQrResumo: TPixPDVQrResumo;
    fQrSimulaPagar: TPixPDVQrSimulaPagar;
    fSaldo: TPixPDVSaldo;
    fRetirada: TPixPDVRetirada;
    fExtrato: TPixPDVExtrato;
  protected
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;
    procedure ConfigurarAutenticacao(const Method, EndPoint: string); override;
    procedure ConfigurarHeaders(const Method, AURL: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;

    procedure PostStatusToken;
    procedure PostQrDinamico;
    procedure PostQrCobranca;
    procedure PostQrRefund(const aQrCodeId: string);
    procedure PostQrSimulaPagar(const aQrCodeId: string);
    procedure PostRetirada(aValor: Currency);

    procedure GetQrStatus(const aQRCodeId: string);
    procedure GetQrResumo(inicio, fim: TDateTime; tipo: TACBrPSPPIXPDVTipoData = tpdtEmissao);
    procedure GetSaldo;
    procedure GetExtrato(inicio, fim: TDateTime);

    property Error: TPixPDVError read fError;
    property StatusTokenDados: TPixPDVStatusTokenDados read fStatusTokenDados;
    property QrDinamico: TPixPDVQrDinamico read fQrDinamico;
    property QrCobranca: TPixPDVQrCobranca read fQrCobranca;
    property QrGerado: TPixPDVQrGerado read fQrGerado;
    property QrStatus: TPixPDVQrStatus read fQrStatus;
    property QrRefund: TPixPDVQrRefund read fQrRefund;
    property QrResumo: TPixPDVQrResumo read fQrResumo;
    property QrSimulaPagar: TPixPDVQrSimulaPagar read fQrSimulaPagar;
    property Saldo: TPixPDVSaldo read fSaldo;
    property Retirada: TPixPDVRetirada read fRetirada;
    property Extrato: TPixPDVExtrato read fExtrato;
  published
    property CNPJ: String read fCNPJ write fCNPJ;
    property Token: String read fToken write fToken;
    property ClientSecret;
  end;

implementation

uses
  synautil, DateUtils, ACBrJSON, ACBrUtil.Strings, ACBrUtil.Base,
  ACBrUtil.FilesIO, ACBrOpenSSLUtils;

{ TACBrPSPPixPDV }

constructor TACBrPSPPixPDV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fError            := TPixPDVError.Create('error');
  fStatusTokenDados := TPixPDVStatusTokenDados.Create('data');
  fQrDinamico       := TPixPDVQrDinamico.Create;
  fQrCobranca       := TPixPDVQrCobranca.Create;
  fQrGerado         := TPixPDVQrGerado.Create('data');
  fQrStatus         := TPixPDVQrStatus.Create('data');
  fQrRefund         := TPixPDVQrRefund.Create('data');
  fQrResumo         := TPixPDVQrResumo.Create('data');
  fQrSimulaPagar    := TPixPDVQrSimulaPagar.Create('data');
  fSaldo            := TPixPDVSaldo.Create('data');
  fRetirada         := TPixPDVRetirada.Create('data');
  fExtrato          := TPixPDVExtrato.Create('data');

  fCNPJ := EmptyStr;
  fToken := EmptyStr;
end;

destructor TACBrPSPPixPDV.Destroy;
begin
  fError.Free;
  fStatusTokenDados.Free;
  fQrDinamico.Free;
  fQrCobranca.Free;
  fQrGerado.Free;
  fQrStatus.Free;
  fQrRefund.Free;
  fQrResumo.Free;
  fQrSimulaPagar.Free;
  fSaldo.Free;
  fRetirada.Free;
  fExtrato.Free;
  inherited Destroy;
end;

procedure TACBrPSPPixPDV.Clear;
begin
  inherited Clear;
  fError.Clear;
  fStatusTokenDados.Clear;
  fQrDinamico.Clear;
  fQrCobranca.Clear;
  fQrGerado.Clear;
  fQrStatus.Clear;
  fQrRefund.Clear;
  fQrResumo.Clear;
  fSaldo.Clear;
  fRetirada.Clear;
  fExtrato.Clear;
end;

procedure TACBrPSPPixPDV.PostStatusToken;
var
  wBody, wURL: String;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wJs: TACBrJSONObject;
  wOk: Boolean;
begin
  if EstaVazio(CNPJ) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido),['CNPJ']));

  wJs := TACBrJSONObject.Create;
  try
    wJs.AddPair('cnpj', CNPJ);
    wBody := wJs.ToJSON;
  finally
    wJs.Free;
  end;

  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;
  AcessarEndPoint(ChttpMethodPOST, cPIXPDVEndPointStatusToken, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOk then
    fStatusTokenDados.AsJSON := String(UTF8ToNativeString(wRespostaHttp))
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointStatusToken);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.PostQrDinamico;
var
  wBody, wURL: string;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  wBody := Trim(fQrDinamico.AsJSON);
  if EstaVazio(wBody) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido),
      [PathWithoutDelim(cPIXPDVEndPointQrDinamico)]));

  if EstaZerado(fQrDinamico.Valor) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido),['Valor']));

  fQrGerado.Clear;
  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;
  AcessarEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrDinamico, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOk then
    fQrGerado.AsJSON := String(UTF8ToNativeString(wRespostaHttp))
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrDinamico);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.PostQrCobranca;
var
  wBody, wURL: string;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  wBody := Trim(fQrCobranca.AsJSON);
  if EstaVazio(wBody) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido),
      [PathWithoutDelim(cPIXPDVEndPointQrCobranca)]));

  if (Ord(fQrCobranca.Juros.modalidade) > 0) and (EstaZerado(fQrCobranca.Juros.valorPerc))  then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido),['Valor/Percentual de Juros']));

  if (Ord(fQrCobranca.Multa.modalidade) > 0) and (EstaZerado(fQrCobranca.Multa.valorPerc))  then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido),['Valor/Percentual de Multa']));

  if (Ord(fQrCobranca.Desconto.modalidade) > 0) and (EstaZerado(fQrCobranca.Desconto.valorPerc))  then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido),['Valor/Percentual de Desconto']));

  if (Ord(fQrCobranca.Desconto.modalidade) in [1, 2]) and (EstaZerado(fQrCobranca.Desconto.descontosDataFixa[0].data))  then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido),['Data do Desconto']));

  fQrGerado.Clear;
  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;
  AcessarEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrCobranca, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOk then
    fQrGerado.AsJSON := String(UTF8ToNativeString(wRespostaHttp))
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrCobranca);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.PostQrRefund(const aQrCodeId: string);
var
  wBody, wURL: String;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wJs: TACBrJSONObject;
  wOk: Boolean;
begin
  if EstaVazio(aQrCodeId) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['QRCode_ID']));

  wJs := TACBrJSONObject.Create;
  try
    wJs.AddPair('qrcodeId', aQrCodeId);
    wBody := wJs.ToJSON;
  finally
    wJs.Free;
  end;

  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;
  AcessarEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrRefund, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOk then
    fQrRefund.AsJSON := String(UTF8ToNativeString(wRespostaHttp))
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrRefund);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.PostQrSimulaPagar(const aQrCodeId: string);
var
  wBody, wURL: string;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wJs: TACBrJSONObject;
  wOk: Boolean;
begin
  if EstaVazio(aQrCodeId) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['QRCode_ID']));

  wJs := TACBrJSONObject.Create;
  try
    wJs.AddPair('qrcodeId', aQrCodeId);
    wBody := wJs.ToJSON;
  finally
    wJs.Free;
  end;

  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;
  AcessarEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrSimulaPagar, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOk then
    fStatusTokenDados.AsJSON := String(UTF8ToNativeString(wRespostaHttp))
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrSimulaPagar);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.PostRetirada(aValor: Currency);
var
  wBody, wURL: String;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wJs: TACBrJSONObject;
  wOk: Boolean;
begin
  if EstaZerado(aValor) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['Valor']));

  wJs := TACBrJSONObject.Create;
  try
    wJs.AddPair('valor', aValor);
    wBody := wJs.ToJSON;
  finally
    wJs.Free;
  end;

  PrepararHTTP;
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;
  AcessarEndPoint(ChttpMethodPOST, cPIXPDVEndPointRetirada, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOk then
    fRetirada.AsJSON := String(UTF8ToNativeString(wRespostaHttp))
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointRetirada);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.GetQrStatus(const aQRCodeId: string);
var
  wURL: String;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  if EstaVazio(Trim(aQrCodeId)) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['QrCodeId']));

  Clear;
  PrepararHTTP;
  URLQueryParams.Values['qrcodeid'] := aQrCodeId;
  Http.MimeType := CContentTypeApplicationJSon;
  AcessarEndPoint(ChttpMethodGET, cPIXPDVEndPointQrStatus, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOK then
  begin
    fQrStatus.AsJSON := String(UTF8ToNativeString(wRespostaHttp))
  end
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrStatus);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodGET, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.GetQrResumo(inicio, fim: TDateTime;
  tipo: TACBrPSPPIXPDVTipoData);
var
  wURL, wStartDate, wEndDate: String;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wOK: Boolean;
begin
  Clear;
  PrepararHTTP;

  wStartDate := FormatDateTime('yyyy-mm-dd', inicio);
  if EstaVazio(Trim(wStartDate)) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['inicio']));
  URLQueryParams.Values['inicio'] := wStartDate;

  wEndDate := FormatDateTime('yyyy-mm-dd', fim);
  if EstaVazio(Trim(wEndDate)) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['fim']));
  URLQueryParams.Values['fim'] := wEndDate;

  if (tipo = tpdtEmissao) then
    URLQueryParams.Values['tipo'] := 'emissao'
  else
  if (tipo = tpdtRecebimento) then
    URLQueryParams.Values['tipo'] := 'recebimento'
  else
  if (tipo = tpdtVencimento) then
    URLQueryParams.Values['tipo'] := 'vencimento'
  else
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['Tipo']));

  AcessarEndPoint(ChttpMethodGET, cPIXPDVEndPointQrResumo, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOK then
    fQrResumo.AsJSON := String(wRespostaHttp)
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointQrResumo);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodGET, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.GetSaldo;
var
  wURL: String;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wOk: Boolean;
begin
  Clear;
  PrepararHTTP;
  AcessarEndPoint(ChttpMethodGET, cPIXPDVEndPointSaldo, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOK then
  begin
    fSaldo.AsJSON := String(UTF8ToNativeString(wRespostaHttp))
  end
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointSaldo);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodGET, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.GetExtrato(inicio, fim: TDateTime);
var
  wURL, wStartDate, wEndDate: String;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wOK: Boolean;
begin
  Clear;
  PrepararHTTP;

  wStartDate := FormatDateTime('yyyy-mm-dd', inicio);
  if EstaVazio(Trim(wStartDate)) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['inicio']));
  URLQueryParams.Values['inicio'] := wStartDate;

  wEndDate := FormatDateTime('yyyy-mm-dd', fim);
  if EstaVazio(Trim(wEndDate)) then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['fim']));
  URLQueryParams.Values['fim'] := wEndDate;

  AcessarEndPoint(ChttpMethodGET, cPIXPDVEndPointExtrato, wResultCode, wRespostaHttp);
  wOk := (wResultCode = HTTP_OK);

  if wOK then
    fExtrato.AsJSON := String(wRespostaHttp)
  else
  begin
    fError.AsJSON := String(wRespostaHttp);
    wURL := CalcularURLEndPoint(ChttpMethodPOST, cPIXPDVEndPointExtrato);
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodGET, wURL]));
  end;
end;

procedure TACBrPSPPixPDV.ConfigurarAutenticacao(const Method, EndPoint: string);
begin
  inherited ConfigurarAutenticacao(Method, EndPoint);

  if NaoEstaVazio(Token) then
  begin
    Http.UserName := CNPJ;
    Http.Password := Token;
  end;
end;

procedure TACBrPSPPixPDV.ConfigurarHeaders(const Method, AURL: string);
var
  wHash: String;
  wStrStream: TStringStream;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  inherited ConfigurarHeaders(Method, AURL);

  Http.Headers.Add(cPixPDVHeaderPoweredBy);

  if (Http.Document.Size <= 0) then
    Exit;

  wStrStream := TStringStream.Create('');
  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wStrStream.CopyFrom(Http.Document, 0);
    wHash := wOpenSSL.HMACFromString(wStrStream.DataString, ClientSecret, algSHA256);

    Http.Headers.Add('Json-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
    wStrStream.Free;
  end;
end;

function TACBrPSPPixPDV.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  if (Ambiente = ambProducao) then
    Result := cPixPDVURLProducao
  else
    Result := cPixPDVURLSandbox;
end;

end.
