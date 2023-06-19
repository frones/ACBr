{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

  Documentação
  https://developers.bb.com.br/

*)

{$I ACBr.inc}

unit ACBrPIXPSPBancoDoBrasil;

interface

uses
  Classes, SysUtils,
  ACBrPIXCD, ACBrBase, ACBrPIXSchemasProblema;

const
  cBBParamDevAppKey = 'gw-dev-app-key';
  cBBParamAppKey = 'gw-app-key';
  cBBURLSandbox = 'https://api.hm.bb.com.br';
  cBBURLProducao = 'https://api.bb.com.br';
  cBBPathAPIPix = '/pix/v1';
  cBBPathAPIPixV2 = '/pix/v2';
  cBBURLAuthTeste = 'https://oauth.hm.bb.com.br/oauth/token';
  cBBURLAuthProducao = 'https://oauth.bb.com.br/oauth/token';
  cBBPathSandboxPagarPix = '/testes-portal-desenvolvedor/v1';
  cBBEndPointPagarPix = '/boletos-pix/pagar';
  cBBURLSandboxPagarPix = cBBURLSandbox + cBBPathSandboxPagarPix + cBBEndPointPagarPix;
  cBBKeySandboxPagarPix = '95cad3f03fd9013a9d15005056825665';
  cBBEndPointCobHomologacao = '/cobqrcode';

resourcestring
  sErroCertificadoNaoInformado = 'Certificado e/ou Chave Privada não informados';

type
        
  TACBrBBAPIVersao = (apiVersao1, apiVersao2);

  { TACBrPSPBancoDoBrasil }
  
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPBancoDoBrasil = class(TACBrPSPCertificate)
  private
    fBBAPIVersao: TACBrBBAPIVersao;
    fDeveloperApplicationKey: String;

    procedure QuandoAcessarEndPoint(const AEndPoint: String;
      var AURL: String; var AMethod: String);
    procedure QuandoReceberRespostaEndPoint(const AEndPoint, AURL, AMethod: String;
      var AResultCode: Integer; var RespostaHttp: AnsiString);
  protected
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;
    function CalcularEndPointPath(const aMethod, aEndPoint: String): String; override;

    procedure ConfigurarQueryParameters(const Method, EndPoint: String); override;
    procedure TratarRetornoComErro(ResultCode: Integer; const RespostaHttp: AnsiString;
      Problema: TACBrPIXProblema); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Autenticar; override;

    procedure SimularPagamentoPIX(const pixCopiaECola: String;
      var Code: Integer; var Texto: String);
  published
    property APIVersion;
    property ClientID;
    property ClientSecret;

    property BBAPIVersao: TACBrBBAPIVersao read fBBAPIVersao write fBBAPIVersao default apiVersao1;
    property DeveloperApplicationKey: String read fDeveloperApplicationKey write fDeveloperApplicationKey;
  end;

implementation

uses
  synautil, synacode,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrJSON,
  ACBrPIXBase,
  DateUtils;

{ TACBrPSPBancoDoBrasil }

constructor TACBrPSPBancoDoBrasil.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDeveloperApplicationKey := '';
  fpQuandoAcessarEndPoint := QuandoAcessarEndPoint;
  fpQuandoReceberRespostaEndPoint := QuandoReceberRespostaEndPoint;
end;

procedure TACBrPSPBancoDoBrasil.Autenticar;
var
  AURL, Body, BasicAutentication: String;
  RespostaHttp: AnsiString;
  ResultCode, sec: Integer;
  js: TACBrJSONObject;
  qp: TACBrQueryParams;
begin
  LimparHTTP;

  if (ACBrPixCD.Ambiente = ambProducao) then
    AURL := cBBURLAuthProducao
  else
    AURL := cBBURLAuthTeste;

  qp := TACBrQueryParams.Create;
  try
    qp.Values['grant_type'] := 'client_credentials';
    qp.Values['scope'] := ScopesToString(Scopes);
    Body := qp.AsURL;
    WriteStrToStream(Http.Document, Body);
    Http.MimeType := CContentTypeApplicationWwwFormUrlEncoded;
  finally
    qp.Free;
  end;

  BasicAutentication := 'Basic '+EncodeBase64(ClientID + ':' + ClientSecret);
  Http.Headers.Add(ChttpHeaderAuthorization+' '+BasicAutentication);
  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);

  if (ResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(RespostaHttp);
    try
      fpToken := js.AsString['access_token'];
      sec := js.AsInteger['expires_in'];
    finally
      js.Free;
    end;

   if (Trim(fpToken) = '') then
     DispararExcecao(EACBrPixHttpException.Create(ACBrStr(sErroAutenticacao)));

   fpValidadeToken := IncSecond(Now, sec);
   fpAutenticado := True;
  end
  else
    DispararExcecao(EACBrPixHttpException.CreateFmt( sErroHttp,
       [Http.ResultCode, ChttpMethodPOST, AURL]));
end;

procedure TACBrPSPBancoDoBrasil.SimularPagamentoPIX(
  const pixCopiaECola: String; var Code: Integer; var Texto: String);
var
  Body, AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  js: TACBrJSONObject;
begin
  if (Trim(pixCopiaECola) = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['pixCopiaECola']);
    
  js := TACBrJSONObject.Create;
  try
    js.AddPair('pix', pixCopiaECola);
    Body := js.ToJSON;
  finally
    js.Free;
  end;

  PrepararHTTP;
  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeApplicationJSon;
  ConfigurarAutenticacao(ChttpMethodPOST, cBBEndPointPagarPix);
  AURL := cBBURLSandboxPagarPix + '?' + cBBParamAppKey + '=' + cBBKeySandboxPagarPix;

  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);
  if (ResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(RespostaHttp);
    try
      code := js.AsInteger['code'];
      texto := js.AsString['texto'];
    finally
      js.Free;
    end;

    if (code <> 0) then
      DispararExcecao(EACBrPixHttpException.Create('Code: '+
        IntToStr(code) +' - '+ UTF8ToNativeString(texto)));
  end
  else
    DispararExcecao(EACBrPixHttpException.CreateFmt( sErroHttp,
       [Http.ResultCode, ChttpMethodPOST, AURL]));
end;

procedure TACBrPSPBancoDoBrasil.QuandoAcessarEndPoint(
  const AEndPoint: String; var AURL: String; var AMethod: String);
begin
  // BB v1 não tem: POST /cob - Mudando para /PUT com "txid" vazio
  if (BBAPIVersao = apiVersao1) and (UpperCase(AMethod) = ChttpMethodPOST) then
    AMethod := ChttpMethodPUT;

  // Certificado é obrigatório em Produção na API BB versão 2
  if (BBAPIVersao = apiVersao2) and (ACBrPixCD.Ambiente = ambProducao) and
     (EstaVazio(ArquivoCertificado) or EstaVazio(ArquivoChavePrivada)) then
    raise EACBrPixHttpException.Create(ACBrStr(sErroCertificadoNaoInformado));
end;

procedure TACBrPSPBancoDoBrasil.QuandoReceberRespostaEndPoint(const AEndPoint,
  AURL, AMethod: String; var AResultCode: Integer; var RespostaHttp: AnsiString);
begin
  // Banco do Brasil não possui consulta de cobranças por período
  if (UpperCase(AMethod) = ChttpMethodGET) and (AEndPoint = cEndPointCob) and (AResultCode <> HTTP_OK) and
     (Pos('inicio', LowerCase(AURL)) > 0) then
    raise EACBrPixException.Create(ACBrStr(sErroEndpointNaoImplementado));

  // Banco do Brasil, responde OK a esse EndPoint, de forma diferente da especificada
  if (UpperCase(AMethod) = ChttpMethodPUT) and (AEndPoint = cEndPointCob) and (AResultCode = HTTP_OK) then
  begin
    AResultCode := HTTP_CREATED;

    // Ajuste no Json de Resposta em Testes alterando textoImagemQRcode p/ pixCopiaECola - Icozeira
    if (ACBrPixCD.Ambiente = ambTeste) then
      RespostaHttp := StringReplace(RespostaHttp, 'textoImagemQRcode', 'pixCopiaECola', [rfReplaceAll]);
  end;

  // Ajuste para o Método Patch do BB - Icozeira - 14/04/2022
  if (UpperCase(AMethod) = ChttpMethodPATCH) and (AEndPoint = cEndPointCob) and (AResultCode = HTTP_CREATED) then
    AResultCode := HTTP_OK;
end;

function TACBrPSPBancoDoBrasil.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  if (Ambiente = ambProducao) then
    Result := cBBURLProducao
  else
    Result := cBBURLSandbox;

  if (BBAPIVersao = apiVersao2) then
    Result := Result + cBBPathAPIPixV2
  else
    Result := Result + cBBPathAPIPix;
end;

function TACBrPSPBancoDoBrasil.CalcularEndPointPath(const aMethod, aEndPoint: String): String;
begin
  Result := Trim(aEndPoint);

  // Alterações devem ser feitas apenas na versão 1 da API do BB
  if (BBAPIVersao = apiVersao1) then
  begin
    // BB v1 deve utilizar /cobqrcode em ambiente de homologação
    if ((UpperCase(aMethod) = ChttpMethodPOST) or
        (UpperCase(aMethod) = ChttpMethodPUT)) and
        (aEndPoint = cEndPointCob) and (ACBrPixCD.Ambiente = ambTeste) then
      Result := cBBEndPointCobHomologacao;

    // BB utiliza delimitador antes dos parâmetros de query
    if (aEndPoint = cEndPointCob) then
      Result := URLComDelimitador(Result);
  end;
end;

procedure TACBrPSPBancoDoBrasil.ConfigurarQueryParameters(const Method, EndPoint: String);
begin
  inherited ConfigurarQueryParameters(Method, EndPoint);

  if (fDeveloperApplicationKey <> '') then
    URLQueryParams.Values[cBBParamDevAppKey] := fDeveloperApplicationKey;
end;

procedure TACBrPSPBancoDoBrasil.TratarRetornoComErro(ResultCode: Integer;
  const RespostaHttp: AnsiString; Problema: TACBrPIXProblema);
var
  js, ej: TACBrJSONObject;
  ae: TACBrJSONArray;
begin
  if (pos('"ocorrencia"', RespostaHttp) > 0) then   // Erro no formato próprio do B.B.
  begin
     (* Exemplo de Retorno
       {
	    "erros": [{
		    "codigo": "4769515",
		    "versao": "1",
		    "mensagem": "Não há informações para os dados informados.",
		    "ocorrencia": "CHOM00000062715498140101"
	    }]
       }
     *)

    js := TACBrJSONObject.Parse(RespostaHttp);
    try
      ae := js.AsJSONArray['erros'];
      if Assigned(ae) and (ae.Count > 0) then
      begin
        ej := ae.ItemAsJSONObject[0];
        Problema.title := ej.AsString['ocorrencia'];
        Problema.status := StrToIntDef(ej.AsString['codigo'], -1);
        Problema.detail := ej.AsString['mensagem'];
      end;
    finally
      js.Free;
    end;

    if (Problema.title = '') then
      AtribuirErroHTTPProblema(Problema);
  end
  else if  (pos('"statusCode"', RespostaHttp) > 0) then   // Erro interno
  begin
    // Exemplo de Retorno
    // {"statusCode":401,"error":"Unauthorized","message":"Bad Credentials","attributes":{"error":"Bad Credentials"}}

    js := TACBrJSONObject.Parse(RespostaHttp);
    try
      Problema.title := js.AsString['error'];
      Problema.status := js.AsInteger['statusCode'];
      Problema.detail := js.AsString['message'];
    finally
      js.Free;
    end;

    if (Problema.title = '') then
      AtribuirErroHTTPProblema(Problema);
  end
  else
    inherited TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

end.

