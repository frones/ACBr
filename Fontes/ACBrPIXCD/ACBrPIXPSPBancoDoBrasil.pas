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
  ACBrPIXCD, ACBrPIXSchemasProblema;

const
  cBBParamDevAppKey = 'gw-dev-app-key';
  cBBParamAppKey = 'gw-app-key';
  cBBURLSandbox = 'https://api.hm.bb.com.br';  // 'https://api.sandbox.bb.com.br';
  cBBURLProducao = 'https://api.bb.com.br';
  cBBPathAPIPix = '/pix/v1';
  cBBURLAuthTeste = 'https://oauth.hm.bb.com.br/oauth/token';
  cBBURLAuthProducao = 'https://oauth.bb.com.br/oauth/token';
  cBBPathSandboxPagarPix = '/testes-portal-desenvolvedor/v1';
  cBBEndPointPagarPix = '/boletos-pix/pagar';
  cBBURLSandboxPagarPix = cBBURLSandbox + cBBPathSandboxPagarPix + cBBEndPointPagarPix;
  cBBKeySandboxPagarPix = '95cad3f03fd9013a9d15005056825665';

type

  { TACBrPSPBancoDoBrasil }

  TACBrPSPBancoDoBrasil = class(TACBrPSP)
  private
    fDeveloperApplicationKey: String;

    procedure QuandoAcessarEndPoint(const AEndPoint: String;
      var AURL: String; var AMethod: String);
    procedure QuandoReceberRespostaEndPoint(const AEndPoint, AURL, AMethod: String;
      var AResultCode: Integer; var RespostaHttp: AnsiString);
  protected
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;
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

    property DeveloperApplicationKey: String read fDeveloperApplicationKey
      write fDeveloperApplicationKey;
  end;

implementation

uses
  synautil, synacode,
  ACBrUtil, ACBrPIXBase,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr
  {$Else}
   Jsons
  {$EndIf},
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
  js: TJsonObject;
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
    qp.Values['scope'] := 'cob.read cob.write pix.read pix.write';
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
   {$IfDef USE_JSONDATAOBJECTS_UNIT}
    js := TJsonObject.Parse(RespostaHttp) as TJsonObject;
    try
      fpToken := js.S['access_token'];
      sec := js.I['expires_in'];
    finally
      js.Free;
    end;
   {$Else}
    js := TJsonObject.Create;
    try
      js.Parse(RespostaHttp);
      fpToken := js['access_token'].AsString;
      sec := js['expires_in'].AsInteger;
    finally
      js.Free;
    end;
   {$EndIf}

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
  js: TJsonObject;
begin
  if (Trim(pixCopiaECola) = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['pixCopiaECola']);

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   js := TJsonObject.Parse(RespostaHttp) as TJsonObject;
   try
     js.S['pix'] := pixCopiaECola;
     Body := js.ToJSON();
   finally
     js.Free;
   end;
  {$Else}
   js := TJsonObject.Create;
   try
     js['pix'].AsString := pixCopiaECola;
     Body := js.Stringify;
   finally
     js.Free;
   end;
  {$EndIf}

  PrepararHTTP;
  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeApplicationJSon;
  ConfigurarAutenticacao(ChttpMethodPOST, cBBEndPointPagarPix);
  AURL := cBBURLSandboxPagarPix + '?' + cBBParamAppKey + '=' + cBBKeySandboxPagarPix;

  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);
  if (ResultCode = HTTP_OK) then
  begin
   {$IfDef USE_JSONDATAOBJECTS_UNIT}
    js := TJsonObject.Parse(RespostaHttp) as TJsonObject;
    try
      code := js.I['code'];
      texto := js.S['texto'];
    finally
      js.Free;
    end;
   {$Else}
    js := TJsonObject.Create;
    try
      js.Parse(RespostaHttp);
      code := js['code'].AsInteger;
      texto := js['texto'].AsString;
    finally
      js.Free;
    end;
   {$EndIf}

   if (code <> 0) then
     DispararExcecao(EACBrPixHttpException.Create( 'Code: '+IntToStr(code)+' - '+
                                                   UTF8ToNativeString(texto) ));
  end
  else
    DispararExcecao(EACBrPixHttpException.CreateFmt( sErroHttp,
       [Http.ResultCode, ChttpMethodPOST, AURL]));
end;

procedure TACBrPSPBancoDoBrasil.QuandoAcessarEndPoint(
  const AEndPoint: String; var AURL: String; var AMethod: String);
begin
  // Banco do Brasil, não tem: POST /cob   Mudando para /PUT com "txid" vazio
  if (UpperCase(AMethod) = ChttpMethodPOST) and (AEndPoint = cEndPointCob) then
  begin
    AMethod := ChttpMethodPUT;
    AURL := StringReplace(AURL, cEndPointCob, '/cob/', [rfReplaceAll]);
  end;
end;

procedure TACBrPSPBancoDoBrasil.QuandoReceberRespostaEndPoint(const AEndPoint,
  AURL, AMethod: String; var AResultCode: Integer; var RespostaHttp: AnsiString
  );
begin
  // Banco do Brasil, responde OK a esse EndPoint, de forma diferente da espcificada
  if (UpperCase(AMethod) = ChttpMethodPUT) and (AEndPoint = cEndPointCob) and (AResultCode = HTTP_OK) then
    AResultCode := HTTP_CREATED;

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

  Result := Result + cBBPathAPIPix;
end;

procedure TACBrPSPBancoDoBrasil.ConfigurarQueryParameters(const Method, EndPoint: String);
begin
  inherited ConfigurarQueryParameters(Method, EndPoint);

  with URLQueryParams do
  begin
    if (fDeveloperApplicationKey <> '') then
      Values[cBBParamDevAppKey] := fDeveloperApplicationKey;
  end;
end;

procedure TACBrPSPBancoDoBrasil.TratarRetornoComErro(ResultCode: Integer;
  const RespostaHttp: AnsiString; Problema: TACBrPIXProblema);
var
  js, ej: TJsonObject;
  ae: TJsonArray;
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
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     js := TJsonObject.Parse(RespostaHttp) as TJsonObject;
     try
       ae := js.A['erros'];
       if Assigned(ae) and (ae.Count > 0) then
       begin
         ej := ae.O[0];
         Problema.title := ej.S['ocorrencia'];
         Problema.status := ej.I['codigo'];
         Problema.detail := ej.S['mensagem'];
       end;
     finally
       js.Free;
     end;
    {$Else}
     js := TJsonObject.Create;
     try
       js.Parse(RespostaHttp);
       ae := js['erros'].AsArray;
       if Assigned(ae) and (ae.Count > 0) then
       begin
         ej := ae[0].AsObject;
         Problema.title := ej['ocorrencia'].AsString;
         Problema.status := ej['codigo'].AsInteger;
         Problema.detail := ej['mensagem'].AsString;
       end;
     finally
       js.Free;
     end;
    {$EndIf}

    if (Problema.title = '') then
      AtribuirErroHTTPProblema(Problema);
  end
  else if  (pos('"statusCode"', RespostaHttp) > 0) then   // Erro interno
  begin
    // Exemplo de Retorno
    // {"statusCode":401,"error":"Unauthorized","message":"Bad Credentials","attributes":{"error":"Bad Credentials"}}

    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     js := TJsonObject.Parse(RespostaHttp) as TJsonObject;
     try
       Problema.title := js.S['error'];
       Problema.status := js.I['statusCode'];
       Problema.detail := js.S['message'];
     finally
       js.Free;
     end;
    {$Else}
     js := TJsonObject.Create;
     try
       js.Parse(RespostaHttp);
       Problema.title := js['error'].AsString;
       Problema.status := js['statusCode'].AsInteger;
       Problema.detail := js['message'].AsString;
     finally
       js.Free;
     end;
    {$EndIf}

    if (Problema.title = '') then
      AtribuirErroHTTPProblema(Problema);
  end
  else
    inherited TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

end.

