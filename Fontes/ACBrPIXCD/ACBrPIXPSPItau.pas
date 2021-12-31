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

{$I ACBr.inc}

unit ACBrPIXPSPItau;

interface

uses
  Classes, SysUtils,
  ACBrPIXCD;

const
  cURLItauSandbox = 'https://api.itau.com.br/sandbox';
  cURLItauProducao = 'https://secure.api.itau';
  cURLItauAPIPix = '/pix_recebimentos/v2/';
  cURLItauAuthTeste = cURLItauSandbox+'/api/oauth/token';
  cURLItauAuthProducao = 'https://sts.itau.com.br/as/token.oauth2';

type

  { TACBrPSPItau }

  TACBrPSPItau = class(TACBrPSP)
  private
    fSandboxStatusCode: String;
    fxCorrelationID: String;
  protected
    procedure Autenticar; override;
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;
    procedure ConfigurarAutenticacao(const Method, EndPoint: String); override;
    procedure ConfigurarQueryParameters(const Method, EndPoint: String); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ClientID;
    property ClientSecret;

    property xCorrelationID: String read fxCorrelationID write fxCorrelationID;
    property SandboxStatusCode: String read fSandboxStatusCode write fSandboxStatusCode;
  end;

implementation

uses
  synautil,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr
  {$Else}
   Jsons
  {$EndIf},
  DateUtils;

{ TACBrPSPItau }

constructor TACBrPSPItau.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fSandboxStatusCode := '';
  fxCorrelationID := '';
end;

procedure TACBrPSPItau.Autenticar;
var
  AURL, RespostaHttp, Body: String;
  ResultCode, sec: Integer;
  js: TJsonObject;
  qp: TACBrQueryParams;
begin
  LimparHTTP;

  if (ACBrPixCD.Ambiente = ambProducao) then
    AURL := cURLItauAuthProducao
  else
    AURL := cURLItauAuthTeste;

  qp := TACBrQueryParams.Create;
  try
    qp.Values['grant_type'] := 'client_credentials';
    qp.Values['client_id'] := ClientID;
    qp.Values['client_secret'] := ClientSecret;
    Body := qp.AsURL;
    Delete(Body, 1, 1);
    WriteStrToStream(Http.Document, Body);
    Http.MimeType := CContentTypeApplicationWwwFormUrlEncoded;
  finally
    qp.Free;
  end;

  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);

  if (ResultCode = HTTP_OK) then
  begin
   {$IfDef USE_JSONDATAOBJECTS_UNIT}
    js := TJsonObject.Parse(RespostaHttp) as TJsonObject;
    try
      fpToken := js.S['access_token'];
      sec := js.I['expires_in'];
      fpRefereshToken := js.S['refresh_token'];
    finally
      js.Free;
    end;
   {$Else}
    js := TJsonObject.Create;
    try
      js.Parse(RespostaHttp);
      fpToken := js['access_token'].AsString;
      sec := js['expires_in'].AsInteger;
      fpRefereshToken := js['refresh_token'].AsString;
    finally
      js.Free;
    end;
   {$EndIf}

   fpValidadeToken := IncSecond(Now, sec);
   fpAutenticado := True;
  end
  else
    ACBrPixCD.DispararExcecao(EACBrPixHttpException.CreateFmt(
      sErroHttp,[Http.ResultCode, ChttpMethodPOST, AURL]));
end;

function TACBrPSPItau.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  if (Ambiente = ambProducao) then
    Result := cURLItauProducao
  else
    Result := cURLItauSandbox;

  Result := Result + cURLItauAPIPix;
end;

procedure TACBrPSPItau.ConfigurarAutenticacao(const Method, EndPoint: String);
begin
  inherited;
end;

procedure TACBrPSPItau.ConfigurarQueryParameters(const Method, EndPoint: String);
begin
  with QueryParams do
  begin
    if (fxCorrelationID <> '') then
      Values['x-correlationID'] := fxCorrelationID;

    if (fSandboxStatusCode <> '') then
      Values['status_code'] := fSandboxStatusCode;
  end;
end;

end.


