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
  https://developers.c6bank.com.br/pix-api

*)

unit ACBrPIXPSPC6Bank;

interface

uses
  Classes, SysUtils,
  {$IFDEF RTL230_UP}ACBrBase,{$ENDIF RTL230_UP}
  ACBrPIXCD, ACBrOpenSSLUtils;

const
  cC6URLSandbox      = 'htpps://baas-api-sandbox.c6bank.info';
  cC6URLProducao     = 'htpps://baas-api.c6bank.info';
  cC6PathAuthToken   = '/v1/auth';
  cC6PathAPIPix      = '/v2/pix';
  cC6URLAuthTeste    = cC6URLSandbox + cC6PathAuthToken;
  cC6URLAuthProducao = cC6URLProducao + cC6PathAuthToken;

type

  { TACBrPSPC6Bank }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPC6Bank = class(TACBrPSPCertificate)
  protected
    function ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String; override;
  public
    procedure Autenticar; override;
  published
    property ClientID;
    property ClientSecret;
  end;

implementation

uses
  synautil, blcksock, DateUtils, ACBrJSON, ACBrUtil.Strings;

{ TACBrPSPC6Bank }

function TACBrPSPC6Bank.ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String;
begin
  if (aAmbiente = ambProducao) then
    Result := cC6URLProducao + cC6PathAPIPix
  else
    Result := cC6URLSandbox + cC6PathAPIPix;
end;

procedure TACBrPSPC6Bank.Autenticar;
var
  wURL, Body: String;
  wRespostaHttp: AnsiString;
  wResultCode, sec: Integer;
  js: TACBrJSONObject;
  qp: TACBrQueryParams;
  //h: TACBrHTTP;
  //q: TACBrHTTPQueryParams;
begin
  raise Exception.Create('EM DESENVOLVIMENTO');
  {h := TACBrHTTP.Create(Nil);
  try
    h.ArqLOG := '_log.txt';
    h.NivelLog := 4;

    h.HTTPSend.Sock.SSL.CertificateFile := 'cert_C6Bank.crt';
    h.HTTPSend.Sock.SSL.PrivateKeyFile := 'cert_C6Bank.key';
    h.HTTPSend.UserName := '924728b8-be7b-4307-a75c-73abc2f5a3ef';
    h.HTTPSend.Password := 'KqHSWjKbHHGeFICmfHtMfuSxTrKETRmi';

    q := TACBrHTTPQueryParams.Create;
    try
      q.Values['grant_type'] := 'client_credentials';
      //q.Values['scope'] := ScopesToString(Scopes);
      Body := q.AsURL;
      WriteStrToStream(h.HTTPSend.Document, Body);
      h.HTTPSend.MimeType := cContentTypeApplicationWwwFormUrlEncoded;
    finally
      q.Free;
    end;

    h.HTTPPost('https://baas-api-sandbox.c6bank.info/v1/auth');

    wResultCode := h.HTTPResultCode;
    wRespostaHttp := h.HTTPResponse;
    if (wResultCode = HTTP_OK) then
    begin
      js := TACBrJSONObject.Parse(wRespostaHttp);
      try
        fpToken := js.AsString['access_token'];
        sec := js.AsInteger['expires_in'];
      finally
        js.Free;
      end;

      if (Trim(fpToken) = EmptyStr) then
        DispararExcecao(EACBrPixHttpException.Create(ACBrStr(sErroAutenticacao)));

      fpValidadeToken := IncSecond(Now, sec);
      fpAutenticado := True;
    end
    else
      DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
  finally
    h.Free;
  end;}

  LimparHTTP;

  if (ACBrPixCD.Ambiente = ambProducao) then
    wURL := cC6URLAuthProducao
  else
    wURL := cC6URLAuthTeste;

  qp := TACBrQueryParams.Create;
  try
    qp.Values['grant_type'] := 'client_credentials';
    //qp.Values['scope'] := ScopesToString(Scopes);
    Body := qp.AsURL;
    WriteStrToStream(Http.Document, Body);
    Http.MimeType := CContentTypeApplicationWwwFormUrlEncoded;
  finally
    qp.Free;
  end;

  Http.UserName := ClientID;
  Http.Password := ClientSecret;
  http.Sock.SSL.SSLType := LT_TLSv1_2;
  Http.Protocol := '1.1';
  TransmitirHttp(ChttpMethodPOST, wURL, wResultCode, wRespostaHttp);

  if (wResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(wRespostaHttp);
    try
      fpToken := js.AsString['access_token'];
      sec := js.AsInteger['expires_in'];
    finally
      js.Free;
    end;

    if (Trim(fpToken) = EmptyStr) then
      DispararExcecao(EACBrPixHttpException.Create(ACBrStr(sErroAutenticacao)));

    fpValidadeToken := IncSecond(Now, sec);
    fpAutenticado := True;
  end
  else
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
end;

end.

