{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

  Documentação
  https://developercielo.github.io/manual/apipix#introdu%C3%A7%C3%A3o

*)

unit ACBrPIXPSPCielo;

interface

uses
  Classes, SysUtils,
  ACBrPIXCD, ACBrOpenSSLUtils;

const
  cCieloURLSandbox      = 'https://api2.cielo.com.br/sandbox';
  cCieloURLProducao     = 'https://api-mtls.cielo.com.br';
  cCieloPathAuthToken   = '/v2/oauth/access-token';
  cCieloPathAPIPix      = '/cielo-pix/v1';
  cCieloURLAuthTeste    = cCieloURLSandbox+cCieloPathAuthToken;
  cCieloURLAuthProducao = 'https://api2.cielo.com.br'+cCieloPathAuthToken;

type

  { TACBrPSPCielo }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPCielo = class(TACBrPSPCertificate)
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
  synacode, synautil, DateUtils, ACBrJSON,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrPIXUtil;

{ TACBrPSPCielo }

function TACBrPSPCielo.ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String;
begin
  if (aAmbiente = ambProducao) then
    Result := cCieloURLProducao + cCieloPathAPIPix
  else
    Result := cCieloURLSandbox + cCieloPathAPIPix;
end;

procedure TACBrPSPCielo.Autenticar;
var
  wURL, Body: String;
  wRespostaHttp: AnsiString;
  wResultCode, sec: Integer;
  js, jb: TACBrJSONObject;
begin
  LimparHTTP;

  if (ACBrPixCD.Ambiente = ambProducao) then
    wURL := cCieloURLAuthProducao
  else
    wURL := cCieloURLAuthTeste;

  jb := TACBrJSONObject.Create;
  try
    jb.AddPair('grant_type', 'client_credentials');
    jb.AddPair('scope', ScopesToString(Scopes));
    Body := jb.ToJSON;
    WriteStrToStream(Http.Document, Body);
    Http.MimeType := CContentTypeApplicationJSon;
  finally
    jb.Free;
  end;

  Http.UserName := ClientID;
  Http.Password := ClientSecret;
  TransmitirHttp(ChttpMethodPOST, wURL, wResultCode, wRespostaHttp);

  if (wResultCode = HTTP_CREATED) then
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

