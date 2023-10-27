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

{$I ACBr.inc}

unit ACBrPIXPSPBradesco;

interface

uses
  Classes, SysUtils, ACBrPIXCD;

const

  cBradescoURLSandbox = 'https://qrpix-h.bradesco.com.br';
  cBradescoURLProducao = 'https://qrpix.bradesco.com.br';
  cBradescoPathAuthToken = '/oauth/token';
  cBradescoPathAPIPix = '/v2';

type

  { TACBrPSPBradesco }
                    
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPBradesco = class(TACBrPSPCertificate)
  protected
    function ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String; override;
  private
    procedure QuandoReceberRespostaEndPoint(const aEndPoint, AURL, aMethod: String;
      var aResultCode: Integer; var aRespostaHttp: AnsiString);
  public
    constructor Create(aOwner: TComponent); override;
    procedure Autenticar; override;
  published
    property ClientID;
    property ClientSecret;
  end;

implementation

uses synautil, DateUtils, ACBrJSON, ACBrUtil.Strings;

{ TACBrPSPBradesco }

function TACBrPSPBradesco.ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String;
begin
  if (aAmbiente = ambProducao) then
    Result := cBradescoURLProducao + cBradescoPathAPIPix
  else
    Result := cBradescoURLSandbox + cBradescoPathAPIPix;
end;

procedure TACBrPSPBradesco.QuandoReceberRespostaEndPoint(const aEndPoint, AURL,
  aMethod: String; var aResultCode: Integer; var aRespostaHttp: AnsiString);
begin
  // Bradesco responde OK ao EndPoint /cobv, de forma diferente da especificada
  if (UpperCase(AMethod) = ChttpMethodPUT) and (AEndPoint = cEndPointCobV) and (AResultCode = HTTP_OK) then
    AResultCode := HTTP_CREATED;
end;

constructor TACBrPSPBradesco.Create(aOwner: TComponent);
begin
  inherited Create(AOwner);
  fpQuandoReceberRespostaEndPoint := QuandoReceberRespostaEndPoint;
end;

procedure TACBrPSPBradesco.Autenticar;
var
  wURL: String;
  qp: TACBrQueryParams;
  wResultCode: Integer;
  wRespostaHttp: AnsiString;
  js: TACBrJSONObject;
  sec: LongInt;
begin
  LimparHTTP;

  if (ACBrPixCD.Ambiente = ambProducao) then
    wURL := cBradescoURLProducao + cBradescoPathAuthToken
  else
    wURL := cBradescoURLSandbox + cBradescoPathAuthToken;

  qp := TACBrQueryParams.Create;
  try
    qp.Values['grant_type'] := 'client_credentials';
    WriteStrToStream(Http.Document, qp.AsURL);
    Http.MimeType := CContentTypeApplicationWwwFormUrlEncoded;
  finally
    qp.Free;
  end;
  
  Http.Protocol := '1.1';
  Http.UserName := ClientID;
  Http.Password := ClientSecret;
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
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [wResultCode, ChttpMethodPOST, wURL]));
end;

end.

