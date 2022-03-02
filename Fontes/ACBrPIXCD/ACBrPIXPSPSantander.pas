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
  https://developer.santander.com.br/

*)

{$I ACBr.inc}

unit ACBrPIXPSPSantander;

interface

uses
  Classes, SysUtils,
  ACBrPIXCD;

const
  cSantanderPathApiPIX = '/api/v1';
  cSantanderURLSandbox = 'https://pix.santander.com.br'+cSantanderPathApiPIX+'/sandbox';
  cSantanderURLPreProducao = 'https://trust-pix-h.santander.com.br'+cSantanderPathApiPIX;
  cSantanderURLProducao = 'https://trust-pix.santander.com.br'+cSantanderPathApiPIX;

  cSantanderURLAuthTeste = 'https://pix.santander.com.br/sandbox/oauth/token';
  cSantanderURLAuthPreProducao = 'https://trust-pix-h.santander.com.br/oauth/token';
  cSantanderURLAuthProducao = 'https://trust-pix.santander.com.br/oauth/token';

resourcestring
  sErroClienteIdDiferente = 'Cliente_Id diferente do Informado';

type

  { TACBrPSPSantander }

  TACBrPSPSantander = class(TACBrPSP)
  private
    fRefreshURL: String;
    function GetConsumerKey: String;
    function GetConsumerSecret: String;
    procedure SetConsumerKey(AValue: String);
    procedure SetConsumerSecret(AValue: String);
  protected
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Autenticar; override;
    procedure RenovarToken; override;
  published
    property APIVersion;
    property ConsumerKey: String read GetConsumerKey write SetConsumerKey;
    property ConsumerSecret: String read GetConsumerSecret write SetConsumerSecret;
  end;

implementation

uses
  synautil,
  ACBrUtil,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr
  {$Else}
   Jsons
  {$EndIf},
  DateUtils;

{ TACBrPSPSantander }

constructor TACBrPSPSantander.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fRefreshURL := '';
end;

procedure TACBrPSPSantander.Autenticar;
var
  AURL, Body, client_id: String;
  RespostaHttp: AnsiString;
  ResultCode, sec: Integer;
  js: TJsonObject;
  qp: TACBrQueryParams;
begin
  LimparHTTP;

  case ACBrPixCD.Ambiente of
    ambProducao: AURL := cSantanderURLAuthProducao;
    ambPreProducao: AURL := cSantanderURLAuthPreProducao;
  else
    AURL := cSantanderURLAuthTeste;
  end;

  AURL := AURL + '?grant_type=client_credentials';

  qp := TACBrQueryParams.Create;
  try
    qp.Values['client_id'] := ClientID;
    qp.Values['client_secret'] := ClientSecret;
    Body := qp.AsURL;
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
      client_id := Trim(js.S['client_id']);
      if (client_id <> ClientID) then
        raise EACBrPixHttpException.Create(ACBrStr(sErroClienteIdDiferente));
      fpToken := js.S['access_token'];
      sec := js.I['expires_in'];
      fRefreshURL := js.S['refresh_token'];
    finally
      js.Free;
    end;
   {$Else}
    js := TJsonObject.Create;
    try
      js.Parse(RespostaHttp);
      client_id := Trim(js['client_id'].AsString);
      if (client_id <> ClientID) then
        raise EACBrPixHttpException.Create(ACBrStr(sErroClienteIdDiferente));
      fpToken := js['access_token'].AsString;
      sec := js['expires_in'].AsInteger;
      fRefreshURL := js['refresh_token'].AsString;
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

procedure TACBrPSPSantander.RenovarToken;
begin
  // TODO: ??
  inherited RenovarToken;
end;

function TACBrPSPSantander.GetConsumerKey: String;
begin
  Result := ClientID;
end;

function TACBrPSPSantander.GetConsumerSecret: String;
begin
  Result := ClientSecret;
end;

procedure TACBrPSPSantander.SetConsumerKey(AValue: String);
begin
  ClientID := AValue;
end;

procedure TACBrPSPSantander.SetConsumerSecret(AValue: String);
begin
  ClientSecret := AValue;
end;

function TACBrPSPSantander.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  case ACBrPixCD.Ambiente of
    ambProducao: Result := cSantanderURLProducao;
    ambPreProducao: Result := cSantanderURLPreProducao;
  else
    Result := cSantanderURLSandbox;
  end;
end;

end.


