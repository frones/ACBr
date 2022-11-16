{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Gabriel Baltazar                               }
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

unit ACBrOpenDeliveryHTTP;

interface

uses
  ACBrJSON,
  ACBrOpenDeliverySchemaClasses,
  ACBrOpenDeliveryException,
  Classes,
  SysUtils;

type
  TACBrRestMethodType = (mtGet, mtPost, mtPut, mtDelete, mtPatch);
  TACBrOpenDeliveryHTTPResponse = class;
  TACBrOpenDeliveryHTTPLogEnvio = class;
  TACBrOpenDeliveryHTTPLogResposta = class;

  TACBrOpenDeliveryOnHTTPEnviar = procedure(ALogEnvio: TACBrOpenDeliveryHTTPLogEnvio) of object;
  TACBrOpenDeliveryOnHTTPRetornar = procedure(ALogResposta: TACBrOpenDeliveryHTTPLogResposta) of object;
  TACBrOpenDeliveryOnHTTPError = procedure(ALogEnvio: TACBrOpenDeliveryHTTPLogEnvio;
    ALogResposta: TACBrOpenDeliveryHTTPLogResposta; AErro: EACBrOpenDeliveryHTTPException; var ATratado: Boolean) of object;

  TACBrOpenDeliveryHTTPLogEnvio = class
  private
    FId: string;
    FHeaders: TStrings;
    FMethod: string;
    FBody: string;
    FURL: string;
    FData: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Id: string read FId write FId;
    property Data: TDateTime read FData write FData;
    property URL: string read FURL write FURL;
    property Method: string read FMethod write FMethod;
    property Headers: TStrings read FHeaders write FHeaders;
    property Body: string read FBody write FBody;
  end;

  TACBrOpenDeliveryHTTPLogResposta = class
  private
    FId: string;
    FHeaders: TStrings;
    FBody: string;
    FURL: string;
    FStatus: Integer;
    FData: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Id: string read FId write FId;
    property Data: TDateTime read FData write FData;
    property URL: string read FURL write FURL;
    property Headers: TStrings read FHeaders write FHeaders;
    property Body: string read FBody write FBody;
    property Status: Integer read FStatus write FStatus;
  end;

  TACBrOpenDeliveryHTTPRequest = class
  protected
    FRequestId: string;
    FQuery: TStrings;
    FFormUrlEncoded: TStrings;
    FHeaders: TStrings;
    FUsername: string;
    FPassword: string;
    FToken: string;
    FMethodType: TACBrRestMethodType;
    FTimeOut: Integer;
    FAccept: string;
    FContentType: string;
    FBaseURL: string;
    FResource: string;
    FBody: string;
    FProxyHost: string;
    FProxyPort: string;
    FProxyUser: string;
    FProxyPass: string;
    FEnvio: TACBrOpenDeliveryHTTPLogEnvio;
    FResposta: TACBrOpenDeliveryHTTPLogResposta;
    FOnHTTPEnvio: TACBrOpenDeliveryOnHTTPEnviar;
    FOnHTTPResposta: TACBrOpenDeliveryOnHTTPRetornar;
  public
    constructor Create(const ARequestId: string = ''); virtual;
    destructor Destroy; override;
    class function New(const ARequestId: string = ''): TACBrOpenDeliveryHTTPRequest;

    function POST: TACBrOpenDeliveryHTTPRequest;
    function PUT: TACBrOpenDeliveryHTTPRequest;
    function GET: TACBrOpenDeliveryHTTPRequest;
    function DELETE: TACBrOpenDeliveryHTTPRequest;
    function PATCH: TACBrOpenDeliveryHTTPRequest;

    function ProxyHost(const AValue: string): TACBrOpenDeliveryHTTPRequest;
    function ProxyPort(const AValue: string): TACBrOpenDeliveryHTTPRequest;
    function ProxyUser(const AValue: string): TACBrOpenDeliveryHTTPRequest;
    function ProxyPass(const AValue: string): TACBrOpenDeliveryHTTPRequest;

    function BaseURL(const AValue: string): TACBrOpenDeliveryHTTPRequest;
    function Resource(const AValue: string): TACBrOpenDeliveryHTTPRequest;
    function TimeOut(const AValue: Integer): TACBrOpenDeliveryHTTPRequest;

    function ContentType(const AValue: string): TACBrOpenDeliveryHTTPRequest;
    function BasicAuth(AUsername, APassword: string): TACBrOpenDeliveryHTTPRequest;
    function Token(const AValue: string): TACBrOpenDeliveryHTTPRequest;

    function AddOrSetHeader(const AName, AValue: string): TACBrOpenDeliveryHTTPRequest; overload;
    function AddOrSetHeader(const AName: string; const AValue: Integer): TACBrOpenDeliveryHTTPRequest; overload;
    function AddOrSetHeader(const AName: string; const AValue: Double): TACBrOpenDeliveryHTTPRequest; overload;
    function AddOrSetQuery(const AName, AValue: string): TACBrOpenDeliveryHTTPRequest; overload;
    function AddOrSetQuery(const AName: string; const AValue: Integer): TACBrOpenDeliveryHTTPRequest; overload;
    function AddOrSetQuery(const AName: string; const AValue: Double): TACBrOpenDeliveryHTTPRequest; overload;
    function AddOrSetUrlEncoded(const AName, AValue: string): TACBrOpenDeliveryHTTPRequest; overload;
    function AddOrSetUrlEncoded(const AName: string; const AValue: Integer): TACBrOpenDeliveryHTTPRequest; overload;
    function AddOrSetUrlEncoded(const AName: string; const AValue: Double): TACBrOpenDeliveryHTTPRequest; overload;

    function Body(AValue: TACBrJSON; AOwner: Boolean = True): TACBrOpenDeliveryHTTPRequest; overload;
    function Body(const AValue: string): TACBrOpenDeliveryHTTPRequest; overload;

    function Send: TACBrOpenDeliveryHTTPResponse; virtual; abstract;
    procedure Clear; virtual;

    function OnHTTPEnvio(AValue: TACBrOpenDeliveryOnHTTPEnviar): TACBrOpenDeliveryHTTPRequest;
    function OnHTTPResposta(AValue: TACBrOpenDeliveryOnHTTPRetornar): TACBrOpenDeliveryHTTPRequest;
  end;

  TACBrOpenDeliveryHTTPResponse = class
  protected
    FStatusCode: Integer;
    FStatusText: string;
    FBodyText: string;
    FJSONObject: TACBrJSONObject;
    FJSONArray: TACBrJSONArray;
    FHeaders: TStrings;
  public
    destructor Destroy; override;

    function StatusCode: Integer;
    function StatusText: string;
    function GetContent: string;
    function GetJSONObject: TACBrJSONObject;
    function GetJSONArray: TACBrJSONArray;

    function HeaderAsString(AName: String): string;
    function HeaderAsInteger(AName: String): Integer;
    function HeaderAsFloat(AName: String): Double;
  end;

implementation

uses
  ACBrOpenDeliveryHTTPSynapse;

{ TACBrOpenDeliveryHTTPRequest }

function TACBrOpenDeliveryHTTPRequest.AddOrSetHeader(const AName, AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FHeaders.Values[AName] := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.AddOrSetHeader(const AName: string; const AValue: Double): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  AddOrSetHeader(AName, FloatToStr(AValue));
end;

function TACBrOpenDeliveryHTTPRequest.AddOrSetHeader(const AName: string; const AValue: Integer): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  AddOrSetHeader(AName, IntToStr(AValue));
end;

function TACBrOpenDeliveryHTTPRequest.AddOrSetQuery(const AName: string; const AValue: Integer): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  AddOrSetQuery(AName, IntToStr(AValue));
end;

function TACBrOpenDeliveryHTTPRequest.AddOrSetQuery(const AName, AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FQuery.Values[AName] := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.AddOrSetQuery(const AName: string; const AValue: Double): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  AddOrSetQuery(AName, FloatToStr(AValue));
end;

function TACBrOpenDeliveryHTTPRequest.AddOrSetUrlEncoded(const AName, AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FFormUrlEncoded.Values[AName] := AValue;
  FContentType := 'application/x-www-form-urlencoded';
end;

function TACBrOpenDeliveryHTTPRequest.AddOrSetUrlEncoded(const AName: string; const AValue: Integer): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  AddOrSetUrlEncoded(AName, IntToStr(AValue));
end;

function TACBrOpenDeliveryHTTPRequest.AddOrSetUrlEncoded(const AName: string; const AValue: Double): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  AddOrSetUrlEncoded(AName, FloatToStr(AValue));
end;

function TACBrOpenDeliveryHTTPRequest.BaseURL(const AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FBaseURL := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.BasicAuth(AUsername, APassword: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FUsername := AUsername;
  FPassword := APassword;
end;

function TACBrOpenDeliveryHTTPRequest.Body(const AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FBody := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.Body(AValue: TACBrJSON; AOwner: Boolean): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FBody := AValue.ToJSON;
  if AOwner then
    FreeAndNil(AValue);
end;

procedure TACBrOpenDeliveryHTTPRequest.Clear;
begin
  FHeaders.Clear;
  FQuery.Clear;
  FBody := EmptyStr;
  FFormUrlEncoded.Clear;
end;

function TACBrOpenDeliveryHTTPRequest.ContentType(const AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FContentType := AValue;
end;

constructor TACBrOpenDeliveryHTTPRequest.Create(const ARequestId: string);
begin
  FRequestId := ARequestId;
  FQuery := TStringList.Create;
  FFormUrlEncoded := TStringList.Create;
  FHeaders := TStringList.Create;
  FMethodType := mtGet;
  FTimeOut := 60000;
  FContentType := 'application/json';
  FAccept := 'application/json';
  FEnvio := TACBrOpenDeliveryHTTPLogEnvio.Create;
  FResposta := TACBrOpenDeliveryHTTPLogResposta.Create;
end;

function TACBrOpenDeliveryHTTPRequest.DELETE: TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FMethodType := mtDelete;
end;

destructor TACBrOpenDeliveryHTTPRequest.Destroy;
begin
  FQuery.Free;
  FHeaders.Free;
  FFormUrlEncoded.Free;
  FEnvio.Free;
  FResposta.Free;
  inherited;
end;

function TACBrOpenDeliveryHTTPRequest.GET: TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FMethodType := mtGet;
end;

class function TACBrOpenDeliveryHTTPRequest.New(const ARequestId: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := TACBrOpenDeliveryHTTPRequestSynapse.Create(ARequestId);
end;

function TACBrOpenDeliveryHTTPRequest.OnHTTPEnvio(AValue: TACBrOpenDeliveryOnHTTPEnviar): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FOnHTTPEnvio := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.OnHTTPResposta(AValue: TACBrOpenDeliveryOnHTTPRetornar): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FOnHTTPResposta := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.PATCH: TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FMethodType := mtPatch;
end;

function TACBrOpenDeliveryHTTPRequest.POST: TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FMethodType := mtPost;
end;

function TACBrOpenDeliveryHTTPRequest.ProxyHost(const AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FProxyHost := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.ProxyPass(const AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FProxyPass := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.ProxyPort(const AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FProxyPort := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.ProxyUser(const AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FProxyUser := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.PUT: TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FMethodType := mtPut;
end;

function TACBrOpenDeliveryHTTPRequest.Resource(const AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FResource := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.TimeOut(const AValue: Integer): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FTimeOut := AValue;
end;

function TACBrOpenDeliveryHTTPRequest.Token(const AValue: string): TACBrOpenDeliveryHTTPRequest;
begin
  Result := Self;
  FToken := AValue;
end;

{ TACBrOpenDeliveryHTTPResponse }

destructor TACBrOpenDeliveryHTTPResponse.Destroy;
begin
  FreeAndNil(FJSONArray);
  FreeAndNil(FJSONObject);
  FreeAndNil(FHeaders);
  inherited;
end;

function TACBrOpenDeliveryHTTPResponse.GetContent: string;
begin
  Result := FBodyText;
end;

function TACBrOpenDeliveryHTTPResponse.GetJSONArray: TACBrJSONArray;
begin
  if (not Assigned(FJSONArray)) and (GetContent <> '') then
    if GetContent[1] = '[' then // alguns marketplaces não estão retornando
      FJSONArray := TACBrJSONArray.Parse(GetContent);
  Result := FJSONArray;
end;

function TACBrOpenDeliveryHTTPResponse.GetJSONObject: TACBrJSONObject;
begin
  if (not Assigned(FJSONObject)) and (GetContent <> '') then
    if GetContent[1] = '{' then // alguns marketplaces não estão retornando
      FJSONObject := TACBrJSONObject.Parse(GetContent);
  Result := FJSONObject;
end;

function TACBrOpenDeliveryHTTPResponse.HeaderAsFloat(AName: String): Double;
begin
  Result := StrToFloat(FHeaders.Values[AName]);
end;

function TACBrOpenDeliveryHTTPResponse.HeaderAsInteger(AName: String): Integer;
begin
  Result := StrToInt(FHeaders.Values[AName]);
end;

function TACBrOpenDeliveryHTTPResponse.HeaderAsString(AName: String): string;
begin
  Result := FHeaders.Values[AName];
end;

function TACBrOpenDeliveryHTTPResponse.StatusCode: Integer;
begin
  Result := FStatusCode;
end;

function TACBrOpenDeliveryHTTPResponse.StatusText: string;
begin
  Result := FStatusText;
end;

{ TACBrOpenDeliveryHTTPLogEnvio }

procedure TACBrOpenDeliveryHTTPLogEnvio.Clear;
begin
  FId := '';
  FURL := '';
  FMethod := 'GET';
  FBody := '';
  FData := 0;
  FHeaders.Clear;
end;

constructor TACBrOpenDeliveryHTTPLogEnvio.Create;
begin
  inherited Create;
  FHeaders := TStringList.Create;
end;

destructor TACBrOpenDeliveryHTTPLogEnvio.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

{ TACBrOpenDeliveryHTTPLogResposta }

procedure TACBrOpenDeliveryHTTPLogResposta.Clear;
begin
  FId := '';
  FURL := '';
  FBody := '';
  FStatus := 0;
  FData := 0;
  FHeaders.Clear;
end;

constructor TACBrOpenDeliveryHTTPLogResposta.Create;
begin
  inherited Create;
  FHeaders := TStringList.Create;
end;

destructor TACBrOpenDeliveryHTTPLogResposta.Destroy;
begin
  FHeaders.Free;
  inherited;
end;

end.
