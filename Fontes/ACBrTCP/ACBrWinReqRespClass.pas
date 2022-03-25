{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Classe para Lazarus/Free Pascal e Delphi para requisições SOAP com suporte  }
{ certificados A1 e A3 usando as bibliotecas WinINet e CAPICOM                 }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrWinReqRespClass;

interface

{$IfDef MSWINDOWS}
uses
  Windows, Classes, SysUtils,
  ACBr_WinCrypt, ACBr_WinHttp, ACBrBase,
  blcksock;

const
  CBufferSize = 32768;  // 32K
  CMaxRedirections = 10;
  CTimeOutDef = 10000;   // 10s
  CMimeTypeDef = 'text/html';
  CMethodDef = 'GET';
  CCharSetDef = 'utf-8';

resourcestring
  sErrOpenHTTP = 'Falha abrindo Sessão HTTP. %s';
  sErrOpenConnect = 'Falha abrindo Conexão HTTP. %s';
  sErrOpenRequest = 'Falha abrindo Requisição HTTP. %s';
  sErrSetTimeOut = 'Falha ajustando Timeout da Conexão. %s';
  sErrSetSSL = 'Falha ajustando SSL. %s';
  sErrSetCertificate = 'Falha ajustando certificado na Conexão. %s';
  sErrSetSecurityFlags = 'Falha ajustando parâmetros de segurança na Conexão. %s';
  sErrSetProxyUser = 'Falha ajustando Usuário no Proxy. %s';
  sErrSetHeader = 'Falha ajustando o Cabeçalho da Requisição. %s';
  sErrSentData = 'Falha Enviando a Requisição. %s';
  sErrReceiveResponse = 'Falha ao Receber Resposta. %s';
  sErrReadData = 'Falha ao Ler Dados da Resposta. %s';

  sErrConnectionAborted = 'Conexão foi abortada';

type

  { EACBrWinReqResp }

  EACBrWinReqResp = class(Exception)
  public
    constructor Create(const Msg: String);
  end;

  { TACBrWinReqResp }

  TACBrWinReqResp = class
  private
    FCertContext: PCCERT_CONTEXT;
    FHeaderReq: THttpHeader;
    FHeaderResp: THttpHeader;
    FRedirections: Integer;
    FInternalErrorCode, FHttpResultCode: Integer;
    FEncodeDataToUTF8: Boolean;
    FLogFile: String;
    FMethod: String;
    FSOAPAction: String;
    FMimeType: String;  // (ex.: 'application/soap+xml' ou 'text/xml' - que é o Content-Type)
    FCharsets: String; //  (ex.: 'ISO-8859-1,utf-8' - que é o Accept-Charset)
    FData: AnsiString;
    FProxyHost: String;
    FProxyPass: String;
    FProxyPort: String;
    FProxyUser: String;
    FSSLType: TSSLType;
    FTimeOut: Integer;
    FURL: String;
    FURI: String;
    FProt: String;
    FHost: String;
    FPort: String;
    function GetPortNumber: Word;
    function GetProxyHostAndPort: String;
    procedure SetMethod(AValue: String);
    procedure SetURL(AValue: String);
  protected
    FpSession, FpConnection, FpRequest: Pointer;

    procedure UpdateResultCodes;
    function GetLastHttpResultCode: DWORD; virtual;
    procedure CheckNotAborted;
    function IsConnectionActive: Boolean; virtual;
    function IsHttpRedirection: Boolean;
    procedure AdjustForRedirection;

    function HasLogFile: Boolean;
    procedure DoLog(AMsg: AnsiString; Translate: Boolean = False);

    procedure FindIEProxy;
    function CalculateHeaderReq: String;
    function UseSSL: Boolean;
    function UseCertificate: Boolean;

    procedure InitExecute; virtual;
    procedure OpenSession;
    function InternalOpenSession: Boolean; virtual;
    procedure OpenConnection;
    function InternalOpenConnection: Boolean; virtual;
    procedure OpenRequest;
    function InternalOpenRequest: Boolean; virtual;
    procedure Send;
    function SetHeaderReq: Boolean; virtual;
    function SendData(const AData: AnsiString): Boolean; virtual;
    procedure Receive(Resp: TStream);
    function ReceiveResponse: Boolean; virtual;
    function ReadData(ABuffer: Pointer; BufferSize: Integer): Integer; virtual;

    procedure CheckProxyParams;
    function SetConnectionTimeOut: Boolean; virtual;
    function SetConnectionSSL: Boolean; virtual;
    function SetConnectionCertificate: Boolean; virtual;
    function SetConnectionSecurityFlags: Boolean; virtual;
    function SetProxyUser: Boolean; virtual;

    procedure CloseConnection; virtual;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    property LogFile: String read FLogFile write FLogFile;
    property CertContext: PCCERT_CONTEXT read FCertContext write FCertContext;
    property HeaderReq: THttpHeader read FHeaderReq;
    property HeaderResp: THttpHeader read FHeaderResp;
    property SOAPAction: String read FSOAPAction write FSOAPAction;
    property MimeType: String read FMimeType write FMimeType;
    property Method: String read FMethod write SetMethod;
    property Charsets: String read FCharsets write FCharsets;
    property URL: String read FURL write SetURL;
    property URI: String read FURI;
    property Prot: String read FProt;
    property PortNumber: Word read GetPortNumber;
    property Host: String read FHost;
    property Port: String read FPort;
    property Data: AnsiString read FData write FData;

    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: String read FProxyPort write FProxyPort;
    property ProxyUser: String read FProxyUser write FProxyUser;
    property ProxyPass: String read FProxyPass write FProxyPass;
    property ProxyHostAndPort: String read GetProxyHostAndPort;

    property EncodeDataToUTF8: Boolean read FEncodeDataToUTF8 write FEncodeDataToUTF8;
    property TimeOut: Integer read FTimeOut write FTimeOut default CTimeOutDef;
    property SSLType: TSSLType read FSSLType write FSSLType;

    property HttpResultCode: Integer read FHttpResultCode;
    property InternalErrorCode: Integer read FInternalErrorCode;
    function GetWinInetError(ErrorCode: DWORD): String; virtual;

    procedure Execute(Resp: TStream); overload; virtual;
    procedure Execute(const DataToPost: AnsiString; AResp: TStream); overload;

    procedure Abortar; virtual;
  end;

implementation

uses
  strutils, wininet,
  synautil,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO;

{ EACBrWinINetReqResp }

constructor EACBrWinReqResp.Create(const Msg: String);
begin
  inherited Create(ACBrStr(Msg));
end;

{ TACBrWinINetReqResp }

constructor TACBrWinReqResp.Create;
begin
  FpConnection := nil;
  FpSession := nil;
  FpRequest := nil;
  FHeaderReq := THttpHeader.Create;
  FHeaderResp := THttpHeader.Create;
  Clear;

  //DEBUG
  //FLogFile := 'C:\temp\ACBrWinReqResp.txt';
end;

destructor TACBrWinReqResp.Destroy;
begin
  Abortar;
  FHeaderReq.Free;
  FHeaderResp.Free;
  inherited Destroy;
end;

procedure TACBrWinReqResp.Clear;
begin
  FData := '';
  FHeaderReq.Clear;
  FHeaderResp.Clear;
  FTimeOut := CTimeOutDef;
  FMethod := CMethodDef;
  FMimeType := CMimeTypeDef;
  FCharsets := CCharSetDef;
  FSOAPAction := '';
  FCertContext := Nil;
  FEncodeDataToUTF8 := False;
  FSSLType := LT_all;
  FRedirections := 0;
  FHttpResultCode := 0;
  FInternalErrorCode := 0;
end;

function TACBrWinReqResp.GetWinInetError(ErrorCode: DWORD): String;
var
  ErrorMsg: AnsiString;
  Len: DWORD;
  WinINetHandle: HMODULE;
begin
  Result := '';

  WinINetHandle := GetModuleHandle('wininet.dll');
  ErrorMsg := Space(1024);
  Len := FormatMessage( FORMAT_MESSAGE_FROM_SYSTEM or
                        FORMAT_MESSAGE_FROM_HMODULE or
                        FORMAT_MESSAGE_IGNORE_INSERTS or
                        FORMAT_MESSAGE_MAX_WIDTH_MASK or
                        FORMAT_MESSAGE_ARGUMENT_ARRAY,
                        @WinINetHandle,
                        ErrorCode,
                        0,
                        @ErrorMsg[1], 1024,
                        nil);
  if (Len > 0) then
    ErrorMsg := Trim(ErrorMsg)
  else
  begin
    case ErrorCode of
       ERROR_WINHTTP_TIMEOUT:
         ErrorMsg := 'TimeOut de Requisição';
       ERROR_WINHTTP_NAME_NOT_RESOLVED:
         ErrorMsg := 'O nome do servidor não pode ser resolvido';
       ERROR_WINHTTP_CANNOT_CONNECT:
         ErrorMsg := 'Conexão com o Servidor falhou';
       ERROR_WINHTTP_CONNECTION_ERROR:
         ErrorMsg := 'A conexão com o servidor foi redefinida ou encerrada, ou um protocolo SSL incompatível foi encontrado';
       ERROR_INTERNET_CONNECTION_RESET:
         ErrorMsg := 'A conexão com o servidor foi redefinida';
       ERROR_WINHTTP_SECURE_INVALID_CA:
         ErrorMsg := 'Certificado raiz não é confiável pelo provedor de confiança';
       ERROR_WINHTTP_SECURE_CERT_REV_FAILED:
         ErrorMsg := 'Revogação do Certificado não pode ser verificada porque o servidor de revogação está offline';
       ERROR_WINHTTP_SECURE_CHANNEL_ERROR:
         ErrorMsg := 'Erro relacionado ao Canal Seguro';
       ERROR_WINHTTP_SECURE_FAILURE:
         ErrorMsg := 'Um ou mais erros foram encontrados no certificado Secure Sockets Layer (SSL) enviado pelo servidor';
       ERROR_WINHTTP_CLIENT_CERT_NO_PRIVATE_KEY:
         ErrorMsg := 'O contexto para o certificado de cliente SSL não tem uma chave privada associada a ele. O certificado de cliente pode ter sido importado para o computador sem a chave privada';
       ERROR_WINHTTP_CLIENT_CERT_NO_ACCESS_PRIVATE_KEY:
         ErrorMsg := 'Falha ao obter a Chave Privada do Certificado para comunicação segura';
    else
      ErrorMsg := '';
    end;
  end;

  if (ErrorMsg <> '') then
    Result := ACBrStr(ErrorMsg);

  if (ErrorCode > 0) then
    Result := IntToStr(ErrorCode) + ' - ' + Result;

  if (Result <> '') then
    Result := 'Erro: ' + Result;

end;

procedure TACBrWinReqResp.SetURL(AValue: String);
var
  ANone: String;
begin
  if FURL = AValue then
    Exit;

  FProt := ''; FHost := ''; FPort := ''; ANone := '';
  FURL  := AValue;
  FURI := ParseURL(FURL, FProt, ANone, ANone, FHost, FPort, ANone, ANone);
end;

function TACBrWinReqResp.GetPortNumber: Word;
begin
  Result := StrToIntDef(FPort, 0);

  if (Result = 0) then
  begin
    if UseSSL then
      Result := INTERNET_DEFAULT_HTTPS_PORT
    else
      Result := INTERNET_DEFAULT_HTTP_PORT;
  end;
end;

function TACBrWinReqResp.GetProxyHostAndPort: String;
begin
  Result := ProxyHost;
  if (StrToIntDef(ProxyPort, 0) <> 0) then
    Result := ProxyHost + ':' + ProxyPort;
end;

procedure TACBrWinReqResp.SetMethod(AValue: String);
begin
  if FMethod = AValue then Exit;
  FMethod := UpperCase(AValue);
end;

procedure TACBrWinReqResp.UpdateResultCodes;
begin
  FInternalErrorCode := GetLastError;
  FHttpResultCode := GetLastHttpResultCode;
end;

function TACBrWinReqResp.GetLastHttpResultCode: DWORD;
begin
  Result := 0;
end;

procedure TACBrWinReqResp.CheckNotAborted;
begin
  if not IsConnectionActive then
    raise EACBrWinReqResp.Create(sErrConnectionAborted);
end;

function TACBrWinReqResp.IsConnectionActive: Boolean;
begin
  Result := Assigned(FpSession);
end;

function TACBrWinReqResp.IsHttpRedirection: Boolean;
begin
  // Note: apparently, WinHTTP and WinINet already take care of redirects
  // https://tools.ietf.org/html/rfc2616#page-62
  case FHttpResultCode of
    301,  // Moved Permanently
    302,  // Found
    303,  // See Other
    307:  // Temporary Redirect
      Result := True;
  else
    Result := False;
  end;

  Result := Result and (FRedirections < CMaxRedirections);

  if Result then
    AdjustForRedirection;
end;

procedure TACBrWinReqResp.AdjustForRedirection;

  function GetURLBasePath(const AURL: String): String;
  begin
    Result := Copy(AURL, 1, PosLast('/',AURL) );
  end;

  function IsAbsoluteURL(const AURL: String): Boolean;
  begin
    Result := (Pos('//', AURL) = 1) or
              (Pos('://', AURL) > 0) or
              (Pos('www.', AURL) = 1);
  end;

  procedure ClearRedirect;
  begin
    FData := '';
    FHeaderReq.Clear;
    FHeaderResp.Clear;
    FMimeType := CMimeTypeDef;
  end;

  function NeedChangeMethod: Boolean;
  begin
    Result := (FHttpResultCode = 303) or
              ( (Method = 'POST') and
                ((FHttpResultCode = 301) or (FHttpResultCode = 302))
              );
  end;

var
  NewLocation: String;
begin
  // https://developer.mozilla.org/pt-BR/docs/Web/HTTP/Redirecionamento

  Inc( FRedirections );
  DoLog('AdjustForRedirection - '+IntToStr(FRedirections));

  NewLocation := FHeaderResp.GetHeaderValue('Location:');
  NewLocation := Trim(SeparateLeft( NewLocation, ';' ));
  DoLog('  Location: '+NewLocation);

  if IsAbsoluteURL(NewLocation) then
    URL := NewLocation
  else
    URL := GetURLBasePath( FURL ) + NewLocation;

  ClearRedirect;

  if (FMethod <> CMethodDef) and NeedChangeMethod then
    Method := CMethodDef;
end;

function TACBrWinReqResp.HasLogFile: Boolean;
begin
  Result := (LogFile <> '')
end;

procedure TACBrWinReqResp.DoLog(AMsg: AnsiString; Translate: Boolean);
begin
  if HasLogFile then
    WriteLog(FLogFile, FormatDateTime('hh:nn:ss:zzz', Now) + ' - ' +AMsg, Translate);
end;

procedure TACBrWinReqResp.FindIEProxy;
var
  pProxyConfig: TWinHttpCurrentUserIEProxyConfig;
  IEProxyHost: String;
  P: Integer;
begin
  ZeroMemory(@pProxyConfig, SizeOf(pProxyConfig));
  if WinHttpGetIEProxyConfigForCurrentUser(pProxyConfig) then
  begin
    IEProxyHost := String( pProxyConfig.lpszProxy );
    DoLog('  FindIEProxy: '+IEProxyHost);

    P := pos(':',IEProxyHost);
    if (P > 0) then
    begin
      ProxyHost := copy(IEProxyHost, 1, P-1);
      ProxyPort := copy(IEProxyHost, P+1, Length(IEProxyHost));
    end
    else
      ProxyHost := IEProxyHost;
  end;
end;

function TACBrWinReqResp.CalculateHeaderReq: String;
var
  AHost: String;
begin
  if ( (FPort <> IntToStr(INTERNET_DEFAULT_HTTP_PORT)) and (not UseSSL) ) or
     ( (FPort <> IntToStr(INTERNET_DEFAULT_HTTPS_PORT)) and (UseSSL) ) then
    AHost := FHost +':'+ FPort
  else
    AHost := FHost;


  HeaderReq.Insert(0, 'Host: '+ AHost);
  if (FMimeType <> '') then
    HeaderReq.AddHeader('Content-Type', FMimeType + IfThen(FCharsets <> '','; charset=' + FCharsets, '') );

  if (FCharsets <> '') then
    HeaderReq.AddHeader('Accept-Charset', FCharsets);

  if (FSOAPAction <> '') then
    HeaderReq.AddHeader('SOAPAction', AnsiQuotedStr(FSOAPAction, '"'));

  Result := HeaderReq.Text;
end;

function TACBrWinReqResp.UseSSL: Boolean;
begin
  Result := (UpperCase(FProt) = 'HTTPS');
end;

function TACBrWinReqResp.UseCertificate: Boolean;
begin
  Result := UseSSL and Assigned(FCertContext);
end;

procedure TACBrWinReqResp.InitExecute;
begin
  CloseConnection;
  FHeaderResp.Clear;
  FRedirections := 0;
end;

procedure TACBrWinReqResp.OpenSession;
var
  Ok: Boolean;
begin
  DoLog('OpenSession, URL: '+FURL+', Proxy: '+ProxyHostAndPort);

  CheckProxyParams;

  Ok := InternalOpenSession;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrOpenHTTP), [GetWinInetError(FInternalErrorCode)] );

  Ok := SetConnectionTimeOut;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrSetTimeOut), [GetWinInetError(FInternalErrorCode)]);

  Ok := SetConnectionSSL;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrSetSSL), [GetWinInetError(FInternalErrorCode)]);
end;

function TACBrWinReqResp.InternalOpenSession: Boolean;
begin
  Result := True;
  { sobrescrever }
end;

procedure TACBrWinReqResp.OpenConnection;
var
  Ok: Boolean;
begin
  DoLog('OpenConnection, Port: '+FPort+', Host: '+FHost);

  Ok := InternalOpenConnection;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrOpenConnect), [GetWinInetError(FInternalErrorCode)] );
end;

function TACBrWinReqResp.InternalOpenConnection: Boolean;
begin
  CheckNotAborted;
  Result := True;
end;

procedure TACBrWinReqResp.OpenRequest;
var
  Ok: Boolean;
begin
  DoLog('OpenRequest, Method: '+FMethod+', URI: '+FURI+', UseSSL: '+BoolToStr(UseSSL, True));

  Ok := InternalOpenRequest;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrOpenRequest), [GetWinInetError(FInternalErrorCode)] );

  Ok := SetConnectionCertificate;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrSetCertificate), [GetWinInetError(FInternalErrorCode)]);

  Ok := SetConnectionSecurityFlags;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrSetSecurityFlags), [GetWinInetError(FInternalErrorCode)]);

  Ok := SetProxyUser;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrSetProxyUser), [GetWinInetError(FInternalErrorCode)] );
end;

function TACBrWinReqResp.InternalOpenRequest: Boolean;
begin
  CheckNotAborted;
  Result := True;
end;

procedure TACBrWinReqResp.Send;
var
  Ok: Boolean;
  DataToSend: AnsiString;
begin
  DoLog('Send');

  Ok := SetHeaderReq;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrSetHeader), [GetWinInetError(FInternalErrorCode)] );

  if EncodeDataToUTF8 then
    DataToSend := UTF8Encode(Data)
  else
    DataToSend := Data;

  Ok := SendData(DataToSend);
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrSentData), [GetWinInetError(FInternalErrorCode)] );

  FHeaderReq.Clear;
end;

function TACBrWinReqResp.SetHeaderReq: Boolean;
begin
  if HasLogFile then
    DoLog('  SetHeaderReq:' + sLineBreak + CalculateHeaderReq);

  CheckNotAborted;
  Result := Assigned(FpRequest);
end;

function TACBrWinReqResp.SendData(const AData: AnsiString): Boolean;
begin
  if HasLogFile then
    DoLog('  SendData: '+IntToStr(Length(AData))+' bytes' + sLineBreak + AData);

  CheckNotAborted;
  Result := Assigned(FpRequest);
end;

procedure TACBrWinReqResp.Receive(Resp: TStream);
var
  Ok: Boolean;
  BytesRead: Integer;
  ABuffer: Pointer;
  DataResp: AnsiString;
  HeaderStr: String;
begin
  DoLog('Receive');

  Ok := ReceiveResponse;
  if not Ok then
    raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrReceiveResponse), [GetWinInetError(FInternalErrorCode)] );

  if HasLogFile then
  begin
    HeaderStr := FHeaderResp.Text;
    DoLog('  Response Header: '+IntToStr(Length(HeaderStr))+' bytes' + sLineBreak + HeaderStr);
  end;

  ABuffer := AllocMem(CBufferSize);
  try
    repeat
      BytesRead := ReadData(ABuffer, CBufferSize);
      if (BytesRead < 0) then
        raise EACBrWinReqResp.CreateFmt(ACBrStr(sErrReadData), [GetWinInetError(FInternalErrorCode)] );

      DoLog('  BytesRead: '+IntToStr(BytesRead));
      if (BytesRead > 0) then
        Resp.Write(ABuffer^, BytesRead);
    until (BytesRead = 0);

    if HasLogFile and (Resp.Size > 0) then
    begin
      Resp.Position := 0;
      DataResp := ReadStrFromStream(Resp, Resp.Size);
      DoLog('  Response Data: '+IntToStr(Resp.Size)+' bytes' + sLineBreak + DataResp);
    end;
  finally
    Resp.Position := 0;
    Freemem(ABuffer);
  end;
end;

function TACBrWinReqResp.ReceiveResponse: Boolean;
begin
  DoLog('  ReceiveResponse');
  CheckNotAborted;
  Result := Assigned(FpRequest);
end;

function TACBrWinReqResp.ReadData(ABuffer: Pointer; BufferSize: Integer
  ): Integer;
begin
  DoLog('  ReadData, BufferSize: '+IntToStr(BufferSize));
  CheckNotAborted;
  if Assigned(FpRequest) then
    Result := 0
  else
    Result := -1;
end;

procedure TACBrWinReqResp.CheckProxyParams;
begin
  if (ProxyHost = '') then  // No Proxy ? Query Windows for proxy configuration...
    FindIEProxy;

  DoLog('  CheckProxyParams: '+ProxyHostAndPort);
end;

function TACBrWinReqResp.SetConnectionTimeOut: Boolean;
begin
  DoLog('  SetConnectionTimeOut '+IntToStr(TimeOut));
  CheckNotAborted;
  Result := Assigned(FpSession);
  { sobrescrever com inherited }
end;

function TACBrWinReqResp.SetConnectionSSL: Boolean;
begin
  DoLog('  SetConnectionSSL, UseSSL: '+BoolToStr(UseSSL, True));
  CheckNotAborted;
  Result := Assigned(FpSession);
  { sobrescrever com inherited }
end;

function TACBrWinReqResp.SetConnectionCertificate: Boolean;
begin
  DoLog('  SetConnectionCertificate, UseSSL: '+BoolToStr(UseSSL, True));
  CheckNotAborted;
  Result := Assigned(FpRequest);
  { sobrescrever com inherited }
end;

function TACBrWinReqResp.SetConnectionSecurityFlags: Boolean;
begin
  DoLog('  SetConnectionSecurityFlags, UseSSL: '+BoolToStr(UseSSL, True));
  CheckNotAborted;
  Result := Assigned(FpRequest);
  { sobrescrever com inherited }
end;

function TACBrWinReqResp.SetProxyUser: Boolean;
begin
  DoLog('  SetProxyUser, User: '+ProxyUser);
  CheckNotAborted;
  Result := Assigned(FpRequest);
  { sobrescrever com inherited }
end;

procedure TACBrWinReqResp.CloseConnection;
begin
  { sobrescrever };
end;

procedure TACBrWinReqResp.Execute(const DataToPost: AnsiString; AResp: TStream);
begin
  FData := DataToPost;
  Method := 'POST';
  Execute(AResp);
end;

procedure TACBrWinReqResp.Execute(Resp: TStream);
begin
  InitExecute;
  try
    OpenSession;
    OpenConnection;
    OpenRequest;
    repeat
      Send;
      Receive(Resp);
      UpdateResultCodes;
    until (not IsHttpRedirection) ;
  finally
    CloseConnection;
  end;
end;

procedure TACBrWinReqResp.Abortar;
begin
  CloseConnection;
end;

{$Else}
implementation

{$EndIf}

end.

