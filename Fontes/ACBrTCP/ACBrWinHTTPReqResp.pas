{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Classe para Lazarus/Free Pascal e Delphi para requisições SOAP com suporte  }
{ certificados A1 e A3 usando as bibliotecas WinHTTP                           }
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
{.$Define DEBUG_WINHTTP}

unit ACBrWinHTTPReqResp;

interface

{$IfDef MSWINDOWS}
uses
  {$IFDEF DELPHIXE4_UP}
  AnsiStrings,
  {$ENDIF}
  Windows, Classes, SysUtils,
  ACBrWinReqRespClass,
  ACBr_WinCrypt, ACBr_WinHttp,
  blcksock;

type

  { TACBrWinHTTPReqResp }

  TACBrWinHTTPReqResp = class(TACBrWinReqResp)
  protected
    function GetLastHttpResultCode: DWORD; override;

    function InternalOpenSession: Boolean; override;
    function InternalOpenConnection: Boolean; override;
    function InternalOpenRequest: Boolean; override;
    function SetConnectionTimeOut: Boolean; override;
    function SetConnectionSSL: Boolean; override;
    function SetConnectionCertificate: Boolean; override;
    function SetConnectionSecurityFlags: Boolean; override;
    function SetProxyUser: Boolean; override;
    function SetHeaderReq: Boolean; override;
    function SendData(const AData: AnsiString): Boolean; override;
    function ReceiveResponse: Boolean; override;
    function ReadData(ABuffer: Pointer; BufferSize: Integer): Integer; override;

    procedure CloseConnection; override;
  end;

implementation

uses
  synautil;

{ TACBrWinHTTPReqResp }

function TACBrWinHTTPReqResp.GetLastHttpResultCode: DWORD;
Var
  AStatusCode, ASize: DWORD;
begin
  Result := 0;

  if Assigned(FpRequest) then
  begin
    AStatusCode := 0;
    ASize := SizeOf(DWORD);
    if WinHttpQueryHeaders( FpRequest,
                            WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER,
                            WINHTTP_HEADER_NAME_BY_INDEX,
                            @AStatusCode, @ASize,
                            WINHTTP_NO_HEADER_INDEX ) then
    begin
      Result := AStatusCode;
    end;
  end;
end;

function TACBrWinHTTPReqResp.InternalOpenSession: Boolean;
var
  AccessType: DWORD;
  HttpProxyHostName, HttpProxyByPass: LPCWSTR;
begin
  inherited InternalOpenSession;

  if (ProxyHost <> '') then
  begin
    AccessType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
    HttpProxyHostName := LPCWSTR( WideString(ProxyHostAndPort) );
    HttpProxyByPass := LPCWSTR( WideString('localhost') );
  end
  else
  begin
    AccessType := WINHTTP_ACCESS_TYPE_DEFAULT_PROXY;
    HttpProxyHostName := WINHTTP_NO_PROXY_NAME;
    HttpProxyByPass := WINHTTP_NO_PROXY_BYPASS;
  end;

  FpSession := WinHttpOpen( LPCWSTR(WideString('WinHTTP ACBr/1.0')),
                            AccessType,
                            HttpProxyHostName, HttpProxyByPass, 0 );
  Result := Assigned(FpSession);

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.InternalOpenConnection: Boolean;
begin
  Result := inherited InternalOpenConnection;

  if Result then
  begin
    FpConnection := WinHttpConnect( FpSession,
                                    LPCWSTR(WideString(Host)),
                                    WORD(PortNumber), 0);
    Result := Assigned(FpConnection);
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.InternalOpenRequest: Boolean;
var
  RequestFlags: DWORD;
begin
  Result := inherited InternalOpenRequest;

  if Result then
  begin
    if UseSSL then
      RequestFlags := WINHTTP_FLAG_SECURE
    else
      RequestFlags := 0;

    FpRequest := WinHttpOpenRequest( FpConnection,
                                     LPCWSTR(WideString(Method)),
                                     LPCWSTR(WideString(URI)),
                                     Nil,
                                     WINHTTP_NO_REFERER,
                                     WINHTTP_DEFAULT_ACCEPT_TYPES,
                                     RequestFlags);
    Result := Assigned(FpRequest);
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.SetConnectionTimeOut: Boolean;
begin
  Result := inherited SetConnectionTimeOut;

  if Result and (TimeOut > 0) then
  begin
    //Result := WinHttpSetOption( pSession,
    //                            WINHTTP_OPTION_CONNECT_TIMEOUT,
    //                            @TimeOut,
    //                            SizeOf(TimeOut));
    Result := WinHttpSetTimeouts( FpSession, TimeOut, TimeOut, TimeOut, TimeOut);
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.SetConnectionSSL: Boolean;
var
  Flags, FlagsLen: DWORD;
begin
  Result := inherited SetConnectionSSL;

  if Result and UseSSL then
  begin
    case SSLType of
      LT_SSLv2:
        Flags := WINHTTP_FLAG_SECURE_PROTOCOL_SSL2;
      LT_SSLv3:
        Flags := WINHTTP_FLAG_SECURE_PROTOCOL_SSL3;
      LT_TLSv1:
        Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1;
      LT_TLSv1_1:
        Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1;
      LT_TLSv1_2:
        Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;
      LT_TLSv1_3:
        Flags := WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_3;
    else
      Flags := WINHTTP_FLAG_SECURE_PROTOCOL_ALL;
    end;

    FlagsLen := SizeOf(Flags);
    Result := WinHttpSetOption(FpSession, WINHTTP_OPTION_SECURE_PROTOCOLS, @Flags, FlagsLen);
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.SetConnectionCertificate: Boolean;
begin
  Result := inherited SetConnectionCertificate;

  if Result and UseCertificate then
  begin
    Result := WinHttpSetOption( FpRequest,
                                WINHTTP_OPTION_CLIENT_CERT_CONTEXT,
                                CertContext, SizeOf(CERT_CONTEXT));
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.SetConnectionSecurityFlags: Boolean;
var
  SecurityFlags, FlagsLen: DWORD;
begin
  Result := inherited SetConnectionSecurityFlags;

  if Result then
  begin
    // Ignorando alguns erros de conexão //
    SecurityFlags := 0;
    FlagsLen := SizeOf(SecurityFlags);
    // Query actual Flags
    Result := WinHttpQueryOption( FpRequest, WINHTTP_OPTION_SECURITY_FLAGS,
                                  @SecurityFlags, @FlagsLen );
    if Result then
    begin
      SecurityFlags := SecurityFlags or
                       SECURITY_FLAG_IGNORE_UNKNOWN_CA or
                       SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE or
                       SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
                       SECURITY_FLAG_IGNORE_CERT_DATE_INVALID;
      Result := WinHttpSetOption( FpRequest, WINHTTP_OPTION_SECURITY_FLAGS,
                                  @SecurityFlags, FlagsLen);
    end;
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.SetProxyUser: Boolean;
begin
  Result := inherited SetProxyUser;

  if Result and (Trim(ProxyUser) <> '') then
  begin
    Result := WinHttpSetOption( FpRequest, WINHTTP_OPTION_PROXY_USERNAME,
                                @ProxyUser, Length(ProxyUser));

    if Result and (Trim(ProxyPass) <> '') then
      Result := WinHttpSetOption( FpRequest, WINHTTP_OPTION_PROXY_PASSWORD,
                                  @ProxyPass, Length(ProxyPass));
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.SetHeaderReq: Boolean;
begin
  Result := inherited SetHeaderReq;
end;

function TACBrWinHTTPReqResp.SendData(const AData: AnsiString): Boolean;
var
  AHeader: String;
  wHeader: WideString;
  BytesWrite: DWORD;
begin
  Result := inherited SendData(AData);

  if Result then
  begin
    AHeader := CalculateHeaderReq;
    wHeader := WideString(AHeader);

    Result := WinHttpSendRequest( FpRequest,
                                  LPCWSTR(wHeader), Length(wHeader),
                                  WINHTTP_NO_REQUEST_DATA, 0,
                                  Length(AData), 0);
    if Result then
    begin
      CheckNotAborted;
      BytesWrite := 0;
      if (Length(AData) > 0) then
        Result := WinHttpWriteData( FpRequest,
                                    PAnsiChar(AData), Length(AData),
                                    @BytesWrite );
    end;
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.ReceiveResponse: Boolean;
var
  BufferSize: DWORD;
  Buffer: Pointer;
  wHeaderStr: WideString;
begin
  Result := inherited ReceiveResponse;

  if Result then
  begin
    Result := WinHttpReceiveResponse( FpRequest, nil );

    if Result then
    begin
      // Quering Headers Size
      BufferSize := 0;
      Buffer := nil;
      WinHttpQueryHeaders( FpRequest,
                           WINHTTP_QUERY_RAW_HEADERS_CRLF,
                           WINHTTP_HEADER_NAME_BY_INDEX,
                           Buffer,
                           @BufferSize,
                           WINHTTP_NO_HEADER_INDEX);

      // Allocate memory for the buffer.
      if (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
      begin
        wHeaderStr := '';
        Buffer := AllocMem(BufferSize);
        try
          // Now, use WinHttpQueryHeaders to retrieve the header.
           Result := WinHttpQueryHeaders( FpRequest,
                                          WINHTTP_QUERY_RAW_HEADERS_CRLF,
                                          WINHTTP_HEADER_NAME_BY_INDEX,
                                          Buffer,
                                          @BufferSize,
                                          WINHTTP_NO_HEADER_INDEX);
           if Result then
           begin
             SetLength(wHeaderStr, trunc(BufferSize/2));
             Move(Buffer^, wHeaderStr[1], BufferSize);
           end;
        finally
          Freemem(Buffer);
        end;

        HeaderResp.Text := String(wHeaderStr);
      end;
    end;
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinHTTPReqResp.ReadData(ABuffer: Pointer; BufferSize: Integer
  ): Integer;
var
  BytesRead: DWORD;
begin
  Result := inherited ReadData(ABuffer, BufferSize);

  if (Result = 0) then
  begin
    BytesRead := 0;
    if WinHttpReadData( FpRequest, ABuffer, BufferSize, @BytesRead) then
      Result := BytesRead
    else
      Result := -1;
  end;

  if (Result < 0) then
    UpdateResultCodes;
end;

procedure TACBrWinHTTPReqResp.CloseConnection;
begin
  if Assigned(FpRequest) then
  begin
    WinHttpCloseHandle(FpRequest);
    FpRequest := Nil
  end;

  if Assigned(FpConnection) then
  begin
    WinHttpCloseHandle(FpConnection);
    FpConnection := Nil
  end;

  if Assigned(FpSession) then
  begin
    WinHttpCloseHandle(FpSession);
    FpSession := Nil
  end;
end;

{$Else}
implementation

{$EndIf}

end.


