{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Classe para Lazarus/Free Pascal e Delphi para requisições SOAP com suporte  }
{ certificados A1 e A3 usando as bibliotecas WinINet e CAPICOM                 }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Jean Patrick Figueiredo dos Santos,            }
{                               André Ferreira de Moraes                       }
{                               Juliomar Marchetti                             }
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

unit ACBrWinINetReqResp;

interface

{$IfDef MSWINDOWS}
uses
  {$IFDEF DELPHIXE4_UP}
  AnsiStrings,
  {$ENDIF}
  Windows, Classes, SysUtils, wininet,
  ACBrWinReqRespClass,
  ACBr_WinCrypt;

const
  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;

type

  { TACBrWinINetReqResp }

  TACBrWinINetReqResp = class(TACBrWinReqResp)
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

uses synautil;

{ TACBrWinINetReqResp }

function TACBrWinINetReqResp.GetLastHttpResultCode: DWORD;
Var
  dummy, AStatusCode, ASize: DWORD;
begin
  Result := 0;
  if Assigned(FpRequest) then
  begin
    dummy := 0;
    AStatusCode := 0;
    ASize := SizeOf(DWORD);
    if HttpQueryInfo( FpRequest,
                      HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,
                      @AStatusCode, ASize,
                      dummy ) then
    begin
      Result := AStatusCode;
    end;
  end;
end;

function TACBrWinINetReqResp.InternalOpenSession: Boolean;
var
  AccessType: DWORD;
begin
  inherited InternalOpenSession;

  if ProxyHost <> '' then
    AccessType := INTERNET_OPEN_TYPE_PROXY
  else
    AccessType := INTERNET_OPEN_TYPE_PRECONFIG;

  FpSession := InternetOpen( PChar('WinINet ACBr/1.0'),
                             AccessType,
                             PChar(ProxyHostAndPort),
                             nil, 0);
  Result := Assigned(FpSession);

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.InternalOpenConnection: Boolean;
begin
  inherited InternalOpenConnection;
  FpConnection := InternetConnect( FpSession,
                                  PChar(Host), PortNumber,
                                  PChar(ProxyUser), PChar(ProxyPass),
                                  INTERNET_SERVICE_HTTP,
                                  0, 0 {cardinal(Self)} );
  Result := Assigned(FpConnection);

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.InternalOpenRequest: Boolean;
var
  RequestFlags: DWORD;
begin
  Result := inherited InternalOpenRequest;
  if not Result then
    Exit;

  if (UseSSL) then
  begin
    RequestFlags := INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_CACHE_WRITE;
    RequestFlags := RequestFlags or INTERNET_FLAG_SECURE;

    if (UseCertificate) then
      RequestFlags := RequestFlags or
                       ( INTERNET_FLAG_IGNORE_CERT_CN_INVALID or
                         INTERNET_FLAG_IGNORE_CERT_DATE_INVALID );
  end
  else
    RequestFlags := INTERNET_SERVICE_HTTP;

  FpRequest := HttpOpenRequest( FpConnection,
                                PChar(Method), PChar(URI),
                                nil, nil, nil, RequestFlags, 0);
  Result:= Assigned(FpRequest);

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.SetConnectionTimeOut: Boolean;
begin
  Result := inherited SetConnectionTimeOut;

  if Result and (TimeOut > 0) then
  begin
    Result := InternetSetOption(FpSession, INTERNET_OPTION_CONNECT_TIMEOUT, @TimeOut, SizeOf(TimeOut)) and
              InternetSetOption(FpSession, INTERNET_OPTION_SEND_TIMEOUT, @TimeOut, SizeOf(TimeOut))    and
              InternetSetOption(FpSession, INTERNET_OPTION_RECEIVE_TIMEOUT, @TimeOut, SizeOf(TimeOut));
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.SetConnectionSSL: Boolean;
begin
  { Não disponível, configuração de SSL deve ser feita no Windows I.E. }
  Result := True;
end;

function TACBrWinINetReqResp.SetConnectionCertificate: Boolean;
begin
  Result := inherited SetConnectionCertificate;

  if Result and UseCertificate then
    Result := InternetSetOption( FpRequest,
                                 INTERNET_OPTION_CLIENT_CERT_CONTEXT,
                                 CertContext, SizeOf(CERT_CONTEXT) );
  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.SetConnectionSecurityFlags: Boolean;
var
  SecurityFlags, FlagsLen: DWORD;
begin
  Result := inherited SetConnectionSecurityFlags;

  if Result then
  begin
    SecurityFlags := 0;
    FlagsLen := SizeOf(SecurityFlags);
    CheckNotAborted;
    // Query actual Flags
    Result:= InternetQueryOption( FpRequest,
                                  INTERNET_OPTION_SECURITY_FLAGS,
                                  @SecurityFlags, FlagsLen );
    if Result then
    begin
      SecurityFlags := SecurityFlags or
                       SECURITY_FLAG_IGNORE_REVOCATION or
                       SECURITY_FLAG_IGNORE_UNKNOWN_CA or
                       SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
                       SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
                       SECURITY_FLAG_IGNORE_WRONG_USAGE;
      Result := InternetSetOption( FpRequest,
                                   INTERNET_OPTION_SECURITY_FLAGS,
                                   @SecurityFlags, FlagsLen );
    end;
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.SetProxyUser: Boolean;
begin
  Result := inherited SetProxyUser;

  if Result and (Trim(ProxyUser) <> '') then
  begin
    Result := InternetSetOption( FpRequest, INTERNET_OPTION_PROXY_USERNAME,
                                 PChar(ProxyUser), Length(ProxyUser));

    if Result and (Trim(ProxyPass) <> '') then
      Result := InternetSetOption( FpRequest, INTERNET_OPTION_PROXY_PASSWORD,
                                   PChar(ProxyPass), Length(ProxyPass));
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.SetHeaderReq: Boolean;
var
  AHeader: String;
begin
  Result := inherited SetHeaderReq;

  if Result then
  begin
    AHeader := CalculateHeaderReq;
    Result := HttpAddRequestHeaders(FpRequest, PChar(AHeader), Length(AHeader), HTTP_ADDREQ_FLAG_ADD);
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.SendData(const AData: AnsiString): Boolean;
begin
  Result := inherited SendData(AData);
  if Result then
    Result := HttpSendRequest(FpRequest, nil, 0, Pointer(AData), Length(AData));

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.ReceiveResponse: Boolean;
var
  BufferSize, dummy: DWORD;
  Buffer: Pointer;
  HeaderStr: String;
begin
  Result := inherited ReceiveResponse;

  if Result then
  begin
    // Quering Headers Size
    BufferSize := 0;
    Buffer := nil;
    dummy := 0;
    HttpQueryInfo( FpRequest,
                   HTTP_QUERY_RAW_HEADERS_CRLF,
                   Buffer,
                   BufferSize,
                   dummy );

    // Allocate memory for the buffer.
    if (GetLastError = ERROR_INSUFFICIENT_BUFFER) then
    begin
      HeaderStr := '';
      Buffer := AllocMem(BufferSize);
      try
        // Now, use WinHttpQueryHeaders to retrieve the header.
        Result := HttpQueryInfo( FpRequest,
                                 HTTP_QUERY_RAW_HEADERS_CRLF,
                                 Buffer,
                                 BufferSize,
                                 dummy );
        if Result then
        begin
          SetLength(HeaderStr, BufferSize);
          Move(Buffer^, HeaderStr[1], BufferSize);
        end;
      finally
        Freemem(Buffer);
      end;

      HeaderResp.Text := HeaderStr;
    end;
  end;

  if not Result then
    UpdateResultCodes;
end;

function TACBrWinINetReqResp.ReadData(ABuffer: Pointer; BufferSize: Integer
  ): Integer;
var
  BytesRead: DWORD;
begin
  Result := inherited ReadData(ABuffer, BufferSize);

  if (Result = 0) then
  begin
    BytesRead := 0;
    if InternetReadFile(FpRequest, ABuffer, BufferSize, BytesRead) then
      Result := BytesRead
    else
      Result := -1;
  end;

  if (Result < 0) then
    UpdateResultCodes;
end;

procedure TACBrWinINetReqResp.CloseConnection;
begin
  if Assigned(FpRequest) then
  begin
    InternetCloseHandle(FpRequest);
    FpRequest := Nil
  end;

  if Assigned(FpConnection) then
  begin
    InternetCloseHandle(FpConnection);
    FpConnection := Nil
  end;

  if Assigned(FpSession) then
  begin
    InternetSetOption(FpSession, INTERNET_OPTION_END_BROWSER_SESSION, nil, 0);
    InternetCloseHandle(FpSession);
    FpSession := Nil
  end;
end;

{$Else}
implementation

{$EndIf}

end.
