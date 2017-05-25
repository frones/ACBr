{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Classe para Lazarus/Free Pascal e Delphi para requisições SOAP com suporte  }
{ certificados A1 e A3 usando as bibliotecas WinINet e CAPICOM                 }
{ Direitos Autorais Reservados (c) 2014 Jean Patrick Figueiredo dos Santos     }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{ Colaboradores nesse arquivo:                                                 }
{                                       Juliomar Marchetti                     }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 15/01/2014: Jean Patrick Figueiredo dos Santos
|*  - Contribuição da classe para o Projeto ACBr
******************************************************************************}

{$I ACBr.inc}

unit ACBrWinReqRespClass;

interface

{$IfDef MSWINDOWS}
uses
  Windows, Classes, SysUtils,
  ACBr_WinCrypt,
  blcksock;

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
    FEncodeDataToUTF8: Boolean;
    FSOAPAction: String;
    FMimeType: String;
    // (ex.: 'application/soap+xml' ou 'text/xml' - que é o Content-Type)
    FCharsets: String; //  (ex.: 'ISO-8859-1,utf-8' - que é o Accept-Charset)
    FData: AnsiString;
    FProxyHost: String;
    FProxyPass: String;
    FProxyPort: String;
    FProxyUser: String;
    FSSLType: TSSLType;
    FTimeOut: Integer;
    FUrl: String;
  protected
    FpInternalErrorCode: Integer;
    FpHTTPResultCode: Integer;

    function GetWinInetError(ErrorCode: DWORD): String; virtual;
    procedure UpdateErrorCodes(ARequest: Pointer); virtual;

  public
    constructor Create;

    property CertContext: PCCERT_CONTEXT read FCertContext write FCertContext;
    property SOAPAction: String read FSOAPAction write FSOAPAction;
    property MimeType: String read FMimeType write FMimeType;
    property Charsets: String read FCharsets write FCharsets;
    property Url: String read FUrl write FUrl;
    property Data: AnsiString read FData write FData;
    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: String read FProxyPort write FProxyPort;
    property ProxyUser: String read FProxyUser write FProxyUser;
    property ProxyPass: String read FProxyPass write FProxyPass;
    property EncodeDataToUTF8: Boolean read FEncodeDataToUTF8 write FEncodeDataToUTF8;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property SSLType: TSSLType read FSSLType write FSSLType;

    property HTTPResultCode: Integer read FpHTTPResultCode;
    property InternalErrorCode: Integer read FpInternalErrorCode;

    procedure Execute(Resp: TStream); overload; virtual;
    procedure Execute(const DataMsg: String; Resp: TStream); overload;
  end;

implementation

uses
  ACBrUtil, ACBr_WinHttp, wininet;

{ EACBrWinINetReqResp }

constructor EACBrWinReqResp.Create(const Msg: String);
begin
  inherited Create(ACBrStr(Msg));
end;

{ TACBrWinINetReqResp }

constructor TACBrWinReqResp.Create;
begin
  FMimeType := 'application/soap+xml';
  FCharsets := 'utf-8';
  FSOAPAction := '';
  FCertContext := Nil;
  FTimeOut := 0;
  FEncodeDataToUTF8 := False;
  FSSLType := LT_all;

  FpHTTPResultCode := 0;
  FpInternalErrorCode := 0;
end;

function TACBrWinReqResp.GetWinInetError(ErrorCode: DWORD): String;
var
  ErrorMsg: AnsiString;
  Len: DWORD;
  WinINetHandle: HMODULE;
begin
  ErrorMsg := '';
  Result  := 'Erro: '+IntToStr(ErrorCode);

  WinINetHandle := GetModuleHandle('wininet.dll');
  SetLength(ErrorMsg, 1024);
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
    end;
  end;

  if (ErrorMsg <> '') then
    Result := Result + ' - '+ ErrorMsg;
end;

procedure TACBrWinReqResp.UpdateErrorCodes(ARequest: Pointer);
begin
  FpInternalErrorCode := GetLastError;
end;

procedure TACBrWinReqResp.Execute(const DataMsg: String; Resp: TStream);
begin
  Data := DataMsg;
  Execute(Resp);
end;

procedure TACBrWinReqResp.Execute(Resp: TStream);
begin
  raise EACBrWinReqResp.Create('Metodo "Execute" não implementado em '+ClassName);
end;

{$Else}
implementation

{$EndIf}

end.
