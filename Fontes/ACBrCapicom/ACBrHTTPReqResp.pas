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

unit ACBrHTTPReqResp;

interface

uses
  Windows, Classes, SysUtils, wininet, ACBrCAPICOM_TLB;

type

  { EACBrHTTPReqResp }

  EACBrHTTPReqResp = class(Exception)
  public
    constructor Create(const Msg: String);
  end;

  { TACBrHTTPReqResp }

  TACBrHTTPReqResp = class
  private
    FCertificate: ICertificate2;
    FCertSerialNumber: String;
    FCertStoreName: String;
    FSOAPAction: String;
    FMimeType: String;
    // (ex.: 'application/soap+xml' ou 'text/xml' - que é o Content-Type)
    FCharsets: String; //  (ex.: 'ISO-8859-1,utf-8' - que é o Accept-Charset)
    FData: String;
    FProxyHost: String;
    FProxyPass: String;
    FProxyPort: String;
    FProxyUser: String;
    FUrl: String;
    FUseCertificate: Boolean;
    FShowCertStore: Boolean;
    FUseSSL: Boolean;

    function GetWinInetError(ErrorCode: cardinal): String;
    function OpenCertStore: String;

  protected

  public
    property SOAPAction: String read FSOAPAction write FSOAPAction;
    property MimeType: String read FMimeType write FMimeType;
    property Charsets: String read FCharsets write FCharsets;
    property Url: String read FUrl write FUrl;
    property Data: String read FData write FData;
    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: String read FProxyPort write FProxyPort;
    property ProxyUser: String read FProxyUser write FProxyUser;
    property ProxyPass: String read FProxyPass write FProxyPass;
    property CertStoreName: String read FCertStoreName write FCertStoreName;
    property UseCertificate: Boolean read FUseCertificate write FUseCertificate;
    property UseSSL: Boolean read FUseSSL write FUseSSL;
    property ShowCertStore: Boolean read FShowCertStore write FShowCertStore;

    procedure SetCertificate(pCertSerialNumber: String); overload;
    procedure SetCertificate(pCertificate: ICertificate2); overload;
    procedure Execute(Resp: TStream); overload;
    procedure Execute(const DataMsg: String; Resp: TStream); overload;
    constructor Create;
  end;

  {$EXTERNALSYM CERT_CONTEXT}
  _CERT_CONTEXT = record
    dwCertEncodingType: longword;
    pbCertEncoded: ^byte;
    cbCertEncoded: longword;
    pCertInfo: Pointer;
    hCertStore: Pointer;
  end;

  {$EXTERNALSYM _CERT_CONTEXT}
  CERT_CONTEXT = _CERT_CONTEXT;

implementation

uses StrUtils, ACBrUtil, synautil;

{ EACBrHTTPReqResp }

constructor EACBrHTTPReqResp.Create(const Msg: String);
begin
  inherited Create(ACBrStr(Msg));
end;

{ TACBrHTTPReqResp }

function TACBrHTTPReqResp.GetWinInetError(ErrorCode: cardinal): String;
const
  winetdll = 'wininet.dll';
var
  Len: integer;
  Buffer: PChar;
begin
  Len := FormatMessage(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or
    FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_IGNORE_INSERTS or
    FORMAT_MESSAGE_ARGUMENT_ARRAY, Pointer(GetModuleHandle(winetdll)),
    ErrorCode, 0, @Buffer, SizeOf(Buffer), nil);
  try
    while (Len > 0) and
     {$IFDEF DELPHI12_UP}
      (CharInSet(Buffer[Len - 1], [#0..#32, '.']))
     {$ELSE}
      (Buffer[Len - 1] in [#0..#32, '.'])
     {$ENDIF}
      do
    begin
      Dec(Len);
    end;

    SetString(Result, Buffer, Len);
  finally
    LocalFree(HLOCAL(Buffer));
  end;
end;

function TACBrHTTPReqResp.OpenCertStore: String;
var
  Store: IStore3;
  Certs: ICertificates2;
  Certs2: ICertificates2;
  Cert: ICertificate2;
  FNumeroSerie: WideString;
begin
  Store := CoStore.Create;
  try
    Store.Open(CAPICOM_CURRENT_USER_STORE, FCertStoreName, CAPICOM_STORE_OPEN_READ_ONLY);

    Certs := Store.Certificates as ICertificates2;

    Certs2 := Certs.Select(ACBrStr('Certificado(s) Digital(is) disponível(is)'),
      'Selecione o Certificado Digital para uso no aplicativo', False);

    if not (Certs2.Count = 0) then
    begin
      Cert := IInterface(Certs2.Item[1]) as ICertificate2;
      FNumeroSerie := Cert.SerialNumber;
    end;
  finally
    FreeAndNil(Store);
  end;

  Result := FNumeroSerie;
end;

procedure TACBrHTTPReqResp.SetCertificate(pCertSerialNumber: String);
begin
  if FCertSerialNumber = pCertSerialNumber then
    Exit;
  FCertSerialNumber := pCertSerialNumber;
  FCertificate := nil;
end;

procedure TACBrHTTPReqResp.SetCertificate(pCertificate: ICertificate2);
begin
  if FCertificate = pCertificate then
    Exit;
  FCertificate := pCertificate;
  FCertSerialNumber := '';
end;

procedure TACBrHTTPReqResp.Execute(const DataMsg: String; Resp: TStream);
begin
  Data := DataMsg;
  Execute(Resp);
end;

procedure TACBrHTTPReqResp.Execute(Resp: TStream);
var
  aBuffer: array[0..4096] of AnsiChar;
  BytesRead: cardinal;
  pSession: HINTERNET;
  pConnection: HINTERNET;
  pRequest: HINTERNET;
  flags: longword;

  Store: IStore;
  Certs: ICertificates;
  Cert: ICertificate2;
  Cert2: ICertificate2;
  CertContext: ICertContext;
  PCertContext: Pointer;

  Ok: Boolean;
  port, i, AccessType, ErrorCode, PosError: integer;
  ANone, AHost, APage, pProxy, ErrorMsg, Header: String;
begin

  if (FUseCertificate) then
    FUseSSL := True;

  ParseURL(FUrl, ANone, ANone, ANone, AHost, ANone, APage, ANone);

  if ((ShowCertStore) or ((FCertSerialNumber = '') and (FCertificate = nil))) then
  begin
    FCertSerialNumber := OpenCertStore;
    if FCertSerialNumber <> '' then
      FCertificate := nil;
  end;

  if FCertSerialNumber <> '' then
  begin
    Store := CoStore.Create;
    Store.Open(CAPICOM_CURRENT_USER_STORE, FCertStoreName, CAPICOM_STORE_OPEN_READ_ONLY);

    Certs := Store.Certificates as ICertificates2;

    if Certs.Count > 0 then
    begin
      for i := 1 to Certs.Count do
      begin
        Cert2 := IInterface(Certs.Item[i]) as ICertificate2;
        if Cert2.SerialNumber = FCertSerialNumber then
        begin
          Cert := Cert2;
          break;
        end;
      end;

      CertContext := Cert as ICertContext;
      CertContext.Get_CertContext(integer(PCertContext));
    end;
  end
  else
  begin
    CertContext := FCertificate as ICertContext;
    CertContext.Get_CertContext(integer(PCertContext));
  end;

  if FProxyHost <> '' then
  begin
    AccessType := INTERNET_OPEN_TYPE_PROXY;
    if (FProxyPort <> '') and (FProxyPort <> '0') then
      pProxy := FProxyHost + ':' + FProxyPort
    else
      pProxy := FProxyHost;
  end
  else
    AccessType := INTERNET_OPEN_TYPE_PRECONFIG;

  pSession := InternetOpen(PChar('Borland SOAP 1.2'), AccessType, PChar(pProxy), nil, 0);

  if not Assigned(pSession) then
    raise EACBrHTTPReqResp.Create('Erro: Internet Open or Proxy');

  try
    if (FUseSSL) then
      Port := INTERNET_DEFAULT_HTTPS_PORT
    else
      Port := INTERNET_DEFAULT_HTTP_PORT;

    pConnection := InternetConnect(pSession, PChar(AHost), Port,
      PChar(FProxyUser), PChar(FProxyPass), INTERNET_SERVICE_HTTP, 0, cardinal(Self));

    if not Assigned(pConnection) then
      raise EACBrHTTPReqResp.Create('Erro: Internet Connect or Host');

    try
      if (FUseSSL) then
      begin
        flags := INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_CACHE_WRITE;
        flags := flags or INTERNET_FLAG_SECURE;

        if (FUseCertificate) then
          flags := flags or (INTERNET_FLAG_IGNORE_CERT_CN_INVALID or
            INTERNET_FLAG_IGNORE_CERT_DATE_INVALID);
      end
      else
        flags := INTERNET_SERVICE_HTTP;

      pRequest := HttpOpenRequest(pConnection, PChar('POST'),
        PChar(APage), nil, nil, nil, flags, 0);

      if not Assigned(pRequest) then
        raise EACBrHTTPReqResp.Create('Erro: Open Request');

      try
        Header := 'Host: ' + AHost + sLineBreak + 'Content-Type: ' +
          FMimeType + SLineBreak + 'Accept-Charset: ' + FCharsets +
          SLineBreak + 'SOAPAction: "' + FSOAPAction + '"' +
          SLineBreak + SLineBreak;

        if (FUseCertificate) then
          if not InternetSetOption(pRequest, 84 {INTERNET_OPTION_CLIENT_CERT_CONTEXT},
            PCertContext, SizeOf(CERT_CONTEXT)) then
            raise EACBrHTTPReqResp.Create('Erro: Problema ao inserir o certificado');

        if trim(FProxyUser) <> '' then
          if not InternetSetOption(pRequest, INTERNET_OPTION_PROXY_USERNAME,
            PChar(FProxyUser), Length(FProxyUser)) then
            raise EACBrHTTPReqResp.Create('Erro: Proxy User');

        if trim(FProxyPass) <> '' then
          if not InternetSetOption(pRequest, INTERNET_OPTION_PROXY_PASSWORD,
            PChar(FProxyPass), Length(FProxyPass)) then
            raise EACBrHTTPReqResp.Create('Erro: Proxy Password');

        HttpAddRequestHeaders(pRequest, PChar(Header), Length(Header),
          HTTP_ADDREQ_FLAG_ADD);

        Ok := False;
        Resp.Size := 0;
        if HttpSendRequest(pRequest, nil, 0, Pointer(UTF8Encode(FData)),
          Length(UTF8Encode(FData))) then
        begin
          while InternetReadFile(pRequest, @aBuffer, SizeOf(aBuffer), BytesRead) do
          begin
            if (BytesRead = 0) then
              Break;

            Resp.Write(aBuffer, BytesRead);
          end;

          if Resp.Size > 0 then
          begin
            aBuffer[0] := #0;
            Resp.Write(aBuffer, 1);
            Resp.Position := 0;

            Ok := True;

            //TODO: Tratar a resposta abaixo, ler o conteudo do documento... Precisa ????

            //          if Pos('<TITLE',UpperCase(Result)) > 0 then
            //          begin
            //            PosError := Pos('<TITLE>',UpperCase(Result))+7;
            //            ErrorMsg := trim(copy(Result, PosError, (pos('</TITLE>', UpperCase(Result)) - PosError)));
            //            raise EACBrHTTPReqResp.Create('Erro: Requisição não enviada.'+sLineBreak+ErrorMsg);
            //          end;
          end;
        end;

        if not OK then
        begin
          ErrorCode := GetLastError;
          raise EACBrHTTPReqResp.Create('Erro: Requisição não enviada.' +
            sLineBreak + IntToStr(ErrorCode) + ' - ' + GetWinInetError(ErrorCode));
        end;
      finally
        InternetCloseHandle(pRequest);
      end;
    finally
      InternetCloseHandle(pConnection);
    end;
  finally
    InternetCloseHandle(pSession);
  end;
end;

constructor TACBrHTTPReqResp.Create;
begin
  FMimeType := 'application/soap+xml';
  FCharsets := 'utf-8';
  FCertStoreName := 'My';
  FCertSerialNumber := '';
  FCertificate := nil;
  FUseCertificate := True;
  FUseSSL := True;
  FShowCertStore := False;
end;

end.
