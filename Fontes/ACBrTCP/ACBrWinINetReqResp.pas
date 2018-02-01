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
    procedure UpdateErrorCodes(ARequest: HINTERNET); override;
  public
    procedure Execute(Resp: TStream); override;
  end;

implementation

uses synautil;

{ TACBrWinINetReqResp }

procedure TACBrWinINetReqResp.UpdateErrorCodes(ARequest: HINTERNET);
Var
  dummy, AStatusCode, ASize: DWORD;
begin
  FpInternalErrorCode := GetLastError;
  FpHTTPResultCode := 0;

  dummy := 0;
  AStatusCode := 0;
  ASize := SizeOf(DWORD);
  if HttpQueryInfo( ARequest,
                    HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER,
                    @AStatusCode, ASize,
                    dummy ) then
    FpHTTPResultCode := AStatusCode;
end;

procedure TACBrWinINetReqResp.Execute(Resp: TStream);
var
  aBuffer: array[0..4096] of AnsiChar;
  BytesRead: cardinal;
  pSession, pConnection, pRequest: HINTERNET;
  flags, flagsLen: longword;
  Ok, UseSSL, UseCertificate: Boolean;
  AccessType: Integer;
  ANone, AHost, AProt, APort, APath, AParam, pProxy, Header: String;
begin

  AProt := '';
  AHost := '';
  APort := '';
  APath := '';
  AParam:= '';
  ANone := '';
  ParseURL(Url, AProt, ANone, ANone, AHost, APort, APath, AParam);

  if (AParam <> '') then
    APath := APath + '?' + AParam;

  UseSSL := (UpperCase(AProt) = 'HTTPS');
  UseCertificate := UseSSL and Assigned( CertContext );

  if ProxyHost <> '' then
  begin
    AccessType := INTERNET_OPEN_TYPE_PROXY;
    if (ProxyPort <> '') and (ProxyPort <> '0') then
      pProxy := ProxyHost + ':' + ProxyPort
    else
      pProxy := ProxyHost;
  end
  else
    AccessType := INTERNET_OPEN_TYPE_PRECONFIG;

  //DEBUG
  //WriteToTXT('c:\temp\httpreqresp.log', FormatDateTime('hh:nn:ss:zzz', Now)+ ' - Abrindo sessão');

  pSession := InternetOpen(PChar('Borland SOAP 1.2'), AccessType, PChar(pProxy), nil, 0);

  try
    if not Assigned(pSession) then
      raise EACBrWinReqResp.Create('Erro: Internet Open or Proxy');

    if TimeOut > 0 then
    begin
      //DEBUG
      //WriteToTXT('c:\temp\httpreqresp.log', FormatDateTime('hh:nn:ss:zzz', Now)+ ' - Ajustando TimeOut: '+IntToStr(FTimeOut));

      if not InternetSetOption(pSession, INTERNET_OPTION_CONNECT_TIMEOUT, @TimeOut, SizeOf(TimeOut)) then
        raise EACBrWinReqResp.Create('Erro ao definir TimeOut de Conexão');

      if not InternetSetOption(pSession, INTERNET_OPTION_SEND_TIMEOUT, @TimeOut, SizeOf(TimeOut)) then
        raise EACBrWinReqResp.Create('Erro ao definir TimeOut de Conexão');

      if not InternetSetOption(pSession, INTERNET_OPTION_RECEIVE_TIMEOUT, @TimeOut, SizeOf(TimeOut)) then
        raise EACBrWinReqResp.Create('Erro ao definir TimeOut de Conexão');
    end;

    if APort = '' then
    begin
      if (UseSSL) then
        APort := IntToStr(INTERNET_DEFAULT_HTTPS_PORT)
      else
        APort := IntToStr(INTERNET_DEFAULT_HTTP_PORT);
    end;

    //Debug, TimeOut Test
    //AHost := 'www.google.com';
    //port := 81;

    //DEBUG
    //WriteToTXT('c:\temp\httpreqresp.log', FormatDateTime('hh:nn:ss:zzz', Now)+ ' - Abrindo Conexão: '+AHost+':'+APort);

    pConnection := InternetConnect(pSession, PChar(AHost), StrToInt(APort),
                                   PChar(ProxyUser), PChar(ProxyPass),
                                   INTERNET_SERVICE_HTTP,
                                   0, 0{cardinal(Self)});
    if not Assigned(pConnection) then
      raise EACBrWinReqResp.Create('Erro: Internet Connect or Host');

    try
      if (UseSSL) then
      begin
        flags := INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_NO_CACHE_WRITE;
        flags := flags or INTERNET_FLAG_SECURE;

        if (UseCertificate) then
          flags := flags or (INTERNET_FLAG_IGNORE_CERT_CN_INVALID or
                             INTERNET_FLAG_IGNORE_CERT_DATE_INVALID);
      end
      else
        flags := INTERNET_SERVICE_HTTP;

      //DEBUG
      //WriteToTXT('c:\temp\httpreqresp.log', FormatDateTime('hh:nn:ss:zzz', Now)+ ' - Fazendo POST: '+APath);

      pRequest := HttpOpenRequest(pConnection, PChar('POST'),
                                  PChar(APath), nil, nil, nil, flags, 0);

      if not Assigned(pRequest) then
        raise EACBrWinReqResp.Create('Erro: Open Request');

      UpdateErrorCodes(pRequest);

      try
        if ( (APort <> IntToStr(INTERNET_DEFAULT_HTTP_PORT)) and (not UseSSL) ) or
           ( (APort <> IntToStr(INTERNET_DEFAULT_HTTPS_PORT)) and (UseSSL) ) then
          AHost := AHost +':'+ APort;

        Header := 'Host: ' + AHost + sLineBreak +
                  'Content-Type: ' + MimeType + '; charset='+Charsets + SLineBreak +
                  'Accept-Charset: ' + Charsets + SLineBreak;

        if SOAPAction <> '' then
          Header := Header +'SOAPAction: "' + SOAPAction + '"' +SLineBreak;

        if (UseCertificate) then
        begin
          if not InternetSetOption(pRequest, INTERNET_OPTION_CLIENT_CERT_CONTEXT,
                                   CertContext, SizeOf(CERT_CONTEXT)) then
            raise EACBrWinReqResp.Create('Erro: Problema ao inserir o certificado')
        end;

        flags := 0;
        flagsLen := SizeOf(flags);
        if not InternetQueryOption(pRequest, INTERNET_OPTION_SECURITY_FLAGS, @flags, flagsLen) then
          raise EACBrWinReqResp.Create('InternetQueryOption erro ao ler wininet flags.' + GetWininetError(GetLastError));

        flags := flags or SECURITY_FLAG_IGNORE_UNKNOWN_CA or
                          SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
                          SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
                          SECURITY_FLAG_IGNORE_REVOCATION;
        if not InternetSetOption(pRequest, INTERNET_OPTION_SECURITY_FLAGS, @flags, flagsLen) then
          raise EACBrWinReqResp.Create('InternetQueryOption erro ao ajustar INTERNET_OPTION_SECURITY_FLAGS' + GetWininetError(GetLastError));

        if trim(ProxyUser) <> '' then
          if not InternetSetOption(pRequest, INTERNET_OPTION_PROXY_USERNAME,
                                   PChar(ProxyUser), Length(ProxyUser)) then
            raise EACBrWinReqResp.Create('Erro: Proxy User');

        if trim(ProxyPass) <> '' then
          if not InternetSetOption(pRequest, INTERNET_OPTION_PROXY_PASSWORD,
                                   PChar(ProxyPass), Length(ProxyPass)) then
            raise EACBrWinReqResp.Create('Erro: Proxy Password');

        HttpAddRequestHeaders(pRequest, PChar(Header), Length(Header), HTTP_ADDREQ_FLAG_ADD);

        if EncodeDataToUTF8 then
          Data := UTF8Encode(Data);

        //DEBUG
        //WriteToTXT('c:\temp\httpreqresp.log', FormatDateTime('hh:nn:ss:zzz', Now)+ ' - Enviando Dados: '+APath);
        //WriteToTXT('c:\temp\httpreqresp.log', FData);

        Ok := False;
        Resp.Size := 0;
        if HttpSendRequest(pRequest, nil, 0, Pointer(Data), Length(Data)) then
        begin
          BytesRead := 0;
          //DEBUG
          //WriteToTXT('c:\temp\httpreqresp.log', FormatDateTime('hh:nn:ss:zzz', Now)+ ' - Lendo Dados');

          while InternetReadFile(pRequest, @aBuffer, SizeOf(aBuffer), BytesRead) do
          begin
            //DEBUG
            //WriteToTXT('c:\temp\httpreqresp.log', FormatDateTime('hh:nn:ss:zzz', Now)+ ' - Bytes Lido: '+IntToStr(BytesRead));

            if (BytesRead = 0) then
              Break;

            Resp.Write(aBuffer, BytesRead);
          end;

          if Resp.Size > 0 then
          begin
            Resp.Position := 0;

            //DEBUG
            //WriteToTXT('c:\temp\httpreqresp.log', FormatDateTime('hh:nn:ss:zzz', Now)+ ' - Total Lido: '+IntToStr(Resp.Size));
            //Resp.Position := 0;
            //FData := ReadStrFromStream(Resp, Resp.Size);
            //Resp.Position := 0;
            //WriteToTXT('c:\temp\httpreqresp.log', FData);

            Ok := True;
          end;
        end;

        UpdateErrorCodes(pRequest);

        if not OK then
        begin
          //DEBUG
          //WriteToTXT('c:\temp\httpreqresp.log', FormatDateTime('hh:nn:ss:zzz', Now)+
          //   ' - Erro WinNetAPI: '+IntToStr(InternalErrorCode)+' HTTP: '+IntToStr(HTTPResultCode));

          raise EACBrWinReqResp.Create('Erro: Requisição não enviada.' + sLineBreak +
                                        GetWinInetError(InternalErrorCode));
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

{$Else}
implementation

{$EndIf}

end.
