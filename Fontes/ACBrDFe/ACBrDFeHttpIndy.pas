{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: André Ferreira de Moraes                        }
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

unit ACBrDFeHttpIndy;

interface

uses
  Classes, SysUtils,
  {$IF CompilerVersion >= 33}System.Net.HttpClient,{$IFEND}
  ACBrDFeSSL,
  SoapHTTPClient, SOAPHTTPTrans;

const
  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;

type
  { TDFeDelphiSoap }

  { TDFeHttpIndy }

  TDFeHttpIndy = class(TDFeSSLHttpClass)
  private
    FIndyReqResp: THTTPReqResp;
    FMimeType: String;
  {$IF CompilerVersion >= 33}
    procedure OnBeforePost(const HTTPReqResp: THTTPReqResp; Client: THTTPClient);
  {$ELSE}
    procedure OnBeforePost(const HTTPReqResp: THTTPReqResp; ASession: Pointer);
  {$IFEND}
  protected
    procedure ConfigConnection; override;

  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    procedure Execute; override;
    procedure Abortar; override;
  end;

implementation

uses
  strutils, WinInet, SOAPConst,
  ACBr_WinCrypt, ACBrDFeException, ACBRConsts,
  synautil;

{ TDFeDelphiSoap }

constructor TDFeHttpIndy.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create(ADFeSSL);

  FIndyReqResp := THTTPReqResp.Create(nil);
end;

destructor TDFeHttpIndy.Destroy;
begin
  FIndyReqResp.Free;

  inherited Destroy;
end;

procedure TDFeHttpIndy.Execute;
begin
  inherited;

  // Enviando, dispara exceptions no caso de erro //
  FIndyReqResp.Execute(DataReq, DataResp);
  FpInternalErrorCode := GetLastError;
  FpHTTPResultCode := 0;

  // DEBUG //
  //DataResp.SaveToFile('c:\temp\ReqResp.xml');
end;

procedure TDFeHttpIndy.Abortar;
begin
  FreeAndNil( FIndyReqResp );
  FIndyReqResp := THTTPReqResp.Create(nil);
end;

procedure TDFeHttpIndy.ConfigConnection;
begin
  inherited;

  // Proxy //
  if (FpDFeSSL.ProxyHost <> '') then
  begin
    FIndyReqResp.Proxy := FpDFeSSL.ProxyHost + ':' + FpDFeSSL.ProxyPort;
    FIndyReqResp.UserName := FpDFeSSL.ProxyUser;
    FIndyReqResp.Password := FpDFeSSL.ProxyPass;
  end;

  // Header //
  FIndyReqResp.URL := URL;
  //FIndyReqResp.Method := Method;
  FIndyReqResp.UseUTF8InHeader := True;
  FMimeType := MimeType;
  FIndyReqResp.SoapAction := SoapAction;
  //Headers.Insert(0, UpperCase(Method) + ' ' + URL + ' HTTP/1.0');
  //if Headers.Count > 0 then
  //  FIndyReqResp.Headers.AddStrings(Headers);

  // SSL e Certificado //

  // TimeOut //
  FIndyReqResp.ConnectTimeout := FpDFeSSL.TimeOut;
  FIndyReqResp.ReceiveTimeout := FpDFeSSL.TimeOut;
  {$IF CompilerVersion < 33}
    //NOTA: Não existe a propriedade SendTimeout em Soap.SOAPHTTPTrans (Delphi 10.3.1)
    //No Delphi 10.3 SendTimeout = ReceiveTimeout
    FIndyReqResp.SendTimeout := FpDFeSSL.TimeOut;
  {$IFEND}

  FIndyReqResp.OnBeforePost := OnBeforePost;
end;

{$IF CompilerVersion >= 33}
//Client: THTTPClient requer a unit System.Net.HttpClient
procedure TDFeHttpIndy.OnBeforePost(const HTTPReqResp: THTTPReqResp;
  Client: THTTPClient);
var
  ContentHeader: String;
begin
  with FpDFeSSL do
  begin
    if (UseCertificateHTTP) then
    begin
      if not InternetSetOption(Client, INTERNET_OPTION_CLIENT_CERT_CONTEXT,
        FpDFeSSL.CertContextWinApi, SizeOf(CERT_CONTEXT)) then
        raise EACBrDFeException.Create('Erro ao ajustar INTERNET_OPTION_CLIENT_CERT_CONTEXT: ' +
                                       IntToStr(GetLastError));
    end;

    if (trim(ProxyUser) <> '') then
    begin
      if not InternetSetOption(Client, INTERNET_OPTION_PROXY_USERNAME,
        PChar(ProxyUser), Length(ProxyUser)) then
        raise EACBrDFeException.Create('Erro ao ajustar INTERNET_OPTION_PROXY_USERNAME: ' +
                                       IntToStr(GetLastError));

      if (trim(ProxyPass) <> '') then
        if not InternetSetOption(Client, INTERNET_OPTION_PROXY_PASSWORD,
          PChar(ProxyPass), Length(ProxyPass)) then
          raise EACBrDFeException.Create('Erro ao ajustar INTERNET_OPTION_PROXY_PASSWORD: ' +
                                         IntToStr(GetLastError));
    end;

    if (FMimeType <> '') then
    begin
      ContentHeader := Format(ContentTypeTemplate, [FMimeType]);
      HttpAddRequestHeaders(Client, PChar(ContentHeader), Length(ContentHeader),
                              HTTP_ADDREQ_FLAG_REPLACE);

    end;
  end;
  //Não existe este método CheckContentType em Soap.SOAPHTTPTrans (D10.3.1)
  //FIndyReqResp.CheckContentType;
end;

{$ELSE}

procedure TDFeHttpIndy.OnBeforePost(const HTTPReqResp: THTTPReqResp;
  ASession: Pointer);
var
  ContentHeader: String;
begin
  with FpDFeSSL do
  begin
    if (UseCertificateHTTP) then
    begin
      if not InternetSetOption(ASession, INTERNET_OPTION_CLIENT_CERT_CONTEXT,
        FpDFeSSL.CertContextWinApi, SizeOf(CERT_CONTEXT)) then
        raise EACBrDFeException.Create('Erro ao ajustar INTERNET_OPTION_CLIENT_CERT_CONTEXT: ' +
                                       IntToStr(GetLastError));
    end;

    if trim(ProxyUser) <> '' then
    begin
      if not InternetSetOption(ASession, INTERNET_OPTION_PROXY_USERNAME,
        PChar(ProxyUser), Length(ProxyUser)) then
        raise EACBrDFeException.Create('Erro ao ajustar INTERNET_OPTION_PROXY_USERNAME: ' +
                                       IntToStr(GetLastError));

      if trim(ProxyPass) <> '' then
        if not InternetSetOption(ASession, INTERNET_OPTION_PROXY_PASSWORD,
          PChar(ProxyPass), Length(ProxyPass)) then
          raise EACBrDFeException.Create('Erro ao ajustar INTERNET_OPTION_PROXY_PASSWORD: ' +
                                         IntToStr(GetLastError));
    end;

    if (FMimeType <> '') then
    begin
      ContentHeader := Format(ContentTypeTemplate, [FMimeType]);
      HttpAddRequestHeaders(ASession, PChar(ContentHeader), Length(ContentHeader),
                            HTTP_ADDREQ_FLAG_REPLACE);
    end;
  end;

  FIndyReqResp.CheckContentType;
end;
{$IFEND}

end.


