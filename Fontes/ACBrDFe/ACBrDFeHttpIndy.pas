{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2015 Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

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

{$I ACBr.inc}

unit ACBrDFeHttpIndy;

interface

uses
  Classes, SysUtils,
  ACBrDFeSSL,
  SoapHTTPClient, SOAPHTTPTrans;

const
  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;      

type
  { TDFeDelphiSoap }

  TDFeHttpIndy = class(TDFeSSLHttpClass)
  private
    FIndyReqResp: THTTPReqResp;
    FMimeType: String;

    procedure OnBeforePost(const HTTPReqResp: THTTPReqResp; Data: Pointer);
  protected
    function GetHTTPResultCode: Integer; override;
    procedure ConfigurarHTTP(const AURL, ASoapAction: String; const AMimeType: String); override;

  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    function Enviar(const ConteudoXML: String; const AURL: String;
      const ASoapAction: String; const AMimeType: String = ''): String; override;
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

function TDFeHttpIndy.Enviar(const ConteudoXML, AURL, ASoapAction: String;
  const AMimeType: String): String;
var
  Resp: TMemoryStream;
begin
  Result := '';

  ConfigurarHTTP(AURL, ASoapAction, AMimeType);

  Resp := TMemoryStream.Create;
  try
    try
      // Enviando, dispara exceptions no caso de erro //
      FIndyReqResp.Execute(ConteudoXML, Resp);
    except
      On E: Exception do
      begin
        raise EACBrDFeException.Create( Format( cACBrDFeSSLEnviarException,
                                        [InternalErrorCode, HTTPResultCode, AURL] ) + sLineBreak +
                                        E.Message ) ;
      end;
    end;

    Resp.Position := 0;
    Result := ReadStrFromStream(Resp, Resp.Size);
    // DEBUG //
    //Resp.SaveToFile('c:\temp\ReqResp.xml');
  finally
    Resp.Free;
  end;
end;

procedure TDFeHttpIndy.Abortar;
begin
  FreeAndNil( FIndyReqResp );
  FIndyReqResp := THTTPReqResp.Create(nil);
end;

procedure TDFeHttpIndy.ConfigurarHTTP(const AURL, ASoapAction: String;
  const AMimeType: String);
begin
  with FpDFeSSL do
  begin
    if ProxyHost <> '' then
    begin
      FIndyReqResp.Proxy := ProxyHost + ':' + ProxyPort;
      FIndyReqResp.UserName := ProxyUser;
      FIndyReqResp.Password := ProxyPass;
    end;

    FIndyReqResp.ConnectTimeout := TimeOut;
    FIndyReqResp.SendTimeout    := TimeOut;
    FIndyReqResp.ReceiveTimeout := TimeOut;
  end;

  FMimeType := AMimeType;

  FIndyReqResp.OnBeforePost := OnBeforePost;
  FIndyReqResp.UseUTF8InHeader := True;
  FIndyReqResp.SoapAction := ASoapAction;
  FIndyReqResp.URL := AURL;
end;

procedure TDFeHttpIndy.OnBeforePost(const HTTPReqResp: THTTPReqResp;
  Data: Pointer);
var
  ContentHeader: String;
begin
  with FpDFeSSL do
  begin
    if (UseCertificateHTTP) then
    begin
      if not InternetSetOption(Data, INTERNET_OPTION_CLIENT_CERT_CONTEXT,
        FpDFeSSL.CertContextWinApi, SizeOf(CERT_CONTEXT)) then
        raise EACBrDFeException.Create('Erro ao ajustar INTERNET_OPTION_CLIENT_CERT_CONTEXT: ' +
                                       IntToStr(GetLastError));
    end;

    if trim(ProxyUser) <> '' then
      if not InternetSetOption(Data, INTERNET_OPTION_PROXY_USERNAME,
        PChar(ProxyUser), Length(ProxyUser)) then
        raise EACBrDFeException.Create('Erro ao ajustar INTERNET_OPTION_PROXY_USERNAME: ' +
                                       IntToStr(GetLastError));

    if trim(ProxyPass) <> '' then
      if not InternetSetOption(Data, INTERNET_OPTION_PROXY_PASSWORD,
        PChar(ProxyPass), Length(ProxyPass)) then
        raise EACBrDFeException.Create('Erro ao ajustar INTERNET_OPTION_PROXY_PASSWORD: ' +
                                       IntToStr(GetLastError));

    ContentHeader := Format(ContentTypeTemplate, [FMimeType]);
    HttpAddRequestHeaders(Data, PChar(ContentHeader), Length(ContentHeader),
                            HTTP_ADDREQ_FLAG_REPLACE);
  end;

  FIndyReqResp.CheckContentType;
end;

function TDFeHttpIndy.GetHTTPResultCode: Integer;
begin
  Result := GetLastError;
end;

end.

