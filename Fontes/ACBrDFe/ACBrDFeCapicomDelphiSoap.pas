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

unit ACBrDFeCapicomDelphiSoap;

interface

uses
  Classes, SysUtils, ACBrDFeCapicom, ACBrDFeConfiguracoes,
  SoapHTTPClient, SOAPHTTPTrans;

const
  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;      

type
  { TDFeCapicomDelphiSoap }

  TDFeCapicomDelphiSoap = class(TDFeCapicom)
  private
    FIndyReqResp: THTTPReqResp;
    FURL: String;

    procedure OnBeforePost(const HTTPReqResp: THTTPReqResp; Data: Pointer);
  protected
    procedure ConfiguraReqResp(const URL, SoapAction: String); override;
    procedure Executar(const ConteudoXML: String; Resp: TMemoryStream); override;

  public
    constructor Create(AConfiguracoes: TConfiguracoes);
    destructor Destroy; override;
  end;

implementation

uses
  strutils, WinInet, SOAPConst,
  ACBrCAPICOM_TLB, JwaWinCrypt,
  ACBrUtil, ACBrDFeUtil, ACBrDFe;

{ TDFeCapicomDelphiSoap }

constructor TDFeCapicomDelphiSoap.Create(AConfiguracoes: TConfiguracoes);
begin
  inherited Create(AConfiguracoes);

  FIndyReqResp := THTTPReqResp.Create(nil);
end;

destructor TDFeCapicomDelphiSoap.Destroy;
begin
  FIndyReqResp.Free;

  inherited Destroy;
end;

procedure TDFeCapicomDelphiSoap.OnBeforePost(const HTTPReqResp: THTTPReqResp;
  Data: Pointer);
var
  CertContext: ICertContext;
  PCertContext: Pointer;
  ContentHeader: String;
begin
  CertContext := Certificado as ICertContext;
  CertContext.Get_CertContext(integer(PCertContext));

  if not InternetSetOption(Data, INTERNET_OPTION_CLIENT_CERT_CONTEXT,
    PCertContext, SizeOf(CERT_CONTEXT)) then
    raise EACBrDFeException.Create('OnBeforePost: ' + IntToStr(GetLastError));

  if trim(Configuracoes.WebServices.ProxyUser) <> '' then
    if not InternetSetOption(Data, INTERNET_OPTION_PROXY_USERNAME,
      PChar(Configuracoes.WebServices.ProxyUser),
      Length(Configuracoes.WebServices.ProxyUser)) then
      raise EACBrDFeException.Create('OnBeforePost: ' + IntToStr(GetLastError));

  if trim(Configuracoes.WebServices.ProxyPass) <> '' then
    if not InternetSetOption(Data, INTERNET_OPTION_PROXY_PASSWORD,
      PChar(Configuracoes.WebServices.ProxyPass),
      Length(Configuracoes.WebServices.ProxyPass)) then
      raise EACBrDFeException.Create('OnBeforePost: ' + IntToStr(GetLastError));

  if (pos('SCERECEPCAORFB', UpperCase(FURL)) <= 0) and
    (pos('SCECONSULTARFB', UpperCase(FURL)) <= 0) then
  begin
    ContentHeader := Format(ContentTypeTemplate,
      ['application/soap+xml; charset=utf-8']);
    HttpAddRequestHeaders(Data, PChar(ContentHeader),
      Length(ContentHeader), HTTP_ADDREQ_FLAG_REPLACE);
  end;

  FIndyReqResp.CheckContentType;
end;

procedure TDFeCapicomDelphiSoap.ConfiguraReqResp(const URL, SoapAction: String);
begin
  with Configuracoes.WebServices do
  begin
    if ProxyHost <> '' then
    begin
      FIndyReqResp.Proxy := ProxyHost + ':' + ProxyPort;
      FIndyReqResp.UserName := ProxyUser;
      FIndyReqResp.Password := ProxyPass;
    end;
  end;

  FIndyReqResp.OnBeforePost := OnBeforePost;
  FIndyReqResp.UseUTF8InHeader := True;
  FURL := URL;
end;

procedure TDFeCapicomDelphiSoap.Executar(const ConteudoXML: String;
  Resp: TMemoryStream);
begin
  // Enviando, dispara exceptions no caso de erro //
  FIndyReqResp.Execute(ConteudoXML, Resp);
end;


end.
