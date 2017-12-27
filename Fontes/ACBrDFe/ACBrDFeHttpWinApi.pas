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

unit ACBrDFeHttpWinApi;

interface

uses
  Classes, SysUtils,
  ACBrDFeSSL, ACBrWinReqRespClass, ACBrWinHTTPReqResp, ACBrWinINetReqResp;

type

  { TDFeHttpWinHttp }

  TDFeHttpWinHttp = class(TDFeSSLHttpClass)
  private
    FWinHTTPReqResp: TACBrWinReqResp;

  protected
    function GetHTTPResultCode: Integer; override;
    function GetInternalErrorCode: Integer; override;
    procedure ConfigurarHTTP(const AURL, ASoapAction: String; AMimeType: String); override;

  public
    constructor Create(ADFeSSL: TDFeSSL; WinApi: TSSLHttpLib); reintroduce;
    destructor Destroy; override;

    function Enviar(const ConteudoXML: String; const AURL: String;
      const ASoapAction: String; AMimeType: String = ''): String; override;
  end;


implementation

uses
  typinfo,
  ACBrDFeException, ACBrConsts,
  synautil;

{ TDFeHttpWinHttp }

constructor TDFeHttpWinHttp.Create(ADFeSSL: TDFeSSL; WinApi: TSSLHttpLib);
begin
  inherited Create(ADFeSSL);

  if WinApi = httpWinINet then
    FWinHTTPReqResp := TACBrWinINetReqResp.Create
  else
    FWinHTTPReqResp := TACBrWinHTTPReqResp.Create;
end;

destructor TDFeHttpWinHttp.Destroy;
begin
  FWinHTTPReqResp.Free;
  inherited Destroy;
end;

function TDFeHttpWinHttp.Enviar(const ConteudoXML: String; const AURL: String;
  const ASoapAction: String; AMimeType: String): String;
var
  Resp: TMemoryStream;
begin
  Result := '';

  ConfigurarHTTP(AURL, ASoapAction, AMimeType);

  Resp := TMemoryStream.Create;
  try
    try
      // Enviando, dispara exceptions no caso de erro //
      FWinHTTPReqResp.Execute(ConteudoXML, Resp);
      // DEBUG //
      //Resp.SaveToFile('c:\temp\ReqResp.xml');

      Resp.Position := 0;
      Result := String( ReadStrFromStream(Resp, Resp.Size) );

      // Verifica se o ResultCode é: 200 OK; 201 Created; 202 Accepted
      // https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
      if not (FWinHTTPReqResp.HTTPResultCode in [200, 201, 202]) then
        raise EACBrDFeException.Create('');

    except
      On E: Exception do
      begin
        raise EACBrDFeException.Create( Format( cACBrDFeSSLEnviarException,
                                        [InternalErrorCode, HTTPResultCode] ) + sLineBreak +
                                        E.Message ) ;
      end;
    end;
  finally
    Resp.Free;
  end;
end;

procedure TDFeHttpWinHttp.ConfigurarHTTP(const AURL, ASoapAction: String;
  AMimeType: String);
begin
  with FpDFeSSL do
  begin
    if UseCertificateHTTP then
      FWinHTTPReqResp.CertContext := FpDFeSSL.CertContextWinApi
    else
      FWinHTTPReqResp.CertContext := Nil;

    FWinHTTPReqResp.SSLType   := SSLType;
    FWinHTTPReqResp.ProxyHost := ProxyHost;
    FWinHTTPReqResp.ProxyPort := ProxyPort;
    FWinHTTPReqResp.ProxyUser := ProxyUser;
    FWinHTTPReqResp.ProxyPass := ProxyPass;
    FWinHTTPReqResp.TimeOut   := TimeOut;
  end;

  FWinHTTPReqResp.Url        := AURL;
  FWinHTTPReqResp.SOAPAction := ASoapAction;
  FWinHTTPReqResp.MimeType   := AMimeType;
end;

function TDFeHttpWinHttp.GetHTTPResultCode: Integer;
begin
  Result := FWinHTTPReqResp.HTTPResultCode;
end;

function TDFeHttpWinHttp.GetInternalErrorCode: Integer;
begin
  Result := FWinHTTPReqResp.InternalErrorCode;
end;

end.

