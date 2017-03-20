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

unit ACBrDFeHttpOpenSSL;

interface

uses
  Classes, SysUtils,
  HTTPSend, ssl_openssl, ssl_openssl_lib, blcksock,
  ACBrDFeSSL,
  {$IFDEF USE_libeay32}libeay32{$ELSE} OpenSSLExt{$ENDIF};

type

  { TDFeHttpOpenSSL }

  TDFeHttpOpenSSL = class(TDFeSSLHttpClass)
  private
    FHTTP: THTTPSend;
    procedure VerificarSSLType(AValue: TSSLType);

  protected
    function GetHTTPResultCode: Integer; override;
    function GetInternalErrorCode: Integer; override;
    procedure ConfigurarHTTP(const AURL, ASoapAction: String; AMimeType: String); override;

  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    function Enviar(const ConteudoXML: String; const AURL: String;
      const ASoapAction: String; AMimeType: String = ''): String; override;
  end;

implementation

uses
  typinfo,
  ACBrDFeException, ACBrUtil, ACBrConsts,
  synautil;

{ TDFeHttpOpenSSL }

constructor TDFeHttpOpenSSL.Create(ADFeSSL: TDFeSSL);
begin
  inherited;
  FHTTP := THTTPSend.Create;
end;

destructor TDFeHttpOpenSSL.Destroy;
begin
  FHTTP.Free;
  inherited Destroy;
end;

function TDFeHttpOpenSSL.Enviar(const ConteudoXML: String; const AURL: String;
  const ASoapAction: String; AMimeType: String): String;
var
  OK: Boolean;
  RetornoWS: AnsiString;
begin
  RetornoWS := '';

  // Configurando o THTTPSend //
  ConfigurarHTTP(AURL, ASoapAction, AMimeType);

  // Gravando no Buffer de Envio //
  WriteStrToStream(FHTTP.Document, AnsiString(ConteudoXML)) ;

  // DEBUG //
  //FHTTP.Document.SaveToFile( 'c:\temp\HttpSendDocument.xml' );
  //FHTTP.Headers.SaveToFile( 'c:\temp\HttpSendHeader.xml' );

  // Transmitindo //
  OK := FHTTP.HTTPMethod('POST', AURL);

  // Provedor Agili (RESTFul) retorna 202 em vez de 200 //
  OK := OK and (FHTTP.ResultCode in [200, 202]);
  if not OK then
    raise EACBrDFeException.CreateFmt( cACBrDFeSSLEnviarException,
                                       [InternalErrorCode, HTTPResultCode] );

  // Lendo a resposta //
  FHTTP.Document.Position := 0;
  RetornoWS := ReadStrFromStream(FHTTP.Document, FHTTP.Document.Size);

  // DEBUG //
  //HTTP.Document.SaveToFile('c:\temp\ReqResp.xml');

  Result := String( RetornoWS );
end;

procedure TDFeHttpOpenSSL.ConfigurarHTTP(const AURL, ASoapAction: String;
  AMimeType: String);
begin
  FHTTP.Clear;

  if not FpDFeSSL.UseCertificateHTTP then
  begin
    FHTTP.Sock.SSL.PFX := '';
    FHTTP.Sock.SSL.KeyPassword := '';
  end
  else
  begin
    FHTTP.Sock.SSL.PFX := FpDFeSSL.SSLCryptClass.CertPFXData;
    FHTTP.Sock.SSL.KeyPassword := FpDFeSSL.Senha;
  end;

  VerificarSSLType(FpDFeSSL.SSLType);
  FHTTP.Sock.SSL.SSLType := FpDFeSSL.SSLType;

  FHTTP.Timeout := FpDFeSSL.TimeOut;
  FHTTP.Sock.ConnectionTimeout := FpDFeSSL.TimeOut;

  FHTTP.ProxyHost := FpDFeSSL.ProxyHost;
  FHTTP.ProxyPort := FpDFeSSL.ProxyPort;
  FHTTP.ProxyUser := FpDFeSSL.ProxyUser;
  FHTTP.ProxyPass := FpDFeSSL.ProxyPass;

  if AMimeType <> '' then
    AMimeType := AMimeType+'; ';

  AMimeType := AMimeType + 'charset=utf-8'; // Todos DFes usam UTF8
  FHTTP.MimeType := AMimeType;

  FHTTP.UserAgent := '';
  FHTTP.Protocol  := '1.1';
  FHTTP.AddPortNumberToHost := False;

  if ASoapAction <> '' then
    FHTTP.Headers.Add('SOAPAction: "' + ASoapAction + '"');
end;

procedure TDFeHttpOpenSSL.VerificarSSLType(AValue: TSSLType);
var
  SSLMethod: ssl_openssl_lib.PSSL_METHOD;
  OpenSSLVersion: String;
begin
  SSLMethod := Nil;

  case AValue of
    LT_SSLv2:
      SSLMethod := ssl_openssl_lib.SslMethodV2;
    LT_SSLv3:
      SSLMethod := ssl_openssl_lib.SslMethodV3;
    LT_TLSv1:
      SSLMethod := ssl_openssl_lib.SslMethodTLSV1;
    LT_TLSv1_1:
      SSLMethod := ssl_openssl_lib.SslMethodTLSV11;
    LT_TLSv1_2:
      SSLMethod := ssl_openssl_lib.SslMethodTLSV12;
    LT_all:
      SSLMethod := ssl_openssl_lib.SslMethodV23;
  end;

  if SSLMethod = Nil then
  begin
    OpenSSLVersion := String(SSLeay_version( 0 ));

    raise EACBrDFeException.CreateFmt(ACBrStr('%s, não suporta %s'),
          [OpenSSLVersion, GetEnumName(TypeInfo(TSSLType), integer(AValue) )]);
  end;
end;

function TDFeHttpOpenSSL.GetHTTPResultCode: Integer;
begin
  Result := FHTTP.ResultCode;
end;

function TDFeHttpOpenSSL.GetInternalErrorCode: Integer;
begin
  Result := FHTTP.Sock.LastError;
end;

end.



