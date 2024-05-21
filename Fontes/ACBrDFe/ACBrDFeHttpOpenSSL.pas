{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  André Ferreira de Moraes                       }
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
{.$Define SYNADEBUG}

unit ACBrDFeHttpOpenSSL;

interface

uses
  Classes, SysUtils,
  HTTPSend, ssl_openssl, ssl_openssl_lib, blcksock,
  ACBrDFeSSL,
  {$IfDef SYNADEBUG}synadbg,{$EndIf}
  OpenSSLExt;

type

  { TDFeHttpOpenSSL }

  TDFeHttpOpenSSL = class(TDFeSSLHttpClass)
  private
    FHTTP: THTTPSend;
    FLastErrorMsg: String;

    {$IfDef SYNADEBUG}
     FSynaDebug: TSynaDebug;
    {$EndIf}
    procedure CheckSSLType(AValue: TSSLType);
    procedure DoException(AMessage: String);

  protected
    procedure ConfigConnection; override;
    function GetLastErrorDesc: String; override;

  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    procedure Execute; override;
    procedure Abortar; override;

  end;

implementation

uses
  typinfo, synautil,
  ACBrDFeException,
  ACBrUtil.Strings,
  ACBrConsts;

{ TDFeHttpOpenSSL }

constructor TDFeHttpOpenSSL.Create(ADFeSSL: TDFeSSL);
begin
  inherited;
  FHTTP := THTTPSend.Create;
  FLastErrorMsg := '';

  {$IfDef SYNADEBUG}
  FSynaDebug := TsynaDebug.Create;
  FHTTP.Sock.OnStatus := FSynaDebug.HookStatus;
  FHTTP.Sock.OnMonitor := FSynaDebug.HookMonitor;
  {$EndIf}
end;

destructor TDFeHttpOpenSSL.Destroy;
begin
  FHTTP.Free;
  {$IfDef SYNADEBUG}
  FSynaDebug.Free;
  {$EndIf}
  inherited Destroy;
end;

procedure TDFeHttpOpenSSL.Abortar;
begin
  FHTTP.Sock.CloseSocket;
end;

procedure TDFeHttpOpenSSL.Execute;
var
  OK: Boolean;
begin
  inherited;

  // DEBUG //
  //FHTTP.Document.SaveToFile( 'c:\temp\HttpSendDocument.xml' );
  //FHTTP.Headers.SaveToFile( 'c:\temp\HttpSendHeader.xml' );

  // Transmitindo //
  try
    OK := FHTTP.HTTPMethod(Method, URL);
    if not OK then
      DoException( Format( ACBrStr(cACBrDFeSSLEnviarException),
                           [FpInternalErrorCode, FpHTTPResultCode, URL] ) +
                           sLineBreak + LastErrorDesc);

  finally
    FpHTTPResultCode := FHTTP.ResultCode;
    FpInternalErrorCode := FHTTP.Sock.LastError;
  end;

  // Lendo a resposta //
  // DEBUG //
  //HTTP.Document.SaveToFile('c:\temp\ReqResp.xml');
  FHTTP.Document.Position := 0;
  DataResp.LoadFromStream(FHTTP.Document);
  HeaderResp.Text := FHTTP.Headers.Text;
end;

procedure TDFeHttpOpenSSL.ConfigConnection;
begin
  inherited;

  FHTTP.Clear;
  FLastErrorMsg := '';

  // Proxy //
  FHTTP.ProxyHost := FpDFeSSL.ProxyHost;
  FHTTP.ProxyPort := FpDFeSSL.ProxyPort;
  FHTTP.ProxyUser := FpDFeSSL.ProxyUser;
  FHTTP.ProxyPass := FpDFeSSL.ProxyPass;

  // Header //
  FHTTP.MimeType  := MimeType;
  FHTTP.UserAgent := 'Synapse OpenSSL ACBr/1.0';
  FHTTP.Protocol  := '1.1';
  if (SoapAction <> '') then
    FHTTP.Headers.Add('SOAPAction: "' + SoapAction + '"');

  if HeaderReq.Count > 0 then
    FHTTP.Headers.AddStrings(HeaderReq);

  // SSL e Certificado //
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

  CheckSSLType(FpDFeSSL.SSLType);
  FHTTP.Sock.SSL.SSLType := FpDFeSSL.SSLType;

  // TimeOut //
  FHTTP.Timeout := FpDFeSSL.TimeOut;
  with FHTTP.Sock do
  begin
    SetTimeout(FpDFeSSL.TimeOut);
    ConnectionTimeout := FpDFeSSL.TimeOut;
    InterPacketTimeout := False;
    NonblockSendTimeout := FpDFeSSL.TimeOut;
    SocksTimeout := FpDFeSSL.TimeOut;
    HTTPTunnelTimeout := FpDFeSSL.TimeOut;
  end;

  // Document //
  if (DataReq.Size > 0) then
  begin
    DataReq.Position := 0;
    FHTTP.Document.LoadFromStream(DataReq);
  end;

  //FHTTP.Sock.SSL.VerifyCert := False;
  FHTTP.AddPortNumberToHost := False;
end;

function TDFeHttpOpenSSL.GetLastErrorDesc: String;
begin
  Result := FHTTP.Sock.LastErrorDesc;
  if Result = '' then
    Result := FLastErrorMsg;
end;

procedure TDFeHttpOpenSSL.CheckSSLType(AValue: TSSLType);
var
  SSLMethod: ssl_openssl_lib.PSSL_METHOD;
  s: String;
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
  else
    begin
      SSLMethod := ssl_openssl_lib.SslMethodTLS;
      if not Assigned(SSLMethod) then
        if not LibVersionIsGreaterThan1_0_0 then
          SSLMethod := ssl_openssl_lib.SslMethodV23;
    end;
  end;

  if not Assigned(SSLMethod) then
  begin
    s := String(ssl_openssl_lib.OpenSSLVersion( 0 ));
    DoException( Format( ACBrStr('%s, não suporta %s'),
                         [s, GetEnumName(TypeInfo(TSSLType), integer(AValue) )]));
  end;
end;

procedure TDFeHttpOpenSSL.DoException(AMessage: String);
begin
  FLastErrorMsg := AMessage;
  raise EACBrDFeException.Create(AMessage);
end;

end.



