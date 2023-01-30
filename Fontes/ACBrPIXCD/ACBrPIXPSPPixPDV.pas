{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Elias César Vieira                              }
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

unit ACBrPIXPSPPixPDV;

interface

uses
  Classes, SysUtils, ACBrPIXCD;

const

  cPixPDVURLSandbox = 'https://pixpdv.com.br/api-h/v1';
  cPixPDVURLProducao = 'https://pixpdv.com.br/api/v1';
  cPixPDVHeaderPoweredBy = 'X-PoweredBy: Projeto ACBr';

type

  TACBrPSPPixPDV = class(TACBrPSP)
  private
    fCNPJ: String;
    fToken: String;

  protected
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;

    procedure ConfigurarAutenticacao(const Method: string; const EndPoint: string); override;
    procedure ConfigurarHeaders(const Method: string; const AURL: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property CNPJ: String read fCNPJ write fCNPJ;
    property Token: String read fToken write fToken;
    property ClientSecret;
  end;

implementation

uses
  ACBrOpenSSLUtils;

{ TACBrPSPPixPDV }

procedure TACBrPSPPixPDV.ConfigurarAutenticacao(const Method, EndPoint: string);
begin
  inherited ConfigurarAutenticacao(Method, EndPoint);

  Http.UserName := CNPJ;
  Http.Password := Token;
end;

procedure TACBrPSPPixPDV.ConfigurarHeaders(const Method, AURL: string);
var
  wHash: String;
  wStrStream: TStringStream;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  inherited ConfigurarHeaders(Method, AURL);

  Http.Headers.Add(cPixPDVHeaderPoweredBy);

  if (Http.Document.Size <= 0) then
    Exit;

  wStrStream := TStringStream.Create('');
  wOpenSSL := TACBrOpenSSLUtils.Create(Nil);
  try
    wStrStream.CopyFrom(Http.Document, 0);
    wHash := wOpenSSL.HMACFromString(wStrStream.DataString, ClientSecret, algSHA256);

    Http.Headers.Add('Json-Hash: ' + wHash);
  finally
    wOpenSSL.Free;
    wStrStream.Free;
  end;
end;

constructor TACBrPSPPixPDV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCNPJ := EmptyStr;
  fToken := EmptyStr;
end;

function TACBrPSPPixPDV.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  if (Ambiente = ambProducao) then
    Result := cPixPDVURLProducao
  else
    Result := cPixPDVURLSandbox;
end;

end.
