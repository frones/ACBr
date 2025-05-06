{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Victor H Goznales - Pandaa                     }
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

//Incluido em 06/05/2025
{$I ACBr.inc}
unit ACBrBoletoWS.URL;

interface

uses
  ACBrBoletoConversao,
  ACBrBoleto,
  SysUtils;

type
  EACBrBoletoWSException = class(Exception);
  TACBrBoletoWebServiceURL = class
  private
    FBoletoWS: TACBrWebService;
    FURLProducao: String;
    FURLHomologacao: String;
    FURLSandBox: String;
    FPathURI : String;
  public
    constructor Create(ABoletoWS: TACBrWebService);
    function GetURL : String;
    procedure SetPathURI (const APathURI : String);
    procedure Clear;
    property URLProducao: String read FURLProducao write FURLProducao;
    property URLHomologacao: String read FURLHomologacao write FURLHomologacao;
    property URLSandBox: String read FURLSandBox write FURLSandBox;
  end;

implementation

uses
  ACBrUtil;

{ TACBrBoletoWebServiceURL }
procedure TACBrBoletoWebServiceURL.Clear;
begin
  FURLProducao    := '';
  FURLHomologacao := '';
  FURLSandBox     := '';
  FPathURI         := '';
end;

constructor TACBrBoletoWebServiceURL.Create(ABoletoWS: TACBrWebService);
begin
  inherited Create;
  FBoletoWS := ABoletoWS;
  Self.Clear;
end;

function TACBrBoletoWebServiceURL.GetURL : String;
var LURL : String;
begin
  Result := '';

  case FBoletoWS.Ambiente of
    tawsProducao    : LURL := FURLProducao;
    tawsHomologacao : LURL := FURLHomologacao;
    tawsSandBox     : LURL := FURLSandBox;
  end;

  if Trim(LURL) = '' then
    raise EACBrBoletoWSException.Create(Format( 'A URL %s não foi definida para o metodo' ,
                                        [AmbienteBoletoWSToStr( FBoletoWS.Ambiente )]));

  Result := LURL + FPathURI;
end;

procedure TACBrBoletoWebServiceURL.SetPathURI(const APathURI: String);
begin
  FPathURI := APathURI;
end;

end.
