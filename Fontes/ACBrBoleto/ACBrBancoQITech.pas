{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
//incluido 30/10/2023
{$I ACBr.inc}
unit ACBrBancoQITech;
interface
uses
  Classes, SysUtils, ACBrBancoBradesco, ACBrBoleto;
type
  { TACBrBancoQITechSCD }
  TACBrBancoQITechSCD = class(TACBrBancoBradesco)
  private
  public
    Constructor create(AOwner: TACBrBanco);
    function GetLocalPagamento: String; override;
    function CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String; override;
  end;
implementation
uses ACBrUtil.Base,
     ACBrUtil.FilesIO,
     ACBrUtil.Strings;
constructor TACBrBancoQITechSCD.create(AOwner: TACBrBanco);
begin
  inherited create(AOwner);

  fpNome                       := 'QI SCD';
  fpNumero                     := 329;
  fpDigito                     := 2;
  fpTamanhoMaximoNossoNum      := 11;
  fpTamanhoAgencia             := 4;
  fpTamanhoConta               := 7;
  fpTamanhoCarteira            := 2;
  fpModuloMultiplicadorInicial := 0;
  fpModuloMultiplicadorFinal   := 7;
  fpCodParametroMovimento      := 'MX';
end;

function TACBrBancoQITechSCD.GetLocalPagamento: String;
begin
  Result := ACBrStr('Pagável preferencialmente na QI SCD');
end;

function TACBrBancoQITechSCD.CodMotivoRejeicaoToDescricao(const TipoOcorrencia: TACBrTipoOcorrencia; const CodMotivo: String): String;
var LMotivo : String;
begin
  LMotivo := inherited CodMotivoRejeicaoToDescricao(TipoOcorrencia, CodMotivo);
  LMotivo := StringReplace( LMotivo, 'Bradesco Expresso', 'QI Tech', [rfReplaceAll,rfIgnoreCase]);
  Result  := StringReplace( LMotivo, 'Bradesco', 'QI Tech', [rfReplaceAll,rfIgnoreCase]);
end;
end.
