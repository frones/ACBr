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

{$I ACBr.inc}

unit ACBr.DANFSeX.Classes;

interface

uses
  Classes, SysUtils, ACBrBase;

type


  TDadosNecessariosParaDANFSeX = class(TObject)
  private
    FNaturezaOperacaoDescricao: string;     //FProvider.NaturezaOperacaoDescricao(NaturezaOperacao)
    FRegimeEspecialDescricao: string;       //FProvider.RegimeEspecialTributacaoDescricao(RegimeEspecialTributacao)
    FIssAReterDescricao: string;            //FProvider.SituacaoTributariaDescricao(IssRetido);
    FOptanteSimplesDescricao: string;       //FProvider.SimNaoDescricao(OptanteSimplesNacional);
    FIncentivadorCulturalDescricao: string; //FProvider.SimNaoDescricao(IncentivadorCultural);
    FDetalhar: Boolean;                     //FProvider.ConfigGeral.DetalharServico;
    FQuebradeLinha: string;                 //FProvider.ConfigGeral.QuebradeLinha

  public
    constructor Create();

    //provider
    property NaturezaOperacaoDescricao: string read FNaturezaOperacaoDescricao write FNaturezaOperacaoDescricao;
    property RegimeEspecialDescricao: string read FRegimeEspecialDescricao write FRegimeEspecialDescricao;
    property IssAReterDescricao: string read FIssAReterDescricao write FIssAReterDescricao;
    property OptanteSimplesDescricao: string read FOptanteSimplesDescricao write FOptanteSimplesDescricao;
    property IncentivadorCulturalDescricao: string read FIncentivadorCulturalDescricao write FIncentivadorCulturalDescricao;
    property Detalhar: Boolean read FDetalhar write FDetalhar;
    property QuebradeLinha: string read FQuebradeLinha write FQuebradeLinha;
    //...provider
  end;

implementation

{ TDadosNecessariosParaDANFSeX }

constructor TDadosNecessariosParaDANFSeX.Create();
begin
  inherited;

  FNaturezaOperacaoDescricao := '';
  FRegimeEspecialDescricao := '';
  FIssAReterDescricao := '';
  FOptanteSimplesDescricao := '';
  FIncentivadorCulturalDescricao := '';
  FDetalhar := False;
end;

end.
