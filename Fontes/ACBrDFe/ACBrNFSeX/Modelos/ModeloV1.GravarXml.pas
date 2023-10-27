{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ModeloV1.GravarXml;
{
  Trocar todas as ocorrencias de "ModeloV1" pelo nome do provedor
}

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrNFSeXGravarXml_ABRASFv1;

type
  { TNFSeW_ModeloV1 }

  TNFSeW_ModeloV1 = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     ModeloV1
//==============================================================================

{ TNFSeW_ModeloV1 }

procedure TNFSeW_ModeloV1.Configuracao;
begin
  // Executa a Configuração Padrão
  inherited Configuracao;

  {
     Todos os parâmetros de configuração estão com os seus valores padrões.

     Se a configuração padrão atende o provedor ela pode ser excluida dessa
     procedure.

     Portanto deixe somente os parâmetros de configuração que foram alterados
     para atender o provedor.
  }

  // Propriedades de Formatação de informações
  // elas requerem que seja declarado em uses a unit: ACBrXmlBase
  {
  FormatoEmissao     := tcDatHor;
  FormatoCompetencia := tcDatHor;

  FormatoAliq := tcDe4;
  }

  // elas requerem que seja declarado em uses a unit: ACBrNFSeXConversao
  {
  // filsComFormatacao, filsSemFormatacao, filsComFormatacaoSemZeroEsquerda
  FormatoItemListaServico := filsComFormatacao;
  }

  DivAliq100  := False;

  NrMinExigISS := 1;
  NrMaxExigISS := 1;

  // Numero de Ocorrencias Minimas de uma tag
  // se for  0 só gera a tag se o conteudo for diferente de vazio ou zero
  // se for  1 sempre vai gerar a tag
  // se for -1 nunca gera a tag

  // Por padrão as tags abaixo são opcionais
  NrOcorrComplTomador := 0;
  NrOcorrFoneTomador := 0;
  NrOcorrEmailTomador := 0;
  NrOcorrOutrasRet := 0;
  NrOcorrAliquota := 0;
  NrOcorrValorPis := 0;
  NrOcorrValorCofins := 0;
  NrOcorrValorInss := 0;
  NrOcorrValorIr := 0;
  NrOcorrValorCsll := 0;
  NrOcorrValorIss := 0;
  NrOcorrBaseCalc := 0;
  NrOcorrDescIncond := 0;
  NrOcorrDescCond := 0;
  NrOcorrValLiq := 0;
  NrOcorrCodigoCnae := 0;
  NrOcorrCodTribMun := 0;
  NrOcorrMunIncid := 0;
  NrOcorrCodigoPaisTomador := 0;
  NrOcorrRazaoSocialInterm := 0;
  NrOcorrValorDeducoes := 0;
  NrOcorrValorISSRetido_1 := 0;
  NrOcorrInscMunTomador := 0;

  // Por padrão as tags abaixo são obrigatórias
  NrOcorrOptanteSN := 1;
  NrOcorrIncentCult := 1;
  NrOcorrStatus := 1;
  NrOcorrItemListaServico := 1;
  NrOcorrNaturezaOperacao := 1;

  // Por padrão as tags abaixo não devem ser geradas
  NrOcorrRespRetencao := -1;
  NrOcorrIdCidade := -1;
  NrOcorrValorISSRetido_2 := -1;
  NrOcorrValorTotalRecebido := -1;
  NrOcorrInscEstTomador := -1;
  NrOcorrOutrasInformacoes := -1;
  NrOcorrCodPaisTomador := -1;
  NrOcorrInformacoesComplemetares := -1;
  NrOcorrRegimeEspecialTributacao := -1;
end;

end.
