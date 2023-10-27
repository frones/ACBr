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

unit ModeloV2.GravarXml;
{
  Trocar todas as ocorrencias de "ModeloV2" pelo nome do provedor
}

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrNFSeXGravarXml_ABRASFv2;

type
  { TNFSeW_ModeloV2200 }

  TNFSeW_ModeloV2200 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     ModeloV2
//==============================================================================

{ TNFSeW_ModeloV2200 }

procedure TNFSeW_ModeloV2200.Configuracao;
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
  FormatoEmissao     := tcDat;
  FormatoCompetencia := tcDat;

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

  GerarTagServicos := True;

  // Gera ou não o atributo ID no grupo <Rps> da versão 2 do layout da ABRASF.
  GerarIDRps := False;
  // Gera ou não o NameSpace no grupo <Rps> da versão 2 do layout da ABRASF.
  GerarNSRps := True;

  GerarIDDeclaracao := True;
  GerarEnderecoExterior := True;

  TagTomador := 'Tomador';
  TagIntermediario := 'Intermediario';

  // Numero de Ocorrencias Minimas de uma tag
  // se for  0 só gera a tag se o conteudo for diferente de vazio ou zero
  // se for  1 sempre vai gerar a tag
  // se for -1 nunca gera a tag

  // Por padrão as tags abaixo são opcionais
  NrOcorrRazaoSocialInterm := 0;
  NrOcorrValorDeducoes := 0;
  NrOcorrRegimeEspecialTributacao := 0;
  NrOcorrValorISS := 0;
  NrOcorrAliquota := 0;
  NrOcorrDescIncond := 0;
  NrOcorrDescCond := 0;
  NrOcorrMunIncid := 0;
  NrOcorrInscEstInter := 0;
  NrOcorrOutrasRet := 0;
  NrOcorrCodigoCNAE := 0;
  NrOcorrEndereco := 0;
  NrOcorrCodigoPaisTomador := 0;
  NrOcorrUFTomador := 0;
  NrOcorrCepTomador := 0;
  NrOcorrCodTribMun_1 := 0;
  NrOcorrNumProcesso := 0;
  NrOcorrInscMunTomador := 0;
  NrOcorrCodigoPaisServico := 0;
  NrOcorrRespRetencao := 0;

  // Por padrão as tags abaixo são obrigatórias
  NrOcorrIssRetido := 1;
  NrOcorrOptanteSimplesNacional := 1;
  NrOcorrIncentCultural := 1;
  NrOcorrItemListaServico := 1;
  NrOcorrCompetencia := 1;
  NrOcorrSerieRPS := 1;
  NrOcorrTipoRPS := 1;
  NrOcorrDiscriminacao_1 := 1;
  NrOcorrExigibilidadeISS := 1;
  NrOcorrCodigoMunic_1 := 1;

  // Por padrão as tags abaixo não devem ser geradas
  NrOcorrCodTribMun_2 := -1;
  NrOcorrDiscriminacao_2 := -1;
  NrOcorrNaturezaOperacao := -1;
  NrOcorrIdCidade := -1;
  NrOcorrValorTotalRecebido := -1;
  NrOcorrInscEstTomador_1 := -1;
  NrOcorrInscEstTomador_2 := -1;
  NrOcorrOutrasInformacoes := -1;
  NrOcorrTipoNota := -1;
  NrOcorrSiglaUF := -1;
  NrOcorrEspDoc := -1;
  NrOcorrSerieTal := -1;
  NrOcorrFormaPag := -1;
  NrOcorrNumParcelas := -1;
  NrOcorrBaseCalcCRS := -1;
  NrOcorrIrrfInd := -1;
  NrOcorrRazaoSocialPrest := -1;
  NrOcorrPercCargaTrib := -1;
  NrOcorrValorCargaTrib := -1;
  NrOcorrPercCargaTribMun := -1;
  NrOcorrValorCargaTribMun := -1;
  NrOcorrPercCargaTribEst := -1;
  NrOcorrValorCargaTribEst := -1;
  NrOcorrInformacoesComplemetares := -1;
  NrOcorrValTotTrib := -1;
  NrOcorrTipoLogradouro := -1;
  NrOcorrLogradouro := -1;
  NrOcorrDDD := -1;
  NrOcorrTipoTelefone := -1;
  NrOcorrProducao := -1;
  NrOcorrAtualizaTomador := -1;
  NrOcorrTomadorExterior := -1;
  NrOcorrCodigoMunic_2 := -1;
  NrOcorrID := -1;
  NrOcorrToken := -1;
  NrOcorrSenha := -1;
  NrOcorrFraseSecreta := -1;
  NrOcorrAliquotaPis := -1;
  NrOcorrRetidoPis := -1;
  NrOcorrAliquotaCofins := -1;
  NrOcorrRetidoCofins := -1;
  NrOcorrAliquotaInss := -1;
  NrOcorrRetidoInss := -1;
  NrOcorrAliquotaIr := -1;
  NrOcorrRetidoIr := -1;
  NrOcorrAliquotaCsll := -1;
  NrOcorrRetidoCsll := -1;
end;

end.
