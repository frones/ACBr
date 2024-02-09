{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit Sigep.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml_ABRASFv2, ACBrNFSeXConversao,
  ACBrNFSeXConsts;

type
  { TNFSeW_Sigep200 }

  TNFSeW_Sigep200 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Sigep
//==============================================================================

{ TNFSeW_Sigep200 }

procedure TNFSeW_Sigep200.Configuracao;
begin
  inherited Configuracao;

  FormatoEmissao := tcDatHor;
  FormatoCompetencia := tcDatHor;
  FormatoItemListaServico := filsComFormatacaoSemZeroEsquerda;

  NrMinExigISS := 2;
  NrMaxExigISS := 2;

  NrOcorrTipoLogradouro := 0;
  NrOcorrLogradouro := 0;
  NrOcorrDDD := 0;
  NrOcorrTipoTelefone := 0;
  NrOcorrInscEstTomador_1 := 0;

  NrOcorrCompetencia := -1;
  NrOcorrEndereco := -1;
  NrOcorrSerieRPS := -1;
  NrOcorrValorISS := -1;
  NrOcorrIssRetido := -1;
  NrOcorrRespRetencao := -1;
  NrOcorrCodigoPaisServico := -1;
  NrOcorrCodigoPaisTomador := -1;
  NrOcorrMunIncid := -1;
  NrOcorrRegimeEspecialTributacao := -1;
  NrOcorrOptanteSimplesNacional := -1;
  NrOcorrIncentCultural := -1;
end;

end.
