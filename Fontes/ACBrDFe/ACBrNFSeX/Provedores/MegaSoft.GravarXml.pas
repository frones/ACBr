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

unit MegaSoft.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml_ABRASFv2, ACBrNFSeXConversao;

type
  { TNFSeW_MegaSoft200 }

  TNFSeW_MegaSoft200 = class(TNFSeW_ABRASFv2)
  protected
    function GerarContatoTomador: TACBrXmlNode; override;
    function GerarStatus: TACBrXmlNode; override;

    procedure Configuracao; override;

  end;

implementation

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     MegaSoft
//==============================================================================

{ TNFSeW_MegaSoft200 }

procedure TNFSeW_MegaSoft200.Configuracao;
begin
  inherited Configuracao;

  FormatoEmissao := tcDatHor;
  FormatoCompetencia := tcDatHor;

  NrOcorrCodTribMun_2 := 0;
  NrOcorrInfAdicional := 0;
  NrOcorrCepTomador_2 := 0;

  NrOcorrDiscriminacao_2 := 1;

  NrOcorrSerieRPS := -1;
  NrOcorrTipoRPS := -1;
  NrOcorrUFTomador := -1;
  NrOcorrCodigoPaisServico := -1;
  NrOcorrCodigoPaisTomador := -1;
  NrOcorrCepTomador := -1;
  NrOcorrValorISS := -1;
  NrOcorrCompetencia := -1;
  NrOcorrRegimeEspecialTributacao := -1;
  NrOcorrOptanteSimplesNacional := -1;
  NrOcorrIncentCultural := -1;
  NrOcorrItemListaServico := -1;
  NrOcorrCodigoCNAE := -1;
  NrOcorrCodTribMun_1 := -1;
  NrOcorrDiscriminacao_1 := -1;
  NrOcorrExigibilidadeISS := -1;
  NrOcorrMunIncid := -1;
  NrOcorrRespRetencao := -1;
end;

function TNFSeW_MegaSoft200.GerarContatoTomador: TACBrXmlNode;
begin
  Result := nil;
end;

function TNFSeW_MegaSoft200.GerarStatus: TACBrXmlNode;
begin
  Result := nil;
end;

end.
