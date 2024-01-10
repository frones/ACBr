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

unit SimplISS.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml_ABRASFv1, ACBrNFSeXGravarXml_ABRASFv2;

type
  { TNFSeW_SimplISS }

  TNFSeW_SimplISS = class(TNFSeW_ABRASFv1)
  protected
    procedure Configuracao; override;

    function GerarItensServico: TACBrXmlNodeArray; override;
  end;

  { TNFSeW_SimplISS203 }

  TNFSeW_SimplISS203 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;
    function GerarServico: TACBrXmlNode; override;
  end;

implementation

uses
  ACBrNFSeXConversao,
  ACBrNFSeXConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     SimplISS
//==============================================================================

{ TNFSeW_SimplISS }

procedure TNFSeW_SimplISS.Configuracao;
begin
  inherited Configuracao;

  FormatoItemListaServico := filsComFormatacaoSemZeroEsquerda;

  NrOcorrOutrasInformacoes := 0;
  NrOcorrInscEstTomador := 0;

  PrefixoPadrao := 'nfse';
end;

function TNFSeW_SimplISS.GerarItensServico: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('ItensServico');

    Result[i].AppendChild(AddNode(tcStr, '#33', 'Descricao    ', 1, 100, 1,
                             NFSe.Servico.ItemServico[i].Descricao, DSC_XSERV));

    Result[i].AppendChild(AddNode(tcDe2, '#34', 'Quantidade   ', 1, 015, 1,
                             NFSe.Servico.ItemServico[i].Quantidade, DSC_QTDE));

    Result[i].AppendChild(AddNode(tcDe2, '#35', 'ValorUnitario', 1, 015, 1,
                         NFSe.Servico.ItemServico[i].ValorUnitario, DSC_VUNIT));
  end;

  if NFSe.Servico.ItemServico.Count > 999 then
    wAlerta('#54', 'ItensServico', '', ERR_MSG_MAIOR_MAXIMO + '999');
end;

{ TNFSeW_SimplISS203 }

procedure TNFSeW_SimplISS203.Configuracao;
begin
  inherited Configuracao;

  FormatoAliq := tcDe2;

  if FpAOwner.ConfigGeral.Params.TemParametro('Aliquota4Casas') then
    FormatoAliq := tcDe4;

  NrOcorrValorDeducoes := 1;
  NrOcorrValorPis := 1;
  NrOcorrValorCofins := 1;
  NrOcorrValorInss := 1;
  NrOcorrValorIr := 1;
  NrOcorrValorCsll := 1;
  NrOcorrValorIss := 1;
  NrOcorrOutrasRet := 1;
  NrOcorrValTotTrib := 1;
  NrOcorrAliquota := 1;
  NrOcorrDescIncond := 1;
  NrOcorrDescCond := 1;
  NrOcorrCodigoPaisServico := 1;

  GerarIDRps := True;
end;

function TNFSeW_SimplISS203.GerarServico: TACBrXmlNode;
begin
  Result := inherited GerarServico;

  if GerarTagServicos then
  begin
    Result.AppendChild(AddNode(tcStr, '#22', 'OutrasInformacoes', 0, 255, 0,
                                        NFSe.OutrasInformacoes, DSC_OUTRASINF));
  end;
end;

end.
