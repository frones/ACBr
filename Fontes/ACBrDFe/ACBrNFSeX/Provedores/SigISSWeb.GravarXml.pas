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

unit SigISSWeb.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  pcnConsts,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao;

type
  { Provedor com layout próprio }
  { TNFSeW_SigISSWeb }

  TNFSeW_SigISSWeb = class(TNFSeWClass)
  protected

  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     SigISSWeb
//==============================================================================

{ TNFSeW_SigISSWeb }

function TNFSeW_SigISSWeb.GerarXml: Boolean;
var
  NFSeNode: TACBrXmlNode;
  tomadorIdentificado, tipoPessoa: string;
begin
  Configuracao;

  Opcoes.DecimalChar := ',';
  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('notafiscal');

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cnpj_cpf_prestador', 11, 14, 1,
                            NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, ''));

  tomadorIdentificado := '0';

  if NFSe.Tomador.IdentificacaoTomador.CpfCnpj = '' then
    tomadorIdentificado := '1';

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'exterior_dest', 1, 1, 1,
                                                      tomadorIdentificado, ''));

  if tomadorIdentificado = '0' then
  begin
    tipoPessoa := 'J';

    if Length(NFSe.Tomador.IdentificacaoTomador.CpfCnpj) < 14 then
      tipoPessoa := 'F';

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cnpj_cpf_destinatario', 11, 14, 1,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'pessoa_destinatario', 1, 1, 1,
                                                               tipoPessoa, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ie_destinatario', 1, 16, 1,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'im_destinatario', 1, 16, 1,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));
  end;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'razao_social_destinatario', 1, 60, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'endereco_destinatario', 1, 60, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'numero_ende_destinatario', 1, 10, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'complemento_ende_destinatario', 1, 60, 1,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'bairro_destinatario', 1, 60, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cep_destinatario', 1, 8, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cidade_destinatario', 1, 60, 1,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'uf_destinatario', 2, 2, 1,
                                               NFSe.Tomador.Endereco.UF, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'pais_destinatario', 1, 60, 1,
                                            NFSe.Tomador.Endereco.xPais, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'fone_destinatario', 1, 10, 0,
                                          NFSe.Tomador.Contato.Telefone, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'email_destinatario', 1, 100, 1,
                                             NFSe.Tomador.Contato.Email, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valor_nf', 1, 15, 1,
                                    NFSe.Servico.Valores.ValorServicos, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'deducao', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valor_servico', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorLiquidoNfse, ''));

  NFSeNode.AppendChild(AddNode(tcDatVcto, '#1', 'data_emissao', 10, 10, 1,
                                                         NFSe.DataEmissao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'forma_de_pagamento', 1, 2, 1,
                                                                       '', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'descricao', 1, 100, 1,
                                               NFSe.Servico.Discriminacao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'id_codigo_servico', 1, 5, 1,
                                            NFSe.Servico.ItemListaServico, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cancelada', 1, 1, 1, 'N', ''));

  if NFSe.Servico.Valores.IssRetido = stRetencao then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'iss_retido', 1, 1, 1, 'S', ''))
  else if NFSe.Servico.Valores.IssRetido = stRetidoForaMunicipio then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'iss_retido', 1, 1, 1, 'F', ''))
  else if NFSe.Servico.Valores.IssRetido = stDevidoForaMunicipioNaoRetido then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'iss_retido', 1, 1, 1, 'D', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'iss_retido', 1, 1, 1, 'N', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'aliq_iss', 1, 15, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valor_iss', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorIss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'bc_pis', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'aliq_pis', 1, 15, 1,
                                         NFSe.Servico.Valores.AliquotaPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valor_pis', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'bc_cofins', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'aliq_cofins', 1, 15, 1,
                                      NFSe.Servico.Valores.AliquotaCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valor_cofins', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'bc_csll', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'aliq_csll', 1, 15, 1,
                                        NFSe.Servico.Valores.AliquotaCsll, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valor_csll', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'bc_irrf', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'aliq_irrf', 1, 15, 1,
                                          NFSe.Servico.Valores.AliquotaIr, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valor_irrf', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'bc_inss', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'aliq_inss', 1, 15, 1,
                                        NFSe.Servico.Valores.AliquotaInss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valor_inss', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'sistema_gerador', 1, 60, 1,
                                                            NFSe.verAplic, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'serie_rps', 1, 10, 1,
                                              NFSe.IdentificacaoRps.Serie, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'rps', 1, 10, 1,
                                             NFSe.IdentificacaoRps.Numero, ''));

  Result := True;
end;

end.
