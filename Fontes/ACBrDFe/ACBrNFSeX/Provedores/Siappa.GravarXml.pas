{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Renato Tanchela Rubinho                         }
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

unit Siappa.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXGravarXml;

type
  { TNFSeW_Siappa }

  TNFSeW_Siappa = class(TNFSeWClass)
  protected
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrNFSeXConversao,
  ACBrNFSeXConsts,
  ACBrUtil.Strings,
  ACBrNFSeXClass;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Siappa
//==============================================================================

{ TNFSeW_Siappa }

function TNFSeW_Siappa.GerarXml: Boolean;
var
  NFSeNode: TACBrXmlNode;
  OpcExecucao: String;
  cpfCnpj: String;
  tipoDocumento: String;
  DescReduzida: String;
  envEmail: String;
  issRetido: String;
  aliquota: double;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSe.InfID.ID := NFSe.IdentificacaoRps.Numero;

  // Atenção: Neste xml apenas o "Sdt_" da tag raiz deve ter o primeiro "S" em maiúsculo
  NFSeNode := CreateElement('Sdt_ws_001_in_gera_nfse_token');

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_prest_insc_seq', 1, 8, 1,
                            Usuario, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_prest_cnpj', 14, 14, 1,
                            OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_prest_ws_senha', 1, 20, 1,
                            Senha, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_prest_ws_token', 1, 20, 1,
                            ChaveAutoriz, ''));

  cpfCnpj := OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj);

  if Length(cpfCnpj) > 11 then
    tipoDocumento := 'J'
  else
    tipoDocumento := 'F';

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_nat', 1, 1, 1,
                                      tipoDocumento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_cpf_cnpj', 11, 14, 1,
                                      cpfCnpj, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_nome_rso', 1, 50, 1,
                                       NFSe.Tomador.RazaoSocial, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_end_log_tip', 1, 3, 1,
                                       NFSe.Tomador.Endereco.TipoLogradouro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_end_log_des', 1, 50, 1,
                                       NFSe.Tomador.Endereco.Endereco, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_end_nro', 1, 5, 1,
                                       NFSe.Tomador.Endereco.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_end_compl', 1, 20, 1,
                                       NFSe.Tomador.Endereco.Complemento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_end_bairro', 1, 20, 1,
                                       NFSe.Tomador.Endereco.Bairro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_end_cidade', 1, 50, 1,
                                       NFSe.Tomador.Endereco.xMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_end_uf', 2, 2, 1,
                                       NFSe.Tomador.Endereco.UF, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_end_cep', 8, 8, 1,
                                       NFSe.Tomador.Endereco.CEP, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_tom_e_mail', 1, 100, 0,
                                       NFSe.Tomador.Contato.Email, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_nfse_cod_atividade', 1, 4, 1,
                                       NFSe.Servico.ItemListaServico, ''));

  DescReduzida := NFSe.OutrasInformacoes;

  if DescReduzida = '' then
    DescReduzida := 'VIDE DESCRICAO DETALHADA';

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_nfse_desc_resumida', 1, 100, 1,
                                       DescReduzida, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_nfse_desc_detalhada', 1, 1000, 1,
                                       NFSe.Servico.Discriminacao, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ws_001_in_nfse_valor_bruto', 1, 18, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ws_001_in_nfse_valor_deducoes', 1, 18, 0,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  if NFSe.Tomador.Endereco.CodigoMunicipio = NFSe.Prestador.Endereco.CodigoMunicipio then
    issRetido := 'S'
  else
    issRetido := 'N';

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_nfse_local_retencao', 1, 1, 1,
                                      issRetido, ''));

  aliquota := 0;
  if issRetido = 'N' then
    aliquota := NFSe.Servico.Valores.Aliquota;

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ws_001_in_nfse_aliquota_aplicada', 1, 8, 0,
                                       aliquota, ''));

  if NFSe.Tomador.Contato.Email = '' then
    envEmail := 'N'
  else
    envEmail := 'S';

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_nfse_opcao_envio_e_mail', 1, 1, 1,
                                      envEmail, ''));

  // (T)estes ou (D)efinitivo
  if FpAOwner.ConfigGeral.Ambiente = taHomologacao then
    OpcExecucao := 'T'
  else
    OpcExecucao := 'D';

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ws_001_in_opcao_execucao', 1, 1, 1,
                                      OpcExecucao, ''));

  Result := True;
end;

end.
