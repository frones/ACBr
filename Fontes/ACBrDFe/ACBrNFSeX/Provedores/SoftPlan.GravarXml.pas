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

unit SoftPlan.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXConversao, ACBrNFSeXGravarXml;

type
  { TNFSeW_SoftPlan }

  TNFSeW_SoftPlan = class(TNFSeWClass)
  protected
    function GerarItensServico: TACBrXmlNode;
    function GerarItemServico: TACBrXmlNodeArray;

    function GerarXmlEnvio: Boolean;
    function GerarXmlSubstituicao: Boolean;
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     SoftPlan
//==============================================================================

{ TNFSeW_SoftPlan }

function TNFSeW_SoftPlan.GerarItemServico: TACBrXmlNodeArray;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('itemServico');

    // NFS-e Substituta envia apenas a descrição do serviço.
    if (NFSe.NfseSubstituida <> '') then
    begin
      Result[i].AppendChild(AddNode(tcStr, '#1', 'descricaoServico', 0, 1500, 1,
                                      NFSe.Servico.ItemServico[i].Descricao, ''));

      Continue;
    end;

    Result[i].AppendChild(AddNode(tcDe4, '#1', 'aliquota', 1, 6, 1,
                                 NFSe.Servico.ItemServico[i].Aliquota/100, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#1', 'baseCalculo', 1, 15, 1,
                                  NFSe.Servico.ItemServico[i].BaseCalculo, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'cst', 1, 5, 1,
                                      NFSe.Servico.ItemServico[i].CodServ, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'descricaoServico', 0, 1500, 1,
                                    NFSe.Servico.ItemServico[i].Descricao, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'idCNAE', 0, 5, 1,
                                   NFSe.Servico.ItemServico[i].CodigoCnae, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'quantidade', 1, 8, 1,
                                   NFSe.Servico.ItemServico[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#1', 'valorTotal', 1, 15, 1,
                                   NFSe.Servico.ItemServico[i].ValorTotal, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#1', 'valorUnitario', 1, 15, 1,
                                NFSe.Servico.ItemServico[i].ValorUnitario, ''));
  end;
end;

function TNFSeW_SoftPlan.GerarItensServico: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('itensServico');

  nodeArray := GerarItemServico;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_SoftPlan.GerarXml: Boolean;
begin
  Configuracao;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSe.InfID.ID := NFSe.IdentificacaoRps.Numero;

  if (NFSe.NfseSubstituida = '') then
    Result := GerarXmlEnvio
  else
    Result := GerarXmlSubstituicao;
end;

function TNFSeW_SoftPlan.GerarXmlEnvio: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  NFSeNode := CreateElement('xmlProcessamentoNfpse');

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'bairroTomador', 0, 60, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'baseCalculo', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'baseCalculoSubstituicao', 1, 15, 0,
                                                                        0, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cfps', 4, 4, 1,
                                                  NFSe.Servico.CFPS, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'codigoMunicipioTomador', 7, 7, 1,
                                    NFSe.Tomador.Endereco.CodigoMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'codigoPostalTomador', 8, 8, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'complementoEnderecoTomador', 1, 30, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'dadosAdicionais', 0, 600, 0,
                                                   NFSe.OutrasInformacoes, ''));

  NFSeNode.AppendChild(AddNode(tcDat, '#1', 'dataEmissao', 1, 10, 1,
                                                         NFse.DataEmissao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'emailTomador', 6, 500, 1,
                                               NFSe.Tomador.Contato.Email, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'identificacao', 1, 10, 1,
                                                            NFSe.InfID.ID, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'identificacaoTomador', 0, 20, 1,
                       OnlyNumber(NFSe.Tomador.IdentificacaoTomador.Cnpj), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'inscricaoMunicipalTomador', 0, 30, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

  xmlNode := GerarItensServico;
  NFSeNode.AppendChild(xmlNode);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'logradouroTomador', 0, 60, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'nomeMunicipioTomador', 0, 60, 0,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  // AEDF = Autorização para emissão de documentos fiscais eletrônicos.
  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'numeroAEDF', 6, 7, 1,
                                                             ChaveAutoriz, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'numeroEnderecoTomador', 0, 9, 0,
                                             NFSe.Tomador.Endereco.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcInt, '#1', 'paisTomador', 0, 9, 0,
                                         NFSe.Tomador.Endereco.CodigoPais, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'razaoSocialTomador', 0, 80, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'telefoneTomador', 0, 10, 0,
                                OnlyNumber(NFSe.Tomador.Contato.Telefone), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ufTomador', 2, 2, 0,
                                                 NFSe.Tomador.Endereco.UF, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valorISSQN', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorIss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valorISSQNSubstituicao', 1, 15, 0,
                                                                        0, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'valorTotalServicos', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  Result := True;
end;

function TNFSeW_SoftPlan.GerarXmlSubstituicao: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  NFSeNode := CreateElement('xmlProcessamentoNfpseSubstituta');

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'bairroTomador', 0, 60, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'codigoPostalTomador', 8, 8, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'complementoEnderecoTomador', 1, 30, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'dadosAdicionais', 0, 600, 0,
                                                   NFSe.OutrasInformacoes, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'emailTomador', 6, 500, 1,
                                               NFSe.Tomador.Contato.Email, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'identificacao', 1, 10, 1,
                                                            NFSe.InfID.ID, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'inscricaoMunicipalTomador', 0, 30, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

  xmlNode := GerarItensServico;
  NFSeNode.AppendChild(xmlNode);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'logradouroTomador', 0, 60, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  // AEDF = Autorização para emissão de documentos fiscais eletrônicos.
  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'numeroAEDF', 6, 7, 1,
                                                             ChaveAutoriz, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'numeroEnderecoTomador', 0, 9, 0,
                                             NFSe.Tomador.Endereco.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcInt, '#1', 'nuNotaFiscal', 0, 9, 1,
                                         NFSe.NfseSubstituida, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'razaoSocialTomador', 0, 80, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'telefoneTomador', 0, 10, 0,
                                OnlyNumber(NFSe.Tomador.Contato.Telefone), ''));

  Result := True;
end;

end.
