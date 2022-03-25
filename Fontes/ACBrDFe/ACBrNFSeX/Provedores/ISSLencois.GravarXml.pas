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

unit ISSLencois.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  pcnAuxiliar,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao;

type
  { TNFSeW_ISSLencois }

  TNFSeW_ISSLencois = class(TNFSeWClass)
  protected
    function GerarPASNF: TACBrXmlNode;
    function GerarTomador: TACBrXmlNode;
    function GerarEnderecoTomador: TACBrXmlNode;
    function GerarRecolhimentoFora: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Lencois
//==============================================================================

{ TNFSeW_ISSLencois }

function TNFSeW_ISSLencois.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('Nota');
  NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.XmlRps.xmlns, Self.PrefixoPadrao);

  FDocument.Root := NFSeNode;

  NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) + NFSe.IdentificacaoRps.Serie;

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'Versao', 1, 3, 1, '1.1', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'InscricaoMunicipal', 1, 5, 1,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));

  xmlNode := GerarPASNF;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarTomador;
  NFSeNode.AppendChild(xmlNode);

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'CidadeExecucao', 1, 7, 0,
                                             NFSe.Servico.CodigoMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#', 'Descricao', 1, 1000, 1,
                                                   NFSe.Servico.Descricao, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'ValorTotal', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'ValorDeducao', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  NFSeNode.AppendChild(AddNode(tcDe6, '#', 'Aliquota', 1, 7, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'ValorPIS', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'ValorCOFINS', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'RetencaoIRRF', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'RetencaoINSS', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'RetencaoPIS', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'RetencaoCOFINS', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#', 'RetencaoCSLL', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  NFSeNode.AppendChild(AddNode(tcInt, '#', 'EnviarEmail', 1, 1, 1, 1, ''));

  NFSeNode.AppendChild(AddNode(tcInt, '#', 'TributacaoISS', 1, 1, 1, 0, ''));

  xmlNode := GerarRecolhimentoFora;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_ISSLencois.GerarPASNF: TACBrXmlNode;
begin
  Result := CreateElement('PASNF');

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 5, 1, NFSe.Numero, ''));

  Result.AppendChild(AddNode(tcDat, '#1', 'Data', 10, 10, 1,
                                                         NFSe.DataEmissao, ''));
end;

function TNFSeW_ISSLencois.GerarRecolhimentoFora: TACBrXmlNode;
begin
  Result := nil;

  if NFSe.Servico.MunicipioIncidencia > 0 then
  begin
    Result := CreateElement('RecolhimentoFora');

    Result.AppendChild(AddNode(tcDe6, '#1', 'Aliquota', 1, 8, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

    case NFSe.Servico.ResponsavelRetencao of
      rtTomador:
        Result.AppendChild(AddNode(tcInt, '#1', 'Obrigacao', 1, 1, 1, 1, ''));
    else
      Result.AppendChild(AddNode(tcInt, '#1', 'Obrigacao', 1, 1, 1, 0, ''));
    end;
  end;
end;

function TNFSeW_ISSLencois.GerarTomador: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('Tomador');

  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or
     (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or
     (NFSe.Tomador.Contato.Email <> '') then
  begin
    if NFSe.Tomador.Endereco.UF <> 'EX' then
      Result.AppendChild(AddNode(tcStr, '#1', 'CPF_CNPJ', 11, 14, 1,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'Nome', 1, 50, 0,
                                                 NFSe.Tomador.RazaoSocial, ''));

    xmlNode := GerarEnderecoTomador;
    Result.AppendChild(xmlNode);

    Result.AppendChild(AddNode(tcStr, '#1', 'Email', 1, 50, 0,
                                               NFSe.Tomador.Contato.Email, ''));

    if Length(OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj)) <= 11 then
      Result.AppendChild(AddNode(tcStr, '#1', 'Particular', 1, 1, 1, '1', ''))
    else
      Result.AppendChild(AddNode(tcStr, '#1', 'Particular', 1, 1, 1, '0', ''));
  end;
end;

function TNFSeW_ISSLencois.GerarEnderecoTomador: TACBrXmlNode;
begin
  Result := CreateElement('Endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'Logradouro', 1, 55, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 15, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Complemento', 1, 25, 1,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Bairro', 1, 75, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Municipio', 7, 7, 1,
                        OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), ''));
end;

end.
