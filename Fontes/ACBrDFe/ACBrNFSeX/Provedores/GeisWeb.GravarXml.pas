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

unit GeisWeb.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao;

type
  { TNFSeW_GeisWeb }

  TNFSeW_GeisWeb = class(TNFSeWClass)
  protected
    function GerarIdentificacaoRps: TACBrXmlNode;
    function GerarServico: TACBrXmlNode;
    function GerarValores: TACBrXmlNode;
    function GerarPrestadorServico: TACBrXmlNode;
    function GerarIdentificacaoPrestador: TACBrXmlNode;
    function GerarTomadorServico: TACBrXmlNode;
    function GerarIdentificacaoTomador: TACBrXmlNode;
    function GerarEnderecoTomador: TACBrXmlNode;
    function GerarOrgaoGerador: TACBrXmlNode;
    function GerarOutrosImpostos: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     GeisWeb
//==============================================================================

{ TNFSeW_GeisWeb }

function TNFSeW_GeisWeb.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('Rps', FpAOwner.ConfigMsgDados.LoteRps.xmlns, '');

  FDocument.Root := NFSeNode;

  xmlNode := GerarIdentificacaoRps;
  NFSeNode.AppendChild(xmlNode);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DataEmissao', 1, 10, 1,
                           FormatDateTime('dd/mm/yyyy', NFSe.DataEmissao), ''));

  xmlNode := GerarServico;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarPrestadorServico;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarTomadorServico;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarOrgaoGerador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarOutrosImpostos;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_GeisWeb.GerarEnderecoTomador: TACBrXmlNode;
begin
  Result := CreateElement('Endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'Rua', 1, 60, 1,
                                           Nfse.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 10, 1,
                                             Nfse.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Bairro', 1, 50, 1,
                                             Nfse.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Cidade', 1, 40, 1,
                                    Nfse.Tomador.Endereco.CodigoMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Estado', 1, 2, 1,
                                                 Nfse.Tomador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Cep', 1, 11, 1,
                                                Nfse.Tomador.Endereco.CEP, ''));
end;

function TNFSeW_GeisWeb.GerarIdentificacaoPrestador: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoPrestador');

  Result.AppendChild(AddNode(tcStr, '#1', 'CnpjCpf', 1, 14, 1,
                            NFSe.Prestador.IdentificacaoPrestador.CpfCnpj, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 16, 1,
                 NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Regime', 1, 1, 1,
    FpAOwner.RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), ''));
end;

function TNFSeW_GeisWeb.GerarIdentificacaoRps: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoRps');

  Result.AppendChild(AddNode(tcInt, '#1', 'NumeroRps', 1, 8, 1,
                                             NFSe.IdentificacaoRps.Numero, ''));
end;

function TNFSeW_GeisWeb.GerarIdentificacaoTomador: TACBrXmlNode;
begin
  Result := CreateElement('IdentificacaoTomador');

  Result.AppendChild(AddNode(tcStr, '#1', 'CnpjCpf', 1, 14, 1,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));
end;

function TNFSeW_GeisWeb.GerarOrgaoGerador: TACBrXmlNode;
begin
  Result := CreateElement('OrgaoGerador');

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoMunicipio', 1, 10, 1,
                                             Nfse.Servico.CodigoMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Uf', 1, 2, 1,
                                                 Nfse.Servico.UFPrestacao, ''));
end;

function TNFSeW_GeisWeb.GerarOutrosImpostos: TACBrXmlNode;
begin
  Result := CreateElement('OutrosImpostos');

  Result.AppendChild(AddNode(tcDe2, '#1', 'Pis', 1, 13, 1,
                                            Nfse.Servico.Valores.ValorPis, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'Cofins', 1, 13, 1,
                                         Nfse.Servico.Valores.ValorCofins, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'Csll', 1, 13, 1,
                                           Nfse.Servico.Valores.ValorCsll, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'Irrf', 1, 13, 1,
                                             Nfse.Servico.Valores.ValorIr, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'Inss', 1, 13, 1,
                                           Nfse.Servico.Valores.ValorInss, ''));
end;

function TNFSeW_GeisWeb.GerarPrestadorServico: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('PrestadorServico');

  xmlNode := GerarIdentificacaoPrestador;
  Result.AppendChild(xmlNode);
end;

function TNFSeW_GeisWeb.GerarServico: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('Servico');

  xmlNode := GerarValores;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'CodigoServico', 1, 4, 1,
                                OnlyNumber(NFSe.Servico.ItemListaServico), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'TipoLancamento', 1, 1, 1,
                         TipoLancamentoToStr(NFSe.Servico.TipoLancamento), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Discriminacao', 1, 1500, 1,
                                               NFSe.Servico.Discriminacao, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'MunicipioPrestacaoServico', 1, 100, 1,
                                   NFSe.Servico.MunicipioPrestacaoServico, ''));
end;

function TNFSeW_GeisWeb.GerarTomadorServico: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('TomadorServico');

  xmlNode := GerarIdentificacaoTomador;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'RazaoSocial', 1, 100, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  xmlNode := GerarEnderecoTomador;
  Result.AppendChild(xmlNode);
end;

function TNFSeW_GeisWeb.GerarValores: TACBrXmlNode;
begin
  Result := CreateElement('Valores');

  Result.AppendChild(AddNode(tcDe2, '#1', 'ValorServicos', 1, 13, 1,
                                       Nfse.Servico.Valores.ValorServicos, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'BaseCalculo', 1, 13, 1,
                                         Nfse.Servico.Valores.BaseCalculo, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'Aliquota', 1, 13, 1,
                                            Nfse.Servico.Valores.Aliquota, ''));
end;

end.
