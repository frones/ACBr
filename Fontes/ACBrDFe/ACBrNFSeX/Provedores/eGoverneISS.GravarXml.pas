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

unit eGoverneISS.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXConversao;

type
  { TNFSeW_eGoverneISS }

  TNFSeW_eGoverneISS = class(TNFSeWClass)
  protected
    procedure Configuracao; override;

    function GerarTomador: TACBrXmlNode;
    function GerarEndereco: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     eGoverneISS
//==============================================================================

{ TNFSeW_eGoverneISS }

procedure TNFSeW_eGoverneISS.Configuracao;
begin
  inherited Configuracao;

end;

function TNFSeW_eGoverneISS.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();
  PrefixoPadrao := 'eis';

  NFSeNode := CreateElement('NotaFiscal');

  FDocument.Root := NFSeNode;

  NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                      NFSe.IdentificacaoRps.Serie;

  PrefixoPadrao := 'eis1';

  NFSeNode.AppendChild(AddNode(tcDe4, '#1', 'Aliquota', 1, 15, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Atividade', 1, 9, 1,
                                   NFSe.Servico.CodigoTributacaoMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CEPPrestacaoServico', 1, 36, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ChaveAutenticacao', 1, 36, 1,
                                                              ChaveAcesso, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CidadePrestacaoServico', 1, 36, 1,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CodObra', 1, 15, 0,
                                          NFSe.ConstrucaoCivil.CodigoObra, ''));

  if NFSe.IdentificacaoRps.Numero <> '' then
    NFSeNode.AppendChild(AddNode(tcDat, '#1', 'DataRecibo', 1, 10, 0,
                                                      NFSe.DataEmissaoRps, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'EnderecoPrestacaoServico', 1, 36, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'EqptoRecibo', 1, 5, 0,
                                                         NFSe.EqptoRecibo, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'EstadoPrestacaoServico', 2, 2, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'Homologacao', 4, 5, 1,
                           ifThen(NFSe.Producao = snSim, 'false', 'true'), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'InformacoesAdicionais', 1, 2300, 0,
                                                   NFSe.OutrasInformacoes, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NotaSubstituida', 1, 10, 0,
                                                     NFSe.NfseSubstituida, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NotificarTomadorPorEmail', 4, 5, 1,
                 ifThen(NFSe.Tomador.Contato.Email = '', 'false', 'true'), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumeroCDC', 1, 15, 0,
                                                 NFSe.ConstrucaoCivil.Art, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumeroCei', 1, 15, 0,
                                                NFSe.ConstrucaoCivil.nCei, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NumeroRecibo', 1, 15, 0,
                                             NFSe.IdentificacaoRps.Numero, ''));

  if (Trim(NFSe.Tomador.Endereco.xPais) <> '') and
     (NFSe.Tomador.Endereco.xPais <> 'BRASIL') then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SemIncidenciaISS', 4, 5, 1,
                                                                    'true', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SemIncidenciaISS', 4, 5, 1,
                                                                  'false', ''));

  if NFSe.OptanteSimplesNacional = snSim then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SimplesNacional', 4, 5, 1,
                                                                    'true', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SimplesNacional', 4, 5, 1,
                                                                  'false', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SubstituicaoTributaria', 5, 5, 1,
                                                                  'false', ''));

  if NFSE.Tomador.IdentificacaoTomador.CpfCnpj <> '' then
  begin
    xmlNode := GerarTomador;
    NFSeNode.AppendChild(xmlNode);
  end;

  PrefixoPadrao := 'eis1';

  if (Trim(NFSe.Tomador.Endereco.xPais) <> '') and
     (NFSe.Tomador.Endereco.xPais <> 'BRASIL') then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TomadorEstrangeiro', 4, 5, 1,
                                                                    'true', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'TomadorEstrangeiro', 4, 5, 1,
                                                                  'false', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'Valor', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorCSLL', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorCOFINS', 1, 15, 0,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorDeducao', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorINSS', 1, 15, 0,
                                           NFSe.Servico.Valores.ValorInss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorIR', 1, 15, 0,
                                             NFSe.Servico.Valores.ValorIr, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorOutrosImpostos', 1, 15, 0,
                                     NFSe.Servico.Valores.OutrasRetencoes, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorPisPasep', 1, 15, 0,
                                            NFSe.Servico.Valores.ValorPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ValorRepasse', 1, 15, 0,
                                        NFSe.Servico.Valores.ValorRepasse, ''));

  Result := True;
end;

function TNFSeW_eGoverneISS.GerarEndereco: TACBrXmlNode;
begin
  Result := CreateElement('Endereco');

  Result.AppendChild(AddNode(tcStr, '#1', 'Bairro', 1, 50, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'CEP', 1, 8, 1,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Cidade', 1, 50, 1,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Complemento', 1, 30, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Estado', 2, 2, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Logradouro', 1, 50, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 9, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Pais', 1, 8, 1,
                                              NFSe.Tomador.Endereco.xPais, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'TipoLogradouro', 1, 10, 0,
                                     NFSe.Tomador.Endereco.TipoLogradouro, ''));
end;

function TNFSeW_eGoverneISS.GerarTomador: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('Tomador');

  PrefixoPadrao := 'eis2';

  if Length(NFSE.Tomador.IdentificacaoTomador.CpfCnpj) > 11 then
    Result.AppendChild(AddNode(tcStr, '#1', 'CNPJ', 1, 14, 1,
                                 NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''))
  else
    Result.AppendChild(AddNode(tcStr, '#1', 'CPF', 1, 14, 1,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));

  if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 11 then
    Result.AppendChild(AddNode(tcStr, '#1', 'DDD', 1, 3, 0,
                     LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 3), ''))
  else
    if Length(OnlyNumber(NFSe.Tomador.Contato.Telefone)) = 10 then
      Result.AppendChild(AddNode(tcStr, '#1', 'DDD', 1, 3, 0,
                    LeftStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 2), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Email', 1, 120, 0,
                                               NFSe.Tomador.Contato.Email, ''));

  xmlNode := GerarEndereco;
  Result.AppendChild(xmlNode);

  Result.AppendChild(AddNode(tcStr, '#1', 'InscricaoMunicipal', 1, 11, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Nome', 1, 120, 0,
                                                 NFSe.Tomador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'Telefone', 1, 8, 0,
                   RightStr(OnlyNumber(NFSe.Tomador.Contato.Telefone), 8), ''));
end;

end.
