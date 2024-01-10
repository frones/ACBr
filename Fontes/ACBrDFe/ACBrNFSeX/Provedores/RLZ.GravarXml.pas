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

unit RLZ.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml, ACBrNFSeXGravarXml_ABRASFv2;

type
  { TNFSeW_RLZ }

  TNFSeW_RLZ = class(TNFSeWClass)
  protected
    function GerarServicos: TACBrXmlNode;
    function GerarTomador: TACBrXmlNode;
    function GerarServico: TACBrXmlNodeArray;
  public
    function GerarXml: Boolean; override;

  end;

  { TNFSeW_RLZ203 }

  TNFSeW_RLZ203 = class(TNFSeW_ABRASFv2)
  protected
    procedure Configuracao; override;

  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrNFSeXConsts,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     RLZ
//==============================================================================

{ TNFSeW_RLZ }

function TNFSeW_RLZ.GerarServico: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('servico');

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'quantidade', 1, 18, 1,
                             NFSe.Servico.ItemServico[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'atividade', 1, 320, 1,
                              NFSe.Servico.ItemServico[i].Descricao, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'valor', 1, 18, 1,
                          NFSe.Servico.ItemServico[i].ValorUnitario, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'deducao', 1, 18, 1,
                 NFSe.Servico.ItemServico[i].DescontoIncondicionado, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'codigoservico', 1, 5, 1,
                       NFSe.Servico.ItemServico[i].ItemListaServico, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'aliquota', 1, 6, 0,
                               NFSe.Servico.ItemServico[i].Aliquota, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'inss', 1, 18, 0,
                              NFSe.Servico.ItemServico[i].ValorINSS, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'total', 1, 18, 0,
                             NFSe.Servico.ItemServico[i].ValorTotal, ''));

  end;

  if NFSe.Servico.ItemServico.Count > 10 then
    wAlerta('#1', 'servico', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_RLZ.GerarServicos: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('servicos');

  nodeArray := GerarServico;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_RLZ.GerarTomador: TACBrXmlNode;
begin
  Result := CreateElement('tomador');

  Result.AppendChild(AddNode(tcStr, '#2', 'endereco', 1, 40, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'numero', 1, 10, 0,
                                             NFSe.Tomador.Endereco.Numero, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'complemento', 1, 20, 0,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'bairro', 1, 40, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'cep', 1, 10, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'cidade', 1, 50, 1,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'uf', 1, 2, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'nome', 1, 80, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'nomefantasia', 1, 40, 0,
                                                NFSe.Tomador.NomeFantasia, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'inscricao', 1, 20, 0,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'cpfcnpj', 11, 14, 1,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'rgie', 1, 20, 0,
          OnlyNumber(NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual), ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'email', 1, 40, 0,
                                               NFSe.Tomador.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'ddd', 1, 2, 0,
                                                 NFSe.Tomador.Contato.DDD, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'fone', 1, 8, 0,
                                OnlyNumber(NFSe.Tomador.Contato.Telefone), ''));
end;

function TNFSeW_RLZ.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

//  Opcoes.SuprimirDecimais := True;
//  Opcoes.DecimalChar := '.';
  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('nota');

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cpfcnpj', 11, 14, 1,
                OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'inscricao', 1, 20, 1,
     OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'chave', 1, 50, 1, ChaveAcesso, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'cep', 1, 10, 1,
                                              NFSe.Prestador.Endereco.CEP, ''));

  NFSeNode.AppendChild(AddNode(tcDat, '#1', 'data', 1, 10, 1,
                                                         NFse.DataEmissao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'modelo', 1, 10, 1,
                                                             'Eletronica', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'serie', 1, 5, 1,
                                              NFse.IdentificacaoRps.Serie, ''));

//  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'tipo', 1, 10, 1,
//                                                                  'Valor', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'fatura', 1, 10, 0, '', ''));
  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'orcamento', 1, 10, 0, '', ''));

  NFSeNode.AppendChild(AddNode(tcDat, '#1', 'vencimento', 1, 10, 1,
                                                          NFse.Vencimento, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'pis', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'csll', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'cofins', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'irff', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'situacao', 1, 10, 1,
                                                             'Emitida', ''));

  if NFSe.OptanteSimplesNacional = snSim then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'optante', 1, 3, 1, 'Sim', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'optante', 1, 3, 1, 'Nao', ''));

  NFSeNode.AppendChild(AddNode(tcDe4, '#1', 'aliquota', 1, 6, 0,
                                            NFSe.Servico.Valores.Aliquota, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'texto', 1, 3200, 0,
                                                   NFSe.OutrasInformacoes, ''));

  xmlNode := GerarServicos;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarTomador;
  NFSeNode.AppendChild(xmlNode);

//  if NFSe.Servico.Valores.IssRetido = stRetencao then
//    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'retido', 1, 3, 1, 'Sim', ''))
//  else
//    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'retido', 1, 3, 1, 'Nao', ''));

  Result := True;
end;

{ TNFSeW_RLZ203 }

procedure TNFSeW_RLZ203.Configuracao;
begin
  inherited Configuracao;

  FormatoAliq := tcDe2;
end;

end.
