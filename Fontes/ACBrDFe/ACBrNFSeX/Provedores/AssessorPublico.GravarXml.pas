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

unit AssessorPublico.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml;

type
  { Provedor com layout próprio }
  { TNFSeW_AssessorPublico }

  TNFSeW_AssessorPublico = class(TNFSeWClass)
  protected
    function GerarServicos: TACBrXmlNode;
    function GerarServico: TACBrXmlNodeArray;
  public
    function GerarXml: Boolean; override;
  end;

implementation

uses
  ACBrNFSeXConsts,
  ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     AssessorPublico
//==============================================================================

{ TNFSeW_AssessorPublico }

function TNFSeW_AssessorPublico.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('NOTA');

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LOTE', 1, 15, 1,
                                                          NFSe.NumeroLote, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'SEQUENCIA', 1, 15, 1,
                                             NFSe.IdentificacaoRps.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DATAEMISSAO', 1, 10, 1,
                            FormatDateTime('dd/MM/yyyy',NFSe.DataEmissao), ''));

  NFSeNode.AppendChild(AddNode(tcHor, '#1', 'HORAEMISSAO', 1, 10, 1,
                                                         NFSe.DataEmissao, ''));

  if NFSe.Servico.CodigoMunicipio = IntToStr(NFSe.Servico.MunicipioIncidencia) then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LOCAL', 1, 1, 1, 'D', ''))
  else
  begin
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LOCAL', 1, 1, 1, 'F', ''));

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'UFFORA', 1, 1, 1,
                                                 NFSe.Servico.UFPrestacao, ''));
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MUNICIPIOFORA', 1, 1, 1,
                               IntToStr(NFSe.Servico.MunicipioIncidencia), ''));

//    Gerador.wCampo(tcStr, '', 'PAISFORA', 1, 1, 1, '1', '');
  end;

  NFSeNode.AppendChild(AddNode(tcInt, '#1', 'SITUACAO', 1, 4, 1,
                                                            NFSe.Situacao, ''));

  if NFSe.Servico.Valores.IssRetido = stRetencao then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETIDO', 1, 1, 1, 'S', ''))
  else
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETIDO', 1, 1, 1, 'N', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'ATIVIDADE', 1, 10, 1,
                                            NFSe.Servico.CodigoCnae, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'ALIQUOTAAPLICADA', 1, 5, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'DEDUCAO', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'IMPOSTO', 1, 15, 1,
                                NFSe.Servico.Valores.valorOutrasRetencoes, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'RETENCAO', 1, 15, 1,
                                      NFSe.Servico.Valores.ValorIssRetido, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'OBSERVACAO', 1, 1000, 1,
                                               NFSe.Servico.Discriminacao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CPFCNPJ', 1, 14, 1,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RGIE', 1, 14, 1,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NOMERAZAO', 1, 60, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NOMEFANTASIA', 1, 60, 1,
                                                 NFSe.Tomador.NomeFantasia, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'MUNICIPIO', 1, 7, 1,
                                    NFSe.Tomador.Endereco.CodigoMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'BAIRRO', 1, 60, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'CEP', 1, 10, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'PREFIXO', 1, 10, 1,
                                     NFSe.Tomador.Endereco.TipoLogradouro, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'LOGRADOURO', 1, 60, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'COMPLEMENTO', 1, 60, 1,
                                        NFSe.Tomador.Endereco.Complemento, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'NUMERO', 1, 10, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'EMAIL', 1, 100, 1,
                                             NFSe.Tomador.Contato.Email, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'DENTROPAIS', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'PIS', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  if NFSe.Servico.Valores.ValorPis > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETPIS', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'COFINS', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  if NFSe.Servico.Valores.ValorCofins > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETCOFINS', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'INSS', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  if NFSe.Servico.Valores.ValorInss > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETINSS', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'IR', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  if NFSe.Servico.Valores.ValorIr > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETIR', 1, 1, 1, 'S', ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'CSLL', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  if NFSe.Servico.Valores.ValorCsll > 0 then
    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'RETCSLL', 1, 1, 1, 'S', ''));

  xmlNode := GerarServicos;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_AssessorPublico.GerarServico: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('SERVICO');

    Result[i].AppendChild(AddNode(tcStr, '#1', 'DESCRICAO', 1, 60, 1,
                              NFSe.Servico.ItemServico.Items[i].Descricao, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'VALORUNIT', 1, 15, 1,
                          NFSe.Servico.ItemServico.Items[i].ValorUnitario, ''));

    Result[i].AppendChild(AddNode(tcDe4, '#1', 'QUANTIDADE', 1, 10, 1,
                             NFSe.Servico.ItemServico.Items[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'DESCONTO', 1, 10, 1,
                 NFSe.Servico.ItemServico.Items[i].DescontoIncondicionado, ''));
  end;

  if NFSe.Servico.ItemServico.Count > 10 then
    wAlerta('#54', 'SERVICO', '', ERR_MSG_MAIOR_MAXIMO + '10');
end;

function TNFSeW_AssessorPublico.GerarServicos: TACBrXmlNode;
var
  i : integer;
  nodeArray: TACBrXmlNodeArray;
begin
  Result := CreateElement('SERVICOS');

  nodeArray := GerarServico;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

end.
