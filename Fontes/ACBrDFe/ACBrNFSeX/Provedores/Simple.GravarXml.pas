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

unit Simple.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument, pcnConsts,
  ACBrNFSeXConversao, ACBrNFSeXConsts, ACBrNFSeXGravarXml;

type
  { TNFSeW_Simple }

  TNFSeW_Simple = class(TNFSeWClass)
  protected
    function GerarTomador: TACBrXmlNode;
    function GerarTItens: TACBrXmlNode;
    function GerarItens: TACBrXmlNodeArray;
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrUtil.Strings;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Simple
//==============================================================================

{ TNFSeW_Simple }

function TNFSeW_Simple.GerarTomador: TACBrXmlNode;
begin
  Result := CreateElement('tTomador');

  Result.AppendChild(AddNode(tcStr, '#1', 'sCPFTomador', 1, 14, 0,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'sNomeTomador', 1, 60, 0,
                                                 NFSe.Tomador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'sCidadeTomador', 1, 70, 0,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'sEnderecoTomador', 1, 80, 0,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'sEmailTomador', 1, 50, 0,
                                               NFSe.Tomador.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'sUfTomador', 1, 2, 0,
                                                 NFSe.Tomador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#2', 'sTipoTomador ', 1, 1, 1,
                                                                       '', ''));
end;

function TNFSeW_Simple.GerarTItens: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  Result := CreateElement('tItens');

  nodeArray := GerarItens;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_Simple.GerarItens: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, NFSe.Servico.ItemServico.Count);

  for i := 0 to NFSe.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('Itens');

    Result[i].AppendChild(AddNode(tcInt, '#55', 'iSequencia', 1, 2, 1,
                                                                      i+1, ''));

    Result[i].AppendChild(AddNode(tcStr, '#55', 'iServico', 1, 6, 1,
                 OnlyNumber(NFSe.Servico.ItemServico[i].ItemListaServico), ''));

    Result[i].AppendChild(AddNode(tcStr, '#55', 'sCNAE', 1, 6, 1,
                                   NFSe.Servico.ItemServico[i].CodigoCnae, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#57', 'nValorServico', 1, 15, 1,
                         NFSe.Servico.ItemServico[i].ValorUnitario, DSC_VPARC));

    Result[i].AppendChild(AddNode(tcStr, '#55', 'sDescricao', 1, 140, 1,
                                    NFSe.Servico.ItemServico[i].Descricao, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#57', 'nAliquota', 1, 15, 1,
                              NFSe.Servico.ItemServico[i].Aliquota, DSC_VPARC));

    Result[i].AppendChild(AddNode(tcDe2, '#57', 'nValorIss', 1, 15, 1,
                              NFSe.Servico.ItemServico[i].ValorISS, DSC_VPARC));

    Result[i].AppendChild(AddNode(tcDe2, '#57', 'nValorTotal', 1, 15, 1,
                            NFSe.Servico.ItemServico[i].ValorTotal, DSC_VPARC));
  end;

  if NFSe.Servico.ItemServico.Count > 99 then
    wAlerta('#1', 'Itens', '', ERR_MSG_MAIOR_MAXIMO + '99');
end;

function TNFSeW_Simple.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
  Situacao, Observacao: String;
  i: Integer;
begin
  Configuracao;

//  Opcoes.SuprimirDecimais := True;
//  Opcoes.DecimalChar := '.';
  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSe.InfID.ID := NFSe.IdentificacaoRps.Numero;

  NFSeNode := CreateElement('Nota');

  FDocument.Root := NFSeNode;

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'sRetorno', 1, 2000, 1,
                                                                       '', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'sContribuinte', 1, 14, 1,
                OnlyNumber(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj), ''));

  NFSeNode.AppendChild(AddNode(tcInt, '#1', 'iRecibo', 1, 8, 1,
                                             NFSe.IdentificacaoRps.Numero, ''));

  NFSeNode.AppendChild(AddNode(tcDat, '#1', 'dDataRecibo', 1, 10, 1,
                                                         NFSe.DataEmissao, ''));

  NFSeNode.AppendChild(AddNode(tcInt, '#1', 'iNota', 1, 8, 1, 0, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'sSerie', 1, 2, 1,
                                              NFSe.IdentificacaoRps.Serie, ''));

  NFSeNode.AppendChild(AddNode(tcDat, '#1', 'dDataEmissao', 1, 10, 1,
                                                         NFSe.DataEmissao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'sCodigoVerificador', 1, 2000, 1,
                                                                       '', ''));

  Situacao := EnumeradoToStr(NFSe.StatusRps, ['N', 'C'], [srNormal, srCancelado]);

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'sSituacao', 1, 1, 1, Situacao, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'sSt', 1, 1, 1, 'N', ''));

  Situacao := FpAOwner.TipoTributacaoRPSToStr(NFSe.TipoTributacaoRPS);

  // N - Normal, S - Simples Nacional, I - Isento, R - Iss Retido
  // P - Pago em Outro Município, T - Substituição Tributaria
  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'sTributacao', 1, 1, 1,
                                                                 Situacao, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'nValorTotal', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'nValorIss', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorIss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'nValorBaseCalculo', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

  xmlNode := GerarTomador;
  NFSeNode.AppendChild(xmlNode);

  NFSeNode.AppendChild(AddNode(tcInt, '#1', 'iLinhas', 2, 2, 1,
                                           NFSe.Servico.ItemServico.Count, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'nIrAliquota', 1, 15, 1,
                                          NFSe.Servico.Valores.AliquotaIr, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'nIrValor', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'nPisPasep', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'nCofins', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'nInss', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'nCsll', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  xmlNode := GerarTItens;
  NFSeNode.AppendChild(xmlNode);

  Observacao := NFSe.OutrasInformacoes;
  i := 0;

  repeat
    Inc(i);

    NFSeNode.AppendChild(AddNode(tcStr, '#1', 'sObservacao' + IntToStr(i), 1, 100, 1,
                                                 Copy(Observacao, 1, 100), ''));

    Observacao := Copy(Observacao, 101, Length(Observacao))

  until (i = 10) or (Observacao = '');

  Result := True;
end;

end.
