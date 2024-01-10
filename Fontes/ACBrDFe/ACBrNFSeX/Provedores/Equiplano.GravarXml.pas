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

unit Equiplano.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXClass,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml;

type
  { Provedor com layout próprio }
  { TNFSeW_Equiplano }

  TNFSeW_Equiplano = class(TNFSeWClass)
  protected
    function GerarTomador: TACBrXmlNode;
    function GerarDocumento: TACBrXmlNode;
    function GerarListaServicos: TACBrXmlNode;
    function GerarServico(Indice, Item, SubItem: Integer): TACBrXmlNode;
    function GerarDeducao: TACBrXmlNode;
    function GerarRetencoes: TACBrXmlNode;
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrUtil.Strings, ACBrNFSeXConversao, ACBrNFSeXConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Equiplano
//==============================================================================

{ TNFSeW_Equiplano }

function TNFSeW_Equiplano.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  ListaDeAlertas.Clear;

  FDocument.Clear();

  NFSeNode := CreateElement('rps');
  NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.XmlRps.xmlns, Self.PrefixoPadrao);

  FDocument.Root := NFSeNode;

  NFSe.InfID.ID := OnlyNumber(NFSe.IdentificacaoRps.Numero) +
                      NFSe.IdentificacaoRps.Serie;

  NFSeNode.AppendChild(AddNode(tcInt, '#1', 'nrRps', 1, 15, 1,
                         OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'nrEmissorRps', 1, 1, 1,
                                    NFSe.IdentificacaoRps.Serie, DSC_SERIERPS));

  NFSeNode.AppendChild(AddNode(tcDatHor, '#1', 'dtEmissaoRps', 19, 19, 1,
                                                  NFSe.DataEmissao, DSC_DHEMI));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'stRps', 1, 1, 1, '1', ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'tpTributacao', 1, 1, 1,
                             NaturezaOperacaoToStr(NFSe.NaturezaOperacao), ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'nrCidadeIbgeServico', 0, 7, 0,
                                             NFSe.Servico.CodigoMunicipio, ''));

  NFSeNode.AppendChild(AddNode(tcStr, '#1', 'isIssRetido', 1, 1, 1,
    FpAOwner.SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), ''));

  xmlNode := GerarTomador;
  NFSeNode.AppendChild(xmlNode);

  xmlNode := GerarListaServicos;
  NFSeNode.AppendChild(xmlNode);


  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'vlTotalRps', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

  NFSeNode.AppendChild(AddNode(tcDe2, '#1', 'vlLiquidoRps', 1, 15, 1,
                                    NFSe.Servico.Valores.ValorLiquidoNfse, ''));

  xmlNode := GerarRetencoes;
  NFSeNode.AppendChild(xmlNode);

  if NFSe.Servico.Valores.DescontoIncondicionado > 0 then
    NFseNode.AppendChild(AddNode(tcDe2, '#1', 'vlDesconto', 1, 15, 1, NFSe.Servico.Valores.DescontoIncondicionado, ''));

  Result := True;
end;

function TNFSeW_Equiplano.GerarDocumento: TACBrXmlNode;
var
  sTpDoc: String;
begin
  if (Trim(NFSe.Tomador.IdentificacaoTomador.DocEstrangeiro) <> '') then
    sTpDoc := '3'  // Estrangeiro
  else
  begin
    if (Length(OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj)) = 14) then
      sTpDoc := '2'  // CNPJ
    else
      sTpDoc := '1'; // CPF
  end;

  Result := CreateElement('documento');

  Result.AppendChild(AddNode(tcStr, '#1', 'nrDocumento', 1, 14, 1,
                    OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tpDocumento', 1, 1, 1, sTpDoc, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'dsDocumentoEstrangeiro', 0, 20, 1,
                         NFSe.Tomador.IdentificacaoTomador.DocEstrangeiro, ''));
end;

function TNFSeW_Equiplano.GerarListaServicos: TACBrXmlNode;
var
  iAux, iSerItem, iSerSubItem, i: Integer;
  itemServico: TItemServicoCollectionItem;
  xmlNode: TACBrXmlNode;

  procedure tratarSerItem(AItemServico: string);
  begin
    iAux := StrToIntDef(OnlyNumber(AItemServico), 0); //Ex.: 1402, 901

    if (iAux > 999) then //Ex.: 1402
    begin
      iSerItem    := StrToInt(Copy(IntToStr(iAux), 1, 2)); //14
      iSerSubItem := StrToInt(Copy(IntToStr(iAux), 3, 2)); //2
    end
    else
    begin //Ex.: 901
      iSerItem    := StrToInt(Copy(IntToStr(iAux), 1, 1)); //9
      iSerSubItem := StrToInt(Copy(IntToStr(iAux), 2, 2)); //1
    end;
  end;

begin
  Result := CreateElement('listaServicos');

  if NFSe.Servico.ItemServico.Count > 1 then
  begin
    for i := 0 to NFSe.Servico.ItemServico.Count -1 do
    begin
      itemServico := NFSe.Servico.ItemServico[i];
      if itemServico.CodServ <> '' then
        tratarSerItem(itemServico.CodServ)
      else
        tratarSerItem(NFSe.Servico.ItemListaServico);

      xmlNode := GerarServico(i, iSerItem, iSerSubItem);
      Result.AppendChild(xmlNode);
    end;
  end
  else
  begin
    tratarSerItem(NFSe.Servico.ItemListaServico);

    xmlNode := GerarServico(-1, iSerItem, iSerSubItem);
    Result.AppendChild(xmlNode);
  end;
end;

function TNFSeW_Equiplano.GerarRetencoes: TACBrXmlNode;
begin
  Result := CreateElement('retencoes');

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlCofins', 1, 15, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlCsll', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlInss', 1, 15, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlIrrf', 1, 15, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlPis', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlIss', 1, 15, 1,
                                      NFSe.Servico.Valores.ValorIssRetido, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlAliquotaCofins', 1, 2, 1,
                                      NFSe.Servico.Valores.AliquotaCofins, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlAliquotaCsll', 1, 2, 1,
                                        NFSe.Servico.Valores.AliquotaCsll, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlAliquotaInss', 1, 2, 1,
                                        NFSe.Servico.Valores.AliquotaInss, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlAliquotaIrrf', 1, 2, 1,
                                          NFSe.Servico.Valores.AliquotaIr, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlAliquotaPis', 1, 2, 1,
                                         NFSe.Servico.Valores.AliquotaPis, ''));
end;

function TNFSeW_Equiplano.GerarServico(Indice, Item,
  SubItem: Integer): TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := CreateElement('servico');

  Result.AppendChild(AddNode(tcInt, '#1', 'nrServicoItem', 1, 2, 1, Item, ''));

  Result.AppendChild(AddNode(tcInt, '#1', 'nrServicoSubItem', 1, 2, 1,
                                                                  SubItem, ''));

  if Indice >= 0 then
  begin
    Result.AppendChild(AddNode(tcDe2, '#1', 'vlServico', 1, 15, 1,
                     NFSe.Servico.ItemServico.Items[Indice].ValorUnitario, ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vlAliquota', 1, 2, 1,
                          NFSe.Servico.ItemServico.Items[Indice].Aliquota, ''));
  end
  else
  begin
    Result.AppendChild(AddNode(tcDe2, '#1', 'vlServico', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorServicos, ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vlAliquota', 1, 2, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));
  end;

  if (NFSe.Servico.Valores.ValorDeducoes > 0) then
  begin
    xmlNode := GerarDeducao;
    Result.AppendChild(xmlNode);
  end;

  if Indice >= 0 then
  begin
    Result.AppendChild(AddNode(tcDe2, '#1', 'vlBaseCalculo', 1, 15, 1,
                       NFSe.Servico.ItemServico.Items[Indice].BaseCalculo, ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vlIssServico', 1, 15, 1,
                          NFSe.Servico.ItemServico.Items[Indice].ValorISS, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'dsDiscriminacaoServico', 1, 1024, 1,
                     NFSe.Servico.ItemServico.Items[Indice].Descricao, ''));
  end
  else
  begin
    Result.AppendChild(AddNode(tcDe2, '#1', 'vlBaseCalculo', 1, 15, 1,
                                         NFSe.Servico.Valores.BaseCalculo, ''));

    Result.AppendChild(AddNode(tcDe2, '#1', 'vlIssServico', 1, 15, 1,
                                            NFSe.Servico.Valores.ValorIss, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'dsDiscriminacaoServico', 1, 1024, 1,
                                               NFSe.Servico.Discriminacao, ''));
  end;
end;

function TNFSeW_Equiplano.GerarDeducao: TACBrXmlNode;
begin
  Result := CreateElement('deducao');

  Result.AppendChild(AddNode(tcDe2, '#1', 'vlDeducao', 1, 15, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'dsJustificativaDeducao', 1, 255, 1,
                                NFSe.Servico.Valores.JustificativaDeducao, ''));
end;

function TNFSeW_Equiplano.GerarTomador: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
begin
  Result := nil;

  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') and
     (NFSe.Tomador.RazaoSocial <> '') then
  begin
    Result := CreateElement('tomador');

    xmlNode := GerarDocumento;
    Result.AppendChild(xmlNode);

    Result.AppendChild(AddNode(tcStr, '#1', 'nmTomador', 1, 80, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'dsEmail', 1, 80, 0,
                                               NFSe.Tomador.Contato.Email, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'nrInscricaoEstadual', 1, 20, 0,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'nrInscricaoMunicipal', 1, 20, 0,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'dsEndereco', 1, 40, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'nrEndereco', 0, 10, 1,
                                             NFSe.Tomador.Endereco.Numero, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'dsComplemento', 0, 60, 1,
                                        NFSe.Tomador.Endereco.Complemento, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'nmBairro', 0, 25, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'nrCidadeIbge', 0, 7, 1,
                                    NFSe.Tomador.Endereco.CodigoMunicipio, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'nmUf', 0, 2, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'nmPais', 0, 40, 1,
                                              NFSe.Tomador.Endereco.xPais, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'nrCep', 0, 15, 1,
                                    OnlyNumber(NFSe.Tomador.Endereco.CEP), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'nrTelefone', 0, 20, 1,
                                            NFSe.Tomador.Contato.Telefone, ''));
  end;
end;

end.
