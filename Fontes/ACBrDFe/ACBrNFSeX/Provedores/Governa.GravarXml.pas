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

unit Governa.GravarXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXGravarXml;

type
  { TNFSeW_Governa }

  TNFSeW_Governa = class(TNFSeWClass)
  protected
    function GerarInfRps: TACBrXmlNode;
    function GerarItensRps: TACBrXmlNode;
    function GerarItemRps: TACBrXmlNodeArray;
  public
    function GerarXml: Boolean; override;

  end;

implementation

uses
  ACBrNFSeXConversao,
  ACBrNFSeXConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS do provedor:
//     Governa
//==============================================================================

{ TNFSeW_Governa }

function TNFSeW_Governa.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  Configuracao;

  ListaDeAlertas.Clear;

  Opcoes.DecimalChar := ',';
  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  FDocument.Clear();

  NFSeNode := CreateElement('tcRps');
  NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.XmlRps.xmlns, Self.PrefixoPadrao);

  FDocument.Root := NFSeNode;

  xmlNode := GerarInfRps;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_Governa.GerarInfRps: TACBrXmlNode;
var
  xmlNode: TACBrXmlNode;
  strAux: string;
begin
  Result := CreateElement('tcInfRps');

  Result.AppendChild(AddNode(tcStr, '#1', 'tsNumRps', 1, 10, 1,
                                     NFSe.IdentificacaoRps.Numero, DSC_NUMRPS));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsCodVer', 1, 10, 1,
                                                   NFSe.CodigoVerificacao, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsVrsImp', 1, 1, 1,
            FpAOwner.ConfigGeral.Params.ValorParametro('VersaoImpressao'), ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsNumDocTmd', 11, 14, 1,
                                NFSe.Tomador.IdentificacaoTomador.CpfCnpj, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsInsEstTmd', 1, 20, 1,
                      NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsTlfTmd', 1, 11, 1,
                                            NFSe.Tomador.Contato.Telefone, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsInsMunTmd', 1, 20, 1,
                     NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsNomTmd', 5, 100, 1,
                                                 NFSe.Tomador.RazaoSocial, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsDesEndTmd', 5, 100, 1,
                                           NFSe.Tomador.Endereco.Endereco, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsNomBaiTmd', 1, 60, 1,
                                             NFSe.Tomador.Endereco.Bairro, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsNomCidTmd', 1, 60, 1,
                                         NFSe.Tomador.Endereco.xMunicipio, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsCodEstTmd', 2, 2, 1,
                                                 NFSe.Tomador.Endereco.UF, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsCEPTmd', 8, 8, 1,
                                                NFSe.Tomador.Endereco.CEP, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsEmlTmd', 1, 50, 1,
                                               NFSe.Tomador.Contato.Email, ''));

  Result.AppendChild(AddNode(tcStr, '#1', 'tsCodAti', 4, 10, 1,
                                   NFSe.Servico.CodigoTributacaoMunicipio, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsPerAlq', 1, 15, 1,
                                            NFSe.Servico.Valores.Aliquota, ''));

  if RegRecToStr(NFSe.RegRec) <> '' then
  begin
    Result.AppendChild(AddNode(tcStr, '#1', 'tsRegRec', 1, 2, 1,
                                                 RegRecToStr(NFSe.RegRec), ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'tsFrmRec', 1, 2, 1,
                                                 FrmRecToStr(NFSe.FrmRec), ''));

    if FpAOwner.ConfigGeral.Versao = ve101 then
    begin
      Result.AppendChild(AddNode(tcStr, '#1', 'tsMesCmp', 1, 4, 1,
                                   FormatDateTime('MM', NFSe.Competencia), ''));

      Result.AppendChild(AddNode(tcStr, '#1', 'tsAnoCmp', 1, 4, 1,
                                 FormatDateTime('YYYY', NFSe.Competencia), ''));

      Result.AppendChild(AddNode(tcStr, '#1', 'tsDatEmsRps', 8, 8, 1,
        StringReplace(FormatDateTime('yyyymmdd', NFSe.DataEmissao), '/', '', [rfReplaceAll]), ''));
    end;

    if FpAOwner.ConfigGeral.Versao = ve100 then
    begin
      Result.AppendChild(AddNode(tcStr, '#1', 'tsDatEmsRps', 8, 8, 1,
        StringReplace(FormatDateTime('yyyymmdd', NFSe.DataEmissao), '/', '', [rfReplaceAll]), ''));

      Result.AppendChild(AddNode(tcStr, '#1', 'tsAnoCpt', 1, 4, 1,
                                 FormatDateTime('YYYY', NFSe.Competencia), ''));

      Result.AppendChild(AddNode(tcStr, '#1', 'tsMesCpt', 1, 4, 1,
                                   FormatDateTime('MM', NFSe.Competencia), ''));
    end;
  end
  else
  begin
    Result.AppendChild(AddNode(tcStr, '#1', 'tsFrmTrb', 1, 2, 1, '11', ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'tsTipRec', 1, 1, 1,
                                                    NFSe.TipoRecolhimento, ''));
  end;

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsVlrRep', 1, 16, 1,
                                        NFSe.Servico.Valores.ValorRepasse, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsVlrDed', 1, 16, 1,
                                       NFSe.Servico.Valores.ValorDeducoes, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsVlrDsc', 1, 16, 1,
                                NFSe.Servico.Valores.DescontoCondicionado, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsVlrPIS', 1, 16, 1,
                                            NFSe.Servico.Valores.ValorPis, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsVlrCOFINS', 1, 16, 1,
                                         NFSe.Servico.Valores.ValorCofins, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsVlrINSS', 1, 16, 1,
                                           NFSe.Servico.Valores.ValorInss, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsVlrIR', 1, 16, 1,
                                             NFSe.Servico.Valores.ValorIr, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsVlrCSLL', 1, 16, 1,
                                           NFSe.Servico.Valores.ValorCsll, ''));

  Result.AppendChild(AddNode(tcDe2, '#1', 'tsVlrOtrRtn', 1, 16, 1,
                                NFSe.Servico.Valores.valorOutrasRetencoes, ''));

  if RegRecToStr(NFSe.RegRec) <> '' then
  begin
    if NFSe.Servico.UFPrestacao = '' then
      strAux := NFSe.Tomador.Endereco.UF
    else
      strAux := NFSe.Servico.UFPrestacao;

    Result.AppendChild(AddNode(tcStr, '#1', 'tsEstServ', 2, 2, 1, strAux, ''));

    if NFSe.Servico.CodigoMunicipio = '' then
      strAux := NFSe.Tomador.Endereco.CodigoMunicipio
    else
      strAux := NFSe.Servico.CodigoMunicipio;

    Result.AppendChild(AddNode(tcStr, '#1', 'tsMunSvc', 1, 7, 1, strAux, ''));

    Result.AppendChild(AddNode(tcStr, '#1', 'tsDesOtrRtn', 1, 1000, 1,
                            NFSe.Servico.Valores.DescricaoOutrasRetencoes, ''));
  end;

  Result.AppendChild(AddNode(tcStr, '#1', 'tsObs', 1, 1000, 1,
                                                   NFSe.OutrasInformacoes, ''));

  xmlNode := GerarItensRps;
  Result.AppendChild(xmlNode);
end;

function TNFSeW_Governa.GerarItemRps: TACBrXmlNodeArray;
var
  i: integer;
begin
  Result := nil;
  SetLength(Result, Nfse.Servico.ItemServico.Count);

  for i := 0 to Nfse.Servico.ItemServico.Count - 1 do
  begin
    Result[i] := CreateElement('tcItemRps');

    Result[i].AppendChild(AddNode(tcInt, '#1', 'tsSeqItem', 1, 2, 1, i + 1, ''));

    Result[i].AppendChild(AddNode(tcStr, '#1', 'tsDesSvc', 1, 100, 1,
                              Nfse.Servico.ItemServico.Items[i].Descricao, ''));

//    Result[i].AppendChild(AddNode(tcDe2, '#1', 'tsQdeSvc', 1, 9, 1,
//                             Nfse.Servico.ItemServico.Items[i].Quantidade, ''));

    Result[i].AppendChild(AddNode(tcDe2, '#1', 'tsVlrUnt', 1, 16, 1,
                          Nfse.Servico.ItemServico.Items[i].ValorUnitario, ''));
  end;

  if Nfse.Servico.ItemServico.Count > 20 then
    wAlerta('#54', 'tcItemRps', '', ERR_MSG_MAIOR_MAXIMO + '20');
end;

function TNFSeW_Governa.GerarItensRps: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: integer;
begin
  Result := CreateElement('tcItensRps');

  nodeArray := GerarItemRps;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

end.
