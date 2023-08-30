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

unit ACBrNFSeXGravarXml_ABRASFv1;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrXmlDocument,
  ACBrNFSeXGravarXml;

type
  { TNFSeW_ABRASFv1 }

  TNFSeW_ABRASFv1 = class(TNFSeWClass)
  private
    FNrOcorrComplTomador: Integer;
    FNrOcorrOutrasRet: Integer;
    FNrOcorrAliquota: Integer;
    FNrOcorrBaseCalc: Integer;
    FNrOcorrDescIncond: Integer;
    FNrOcorrDescCond: Integer;
    FNrOcorrFoneTomador: Integer;
    FNrOcorrEmailTomador: Integer;
    FNrOcorrValLiq: Integer;
    FNrOcorrOptanteSN: Integer;
    FNrOcorrIncentCult: Integer;
    FNrOcorrCodigoCnae: Integer;
    FNrOcorrCodTribMun: Integer;
    FNrOcorrValorISSRetido_1: Integer;
    FNrOcorrValorISSRetido_2: Integer;
    FNrOcorrValorTotalRecebido: Integer;
    FNrOcorrInformacoesComplemetares: Integer;
    FNrOcorrInscEstTomador: Integer;
    FNrOcorrOutrasInformacoes: Integer;
    FNrOcorrNaturezaOperacao: Integer;
    FNrOcorrRegimeEspecialTributacao: Integer;
    FNrOcorrValorDeducoes: Integer;
    FNrOcorrRazaoSocialInterm: Integer;
    FNrOcorrRespRetencao: Integer;
    FNrOcorrMunIncid: Integer;
    FNrOcorrIdCidade: Integer;
    FNrOcorrCodPaisTomador: Integer;
    FNrOcorrStatus: Integer;
    FNrOcorrValorPis: Integer;
    FNrOcorrValorInss: Integer;
    FNrOcorrValorIr: Integer;
    FNrOcorrValorCsll: Integer;
    FNrOcorrValorIss: Integer;
    FNrOcorrValorCofins: Integer;
    FNrOcorrInscMunTomador: Integer;

  protected
    procedure Configuracao; override;

    function GerarInfRps: TACBrXmlNode; virtual;
    function GerarIdentificacaoRPS: TACBrXmlNode; virtual;
    function GerarRPSSubstituido: TACBrXmlNode; virtual;
    function GerarServico: TACBrXmlNode; virtual;
    function GerarValores: TACBrXmlNode; virtual;
    function GerarItensServico: TACBrXmlNodeArray; virtual;
    function GerarPrestador: TACBrXmlNode; virtual;
    function GerarTomador: TACBrXmlNode; virtual;
    function GerarIdentificacaoTomador: TACBrXmlNode; virtual;
    function GerarEnderecoTomador: TACBrXmlNode; virtual;
    function GerarContatoTomador: TACBrXmlNode; virtual;
    function GerarIntermediarioServico: TACBrXmlNode; virtual;
    function GerarConstrucaoCivil: TACBrXmlNode; virtual;
    function GerarCondicaoPagamento: TACBrXmlNode; virtual;
    function GerarParcelas: TACBrXmlNodeArray; virtual;

    function GerarServicoCodigoMunicipio: TACBrXmlNode; virtual;
    function GerarCodigoMunicipioUF: TACBrXmlNodeArray; virtual;

  public
    function GerarXml: Boolean; override;

    property NrOcorrComplTomador: Integer read FNrOcorrComplTomador write FNrOcorrComplTomador;
    property NrOcorrOutrasRet: Integer    read FNrOcorrOutrasRet    write FNrOcorrOutrasRet;
    property NrOcorrAliquota: Integer     read FNrOcorrAliquota     write FNrOcorrAliquota;
    property NrOcorrValorPis: Integer     read FNrOcorrValorPis     write FNrOcorrValorPis;
    property NrOcorrValorCofins: Integer  read FNrOcorrValorCofins  write FNrOcorrValorCofins;
    property NrOcorrValorInss: Integer    read FNrOcorrValorInss    write FNrOcorrValorInss;
    property NrOcorrValorIr: Integer      read FNrOcorrValorIr      write FNrOcorrValorIr;
    property NrOcorrValorCsll: Integer    read FNrOcorrValorCsll    write FNrOcorrValorCsll;
    property NrOcorrValorIss: Integer     read FNrOcorrValorIss     write FNrOcorrValorIss;
    property NrOcorrBaseCalc: Integer     read FNrOcorrBaseCalc     write FNrOcorrBaseCalc;
    property NrOcorrDescIncond: Integer   read FNrOcorrDescIncond   write FNrOcorrDescIncond;
    property NrOcorrDescCond: Integer     read FNrOcorrDescCond     write FNrOcorrDescCond;
    property NrOcorrFoneTomador: Integer  read FNrOcorrFoneTomador  write FNrOcorrFoneTomador;
    property NrOcorrEmailTomador: Integer read FNrOcorrEmailTomador write FNrOcorrEmailTomador;
    property NrOcorrValLiq: Integer       read FNrOcorrValLiq       write FNrOcorrValLiq;
    property NrOcorrOptanteSN: Integer    read FNrOcorrOptanteSN    write FNrOcorrOptanteSN;
    property NrOcorrIncentCult: Integer   read FNrOcorrIncentCult   write FNrOcorrIncentCult;
    property NrOcorrCodigoCnae: Integer   read FNrOcorrCodigoCnae   write FNrOcorrCodigoCnae;
    property NrOcorrCodTribMun: Integer   read FNrOcorrCodTribMun   write FNrOcorrCodTribMun;
    property NrOcorrRespRetencao: Integer read FNrOcorrRespRetencao write FNrOcorrRespRetencao;
    property NrOcorrMunIncid: Integer     read FNrOcorrMunIncid     write FNrOcorrMunIncid;
    property NrOcorrIdCidade: Integer     read FNrOcorrIdCidade     write FNrOcorrIdCidade;
    property NrOcorrStatus: Integer       read FNrOcorrStatus       write FNrOcorrStatus;

    property NrOcorrValorDeducoes: Integer      read FNrOcorrValorDeducoes      write FNrOcorrValorDeducoes;
    property NrOcorrNaturezaOperacao: Integer   read FNrOcorrNaturezaOperacao   write FNrOcorrNaturezaOperacao;
    property NrOcorrOutrasInformacoes: Integer  read FNrOcorrOutrasInformacoes  write FNrOcorrOutrasInformacoes;
    property NrOcorrValorTotalRecebido: Integer read FNrOcorrValorTotalRecebido write FNrOcorrValorTotalRecebido;
    property NrOcorrValorISSRetido_1: Integer   read FNrOcorrValorISSRetido_1   write FNrOcorrValorISSRetido_1;
    property NrOcorrValorISSRetido_2: Integer   read FNrOcorrValorISSRetido_2   write FNrOcorrValorISSRetido_2;
    property NrOcorrInscEstTomador: Integer     read FNrOcorrInscEstTomador     write FNrOcorrInscEstTomador;
    property NrOcorrCodPaisTomador: Integer     read FNrOcorrCodPaisTomador     write FNrOcorrCodPaisTomador;
    property NrOcorrRazaoSocialInterm: Integer  read FNrOcorrRazaoSocialInterm  write FNrOcorrRazaoSocialInterm;
    property NrOcorrInscMunTomador: Integer     read FNrOcorrInscMunTomador     write FNrOcorrInscMunTomador;

    property NrOcorrRegimeEspecialTributacao: Integer read FNrOcorrRegimeEspecialTributacao write FNrOcorrRegimeEspecialTributacao;
    property NrOcorrInformacoesComplemetares: Integer read FNrOcorrInformacoesComplemetares write FNrOcorrInformacoesComplemetares;
  end;

implementation

uses
  pcnConsts,
  ACBrUtil.Strings,
  ACBrXmlBase,
  ACBrNFSeXConversao, ACBrNFSeXConsts;

//==============================================================================
// Essa unit tem por finalidade exclusiva gerar o XML do RPS dos provedores:
//     que seguem a versão 1.xx do layout da ABRASF
//==============================================================================

{ TNFSeW_ABRASFv1 }

procedure TNFSeW_ABRASFv1.Configuracao;
begin
  // Executa a Configuração Padrão
  inherited Configuracao;

  // Numero de Ocorrencias Minimas de uma tag
  // se for  0 só gera a tag se o conteudo for diferente de vazio ou zero
  // se for  1 sempre vai gerar a tag
  // se for -1 nunca gera a tag

  // Por padrão as tags abaixo são opcionais
  FNrOcorrComplTomador := 0;
  FNrOcorrFoneTomador := 0;
  FNrOcorrEmailTomador := 0;
  FNrOcorrOutrasRet := 0;
  FNrOcorrAliquota := 0;

  FNrOcorrValorPis := 0;
  FNrOcorrValorCofins := 0;
  FNrOcorrValorInss := 0;
  FNrOcorrValorIr := 0;
  FNrOcorrValorCsll := 0;
  FNrOcorrValorIss := 0;
  FNrOcorrBaseCalc := 0;
  FNrOcorrDescIncond := 0;
  FNrOcorrDescCond := 0;
  FNrOcorrValLiq := 0;
  FNrOcorrCodigoCnae := 0;
  FNrOcorrCodTribMun := 0;
  FNrOcorrMunIncid := 0;
  FNrOcorrInscMunTomador := 0;

  FNrOcorrRazaoSocialInterm := 0;
  FNrOcorrValorDeducoes := 0;
  FNrOcorrValorISSRetido_1 := 0;

  FNrOcorrRegimeEspecialTributacao := 0;

  // Por padrão as tags abaixo são obrigatórias
  FNrOcorrOptanteSN := 1;
  FNrOcorrIncentCult := 1;
  FNrOcorrStatus := 1;

  FNrOcorrNaturezaOperacao := 1;

  // Por padrão as tags abaixo não devem ser geradas
  FNrOcorrRespRetencao := -1;
  FNrOcorrIdCidade := -1;
  FNrOcorrValorISSRetido_2 := -1;
  FNrOcorrValorTotalRecebido := -1;
  FNrOcorrInscEstTomador := -1;
  FNrOcorrOutrasInformacoes := -1;
  FNrOcorrCodPaisTomador := -1;

  FNrOcorrInformacoesComplemetares := -1;
end;

function TNFSeW_ABRASFv1.GerarXml: Boolean;
var
  NFSeNode, xmlNode: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  // Configuracao;

  ListaDeAlertas.Clear;

  Opcoes.QuebraLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  FDocument.Clear();

  NFSeNode := CreateElement('Rps');

  if FpAOwner.ConfigMsgDados.XmlRps.xmlns <> '' then
    NFSeNode.SetNamespace(FpAOwner.ConfigMsgDados.XmlRps.xmlns, Self.PrefixoPadrao);

  FDocument.Root := NFSeNode;

  {
    O ConsolidarVariosItensServicosEmUmSo esta comentado pois requer varios
    testes.
  }
//  ConsolidarVariosItensServicosEmUmSo;

  xmlNode := GerarInfRps;
  NFSeNode.AppendChild(xmlNode);

  Result := True;
end;

function TNFSeW_ABRASFv1.GerarInfRps: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := CreateElement('InfRps');

  DefinirIDRps;

  if (FpAOwner.ConfigGeral.Identificador <> '') then
    Result.SetAttribute(FpAOwner.ConfigGeral.Identificador, NFSe.infID.ID);

  Result.AppendChild(GerarIdentificacaoRPS);

  Result.AppendChild(AddNode(FormatoEmissao, '#4', 'DataEmissao', 19, 19, 1,
                                                   NFSe.DataEmissao, DSC_DEMI));

  Result.AppendChild(AddNode(tcStr, '#5', 'NaturezaOperacao', 1, 3, NrOcorrNaturezaOperacao,
                   NaturezaOperacaoToStr(NFSe.NaturezaOperacao), DSC_INDNATOP));

  if (NFSe.RegimeEspecialTributacao <> retNenhum) then
    Result.AppendChild(AddNode(tcStr, '#6', 'RegimeEspecialTributacao', 1, 1, NrOcorrRegimeEspecialTributacao,
   FpAOwner.RegimeEspecialTributacaoToStr(NFSe.RegimeEspecialTributacao), DSC_REGISSQN));

  Result.AppendChild(AddNode(tcStr, '#7', 'OptanteSimplesNacional', 1, 1, NrOcorrOptanteSN,
   FpAOwner.SimNaoToStr(NFSe.OptanteSimplesNacional), DSC_INDOPSN));

  Result.AppendChild(AddNode(tcStr, '#8', 'IncentivadorCultural', 1, 1, NrOcorrIncentCult,
   FpAOwner.SimNaoToStr(NFSe.IncentivadorCultural), DSC_INDINCCULT));

  Result.AppendChild(AddNode(tcStr, '#9', 'Status', 1, 1, NrOcorrStatus,
                                   StatusRPSToStr(NFSe.StatusRps), DSC_INDSTATUS));

  Result.AppendChild(AddNode(tcStr, '#11', 'OutrasInformacoes', 1, 255, NrOcorrOutrasInformacoes,
                                        NFSe.OutrasInformacoes, DSC_OUTRASINF));

  Result.AppendChild(GerarRPSSubstituido);
  Result.AppendChild(GerarServico);
  Result.AppendChild(GerarPrestador);
  Result.AppendChild(GerarTomador);
  Result.AppendChild(GerarIntermediarioServico);
  Result.AppendChild(GerarConstrucaoCivil);
  Result.AppendChild(GerarCondicaoPagamento);
end;

function TNFSeW_ABRASFv1.GerarIdentificacaoRPS: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := CreateElement('IdentificacaoRps');

  Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 15, 1,
                         OnlyNumber(NFSe.IdentificacaoRps.Numero), DSC_NUMRPS));

  Result.AppendChild(AddNode(tcStr, '#2', 'Serie', 1, 5, 1,
                                    NFSe.IdentificacaoRps.Serie, DSC_SERIERPS));

  Result.AppendChild(AddNode(tcStr, '#3', 'Tipo', 1, 1, 1,
               FpAOwner.TipoRPSToStr(NFSe.IdentificacaoRps.Tipo), DSC_TIPORPS));
end;

function TNFSeW_ABRASFv1.GerarRPSSubstituido: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := nil;

  if NFSe.RpsSubstituido.Numero <> '' then
  begin
    Result := CreateElement('RpsSubstituido');

    Result.AppendChild(AddNode(tcStr, '#1', 'Numero', 1, 15, 1,
                        OnlyNumber(NFSe.RpsSubstituido.Numero), DSC_NUMRPSSUB));

    Result.AppendChild(AddNode(tcStr, '#2', 'Serie', 1, 5, 1,
                                   NFSe.RpsSubstituido.Serie, DSC_SERIERPSSUB));

    Result.AppendChild(AddNode(tcStr, '#3', 'Tipo', 1, 1, 1,
              FpAOwner.TipoRPSToStr(NFSe.RpsSubstituido.Tipo), DSC_TIPORPSSUB));
  end;
end;

function TNFSeW_ABRASFv1.GerarServico: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
  item: string;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := CreateElement('Servico');

  Result.AppendChild(GerarValores);

  item := FormatarItemServico(NFSe.Servico.ItemListaServico, FormatoItemListaServico);

  Result.AppendChild(AddNode(tcStr, '#29', 'ItemListaServico', 1, 5, NrOcorrItemListaServico,
                                                          item, DSC_CLISTSERV));

  Result.AppendChild(AddNode(tcStr, '#30', 'CodigoCnae', 1, 7, NrOcorrCodigoCnae,
                                OnlyNumber(NFSe.Servico.CodigoCnae), DSC_CNAE));

  Result.AppendChild(AddNode(tcStr, '#31', 'CodigoTributacaoMunicipio', 1, 20, NrOcorrCodTribMun,
                     NFSe.Servico.CodigoTributacaoMunicipio, DSC_CSERVTRIBMUN));

  Result.AppendChild(AddNode(tcStr, '#32', 'Discriminacao', 1, 2000, 1,
    StringReplace(NFSe.Servico.Discriminacao, ';', FpAOwner.ConfigGeral.QuebradeLinha,
                                     [rfReplaceAll, rfIgnoreCase]), DSC_DISCR));

  Result.AppendChild(AddNode(tcStr, '#', 'InformacoesComplementares', 1, 255, NrOcorrInformacoesComplemetares,
                                           NFSe.InformacoesComplementares, ''));

  Result.AppendChild(GerarServicoCodigoMunicipio);

  nodeArray := GerarItensServico;
  if nodeArray <> nil then
  begin
    for i := 0 to Length(nodeArray) - 1 do
    begin
      Result.AppendChild(nodeArray[i]);
    end;
  end;
end;

function TNFSeW_ABRASFv1.GerarValores: TACBrXmlNode;
var
  Aliquota: Double;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := CreateElement('Valores');

  Result.AppendChild(AddNode(tcDe2, '#13', 'ValorServicos', 1, 15, 1,
                             NFSe.Servico.Valores.ValorServicos, DSC_VSERVICO));

  Result.AppendChild(AddNode(tcDe2, '#14', 'ValorDeducoes', 1, 15, NrOcorrValorDeducoes,
                            NFSe.Servico.Valores.ValorDeducoes, DSC_VDEDUCISS));

  Result.AppendChild(AddNode(tcDe2, '#14', 'ValorTotalRecebido', 1, 15, NrOcorrValorTotalRecebido,
                         NFSe.Servico.Valores.ValorTotalRecebido, DSC_VTOTREC));

  Result.AppendChild(AddNode(tcDe2, '#15', 'ValorPis', 1, 15, NrOcorrValorPis,
                                      NFSe.Servico.Valores.ValorPis, DSC_VPIS));

  Result.AppendChild(AddNode(tcDe2, '#16', 'ValorCofins', 1, 15, NrOcorrValorCofins,
                                NFSe.Servico.Valores.ValorCofins, DSC_VCOFINS));

  Result.AppendChild(AddNode(tcDe2, '#17', 'ValorInss', 1, 15, NrOcorrValorInss,
                                    NFSe.Servico.Valores.ValorInss, DSC_VINSS));

  Result.AppendChild(AddNode(tcDe2, '#18', 'ValorIr', 1, 15, NrOcorrValorIr,
                                        NFSe.Servico.Valores.ValorIr, DSC_VIR));

  Result.AppendChild(AddNode(tcDe2, '#19', 'ValorCsll', 1, 15, NrOcorrValorCsll,
                                    NFSe.Servico.Valores.ValorCsll, DSC_VCSLL));

  Result.AppendChild(AddNode(tcStr, '#20', 'IssRetido', 1, 1, 1,
    FpAOwner.SituacaoTributariaToStr(NFSe.Servico.Valores.IssRetido), DSC_INDISSRET));

  Result.AppendChild(AddNode(tcDe2, '#21', 'ValorIss', 1, 15, NrOcorrValorIss,
                                      NFSe.Servico.Valores.ValorIss, DSC_VISS));

  Result.AppendChild(AddNode(tcDe2, '#22', 'ValorIssRetido', 1, 15, NrOcorrValorISSRetido_1,
                               NFSe.Servico.Valores.ValorIssRetido, DSC_VNFSE));

  Result.AppendChild(AddNode(tcDe2, '#23', 'OutrasRetencoes', 1, 15, NrOcorrOutrasRet,
                    NFSe.Servico.Valores.OutrasRetencoes, DSC_OUTRASRETENCOES));

  Result.AppendChild(AddNode(tcDe2, '#24', 'BaseCalculo', 1, 15, NrOcorrBaseCalc,
                                 NFSe.Servico.Valores.BaseCalculo, DSC_VBCISS));

  Aliquota := NormatizarAliquota(NFSe.Servico.Valores.Aliquota, DivAliq100);

  Result.AppendChild(AddNode(FormatoAliq, '#25', 'Aliquota', 1, 5, NrOcorrAliquota,
                                                          Aliquota, DSC_VALIQ));

  Result.AppendChild(AddNode(tcDe2, '#26', 'ValorLiquidoNfse', 1, 15, NrOcorrValLiq,
                             NFSe.Servico.Valores.ValorLiquidoNfse, DSC_VNFSE));

  Result.AppendChild(AddNode(tcDe2, '#22', 'ValorIssRetido', 1, 15, NrOcorrValorISSRetido_2,
                               NFSe.Servico.Valores.ValorIssRetido, DSC_VNFSE));

  Result.AppendChild(AddNode(tcDe2, '#27', 'DescontoIncondicionado', 1, 15, NrOcorrDescIncond,
                 NFSe.Servico.Valores.DescontoIncondicionado, DSC_VDESCINCOND));

  Result.AppendChild(AddNode(tcDe2, '#28', 'DescontoCondicionado', 1, 15, NrOcorrDescCond,
                     NFSe.Servico.Valores.DescontoCondicionado, DSC_VDESCCOND));
end;

function TNFSeW_ABRASFv1.GerarServicoCodigoMunicipio: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  // No provedor ISSNet o nome da tag é diferente do padrão
  Result := AddNode(tcStr, '#33', 'CodigoMunicipio', 1, 7, 1,
                            OnlyNumber(NFSe.Servico.CodigoMunicipio), DSC_CMUN);
end;

function TNFSeW_ABRASFv1.GerarCodigoMunicipioUF: TACBrXmlNodeArray;
begin
  SetLength(Result, 3);

  Result[0] := AddNode(tcStr, '#43', 'CodigoMunicipio', 7, 7, 0,
                   OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio), DSC_CMUN);

  Result[1] := AddNode(tcStr, '#44', 'Uf', 2, 2, 0,
                                              NFSe.Tomador.Endereco.UF, DSC_UF);

  if (OnlyNumber(NFSe.Tomador.Endereco.CodigoMunicipio) = '9999999') or
     (NrOcorrCodPaisTomador = 1) then
    Result[2] := AddNode(tcStr, '#45', 'CodigoPais', 4, 4, NrOcorrCodPaisTomador,
                                   NFSe.Tomador.Endereco.CodigoPais, DSC_CPAIS);
end;

function TNFSeW_ABRASFv1.GerarItensServico: TACBrXmlNodeArray;
begin
  // Aqui não fazer nada, pois por padrão ABRASF V1 não tem lista de serviços
  Result := nil;
end;

function TNFSeW_ABRASFv1.GerarPrestador: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := CreateElement('Prestador');

  Result.AppendChild(GerarCNPJ(NFSe.Prestador.IdentificacaoPrestador.CpfCnpj));

  Result.AppendChild(AddNode(tcStr, '#35', 'InscricaoMunicipal', 1, 15, 0,
             NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, DSC_IM));
end;

function TNFSeW_ABRASFv1.GerarTomador: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := nil;

  if (NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
     (NFSe.Tomador.RazaoSocial <> '') or
     (NFSe.Tomador.Endereco.Endereco <> '') or
     (NFSe.Tomador.Contato.Telefone <> '') or
     (NFSe.Tomador.Contato.Email <>'') then
  begin
    Result := CreateElement('Tomador');

    if ((NFSe.Tomador.Endereco.UF <> 'EX') and
        ((NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '') or
         (NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal <> ''))) then
    begin
      Result.AppendChild(GerarIdentificacaoTomador);
    end;

    Result.AppendChild(AddNode(tcStr, '#38', 'RazaoSocial', 1, 115, 0,
                                          NFSe.Tomador.RazaoSocial, DSC_XNOME));

    Result.AppendChild(GerarEnderecoTomador);
    Result.AppendChild(GerarContatoTomador);
  end;
end;

function TNFSeW_ABRASFv1.GerarIdentificacaoTomador: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := CreateElement('IdentificacaoTomador');

  if NFSe.Tomador.IdentificacaoTomador.CpfCnpj <> '' then
    Result.AppendChild(GerarCPFCNPJ(NFSe.Tomador.IdentificacaoTomador.CpfCnpj));

  Result.AppendChild(AddNode(tcStr, '#37', 'InscricaoMunicipal', 1, 15, NrOcorrInscMunTomador,
                 NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal, DSC_IM));

  Result.AppendChild(AddNode(tcStr, '#38', 'InscricaoEstadual', 1, 20, NrocorrInscEstTomador,
                  NFSe.Tomador.IdentificacaoTomador.InscricaoEstadual, DSC_IE));
end;

function TNFSeW_ABRASFv1.GerarEnderecoTomador: TACBrXmlNode;
var
  nodeArray: TACBrXmlNodeArray;
  i: Integer;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := nil;

  if (NFSe.Tomador.Endereco.Endereco <> '') or (NFSe.Tomador.Endereco.Numero <> '') or
     (NFSe.Tomador.Endereco.Bairro <> '') or (NFSe.Tomador.Endereco.CodigoMunicipio <> '') or
     (NFSe.Tomador.Endereco.UF <> '') or (NFSe.Tomador.Endereco.CEP <> '') then
  begin
    Result := CreateElement('Endereco');

    Result.AppendChild(AddNode(tcStr, '#39', 'Endereco', 1, 125, 0,
                                     NFSe.Tomador.Endereco.Endereco, DSC_XLGR));

    Result.AppendChild(AddNode(tcStr, '#40', 'Numero', 1, 10, 0,
                                        NFSe.Tomador.Endereco.Numero, DSC_NRO));

    Result.AppendChild(AddNode(tcStr, '#41', 'Complemento', 1, 60, NrOcorrComplTomador,
                                  NFSe.Tomador.Endereco.Complemento, DSC_XCPL));

    Result.AppendChild(AddNode(tcStr, '#42', 'Bairro', 1, 60, 0,
                                    NFSe.Tomador.Endereco.Bairro, DSC_XBAIRRO));

    nodeArray := GerarCodigoMunicipioUF;

    if nodeArray <> nil then
    begin
      for i := 0 to Length(nodeArray) - 1 do
      begin
        Result.AppendChild(nodeArray[i]);
      end;
    end;

    Result.AppendChild(AddNode(tcStr, '#45', 'Cep', 8, 8, 0,
                               OnlyNumber(NFSe.Tomador.Endereco.CEP), DSC_CEP));
  end;
end;

function TNFSeW_ABRASFv1.GerarContatoTomador: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := nil;

  if (NFSe.Tomador.Contato.Telefone <> '') or (NFSe.Tomador.Contato.Email <> '') then
  begin
    Result := CreateElement('Contato');

    Result.AppendChild(AddNode(tcStr, '#46', 'Telefone', 1, 11, NrOcorrFoneTomador,
                          OnlyNumber(NFSe.Tomador.Contato.Telefone), DSC_FONE));

    Result.AppendChild(AddNode(tcStr, '#47', 'Email', 1, 80, NrOcorrEmailTomador,
                                        NFSe.Tomador.Contato.Email, DSC_EMAIL));
  end;
end;

function TNFSeW_ABRASFv1.GerarIntermediarioServico: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := nil;

  if (NFSe.Intermediario.RazaoSocial <> '') or
     (NFSe.Intermediario.Identificacao.CpfCnpj <> '') then
  begin
    Result := CreateElement('IntermediarioServico');

    Result.AppendChild(AddNode(tcStr, '#48', 'RazaoSocial', 1, 115, 1,
                                    NFSe.Intermediario.RazaoSocial, DSC_XNOME));

    Result.AppendChild(GerarCPFCNPJ(NFSe.Intermediario.Identificacao.CpfCnpj));

    Result.AppendChild(AddNode(tcStr, '#50', 'InscricaoMunicipal', 1, 15, 0,
                  NFSe.Intermediario.Identificacao.InscricaoMunicipal, DSC_IM));
  end;
end;

function TNFSeW_ABRASFv1.GerarConstrucaoCivil: TACBrXmlNode;
begin
  // Em conformidade com a versão 1 do layout da ABRASF não deve ser alterado
  Result := nil;

  if (NFSe.ConstrucaoCivil.CodigoObra <> '') then
  begin
    Result := CreateElement('ConstrucaoCivil');

    Result.AppendChild(AddNode(tcStr, '#51', 'CodigoObra', 1, 15, 1,
                                   NFSe.ConstrucaoCivil.CodigoObra, DSC_COBRA));

    Result.AppendChild(AddNode(tcStr, '#52', 'Art', 1, 15, 1,
                                            NFSe.ConstrucaoCivil.Art, DSC_ART));
  end;
end;

function TNFSeW_ABRASFv1.GerarCondicaoPagamento: TACBrXmlNode;
begin
  // Aqui não fazer nada
  Result := nil;
end;

function TNFSeW_ABRASFv1.GerarParcelas: TACBrXmlNodeArray;
begin
  // Aqui não fazer nada
  Result := nil;
end;

end.
