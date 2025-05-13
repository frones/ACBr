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

unit PadraoNacional.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  IniFiles,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXClass,
  ACBrNFSeXConversao, ACBrNFSeXLerXml;

type
  { TNFSeR_PadraoNacional }

  TNFSeR_PadraoNacional = class(TNFSeRClass)
  protected
    FpTipoXML: string;

    procedure Ler_infNFSe(const ANode: TACBrXmlNode);
    procedure Ler_Emitente(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoEmitente(const ANode: TACBrXmlNode);

    procedure Ler_ValoresNFSe(const ANode: TACBrXmlNode);
    procedure Ler_DPS(const ANode: TACBrXmlNode);

    procedure Ler_infDPS(const ANode: TACBrXmlNode);
    procedure Ler_Substituicao(const ANode: TACBrXmlNode);

    procedure Ler_Prestador(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoPrestador(const ANode: TACBrXmlNode);
    procedure Ler_RegimeTributacaoPrestador(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoNacionalPrestador(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoExteriorPrestador(const ANode: TACBrXmlNode);

    procedure Ler_Tomador(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoTomador(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoNacionalTomador(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoExteriorTomador(const ANode: TACBrXmlNode);

    procedure Ler_Intermediario(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoItermediario(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoNacionalIntermediario(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoExteriorIntermediario(const ANode: TACBrXmlNode);

    procedure Ler_Servico(const ANode: TACBrXmlNode);
    procedure Ler_LocalPrestacao(const ANode: TACBrXmlNode);
    procedure Ler_CodigoServico(const ANode: TACBrXmlNode);
    procedure Ler_ComercioExterior(const ANode: TACBrXmlNode);
    procedure Ler_LocacaoSubLocacao(const ANode: TACBrXmlNode);
    procedure Ler_Obra(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoObra(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoExteriorObra(const ANode: TACBrXmlNode);
    procedure Ler_AtividadeEvento(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoEvento(const ANode: TACBrXmlNode);
    procedure Ler_EnderecoExteriorEvento(const ANode: TACBrXmlNode);
    procedure Ler_ExploracaoRodoviaria(const ANode: TACBrXmlNode);
    procedure Ler_InformacoesComplementares(const ANode: TACBrXmlNode);

    procedure Ler_Valores(const ANode: TACBrXmlNode);
    procedure Ler_ServicoPrestado(const ANode: TACBrXmlNode);
    procedure Ler_Descontos(const ANode: TACBrXmlNode);
    procedure Ler_Deducoes(const ANode: TACBrXmlNode);
    procedure Ler_DocDeducoes(const ANode: TACBrXmlNode);
    procedure Ler_NFSeMunicipio(const ANode: TACBrXmlNode; Item: Integer);
    procedure Ler_NFNFS(const ANode: TACBrXmlNode; Item: Integer);
    procedure Ler_Fornecedor(const ANode: TACBrXmlNode; Item: Integer);
    procedure Ler_EnderecoFornecedor(const ANode: TACBrXmlNode; Item: Integer);
    procedure Ler_EnderecoNacionalFornecedor(const ANode: TACBrXmlNode; Item: Integer);
    procedure Ler_EnderecoExteriorFornecedr(const ANode: TACBrXmlNode; Item: Integer);

    procedure Ler_Tributacao(const ANode: TACBrXmlNode);
    procedure Ler_TributacaoMunicipal(const ANode: TACBrXmlNode);
    procedure Ler_BeneficioMunicipal(const ANode: TACBrXmlNode);
    procedure Ler_ExigibilidadeSuspensa(const ANode: TACBrXmlNode);
    procedure Ler_TributacaoFederal(const ANode: TACBrXmlNode);
    procedure Ler_TributacaoOutrosPisCofins(const ANode: TACBrXmlNode);
    procedure Ler_TotalTributos(const ANode: TACBrXmlNode);
    procedure Ler_ValorTotalTributos(const ANode: TACBrXmlNode);
    procedure Ler_PercentualTotalTributos(const ANode: TACBrXmlNode);

    // Reforma Tributária
    procedure Ler_IBSCBSDPS(const ANode: TACBrXmlNode; IBSCBS: TIBSCBSDPS);

    procedure Ler_Destinatario(const ANode: TACBrXmlNode; Dest: TDadosdaPessoa);
    procedure Ler_EnderecoDestinatario(const ANode: TACBrXmlNode; ender: Tender);
    procedure Ler_EnderecoNacionalDestinatario(const ANode: TACBrXmlNode; endNac: TendNac);
    procedure Ler_EnderecoExteriorDestinatario(const ANode: TACBrXmlNode; endExt: TendExt);

    procedure Ler_Adquirente(const ANode: TACBrXmlNode; Adq: TDadosdaPessoa);
    procedure Ler_EnderecoAdquirente(const ANode: TACBrXmlNode; ender: Tender);
    procedure Ler_EnderecoNacionalAdquirente(const ANode: TACBrXmlNode; endNac: TendNac);
    procedure Ler_EnderecoExteriorAdquirente(const ANode: TACBrXmlNode; endExt: TendExt);

    procedure Ler_ServicoIBSCBS(const ANode: TACBrXmlNode; serv: Tserv);
    procedure Ler_gCompraGov(const ANode: TACBrXmlNode; gCompraGov: TgCompraGov);

    procedure Ler_ValoresIBSCBS(const ANode: TACBrXmlNode; valores: Tvalorestrib);
    procedure Ler_Tributos(const ANode: TACBrXmlNode; trib: Ttrib);

    procedure Ler_gIBSCBS(const ANode: TACBrXmlNode; gIBSCBS: TgIBSCBS);

    procedure Ler_gIBSCredPres(const ANode: TACBrXmlNode; gIBSCredPres: TgIBSCredPres);

    procedure Ler_gIBSUF(const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
    procedure Ler_gDifUF(const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
    procedure Ler_gDevTribUF(const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
    procedure Ler_gDesonUF(const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);

    procedure Ler_gIBSMun(const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
    procedure Ler_gDifMun(const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
    procedure Ler_gDevTribMun(const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
    procedure Ler_gDesonMun(const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);

    procedure Ler_gCBS(const ANode: TACBrXmlNode; gCBS: TgCBSValores);
    procedure Ler_gCBSCredPres(const ANode: TACBrXmlNode; gCBS: TgCBSValores);
    procedure Ler_gDifCBS(const ANode: TACBrXmlNode; gCBS: TgCBSValores);
    procedure Ler_gDevTribCBS(const ANode: TACBrXmlNode; gCBS: TgCBSValores);
    procedure Ler_gDesonCBS(const ANode: TACBrXmlNode; gCBS: TgCBSValores);

    procedure Ler_IBSCBSNFSe(const ANode: TACBrXmlNode; IBSCBS: TIBSCBSNfse);
    procedure Ler_CompGov(const ANode: TACBrXmlNode; CompGov: TCompGov);
    procedure Ler_ValoresIBSCBSNFSe(const ANode: TACBrXmlNode; valores: TvaloresIBSCBS);

    procedure Ler_TotCIBS(const ANode: TACBrXmlNode; totCIBS: TtotCIBS);

    procedure Ler_TotgIBS(const ANode: TACBrXmlNode; gIBS: TgIBS);
    procedure Ler_TotgIBSUFTot(const ANode: TACBrXmlNode; gIBS: TgIBS);
    procedure Ler_TotgIBSMunTot(const ANode: TACBrXmlNode; gIBS: TgIBS);

    procedure Ler_TotgCBS(const ANode: TACBrXmlNode; gCBS: TgCBS);

    //====== Ler o Arquivo INI===========================================
    procedure LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
    procedure LerINIIdentificacaoRps(AINIRec: TMemIniFile);
    procedure LerININFSeSubstituicao(AINIRec: TMemIniFile);
    procedure LerINIDadosEmitente(AINIRec: TMemIniFile);
    procedure LerINIValoresNFSe(AINIRec: TMemIniFile);

    procedure LerINIDadosPrestador(AINIRec: TMemIniFile);
    procedure LerINIDadosTomador(AINIRec: TMemIniFile);
    procedure LerINIDadosIntermediario(AINIRec: TMemIniFile);
    procedure LerINIConstrucaoCivil(AINIRec: TMemIniFile);
    procedure LerINIDadosServico(AINIRec: TMemIniFile);
    procedure LerINIComercioExterior(AINIRec: TMemIniFile);
    procedure LerINILocacaoSubLocacao(AINIRec: TMemIniFile);
    procedure LerINIEvento(AINIRec: TMemIniFile);
    procedure LerINIRodoviaria(AINIRec: TMemIniFile);
    procedure LerINIInformacoesComplementares(AINIRec: TMemIniFile);
    procedure LerINIValores(AINIRec: TMemIniFile);
    procedure LerINIDocumentosDeducoes(AINIRec: TMemIniFile);
    procedure LerINIDocumentosDeducoesFornecedor(AINIRec: TMemIniFile;
      fornec: TInfoPessoa; Indice: Integer);
    procedure LerINIValoresTribMun(AINIRec: TMemIniFile);
    procedure LerINIValoresTribFederal(AINIRec: TMemIniFile);
    procedure LerINIValoresTotalTrib(AINIRec: TMemIniFile);
    // Reforma Tributária
    procedure LerINIDestinatario(AINIRec: TMemIniFile; Dest: TDadosdaPessoa);
    procedure LerINIAdquirente(AINIRec: TMemIniFile; Adq: TDadosdaPessoa);
    procedure LerINIServicoIBSCBS(AINIRec: TMemIniFile; serv: Tserv);
    procedure LerINIgIBSCBS(AINIRec: TMemIniFile; gIBSCBS: TgIBSCBS);
    procedure LerINIgIBSCredPres(AINIRec: TMemIniFile; gIBSCredPres: TgIBSCredPres);
    procedure LerINIgIBSUF(AINIRec: TMemIniFile; gIBSUF: TgIBSUFValores);
    procedure LerINIgIBSMun(AINIRec: TMemIniFile; gIBSMun: TgIBSMunValores);
    procedure LerINIgCBS(AINIRec: TMemIniFile; gCBS: TgCBSValores);
    {

    procedure LerINIIBSCBSNFSe(AINIRec: TMemIniFile; IBSCBS: TIBSCBSNfse);
    procedure LerINICompGov(AINIRec: TMemIniFile; CompGov: TCompGov);
    procedure LerINIValoresIBSCBSNFSe(AINIRec: TMemIniFile; valores: TvaloresIBSCBS);

    procedure LerINITotCIBS(AINIRec: TMemIniFile; totCIBS: TtotCIBS);

    procedure LerINITotgIBS(AINIRec: TMemIniFile; gIBS: TgIBS);
    procedure LerINITotgIBSUFTotAINIRec: TMemIniFile; gIBS: TgIBS);
    procedure LerINITotgIBSMunTot(AINIRec: TMemIniFile; gIBS: TgIBS);

    procedure LerINITotgCBS(AINIRec: TMemIniFile; gCBS: TgCBS);
    }
    procedure LerIniRps(AINIRec: TMemIniFile);
    procedure LerIniNfse(AINIRec: TMemIniFile);
  public
    function LerXml: Boolean; override;
    function LerXmlRps(const ANode: TACBrXmlNode): Boolean;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;

    function LerIni: Boolean; override;
  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.XMLHTML,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrDFeUtil;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     PadraoNacional
//==============================================================================

{ TNFSeR_PadraoNacional }

procedure TNFSeR_PadraoNacional.Ler_AtividadeEvento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('atvEvento');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Evento do
    begin
      xNome := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);
      dtIni := ObterConteudo(AuxNode.Childrens.FindAnyNs('dtIni'), tcDat);
      dtFim := ObterConteudo(AuxNode.Childrens.FindAnyNs('dtFim'), tcDat);
      idAtvEvt := ObterConteudo(AuxNode.Childrens.FindAnyNs('idAtvEvt'), tcStr);

      Ler_EnderecoEvento(AuxNode);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_BeneficioMunicipal(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('BM');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribMun do
    begin
      tpBM := StrTotpBM(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpBM'), tcStr));
      nBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('nBM'), tcStr);
      vRedBCBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRedBCBM'), tcDe2);
      pRedBCBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRedBCBM'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_CodigoServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('cServ');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      ItemListaServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('cTribNac'), tcStr);

      if NFSe.infNFSe.xTribNac = '' then
        xItemListaServico := ItemListaServicoDescricao(ItemListaServico)
      else
        xItemListaServico := NFSe.infNFSe.xTribNac;

      CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cTribMun'), tcStr);
      Discriminacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('xDescServ'), tcStr);
      Discriminacao := StringReplace(Discriminacao, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);

      VerificarSeConteudoEhLista(Discriminacao);

      CodigoNBS := ObterConteudo(AuxNode.Childrens.FindAnyNs('cNBS'), tcStr);
      CodigoInterContr := ObterConteudo(AuxNode.Childrens.FindAnyNs('cIntContrib'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_ComercioExterior(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('comExt');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.comExt do
    begin
      mdPrestacao := StrTomdPrestacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('mdPrestacao'), tcStr));
      vincPrest := StrTovincPrest(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('vincPrest'), tcStr));
      tpMoeda := ObterConteudo(AuxNode.Childrens.FindAnyNs('tpMoeda'), tcInt);
      vServMoeda := ObterConteudo(AuxNode.Childrens.FindAnyNs('vServMoeda'), tcDe2);
      mecAFComexP := StrTomecAFComexP(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('mecAFComexP'), tcStr));
      mecAFComexT := StrTomecAFComexT(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('mecAFComexT'), tcStr));
      movTempBens := StrTomovTempBens(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('movTempBens'), tcStr));
      nDI := ObterConteudo(AuxNode.Childrens.FindAnyNs('nDI'), tcStr);
      nRE := ObterConteudo(AuxNode.Childrens.FindAnyNs('nRE'), tcStr);
      mdic := ObterConteudo(AuxNode.Childrens.FindAnyNs('mdic'), tcInt);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Deducoes(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('vDedRed');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      AliquotaDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('pDR'), tcDe2);
      ValorDeducoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('vDR'), tcDe2);

      Ler_DocDeducoes(AuxNode);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Descontos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('vDescCondIncond');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      DescontoIncondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('vDescIncond'), tcDe2);
      DescontoCondicionado := ObterConteudo(AuxNode.Childrens.FindAnyNs('vDescCond'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_DocDeducoes(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('documentos');

  if AuxNode <> nil then
  begin
    ANodes := AuxNode.Childrens.FindAllAnyNs('docDedRed');

    for i := 0 to Length(ANodes) - 1 do
    begin
      NFSe.Servico.Valores.DocDeducao.New;
      with NFSe.Servico.Valores.DocDeducao[i] do
      begin
        chNFSe := ObterConteudo(ANodes[i].Childrens.FindAnyNs('chNFSe'), tcStr);
        chNFe := ObterConteudo(ANodes[i].Childrens.FindAnyNs('chNFe'), tcStr);

        Ler_NFSeMunicipio(ANodes[i].Childrens.FindAnyNs('NFSeMun'), I);
        Ler_NFNFS(ANodes[i].Childrens.FindAnyNs('NFNFS'), I);

        nDocFisc := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nDocFisc'), tcStr);
        nDoc := ObterConteudo(ANodes[i].Childrens.FindAnyNs('nDoc'), tcStr);
        tpDedRed := StrTotpDedRed(Ok, ObterConteudo(ANodes[i].Childrens.FindAnyNs('tpDedRed'), tcStr));
        xDescOutDed := ObterConteudo(ANodes[i].Childrens.FindAnyNs('xDescOutDed'), tcStr);
        dtEmiDoc := ObterConteudo(ANodes[i].Childrens.FindAnyNs('dtEmiDoc'), tcDat);
        vDedutivelRedutivel := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vDedutivelRedutivel'), tcDe2);
        vDeducaoReducao := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vDeducaoReducao'), tcDe2);

        Ler_Fornecedor(ANodes[i].Childrens.FindAnyNs('fornec'), I);
      end;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_DPS(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('DPS');

  if AuxNode <> nil then
    Ler_infDPS(AuxNode);
end;

procedure TNFSeR_PadraoNacional.Ler_Emitente(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('emit');

  if AuxNode <> nil then
  begin
    with NFSe.infNFSe.emit do
    begin
      Identificacao.CpfCnpj := ObterCNPJCPF(AuxNode);
      Identificacao.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);

      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);
      NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('xFant'), tcStr);

      Ler_EnderecoEmitente(AuxNode);

      Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
    end;

    with NFSe.Prestador do
    begin
      IdentificacaoPrestador.CpfCnpj := NFSe.infNFSe.emit.Identificacao.CpfCnpj;
      IdentificacaoPrestador.InscricaoMunicipal := NFSe.infNFSe.emit.Identificacao.InscricaoMunicipal;

      RazaoSocial := NFSe.infNFSe.emit.RazaoSocial;
      NomeFantasia := NFSe.infNFSe.emit.NomeFantasia;

      Endereco.Endereco := NFSe.infNFSe.emit.Endereco.Endereco;
      Endereco.Numero := NFSe.infNFSe.emit.Endereco.Numero;
      Endereco.Complemento := NFSe.infNFSe.emit.Endereco.Complemento;
      Endereco.Bairro := NFSe.infNFSe.emit.Endereco.Bairro;
      Endereco.UF := NFSe.infNFSe.emit.Endereco.UF;
      Endereco.CEP := NFSe.infNFSe.emit.Endereco.CEP;
      Endereco.CodigoMunicipio := NFSe.infNFSe.emit.Endereco.CodigoMunicipio;
      Endereco.xMunicipio := NFSe.infNFSe.emit.Endereco.xMunicipio;

      Contato.Telefone := NFSe.infNFSe.emit.Contato.Telefone;
      Contato.Email := NFSe.infNFSe.emit.Contato.Email;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoEmitente(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('enderNac');

  if AuxNode <> nil then
  begin
    with NFSe.infNFSe.emit.Endereco do
    begin
      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoEvento(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Evento.Endereco do
    begin
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);

      Ler_EnderecoExteriorEvento(AuxNode);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoExteriorEvento(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Evento.Endereco do
    begin
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoExteriorFornecedr(
  const ANode: TACBrXmlNode; Item: Integer);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[Item].fornec.Endereco do
    begin
      CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcStr));
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoExteriorIntermediario(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Intermediario.Endereco do
    begin
      CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcStr));
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoExteriorObra(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoExteriorPrestador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      if CodigoPais = 0 then
        CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcStr));

      if CEP = '' then
        CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);

      if xMunicipio = '' then
        xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);

      if UF = '' then
        UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoExteriorTomador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endExt');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPais'), tcStr));
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cEndPost'), tcStr);
      xMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCidade'), tcStr);
      UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoFornecedor(const ANode: TACBrXmlNode;
  Item: Integer);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].fornec.Endereco do
    begin
      Ler_EnderecoNacionalFornecedor(AuxNode, Item);
      Ler_EnderecoExteriorFornecedr(AuxNode, Item);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoItermediario(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Intermediario.Endereco do
    begin
      Ler_EnderecoNacionalIntermediario(AuxNode);
      Ler_EnderecoExteriorIntermediario(AuxNode);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoNacionalFornecedor(
  const ANode: TACBrXmlNode; Item: Integer);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endNac');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].fornec.Endereco do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoNacionalIntermediario(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endNac');

  if AuxNode <> nil then
  begin
    with NFSe.Intermediario.Endereco do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoNacionalPrestador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endNac');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      if CodigoMunicipio = '' then
        CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);

      if CEP = '' then
        CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);

      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoNacionalTomador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('endNac');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcStr);
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);
      xMunicipio := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);

      if UF = '' then
        UF := xUF;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoObra(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.ConstrucaoCivil.Endereco do
    begin
      CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcStr);

      Ler_EnderecoExteriorObra(AuxNode);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador.Endereco do
    begin
      Ler_EnderecoNacionalPrestador(AuxNode);
      Ler_EnderecoExteriorPrestador(AuxNode);

      if Endereco = '' then
        Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);

      if Numero = '' then
        Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);

      if Complemento = '' then
        Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);

      if Bairro = '' then
        Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('end');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.Endereco do
    begin
      Ler_EnderecoNacionalTomador(AuxNode);
      Ler_EnderecoExteriorTomador(AuxNode);

      Endereco := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
      Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
      Complemento := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
      Bairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_ExigibilidadeSuspensa(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('exigSusp');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribMun do
    begin
      tpSusp := StrTotpSusp(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpSusp'), tcStr));
      nProcesso := ObterConteudo(AuxNode.Childrens.FindAnyNs('nProcesso'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_ExploracaoRodoviaria(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('explRod');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.explRod do
    begin
      categVeic := StrTocategVeic(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('categVeic'), tcStr));
      nEixos := ObterConteudo(AuxNode.Childrens.FindAnyNs('nEixos'), tcInt);
      rodagem := ObterConteudo(AuxNode.Childrens.FindAnyNs('rodagem'), tcStr);
      sentido := ObterConteudo(AuxNode.Childrens.FindAnyNs('sentido'), tcStr);
      placa := ObterConteudo(AuxNode.Childrens.FindAnyNs('placa'), tcStr);
      codAcessoPed := ObterConteudo(AuxNode.Childrens.FindAnyNs('codAcessoPed'), tcStr);
      codContrato := ObterConteudo(AuxNode.Childrens.FindAnyNs('codContrato'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Fornecedor(const ANode: TACBrXmlNode;
  Item: Integer);
var
  Ok: Boolean;
begin
  if ANode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].fornec do
    begin
      Identificacao.CpfCnpj := ObterCNPJCPF(ANode);

      if Identificacao.CpfCnpj = '' then
        Identificacao.Nif := ObterConteudo(ANode.Childrens.FindAnyNs('NIF'), tcStr);

      if Identificacao.Nif = '' then
        Identificacao.cNaoNIF := StrToNaoNIF(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('cNaoNIF'), tcStr));

      Identificacao.CAEPF := ObterConteudo(ANode.Childrens.FindAnyNs('CAEPF'), tcStr);
      Identificacao.InscricaoMunicipal := ObterConteudo(ANode.Childrens.FindAnyNs('IM'), tcStr);

      RazaoSocial := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);

      Ler_EnderecoFornecedor(ANode, Item);

      Contato.Telefone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
      Contato.Email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_infDPS(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('infDPS');

  if AuxNode <> nil then
  begin
    NFSe.infID.ID := OnlyNumber(ObterConteudoTag(AuxNode.Attributes.Items['Id']));
    NFSe.DataEmissao := ObterConteudo(AuxNode.Childrens.FindAnyNs('dhEmi'), tcDatHor);
    NFSe.verAplic := ObterConteudo(AuxNode.Childrens.FindAnyNs('verAplic'), tcStr);
    NFSe.IdentificacaoRps.Serie := ObterConteudo(AuxNode.Childrens.FindAnyNs('serie'), tcStr);
    NFSe.IdentificacaoRps.Numero := ObterConteudo(AuxNode.Childrens.FindAnyNs('nDPS'), tcStr);
    NFSe.Competencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('dCompet'), tcDat);
    NFSe.tpEmit := StrTotpEmit(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpEmit'), tcStr));

    Ler_Substituicao(AuxNode);
    Ler_Prestador(AuxNode);
    Ler_Tomador(AuxNode);
    Ler_Intermediario(AuxNode);
    Ler_Servico(AuxNode);
    Ler_Valores(AuxNode);

    // Reforma Tributária
    Ler_IBSCBSDPS(AuxNode.Childrens.FindAnyNs('IBSCBS'), NFSe.IBSCBS);
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_infNFSe(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('infNFSe');

  if AuxNode <> nil then
  begin
    {
    A formação do identificador de 53 posições da NFS é:

    "NFS" +
    Cód.Mun. (7) +
    Amb.Ger. (1) +
    Tipo de Inscrição Federal (1) +
    Inscrição Federal (14 - CPF completar com 000 à esquerda) +
    nNFSe (13) +
    AnoMes Emis. da DPS (4) +
    Cód.Num. (9) +
    DV (1)

    Código numérico de 9 Posições numérico, aleatório,
    gerado automaticamente pelo sistema gerador da NFS-e.
    }

    NFSe.infNFSe.ID := OnlyNumber(ObterConteudoTag(AuxNode.Attributes.Items['Id']));
    NFSe.infNFSe.xLocEmi := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLocEmi'), tcStr);
    NFSe.infNFSe.xLocPrestacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLocPrestacao'), tcStr);
    NFSe.infNFSe.nNFSe := ObterConteudo(AuxNode.Childrens.FindAnyNs('nNFSe'), tcStr);
    NFSe.infNFSe.cLocIncid := ObterConteudo(AuxNode.Childrens.FindAnyNs('cLocIncid'), tcInt);
    NFSe.infNFSe.xLocIncid := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLocIncid'), tcStr);
    NFSe.infNFSe.xTribNac := ObterConteudo(AuxNode.Childrens.FindAnyNs('xTribNac'), tcStr);
    NFSe.infNFSe.xTribMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('xTribMun'), tcStr);
    NFSe.infNFSe.xNBS := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNBS'), tcStr);
    NFSe.infNFSe.verAplic := ObterConteudo(AuxNode.Childrens.FindAnyNs('verAplic'), tcStr);
    NFSe.infNFSe.ambGer := StrToambGer(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('ambGer'), tcStr));
    NFSe.infNFSe.tpEmis := StrTotpEmis(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpEmis'), tcStr));
    NFSe.infNFSe.procEmi := StrToprocEmi(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('procEmi'), tcStr));
    NFSe.infNFSe.cStat := ObterConteudo(AuxNode.Childrens.FindAnyNs('cStat'), tcInt);
    NFSe.infNFSe.dhProc := ObterConteudo(AuxNode.Childrens.FindAnyNs('dhProc'), tcDatHor);
    NFSe.infNFSe.nDFSe := ObterConteudo(AuxNode.Childrens.FindAnyNs('nDFSe'), tcStr);

    NFSe.Servico.MunicipioIncidencia := NFSe.infNFSe.cLocIncid;
    NFSe.Servico.xMunicipioIncidencia := NFSe.infNFSe.xLocIncid;

    Ler_Emitente(AuxNode);
    Ler_ValoresNFSe(AuxNode);
    Ler_DPS(AuxNode);

    NFSe.Numero := NFSe.infNFSe.nNFSe;
    NFSe.CodigoVerificacao := NFSe.infNFSe.ID;

    with NFSe.Servico.Valores do
    begin
      BaseCalculo := ValorServicos - ValorDeducoes - DescontoIncondicionado;

      if tribFed.tpRetPisCofins = trpcNaoRetido then
         RetencoesFederais := ValorInss + ValorIr + ValorCsll
      else
         RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;

      ValorLiquidoNfse := ValorServicos - RetencoesFederais - OutrasRetencoes -
                 ValorIssRetido - DescontoIncondicionado - DescontoCondicionado;

      ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                              DescontoIncondicionado;
    end;

    // Reforma Tributária
    Ler_IBSCBSNFSe(AuxNode.Childrens.FindAnyNs('IBSCBS'), NFSe.infNFSe.IBSCBS);
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_InformacoesComplementares(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('infoCompl');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.infoCompl do
    begin
      idDocTec := ObterConteudo(AuxNode.Childrens.FindAnyNs('idDocTec'), tcStr);
      docRef := ObterConteudo(AuxNode.Childrens.FindAnyNs('docRef'), tcStr);
      xInfComp := ObterConteudo(AuxNode.Childrens.FindAnyNs('xInfComp'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Intermediario(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('interm');

  if AuxNode <> nil then
  begin
    with NFSe.Intermediario do
    begin
      Identificacao.CpfCnpj := ObterCNPJCPF(AuxNode);

      if Identificacao.CpfCnpj = '' then
        Identificacao.Nif := ObterConteudo(AuxNode.Childrens.FindAnyNs('NIF'), tcStr);

      if Identificacao.Nif = '' then
        Identificacao.cNaoNIF := StrToNaoNIF(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('cNaoNIF'), tcStr));

      Identificacao.CAEPF := ObterConteudo(AuxNode.Childrens.FindAnyNs('CAEPF'), tcStr);
      Identificacao.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);

      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);

      Ler_EnderecoItermediario(AuxNode);

      Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_LocacaoSubLocacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('lsadppu');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Locacao do
    begin
      categ := StrTocateg(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('categ'), tcStr));
      objeto := StrToobjeto(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('objeto'), tcStr));
      extensao := ObterConteudo(AuxNode.Childrens.FindAnyNs('extensao'), tcStr);
      nPostes := ObterConteudo(AuxNode.Childrens.FindAnyNs('nPostes'), tcInt);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_LocalPrestacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  xUF: string;
begin
  AuxNode := ANode.Childrens.FindAnyNs('locPrest');

  if AuxNode <> nil then
  begin
    with NFSe.Servico do
    begin
      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('cLocPrestacao'), tcStr);
      CodigoPais := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPaisPrestacao'), tcStr));

      MunicipioPrestacaoServico := ObterNomeMunicipioUF(StrToIntDef(CodigoMunicipio, 0), xUF);
      MunicipioPrestacaoServico := MunicipioPrestacaoServico + '/' + xUF;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_NFNFS(const ANode: TACBrXmlNode;
  Item: Integer);
begin
  if ANode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].NFNFS do
    begin
      nNFS := ObterConteudo(ANode.Childrens.FindAnyNs('nNFS'), tcStr);
      modNFS := ObterConteudo(ANode.Childrens.FindAnyNs('modNFS'), tcStr);
      serieNFS := ObterConteudo(ANode.Childrens.FindAnyNs('serieNFS'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_NFSeMunicipio(
  const ANode: TACBrXmlNode; Item: Integer);
begin
  if ANode <> nil then
  begin
    with NFSe.Servico.Valores.DocDeducao.Items[item].NFSeMun do
    begin
      cMunNFSeMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMunNFSeMun'), tcStr);
      nNFSeMun := ObterConteudo(ANode.Childrens.FindAnyNs('nNFSeMun'), tcStr);
      cVerifNFSeMun := ObterConteudo(ANode.Childrens.FindAnyNs('cVerifNFSeMun'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Obra(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('obra');

  if AuxNode <> nil then
  begin
    with NFSe.ConstrucaoCivil do
    begin
      CodigoObra := ObterConteudo(AuxNode.Childrens.FindAnyNs('cObra'), tcStr);
      inscImobFisc := ObterConteudo(AuxNode.Childrens.FindAnyNs('inscImobFisc'), tcStr);

      Ler_EnderecoObra(AuxNode);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_PercentualTotalTributos(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('pTotTrib');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.totTrib do
    begin
      pTotTribFed := ObterConteudo(AuxNode.Childrens.FindAnyNs('pTotTribFed'), tcDe2);
      pTotTribEst := ObterConteudo(AuxNode.Childrens.FindAnyNs('pTotTribEst'), tcDe2);
      pTotTribMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('pTotTribMun'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Prestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('prest');

  if AuxNode <> nil then
  begin
    with NFSe.Prestador do
    begin
      if IdentificacaoPrestador.CpfCnpj = '' then
        IdentificacaoPrestador.CpfCnpj := ObterCNPJCPF(AuxNode);

      if IdentificacaoPrestador.CpfCnpj = '' then
        IdentificacaoPrestador.Nif := ObterConteudo(AuxNode.Childrens.FindAnyNs('NIF'), tcStr);

      if IdentificacaoPrestador.Nif = '' then
        IdentificacaoPrestador.cNaoNIF := StrToNaoNIF(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('cNaoNIF'), tcStr));

      IdentificacaoPrestador.CAEPF := ObterConteudo(AuxNode.Childrens.FindAnyNs('CAEPF'), tcStr);

      if IdentificacaoPrestador.InscricaoMunicipal = '' then
        IdentificacaoPrestador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);

      if RazaoSocial = '' then
        RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);

      Ler_EnderecoPrestador(AuxNode);

      if Contato.Telefone = '' then
        Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);

      if Contato.Email = '' then
      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);

      Ler_RegimeTributacaoPrestador(AuxNode);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_RegimeTributacaoPrestador(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('regTrib');

  if AuxNode <> nil then
  begin
    NFSe.OptanteSN := StrToOptanteSN(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('opSimpNac'), tcStr));
    NFSe.RegimeApuracaoSN := StrToRegimeApuracaoSN(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('regApTribSN'), tcStr));
    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('regEspTrib'), tcStr));
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Servico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('serv');

  if AuxNode <> nil then
  begin
    Ler_LocalPrestacao(AuxNode);
    Ler_CodigoServico(AuxNode);
    Ler_ComercioExterior(AuxNode);
    Ler_LocacaoSubLocacao(AuxNode);
    Ler_Obra(AuxNode);
    Ler_AtividadeEvento(AuxNode);
    Ler_ExploracaoRodoviaria(AuxNode);
    Ler_InformacoesComplementares(AuxNode);
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_ServicoPrestado(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('vServPrest');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorRecebido := ObterConteudo(AuxNode.Childrens.FindAnyNs('vReceb'), tcDe2);
      ValorServicos := ObterConteudo(AuxNode.Childrens.FindAnyNs('vServ'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Substituicao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('subst');

  if AuxNode <> nil then
  begin
      {
      A formação da chSubstda de 50 posições da NFS é:

      Cód.Mun. (7) +
      Amb.Ger. (1) +
      Tipo de Inscrição Federal (1) +
      Inscrição Federal (14 - CPF completar com 000 à esquerda) +
      nNFSe (13) +
      AnoMes Emis. da DPS (4) +
      Cód.Num. (9) +
      DV (1)

      Código numérico de 9 Posições numérico, aleatório,
      gerado automaticamente pelo sistema gerador da NFS-e.
      }
    NFSe.subst.chSubstda := ObterConteudo(AuxNode.Childrens.FindAnyNs('chSubstda'), tcStr);
    NFSe.subst.cMotivo := StrTocMotivo(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('cMotivo'), tcStr));
    NFSe.subst.xMotivo := ObterConteudo(AuxNode.Childrens.FindAnyNs('xMotivo'), tcStr);

    NFSe.NfseSubstituida := Copy(NFSe.subst.chSubstda, 24, 13);
    NFSe.OutrasInformacoes := NFSe.OutrasInformacoes + sLineBreak +
      'Chave da NFSe Substituida: ' + NFSe.subst.chSubstda;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Tomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('toma');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador do
    begin
      IdentificacaoTomador.CpfCnpj := ObterCNPJCPF(AuxNode);

      if IdentificacaoTomador.CpfCnpj = '' then
        IdentificacaoTomador.Nif := ObterConteudo(AuxNode.Childrens.FindAnyNs('NIF'), tcStr);

      if IdentificacaoTomador.Nif = '' then
        IdentificacaoTomador.cNaoNIF := StrToNaoNIF(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('cNaoNIF'), tcStr));

      IdentificacaoTomador.CAEPF := ObterConteudo(AuxNode.Childrens.FindAnyNs('CAEPF'), tcStr);
      IdentificacaoTomador.InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);

      RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);

      Ler_EnderecoTomador(AuxNode);

      Contato.Telefone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
      Contato.Email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_TotalTributos(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('totTrib');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.totTrib do
    begin
      Ler_ValorTotalTributos(AuxNode);
      Ler_PercentualTotalTributos(AuxNode);

      indTotTrib := StrToindTotTrib(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('indTotTrib'), tcStr));
      pTotTribSN := ObterConteudo(AuxNode.Childrens.FindAnyNs('pTotTribSN'), tcDe2);
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Tributacao(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('trib');

  if AuxNode <> nil then
  begin
    Ler_TributacaoMunicipal(AuxNode);
    Ler_TributacaoFederal(AuxNode);
    Ler_TotalTributos(AuxNode);
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_TributacaoMunicipal(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('tribMun');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribMun do
    begin
      tribISSQN := StrTotribISSQN(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tribISSQN'), tcStr));
      cPaisResult := SiglaISO2ToCodIBGEPais(ObterConteudo(AuxNode.Childrens.FindAnyNs('cPaisResult'), tcStr));

      Ler_BeneficioMunicipal(AuxNode);
      Ler_ExigibilidadeSuspensa(AuxNode);

      tpImunidade := StrTotpImunidade(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpImunidade'), tcStr));
      pAliq := ObterConteudo(AuxNode.Childrens.FindAnyNs('pAliq'), tcDe2);
      tpRetISSQN := StrTotpRetISSQN(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpRetISSQN'), tcStr));

      if tpRetISSQN = trNaoRetido then
      begin
        NFSe.Servico.Valores.IssRetido := stNormal;
        NFSe.Servico.Valores.ValorIssRetido := 0;
      end
      else
      begin
        NFSe.Servico.Valores.IssRetido := stRetencao;
        NFSe.Servico.Valores.ValorIssRetido := NFSe.infNFSe.valores.ValorIss;
      end;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_TributacaoFederal(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('tribFed');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribFed do
    begin
      Ler_TributacaoOutrosPisCofins(AuxNode);

      vRetCP := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCP'), tcDe2);
      vRetIRRF := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetIRRF'), tcDe2);
      vRetCSLL := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCSLL'), tcDe2);

      NFSe.Servico.Valores.ValorIr := vRetIRRF;
      NFSe.Servico.Valores.ValorCsll := vRetCSLL;
      NFSe.Servico.Valores.ValorInss := vRetCP;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_TributacaoOutrosPisCofins(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
begin
  AuxNode := ANode.Childrens.FindAnyNs('piscofins');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.tribFed do
    begin
      CST := StrToCST(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('CST'), tcStr));
      vBCPisCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCPisCofins'), tcDe2);
      pAliqPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('pAliqPis'), tcDe2);
      pAliqCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('pAliqCofins'), tcDe2);
      vPis := ObterConteudo(AuxNode.Childrens.FindAnyNs('vPis'), tcDe2);
      vCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('vCofins'), tcDe2);
      tpRetPisCofins := StrTotpRetPisCofins(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpRetPisCofins'), tcStr));

      NFSe.Servico.Valores.ValorPis := vPis;
      NFSe.Servico.Valores.ValorCofins := vCofins;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_Valores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('valores');

  if AuxNode <> nil then
  begin
    Ler_ServicoPrestado(AuxNode);
    Ler_Descontos(AuxNode);
    Ler_Deducoes(AuxNode);
    Ler_Tributacao(AuxNode);
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_ValoresNFSe(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('valores');

  if AuxNode <> nil then
  begin
    with NFSe.infNFSe.valores do
    begin
      vCalcDR := ObterConteudo(AuxNode.Childrens.FindAnyNs('vCalcDR'), tcDe2);
      tpBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('tpBM'), tcStr);
      vCalcBM := ObterConteudo(AuxNode.Childrens.FindAnyNs('vCalcBM'), tcDe2);
      BaseCalculo := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBC'), tcDe2);
      Aliquota := ObterConteudo(AuxNode.Childrens.FindAnyNs('pAliqAplic'), tcDe2);
      ValorIss := ObterConteudo(AuxNode.Childrens.FindAnyNs('vISSQN'), tcDe2);
      vTotalRet := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTotalRet'), tcDe2);
      ValorLiquidoNfse := ObterConteudo(AuxNode.Childrens.FindAnyNs('vLiq'), tcDe2);
    end;

    NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('xOutInf'), tcStr);
    NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                                    sLineBreak, [rfReplaceAll]);
    NFSe.Servico.Valores.Aliquota := NFSe.infNFSe.valores.Aliquota;
    NFSe.Servico.Valores.ValorIss := NFSe.infNFSe.valores.ValorIss;
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_ValorTotalTributos(
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('vTotTrib');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores.totTrib do
    begin
      vTotTribFed := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTotTribFed'), tcDe2);
      vTotTribEst := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTotTribEst'), tcDe2);
      vTotTribMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTotTribMun'), tcDe2);
    end;
  end;
end;

function TNFSeR_PadraoNacional.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  Arquivo := RemoverCaracteresDesnecessarios(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  // Não remover o espaço em branco, caso contrario vai encontrar tags que tem
  // NFSe em sua grafia.
  if (Pos('NFSe ', Arquivo) > 0) then
    tpXML := txmlNFSe
  else
    tpXML := txmlRPS;

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  NFSe.tpXML := tpXml;

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_PadraoNacional.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  Ler_infNFSe(ANode);

  LerCampoLink;
end;

function TNFSeR_PadraoNacional.LerXmlRps(const ANode: TACBrXmlNode): Boolean;
begin
  Result := True;

  if not Assigned(ANode) then Exit;

  Ler_infDPS(ANode);
end;

// Reforma Tributária
procedure TNFSeR_PadraoNacional.Ler_IBSCBSDPS(const ANode: TACBrXmlNode;
  IBSCBS: TIBSCBSDPS);
begin
  if not Assigned(ANode) then Exit;

  Ler_Destinatario(ANode.Childrens.FindAnyNs('dest'), IBSCBS.dest);
  Ler_Adquirente(ANode.Childrens.FindAnyNs('adq'), IBSCBS.adq);
  Ler_ServicoIBSCBS(ANode.Childrens.FindAnyNs('serv'), IBSCBS.serv);
  Ler_ValoresIBSCBS(ANode.Childrens.FindAnyNs('valores'), IBSCBS.valores);
end;

procedure TNFSeR_PadraoNacional.Ler_Destinatario(const ANode: TACBrXmlNode;
  Dest: TDadosdaPessoa);
var
  oK: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Dest.CNPJCPF := ObterCNPJCPF(ANode);
  Dest.NIF := ObterConteudo(ANode.Childrens.FindAnyNs('NIF'), tcStr);
  Dest.cNaoNIF := StrToNaoNIF(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('cNaoNIF'), tcStr));

  Dest.CAEPF := ObterConteudo(ANode.Childrens.FindAnyNs('CAEPF'), tcStr);

  Dest.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);

  Ler_EnderecoDestinatario(ANode.Childrens.FindAnyNs('end'), Dest.ender);

  Dest.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  Dest.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoDestinatario(
  const ANode: TACBrXmlNode; ender: Tender);
begin
  if not Assigned(ANode) then Exit;

  Ler_EnderecoNacionalDestinatario(ANode.Childrens.FindAnyNs('endNac'), ender.endNac);
  Ler_EnderecoExteriorDestinatario(ANode.Childrens.FindAnyNs('endExt'), ender.endExt);

  ender.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  ender.nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  ender.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  ender.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoNacionalDestinatario(
  const ANode: TACBrXmlNode; endNac: TendNac);
begin
  if not Assigned(ANode) then Exit;

  endNac.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  endNac.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcStr);
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoExteriorDestinatario(
  const ANode: TACBrXmlNode; endExt: TendExt);
begin
  if not Assigned(ANode) then Exit;

  endExt.cPais := SiglaISO2ToCodIBGEPais(ObterConteudo(ANode.Childrens.FindAnyNs('cPais'), tcStr));
  endExt.cEndPost := ObterConteudo(ANode.Childrens.FindAnyNs('cEndPost'), tcStr);
  endExt.xCidade := ObterConteudo(ANode.Childrens.FindAnyNs('xCidade'), tcStr);
  endExt.xEstProvReg := ObterConteudo(ANode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
end;

procedure TNFSeR_PadraoNacional.Ler_Adquirente(const ANode: TACBrXmlNode;
  Adq: TDadosdaPessoa);
var
  oK: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Adq.CNPJCPF := ObterCNPJCPF(ANode);
  Adq.NIF := ObterConteudo(ANode.Childrens.FindAnyNs('NIF'), tcStr);
  Adq.cNaoNIF := StrToNaoNIF(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('cNaoNIF'), tcStr));

  Adq.CAEPF := ObterConteudo(ANode.Childrens.FindAnyNs('CAEPF'), tcStr);

  Adq.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);

  Ler_EnderecoAdquirente(ANode.Childrens.FindAnyNs('end'), Adq.ender);

  Adq.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  Adq.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoAdquirente(
  const ANode: TACBrXmlNode; ender: Tender);
begin
  if not Assigned(ANode) then Exit;

  Ler_EnderecoNacionalAdquirente(ANode.Childrens.FindAnyNs('endNac'), ender.endNac);
  Ler_EnderecoExteriorAdquirente(ANode.Childrens.FindAnyNs('endExt'), ender.endExt);

  ender.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  ender.nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  ender.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  ender.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoNacionalAdquirente(
  const ANode: TACBrXmlNode; endNac: TendNac);
begin
  if not Assigned(ANode) then Exit;

  endNac.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  endNac.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcStr);
end;

procedure TNFSeR_PadraoNacional.Ler_EnderecoExteriorAdquirente(
  const ANode: TACBrXmlNode; endExt: TendExt);
begin
  if not Assigned(ANode) then Exit;

  endExt.cPais := SiglaISO2ToCodIBGEPais(ObterConteudo(ANode.Childrens.FindAnyNs('cPais'), tcStr));
  endExt.cEndPost := ObterConteudo(ANode.Childrens.FindAnyNs('cEndPost'), tcStr);
  endExt.xCidade := ObterConteudo(ANode.Childrens.FindAnyNs('xCidade'), tcStr);
  endExt.xEstProvReg := ObterConteudo(ANode.Childrens.FindAnyNs('xEstProvReg'), tcStr);
end;

procedure TNFSeR_PadraoNacional.Ler_ServicoIBSCBS(const ANode: TACBrXmlNode;
  serv: Tserv);
begin
  if not Assigned(ANode) then Exit;

  serv.modoPrestServ := ObterConteudo(ANode.Childrens.FindAnyNs('modoPrestServ'), tcStr);
  serv.clocalPrestServ := ObterConteudo(ANode.Childrens.FindAnyNs('clocalPrestServ'), tcInt);
  serv.cPaisPrestServ := ObterConteudo(ANode.Childrens.FindAnyNs('cPaisPrestServ'), tcInt);
  serv.cCIB := ObterConteudo(ANode.Childrens.FindAnyNs('cCIB'), tcStr);

  Ler_gCompraGov(ANode.Childrens.FindAnyNs('gCompraGov'), serv.gCompraGov);
end;

procedure TNFSeR_PadraoNacional.Ler_gCompraGov(const ANode: TACBrXmlNode;
  gCompraGov: TgCompraGov);
begin
  if not Assigned(ANode) then Exit;

  gCompraGov.indCompGov := StrToindCompGov(ObterConteudo(ANode.Childrens.FindAnyNs('indCompGov'), tcStr));
end;

procedure TNFSeR_PadraoNacional.Ler_ValoresIBSCBS(const ANode: TACBrXmlNode;
  valores: Tvalorestrib);
begin
  if not Assigned(ANode) then Exit;

  Ler_Tributos(ANode.Childrens.FindAnyNs('trib'), valores.trib);
end;

procedure TNFSeR_PadraoNacional.Ler_Tributos(
  const ANode: TACBrXmlNode; trib: Ttrib);
begin
  if not Assigned(ANode) then Exit;

  Ler_gIBSCBS(ANode.Childrens.FindAnyNs('gIBSCBS'), trib.gIBSCBS);
end;

procedure TNFSeR_PadraoNacional.Ler_gIBSCBS(
  const ANode: TACBrXmlNode; gIBSCBS: TgIBSCBS);
begin
  if not Assigned(ANode) then Exit;

  gIBSCBS.cstIBSCBS := ObterConteudo(ANode.Childrens.FindAnyNs('cstIBSCBS'), tcInt);
  gIBSCBS.cClassTribIBSCBS := ObterConteudo(ANode.Childrens.FindAnyNs('cClassTribIBSCBS'), tcInt);

  Ler_gIBSCredPres(ANode.Childrens.FindAnyNs('gIBSCredPres'), gIBSCBS.gIBSCredPres);
  Ler_gIBSUF(ANode.Childrens.FindAnyNs('gIBSUF'), gIBSCBS.gIBSUF);
  Ler_gIBSMun(ANode.Childrens.FindAnyNs('gIBSMun'), gIBSCBS.gIBSMun);
  Ler_gCBS(ANode.Childrens.FindAnyNs('gCBS'), gIBSCBS.gCBS);
end;

procedure TNFSeR_PadraoNacional.Ler_gIBSCredPres(const ANode: TACBrXmlNode;
  gIBSCredPres: TgIBSCredPres);
begin
  if not Assigned(ANode) then Exit;

  gIBSCredPres.cCredPresIBS := ObterConteudo(ANode.Childrens.FindAnyNs('cCredPresIBS'), tcInt);
  gIBSCredPres.pCredPresIBS := ObterConteudo(ANode.Childrens.FindAnyNs('pCredPresIBS'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gIBSUF(
  const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
begin
  if not Assigned(ANode) then Exit;

  Ler_gDifUF(ANode.Childrens.FindAnyNs('gDif'), gIBSUF);
  Ler_gDevTribUF(ANode.Childrens.FindAnyNs('gDevTrib'), gIBSUF);
  Ler_gDesonUF(ANode.Childrens.FindAnyNs('gDeson'), gIBSUF);
end;

procedure TNFSeR_PadraoNacional.Ler_gDifUF(
  const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSUF.pDifUF := ObterConteudo(ANode.Childrens.FindAnyNs('pDifUF'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gDevTribUF(
  const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSUF.vDevTribUF := ObterConteudo(ANode.Childrens.FindAnyNs('vDevTribUF'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gDesonUF(
  const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSUF.cstUFDeson := ObterConteudo(ANode.Childrens.FindAnyNs('cstUFDeson'), tcInt);
  gIBSUF.cClassTribUFDeson := ObterConteudo(ANode.Childrens.FindAnyNs('cClassTribUFDeson'), tcInt);
  gIBSUF.pAliqUFDeson := ObterConteudo(ANode.Childrens.FindAnyNs('pAliqUFDeson'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gIBSMun(
  const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
begin
  if not Assigned(ANode) then Exit;

  Ler_gDifMun(ANode.Childrens.FindAnyNs('gDif'), gIBSMun);
  Ler_gDevTribMun(ANode.Childrens.FindAnyNs('gDevTrib'), gIBSMun);
  Ler_gDesonMun(ANode.Childrens.FindAnyNs('gDeson'), gIBSMun);
end;

procedure TNFSeR_PadraoNacional.Ler_gDifMun(
  const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSMun.pDifMun := ObterConteudo(ANode.Childrens.FindAnyNs('pDifMun'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gDevTribMun(
  const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSMun.vDevTribMun := ObterConteudo(ANode.Childrens.FindAnyNs('vDevTribMun'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gDesonMun(
  const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSMun.cstMunDeson := ObterConteudo(ANode.Childrens.FindAnyNs('cstMunDeson'), tcInt);
  gIBSMun.cClassTribMunDeson := ObterConteudo(ANode.Childrens.FindAnyNs('cClassTribMunDeson'), tcInt);
  gIBSMun.pAliqMunDeson := ObterConteudo(ANode.Childrens.FindAnyNs('pAliqMunDeson'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gCBS(
  const ANode: TACBrXmlNode; gCBS: TgCBSValores);
begin
  if not Assigned(ANode) then Exit;

  Ler_gCBSCredPres(ANode.Childrens.FindAnyNs('gCredPres'), gCBS);
  Ler_gDifCBS(ANode.Childrens.FindAnyNs('gDif'), gCBS);
  Ler_gDevTribCBS(ANode.Childrens.FindAnyNs('gDevTrib'), gCBS);
  Ler_gDesonCBS(ANode.Childrens.FindAnyNs('gDeson'), gCBS);
end;

procedure TNFSeR_PadraoNacional.Ler_gCBSCredPres(
  const ANode: TACBrXmlNode; gCBS: TgCBSValores);
begin
  if not Assigned(ANode) then Exit;

  gCBS.cCredPresCBS := ObterConteudo(ANode.Childrens.FindAnyNs('cCredPresCBS'), tcInt);
  gCBS.pCredPresCBS := ObterConteudo(ANode.Childrens.FindAnyNs('pCredPresCBS'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gDifCBS(
  const ANode: TACBrXmlNode; gCBS: TgCBSValores);
begin
  if not Assigned(ANode) then Exit;

  gCBS.pDifCBS := ObterConteudo(ANode.Childrens.FindAnyNs('pDifCBS'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gDevTribCBS(
  const ANode: TACBrXmlNode; gCBS: TgCBSValores);
begin
  if not Assigned(ANode) then Exit;

  gCBS.vDevTribCBS := ObterConteudo(ANode.Childrens.FindAnyNs('vDevTribCBS'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_gDesonCBS(
  const ANode: TACBrXmlNode; gCBS: TgCBSValores);
begin
  if not Assigned(ANode) then Exit;

  gCBS.cstCBSDeson := ObterConteudo(ANode.Childrens.FindAnyNs('cstCBSDeson'), tcInt);
  gCBS.cClassTribCBSDeson := ObterConteudo(ANode.Childrens.FindAnyNs('cClassTribCBSDeson'), tcInt);
  gCBS.pAliqCBSDeson := ObterConteudo(ANode.Childrens.FindAnyNs('pAliqCBSDeson'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_IBSCBSNFSe(const ANode: TACBrXmlNode;
  IBSCBS: TIBSCBSNfse);
begin
  if not Assigned(ANode) then Exit;

  IBSCBS.xLocalidadeIncid := ObterConteudo(ANode.Childrens.FindAnyNs('xLocalidadeIncid'), tcStr);
  IBSCBS.xCSTIBSCBS := ObterConteudo(ANode.Childrens.FindAnyNs('xCSTIBSCBS'), tcStr);
  IBSCBS.xClassTribIBSCBS := ObterConteudo(ANode.Childrens.FindAnyNs('xClassTribIBSCBS'), tcStr);

  Ler_CompGov(ANode.Childrens.FindAnyNs('compGov'), IBSCBS.CompGov);

  Ler_ValoresIBSCBSNFSe(ANode.Childrens.FindAnyNs('valores'), IBSCBS.valores);

  Ler_TotCIBS(ANode.Childrens.FindAnyNs('totCIBSSel'), IBSCBS.totCIBS);
end;

procedure TNFSeR_PadraoNacional.Ler_CompGov(const ANode: TACBrXmlNode;
  CompGov: TCompGov);
begin
  if not Assigned(ANode) then Exit;

  CompGov.tpCompraGov := StrTotpCompraGov(ObterConteudo(ANode.Childrens.FindAnyNs('tpCompraGov'), tcStr));
  CompGov.pRedutor := ObterConteudo(ANode.Childrens.FindAnyNs('pRedutor'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_ValoresIBSCBSNFSe(const ANode: TACBrXmlNode;
  valores: TvaloresIBSCBS);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  valores.vBC := ObterConteudo(ANode.Childrens.FindAnyNs('vBC'), tcDe2);

  AuxNode := ANode.Childrens.FindAnyNs('uf');

  if AuxNode <> nil then
  begin
    valores.pIBSUF := ObterConteudo(AuxNode.Childrens.FindAnyNs('pIBSUF'), tcDe2);
    valores.pRedAliqUF := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRedAliqUF'), tcDe2);
    valores.pAliqEfetUF := ObterConteudo(AuxNode.Childrens.FindAnyNs('pAliqEfetUF'), tcDe2);
    valores.vTribOpUF := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTribOpUF'), tcDe2);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('mun');

  if AuxNode <> nil then
  begin
    valores.pIBSMun := ObterConteudo(ANode.Childrens.FindAnyNs('pIBSMun'), tcDe2);
    valores.pRedAliqMun := ObterConteudo(ANode.Childrens.FindAnyNs('pRedAliqMun'), tcDe2);
    valores.pAliqEfetMun := ObterConteudo(ANode.Childrens.FindAnyNs('pAliqEfetMun'), tcDe2);
    valores.vTribOpMun := ObterConteudo(ANode.Childrens.FindAnyNs('vTribOpMun'), tcDe2);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('fed');

  if AuxNode <> nil then
  begin
    valores.pCBS := ObterConteudo(ANode.Childrens.FindAnyNs('pCBS'), tcDe2);
    valores.pRedAliqCBS := ObterConteudo(ANode.Childrens.FindAnyNs('pRedAliqCBS'), tcDe2);
    valores.pAliqEfetCBS := ObterConteudo(ANode.Childrens.FindAnyNs('pAliqEfetCBS'), tcDe2);
    valores.vTribOpCBS := ObterConteudo(ANode.Childrens.FindAnyNs('vTribOpCBS'), tcDe2);
  end;
end;

procedure TNFSeR_PadraoNacional.Ler_TotCIBS(const ANode: TACBrXmlNode;
  totCIBS: TtotCIBS);
begin
  if not Assigned(ANode) then Exit;

  totCIBS.vTotNF := ObterConteudo(ANode.Childrens.FindAnyNs('vTotNF'), tcDe2);

  Ler_TotgIBS(ANode.Childrens.FindAnyNs('gIBS'), totCIBS.gIBS);
  Ler_TotgCBS(ANode.Childrens.FindAnyNs('gCBS'), totCIBS.gCBS);
end;

procedure TNFSeR_PadraoNacional.Ler_TotgIBS(
  const ANode: TACBrXmlNode; gIBS: TgIBS);
begin
  if not Assigned(ANode) then Exit;

  gIBS.vCredPresIBS := ObterConteudo(ANode.Childrens.FindAnyNs('vCredPresIBS'), tcDe2);

  Ler_TotgIBSUFTot(ANode.Childrens.FindAnyNs('gIBSUFTot'), gIBS);
  Ler_TotgIBSMunTot(ANode.Childrens.FindAnyNs('gIBSMunTot'), gIBS);

  gIBS.vIBSTot := ObterConteudo(ANode.Childrens.FindAnyNs('vIBSTot'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_TotgIBSUFTot(
  const ANode: TACBrXmlNode; gIBS: TgIBS);
begin
  if not Assigned(ANode) then Exit;

  gIBS.vDifUF := ObterConteudo(ANode.Childrens.FindAnyNs('vDifUF'), tcDe2);
  gIBS.vDesonUF := ObterConteudo(ANode.Childrens.FindAnyNs('vDesonUF'), tcDe2);
  gIBS.vIBSUF := ObterConteudo(ANode.Childrens.FindAnyNs('vIBSUF'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_TotgIBSMunTot(
  const ANode: TACBrXmlNode; gIBS: TgIBS);
begin
  if not Assigned(ANode) then Exit;

  gIBS.vDifMun := ObterConteudo(ANode.Childrens.FindAnyNs('vDifMun'), tcDe2);
  gIBS.vDesonMun := ObterConteudo(ANode.Childrens.FindAnyNs('vDesonMun'), tcDe2);
  gIBS.vIBSMun := ObterConteudo(ANode.Childrens.FindAnyNs('vIBSMun'), tcDe2);
end;

procedure TNFSeR_PadraoNacional.Ler_TotgCBS(
  const ANode: TACBrXmlNode; gCBS: TgCBS);
begin
  if not Assigned(ANode) then Exit;

  gCBS.vCredPresCBS := ObterConteudo(ANode.Childrens.FindAnyNs('vCredPresCBS'), tcDe2);
  gCBS.vDifCBS := ObterConteudo(ANode.Childrens.FindAnyNs('vDifCBS'), tcDe2);
  gCBS.vDesonCBS := ObterConteudo(ANode.Childrens.FindAnyNs('vDesonCBS'), tcDe2);
  gCBS.vCBS := ObterConteudo(ANode.Childrens.FindAnyNs('vCBS'), tcDe2);
end;

function TNFSeR_PadraoNacional.LerIni: Boolean;
var
  INIRec: TMemIniFile;
begin
  INIRec := TMemIniFile.Create('');

  // Usar o FpAOwner em vez de  FProvider

  try
    LerIniArquivoOuString(Arquivo, INIRec);

    FpTipoXML := INIRec.ReadString('IdentificacaoNFSe', 'TipoXML', '');

    if FpTipoXML = 'NFSE' then
      LerIniNfse(INIRec)
    else
      LerIniRps(INIRec);

  finally
    INIRec.Free;
  end;

  Result := True;
end;

procedure TNFSeR_PadraoNacional.LerIniRps(AINIRec: TMemIniFile);
begin
  LerINIIdentificacaoNFSe(AINIRec);
  LerINIIdentificacaoRps(AINIRec);
  LerININFSeSubstituicao(AINIRec);
  LerINIDadosPrestador(AINIRec);
  LerINIDadosTomador(AINIRec);
  LerINIDadosIntermediario(AINIRec);
  LerINIDadosServico(AINIRec);
  LerINIComercioExterior(AINIRec);
  LerINILocacaoSubLocacao(AINIRec);
  LerINIConstrucaoCivil(AINIRec);
  LerINIEvento(AINIRec);
  LerINIRodoviaria(AINIRec);
  LerINIInformacoesComplementares(AINIRec);
  LerINIValores(AINIRec);
  LerINIDocumentosDeducoes(AINIRec);
  LerINIValoresTribMun(AINIRec);
  LerINIValoresTribFederal(AINIRec);
  LerINIValoresTotalTrib(AINIRec);
  LerINIValoresTotalTrib(AINIRec);
  // Reforma Tributária
  LerINIDestinatario(AINIRec, NFSe.IBSCBS.dest);
  LerINIAdquirente(AINIRec, NFSe.IBSCBS.adq);
  LerINIServicoIBSCBS(AINIRec, NFSe.IBSCBS.serv);
  LerINIgIBSCBS(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS);
  LerINIgIBSCredPres(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS.gIBSCredPres);
  LerINIgIBSUF(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS.gIBSUF);
  LerINIgIBSMun(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS.gIBSMun);
  LerINIgCBS(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS.gCBS);
end;

procedure TNFSeR_PadraoNacional.LerIniNfse(AINIRec: TMemIniFile);
begin
  LerINIIdentificacaoNFSe(AINIRec);
  LerINIDadosEmitente(AINIRec);
  LerINIValoresNFSe(AINIRec);
  // Informações sobre o DPS
  LerINIIdentificacaoRps(AINIRec);
  LerININFSeSubstituicao(AINIRec);
  LerINIDadosPrestador(AINIRec);
  LerINIDadosTomador(AINIRec);
  LerINIDadosIntermediario(AINIRec);
  LerINIDadosServico(AINIRec);
  LerINIComercioExterior(AINIRec);
  LerINILocacaoSubLocacao(AINIRec);
  LerINIConstrucaoCivil(AINIRec);
  LerINIEvento(AINIRec);
  LerINIRodoviaria(AINIRec);
  LerINIInformacoesComplementares(AINIRec);
  LerINIValores(AINIRec);
  LerINIDocumentosDeducoes(AINIRec);
  LerINIValoresTribMun(AINIRec);
  LerINIValoresTribFederal(AINIRec);
  LerINIValoresTotalTrib(AINIRec);
  LerINIValoresTotalTrib(AINIRec);
  // Reforma Tributária
  LerINIDestinatario(AINIRec, NFSe.IBSCBS.dest);
  LerINIAdquirente(AINIRec, NFSe.IBSCBS.adq);
  LerINIServicoIBSCBS(AINIRec, NFSe.IBSCBS.serv);
  LerINIgIBSCBS(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS);
  LerINIgIBSCredPres(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS.gIBSCredPres);
  LerINIgIBSUF(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS.gIBSUF);
  LerINIgIBSMun(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS.gIBSMun);
  LerINIgCBS(AINIRec, NFSe.IBSCBS.valores.trib.gIBSCBS.gCBS);

  with NFSe.Servico.Valores do
  begin
    BaseCalculo := ValorServicos - ValorDeducoes - DescontoIncondicionado;

    if tribFed.tpRetPisCofins = trpcNaoRetido then
       RetencoesFederais := ValorInss + ValorIr + ValorCsll
    else
       RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;

    ValorLiquidoNfse := ValorServicos - RetencoesFederais - OutrasRetencoes -
               ValorIssRetido - DescontoIncondicionado - DescontoCondicionado;

    ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                            DescontoIncondicionado;
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIIdentificacaoNFSe(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'IdentificacaoNFSe';
  if AINIRec.SectionExists(sSecao) then
  begin
    if FpTipoXML = 'NFSE' then
    begin
      NFSe.infNFSe.ID := AINIRec.ReadString(sSecao, 'Id', '');
      NFSe.infNFSe.xLocEmi := AINIRec.ReadString(sSecao, 'xLocEmi', '');
      NFSe.infNFSe.xLocPrestacao := AINIRec.ReadString(sSecao, 'xLocPrestacao', '');
      NFSe.infNFSe.nNFSe := AINIRec.ReadString(sSecao, 'nNFSe', '');
      NFSe.infNFSe.cLocIncid := AINIRec.ReadInteger(sSecao, 'cLocIncid', 0);
      NFSe.infNFSe.xLocIncid := AINIRec.ReadString(sSecao, 'xLocIncid', '');
      NFSe.infNFSe.xTribNac := AINIRec.ReadString(sSecao, 'xTribNac', '');
      NFSe.infNFSe.xTribMun := AINIRec.ReadString(sSecao, 'xTribMun', '');
      NFSe.infNFSe.xNBS := AINIRec.ReadString(sSecao, 'xNBS', '');
      NFSe.infNFSe.verAplic := AINIRec.ReadString(sSecao, 'verAplic', '');
      NFSe.infNFSe.ambGer := StrToambGer(Ok, AINIRec.ReadString(sSecao, 'ambGer', ''));
      NFSe.infNFSe.tpEmis := StrTotpEmis(Ok, AINIRec.ReadString(sSecao, 'tpEmis', ''));
      NFSe.infNFSe.procEmi := StrToprocEmi(Ok, AINIRec.ReadString(sSecao, 'procEmi', ''));
      NFSe.infNFSe.cStat := AINIRec.ReadInteger(sSecao, 'cStat', 0);
      NFSe.infNFSe.dhProc := StringToDateTimeDef(AINIRec.ReadString(sSecao, 'dhProc', ''), 0);
      NFSe.infNFSe.nDFSe := AINIRec.ReadString(sSecao, 'nDFSe', '');
      NFSe.Servico.MunicipioIncidencia := NFSe.infNFSe.cLocIncid;
      NFSe.Servico.xMunicipioIncidencia := NFSe.infNFSe.xLocIncid;
      NFSe.Numero := NFSe.infNFSe.nNFSe;
      NFSe.CodigoVerificacao := NFSe.infNFSe.ID;
    end;
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIIdentificacaoRps(AINIRec: TMemIniFile);
var
  sSecao, sData: string;
  Ok: Boolean;
begin
  sSecao := 'IdentificacaoRps';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.IdentificacaoRps.Numero := AINIRec.ReadString(sSecao, 'Numero', '0');
    NFSe.IdentificacaoRps.Serie := AINIRec.ReadString(sSecao, 'Serie', '0');

    sData := AINIRec.ReadString(sSecao, 'DataEmissao', '');
    if sData <> '' then
      NFSe.DataEmissao := StringToDateTimeDef(sData, 0);

    sData := AINIRec.ReadString(sSecao, 'Competencia', '');
    if sData <> '' then
      NFSe.Competencia := StringToDateTimeDef(sData, 0);

    NFSe.verAplic := AINIRec.ReadString(sSecao, 'verAplic', 'ACBrNFSeX-1.00');
    NFSe.tpEmit := StrTotpEmit(Ok, AINIRec.ReadString(sSecao, 'tpEmit', '1'));
  end;
end;

procedure TNFSeR_PadraoNacional.LerININFSeSubstituicao(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'NFSeSubstituicao';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.subst.chSubstda := AINIRec.ReadString(sSecao, 'chSubstda', '');
    NFSe.subst.cMotivo := StrTocMotivo(Ok, AINIRec.ReadString(sSecao, 'cMotivo', ''));
    NFSe.subst.xMotivo := AINIRec.ReadString(sSecao, 'xMotivo', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIDadosEmitente(AINIRec: TMemIniFile);
var
  sSecao: string;
  xUF: string;
begin
  sSecao := 'Emitente';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.infNFSe.emit.Identificacao.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJ', '');
    NFSe.infNFSe.emit.Identificacao.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');

    NFSe.infNFSe.emit.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');
    NFSe.infNFSe.emit.NomeFantasia := AINIRec.ReadString(sSecao, 'NomeFantasia', '');

    NFSe.infNFSe.emit.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.infNFSe.emit.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.infNFSe.emit.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.infNFSe.emit.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
    NFSe.infNFSe.emit.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.infNFSe.emit.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    NFSe.infNFSe.emit.Endereco.xMunicipio := ObterNomeMunicipioUF(StrToIntDef(NFSe.infNFSe.emit.Endereco.CodigoMunicipio, 0), xUF);
    NFSe.infNFSe.emit.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');

    NFSe.infNFSe.emit.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    NFSe.infNFSe.emit.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');

    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := NFSe.infNFSe.emit.Identificacao.CpfCnpj;
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := NFSe.infNFSe.emit.Identificacao.InscricaoMunicipal;

    NFSe.Prestador.RazaoSocial := NFSe.infNFSe.emit.RazaoSocial;
    NFSe.Prestador.NomeFantasia := NFSe.infNFSe.emit.NomeFantasia;

    NFSe.Prestador.Endereco.Endereco := NFSe.infNFSe.emit.Endereco.Endereco;
    NFSe.Prestador.Endereco.Numero := NFSe.infNFSe.emit.Endereco.Numero;
    NFSe.Prestador.Endereco.Complemento := NFSe.infNFSe.emit.Endereco.Complemento;
    NFSe.Prestador.Endereco.Bairro := NFSe.infNFSe.emit.Endereco.Bairro;
    NFSe.Prestador.Endereco.UF := NFSe.infNFSe.emit.Endereco.UF;
    NFSe.Prestador.Endereco.CEP := NFSe.infNFSe.emit.Endereco.CEP;
    NFSe.Prestador.Endereco.CodigoMunicipio := NFSe.infNFSe.emit.Endereco.CodigoMunicipio;
    NFSe.Prestador.Endereco.xMunicipio := NFSe.infNFSe.emit.Endereco.xMunicipio;

    NFSe.Prestador.Contato.Telefone := NFSe.infNFSe.emit.Contato.Telefone;
    NFSe.Prestador.Contato.Email := NFSe.infNFSe.emit.Contato.Email;
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIValoresNFSe(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'ValoresNFSe';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.infNFSe.valores.vCalcDR := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCalcDR', ''), 0);
    NFSe.infNFSe.valores.tpBM := AINIRec.ReadString(sSecao, 'tpBM', '');
    NFSe.infNFSe.valores.vCalcBM := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCalcBM', ''), 0);
    NFSe.infNFSe.valores.BaseCalculo := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBC', ''), 0);
    NFSe.infNFSe.valores.Aliquota := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqAplic', ''), 0);
    NFSe.infNFSe.valores.ValorIss := StringToFloatDef(AINIRec.ReadString(sSecao, 'vISSQN', ''), 0);
    NFSe.infNFSe.valores.vTotalRet := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotalRet', ''), 0);
    NFSe.infNFSe.valores.ValorLiquidoNfse := StringToFloatDef(AINIRec.ReadString(sSecao, 'vLiq', ''), 0);
    NFSe.OutrasInformacoes := AINIRec.ReadString(sSecao, 'xOutInf', '');

    NFSe.Servico.Valores.Aliquota := NFSe.infNFSe.valores.Aliquota;
    NFSe.Servico.Valores.ValorIss := NFSe.infNFSe.valores.ValorIss;
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIDadosPrestador(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Prestador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Prestador.IdentificacaoPrestador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJ', '');
    NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');

    NFSe.Prestador.IdentificacaoPrestador.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
    NFSe.Prestador.IdentificacaoPrestador.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
    NFSe.Prestador.IdentificacaoPrestador.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');

    NFSe.Prestador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');

    NFSe.Prestador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Prestador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    NFSe.Prestador.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
    NFSe.Prestador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.Prestador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.Prestador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Prestador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Prestador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Prestador.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');

    NFSe.Prestador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    NFSe.Prestador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');

    NFSe.OptanteSN := StrToOptanteSN(Ok, AINIRec.ReadString(sSecao, 'opSimpNac', '2'));

    if AINIRec.ReadString(sSecao, 'RegimeApuracaoSN', '') <> '' then
      NFSe.RegimeApuracaoSN := StrToRegimeApuracaoSN(Ok, AINIRec.ReadString(sSecao, 'RegimeApuracaoSN', '1'));

    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, AINIRec.ReadString(sSecao, 'Regime', '0'));
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIDadosTomador(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Tomador';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Tomador.IdentificacaoTomador.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');

    NFSe.Tomador.IdentificacaoTomador.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
    NFSe.Tomador.IdentificacaoTomador.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
    NFSe.Tomador.IdentificacaoTomador.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');

    NFSe.Tomador.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');

    NFSe.Tomador.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Tomador.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    NFSe.Tomador.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
    NFSe.Tomador.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.Tomador.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.Tomador.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Tomador.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Tomador.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Tomador.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');

    NFSe.Tomador.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    NFSe.Tomador.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIDadosIntermediario(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Intermediario';
  if AINIRec.SectionExists(sSecao)then
  begin
    NFSe.Intermediario.Identificacao.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    NFSe.Intermediario.Identificacao.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
    NFSe.Intermediario.Identificacao.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
    NFSe.Intermediario.Identificacao.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
    NFSe.Intermediario.Identificacao.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');

    NFSe.Intermediario.RazaoSocial := AINIRec.ReadString(sSecao, 'RazaoSocial', '');

    NFSe.Intermediario.Endereco.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Intermediario.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    NFSe.Intermediario.Endereco.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
    NFSe.Intermediario.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.Intermediario.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.Intermediario.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Intermediario.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Intermediario.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Intermediario.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');

    NFSe.Intermediario.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    NFSe.Intermediario.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIDadosServico(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'Servico';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.CodigoMunicipio := AINIRec.ReadString(sSecao, 'CodigoMunicipio', '');
    NFSe.Servico.CodigoPais := AINIRec.ReadInteger(sSecao, 'CodigoPais', 0);
    NFSe.Servico.ItemListaServico := AINIRec.ReadString(sSecao, 'ItemListaServico', '');
    NFSe.Servico.CodigoTributacaoMunicipio := AINIRec.ReadString(sSecao, 'CodigoTributacaoMunicipio', '');
    NFSe.Servico.Discriminacao := AINIRec.ReadString(sSecao, 'Discriminacao', '');
    NFSe.Servico.CodigoNBS := AINIRec.ReadString(sSecao, 'CodigoNBS', '');
    NFSe.Servico.CodigoInterContr := AINIRec.ReadString(sSecao, 'CodigoInterContr', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIComercioExterior(AINIRec: TMemIniFile);
var
  SSecao: string;
  Ok: Boolean;
begin
  sSecao := 'ComercioExterior';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.comExt.mdPrestacao := StrTomdPrestacao(Ok, AINIRec.ReadString(sSecao, 'mdPrestacao', '0'));
    NFSe.Servico.comExt.vincPrest := StrTovincPrest(Ok, AINIRec.ReadString(sSecao, 'vincPrest', '0'));
    NFSe.Servico.comExt.tpMoeda := AINIRec.ReadInteger(sSecao, 'tpMoeda', 0);
    NFSe.Servico.comExt.vServMoeda := StringToFloatDef(AINIRec.ReadString(sSecao, 'vServMoeda', '0'), 0);
    NFSe.Servico.comExt.mecAFComexP := StrTomecAFComexP(Ok, AINIRec.ReadString(sSecao, 'mecAFComexP', '00'));
    NFSe.Servico.comExt.mecAFComexT := StrTomecAFComexT(Ok, AINIRec.ReadString(sSecao, 'mecAFComexT', '00'));
    NFSe.Servico.comExt.movTempBens := StrToMovTempBens(Ok, AINIRec.ReadString(sSecao, 'movTempBens', '00'));
    NFSe.Servico.comExt.nDI := AINIRec.ReadString(sSecao, 'nDI', '');
    NFSe.Servico.comExt.nRE := AINIRec.ReadString(sSecao, 'nRE', '');
    NFSe.Servico.comExt.mdic := AINIRec.ReadInteger(sSecao, 'mdic', 0);
  end;
end;

procedure TNFSeR_PadraoNacional.LerINILocacaoSubLocacao(AINIRec: TMemIniFile);
var
  SSecao: string;
  Ok: Boolean;
begin
  sSecao := 'LocacaoSubLocacao';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Locacao.categ := StrTocateg(Ok, AINIRec.ReadString(sSecao, 'categ', '1'));
    NFSe.Servico.Locacao.objeto := StrToobjeto(Ok, AINIRec.ReadString(sSecao, 'objeto', '1'));
    NFSe.Servico.Locacao.extensao := AINIRec.ReadString(sSecao, 'extensao', '');
    NFSe.Servico.Locacao.nPostes := AINIRec.ReadInteger(sSecao, 'nPostes', 0);
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIConstrucaoCivil(AINIRec: TMemIniFile);
var
  sSecao: string;
begin
  sSecao := 'ConstrucaoCivil';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.ConstrucaoCivil.CodigoObra := AINIRec.ReadString(sSecao, 'CodigoObra', '');
    NFSe.ConstrucaoCivil.inscImobFisc := AINIRec.ReadString(sSecao, 'inscImobFisc', '');

    NFSe.ConstrucaoCivil.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    NFSe.ConstrucaoCivil.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.ConstrucaoCivil.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.ConstrucaoCivil.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.ConstrucaoCivil.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.ConstrucaoCivil.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.ConstrucaoCivil.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIEvento(AINIRec: TMemIniFile);
var
  SSecao: string;
begin
  sSecao := 'Evento';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Evento.xNome := AINIRec.ReadString(sSecao, 'xNome', '');
    NFSe.Servico.Evento.dtIni := AINIRec.ReadDate(sSecao, 'dtIni', Now);
    NFSe.Servico.Evento.dtFim := AINIRec.ReadDate(sSecao, 'dtFim', Now);
    NFSe.Servico.Evento.idAtvEvt := AINIRec.ReadString(sSecao, 'idAtvEvt', '');

    NFSe.Servico.Evento.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    NFSe.Servico.Evento.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    NFSe.Servico.Evento.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    NFSe.Servico.Evento.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    NFSe.Servico.Evento.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    NFSe.Servico.Evento.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    NFSe.Servico.Evento.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIRodoviaria(AINIRec: TMemIniFile);
var
  SSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Rodoviaria';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.explRod.categVeic := StrTocategVeic(Ok, AINIRec.ReadString(sSecao, 'categVeic', '00'));
    NFSe.Servico.explRod.nEixos := AINIRec.ReadInteger(sSecao, 'nEixos', 0);
    NFSe.Servico.explRod.rodagem := StrTorodagem(Ok, AINIRec.ReadString(sSecao, 'rodagem', '1'));
    NFSe.Servico.explRod.sentido := AINIRec.ReadString(sSecao, 'sentido', '');
    NFSe.Servico.explRod.placa := AINIRec.ReadString(sSecao, 'placa', '');
    NFSe.Servico.explRod.codAcessoPed := AINIRec.ReadString(sSecao, 'codAcessoPed', '');
    NFSe.Servico.explRod.codContrato := AINIRec.ReadString(sSecao, 'codContrato', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIInformacoesComplementares(
  AINIRec: TMemIniFile);
var
  SSecao: string;
begin
  sSecao := 'InformacoesComplementares';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.infoCompl.idDocTec := AINIRec.ReadString(sSecao, 'idDocTec', '');
    NFSe.Servico.infoCompl.docRef := AINIRec.ReadString(sSecao, 'docRef', '');
    NFSe.Servico.infoCompl.xInfComp := AINIRec.ReadString(sSecao, 'xInfComp', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIValores(AINIRec: TMemIniFile);
var
  SSecao: string;
begin
  sSecao := 'Valores';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Valores.ValorRecebido := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorRecebido', ''), 0);
    NFSe.Servico.Valores.ValorServicos := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorServicos', ''), 0);
    NFSe.Servico.Valores.DescontoIncondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoIncondicionado', ''), 0);
    NFSe.Servico.Valores.DescontoCondicionado := StringToFloatDef(AINIRec.ReadString(sSecao, 'DescontoCondicionado', ''), 0);
    NFSe.Servico.Valores.AliquotaDeducoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'AliquotaDeducoes', ''), 0);
    NFSe.Servico.Valores.ValorDeducoes := StringToFloatDef(AINIRec.ReadString(sSecao, 'ValorDeducoes', ''), 0);
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIDocumentosDeducoes(AINIRec: TMemIniFile);
var
  i: Integer;
  sSecao: string;
  Ok: Boolean;
  Item: TDocDeducaoCollectionItem;
begin
  i := 1;
  while true do
  begin
    sSecao := 'DocumentosDeducoes' + IntToStrZero(i, 3);

    if not AINIRec.SectionExists(sSecao) then
      break;

    Item := NFSe.Servico.Valores.DocDeducao.New;

    Item.chNFSe := AINIRec.ReadString(sSecao,'chNFSe', '');
    Item.chNFe := AINIRec.ReadString(sSecao, 'chNFe', '');
    Item.nDocFisc := AINIRec.ReadString(sSecao, 'nDocFisc', '');
    Item.nDoc := AINIRec.ReadString(sSecao, 'nDoc', '');
    Item.tpDedRed := StrTotpDedRed(Ok, AINIRec.ReadString(sSecao, 'tpDedRed', '1'));
    Item.xDescOutDed := AINIRec.ReadString(sSecao, 'xDescOutDed', '');
    Item.dtEmiDoc := AINIRec.ReadDate(sSecao, 'dtEmiDoc', Now);
    Item.vDedutivelRedutivel := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDedutivelRedutivel', ''), 0);
    Item.vDeducaoReducao := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDeducaoReducao', ''), 0);

    Item.NFSeMun.cMunNFSeMun := AINIRec.ReadString(sSecao, 'cMunNFSeMun', '');
    Item.NFSeMun.nNFSeMun := AINIRec.ReadString(sSecao, 'nNFSeMun', '');
    Item.NFSeMun.cVerifNFSeMun := AINIRec.ReadString(sSecao, 'cVerifNFSeMun', '');

    Item.NFNFS.nNFS := AINIRec.ReadString(sSecao, 'nNFS', '');
    Item.NFNFS.modNFS := AINIRec.ReadString(sSecao, 'modNFS', '');
    Item.NFNFS.serieNFS := AINIRec.ReadString(sSecao, 'serieNFS', '');

    LerINIDocumentosDeducoesFornecedor(AINIRec, Item.fornec, i);

    Inc(i);
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIDocumentosDeducoesFornecedor(
  AINIRec: TMemIniFile; fornec: TInfoPessoa; Indice: Integer);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'DocumentosDeducoesFornecedor' + IntToStrZero(Indice, 3);
  if AINIRec.SectionExists(sSecao) then
  begin
    fornec.Identificacao.CpfCnpj := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    fornec.Identificacao.InscricaoMunicipal := AINIRec.ReadString(sSecao, 'InscricaoMunicipal', '');
    fornec.Identificacao.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
    fornec.Identificacao.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
    fornec.Identificacao.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');

    fornec.Endereco.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    fornec.Endereco.xMunicipio := AINIRec.ReadString(sSecao, 'xMunicipio', '');
    fornec.Endereco.UF := AINIRec.ReadString(sSecao, 'UF', '');
    fornec.Endereco.Endereco := AINIRec.ReadString(sSecao, 'Logradouro', '');
    fornec.Endereco.Numero := AINIRec.ReadString(sSecao, 'Numero', '');
    fornec.Endereco.Complemento := AINIRec.ReadString(sSecao, 'Complemento', '');
    fornec.Endereco.Bairro := AINIRec.ReadString(sSecao, 'Bairro', '');

    fornec.Contato.Telefone := AINIRec.ReadString(sSecao, 'Telefone', '');
    fornec.Contato.Email := AINIRec.ReadString(sSecao, 'Email', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIValoresTribMun(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'tribMun';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Valores.tribMun.tribISSQN := StrTotribISSQN(Ok, AINIRec.ReadString(sSecao, 'tribISSQN', '1'));
    NFSe.Servico.Valores.tribMun.cPaisResult := AINIRec.ReadInteger(sSecao, 'cPaisResult', 0);
    NFSe.Servico.Valores.tribMun.tpBM := StrTotpBM(Ok, AINIRec.ReadString(sSecao, 'tpBM', '1'));
    NFSe.Servico.Valores.tribMun.nBM := AINIRec.ReadString(sSecao, 'nBM', '');
    NFSe.Servico.Valores.tribMun.vRedBCBM := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRedBCBM', ''), 0);
    NFSe.Servico.Valores.tribMun.pRedBCBM := StringToFloatDef(AINIRec.ReadString(sSecao, 'pRedBCBM', ''), 0);
    NFSe.Servico.Valores.tribMun.tpSusp := StrTotpSusp(Ok, AINIRec.ReadString(sSecao, 'tpSusp', ''));
    NFSe.Servico.Valores.tribMun.nProcesso := AINIRec.ReadString(sSecao, 'nProcesso', '');
    NFSe.Servico.Valores.tribMun.tpImunidade := StrTotpImunidade(Ok, AINIRec.ReadString(sSecao, 'tpImunidade', ''));
    NFSe.Servico.Valores.tribMun.pAliq := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliq', ''), 0);
    NFSe.Servico.Valores.tribMun.tpRetISSQN := StrTotpRetISSQN(Ok, AINIRec.ReadString(sSecao, 'tpRetISSQN', ''));
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIValoresTribFederal(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'tribFederal';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Valores.tribFed.CST := StrToCST(Ok, AINIRec.ReadString(sSecao, 'CST', ''));
    NFSe.Servico.Valores.tribFed.vBCPisCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'vBCPisCofins', ''), 0);
    NFSe.Servico.Valores.tribFed.pAliqPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqPis', ''), 0);
    NFSe.Servico.Valores.tribFed.pAliqCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqCofins' ,''), 0);
    NFSe.Servico.Valores.tribFed.vPis := StringToFloatDef(AINIRec.ReadString(sSecao, 'vPis', ''), 0);
    NFSe.Servico.Valores.tribFed.vCofins := StringToFloatDef(AINIRec.ReadString(sSecao, 'vCofins', ''), 0);
    NFSe.Servico.Valores.tribFed.tpRetPisCofins := StrTotpRetPisCofins(Ok, AINIRec.ReadString(sSecao, 'tpRetPisCofins', ''));
    NFSe.Servico.Valores.tribFed.vRetCP := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRetCP', ''), 0);
    NFSe.Servico.Valores.tribFed.vRetIRRF := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRetIRRF', ''), 0);
    NFSe.Servico.Valores.tribFed.vRetCSLL := StringToFloatDef(AINIRec.ReadString(sSecao, 'vRetCSLL', ''), 0);
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIValoresTotalTrib(AINIRec: TMemIniFile);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'totTrib';
  if AINIRec.SectionExists(sSecao) then
  begin
    NFSe.Servico.Valores.totTrib.indTotTrib := StrToindTotTrib(Ok, AINIRec.ReadString(sSecao, 'indTotTrib', '0'));
    NFSe.Servico.Valores.totTrib.pTotTribSN := StringToFloatDef(AINIRec.ReadString(sSecao, 'pTotTribSN', ''), 0);

    NFSe.Servico.Valores.totTrib.vTotTribFed := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotTribFed', ''), 0);
    NFSe.Servico.Valores.totTrib.vTotTribEst := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotTribEst', ''), 0);
    NFSe.Servico.Valores.totTrib.vTotTribMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'vTotTribMun', ''), 0);

    NFSe.Servico.Valores.totTrib.pTotTribFed := StringToFloatDef(AINIRec.ReadString(sSecao, 'pTotTribFed', ''), 0);
    NFSe.Servico.Valores.totTrib.pTotTribEst := StringToFloatDef(AINIRec.ReadString(sSecao, 'pTotTribEst', ''), 0);
    NFSe.Servico.Valores.totTrib.pTotTribMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'pTotTribMun', ''), 0);
  end;
end;

// Reforma Tributária
procedure TNFSeR_PadraoNacional.LerINIDestinatario(AINIRec: TMemIniFile; Dest: TDadosdaPessoa);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Destinatario';
  if AINIRec.SectionExists(sSecao) then
  begin
    Dest.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    Dest.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
    Dest.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
    Dest.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');
    Dest.xNome := AINIRec.ReadString(sSecao, 'xNome', '');

    Dest.ender.endNac.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    Dest.ender.endNac.cMun := AINIRec.ReadInteger(sSecao, 'cMun', 0);
    Dest.ender.endExt.cPais := AINIRec.ReadInteger(sSecao, 'cPais', 0);
    Dest.ender.endExt.cEndPost := AINIRec.ReadString(sSecao, 'cEndPost', '');
    Dest.ender.endExt.xCidade := AINIRec.ReadString(sSecao, 'xCidade', '');
    Dest.ender.endExt.xEstProvReg := AINIRec.ReadString(sSecao, 'xEstProvReg', '');

    Dest.ender.xLgr := AINIRec.ReadString(sSecao, 'Logradouro', '');
    Dest.ender.nro := AINIRec.ReadString(sSecao, 'Numero', '');
    Dest.ender.xCpl := AINIRec.ReadString(sSecao, 'Complemento', '');
    Dest.ender.xBairro := AINIRec.ReadString(sSecao, 'Bairro', '');

    Dest.fone := AINIRec.ReadString(sSecao, 'Telefone', '');
    Dest.email := AINIRec.ReadString(sSecao, 'Email', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIAdquirente(AINIRec: TMemIniFile;
  Adq: TDadosdaPessoa);
var
  sSecao: string;
  Ok: Boolean;
begin
  sSecao := 'Adquirente';
  if AINIRec.SectionExists(sSecao) then
  begin
    Adq.CNPJCPF := AINIRec.ReadString(sSecao, 'CNPJCPF', '');
    Adq.Nif := AINIRec.ReadString(sSecao, 'NIF', '');
    Adq.cNaoNIF := StrToNaoNIF(Ok, AINIRec.ReadString(sSecao, 'cNaoNIF', '0'));
    Adq.CAEPF := AINIRec.ReadString(sSecao, 'CAEPF', '');
    Adq.xNome := AINIRec.ReadString(sSecao, 'xNome', '');

    Adq.ender.endNac.CEP := AINIRec.ReadString(sSecao, 'CEP', '');
    Adq.ender.endNac.cMun := AINIRec.ReadInteger(sSecao, 'cMun', 0);
    Adq.ender.endExt.cPais := AINIRec.ReadInteger(sSecao, 'cPais', 0);
    Adq.ender.endExt.cEndPost := AINIRec.ReadString(sSecao, 'cEndPost', '');
    Adq.ender.endExt.xCidade := AINIRec.ReadString(sSecao, 'xCidade', '');
    Adq.ender.endExt.xEstProvReg := AINIRec.ReadString(sSecao, 'xEstProvReg', '');

    Adq.ender.xLgr := AINIRec.ReadString(sSecao, 'Logradouro', '');
    Adq.ender.nro := AINIRec.ReadString(sSecao, 'Numero', '');
    Adq.ender.xCpl := AINIRec.ReadString(sSecao, 'Complemento', '');
    Adq.ender.xBairro := AINIRec.ReadString(sSecao, 'Bairro', '');

    Adq.fone := AINIRec.ReadString(sSecao, 'Telefone', '');
    Adq.email := AINIRec.ReadString(sSecao, 'Email', '');
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIServicoIBSCBS(AINIRec: TMemIniFile; serv: Tserv);
var
  sSecao: string;
begin
  sSecao := 'ServicoIBSCBS';
  if AINIRec.SectionExists(sSecao) then
  begin
    serv.modoPrestServ := AINIRec.ReadString(sSecao, 'modoPrestServ', '');
    serv.clocalPrestServ := AINIRec.ReadInteger(sSecao, 'clocalPrestServ', 0);
    serv.cPaisPrestServ := AINIRec.ReadInteger(sSecao, 'cPaisPrestServ', 0);
    serv.cCIB := AINIRec.ReadString(sSecao, 'cCIB', '');

    serv.gCompraGov.indCompGov := StrToindCompGov(AINIRec.ReadString(sSecao, 'indCompGov', ''));
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIgIBSCBS(AINIRec: TMemIniFile;
  gIBSCBS: TgIBSCBS);
var
  sSecao: string;
begin
  sSecao := 'gIBSCBS';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSCBS.cstIBSCBS := AINIRec.ReadInteger(sSecao, 'cstIBSCBS', 0);
    gIBSCBS.cClassTribIBSCBS := AINIRec.ReadInteger(sSecao, 'cClassTribIBSCBS', 0);

    LerINIgIBSCredPres(AINIRec, gIBSCBS.gIBSCredPres);
    LerINIgIBSUF(AINIRec, gIBSCBS.gIBSUF);
    LerINIgIBSMun(AINIRec, gIBSCBS.gIBSMun);
    LerINIgCBS(AINIRec, gIBSCBS.gCBS);
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIgIBSCredPres(AINIRec: TMemIniFile;
  gIBSCredPres: TgIBSCredPres);
var
  sSecao: string;
begin
  sSecao := 'gIBSCredPres';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSCredPres.cCredPresIBS := AINIRec.ReadInteger(sSecao, 'cCredPresIBS', 0);
    gIBSCredPres.pCredPresIBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pCredPresIBS', ''), 0);
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIgIBSUF(AINIRec: TMemIniFile;
  gIBSUF: TgIBSUFValores);
var
  sSecao: string;
begin
  sSecao := 'gIBSUF';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSUF.pDifUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'pDifUF', ''), 0);
    gIBSUF.vDevTribUF := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDevTribUF', ''), 0);
    gIBSUF.cstUFDeson := AINIRec.ReadInteger(sSecao, 'cstUFDeson', 0);
    gIBSUF.cClassTribUFDeson := AINIRec.ReadInteger(sSecao, 'cClassTribUFDeson', 0);
    gIBSUF.pAliqUFDeson := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqUFDeson', ''), 0);
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIgIBSMun(AINIRec: TMemIniFile;
  gIBSMun: TgIBSMunValores);
var
  sSecao: string;
begin
  sSecao := 'gIBSMun';
  if AINIRec.SectionExists(sSecao) then
  begin
    gIBSMun.pDifMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'pDifMun', ''), 0);
    gIBSMun.vDevTribMun := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDevTribMun', ''), 0);
    gIBSMun.cstMunDeson := AINIRec.ReadInteger(sSecao, 'cstMunDeson', 0);
    gIBSMun.cClassTribMunDeson := AINIRec.ReadInteger(sSecao, 'cClassTribMunDeson', 0);
    gIBSMun.pAliqMunDeson := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqMunDeson', ''), 0);
  end;
end;

procedure TNFSeR_PadraoNacional.LerINIgCBS(AINIRec: TMemIniFile;
  gCBS: TgCBSValores);
var
  sSecao: string;
begin
  sSecao := 'gCBS';
  if AINIRec.SectionExists(sSecao) then
  begin
    gCBS.cCredPresCBS := AINIRec.ReadInteger(sSecao, 'cCredPresCBS', 0);
    gCBS.pCredPresCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pCredPresCBS', ''), 0);

    gCBS.pDifCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'pDifCBS', ''), 0);
    gCBS.vDevTribCBS := StringToFloatDef(AINIRec.ReadString(sSecao, 'vDevTribCBS', ''), 0);
    gCBS.cstCBSDeson := AINIRec.ReadInteger(sSecao, 'cstCBSDeson', 0);
    gCBS.cClassTribCBSDeson := AINIRec.ReadInteger(sSecao, 'cClassTribCBSDeson', 0);
    gCBS.pAliqCBSDeson := StringToFloatDef(AINIRec.ReadString(sSecao, 'pAliqCBSDeson', ''), 0);
  end;
end;

end.
