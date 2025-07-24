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

unit ACBrNF3eXmlReader;

interface

uses
  Classes, SysUtils,
  ACBrXmlDocument, ACBrXmlReader,
  ACBrNF3eClass;

type
  { TNF3eXmlReader }
  TNF3eXmlReader = class(TACBrXmlReader)
  private
    FNF3e: TNF3e;

    function NodeNaoEncontrado(const ANode: TACBrXmlNode): Boolean;

    procedure Ler_ProtNF3e(const ANode: TACBrXmlNode);
    procedure Ler_InfNF3e(const ANode: TACBrXmlNode);
    procedure Ler_Ide(const ANode: TACBrXmlNode);
    procedure Ler_Emit(const ANode: TACBrXmlNode);
    procedure Ler_EmitEnderEmit(const ANode: TACBrXmlNode);
    procedure Ler_Dest(const ANode: TACBrXmlNode);
    procedure Ler_DestEnderDest(const ANode: TACBrXmlNode);
    procedure Ler_Acessante(const ANode: TACBrXmlNode);
    procedure Ler_gSub(const ANode: TACBrXmlNode);
    procedure Ler_gNF(const ANode: TACBrXmlNode);
    procedure Ler_gJudic(const ANode: TACBrXmlNode);
    procedure Ler_gGrContrat(const ANode: TACBrXmlNode);
    procedure Ler_gMed(const ANode: TACBrXmlNode);
    procedure Ler_gSCEE(const ANode: TACBrXmlNode);
    procedure Ler_gConsumidor(const ANode: TACBrXmlNode);
    procedure Ler_gSaldoCred(const ANode: TACBrXmlNode);
    procedure Ler_gSaldoCred2(const ANode: TACBrXmlNode; gTipoSaldo: TgTipoSaldoCollectionItem);
    procedure Ler_gTipoSaldo(const ANode: TACBrXmlNode);
    procedure Ler_NFDet(const ANode: TACBrXmlNode);
    procedure Ler_Total(const ANode: TACBrXmlNode);
    procedure Ler_gFat(const ANode: TACBrXmlNode);
    procedure Ler_gANEEL(const ANode: TACBrXmlNode);
    procedure Ler_gHistFat(const ANode: TACBrXmlNode);
    procedure Ler_gGrandFat(const ANode: TACBrXmlNode; I: Integer);
    procedure Ler_autXML(const ANode: TACBrXmlNode);
    procedure Ler_InfAdic(const ANode: TACBrXmlNode);
    procedure Ler_gRespTec(const ANode: TACBrXmlNode);
    procedure Ler_InfNF3eSupl(const ANode: TACBrXmlNode);

    // Reforma Tributária
    procedure Ler_gCompraGov(gCompraGov: TgCompraGovReduzido; const ANode: TACBrXmlNode);

    procedure Ler_IBSCBS(const ANode: TACBrXmlNode; IBSCBS: TIBSCBS);
    procedure Ler_IBSCBS_gIBSCBS(const ANode: TACBrXmlNode; gIBSCBS: TgIBSCBS);

    procedure Ler_gIBSUF(const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
    procedure Ler_gIBSUF_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
    procedure Ler_gIBSUF_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
    procedure Ler_gIBSUF_gRed(const ANode: TACBrXmlNode; gRed: TgRed);

    procedure Ler_gIBSMun(const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
    procedure Ler_gIBSMun_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
    procedure Ler_gIBSMun_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
    procedure Ler_gIBSMun_gRed(const ANode: TACBrXmlNode; gRed: TgRed);

    procedure Ler_gCBS(const ANode: TACBrXmlNode; gCBS: TgCBSValores);
    procedure Ler_gCBS_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
    procedure Ler_gCBS_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
    procedure Ler_gCBS_gRed(const ANode: TACBrXmlNode; gRed: TgRed);

    procedure Ler_gIBSCBS_gTribRegular(const ANode: TACBrXmlNode; gTribRegular: TgTribRegular);
    procedure Ler_gIBSCredPres(const ANode: TACBrXmlNode; gIBSCredPres: TgIBSCBSCredPres);
    procedure Ler_gCBSCredPres(const ANode: TACBrXmlNode; gCBSCredPres: TgIBSCBSCredPres);
    procedure Ler_gTribCompraGov(const ANode: TACBrXmlNode; gTribCompraGov: TgTribCompraGov);

    procedure Ler_IBSCBSTot(const ANode: TACBrXmlNode; IBSCBSTot: TIBSCBSTot);
    procedure Ler_IBSCBSTot_gIBS(const ANode: TACBrXmlNode; gIBS: TgIBS);
    procedure Ler_IBSCBSTot_gIBS_gIBSUFTot(const ANode: TACBrXmlNode; gIBSUFTot: TgIBSUFTot);
    procedure Ler_IBSCBSTot_gIBS_gIBSMunTot(const ANode: TACBrXmlNode; gIBSMunTot: TgIBSMunTot);
    procedure Ler_IBSCBSTot_gCBS(const ANode: TACBrXmlNode; gCBS: TgCBS);
  public
    constructor Create(AOwner: TNF3e); reintroduce;

    function LerXml: Boolean; override;

    property NF3e: TNF3e read FNF3e write FNF3e;
  end;

implementation

uses
  ACBrXmlBase, ACBrUtil.Base,
  ACBrDFe.Conversao,
  ACBrNF3eConversao;

{ TNF3eXmlReader }

constructor TNF3eXmlReader.Create(AOwner: TNF3e);
begin
  inherited Create;

  FNF3e := AOwner;
end;

procedure TNF3eXmlReader.Ler_Acessante(const ANode: TACBrXmlNode);
var
  sAux: string;
begin
  if not Assigned(ANode) then Exit;

  NF3e.acessante.idAcesso := ObterConteudo(ANode.Childrens.Find('idAcesso'), tcStr);
  NF3e.acessante.idCodCliente := ObterConteudo(ANode.Childrens.Find('idCodCliente'), tcStr);
  NF3e.acessante.tpAcesso := StrTotpAcesso(ObterConteudo(ANode.Childrens.Find('tpAcesso'), tcStr));
  NF3e.acessante.xNomeUC := ObterConteudo(ANode.Childrens.Find('xNomeUC'), tcStr);

  sAux := ObterConteudo(ANode.Childrens.Find('tpClasse'), tcStr);
  NF3e.acessante.tpClasse := tcComercial;
  if sAux <> '' then
    NF3e.acessante.tpClasse := StrTotpClasse(sAux);

  sAux := ObterConteudo(ANode.Childrens.Find('tpSubClasse'), tcStr);
  NF3e.acessante.tpSubClasse := tscResidencial;
  if sAux <> '' then
    NF3e.acessante.tpSubClasse := StrTotpSubClasse(sAux);

  NF3e.acessante.tpFase := StrTotpFase(ObterConteudo(ANode.Childrens.Find('tpFase'), tcStr));
  NF3e.acessante.tpGrpTensao := StrTotpGrpTensao(ObterConteudo(ANode.Childrens.Find('tpGrpTensao'), tcStr));
  NF3e.acessante.tpModTar := StrTotpModTar(ObterConteudo(ANode.Childrens.Find('tpModTar'), tcStr));
  NF3e.acessante.latGPS := ObterConteudo(ANode.Childrens.Find('latGPS'), tcStr);
  NF3e.acessante.longGPS := ObterConteudo(ANode.Childrens.Find('longGPS'), tcStr);
  NF3e.acessante.codRoteiroLeitura := ObterConteudo(ANode.Childrens.Find('codRoteiroLeitura'), tcStr);
end;

procedure TNF3eXmlReader.Ler_Dest(const ANode: TACBrXmlNode);
begin
  NF3e.Dest.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);

  NF3e.Dest.CNPJCPF := ObterCNPJCPF(ANode);

  if NF3e.Dest.CNPJCPF = '' then
    NF3e.Dest.idOutros := ObterConteudo(ANode.Childrens.Find('idOutros'), tcStr);

  NF3e.Dest.indIEDest := StrToindIEDest(ObterConteudo(ANode.Childrens.Find('indIEDest'), tcStr));
  NF3e.Dest.IE := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
  NF3e.Dest.IM := ObterConteudo(ANode.Childrens.Find('IM'), tcStr);
  NF3e.Dest.cNIS := ObterConteudo(ANode.Childrens.Find('cNIS'), tcStr);
  NF3e.Dest.NB := ObterConteudo(ANode.Childrens.Find('NB'), tcStr);
  NF3e.Dest.xNomeAdicional := ObterConteudo(ANode.Childrens.Find('xNomeAdicional'), tcStr);

  Ler_DestEnderDest(ANode.Childrens.Find('enderDest'));
end;

procedure TNF3eXmlReader.Ler_DestEnderDest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NF3e.Dest.enderDest.xLgr := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  NF3e.Dest.enderDest.nro := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  NF3e.Dest.enderDest.xCpl := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  NF3e.Dest.enderDest.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  NF3e.Dest.enderDest.cMun := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  NF3e.Dest.enderDest.xMun := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  NF3e.Dest.enderDest.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  NF3e.Dest.enderDest.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  NF3e.Dest.enderDest.fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  NF3e.Dest.enderDest.email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
end;

procedure TNF3eXmlReader.Ler_Emit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NF3e.Emit.CNPJ := ObterCNPJCPF(ANode);
  NF3e.Emit.IE := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
  NF3e.Emit.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  NF3e.Emit.xFant := ObterConteudo(ANode.Childrens.Find('xFant'), tcStr);

  Ler_EmitEnderEmit(ANode.Childrens.Find('enderEmit'));
end;

procedure TNF3eXmlReader.Ler_EmitEnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NF3e.Emit.enderEmit.xLgr := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  NF3e.Emit.enderEmit.nro := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  NF3e.Emit.enderEmit.xCpl := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  NF3e.Emit.enderEmit.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  NF3e.Emit.EnderEmit.cMun := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  NF3e.Emit.enderEmit.xMun := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  NF3e.Emit.enderEmit.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  NF3e.Emit.enderEmit.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  NF3e.Emit.enderEmit.fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  NF3e.Emit.enderEmit.email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
end;

procedure TNF3eXmlReader.Ler_gGrContrat(const ANode: TACBrXmlNode);
var
  Item: TgGrContratCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := NF3e.gGrContrat.New;

  Item.nContrat := StrToInt(ObterConteudoTag(ANode.Attributes.Items['nContrat']));
  Item.tpGrContrat := StrTotpGrContrat(ObterConteudo(ANode.Childrens.Find('tpGrContrat'), tcStr));
  Item.tpPosTar := StrTotpPosTar(ObterConteudo(ANode.Childrens.Find('tpPosTar'), tcStr));
  Item.qUnidContrat := ObterConteudo(ANode.Childrens.Find('qUnidContrat'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gJudic(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NF3e.gJudic.chNF3e := ObterConteudo(ANode.Childrens.Find('chNF3e'), tcStr);
end;

procedure TNF3eXmlReader.Ler_gMed(const ANode: TACBrXmlNode);
var
  Item: TgMedCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := NF3e.gMed.New;

  Item.nMed := StrToInt(ObterConteudoTag(ANode.Attributes.Items['nMed']));
  Item.idMedidor := ObterConteudo(ANode.Childrens.Find('idMedidor'), tcStr);
  Item.dMedAnt := ObterConteudo(ANode.Childrens.Find('dMedAnt'), tcDat);
  Item.dMedAtu := ObterConteudo(ANode.Childrens.Find('dMedAtu'), tcDat);
end;

procedure TNF3eXmlReader.Ler_gNF(const ANode: TACBrXmlNode);
var
  xData: string;
begin
  if not Assigned(ANode) then Exit;

  NF3e.gSub.CNPJ := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NF3e.gSub.serie := ObterConteudo(ANode.Childrens.Find('serie'), tcStr);
  NF3e.gSub.nNF := ObterConteudo(ANode.Childrens.Find('nNF'), tcInt);

  xData := ObterConteudo(ANode.Childrens.Find('CompetEmis'), tcStr);
  if xData <> '' then
  begin
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    NF3e.gSub.CompetEmis := StrToDate(xData);
  end
  else
    NF3e.gSub.CompetEmis := 0;

  xData := ObterConteudo(ANode.Childrens.Find('CompetApur'), tcStr);
  if xData <> '' then
  begin
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    NF3e.gSub.CompetApur := StrToDate(xData);
  end
  else
    NF3e.gSub.CompetApur := 0;

  NF3e.gSub.hash115 := ObterConteudo(ANode.Childrens.Find('hash115'), tcStr);
end;

procedure TNF3eXmlReader.Ler_gConsumidor(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  Item: TgConsumidorCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := NF3e.gSCEE.gConsumidor.New;

  Item.idAcessGer := ObterConteudo(ANode.Childrens.Find('idAcessGer'), tcStr);
  Item.vPotInst := ObterConteudo(ANode.Childrens.Find('vPotInst'), tcDe3);
  Item.tpFonteEnergia := StrTotpFonteEnergia(ObterConteudo(ANode.Childrens.Find('tpFonteEnergia'), tcStr));

  Item.enerAlocLista.Clear;
  ANodes := ANode.Childrens.FindAllAnyNs('enerAloc');

  for i := 0 to Length(ANodes) - 1 do
  begin
    Item.enerAlocLista.New;

    Item.enerAlocLista[i].enerAloc := ObterConteudo(ANode.Childrens.Find('enerAloc'), tcDe3);
    Item.enerAlocLista[i].tpPosTar := StrTotpPosTar(ObterConteudo(ANode.Childrens.Find('tpPosTar'), tcStr));
  end;

  Item.enerInjetLista.Clear;
  ANodes := ANode.Childrens.FindAllAnyNs('enerInjet');

  for i := 0 to Length(ANodes) - 1 do
  begin
    Item.enerInjetLista.New;

    Item.enerInjetLista[i].enerInjet := ObterConteudo(ANode.Childrens.Find('enerInjet'), tcDe3);
    Item.enerInjetLista[i].tpPosTarInjet := StrTotpPosTar(ObterConteudo(ANode.Childrens.Find('tpPosTarInjet'), tcStr));
  end;
end;

procedure TNF3eXmlReader.Ler_gSaldoCred(const ANode: TACBrXmlNode);
var
  Item: TgSaldoCredCollectionItem;
  xData: string;
begin
  if not Assigned(ANode) then Exit;

  Item := NF3e.gSCEE.gSaldoCred.New;

  Item.tpPosTar := StrTotpPosTar(ObterConteudo(ANode.Childrens.Find('tpPosTar'), tcStr));
  Item.vSaldAnt := ObterConteudo(ANode.Childrens.Find('vSaldAnt'), tcDe4);
  Item.vCredExpirado := ObterConteudo(ANode.Childrens.Find('vCredExpirado'), tcDe4);
  Item.vSaldAtual := ObterConteudo(ANode.Childrens.Find('vSaldAtual'), tcDe4);
  Item.vCredExpirar := ObterConteudo(ANode.Childrens.Find('vCredExpirar'), tcDe4);

  xData := ObterConteudo(ANode.Childrens.Find('CompetExpirar'), tcStr);
  if xData <> '' then
  begin
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    Item.CompetExpirar := StrToDate(xData);
  end
  else
    Item.CompetExpirar := 0;
end;

procedure TNF3eXmlReader.Ler_gSaldoCred2(const ANode: TACBrXmlNode;
  gTipoSaldo: TgTipoSaldoCollectionItem);
var
  Item: TgSaldoCredCollectionItem;
  xData: string;
begin
  if not Assigned(ANode) then Exit;

  Item := gTipoSaldo.gSaldoCred.New;

  Item.tpPosTar := StrTotpPosTar(ObterConteudo(ANode.Childrens.Find('tpPosTar'), tcStr));
  Item.vSaldAnt := ObterConteudo(ANode.Childrens.Find('vSaldAnt'), tcDe4);
  Item.vCredExpirado := ObterConteudo(ANode.Childrens.Find('vCredExpirado'), tcDe4);
  Item.vSaldAtual := ObterConteudo(ANode.Childrens.Find('vSaldAtual'), tcDe4);
  Item.vCredExpirar := ObterConteudo(ANode.Childrens.Find('vCredExpirar'), tcDe4);

  xData := ObterConteudo(ANode.Childrens.Find('CompetExpirar'), tcStr);
  if xData <> '' then
  begin
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    Item.CompetExpirar := StrToDate(xData);
  end
  else
    Item.CompetExpirar := 0;
end;

procedure TNF3eXmlReader.Ler_gTipoSaldo(const ANode: TACBrXmlNode);
var
  i: Integer;
  Item: TgTipoSaldoCollectionItem;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  Item := NF3e.gSCEE.gTipoSaldo.New;

  ANodes := ANode.Childrens.FindAll('gSaldoCred');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_gSaldoCred2(ANodes[i], Item);
  end;
end;

procedure TNF3eXmlReader.Ler_gSCEE(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  NF3e.gSCEE.tpPartComp := StrTotpPartComp(ObterConteudo(ANode.Childrens.Find('tpPartComp'), tcStr));

  ANodes := ANode.Childrens.FindAll('gConsumidor');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_gConsumidor(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('gSaldoCred');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_gSaldoCred(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('gTipoSaldo');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_gTipoSaldo(ANodes[i]);
  end;
end;

procedure TNF3eXmlReader.Ler_gSub(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NF3e.gSub.chNF3e := ObterConteudo(ANode.Childrens.Find('chNF3e'), tcStr);
  NF3e.gSub.motSub := StrToMotSub(ObterConteudo(ANode.Childrens.Find('motSub'), tcStr));

  Ler_gNF(ANode.Childrens.Find('gNF'));
end;

procedure TNF3eXmlReader.Ler_Ide(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  NF3e.ide.cUF := ObterConteudo(ANode.Childrens.Find('cUF'), tcInt);
  NF3e.Ide.tpAmb := StrToTipoAmbiente(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  NF3e.ide.modelo := ObterConteudo(ANode.Childrens.Find('mod'), tcInt);
  NF3e.ide.serie := ObterConteudo(ANode.Childrens.Find('serie'), tcInt);
  NF3e.ide.nNF := ObterConteudo(ANode.Childrens.Find('nNF'), tcInt);
  NF3e.ide.cNF := ObterConteudo(ANode.Childrens.Find('cNF'), tcInt);
  NF3e.Ide.cDV := ObterConteudo(ANode.Childrens.Find('cDV'), tcInt);
  NF3e.ide.dhEmi := ObterConteudo(ANode.Childrens.Find('dhEmi'), tcDatHor);
  NF3e.Ide.tpEmis := StrToTipoEmissao(ok, ObterConteudo(ANode.Childrens.Find('tpEmis'), tcStr));
  NF3e.Ide.nSiteAutoriz := StrToSiteAutorizator(ObterConteudo(ANode.Childrens.Find('nSiteAutoriz'), tcStr));
  NF3e.ide.cMunFG := ObterConteudo(ANode.Childrens.Find('cMunFG'), tcInt);
  NF3e.Ide.finNF3e := StrToFinNF3e(ObterConteudo(ANode.Childrens.Find('finNF3e'), tcStr));
  NF3e.Ide.verProc := ObterConteudo(ANode.Childrens.Find('verProc'), tcStr);
  NF3e.Ide.dhCont := ObterConteudo(ANode.Childrens.Find('dhCont'), tcDatHor);
  NF3e.Ide.xJust := ObterConteudo(ANode.Childrens.Find('xJust'), tcStr);

  // Reforma Tritutária
  Ler_gCompraGov(NF3e.Ide.gCompraGov, ANode.Childrens.Find('gCompraGov'));
end;

procedure TNF3eXmlReader.Ler_InfNF3e(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  Ler_Ide(ANode.Childrens.Find('ide'));
  Ler_Emit(ANode.Childrens.Find('emit'));
  Ler_Dest(ANode.Childrens.Find('dest'));
  Ler_Acessante(ANode.Childrens.Find('acessante'));
  Ler_gSub(ANode.Childrens.Find('gSub'));
  Ler_gJudic(ANode.Childrens.Find('gJudic'));

  ANodes := ANode.Childrens.FindAll('gGrContrat');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_gGrContrat(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('gMed');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_gMed(ANodes[i]);
  end;

  Ler_gSCEE(ANode.Childrens.Find('gSCEE'));

  NF3e.NFDet.Clear;
  ANodes := ANode.Childrens.FindAll('NFdet');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_NFdet(ANodes[i]);
  end;

  Ler_Total(ANode.Childrens.Find('total'));

  Ler_gFat(ANode.Childrens.Find('gFat'));

  Ler_gANEEL(ANode.Childrens.Find('gANEEL'));

  ANodes := ANode.Childrens.FindAll('autXML');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_autXML(ANodes[i]);
  end;

  Ler_InfAdic(ANode.Childrens.Find('infAdic'));

  Ler_gRespTec(ANode.Childrens.Find('gRespTec'));
end;

procedure TNF3eXmlReader.Ler_NFDet(const ANode: TACBrXmlNode);
var
  Item: TNFDetCollectionItem;
  ADetNodes: TACBrXmlNodeArray;
  ADetChildrensNodes: TACBrXmlNodeArray;
  i: Integer;
  j: Integer;
  ANodeNivel3: TACBrXmlNode;
  ANodeNivel4: TACBrXmlNode;
  ANodeNivel5: TACBrXmlNode;
  ANodeNivel6: TACBrXmlNode;
  snItemAnt, sAux: string;
  ItemgTarif: TgTarifCollectionItem;
  ItemgContab: TgContabCollectionItem;
  ItemgProc: TgProcCollectionItem;
  ItemgAdBand: TgAdBandCollectionItem;

  procedure LerICMS(sIcms: String; ANodeImposto: TImposto);
  begin
    ANodeNivel5 := ANodeNivel4.Childrens.Find(sIcms);

    if NodeNaoEncontrado(ANodeNivel5) then
      Exit;

    ANodeImposto.ICMS.CST        := StrToCSTICMS(ObterConteudo(ANodeNivel5.Childrens.Find('CST'), tcStr));
    ANodeImposto.ICMS.vBC        := ObterConteudo(ANodeNivel5.Childrens.Find('vBC'), tcDe2);
    ANodeImposto.ICMS.pICMS      := ObterConteudo(ANodeNivel5.Childrens.Find('pICMS'), tcDe2);
    ANodeImposto.ICMS.vICMS      := ObterConteudo(ANodeNivel5.Childrens.Find('vICMS'), tcDe2);
    ANodeImposto.ICMS.pFCP       := ObterConteudo(ANodeNivel5.Childrens.Find('pFCP'), tcDe2);
    ANodeImposto.ICMS.vFCP       := ObterConteudo(ANodeNivel5.Childrens.Find('vFCP'), tcDe2);
    ANodeImposto.ICMS.vBCST      := ObterConteudo(ANodeNivel5.Childrens.Find('vBCST'), tcDe2);
    ANodeImposto.ICMS.pICMSST    := ObterConteudo(ANodeNivel5.Childrens.Find('pICMSST'), tcDe2);
    ANodeImposto.ICMS.vICMSST    := ObterConteudo(ANodeNivel5.Childrens.Find('vICMSST'), tcDe2);
    ANodeImposto.ICMS.pFCPST     := ObterConteudo(ANodeNivel5.Childrens.Find('pFCPST'), tcDe2);
    ANodeImposto.ICMS.vFCPST     := ObterConteudo(ANodeNivel5.Childrens.Find('vFCPST'), tcDe2);
    ANodeImposto.ICMS.pRedBC     := ObterConteudo(ANodeNivel5.Childrens.Find('pRedBC'), tcDe2);
    ANodeImposto.ICMS.vICMSDeson := ObterConteudo(ANodeNivel5.Childrens.Find('vICMSDeson'), tcDe2);
    ANodeImposto.ICMS.cBenef     := ObterConteudo(ANodeNivel5.Childrens.Find('cBenef'), tcStr);
    ANodeImposto.ICMS.vBCSTRET := ObterConteudo(ANodeNivel5.Childrens.Find('vBCSTRET'), tcDe2);
    ANodeImposto.ICMS.pICMSSTRet := ObterConteudo(ANodeNivel5.Childrens.Find('pICMSSTRet'), tcDe2);
    ANodeImposto.ICMS.vICMSSubstituto := ObterConteudo(ANodeNivel5.Childrens.Find('vICMSSubstituto'), tcDe2);
    ANodeImposto.ICMS.vICMSSTRET := ObterConteudo(ANodeNivel5.Childrens.Find('vICMSSTRET'), tcDe2);
    ANodeImposto.ICMS.vBCFCPSTRet := ObterConteudo(ANodeNivel5.Childrens.Find('vBCFCPSTRet'), tcDe2);
    ANodeImposto.ICMS.pFCPSTRet := ObterConteudo(ANodeNivel5.Childrens.Find('pFCPSTRet'), tcDe2);
    ANodeImposto.ICMS.vFCPSTRet := ObterConteudo(ANodeNivel5.Childrens.Find('vFCPSTRet'), tcDe2);
    ANodeImposto.ICMS.pRedBCEfet := ObterConteudo(ANodeNivel5.Childrens.Find('pRedBCEfet'), tcDe2);
    ANodeImposto.ICMS.vBCEfet := ObterConteudo(ANodeNivel5.Childrens.Find('vBCEfet'), tcDe2);
    ANodeImposto.ICMS.pICMSEfet := ObterConteudo(ANodeNivel5.Childrens.Find('pICMSEfet'), tcDe2);
    ANodeImposto.ICMS.vICMSEfet := ObterConteudo(ANodeNivel5.Childrens.Find('vICMSEfet'), tcDe2);
  end;
begin
  Item := NF3e.NFDet.New;
  Item.chNF3eAnt   := ObterConteudoTag(ANode.Attributes.Items['chNF3eAnt']);
  Item.mod6HashAnt := ObterConteudoTag(ANode.Attributes.Items['mod6HashAnt']);

  Item.Det.Clear;
  ADetNodes := ANode.Childrens.FindAll('det');
  for i := 0 to Length(ADetNodes) - 1 do
  begin
    with Item.Det.New do
    begin
      nItem := StrToInt(ObterConteudoTag(ADetNodes[i].Attributes.Items['nItem']));

      ANodeNivel3 := ADetNodes[i].Childrens.Find('gAjusteNF3eAnt');

      if NodeNaoEncontrado(ANodeNivel3) then
        gAjusteNF3eAnt.tpAjuste := taNenhum
      else
      begin
        gAjusteNF3eAnt.tpAjuste := StrTotpAjuste(ObterConteudo(ANodeNivel3.Childrens.Find('tpAjuste'), tcStr));
        gAjusteNF3eAnt.motAjuste := StrToMotAjuste(ObterConteudo(ANodeNivel3.Childrens.Find('motAjuste'), tcStr));
      end;

      ANodeNivel3 := ADetNodes[i].Childrens.Find('detItemAnt');

      if not(NodeNaoEncontrado(ANodeNivel3)) then
      begin
        detItemAnt.nItemAnt    := StrToInt(ObterConteudoTag(ANodeNivel3.Attributes.Items['nItemAnt']));
        detItemAnt.vItem       := ObterConteudo(ANodeNivel3.Childrens.Find('vItem'), tcDe10);
        detItemAnt.qFaturada   := ObterConteudo(ANodeNivel3.Childrens.Find('qFaturada'), tcDe4);
        detItemAnt.vProd       := ObterConteudo(ANodeNivel3.Childrens.Find('vProd'), tcDe10);
        detItemAnt.cClass      := ObterConteudo(ANodeNivel3.Childrens.Find('cClass'), tcInt);
        detItemAnt.vBC         := ObterConteudo(ANodeNivel3.Childrens.Find('vBC'), tcDe2);
        detItemAnt.pICMS       := ObterConteudo(ANodeNivel3.Childrens.Find('pICMS'), tcDe2);
        detItemAnt.vICMS       := ObterConteudo(ANodeNivel3.Childrens.Find('vICMS'), tcDe2);
        detItemAnt.vFCP        := ObterConteudo(ANodeNivel3.Childrens.Find('vFCP'), tcDe2);
        detItemAnt.vBCST       := ObterConteudo(ANodeNivel3.Childrens.Find('vBCST'), tcDe2);
        detItemAnt.vICMSST     := ObterConteudo(ANodeNivel3.Childrens.Find('vICMSST'), tcDe2);
        detItemAnt.vFCPST      := ObterConteudo(ANodeNivel3.Childrens.Find('vFCPST'), tcDe2);
        detItemAnt.vPIS        := ObterConteudo(ANodeNivel3.Childrens.Find('vPIS'), tcDe2);
        detItemAnt.vPISEfet    := ObterConteudo(ANodeNivel3.Childrens.Find('vPISEfet'), tcDe2);
        detItemAnt.vCOFINS     := ObterConteudo(ANodeNivel3.Childrens.Find('vCOFINS'), tcDe2);
        detItemAnt.vCOFINSEfet := ObterConteudo(ANodeNivel3.Childrens.Find('vCOFINSEfet'), tcDe2);

        sAux := ObterConteudo(ANodeNivel3.Childrens.Find('indDevolucao'), tcStr);
        detItemAnt.indDevolucao := tiNao;
        if sAux = '1' then
          detItemAnt.indDevolucao := tiSim;

        ANodeNivel4 := ANodeNivel3.Childrens.Find('retTrib');

        if not(NodeNaoEncontrado(ANodeNivel4)) then
        begin
          detItemAnt.retTrib.vRetPIS    := ObterConteudo(ANodeNivel4.Childrens.Find('vRetPIS'), tcDe2);
          detItemAnt.retTrib.vRetCOFINS := ObterConteudo(ANodeNivel4.Childrens.Find('vRetCOFINS'), tcDe2);
          detItemAnt.retTrib.vRetCSLL   := ObterConteudo(ANodeNivel4.Childrens.Find('vRetCSLL'), tcDe2);
          detItemAnt.retTrib.vBCIRRF    := ObterConteudo(ANodeNivel4.Childrens.Find('vBCIRRF'), tcDe2);
          detItemAnt.retTrib.vIRRF      := ObterConteudo(ANodeNivel4.Childrens.Find('vIRRF'), tcDe2);
        end;
      end;

      ANodeNivel3 := ADetNodes[i].Childrens.Find('detItem');

      if not(NodeNaoEncontrado(ANodeNivel3)) then
      begin
        snItemAnt := ObterConteudoTag(ANodeNivel3.Attributes.Items['nItemAnt']);

        if snItemAnt <> '' then
          detItem.nItemAnt  := StrToInt(snItemAnt);

        detItem.infAdProd := ObterConteudo(ANodeNivel3.Childrens.Find('infAdProd'), tcStr);

        detItem.gTarif.Clear;
        ADetChildrensNodes := ANodeNivel3.Childrens.FindAll('gTarif');

        for j := 0 to Length(ADetChildrensNodes) - 1 do
        begin
          ItemgTarif := detItem.gTarif.New;

          ItemgTarif.dIniTarif   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('dIniTarif'), tcDat);
          ItemgTarif.dFimTarif   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('dFimTarif'), tcDat);
          ItemgTarif.tpAto       := StrTotpAto(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpAto'), tcStr));
          ItemgTarif.nAto        := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('nAto'), tcStr);
          ItemgTarif.anoAto      := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('anoAto'), tcInt);
          ItemgTarif.tpTarif     := StrTotpTarif(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpTarif'), tcStr));
          ItemgTarif.cPosTarif   := StrTocPosTarif(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('cPosTarif'), tcStr));
          ItemgTarif.uMed        := StrTouMed(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('uMed'), tcStr));
          ItemgTarif.vTarifHom   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vTarifHom'), tcDe8);
          ItemgTarif.vTarifAplic := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vTarifAplic'), tcDe8);

          sAux := ObterConteudo(ANodeNivel3.Childrens.Find('motDifTarif'), tcStr);
          ItemgTarif.motDifTarif := mdtDecisaoJudicial;
          if sAux <> '' then
            ItemgTarif.motDifTarif := StrTomotDifTarif(sAux);
        end;

        detItem.gAdBand.Clear;
        ADetChildrensNodes := ANodeNivel3.Childrens.FindAll('gAdBand');

        for j := 0 to Length(ADetChildrensNodes) - 1 do
        begin
          ItemgAdBand := detItem.gAdBand.New;

          ItemgAdBand.dIniAdBand   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('dIniAdBand'), tcDat);
          ItemgAdBand.dFimAdBand   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('dFimAdBand'), tcDat);
          ItemgAdBand.tpBand       := StrTotpBand(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpBand'), tcStr));
          ItemgAdBand.vAdBand      := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vAdBand'), tcDe10);
          ItemgAdBand.vAdBandAplic := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vAdBandAplic'), tcDe10);

          sAux := ObterConteudo(ANodeNivel3.Childrens.Find('motDifBand'), tcStr);
          ItemgAdBand.motDifBand := mdbDecisaoJudicial;
          if sAux <> '' then
            ItemgAdBand.motDifBand := StrTomotDifBand(sAux);
        end;

        ANodeNivel4 := ANodeNivel3.Childrens.Find('prod');

        if not(NodeNaoEncontrado(ANodeNivel4)) then
        begin
          detItem.Prod.indOrigemQtd := StrToindOrigemQtd(ObterConteudo(ANodeNivel4.Childrens.Find('indOrigemQtd'), tcStr));
          detItem.Prod.cProd        := ObterConteudo(ANodeNivel4.Childrens.Find('cProd'), tcStr);
          detItem.Prod.xProd        := ObterConteudo(ANodeNivel4.Childrens.Find('xProd'), tcStr);
          detItem.Prod.cClass       := ObterConteudo(ANodeNivel4.Childrens.Find('cClass'), tcInt);
          detItem.Prod.CFOP         := ObterConteudo(ANodeNivel4.Childrens.Find('CFOP'), tcInt);
          detItem.Prod.uMed         := StrTouMedFat(ObterConteudo(ANodeNivel4.Childrens.Find('uMed'), tcStr));
          detItem.Prod.qFaturada    := ObterConteudo(ANodeNivel4.Childrens.Find('qFaturada'), tcDe4);
          detItem.Prod.vItem        := ObterConteudo(ANodeNivel4.Childrens.Find('vItem'), tcDe10);
          detItem.Prod.vProd        := ObterConteudo(ANodeNivel4.Childrens.Find('vProd'), tcDe10);

          sAux := ObterConteudo(ANodeNivel4.Childrens.Find('indDevolucao'), tcStr);
          detItem.Prod.indDevolucao := tiNao;
          if sAux = '1' then
            detItem.Prod.indDevolucao := tiSim;

          sAux := ObterConteudo(ANodeNivel4.Childrens.Find('indPrecoACL'), tcStr);
          detItem.Prod.indPrecoACL := tiNao;
          if sAux = '1' then
            detItem.Prod.indPrecoACL := tiSim;

          ANodeNivel5 := ANodeNivel4.Childrens.Find('gMedicao');

          if not(NodeNaoEncontrado(ANodeNivel5)) then
          begin
            detItem.Prod.gMedicao.nMed := ObterConteudo(ANodeNivel5.Childrens.Find('nMed'), tcInt);
            detItem.Prod.gMedicao.nContrat := ObterConteudo(ANodeNivel5.Childrens.Find('nContrat'), tcInt);

            sAux := ObterConteudo(ANodeNivel3.Childrens.Find('tpMotNaoLeitura'), tcStr);
            detItem.Prod.gMedicao.tpMotNaoLeitura := tmConsumidor;
            if sAux <> '' then
              detItem.Prod.gMedicao.tpMotNaoLeitura := StrTotpMotNaoLeitura(sAux);

            ANodeNivel6 := ANodeNivel5.Childrens.Find('gMedida');

            if not(NodeNaoEncontrado(ANodeNivel6)) then
            begin
              detItem.Prod.gMedicao.tpGrMed         := StrTotpGrMed(ObterConteudo(ANodeNivel6.Childrens.Find('tpGrMed'), tcStr));
              detItem.Prod.gMedicao.cPosTarif       := StrTocPosTarif(ObterConteudo(ANodeNivel6.Childrens.Find('cPosTarif'), tcStr));
              detItem.Prod.gMedicao.uMed            := StrTouMedFat(ObterConteudo(ANodeNivel6.Childrens.Find('uMed'), tcStr));
              detItem.Prod.gMedicao.vMedAnt         := ObterConteudo(ANodeNivel6.Childrens.Find('vMedAnt'), tcDe2);
              detItem.Prod.gMedicao.vMedAtu         := ObterConteudo(ANodeNivel6.Childrens.Find('vMedAtu'), tcDe2);
              detItem.Prod.gMedicao.vConst          := ObterConteudo(ANodeNivel6.Childrens.Find('vConst'), tcDe6);
              detItem.Prod.gMedicao.vMed            := ObterConteudo(ANodeNivel6.Childrens.Find('vMed'), tcDe2);
              detItem.Prod.gMedicao.pPerdaTran      := ObterConteudo(ANodeNivel6.Childrens.Find('pPerdaTran'), tcDe2);
              detItem.Prod.gMedicao.vMedPerdaTran   := ObterConteudo(ANodeNivel6.Childrens.Find('vMedPerdaTran'), tcDe2);
              detItem.Prod.gMedicao.vMedPerdaTec    := ObterConteudo(ANodeNivel6.Childrens.Find('vMedPerdaTec'), tcDe2);
            end;
          end;
        end;

        ANodeNivel4 := ANodeNivel3.Childrens.Find('imposto');

        if not(NodeNaoEncontrado(ANodeNivel4)) then
        begin
          sAux := ObterConteudo(ANodeNivel4.Childrens.Find('indSemCST'), tcStr);
          detItem.Imposto.ICMS.indSemCST := tiNao;
          if sAux = '1' then
            detItem.Imposto.ICMS.indSemCST := tiSim;

          if detItem.Imposto.ICMS.indSemCST = tiNao then
          begin
            LerICMS('ICMS00', detItem.Imposto);
            LerICMS('ICMS10', detItem.Imposto);
            LerICMS('ICMS20', detItem.Imposto);
            LerICMS('ICMS40', detItem.Imposto);
            LerICMS('ICMS51', detItem.Imposto);
            LerICMS('ICMS60', detItem.Imposto);
            LerICMS('ICMS90', detItem.Imposto);
          end;

          ANodeNivel5 := ANodeNivel4.Childrens.Find('PIS');

          if not(NodeNaoEncontrado(ANodeNivel5)) then
          begin
            detItem.Imposto.PIS.CST  := StrToCSTPIS(ObterConteudo(ANodeNivel5.Childrens.Find('CST'), tcStr));
            detItem.Imposto.PIS.vBC  := ObterConteudo(ANodeNivel5.Childrens.Find('vBC'), tcDe2);
            detItem.Imposto.PIS.pPIS := ObterConteudo(ANodeNivel5.Childrens.Find('pPIS'), tcDe4);
            detItem.Imposto.PIS.vPIS := ObterConteudo(ANodeNivel5.Childrens.Find('vPIS'), tcDe2);
          end;

          ANodeNivel5 := ANodeNivel4.Childrens.Find('PISEfet');

          if not(NodeNaoEncontrado(ANodeNivel5)) then
          begin
            detItem.Imposto.PISEfet.vBCPISEfet := ObterConteudo(ANodeNivel5.Childrens.Find('vBCPISEfet'), tcDe2);
            detItem.Imposto.PISEfet.pPISEfet   := ObterConteudo(ANodeNivel5.Childrens.Find('pPISEfet'), tcDe4);
            detItem.Imposto.PISEfet.vPISEfet   := ObterConteudo(ANodeNivel5.Childrens.Find('vPISEfet'), tcDe2);
          end;

          ANodeNivel5 := ANodeNivel4.Childrens.Find('COFINS');

          if not(NodeNaoEncontrado(ANodeNivel5)) then
          begin
            detItem.Imposto.COFINS.CST     := StrToCSTCOFINS(ObterConteudo(ANodeNivel5.Childrens.Find('CST'), tcStr));
            detItem.Imposto.COFINS.vBC     := ObterConteudo(ANodeNivel5.Childrens.Find('vBC'), tcDe2);
            detItem.Imposto.COFINS.pCOFINS := ObterConteudo(ANodeNivel5.Childrens.Find('pCOFINS'), tcDe4);
            detItem.Imposto.COFINS.vCOFINS := ObterConteudo(ANodeNivel5.Childrens.Find('vCOFINS'), tcDe2);
          end;

          ANodeNivel5 := ANodeNivel4.Childrens.Find('COFINSEfet');

          if not(NodeNaoEncontrado(ANodeNivel5)) then
          begin
            detItem.Imposto.COFINSEfet.vBCCOFINSEfet := ObterConteudo(ANodeNivel5.Childrens.Find('vBCCOFINSEfet'), tcDe2);
            detItem.Imposto.COFINSEfet.pCOFINSEfet   := ObterConteudo(ANodeNivel5.Childrens.Find('pCOFINSEfet'), tcDe4);
            detItem.Imposto.COFINSEfet.vCOFINSEfet   := ObterConteudo(ANodeNivel5.Childrens.Find('vCOFINSEfet'), tcDe2);
          end;

          ANodeNivel5 := ANodeNivel4.Childrens.Find('retTrib');

          if not(NodeNaoEncontrado(ANodeNivel5)) then
          begin
            detItem.Imposto.retTrib.vRetPIS    := ObterConteudo(ANodeNivel5.Childrens.Find('vRetPIS'), tcDe2);
            detItem.Imposto.retTrib.vRetCOFINS := ObterConteudo(ANodeNivel5.Childrens.Find('vRetCOFINS'), tcDe2);
            detItem.Imposto.retTrib.vRetCSLL   := ObterConteudo(ANodeNivel5.Childrens.Find('vRetCSLL'), tcDe2);
            detItem.Imposto.retTrib.vBCIRRF    := ObterConteudo(ANodeNivel5.Childrens.Find('vBCIRRF'), tcDe2);
            detItem.Imposto.retTrib.vIRRF      := ObterConteudo(ANodeNivel5.Childrens.Find('vIRRF'), tcDe2);
          end;

          // Reforma Tributária
          Ler_IBSCBS(ANodeNivel4.Childrens.Find('IBSCBS'), detItem.Imposto.IBSCBS);
        end;

        ANodeNivel4 := ANodeNivel3.Childrens.Find('gProcRef');

        if not(NodeNaoEncontrado(ANodeNivel4)) then
        begin
          detItem.gProcRef.vItem        := ObterConteudo(ANodeNivel4.Childrens.Find('vItem'), tcDe10);
          detItem.gProcRef.qFaturada    := ObterConteudo(ANodeNivel4.Childrens.Find('qFaturada'), tcDe4);
          detItem.gProcRef.vProd        := ObterConteudo(ANodeNivel4.Childrens.Find('vProd'), tcDe10);

          sAux := ObterConteudo(ANodeNivel4.Childrens.Find('indDevolucao'), tcStr);
          detItem.gProcRef.indDevolucao := tiNao;
          if sAux = '1' then
            detItem.gProcRef.indDevolucao := tiSim;

          detItem.gProcRef.vBC          := ObterConteudo(ANodeNivel4.Childrens.Find('vBC'), tcDe2);
          detItem.gProcRef.pICMS        := ObterConteudo(ANodeNivel4.Childrens.Find('pICMS'), tcDe2);
          detItem.gProcRef.vICMS        := ObterConteudo(ANodeNivel4.Childrens.Find('vICMS'), tcDe2);
          detItem.gProcRef.pFCP         := ObterConteudo(ANodeNivel4.Childrens.Find('pFCP'), tcDe2);
          detItem.gProcRef.vFCP         := ObterConteudo(ANodeNivel4.Childrens.Find('vFCP'), tcDe2);
          detItem.gProcRef.vBCST        := ObterConteudo(ANodeNivel4.Childrens.Find('vBCST'), tcDe2);
          detItem.gProcRef.pICMSST      := ObterConteudo(ANodeNivel4.Childrens.Find('pICMSST'), tcDe2);
          detItem.gProcRef.vICMSST      := ObterConteudo(ANodeNivel4.Childrens.Find('vICMSST'), tcDe2);
          detItem.gProcRef.pFCPST       := ObterConteudo(ANodeNivel4.Childrens.Find('pFCPST'), tcDe2);
          detItem.gProcRef.vFCPST       := ObterConteudo(ANodeNivel4.Childrens.Find('vFCPST'), tcDe2);
          detItem.gProcRef.vPIS         := ObterConteudo(ANodeNivel4.Childrens.Find('vPIS'), tcDe2);
          detItem.gProcRef.vPISEfet     := ObterConteudo(ANodeNivel4.Childrens.Find('vPISEfet'), tcDe2);
          detItem.gProcRef.vCOFINS      := ObterConteudo(ANodeNivel4.Childrens.Find('vCOFINS'), tcDe2);
          detItem.gProcRef.vCOFINSEfet  := ObterConteudo(ANodeNivel4.Childrens.Find('vCOFINSEfet'), tcDe2);

          detItem.gProcRef.gProc.Clear;
          ADetChildrensNodes := ANodeNivel4.Childrens.FindAll('gProc');

          for j := 0 to Length(ADetChildrensNodes) - 1 do
          begin
            ItemgProc := detItem.gProcRef.gProc.New;

            ItemgProc.tpProc    := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpProc'), tcStr);
            ItemgProc.nProcesso := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('nProcesso'), tcStr);
          end;
        end;

        detItem.gContab.Clear;
        ADetChildrensNodes := ANodeNivel3.Childrens.FindAll('gContab');

        for j := 0 to Length(ADetChildrensNodes) - 1 do
        begin
          ItemgContab := detItem.gContab.New;

          ItemgContab.cContab := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('cContab'), tcStr);
          ItemgContab.xContab := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('xContab'), tcStr);
          ItemgContab.vContab := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vContab'), tcDe2);
          ItemgContab.tpLanc  := StrTotpLanc(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpLanc'), tcStr));
        end;
      end;
    end;
  end;
end;

procedure TNF3eXmlReader.Ler_Total(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  NF3e.Total.vProd := ObterConteudo(ANode.Childrens.Find('vProd'), tcDe2);

  AuxNode := ANode.Childrens.Find('ICMSTot');

  if AuxNode <> nil then
  begin
    NF3e.Total.vBC := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    NF3e.Total.vICMS := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    NF3e.Total.vICMSDeson := ObterConteudo(AuxNode.Childrens.Find('vICMSDeson'), tcDe2);
    NF3e.Total.vFCP := ObterConteudo(AuxNode.Childrens.Find('vFCP'), tcDe2);
    NF3e.Total.vBCST := ObterConteudo(AuxNode.Childrens.Find('vBCST'), tcDe2);
    NF3e.Total.vST := ObterConteudo(AuxNode.Childrens.Find('vST'), tcDe2);
    NF3e.Total.vFCPST := ObterConteudo(AuxNode.Childrens.Find('vFCPST'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('vRetTribTot');

  if AuxNode <> nil then
  begin
    NF3e.Total.vRetPIS := ObterConteudo(AuxNode.Childrens.Find('vRetPIS'), tcDe2);
    NF3e.Total.vRetCOFINS := ObterConteudo(AuxNode.Childrens.Find('vRetCOFINS'), tcDe2);
    NF3e.Total.vRetCSLL := ObterConteudo(AuxNode.Childrens.Find('vRetCSLL'), tcDe2);
    NF3e.Total.vIRRF := ObterConteudo(AuxNode.Childrens.Find('vIRRF'), tcDe2);
  end;

  NF3e.Total.vCOFINS := ObterConteudo(ANode.Childrens.Find('vCOFINS'), tcDe2);
  NF3e.Total.vCOFINSEfet := ObterConteudo(ANode.Childrens.Find('vCOFINSEfet'), tcDe2);
  NF3e.Total.vPIS := ObterConteudo(ANode.Childrens.Find('vPIS'), tcDe2);
  NF3e.Total.vPISEfet := ObterConteudo(ANode.Childrens.Find('vPISEfet'), tcDe2);
  NF3e.Total.vNF := ObterConteudo(ANode.Childrens.Find('vNF'), tcDe2);
  NF3e.Total.vTotDFe := ObterConteudo(ANode.Childrens.Find('vTotDFe'), tcDe2);

  // Reforma Tributária
  Ler_IBSCBSTot(ANode.Childrens.Find('IBSCBSTot'), NF3e.total.IBSCBSTot);
end;

procedure TNF3eXmlReader.Ler_gFat(const ANode: TACBrXmlNode);
var
  xData: string;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  xData := ObterConteudo(ANode.Childrens.Find('CompetFat'), tcStr);
  if xData <> '' then
  begin
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    NF3e.gFat.CompetFat := StrToDate(xData);
  end
  else
    NF3e.gFat.CompetFat := 0;

  NF3e.gFat.dVencFat := ObterConteudo(ANode.Childrens.Find('dVencFat'), tcDat);
  NF3e.gFat.dApresFat := ObterConteudo(ANode.Childrens.Find('dApresFat'), tcDat);
  NF3e.gFat.dProxLeitura := ObterConteudo(ANode.Childrens.Find('dProxLeitura'), tcDat);
  NF3e.gFat.nFat := ObterConteudo(ANode.Childrens.Find('nFat'), tcStr);
  NF3e.gFat.codBarras := ObterConteudo(ANode.Childrens.Find('codBarras'), tcStr);
  NF3e.gFat.codDebAuto := ObterConteudo(ANode.Childrens.Find('codDebAuto'), tcStr);
  NF3e.gFat.codBanco := ObterConteudo(ANode.Childrens.Find('codBanco'), tcStr);
  NF3e.gFat.codAgencia := ObterConteudo(ANode.Childrens.Find('codAgencia'), tcStr);

  AuxNode := ANode.Childrens.Find('enderCorresp');

  if AuxNode <> nil then
  begin
    NF3e.gFat.enderCorresp.xLgr := ObterConteudo(AuxNode.Childrens.Find('xLgr'), tcStr);
    NF3e.gFat.enderCorresp.nro := ObterConteudo(AuxNode.Childrens.Find('nro'), tcStr);
    NF3e.gFat.enderCorresp.xCpl := ObterConteudo(AuxNode.Childrens.Find('xCpl'), tcStr);
    NF3e.gFat.enderCorresp.xBairro := ObterConteudo(AuxNode.Childrens.Find('xBairro'), tcStr);
    NF3e.gFat.enderCorresp.cMun := ObterConteudo(AuxNode.Childrens.Find('cMun'), tcInt);
    NF3e.gFat.enderCorresp.xMun := ObterConteudo(AuxNode.Childrens.Find('xMun'), tcStr);
    NF3e.gFat.enderCorresp.CEP := ObterConteudo(AuxNode.Childrens.Find('CEP'), tcInt);
    NF3e.gFat.enderCorresp.UF := ObterConteudo(AuxNode.Childrens.Find('UF'), tcStr);
    NF3e.gFat.enderCorresp.fone := ObterConteudo(AuxNode.Childrens.Find('fone'), tcStr);
    NF3e.gFat.enderCorresp.email := ObterConteudo(AuxNode.Childrens.Find('email'), tcStr);
  end;

  AuxNode := ANode.Childrens.Find('gPIX');

  if AuxNode <> nil then
  begin
    NF3e.gFat.gPIX.urlQRCodePIX := ObterConteudo(AuxNode.Childrens.Find('urlQRCodePIX'), tcStr);
  end;
end;

procedure TNF3eXmlReader.Ler_gANEEL(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  ANodes := ANode.Childrens.FindAll('gHistFat');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_gHistFat(ANodes[i]);
  end;
end;

procedure TNF3eXmlReader.Ler_gHistFat(const ANode: TACBrXmlNode);
var
  Item: TgHistFatCollectionItem;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  Item := NF3e.gANEEL.gHistFat.New;

  Item.xGrandFat := ObterConteudo(ANode.Childrens.Find('xGrandFat'), tcStr);

  ANodes := ANode.Childrens.FindAll('gGrandFat');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_gGrandFat(ANodes[i], NF3e.gANEEL.gHistFat.Count -1);
  end;
end;

procedure TNF3eXmlReader.Ler_gGrandFat(const ANode: TACBrXmlNode; I: Integer);
var
  Item: TgGrandFatCollectionItem;
  xData: string;
begin
  if not Assigned(ANode) then Exit;

  Item := NF3e.gANEEL.gHistFat[I].gGrandFat.New;

  xData := ObterConteudo(ANode.Childrens.Find('CompetFat'), tcStr);
  if xData <> '' then
  begin
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    Item.CompetFat := StrToDate(xData);
  end
  else
    Item.CompetFat := 0;

  Item.vFat := ObterConteudo(ANode.Childrens.Find('vFat'), tcDe2);
  Item.uMed := StrTouMedFat(ObterConteudo(ANode.Childrens.Find('uMed'), tcStr));
  Item.qtdDias := ObterConteudo(ANode.Childrens.Find('qtdDias'), tcInt);
end;

procedure TNF3eXmlReader.Ler_autXML(const ANode: TACBrXmlNode);
var
  Item: TautXMLCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := NF3e.autXML.New;

  Item.CNPJCPF := ObterCNPJCPF(ANode);
end;

procedure TNF3eXmlReader.Ler_InfAdic(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NF3e.InfAdic.infAdFisco := ObterConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);
  NF3e.InfAdic.infCpl := ObterConteudo(ANode.Childrens.Find('infCpl'), tcStr);
end;

procedure TNF3eXmlReader.Ler_gRespTec(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NF3e.infRespTec.CNPJ     := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NF3e.infRespTec.xContato := ObterConteudo(ANode.Childrens.Find('xContato'), tcStr);
  NF3e.infRespTec.email    := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  NF3e.infRespTec.fone     := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  NF3e.infRespTec.idCSRT   := ObterConteudo(ANode.Childrens.Find('idCSRT'), tcInt);
  NF3e.infRespTec.hashCSRT := ObterConteudo(ANode.Childrens.Find('hashCSRT'), tcStr);
end;

procedure TNF3eXmlReader.Ler_InfNF3eSupl(const ANode: TACBrXmlNode);
var
 sQrCode: string;
begin
  if not Assigned(ANode) then Exit;

  sQrCode := ObterConteudo(ANode.Childrens.Find('qrCodNF3e'), tcStr);
  sQrCode := StringReplace(sQrCode, '<![CDATA[', '', []);
  sQrCode := StringReplace(sQrCode, ']]>', '', []);

  NF3e.infNF3eSupl.qrCodNF3e := sQrCode;
end;

procedure TNF3eXmlReader.Ler_ProtNF3e(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  NF3e.procNF3e.tpAmb    := StrToTipoAmbiente(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  NF3e.procNF3e.verAplic := ObterConteudo(ANode.Childrens.Find('verAplic'), tcStr);
  NF3e.procNF3e.chDFe    := ObterConteudo(ANode.Childrens.Find('chNF3e'), tcStr);
  NF3e.procNF3e.dhRecbto := ObterConteudo(ANode.Childrens.Find('dhRecbto'), tcDatHor);
  NF3e.procNF3e.nProt    := ObterConteudo(ANode.Childrens.Find('nProt'), tcStr);
  NF3e.procNF3e.digVal   := ObterConteudo(ANode.Childrens.Find('digVal'), tcStr);
  NF3e.procNF3e.cStat    := ObterConteudo(ANode.Childrens.Find('cStat'), tcInt);
  NF3e.procNF3e.xMotivo  := ObterConteudo(ANode.Childrens.Find('xMotivo'), tcStr);
  NF3e.procNF3e.cMsg     := ObterConteudo(ANode.Childrens.Find('cMsg'), tcInt);
  NF3e.procNF3e.xMsg     := ObterConteudo(ANode.Childrens.Find('xMsg'), tcStr);
end;

function TNF3eXmlReader.LerXml: Boolean;
Var
  NF3eNode, infNF3eNode: TACBrXmlNode;
  att: TACBrXmlAttribute;
begin
  if not Assigned(FNF3e) then
    raise Exception.Create('Destino não informado, informe a classe [TNF3e] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml da NF3e não carregado.');

  Result := False;
  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'nf3eProc' then
  begin
    Ler_ProtNF3e(Document.Root.Childrens.Find('protNF3e').Childrens.Find('infProt'));
    NF3eNode := Document.Root.Childrens.Find('NF3e');
  end
  else
  begin
    NF3eNode := Document.Root;
  end;

  if NF3eNode <> nil then
  begin
    infNF3eNode := NF3eNode.Childrens.Find('infNF3e');

    if infNF3eNode = nil then
      raise Exception.Create('Arquivo xml incorreto.');

    att := infNF3eNode.Attributes.Items['Id'];
    if att = nil then
      raise Exception.Create('Não encontrei o atributo: Id');

    NF3e.infNF3e.Id := att.Content;

    att := infNF3eNode.Attributes.Items['versao'];
    if att = nil then
      raise Exception.Create('Não encontrei o atributo: versao');

    NF3e.infNF3e.Versao := StringToFloat(att.Content);

    Ler_InfNF3e(infNF3eNode);
    Ler_InfNF3eSupl(NF3eNode.Childrens.Find('infNF3eSupl'));

    LerSignature(NF3eNode.Childrens.Find('Signature'), NF3e.signature);

    Result := True;
  end;
end;

function TNF3eXmlReader.NodeNaoEncontrado(const ANode: TACBrXmlNode): Boolean;
begin
  Result := not Assigned(ANode);
end;

// Reforma Tributária
procedure TNF3eXmlReader.Ler_gCompraGov(gCompraGov: TgCompraGovReduzido;
  const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  gCompraGov.tpEnteGov := StrTotpEnteGov(ObterConteudo(ANode.Childrens.Find('tpEnteGov'), tcStr));
  gCompraGov.pRedutor := ObterConteudo(ANode.Childrens.Find('pRedutor'), tcDe4);
end;

procedure TNF3eXmlReader.Ler_IBSCBS(const ANode: TACBrXmlNode; IBSCBS: TIBSCBS);
begin
  if not Assigned(ANode) then Exit;

  IBSCBS.CST := StrToCSTIBSCBS(ObterConteudo(ANode.Childrens.Find('CST'), tcStr));
  IBSCBS.cClassTrib := StrTocClassTrib(ObterConteudo(ANode.Childrens.Find('cClassTrib'), tcStr));

  Ler_IBSCBS_gIBSCBS(ANode.Childrens.Find('gIBSCBS'), IBSCBS.gIBSCBS);
end;

procedure TNF3eXmlReader.Ler_IBSCBS_gIBSCBS(const ANode: TACBrXmlNode; gIBSCBS: TgIBSCBS);
begin
  if not Assigned(ANode) then Exit;

  gIBSCBS.vBC := ObterConteudo(ANode.Childrens.Find('vBC'), tcDe2);
  gIBSCBS.vIBS := ObterConteudo(ANode.Childrens.Find('vIBS'), tcDe2);

  Ler_gIBSUF(ANode.Childrens.Find('gIBSUF'), gIBSCBS.gIBSUF);
  Ler_gIBSMun(ANode.Childrens.Find('gIBSMun'), gIBSCBS.gIBSMun);
  Ler_gCBS(ANode.Childrens.Find('gCBS'), gIBSCBS.gCBS);
  Ler_gIBSCBS_gTribRegular(ANode.Childrens.Find('gTribRegular'), gIBSCBS.gTribRegular);

  Ler_gIBSCredPres(ANode.Childrens.Find('gIBSCredPres'), gIBSCBS.gIBSCredPres);
  Ler_gCBSCredPres(ANode.Childrens.Find('gCBSCredPres'), gIBSCBS.gCBSCredPres);
  Ler_gTribCompraGov(ANode.Childrens.Find('gTribCompraGov'), gIBSCBS.gTribCompraGov);
end;

procedure TNF3eXmlReader.Ler_gIBSUF(const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSUF.pIBS := ObterConteudo(ANode.Childrens.Find('pIBSUF'), tcDe4);

  Ler_gIBSUF_gDif(ANode.Childrens.Find('gDif'), gIBSUF.gDif);
  Ler_gIBSUF_gDevTrib(ANode.Childrens.Find('gDevTrib'), gIBSUF.gDevTrib);
  Ler_gIBSUF_gRed(ANode.Childrens.Find('gRed'), gIBSUF.gRed);

  gIBSUF.vIBS := ObterConteudo(ANode.Childrens.Find('vIBSUF'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gIBSUF_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
begin
  if not Assigned(ANode) then Exit;

  gDif.pDif := ObterConteudo(ANode.Childrens.Find('pDif'), tcDe4);
  gDif.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gIBSUF_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
begin
  if not Assigned(ANode) then Exit;

  gDevTrib.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gIBSUF_gRed(const ANode: TACBrXmlNode; gRed: TgRed);
begin
  if not Assigned(ANode) then Exit;

  gRed.pRedAliq := ObterConteudo(ANode.Childrens.Find('pRedAliq'), tcDe4);
  gRed.pAliqEfet := ObterConteudo(ANode.Childrens.Find('pAliqEfet'), tcDe4);
end;

procedure TNF3eXmlReader.Ler_gIBSMun(const ANode: TACBrXmlNode;
  gIBSMun: TgIBSMunValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSMun.pIBS := ObterConteudo(ANode.Childrens.Find('pIBSMun'), tcDe4);

  Ler_gIBSMun_gDif(ANode.Childrens.Find('gDif'), gIBSMun.gDif);
  Ler_gIBSMun_gDevTrib(ANode.Childrens.Find('gDevTrib'), gIBSMun.gDevTrib);
  Ler_gIBSMun_gRed(ANode.Childrens.Find('gRed'), gIBSMun.gRed);

  gIBSMun.vIBS := ObterConteudo(ANode.Childrens.Find('vIBSMun'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gIBSMun_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
begin
  if not Assigned(ANode) then Exit;

  gDif.pDif := ObterConteudo(ANode.Childrens.Find('pDif'), tcDe4);
  gDif.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gIBSMun_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
begin
  if not Assigned(ANode) then Exit;

  gDevTrib.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gIBSMun_gRed(const ANode: TACBrXmlNode; gRed: TgRed);
begin
  if not Assigned(ANode) then Exit;

  gRed.pRedAliq := ObterConteudo(ANode.Childrens.Find('pRedAliq'), tcDe4);
  gRed.pAliqEfet := ObterConteudo(ANode.Childrens.Find('pAliqEfet'), tcDe4);
end;

procedure TNF3eXmlReader.Ler_gCBS(const ANode: TACBrXmlNode; gCBS: TgCBSValores);
begin
  if not Assigned(ANode) then Exit;

  gCBS.pCBS := ObterConteudo(ANode.Childrens.Find('pCBS'), tcDe4);

  Ler_gCBS_gDif(ANode.Childrens.Find('gDif'), gCBS.gDif);
  Ler_gCBS_gDevTrib(ANode.Childrens.Find('gDevTrib'), gCBS.gDevTrib);
  Ler_gCBS_gRed(ANode.Childrens.Find('gRed'), gCBS.gRed);

  gCBS.vCBS := ObterConteudo(ANode.Childrens.Find('vCBS'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gCBS_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
begin
  if not Assigned(ANode) then Exit;

  gDif.pDif := ObterConteudo(ANode.Childrens.Find('pDif'), tcDe4);
  gDif.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gCBS_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
begin
  if not Assigned(ANode) then Exit;

  gDevTrib.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gCBS_gRed(const ANode: TACBrXmlNode; gRed: TgRed);
begin
  if not Assigned(ANode) then Exit;

  gRed.pRedAliq := ObterConteudo(ANode.Childrens.Find('pRedAliq'), tcDe4);
  gRed.pAliqEfet := ObterConteudo(ANode.Childrens.Find('pAliqEfet'), tcDe4);
end;

procedure TNF3eXmlReader.Ler_gIBSCBS_gTribRegular(const ANode: TACBrXmlNode;
  gTribRegular: TgTribRegular);
begin
  if not Assigned(ANode) then Exit;

  gTribRegular.CSTReg := StrToCSTIBSCBS(ObterConteudo(ANode.Childrens.Find('CSTReg'), tcStr));
  gTribRegular.cClassTribReg := StrTocClassTrib(ObterConteudo(ANode.Childrens.Find('cClassTribReg'), tcStr));
  gTribRegular.pAliqEfetRegIBSUF := ObterConteudo(ANode.Childrens.Find('pAliqEfetRegIBSUF'), tcDe4);
  gTribRegular.vTribRegIBSUF := ObterConteudo(ANode.Childrens.Find('vTribRegIBSUF'), tcDe2);
  gTribRegular.pAliqEfetRegIBSMun := ObterConteudo(ANode.Childrens.Find('pAliqEfetRegIBSMun'), tcDe4);
  gTribRegular.vTribRegIBSMun := ObterConteudo(ANode.Childrens.Find('vTribRegIBSMun'), tcDe2);
  gTribRegular.pAliqEfetRegCBS := ObterConteudo(ANode.Childrens.Find('pAliqEfetRegCBS'), tcDe4);
  gTribRegular.vTribRegCBS := ObterConteudo(ANode.Childrens.Find('vTribRegCBS'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gIBSCredPres(const ANode: TACBrXmlNode;
  gIBSCredPres: TgIBSCBSCredPres);
begin
  if not Assigned(ANode) then Exit;

  gIBSCredPres.cCredPres := StrTocCredPres(ObterConteudo(ANode.Childrens.Find('cCredPres'), tcStr));
  gIBSCredPres.pCredPres := ObterConteudo(ANode.Childrens.Find('pCredPres'), tcDe4);
  gIBSCredPres.vCredPres := ObterConteudo(ANode.Childrens.Find('vCredPres'), tcDe2);
  gIBSCredPres.vCredPresCondSus := ObterConteudo(ANode.Childrens.Find('vCredPresCondSus'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gCBSCredPres(const ANode: TACBrXmlNode;
  gCBSCredPres: TgIBSCBSCredPres);
begin
  if not Assigned(ANode) then Exit;

  gCBSCredPres.cCredPres := StrTocCredPres(ObterConteudo(ANode.Childrens.Find('cCredPres'), tcStr));
  gCBSCredPres.pCredPres := ObterConteudo(ANode.Childrens.Find('pCredPres'), tcDe4);
  gCBSCredPres.vCredPres := ObterConteudo(ANode.Childrens.Find('vCredPres'), tcDe2);
  gCBSCredPres.vCredPresCondSus := ObterConteudo(ANode.Childrens.Find('vCredPresCondSus'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_gTribCompraGov(const ANode: TACBrXmlNode;
  gTribCompraGov: TgTribCompraGov);
begin
  if not Assigned(ANode) then Exit;

  gTribCompraGov.pAliqIBSUF := ObterConteudo(ANode.Childrens.Find('pAliqIBSUF'), tcDe4);
  gTribCompraGov.vTribIBSUF := ObterConteudo(ANode.Childrens.Find('vTribIBSUF'), tcDe2);
  gTribCompraGov.pAliqIBSMun := ObterConteudo(ANode.Childrens.Find('pAliqIBSMun'), tcDe4);
  gTribCompraGov.vTribIBSMun := ObterConteudo(ANode.Childrens.Find('vTribIBSMun'), tcDe2);
  gTribCompraGov.pAliqCBS := ObterConteudo(ANode.Childrens.Find('pAliqCBS'), tcDe4);
  gTribCompraGov.vTribCBS := ObterConteudo(ANode.Childrens.Find('vTribCBS'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_IBSCBSTot(const ANode: TACBrXmlNode;
  IBSCBSTot: TIBSCBSTot);
begin
  if not Assigned(ANode) then Exit;

  Ler_IBSCBSTot_gIBS(ANode.Childrens.Find('gIBS'), IBSCBSTot.gIBS);
  Ler_IBSCBSTot_gCBS(ANode.Childrens.Find('gCBS'), IBSCBSTot.gCBS);
end;

procedure TNF3eXmlReader.Ler_IBSCBSTot_gIBS(const ANode: TACBrXmlNode;
  gIBS: TgIBS);
begin
  if not Assigned(ANode) then Exit;

  Ler_IBSCBSTot_gIBS_gIBSUFTot(ANode.Childrens.Find('gIBSUFTot'), gIBS.gIBSUFTot);
  Ler_IBSCBSTot_gIBS_gIBSMunTot(ANode.Childrens.Find('gIBSMunTot'), gIBS.gIBSMunTot);

  gIBS.vCredPres := ObterConteudo(ANode.Childrens.Find('vCredPres'), tcDe2);
  gIBS.vCredPresCondSus := ObterConteudo(ANode.Childrens.Find('vCredPresCondSus'), tcDe2);
  gIBS.vIBS := ObterConteudo(ANode.Childrens.Find('vIBS'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_IBSCBSTot_gIBS_gIBSUFTot(const ANode: TACBrXmlNode;
  gIBSUFTot: TgIBSUFTot);
begin
  if not Assigned(ANode) then Exit;

  gIBSUFTot.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
  gIBSUFTot.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
  gIBSUFTot.vIBSUF := ObterConteudo(ANode.Childrens.Find('vIBSUF'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_IBSCBSTot_gIBS_gIBSMunTot(const ANode: TACBrXmlNode;
  gIBSMunTot: TgIBSMunTot);
begin
  if not Assigned(ANode) then Exit;

  gIBSMunTot.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
  gIBSMunTot.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
  gIBSMunTot.vIBSMun := ObterConteudo(ANode.Childrens.Find('vIBSMun'), tcDe2);
end;

procedure TNF3eXmlReader.Ler_IBSCBSTot_gCBS(const ANode: TACBrXmlNode;
  gCBS: TgCBS);
begin
  if not Assigned(ANode) then Exit;

  gCBS.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
  gCBS.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
  gCBS.vCBS := ObterConteudo(ANode.Childrens.Find('vCBS'), tcDe2);
  gCBS.vCredPres := ObterConteudo(ANode.Childrens.Find('vCredPres'), tcDe2);
  gCBS.vCredPresCondSus := ObterConteudo(ANode.Childrens.Find('vCredPresCondSus'), tcDe2);
end;

end.

