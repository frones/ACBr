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

    procedure LerProtNF3e(const ANode: TACBrXmlNode);
    procedure LerInfNF3e(const ANode: TACBrXmlNode);
    procedure LerIde(const ANode: TACBrXmlNode);
    procedure LerEmit(const ANode: TACBrXmlNode);
    procedure LerEmitEnderEmit(const ANode: TACBrXmlNode);
    procedure LerDest(const ANode: TACBrXmlNode);
    procedure LerDestEnderDest(const ANode: TACBrXmlNode);
    procedure LerAcessante(const ANode: TACBrXmlNode);
    procedure LergSub(const ANode: TACBrXmlNode);
    procedure LergNF(const ANode: TACBrXmlNode);
    procedure LergJudic(const ANode: TACBrXmlNode);
    procedure LergGrContrat(const ANode: TACBrXmlNode);
    procedure LergMed(const ANode: TACBrXmlNode);
    procedure LergSCEE(const ANode: TACBrXmlNode);
    procedure LergConsumidor(const ANode: TACBrXmlNode);
    procedure LergSaldoCred(const ANode: TACBrXmlNode);
    procedure LergTipoSaldo(const ANode: TACBrXmlNode);
    procedure LerNFDet(const ANode: TACBrXmlNode);

    procedure LerTotal(const ANode: TACBrXmlNode);
    procedure LergFat(const ANode: TACBrXmlNode);
    procedure LergANEEL(const ANode: TACBrXmlNode);
    procedure LergHistFat(const ANode: TACBrXmlNode);
    procedure LergGrandFat(const ANode: TACBrXmlNode; I: Integer);
    procedure LerautXML(const ANode: TACBrXmlNode);
    procedure LerInfAdic(const ANode: TACBrXmlNode);
    procedure LergRespTec(const ANode: TACBrXmlNode);
    procedure LerInfNF3eSupl(const ANode: TACBrXmlNode);
    procedure LerSignature(const ANode: TACBrXmlNode);
  public
    constructor Create(AOwner: TNF3e); reintroduce;

    function LerXml: Boolean; override;

    property NF3e: TNF3e read FNF3e write FNF3e;
  end;

implementation

uses
  ACBrXmlBase, ACBrUtil.Base,
  ACBrNF3eConversao;

{ TNF3eXmlReader }

constructor TNF3eXmlReader.Create(AOwner: TNF3e);
begin
  inherited Create;

  FNF3e := AOwner;
end;

procedure TNF3eXmlReader.LerAcessante(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.acessante.idAcesso := ObterConteudo(ANode.Childrens.Find('idAcesso'), tcStr);
  NF3e.acessante.idCodCliente := ObterConteudo(ANode.Childrens.Find('idCodCliente'), tcStr);
  NF3e.acessante.tpAcesso := StrTotpAcesso(ObterConteudo(ANode.Childrens.Find('tpAcesso'), tcStr));
  NF3e.acessante.xNomeUC := ObterConteudo(ANode.Childrens.Find('xNomeUC'), tcStr);
  NF3e.acessante.tpClasse := StrTotpClasse(ObterConteudo(ANode.Childrens.Find('tpClasse'), tcStr));
  NF3e.acessante.tpSubClasse := StrTotpSubClasse(ObterConteudo(ANode.Childrens.Find('tpSubClasse'), tcStr));
  NF3e.acessante.tpFase := StrTotpFase(ObterConteudo(ANode.Childrens.Find('tpFase'), tcStr));
  NF3e.acessante.tpGrpTensao := StrTotpGrpTensao(ObterConteudo(ANode.Childrens.Find('tpGrpTensao'), tcStr));
  NF3e.acessante.tpModTar := StrTotpModTar(ObterConteudo(ANode.Childrens.Find('tpModTar'), tcStr));
  NF3e.acessante.latGPS := ObterConteudo(ANode.Childrens.Find('latGPS'), tcStr);
  NF3e.acessante.longGPS := ObterConteudo(ANode.Childrens.Find('longGPS'), tcStr);
  NF3e.acessante.codRoteiroLeitura := ObterConteudo(ANode.Childrens.Find('codRoteiroLeitura'), tcStr);
end;

procedure TNF3eXmlReader.LerDest(const ANode: TACBrXmlNode);
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

  LerDestEnderDest(ANode.Childrens.Find('enderDest'));
end;

procedure TNF3eXmlReader.LerDestEnderDest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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

procedure TNF3eXmlReader.LerEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.Emit.CNPJ := ObterCNPJCPF(ANode);
  NF3e.Emit.IE := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
  NF3e.Emit.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  NF3e.Emit.xFant := ObterConteudo(ANode.Childrens.Find('xFant'), tcStr);

  LerEmitEnderEmit(ANode.Childrens.Find('enderEmit'));
end;

procedure TNF3eXmlReader.LerEmitEnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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

procedure TNF3eXmlReader.LergGrContrat(const ANode: TACBrXmlNode);
var
  Item: TgGrContratCollectionItem;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NF3e.gGrContrat.New;

  Item.nContrat := StrToInt(ObterConteudoTag(ANode.Attributes.Items['nContrat']));
  Item.tpGrContrat := StrTotpGrContrat(ObterConteudo(ANode.Childrens.Find('tpGrContrat'), tcStr));
  Item.tpPosTar := StrTotpPosTar(ObterConteudo(ANode.Childrens.Find('tpPosTar'), tcStr));
  Item.qUnidContrat := ObterConteudo(ANode.Childrens.Find('qUnidContrat'), tcStr);
end;

procedure TNF3eXmlReader.LergJudic(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.gJudic.chNF3e := ObterConteudo(ANode.Childrens.Find('chNF3e'), tcStr);
end;

procedure TNF3eXmlReader.LergMed(const ANode: TACBrXmlNode);
var
  Item: TgMedCollectionItem;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NF3e.gMed.New;

  Item.nMed := StrToInt(ObterConteudoTag(ANode.Attributes.Items['nMed']));
  Item.idMedidor := ObterConteudo(ANode.Childrens.Find('idMedidor'), tcStr);
  Item.dMedAnt := ObterConteudo(ANode.Childrens.Find('dMedAnt'), tcDat);
  Item.dMedAtu := ObterConteudo(ANode.Childrens.Find('dMedAtu'), tcDat);
end;

procedure TNF3eXmlReader.LergNF(const ANode: TACBrXmlNode);
var
  xData: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.gSub.CNPJ := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NF3e.gSub.serie := ObterConteudo(ANode.Childrens.Find('serie'), tcStr);
  NF3e.gSub.nNF := ObterConteudo(ANode.Childrens.Find('nNF'), tcInt);

  xData := ObterConteudo(ANode.Childrens.Find('CompetEmis'), tcStr);
  xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
  NF3e.gSub.CompetEmis := StrToDate(xData);

  xData := ObterConteudo(ANode.Childrens.Find('CompetApur'), tcStr);
  xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
  NF3e.gSub.CompetApur := StrToDate(xData);

  NF3e.gSub.hash115 := ObterConteudo(ANode.Childrens.Find('hash115'), tcStr);
end;

procedure TNF3eXmlReader.LergConsumidor(const ANode: TACBrXmlNode);
var
  Item: TgConsumidorCollectionItem;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NF3e.gSCEE.gConsumidor.New;

  Item.idAcessGer := ObterConteudo(ANode.Childrens.Find('idAcessGer'), tcStr);
  Item.vPotInst := ObterConteudo(ANode.Childrens.Find('vPotInst'), tcDe3);
  Item.tpFonteEnergia := StrTotpFonteEnergia(ObterConteudo(ANode.Childrens.Find('tpFonteEnergia'), tcStr));
  Item.enerAloc := ObterConteudo(ANode.Childrens.Find('enerAloc'), tcDe3);
  Item.tpPosTar := StrTotpPosTar(ObterConteudo(ANode.Childrens.Find('tpPosTar'), tcStr));
end;

procedure TNF3eXmlReader.LergSaldoCred(const ANode: TACBrXmlNode);
var
  Item: TgSaldoCredCollectionItem;
  xData: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NF3e.gSCEE.gSaldoCred.New;

  Item.tpPosTar := StrTotpPosTar(ObterConteudo(ANode.Childrens.Find('tpPosTar'), tcStr));
  Item.vSaldAnt := ObterConteudo(ANode.Childrens.Find('vSaldAnt'), tcDe4);
  Item.vCredExpirado := ObterConteudo(ANode.Childrens.Find('vCredExpirado'), tcDe4);
  Item.vSaldAtual := ObterConteudo(ANode.Childrens.Find('vSaldAtual'), tcDe4);
  Item.vCredExpirar := ObterConteudo(ANode.Childrens.Find('vCredExpirar'), tcDe4);

  xData := ObterConteudo(ANode.Childrens.Find('CompetExpirar'), tcStr);
  xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
  Item.CompetExpirar :=  StrToDate(xData);
end;

procedure TNF3eXmlReader.LergTipoSaldo(const ANode: TACBrXmlNode);
var
  Item: TgTipoSaldoCollectionItem;
  xData: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NF3e.gSCEE.gTipoSaldo.New;

  Item.tpPosTar := StrTotpPosTar(ObterConteudo(ANode.Childrens.Find('tpPosTar'), tcStr));
  Item.vSaldAnt := ObterConteudo(ANode.Childrens.Find('vSaldAnt'), tcDe4);
  Item.vCredExpirado := ObterConteudo(ANode.Childrens.Find('vCredExpirado'), tcDe4);
  Item.vSaldAtual := ObterConteudo(ANode.Childrens.Find('vSaldAtual'), tcDe4);
  Item.vCredExpirar := ObterConteudo(ANode.Childrens.Find('vCredExpirar'), tcDe4);

  xData := ObterConteudo(ANode.Childrens.Find('CompetExpirar'), tcStr);
  xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
  Item.CompetExpirar :=  StrToDate(xData);
end;

procedure TNF3eXmlReader.LergSCEE(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.gSCEE.tpPartComp := StrTotpPartComp(ObterConteudo(ANode.Childrens.Find('tpPartComp'), tcStr));

  ANodes := ANode.Childrens.FindAll('gConsumidor');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LergConsumidor(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('gSaldoCred');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LergSaldoCred(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('gTipoSaldo');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LergTipoSaldo(ANodes[i]);
  end;
end;

procedure TNF3eXmlReader.LergSub(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.gSub.chNF3e := ObterConteudo(ANode.Childrens.Find('chNF3e'), tcStr);
  NF3e.gSub.motSub := StrToMotSub(ObterConteudo(ANode.Childrens.Find('motSub'), tcStr));

  LergNF(ANode.Childrens.Find('gNF'));
end;

procedure TNF3eXmlReader.LerIde(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
end;

procedure TNF3eXmlReader.LerInfNF3e(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  LerIde(ANode.Childrens.Find('ide'));
  LerEmit(ANode.Childrens.Find('emit'));
  LerDest(ANode.Childrens.Find('dest'));
  LerAcessante(ANode.Childrens.Find('acessante'));
  LergSub(ANode.Childrens.Find('gSub'));
  LergJudic(ANode.Childrens.Find('gJudic'));

  ANodes := ANode.Childrens.FindAll('gGrContrat');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LergGrContrat(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('gMed');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LergMed(ANodes[i]);
  end;

  LergSCEE(ANode.Childrens.Find('gSCEE'));

  NF3e.NFDet.Clear;
  ANodes := ANode.Childrens.FindAll('NFdet');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerNFdet(ANodes[i]);
  end;

  LerTotal(ANode.Childrens.Find('total'));

  LergFat(ANode.Childrens.Find('gFat'));

  LergANEEL(ANode.Childrens.Find('gANEEL'));

  ANodes := ANode.Childrens.FindAll('autXML');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerautXML(ANodes[i]);
  end;

  LerInfAdic(ANode.Childrens.Find('infAdic'));

  LergRespTec(ANode.Childrens.Find('gRespTec'));
end;

procedure TNF3eXmlReader.LerNFDet(const ANode: TACBrXmlNode);
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
  snItemAnt: String;

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
        detItemAnt.indDevolucao := StrToTIndicador(ObterConteudo(ANodeNivel3.Childrens.Find('indDevolucao'), tcStr));

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
          with detItem.gTarif.New do
          begin
            dIniTarif   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('dIniTarif'), tcDat);
            dFimTarif   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('dFimTarif'), tcDat);
            tpAto       := StrTotpAto(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpAto'), tcStr));
            nAto        := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('nAto'), tcStr);
            anoAto      := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('anoAto'), tcInt);
            tpTarif     := StrTotpTarif(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpTarif'), tcStr));
            cPosTarif   := StrTocPosTarif(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('cPosTarif'), tcStr));
            uMed        := StrTouMed(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('uMed'), tcStr));
            vTarifHom   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vTarifHom'), tcDe8);
            vTarifAplic := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vTarifAplic'), tcDe8);
            motDifTarif := StrTomotDifTarif(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('motDifTarif'), tcStr));
          end;
        end;

        detItem.gAdBand.Clear;
        ADetChildrensNodes := ANodeNivel3.Childrens.FindAll('gAdBand');
        for j := 0 to Length(ADetChildrensNodes) - 1 do
        begin
          with detItem.gAdBand.New do
          begin
            dIniAdBand   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('dIniAdBand'), tcDat);
            dFimAdBand   := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('dFimAdBand'), tcDat);
            tpBand       := StrTotpBand(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpBand'), tcStr));
            vAdBand      := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vAdBand'), tcDe10);
            vAdBandAplic := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vAdBandAplic'), tcDe10);
            motDifBand   := StrTomotDifBand(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('motDifBand'), tcStr));
          end;
        end;

        ANodeNivel4 := ANodeNivel3.Childrens.Find('prod');

        if not(NodeNaoEncontrado(ANodeNivel4)) then
        begin
          detItem.Prod.indOrigemQtd := ObterConteudo(ANodeNivel4.Childrens.Find('indOrigemQtd'), tcStr);
          detItem.Prod.cProd        := ObterConteudo(ANodeNivel4.Childrens.Find('cProd'), tcStr);
          detItem.Prod.xProd        := ObterConteudo(ANodeNivel4.Childrens.Find('xProd'), tcStr);
          detItem.Prod.cClass       := ObterConteudo(ANodeNivel4.Childrens.Find('cClass'), tcInt);
          detItem.Prod.CFOP         := ObterConteudo(ANodeNivel4.Childrens.Find('CFOP'), tcInt);
          detItem.Prod.uMed         := StrTouMedFat(ObterConteudo(ANodeNivel4.Childrens.Find('uMed'), tcStr));
          detItem.Prod.qFaturada    := ObterConteudo(ANodeNivel4.Childrens.Find('qFaturada'), tcDe4);
          detItem.Prod.vItem        := ObterConteudo(ANodeNivel4.Childrens.Find('vItem'), tcDe10);
          detItem.Prod.vProd        := ObterConteudo(ANodeNivel4.Childrens.Find('vProd'), tcDe10);
          detItem.Prod.indDevolucao := StrToTIndicador(ObterConteudo(ANodeNivel4.Childrens.Find('indDevolucao'), tcStr));
          detItem.Prod.indPrecoACL  := StrToTIndicador(ObterConteudo(ANodeNivel4.Childrens.Find('indPrecoACL'), tcStr));

          ANodeNivel5 := ANodeNivel4.Childrens.Find('gMedicao');

          if not(NodeNaoEncontrado(ANodeNivel5)) then
          begin
            detItem.Prod.gMedicao.nMed            := ObterConteudo(ANodeNivel5.Childrens.Find('nMed'), tcInt);
            detItem.Prod.gMedicao.nContrat        := ObterConteudo(ANodeNivel5.Childrens.Find('nContrat'), tcInt);
            detItem.Prod.gMedicao.tpMotNaoLeitura := StrTotpMotNaoLeitura(ObterConteudo(ANodeNivel5.Childrens.Find('tpMotNaoLeitura'), tcStr));

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
          LerICMS('ICMS00', detItem.Imposto);
          LerICMS('ICMS10', detItem.Imposto);
          LerICMS('ICMS20', detItem.Imposto);
          LerICMS('ICMS40', detItem.Imposto);
          LerICMS('ICMS51', detItem.Imposto);
          LerICMS('ICMS60', detItem.Imposto);
          LerICMS('ICMS90', detItem.Imposto);

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

          ANodeNivel5 := ANodeNivel4.Childrens.Find('COFINSEfet');

          if not(NodeNaoEncontrado(ANodeNivel5)) then
          begin
            detItem.Imposto.retTrib.vRetPIS    := ObterConteudo(ANodeNivel5.Childrens.Find('vRetPIS'), tcDe2);
            detItem.Imposto.retTrib.vRetCOFINS := ObterConteudo(ANodeNivel5.Childrens.Find('vRetCOFINS'), tcDe2);
            detItem.Imposto.retTrib.vRetCSLL   := ObterConteudo(ANodeNivel5.Childrens.Find('vRetCSLL'), tcDe2);
            detItem.Imposto.retTrib.vBCIRRF    := ObterConteudo(ANodeNivel5.Childrens.Find('vBCIRRF'), tcDe2);
            detItem.Imposto.retTrib.vIRRF      := ObterConteudo(ANodeNivel5.Childrens.Find('vIRRF'), tcDe2);
          end;
        end;

        ANodeNivel4 := ANodeNivel3.Childrens.Find('gProcRef');

        if not(NodeNaoEncontrado(ANodeNivel4)) then
        begin
          detItem.gProcRef.vItem        := ObterConteudo(ANodeNivel4.Childrens.Find('vItem'), tcDe10);
          detItem.gProcRef.qFaturada    := ObterConteudo(ANodeNivel4.Childrens.Find('qFaturada'), tcDe4);
          detItem.gProcRef.vProd        := ObterConteudo(ANodeNivel4.Childrens.Find('vProd'), tcDe10);
          detItem.gProcRef.indDevolucao := StrToTIndicador(ObterConteudo(ANodeNivel4.Childrens.Find('indDevolucao'), tcStr));
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
            with detItem.gProcRef.gProc.New do
            begin
              tpProc    := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpProc'), tcStr);
              nProcesso := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('nProcesso'), tcStr);
            end;
          end;
        end;

        detItem.gContab.Clear;
        ADetChildrensNodes := ANodeNivel3.Childrens.FindAll('gContab');
        for j := 0 to Length(ADetChildrensNodes) - 1 do
        begin
          with detItem.gContab.New do
          begin
            cContab := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('cContab'), tcStr);
            xContab := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('xContab'), tcStr);
            vContab := ObterConteudo(ADetChildrensNodes[j].Childrens.Find('vContab'), tcDe2);
            tpLanc  := StrTotpLanc(ObterConteudo(ADetChildrensNodes[j].Childrens.Find('tpLanc'), tcStr));
          end;
        end;
      end;
    end;
  end;
end;

procedure TNF3eXmlReader.LerTotal(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
end;

procedure TNF3eXmlReader.LergFat(const ANode: TACBrXmlNode);
var
  xData: string;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  xData := ObterConteudo(ANode.Childrens.Find('CompetFat'), tcStr);
  xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
  NF3e.gFat.CompetFat := StrToDate(xData);

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

procedure TNF3eXmlReader.LergANEEL(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  ANodes := ANode.Childrens.FindAll('gHistFat');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LergHistFat(ANodes[i]);
  end;
end;

procedure TNF3eXmlReader.LergHistFat(const ANode: TACBrXmlNode);
var
  Item: TgHistFatCollectionItem;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NF3e.gANEEL.gHistFat.New;

  Item.xGrandFat := ObterConteudo(ANode.Childrens.Find('xGrandFat'), tcStr);

  ANodes := ANode.Childrens.FindAll('gGrandFat');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LergGrandFat(ANodes[i], NF3e.gANEEL.gHistFat.Count -1);
  end;
end;

procedure TNF3eXmlReader.LergGrandFat(const ANode: TACBrXmlNode; I: Integer);
var
  Item: TgGrandFatCollectionItem;
  xData: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NF3e.gANEEL.gHistFat[I].gGrandFat.New;

  xData := ObterConteudo(ANode.Childrens.Find('CompetFat'), tcStr);
  xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
  Item.CompetFat := StrToDate(xData);

  Item.vFat := ObterConteudo(ANode.Childrens.Find('vFat'), tcDe2);
  Item.uMed := StrTouMedFat(ObterConteudo(ANode.Childrens.Find('uMed'), tcStr));
  Item.qtdDias := ObterConteudo(ANode.Childrens.Find('qtdDias'), tcInt);
end;

procedure TNF3eXmlReader.LerautXML(const ANode: TACBrXmlNode);
var
  Item: TautXMLCollectionItem;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NF3e.autXML.New;

  Item.CNPJCPF := ObterCNPJCPF(ANode);
end;

procedure TNF3eXmlReader.LerInfAdic(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.InfAdic.infAdFisco := ObterConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);
  NF3e.InfAdic.infCpl := ObterConteudo(ANode.Childrens.Find('infCpl'), tcStr);
end;

procedure TNF3eXmlReader.LergRespTec(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.infRespTec.CNPJ     := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NF3e.infRespTec.xContato := ObterConteudo(ANode.Childrens.Find('xContato'), tcStr);
  NF3e.infRespTec.email    := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  NF3e.infRespTec.fone     := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  NF3e.infRespTec.idCSRT   := ObterConteudo(ANode.Childrens.Find('idCSRT'), tcInt);
  NF3e.infRespTec.hashCSRT := ObterConteudo(ANode.Childrens.Find('hashCSRT'), tcStr);
end;

procedure TNF3eXmlReader.LerInfNF3eSupl(const ANode: TACBrXmlNode);
var
 sQrCode: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  sQrCode := ObterConteudo(ANode.Childrens.Find('qrCodNF3e'), tcStr);
  sQrCode := StringReplace(sQrCode, '<![CDATA[', '', []);
  sQrCode := StringReplace(sQrCode, ']]>', '', []);

  NF3e.infNF3eSupl.qrCodNF3e := sQrCode;
end;

procedure TNF3eXmlReader.LerSignature(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('SignedInfo');

  if AuxNode <> nil then
  begin
    AuxNode := AuxNode.Childrens.Find('Reference');

    if AuxNode <> nil then
    begin
      NF3e.signature.URI := AuxNode.Attributes.Items['URI'].Content;
      NF3e.signature.DigestValue := ObterConteudo(AuxNode.Childrens.Find('DigestValue'), tcStr);
    end;
  end;

  NF3e.signature.SignatureValue  := ObterConteudo(ANode.Childrens.Find('SignatureValue'), tcStr);

  AuxNode := ANode.Childrens.Find('KeyInfo');
  if AuxNode <> nil then
  begin
    NF3e.signature.X509Certificate := ObterConteudo(ANode.Childrens.Find('X509Certificate'), tcStr);
  end;
end;

procedure TNF3eXmlReader.LerProtNF3e(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
  if not Assigned(FNF3e) or (FNF3e = nil) then
    raise Exception.Create('Destino não informado, informe a classe [TNF3e] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml da NF3e não carregado.');

  Result := False;
  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'nf3eProc' then
  begin
    LerProtNF3e(Document.Root.Childrens.Find('protNF3e').Childrens.Find('infProt'));
    NF3eNode := Document.Root.Childrens.Find('NF3e');
  end
  else
  begin
    NF3eNode := Document.Root;
  end;

  if NF3eNode <> nil then
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

  LerInfNF3e(infNF3eNode);
  LerInfNF3eSupl(NF3eNode.Childrens.Find('infNF3eSupl'));
  LerSignature(NF3eNode.Childrens.Find('Signature'));

  Result := True;
end;

function TNF3eXmlReader.NodeNaoEncontrado(const ANode: TACBrXmlNode): Boolean;
begin
  Result := not Assigned(ANode) or (ANode = nil);
end;

(*
procedure TNF3eXmlReader.LerDetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;
  {
  Item.Prod.cProd := ObterConteudo(ANode.Childrens.Find('cProd'), tcStr);
  Item.Prod.cEAN  := ObterConteudo(ANode.Childrens.Find('cEAN'), tcStr);
  Item.Prod.xProd := ObterConteudo(ANode.Childrens.Find('xProd'), tcStr);
  Item.Prod.NCM   := ObterConteudo(ANode.Childrens.Find('NCM'), tcStr);
  Item.Prod.CEST := ObterConteudo(ANode.Childrens.Find('CEST'), tcStr);
  if NF3e.infNF3e.Versao >= 4 then
  begin
    Item.Prod.indEscala := StrToindEscala(ok, ObterConteudo(ANode.Childrens.Find('indEscala'), tcStr));
    Item.Prod.CNPJFab   := ObterConteudo(ANode.Childrens.Find('CNPJFab'), tcStr);
    Item.Prod.cBenef    := ObterConteudo(ANode.Childrens.Find('cBenef'), tcStr);
  end;
  Item.Prod.EXTIPI   := ObterConteudo(ANode.Childrens.Find('EXTIPI'), tcStr);
  Item.Prod.CFOP     := ObterConteudo(ANode.Childrens.Find('CFOP'), tcEsp);
  Item.Prod.uCom     := ObterConteudo(ANode.Childrens.Find('uCom'), tcStr);
  Item.Prod.qCom     := ObterConteudo(ANode.Childrens.Find('qCom'), tcDe4);
  Item.Prod.vUnCom  := ObterConteudo(ANode.Childrens.Find('vUnCom'), tcDe10);
  Item.Prod.vProd    := ObterConteudo(ANode.Childrens.Find('vProd'), tcDe2);
  Item.Prod.cEANTrib := ObterConteudo(ANode.Childrens.Find('cEANTrib'), tcStr);
  Item.Prod.uTrib    := ObterConteudo(ANode.Childrens.Find('uTrib'), tcStr);
  Item.Prod.qTrib    := ObterConteudo(ANode.Childrens.Find('qTrib'), tcDe4);
  Item.Prod.vUnTrib := ObterConteudo(ANode.Childrens.Find('vUnTrib'), tcDe10);
  Item.Prod.vFrete   := ObterConteudo(ANode.Childrens.Find('vFrete'), tcDe2);
  Item.Prod.vSeg     := ObterConteudo(ANode.Childrens.Find('vSeg'), tcDe2);
  Item.Prod.vDesc    := ObterConteudo(ANode.Childrens.Find('vDesc'), tcDe2);
  Item.Prod.vOutro  := ObterConteudo(ANode.Childrens.Find('vOutro'), tcDe2);
  Item.Prod.IndTot  := StrToindTot(ok,ObterConteudo(ANode.Childrens.Find('indTot'), tcDe2));
  Item.Prod.xPed     := ObterConteudo(ANode.Childrens.Find('xPed'), tcStr);
  Item.Prod.nItemPed := ObterConteudo(ANode.Childrens.Find('nItemPed'), tcStr);
  Item.Prod.nRECOPI  := ObterConteudo(ANode.Childrens.Find('nRECOPI'), tcStr);
  Item.Prod.nFCI     := ObterConteudo(ANode.Childrens.Find('nFCI'), tcStr);

  ANodes := ANode.Childrens.FindAll('NVE');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Item.Prod.NVE.New;
    Item.Prod.NVE.Items[i].NVE := ObterConteudo(ANodes[i].Childrens.Find('NVE'), tcStr);
  end;

  Item.Prod.DI.Clear;
  ANodes := ANode.Childrens.FindAll('DI');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerDetProdDI(Item, ANodes[i]);
  end;

  Item.Prod.detExport.Clear;
  ANodes := ANode.Childrens.FindAll('detExport');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerDetProdDetExport(Item, ANodes[i]);
  end;

  Item.Prod.rastro.Clear;
  ANodes := ANode.Childrens.FindAll('rastro');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerDetProdRastro(Item, ANodes[i]);
  end;

  LerDetProdVeicProd(Item, ANode.Childrens.Find('veicProd'));

  Item.Prod.med.Clear;
  ANodes := ANode.Childrens.FindAll('med');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerDetProdMed(Item, ANodes[i]);
  end;

  Item.Prod.arma.Clear;
  ANodes := ANode.Childrens.FindAll('arma');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerDetProdArma(Item, ANodes[i]);
  end;
  }
  LerDetProdComb(Item, ANode.Childrens.Find('comb'));
end;

procedure TNF3eXmlReader.LerDetProdDI(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
//  DIItem: TDICollectionItem;
//  AdiItem: TAdiCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;
  {
  DIItem := Item.Prod.DI.New;
  DIItem.nDI        := ObterConteudo(ANode.Childrens.Find('nDI'), tcStr);
  DIItem.dDI        := ObterConteudo(ANode.Childrens.Find('dDI'), tcDat);
  DIItem.xLocDesemb := ObterConteudo(ANode.Childrens.Find('xLocDesemb'), tcStr);
  DIItem.UFDesemb   := ObterConteudo(ANode.Childrens.Find('UFDesemb'), tcStr);
  DIItem.dDesemb    := ObterConteudo(ANode.Childrens.Find('dDesemb'), tcDat);

  DIItem.tpViaTransp  := StrToTipoViaTransp(Ok, ObterConteudo(ANode.Childrens.Find('tpViaTransp'), tcInt));
  DIItem.vAFRMM       := ObterConteudo(ANode.Childrens.Find('vAFRMM'), tcDe2);
  DIItem.tpIntermedio := StrToTipoIntermedio(Ok, ObterConteudo(ANode.Childrens.Find('tpIntermedio'), tcInt));
  DIItem.CNPJ         := ProcessarCNPJCPF(ANode);
  DIItem.UFTerceiro   := ObterConteudo(ANode.Childrens.Find('UFTerceiro'), tcStr);

  DIItem.cExportador   := ObterConteudo(ANode.Childrens.Find('cExportador'), tcStr);

  DIItem.adi.Clear;
  ANodes := ANode.Childrens.FindAll('adi');
  for i := 0 to Length(ANodes) - 1 do
  begin
    AdiItem := DIItem.adi.New;
    AdiItem.nAdicao     := ObterConteudo(ANodes[i].Childrens.Find('nAdicao'), tcInt);
    AdiItem.nSeqAdi     := ObterConteudo(ANodes[i].Childrens.Find('nSeqAdic'), tcInt);
    AdiItem.cFabricante := ObterConteudo(ANodes[i].Childrens.Find('cFabricante'), tcStr);
    AdiItem.vDescDI     := ObterConteudo(ANodes[i].Childrens.Find('vDescDI'), tcDe2);
    AdiItem.nDraw      := ObterConteudo(ANodes[i].Childrens.Find('nDraw'), tcStr);
  end;
  }
end;

procedure TNF3eXmlReader.LerDetImposto(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  AuxNode, AxNode: TACBrXmlNode;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;
  {
  Item.Imposto.vTotTrib := ObterConteudo(ANode.Childrens.Find('vTotTrib'), tcDe2);

  AuxNode := ANode.Childrens.Find('ICMS');
  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];
  if (AuxNode <> nil) then
  begin
    Item.Imposto.ICMS.orig        := StrToOrig(ok, ObterConteudo(AuxNode.Childrens.Find('orig'), tcStr));
    Item.Imposto.ICMS.CST         := StrToCSTICMS(ok, ObterConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    Item.Imposto.ICMS.CSOSN      := StrToCSOSNIcms( ok,ObterConteudo(AuxNode.Childrens.Find('CSOSN'), tcInt));
    Item.Imposto.ICMS.modBC       := StrToModBC(ok, ObterConteudo(AuxNode.Childrens.Find('modBC'), tcStr));
    Item.Imposto.ICMS.pRedBC      := ObterConteudo(AuxNode.Childrens.Find('pRedBC'), tcDe2);
    Item.Imposto.ICMS.vBC         := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    Item.Imposto.ICMS.pICMS       := ObterConteudo(AuxNode.Childrens.Find('pICMS'), tcDe2);
    Item.Imposto.ICMS.vICMSOp    := ObterConteudo(AuxNode.Childrens.Find('vICMSOp'), tcDe2);
    Item.Imposto.ICMS.pDif       := ObterConteudo(AuxNode.Childrens.Find('pDif'), tcDe4);
    Item.Imposto.ICMS.vICMSDif   := ObterConteudo(AuxNode.Childrens.Find('vICMSDif'), tcDe2);
    Item.Imposto.ICMS.vICMS       := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    Item.Imposto.ICMS.vBCFCP     := ObterConteudo(AuxNode.Childrens.Find('vBCFCP'), tcDe2);
    Item.Imposto.ICMS.pFCP       := ObterConteudo(AuxNode.Childrens.Find('pFCP'), tcDe4);
    Item.Imposto.ICMS.vFCP       := ObterConteudo(AuxNode.Childrens.Find('vFCP'), tcDe2);
    Item.Imposto.ICMS.modBCST     := StrToModBCST(ok, ObterConteudo(AuxNode.Childrens.Find('modBCST'), tcStr));
    Item.Imposto.ICMS.pMVAST      := ObterConteudo(AuxNode.Childrens.Find('pMVAST'), tcDe2);
    Item.Imposto.ICMS.pRedBCST    := ObterConteudo(AuxNode.Childrens.Find('pRedBCST'), tcDe2);
    Item.Imposto.ICMS.vBCST       := ObterConteudo(AuxNode.Childrens.Find('vBCST'), tcDe2);
    Item.Imposto.ICMS.pICMSST     := ObterConteudo(AuxNode.Childrens.Find('pICMSST'), tcDe2);
    Item.Imposto.ICMS.vICMSST     := ObterConteudo(AuxNode.Childrens.Find('vICMSST'), tcDe2);
    Item.Imposto.ICMS.vBCFCPST   := ObterConteudo(AuxNode.Childrens.Find('vBCFCPST'), tcDe2);
    Item.Imposto.ICMS.pFCPST     := ObterConteudo(AuxNode.Childrens.Find('pFCPST'), tcDe4);
    Item.Imposto.ICMS.vFCPST     := ObterConteudo(AuxNode.Childrens.Find('vFCPST'), tcDe2);
    Item.Imposto.ICMS.UFST        := ObterConteudo(AuxNode.Childrens.Find('UFST'), tcStr);
    Item.Imposto.ICMS.pBCOp       := ObterConteudo(AuxNode.Childrens.Find('pBCOp'), tcDe2);
    Item.Imposto.ICMS.vBCSTRet    := ObterConteudo(AuxNode.Childrens.Find('vBCSTRet'), tcDe2);
    Item.Imposto.ICMS.vICMSSTRet  := ObterConteudo(AuxNode.Childrens.Find('vICMSSTRet'), tcDe2);
    Item.Imposto.ICMS.vICMSDeson := ObterConteudo(AuxNode.Childrens.Find('vICMSDeson'), tcDe2);
    Item.Imposto.ICMS.vBCFCPSTRet:= ObterConteudo(AuxNode.Childrens.Find('vBCFCPSTRet'), tcDe2);
    Item.Imposto.ICMS.pFCPSTRet  := ObterConteudo(AuxNode.Childrens.Find('pFCPSTRet'), tcDe4);
    Item.Imposto.ICMS.vFCPSTRet  := ObterConteudo(AuxNode.Childrens.Find('vFCPSTRet'), tcDe2);
    Item.Imposto.ICMS.pST        := ObterConteudo(AuxNode.Childrens.Find('pST'), tcDe4);
    Item.Imposto.ICMS.motDesICMS  := StrTomotDesICMS(ok, ObterConteudo(AuxNode.Childrens.Find('motDesICMS'), tcStr));
    Item.Imposto.ICMS.pCredSN     := ObterConteudo(AuxNode.Childrens.Find('pCredSN'), tcDe2);
    Item.Imposto.ICMS.vCredICMSSN := ObterConteudo(AuxNode.Childrens.Find('vCredICMSSN'), tcDe2);
    Item.Imposto.ICMS.vBCSTDest   := ObterConteudo(AuxNode.Childrens.Find('vBCSTDest'), tcDe2);
    Item.Imposto.ICMS.vICMSSTDest := ObterConteudo(AuxNode.Childrens.Find('vICMSSTDest'), tcDe2);
    Item.Imposto.ICMS.pRedBCEfet  := ObterConteudo(AuxNode.Childrens.Find('pRedBCEfet'), tcDe4);
    Item.Imposto.ICMS.vBCEfet     := ObterConteudo(AuxNode.Childrens.Find('vBCEfet'), tcDe2);
    Item.Imposto.ICMS.pICMSEfet   := ObterConteudo(AuxNode.Childrens.Find('pICMSEfet'), tcDe4);
    Item.Imposto.ICMS.vICMSEfet   := ObterConteudo(AuxNode.Childrens.Find('vICMSEfet'), tcDe2);
    Item.Imposto.ICMS.vICMSSubstituto := ObterConteudo(AuxNode.Childrens.Find('vICMSSubstituto'), tcDe2);

    if (AuxNode.Name = 'ICMSPart') then
    begin
      case Item.Imposto.ICMS.CST of
          cst10 : Item.Imposto.ICMS.CST := cstPart10;
          cst90 : Item.Imposto.ICMS.CST := cstPart90;
      end;
    end
    else if (AuxNode.Name = 'ICMSST') then
    begin
      case Item.Imposto.ICMS.CST of
          cst41 : Item.Imposto.ICMS.CST := cstRep41;
          cst60 : Item.Imposto.ICMS.CST := cstRep60;
      end;
    end;
  end;

  AuxNode := ANode.Childrens.Find('ICMSUFDest');
  if (AuxNode <> nil) then
  begin
    Item.Imposto.ICMSUFDest.vBCUFDest      := ObterConteudo(AuxNode.Childrens.Find('vBCUFDest'), tcDe2);
    Item.Imposto.ICMSUFDest.vBCFCPUFDest   := ObterConteudo(AuxNode.Childrens.Find('vBCFCPUFDest'), tcDe2);
    Item.Imposto.ICMSUFDest.pFCPUFDest     := ObterConteudo(AuxNode.Childrens.Find('pFCPUFDest'), tcDe2);
    Item.Imposto.ICMSUFDest.pICMSUFDest    := ObterConteudo(AuxNode.Childrens.Find('pICMSUFDest'), tcDe2);
    Item.Imposto.ICMSUFDest.pICMSInter     := ObterConteudo(AuxNode.Childrens.Find('pICMSInter'), tcDe2);
    Item.Imposto.ICMSUFDest.pICMSInterPart := ObterConteudo(AuxNode.Childrens.Find('pICMSInterPart'), tcDe2);
    Item.Imposto.ICMSUFDest.vFCPUFDest     := ObterConteudo(AuxNode.Childrens.Find('vFCPUFDest'), tcDe2);
    Item.Imposto.ICMSUFDest.vICMSUFDest    := ObterConteudo(AuxNode.Childrens.Find('vICMSUFDest'), tcDe2);
    Item.Imposto.ICMSUFDest.vICMSUFRemet   := ObterConteudo(AuxNode.Childrens.Find('vICMSUFRemet'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('IPI');
  if (AuxNode <> nil) then
  begin
    Item.Imposto.IPI.clEnq    := ObterConteudo(AuxNode.Childrens.Find('clEnq'), tcStr);
    Item.Imposto.IPI.CNPJProd := ObterConteudo(AuxNode.Childrens.Find('CNPJProd'), tcStr);
    Item.Imposto.IPI.cSelo    := ObterConteudo(AuxNode.Childrens.Find('cSelo'), tcStr);
    Item.Imposto.IPI.qSelo    := ObterConteudo(AuxNode.Childrens.Find('qSelo'), tcInt);
    Item.Imposto.IPI.cEnq     := ObterConteudo(AuxNode.Childrens.Find('cEnq'), tcStr);

    // Inicializa CST com sendo Não tributada e conforme o TIPO entrada ou saida
    // Caso a Tag não seja informada sera gravada com sendo não tributada
    if NF3e.ide.tpNF = tnEntrada then
      Item.Imposto.IPI.CST := ipi53;
    if NF3e.ide.tpNF = tnSaida then
      Item.Imposto.IPI.CST := ipi03;

    AxNode := AuxNode.Childrens.Find('IPITrib');
    if (AxNode <> nil) then
    begin
      Item.Imposto.IPI.CST   := StrToCSTIPI(ok, ObterConteudo(AxNode.Childrens.Find('CST'), tcStr));
      Item.Imposto.IPI.vBC   := ObterConteudo(AxNode.Childrens.Find('vBC'), tcDe2);
      Item.Imposto.IPI.qUnid := ObterConteudo(AxNode.Childrens.Find('qUnid'), tcDe4);
      Item.Imposto.IPI.vUnid := ObterConteudo(AxNode.Childrens.Find('vUnid'), tcDe4);
      Item.Imposto.IPI.pIPI  := ObterConteudo(AxNode.Childrens.Find('pIPI'), tcDe2);
      Item.Imposto.IPI.vIPI  := ObterConteudo(AxNode.Childrens.Find('vIPI'), tcDe2);
    end;

    AxNode := AuxNode.Childrens.Find('IPINT');
    if (AxNode <> nil) then
    begin
      Item.Imposto.IPI.CST := StrToCSTIPI(ok, ObterConteudo(AxNode.Childrens.Find('CST'), tcStr));
    end;
  end;

  AuxNode := ANode.Childrens.Find('II');
  if (AuxNode <> nil) then
  begin
    Item.Imposto.II.vBc      := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    Item.Imposto.II.vDespAdu := ObterConteudo(AuxNode.Childrens.Find('vDespAdu'), tcDe2);
    Item.Imposto.II.vII      := ObterConteudo(AuxNode.Childrens.Find('vII'), tcDe2);
    Item.Imposto.II.vIOF     := ObterConteudo(AuxNode.Childrens.Find('vIOF'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('PIS');
  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];
  if (AuxNode <> nil) then
  begin
    Item.Imposto.PIS.CST       := StrToCSTPIS(ok, ObterConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    Item.Imposto.PIS.vBC       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    Item.Imposto.PIS.pPIS      := ObterConteudo(AuxNode.Childrens.Find('pPIS'), tcDe2);
    Item.Imposto.PIS.vPIS      := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    Item.Imposto.PIS.qBCProd   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    Item.Imposto.PIS.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
  end;

  AuxNode := ANode.Childrens.Find('PISST');
  if (AuxNode <> nil) then
  begin
    Item.Imposto.PISST.vBc       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    Item.Imposto.PISST.pPis      := ObterConteudo(AuxNode.Childrens.Find('pPIS'), tcDe2);
    Item.Imposto.PISST.qBCProd   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    Item.Imposto.PISST.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    Item.Imposto.PISST.vPIS      := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('COFINS');
  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];
  if (AuxNode <> nil) then
  begin
    Item.Imposto.COFINS.CST       := StrToCSTCOFINS(ok, ObterConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    Item.Imposto.COFINS.vBC       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    Item.Imposto.COFINS.pCOFINS   := ObterConteudo(AuxNode.Childrens.Find('pCOFINS'), tcDe2);
    Item.Imposto.COFINS.qBCProd   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    Item.Imposto.COFINS.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    Item.Imposto.COFINS.vCOFINS   := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('COFINSST');
  if (AuxNode <> nil) then
  begin
    Item.Imposto.COFINSST.vBC       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    Item.Imposto.COFINSST.pCOFINS   := ObterConteudo(AuxNode.Childrens.Find('pCOFINS'), tcDe2);
    Item.Imposto.COFINSST.qBCProd   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    Item.Imposto.COFINSST.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    Item.Imposto.COFINSST.vCOFINS   := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ISSQN');
  if (AuxNode <> nil) then
  begin
    Item.Imposto.ISSQN.vBC       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    Item.Imposto.ISSQN.vAliq     := ObterConteudo(AuxNode.Childrens.Find('vAliq'), tcDe2);
    Item.Imposto.ISSQN.vISSQN    := ObterConteudo(AuxNode.Childrens.Find('vISSQN'), tcDe2);
    Item.Imposto.ISSQN.cMunFG    := ObterConteudo(AuxNode.Childrens.Find('cMunFG'), tcInt);
    Item.Imposto.ISSQN.cListServ := ObterConteudo(AuxNode.Childrens.Find('cListServ'), tcStr);
    Item.Imposto.ISSQN.cSitTrib  := StrToISSQNcSitTrib( ok,  ObterConteudo(AuxNode.Childrens.Find('cSitTrib'), tcStr) ) ;
    Item.Imposto.ISSQN.vDeducao     := ObterConteudo(AuxNode.Childrens.Find('vDeducao'), tcDe2);
    Item.Imposto.ISSQN.vOutro       := ObterConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
    Item.Imposto.ISSQN.vDescIncond  := ObterConteudo(AuxNode.Childrens.Find('vDescIncond'), tcDe2);
    Item.Imposto.ISSQN.vDescCond    := ObterConteudo(AuxNode.Childrens.Find('vDescCond'), tcDe2);
    Item.Imposto.ISSQN.vISSRet      := ObterConteudo(AuxNode.Childrens.Find('vISSRet'), tcDe2);
    Item.Imposto.ISSQN.indISS       := StrToindISS(Ok, ObterConteudo(AuxNode.Childrens.Find('indISS'), tcStr));
    Item.Imposto.ISSQN.cServico     := ObterConteudo(AuxNode.Childrens.Find('cServico'), tcStr);
    Item.Imposto.ISSQN.cMun         := ObterConteudo(AuxNode.Childrens.Find('cMun'), tcInt);
    Item.Imposto.ISSQN.cPais        := ObterConteudo(AuxNode.Childrens.Find('cPais'), tcInt);
    Item.Imposto.ISSQN.nProcesso    := ObterConteudo(AuxNode.Childrens.Find('nProcesso'), tcStr);
    Item.Imposto.ISSQN.indIncentivo := StrToindIncentivo(Ok, ObterConteudo(AuxNode.Childrens.Find('indIncentivo'), tcStr));
  end;
  }
end;
*)
end.

