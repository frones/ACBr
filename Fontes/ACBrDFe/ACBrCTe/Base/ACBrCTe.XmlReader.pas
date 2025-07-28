{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrCTe.XmlReader;

interface

uses
  Classes, SysUtils, StrUtils, 
  ACBrXmlDocument, ACBrXmlReader, 
  ACBrCTe.Classes;

type
  TCTeXmlReader = class(TACBrXmlReader)
  private
    FCTe: TCTe;

    procedure ValidarXML(out infCTeNode: TACBrXmlNode; out CTeNode: TACBrXmlNode);
    procedure Ler_ProtCTe(const ANode: TACBrXmlNode);
    procedure Ler_InfCTe(const ANode: TACBrXmlNode);
    procedure Ler_Ide(const ANode: TACBrXmlNode);

    procedure Ler_CTe(const ANode: TACBrXmlNode);
    procedure Ler_CTeOS(const ANode: TACBrXmlNode);
    procedure Ler_CTeSimp(const ANode: TACBrXmlNode);
    procedure Ler_GTVe(const ANode: TACBrXmlNode);

    procedure Ler_InfPercurso(ANode: TACBrXmlNode);
    procedure Ler_Toma03(ANode: TACBrXmlNode);
    procedure Ler_Toma04(ANode: TACBrXmlNode);
    procedure Ler_Toma04EnderToma(ANode: TACBrXmlNode);
    procedure Ler_Compl(ANode: TACBrXmlNode);
    procedure Ler_Fluxo(ANode: TACBrXmlNode);
    procedure Ler_Entrega(ANode: TACBrXmlNode);
    procedure Ler_ObsCont(ANodeArr: TACBrXmlNodeArray);
    procedure Ler_ObsFisco(ANodeArr: TACBrXmlNodeArray);
    procedure Ler_Emit(ANode: TACBrXmlNode);
    procedure Ler_EnderEmit(ANode: TACBrXmlNode);
    procedure Ler_Toma(ANode: TACBrXmlNode);
    procedure Ler_TomaEnderToma(ANode: TACBrXmlNode);
    procedure Ler_Rem(ANode: TACBrXmlNode);
    procedure Ler_EnderReme(ANode: TACBrXmlNode);
    procedure Ler_Exped(ANode: TACBrXmlNode);
    procedure Ler_EnderExped(ANode: TACBrXmlNode);
    procedure Ler_Receb(ANode: TACBrXmlNode);
    procedure Ler_EnderReceb(ANode: TACBrXmlNode);
    procedure Ler_Dest(ANode: TACBrXmlNode);
    procedure Ler_EnderDest(ANode: TACBrXmlNode);
    procedure Ler_Origem(ANode: TACBrXmlNode);
    procedure Ler_Destino(ANode: TACBrXmlNode);
    procedure Ler_DetGTVe(ANode: TACBrXmlNode);
    procedure Ler_InfEspecie(ANodeArr: TACBrXmlNodeArray);
    procedure Ler_InfVeiculo(ANodeArr: TACBrXmlNodeArray);
    procedure Ler_VPrest(ANode: TACBrXmlNode);
    procedure Ler_Comp(ANodeArr: TACBrXmlNodeArray; comp: TCompCollection);
    procedure Ler_Imp(ANode: TACBrXmlNode);
    procedure Ler_InfCarga(ANode: TACBrXmlNode; infCarga: TInfCarga);
    procedure Ler_InfCTeNorm(ANode: TACBrXmlNode);
    procedure Ler_InfCTeComp(ANode: TACBrXmlNode);
    procedure Ler_InfCTeAnu(ANode: TACBrXmlNode);
    procedure Ler_DocAnt(ANode: TACBrXmlNode);
    procedure Ler_Seg(segArr: TACBrXmlNodeArray);
    procedure Ler_Rodo(ANode: TACBrXmlNode; rodo: TRodo);
    procedure Ler_RodoOS(ANode: TACBrXmlNode);
    procedure Ler_Aereo(ANode: TACBrXmlNode; aereo: TAereo);
    procedure Ler_Aquav(ANode: TACBrXmlNode; aquav: TAquav);
    procedure Ler_Ferrov(ANode: TACBrXmlNode);
    procedure Ler_Duto(ANode: TACBrXmlNode);
    procedure Ler_MultiModal(ANode: TACBrXmlNode);
    procedure Ler_VeicNovos(veicArr: TACBrXmlNodeArray);
    procedure Ler_Cobr(ANode: TACBrXmlNode);
    procedure Ler_InfGTVe(infGTVeArr: TACBrXmlNodeArray);
    procedure Ler_InfCTeSub(infCteSubNd: TACBrXmlNode; infCTeSub: TInfCteSub);
    procedure Ler_Signature(SignatureNd: TACBrXmlNode);
    procedure Ler_InfCTeSupl(infCTeSuplNd: TACBrXmlNode);
    procedure Ler_AutXML(ANode: TACBrXmlNode);
    procedure Ler_InfRespTec(ANode: TACBrXmlNode);
    procedure Ler_Total(ANode: TACBrXmlNode);
    procedure Ler_InfModal(ANode: TACBrXmlNode);
    procedure Ler_Det(ANode: TACBrXmlNode);
    procedure Ler_InfNFe(ANode: TACBrXmlNode; infNFe: TInfNFeCollection);
    procedure Ler_InfDCe(ANode: TACBrXmlNode);
    procedure Ler_InfDocAnt(ANode: TACBrXmlNode; infDocAnt: TinfDocAntCollection);

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
  public
    constructor Create(AOwner: TCTe); reintroduce;

    function LerXml: Boolean; override;

    property CTe: TCTe read FCTe;
  end;

implementation

uses
  ACBrUtil.Base, 
  pcnConversao, 
  ACBrXmlBase, 
  ACBrDFe.Conversao,
  pcteConversaoCTe;

{ TCTeXmlReader }

constructor TCTeXmlReader.Create(AOwner: TCTe);
begin
  inherited Create;

  FCTe := AOwner;
end;

procedure TCTeXmlReader.Ler_Aereo(ANode: TACBrXmlNode; aereo: TAereo);
var
  AuxNode, AuxNode2: TACBrXmlNode;
  AuxArr: TACBrXmlNodeArray;
  i: integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  aereo.nMinu := ObterConteudo(ANode.Childrens.FindAnyNs('nMinu'), tcInt);
  aereo.nOCA := ObterConteudo(ANode.Childrens.FindAnyNs('nOCA'), tcStr);
  aereo.dPrevAereo := ObterConteudo(ANode.Childrens.FindAnyNs('dPrevAereo'), tcDat);
  aereo.xLAgEmi := ObterConteudo(ANode.Childrens.FindAnyNs('xLAgEmi'), tcStr);
  aereo.IdT := ObterConteudo(ANode.Childrens.FindAnyNs('IdT'), tcStr);

  AuxNode := ANode.Childrens.FindAnyNs('tarifa');
  if Assigned(AuxNode) then
  begin
    aereo.tarifa.CL := ObterConteudo(AuxNode.Childrens.FindAnyNs('CL'), tcStr);
    aereo.tarifa.cTar := ObterConteudo(AuxNode.Childrens.FindAnyNs('cTar'), tcStr);
    aereo.tarifa.vTar := ObterConteudo(AuxNode.Childrens.FindAnyNs('vTar'), tcDe2);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('natCarga');
  if Assigned(AuxNode) then
  begin
    aereo.natCarga.xDime := ObterConteudo(AuxNode.Childrens.FindAnyNs('xDime'), tcStr);
    aereo.natCarga.cIMP := ObterConteudo(AuxNode.Childrens.FindAnyNs('cIMP'), tcStr);

    aereo.natCarga.cinfManu.Clear;
    for i := 0 to AuxNode.Childrens.Count-1 do
    begin
      if AuxNode.Childrens[i].Name = 'cInfManu' then
      begin
        with aereo.natCarga.cInfManu.New do
          nInfManu := StrToTpInfManu(ok, ObterConteudo(AuxNode.Childrens[i], tcStr));
      end;
    end;
  end;

  AuxArr := ANode.Childrens.FindAllAnyNs('peri');
  aereo.peri.Clear;
  for i := 0 to Length(AuxArr)-1 do
  begin
    aereo.peri.New;
    aereo.peri[i].nONU := ObterConteudo(AuxArr[i].Childrens.FindAnyNs('nONU'), tcStr);
    aereo.peri[i].qTotEmb := ObterConteudo(AuxArr[i].Childrens.FindAnyNs('qTotEmb'), tcStr);

    AuxNode2 := AuxArr[i].Childrens.FindAnyNs('infTotAP');
    if Assigned(AuxNode2) then
    begin
      aereo.peri[i].qTotProd := ObterConteudo(AuxNode2.Childrens.FindAnyNs('qTotProd'), tcStr);
      aereo.peri[i].uniAP := StrToUniMed(Ok, ObterConteudo(AuxNode2.Childrens.FindAnyNs('uniAP'), tcStr));
    end;
  end;
end;

procedure TCTeXmlReader.Ler_Aquav(ANode: TACBrXmlNode; aquav: TAquav);
var
  OK: boolean;
  infDocND: TACBrXmlNode;
  balsaArr, detContArr, lacreArr, infArr: TACBrXmlNodeArray;
  i, j: Integer;
begin
  if not Assigned(ANode) then exit;

  aquav.vPrest := ObterConteudo(ANode.Childrens.FindAnyNs('vPrest'), tcDe2);
  aquav.vAFRMM := ObterConteudo(ANode.Childrens.FindAnyNs('vAFRMM'), tcDe2);
  aquav.nBooking := ObterConteudo(ANode.Childrens.FindAnyNs('nBooking'), tcStr);
  aquav.nCtrl := ObterConteudo(ANode.Childrens.FindAnyNs('nCtrl'), tcStr);
  aquav.xNavio := ObterConteudo(ANode.Childrens.FindAnyNs('xNavio'), tcStr);
  aquav.nViag := ObterConteudo(ANode.Childrens.FindAnyNs('nViag'), tcStr);
  aquav.direc := StrToTpDirecao(ok, ObterConteudo(ANode.Childrens.FindAnyNs('direc'), tcStr));
  aquav.prtEmb := ObterConteudo(ANode.Childrens.FindAnyNs('prtEmb'), tcStr);
  aquav.prtTrans := ObterConteudo(ANode.Childrens.FindAnyNs('prtTrans'), tcStr);
  aquav.prtDest := ObterConteudo(ANode.Childrens.FindAnyNs('prtDest'), tcStr);
  aquav.tpNav := StrToTpNavegacao(ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpNav'), tcStr));
  aquav.irin := ObterConteudo(ANode.Childrens.FindAnyNs('irin'), tcStr);

  aquav.balsa.Clear;
  balsaArr := ANode.Childrens.FindAllAnyNs('balsa');
  for i := 0 to Length(balsaArr)-1 do
  begin
    aquav.balsa.New;
    aquav.balsa[i].xBalsa := ObterConteudo(balsaArr[i].Childrens.FindAnyNs('xBalsa'), tcStr);
  end;

  aquav.detCont.Clear;
  detContArr := ANode.Childrens.FindAllAnyNs('detCont');
  for i := 0 to Length(detContArr)-1 do
  begin
    aquav.detCont.New;
    aquav.detCont[i].nCont := ObterConteudo(detContArr[i].Childrens.FindAnyNs('nCont'), tcStr);

    aquav.detCont[i].Lacre.Clear;
    lacreArr := detContArr[i].Childrens.FindAllAnyNs('lacre');
    for j := 0 to Length(lacreArr)-1 do
    begin
      aquav.detCont[i].Lacre.New;
      aquav.detCont[i].Lacre[j].nLacre := ObterConteudo(lacreArr[j].Childrens.FindAnyNs('nLacre'), tcStr);
    end;

    infDocND := detContArr[i].Childrens.FindAnyNs('infDoc');
    if Assigned(infDocND) then
    begin
      infArr := infDocND.Childrens.FindAllAnyNs('infNF');
      for j := 0 to Length(infArr)-1 do
      begin
        aquav.detCont[i].infDoc.infNF.New;
        aquav.detCont[i].infDoc.infNF[j].serie := ObterConteudo(infArr[j].Childrens.FindAnyNs('serie'), tcStr);
        aquav.detCont[i].infDoc.infNF[j].nDoc := ObterConteudo(infArr[j].Childrens.FindAnyNs('nDoc'), tcStr);
        aquav.detCont[i].infDoc.infNF[j].unidRat := ObterConteudo(infArr[j].Childrens.FindAnyNs('unidRat'), tcDe2);
      end;

      infArr := infDocND.Childrens.FindAllAnyNs('infNFe');
      for j := 0 to Length(infArr)-1 do
      begin
        aquav.detCont[i].infDoc.infNFe.New;
        aquav.detCont[i].infDoc.infNFe[j].chave := ObterConteudo(infArr[j].Childrens.FindAnyNs('chave'), tcStr);
        aquav.detCont[i].infDoc.infNFe[j].unidRat := ObterConteudo(infArr[j].Childrens.FIndAnyNs('unidRat'), tcDe2);
      end;
    end;
  end;
end;

procedure TCTeXmlReader.Ler_AutXML(ANode: TACBrXmlNode);
var
  autXMLArr: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  autXMLArr := ANode.Childrens.FindAllAnyNs('autXML');
  FCTe.autXML.Clear;

  for i := 0 to Length(autXMLArr)-1 do
  begin
    FCTe.autXML.New;
    FCTe.autXML[i].CNPJCPF := ObterCNPJCPF(autXMLArr[i]);
  end;
end;

procedure TCTeXmlReader.Ler_Cobr(ANode: TACBrXmlNode);
var
  dupArr: TACBrXmlNodeArray;
  fatND: TACBrXmlNode;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  fatND := ANode.Childrens.FindAnyNs('fat');
  if Assigned(fatND) then
  begin
    FCTe.infCTeNorm.cobr.fat.nFat := ObterConteudo(fatND.Childrens.FindAnyNs('nFat'), tcStr);
    FCTe.infCTeNorm.cobr.fat.vOrig := ObterConteudo(fatND.Childrens.FindAnyNs('vOrig'), tcDe2);
    FCTe.infCTeNorm.cobr.fat.vDesc := ObterConteudo(fatND.Childrens.FindAnyNs('vDesc'), tcDe2);
    FCTe.infCTeNorm.cobr.fat.vLiq := ObterConteudo(fatND.Childrens.FindAnyNs('vLiq'), tcDe2);
  end;

  dupArr := ANode.Childrens.FindAllAnyNs('dup');
  FCTe.infCTeNorm.cobr.dup.Clear;
  for i := 0 to Length(dupArr)-1 do
  begin
    FCTe.infCTeNorm.cobr.dup.New;
    FCTe.infCTeNorm.cobr.dup[i].nDup := ObterConteudo(dupArr[i].Childrens.FindAnyNs('nDup'), tcStr);
    FCTe.infCTeNorm.cobr.dup[i].dVenc := ObterConteudo(dupArr[i].Childrens.FindANyNs('dVenc'), tcDat);
    FCTe.infCTeNorm.cobr.dup[i].vDup := ObterConteudo(dupArr[i].Childrens.FindAnyNs('vDup'), tcDe2);
  end;
end;

procedure TCTeXmlReader.Ler_Comp(ANodeArr: TACBrXmlNodeArray; comp: TCompCollection);
var
  i: Integer;
begin
  Comp.Clear;
  for i := 0 to Length(ANodeArr)-1 do
  begin
    Comp.New;
    Comp[i].xNome := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('xNome'), tcStr);
    Comp[i].vComp := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('vComp'), tcDe2);
  end;
end;

procedure TCTeXmlReader.Ler_Compl(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.Compl.xCaracAd := ObterConteudo(ANode.Childrens.FindAnyNs('xCaracAd'), tcStr);
  FCTe.Compl.xCaracSer := ObterConteudo(ANode.Childrens.FindAnyNs('xCaracSer'), tcStr);
  FCTe.Compl.xEmi := ObterConteudo(ANode.Childrens.FindAnyNs('xEmi'), tcStr);
  FCTe.Compl.origCalc := ObterConteudo(ANode.Childrens.FindAnyNs('origCalc'), tcStr);
  FCTe.Compl.destCalc := ObterConteudo(ANode.Childrens.FindAnyNs('destCalc'), tcStr);
  FCTe.Compl.xObs := ObterConteudo(ANode.Childrens.FindAnyNs('xObs'), tcStr);

  Ler_Fluxo(ANode.Childrens.FindAnyNs('fluxo'));
  Ler_Entrega(ANode.Childrens.FindAnyNs('Entrega'));
  Ler_ObsCont(ANode.Childrens.FindAllAnyNs('ObsCont'));
  Ler_ObsFisco(ANode.Childrens.FindAllAnyNs('ObsFisco'));
end;

procedure TCTeXmlReader.Ler_Dest(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.Dest.CNPJCPF := ObterCNPJCPF(ANode);
  FCTe.Dest.IE := ObterConteudo(ANode.Childrens.FindAnyNs('IE'), tcStr);
  FCTe.Dest.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  FCTe.Dest.fone := ObterConteudo(ANode.Childrens.FindANyNs('fone'), tcStr);
  FCTe.Dest.ISUF := ObterConteudo(ANode.Childrens.FindAnyNs('ISUF'), tcStr);
  FCTe.Dest.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);

  Ler_EnderDest(ANode.Childrens.FindAnyNs('enderDest'));
end;

procedure TCTeXmlReader.Ler_Destino(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.destino.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  FCTe.destino.Nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  FCTe.destino.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  FCTe.destino.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  FCTe.destino.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  FCTe.destino.xMun := ObterConteudo(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  FCTe.destino.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  FCTe.destino.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  FCTe.destino.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
end;

procedure TCTeXmlReader.Ler_Det(ANode: TACBrXmlNode);
var
  detArr: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  detArr := ANode.Childrens.FindAllAnyNs('det');
  FCTe.det.Clear;

  for i := 0 to Length(detArr)-1 do
  begin
    FCTe.det.New;

    FCTe.det[i].cMunIni := ObterConteudo(detArr[i].Childrens.FindAnyNs('cMunIni'), tcInt);
    FCTe.det[i].xMunIni := ObterConteudo(detArr[i].Childrens.FindAnyNs('xMunIni'), tcStr);
    FCTe.det[i].cMunFim := ObterConteudo(detArr[i].Childrens.FindAnyNs('cMunFim'), tcInt);
    FCTe.det[i].xMunFim := ObterConteudo(detArr[i].Childrens.FindAnyNs('xMunFim'), tcStr);
    FCTe.det[i].vPrest := ObterConteudo(detArr[i].Childrens.FindAnyNs('vPrest'), tcDe2);
    FCTe.det[i].vRec := ObterConteudo(detArr[i].Childrens.FindAnyNs('vRec'), tcDe2);

    Ler_Comp(detArr[i].Childrens.FindAllAnyNs('Comp'), FCTe.det[i].Comp);
    Ler_InfNFe(detArr[i], FCTe.det[i].infNFe);
    Ler_InfDocAnt(detArr[i], FCTe.det[i].infdocAnt);
  end;
end;

procedure TCTeXmlReader.Ler_DetGTVe(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.detGTV.qCarga := ObterConteudo(ANode.Childrens.FindAnyNs('qCarga'), tcDe4);

  Ler_InfEspecie(ANode.Childrens.FindAllAnyNs('infEspecie'));
  Ler_InfVeiculo(ANode.Childrens.FindAllAnyNs('infVeiculo'));
end;

procedure TCTeXmlReader.Ler_DocAnt(ANode: TACBrXmlNode);
var
  emiDocArr, idDocArr, idDocAntArr: TACBrXMLNodeArray;
  i, j, k: Integer;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  FCTe.infCTeNorm.docAnt.emiDocAnt.Clear;
  emiDocArr := ANode.Childrens.FindAllAnyNs('emiDocAnt');
  for i := 0 to Length(emiDocArr)-1 do
  begin
    FCTe.infCTeNorm.docAnt.emiDocAnt.New;
    FCTe.infCTeNorm.docAnt.emiDocAnt[i].CNPJCPF := ObterCNPJCPF(emiDocArr[i]);
    FCTe.infCTeNorm.docAnt.emiDocAnt[i].IE := ObterConteudo(emiDocArr[i].Childrens.FindAnyNs('IE'), tcStr);
    FCTe.infCTeNorm.docAnt.emiDocAnt[i].UF := ObterConteudo(emiDocArr[i].Childrens.FindAnyNs('UF'), tcStr);
    FCTe.infCTeNorm.docAnt.emiDocAnt[i].xNome := ObterConteudo(emiDocArr[i].Childrens.FindAnyNs('xNome'), tcStr);

    idDocAntArr := emiDocArr[i].Childrens.FindAllAnyNs('idDocAnt');
    for j := 0 to Length(idDocAntArr)-1 do
    begin
      FCTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt.New;

      idDocArr := idDocAntArr[j].Childrens.FindAllAnyNs('idDocAntPap');
      for k:=0 to Length(idDocArr)-1 do
      begin
        FCTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap.New;
        FCTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].tpDoc := StrToTpDocumentoAnterior(ok, ObterConteudo(idDocArr[k].Childrens.FindAnyNs('tpDoc'), tcStr));
        FCTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].serie := ObterConteudo(idDocArr[k].Childrens.FindAnyNs('serie'), tcStr);
        FCTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].subser := ObterConteudo(idDocArr[k].Childrens.FindAnyNs('subser'), tcStr);
        FCTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].nDoc := ObterConteudo(idDocArr[k].Childrens.FindAnyNs('nDoc'), tcStr);
        FCTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntPap[k].dEmi := ObterConteudo(idDocArr[k].Childrens.FindAnyNs('dEmi'), tcDat);
      end;

      idDocArr := idDocAntArr[j].Childrens.FindAllAnyNs('idDocAntEle');
      for k := 0 to Length(idDocArr)-1 do
      begin
        FCTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntEle.New;
        FCTe.infCTeNorm.docAnt.emiDocAnt[i].idDocAnt[j].idDocAntEle[k].chCTe := ObterConteudo(idDocArr[k].Childrens.FindAnyNs('chCTe'), tcStr);
      end;
    end;
  end;
end;

procedure TCTeXmlReader.Ler_Duto(ANode: TACBrXmlNode);
var
  OK: Boolean;
begin
  if not Assigned(ANode) then exit;

  FCTe.infCTeNorm.duto.vTar := ObterConteudo(ANode.Childrens.FindAnyNs('vTar'), tcDe6);
  FCTe.infCTeNorm.duto.dIni := ObterConteudo(ANode.Childrens.FindAnyNs('dIni'), tcDat);
  FCTe.infCTeNorm.duto.dFim := ObterConteudo(ANode.Childrens.FindAnyNs('dFim'), tcDat);
  FCTe.infCTeNorm.duto.classDuto := StrToclassDuto(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('classDuto'), tcStr));
  FCTe.infCTeNorm.duto.tpContratacao := StrTotpContratacao(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpContratacao'), tcStr));
  FCTe.infCTeNorm.duto.codPontoEntrada := ObterConteudo(ANode.Childrens.FindAnyNs('codPontoEntrada'), tcStr);
  FCTe.infCTeNorm.duto.codPontoSaida := ObterConteudo(ANode.Childrens.FindAnyNs('codPontoSaida'), tcStr);
  FCTe.infCTeNorm.duto.nContrato := ObterConteudo(ANode.Childrens.FindAnyNs('nContrato'), tcStr);
end;

procedure TCTeXmlReader.Ler_Emit(ANode: TACBrXmlNode);
var
  OK: Boolean;
begin
  if not Assigned(ANode) then exit;

  FCTe.emit.CNPJ := ObterCNPJCPF(ANode);
  FCTe.emit.IE := ObterConteudo(ANode.Childrens.FindAnyNs('IE'), tcStr);
  FCTe.emit.IEST := ObterConteudo(ANode.Childrens.FindAnyNs('IEST'), tcStr);
  FCTe.emit.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  FCTe.emit.xFant := ObterConteudo(ANode.Childrens.FindAnyNs('xFant'), tcStr);
  FCTe.emit.CRT := StrToCRTCTe(ok, ObterConteudo(ANode.Childrens.FindAnyNs('CRT'), tcStr));

  Ler_EnderEmit(ANode.Childrens.FindAnyNs('enderEmit'));
end;

procedure TCTeXmlReader.Ler_EnderDest(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.Dest.EnderDest.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  FCTe.Dest.EnderDest.Nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  FCTe.Dest.EnderDest.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  FCTe.Dest.EnderDest.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  FCTe.Dest.EnderDest.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  FCTe.Dest.EnderDest.xMun := ObterConteudo(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  FCTe.Dest.EnderDest.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  FCTe.Dest.EnderDest.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  FCTe.Dest.EnderDest.cPais := ObterConteudo(ANode.Childrens.FindAnyNs('cPais'), tcInt);
  FCTe.Dest.EnderDest.xPais := ObterConteudo(ANode.Childrens.FindAnyNs('xPais'), tcStr);
end;

procedure TCTeXmlReader.Ler_EnderEmit(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.emit.enderemit.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  FCTe.emit.enderemit.Nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  FCTe.emit.enderemit.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  FCTe.emit.enderemit.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  FCTe.emit.enderemit.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  FCTe.emit.enderemit.xMun := ObterConteudo(ANode.Childrens.FindANyNs('xMun'), tcStr);
  FCTe.emit.enderemit.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  FCTe.emit.enderemit.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  FCTe.emit.enderemit.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
end;

procedure TCTeXmlReader.Ler_EnderExped(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.Exped.EnderExped.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  FCTe.Exped.EnderExped.Nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  FCTe.Exped.EnderExped.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  FCTe.Exped.EnderExped.xBairro := ObterConteudo(ANode.CHildrens.FindANyNs('xBairro'), tcStr);
  FCTe.Exped.EnderExped.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'),tcInt);
  FCTe.Exped.EnderExped.xMun := ObterConteudo(ANode.Childrens.FindAnyNS('xMun'), tcStr);
  FCTe.Exped.EnderExped.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  FCTe.Exped.EnderExped.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  FCTe.Exped.EnderExped.cPais := ObterConteudo(ANode.Childrens.FindAnyNs('cPais'), tcInt);
  FCTe.Exped.EnderExped.xPais := ObterConteudo(ANode.Childrens.FindANyNs('xPais'), tcStr);
end;

procedure TCTeXmlReader.Ler_EnderReceb(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.receb.Enderreceb.xLgr := ObterConteudo(ANode.Childrens.FindAnyNS('xLgr'), tcStr);
  FCTe.receb.Enderreceb.Nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  FCTe.receb.Enderreceb.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  FCTe.receb.Enderreceb.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  FCTe.receb.Enderreceb.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  FCTe.receb.Enderreceb.xMun := ObterConteudo(ANode.Childrens.FindANyNs('xMun'), tcStr);
  FCTe.receb.Enderreceb.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  FCTe.receb.Enderreceb.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  FCTe.receb.Enderreceb.cPais := ObterConteudo(ANode.Childrens.FindANyNs('cPais'), tcInt);
  FCTe.receb.Enderreceb.xPais := ObterConteudo(ANode.Childrens.FindAnyNs('xPais'), tcStr);
end;

procedure TCTeXmlReader.Ler_EnderReme(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.Rem.enderReme.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  FCTe.Rem.enderReme.Nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  FCTe.Rem.enderReme.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  FCTe.Rem.enderReme.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  FCTe.Rem.enderReme.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  FCTe.Rem.enderReme.xMun := ObterConteudo(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  FCTe.Rem.enderReme.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  FCTe.Rem.enderReme.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  FCTe.Rem.enderReme.cPais := ObterConteudo(ANode.Childrens.FindAnyNs('cPais'), tcInt);
  FCTe.Rem.enderReme.xPais := ObterConteudo(ANode.Childrens.FindAnyNs('xPais'), tcStr);
end;

procedure TCTeXmlReader.Ler_Entrega(ANode: TACBrXmlNode);
var
  OK: Boolean;
  ANodeAux: TACBrXmlNode;
begin
  if not Assigned(ANode) then exit;

  FCTe.Compl.Entrega.TipoData := tdNaoInformado;
  FCTe.Compl.Entrega.TipoHora := thNaoInformado;

  if Assigned(ANode.Childrens.FindAnyNs('semData')) then
  begin
    ANodeAux := ANode.Childrens.FindAnyNs('semData');
    FCTe.Compl.Entrega.TipoData := tdSemData;
    FCTe.Compl.Entrega.semData.tpPer := StrTotpDataPeriodo(Ok, ObterConteudo(ANodeAux.Childrens.FindAnyNS('tpPeriodo'), tcStr));
  end else
  if Assigned(ANode.Childrens.FindAnyNs('comData'))  then
  begin
    ANodeAux := ANode.Childrens.FindAnyNs('comData');
    FCTe.Compl.Entrega.TipoDAta := tdNaData;
    FCTe.Compl.Entrega.comData.tpPer := StrToTpDataPeriodo(Ok, ObterConteudo(ANodeAux.Childrens.FindAnyNs('tpPer'), tcStr));
    FCTe.Compl.Entrega.comData.dProg := ObterConteudo(ANodeAux.Childrens.FindAnyNs('dProg'), tcDat);
  end else
  if Assigned(ANode.Childrens.FindAnyNs('noPeriodo')) then
  begin
    ANodeAux := ANode.Childrens.FindAnyNs('noPeriodo');
    FCTe.Compl.Entrega.TipoData := tdNoPeriodo;
    FCTe.Compl.Entrega.noPeriodo.tpPer := StrToTpDataPeriodo(ok, ObterConteudo(ANodeAux.Childrens.FindAnyNs('tpPer'), tcStr));
    FCTe.Compl.Entrega.noPeriodo.dIni := ObterConteudo(ANodeAux.Childrens.FindAnyNs('dIni'), tcDat);
    FCTe.Compl.Entrega.noPeriodo.dFim := ObterConteudo(ANodeAux.Childrens.FindAnyNs('dFim'), tcDat);
  end else
  if Assigned(ANode.Childrens.FindAnyNs('semHora')) then
  begin
    ANodeAux := ANode.Childrens.FindAnyNs('semHora');
    FCTe.Compl.Entrega.TipoHora := thSemHorario;
    FCTe.Compl.Entrega.semHora.tpHor := StrToTpHorarioIntervalo(ok, ObterConteudo(ANodeAux.Childrens.FindAnyNs('tpHor'), tcStr));
  end else
  if Assigned(ANode.Childrens.FindAnyNs('comHora')) then
  begin
    ANodeAux := ANode.Childrens.FindAnyNs('comHora');
    FCTe.Compl.Entrega.TipoHora := thNoHorario;
    FCTe.Compl.Entrega.comHora.tpHor := StrToTpHorarioIntervalo(ok, ObterConteudo(ANodeAux.Childrens.FindAnyNs('tpHor'), tcStr));
    FCTe.Compl.Entrega.comHora.hProg := ObterConteudo(ANodeAux.Childrens.FindAnyNs('hProg'), tcHor);
  end else
  if Assigned(ANode.Childrens.FindAnyNs('noInter')) then
  begin
    ANodeAux := ANode.Childrens.FindAnyNs('noInter');
    FCTe.Compl.Entrega.TipoHora := thNoIntervalo;
    FCTe.Compl.Entrega.noInter.tpHor := StrToTpHorarioIntervalo(ok, ObterConteudo(ANodeAux.Childrens.FindAnyNs('tpHor'), tcStr));
    FCTe.Compl.Entrega.noInter.hIni := ObterConteudo(ANodeAux.Childrens.FindAnyNs('hIni'), tcHor);
    FCTe.Compl.Entrega.noInter.hFim := ObterConteudo(ANodeAux.Childrens.FindAnyNs('hFim'), tcHor);
  end;
end;

procedure TCTeXmlReader.Ler_Exped(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.Exped.CNPJCPF := ObterCNPJCPF(ANode);
  FCTe.Exped.IE := ObterConteudo(ANode.Childrens.FindAnyNs('IE'), tcStr);
  FCTe.Exped.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  FCTe.Exped.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  FCTe.Exped.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);

  Ler_EnderExped(ANode.Childrens.FindAnyNs('enderExped'));
end;

procedure TCTeXmlReader.Ler_Ferrov(ANode: TACBrXmlNode);
var
  Ok: Boolean;
  tratMutND, enderFerroND: TACBrXmlNode;
  ferroEnvArr: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  FCTe.infCTeNorm.ferrov.tpTraf := StrToTpTrafego(ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpTraf'), tcStr));
  FCTe.infCTeNorm.ferrov.fluxo := ObterConteudo(ANode.Childrens.FindAnyNs('fluxo'), tcStr);
  FCTe.infCTeNorm.ferrov.idTrem := ObterConteudo(ANode.Childrens.FindAnyNs('idTrem'), tcStr);
  FCTe.infCTeNorm.ferrov.vFrete := ObterConteudo(ANode.Childrens.FindANyNs('vFrete'), tcDe2);

  tratMutND := ANode.Childrens.FindAnyNs('trafMut');
  if Assigned(tratMutND) then
  begin
    FCTe.infCTeNorm.ferrov.trafMut.respFat := StrToTrafegoMutuo(ok, ObterConteudo(tratMutND.Childrens.FindAnyNs('respFat'), tcStr));
    FCTe.infCTeNorm.ferrov.trafMut.ferrEmi := StrToTrafegoMutuo(ok, ObterConteudo(tratMutND.Childrens.FindAnyNs('ferrEmi'), tcStr));
    FCTe.infCTeNorm.ferrov.trafMut.vFrete := ObterConteudo(tratMutND.Childrens.FindANyNs('vFrete'), tcDe2);
    FCTe.infCTeNorm.ferrov.trafMut.chCTeFerroOrigem := ObterConteudo(tratMutND.Childrens.FindAnyNs('chCTeFerroOrigem'), tcStr);

    FCTe.infCTeNorm.ferrov.ferroEnv.Clear;
    ferroEnvArr := tratMutND.Childrens.FindAllAnyNs('ferroEnv');
    for i := 0 to Length(ferroEnvArr)-1 do
    begin
      FCTe.infCTeNorm.ferrov.ferroEnv.New;
      FCTe.infCTeNorm.ferrov.ferroEnv[i].CNPJ := ObterConteudo(ferroEnvArr[i].Childrens.FindAnyNs('CNPJ'), tcStr);
      FCTe.infCTeNorm.ferrov.ferroEnv[i].cInt := ObterConteudo(ferroEnvArr[i].Childrens.FindAnyNs('cInt'), tcStr);
      FCTe.infCTeNorm.ferrov.ferroEnv[i].IE := ObterConteudo(ferroEnvArr[i].Childrens.FindAnyNs('IE'), tcStr);
      FCTe.infCTeNorm.ferrov.ferroEnv[i].xNome := ObterConteudo(ferroEnvArr[i].Childrens.FindAnyNs('xNome'), tcStr);

      enderFerroND := ferroEnvArr[i].Childrens.FindAnyNs('enderFerro');
      if Assigned(enderFerroND) then
      begin
        FCTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.xLgr := ObterConteudo(enderFerroND.Childrens.FindAnyNs('xLgr'), tcStr);
        FCTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.nro := ObterConteudo(enderFerroND.Childrens.FindAnyNs('nro'), tcStr);
        FCTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.xCpl := ObterConteudo(enderFerroND.Childrens.FindAnyNs('xCpl'), tcStr);
        FCTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.xBairro := ObterConteudo(enderFerroND.Childrens.FindAnyNs('xBairro'), tcStr);
        FCTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.cMun := ObterConteudo(enderFerroND.Childrens.FindAnyNs('cMun'), tcInt);
        FCTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.xMun := ObterConteudo(enderFerroND.Childrens.FindAnyNs('xMun'), tcStr);
        FCTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.CEP := ObterConteudo(enderFerroND.Childrens.FindAnyNs('CEP'), tcInt);
        FCTe.infCTeNorm.ferrov.ferroEnv[i].EnderFerro.UF := ObterConteudo(enderFerroND.Childrens.FindAnyNs('UF'), tcStr);
      end;
    end;
  end;
end;

procedure TCTeXmlReader.Ler_Fluxo(ANode: TACBrXmlNode);
var
  pass: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  FCTe.Compl.fluxo.xOrig := ObterConteudo(ANode.Childrens.FindAnyNs('xOrig'), tcStr);
  FCTe.Compl.fluxo.xDest := ObterConteudo(ANode.Childrens.FindAnyNs('xDest'), tcStr);
  FCTe.Compl.fluxo.xRota := ObterConteudo(ANode.Childrens.FindAnyNs('xRota'), tcStr);

  pass := ANode.Childrens.FindAllAnyNs('pass');

  for i := 0 to Length(pass)-1 do
  begin
    FCTe.Compl.fluxo.pass.New;
    FCTe.Compl.fluxo.pass[i].xPass := ObterConteudo(pass[i].Childrens.FindAnyNs('xPass'), tcStr);
  end;
end;

procedure TCTeXmlReader.Ler_Ide(const ANode: TACBrXmlNode);
var
  OK: Boolean;
  sAux: string;
begin
  if not Assigned(ANode) then exit;

  FCTe.Ide.cUF := ObterConteudo(ANode.Childrens.FindAnyNs('cUF'), tcInt);
  FCTe.Ide.cCT := ObterConteudo(ANode.Childrens.FindAnyNs('cCT'), tcInt);
  FCTe.Ide.CFOP := ObterConteudo(ANode.Childrens.FindAnyNs('CFOP'), tcStr);
  FCTe.Ide.natOp := ObterConteudo(ANode.Childrens.FindAnyNs('natOp'), tcStr);

  FCTe.Ide.modelo := ObterConteudo(ANode.Childrens.FindAnyNs('mod'), tcStr);
  FCTe.Ide.serie := ObterConteudo(ANode.Childrens.FindAnyNs('serie'), tcStr);
  FCTe.Ide.nCT := ObterConteudo(ANode.Childrens.FindAnyNs('nCT'), tcInt);
  FCTe.Ide.dhEmi := ObterConteudo(ANode.Childrens.FindAnyNs('dhEmi'), tcDatHor);
  FCTe.Ide.tpImp := StrToTpImp(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpImp'), tcStr));
  FCTe.Ide.tpEmis := StrToTpEmis(OK, ObterConteudo(ANode.Childrens.FindAnyNs('tpEmis'), tcStr));
  FCTe.Ide.cDV := ObterConteudo(ANode.Childrens.FindAnyNs('cDV'), tcInt);
  FCTe.Ide.tpAmb := StrToTpAmb(OK, ObterConteudo(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  FCTe.Ide.tpCTe := StrTotpCTe(OK, ObterConteudo(ANode.Childrens.FindAnyNs('tpCTe'), tcStr));
  FCTe.Ide.procEmi := StrToprocEmi(OK, ObterConteudo(ANode.Childrens.FindAnyNs('procEmi'), tcStr));
  FCTe.Ide.verProc := ObterConteudo(ANode.Childrens.FindAnyNs('verProc'), tcStr);

  if ObterConteudo(ANode.Childrens.FindAnyNs('indGlobalizado'), tcStr) = '1' then
    FCTe.Ide.indGlobalizado := tiSim
  else
    FCTe.Ide.IndGlobalizado := tiNao;

  FCTe.Ide.refCTE := ObterConteudo(ANode.Childrens.FindAnyNs('refCTE'), tcStr);
  FCTe.Ide.cMunEnv := ObterConteudo(ANode.Childrens.FindAnyNs('cMunEnv'), tcInt);
  FCTe.Ide.xMunEnv := ObterConteudo(ANode.Childrens.FindAnyNs('xMunEnv'), tcStr);
  FCTe.Ide.UFEnv := ObterConteudo(ANode.Childrens.FindAnyNs('UFEnv'), tcStr);
  FCTe.Ide.modal := StrToTpModal(OK, ObterConteudo(ANode.Childrens.FindAnyNs('modal'), tcStr));
  FCTe.Ide.tpServ := StrtoTpServ(Ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpServ'), tcStr));
  FCTe.Ide.cMunIni := ObterConteudo(ANode.Childrens.FindAnyNs('cMunIni'), tcInt);
  FCTe.Ide.xMunIni := ObterConteudo(ANode.Childrens.FindAnyNs('xMunIni'), tcStr);
  FCTe.Ide.UFIni := ObterConteudo(ANode.Childrens.FindAnyNs('UFIni'), tcStr);
  FCTe.Ide.cMunFim := ObterConteudo(ANode.Childrens.FindAnyNs('cMunFim'), tcInt);
  FCTe.Ide.xMunFim := ObterConteudo(ANode.Childrens.FindAnyNs('xMunFim'), tcStr);
  FCTe.Ide.UFFim := ObterConteudo(ANode.Childrens.FindAnyNs('UFFim'), tcStr);

  sAux := ObterConteudo(ANode.Childrens.FindAnyNs('retira'), tcStr);

  if sAux <> '' then
    FCTe.Ide.retira := StrToTpRetira(ok, sAux);

  FCTe.Ide.xdetretira := ObterConteudo(ANode.Childrens.FindAnyNs('xDetRetira'), tcStr);
  FCTe.Ide.dhCont := ObterConteudo(ANode.Childrens.FindAnyNs('dhCont'), tcDatHor);
  FCTe.Ide.xJust := ObterConteudo(ANode.Childrens.FindAnyNs('xJust'), tcStr);

  FCTe.Ide.IndIEToma := StrToindIEDest(OK, ObterConteudo(ANode.Childrens.FindAnyNs('indIEToma'), tcStr));

  FCTe.Ide.dhSaidaOrig := ObterConteudo(ANode.Childrens.FindAnyNs('dhSaidaOrig'), tcDatHor);
  FCTe.Ide.dhChegadaDest := ObterConteudo(ANode.Childrens.FindAnyNs('dhChegadaDest'), tcDatHor);

  // Reforma Tritutária
  Ler_gCompraGov(CTe.Ide.gCompraGov, ANode.Childrens.Find('gCompraGov'));

  Ler_InfPercurso(ANode);
  Ler_Toma03(ANode);
  Ler_Toma04(ANode);
end;

procedure TCTeXmlReader.Ler_Imp(ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;

  procedure LerICMS00(ANode: TACBrXmlNode);
  begin
    if not Assigned(ANode) then exit;

    FCTe.Imp.ICMS.SituTrib := cst00;
    FCTe.Imp.ICMS.ICMS00.CST := StrToCSTICMS(ok, ObterConteudo(ANode.Childrens.FindAnyNs('CST'), tcStr));
    FCTe.Imp.ICMS.ICMS00.vBC := ObterConteudo(ANode.Childrens.FindAnyNs('vBC'), tcDe2);
    FCTe.Imp.ICMS.ICMS00.pICMS := ObterConteudo(ANode.Childrens.FindAnyNs('pICMS'), tcDe2);
    FCTe.Imp.ICMS.ICMS00.vICMS := ObterConteudo(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);
  end;

  procedure LerICMS20(ANode: TACBrXmlNode);
  begin
    if not Assigned(ANode) then exit;

    FCTe.Imp.ICMS.SituTrib := cst20;
    FCTe.Imp.ICMS.ICMS20.CST := StrToCSTICMS(ok, ObterConteudo(ANode.Childrens.FindAnyNs('CST'), tcStr));
    FCTe.Imp.ICMS.ICMS20.pRedBC := ObterConteudo(ANode.Childrens.FindAnyNs('pRedBC'), tcDe2);
    FCTe.Imp.ICMS.ICMS20.vBC := ObterConteudo(ANode.Childrens.FindAnyNs('vBC'), tcDe2);
    FCTe.Imp.ICMS.ICMS20.pICMS := ObterConteudo(ANode.Childrens.FindAnyNs('pICMS'), tcDe2);
    FCTe.Imp.ICMS.ICMS20.vICMS := ObterConteudo(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);
  end;

  procedure LerICMS45(ANode: TACBrXmlNode);
  begin
    if not Assigned(ANode) then exit;

    FCTe.Imp.ICMS.ICMS45.CST := StrToCSTICMS(ok, ObterConteudo(ANode.Childrens.FindAnyNs('CST'), tcStr));

    case FCTe.Imp.ICMS.ICMS45.CST of
      cst40: FCTe.Imp.ICMS.SituTrib := cst40;
      cst41: FCTe.Imp.ICMS.SituTrib := cst41;
      cst51: FCTe.Imp.ICMS.SituTrib := cst51;
    end;
  end;

  procedure LerICMS60(ANode: TACBrXmlNode);
  begin
    if not Assigned(ANode) then exit;
    FCTe.Imp.ICMS.SituTrib := cst60;
    FCTe.Imp.ICMS.ICMS60.CST := StrToCSTICMS(ok, ObterConteudo(ANode.Childrens.FindAnyNs('CST'), tcStr));
    FCTe.Imp.ICMS.ICMS60.vBCSTRet := ObterConteudo(ANode.Childrens.FindAnyNs('vBCSTRet'), tcDe2);
    FCTe.Imp.ICMS.ICMS60.vICMSSTRet := ObterConteudo(ANode.Childrens.FindAnyNs('vICMSSTRet'), tcDe2);
    FCTe.Imp.ICMS.ICMS60.pICMSSTRet := ObterConteudo(ANode.Childrens.FindANyNs('pICMSSTRet'), tcDe2);
    FCTe.Imp.ICMS.ICMS60.vCred := ObterConteudo(ANode.Childrens.FindAnyNs('vCred'), tcDe2);
  end;

  procedure LerICMS90(ANode: TACBrXmlNode);
  begin
    if not Assigned(ANode) then exit;

    FCTe.Imp.ICMS.SituTrib := cst90;
    FCTe.Imp.ICMS.ICMS90.CST := StrToCSTICMS(ok, ObterConteudo(ANode.Childrens.FindAnyNs('CST'), tcStr));
    FCTe.Imp.ICMS.ICMS90.pRedBC := ObterConteudo(ANode.Childrens.FindAnyNs('pRedBC'), tcDe2);
    FCTe.Imp.ICMS.ICMS90.vBC := ObterConteudo(ANode.Childrens.FindAnyNs('vBC'), tcDe2);
    FCTe.Imp.ICMS.ICMS90.pICMS := ObterConteudo(ANode.Childrens.FindAnyNs('pICMS'), tcDe2);
    FCTe.Imp.ICMS.ICMS90.vICMS := ObterConteudo(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);
    FCTe.Imp.ICMS.ICMS90.vCred := ObterConteudo(ANode.Childrens.FindAnyNs('vCred'), tcDe2);
  end;

  procedure LerICMSOutrasUF(ANode: TACBrXmlNode);
  begin
    if not Assigned(ANode) then exit;

    // ICMS devido à UF de origem da prestação, quando diferente da UF do emitente
    FCTe.Imp.ICMS.SituTrib := cstICMSOutraUF;
    FCTe.Imp.ICMS.ICMSOutraUF.CST := StrToCSTICMS(ok, ObterConteudo(ANode.Childrens.FindAnyNs('CST'), tcStr));
    FCTe.Imp.ICMS.ICMSOutraUF.pRedBCOutraUF := ObterConteudo(ANode.Childrens.FindAnyNs('pRedBCOutraUF'), tcDe2);
    FCTe.Imp.ICMS.ICMSOutraUF.vBCOutraUF := ObterConteudo(ANode.Childrens.FindAnyNs('vBCOutraUF'), tcDe2);
    FCTe.Imp.ICMS.ICMSOutraUF.pICMSOutraUF := ObterConteudo(ANode.Childrens.FindAnyNs('pICMSOutraUF'), tcDe2);
    FCTe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF := ObterConteudo(ANode.Childrens.FindAnyNs('vICMSOutraUF'), tcDe2);
  end;

  procedure LerICMSSN(ANode: TACBrXmlNode);
  begin
    if not Assigned(ANode) then exit;

    // ICMS Simples Nacional
    FCTe.Imp.ICMS.SituTrib := cstICMSSN;

    FCTe.Imp.ICMS.ICMSSN.CST := StrToCSTICMS(ok, ObterConteudo(ANode.Childrens.FindAnyNs('CST'), tcStr));

    FCTe.Imp.ICMS.ICMSSN.indSN := ObterConteudo(ANode.Childrens.FindAnyNs('indSN'), tcInt);
  end;

  procedure LerICMSUFFim(ANode: TACBrXmlNode);
  begin
    if not Assigned(ANode) then exit;

    FCTe.Imp.ICMSUFFim.vBCUFFim := ObterConteudo(ANode.Childrens.FindAnyNs('vBCUFFim'), tcDe2);
    FCTe.Imp.ICMSUFFim.pFCPUFFim := ObterConteudo(ANode.Childrens.FindAnyNs('pFCPUFFim'), tcDe2);
    FCTe.Imp.ICMSUFFim.pICMSUFFim := ObterConteudo(ANode.Childrens.FindAnyNs('pICMSUFFim'), tcDe2);
    FCTe.Imp.ICMSUFFim.pICMSInter := ObterConteudo(ANode.Childrens.FindAnyNs('pICMSInter'), tcDe2);
    FCTe.Imp.ICMSUFFim.pICMSInterPart := ObterConteudo(ANode.Childrens.FindAnyNs('pICMSInterPart'), tcDe2);
    FCTe.Imp.ICMSUFFim.vFCPUFFim := ObterConteudo(ANode.Childrens.FindAnyNs('vFCPUFFim'), tcDe2);
    FCTe.Imp.ICMSUFFim.vICMSUFFim := ObterConteudo(ANode.Childrens.FindAnyNs('vICMSUFFim'), tcDe2);
    FCTe.Imp.ICMSUFFim.vICMSUFIni := ObterConteudo(ANode.Childrens.FindAnyNs('vICMSUFIni'), tcDe2);
  end;

  procedure LerInfTribFed(ANode: TACBrXmlNode);
  begin
    if not Assigned(ANode) then exit;

    FCTe.Imp.infTribFed.vPIS := ObterConteudo(ANode.Childrens.FindAnyNs('vPIS'), tcDe2);
    FCTe.Imp.infTribFed.vCOFINS := ObterConteudo(ANode.Childrens.FindAnyNs('vCOFINS'), tcDe2);
    FCTe.Imp.infTribFed.vIR := ObterConteudo(ANode.Childrens.FindAnyNs('vIR'), tcDe2);
    FCTe.Imp.infTribFed.vINSS := ObterConteudo(ANode.Childrens.FindAnyNs('vINSS'), tcDe2);
    FCTe.Imp.infTribFed.vCSLL := ObterConteudo(ANode.Childrens.FindAnyNs('vCSLL'), tcDe2);
  end;
begin
  if not Assigned(ANode) then exit;

  FCTe.Imp.vTotTrib := ObterConteudo(ANode.Childrens.FindAnyNs('vTotTrib'), tcDe2);
  FCTe.Imp.infAdFisco := ObterConteudo(ANode.Childrens.FindAnyNs('infAdFisco'), tcStr);

  AuxNode := ANode.Childrens.FindAnyNs('ICMS');

  if Assigned(AuxNode) then
  begin
    LerICMS00(AuxNode.Childrens.FindAnyNs('ICMS00'));
    LerICMS20(AuxNode.Childrens.FindAnyNs('ICMS20'));
    LerICMS45(AuxNode.Childrens.FindAnyNs('ICMS45'));
    LerICMS60(AuxNode.Childrens.FindAnyNs('ICMS60'));
    LerICMS90(AuxNode.Childrens.FindAnyNs('ICMS90'));
    LerICMSOutrasUF(AuxNode.Childrens.FindAnyNs('ICMSOutraUF'));
    LerICMSSN(AuxNode.Childrens.FindAnyNs('ICMSSN'));
  end;

  LerICMSUFFim(ANode.Childrens.FindAnyNs('ICMSUFFim'));
  LerInfTribFed(ANode.Childrens.FindAnyNs('infTribFed'));

  // Reforma Tributária
  Ler_IBSCBS(ANode.Childrens.Find('IBSCBS'), FCTe.Imp.IBSCBS);

  FCTe.Imp.vTotDFe := ObterConteudo(ANode.Childrens.FindAnyNs('vTotDFe'), tcDe2);
end;

procedure TCTeXmlReader.Ler_InfCarga(ANode: TACBrXmlNode; infCarga: TInfCarga);
var
  ANodeArr: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  infCarga.vCarga := ObterConteudo(ANode.Childrens.FindAnyNs('vCarga'), tcDe2);
  InfCarga.proPred := ObterConteudo(ANode.Childrens.FindAnyNs('proPred'), tcStr);
  InfCarga.xOutCat := ObterConteudo(ANode.Childrens.FindAnyNs('xOutCat'), tcStr);
  infCarga.vCargaAverb := ObterConteudo(ANode.Childrens.FindAnyNs('vCargaAverb'), tcDe2);

  ANodeArr := ANode.Childrens.FindAllAnyNs('infQ');

  InfCarga.infQ.Clear;
  for i := 0 to Length(ANodeArr)-1 do
  begin
    InfCarga.infQ.New;
    InfCarga.infQ[i].cUnid := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('cUnid'), tcStr);
    InfCarga.infQ[i].tpMed := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('tpMed'), tcStr);
    InfCarga.infQ[i].qCarga := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('qCarga'), tcDe4);
  end;
end;

procedure TCTeXmlReader.Ler_InfCTe(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  Ler_Ide(ANode.Childrens.FindAnyNs('ide'));

  case FCTe.ide.tpCTe of
    tcGTVe: Ler_GTVe(ANode);

    tcCTeSimp,
    tcSubstCTeSimpl: Ler_CTeSimp(ANode);
  else
    if FCTe.ide.modelo = 57 then
      Ler_CTe(ANode)
    else
      Ler_CTeOS(ANode);
  end;
end;

procedure TCTeXmlReader.Ler_InfCTeNorm(ANode: TACBrXmlNode);
var
  AuxNode, infGlobalizadoNd: TACBrXmlNode;
  ANodeArr, infArr, infUnidArr, UnidArr: TACBrXmlNodeArray;
  i, j, k, l: Integer;
  OK: Boolean;
begin
  if not Assigned(ANode) then exit;

  FCTe.infCTeNorm.refCTeCanc := ObterConteudo(ANode.Childrens.FindAnyNs('refCTeCanc'), tcStr);

  AuxNode := ANode.Childrens.FindAnyNs('infServico');

  if Assigned(AuxNode) then
  begin
    FCTe.infCTeNorm.infServico.xDescServ := ObterConteudo(AuxNode.Childrens.FindAnyNs('xDescServ'), tcStr);
    AuxNode := AuxNode.Childrens.FindAnyNs('infQ');
    if Assigned(AuxNode) then
      FCTe.infCTeNorm.infServico.qCarga := ObterConteudo(AuxNode.Childrens.FindAnyNs('qCarga'), tcDe4);
  end;

  ANodeArr := ANode.Childrens.FindAllAnyNs('infDocRef');

  FCTe.infCTeNorm.infDocRef.Clear;
  for i := 0 to Length(ANodeArr)-1 do
  begin
    FCTe.infCTeNorm.infDocRef.New;
    FCTe.infCTeNorm.infDocRef[i].nDoc := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('nDoc'), tcEsp);
    FCTe.infCTeNorm.infDocRef[i].serie := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('serie'), tcStr);
    FCTe.infCTeNorm.infDocRef[i].subserie := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('subserie'), tcStr);
    FCTe.infCTeNorm.infDocRef[i].dEmi := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('dEmi'), tcDat);
    FCTe.infCTeNorm.infDocRef[i].vDoc := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('vDoc'), tcDe2);

    FCTe.infCTeNorm.infDocRef[i].chBPe := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('chBPe'), tcStr);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('infCarga');

  Ler_InfCarga(AuxNode, FCTe.infCTeNorm.infCarga);

  AuxNode := ANode.Childrens.FindAnyNs('infDoc');
  if Assigned(AuxNode) then
  begin
    FCTe.infCTeNorm.infDoc.infNF.Clear;
    infArr := AuxNode.Childrens.FindAllAnyNs('infNF');
    for i := 0 to Length(infArr)-1 do
    begin
      FCTe.infCTeNorm.infDoc.infNF.New;
      FCTe.infCTeNorm.infDoc.InfNF[i].nRoma := ObterConteudo(infArr[i].Childrens.FindAnyNs('nRoma'), tcStr);
      FCTe.infCTeNorm.infDoc.InfNF[i].nPed := ObterConteudo(infArr[i].Childrens.FindAnyNs('nPed'), tcStr);
      FCTe.infCTeNorm.infDoc.InfNF[i].Modelo := StrToModeloNF(Ok, ObterConteudo(infArr[i].Childrens.FindAnyNs('mod'), tcStr));
      FCTe.infCTeNorm.infDoc.InfNF[i].serie := ObterConteudo(infArr[i].Childrens.FindAnyNs('serie'), tcStr);
      FCTe.infCTeNorm.infDoc.InfNF[i].nDoc := ObterConteudo(infArr[i].Childrens.FindAnyNs('nDoc'), tcEsp);
      FCTe.infCTeNorm.infDoc.InfNF[i].dEmi := ObterConteudo(infArr[i].Childrens.FindAnyNs('dEmi'), tcDat);
      FCTe.infCTeNorm.infDoc.InfNF[i].vBC := ObterConteudo(infArr[i].Childrens.FindAnyNs('vBC'), tcDe2);
      FCTe.infCTeNorm.infDoc.InfNF[i].vICMS := ObterConteudo(infArr[i].Childrens.FindAnyNs('vICMS'), tcDe2);
      FCTe.infCTeNorm.infDoc.InfNF[i].vBCST := ObterConteudo(infArr[i].Childrens.FindAnyNs('vBCST'), tcDe2);
      FCTe.infCTeNorm.infDoc.InfNF[i].vST := ObterConteudo(infArr[i].Childrens.FindAnyNs('vST'), tcDe2);
      FCTe.infCTeNorm.infDoc.InfNF[i].vProd := ObterConteudo(infArr[i].Childrens.FindAnyNs('vProd'), tcDe2);
      FCTe.infCTeNorm.infDoc.InfNF[i].vNF := ObterConteudo(infArr[i].Childrens.FindAnyNs('vNF'), tcDe2);
      FCTe.infCTeNorm.infDoc.InfNF[i].nCFOP := ObterConteudo(infArr[i].Childrens.FindAnyNs('nCFOP'), tcInt);
      FCTe.infCTeNorm.infDoc.InfNF[i].nPeso := ObterConteudo(infArr[i].Childrens.FindAnyNs('nPeso'), tcDe3);
      FCTe.infCTeNorm.infDoc.InfNF[i].PIN := ObterConteudo(infArr[i].Childrens.FindAnyNs('PIN'), tcStr);
      FCTe.infCTeNorm.infDoc.InfNF[i].dPrev := ObterConteudo(infArr[i].Childrens.FindAnyNs('dPrev'), tcDat);

      infUnidArr := infArr[i].Childrens.FindAllAnyNs('infUnidTransp');
      for j := 0 to Length(infUnidArr)-1 do
      begin
        FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp.New;
        FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].tpUnidTransp := StrToUnidTransp(ok, ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('tpUnidTransp'), tcStr));
        FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].idUnidTransp := ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('idUnidTransp'), tcStr);
        FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].qtdRat := ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('qtdRat'), tcDe2);


        UnidArr := infUnidArr[j].Childrens.FindAllAnyNs('lacUnidTransp');
        for k := 0 to Length(UnidArr)-1 do
        begin
          FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].lacUnidTransp.New;
          FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].lacUnidTransp[k].nLacre := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('nLacre'), tcStr);
        end;

        UnidArr := infUnidArr[j].Childrens.FindAllAnyNs('infUnidCarga');
        for k := 0 to Length(UnidArr)-1 do
        begin
          FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga.New;
          FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga[k].tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(UnidArr[k].Childrens.FindAnyNs('tpUnidCarga'), tcStr));
          FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga[k].idUnidCarga := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('idUnidCarga'), tcStr);
          FCTe.infCTeNorm.infDoc.infNF[i].infUnidTransp[j].infUnidCarga[k].qtdRat := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('qtdRat'), tcDe2);

          ANodeArr := UnidArr[k].Childrens.FindAllAnyNs('lacUnidCarga');
          for l := 0 to Length(ANodeArr)-1 do
          begin
            FCTe.infCTeNorm.infDoc.infNF[l].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.New;
            FCTe.infCTeNorm.infDoc.infNF[l].infUnidTransp[j].infUnidCarga[k].lacUnidCarga[l].nLacre := ObterConteudo(ANodeArr[l].Childrens.FindAnyNs('nLacre'), tcStr);
          end;

        end;
      end;

      UnidArr := infArr[i].Childrens.FindAllAnyNs('infUnidCarga');
      for j := 0 to Length(UnidArr)-1 do
      begin
        FCTe.infCTeNorm.infDoc.infNF[i].infUnidCarga.New;
        FCTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(UnidArr[j].Childrens.FindAnyNs('tpUnidCarga'), tcStr));
        FCTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].idUnidCarga := ObterConteudo(UnidArr[j].Childrens.FindAnyNs('idUnidCarga'), tcStr);
        FCTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].qtdRat := ObterConteudo(UnidArr[j].Childrens.FindAnyNs('qtdRat'), tcDe2);

        ANodeArr := UnidArr[j].Childrens.FindAllAnyNs('lacUnidCarga');
        for k := 0 to Length(ANodeArr)-1 do
        begin
          FCTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].lacUnidCarga.New;
          FCTe.infCTeNorm.infDoc.infNF[i].infUnidCarga[j].lacUnidCarga[k].nLacre := ObterConteudo(ANodeArr[k].Childrens.FindANyNs('nLacre'), tcStr);
        end;
      end;
    end;

    Ler_InfNFe(AuxNode, FCTe.infCTeNorm.infDoc.infNFe);

    FCTe.infCTeNorm.infDoc.InfOutros.Clear;
    infArr := AuxNode.Childrens.FindAllAnyNs('infOutros');
    for i := 0 to Length(infArr)-1 do
    begin
      FCTe.infCTeNorm.infDoc.InfOutros.New;
      FCTe.infCTeNorm.infDoc.InfOutros[i].tpDoc := StrToTpDocumento(ok, ObterConteudo(infArr[i].Childrens.FindAnyNs('tpDoc'), tcStr));
      FCTe.infCTeNorm.infDoc.InfOutros[i].descOutros := ObterConteudo(infArr[i].Childrens.FindAnyNs('descOutros'), tcStr);
      FCTe.infCTeNorm.infDoc.InfOutros[i].nDoc := ObterConteudo(infArr[i].Childrens.FindAnyNs('nDoc'), tcStr);
      FCTe.infCTeNorm.infDoc.InfOutros[i].dEmi := ObterConteudo(infArr[i].Childrens.FindAnyNs('dEmi'), tcDat);
      FCTe.infCTeNorm.infDoc.InfOutros[i].vDocFisc := ObterConteudo(infArr[i].Childrens.FindAnyNs('vDocFisc'), tcDe2);
      FCTe.infCTeNorm.infDoc.InfOutros[i].dPrev := ObterConteudo(infArr[i].Childrens.FindAnyNs('dPrev'), tcDat);

      infUnidArr := infArr[i].Childrens.FindAllAnyNs('infUnidTransp');
      for j := 0 to Length(infUnidArr)-1 do
      begin
        FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp.New;
        FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].tpUnidTransp := StrToUnidTransp(ok, ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('tpUnidTransp'), tcStr));
        FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].idUnidTransp := ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('idUnidTransp'), tcStr);
        FCTe.infCTeNorm.infDoc.infOutros[i].infUnidTransp[j].qtdRat := ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('qtdRat'), tcDe2);

        UnidArr := infUnidArr[j].Childrens.FindAllAnyNs('lacUnidTransp');
        for k := 0 to Length(UnidArr)-1 do
        begin
          FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].lacUnidTransp.New;
          FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].lacUnidTransp[k].nLacre := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('nLacre'), tcStr);
        end;

        UnidArr := infUnidArr[j].Childrens.FindAllAnyNs('infUnidCarga');
        for k := 0 to Length(UnidArr)-1 do
        begin
          FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].infUnidCarga.New;
          FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].infUnidCarga[k].tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(UnidArr[k].Childrens.FindAnyNs('tpUnidCarga'), tcStr));
          FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].infUnidCarga[k].idUnidCarga := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('idUnidCarga'), tcStr);
          FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].infUnidCarga[k].qtdRat := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('qtdRat'), tcDe2);

          ANodeArr := UnidArr[k].Childrens.FindAllAnyNs('lacUnidCarga');
          for l := 0 to Length(ANodeArr)-1 do
          begin
            FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.New;
            FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga[l].nLacre := ObterConteudo(ANodeArr[l].Childrens.FindAnyNs('nLacre'), tcStr);
          end;
        end;
      end;

      infUnidArr := infArr[i].Childrens.FindAllAnyNs('infUnidCarga');
      for j := 0 to Length(infUnidArr)-1 do
      begin
        FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga.New;
        FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('tpUnidCarga'), tcStr));
        FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].idUnidCarga := ObterConteudo(InfUnidArr[j].Childrens.FindAnyNs('idUnidCarga'), tcStr);
        FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].qtdRat := ObterConteudo(InfUnidArr[j].Childrens.FindAnyNs('qtdRat'), tcDe2);

        UnidArr := infUnidArr[j].Childrens.FindAllAnyNs('lacUnidCarga');
        for k := 0 to Length(UnidArr)-1 do
        begin
          FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].lacUnidCarga.New;
          FCTe.infCTeNorm.infDoc.InfOutros[i].infUnidCarga[j].lacUnidCarga[k].nLacre := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('nLacre'), tcStr);
        end;
      end;
    end;

    Ler_InfDCe(AuxNode);
  end;

  Ler_DocAnt(ANode.Childrens.FindAnyNs('docAnt'));
  Ler_Seg(ANode.Childrens.FindAllAnyNs('seg'));
  AuxNode := ANode.Childrens.FindAnyNs('infModal');

  if Assigned(AuxNode) then
  begin
    Ler_Rodo(AuxNode.Childrens.FindAnyNs('rodo'), FCTe.infCTeNorm.rodo);
    Ler_RodoOS(AuxNode.Childrens.FindAnyNs('rodoOS'));
    Ler_Aereo(AuxNode.Childrens.FindAnyNs('aereo'), FCTe.infCTeNorm.aereo);
    Ler_Aquav(AuxNode.Childrens.FindAnyNs('aquav'), FCTe.infCTeNorm.aquav);
    Ler_Ferrov(AuxNode.Childrens.FindAnyNs('ferrov'));
    Ler_Duto(AuxNode.Childrens.FindAnyNs('duto'));
    Ler_Multimodal(AuxNode.Childrens.FindAnyNs('multimodal'));
  end;

  Ler_VeicNovos(ANode.Childrens.FindAllAnyNs('veicNovos'));
  Ler_Cobr(ANode.Childrens.FindAnyNs('cobr'));
  Ler_InfGTVe(ANode.Childrens.FindAllAnyNs('infGTVe'));
  Ler_InfCTeSub(ANode.Childrens.FindAnyNs('infCteSub'), FCTe.infCTeNorm.infCTeSub);

  infGlobalizadoNd := ANode.Childrens.FindAnyNs('infGlobalizado');
  if Assigned(infGlobalizadoNd) then
    FCTe.infCTeNorm.infGlobalizado.xObs := ObterConteudo(infGlobalizadoNd.Childrens.FindAnyNs('xObs'), tcStr);

  AuxNode := ANode.Childrens.FindAnyNs('infServVinc');
  if Assigned(AuxNode) then
  begin
    FCTe.infCTeNorm.infServVinc.infCTeMultimodal.Clear;
    ANodeArr := AuxNode.Childrens.FindAllAnyNs('infCTeMultimodal');
    for i := 0 to Length(ANodeArr)-1 do
    begin
      FCTe.infCTeNorm.infServVinc.infCTeMultimodal.New;
      FCTe.infCTeNorm.infServVinc.infCTeMultimodal[i].chCTeMultimodal := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('chCTeMultimodal'), tcStr);
    end;
  end;
end;

procedure TCTeXmlReader.Ler_InfCTeComp(ANode: TACBrXmlNode);
var
  infCteCompNd: TACBrXmlNode;
  infCTeCompArr: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  if FCTe.infCTe.versao <= 3 then
  begin
    infCTeCompNd := ANode.Childrens.FindAnyNs('infCteComp');

    if Assigned(infCTeCompNd) then
      FCTe.InfCTeComp.Chave := ObterConteudo(infCTeCompNd.Childrens.FindAnyNs('chCTe'), tcStr);
  end
  else
  begin
    infCTeCompArr := ANode.Childrens.FindAllAnyNs('infCteComp');
    FCTe.InfCTeComp10.Clear;

    for i := 0 to Length(infCTeCompArr)-1 do
    begin
      FCTe.InfCTeComp10.New;
      FCTe.InfCTeComp10[i].chCTe := ObterConteudo(infCTeCompArr[i].Childrens.FindAnyNs('chCTe'), tcStr);
    end;
  end;
end;

procedure TCTeXmlReader.Ler_InfCTeAnu(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.InfCTeAnu.chCTe := ObterConteudo(ANode.Childrens.FindAnyNs('chCte'), tcStr);
  FCTe.InfCTeAnu.dEmi := ObterConteudo(ANode.Childrens.FindAnyNs('dEmi'), tcDat);
end;

procedure TCTeXmlReader.Ler_InfCTeSub(infCteSubNd: TACBrXmlNode; infCTeSub: TInfCteSub);
var
  indAlteraToma: string;
  Ok: Boolean;
  tomaICMSNd, refNFNd, tomaNaoICMSNd: TACBrXmlNode;
begin
  if not Assigned(infCTeSubNd) then exit;

  infCTeSub.chCte := ObterConteudo(infCTeSubNd.Childrens.FindAnyNs('chCte'), tcStr);
  infCTeSub.refCteAnu := ObterConteudo(infCTeSubNd.Childrens.FindAnyNs('refCteAnu'), tcStr);

  indAlteratoma := ObterConteudo(infCTeSubNd.Childrens.FindAnyNs('indAlteraToma'), tcStr);

  if indAlteratoma <> '' then
    infCTeSub.indAlteraToma := StrToTIndicador(Ok, indAlteraToma);

  tomaICMSNd := infCTeSubNd.Childrens.FindAnyNs('tomaICMS');

  if Assigned(tomaICMSNd) then
  begin
    infCTeSub.tomaICMS.refNFe := ObterConteudo(tomaICMSNd.Childrens.FindAnyNs('refNFe'), tcStr);
    infCTeSub.tomaICMS.refCte := ObterConteudo(tomaICMSNd.Childrens.FindAnyNs('refCte'), tcStr);

    refNFNd := tomaICMSNd.Childrens.FindAnyNs('refNF');

    if Assigned(refNFNd) then
    begin
      infCTeSub.tomaICMS.refNF.CNPJCPF := ObterCNPJCPF(refNFNd);
      infCTeSub.tomaICMS.refNF.modelo := ObterConteudo(refNFNd.Childrens.FindAnyNs('mod'), tcStr);
      infCTeSub.tomaICMS.refNF.serie := ObterConteudo(refNFNd.Childrens.FindAnyNs('serie'), tcInt);
      infCTeSub.tomaICMS.refNF.subserie := ObterConteudo(refNFNd.Childrens.FindAnyNs('subserie'), tcInt);
      infCTeSub.tomaICMS.refNF.nro := ObterConteudo(refNFNd.Childrens.FindAnyNs('nro'), tcInt);
      infCTeSub.tomaICMS.refNF.valor := ObterConteudo(refNFNd.Childrens.FindAnyNs('valor'), tcDe2);
      infCTeSub.tomaICMS.refNF.dEmi := ObterConteudo(refNFNd.Childrens.FindAnyNs('dEmi'), tcDat);
    end;
  end;

  tomaNaoICMSNd := infCTeSubNd.Childrens.FindAnyNs('tomaNaoICMS');
  if Assigned(tomaNaoICMSNd) then
    infCTeSub.tomaNaoICMS.refCteAnu := ObterConteudo(tomaNaoICMSNd.Childrens.FindAnyNs('refCteAnu'), tcStr);
end;

procedure TCTeXmlReader.Ler_InfCTeSupl(infCTeSuplNd: TACBrXmlNode);
begin
  if not Assigned(InfCTeSuplNd) then exit;

  FCTe.infCTeSupl.qrCodCTe := ObterConteudo(infCTeSuplNd.Childrens.Find('qrCodCTe'), tcStr);
  FCTe.infCTeSupl.qrCodCTe := StringReplace(FCTe.infCTeSupl.qrCodCTe, '<![CDATA[', '', []);
  FCTe.infCTeSupl.qrCodCTe := StringReplace(FCTe.infCTeSupl.qrCodCTe, ']]>', '', []);
end;

procedure TCTeXmlReader.Ler_InfDocAnt(ANode: TACBrXmlNode;
  infDocAnt: TinfDocAntCollection);
var
  i, j: Integer;
  ok: Boolean;
  infArr, infNFeArr: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then exit;

  infdocAnt.Clear;

  infArr := ANode.Childrens.FindAllAnyNs('infDocAnt');

  for i := 0 to Length(infArr)-1 do
  begin
    infdocAnt.New;

    infdocAnt[i].chCTe := ObterConteudo(infArr[i].Childrens.FindAnyNs('chCTe'), tcStr);
    infdocAnt[i].tpPrest := StrTotpPrest(ok, ObterConteudo(infArr[i].Childrens.FindAnyNs('tpPrest'), tcStr));

    infdocAnt[i].infNFeTranspParcial.Clear;

    infNFeArr := infArr[i].Childrens.FindAllAnyNs('infNFeTranspParcial');

    for j := 0 to Length(infNFeArr)-1 do
    begin
      infdocAnt[i].infNFeTranspParcial.New;

      infdocAnt[i].infNFeTranspParcial[j].chNFe := ObterConteudo(infArr[i].Childrens.FindAnyNs('chNFe'), tcStr);
    end;
  end;
end;

procedure TCTeXmlReader.Ler_InfEspecie(ANodeArr: TACBrXmlNodeArray);
var
  i: Integer;
  Ok: Boolean;
begin
  FCTe.detGTV.infEspecie.Clear;
  for i := 0 to Length(ANodeArr)-1 do
  begin
    FCTe.detGTV.infEspecie.New;
    FCTe.detGTV.infEspecie[i].tpEspecie := StrToTEspecie(ok, ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('tpEspecie'), tcStr));
    FCTe.detGTV.infEspecie[i].vEspecie := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('vEspecie'), tcDe2);
    FCTe.detGTV.infEspecie[i].tpNumerario := StrTotpNumerario(ok, ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('tpNumerario'), tcStr));
    FCTe.detGTV.infEspecie[i].xMoedaEstr := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('xMoedaEstr'), tcStr);
  end;
end;

procedure TCTeXmlReader.Ler_InfGTVe(infGTVeArr: TACBrXmlNodeArray);
var
  i, j: Integer;
  compArr: TACBrXmlNodeArray;
  Ok: Boolean;
begin
  for i := 0 to Length(infGTVeArr)-1 do
  begin
    FCTe.infCTeNorm.infGTVe.New;
    FCTe.infCTeNorm.infGTVe[i].chCTe := ObterConteudo(infGTVeArr[i].Childrens.FindAnyNs('chCTe'), tcStr);

    FCTe.infCTeNorm.infGTVe[i].Comp.Clear;
    compArr := infGTVeArr[i].Childrens.FindAllAnyNs('Comp');
    for j := 0 to Length(compArr)-1 do
    begin
      FCTe.infCTeNorm.infGTVe[i].Comp.New;
      FCTe.infCTeNorm.infGTVe[i].Comp[j].tpComp := StrTotpComp(Ok, ObterConteudo(compArr[j].Childrens.FindAnyNs('tpComp'), tcStr));
      FCTe.infCTeNorm.infGTVe[i].Comp[j].vComp := ObterConteudo(compArr[j].Childrens.FindAnyNs('vComp'), tcDe2);
      FCTe.infCTeNorm.infGTVe[i].Comp[j].xComp := ObterConteudo(compArr[j].Childrens.FindAnyNs('xComp'), tcStr);
    end;
  end;
end;

procedure TCTeXmlReader.Ler_InfModal(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  Ler_Rodo(ANode.Childrens.FindAnyNs('rodo'), FCTe.infModal.rodo);
  Ler_Aereo(ANode.Childrens.FindAnyNs('aereo'), FCTe.infModal.aereo);
  Ler_Aquav(ANode.Childrens.FindAnyNs('aquav'), FCTe.infModal.aquav);
end;

procedure TCTeXmlReader.Ler_InfNFe(ANode: TACBrXmlNode; infNFe: TInfNFeCollection);
var
  infArr, infUnidArr, UnidArr, ANodeArr: TACBrXmlNodeArray;
  i, j, k, l: Integer;
  ok: Boolean;
begin
  if not Assigned(ANode) then exit;
  infArr := ANode.Childrens.FindAllAnyNs('infNFe');

  InfNFE.Clear;
  for i := 0 to Length(infArr)-1 do
  begin
    InfNFE.New;
    InfNFE[i].chave := ObterConteudo(infArr[i].Childrens.FindAnyNs('chave'), tcStr);

    if InfNFE[i].chave = '' then
      InfNFE[i].chave := ObterConteudo(infArr[i].Childrens.FindAnyNs('chNFe'), tcStr);

    InfNFE[i].PIN := ObterConteudo(infArr[i].Childrens.FindAnyNs('PIN'), tcStr);
    InfNFE[i].dPrev := ObterConteudo(infArr[i].Childrens.FindAnyNs('dPrev'), tcDat);

    infUnidArr := infArr[i].Childrens.FindAllAnyNs('infUnidTransp');
    for j := 0 to Length(infUnidArr)-1 do
    begin
      InfNFE[i].infUnidTransp.New;
      InfNFE[i].infUnidTransp[j].tpUnidTransp := StrToUnidTransp(ok, ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('tpUnidTransp'), tcStr));
      InfNFE[i].infUnidTransp[j].idUnidTransp := ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('idUnidTransp'), tcStr);
      infNFE[i].infUnidTransp[j].qtdRat := ObterConteudo(infUnidArr[j].Childrens.FindAnyNs('qtdRat'), tcDe2);

      UnidArr := infUnidArr[j].Childrens.FindAllAnyNs('lacUnidTransp');
      for k := 0 to Length(UnidArr)-1 do
      begin
        InfNFE[i].infUnidTransp[j].lacUnidTransp.New;
        InfNFE[i].infUnidTransp[j].lacUnidTransp[k].nLacre := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('nLacre'), tcStr);
      end;

      UnidArr := infUnidArr[j].Childrens.FindAllAnyNs('infUnidCarga');
      for k := 0 to Length(UnidArr)-1 do
      begin
        InfNFE[i].infUnidTransp[j].infUnidCarga.New;
        InfNFE[i].infUnidTransp[j].infUnidCarga[k].tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(UnidArr[k].Childrens.FindAnyNS('tpUnidCarga'), tcStr));
        InfNFE[i].infUnidTransp[j].infUnidCarga[k].idUnidCarga := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('idUnidCarga'), tcStr);
        InfNFE[i].infUnidTransp[j].infUnidCarga[k].qtdRat := ObterConteudo(UnidArr[k].Childrens.FindAnyNs('qtdRat'), tcDe2);

        ANodeArr := UnidArr[k].Childrens.FindAllAnyNs('lacUnidCarga');
        for l := 0 to Length(ANodeArr)-1 do
        begin
          InfNFE[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga.New;
          InfNFE[i].infUnidTransp[j].infUnidCarga[k].lacUnidCarga[l].nLacre := ObterConteudo(ANodeArr[l].Childrens.FindAnyNs('nLacre'), tcStr);
        end;
      end;
    end;

    UnidArr := infArr[i].Childrens.FindAllAnyNs('infUnidCarga');
    for j := 0 to Length(UnidArr)-1 do
    begin
      infNFE[i].infUnidCarga.New;
      infNFE[i].infUnidCarga[j].tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(UnidArr[j].Childrens.FindAnyNs('tpUnidCarga'), tcStr));
      infNFE[i].infUnidCarga[j].idUnidCarga := ObterConteudo(UnidArr[j].Childrens.FindAnyNs('idUnidCarga'), tcStr);
      infNFE[i].infUnidCarga[j].qtdRat := ObterConteudo(UnidArr[j].Childrens.FindAnyNs('qtdRat'), tcDe2);

      ANodeArr := UnidArr[j].Childrens.FindAllAnyNs('lacUnidCarga');
      for k := 0 to Length(ANodeArr)-1 do
      begin
        infNFE[i].infUnidCarga[j].lacUnidCarga.New;
        infNFE[i].infUnidCarga[j].lacUnidCarga[k].nLacre := ObterConteudo(ANodeArr[k].Childrens.FindAnyNs('nLacre'), tcStr);
      end;
    end;
  end;
end;

procedure TCTeXmlReader.Ler_InfDCe(ANode: TACBrXmlNode);
var
  infArr: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then exit;
  infArr := ANode.Childrens.FindAllAnyNs('infDCe');

  FCTe.infCTeNorm.infDoc.InfDCe.Clear;
  for i := 0 to Length(infArr)-1 do
  begin
    FCTe.infCTeNorm.infDoc.InfDCe.New;
    FCTe.infCTeNorm.infDoc.InfDCe[i].chave := ObterConteudo(infArr[i].Childrens.FindAnyNs('chave'), tcStr);
  end;
end;

procedure TCTeXmlReader.Ler_InfPercurso(ANode: TACBrXmlNode);
var
  infPercurso: TACBrXmlNodeArray;
  i: integer;
begin
  if not Assigned(ANode) then exit;

  infPercurso := ANode.Childrens.FindAllAnyNs('infPercurso');

  FCTe.Ide.InfPercurso.Clear;
  for i := 0 to Length(infPercurso)-1 do
  begin
    with FCTe.Ide.InfPercurso.New do
      UFPer := ObterConteudo(infPercurso[i].Childrens.FindAnyNs('UFPer'), tcStr);
  end;
end;

procedure TCTeXmlReader.Ler_InfRespTec(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.infRespTec.CNPJ := ObterConteudo(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  FCTe.infRespTec.xContato := ObterConteudo(ANode.Childrens.FindAnyNs('xContato'), tcStr);
  FCTe.infRespTec.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);
  FCTe.infRespTec.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  FCTe.infRespTec.idCSRT := ObterConteudo(ANode.Childrens.FindAnyNs('idCSRT'), tcInt);
  FCTe.infRespTec.hashCSRT := ObterConteudo(ANode.Childrens.FindAnyNs('hashCSRT'), tcStr);
end;

procedure TCTeXmlReader.Ler_InfVeiculo(ANodeArr: TACBrXmlNodeArray);
var
  i: Integer;
begin
  FCTe.detGTV.infVeiculo.Clear;

  for i := 0 to Length(ANodeArr)-1 do
  begin
    FCTe.detGTV.infVeiculo.New;
    FCTe.detGTV.infVeiculo[i].placa := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('placa'), tcStr);
    FCTe.detGTV.infVeiculo[i].UF := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('UF'), tcStr);
    FCTe.detGTV.infVeiculo[i].RNTRC := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('RNTRC'), tcStr);
  end;
end;

procedure TCTeXmlReader.Ler_MultiModal(ANode: TACBrXmlNode);
var
  OK: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then exit;

  FCTe.infCTeNorm.multimodal.COTM := ObterConteudo(ANode.Childrens.FindAnyNs('COTM'), tcStr);
  FCTe.infCTeNorm.multimodal.indNegociavel := StrToindNegociavel(ok, ObterConteudo(ANode.Childrens.FindAnyNs('indNegociavel'), tcStr));
  // dados sobre o seguro informados somente na versão 3.00
  AuxNode := ANode.Childrens.FindAnyNs('seg');

  if Assigned(AuxNode) then
  begin
    FCTe.infCTeNorm.multimodal.nApol := ObterConteudo(AuxNode.Childrens.FindAnyNs('nApol'), tcStr);
    FCTe.infCTeNorm.multimodal.nAver := ObterConteudo(AuxNode.Childrens.FindAnyNs('nAver'), tcStr);

    AuxNode := AuxNode.Childrens.FindAnyNs('infSeg');

    if Assigned(AuxNode) then
    begin
      FCTe.infCTeNorm.multimodal.xSeg := ObterConteudo(AuxNode.Childrens.FindAnyNs('xSeg'), tcStr);
      FCTe.infCTeNorm.multimodal.CNPJ := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);
    end;
  end;
end;

procedure TCTeXmlReader.Ler_ObsCont(ANodeArr: TACBrXmlNodeArray);
var
  i: Integer;
begin
  for i := 0 to Length(ANodeArr)-1 do
  begin
    FCTe.compl.ObsCont.New;
    FCTe.compl.ObsCont[i].xCampo := ANodeArr[i].Attributes.Items['xCampo'].Content;
    FCTe.compl.ObsCont[i].xTexto := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('xTexto'), tcStr);
  end;
end;

procedure TCTeXmlReader.Ler_ObsFisco(ANodeArr: TACBrXmlNodeArray);
var
  i: Integer;
begin
  for i := 0 to Length(ANodeArr)-1 do
  begin
    FCTe.compl.ObsFisco.New;
    FCTe.compl.ObsFisco[i].xCampo := ANodeArr[i].Attributes.Items['xCampo'].Content;
    FCTe.compl.ObsFisco[i].xTexto := ObterConteudo(ANodeArr[i].Childrens.FindAnyNs('xTexto'), tcStr);
  end;
end;

procedure TCTeXmlReader.Ler_Origem(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.origem.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  FCTe.origem.Nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  FCTe.origem.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  FCTe.origem.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  FCTe.origem.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  FCTe.origem.xMun := ObterConteudo(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  FCTe.origem.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  FCTe.origem.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  FCTe.origem.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
end;

procedure TCTeXmlReader.Ler_ProtCTe(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ANodeAux: TACBrXmlNode;
begin
  if not Assigned(ANode) then exit;

  ANodeAux := ANode.Childrens.FindAnyNs('infProt');

  if not Assigned(ANodeAux) then Exit;

  FCTe.procCTe.tpAmb := StrToTpAmb(Ok, ObterConteudo(ANodeAux.Childrens.FindAnyNs('tpAmb'), tcStr));
  FCTe.procCTe.verAplic := ObterConteudo(ANodeAux.Childrens.FindAnyNs('verAplic'), tcStr);
  FCTe.procCTe.chCTe := ObterConteudo(ANodeAux.Childrens.FindAnyNs('chCTe'), tcStr);
  FCTe.procCTe.dhRecbto := ObterConteudo(ANodeAux.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
  FCTe.procCTe.nProt := ObterConteudo(ANodeAux.Childrens.FindAnyNs('nProt'), tcStr);
  FCTe.procCTe.digVal := ObterConteudo(ANodeAux.Childrens.FindAnyNS('digVal'), tcStr);
  FCTe.procCTe.cStat := ObterConteudo(ANodeAux.Childrens.FindAnyNs('cStat'), tcInt);
  FCTe.procCTe.xMotivo := ObterConteudo(ANodeAux.Childrens.FindAnyNS('xMotivo'), tcStr);
  FCTe.procCTe.cMSg := ObterConteudo(ANodeAux.Childrens.FindAnyNs('cMsg'), tcInt);
  FCTe.procCTe.xMsg := ObterConteudo(ANodeAux.Childrens.FindAnyNs('xMsg'), tcStr);
end;

procedure TCTeXmlReader.Ler_Receb(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.receb.CNPJCPF := ObterCNPJCPF(ANode);
  FCTe.receb.IE := ObterConteudo(ANode.Childrens.FindAnyNs('IE'), tcStr);
  FCTe.receb.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  FCTe.receb.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  FCTe.receb.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);

  Ler_EnderReceb(ANode.Childrens.FindAnyNs('enderReceb'));
end;

procedure TCTeXmlReader.Ler_Rem(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.Rem.CNPJCPF := ObterCNPJCPF(ANode);
  FCTe.Rem.IE := ObterConteudo(ANode.Childrens.FindAnyNs('IE'), tcStr);
  FCTe.Rem.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  FCTe.Rem.xFant := ObterConteudo(ANode.Childrens.FindAnyNs('xFant'), tcStr);
  FCTe.Rem.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  FCTe.Rem.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);

  Ler_EnderReme(ANode.Childrens.FindAnyNs('enderReme'));
end;

procedure TCTeXmlReader.Ler_Rodo(ANode: TACBrXmlNode; rodo: TRodo);
var
  OK: Boolean;
  AuxNodeArr: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
  i: Integer;
begin
  if not Assigned(ANode) then exit;

  rodo.RNTRC := ObterConteudo(ANode.Childrens.FindAnyNs('RNTRC'), tcStr);
  rodo.dPrev := ObterConteudo(ANode.Childrens.FindAnyNs('dPrev'), tcDat);
  rodo.lota := StrToTpLotacao(ok, ObterConteudo(ANode.Childrens.FindAnyNs('lota'), tcDat));
  rodo.CIOT := ObterConteudo(ANode.Childrens.FindAnyNs('CIOT'), tcStr);

  AuxNodeArr := ANode.Childrens.FindAllAnyNs('occ');
  rodo.occ.Clear;
  for i := 0 to Length(AuxNodeArr)-1 do
  begin
    rodo.occ.New;
    rodo.occ[i].serie := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('serie'), tcStr);
    rodo.occ[i].nOcc := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('nOcc'), tcInt);
    rodo.occ[i].dEmi := ObterConteudo(AuxNodeArr[i].Childrens.FindaNyNs('dEmi'), tcDat);

    AuxNode := AuxNodeArr[i].Childrens.FindAnyNs('emiOcc');
    if Assigned(AuxNode) then
    begin
      rodo.occ[i].emiOcc.CNPJ := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);
      rodo.occ[i].emiOcc.cInt := ObterConteudo(AuxNode.Childrens.FindAnyNs('cInt'), tcStr);
      rodo.occ[i].emiOcc.IE := ObterConteudo(AuxNode.Childrens.FindAnyNs('IE'), tcStr);
      rodo.occ[i].emiOcc.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      rodo.occ[i].emiOcc.fone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
    end;
  end;

  AuxNodeArr := ANode.Childrens.FindAllAnyNs('valePed');
  rodo.valePed.Clear;
  for i := 0 to Length(AuxNodeArr)-1 do
  begin
    rodo.valePed.New;
    rodo.valePed[i].CNPJForn := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('CNPJForn'), tcStr);
    rodo.valePed[i].nCompra := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('nCompra'), tcStr);
    rodo.valePed[i].CNPJPg := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('CNPJPg'), tcStr);
    rodo.valePed[i].vValePed := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('vValePed'), tcDe2);
  end;

  AuxNodeArr := ANode.Childrens.FindAllAnyNs('veic');
  rodo.veic.Clear;
  for i := 0 to Length(AuxNodeArr)-1 do
  begin
    rodo.veic.New;
    rodo.veic[i].cInt := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('cInt'), tcInt);
    rodo.veic[i].RENAVAM := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('RENAVAM'), tcStr);
    rodo.veic[i].placa := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('placa'), tcStr);
    rodo.veic[i].tara := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('tara'), tcInt);
    rodo.veic[i].capKG := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('capKG'), tcInt);
    rodo.veic[i].capM3 := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('capM3'), tcInt);
    rodo.veic[i].tpProp := StrToTpPropriedade(ok, ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('tpProp'), tcStr));
    rodo.veic[i].tpVeic := StrToTpVeiculo(ok, ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('tpVeic'), tcStr));
    rodo.veic[i].tpRod := StrToTpRodado(ok, ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('tpRod'), tcStr));
    rodo.veic[i].tpCar := StrToTpCarroceria(ok, ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('tpCar'), tcStr));
    rodo.veic[i].UF := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('UF'), tcStr);

    AuxNode := AuxNodeArr[i].Childrens.FindAnyNs('prop');

    if Assigned(AuxNode) then
    begin
      rodo.veic[i].prop.CNPJCPF := ObterCNPJCPF(AuxNode);
      rodo.veic[i].prop.RNTRC := ObterConteudo(AuxNode.Childrens.FindAnyNs('RNTRC'), tcStr);
      rodo.veic[i].prop.xNome := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);
      rodo.veic[i].prop.IE := ObterConteudo(AuxNode.Childrens.FindAnyNs('IE'), tcStr);
      rodo.veic[i].prop.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
      rodo.veic[i].prop.tpProp := StrToTpProp(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpProp'), tcStr));
    end;
  end;

  AuxNodeArr := ANode.Childrens.FindAllAnyNs('lacRodo');
  rodo.lacRodo.Clear;
  for i := 0 to Length(AuxNodeArr)-1 do
  begin
    rodo.lacRodo.New;
    rodo.lacRodo[i].nLacre := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('nLacre'), tcStr);
  end;

  AuxNodeArr := ANode.Childrens.FindAllAnyNs('moto');
  Rodo.moto.Clear;
  for i := 0 to Length(AuxNodeArr)-1 do
  begin
    Rodo.moto.New;
    Rodo.moto[i].xNome := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('xNome'), tcStr);
    Rodo.moto[i].CPF := ObterConteudo(AuxNodeArr[i].Childrens.FindAnyNs('CPF'), tcStr);
  end;
end;

procedure TCTeXmlReader.Ler_RodoOS(ANode: TACBrXmlNode);
var
  veicNd, propNd, infFretamentoNd: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  FCTe.infCTeNorm.rodoOS.TAF := ObterConteudo(ANode.Childrens.FindAnyNs('TAF'), tcStr);
  FCTe.infCTeNorm.rodoOS.NroRegEstadual := ObterConteudo(ANode.Childrens.FindAnyNs('NroRegEstadual'), tcStr);

  veicNd := ANode.Childrens.FindAnyNs('veic');

  if Assigned(veicNd) then
  begin
    FCTe.infCTeNorm.rodoOS.veic.placa := ObterConteudo(veicNd.Childrens.FindAnyNs('placa'), tcStr);
    FCTe.infCTeNorm.rodoOS.veic.RENAVAM := ObterConteudo(veicNd.Childrens.FindAnyNs('RENAVAM'), tcStr);
    FCTe.infCTeNorm.rodoOS.veic.UF := ObterConteudo(veicNd.Childrens.FindAnyNs('UF'), tcStr);

    propNd := veicNd.Childrens.FindAnyNs('prop');

    if Assigned(propNd) then
    begin
      FCTe.infCTeNorm.rodoOS.veic.prop.CNPJCPF := ObterCNPJCPF(propNd);
      FCTe.infCTeNorm.rodoOS.veic.prop.TAF := ObterConteudo(propNd.Childrens.FindAnyNs('TAF'), tcStr);
      FCTe.infCTeNorm.rodoOS.veic.prop.NroRegEstadual := ObterConteudo(propNd.Childrens.FindAnyNs('NroRegEstadual'), tcStr);
      FCTe.infCTeNorm.rodoOS.veic.prop.xNome := ObterConteudo(propNd.Childrens.FindAnyNs('xNome'), tcStr);
      FCTe.infCTeNorm.rodoOS.veic.prop.IE := ObterConteudo(propNd.Childrens.FindAnyNs('IE'), tcStr);
      FCTe.infCTeNorm.rodoOS.veic.prop.UF := ObterConteudo(propNd.Childrens.FindAnyNs('UF'), tcStr);
      FCTe.infCTeNorm.rodoOS.veic.prop.tpProp := StrToTpProp(ok, ObterConteudo(propNd.Childrens.FindAnyNs('tpProp'), tcStr));
    end;
  end;

  infFretamentoNd := ANode.Childrens.FindAnyNs('infFretamento');

  if Assigned(infFretamentoNd) then
  begin
    FCTe.infCTeNorm.rodoOS.infFretamento.tpFretamento := StrToTpFretamento(ok, ObterConteudo(infFretamentoNd.Childrens.FindAnyNs('tpFretamento'), tcStr));
    FCTe.infCTeNorm.rodoOS.infFretamento.dhViagem := ObterConteudo(infFretamentoNd.Childrens.FindAnyNs('dhViagem'), tcDatHor);
  end;
end;

procedure TCTeXmlReader.Ler_Seg(segArr: TACBrXmlNodeArray);
var
  i: Integer;
  Ok: Boolean;
begin
  for i := 0 to Length(segArr) -1 do
  begin
    FCTe.infCTeNorm.seg.New;
    FCTe.infCTeNorm.seg[i].respSeg := StrToTpRspSeguro(ok, ObterConteudo(segArr[i].Childrens.FindAnyNs('respSeg'), tcStr));
    FCTe.infCTeNorm.seg[i].xSeg := ObterConteudo(segArr[i].Childrens.FindAnyNs('xSeg'), tcStr);
    FCTe.infCTeNorm.seg[i].nApol := ObterConteudo(segArr[i].Childrens.FindAnyNs('nApol'), tcStr);
    FCTe.infCTeNorm.seg[i].nAver := ObterConteudo(segArr[i].Childrens.FindAnyNs('nAver'), tcStr);
    FCTe.infCTeNorm.seg[i].vCarga := ObterConteudo(segArr[i].Childrens.FindAnyNs('vCarga'), tcDe2);
  end;
end;

procedure TCTeXmlReader.Ler_Signature(SignatureNd: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(SignatureNd) then exit;

  AuxNode := SignatureNd.Childrens.FindAnyNs('SignedInfo');
  AuxNode := AuxNode.Childrens.FindAnyNs('Reference');

  if Assigned(AuxNode) then
  begin
    FCTe.signature.URI := AuxNode.Attributes.Items['URI'].Content;
    FCTe.signature.DigestValue := ObterConteudo(AuxNode.Childrens.Find('DigestValue'), tcStr);
  end;

  FCTe.signature.SignatureValue := ObterConteudo(SignatureNd.Childrens.Find('SignatureValue'), tcStr);

  AuxNode := SignatureNd.Childrens.FindAnyNs('KeyInfo');
  AuxNode := AuxNode.Childrens.FindAnyNs('X509Data');

  if Assigned(AuxNode) then
    FCTe.signature.X509Certificate := ObterConteudo(AuxNode.Childrens.Find('X509Certificate'), tcStr);
end;

procedure TCTeXmlReader.Ler_Toma(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.toma.CNPJCPF := ObterCNPJCPF(ANode);
  FCTe.toma.IE := ObterConteudo(ANode.Childrens.FindAnyNs('IE'), tcStr);
  FCTe.toma.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  FCTe.toma.xFant := ObterConteudo(ANode.Childrens.FindAnyNs('xFant'), tcStr);
  FCTe.toma.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  FCTe.toma.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);

  Ler_TomaEnderToma(ANode.Childrens.FindAnyNs('enderToma'));
end;

procedure TCTeXmlReader.Ler_Toma03(ANode: TACBrXmlNode);
var
  tomaNode: TACBrXmlNode;
  Ok: Boolean;
begin
  if not Assigned(ANode) then exit;

  tomaNode := ANode.Childrens.FindAnyNs('toma03');

  if not Assigned(tomaNode) then
    tomaNode := ANode.Childrens.FindAnyNs('toma3');

  if not Assigned(tomaNode) then
    tomaNode := ANode.Childrens.FindAnyNs('toma');

  if not Assigned(tomaNode) then exit;

  FCTe.Ide.toma03.Toma := StrToTpTomador(Ok, ObterConteudo(tomaNode.Childrens.FindAnyNs('toma'), tcStr));
end;

procedure TCTeXmlReader.Ler_Toma04(ANode: TACBrXmlNode);
var
  tomaNode: TACBrXmlNode;
  OK: Boolean;
begin
  if not Assigned(ANode) then exit;

  tomaNode := ANode.Childrens.FindAnyNs('toma4');

  if not Assigned(tomaNode) then
    tomaNode := ANode.Childrens.FindAnyNs('tomaTerceiro');

  if not Assigned(tomaNode) then exit;

  FCTe.Ide.toma4.toma := StrToTpTomador(OK, ObterConteudo(tomaNode.Childrens.FindAnyNs('toma'), tcStr));
  FCTe.Ide.toma03.Toma := FCTe.Ide.toma4.Toma;

  FCTe.Ide.toma4.CNPJCPF := ObterCNPJCPF(tomaNode);
  FCTe.Ide.toma4.IE := ObterConteudo(tomaNode.Childrens.FindAnyNs('IE'), tcStr);
  FCTe.Ide.toma4.xNome := ObterConteudo(tomaNode.Childrens.FindAnyNs('xNome'), tcStr);
  FCTe.Ide.toma4.xFant := ObterConteudo(tomaNode.Childrens.FindAnyNs('xFant'), tcStr);
  FCTe.Ide.toma4.fone := ObterConteudo(tomaNode.Childrens.FindAnyNs('fone'), tcStr);
  FCTe.Ide.toma4.email := ObterConteudo(tomaNode.Childrens.FindAnyNs('email'), tcStr);

  Ler_Toma04EnderToma(tomaNode.Childrens.FindAnyNs('enderToma'));
end;

procedure TCTeXmlReader.Ler_Toma04EnderToma(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.Ide.Toma4.EnderToma.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  FCTe.Ide.Toma4.EnderToma.nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  FCTe.Ide.Toma4.EnderToma.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  FCTe.Ide.Toma4.EnderToma.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  FCTe.Ide.Toma4.EnderToma.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  FCTe.Ide.Toma4.EnderToma.xMun := ObterConteudo(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  FCTe.Ide.Toma4.EnderToma.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  FCTe.Ide.Toma4.EnderToma.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  FCTe.Ide.Toma4.EnderToma.cPais := ObterConteudo(ANode.Childrens.FindAnyNs('cPais'), tcInt);
  FCTe.Ide.Toma4.EnderToma.xPais := ObterConteudo(ANode.Childrens.FindAnyNs('xPais'), tcStr);
end;

procedure TCTeXmlReader.Ler_TomaEnderToma(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.toma.EnderToma.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  FCTe.toma.EnderToma.nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  FCTe.toma.EnderToma.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  FCTe.toma.EnderToma.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  FCTe.toma.EnderToma.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  FCTe.toma.EnderToma.xMun := ObterConteudo(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  FCTe.toma.EnderToma.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  FCTe.toma.EnderToma.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  FCTe.toma.EnderToma.cPais := ObterConteudo(ANode.Childrens.FindAnyNs('cPais'), tcInt);
  FCTe.toma.EnderToma.xPais := ObterConteudo(ANode.Childrens.FindANyNs('xPais'), tcStr);
end;

procedure TCTeXmlReader.Ler_Total(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.total.vTPrest := ObterConteudo(ANode.Childrens.FindAnyNs('vTPrest'), tcDe2);
  FCTe.total.vTRec := ObterConteudo(ANode.Childrens.FindAnyNs('vTRec'), tcDe2);
end;

procedure TCTeXmlReader.Ler_VeicNovos(veicArr: TACBrXmlNodeArray);
var
  i: Integer;
begin
  FCTe.infCTeNorm.veicNovos.Clear;
  for i := 0 to Length(veicArr)-1 do
  begin
    FCTe.infCTeNorm.veicNovos.New;
    FCTe.infCTeNorm.veicNovos[i].chassi := ObterConteudo(veicArr[i].Childrens.FindAnyNs('chassi'), tcStr);
    FCTe.infCTeNorm.veicNovos[i].cCor := ObterConteudo(veicArr[i].Childrens.FindAnyNs('cCor'), tcStr);
    FCTe.infCTeNorm.veicNovos[i].xCor := ObterConteudo(veicArr[i].Childrens.FindAnyNs('xCor'), tcStr);
    FCTe.infCTeNorm.veicNovos[i].cMod := ObterConteudo(veicArr[i].Childrens.FindAnyNs('cMod'), tcStr);
    FCTe.infCTeNorm.veicNovos[i].vUnit := ObterConteudo(veicArr[i].Childrens.FindAnyNs('vUnit'), tcDe2);
    FCTe.infCTeNorm.veicNovos[i].vFrete := ObterConteudo(veicArr[i].Childrens.FindAnyNs('vFrete'), tcDe2);
  end;
end;

procedure TCTeXmlReader.Ler_VPrest(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  FCTe.vPrest.vTPrest := ObterConteudo(ANode.Childrens.FindAnyNs('vTPrest'), tcDe2);
  FCTe.vPrest.vRec := ObterConteudo(ANode.Childrens.FindAnyNs('vRec'), tcDe2);

  Ler_Comp(ANode.Childrens.FindAllAnyNs('Comp'), FCTe.vPrest.Comp);
end;

function TCTeXmlReader.LerXml: Boolean;
var
  infCTeNode, CTeNode: TACBrXmlNode;
  att: TACBrXmlAttribute;
begin
  if not Assigned(FCTe) or (FCTe = nil) then
    raise Exception.Create('Destino não informado, informe a classe [TCTe] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo Xml do CTe não carregado.');

  infCTeNode := nil;
  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'cteProc' then
  begin
    Ler_ProtCTe(Document.Root.Childrens.FindAnyNs('protCTe'));
    CTeNode := Document.Root.Childrens.FindAnyNs('CTe');
  end
  else
  begin
    if Document.Root.Name = 'cteOSProc' then
    begin
      Ler_ProtCTe(Document.Root.Childrens.FindAnyNs('protCTe'));
      CTeNode := Document.Root.Childrens.FindAnyNs('CTeOS');
    end
    else
    begin
      if Document.Root.Name = 'cteSimpProc' then
      begin
        Ler_ProtCTe(Document.Root.Childrens.FindAnyNs('protCTe'));
        CTeNode := Document.Root.Childrens.FindAnyNs('CTeSimp');
      end
      else
      begin
        if Document.Root.Name = 'GTVeProc' then
        begin
          Ler_ProtCTe(Document.Root.Childrens.FindAnyNs('protCTe'));
          CTeNode := Document.Root.Childrens.FindAnyNs('GTVe');
        end
        else
        begin
          CTeNode := Document.Root;
        end;
      end;
    end;
  end;

  if Assigned(CTeNode) then
    infCTeNode := CTeNode.Childrens.FindAnyNs('infCte');

  if not Assigned(infCTeNode) then
    raise Exception.Create('Arquivo Xml incorreto.');

  att := infCTeNode.Attributes.Items['Id'];
  if not Assigned(att) then
    raise Exception.Create('Não encontrei o atributo: Id.');

  FCTe.infCTe.Id := att.Content;

  att := infCTeNode.Attributes.Items['versao'];
  if not Assigned(att) then
    raise Exception.Create('Não encontrei o atributo: versão.');

  FCTe.infCTe.versao := StringToFloat(att.Content);

  Ler_InfCTe(infCTeNode);
  Ler_InfCTeSupl(CTeNode.Childrens.FindAnyNs('infCTeSupl'));
  Ler_Signature(CTeNode.Childrens.FindAnyNs('Signature'));

  Result := True;
end;

procedure TCTeXmlReader.Ler_CTe(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  Ler_Compl(ANode.Childrens.FindAnyNs('compl'));
  Ler_Emit(ANode.Childrens.FindAnyNs('emit'));
  Ler_Rem(ANode.Childrens.FindAnyNs('rem'));
  Ler_Exped(ANode.Childrens.FindAnyNs('exped'));
  Ler_Receb(ANode.Childrens.FindANyNs('receb'));
  Ler_Dest(ANode.Childrens.FindAnyNs('dest'));
  Ler_VPrest(ANode.Childrens.FindAnyNs('vPrest'));
  Ler_Imp(ANode.Childrens.FindAnyNs('imp'));
  Ler_InfCTeNorm(ANode.Childrens.FindAnyNs('infCTeNorm'));
  Ler_InfCTeComp(ANode);
  Ler_InfCTeAnu(ANode.Childrens.FindAnyNs('infCteAnu'));
  Ler_AutXML(ANode);
  Ler_InfRespTec(ANode.Childrens.FindAnyNs('infRespTec'));
end;

procedure TCTeXmlReader.Ler_CTeOS(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  Ler_Compl(ANode.Childrens.FindAnyNs('compl'));
  Ler_Emit(ANode.Childrens.FindAnyNs('emit'));
  Ler_Toma(ANode.Childrens.FindAnyNs('toma'));
  Ler_VPrest(ANode.Childrens.FindAnyNs('vPrest'));
  Ler_Imp(ANode.Childrens.FindAnyNs('imp'));
  Ler_InfCTeNorm(ANode.Childrens.FindAnyNs('infCTeNorm'));
  Ler_InfCTeComp(ANode);
  Ler_AutXML(ANode);
  Ler_InfRespTec(ANode.Childrens.FindAnyNs('infRespTec'));
end;

procedure TCTeXmlReader.Ler_CTeSimp(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  Ler_Compl(ANode.Childrens.FindAnyNs('compl'));
  Ler_Emit(ANode.Childrens.FindAnyNs('emit'));
  Ler_Toma(ANode.Childrens.FindAnyNs('toma'));
  Ler_InfCarga(ANode.Childrens.FindAnyNs('infCarga'), FCTe.infCarga);
  Ler_Det(ANode);
  Ler_InfModal(ANode.Childrens.FindAnyNs('infModal'));
  Ler_Cobr(ANode.Childrens.FindAnyNs('cobr'));
  Ler_InfCTeSub(ANode.Childrens.FindAnyNs('infCteSub'), FCTe.infCTeSub);
  Ler_Imp(ANode.Childrens.FindAnyNs('imp'));
  Ler_Total(ANode.Childrens.FindAnyNs('total'));
  Ler_AutXML(ANode);
  Ler_InfRespTec(ANode.Childrens.FindAnyNs('infRespTec'));
end;

procedure TCTeXmlReader.Ler_GTVe(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then exit;

  Ler_Compl(ANode.Childrens.FindAnyNs('compl'));
  Ler_Emit(ANode.Childrens.FindAnyNs('emit'));
  Ler_Rem(ANode.Childrens.FindAnyNs('rem'));
  Ler_Dest(ANode.Childrens.FindAnyNs('dest'));
  Ler_Origem(ANode.Childrens.FindAnyNs('origem'));
  Ler_Destino(ANode.Childrens.FindAnyNs('destino'));
  Ler_DetGTVe(ANode.Childrens.FindAnyNs('detGTV'));
  Ler_AutXML(ANode);
  Ler_InfRespTec(ANode.Childrens.FindAnyNs('infRespTec'));
end;

procedure TCTeXmlReader.ValidarXML(out infCTeNode, CTeNode: TACBrXmlNode);
var
  att: TACBrXmlAttribute;
begin
  if not Assigned(FCTe) or (FCTe = nil) then
    raise Exception.Create('Destino não informado, informe a classe [TCTe] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo Xml do CTe não carregado.');

  infCTeNode := nil;
  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'cteProc' then
  begin
    Ler_ProtCTe(Document.Root.Childrens.FindAnyNs('protCTe'));
    CTeNode := Document.Root.Childrens.FindAnyNs('CTe');
  end
  else
  begin
    if Document.Root.Name = 'cteOSProc' then
    begin
      Ler_ProtCTe(Document.Root.Childrens.FindAnyNs('protCTe'));
      CTeNode := Document.Root.Childrens.FindAnyNs('CTeOS');
    end
    else
    begin
      if Document.Root.Name = 'cteSimpProc' then
      begin
        Ler_ProtCTe(Document.Root.Childrens.FindAnyNs('protCTe'));
        CTeNode := Document.Root.Childrens.FindAnyNs('CTeSimp');
      end
      else
      begin
        if Document.Root.Name = 'GTVeProc' then
        begin
          Ler_ProtCTe(Document.Root.Childrens.FindAnyNs('protCTe'));
          CTeNode := Document.Root.Childrens.FindAnyNs('GTVe');
        end
        else
        begin
          CTeNode := Document.Root;
        end;
      end;
    end;
  end;

  if Assigned(CTeNode) then
    infCTeNode := CTeNode.Childrens.FindAnyNs('infCte');

  if not Assigned(infCTeNode) then
    raise Exception.Create('Arquivo Xml incorreto.');

  att := infCTeNode.Attributes.Items['Id'];
  if not Assigned(att) then
    raise Exception.Create('Não encontrei o atributo: Id.');

  FCTe.infCTe.Id := att.Content;

  att := infCTeNode.Attributes.Items['versao'];
  if not Assigned(att) then
    raise Exception.Create('Não encontrei o atributo: versão.');

  FCTe.infCTe.versao := StringToFloat(att.Content);
end;

// Reforma Tributária
procedure TCTeXmlReader.Ler_gCompraGov(gCompraGov: TgCompraGovReduzido;
  const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  gCompraGov.tpEnteGov := StrTotpEnteGov(ObterConteudo(ANode.Childrens.Find('tpEnteGov'), tcStr));
  gCompraGov.pRedutor := ObterConteudo(ANode.Childrens.Find('pRedutor'), tcDe4);
end;

procedure TCTeXmlReader.Ler_IBSCBS(const ANode: TACBrXmlNode; IBSCBS: TIBSCBS);
begin
  if not Assigned(ANode) then Exit;

  IBSCBS.CST := StrToCSTIBSCBS(ObterConteudo(ANode.Childrens.Find('CST'), tcStr));
  IBSCBS.cClassTrib := StrTocClassTrib(ObterConteudo(ANode.Childrens.Find('cClassTrib'), tcStr));

  Ler_IBSCBS_gIBSCBS(ANode.Childrens.Find('gIBSCBS'), IBSCBS.gIBSCBS);
end;

procedure TCTeXmlReader.Ler_IBSCBS_gIBSCBS(const ANode: TACBrXmlNode; gIBSCBS: TgIBSCBS);
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

procedure TCTeXmlReader.Ler_gIBSUF(const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSUF.pIBS := ObterConteudo(ANode.Childrens.Find('pIBSUF'), tcDe4);

  Ler_gIBSUF_gDif(ANode.Childrens.Find('gDif'), gIBSUF.gDif);
  Ler_gIBSUF_gDevTrib(ANode.Childrens.Find('gDevTrib'), gIBSUF.gDevTrib);
  Ler_gIBSUF_gRed(ANode.Childrens.Find('gRed'), gIBSUF.gRed);

  gIBSUF.vIBS := ObterConteudo(ANode.Childrens.Find('vIBSUF'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gIBSUF_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
begin
  if not Assigned(ANode) then Exit;

  gDif.pDif := ObterConteudo(ANode.Childrens.Find('pDif'), tcDe4);
  gDif.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gIBSUF_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
begin
  if not Assigned(ANode) then Exit;

  gDevTrib.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gIBSUF_gRed(const ANode: TACBrXmlNode; gRed: TgRed);
begin
  if not Assigned(ANode) then Exit;

  gRed.pRedAliq := ObterConteudo(ANode.Childrens.Find('pRedAliq'), tcDe4);
  gRed.pAliqEfet := ObterConteudo(ANode.Childrens.Find('pAliqEfet'), tcDe4);
end;

procedure TCTeXmlReader.Ler_gIBSMun(const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSMun.pIBS := ObterConteudo(ANode.Childrens.Find('pIBSMun'), tcDe4);

  Ler_gIBSMun_gDif(ANode.Childrens.Find('gDif'), gIBSMun.gDif);
  Ler_gIBSMun_gDevTrib(ANode.Childrens.Find('gDevTrib'), gIBSMun.gDevTrib);
  Ler_gIBSMun_gRed(ANode.Childrens.Find('gRed'), gIBSMun.gRed);

  gIBSMun.vIBS := ObterConteudo(ANode.Childrens.Find('vIBSMun'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gIBSMun_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
begin
  if not Assigned(ANode) then Exit;

  gDif.pDif := ObterConteudo(ANode.Childrens.Find('pDif'), tcDe4);
  gDif.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gIBSMun_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
begin
  if not Assigned(ANode) then Exit;

  gDevTrib.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gIBSMun_gRed(const ANode: TACBrXmlNode; gRed: TgRed);
begin
  if not Assigned(ANode) then Exit;

  gRed.pRedAliq := ObterConteudo(ANode.Childrens.Find('pRedAliq'), tcDe4);
  gRed.pAliqEfet := ObterConteudo(ANode.Childrens.Find('pAliqEfet'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gCBS(const ANode: TACBrXmlNode; gCBS: TgCBSValores);
begin
  if not Assigned(ANode) then Exit;

  gCBS.pCBS := ObterConteudo(ANode.Childrens.Find('pCBS'), tcDe4);

  Ler_gCBS_gDif(ANode.Childrens.Find('gDif'), gCBS.gDif);
  Ler_gCBS_gDevTrib(ANode.Childrens.Find('gDevTrib'), gCBS.gDevTrib);
  Ler_gCBS_gRed(ANode.Childrens.Find('gRed'), gCBS.gRed);

  gCBS.vCBS := ObterConteudo(ANode.Childrens.Find('vCBS'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gCBS_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
begin
  if not Assigned(ANode) then Exit;

  gDif.pDif := ObterConteudo(ANode.Childrens.Find('pDif'), tcDe4);
  gDif.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gCBS_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
begin
  if not Assigned(ANode) then Exit;

  gDevTrib.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gCBS_gRed(const ANode: TACBrXmlNode; gRed: TgRed);
begin
  if not Assigned(ANode) then Exit;

  gRed.pRedAliq := ObterConteudo(ANode.Childrens.Find('pRedAliq'), tcDe4);
  gRed.pAliqEfet := ObterConteudo(ANode.Childrens.Find('pAliqEfet'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gIBSCBS_gTribRegular(const ANode: TACBrXmlNode;
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

procedure TCTeXmlReader.Ler_gIBSCredPres(const ANode: TACBrXmlNode; gIBSCredPres: TgIBSCBSCredPres);
begin
  if not Assigned(ANode) then Exit;

  gIBSCredPres.cCredPres := StrTocCredPres(ObterConteudo(ANode.Childrens.Find('cCredPres'), tcStr));
  gIBSCredPres.pCredPres := ObterConteudo(ANode.Childrens.Find('pCredPres'), tcDe4);
  gIBSCredPres.vCredPres := ObterConteudo(ANode.Childrens.Find('vCredPres'), tcDe2);
  gIBSCredPres.vCredPresCondSus := ObterConteudo(ANode.Childrens.Find('vCredPresCondSus'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gCBSCredPres(const ANode: TACBrXmlNode; gCBSCredPres: TgIBSCBSCredPres);
begin
  if not Assigned(ANode) then Exit;

  gCBSCredPres.cCredPres := StrTocCredPres(ObterConteudo(ANode.Childrens.Find('cCredPres'), tcStr));
  gCBSCredPres.pCredPres := ObterConteudo(ANode.Childrens.Find('pCredPres'), tcDe4);
  gCBSCredPres.vCredPres := ObterConteudo(ANode.Childrens.Find('vCredPres'), tcDe2);
  gCBSCredPres.vCredPresCondSus := ObterConteudo(ANode.Childrens.Find('vCredPresCondSus'), tcDe2);
end;

procedure TCTeXmlReader.Ler_gTribCompraGov(const ANode: TACBrXmlNode;
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

end.
