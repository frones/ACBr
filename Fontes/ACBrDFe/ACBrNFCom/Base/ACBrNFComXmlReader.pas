{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrNFComXmlReader;

interface

uses
  Classes, SysUtils,
  ACBrXmlDocument, ACBrXmlReader,
  ACBrNFComClass;

type
  { TNFComXmlReader }
  TNFComXmlReader = class(TACBrXmlReader)
  private
    FNFCom: TNFCom;

    procedure Ler_ProtNFCom(const ANode: TACBrXmlNode);
    procedure Ler_InfNFCom(const ANode: TACBrXmlNode);
    procedure Ler_Ide(const ANode: TACBrXmlNode);
    procedure Ler_Emit(const ANode: TACBrXmlNode);
    procedure Ler_EmitEnderEmit(const ANode: TACBrXmlNode);
    procedure Ler_Dest(const ANode: TACBrXmlNode);
    procedure Ler_DestEnderDest(const ANode: TACBrXmlNode);
    procedure Ler_Assinante(const ANode: TACBrXmlNode);
    procedure Ler_gSub(const ANode: TACBrXmlNode);
    procedure Ler_gSub_gNF(const ANode: TACBrXmlNode);
    procedure Ler_gCofat(const ANode: TACBrXmlNode);
    procedure Ler_gCofat_gNF(const ANode: TACBrXmlNode);
    procedure Ler_Det(const ANode: TACBrXmlNode);
    procedure Ler_DetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure Ler_DetImposto(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure Ler_DetgProcRef(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure Ler_DetgProcRefgProc(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure Ler_DetgRessarc(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure Ler_Total(const ANode: TACBrXmlNode);
    procedure Ler_gFidelidade(const ANode: TACBrXmlNode);
    procedure Ler_gFat(const ANode: TACBrXmlNode);
    procedure Ler_gFatEnderCorresp(const ANode: TACBrXmlNode);
    procedure Ler_gFatgPIX(const ANode: TACBrXmlNode);
    procedure Ler_gFatCentral(const ANode: TACBrXmlNode);
    procedure Ler_autXML(const ANode: TACBrXmlNode);
    procedure Ler_InfAdic(const ANode: TACBrXmlNode);
    procedure Ler_gRespTec(const ANode: TACBrXmlNode);
    procedure Ler_InfNFComSupl(const ANode: TACBrXmlNode);

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
    constructor Create(AOwner: TNFCom); reintroduce;

    function LerXml: Boolean; override;

    property NFCom: TNFCom read FNFCom write FNFCom;
  end;

implementation

uses
  ACBrXmlBase, ACBrUtil.Base,
  ACBrDFe.Conversao,
  ACBrNFComConversao;

{ TNFComXmlReader }

constructor TNFComXmlReader.Create(AOwner: TNFCom);
begin
  inherited Create;

  FNFCom := AOwner;
end;

function TNFComXmlReader.LerXml: Boolean;
var
  NFComNode, infNFComNode: TACBrXmlNode;
  att: TACBrXmlAttribute;
begin
  Result := False;

  if not Assigned(FNFCom) then
    raise Exception.Create('Destino não informado, informe a classe [TNFCom] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml da NFCom não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'nfcomProc' then
  begin
    Ler_ProtNFCom(Document.Root.Childrens.FindAnyNs('protNFCom'));
    NFComNode := Document.Root.Childrens.FindAnyNs('NFCom');
  end
  else
  begin
    NFComNode := Document.Root;
  end;

  if NFComNode <> nil then
  begin
    infNFComNode := NFComNode.Childrens.Find('infNFCom');

    if infNFComNode = nil then
      raise Exception.Create('Arquivo xml incorreto.');

    att := infNFComNode.Attributes.Items['Id'];

    if att = nil then
      raise Exception.Create('Não encontrei o atributo: Id');

    NFCom.infNFCom.Id := att.Content;

    att := infNFComNode.Attributes.Items['versao'];

    if att = nil then
      raise Exception.Create('Não encontrei o atributo: versao');

    NFCom.infNFCom.Versao := StringToFloat(att.Content);

    Ler_InfNFCom(infNFComNode);

    Ler_InfNFComSupl(NFComNode.Childrens.Find('infNFComSupl'));

    LerSignature(NFComNode.Childrens.Find('Signature'), NFCom.signature);

    Result := True;
  end;
end;

procedure TNFComXmlReader.Ler_ProtNFCom(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ANodeAux: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  ANodeAux := ANode.Childrens.FindAnyNs('infProt');

  if not Assigned(ANodeAux) then Exit;

  NFCom.procNFCom.tpAmb := StrToTipoAmbiente(ok, ObterConteudo(ANodeAux.Childrens.FindAnyNs('tpAmb'), tcStr));
  NFCom.procNFCom.verAplic := ObterConteudo(ANodeAux.Childrens.FindAnyNs('verAplic'), tcStr);
  NFCom.procNFCom.chDFe := ObterConteudo(ANodeAux.Childrens.FindAnyNs('chNFCom'), tcStr);
  NFCom.procNFCom.dhRecbto := ObterConteudo(ANodeAux.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
  NFCom.procNFCom.nProt := ObterConteudo(ANodeAux.Childrens.FindAnyNs('nProt'), tcStr);
  NFCom.procNFCom.digVal := ObterConteudo(ANodeAux.Childrens.FindAnyNs('digVal'), tcStr);
  NFCom.procNFCom.cStat := ObterConteudo(ANodeAux.Childrens.FindAnyNs('cStat'), tcInt);
  NFCom.procNFCom.xMotivo := ObterConteudo(ANodeAux.Childrens.FindAnyNs('xMotivo'), tcStr);
  NFCom.procNFCom.cMsg := ObterConteudo(ANodeAux.Childrens.FindAnyNs('cMsg'), tcInt);
  NFCom.procNFCom.xMsg := ObterConteudo(ANodeAux.Childrens.FindAnyNs('xMsg'), tcStr);
end;

procedure TNFComXmlReader.Ler_InfNFCom(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  Ler_Ide(ANode.Childrens.Find('ide'));
  Ler_Emit(ANode.Childrens.FindAnyNs('emit'));
  Ler_Dest(ANode.Childrens.FindAnyNs('dest'));
  Ler_Assinante(ANode.Childrens.FindAnyNs('assinante'));
  Ler_gSub(ANode.Childrens.FindAnyNs('gSub'));
  Ler_gCofat(ANode.Childrens.FindAnyNs('gCofat'));

  ANodes := ANode.Childrens.FindAllAnyNs('det');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_det(ANodes[i]);
  end;

  Ler_Total(ANode.Childrens.FindAnyNs('total'));
  Ler_gFidelidade(ANode.Childrens.FindAnyNs('gFidelidade'));
  Ler_gFat(ANode.Childrens.FindAnyNs('gFat'));
  Ler_gFatCentral(ANode.Childrens.FindAnyNs('gFatCentral'));

  ANodes := ANode.Childrens.FindAllAnyNs('autXML');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_autXML(ANodes[i]);
  end;

  Ler_InfAdic(ANode.Childrens.FindAnyNs('infAdic'));
  Ler_gRespTec(ANode.Childrens.FindAnyNs('gRespTec'));
end;

procedure TNFComXmlReader.Ler_Ide(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  sAux: string;
begin
  if not Assigned(ANode) then Exit;

  NFCom.ide.cUF := ObterConteudo(ANode.Childrens.FindAnyNs('cUF'), tcInt);
  NFCom.Ide.tpAmb := StrToTipoAmbiente(ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  NFCom.ide.modelo := ObterConteudo(ANode.Childrens.FindAnyNs('mod'), tcInt);
  NFCom.ide.serie := ObterConteudo(ANode.Childrens.FindAnyNs('serie'), tcInt);
  NFCom.ide.nNF := ObterConteudo(ANode.Childrens.FindAnyNs('nNF'), tcInt);
  NFCom.ide.cNF := ObterConteudo(ANode.Childrens.FindAnyNs('cNF'), tcInt);
  NFCom.Ide.cDV := ObterConteudo(ANode.Childrens.FindAnyNs('cDV'), tcInt);
  NFCom.ide.dhEmi := ObterConteudo(ANode.Childrens.FindAnyNs('dhEmi'), tcDatHor);
  NFCom.Ide.tpEmis := StrToTipoEmissao(ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpEmis'), tcStr));
  NFCom.Ide.nSiteAutoriz := StrToSiteAutorizator(ObterConteudo(ANode.Childrens.FindAnyNs('nSiteAutoriz'), tcStr));
  NFCom.ide.cMunFG := ObterConteudo(ANode.Childrens.FindAnyNs('cMunFG'), tcInt);
  NFCom.Ide.finNFCom := StrToFinNFCom(ObterConteudo(ANode.Childrens.FindAnyNs('finNFCom'), tcStr));
  NFCom.Ide.verProc := ObterConteudo(ANode.Childrens.FindAnyNs('verProc'), tcStr);

  sAux := ObterConteudo(ANode.Childrens.FindAnyNs('indPrePago'), tcStr);
  NFCom.Ide.indPrePago := tiNao;
  if sAux = '1' then
    NFCom.Ide.indPrePago := tiSim;

  sAux := ObterConteudo(ANode.Childrens.FindAnyNs('indCessaoMeiosRede'), tcStr);
  NFCom.Ide.indCessaoMeiosRede := tiNao;
  if sAux = '1' then
    NFCom.Ide.indCessaoMeiosRede := tiSim;

  sAux := ObterConteudo(ANode.Childrens.FindAnyNs('indNotaEntrada'), tcStr);
  NFCom.Ide.indNotaEntrada := tiNao;
  if sAux = '1' then
    NFCom.Ide.indNotaEntrada := tiSim;

  NFCom.Ide.dhCont := ObterConteudo(ANode.Childrens.FindAnyNs('dhCont'), tcDatHor);
  NFCom.Ide.xJust := ObterConteudo(ANode.Childrens.FindAnyNs('xJust'), tcStr);

  // Reforma Tritutária
  Ler_gCompraGov(NFCom.Ide.gCompraGov, ANode.Childrens.Find('gCompraGov'));
end;

procedure TNFComXmlReader.Ler_Emit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.Emit.CNPJ := ObterCNPJCPF(ANode);
  NFCom.Emit.IE := ObterConteudo(ANode.Childrens.FindAnyNs('IE'), tcStr);
  NFCom.Emit.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  NFCom.Emit.xFant := ObterConteudo(ANode.Childrens.FindAnyNs('xFant'), tcStr);

  Ler_EmitEnderEmit(ANode.Childrens.FindAnyNs('enderEmit'));
end;

procedure TNFComXmlReader.Ler_EmitEnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.Emit.enderEmit.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  NFCom.Emit.enderEmit.nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  NFCom.Emit.enderEmit.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  NFCom.Emit.enderEmit.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  NFCom.Emit.EnderEmit.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  NFCom.Emit.enderEmit.xMun := ObterConteudo(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  NFCom.Emit.enderEmit.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  NFCom.Emit.enderEmit.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  NFCom.Emit.enderEmit.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  NFCom.Emit.enderEmit.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);
end;

procedure TNFComXmlReader.Ler_Dest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.Dest.xNome := ObterConteudo(ANode.Childrens.FindAnyNs('xNome'), tcStr);

  NFCom.Dest.CNPJCPF := ObterCNPJCPF(ANode);

  if NFCom.Dest.CNPJCPF = '' then
    NFCom.Dest.idOutros := ObterConteudo(ANode.Childrens.FindAnyNs('idOutros'), tcStr);

  NFCom.Dest.indIEDest := StrToindIEDest(ObterConteudo(ANode.Childrens.FindAnyNs('indIEDest'), tcStr));
  NFCom.Dest.IE := ObterConteudo(ANode.Childrens.FindAnyNs('IE'), tcStr);
  NFCom.Dest.IM := ObterConteudo(ANode.Childrens.FindAnyNs('IM'), tcStr);

  Ler_DestEnderDest(ANode.Childrens.FindAnyNs('enderDest'));
end;

procedure TNFComXmlReader.Ler_DestEnderDest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.Dest.enderDest.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  NFCom.Dest.enderDest.nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  NFCom.Dest.enderDest.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  NFCom.Dest.enderDest.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  NFCom.Dest.enderDest.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  NFCom.Dest.enderDest.xMun := ObterConteudo(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  NFCom.Dest.enderDest.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  NFCom.Dest.enderDest.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  NFCom.Dest.enderDest.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  NFCom.Dest.enderDest.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);
end;

procedure TNFComXmlReader.Ler_Assinante(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  NFCom.assinante.iCodAssinante := ObterConteudo(ANode.Childrens.FindAnyNs('iCodAssinante'), tcStr);
  NFCom.assinante.tpAssinante := StrTotpAssinante(ObterConteudo(ANode.Childrens.FindAnyNs('tpAssinante'), tcStr));
  NFCom.assinante.tpServUtil := StrTotpServUtil(ObterConteudo(ANode.Childrens.FindAnyNs('tpServUtil'), tcStr));
  NFCom.assinante.nContrato := ObterConteudo(ANode.Childrens.FindAnyNs('nContrato'), tcStr);
  NFCom.assinante.dContratoIni := ObterConteudo(ANode.Childrens.FindAnyNs('dContratoIni'), tcDat);
  NFCom.assinante.dContratoFim := ObterConteudo(ANode.Childrens.FindAnyNs('dContratoFim'), tcDat);
  NFCom.assinante.NroTermPrinc := ObterConteudo(ANode.Childrens.FindAnyNs('NroTermPrinc'), tcStr);
  NFCom.assinante.cUFPrinc := ObterConteudo(ANode.Childrens.FindAnyNs('cUFPrinc'), tcInt);

  NFCom.assinante.TermAdic.Clear;
  ANodes := ANode.Childrens.FindAllAnyNs('NroTermAdic');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFCom.assinante.TermAdic.New;
    NFCom.assinante.TermAdic[i].NroTermAdic := ObterConteudo(ANodes[i].Childrens.FindAnyNs('NroTermAdic'), tcStr);
    NFCom.assinante.TermAdic[i].cUFAdic := ObterConteudo(ANodes[i].Childrens.FindAnyNs('cUFAdic'), tcInt);
  end;
end;

procedure TNFComXmlReader.Ler_gSub(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.gSub.chNFCom := ObterConteudo(ANode.Childrens.FindAnyNs('chNFCom'), tcStr);
  NFCom.gSub.motSub := StrToMotSub(ObterConteudo(ANode.Childrens.FindAnyNs('motSub'), tcStr));

  Ler_gSub_gNF(ANode.Childrens.FindAnyNs('gNF'));
end;

procedure TNFComXmlReader.Ler_gSub_gNF(const ANode: TACBrXmlNode);
var
  xData: string;
begin
  if not Assigned(ANode) then Exit;

  NFCom.gSub.gNF.CNPJ := ObterConteudo(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  NFCom.gSub.gNF.Modelo := ObterConteudo(ANode.Childrens.FindAnyNs('mod'), tcInt);
  NFCom.gSub.gNF.serie := ObterConteudo(ANode.Childrens.FindAnyNs('serie'), tcStr);
  NFCom.gSub.gNF.nNF := ObterConteudo(ANode.Childrens.FindAnyNs('nNF'), tcInt);

  xData := ObterConteudo(ANode.Childrens.FindAnyNs('CompetEmis'), tcStr);

  if xData <> '' then
  begin
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    NFCom.gSub.gNF.CompetEmis := StrToDate(xData);
  end
  else
    NFCom.gSub.gNF.CompetEmis := 0;

  NFCom.gSub.gNF.hash115 := ObterConteudo(ANode.Childrens.FindAnyNs('hash115'), tcStr);
end;

procedure TNFComXmlReader.Ler_gCofat(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.gCofat.chNFComLocal := ObterConteudo(ANode.Childrens.FindAnyNs('chNFComLocal'), tcStr);

  Ler_gCofat_gNF(ANode.Childrens.FindAnyNs('gNF'));
end;

procedure TNFComXmlReader.Ler_gCofat_gNF(const ANode: TACBrXmlNode);
var
  xData: string;
begin
  if not Assigned(ANode) then Exit;

  NFCom.gCofat.gNF.CNPJ := ObterConteudo(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  NFCom.gCofat.gNF.Modelo := ObterConteudo(ANode.Childrens.FindAnyNs('mod'), tcInt);
  NFCom.gCofat.gNF.serie := ObterConteudo(ANode.Childrens.FindAnyNs('serie'), tcStr);
  NFCom.gCofat.gNF.nNF := ObterConteudo(ANode.Childrens.FindAnyNs('nNF'), tcInt);

  xData := ObterConteudo(ANode.Childrens.FindAnyNs('CompetEmis'), tcStr);

  if xData <> '' then
  begin
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    NFCom.gCofat.gNF.CompetEmis := StrToDate(xData);
  end
  else
    NFCom.gCofat.gNF.CompetEmis := 0;

  NFCom.gCofat.gNF.hash115 := ObterConteudo(ANode.Childrens.FindAnyNs('hash115'), tcStr);
end;

procedure TNFComXmlReader.Ler_Det(const ANode: TACBrXmlNode);
var
  Item: TDetCollectionItem;
  sAux: string;
begin
  if not Assigned(ANode) then Exit;

  Item := NFCom.Det.New;

  Item.nItem := StrToInt(ObterConteudoTag(ANode.Attributes.Items['nItem']));
  Item.chNFComAnt := ObterConteudoTag(ANode.Attributes.Items['chNFComAnt']);
  Item.nItemAnt := StrToIntDef(ObterConteudoTag(ANode.Attributes.Items['nItemAnt']), 0);

  sAux := ObterConteudoTag(ANode.Attributes.Items['indNFComAntPapelFatCentral']);
  Item.indNFComAntPapelFatCentral := tiNao;

  if sAux <> '' then
    Item.indNFComAntPapelFatCentral := StrToTIndicador(sAux);

  Item.infAdProd := ObterConteudo(ANode.Childrens.FindAnyNs('infAdProd'), tcStr);

  Ler_DetProd(Item, ANode.Childrens.FindAnyNs('prod'));
  Ler_DetImposto(Item, ANode.Childrens.FindAnyNs('imposto'));
  Ler_DetgProcRef(Item, ANode.Childrens.FindAnyNs('gProcRef'));
  Ler_DetgRessarc(Item, ANode.Childrens.FindAnyNs('gRessarc'));
end;

procedure TNFComXmlReader.Ler_DetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  sAux: string;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  Item.Prod.cProd := ObterConteudo(ANode.Childrens.FindAnyNs('cProd'), tcStr);
  Item.Prod.xProd := ObterConteudo(ANode.Childrens.FindAnyNs('xProd'), tcStr);
  Item.Prod.cClass := ObterConteudo(ANode.Childrens.FindAnyNs('cClass'), tcStr);
  Item.Prod.CFOP := ObterConteudo(ANode.Childrens.FindAnyNs('CFOP'), tcInt);
  Item.Prod.CNPJLD := ObterConteudo(ANode.Childrens.FindAnyNs('CNPJLD'), tcStr);
  Item.Prod.uMed := StrTouMed(ObterConteudo(ANode.Childrens.FindAnyNs('uMed'), tcStr));
  Item.Prod.qFaturada := ObterConteudo(ANode.Childrens.FindAnyNs('qFaturada'), tcDe4);
  Item.Prod.vItem := ObterConteudo(ANode.Childrens.FindAnyNs('vItem'), tcDe2);
  Item.Prod.vDesc := ObterConteudo(ANode.Childrens.FindAnyNs('vDesc'), tcDe2);
  Item.Prod.vOutro := ObterConteudo(ANode.Childrens.FindAnyNs('vOutro'), tcDe2);
  Item.Prod.vProd := ObterConteudo(ANode.Childrens.FindAnyNs('vProd'), tcDe2);
  Item.Prod.dExpiracao := ObterConteudo(ANode.Childrens.FindAnyNs('dExpiracao'), tcDat);

  sAux := ObterConteudo(ANode.Childrens.FindAnyNs('indDevolucao'), tcStr);
  Item.Prod.indDevolucao := tiNao;
  if sAux = '1' then
    Item.Prod.indDevolucao := tiSim;
end;

procedure TNFComXmlReader.Ler_DetImposto(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  sAux: string;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  sAux := ObterConteudo(ANode.Childrens.FindAnyNs('indSemCST'), tcStr);
  Item.Imposto.indSemCST := tiNao;
  if sAux = '1' then
    Item.Imposto.indSemCST := tiSim;

  if Item.Imposto.indSemCST = tiNao then
  begin
    AuxNode := ANode.Childrens.Items[0];

    if (AuxNode <> nil) then
    begin
      Item.Imposto.ICMS.CST := StrToCSTICMS(ObterConteudo(AuxNode.Childrens.FindAnyNs('CST'), tcStr));
      Item.Imposto.ICMS.vBC := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBC'), tcDe2);
      Item.Imposto.ICMS.pICMS := ObterConteudo(AuxNode.Childrens.FindAnyNs('pICMS'), tcDe2);
      Item.Imposto.ICMS.vICMS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vICMS'), tcDe2);
      Item.Imposto.ICMS.pFCP := ObterConteudo(AuxNode.Childrens.FindAnyNs('pFCP'), tcDe4);
      Item.Imposto.ICMS.vFCP := ObterConteudo(AuxNode.Childrens.FindAnyNs('vFCP'), tcDe2);
      Item.Imposto.ICMS.pRedBC := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRedBC'), tcDe2);
      Item.Imposto.ICMS.vICMSDeson := ObterConteudo(AuxNode.Childrens.FindAnyNs('vICMSDeson'), tcDe2);
      Item.Imposto.ICMS.cBenef := ObterConteudo(AuxNode.Childrens.FindAnyNs('cBenef'), tcStr);

      sAux := ObterConteudo(ANode.Childrens.FindAnyNs('indSN'), tcStr);
      Item.Imposto.ICMS.indSN := tiNao;
      if sAux = '1' then
        Item.Imposto.ICMS.indSN := tiSim;
    end;

    Item.Imposto.ICMSUFDest.Clear;
    ANodes := ANode.Childrens.FindAllAnyNs('ICMSUFDest');

    for i := 0 to Length(ANodes) - 1 do
    begin
      Item.Imposto.ICMSUFDest.New;

      Item.Imposto.ICMSUFDest[i].cUFDest := StrToInt(ObterConteudoTag(ANodes[i].Attributes.Items['cUFDest']));
      Item.Imposto.ICMSUFDest[i].vBCUFDest := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vBCUFDest'), tcDe2);
      Item.Imposto.ICMSUFDest[i].pFCPUFDest := ObterConteudo(ANodes[i].Childrens.FindAnyNs('pFCPUFDest'), tcDe2);
      Item.Imposto.ICMSUFDest[i].pICMSUFDest := ObterConteudo(ANodes[i].Childrens.FindAnyNs('pICMSUFDest'), tcDe2);
      Item.Imposto.ICMSUFDest[i].vFCPUFDest := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vFCPUFDest'), tcDe2);
      Item.Imposto.ICMSUFDest[i].vICMSUFDest := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vICMSUFDest'), tcDe2);
      Item.Imposto.ICMSUFDest[i].vICMSUFEmi := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vICMSUFEmi'), tcDe2);
      Item.Imposto.ICMSUFDest[i].cBenefUFDest := ObterConteudo(ANodes[i].Childrens.FindAnyNs('cBenefUFDest'), tcStr);
    end;
  end;

  AuxNode := ANode.Childrens.FindAnyNs('PIS');

  if (AuxNode <> nil) then
  begin
    Item.Imposto.PIS.CST := StrToCSTPIS(ObterConteudo(AuxNode.Childrens.FindAnyNs('CST'), tcStr));
    Item.Imposto.PIS.vBC := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBC'), tcDe2);
    Item.Imposto.PIS.pPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('pPIS'), tcDe2);
    Item.Imposto.PIS.vPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vPIS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('COFINS');

  if (AuxNode <> nil) then
  begin
    Item.Imposto.COFINS.CST := StrToCSTCOFINS(ObterConteudo(AuxNode.Childrens.FindAnyNs('CST'), tcStr));
    Item.Imposto.COFINS.vBC := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBC'), tcDe2);
    Item.Imposto.COFINS.pCOFINS := ObterConteudo(AuxNode.Childrens.FindAnyNs('pCOFINS'), tcDe2);
    Item.Imposto.COFINS.vCOFINS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vCOFINS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('FUST');

  if (AuxNode <> nil) then
  begin
    Item.Imposto.FUST.vBC := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBC'), tcDe2);
    Item.Imposto.FUST.pFUST := ObterConteudo(AuxNode.Childrens.FindAnyNs('pFUST'), tcDe4);
    Item.Imposto.FUST.vFUST := ObterConteudo(AuxNode.Childrens.FindAnyNs('vFUST'), tcDe2);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('FUNTTEL');

  if (AuxNode <> nil) then
  begin
    Item.Imposto.FUNTTEL.vBC := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBC'), tcDe2);
    Item.Imposto.FUNTTEL.pFUNTTEL := ObterConteudo(AuxNode.Childrens.FindAnyNs('pFUNTTEL'), tcDe4);
    Item.Imposto.FUNTTEL.vFUNTTEL := ObterConteudo(AuxNode.Childrens.FindAnyNs('vFUNTTEL'), tcDe2);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('retTrib');
  if (AuxNode <> nil) then
  begin
    Item.Imposto.retTrib.vRetPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetPIS'), tcDe8);
    Item.Imposto.retTrib.vRetCofins := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCofins'), tcDe8);
    Item.Imposto.retTrib.vRetCSLL := ObterConteudo(AuxNode.Childrens.FindAnyNs('vRetCSLL'), tcDe8);
    Item.Imposto.retTrib.vBCIRRF := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBCIRRF'), tcDe8);
    Item.Imposto.retTrib.vIRRF := ObterConteudo(AuxNode.Childrens.FindAnyNs('vIRRF'), tcDe8);
  end;

  // Reforma Tributária
  Ler_IBSCBS(ANode.Childrens.Find('IBSCBS'), Item.Imposto.IBSCBS);
end;

procedure TNFComXmlReader.Ler_DetgProcRef(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  sAux: string;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  Item.gProcRef.vItem := ObterConteudo(ANode.Childrens.FindAnyNs('vItem'), tcDe2);
  Item.gProcRef.qFaturada := ObterConteudo(ANode.Childrens.FindAnyNs('qFaturada'), tcDe4);
  Item.gProcRef.vProd := ObterConteudo(ANode.Childrens.FindAnyNs('vProd'), tcDe2);
  Item.gProcRef.vDesc := ObterConteudo(ANode.Childrens.FindAnyNs('vDesc'), tcDe2);
  Item.gProcRef.vOutro := ObterConteudo(ANode.Childrens.FindAnyNs('vOutro'), tcDe2);

  sAux := ObterConteudo(ANode.Childrens.FindAnyNs('indDevolucao'), tcStr);
  Item.gProcRef.indDevolucao := tiNao;
  if sAux = '1' then
    Item.gProcRef.indDevolucao := tiSim;

  Item.gProcRef.vBC := ObterConteudo(ANode.Childrens.FindAnyNs('vBC'), tcDe2);
  Item.gProcRef.pICMS := ObterConteudo(ANode.Childrens.FindAnyNs('pICMS'), tcDe2);
  Item.gProcRef.vICMS := ObterConteudo(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);
  Item.gProcRef.vPIS := ObterConteudo(ANode.Childrens.FindAnyNs('vPIS'), tcDe2);
  Item.gProcRef.vCOFINS := ObterConteudo(ANode.Childrens.FindAnyNs('vCOFINS'), tcDe2);

  ANodes := ANode.Childrens.FindAllAnyNs('gProc');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_DetgProcRefgProc(Item, ANodes[i]);
  end;
end;

procedure TNFComXmlReader.Ler_DetgProcRefgProc(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  Itemaux: TgProcCollectionItem;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  Itemaux := Item.gProcRef.gProc.New;

  Itemaux.tpProc := StrTotpProc(ObterConteudo(ANode.Childrens.FindAnyNs('tpProc'), tcStr));
  Itemaux.nProcesso := ObterConteudo(ANode.Childrens.FindAnyNs('nProcesso'), tcStr);
end;

procedure TNFComXmlReader.Ler_DetgRessarc(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  Item.gRessarc.tpRessarc := StrTotpRessarc(ObterConteudo(ANode.Childrens.FindAnyNs('tpRessarc'), tcStr));
  Item.gRessarc.dRef := ObterConteudo(ANode.Childrens.FindAnyNs('dRef'), tcDat);
  Item.gRessarc.nProcesso := ObterConteudo(ANode.Childrens.FindAnyNs('nProcesso'), tcStr);
  Item.gRessarc.nProtReclama := ObterConteudo(ANode.Childrens.FindAnyNs('nProtReclama'), tcStr);
  Item.gRessarc.xObs := ObterConteudo(ANode.Childrens.FindAnyNs('xObs'), tcStr);
end;

procedure TNFComXmlReader.Ler_Total(const ANode: TACBrXmlNode);
var
  AuxNode2: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  NFCom.Total.vProd := ObterConteudo(ANode.Childrens.FindAnyNs('vProd'), tcDe2);

  AuxNode2 := ANode.Childrens.FindAnyNs('ICMSTot');

  if AuxNode2 <> nil then
  begin
    NFCom.Total.vBC := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vBC'), tcDe2);
    NFCom.Total.vICMS := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vICMS'), tcDe2);
    NFCom.Total.vICMSDeson := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vICMSDeson'), tcDe2);
    NFCom.Total.vFCP := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vFCP'), tcDe2);
  end;

  NFCom.Total.vCOFINS := ObterConteudo(ANode.Childrens.FindAnyNs('vCOFINS'), tcDe2);
  NFCom.Total.vPIS := ObterConteudo(ANode.Childrens.FindAnyNs('vPIS'), tcDe2);
  NFCom.Total.vFUNTTEL := ObterConteudo(ANode.Childrens.FindAnyNs('vFUNTTEL'), tcDe2);
  NFCom.Total.vFUST := ObterConteudo(ANode.Childrens.FindAnyNs('vFUST'), tcDe2);

  AuxNode2 := ANode.Childrens.FindAnyNs('vRetTribTot');

  if AuxNode2 <> nil then
  begin
    NFCom.Total.vRetPIS := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vRetPIS'), tcDe2);
    NFCom.Total.vRetCOFINS := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vRetCofins'), tcDe2);
    NFCom.Total.vRetCSLL := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vRetCSLL'), tcDe2);
    NFCom.Total.vIRRF := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vIRRF'), tcDe2);
  end;

  NFCom.Total.vDesc := ObterConteudo(ANode.Childrens.FindAnyNs('vDesc'), tcDe2);
  NFCom.Total.vOutro := ObterConteudo(ANode.Childrens.FindAnyNs('vOutro'), tcDe2);
  NFCom.Total.vNF := ObterConteudo(ANode.Childrens.FindAnyNs('vNF'), tcDe2);

  // Reforma Tributária
  Ler_IBSCBSTot(ANode.Childrens.Find('IBSCBSTot'), NFCom.total.IBSCBSTot);
  NFCom.Total.vTotDFe := ObterConteudo(ANode.Childrens.Find('vTotDFe'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gFidelidade(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.gFidelidade.qtdSaldoPts := ObterConteudo(ANode.Childrens.FindAnyNs('qtdSaldoPts'), tcStr);
  NFCom.gFidelidade.dRefSaldoPts := ObterConteudo(ANode.Childrens.FindAnyNs('dRefSaldoPts'), tcDat);
  NFCom.gFidelidade.qtdPtsResg := ObterConteudo(ANode.Childrens.FindAnyNs('qtdPtsResg'), tcStr);
  NFCom.gFidelidade.dRefResgPts := ObterConteudo(ANode.Childrens.FindAnyNs('dRefResgPts'), tcDat);
end;

procedure TNFComXmlReader.Ler_gFat(const ANode: TACBrXmlNode);
var
  xData: string;
begin
  if not Assigned(ANode) then Exit;

  xData := ObterConteudo(ANode.Childrens.FindAnyNs('CompetFat'), tcStr);

  if xData <> '' then
  begin
    xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
    NFCom.gFat.CompetFat := StrToDate(xData);
  end
  else
    NFCom.gFat.CompetFat := 0;

  NFCom.gFat.dVencFat := ObterConteudo(ANode.Childrens.FindAnyNs('dVencFat'), tcDat);
  NFCom.gFat.dPerUsoIni := ObterConteudo(ANode.Childrens.FindAnyNs('dPerUsoIni'), tcDat);
  NFCom.gFat.dPerUsoFim := ObterConteudo(ANode.Childrens.FindAnyNs('dPerUsoFim'), tcDat);
  NFCom.gFat.codBarras := ObterConteudo(ANode.Childrens.FindAnyNs('codBarras'), tcStr);
  NFCom.gFat.codDebAuto := ObterConteudo(ANode.Childrens.FindAnyNs('codDebAuto'), tcStr);
  NFCom.gFat.codBanco := ObterConteudo(ANode.Childrens.FindAnyNs('codBanco'), tcStr);
  NFCom.gFat.codAgencia := ObterConteudo(ANode.Childrens.FindAnyNs('codAgencia'), tcStr);

  Ler_gFatEnderCorresp(ANode.Childrens.FindAnyNs('enderCorresp'));
  Ler_gFatgPIX(ANode.Childrens.FindAnyNs('gPIX'));
end;

procedure TNFComXmlReader.Ler_gFatEnderCorresp(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.gFat.enderCorresp.xLgr := ObterConteudo(ANode.Childrens.FindAnyNs('xLgr'), tcStr);
  NFCom.gFat.enderCorresp.nro := ObterConteudo(ANode.Childrens.FindAnyNs('nro'), tcStr);
  NFCom.gFat.enderCorresp.xCpl := ObterConteudo(ANode.Childrens.FindAnyNs('xCpl'), tcStr);
  NFCom.gFat.enderCorresp.xBairro := ObterConteudo(ANode.Childrens.FindAnyNs('xBairro'), tcStr);
  NFCom.gFat.enderCorresp.cMun := ObterConteudo(ANode.Childrens.FindAnyNs('cMun'), tcInt);
  NFCom.gFat.enderCorresp.xMun := ObterConteudo(ANode.Childrens.FindAnyNs('xMun'), tcStr);
  NFCom.gFat.enderCorresp.CEP := ObterConteudo(ANode.Childrens.FindAnyNs('CEP'), tcInt);
  NFCom.gFat.enderCorresp.UF := ObterConteudo(ANode.Childrens.FindAnyNs('UF'), tcStr);
  NFCom.gFat.enderCorresp.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  NFCom.gFat.enderCorresp.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);
end;

procedure TNFComXmlReader.Ler_gFatgPIX(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.gFat.gPIX.urlQRCodePIX := ObterConteudo(ANode.Childrens.FindAnyNs('urlQRCodePIX'), tcStr);
end;

procedure TNFComXmlReader.Ler_gFatCentral(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.gFatCentral.CNPJ := ObterConteudo(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  NFCom.gFatCentral.cUF := ObterConteudo(ANode.Childrens.FindAnyNs('cUF'), tcInt);
end;

procedure TNFComXmlReader.Ler_autXML(const ANode: TACBrXmlNode);
var
  Item: TautXMLCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := NFCom.autXML.New;

  Item.CNPJCPF := ObterCNPJCPF(ANode);
end;

procedure TNFComXmlReader.Ler_InfAdic(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.InfAdic.infAdFisco := ObterConteudo(ANode.Childrens.FindAnyNs('infAdFisco'), tcStr);
  NFCom.InfAdic.infCpl := ObterConteudo(ANode.Childrens.FindAnyNs('infCpl'), tcStr);
end;

procedure TNFComXmlReader.Ler_gRespTec(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFCom.infRespTec.CNPJ := ObterConteudo(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
  NFCom.infRespTec.xContato := ObterConteudo(ANode.Childrens.FindAnyNs('xContato'), tcStr);
  NFCom.infRespTec.email := ObterConteudo(ANode.Childrens.FindAnyNs('email'), tcStr);
  NFCom.infRespTec.fone := ObterConteudo(ANode.Childrens.FindAnyNs('fone'), tcStr);
  NFCom.infRespTec.idCSRT := ObterConteudo(ANode.Childrens.FindAnyNs('idCSRT'), tcInt);
  NFCom.infRespTec.hashCSRT := ObterConteudo(ANode.Childrens.FindAnyNs('hashCSRT'), tcStr);
end;

procedure TNFComXmlReader.Ler_InfNFComSupl(const ANode: TACBrXmlNode);
var
  sQrCode: string;
begin
  if not Assigned(ANode) then Exit;

  sQrCode := ObterConteudo(ANode.Childrens.Find('qrCodNFCom'), tcStr);
  sQrCode := StringReplace(sQrCode, '<![CDATA[', '', []);
  sQrCode := StringReplace(sQrCode, ']]>', '', []);

  NFCom.infNFComSupl.qrCodNFCom := sQrCode;
end;

// Reforma Tributária
procedure TNFComXmlReader.Ler_gCompraGov(gCompraGov: TgCompraGovReduzido;
  const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  gCompraGov.tpEnteGov := StrTotpEnteGov(ObterConteudo(ANode.Childrens.Find('tpEnteGov'), tcStr));
  gCompraGov.pRedutor := ObterConteudo(ANode.Childrens.Find('pRedutor'), tcDe4);
end;

procedure TNFComXmlReader.Ler_IBSCBS(const ANode: TACBrXmlNode; IBSCBS: TIBSCBS);
begin
  if not Assigned(ANode) then Exit;

  IBSCBS.CST := StrToCSTIBSCBS(ObterConteudo(ANode.Childrens.Find('CST'), tcStr));
  IBSCBS.cClassTrib := StrTocClassTrib(ObterConteudo(ANode.Childrens.Find('cClassTrib'), tcStr));

  Ler_IBSCBS_gIBSCBS(ANode.Childrens.Find('gIBSCBS'), IBSCBS.gIBSCBS);
end;

procedure TNFComXmlReader.Ler_IBSCBS_gIBSCBS(const ANode: TACBrXmlNode; gIBSCBS: TgIBSCBS);
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

procedure TNFComXmlReader.Ler_gIBSUF(const ANode: TACBrXmlNode; gIBSUF: TgIBSUFValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSUF.pIBS := ObterConteudo(ANode.Childrens.Find('pIBSUF'), tcDe4);

  Ler_gIBSUF_gDif(ANode.Childrens.Find('gDif'), gIBSUF.gDif);
  Ler_gIBSUF_gDevTrib(ANode.Childrens.Find('gDevTrib'), gIBSUF.gDevTrib);
  Ler_gIBSUF_gRed(ANode.Childrens.Find('gRed'), gIBSUF.gRed);

  gIBSUF.vIBS := ObterConteudo(ANode.Childrens.Find('vIBSUF'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gIBSUF_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
begin
  if not Assigned(ANode) then Exit;

  gDif.pDif := ObterConteudo(ANode.Childrens.Find('pDif'), tcDe4);
  gDif.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gIBSUF_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
begin
  if not Assigned(ANode) then Exit;

  gDevTrib.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gIBSUF_gRed(const ANode: TACBrXmlNode; gRed: TgRed);
begin
  if not Assigned(ANode) then Exit;

  gRed.pRedAliq := ObterConteudo(ANode.Childrens.Find('pRedAliq'), tcDe4);
  gRed.pAliqEfet := ObterConteudo(ANode.Childrens.Find('pAliqEfet'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gIBSMun(const ANode: TACBrXmlNode; gIBSMun: TgIBSMunValores);
begin
  if not Assigned(ANode) then Exit;

  gIBSMun.pIBS := ObterConteudo(ANode.Childrens.Find('pIBSMun'), tcDe4);

  Ler_gIBSMun_gDif(ANode.Childrens.Find('gDif'), gIBSMun.gDif);
  Ler_gIBSMun_gDevTrib(ANode.Childrens.Find('gDevTrib'), gIBSMun.gDevTrib);
  Ler_gIBSMun_gRed(ANode.Childrens.Find('gRed'), gIBSMun.gRed);

  gIBSMun.vIBS := ObterConteudo(ANode.Childrens.Find('vIBSMun'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gIBSMun_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
begin
  if not Assigned(ANode) then Exit;

  gDif.pDif := ObterConteudo(ANode.Childrens.Find('pDif'), tcDe4);
  gDif.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gIBSMun_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
begin
  if not Assigned(ANode) then Exit;

  gDevTrib.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gIBSMun_gRed(const ANode: TACBrXmlNode; gRed: TgRed);
begin
  if not Assigned(ANode) then Exit;

  gRed.pRedAliq := ObterConteudo(ANode.Childrens.Find('pRedAliq'), tcDe4);
  gRed.pAliqEfet := ObterConteudo(ANode.Childrens.Find('pAliqEfet'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gCBS(const ANode: TACBrXmlNode; gCBS: TgCBSValores);
begin
  if not Assigned(ANode) then Exit;

  gCBS.pCBS := ObterConteudo(ANode.Childrens.Find('pCBS'), tcDe4);

  Ler_gCBS_gDif(ANode.Childrens.Find('gDif'), gCBS.gDif);
  Ler_gCBS_gDevTrib(ANode.Childrens.Find('gDevTrib'), gCBS.gDevTrib);
  Ler_gCBS_gRed(ANode.Childrens.Find('gRed'), gCBS.gRed);

  gCBS.vCBS := ObterConteudo(ANode.Childrens.Find('vCBS'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gCBS_gDif(const ANode: TACBrXmlNode; gDif: TgDif);
begin
  if not Assigned(ANode) then Exit;

  gDif.pDif := ObterConteudo(ANode.Childrens.Find('pDif'), tcDe4);
  gDif.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gCBS_gDevTrib(const ANode: TACBrXmlNode; gDevTrib: TgDevTrib);
begin
  if not Assigned(ANode) then Exit;

  gDevTrib.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gCBS_gRed(const ANode: TACBrXmlNode; gRed: TgRed);
begin
  if not Assigned(ANode) then Exit;

  gRed.pRedAliq := ObterConteudo(ANode.Childrens.Find('pRedAliq'), tcDe4);
  gRed.pAliqEfet := ObterConteudo(ANode.Childrens.Find('pAliqEfet'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gIBSCBS_gTribRegular(const ANode: TACBrXmlNode;
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

procedure TNFComXmlReader.Ler_gIBSCredPres(const ANode: TACBrXmlNode; gIBSCredPres: TgIBSCBSCredPres);
begin
  if not Assigned(ANode) then Exit;

  gIBSCredPres.cCredPres := StrTocCredPres(ObterConteudo(ANode.Childrens.Find('cCredPres'), tcStr));
  gIBSCredPres.pCredPres := ObterConteudo(ANode.Childrens.Find('pCredPres'), tcDe4);
  gIBSCredPres.vCredPres := ObterConteudo(ANode.Childrens.Find('vCredPres'), tcDe2);
  gIBSCredPres.vCredPresCondSus := ObterConteudo(ANode.Childrens.Find('vCredPresCondSus'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gCBSCredPres(const ANode: TACBrXmlNode; gCBSCredPres: TgIBSCBSCredPres);
begin
  if not Assigned(ANode) then Exit;

  gCBSCredPres.cCredPres := StrTocCredPres(ObterConteudo(ANode.Childrens.Find('cCredPres'), tcStr));
  gCBSCredPres.pCredPres := ObterConteudo(ANode.Childrens.Find('pCredPres'), tcDe4);
  gCBSCredPres.vCredPres := ObterConteudo(ANode.Childrens.Find('vCredPres'), tcDe2);
  gCBSCredPres.vCredPresCondSus := ObterConteudo(ANode.Childrens.Find('vCredPresCondSus'), tcDe2);
end;

procedure TNFComXmlReader.Ler_gTribCompraGov(const ANode: TACBrXmlNode;
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

procedure TNFComXmlReader.Ler_IBSCBSTot(const ANode: TACBrXmlNode;
  IBSCBSTot: TIBSCBSTot);
begin
  if not Assigned(ANode) then Exit;

  IBSCBSTot.vBCIBSCBS := ObterConteudo(ANode.Childrens.Find('vBCIBSCBS'), tcDe2);

  Ler_IBSCBSTot_gIBS(ANode.Childrens.Find('gIBS'), IBSCBSTot.gIBS);
  Ler_IBSCBSTot_gCBS(ANode.Childrens.Find('gCBS'), IBSCBSTot.gCBS);
end;

procedure TNFComXmlReader.Ler_IBSCBSTot_gIBS(const ANode: TACBrXmlNode;
  gIBS: TgIBS);
begin
  if not Assigned(ANode) then Exit;

  Ler_IBSCBSTot_gIBS_gIBSUFTot(ANode.Childrens.Find('gIBSUF'), gIBS.gIBSUFTot);
  Ler_IBSCBSTot_gIBS_gIBSMunTot(ANode.Childrens.Find('gIBSMun'), gIBS.gIBSMunTot);

  gIBS.vIBS := ObterConteudo(ANode.Childrens.Find('vIBS'), tcDe2);
  gIBS.vCredPres := ObterConteudo(ANode.Childrens.Find('vCredPres'), tcDe2);
  gIBS.vCredPresCondSus := ObterConteudo(ANode.Childrens.Find('vCredPresCondSus'), tcDe2);
end;

procedure TNFComXmlReader.Ler_IBSCBSTot_gIBS_gIBSUFTot(const ANode: TACBrXmlNode;
  gIBSUFTot: TgIBSUFTot);
begin
  if not Assigned(ANode) then Exit;

  gIBSUFTot.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
  gIBSUFTot.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
  gIBSUFTot.vIBSUF := ObterConteudo(ANode.Childrens.Find('vIBSUF'), tcDe2);
end;

procedure TNFComXmlReader.Ler_IBSCBSTot_gIBS_gIBSMunTot(const ANode: TACBrXmlNode;
  gIBSMunTot: TgIBSMunTot);
begin
  if not Assigned(ANode) then Exit;

  gIBSMunTot.vDif := ObterConteudo(ANode.Childrens.Find('vDif'), tcDe2);
  gIBSMunTot.vDevTrib := ObterConteudo(ANode.Childrens.Find('vDevTrib'), tcDe2);
  gIBSMunTot.vIBSMun := ObterConteudo(ANode.Childrens.Find('vIBSMun'), tcDe2);
end;

procedure TNFComXmlReader.Ler_IBSCBSTot_gCBS(const ANode: TACBrXmlNode;
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

