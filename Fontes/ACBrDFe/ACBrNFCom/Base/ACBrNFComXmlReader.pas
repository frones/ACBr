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

    procedure LerProtNFCom(const ANode: TACBrXmlNode);
    procedure LerInfNFCom(const ANode: TACBrXmlNode);
    procedure LerIde(const ANode: TACBrXmlNode);
    procedure LerEmit(const ANode: TACBrXmlNode);
    procedure LerEmitEnderEmit(const ANode: TACBrXmlNode);
    procedure LerDest(const ANode: TACBrXmlNode);
    procedure LerDestEnderDest(const ANode: TACBrXmlNode);
    procedure LerAssinante(const ANode: TACBrXmlNode);
    procedure LergSub(const ANode: TACBrXmlNode);
    procedure LergNF(const ANode: TACBrXmlNode);
    procedure LergCofat(const ANode: TACBrXmlNode);
    procedure LerDet(const ANode: TACBrXmlNode);
    procedure LerDetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetImposto(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetgProcRef(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetgProcRefgProc(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetgRessarc(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerTotal(const ANode: TACBrXmlNode);
    procedure LergFidelidade(const ANode: TACBrXmlNode);
    procedure LergFat(const ANode: TACBrXmlNode);
    procedure LergFatEnderCorresp(const ANode: TACBrXmlNode);
    procedure LergFatgPIX(const ANode: TACBrXmlNode);
    procedure LergFatCentral(const ANode: TACBrXmlNode);
    procedure LerautXML(const ANode: TACBrXmlNode);
    procedure LerInfAdic(const ANode: TACBrXmlNode);
    procedure LergRespTec(const ANode: TACBrXmlNode);
    procedure LerInfNFComSupl(const ANode: TACBrXmlNode);
    procedure LerSignature(const ANode: TACBrXmlNode);
  public
    constructor Create(AOwner: TNFCom); reintroduce;

    function LerXml: Boolean; override;

    property NFCom: TNFCom read FNFCom write FNFCom;
  end;

implementation

uses
  ACBrXmlBase, ACBrUtil.Base, //ACBrDFeConversao,
  ACBrNFComConversao;

{ TNFComXmlReader }

constructor TNFComXmlReader.Create(AOwner: TNFCom);
begin
  inherited Create;

  FNFCom := AOwner;
end;

function TNFComXmlReader.LerXml: Boolean;
var
  NFComNode: TACBrXmlNode;
begin
  Result := False;

  if not Assigned(FNFCom) or (FNFCom = nil) then
    raise Exception.Create('Destino não informado, informe a classe [TNFCom] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml da NFCom não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'NFComProc' then
  begin
    LerProtNFCom(Document.Root.Childrens.FindAnyNs('protNFCom'));
    NFComNode := Document.Root.Childrens.FindAnyNs('NFCom');
  end
  else
  begin
    NFComNode := Document.Root;
  end;

  if NFComNode = nil then
    raise Exception.Create('Arquivo xml incorreto.');

  if NFComNode <> nil then
  begin
    LerInfNFCom(NFComNode);
    LerInfNFComSupl(NFComNode);
    LerSignature(NFComNode);

    Result := True;
  end;
end;

procedure TNFComXmlReader.LerProtNFCom(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFCom.procNFCom.tpAmb := StrToTipoAmbiente(ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  NFCom.procNFCom.verAplic := ObterConteudo(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
  NFCom.procNFCom.chNFCom := ObterConteudo(ANode.Childrens.FindAnyNs('chNFCom'), tcStr);
  NFCom.procNFCom.dhRecbto := ObterConteudo(ANode.Childrens.FindAnyNs('dhRecbto'), tcDatHor);
  NFCom.procNFCom.nProt := ObterConteudo(ANode.Childrens.FindAnyNs('nProt'), tcStr);
  NFCom.procNFCom.digVal := ObterConteudo(ANode.Childrens.FindAnyNs('digVal'), tcStr);
  NFCom.procNFCom.cStat := ObterConteudo(ANode.Childrens.FindAnyNs('cStat'), tcInt);
  NFCom.procNFCom.xMotivo := ObterConteudo(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);
  NFCom.procNFCom.cMsg := ObterConteudo(ANode.Childrens.FindAnyNs('cMsg'), tcInt);
  NFCom.procNFCom.xMsg := ObterConteudo(ANode.Childrens.FindAnyNs('xMsg'), tcStr);
end;

procedure TNFComXmlReader.LerInfNFCom(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
  att: TACBrXmlAttribute;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('infNFCom');

  if AuxNode = nil then
    raise Exception.Create('Arquivo xml incorreto.');

  att := AuxNode.Attributes.Items['Id'];
  if att = nil then
    raise Exception.Create('Não encontrei o atributo: Id');

  NFCom.infNFCom.Id := att.Content;

  att := AuxNode.Attributes.Items['versao'];
  if att = nil then
    raise Exception.Create('Não encontrei o atributo: versao');

  NFCom.infNFCom.Versao := StringToFloat(att.Content);

  LerIde(AuxNode);
  LerEmit(AuxNode);
  LerDest(AuxNode);
  LerAssinante(AuxNode);
  LergSub(AuxNode);
  LergCofat(AuxNode);

  ANodes := AuxNode.Childrens.FindAllAnyNs('det');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Lerdet(ANodes[i]);
  end;

  LerTotal(AuxNode);
  LergFidelidade(AuxNode);
  LergFat(AuxNode);
  LergFatCentral(AuxNode);

  ANodes := AuxNode.Childrens.FindAllAnyNs('autXML');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerautXML(ANodes[i]);
  end;

  LerInfAdic(AuxNode);
  LergRespTec(AuxNode);
end;

procedure TNFComXmlReader.LerIde(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ide');

  if AuxNode = nil then Exit;

  NFCom.ide.cUF := ObterConteudo(AuxNode.Childrens.FindAnyNs('cUF'), tcInt);
  NFCom.Ide.tpAmb := StrToTipoAmbiente(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpAmb'), tcStr));
  NFCom.ide.modelo := ObterConteudo(AuxNode.Childrens.FindAnyNs('mod'), tcInt);
  NFCom.ide.serie := ObterConteudo(AuxNode.Childrens.FindAnyNs('serie'), tcInt);
  NFCom.ide.nNF := ObterConteudo(AuxNode.Childrens.FindAnyNs('nNF'), tcInt);
  NFCom.ide.cNF := ObterConteudo(AuxNode.Childrens.FindAnyNs('cNF'), tcInt);
  NFCom.Ide.cDV := ObterConteudo(AuxNode.Childrens.FindAnyNs('cDV'), tcInt);
  NFCom.ide.dhEmi := ObterConteudo(AuxNode.Childrens.FindAnyNs('dhEmi'), tcDatHor);
  NFCom.Ide.tpEmis := StrToTipoEmissao(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpEmis'), tcStr));
  NFCom.Ide.nSiteAutoriz := StrToSiteAutorizator(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('nSiteAutoriz'), tcStr));
  NFCom.ide.cMunFG := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMunFG'), tcInt);
  NFCom.Ide.finNFCom := StrToFinNFCom(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('finNFCom'), tcStr));
  NFCom.Ide.verProc := ObterConteudo(AuxNode.Childrens.FindAnyNs('verProc'), tcStr);
  NFCom.Ide.indPrePago := StrToTIndicador(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('indPrePago'), tcStr));
  NFCom.Ide.indCessaoMeiosRede := StrToTIndicador(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('indCessaoMeiosRede'), tcStr));
  NFCom.Ide.indNotaEntrada := StrToTIndicador(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('indNotaEntrada'), tcStr));
  NFCom.Ide.dhCont := ObterConteudo(AuxNode.Childrens.FindAnyNs('dhCont'), tcDatHor);
  NFCom.Ide.xJust := ObterConteudo(AuxNode.Childrens.FindAnyNs('xJust'), tcStr);
end;

procedure TNFComXmlReader.LerEmit(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('emit');

  if AuxNode = nil then Exit;

  NFCom.Emit.CNPJ := ObterCNPJCPF(AuxNode);
  NFCom.Emit.IE := ObterConteudo(AuxNode.Childrens.FindAnyNs('IE'), tcStr);
  NFCom.Emit.xNome := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);
  NFCom.Emit.xFant := ObterConteudo(AuxNode.Childrens.FindAnyNs('xFant'), tcStr);

  LerEmitEnderEmit(AuxNode);
end;

procedure TNFComXmlReader.LerEmitEnderEmit(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('enderEmit');

  if AuxNode = nil then Exit;

  NFCom.Emit.enderEmit.xLgr := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
  NFCom.Emit.enderEmit.nro := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
  NFCom.Emit.enderEmit.xCpl := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
  NFCom.Emit.enderEmit.xBairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
  NFCom.Emit.EnderEmit.cMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcInt);
  NFCom.Emit.enderEmit.xMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('xMun'), tcStr);
  NFCom.Emit.enderEmit.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcInt);
  NFCom.Emit.enderEmit.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
  NFCom.Emit.enderEmit.fone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
  NFCom.Emit.enderEmit.email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
end;

procedure TNFComXmlReader.LerDest(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('dest');

  if AuxNode = nil then Exit;

  NFCom.Dest.xNome := ObterConteudo(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);

  NFCom.Dest.CNPJCPF := ObterCNPJCPF(AuxNode);

  if NFCom.Dest.CNPJCPF = '' then
    NFCom.Dest.idOutros := ObterConteudo(AuxNode.Childrens.FindAnyNs('idOutros'), tcStr);

  NFCom.Dest.indIEDest := StrToindIEDest(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('indIEDest'), tcStr));
  NFCom.Dest.IE := ObterConteudo(AuxNode.Childrens.FindAnyNs('IE'), tcStr);
  NFCom.Dest.IM := ObterConteudo(AuxNode.Childrens.FindAnyNs('IM'), tcStr);

  LerDestEnderDest(AuxNode);
end;

procedure TNFComXmlReader.LerDestEnderDest(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('enderDest');

  if AuxNode = nil then Exit;

  NFCom.Dest.enderDest.xLgr := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
  NFCom.Dest.enderDest.nro := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
  NFCom.Dest.enderDest.xCpl := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
  NFCom.Dest.enderDest.xBairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
  NFCom.Dest.enderDest.cMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcInt);
  NFCom.Dest.enderDest.xMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('xMun'), tcStr);
  NFCom.Dest.enderDest.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcInt);
  NFCom.Dest.enderDest.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
  NFCom.Dest.enderDest.fone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
  NFCom.Dest.enderDest.email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
end;

procedure TNFComXmlReader.LerAssinante(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('assinante');

  if AuxNode = nil then Exit;

  NFCom.assinante.iCodAssinante := ObterConteudo(AuxNode.Childrens.FindAnyNs('iCodAssinante'), tcStr);
  NFCom.assinante.tpAssinante := StrTotpAssinante(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpAssinante'), tcStr));
  NFCom.assinante.tpServUtil := StrTotpServUtil(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('tpServUtil'), tcStr));
  NFCom.assinante.nContrato := ObterConteudo(AuxNode.Childrens.FindAnyNs('nContrato'), tcStr);
  NFCom.assinante.dContratoIni := ObterConteudo(AuxNode.Childrens.FindAnyNs('dContratoIni'), tcDat);
  NFCom.assinante.dContratoFim := ObterConteudo(AuxNode.Childrens.FindAnyNs('dContratoFim'), tcDat);
  NFCom.assinante.NroTermPrinc := ObterConteudo(AuxNode.Childrens.FindAnyNs('NroTermPrinc'), tcStr);
  NFCom.assinante.cUFPrinc := ObterConteudo(AuxNode.Childrens.FindAnyNs('cUFPrinc'), tcInt);

  NFCom.assinante.TermAdic.Clear;
  ANodes := AuxNode.Childrens.FindAllAnyNs('NroTermAdic');

  for i := 0 to Length(ANodes) - 1 do
  begin
    NFCom.assinante.TermAdic.New;
    NFCom.assinante.TermAdic[i].NroTermAdic := ObterConteudo(ANodes[i].Childrens.FindAnyNs('NroTermAdic'), tcStr);
    NFCom.assinante.TermAdic[i].cUFAdic := ObterConteudo(ANodes[i].Childrens.FindAnyNs('cUFAdic'), tcInt);
  end;
end;

procedure TNFComXmlReader.LergSub(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('gSub');

  if AuxNode = nil then Exit;

  NFCom.gSub.chNFCom := ObterConteudo(AuxNode.Childrens.FindAnyNs('chNFCom'), tcStr);
  NFCom.gSub.motSub := StrToMotSub(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('motSub'), tcStr));

  LergNF(AuxNode);
end;

procedure TNFComXmlReader.LergNF(const ANode: TACBrXmlNode);
var
  xData: string;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('gNF');

  if AuxNode = nil then Exit;

  NFCom.gSub.gNF.CNPJ := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);
  NFCom.gSub.gNF.Modelo := ObterConteudo(AuxNode.Childrens.FindAnyNs('mod'), tcInt);
  NFCom.gSub.gNF.serie := ObterConteudo(AuxNode.Childrens.FindAnyNs('serie'), tcStr);
  NFCom.gSub.gNF.nNF := ObterConteudo(AuxNode.Childrens.FindAnyNs('nNF'), tcInt);

  xData := ObterConteudo(AuxNode.Childrens.FindAnyNs('CompetEmis'), tcStr);
  xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
  NFCom.gSub.gNF.CompetEmis := StrToDate(xData);

  NFCom.gSub.gNF.hash115 := ObterConteudo(AuxNode.Childrens.FindAnyNs('hash115'), tcStr);
end;

procedure TNFComXmlReader.LergCofat(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('gCofat');

  if AuxNode = nil then Exit;

  NFCom.gCofat.chNFComLocal := ObterConteudo(AuxNode.Childrens.FindAnyNs('chNFComLocal'), tcStr);
end;

procedure TNFComXmlReader.LerDet(const ANode: TACBrXmlNode);
var
  Item: TDetCollectionItem;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NFCom.Det.New;

  Item.nItem := StrToInt(ObterConteudoTag(ANode.Attributes.Items['nItem']));
  Item.chNFComAnt := ObterConteudoTag(ANode.Attributes.Items['chNFComAnt']);
  Item.nItemAnt := StrToIntDef(ObterConteudoTag(ANode.Attributes.Items['nItemAnt']), 0);

  Item.infAdProd := ObterConteudo(ANode.Childrens.FindAnyNs('infAdProd'), tcStr);

  LerDetProd(Item, ANode.Childrens.FindAnyNs('prod'));
  LerDetImposto(Item, ANode.Childrens.FindAnyNs('imposto'));
  LerDetgProcRef(Item, ANode.Childrens.FindAnyNs('gProcRef'));
  LerDetgRessarc(Item, ANode.Childrens.FindAnyNs('gRessarc'));
end;

procedure TNFComXmlReader.LerDetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item.Prod.cProd := ObterConteudo(ANode.Childrens.FindAnyNs('cProd'), tcStr);
  Item.Prod.xProd := ObterConteudo(ANode.Childrens.FindAnyNs('xProd'), tcStr);
  Item.Prod.cClass := ObterConteudo(ANode.Childrens.FindAnyNs('cClass'), tcInt);
  Item.Prod.CFOP := ObterConteudo(ANode.Childrens.FindAnyNs('CFOP'), tcInt);
  Item.Prod.CNPJLD := ObterConteudo(ANode.Childrens.FindAnyNs('CNPJLD'), tcStr);
  Item.Prod.uMed := StrTouMed(ok, ObterConteudo(ANode.Childrens.FindAnyNs('uMed'), tcStr));
  Item.Prod.qFaturada := ObterConteudo(ANode.Childrens.FindAnyNs('qFaturada'), tcDe4);
  Item.Prod.vItem := ObterConteudo(ANode.Childrens.FindAnyNs('vItem'), tcDe2);
  Item.Prod.vDesc := ObterConteudo(ANode.Childrens.FindAnyNs('vDesc'), tcDe2);
  Item.Prod.vOutro := ObterConteudo(ANode.Childrens.FindAnyNs('vOutro'), tcDe2);
  Item.Prod.vProd := ObterConteudo(ANode.Childrens.FindAnyNs('vProd'), tcDe2);
  Item.Prod.dExpiracao := ObterConteudo(ANode.Childrens.FindAnyNs('dExpiracao'), tcDat);
  Item.Prod.indDevolucao := StrToTIndicador(ok, ObterConteudo(ANode.Childrens.FindAnyNs('indDevolucao'), tcStr));
end;

procedure TNFComXmlReader.LerDetImposto(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ICMS');

  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];

  if (AuxNode <> nil) then
  begin
    Item.Imposto.ICMS.CST := StrToCSTICMS(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('CST'), tcStr));
    Item.Imposto.ICMS.vBC := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBC'), tcDe2);
    Item.Imposto.ICMS.pICMS := ObterConteudo(AuxNode.Childrens.FindAnyNs('pICMS'), tcDe2);
    Item.Imposto.ICMS.vICMS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vICMS'), tcDe2);
    Item.Imposto.ICMS.pFCP := ObterConteudo(AuxNode.Childrens.FindAnyNs('pFCP'), tcDe4);
    Item.Imposto.ICMS.vFCP := ObterConteudo(AuxNode.Childrens.FindAnyNs('vFCP'), tcDe2);
    Item.Imposto.ICMS.pRedBC := ObterConteudo(AuxNode.Childrens.FindAnyNs('pRedBC'), tcDe2);
    Item.Imposto.ICMS.vICMSDeson := ObterConteudo(AuxNode.Childrens.FindAnyNs('vICMSDeson'), tcDe2);
    Item.Imposto.ICMS.cBenef := ObterConteudo(AuxNode.Childrens.FindAnyNs('cBenef'), tcStr);
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
  end;

  AuxNode := ANode.Childrens.FindAnyNs('PIS');

  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];

  if (AuxNode <> nil) then
  begin
    Item.Imposto.PIS.CST := StrToCSTPIS(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('CST'), tcStr));
    Item.Imposto.PIS.vBC := ObterConteudo(AuxNode.Childrens.FindAnyNs('vBC'), tcDe2);
    Item.Imposto.PIS.pPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('pPIS'), tcDe2);
    Item.Imposto.PIS.vPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vPIS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.FindAnyNs('COFINS');

  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];

  if (AuxNode <> nil) then
  begin
    Item.Imposto.COFINS.CST := StrToCSTCOFINS(ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('CST'), tcStr));
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
end;

procedure TNFComXmlReader.LerDetgProcRef(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item.gProcRef.vItem := ObterConteudo(ANode.Childrens.FindAnyNs('vItem'), tcDe2);
  Item.gProcRef.qFaturada := ObterConteudo(ANode.Childrens.FindAnyNs('qFaturada'), tcDe4);
  Item.gProcRef.vProd := ObterConteudo(ANode.Childrens.FindAnyNs('vProd'), tcDe2);
  Item.gProcRef.vDesc := ObterConteudo(ANode.Childrens.FindAnyNs('vDesc'), tcDe2);
  Item.gProcRef.vOutro := ObterConteudo(ANode.Childrens.FindAnyNs('vOutro'), tcDe2);
  Item.gProcRef.indDevolucao := StrToTIndicador(ok, ObterConteudo(ANode.Childrens.FindAnyNs('indDevolucao'), tcStr));
  Item.gProcRef.vBC := ObterConteudo(ANode.Childrens.FindAnyNs('vBC'), tcDe2);
  Item.gProcRef.pICMS := ObterConteudo(ANode.Childrens.FindAnyNs('pICMS'), tcDe2);
  Item.gProcRef.vICMS := ObterConteudo(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);
  Item.gProcRef.vPIS := ObterConteudo(ANode.Childrens.FindAnyNs('vPIS'), tcDe2);
  Item.gProcRef.vCOFINS := ObterConteudo(ANode.Childrens.FindAnyNs('vCOFINS'), tcDe2);

  ANodes := ANode.Childrens.FindAllAnyNs('gProc');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerDetgProcRefgProc(Item, ANodes[i]);
  end;
end;

procedure TNFComXmlReader.LerDetgProcRefgProc(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  ok: Boolean;
  Itemaux: TgProcCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Itemaux := Item.gProcRef.gProc.New;

  Itemaux.tpProc := StrTotpProc(ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpProc'), tcStr));
  Itemaux.nProcesso := ObterConteudo(ANode.Childrens.FindAnyNs('nProcesso'), tcStr);
end;

procedure TNFComXmlReader.LerDetgRessarc(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item.gRessarc.tpRessarc := StrTotpRessarc(ok, ObterConteudo(ANode.Childrens.FindAnyNs('tpRessarc'), tcStr));
  Item.gRessarc.dRef := ObterConteudo(ANode.Childrens.FindAnyNs('dRef'), tcDat);
  Item.gRessarc.nProcesso := ObterConteudo(ANode.Childrens.FindAnyNs('nProcesso'), tcStr);
  Item.gRessarc.nProtReclama := ObterConteudo(ANode.Childrens.FindAnyNs('nProtReclama'), tcStr);
  Item.gRessarc.xObs := ObterConteudo(ANode.Childrens.FindAnyNs('xObs'), tcStr);
end;

procedure TNFComXmlReader.LerTotal(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNode2: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('total');

  if AuxNode = nil then Exit;

  NFCom.Total.vProd := ObterConteudo(AuxNode.Childrens.FindAnyNs('vProd'), tcDe2);

  AuxNode2 := ANode.Childrens.FindAnyNs('ICMSTot');

  if AuxNode2 <> nil then
  begin
    NFCom.Total.vBC := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vBC'), tcDe2);
    NFCom.Total.vICMS := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vICMS'), tcDe2);
    NFCom.Total.vICMSDeson := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vICMSDeson'), tcDe2);
    NFCom.Total.vFCP := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vFCP'), tcDe2);
  end;

  NFCom.Total.vCOFINS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vCOFINS'), tcDe2);
  NFCom.Total.vPIS := ObterConteudo(AuxNode.Childrens.FindAnyNs('vPIS'), tcDe2);
  NFCom.Total.vFUNTTEL := ObterConteudo(AuxNode.Childrens.FindAnyNs('vFUNTTEL'), tcDe2);
  NFCom.Total.vFUST := ObterConteudo(AuxNode.Childrens.FindAnyNs('vFUST'), tcDe2);

  AuxNode2 := ANode.Childrens.FindAnyNs('vRetTribTot');

  if AuxNode2 <> nil then
  begin
    NFCom.Total.vRetPIS := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vRetPIS'), tcDe2);
    NFCom.Total.vRetCOFINS := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vRetCofins'), tcDe2);
    NFCom.Total.vRetCSLL := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vRetCSLL'), tcDe2);
    NFCom.Total.vIRRF := ObterConteudo(AuxNode2.Childrens.FindAnyNs('vIRRF'), tcDe2);
  end;

  NFCom.Total.vDesc := ObterConteudo(AuxNode.Childrens.FindAnyNs('vDesc'), tcDe2);
  NFCom.Total.vOutro := ObterConteudo(AuxNode.Childrens.FindAnyNs('vOutro'), tcDe2);
  NFCom.Total.vNF := ObterConteudo(AuxNode.Childrens.FindAnyNs('vNF'), tcDe2);
end;

procedure TNFComXmlReader.LergFidelidade(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('gFidelidade');

  if AuxNode = nil then Exit;

  NFCom.gFidelidade.qtdSaldoPts := ObterConteudo(AuxNode.Childrens.FindAnyNs('qtdSaldoPts'), tcStr);
  NFCom.gFidelidade.dRefSaldoPts := ObterConteudo(AuxNode.Childrens.FindAnyNs('dRefSaldoPts'), tcDat);
  NFCom.gFidelidade.qtdPtsResg := ObterConteudo(AuxNode.Childrens.FindAnyNs('qtdPtsResg'), tcStr);
  NFCom.gFidelidade.dRefResgPts := ObterConteudo(AuxNode.Childrens.FindAnyNs('dRefResgPts'), tcDat);
end;

procedure TNFComXmlReader.LergFat(const ANode: TACBrXmlNode);
var
  xData: string;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('gFat');

  if AuxNode = nil then Exit;

  xData := ObterConteudo(AuxNode.Childrens.FindAnyNs('CompetFat'), tcStr);
  xData := '01/' + Copy(xData, 5, 2) + '/' + Copy(xData, 1, 4);
  NFCom.gFat.CompetFat := StrToDate(xData);

  NFCom.gFat.dVencFat := ObterConteudo(AuxNode.Childrens.FindAnyNs('dVencFat'), tcDat);
  NFCom.gFat.dPerUsoIni := ObterConteudo(AuxNode.Childrens.FindAnyNs('dPerUsoIni'), tcDat);
  NFCom.gFat.dPerUsoFim := ObterConteudo(AuxNode.Childrens.FindAnyNs('dPerUsoFim'), tcDat);
  NFCom.gFat.codBarras := ObterConteudo(AuxNode.Childrens.FindAnyNs('codBarras'), tcStr);
  NFCom.gFat.codDebAuto := ObterConteudo(AuxNode.Childrens.FindAnyNs('codDebAuto'), tcStr);
  NFCom.gFat.codBanco := ObterConteudo(AuxNode.Childrens.FindAnyNs('codBanco'), tcStr);
  NFCom.gFat.codAgencia := ObterConteudo(AuxNode.Childrens.FindAnyNs('codAgencia'), tcStr);

  LergFatEnderCorresp(AuxNode);
  LergFatgPIX(AuxNode);
end;

procedure TNFComXmlReader.LergFatEnderCorresp(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('enderCorresp');

  if AuxNode = nil then Exit;

  NFCom.gFat.enderCorresp.xLgr := ObterConteudo(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr);
  NFCom.gFat.enderCorresp.nro := ObterConteudo(AuxNode.Childrens.FindAnyNs('nro'), tcStr);
  NFCom.gFat.enderCorresp.xCpl := ObterConteudo(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr);
  NFCom.gFat.enderCorresp.xBairro := ObterConteudo(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr);
  NFCom.gFat.enderCorresp.cMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('cMun'), tcInt);
  NFCom.gFat.enderCorresp.xMun := ObterConteudo(AuxNode.Childrens.FindAnyNs('xMun'), tcStr);
  NFCom.gFat.enderCorresp.CEP := ObterConteudo(AuxNode.Childrens.FindAnyNs('CEP'), tcInt);
  NFCom.gFat.enderCorresp.UF := ObterConteudo(AuxNode.Childrens.FindAnyNs('UF'), tcStr);
  NFCom.gFat.enderCorresp.fone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
  NFCom.gFat.enderCorresp.email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
end;

procedure TNFComXmlReader.LergFatgPIX(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('gPIX');

  if AuxNode = nil then Exit;

  NFCom.gFat.gPIX.urlQRCodePIX := ObterConteudo(AuxNode.Childrens.FindAnyNs('urlQRCodePIX'), tcStr);
end;

procedure TNFComXmlReader.LergFatCentral(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('gFatCentral');

  if AuxNode = nil then Exit;

  NFCom.gFatCentral.CNPJ := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);
  NFCom.gFatCentral.cUF := ObterConteudo(AuxNode.Childrens.FindAnyNs('cUF'), tcInt);
end;

procedure TNFComXmlReader.LerautXML(const ANode: TACBrXmlNode);
var
  Item: TautXMLCollectionItem;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NFCom.autXML.New;

  Item.CNPJCPF := ObterCNPJCPF(ANode);
end;

procedure TNFComXmlReader.LerInfAdic(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('infAdic');

  if AuxNode = nil then Exit;

  NFCom.InfAdic.infAdFisco := ObterConteudo(AuxNode.Childrens.FindAnyNs('infAdFisco'), tcStr);
  NFCom.InfAdic.infCpl := ObterConteudo(AuxNode.Childrens.FindAnyNs('infCpl'), tcStr);
end;

procedure TNFComXmlReader.LergRespTec(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('gRespTec');

  if AuxNode = nil then Exit;

  NFCom.infRespTec.CNPJ := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);
  NFCom.infRespTec.xContato := ObterConteudo(AuxNode.Childrens.FindAnyNs('xContato'), tcStr);
  NFCom.infRespTec.email := ObterConteudo(AuxNode.Childrens.FindAnyNs('email'), tcStr);
  NFCom.infRespTec.fone := ObterConteudo(AuxNode.Childrens.FindAnyNs('fone'), tcStr);
  NFCom.infRespTec.idCSRT := ObterConteudo(AuxNode.Childrens.FindAnyNs('idCSRT'), tcInt);
  NFCom.infRespTec.hashCSRT := ObterConteudo(AuxNode.Childrens.FindAnyNs('hashCSRT'), tcStr);
end;

procedure TNFComXmlReader.LerInfNFComSupl(const ANode: TACBrXmlNode);
var
 sQrCode: string;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('infNFComSupl');

  if AuxNode <> nil then
  begin
    sQrCode := ObterConteudo(AuxNode.Childrens.FindAnyNs('qrCodNFCom'), tcStr);
    sQrCode := StringReplace(sQrCode, '<![CDATA[', '', []);
    sQrCode := StringReplace(sQrCode, ']]>', '', []);

    NFCom.infNFComSupl.qrCodNFCom := sQrCode;
  end;
end;

procedure TNFComXmlReader.LerSignature(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNode2: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Signature');

  AuxNode2 := AuxNode.Childrens.FindAnyNs('SignedInfo');

  if AuxNode2 <> nil then
  begin
    AuxNode2 := AuxNode2.Childrens.FindAnyNs('Reference');

    if AuxNode2 <> nil then
    begin
      NFCom.signature.URI := AuxNode2.Attributes.Items['URI'].Content;
      NFCom.signature.DigestValue := ObterConteudo(AuxNode2.Childrens.FindAnyNs('DigestValue'), tcStr);
    end;
  end;

  NFCom.signature.SignatureValue := ObterConteudo(AuxNode.Childrens.FindAnyNs('SignatureValue'), tcStr);

  AuxNode2 := AuxNode.Childrens.FindAnyNs('KeyInfo');

  if AuxNode2 <> nil then
    NFCom.signature.X509Certificate := ObterConteudo(AuxNode2.Childrens.FindAnyNs('X509Certificate'), tcStr);
end;

end.

