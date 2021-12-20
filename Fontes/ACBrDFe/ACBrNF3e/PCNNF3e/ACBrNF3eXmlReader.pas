{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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
  pcnNF3e;

type
  { TNF3eXmlReader }
  TNF3eXmlReader = class(TACBrXmlReader)
  private
    FNF3e: TNF3e;

    procedure LerProtNF3e(const ANode: TACBrXmlNode);
    procedure LerInfNF3e(const ANode: TACBrXmlNode);
    procedure LerIde(const ANode: TACBrXmlNode);
    procedure LerIdeNFref(const ANode: TACBrXmlNode);
    procedure LerEmit(const ANode: TACBrXmlNode);
    procedure LerEmitEnderEmit(const ANode: TACBrXmlNode);
    procedure LerAvulsa(const ANode: TACBrXmlNode);
    procedure LerDest(const ANode: TACBrXmlNode);
    procedure LerDestEnderDest(const ANode: TACBrXmlNode);
    procedure LerRetirada(const ANode: TACBrXmlNode);
    procedure LerEntrega(const ANode: TACBrXmlNode);
    procedure LerDet(const ANode: TACBrXmlNode);
    procedure LerDetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetProdDI(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetProdDetExport(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetProdRastro(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetProdVeicProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetProdMed(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetProdArma(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetProdComb(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerDetImposto(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerTotal(const ANode: TACBrXmlNode);
    procedure LerTransp(const ANode: TACBrXmlNode);
    procedure LerTranspVol(const ANode: TACBrXmlNode);
    procedure LerCobr(const ANode: TACBrXmlNode);
    procedure LerInfAdic(const ANode: TACBrXmlNode);
    procedure LerExporta(const ANode: TACBrXmlNode);
    procedure LerCompra(const ANode: TACBrXmlNode);
    procedure LerCana(const ANode: TACBrXmlNode);
    procedure LerInfRespTec(const ANode: TACBrXmlNode);
    procedure LerInfNF3eSupl(const ANode: TACBrXmlNode);
    procedure LerSignature(const ANode: TACBrXmlNode);

  public
    constructor Create(AOwner: TNF3e); reintroduce;

    function LerXml: Boolean; override;

    property NF3e: TNF3e read FNF3e write FNF3e;

  end;

implementation

uses
  pcnConversao, pcnConversaoNF3e,
  ACBrUtil;

{ TNF3eXmlReader }
constructor TNF3eXmlReader.Create(AOwner: TNF3e);
begin
  inherited Create;

  FNF3e := AOwner;
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

  if Document.Root.Name = 'NF3eProc' then
  begin
    LerProtNF3e(Document.Root.Childrens.Find('protNF3e'));
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

procedure TNF3eXmlReader.LerProtNF3e(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.procNF3e.tpAmb    := StrToTpAmb(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  NF3e.procNF3e.verAplic := ObterConteudo(ANode.Childrens.Find('verAplic'), tcStr);
  NF3e.procNF3e.chNF3e    := ObterConteudo(ANode.Childrens.Find('chNF3e'), tcStr);
  NF3e.procNF3e.dhRecbto := ObterConteudo(ANode.Childrens.Find('dhRecbto'), tcDatHor);
  NF3e.procNF3e.nProt    := ObterConteudo(ANode.Childrens.Find('nProt'), tcStr);
  NF3e.procNF3e.digVal   := ObterConteudo(ANode.Childrens.Find('digVal'), tcStr);
  NF3e.procNF3e.cStat    := ObterConteudo(ANode.Childrens.Find('cStat'), tcInt);
  NF3e.procNF3e.xMotivo  := ObterConteudo(ANode.Childrens.Find('xMotivo'), tcStr);
  NF3e.procNF3e.cMsg     := ObterConteudo(ANode.Childrens.Find('cMsg'), tcInt);
  NF3e.procNF3e.xMsg     := ObterConteudo(ANode.Childrens.Find('xMsg'), tcStr);
end;

procedure TNF3eXmlReader.LerInfNF3e(const ANode: TACBrXmlNode);
Var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  LerIde(ANode.Childrens.Find('ide'));
  LerEmit(ANode.Childrens.Find('emit'));
  LerAvulsa(ANode.Childrens.Find('avulsa'));
  LerDest(ANode.Childrens.Find('dest'));
  LerRetirada(ANode.Childrens.Find('retirada'));
  LerEntrega(ANode.Childrens.Find('entrega'));
  ANodes := ANode.Childrens.FindAll('NFref');

  ANodes := ANode.Childrens.FindAll('autXML');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NF3e.autXML.New;
    NF3e.autXML[i].CNPJCPF := ObterCNPJCPF(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('det');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerDet(ANodes[i]);
  end;

  LerTotal(ANode.Childrens.Find('total'));
  LerTransp(ANode.Childrens.Find('transp'));
  LerCobr(ANode.Childrens.Find('cobr'));
  LerInfAdic(ANode.Childrens.Find('infAdic'));
  LerExporta(ANode.Childrens.Find('exporta'));
  LerCompra(ANode.Childrens.Find('compra'));
  LerCana(ANode.Childrens.Find('cana'));
  LerInfRespTec(ANode.Childrens.Find('infRespTec'));
end;

procedure TNF3eXmlReader.LerIde(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*B02*) NF3e.ide.cUF := ObterConteudo(ANode.Childrens.Find('cUF'), tcInt);
  (*B03*) NF3e.ide.cNF := ObterConteudo(ANode.Childrens.Find('cNF'), tcInt);
  if NF3e.ide.cNF = 0 then
    NF3e.ide.cNF := -2;
  (*B06*) NF3e.ide.modelo := ObterConteudo(ANode.Childrens.Find('mod'), tcInt);
  (*B07*) NF3e.ide.serie  := ObterConteudo(ANode.Childrens.Find('serie'), tcInt);
  (*B08*) NF3e.ide.nNF    := ObterConteudo(ANode.Childrens.Find('nNF'), tcInt);
  (*B09*) NF3e.ide.dhEmi   := ObterConteudo(ANode.Childrens.Find('dhEmi'), tcDatHor);

  (*B12*) NF3e.ide.cMunFG := ObterConteudo(ANode.Childrens.Find('cMunFG'), tcInt);
  (*B22*) NF3e.Ide.tpEmis := StrToTpEmis(ok, ObterConteudo(ANode.Childrens.Find('tpEmis'), tcStr));
  (*B23*) NF3e.Ide.cDV    := ObterConteudo(ANode.Childrens.Find('cDV'), tcInt);
  (*B24*) NF3e.Ide.tpAmb  := StrToTpAmb(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  (*B25*) NF3e.Ide.finNF3e := StrToFinNF3e(ok, ObterConteudo(ANode.Childrens.Find('finNF3e'), tcStr));

  (*B27*) NF3e.Ide.verProc := ObterConteudo(ANode.Childrens.Find('verProc'), tcStr);
  (*B28*) NF3e.Ide.dhCont  := ObterConteudo(ANode.Childrens.Find('dhCont'), tcDatHor);
  (*B29*) NF3e.Ide.xJust   := ObterConteudo(ANode.Childrens.Find('xJust'), tcStr);

  ANodes := ANode.Childrens.FindAll('NFref');
  for i := 0 to Length(ANodes) - 1 do
    LerIdeNFref(ANodes[i]);
end;

procedure TNF3eXmlReader.LerIdeNFref(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  refNode: TACBrXmlNode;
  i: Integer;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerEmit(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*C02/C02a*)NF3e.Emit.CNPJ    := ObterCNPJCPF(ANode);
  (*C03*)NF3e.Emit.xNome        := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  (*C04*)NF3e.Emit.xFant        := ObterConteudo(ANode.Childrens.Find('xFant'), tcStr);
  (*C17*)NF3e.Emit.IE           := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);

  LerEmitEnderEmit(ANode.Childrens.Find('enderEmit'));
end;

procedure TNF3eXmlReader.LerEmitEnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*C06*)NF3e.Emit.enderEmit.xLgr    := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*C07*)NF3e.Emit.enderEmit.nro     := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*C08*)NF3e.Emit.enderEmit.xCpl    := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*C09*)NF3e.Emit.enderEmit.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*C10*)NF3e.Emit.EnderEmit.cMun    := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*C11*)NF3e.Emit.enderEmit.xMun    := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*C12*)NF3e.Emit.enderEmit.UF      := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*C13*)NF3e.Emit.enderEmit.CEP     := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*C16*)NF3e.Emit.enderEmit.fone    := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
end;

procedure TNF3eXmlReader.LerAvulsa(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerDest(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then
  begin
    NF3e.Dest.indIEDest := inNaoContribuinte;
    Exit;
  end;

  (*E02/E03*)NF3e.Dest.CNPJCPF := ObterCNPJCPF(ANode);

  (*E04*)NF3e.Dest.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);

  if NF3e.infNF3e.Versao >= 3 then
    (*E16a*)NF3e.Dest.indIEDest := StrToindIEDest(Ok, ObterConteudo(ANode.Childrens.Find('indIEDest'), tcStr));

  (*E17*)NF3e.Dest.IE := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);

  if NF3e.infNF3e.Versao >= 3 then
    (*E18a*)NF3e.Dest.IM := ObterConteudo(ANode.Childrens.Find('IM'), tcStr);

  LerDestEnderDest(ANode.Childrens.Find('enderDest'));
end;

procedure TNF3eXmlReader.LerDestEnderDest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*E06*)NF3e.Dest.enderDest.xLgr    := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*E07*)NF3e.Dest.enderDest.nro     := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*E08*)NF3e.Dest.enderDest.xCpl    := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*E09*)NF3e.Dest.enderDest.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*E10*)NF3e.Dest.enderDest.cMun    := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*E11*)NF3e.Dest.enderDest.xMun    := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*E12*)NF3e.Dest.enderDest.UF      := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*E13*)NF3e.Dest.enderDest.CEP     := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*E16*)NF3e.Dest.enderDest.fone    := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
end;

procedure TNF3eXmlReader.LerRetirada(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerEntrega(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerDet(const ANode: TACBrXmlNode);
Var
  Item: TDetCollectionItem;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;
  (*
  Item := NF3e.Det.New;
  Item.prod.nItem := NF3e.Det.Count;
  Item.infAdProd  := ObterConteudo(ANode.Childrens.Find('infAdProd'), tcStr);

  LerDetProd(Item, ANode.Childrens.Find('prod'));
  LerDetImposto(Item, ANode.Childrens.Find('imposto'));

  AuxNode := ANode.Childrens.Find('impostoDevol');
  if (AuxNode <> nil) then
  begin
    Item.pDevol := ObterConteudo(AuxNode.Childrens.Find('pDevol'), tcDe2);

    AuxNode := AuxNode.Childrens.Find('IPI');
    if (AuxNode <> nil) then
    begin
      Item.vIPIDevol := ObterConteudo(AuxNode.Childrens.Find('vIPIDevol'), tcDe2);
    end;
  end;
  *)
end;

procedure TNF3eXmlReader.LerDetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;
  (*
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
  *)
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
  (*
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
  *)
end;

procedure TNF3eXmlReader.LerDetProdDetExport(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  AuxNode: TACBrXmlNode;
//  DetExportItem: TdetExportCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;
  (*
  DetExportItem := Item.Prod.detExport.New;
  DetExportItem.nDraw := ObterConteudo(ANode.Childrens.Find('nDraw'), tcStr);

  AuxNode := ANode.Childrens.Find('exportInd');
  if (AuxNode <> nil) then
  begin
    DetExportItem.nRE     := ObterConteudo(AuxNode.Childrens.Find('nRE'), tcStr);
    DetExportItem.chNF3e   := ObterConteudo(AuxNode.Childrens.Find('chNF3e'), tcStr);
    DetExportItem.qExport := ObterConteudo(AuxNode.Childrens.Find('qExport'), tcDe4);
  end;
  *)
end;

procedure TNF3eXmlReader.LerDetProdRastro(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
//Var
//  RastroItem: TRastroCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;
  (*
  RastroItem := Item.Prod.rastro.New;
  RastroItem.nLote  := ObterConteudo(ANode.Childrens.Find('nLote'), tcStr);
  RastroItem.qLote  := ObterConteudo(ANode.Childrens.Find('qLote'), tcDe3);
  RastroItem.dFab   := ObterConteudo(ANode.Childrens.Find('dFab'), tcDat);
  RastroItem.dVal   := ObterConteudo(ANode.Childrens.Find('dVal'), tcDat);
  RastroItem.cAgreg := ObterConteudo(ANode.Childrens.Find('cAgreg'), tcStr);
  *)
end;

procedure TNF3eXmlReader.LerDetProdVeicProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerDetProdMed(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;
end;

procedure TNF3eXmlReader.LerDetProdArma(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerDetProdComb(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerDetImposto(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  AuxNode, AxNode: TACBrXmlNode;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;
  (*
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
  *)
end;

procedure TNF3eXmlReader.LerTotal(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;
  (*
  AuxNode := ANode.Childrens.Find('ICMSTot');
  if (AuxNode <> nil) then
  begin
    NF3e.Total.ICMSTot.vBC           := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    NF3e.Total.ICMSTot.vICMS         := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    NF3e.Total.ICMSTot.vICMSDeson   := ObterConteudo(AuxNode.Childrens.Find('vICMSDeson'), tcDe2);
    NF3e.Total.ICMSTot.vFCPUFDest   := ObterConteudo(AuxNode.Childrens.Find('vFCPUFDest'), tcDe2);
    NF3e.Total.ICMSTot.vICMSUFDest  := ObterConteudo(AuxNode.Childrens.Find('vICMSUFDest'), tcDe2);
    NF3e.Total.ICMSTot.vICMSUFRemet := ObterConteudo(AuxNode.Childrens.Find('vICMSUFRemet'), tcDe2);
    NF3e.Total.ICMSTot.vFCP         := ObterConteudo(AuxNode.Childrens.Find('vFCP'), tcDe2);
    NF3e.Total.ICMSTot.vBCST         := ObterConteudo(AuxNode.Childrens.Find('vBCST'), tcDe2);
    NF3e.Total.ICMSTot.vST           := ObterConteudo(AuxNode.Childrens.Find('vST'), tcDe2);
    NF3e.Total.ICMSTot.vFCPST       := ObterConteudo(AuxNode.Childrens.Find('vFCPST'), tcDe2);
    NF3e.Total.ICMSTot.vFCPSTRet    := ObterConteudo(AuxNode.Childrens.Find('vFCPSTRet'), tcDe2);
    NF3e.Total.ICMSTot.vProd         := ObterConteudo(AuxNode.Childrens.Find('vProd'), tcDe2);
    NF3e.Total.ICMSTot.vFrete        := ObterConteudo(AuxNode.Childrens.Find('vFrete'), tcDe2);
    NF3e.Total.ICMSTot.vSeg          := ObterConteudo(AuxNode.Childrens.Find('vSeg'), tcDe2);
    NF3e.Total.ICMSTot.vDesc         := ObterConteudo(AuxNode.Childrens.Find('vDesc'), tcDe2);
    NF3e.Total.ICMSTot.vII           := ObterConteudo(AuxNode.Childrens.Find('vII'), tcDe2);
    NF3e.Total.ICMSTot.vIPI          := ObterConteudo(AuxNode.Childrens.Find('vIPI'), tcDe2);
    NF3e.Total.ICMSTot.vIPIDevol    := ObterConteudo(AuxNode.Childrens.Find('vIPIDevol'), tcDe2);
    NF3e.Total.ICMSTot.vPIS          := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    NF3e.Total.ICMSTot.vCOFINS       := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
    NF3e.Total.ICMSTot.vOutro        := ObterConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
    NF3e.Total.ICMSTot.vNF           := ObterConteudo(AuxNode.Childrens.Find('vNF'), tcDe2);
    NF3e.Total.ICMSTot.vTotTrib     := ObterConteudo(AuxNode.Childrens.Find('vTotTrib'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ISSQNtot');
  if (AuxNode <> nil) then
  begin
    NF3e.Total.ISSQNtot.vServ   := ObterConteudo(AuxNode.Childrens.Find('vServ'), tcDe2);
    NF3e.Total.ISSQNtot.vBC     := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    NF3e.Total.ISSQNtot.vISS    := ObterConteudo(AuxNode.Childrens.Find('vISS'), tcDe2);
    NF3e.Total.ISSQNtot.vPIS    := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    NF3e.Total.ISSQNtot.vCOFINS := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);

    if NF3e.infNF3e.Versao >= 3 then
    begin
      NF3e.Total.ISSQNtot.dCompet     := ObterConteudo(AuxNode.Childrens.Find('dCompet'), tcDat);
      NF3e.Total.ISSQNtot.vDeducao    := ObterConteudo(AuxNode.Childrens.Find('vDeducao'), tcDe2);
      NF3e.Total.ISSQNtot.vOutro      := ObterConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
      NF3e.Total.ISSQNtot.vDescIncond := ObterConteudo(AuxNode.Childrens.Find('vDescIncond'), tcDe2);
      NF3e.Total.ISSQNtot.vDescCond   := ObterConteudo(AuxNode.Childrens.Find('vDescCond'), tcDe2);
      NF3e.Total.ISSQNtot.vISSRet     := ObterConteudo(AuxNode.Childrens.Find('vISSRet'), tcDe2);
      NF3e.Total.ISSQNtot.cRegTrib    := StrToRegTribISSQN(Ok, ObterConteudo(AuxNode.Childrens.Find('cRegTrib'), tcStr));
    end;
  end;

  AuxNode := ANode.Childrens.Find('retTrib');
  if (AuxNode <> nil) then
  begin
    NF3e.Total.retTrib.vRetPIS    := ObterConteudo(AuxNode.Childrens.Find('vRetPIS'), tcDe2);
    NF3e.Total.retTrib.vRetCOFINS := ObterConteudo(AuxNode.Childrens.Find('vRetCOFINS'), tcDe2);
    NF3e.Total.retTrib.vRetCSLL   := ObterConteudo(AuxNode.Childrens.Find('vRetCSLL'), tcDe2);
    NF3e.Total.retTrib.vBCIRRF    := ObterConteudo(AuxNode.Childrens.Find('vBCIRRF'), tcDe2);
    NF3e.Total.retTrib.vIRRF      := ObterConteudo(AuxNode.Childrens.Find('vIRRF'), tcDe2);
    NF3e.Total.retTrib.vBCRetPrev := ObterConteudo(AuxNode.Childrens.Find('vBCRetPrev'), tcDe2);
    NF3e.Total.retTrib.vRetPrev   := ObterConteudo(AuxNode.Childrens.Find('vRetPrev'), tcDe2);
  end;
  *)
end;

procedure TNF3eXmlReader.LerTransp(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerTranspVol(const ANode: TACBrXmlNode);
Var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerCobr(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
  tagPag: String;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('fat');
  (*
  if (AuxNode <> nil) then
  begin
    NF3e.Cobr.Fat.nFat  := ObterConteudo(AuxNode.Childrens.Find('nFat'), tcStr);
    NF3e.Cobr.Fat.vOrig := ObterConteudo(AuxNode.Childrens.Find('vOrig'), tcDe2);
    NF3e.Cobr.Fat.vDesc := ObterConteudo(AuxNode.Childrens.Find('vDesc'), tcDe2);
    NF3e.Cobr.Fat.vLiq  := ObterConteudo(AuxNode.Childrens.Find('vLiq'), tcDe2);
  end;

  NF3e.Cobr.Dup.Clear;
  ANodes := ANode.Childrens.FindAll('dup');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NF3e.Cobr.Dup.New;
    NF3e.Cobr.Dup[i].nDup  := ObterConteudo(ANodes[i].Childrens.Find('nDup'), tcStr);
    NF3e.Cobr.Dup[i].dVenc := ObterConteudo(ANodes[i].Childrens.Find('dVenc'), tcDat);
    NF3e.Cobr.Dup[i].vDup  := ObterConteudo(ANodes[i].Childrens.Find('vDup'), tcDe2);
  end;

  if NF3e.infNF3e.Versao >= 3 then
  begin
    NF3e.pag.Clear;
    if NF3e.infNF3e.Versao >= 4 then
    begin
      AuxNode := ANode.Childrens.Find('pag');
      if (AuxNode <> nil) then
        NF3e.pag.vTroco := ObterConteudo(AuxNode.Childrens.Find('vTroco'), tcDe2);

      tagPag := 'detPag';
    end
    else
      tagPag := 'pag';
    end;

   ANodes := ANode.Childrens.FindAll(tagPag);
   for i := 0 to Length(ANodes) - 1 do
   begin
     NF3e.pag.New;
     NF3e.pag[i].indPag := StrToIndpag(Ok, ObterConteudo(ANodes[i].Childrens.Find('indPag'), tcStr));
     NF3e.pag[i].tPag := StrToFormaPagamento(ok, ObterConteudo(ANodes[i].Childrens.Find('tPag'), tcStr));
     NF3e.pag[i].vPag := ObterConteudo(ANodes[i].Childrens.Find('vPag'), tcDe2);

     AuxNode := ANode.Childrens.Find('card');
     if (AuxNode <> nil) then
     begin
       NF3e.pag[i].tpIntegra := StrTotpIntegra(ok, ObterConteudo(AuxNode.Childrens.Find('tpIntegra'), tcStr));
       NF3e.pag[i].CNPJ  := ObterConteudo(AuxNode.Childrens.Find('CNPJ'), tcStr);
       NF3e.pag[i].tBand := StrToBandeiraCartao(ok, ObterConteudo(AuxNode.Childrens.Find('tBand'), tcStr));
       NF3e.pag[i].cAut  := ObterConteudo(AuxNode.Childrens.Find('cAut'), tcStr);
     end;
   end;
   *)
end;

procedure TNF3eXmlReader.LerInfAdic(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NF3e.InfAdic.infAdFisco := ObterConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);
  NF3e.InfAdic.infCpl     := ObterConteudo(ANode.Childrens.Find('infCpl'), tcStr);
  (*
  NF3e.InfAdic.obsCont.Clear;
  ANodes := ANode.Childrens.FindAll('obsCont');
  for i := 0 to Length(ANodes) - 1 do
    begin
      NF3e.InfAdic.obsCont.New;
      NF3e.InfAdic.obsCont[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
      NF3e.InfAdic.obsCont[i].xTexto := ObterConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
    end;

  NF3e.InfAdic.obsCont.Clear;
  ANodes := ANode.Childrens.FindAll('obsFisco');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NF3e.InfAdic.obsFisco.New;
    NF3e.InfAdic.obsFisco[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
    NF3e.InfAdic.obsFisco[i].xTexto := ObterConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
  end;

  NF3e.InfAdic.procRef.Clear;
  ANodes := ANode.Childrens.FindAll('procRef');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NF3e.InfAdic.obsCont.New;
    NF3e.InfAdic.procRef[i].nProc   := ObterConteudo(ANodes[i].Childrens.Find('nProc'),tcStr);
    NF3e.InfAdic.procRef[i].indProc := StrToIndProc(ok, ObterConteudo(ANodes[i].Childrens.Find('indProc'), tcStr));
  end;
  *)
end;

procedure TNF3eXmlReader.LerExporta(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerCompra(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerCana(const ANode: TACBrXmlNode);
Var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

end;

procedure TNF3eXmlReader.LerInfRespTec(const ANode: TACBrXmlNode);
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
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;
  (*
  NF3e.infNF3eSupl.qrCode := ObterConteudo(ANode.Childrens.Find('qrCode'), tcStr);
  NF3e.infNF3eSupl.qrCode := StringReplace(NF3e.infNF3eSupl.qrCode, '<![CDATA[', '', []);
  NF3e.infNF3eSupl.qrCode := StringReplace(NF3e.infNF3eSupl.qrCode, ']]>', '', []);
  NF3e.infNF3eSupl.urlChave := ObterConteudo(ANode.Childrens.Find('urlChave'), tcStr);
  *)
end;

procedure TNF3eXmlReader.LerSignature(const ANode: TACBrXmlNode);
Var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('SignedInfo');
  if AuxNode <> nil then
  begin

    AuxNode := ANode.Childrens.Find('Reference');
    if AuxNode <> nil then
    begin
      NF3e.signature.URI             := AuxNode.Attributes.Items['URI'].Content;
      NF3e.signature.DigestValue     := ObterConteudo(AuxNode.Childrens.Find('DigestValue'), tcStr);
    end;
  end;

  NF3e.signature.SignatureValue  := ObterConteudo(ANode.Childrens.Find('SignatureValue'), tcStr);

  AuxNode := ANode.Childrens.Find('KeyInfo');
  if AuxNode <> nil then
  begin
    NF3e.signature.X509Certificate := ObterConteudo(ANode.Childrens.Find('X509Certificate'), tcStr);
  end;
end;

end.

