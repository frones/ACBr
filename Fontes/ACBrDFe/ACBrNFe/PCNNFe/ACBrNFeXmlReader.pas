{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrNFeXmlReader;

interface

uses
  Classes, SysUtils,
  ACBrXmlDocument, ACBrXmlReader,
  pcnNFe;

type
  { TNFeXmlReader }
  TNFeXmlReader = class(TACBrXmlReader)
  private
    FNFe: TNFe;

    procedure LerProtNFe(const ANode: TACBrXmlNode);
    procedure LerInfNFe(const ANode: TACBrXmlNode);
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
    procedure LerDetObsItem(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerTotal(const ANode: TACBrXmlNode);
    procedure LerTransp(const ANode: TACBrXmlNode);
    procedure LerTranspVol(const ANode: TACBrXmlNode);
    procedure LerCobr(const ANode: TACBrXmlNode);
    procedure LerInfIntermed(const ANode: TACBrXmlNode);
    procedure LerInfAdic(const ANode: TACBrXmlNode);
    procedure LerExporta(const ANode: TACBrXmlNode);
    procedure LerCompra(const ANode: TACBrXmlNode);
    procedure LerCana(const ANode: TACBrXmlNode);
    procedure LerInfRespTec(const ANode: TACBrXmlNode);
    procedure LerInfNFeSupl(const ANode: TACBrXmlNode);

  public
    constructor Create(AOwner: TNFe); reintroduce;

    function LerXml: Boolean; override;

    property NFe: TNFe read FNFe write FNFe;

  end;

implementation

uses
  ACBrUtil.Base,
  pcnConversao,
  ACBrXmlBase,
  pcnConversaoNFe;

{ TNFeXmlReader }
constructor TNFeXmlReader.Create(AOwner: TNFe);
begin
  inherited Create;

  FNFe := AOwner;
end;

function TNFeXmlReader.LerXml: Boolean;
var
  NFeNode, infNFeNode: TACBrXmlNode;
  att: TACBrXmlAttribute;
begin
  if not Assigned(FNFe) then
    raise Exception.Create('Destino não informado, informe a classe [TNFe] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml da Nfe não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'nfeProc' then
  begin
    LerProtNFe(Document.Root.Childrens.Find('protNFe'));
    NFeNode := Document.Root.Childrens.Find('NFe');
  end
  else
  begin
    NFeNode := Document.Root;
  end;

  if NFeNode <> nil then
  begin
    infNFeNode := NFeNode.Childrens.Find('infNFe');

    if infNFeNode = nil then
      raise Exception.Create('Arquivo xml incorreto.');

    att := infNFeNode.Attributes.Items['Id'];
    if att = nil then
      raise Exception.Create('Não encontrei o atributo: Id');

    NFe.infNFe.Id := att.Content;

    att := infNFeNode.Attributes.Items['versao'];
    if att = nil then
      raise Exception.Create('Não encontrei o atributo: versao');

    NFe.infNFe.Versao := StringToFloat(att.Content);

    LerInfNFe(infNFeNode);
    LerInfNFeSupl(NFeNode.Childrens.Find('infNFeSupl'));
    LerSignature(NFeNode.Childrens.Find('Signature'), NFe.Signature);
  end;

  Result := True;
end;

procedure TNFeXmlReader.LerProtNFe(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('infProt');
  if Assigned(ANode) then
  begin
    NFe.procNFe.tpAmb    := StrToTpAmb(ok, ObterConteudo(AuxNode.Childrens.Find('tpAmb'), tcStr));
    NFe.procNFe.verAplic := ObterConteudo(AuxNode.Childrens.Find('verAplic'), tcStr);
    NFe.procNFe.chNFe    := ObterConteudo(AuxNode.Childrens.Find('chNFe'), tcStr);
    NFe.procNFe.dhRecbto := ObterConteudo(AuxNode.Childrens.Find('dhRecbto'), tcDatHor);
    NFe.procNFe.nProt    := ObterConteudo(AuxNode.Childrens.Find('nProt'), tcStr);
    NFe.procNFe.digVal   := ObterConteudo(AuxNode.Childrens.Find('digVal'), tcStr);
    NFe.procNFe.cStat    := ObterConteudo(AuxNode.Childrens.Find('cStat'), tcInt);
    NFe.procNFe.xMotivo  := ObterConteudo(AuxNode.Childrens.Find('xMotivo'), tcStr);
    NFe.procNFe.cMsg     := ObterConteudo(AuxNode.Childrens.Find('cMsg'), tcInt);
    NFe.procNFe.xMsg     := ObterConteudo(AuxNode.Childrens.Find('xMsg'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerInfNFe(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  LerIde(ANode.Childrens.Find('ide'));
  LerEmit(ANode.Childrens.Find('emit'));
  LerAvulsa(ANode.Childrens.Find('avulsa'));
  LerDest(ANode.Childrens.Find('dest'));
  LerRetirada(ANode.Childrens.Find('retirada'));
  LerEntrega(ANode.Childrens.Find('entrega'));

  ANodes := ANode.Childrens.FindAll('autXML');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.autXML.New;
    NFe.autXML[i].CNPJCPF := ObterCNPJCPF(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('det');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerDet(ANodes[i]);
  end;

  LerTotal(ANode.Childrens.Find('total'));
  LerTransp(ANode.Childrens.Find('transp'));
  LerCobr(ANode);
  LerInfIntermed(ANode.Childrens.Find('infIntermed'));
  LerInfAdic(ANode.Childrens.Find('infAdic'));
  LerExporta(ANode.Childrens.Find('exporta'));
  LerCompra(ANode.Childrens.Find('compra'));
  LerCana(ANode.Childrens.Find('cana'));
  LerInfRespTec(ANode.Childrens.Find('infRespTec'));
end;

procedure TNFeXmlReader.LerIde(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  NFe.ide.cUF := ObterConteudo(ANode.Childrens.Find('cUF'), tcInt);
  NFe.ide.cNF := ObterConteudo(ANode.Childrens.Find('cNF'), tcInt);
  if NFe.ide.cNF = 0 then
    NFe.ide.cNF := -2;
  NFe.ide.natOp  := ObterConteudo(ANode.Childrens.Find('natOp'), tcStr);
  NFe.ide.indPag := StrToIndpag(ok, ObterConteudo(ANode.Childrens.Find('indPag'), tcStr));
  NFe.ide.modelo := ObterConteudo(ANode.Childrens.Find('mod'), tcInt);
  NFe.ide.serie  := ObterConteudo(ANode.Childrens.Find('serie'), tcInt);
  NFe.ide.nNF    := ObterConteudo(ANode.Childrens.Find('nNF'), tcInt);

  if NFe.infNFe.Versao >= 3 then
  begin
    NFe.ide.dEmi    := ObterConteudo(ANode.Childrens.Find('dhEmi'), tcDatHor);
    NFe.ide.dSaiEnt := ObterConteudo(ANode.Childrens.Find('dhSaiEnt'), tcDatHor);
  end
  else
  begin
    NFe.ide.dEmi    := ObterConteudo(ANode.Childrens.Find('dEmi'), tcDat);
    NFe.ide.dSaiEnt := ObterConteudo(ANode.Childrens.Find('dSaiEnt'), tcDat);
    NFe.ide.hSaiEnt := ObterConteudo(ANode.Childrens.Find('hSaiEnt'), tcHor);
  end;
  NFe.ide.tpNF := StrToTpNF(ok, ObterConteudo(ANode.Childrens.Find('tpNF'), tcStr));

  if NFe.infNFe.Versao >= 3 then
    NFe.ide.idDest := StrToDestinoOperacao(ok, ObterConteudo(ANode.Childrens.Find('idDest'),  tcStr));

  NFe.ide.cMunFG := ObterConteudo(ANode.Childrens.Find('cMunFG'), tcInt);
  NFe.Ide.tpImp  := StrToTpImp(ok, ObterConteudo(ANode.Childrens.Find('tpImp'), tcStr));
  NFe.Ide.tpEmis := StrToTpEmis(ok, ObterConteudo(ANode.Childrens.Find('tpEmis'), tcStr));
  NFe.Ide.cDV    := ObterConteudo(ANode.Childrens.Find('cDV'), tcInt);
  NFe.Ide.tpAmb  := StrToTpAmb(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  NFe.Ide.finNFe := StrToFinNFe(ok, ObterConteudo(ANode.Childrens.Find('finNFe'), tcStr));

  if NFe.infNFe.Versao >= 3 then
  begin
    NFe.ide.indFinal := StrToConsumidorFinal(ok, ObterConteudo(ANode.Childrens.Find('indFinal'), tcStr));
    NFe.ide.indPres  := StrToPresencaComprador(ok, ObterConteudo(ANode.Childrens.Find('indPres'), tcStr));
  end;

  if NFe.infNFe.Versao >= 4 then
    NFe.ide.indIntermed := StrToIndIntermed(ok, ObterConteudo(ANode.Childrens.Find('indIntermed'), tcStr));

  NFe.Ide.procEmi := StrToProcEmi(ok, ObterConteudo(ANode.Childrens.Find('procEmi'), tcStr));
  NFe.Ide.verProc := ObterConteudo(ANode.Childrens.Find('verProc'), tcStr);
  NFe.Ide.dhCont  := ObterConteudo(ANode.Childrens.Find('dhCont'), tcDatHor);
  NFe.Ide.xJust   := ObterConteudo(ANode.Childrens.Find('xJust'), tcStr);

  ANodes := ANode.Childrens.FindAll('NFref');
  for i := 0 to Length(ANodes) - 1 do
    LerIdeNFref(ANodes[i]);
end;

procedure TNFeXmlReader.LerIdeNFref(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  refNode: TACBrXmlNode;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  NFe.Ide.NFref.New;
  i := NFe.Ide.NFref.Count - 1;
  NFe.ide.NFref[i].refNFe := ObterConteudo(ANode.Childrens.Find('refNFe'), tcEsp);
  NFe.ide.NFref[i].refNFeSig := ObterConteudo(ANode.Childrens.Find('refNFeSig'), tcEsp);

  refNode := ANode.Childrens.Find('refNF');
  if refNode <> nil then
  begin
    NFe.Ide.NFref[i].RefNF.cUF    := ObterConteudo(refNode.Childrens.Find('cUF'), tcInt);
    NFe.Ide.NFref[i].RefNF.AAMM   := ObterConteudo(refNode.Childrens.Find('AAMM'), tcEsp);
    NFe.Ide.NFref[i].RefNF.CNPJ   := ObterConteudo(refNode.Childrens.Find('CNPJ'), tcEsp);
    NFe.Ide.NFref[i].RefNF.Modelo := StrToIntDef(ObterConteudo(refNode.Childrens.Find('mod'), tcInt),55);
    NFe.ide.NFref[i].RefNF.serie  := ObterConteudo(refNode.Childrens.Find('serie'), tcInt);
    NFe.Ide.NFref[i].RefNF.nNF    := ObterConteudo(refNode.Childrens.Find('nNF'), tcInt);
  end;

  refNode := ANode.Childrens.Find('refNFP');
  if refNode <> nil then
  begin
    NFe.Ide.NFref[i].RefNFP.cUF     := ObterConteudo(refNode.Childrens.Find('cUF'), tcInt);
    NFe.Ide.NFref[i].RefNFP.AAMM    := ObterConteudo(refNode.Childrens.Find('AAMM'), tcEsp);
    NFe.Ide.NFref[i].RefNFP.CNPJCPF := ObterCNPJCPF(refNode);
    NFe.Ide.NFref[i].RefNFP.IE      := ObterConteudo(refNode.Childrens.Find('IE'), tcEsp);
    NFe.Ide.NFref[i].RefNFP.Modelo  := ObterConteudo(refNode.Childrens.Find('mod'), tcInt);
    NFe.ide.NFref[i].RefNFP.serie   := ObterConteudo(refNode.Childrens.Find('serie'), tcInt);
    NFe.Ide.NFref[i].RefNFP.nNF     := ObterConteudo(refNode.Childrens.Find('nNF'), tcInt);
  end;

  NFe.ide.NFref[i].refCTe := ObterConteudo(ANode.Childrens.Find('refCTe'), tcEsp);

  refNode := ANode.Childrens.Find('refECF');
  if refNode <> nil then
  begin
    NFe.Ide.NFref[i].RefECF.modelo := StrToECFModRef(ok,ObterConteudo(refNode.Childrens.Find('mod'), tcStr)) ;
    NFe.ide.NFref[i].RefECF.nECF := ObterConteudo(refNode.Childrens.Find('nECF'), tcStr);
    NFe.Ide.NFref[i].RefECF.nCOO := ObterConteudo(refNode.Childrens.Find('nCOO'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerEmit(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  NFe.Emit.CNPJCPF := ObterCNPJCPF(ANode);
  NFe.Emit.xNome   := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  NFe.Emit.xFant   := ObterConteudo(ANode.Childrens.Find('xFant'), tcStr);
  NFe.Emit.IE      := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
  NFe.Emit.IEST    := ObterConteudo(ANode.Childrens.Find('IEST'), tcStr);
  NFe.Emit.IM      := ObterConteudo(ANode.Childrens.Find('IM'), tcStr);
  NFe.Emit.CNAE    := ObterConteudo(ANode.Childrens.Find('CNAE'), tcStr);
  NFe.Emit.CRT     := StrToCRT(ok, ObterConteudo(ANode.Childrens.Find('CRT'), tcStr));

  LerEmitEnderEmit(ANode.Childrens.Find('enderEmit'));
end;

procedure TNFeXmlReader.LerEmitEnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.Emit.enderEmit.xLgr    := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  NFe.Emit.enderEmit.nro     := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  NFe.Emit.enderEmit.xCpl    := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  NFe.Emit.enderEmit.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  NFe.Emit.EnderEmit.cMun    := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  NFe.Emit.enderEmit.xMun    := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  NFe.Emit.enderEmit.UF      := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  NFe.Emit.enderEmit.CEP     := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  NFe.Emit.enderEmit.cPais   := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);

  if NFe.Emit.enderEmit.cPais = 0 then
    NFe.Emit.enderEmit.cPais := 1058;

  NFe.Emit.enderEmit.xPais := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);

  if NFe.Emit.enderEmit.xPais = '' then
    NFe.Emit.enderEmit.xPais := 'BRASIL';

  NFe.Emit.enderEmit.fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
end;

procedure TNFeXmlReader.LerAvulsa(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.Avulsa.CNPJ    := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NFe.Avulsa.xOrgao  := ObterConteudo(ANode.Childrens.Find('xOrgao'), tcStr);
  NFe.Avulsa.matr    := ObterConteudo(ANode.Childrens.Find('matr'), tcStr);
  NFe.Avulsa.xAgente := ObterConteudo(ANode.Childrens.Find('xAgente'), tcStr);
  NFe.Avulsa.fone    := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  NFe.Avulsa.UF      := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  NFe.Avulsa.nDAR    := ObterConteudo(ANode.Childrens.Find('nDAR'), tcStr);
  NFe.Avulsa.dEmi    := ObterConteudo(ANode.Childrens.Find('dEmi'), tcDat);
  NFe.Avulsa.vDAR    := ObterConteudo(ANode.Childrens.Find('vDAR'), tcDe2);
  NFe.Avulsa.repEmi  := ObterConteudo(ANode.Childrens.Find('repEmi'), tcStr);
  NFe.Avulsa.dPag    := ObterConteudo(ANode.Childrens.Find('dPag'), tcDat);
end;

procedure TNFeXmlReader.LerDest(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then
  begin
    NFe.Dest.indIEDest := inNaoContribuinte;
    Exit;
  end;

  NFe.Dest.CNPJCPF := ObterCNPJCPF(ANode);

  if NFe.infNFe.Versao >= 3 then
    NFe.Dest.idEstrangeiro := ObterConteudo(ANode.Childrens.Find('idEstrangeiro'), tcStr);

  NFe.Dest.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);

  if NFe.infNFe.Versao >= 3 then
    NFe.Dest.indIEDest := StrToindIEDest(Ok, ObterConteudo(ANode.Childrens.Find('indIEDest'), tcStr));

  NFe.Dest.IE := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
  NFe.Dest.ISUF := ObterConteudo(ANode.Childrens.Find('ISUF'), tcStr);

  if NFe.infNFe.Versao >= 3 then
    NFe.Dest.IM := ObterConteudo(ANode.Childrens.Find('IM'), tcStr);

  NFe.Dest.Email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);

  LerDestEnderDest(ANode.Childrens.Find('enderDest'));
end;

procedure TNFeXmlReader.LerDestEnderDest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.Dest.enderDest.xLgr    := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  NFe.Dest.enderDest.nro     := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  NFe.Dest.enderDest.xCpl    := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  NFe.Dest.enderDest.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  NFe.Dest.enderDest.cMun    := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  NFe.Dest.enderDest.xMun    := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  NFe.Dest.enderDest.UF      := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  NFe.Dest.enderDest.CEP     := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  NFe.Dest.enderDest.cPais   := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);

  if NFe.Dest.enderDest.cPais = 0 then
    NFe.Dest.enderDest.cPais := 1058;

  NFe.Dest.enderDest.xPais   := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);

  if NFe.Dest.enderDest.xPais = '' then
    NFe.Dest.enderDest.xPais := 'BRASIL';

  NFe.Dest.enderDest.fone    := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
end;

procedure TNFeXmlReader.LerRetirada(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.Retirada.CNPJCPF := ObterCNPJCPF(ANode);
  NFe.Retirada.xNome       := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  NFe.Retirada.xLgr         := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  NFe.Retirada.nro          := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  NFe.Retirada.xCpl         := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  NFe.Retirada.xBairro      := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  NFe.Retirada.cMun         := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  NFe.Retirada.xMun         := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  NFe.Retirada.UF           := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  NFe.Retirada.CEP          := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  NFe.Retirada.cPais        := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);
  NFe.Retirada.xPais        := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);
  NFe.Retirada.fone         := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  NFe.Retirada.Email        := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  NFe.Retirada.IE           := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
end;

procedure TNFeXmlReader.LerEntrega(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.Entrega.CNPJCPF := ObterCNPJCPF(ANode);
  NFe.Entrega.xNome       := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  NFe.Entrega.xLgr         := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  NFe.Entrega.nro          := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  NFe.Entrega.xCpl         := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  NFe.Entrega.xBairro      := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  NFe.Entrega.cMun         := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  NFe.Entrega.xMun         := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  NFe.Entrega.UF           := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  NFe.Entrega.CEP          := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  NFe.Entrega.cPais        := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);
  NFe.Entrega.xPais        := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);
  NFe.Entrega.fone         := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  NFe.Entrega.Email        := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  NFe.Entrega.IE           := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
end;

procedure TNFeXmlReader.LerDet(const ANode: TACBrXmlNode);
var
  Item: TDetCollectionItem;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  Item := NFe.Det.New;
     Item.prod.nItem := NFe.Det.Count;
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

  LerDetObsItem(Item, ANode.Childrens.Find('obsItem'));
end;

procedure TNFeXmlReader.LerDetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  Item.Prod.cProd := ObterConteudo(ANode.Childrens.Find('cProd'), tcStr);
  Item.Prod.cEAN  := ObterConteudo(ANode.Childrens.Find('cEAN'), tcStr);
  Item.Prod.cBarra := ObterConteudo(ANode.Childrens.Find('cBarra'), tcStr);
  Item.Prod.xProd := ObterConteudo(ANode.Childrens.Find('xProd'), tcStr);
  Item.Prod.NCM   := ObterConteudo(ANode.Childrens.Find('NCM'), tcStr);
  Item.Prod.CEST := ObterConteudo(ANode.Childrens.Find('CEST'), tcStr);

  if NFe.infNFe.Versao >= 4 then
  begin
    Item.Prod.indEscala := StrToindEscala(ok, ObterConteudo(ANode.Childrens.Find('indEscala'), tcStr));
    Item.Prod.CNPJFab   := ObterConteudo(ANode.Childrens.Find('CNPJFab'), tcStr);
    Item.Prod.cBenef    := ObterConteudo(ANode.Childrens.Find('cBenef'), tcStr);

    ANodes := ANode.Childrens.FindAll('gCred');
    for i := 0 to Length(ANodes) - 1 do
    begin
      Item.Prod.CredPresumido.New;
      Item.Prod.CredPresumido[i].cCredPresumido := ObterConteudo(ANodes[i].Childrens.FindAnyNs('cCredPresumido'), tcStr);
      Item.Prod.CredPresumido[i].pCredPresumido := ObterConteudo(ANodes[i].Childrens.FindAnyNs('pCredPresumido'), tcDe4);
      Item.Prod.CredPresumido[i].vCredPresumido := ObterConteudo(ANodes[i].Childrens.FindAnyNs('vCredPresumido'), tcDe2);
    end;
  end;

  Item.Prod.EXTIPI   := ObterConteudo(ANode.Childrens.Find('EXTIPI'), tcStr);
  Item.Prod.CFOP     := ObterConteudo(ANode.Childrens.Find('CFOP'), tcEsp);
  Item.Prod.uCom     := ObterConteudo(ANode.Childrens.Find('uCom'), tcStr);
  Item.Prod.qCom     := ObterConteudo(ANode.Childrens.Find('qCom'), tcDe4);
  Item.Prod.vUnCom  := ObterConteudo(ANode.Childrens.Find('vUnCom'), tcDe10);
  Item.Prod.vProd    := ObterConteudo(ANode.Childrens.Find('vProd'), tcDe2);
  Item.Prod.cEANTrib := ObterConteudo(ANode.Childrens.Find('cEANTrib'), tcStr);
  Item.Prod.cBarraTrib := ObterConteudo(ANode.Childrens.Find('cBarraTrib'), tcStr);
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

  LerDetProdComb(Item, ANode.Childrens.Find('comb'));
end;

procedure TNFeXmlReader.LerDetProdDI(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  DIItem: TDICollectionItem;
  AdiItem: TAdiCollectionItem;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  DIItem := Item.Prod.DI.New;
  DIItem.nDI        := ObterConteudo(ANode.Childrens.Find('nDI'), tcStr);
  DIItem.dDI        := ObterConteudo(ANode.Childrens.Find('dDI'), tcDat);
  DIItem.xLocDesemb := ObterConteudo(ANode.Childrens.Find('xLocDesemb'), tcStr);
  DIItem.UFDesemb   := ObterConteudo(ANode.Childrens.Find('UFDesemb'), tcStr);
  DIItem.dDesemb    := ObterConteudo(ANode.Childrens.Find('dDesemb'), tcDat);

  DIItem.tpViaTransp  := StrToTipoViaTransp(Ok, ObterConteudo(ANode.Childrens.Find('tpViaTransp'), tcInt));
  DIItem.vAFRMM       := ObterConteudo(ANode.Childrens.Find('vAFRMM'), tcDe2);
  DIItem.tpIntermedio := StrToTipoIntermedio(Ok, ObterConteudo(ANode.Childrens.Find('tpIntermedio'), tcInt));
  DIItem.CNPJ         := ObterCNPJCPF(ANode);
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

end;

procedure TNFeXmlReader.LerDetProdDetExport(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  DetExportItem: TdetExportCollectionItem;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  DetExportItem := Item.Prod.detExport.New;
  DetExportItem.nDraw := ObterConteudo(ANode.Childrens.Find('nDraw'), tcStr);

  AuxNode := ANode.Childrens.Find('exportInd');
  if (AuxNode <> nil) then
  begin
    DetExportItem.nRE     := ObterConteudo(AuxNode.Childrens.Find('nRE'), tcStr);
    DetExportItem.chNFe   := ObterConteudo(AuxNode.Childrens.Find('chNFe'), tcStr);
    DetExportItem.qExport := ObterConteudo(AuxNode.Childrens.Find('qExport'), tcDe4);
  end;
end;

procedure TNFeXmlReader.LerDetProdRastro(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  RastroItem: TRastroCollectionItem;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  RastroItem := Item.Prod.rastro.New;
  RastroItem.nLote  := ObterConteudo(ANode.Childrens.Find('nLote'), tcStr);
  RastroItem.qLote  := ObterConteudo(ANode.Childrens.Find('qLote'), tcDe3);
  RastroItem.dFab   := ObterConteudo(ANode.Childrens.Find('dFab'), tcDat);
  RastroItem.dVal   := ObterConteudo(ANode.Childrens.Find('dVal'), tcDat);
  RastroItem.cAgreg := ObterConteudo(ANode.Childrens.Find('cAgreg'), tcStr);
end;

procedure TNFeXmlReader.LerDetProdVeicProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  Item.Prod.veicProd.tpOP         := StrToTpOP(ok, ObterConteudo(ANode.Childrens.Find('tpOp'), tcStr));
  Item.Prod.veicProd.chassi       := ObterConteudo(ANode.Childrens.Find('chassi'), tcStr);
  Item.Prod.veicProd.cCor         := ObterConteudo(ANode.Childrens.Find('cCor'), tcStr);
  Item.Prod.veicProd.xCor         := ObterConteudo(ANode.Childrens.Find('xCor'), tcStr);
  Item.Prod.veicProd.pot          := ObterConteudo(ANode.Childrens.Find('pot'), tcStr);
  Item.Prod.veicProd.Cilin        := ObterConteudo(ANode.Childrens.Find('cilin'), tcStr);
  Item.Prod.veicProd.pesoL        := ObterConteudo(ANode.Childrens.Find('pesoL'), tcStr);
  Item.Prod.veicProd.pesoB        := ObterConteudo(ANode.Childrens.Find(''), tcStr);
  Item.Prod.veicProd.nSerie       := ObterConteudo(ANode.Childrens.Find('nSerie'), tcStr);
  Item.Prod.veicProd.tpComb       := ObterConteudo(ANode.Childrens.Find('tpComb'), tcStr);
  Item.Prod.veicProd.nMotor       := ObterConteudo(ANode.Childrens.Find('nMotor'), tcStr);
  Item.Prod.veicProd.CMT          := ObterConteudo(ANode.Childrens.Find('CMT'), tcStr);
  Item.Prod.veicProd.dist         := ObterConteudo(ANode.Childrens.Find('dist'), tcStr);
  //Item.Prod.veicProd.RENAVAM := ObterConteudo(ANode.Childrens.Find('RENAVAM'), tcEsp);
  Item.Prod.veicProd.anoMod       := ObterConteudo(ANode.Childrens.Find('anoMod'), tcInt);
  Item.Prod.veicProd.anoFab       := ObterConteudo(ANode.Childrens.Find('anoFab'), tcInt);
  Item.Prod.veicProd.tpPint       := ObterConteudo(ANode.Childrens.Find('tpPint'), tcStr);
  Item.Prod.veicProd.tpVeic       := ObterConteudo(ANode.Childrens.Find('tpVeic'), tcInt);
  Item.Prod.veicProd.espVeic      := ObterConteudo(ANode.Childrens.Find('espVeic'), tcInt);
  Item.Prod.veicProd.VIN          := ObterConteudo(ANode.Childrens.Find('VIN'), tcStr);
  Item.Prod.veicProd.condVeic     := StrToCondVeic(ok, ObterConteudo(ANode.Childrens.Find('condVeic'), tcStr));
  Item.Prod.veicProd.cMod         := ObterConteudo(ANode.Childrens.Find('cMod'), tcStr);
  Item.Prod.veicProd.cCorDENATRAN := ObterConteudo(ANode.Childrens.Find('cCorDENATRAN'), tcStr);
  Item.Prod.veicProd.lota         := ObterConteudo(ANode.Childrens.Find('lota'), tcInt);
  Item.Prod.veicProd.tpRest       := ObterConteudo(ANode.Childrens.Find('tpRest'), tcInt);
end;

procedure TNFeXmlReader.LerDetProdMed(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  MedItem: TMedCollectionItem;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  MedItem := Item.Prod.med.New;
  MedItem.cProdANVISA := ObterConteudo(ANode.Childrens.Find('cProdANVISA'), tcStr);
  MedItem.xMotivoIsencao := ObterConteudo(ANode.Childrens.Find('xMotivoIsencao'), tcStr);
  MedItem.nLote := ObterConteudo(ANode.Childrens.Find('nLote'), tcStr);
  MedItem.qLote := ObterConteudo(ANode.Childrens.Find('qLote'), tcDe3);
  MedItem.dFab  := ObterConteudo(ANode.Childrens.Find('dFab'), tcDat);
  MedItem.dVal  := ObterConteudo(ANode.Childrens.Find('dVal'), tcDat);
  MedItem.vPMC  := ObterConteudo(ANode.Childrens.Find('vPMC'), tcDe2);
end;

procedure TNFeXmlReader.LerDetProdArma(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ArmaItem: TArmaCollectionItem;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  ArmaItem := Item.Prod.arma.New;
  ArmaItem.tpArma := StrToTpArma(ok, ObterConteudo(ANode.Childrens.Find('tpArma'), tcStr));
  ArmaItem.nSerie := ObterConteudo(ANode.Childrens.Find('nSerie'), tcStr);
  ArmaItem.nCano  := ObterConteudo(ANode.Childrens.Find('nCano'), tcStr);
  ArmaItem.descr  := ObterConteudo(ANode.Childrens.Find('descr'), tcStr);
end;

procedure TNFeXmlReader.LerDetProdComb(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  Ok: Boolean;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  Item.Prod.comb.cProdANP := ObterConteudo(ANode.Childrens.Find('cProdANP'), tcInt);
  Item.Prod.comb.pMixGN  := ObterConteudo(ANode.Childrens.Find('qMixGN'), tcDe4);
  Item.Prod.comb.descANP  := ObterConteudo(ANode.Childrens.Find('descANP'), tcStr);
  Item.Prod.comb.pGLP    := ObterConteudo(ANode.Childrens.Find('pGLP'), tcDe4);
  Item.Prod.comb.pGNn    := ObterConteudo(ANode.Childrens.Find('pGNn'), tcDe4);
  Item.Prod.comb.pGNi    := ObterConteudo(ANode.Childrens.Find('pGNi'), tcDe4);
  Item.Prod.comb.vPart   := ObterConteudo(ANode.Childrens.Find('vPart'), tcDe2);
  Item.Prod.comb.CODIF    := ObterConteudo(ANode.Childrens.Find('CODIF'), tcEsp);
  Item.Prod.comb.qTemp    := ObterConteudo(ANode.Childrens.Find('qTemp'), tcDe4);
  Item.Prod.comb.UFcons   := ObterConteudo(ANode.Childrens.Find('UFCons'), tcStr);
  Item.Prod.comb.ICMSCons.UFcons := ObterConteudo(ANode.Childrens.Find('UFcons'), tcStr);

  AuxNode := ANode.Childrens.Find('CIDE');
  if (AuxNode <> nil) then
  begin
    Item.Prod.comb.CIDE.qBCprod   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    Item.Prod.comb.CIDE.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    Item.Prod.comb.CIDE.vCIDE     := ObterConteudo(AuxNode.Childrens.Find('vCIDE'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('encerrante');
  if (AuxNode <> nil) then
  begin
    Item.Prod.comb.encerrante.nBico   := ObterConteudo(AuxNode.Childrens.Find('nBico'), tcInt);
    Item.Prod.comb.encerrante.nBomba  := ObterConteudo(AuxNode.Childrens.Find('nBomba'), tcInt);
    Item.Prod.comb.encerrante.nTanque := ObterConteudo(AuxNode.Childrens.Find('nTanque'), tcInt);
    Item.Prod.comb.encerrante.vEncIni := ObterConteudo(AuxNode.Childrens.Find('vEncIni'), tcDe3);
    Item.Prod.comb.encerrante.vEncFin := ObterConteudo(AuxNode.Childrens.Find('vEncFin'), tcDe3);
  end;

  Item.Prod.comb.pBio := ObterConteudo(ANode.Childrens.Find('pBio'), tcDe4);

  ANodes := ANode.Childrens.FindAll('origComb');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Item.Prod.comb.origComb.New;
    Item.Prod.comb.origComb[i].indImport := StrToindImport(Ok, ObterConteudo(ANodes[i].Childrens.Find('indImport'), tcStr));
    Item.Prod.comb.origComb[i].cUFOrig := ObterConteudo(ANodes[i].Childrens.Find('cUFOrig'), tcInt);
    Item.Prod.comb.origComb[i].pOrig := ObterConteudo(ANodes[i].Childrens.Find('pOrig'), tcDe4);
  end;

  AuxNode := ANode.Childrens.Find('ICMSComb');
  if (AuxNode <> nil) then
  begin
    Item.Prod.comb.ICMS.vBCICMS   := ObterConteudo(AuxNode.Childrens.Find('vBCICMS'), tcDe2);
    Item.Prod.comb.ICMS.vICMS     := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    Item.Prod.comb.ICMS.vBCICMSST := ObterConteudo(AuxNode.Childrens.Find('vBCICMSST'), tcDe2);
    Item.Prod.comb.ICMS.vICMSST   := ObterConteudo(AuxNode.Childrens.Find('vICMSST'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ICMSInter');
  if (AuxNode <> nil) then
  begin
    Item.Prod.comb.ICMSInter.vBCICMSSTDest := ObterConteudo(AuxNode.Childrens.Find('vBCICMSSTDest'), tcDe2);
    Item.Prod.comb.ICMSInter.vICMSSTDest   := ObterConteudo(AuxNode.Childrens.Find('vICMSSTDest'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ICMSCons');
  if (AuxNode <> nil) then
  begin
    Item.Prod.comb.ICMSCons.vBCICMSSTCons := ObterConteudo(AuxNode.Childrens.Find('vBCICMSSTCons'), tcDe2);
    Item.Prod.comb.ICMSCons.vICMSSTCons   := ObterConteudo(AuxNode.Childrens.Find('vICMSSTCons'), tcDe2);
    Item.Prod.comb.ICMSCons.UFcons        := ObterConteudo(AuxNode.Childrens.Find('UFCons'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerDetImposto(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode, AxNode: TACBrXmlNode;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

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
    Item.Imposto.ICMS.vICMSSTDeson := ObterConteudo(AuxNode.Childrens.Find('vICMSSTDeson'), tcDe2);
    Item.Imposto.ICMS.motDesICMSST := StrTomotDesICMS(ok, ObterConteudo(AuxNode.Childrens.Find('motDesICMSST'), tcStr));
    Item.Imposto.ICMS.pFCPDif     := ObterConteudo(AuxNode.Childrens.Find('pFCPDif'), tcDe4);
    Item.Imposto.ICMS.vFCPDif     := ObterConteudo(AuxNode.Childrens.Find('vFCPDif'), tcDe2);
    Item.Imposto.ICMS.vFCPEfet    := ObterConteudo(AuxNode.Childrens.Find('vFCPEfet'), tcDe2);

    Item.Imposto.ICMS.adRemICMS := ObterConteudo(AuxNode.Childrens.Find('adRemICMS'), tcDe4);
    Item.Imposto.ICMS.vICMSMono := ObterConteudo(AuxNode.Childrens.Find('vICMSMono'), tcDe2);
    Item.Imposto.ICMS.adRemICMSReten := ObterConteudo(AuxNode.Childrens.Find('adRemICMSReten'), tcDe4);
    Item.Imposto.ICMS.vICMSMonoReten := ObterConteudo(AuxNode.Childrens.Find('vICMSMonoReten'), tcDe2);
    Item.Imposto.ICMS.vICMSMonoDif := ObterConteudo(AuxNode.Childrens.Find('vICMSMonoDif'), tcDe2);
    Item.Imposto.ICMS.adRemICMSRet := ObterConteudo(AuxNode.Childrens.Find('adRemICMSRet'), tcDe4);
    Item.Imposto.ICMS.vICMSMonoRet := ObterConteudo(AuxNode.Childrens.Find('vICMSMonoRet'), tcDe2);

    Item.Imposto.ICMS.qBCMono := ObterConteudo(AuxNode.Childrens.Find('qBCMono'), tcDe2);
    Item.Imposto.ICMS.qBCMonoReten := ObterConteudo(AuxNode.Childrens.Find('qBCMonoReten'), tcDe2);
    Item.Imposto.ICMS.pRedAdRem := ObterConteudo(AuxNode.Childrens.Find('pRedAdRem'), tcDe2);

    if Item.Imposto.ICMS.pRedAdRem <> 0 then
      Item.Imposto.ICMS.motRedAdRem := StrTomotRedAdRem(ok, ObterConteudo(AuxNode.Childrens.Find('motRedAdRem'), tcStr));

    Item.Imposto.ICMS.qBCMonoRet := ObterConteudo(AuxNode.Childrens.Find('qBCMonoRet'), tcDe2);
    Item.Imposto.ICMS.vICMSMonoOp := ObterConteudo(AuxNode.Childrens.Find('vICMSMonoOp'), tcDe2);
    Item.Imposto.ICMS.indDeduzDeson := StrToTIndicador(ok, ObterConteudo(AuxNode.Childrens.Find('indDeduzDeson'), tcStr));
    Item.Imposto.ICMS.cBenefRBC := ObterConteudo(AuxNode.Childrens.Find('cBenefRBC'), tcStr);

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
    if NFe.ide.tpNF = tnEntrada then
      Item.Imposto.IPI.CST := ipi53;
    if NFe.ide.tpNF = tnSaida then
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
    Item.Imposto.PISST.indSomaPISST := StrToindSomaPISST(ok, ObterConteudo(AuxNode.Childrens.Find('indSomaPISST'), tcStr));
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
    Item.Imposto.COFINSST.indSomaCOFINSST := StrToindSomaCOFINSST(Ok, ObterConteudo(AuxNode.Childrens.Find('indSomaCOFINSST'), tcStr));
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
end;

procedure TNFeXmlReader.LerDetObsItem(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Atr: TACBrXmlAttribute;
begin
  if not Assigned(Item) then Exit;
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.Find('obsCont');

  if (AuxNode <> nil) then
  begin
    Atr := AuxNode.Attributes.Items['xCampo'];

    if Atr <> nil then
      Item.obsCont.xCampo := Atr.Content;

    Item.obsCont.xTexto := ObterConteudo(AuxNode.Childrens.Find('xTexto'), tcStr);
  end;

  AuxNode := ANode.Childrens.Find('obsFisco');

  if (AuxNode <> nil) then
  begin
    Atr := AuxNode.Attributes.Items['xCampo'];

    if Atr <> nil then
      Item.obsFisco.xCampo := Atr.Content;

    Item.obsFisco.xTexto := ObterConteudo(AuxNode.Childrens.Find('xTexto'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerTotal(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.Find('ICMSTot');
  if (AuxNode <> nil) then
  begin
    NFe.Total.ICMSTot.vBC           := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    NFe.Total.ICMSTot.vICMS         := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    NFe.Total.ICMSTot.vICMSDeson   := ObterConteudo(AuxNode.Childrens.Find('vICMSDeson'), tcDe2);
    NFe.Total.ICMSTot.vFCPUFDest   := ObterConteudo(AuxNode.Childrens.Find('vFCPUFDest'), tcDe2);
    NFe.Total.ICMSTot.vICMSUFDest  := ObterConteudo(AuxNode.Childrens.Find('vICMSUFDest'), tcDe2);
    NFe.Total.ICMSTot.vICMSUFRemet := ObterConteudo(AuxNode.Childrens.Find('vICMSUFRemet'), tcDe2);
    NFe.Total.ICMSTot.vFCP         := ObterConteudo(AuxNode.Childrens.Find('vFCP'), tcDe2);
    NFe.Total.ICMSTot.vBCST         := ObterConteudo(AuxNode.Childrens.Find('vBCST'), tcDe2);
    NFe.Total.ICMSTot.vST           := ObterConteudo(AuxNode.Childrens.Find('vST'), tcDe2);
    NFe.Total.ICMSTot.vFCPST       := ObterConteudo(AuxNode.Childrens.Find('vFCPST'), tcDe2);
    NFe.Total.ICMSTot.vFCPSTRet    := ObterConteudo(AuxNode.Childrens.Find('vFCPSTRet'), tcDe2);

    NFe.Total.ICMSTot.qBCMono := ObterConteudo(AuxNode.Childrens.Find('qBCMono'), tcDe2);
    NFe.Total.ICMSTot.vICMSMono := ObterConteudo(AuxNode.Childrens.Find('vICMSMono'), tcDe2);
    NFe.Total.ICMSTot.qBCMonoReten := ObterConteudo(AuxNode.Childrens.Find('qBCMonoReten'), tcDe2);
    NFe.Total.ICMSTot.vICMSMonoReten := ObterConteudo(AuxNode.Childrens.Find('vICMSMonoReten'), tcDe2);
    NFe.Total.ICMSTot.qBCMonoRet := ObterConteudo(AuxNode.Childrens.Find('qBCMonoRet'), tcDe2);
    NFe.Total.ICMSTot.vICMSMonoRet := ObterConteudo(AuxNode.Childrens.Find('vICMSMonoRet'), tcDe2);

    NFe.Total.ICMSTot.vProd         := ObterConteudo(AuxNode.Childrens.Find('vProd'), tcDe2);
    NFe.Total.ICMSTot.vFrete        := ObterConteudo(AuxNode.Childrens.Find('vFrete'), tcDe2);
    NFe.Total.ICMSTot.vSeg          := ObterConteudo(AuxNode.Childrens.Find('vSeg'), tcDe2);
    NFe.Total.ICMSTot.vDesc         := ObterConteudo(AuxNode.Childrens.Find('vDesc'), tcDe2);
    NFe.Total.ICMSTot.vII           := ObterConteudo(AuxNode.Childrens.Find('vII'), tcDe2);
    NFe.Total.ICMSTot.vIPI          := ObterConteudo(AuxNode.Childrens.Find('vIPI'), tcDe2);
    NFe.Total.ICMSTot.vIPIDevol    := ObterConteudo(AuxNode.Childrens.Find('vIPIDevol'), tcDe2);
    NFe.Total.ICMSTot.vPIS          := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    NFe.Total.ICMSTot.vCOFINS       := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
    NFe.Total.ICMSTot.vOutro        := ObterConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
    NFe.Total.ICMSTot.vNF           := ObterConteudo(AuxNode.Childrens.Find('vNF'), tcDe2);
    NFe.Total.ICMSTot.vTotTrib     := ObterConteudo(AuxNode.Childrens.Find('vTotTrib'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ISSQNtot');
  if (AuxNode <> nil) then
  begin
    NFe.Total.ISSQNtot.vServ   := ObterConteudo(AuxNode.Childrens.Find('vServ'), tcDe2);
    NFe.Total.ISSQNtot.vBC     := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    NFe.Total.ISSQNtot.vISS    := ObterConteudo(AuxNode.Childrens.Find('vISS'), tcDe2);
    NFe.Total.ISSQNtot.vPIS    := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    NFe.Total.ISSQNtot.vCOFINS := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);

    if NFe.infNFe.Versao >= 3 then
    begin
      NFe.Total.ISSQNtot.dCompet     := ObterConteudo(AuxNode.Childrens.Find('dCompet'), tcDat);
      NFe.Total.ISSQNtot.vDeducao    := ObterConteudo(AuxNode.Childrens.Find('vDeducao'), tcDe2);
      NFe.Total.ISSQNtot.vOutro      := ObterConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
      NFe.Total.ISSQNtot.vDescIncond := ObterConteudo(AuxNode.Childrens.Find('vDescIncond'), tcDe2);
      NFe.Total.ISSQNtot.vDescCond   := ObterConteudo(AuxNode.Childrens.Find('vDescCond'), tcDe2);
      NFe.Total.ISSQNtot.vISSRet     := ObterConteudo(AuxNode.Childrens.Find('vISSRet'), tcDe2);
      NFe.Total.ISSQNtot.cRegTrib    := StrToRegTribISSQN(Ok, ObterConteudo(AuxNode.Childrens.Find('cRegTrib'), tcStr));
    end;
  end;

  AuxNode := ANode.Childrens.Find('retTrib');
  if (AuxNode <> nil) then
  begin
    NFe.Total.retTrib.vRetPIS    := ObterConteudo(AuxNode.Childrens.Find('vRetPIS'), tcDe2);
    NFe.Total.retTrib.vRetCOFINS := ObterConteudo(AuxNode.Childrens.Find('vRetCOFINS'), tcDe2);
    NFe.Total.retTrib.vRetCSLL   := ObterConteudo(AuxNode.Childrens.Find('vRetCSLL'), tcDe2);
    NFe.Total.retTrib.vBCIRRF    := ObterConteudo(AuxNode.Childrens.Find('vBCIRRF'), tcDe2);
    NFe.Total.retTrib.vIRRF      := ObterConteudo(AuxNode.Childrens.Find('vIRRF'), tcDe2);
    NFe.Total.retTrib.vBCRetPrev := ObterConteudo(AuxNode.Childrens.Find('vBCRetPrev'), tcDe2);
    NFe.Total.retTrib.vRetPrev   := ObterConteudo(AuxNode.Childrens.Find('vRetPrev'), tcDe2);
  end;
end;

procedure TNFeXmlReader.LerTransp(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  NFe.Transp.modFrete := StrToModFrete(ok, ObterConteudo(ANode.Childrens.Find('modFrete'), tcStr));
  NFe.Transp.vagao   := ObterConteudo(ANode.Childrens.Find('vagao'), tcStr);
  NFe.Transp.balsa   := ObterConteudo(ANode.Childrens.Find('balsa'), tcStr);

  AuxNode := ANode.Childrens.Find('transporta');
  if (AuxNode <> nil) then
  begin
    NFe.Transp.Transporta.CNPJCPF := ObterCNPJCPF(AuxNode);
    NFe.Transp.Transporta.xNome       := ObterConteudo(AuxNode.Childrens.Find('xNome'), tcStr);
    NFe.Transp.Transporta.IE          := ObterConteudo(AuxNode.Childrens.Find('IE'), tcStr);
    NFe.Transp.Transporta.xEnder      := ObterConteudo(AuxNode.Childrens.Find('xEnder'), tcStr);
    NFe.Transp.Transporta.xMun        := ObterConteudo(AuxNode.Childrens.Find('xMun'), tcStr);
    NFe.Transp.Transporta.UF          := ObterConteudo(AuxNode.Childrens.Find('UF'), tcStr);
  end;

  AuxNode := ANode.Childrens.Find('retTransp');
  if (AuxNode <> nil) then
  begin
    NFe.Transp.retTransp.vServ    := ObterConteudo(AuxNode.Childrens.Find('vServ'), tcDe2);
    NFe.Transp.retTransp.vBCRet   := ObterConteudo(AuxNode.Childrens.Find('vBCRet'), tcDe2);
    NFe.Transp.retTransp.pICMSRet := ObterConteudo(AuxNode.Childrens.Find('pICMSRet'), tcDe2);
    NFe.Transp.retTransp.vICMSRet := ObterConteudo(AuxNode.Childrens.Find('vICMSRet'), tcDe2);
    NFe.Transp.retTransp.CFOP     := ObterConteudo(AuxNode.Childrens.Find('CFOP'), tcEsp);
    NFe.Transp.retTransp.cMunFG   := ObterConteudo(AuxNode.Childrens.Find('cMunFG'), tcStr);
  end;

  AuxNode := ANode.Childrens.Find('veicTransp');
  if (AuxNode <> nil) then
  begin
    NFe.Transp.veicTransp.placa := ObterConteudo(AuxNode.Childrens.Find('placa'), tcStr);
    NFe.Transp.veicTransp.UF    := ObterConteudo(AuxNode.Childrens.Find('UF'), tcStr);
    NFe.Transp.veicTransp.RNTC  := ObterConteudo(AuxNode.Childrens.Find('RNTC'), tcStr);
  end;

  NFe.Transp.Reboque.Clear;
  ANodes := ANode.Childrens.FindAll('reboque');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.Transp.Reboque.New;
    NFe.Transp.Reboque[i].placa := ObterConteudo(ANodes[i].Childrens.Find('placa'), tcStr);
    NFe.Transp.Reboque[i].UF    := ObterConteudo(ANodes[i].Childrens.Find('UF'), tcStr);
    NFe.Transp.Reboque[i].RNTC  := ObterConteudo(ANodes[i].Childrens.Find('RNTC'), tcStr);
  end;

  NFe.Transp.Vol.Clear;
  ANodes := ANode.Childrens.FindAll('vol');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerTranspVol(ANodes[i]);
  end;
end;

procedure TNFeXmlReader.LerTranspVol(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  Item: TVolCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := NFe.Transp.Vol.New;
  Item.qVol  := ObterConteudo(ANode.Childrens.Find('qVol'), tcInt);
  Item.esp   := ObterConteudo(ANode.Childrens.Find('esp'), tcStr);
  Item.marca := ObterConteudo(ANode.Childrens.Find('marca'), tcStr);
  Item.nVol  := ObterConteudo(ANode.Childrens.Find('nVol'), tcStr);
  Item.pesoL := ObterConteudo(ANode.Childrens.Find('pesoL'), tcDe3);
  Item.pesoB := ObterConteudo(ANode.Childrens.Find('pesoB'), tcDe3);

  Item.lacres.Clear;
  ANodes := ANode.Childrens.FindAll('lacres');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Item.lacres.New;
    Item.lacres[i].nLacre := ObterConteudo(ANodes[i].Childrens.Find('nLacre'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerCobr(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  FatNode, AuxNode: TACBrXmlNode;
  tagPag: String;
begin
  if not Assigned(ANode) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('cobr');

  if Assigned(AuxNode) then
  begin
    FatNode := AuxNode.Childrens.FindAnyNs('fat');
    if (FatNode <> nil) then
    begin
      NFe.Cobr.Fat.nFat  := ObterConteudo(FatNode.Childrens.Find('nFat'), tcStr);
      NFe.Cobr.Fat.vOrig := ObterConteudo(FatNode.Childrens.Find('vOrig'), tcDe2);
      NFe.Cobr.Fat.vDesc := ObterConteudo(FatNode.Childrens.Find('vDesc'), tcDe2);
      NFe.Cobr.Fat.vLiq  := ObterConteudo(FatNode.Childrens.Find('vLiq'), tcDe2);
    end;

    NFe.Cobr.Dup.Clear;
    ANodes := AuxNode.Childrens.FindAll('dup');
    for i := 0 to Length(ANodes) - 1 do
    begin
      NFe.Cobr.Dup.New;
      NFe.Cobr.Dup[i].nDup  := ObterConteudo(ANodes[i].Childrens.Find('nDup'), tcStr);
      NFe.Cobr.Dup[i].dVenc := ObterConteudo(ANodes[i].Childrens.Find('dVenc'), tcDat);
      NFe.Cobr.Dup[i].vDup  := ObterConteudo(ANodes[i].Childrens.Find('vDup'), tcDe2);
    end;
  end;

  if NFe.infNFe.Versao >= 3 then
  begin
    NFe.pag.Clear;
    if NFe.infNFe.Versao >= 4 then
    begin
      AuxNode := ANode.Childrens.FindAnyNs('pag');
      if (AuxNode <> nil) then
        NFe.pag.vTroco := ObterConteudo(AuxNode.Childrens.Find('vTroco'), tcDe2);

      tagPag := 'detPag';
    end
    else
      tagPag := 'pag';
  end;

  ANodes := AuxNode.Childrens.FindAllAnyNs(tagPag);
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.pag.New;
    NFe.pag[i].indPag := StrToIndpag(Ok, ObterConteudo(ANodes[i].Childrens.Find('indPag'), tcStr));
    NFe.pag[i].tPag := StrToFormaPagamento(ok, ObterConteudo(ANodes[i].Childrens.Find('tPag'), tcStr));
    NFe.pag[i].xPag := ObterConteudo(ANodes[i].Childrens.Find('xPag'), tcStr);
    NFe.pag[i].vPag := ObterConteudo(ANodes[i].Childrens.Find('vPag'), tcDe2);
    NFe.pag[i].dPag := ObterConteudo(ANodes[i].Childrens.Find('dPag'), tcDat);

    NFe.pag[i].CNPJPag := ObterConteudo(ANodes[i].Childrens.Find('CNPJPag'), tcStr);
    NFe.pag[i].UFPag := ObterConteudo(ANodes[i].Childrens.Find('UFPag'), tcStr);

    AuxNode := ANode.Childrens.Find('card');
    if (AuxNode <> nil) then
    begin
      NFe.pag[i].tpIntegra := StrTotpIntegra(ok, ObterConteudo(AuxNode.Childrens.Find('tpIntegra'), tcStr));
      NFe.pag[i].CNPJ  := ObterConteudo(AuxNode.Childrens.Find('CNPJ'), tcStr);
      NFe.pag[i].tBand := StrToBandeiraCartao(ok, ObterConteudo(AuxNode.Childrens.Find('tBand'), tcStr));
      NFe.pag[i].cAut  := ObterConteudo(AuxNode.Childrens.Find('cAut'), tcStr);

      NFe.pag[i].CNPJReceb := ObterConteudo(AuxNode.Childrens.Find('CNPJReceb'), tcStr);
      NFe.pag[i].idTermPag := ObterConteudo(AuxNode.Childrens.Find('idTermPag'), tcStr);
    end;
  end;
end;

procedure TNFeXmlReader.LerInfAdic(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  NFe.InfAdic.infAdFisco := ObterConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);
  NFe.InfAdic.infCpl     := ObterConteudo(ANode.Childrens.Find('infCpl'), tcStr);

  NFe.InfAdic.obsCont.Clear;
  ANodes := ANode.Childrens.FindAll('obsCont');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.InfAdic.obsCont.New;
    NFe.InfAdic.obsCont[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
    NFe.InfAdic.obsCont[i].xTexto := ObterConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
  end;

  NFe.InfAdic.obsFisco.Clear;
  ANodes := ANode.Childrens.FindAll('obsFisco');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.InfAdic.obsFisco.New;
    NFe.InfAdic.obsFisco[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
    NFe.InfAdic.obsFisco[i].xTexto := ObterConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
  end;

  NFe.InfAdic.procRef.Clear;
  ANodes := ANode.Childrens.FindAll('procRef');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.InfAdic.obsCont.New;
    NFe.InfAdic.procRef[i].nProc := ObterConteudo(ANodes[i].Childrens.Find('nProc'),tcStr);
    NFe.InfAdic.procRef[i].indProc := StrToIndProc(ok, ObterConteudo(ANodes[i].Childrens.Find('indProc'), tcStr));
    NFe.InfAdic.procRef[i].tpAto := StrTotpAto(ok, ObterConteudo(ANodes[i].Childrens.Find('tpAto'), tcStr));
  end;
end;

procedure TNFeXmlReader.LerInfIntermed(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.infIntermed.CNPJ         := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NFe.infIntermed.idCadIntTran := ObterConteudo(ANode.Childrens.Find('idCadIntTran'), tcStr);
end;

procedure TNFeXmlReader.LerExporta(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.exporta.UFembarq   := ObterConteudo(ANode.Childrens.Find('UFEmbarq'), tcStr);
  NFe.exporta.xLocEmbarq := ObterConteudo(ANode.Childrens.Find('xLocEmbarq'), tcStr);

  // Versao 3.10
  NFe.exporta.UFSaidaPais  := ObterConteudo(ANode.Childrens.Find('UFSaidaPais'), tcStr);
  NFe.exporta.xLocExporta  := ObterConteudo(ANode.Childrens.Find('xLocExporta'), tcStr);
  NFe.exporta.xLocDespacho := ObterConteudo(ANode.Childrens.Find('xLocDespacho'), tcStr);
end;

procedure TNFeXmlReader.LerCompra(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.compra.xNEmp := ObterConteudo(ANode.Childrens.Find('xNEmp'), tcStr);
  NFe.compra.xPed  := ObterConteudo(ANode.Childrens.Find('xPed'), tcStr);
  NFe.compra.xCont := ObterConteudo(ANode.Childrens.Find('xCont'), tcStr);
end;

procedure TNFeXmlReader.LerCana(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  NFe.cana.safra   := ObterConteudo(ANode.Childrens.Find('safra'), tcStr);
  NFe.cana.ref     := ObterConteudo(ANode.Childrens.Find('ref'), tcStr);
  NFe.cana.qTotMes := ObterConteudo(ANode.Childrens.Find('qTotMes'), tcDe10);
  NFe.cana.qTotAnt := ObterConteudo(ANode.Childrens.Find('qTotAnt'), tcDe10);
  NFe.cana.qTotGer := ObterConteudo(ANode.Childrens.Find('qTotGer'), tcDe10);
  NFe.cana.vFor    := ObterConteudo(ANode.Childrens.Find('vFor'), tcDe2);
  NFe.cana.vTotDed := ObterConteudo(ANode.Childrens.Find('vTotDed'), tcDe2);
  NFe.cana.vLiqFor := ObterConteudo(ANode.Childrens.Find('vLiqFor'), tcDe2);

  NFe.cana.fordia.Clear;
  ANodes := ANode.Childrens.FindAll('forDia');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.cana.fordia.New;
    NFe.cana.fordia[i].dia  := StrToInt(ANodes[i].Attributes.Items['dia'].Content);
    NFe.cana.fordia[i].qtde := ObterConteudo(ANodes[i].Childrens.Find('qtde'), tcDe10);
  end;

  NFe.cana.deduc.Clear;
  ANodes := ANode.Childrens.FindAll('deduc');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.cana.deduc.New;
    NFe.cana.deduc[i].xDed  := ObterConteudo(ANodes[i].Childrens.Find('xDed'), tcStr);
    NFe.cana.deduc[i].vDed := ObterConteudo(ANodes[i].Childrens.Find('vDed'), tcDe2);
  end;
end;

procedure TNFeXmlReader.LerInfRespTec(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.infRespTec.CNPJ     := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NFe.infRespTec.xContato := ObterConteudo(ANode.Childrens.Find('xContato'), tcStr);
  NFe.infRespTec.email    := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  NFe.infRespTec.fone     := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  NFe.infRespTec.idCSRT   := ObterConteudo(ANode.Childrens.Find('idCSRT'), tcInt);
  NFe.infRespTec.hashCSRT := ObterConteudo(ANode.Childrens.Find('hashCSRT'), tcStr);
end;

procedure TNFeXmlReader.LerInfNFeSupl(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  NFe.infNFeSupl.qrCode := ObterConteudo(ANode.Childrens.Find('qrCode'), tcStr);
  NFe.infNFeSupl.qrCode := StringReplace(NFe.infNFeSupl.qrCode, '<![CDATA[', '', []);
  NFe.infNFeSupl.qrCode := StringReplace(NFe.infNFeSupl.qrCode, ']]>', '', []);
  NFe.infNFeSupl.urlChave := ObterConteudo(ANode.Childrens.Find('urlChave'), tcStr);
end;

end.

