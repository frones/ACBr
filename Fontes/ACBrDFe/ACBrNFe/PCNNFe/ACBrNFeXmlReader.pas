{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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
    procedure LerSignature(const ANode: TACBrXmlNode);

  public
    constructor Create(AOwner: TNFe); reintroduce;

    function LerXml: Boolean; override;

    property NFe: TNFe read FNFe write FNFe;

  end;

implementation

uses
  ACBrUtil.Base, ACBrXmlBase,
  pcnConversao,
  pcnConversaoNFe;

{ TNFeXmlReader }
constructor TNFeXmlReader.Create(AOwner: TNFe);
begin
  inherited Create;

  FNFe := AOwner;
end;

function TNFeXmlReader.LerXml: Boolean;
Var
  NFeNode, infNFeNode: TACBrXmlNode;
  att: TACBrXmlAttribute;
begin
  if not Assigned(FNFe) or (FNFe = nil) then
    raise Exception.Create('Destino não informado, informe a classe [TNFe] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml da Nfe não carregado.');

  Result := False;
  infNFeNode := nil;
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
  LerSignature(NFeNode.Childrens.Find('Signature'));

  Result := True;
end;

procedure TNFeXmlReader.LerProtNFe(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFe.procNFe.tpAmb    := StrToTpAmb(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  NFe.procNFe.verAplic := ObterConteudo(ANode.Childrens.Find('verAplic'), tcStr);
  NFe.procNFe.chNFe    := ObterConteudo(ANode.Childrens.Find('chNFe'), tcStr);
  NFe.procNFe.dhRecbto := ObterConteudo(ANode.Childrens.Find('dhRecbto'), tcDatHor);
  NFe.procNFe.nProt    := ObterConteudo(ANode.Childrens.Find('nProt'), tcStr);
  NFe.procNFe.digVal   := ObterConteudo(ANode.Childrens.Find('digVal'), tcStr);
  NFe.procNFe.cStat    := ObterConteudo(ANode.Childrens.Find('cStat'), tcInt);
  NFe.procNFe.xMotivo  := ObterConteudo(ANode.Childrens.Find('xMotivo'), tcStr);
  NFe.procNFe.cMsg     := ObterConteudo(ANode.Childrens.Find('cMsg'), tcInt);
  NFe.procNFe.xMsg     := ObterConteudo(ANode.Childrens.Find('xMsg'), tcStr);
end;

procedure TNFeXmlReader.LerInfNFe(const ANode: TACBrXmlNode);
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
  LerCobr(ANode.Childrens.Find('cobr'));
  LerInfIntermed(ANode.Childrens.Find('infIntermed'));
  LerInfAdic(ANode.Childrens.Find('infAdic'));
  LerExporta(ANode.Childrens.Find('exporta'));
  LerCompra(ANode.Childrens.Find('compra'));
  LerCana(ANode.Childrens.Find('cana'));
  LerInfRespTec(ANode.Childrens.Find('infRespTec'));
end;

procedure TNFeXmlReader.LerIde(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*B02*) NFe.ide.cUF := ObterConteudo(ANode.Childrens.Find('cUF'), tcInt);
  (*B03*) NFe.ide.cNF := ObterConteudo(ANode.Childrens.Find('cNF'), tcInt);
  if NFe.ide.cNF = 0 then
    NFe.ide.cNF := -2;
  (*B04*) NFe.ide.natOp  := ObterConteudo(ANode.Childrens.Find('natOp'), tcStr);
  (*B05*) NFe.ide.indPag := StrToIndpag(ok, ObterConteudo(ANode.Childrens.Find('indPag'), tcStr));
  (*B06*) NFe.ide.modelo := ObterConteudo(ANode.Childrens.Find('mod'), tcInt);
  (*B07*) NFe.ide.serie  := ObterConteudo(ANode.Childrens.Find('serie'), tcInt);
  (*B08*) NFe.ide.nNF    := ObterConteudo(ANode.Childrens.Find('nNF'), tcInt);

  if NFe.infNFe.Versao >= 3 then
  begin
    (*B09*) NFe.ide.dEmi    := ObterConteudo(ANode.Childrens.Find('dhEmi'), tcDatHor);
    (*B10*) NFe.ide.dSaiEnt := ObterConteudo(ANode.Childrens.Find('dhSaiEnt'), tcDatHor);
  end
  else
  begin
    (*B09*) NFe.ide.dEmi    := ObterConteudo(ANode.Childrens.Find('dEmi'), tcDat);
    (*B10*) NFe.ide.dSaiEnt := ObterConteudo(ANode.Childrens.Find('dSaiEnt'), tcDat);
    (*B10a*)NFe.ide.hSaiEnt := ObterConteudo(ANode.Childrens.Find('hSaiEnt'), tcHor);
  end;
  (*B11*) NFe.ide.tpNF := StrToTpNF(ok, ObterConteudo(ANode.Childrens.Find('tpNF'), tcStr));

  if NFe.infNFe.Versao >= 3 then
    (*B11a*)NFe.ide.idDest := StrToDestinoOperacao(ok, ObterConteudo(ANode.Childrens.Find('idDest'),  tcStr));

  (*B12*) NFe.ide.cMunFG := ObterConteudo(ANode.Childrens.Find('cMunFG'), tcInt);
  (*B21*) NFe.Ide.tpImp  := StrToTpImp(ok, ObterConteudo(ANode.Childrens.Find('tpImp'), tcStr));
  (*B22*) NFe.Ide.tpEmis := StrToTpEmis(ok, ObterConteudo(ANode.Childrens.Find('tpEmis'), tcStr));
  (*B23*) NFe.Ide.cDV    := ObterConteudo(ANode.Childrens.Find('cDV'), tcInt);
  (*B24*) NFe.Ide.tpAmb  := StrToTpAmb(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  (*B25*) NFe.Ide.finNFe := StrToFinNFe(ok, ObterConteudo(ANode.Childrens.Find('finNFe'), tcStr));

  if NFe.infNFe.Versao >= 3 then
  begin
    (*B25a*)NFe.ide.indFinal := StrToConsumidorFinal(ok, ObterConteudo(ANode.Childrens.Find('indFinal'), tcStr));
    (*B25b*)NFe.ide.indPres  := StrToPresencaComprador(ok, ObterConteudo(ANode.Childrens.Find('indPres'), tcStr));
  end;

  if NFe.infNFe.Versao >= 4 then
    NFe.ide.indIntermed := StrToIndIntermed(ok, ObterConteudo(ANode.Childrens.Find('indIntermed'), tcStr));

  (*B26*) NFe.Ide.procEmi := StrToProcEmi(ok, ObterConteudo(ANode.Childrens.Find('procEmi'), tcStr));
  (*B27*) NFe.Ide.verProc := ObterConteudo(ANode.Childrens.Find('verProc'), tcStr);
  (*B28*) NFe.Ide.dhCont  := ObterConteudo(ANode.Childrens.Find('dhCont'), tcDatHor);
  (*B29*) NFe.Ide.xJust   := ObterConteudo(ANode.Childrens.Find('xJust'), tcStr);

  ANodes := ANode.Childrens.FindAll('NFref');
  for i := 0 to Length(ANodes) - 1 do
    LerIdeNFref(ANodes[i]);
end;

procedure TNFeXmlReader.LerIdeNFref(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  refNode: TACBrXmlNode;
  i: Integer;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFe.Ide.NFref.New;
  i := NFe.Ide.NFref.Count - 1;
  (*B13*) NFe.ide.NFref[i].refNFe := ObterConteudo(ANode.Childrens.Find('refNFe'), tcEsp);
          NFe.ide.NFref[i].refNFeSig := ObterConteudo(ANode.Childrens.Find('refNFeSig'), tcEsp);

  refNode := ANode.Childrens.Find('refNF');
  if refNode <> nil then
  begin
    (*B15*) NFe.Ide.NFref[i].RefNF.cUF    := ObterConteudo(refNode.Childrens.Find('cUF'), tcInt);
    (*B16*) NFe.Ide.NFref[i].RefNF.AAMM   := ObterConteudo(refNode.Childrens.Find('AAMM'), tcEsp);
    (*B17*) NFe.Ide.NFref[i].RefNF.CNPJ   := ObterConteudo(refNode.Childrens.Find('CNPJ'), tcEsp);
    (*B18*) NFe.Ide.NFref[i].RefNF.Modelo := StrToIntDef(ObterConteudo(refNode.Childrens.Find('mod'), tcInt),55);
    (*B19*) NFe.ide.NFref[i].RefNF.serie  := ObterConteudo(refNode.Childrens.Find('serie'), tcInt);
    (*B20*) NFe.Ide.NFref[i].RefNF.nNF    := ObterConteudo(refNode.Childrens.Find('nNF'), tcInt);
  end;

  refNode := ANode.Childrens.Find('refNFP');
  if refNode <> nil then
  begin
    (*B20b*) NFe.Ide.NFref[i].RefNFP.cUF     := ObterConteudo(refNode.Childrens.Find('cUF'), tcInt);
    (*B20c*) NFe.Ide.NFref[i].RefNFP.AAMM    := ObterConteudo(refNode.Childrens.Find('AAMM'), tcEsp);
    (*B20d/B20e*) NFe.Ide.NFref[i].RefNFP.CNPJCPF := ObterCNPJCPF(refNode);
    (*B20f*) NFe.Ide.NFref[i].RefNFP.IE      := ObterConteudo(refNode.Childrens.Find('IE'), tcEsp);
    (*B20f*) NFe.Ide.NFref[i].RefNFP.Modelo  := ObterConteudo(refNode.Childrens.Find('mod'), tcInt);
    (*B20g*) NFe.ide.NFref[i].RefNFP.serie   := ObterConteudo(refNode.Childrens.Find('serie'), tcInt);
    (*B20h*) NFe.Ide.NFref[i].RefNFP.nNF     := ObterConteudo(refNode.Childrens.Find('nNF'), tcInt);

  end;

  (*B20i*)NFe.ide.NFref[i].refCTe := ObterConteudo(ANode.Childrens.Find('refCTe'), tcEsp);

  refNode := ANode.Childrens.Find('refECF');
  if refNode <> nil then
  begin
  (*B20k*)NFe.Ide.NFref[i].RefECF.modelo := StrToECFModRef(ok,ObterConteudo(refNode.Childrens.Find('mod'), tcStr)) ;
  (*B20l*)NFe.ide.NFref[i].RefECF.nECF := ObterConteudo(refNode.Childrens.Find('nECF'), tcStr);
  (*B20m*)NFe.Ide.NFref[i].RefECF.nCOO := ObterConteudo(refNode.Childrens.Find('nCOO'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerEmit(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*C02/C02a*)NFe.Emit.CNPJCPF := ObterCNPJCPF(ANode);
  (*C03*)NFe.Emit.xNome        := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  (*C04*)NFe.Emit.xFant        := ObterConteudo(ANode.Childrens.Find('xFant'), tcStr);
  (*C17*)NFe.Emit.IE           := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
  (*C18*)NFe.Emit.IEST         := ObterConteudo(ANode.Childrens.Find('IEST'), tcStr);
  (*C19*)NFe.Emit.IM           := ObterConteudo(ANode.Childrens.Find('IM'), tcStr);
  (*C20*)NFe.Emit.CNAE         := ObterConteudo(ANode.Childrens.Find('CNAE'), tcStr);
  (*C21*)NFe.Emit.CRT          := StrToCRT(ok, ObterConteudo(ANode.Childrens.Find('CRT'), tcStr));
  LerEmitEnderEmit(ANode.Childrens.Find('enderEmit'));
end;

procedure TNFeXmlReader.LerEmitEnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*C06*)NFe.Emit.enderEmit.xLgr    := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*C07*)NFe.Emit.enderEmit.nro     := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*C08*)NFe.Emit.enderEmit.xCpl    := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*C09*)NFe.Emit.enderEmit.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*C10*)NFe.Emit.EnderEmit.cMun    := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*C11*)NFe.Emit.enderEmit.xMun    := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*C12*)NFe.Emit.enderEmit.UF      := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*C13*)NFe.Emit.enderEmit.CEP     := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*C14*)NFe.Emit.enderEmit.cPais   := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);

  if NFe.Emit.enderEmit.cPais = 0 then
    NFe.Emit.enderEmit.cPais := 1058;

  (*C15*)NFe.Emit.enderEmit.xPais   := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);

  if NFe.Emit.enderEmit.xPais = '' then
    NFe.Emit.enderEmit.xPais := 'BRASIL';

  (*C16*)NFe.Emit.enderEmit.fone    := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
end;

procedure TNFeXmlReader.LerAvulsa(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*D02*)NFe.Avulsa.CNPJ    := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  (*D03*)NFe.Avulsa.xOrgao  := ObterConteudo(ANode.Childrens.Find('xOrgao'), tcStr);
  (*D04*)NFe.Avulsa.matr    := ObterConteudo(ANode.Childrens.Find('matr'), tcStr);
  (*D05*)NFe.Avulsa.xAgente := ObterConteudo(ANode.Childrens.Find('xAgente'), tcStr);
  (*D06*)NFe.Avulsa.fone    := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  (*D07*)NFe.Avulsa.UF      := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*D08*)NFe.Avulsa.nDAR    := ObterConteudo(ANode.Childrens.Find('nDAR'), tcStr);
  (*D09*)NFe.Avulsa.dEmi    := ObterConteudo(ANode.Childrens.Find('dEmi'), tcDat);
  (*D10*)NFe.Avulsa.vDAR    := ObterConteudo(ANode.Childrens.Find('vDAR'), tcDe2);
  (*D11*)NFe.Avulsa.repEmi  := ObterConteudo(ANode.Childrens.Find('repEmi'), tcStr);
  (*D12*)NFe.Avulsa.dPag    := ObterConteudo(ANode.Childrens.Find('dPag'), tcDat);
end;

procedure TNFeXmlReader.LerDest(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then
  begin
    NFe.Dest.indIEDest := inNaoContribuinte;
    Exit;
  end;

  (*E02/E03*)NFe.Dest.CNPJCPF := ObterCNPJCPF(ANode);

  if NFe.infNFe.Versao >= 3 then
    (*E03a*)NFe.Dest.idEstrangeiro := ObterConteudo(ANode.Childrens.Find('idEstrangeiro'), tcStr);

  (*E04*)NFe.Dest.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);

  if NFe.infNFe.Versao >= 3 then
    (*E16a*)NFe.Dest.indIEDest := StrToindIEDest(Ok, ObterConteudo(ANode.Childrens.Find('indIEDest'), tcStr));

  (*E17*)NFe.Dest.IE := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
  (*E18*)NFe.Dest.ISUF := ObterConteudo(ANode.Childrens.Find('ISUF'), tcStr);

  if NFe.infNFe.Versao >= 3 then
    (*E18a*)NFe.Dest.IM := ObterConteudo(ANode.Childrens.Find('IM'), tcStr);

  (*E19*)NFe.Dest.Email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);

  LerDestEnderDest(ANode.Childrens.Find('enderDest'));
end;

procedure TNFeXmlReader.LerDestEnderDest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*E06*)NFe.Dest.enderDest.xLgr    := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*E07*)NFe.Dest.enderDest.nro     := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*E08*)NFe.Dest.enderDest.xCpl    := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*E09*)NFe.Dest.enderDest.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*E10*)NFe.Dest.enderDest.cMun    := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*E11*)NFe.Dest.enderDest.xMun    := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*E12*)NFe.Dest.enderDest.UF      := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*E13*)NFe.Dest.enderDest.CEP     := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*E14*)NFe.Dest.enderDest.cPais   := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);

  if NFe.Dest.enderDest.cPais = 0 then
    NFe.Dest.enderDest.cPais := 1058;

  (*E15*)NFe.Dest.enderDest.xPais   := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);

  if NFe.Dest.enderDest.xPais = '' then
    NFe.Dest.enderDest.xPais := 'BRASIL';

  (*E16*)NFe.Dest.enderDest.fone    := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
end;

procedure TNFeXmlReader.LerRetirada(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*F02/F02a*)NFe.Retirada.CNPJCPF := ObterCNPJCPF(ANode);
  (*F02b*)NFe.Retirada.xNome       := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  (*F03*)NFe.Retirada.xLgr         := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*F04*)NFe.Retirada.nro          := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*F05*)NFe.Retirada.xCpl         := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*F06*)NFe.Retirada.xBairro      := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*F07*)NFe.Retirada.cMun         := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*F08*)NFe.Retirada.xMun         := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*F09*)NFe.Retirada.UF           := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*F10*)NFe.Retirada.CEP          := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*F11*)NFe.Retirada.cPais        := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);
  (*F12*)NFe.Retirada.xPais        := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);
  (*F13*)NFe.Retirada.fone         := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  (*F14*)NFe.Retirada.Email        := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  (*F15*)NFe.Retirada.IE           := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
end;

procedure TNFeXmlReader.LerEntrega(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*G02/G02a*)NFe.Entrega.CNPJCPF := ObterCNPJCPF(ANode);
  (*G02b*)NFe.Entrega.xNome       := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  (*G03*)NFe.Entrega.xLgr         := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*G04*)NFe.Entrega.nro          := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*G05*)NFe.Entrega.xCpl         := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*G06*)NFe.Entrega.xBairro      := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*G07*)NFe.Entrega.cMun         := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*G08*)NFe.Entrega.xMun         := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*G09*)NFe.Entrega.UF           := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*G10*)NFe.Entrega.CEP          := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*G11*)NFe.Entrega.cPais        := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);
  (*G12*)NFe.Entrega.xPais        := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);
  (*G13*)NFe.Entrega.fone         := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  (*G14*)NFe.Entrega.Email        := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  (*G15*)NFe.Entrega.IE           := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
end;

procedure TNFeXmlReader.LerDet(const ANode: TACBrXmlNode);
Var
  Item: TDetCollectionItem;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NFe.Det.New;
  (*   *)Item.prod.nItem := NFe.Det.Count;
  (*V01*)Item.infAdProd  := ObterConteudo(ANode.Childrens.Find('infAdProd'), tcStr);

  LerDetProd(Item, ANode.Childrens.Find('prod'));
  LerDetImposto(Item, ANode.Childrens.Find('imposto'));

  AuxNode := ANode.Childrens.Find('impostoDevol');
  if (AuxNode <> nil) then
  begin
    (*U51*)Item.pDevol := ObterConteudo(AuxNode.Childrens.Find('pDevol'), tcDe2);

    AuxNode := AuxNode.Childrens.Find('IPI');
    if (AuxNode <> nil) then
    begin
      (*U61*)Item.vIPIDevol := ObterConteudo(AuxNode.Childrens.Find('vIPIDevol'), tcDe2);
    end;
  end;

  LerDetObsItem(Item, ANode.Childrens.Find('obsItem'));
end;

procedure TNFeXmlReader.LerDetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*I02*)Item.Prod.cProd := ObterConteudo(ANode.Childrens.Find('cProd'), tcStr);
  (*I03*)Item.Prod.cEAN  := ObterConteudo(ANode.Childrens.Find('cEAN'), tcStr);
         Item.Prod.cBarra := ObterConteudo(ANode.Childrens.Find('cBarra'), tcStr);
  (*I04*)Item.Prod.xProd := ObterConteudo(ANode.Childrens.Find('xProd'), tcStr);
  (*I05*)Item.Prod.NCM   := ObterConteudo(ANode.Childrens.Find('NCM'), tcStr);
  (*I05w*)Item.Prod.CEST := ObterConteudo(ANode.Childrens.Find('CEST'), tcStr);
  if NFe.infNFe.Versao >= 4 then
  begin
    (*I05d*)Item.Prod.indEscala := StrToindEscala(ok, ObterConteudo(ANode.Childrens.Find('indEscala'), tcStr));
    (*I05e*)Item.Prod.CNPJFab   := ObterConteudo(ANode.Childrens.Find('CNPJFab'), tcStr);
    (*I05f*)Item.Prod.cBenef    := ObterConteudo(ANode.Childrens.Find('cBenef'), tcStr);
  end;
  (*I06*)Item.Prod.EXTIPI   := ObterConteudo(ANode.Childrens.Find('EXTIPI'), tcStr);
  (*I08*)Item.Prod.CFOP     := ObterConteudo(ANode.Childrens.Find('CFOP'), tcEsp);
  (*I09*)Item.Prod.uCom     := ObterConteudo(ANode.Childrens.Find('uCom'), tcStr);
  (*I10*)Item.Prod.qCom     := ObterConteudo(ANode.Childrens.Find('qCom'), tcDe4);
  (*I10a*)Item.Prod.vUnCom  := ObterConteudo(ANode.Childrens.Find('vUnCom'), tcDe10);
  (*I11*)Item.Prod.vProd    := ObterConteudo(ANode.Childrens.Find('vProd'), tcDe2);
  (*I12*)Item.Prod.cEANTrib := ObterConteudo(ANode.Childrens.Find('cEANTrib'), tcStr);
         Item.Prod.cBarraTrib := ObterConteudo(ANode.Childrens.Find('cBarraTrib'), tcStr);
  (*I13*)Item.Prod.uTrib    := ObterConteudo(ANode.Childrens.Find('uTrib'), tcStr);
  (*I14*)Item.Prod.qTrib    := ObterConteudo(ANode.Childrens.Find('qTrib'), tcDe4);
  (*I14a*)Item.Prod.vUnTrib := ObterConteudo(ANode.Childrens.Find('vUnTrib'), tcDe10);
  (*I15*)Item.Prod.vFrete   := ObterConteudo(ANode.Childrens.Find('vFrete'), tcDe2);
  (*I16*)Item.Prod.vSeg     := ObterConteudo(ANode.Childrens.Find('vSeg'), tcDe2);
  (*I17*)Item.Prod.vDesc    := ObterConteudo(ANode.Childrens.Find('vDesc'), tcDe2);
  (*I17a*)Item.Prod.vOutro  := ObterConteudo(ANode.Childrens.Find('vOutro'), tcDe2);
  (*I17b*)Item.Prod.IndTot  := StrToindTot(ok,ObterConteudo(ANode.Childrens.Find('indTot'), tcDe2));
  (*I30*)Item.Prod.xPed     := ObterConteudo(ANode.Childrens.Find('xPed'), tcStr);
  (*I31*)Item.Prod.nItemPed := ObterConteudo(ANode.Childrens.Find('nItemPed'), tcStr);
  (*I31*)Item.Prod.nRECOPI  := ObterConteudo(ANode.Childrens.Find('nRECOPI'), tcStr);
  (*I70*)Item.Prod.nFCI     := ObterConteudo(ANode.Childrens.Find('nFCI'), tcStr);

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
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  DIItem: TDICollectionItem;
  AdiItem: TAdiCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DIItem := Item.Prod.DI.New;
  (*I19*)DIItem.nDI        := ObterConteudo(ANode.Childrens.Find('nDI'), tcStr);
  (*I20*)DIItem.dDI        := ObterConteudo(ANode.Childrens.Find('dDI'), tcDat);
  (*I21*)DIItem.xLocDesemb := ObterConteudo(ANode.Childrens.Find('xLocDesemb'), tcStr);
  (*I22*)DIItem.UFDesemb   := ObterConteudo(ANode.Childrens.Find('UFDesemb'), tcStr);
  (*I23*)DIItem.dDesemb    := ObterConteudo(ANode.Childrens.Find('dDesemb'), tcDat);

  (*I23a*)DIItem.tpViaTransp  := StrToTipoViaTransp(Ok, ObterConteudo(ANode.Childrens.Find('tpViaTransp'), tcInt));
  (*I23b*)DIItem.vAFRMM       := ObterConteudo(ANode.Childrens.Find('vAFRMM'), tcDe2);
  (*I23c*)DIItem.tpIntermedio := StrToTipoIntermedio(Ok, ObterConteudo(ANode.Childrens.Find('tpIntermedio'), tcInt));
  (*I23d*)DIItem.CNPJ         := ObterCNPJCPF(ANode);
  (*I23e*)DIItem.UFTerceiro   := ObterConteudo(ANode.Childrens.Find('UFTerceiro'), tcStr);

  (*I24*)DIItem.cExportador   := ObterConteudo(ANode.Childrens.Find('cExportador'), tcStr);

  DIItem.adi.Clear;
  ANodes := ANode.Childrens.FindAll('adi');
  for i := 0 to Length(ANodes) - 1 do
  begin
    AdiItem := DIItem.adi.New;
    (*I26*)AdiItem.nAdicao     := ObterConteudo(ANodes[i].Childrens.Find('nAdicao'), tcInt);
    (*I27*)AdiItem.nSeqAdi     := ObterConteudo(ANodes[i].Childrens.Find('nSeqAdic'), tcInt);
    (*I28*)AdiItem.cFabricante := ObterConteudo(ANodes[i].Childrens.Find('cFabricante'), tcStr);
    (*I29*)AdiItem.vDescDI     := ObterConteudo(ANodes[i].Childrens.Find('vDescDI'), tcDe2);
    (*I29a*)AdiItem.nDraw      := ObterConteudo(ANodes[i].Childrens.Find('nDraw'), tcStr);
  end;

end;

procedure TNFeXmlReader.LerDetProdDetExport(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  AuxNode: TACBrXmlNode;
  DetExportItem: TdetExportCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DetExportItem := Item.Prod.detExport.New;
  (*I51*)DetExportItem.nDraw := ObterConteudo(ANode.Childrens.Find('nDraw'), tcStr);

  AuxNode := ANode.Childrens.Find('exportInd');
  if (AuxNode <> nil) then
  begin
    (*I53*)DetExportItem.nRE     := ObterConteudo(AuxNode.Childrens.Find('nRE'), tcStr);
    (*I54*)DetExportItem.chNFe   := ObterConteudo(AuxNode.Childrens.Find('chNFe'), tcStr);
    (*I55*)DetExportItem.qExport := ObterConteudo(AuxNode.Childrens.Find('qExport'), tcDe4);
  end;
end;

procedure TNFeXmlReader.LerDetProdRastro(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  RastroItem: TRastroCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  RastroItem := Item.Prod.rastro.New;
  (*I81*)RastroItem.nLote  := ObterConteudo(ANode.Childrens.Find('nLote'), tcStr);
  (*I82*)RastroItem.qLote  := ObterConteudo(ANode.Childrens.Find('qLote'), tcDe3);
  (*I83*)RastroItem.dFab   := ObterConteudo(ANode.Childrens.Find('dFab'), tcDat);
  (*I84*)RastroItem.dVal   := ObterConteudo(ANode.Childrens.Find('dVal'), tcDat);
  (*I85*)RastroItem.cAgreg := ObterConteudo(ANode.Childrens.Find('cAgreg'), tcStr);
end;

procedure TNFeXmlReader.LerDetProdVeicProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*J02*)Item.Prod.veicProd.tpOP         := StrToTpOP(ok, ObterConteudo(ANode.Childrens.Find('tpOp'), tcStr));
  (*J03*)Item.Prod.veicProd.chassi       := ObterConteudo(ANode.Childrens.Find('chassi'), tcStr);
  (*J04*)Item.Prod.veicProd.cCor         := ObterConteudo(ANode.Childrens.Find('cCor'), tcStr);
  (*J05*)Item.Prod.veicProd.xCor         := ObterConteudo(ANode.Childrens.Find('xCor'), tcStr);
  (*J06*)Item.Prod.veicProd.pot          := ObterConteudo(ANode.Childrens.Find('pot'), tcStr);
  (*J07*)Item.Prod.veicProd.Cilin        := ObterConteudo(ANode.Childrens.Find('cilin'), tcStr);
  (*J08*)Item.Prod.veicProd.pesoL        := ObterConteudo(ANode.Childrens.Find('pesoL'), tcStr);
  (*J09*)Item.Prod.veicProd.pesoB        := ObterConteudo(ANode.Childrens.Find(''), tcStr);
  (*J10*)Item.Prod.veicProd.nSerie       := ObterConteudo(ANode.Childrens.Find('nSerie'), tcStr);
  (*J11*)Item.Prod.veicProd.tpComb       := ObterConteudo(ANode.Childrens.Find('tpComb'), tcStr);
  (*J12*)Item.Prod.veicProd.nMotor       := ObterConteudo(ANode.Childrens.Find('nMotor'), tcStr);
  (*J13*)Item.Prod.veicProd.CMT          := ObterConteudo(ANode.Childrens.Find('CMT'), tcStr);
  (*J14*)Item.Prod.veicProd.dist         := ObterConteudo(ANode.Childrens.Find('dist'), tcStr);
  //(*J15*)Item.Prod.veicProd.RENAVAM := ObterConteudo(ANode.Childrens.Find('RENAVAM'), tcEsp);
  (*J16*)Item.Prod.veicProd.anoMod       := ObterConteudo(ANode.Childrens.Find('anoMod'), tcInt);
  (*J17*)Item.Prod.veicProd.anoFab       := ObterConteudo(ANode.Childrens.Find('anoFab'), tcInt);
  (*J18*)Item.Prod.veicProd.tpPint       := ObterConteudo(ANode.Childrens.Find('tpPint'), tcStr);
  (*J19*)Item.Prod.veicProd.tpVeic       := ObterConteudo(ANode.Childrens.Find('tpVeic'), tcInt);
  (*J20*)Item.Prod.veicProd.espVeic      := ObterConteudo(ANode.Childrens.Find('espVeic'), tcInt);
  (*J21*)Item.Prod.veicProd.VIN          := ObterConteudo(ANode.Childrens.Find('VIN'), tcStr);
  (*J22*)Item.Prod.veicProd.condVeic     := StrToCondVeic(ok, ObterConteudo(ANode.Childrens.Find('condVeic'), tcStr));
  (*J23*)Item.Prod.veicProd.cMod         := ObterConteudo(ANode.Childrens.Find('cMod'), tcStr);
  (*J24*)Item.Prod.veicProd.cCorDENATRAN := ObterConteudo(ANode.Childrens.Find('cCorDENATRAN'), tcStr);
  (*J25*)Item.Prod.veicProd.lota         := ObterConteudo(ANode.Childrens.Find('lota'), tcInt);
  (*J26*)Item.Prod.veicProd.tpRest       := ObterConteudo(ANode.Childrens.Find('tpRest'), tcInt);
end;

procedure TNFeXmlReader.LerDetProdMed(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  MedItem: TMedCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  MedItem := Item.Prod.med.New;
  (*K01a*)MedItem.cProdANVISA := ObterConteudo(ANode.Childrens.Find('cProdANVISA'), tcStr);
  (*K01b*)MedItem.xMotivoIsencao := ObterConteudo(ANode.Childrens.Find('xMotivoIsencao'), tcStr);
  (*K02*)MedItem.nLote := ObterConteudo(ANode.Childrens.Find('nLote'), tcStr);
  (*K03*)MedItem.qLote := ObterConteudo(ANode.Childrens.Find('qLote'), tcDe3);
  (*K04*)MedItem.dFab  := ObterConteudo(ANode.Childrens.Find('dFab'), tcDat);
  (*K05*)MedItem.dVal  := ObterConteudo(ANode.Childrens.Find('dVal'), tcDat);
  (*K06*)MedItem.vPMC  := ObterConteudo(ANode.Childrens.Find('vPMC'), tcDe2);
end;

procedure TNFeXmlReader.LerDetProdArma(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  ArmaItem: TArmaCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  ArmaItem := Item.Prod.arma.New;
  (*L02*)ArmaItem.tpArma := StrToTpArma(ok, ObterConteudo(ANode.Childrens.Find('tpArma'), tcStr));
  (*L03*)ArmaItem.nSerie := ObterConteudo(ANode.Childrens.Find('nSerie'), tcStr);
  (*L04*)ArmaItem.nCano  := ObterConteudo(ANode.Childrens.Find('nCano'), tcStr);
  (*L05*)ArmaItem.descr  := ObterConteudo(ANode.Childrens.Find('descr'), tcStr);
end;

procedure TNFeXmlReader.LerDetProdComb(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  AuxNode: TACBrXmlNode;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  Ok: Boolean;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*L102*)Item.Prod.comb.cProdANP := ObterConteudo(ANode.Childrens.Find('cProdANP'), tcInt);
  (*L102a*)Item.Prod.comb.pMixGN  := ObterConteudo(ANode.Childrens.Find('qMixGN'), tcDe4);
  (*LA03*)Item.Prod.comb.descANP  := ObterConteudo(ANode.Childrens.Find('descANP'), tcStr);
  (*LA03a*)Item.Prod.comb.pGLP    := ObterConteudo(ANode.Childrens.Find('pGLP'), tcDe4);
  (*LA03b*)Item.Prod.comb.pGNn    := ObterConteudo(ANode.Childrens.Find('pGNn'), tcDe4);
  (*LA03c*)Item.Prod.comb.pGNi    := ObterConteudo(ANode.Childrens.Find('pGNi'), tcDe4);
  (*LA03d*)Item.Prod.comb.vPart   := ObterConteudo(ANode.Childrens.Find('vPart'), tcDe2);
  (*LA04*)Item.Prod.comb.CODIF    := ObterConteudo(ANode.Childrens.Find('CODIF'), tcEsp);
  (*LA05*)Item.Prod.comb.qTemp    := ObterConteudo(ANode.Childrens.Find('qTemp'), tcDe4);
  (*LA06*)Item.Prod.comb.UFcons   := ObterConteudo(ANode.Childrens.Find('UFCons'), tcStr);
  (*L120*)Item.Prod.comb.ICMSCons.UFcons := ObterConteudo(ANode.Childrens.Find('UFcons'), tcStr);

  AuxNode := ANode.Childrens.Find('CIDE');
  if (AuxNode <> nil) then
  begin
    (*L106*)Item.Prod.comb.CIDE.qBCprod   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*L107*)Item.Prod.comb.CIDE.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    (*L108*)Item.Prod.comb.CIDE.vCIDE     := ObterConteudo(AuxNode.Childrens.Find('vCIDE'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('encerrante');
  if (AuxNode <> nil) then
  begin
    (*LA12*)Item.Prod.comb.encerrante.nBico   := ObterConteudo(AuxNode.Childrens.Find('nBico'), tcInt);
    (*LA13*)Item.Prod.comb.encerrante.nBomba  := ObterConteudo(AuxNode.Childrens.Find('nBomba'), tcInt);
    (*LA14*)Item.Prod.comb.encerrante.nTanque := ObterConteudo(AuxNode.Childrens.Find('nTanque'), tcInt);
    (*LA15*)Item.Prod.comb.encerrante.vEncIni := ObterConteudo(AuxNode.Childrens.Find('vEncIni'), tcDe3);
    (*LA16*)Item.Prod.comb.encerrante.vEncFin := ObterConteudo(AuxNode.Childrens.Find('vEncFin'), tcDe3);
  end;

  Item.Prod.comb.pBio := ObterConteudo(ANode.Childrens.Find('pBio'), tcDe4);

  ANodes := ANode.Childrens.FindAll('origComb');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Item.Prod.comb.origComb.New;
    Item.Prod.comb.origComb[i].indImport := StrToindImport(Ok, ObterConteudo(ANodes[i]).Childrens.Find('indImport'), tcStr));
    Item.Prod.comb.origComb[i].cUFOrig := ObterConteudo(ANodes[i]).Childrens.Find('cUFOrig'), tcInt);
    Item.Prod.comb.origComb[i].pOrig := ObterConteudo(ANodes[i]).Childrens.Find('pOrig'), tcDe4);
  end;

  AuxNode := ANode.Childrens.Find('ICMSComb');
  if (AuxNode <> nil) then
  begin
    (*L110*)Item.Prod.comb.ICMS.vBCICMS   := ObterConteudo(AuxNode.Childrens.Find('vBCICMS'), tcDe2);
    (*L111*)Item.Prod.comb.ICMS.vICMS     := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    (*L112*)Item.Prod.comb.ICMS.vBCICMSST := ObterConteudo(AuxNode.Childrens.Find('vBCICMSST'), tcDe2);
    (*L113*)Item.Prod.comb.ICMS.vICMSST   := ObterConteudo(AuxNode.Childrens.Find('vICMSST'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ICMSInter');
  if (AuxNode <> nil) then
  begin
    (*L115*)Item.Prod.comb.ICMSInter.vBCICMSSTDest := ObterConteudo(AuxNode.Childrens.Find('vBCICMSSTDest'), tcDe2);
    (*L116*)Item.Prod.comb.ICMSInter.vICMSSTDest   := ObterConteudo(AuxNode.Childrens.Find('vICMSSTDest'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ICMSCons');
  if (AuxNode <> nil) then
  begin
    (*L118*)Item.Prod.comb.ICMSCons.vBCICMSSTCons := ObterConteudo(AuxNode.Childrens.Find('vBCICMSSTCons'), tcDe2);
    (*L119*)Item.Prod.comb.ICMSCons.vICMSSTCons   := ObterConteudo(AuxNode.Childrens.Find('vICMSSTCons'), tcDe2);
    (*L119*)Item.Prod.comb.ICMSCons.UFcons        := ObterConteudo(AuxNode.Childrens.Find('UFCons'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerDetImposto(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  AuxNode, AxNode: TACBrXmlNode;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*M02*)Item.Imposto.vTotTrib := ObterConteudo(ANode.Childrens.Find('vTotTrib'), tcDe2);

  AuxNode := ANode.Childrens.Find('ICMS');
  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];
  if (AuxNode <> nil) then
  begin
    (*N11*)Item.Imposto.ICMS.orig        := StrToOrig(ok, ObterConteudo(AuxNode.Childrens.Find('orig'), tcStr));
    (*N12*)Item.Imposto.ICMS.CST         := StrToCSTICMS(ok, ObterConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    (*N12a*)Item.Imposto.ICMS.CSOSN      := StrToCSOSNIcms( ok,ObterConteudo(AuxNode.Childrens.Find('CSOSN'), tcInt));
    (*N13*)Item.Imposto.ICMS.modBC       := StrToModBC(ok, ObterConteudo(AuxNode.Childrens.Find('modBC'), tcStr));
    (*N14*)Item.Imposto.ICMS.pRedBC      := ObterConteudo(AuxNode.Childrens.Find('pRedBC'), tcDe2);
    (*N15*)Item.Imposto.ICMS.vBC         := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*N16*)Item.Imposto.ICMS.pICMS       := ObterConteudo(AuxNode.Childrens.Find('pICMS'), tcDe2);
    (*N16a*)Item.Imposto.ICMS.vICMSOp    := ObterConteudo(AuxNode.Childrens.Find('vICMSOp'), tcDe2);
    (*N16b*)Item.Imposto.ICMS.pDif       := ObterConteudo(AuxNode.Childrens.Find('pDif'), tcDe4);
    (*N16c*)Item.Imposto.ICMS.vICMSDif   := ObterConteudo(AuxNode.Childrens.Find('vICMSDif'), tcDe2);
    (*N17*)Item.Imposto.ICMS.vICMS       := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    (*N17a*)Item.Imposto.ICMS.vBCFCP     := ObterConteudo(AuxNode.Childrens.Find('vBCFCP'), tcDe2);
    (*N17b*)Item.Imposto.ICMS.pFCP       := ObterConteudo(AuxNode.Childrens.Find('pFCP'), tcDe4);
    (*N17c*)Item.Imposto.ICMS.vFCP       := ObterConteudo(AuxNode.Childrens.Find('vFCP'), tcDe2);
    (*N18*)Item.Imposto.ICMS.modBCST     := StrToModBCST(ok, ObterConteudo(AuxNode.Childrens.Find('modBCST'), tcStr));
    (*N19*)Item.Imposto.ICMS.pMVAST      := ObterConteudo(AuxNode.Childrens.Find('pMVAST'), tcDe2);
    (*N20*)Item.Imposto.ICMS.pRedBCST    := ObterConteudo(AuxNode.Childrens.Find('pRedBCST'), tcDe2);
    (*N21*)Item.Imposto.ICMS.vBCST       := ObterConteudo(AuxNode.Childrens.Find('vBCST'), tcDe2);
    (*N22*)Item.Imposto.ICMS.pICMSST     := ObterConteudo(AuxNode.Childrens.Find('pICMSST'), tcDe2);
    (*N23*)Item.Imposto.ICMS.vICMSST     := ObterConteudo(AuxNode.Childrens.Find('vICMSST'), tcDe2);
    (*N23a*)Item.Imposto.ICMS.vBCFCPST   := ObterConteudo(AuxNode.Childrens.Find('vBCFCPST'), tcDe2);
    (*N23b*)Item.Imposto.ICMS.pFCPST     := ObterConteudo(AuxNode.Childrens.Find('pFCPST'), tcDe4);
    (*N23d*)Item.Imposto.ICMS.vFCPST     := ObterConteudo(AuxNode.Childrens.Find('vFCPST'), tcDe2);
    (*N24*)Item.Imposto.ICMS.UFST        := ObterConteudo(AuxNode.Childrens.Find('UFST'), tcStr);
    (*N25*)Item.Imposto.ICMS.pBCOp       := ObterConteudo(AuxNode.Childrens.Find('pBCOp'), tcDe2);
    (*N26*)Item.Imposto.ICMS.vBCSTRet    := ObterConteudo(AuxNode.Childrens.Find('vBCSTRet'), tcDe2);
    (*N27*)Item.Imposto.ICMS.vICMSSTRet  := ObterConteudo(AuxNode.Childrens.Find('vICMSSTRet'), tcDe2);
    (*N27a*)Item.Imposto.ICMS.vICMSDeson := ObterConteudo(AuxNode.Childrens.Find('vICMSDeson'), tcDe2);
    (*N27a*)Item.Imposto.ICMS.vBCFCPSTRet:= ObterConteudo(AuxNode.Childrens.Find('vBCFCPSTRet'), tcDe2);
    (*N27b*)Item.Imposto.ICMS.pFCPSTRet  := ObterConteudo(AuxNode.Childrens.Find('pFCPSTRet'), tcDe4);
    (*N27d*)Item.Imposto.ICMS.vFCPSTRet  := ObterConteudo(AuxNode.Childrens.Find('vFCPSTRet'), tcDe2);
    (*N27e*)Item.Imposto.ICMS.pST        := ObterConteudo(AuxNode.Childrens.Find('pST'), tcDe4);
    (*N28*)Item.Imposto.ICMS.motDesICMS  := StrTomotDesICMS(ok, ObterConteudo(AuxNode.Childrens.Find('motDesICMS'), tcStr));
    (*N29*)Item.Imposto.ICMS.pCredSN     := ObterConteudo(AuxNode.Childrens.Find('pCredSN'), tcDe2);
    (*N30*)Item.Imposto.ICMS.vCredICMSSN := ObterConteudo(AuxNode.Childrens.Find('vCredICMSSN'), tcDe2);
    (*N31*)Item.Imposto.ICMS.vBCSTDest   := ObterConteudo(AuxNode.Childrens.Find('vBCSTDest'), tcDe2);
    (*N32*)Item.Imposto.ICMS.vICMSSTDest := ObterConteudo(AuxNode.Childrens.Find('vICMSSTDest'), tcDe2);
    (*N34*)Item.Imposto.ICMS.pRedBCEfet  := ObterConteudo(AuxNode.Childrens.Find('pRedBCEfet'), tcDe4);
    (*N35*)Item.Imposto.ICMS.vBCEfet     := ObterConteudo(AuxNode.Childrens.Find('vBCEfet'), tcDe2);
    (*N36*)Item.Imposto.ICMS.pICMSEfet   := ObterConteudo(AuxNode.Childrens.Find('pICMSEfet'), tcDe4);
    (*N37*)Item.Imposto.ICMS.vICMSEfet   := ObterConteudo(AuxNode.Childrens.Find('vICMSEfet'), tcDe2);
    (*N26b*)Item.Imposto.ICMS.vICMSSubstituto := ObterConteudo(AuxNode.Childrens.Find('vICMSSubstituto'), tcDe2);
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
            Item.Imposto.ICMS.motRedAdRem := StrTomotRedAdRem(ok, ObterConteudo(AuxNode.Childrens.Find('motRedAdRem'), tcStr));
            Item.Imposto.ICMS.qBCMonoRet := ObterConteudo(AuxNode.Childrens.Find('qBCMonoRet'), tcDe2);
            Item.Imposto.ICMS.vICMSMonoOp := ObterConteudo(AuxNode.Childrens.Find('vICMSMonoOp'), tcDe2);
            Item.Imposto.ICMS.indDeduzDeson := StrToTIndicador(ok, ObterConteudo(AuxNode.Childrens.Find('indDeduzDeson'), tcStr));

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
    (*NA03*)Item.Imposto.ICMSUFDest.vBCUFDest      := ObterConteudo(AuxNode.Childrens.Find('vBCUFDest'), tcDe2);
    (*NA04*)Item.Imposto.ICMSUFDest.vBCFCPUFDest   := ObterConteudo(AuxNode.Childrens.Find('vBCFCPUFDest'), tcDe2);
    (*NA05*)Item.Imposto.ICMSUFDest.pFCPUFDest     := ObterConteudo(AuxNode.Childrens.Find('pFCPUFDest'), tcDe2);
    (*NA07*)Item.Imposto.ICMSUFDest.pICMSUFDest    := ObterConteudo(AuxNode.Childrens.Find('pICMSUFDest'), tcDe2);
    (*NA09*)Item.Imposto.ICMSUFDest.pICMSInter     := ObterConteudo(AuxNode.Childrens.Find('pICMSInter'), tcDe2);
    (*NA11*)Item.Imposto.ICMSUFDest.pICMSInterPart := ObterConteudo(AuxNode.Childrens.Find('pICMSInterPart'), tcDe2);
    (*NA13*)Item.Imposto.ICMSUFDest.vFCPUFDest     := ObterConteudo(AuxNode.Childrens.Find('vFCPUFDest'), tcDe2);
    (*NA15*)Item.Imposto.ICMSUFDest.vICMSUFDest    := ObterConteudo(AuxNode.Childrens.Find('vICMSUFDest'), tcDe2);
    (*NA17*)Item.Imposto.ICMSUFDest.vICMSUFRemet   := ObterConteudo(AuxNode.Childrens.Find('vICMSUFRemet'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('IPI');
  if (AuxNode <> nil) then
  begin
    (*O02*)Item.Imposto.IPI.clEnq    := ObterConteudo(AuxNode.Childrens.Find('clEnq'), tcStr);
    (*O03*)Item.Imposto.IPI.CNPJProd := ObterConteudo(AuxNode.Childrens.Find('CNPJProd'), tcStr);
    (*O04*)Item.Imposto.IPI.cSelo    := ObterConteudo(AuxNode.Childrens.Find('cSelo'), tcStr);
    (*O05*)Item.Imposto.IPI.qSelo    := ObterConteudo(AuxNode.Childrens.Find('qSelo'), tcInt);
    (*O06*)Item.Imposto.IPI.cEnq     := ObterConteudo(AuxNode.Childrens.Find('cEnq'), tcStr);

    // Inicializa CST com sendo Não tributada e conforme o TIPO entrada ou saida
    // Caso a Tag não seja informada sera gravada com sendo não tributada
    if NFe.ide.tpNF = tnEntrada then
      Item.Imposto.IPI.CST := ipi53;
    if NFe.ide.tpNF = tnSaida then
      Item.Imposto.IPI.CST := ipi03;

    AxNode := AuxNode.Childrens.Find('IPITrib');
    if (AxNode <> nil) then
    begin
      (*O09*)Item.Imposto.IPI.CST   := StrToCSTIPI(ok, ObterConteudo(AxNode.Childrens.Find('CST'), tcStr));
      (*O10*)Item.Imposto.IPI.vBC   := ObterConteudo(AxNode.Childrens.Find('vBC'), tcDe2);
      (*O11*)Item.Imposto.IPI.qUnid := ObterConteudo(AxNode.Childrens.Find('qUnid'), tcDe4);
      (*O12*)Item.Imposto.IPI.vUnid := ObterConteudo(AxNode.Childrens.Find('vUnid'), tcDe4);
      (*O13*)Item.Imposto.IPI.pIPI  := ObterConteudo(AxNode.Childrens.Find('pIPI'), tcDe2);
      (*O14*)Item.Imposto.IPI.vIPI  := ObterConteudo(AxNode.Childrens.Find('vIPI'), tcDe2);
    end;

    AxNode := AuxNode.Childrens.Find('IPINT');
    if (AxNode <> nil) then
    begin
      (*O09*)Item.Imposto.IPI.CST := StrToCSTIPI(ok, ObterConteudo(AxNode.Childrens.Find('CST'), tcStr));
    end;
  end;

  AuxNode := ANode.Childrens.Find('II');
  if (AuxNode <> nil) then
  begin
    (*P02*)Item.Imposto.II.vBc      := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*P03*)Item.Imposto.II.vDespAdu := ObterConteudo(AuxNode.Childrens.Find('vDespAdu'), tcDe2);
    (*P04*)Item.Imposto.II.vII      := ObterConteudo(AuxNode.Childrens.Find('vII'), tcDe2);
    (*P05*)Item.Imposto.II.vIOF     := ObterConteudo(AuxNode.Childrens.Find('vIOF'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('PIS');
  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];
  if (AuxNode <> nil) then
  begin
    (*Q06*)Item.Imposto.PIS.CST       := StrToCSTPIS(ok, ObterConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    (*Q07*)Item.Imposto.PIS.vBC       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*Q08*)Item.Imposto.PIS.pPIS      := ObterConteudo(AuxNode.Childrens.Find('pPIS'), tcDe2);
    (*Q09*)Item.Imposto.PIS.vPIS      := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    (*Q10*)Item.Imposto.PIS.qBCProd   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*Q11*)Item.Imposto.PIS.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
  end;

  AuxNode := ANode.Childrens.Find('PISST');
  if (AuxNode <> nil) then
  begin
    (*R02*)Item.Imposto.PISST.vBc       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*R03*)Item.Imposto.PISST.pPis      := ObterConteudo(AuxNode.Childrens.Find('pPIS'), tcDe2);
    (*R04*)Item.Imposto.PISST.qBCProd   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*R05*)Item.Imposto.PISST.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    (*R06*)Item.Imposto.PISST.vPIS      := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    (*R07*)Item.Imposto.PISST.indSomaPISST := StrToindSomaPISST(ok, ObterConteudo(AuxNode.Childrens.Find('indSomaPISST'), tcStr));
  end;

  AuxNode := ANode.Childrens.Find('COFINS');
  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];
  if (AuxNode <> nil) then
  begin
    (*S06*)Item.Imposto.COFINS.CST       := StrToCSTCOFINS(ok, ObterConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    (*S07*)Item.Imposto.COFINS.vBC       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*S08*)Item.Imposto.COFINS.pCOFINS   := ObterConteudo(AuxNode.Childrens.Find('pCOFINS'), tcDe2);
    (*S09*)Item.Imposto.COFINS.qBCProd   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*S10*)Item.Imposto.COFINS.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    (*S11*)Item.Imposto.COFINS.vCOFINS   := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('COFINSST');
  if (AuxNode <> nil) then
  begin
    (*T02*)Item.Imposto.COFINSST.vBC       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*T03*)Item.Imposto.COFINSST.pCOFINS   := ObterConteudo(AuxNode.Childrens.Find('pCOFINS'), tcDe2);
    (*T04*)Item.Imposto.COFINSST.qBCProd   := ObterConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*T05*)Item.Imposto.COFINSST.vAliqProd := ObterConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    (*T06*)Item.Imposto.COFINSST.vCOFINS   := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
    (*T07*)Item.Imposto.COFINSST.indSomaCOFINSST := StrToindSomaPISST(ok, StrToindSomaCOFINSST(AuxNode.Childrens.Find('indSomaCOFINSST'), tcStr));
  end;

  AuxNode := ANode.Childrens.Find('ISSQN');
  if (AuxNode <> nil) then
  begin
    (*U02*)Item.Imposto.ISSQN.vBC       := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*U03*)Item.Imposto.ISSQN.vAliq     := ObterConteudo(AuxNode.Childrens.Find('vAliq'), tcDe2);
    (*U04*)Item.Imposto.ISSQN.vISSQN    := ObterConteudo(AuxNode.Childrens.Find('vISSQN'), tcDe2);
    (*U05*)Item.Imposto.ISSQN.cMunFG    := ObterConteudo(AuxNode.Childrens.Find('cMunFG'), tcInt);
    (*U06*)Item.Imposto.ISSQN.cListServ := ObterConteudo(AuxNode.Childrens.Find('cListServ'), tcStr);
    (*U07*)Item.Imposto.ISSQN.cSitTrib  := StrToISSQNcSitTrib( ok,  ObterConteudo(AuxNode.Childrens.Find('cSitTrib'), tcStr) ) ;
    (*U07*)Item.Imposto.ISSQN.vDeducao     := ObterConteudo(AuxNode.Childrens.Find('vDeducao'), tcDe2);
    (*U08*)Item.Imposto.ISSQN.vOutro       := ObterConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
    (*U09*)Item.Imposto.ISSQN.vDescIncond  := ObterConteudo(AuxNode.Childrens.Find('vDescIncond'), tcDe2);
    (*U10*)Item.Imposto.ISSQN.vDescCond    := ObterConteudo(AuxNode.Childrens.Find('vDescCond'), tcDe2);
    (*U12*)Item.Imposto.ISSQN.vISSRet      := ObterConteudo(AuxNode.Childrens.Find('vISSRet'), tcDe2);
    (*U13*)Item.Imposto.ISSQN.indISS       := StrToindISS(Ok, ObterConteudo(AuxNode.Childrens.Find('indISS'), tcStr));
    (*U14*)Item.Imposto.ISSQN.cServico     := ObterConteudo(AuxNode.Childrens.Find('cServico'), tcStr);
    (*U15*)Item.Imposto.ISSQN.cMun         := ObterConteudo(AuxNode.Childrens.Find('cMun'), tcInt);
    (*U16*)Item.Imposto.ISSQN.cPais        := ObterConteudo(AuxNode.Childrens.Find('cPais'), tcInt);
    (*U17*)Item.Imposto.ISSQN.nProcesso    := ObterConteudo(AuxNode.Childrens.Find('nProcesso'), tcStr);
    (*U18*)Item.Imposto.ISSQN.indIncentivo := StrToindIncentivo(Ok, ObterConteudo(AuxNode.Childrens.Find('indIncentivo'), tcStr));
  end;
end;

procedure TNFeXmlReader.LerDetObsItem(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Atr: TACBrXmlAttribute;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

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
Var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('ICMSTot');
  if (AuxNode <> nil) then
  begin
    (*W03*)NFe.Total.ICMSTot.vBC           := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*W04*)NFe.Total.ICMSTot.vICMS         := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    (*W04a*)NFe.Total.ICMSTot.vICMSDeson   := ObterConteudo(AuxNode.Childrens.Find('vICMSDeson'), tcDe2);
    (*W04c*)NFe.Total.ICMSTot.vFCPUFDest   := ObterConteudo(AuxNode.Childrens.Find('vFCPUFDest'), tcDe2);
    (*W04e*)NFe.Total.ICMSTot.vICMSUFDest  := ObterConteudo(AuxNode.Childrens.Find('vICMSUFDest'), tcDe2);
    (*W04g*)NFe.Total.ICMSTot.vICMSUFRemet := ObterConteudo(AuxNode.Childrens.Find('vICMSUFRemet'), tcDe2);
    (*W04h*)NFe.Total.ICMSTot.vFCP         := ObterConteudo(AuxNode.Childrens.Find('vFCP'), tcDe2);
    (*W05*)NFe.Total.ICMSTot.vBCST         := ObterConteudo(AuxNode.Childrens.Find('vBCST'), tcDe2);
    (*W06*)NFe.Total.ICMSTot.vST           := ObterConteudo(AuxNode.Childrens.Find('vST'), tcDe2);
    (*W06a*)NFe.Total.ICMSTot.vFCPST       := ObterConteudo(AuxNode.Childrens.Find('vFCPST'), tcDe2);
    (*W06b*)NFe.Total.ICMSTot.vFCPSTRet    := ObterConteudo(AuxNode.Childrens.Find('vFCPSTRet'), tcDe2);

    NFe.Total.ICMSTot.qBCMono := ObterConteudo(AuxNode.Childrens.Find('qBCMono'), tcDe2);
    NFe.Total.ICMSTot.vICMSMono := ObterConteudo(AuxNode.Childrens.Find('vICMSMono'), tcDe2);
    NFe.Total.ICMSTot.qBCMonoReten := ObterConteudo(AuxNode.Childrens.Find('qBCMonoReten'), tcDe2);
    NFe.Total.ICMSTot.vICMSMonoReten := ObterConteudo(AuxNode.Childrens.Find('vICMSMonoReten'), tcDe2);
    NFe.Total.ICMSTot.qBCMonoRet := ObterConteudo(AuxNode.Childrens.Find('qBCMonoRet'), tcDe2);
    NFe.Total.ICMSTot.vICMSMonoRet := ObterConteudo(AuxNode.Childrens.Find('vICMSMonoRet'), tcDe2);

    (*W07*)NFe.Total.ICMSTot.vProd         := ObterConteudo(AuxNode.Childrens.Find('vProd'), tcDe2);
    (*W08*)NFe.Total.ICMSTot.vFrete        := ObterConteudo(AuxNode.Childrens.Find('vFrete'), tcDe2);
    (*W09*)NFe.Total.ICMSTot.vSeg          := ObterConteudo(AuxNode.Childrens.Find('vSeg'), tcDe2);
    (*W10*)NFe.Total.ICMSTot.vDesc         := ObterConteudo(AuxNode.Childrens.Find('vDesc'), tcDe2);
    (*W11*)NFe.Total.ICMSTot.vII           := ObterConteudo(AuxNode.Childrens.Find('vII'), tcDe2);
    (*W12*)NFe.Total.ICMSTot.vIPI          := ObterConteudo(AuxNode.Childrens.Find('vIPI'), tcDe2);
    (*W12a*)NFe.Total.ICMSTot.vIPIDevol    := ObterConteudo(AuxNode.Childrens.Find('vIPIDevol'), tcDe2);
    (*W13*)NFe.Total.ICMSTot.vPIS          := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    (*W14*)NFe.Total.ICMSTot.vCOFINS       := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
    (*W15*)NFe.Total.ICMSTot.vOutro        := ObterConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
    (*W16*)NFe.Total.ICMSTot.vNF           := ObterConteudo(AuxNode.Childrens.Find('vNF'), tcDe2);
    (*W16a*)NFe.Total.ICMSTot.vTotTrib     := ObterConteudo(AuxNode.Childrens.Find('vTotTrib'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ISSQNtot');
  if (AuxNode <> nil) then
  begin
    (*W18*)NFe.Total.ISSQNtot.vServ   := ObterConteudo(AuxNode.Childrens.Find('vServ'), tcDe2);
    (*W19*)NFe.Total.ISSQNtot.vBC     := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*W20*)NFe.Total.ISSQNtot.vISS    := ObterConteudo(AuxNode.Childrens.Find('vISS'), tcDe2);
    (*W21*)NFe.Total.ISSQNtot.vPIS    := ObterConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    (*W22*)NFe.Total.ISSQNtot.vCOFINS := ObterConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);

    if NFe.infNFe.Versao >= 3 then
    begin
      (*W22a*)NFe.Total.ISSQNtot.dCompet     := ObterConteudo(AuxNode.Childrens.Find('dCompet'), tcDat);
      (*W22b*)NFe.Total.ISSQNtot.vDeducao    := ObterConteudo(AuxNode.Childrens.Find('vDeducao'), tcDe2);
      (*W22c*)NFe.Total.ISSQNtot.vOutro      := ObterConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
      (*W22d*)NFe.Total.ISSQNtot.vDescIncond := ObterConteudo(AuxNode.Childrens.Find('vDescIncond'), tcDe2);
      (*W22e*)NFe.Total.ISSQNtot.vDescCond   := ObterConteudo(AuxNode.Childrens.Find('vDescCond'), tcDe2);
      (*W22f*)NFe.Total.ISSQNtot.vISSRet     := ObterConteudo(AuxNode.Childrens.Find('vISSRet'), tcDe2);
      (*W22g*)NFe.Total.ISSQNtot.cRegTrib    := StrToRegTribISSQN(Ok, ObterConteudo(AuxNode.Childrens.Find('cRegTrib'), tcStr));
    end;
  end;

  AuxNode := ANode.Childrens.Find('retTrib');
  if (AuxNode <> nil) then
  begin
    (*W24*)NFe.Total.retTrib.vRetPIS    := ObterConteudo(AuxNode.Childrens.Find('vRetPIS'), tcDe2);
    (*W25*)NFe.Total.retTrib.vRetCOFINS := ObterConteudo(AuxNode.Childrens.Find('vRetCOFINS'), tcDe2);
    (*W26*)NFe.Total.retTrib.vRetCSLL   := ObterConteudo(AuxNode.Childrens.Find('vRetCSLL'), tcDe2);
    (*W27*)NFe.Total.retTrib.vBCIRRF    := ObterConteudo(AuxNode.Childrens.Find('vBCIRRF'), tcDe2);
    (*W28*)NFe.Total.retTrib.vIRRF      := ObterConteudo(AuxNode.Childrens.Find('vIRRF'), tcDe2);
    (*W29*)NFe.Total.retTrib.vBCRetPrev := ObterConteudo(AuxNode.Childrens.Find('vBCRetPrev'), tcDe2);
    (*W30*)NFe.Total.retTrib.vRetPrev   := ObterConteudo(AuxNode.Childrens.Find('vRetPrev'), tcDe2);
  end;
end;

procedure TNFeXmlReader.LerTransp(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*X02*)NFe.Transp.modFrete := StrToModFrete(ok, ObterConteudo(ANode.Childrens.Find('modFrete'), tcStr));
  (*X25a*)NFe.Transp.vagao   := ObterConteudo(ANode.Childrens.Find('vagao'), tcStr);
  (*X25b*)NFe.Transp.balsa   := ObterConteudo(ANode.Childrens.Find('balsa'), tcStr);

  AuxNode := ANode.Childrens.Find('transporta');
  if (AuxNode <> nil) then
  begin
    (*X04/X05*)NFe.Transp.Transporta.CNPJCPF := ObterCNPJCPF(AuxNode);
    (*X06*)NFe.Transp.Transporta.xNome       := ObterConteudo(AuxNode.Childrens.Find('xNome'), tcStr);
    (*X07*)NFe.Transp.Transporta.IE          := ObterConteudo(AuxNode.Childrens.Find('IE'), tcStr);
    (*X08*)NFe.Transp.Transporta.xEnder      := ObterConteudo(AuxNode.Childrens.Find('xEnder'), tcStr);
    (*X09*)NFe.Transp.Transporta.xMun        := ObterConteudo(AuxNode.Childrens.Find('xMun'), tcStr);
    (*X10*)NFe.Transp.Transporta.UF          := ObterConteudo(AuxNode.Childrens.Find('UF'), tcStr);
  end;

  AuxNode := ANode.Childrens.Find('retTransp');
  if (AuxNode <> nil) then
  begin
    (*X12*)NFe.Transp.retTransp.vServ    := ObterConteudo(AuxNode.Childrens.Find('vServ'), tcDe2);
    (*X13*)NFe.Transp.retTransp.vBCRet   := ObterConteudo(AuxNode.Childrens.Find('vBCRet'), tcDe2);
    (*X14*)NFe.Transp.retTransp.pICMSRet := ObterConteudo(AuxNode.Childrens.Find('pICMSRet'), tcDe2);
    (*X15*)NFe.Transp.retTransp.vICMSRet := ObterConteudo(AuxNode.Childrens.Find('vICMSRet'), tcDe2);
    (*X16*)NFe.Transp.retTransp.CFOP     := ObterConteudo(AuxNode.Childrens.Find('CFOP'), tcEsp);
    (*X17*)NFe.Transp.retTransp.cMunFG   := ObterConteudo(AuxNode.Childrens.Find('cMunFG'), tcStr);
  end;

  AuxNode := ANode.Childrens.Find('veicTransp');
  if (AuxNode <> nil) then
  begin
    (*X19*)NFe.Transp.veicTransp.placa := ObterConteudo(AuxNode.Childrens.Find('placa'), tcStr);
    (*X20*)NFe.Transp.veicTransp.UF    := ObterConteudo(AuxNode.Childrens.Find('UF'), tcStr);
    (*X21*)NFe.Transp.veicTransp.RNTC  := ObterConteudo(AuxNode.Childrens.Find('RNTC'), tcStr);
  end;

  NFe.Transp.Reboque.Clear;
  ANodes := ANode.Childrens.FindAll('reboque');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.Transp.Reboque.New;
    (*X23*) NFe.Transp.Reboque[i].placa := ObterConteudo(ANodes[i].Childrens.Find('placa'), tcStr);
    (*X24*) NFe.Transp.Reboque[i].UF    := ObterConteudo(ANodes[i].Childrens.Find('UF'), tcStr);
    (*X25*) NFe.Transp.Reboque[i].RNTC  := ObterConteudo(ANodes[i].Childrens.Find('RNTC'), tcStr);
  end;

  NFe.Transp.Vol.Clear;
  ANodes := ANode.Childrens.FindAll('vol');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerTranspVol(ANodes[i]);
  end;
end;

procedure TNFeXmlReader.LerTranspVol(const ANode: TACBrXmlNode);
Var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  Item: TVolCollectionItem;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NFe.Transp.Vol.New;
  (*X27*)Item.qVol  := ObterConteudo(ANode.Childrens.Find('qVol'), tcInt);
  (*X28*)Item.esp   := ObterConteudo(ANode.Childrens.Find('esp'), tcStr);
  (*X29*)Item.marca := ObterConteudo(ANode.Childrens.Find('marca'), tcStr);
  (*X30*)Item.nVol  := ObterConteudo(ANode.Childrens.Find('nVol'), tcStr);
  (*X31*)Item.pesoL := ObterConteudo(ANode.Childrens.Find('pesoL'), tcDe3);
  (*X32*)Item.pesoB := ObterConteudo(ANode.Childrens.Find('pesoB'), tcDe3);

  Item.lacres.Clear;
  ANodes := ANode.Childrens.FindAll('lacres');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Item.lacres.New;
    (*X34*)Item.lacres[i].nLacre := ObterConteudo(ANodes[i].Childrens.Find('nLacre'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerCobr(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
  tagPag: String;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('fat');
  if (AuxNode <> nil) then
  begin
    (*Y03*)NFe.Cobr.Fat.nFat  := ObterConteudo(AuxNode.Childrens.Find('nFat'), tcStr);
    (*Y04*)NFe.Cobr.Fat.vOrig := ObterConteudo(AuxNode.Childrens.Find('vOrig'), tcDe2);
    (*Y05*)NFe.Cobr.Fat.vDesc := ObterConteudo(AuxNode.Childrens.Find('vDesc'), tcDe2);
    (*Y06*)NFe.Cobr.Fat.vLiq  := ObterConteudo(AuxNode.Childrens.Find('vLiq'), tcDe2);
  end;

  NFe.Cobr.Dup.Clear;
  ANodes := ANode.Childrens.FindAll('dup');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.Cobr.Dup.New;
    (*Y08*)NFe.Cobr.Dup[i].nDup  := ObterConteudo(ANodes[i].Childrens.Find('nDup'), tcStr);
    (*Y09*)NFe.Cobr.Dup[i].dVenc := ObterConteudo(ANodes[i].Childrens.Find('dVenc'), tcDat);
    (*Y10*)NFe.Cobr.Dup[i].vDup  := ObterConteudo(ANodes[i].Childrens.Find('vDup'), tcDe2);
  end;

  if NFe.infNFe.Versao >= 3 then
  begin
    NFe.pag.Clear;
    if NFe.infNFe.Versao >= 4 then
    begin
      AuxNode := ANode.Childrens.Find('pag');
      if (AuxNode <> nil) then
        (*YA09*)NFe.pag.vTroco := ObterConteudo(AuxNode.Childrens.Find('vTroco'), tcDe2);

      tagPag := 'detPag';
    end
    else
      tagPag := 'pag';
    end;

   ANodes := ANode.Childrens.FindAll(tagPag);
   for i := 0 to Length(ANodes) - 1 do
   begin
     NFe.pag.New;
     (*YA01b*)NFe.pag[i].indPag := StrToIndpag(Ok, ObterConteudo(ANodes[i].Childrens.Find('indPag'), tcStr));
     (*YA02*)NFe.pag[i].tPag := StrToFormaPagamento(ok, ObterConteudo(ANodes[i].Childrens.Find('tPag'), tcStr));
     (*YA02a*)NFe.pag[i].xPag := ObterConteudo(ANodes[i].Childrens.Find('xPag'), tcStr));
     (*YA03*)NFe.pag[i].vPag := ObterConteudo(ANodes[i].Childrens.Find('vPag'), tcDe2);
             NFe.pag[i].dPag := ObterConteudo(ANodes[i].Childrens.Find('dPag'), tcDat);

             NFe.pag[i].CNPJPag := ObterConteudo(ANodes[i].Childrens.Find('CNPJPag'), tcStr));
             NFe.pag[i].UFPag := ObterConteudo(ANodes[i].Childrens.Find('UFPag'), tcStr));

     AuxNode := ANode.Childrens.Find('card');
     if (AuxNode <> nil) then
     begin
       (*YA04a*)NFe.pag[i].tpIntegra := StrTotpIntegra(ok, ObterConteudo(AuxNode.Childrens.Find('tpIntegra'), tcStr));
        (*YA05*)NFe.pag[i].CNPJ  := ObterConteudo(AuxNode.Childrens.Find('CNPJ'), tcStr);
        (*YA06*)NFe.pag[i].tBand := StrToBandeiraCartao(ok, ObterConteudo(AuxNode.Childrens.Find('tBand'), tcStr));
        (*YA07*)NFe.pag[i].cAut  := ObterConteudo(AuxNode.Childrens.Find('cAut'), tcStr);

               NFe.pag[i].CNPJReceb := ObterConteudo(AuxNode.Childrens.Find('CNPJReceb'), tcStr));
               NFe.pag[i].idTermPag := ObterConteudo(AuxNode.Childrens.Find('idTermPag'), tcStr));
     end;
   end;
end;

procedure TNFeXmlReader.LerInfAdic(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*Z02*)NFe.InfAdic.infAdFisco := ObterConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);
  (*Z03*)NFe.InfAdic.infCpl     := ObterConteudo(ANode.Childrens.Find('infCpl'), tcStr);

  NFe.InfAdic.obsCont.Clear;
  ANodes := ANode.Childrens.FindAll('obsCont');
  for i := 0 to Length(ANodes) - 1 do
    begin
      NFe.InfAdic.obsCont.New;
      (*Z05*)NFe.InfAdic.obsCont[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
      (*Z06*)NFe.InfAdic.obsCont[i].xTexto := ObterConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
    end;

  NFe.InfAdic.obsFisco.Clear;
  ANodes := ANode.Childrens.FindAll('obsFisco');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.InfAdic.obsFisco.New;
    (*Z08*)NFe.InfAdic.obsFisco[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
    (*Z09*)NFe.InfAdic.obsFisco[i].xTexto := ObterConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
  end;

  NFe.InfAdic.procRef.Clear;
  ANodes := ANode.Childrens.FindAll('procRef');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.InfAdic.obsCont.New;
    (*Z11*)NFe.InfAdic.procRef[i].nProc := ObterConteudo(ANodes[i].Childrens.Find('nProc'),tcStr);
    (*Z12*)NFe.InfAdic.procRef[i].indProc := StrToIndProc(ok, ObterConteudo(ANodes[i].Childrens.Find('indProc'), tcStr));
    (*Z13*)NFe.InfAdic.procRef[i].tpAto := StrTotpAto(ok, ObterConteudo(ANodes[i].Childrens.Find('tpAto'), tcStr));
  end;
end;

procedure TNFeXmlReader.LerInfIntermed(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFe.infIntermed.CNPJ         := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NFe.infIntermed.idCadIntTran := ObterConteudo(ANode.Childrens.Find('idCadIntTran'), tcStr);
end;

procedure TNFeXmlReader.LerExporta(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*ZA02*)NFe.exporta.UFembarq   := ObterConteudo(ANode.Childrens.Find('UFEmbarq'), tcStr);
  (*ZA03*)NFe.exporta.xLocEmbarq := ObterConteudo(ANode.Childrens.Find('xLocEmbarq'), tcStr);

  // Versao 3.10
  (*ZA02*)NFe.exporta.UFSaidaPais  := ObterConteudo(ANode.Childrens.Find('UFSaidaPais'), tcStr);
  (*ZA03*)NFe.exporta.xLocExporta  := ObterConteudo(ANode.Childrens.Find('xLocExporta'), tcStr);
  (*ZA04*)NFe.exporta.xLocDespacho := ObterConteudo(ANode.Childrens.Find('xLocDespacho'), tcStr);
end;

procedure TNFeXmlReader.LerCompra(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*ZB02*)NFe.compra.xNEmp := ObterConteudo(ANode.Childrens.Find('xNEmp'), tcStr);
  (*ZB03*)NFe.compra.xPed  := ObterConteudo(ANode.Childrens.Find('xPed'), tcStr);
  (*ZB04*)NFe.compra.xCont := ObterConteudo(ANode.Childrens.Find('xCont'), tcStr);
end;

procedure TNFeXmlReader.LerCana(const ANode: TACBrXmlNode);
Var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*ZC02*) NFe.cana.safra   := ObterConteudo(ANode.Childrens.Find('safra'), tcStr);
  (*ZC03*) NFe.cana.ref     := ObterConteudo(ANode.Childrens.Find('ref'), tcStr);
  (*ZC07*) NFe.cana.qTotMes := ObterConteudo(ANode.Childrens.Find('qTotMes'), tcDe10);
  (*ZC08*) NFe.cana.qTotAnt := ObterConteudo(ANode.Childrens.Find('qTotAnt'), tcDe10);
  (*ZC09*) NFe.cana.qTotGer := ObterConteudo(ANode.Childrens.Find('qTotGer'), tcDe10);
  (*ZC13*) NFe.cana.vFor    := ObterConteudo(ANode.Childrens.Find('vFor'), tcDe2);
  (*ZC14*) NFe.cana.vTotDed := ObterConteudo(ANode.Childrens.Find('vTotDed'), tcDe2);
  (*ZC15*) NFe.cana.vLiqFor := ObterConteudo(ANode.Childrens.Find('vLiqFor'), tcDe2);

  NFe.cana.fordia.Clear;
  ANodes := ANode.Childrens.FindAll('forDia');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.cana.fordia.New;
    (*ZC05*) NFe.cana.fordia[i].dia  := StrToInt(ANodes[i].Attributes.Items['dia'].Content);
    (*ZC06*) NFe.cana.fordia[i].qtde := ObterConteudo(ANodes[i].Childrens.Find('qtde'), tcDe10);
  end;

  NFe.cana.deduc.Clear;
  ANodes := ANode.Childrens.FindAll('deduc');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.cana.deduc.New;
    (*ZC05*) NFe.cana.deduc[i].xDed  := ObterConteudo(ANodes[i].Childrens.Find('xDed'), tcStr);
    (*ZC06*) NFe.cana.deduc[i].vDed := ObterConteudo(ANodes[i].Childrens.Find('vDed'), tcDe2);
  end;
end;

procedure TNFeXmlReader.LerInfRespTec(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFe.infRespTec.CNPJ     := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NFe.infRespTec.xContato := ObterConteudo(ANode.Childrens.Find('xContato'), tcStr);
  NFe.infRespTec.email    := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  NFe.infRespTec.fone     := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  NFe.infRespTec.idCSRT   := ObterConteudo(ANode.Childrens.Find('idCSRT'), tcInt);
  NFe.infRespTec.hashCSRT := ObterConteudo(ANode.Childrens.Find('hashCSRT'), tcStr);
end;

procedure TNFeXmlReader.LerInfNFeSupl(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFe.infNFeSupl.qrCode := ObterConteudo(ANode.Childrens.Find('qrCode'), tcStr);
  NFe.infNFeSupl.qrCode := StringReplace(NFe.infNFeSupl.qrCode, '<![CDATA[', '', []);
  NFe.infNFeSupl.qrCode := StringReplace(NFe.infNFeSupl.qrCode, ']]>', '', []);
  NFe.infNFeSupl.urlChave := ObterConteudo(ANode.Childrens.Find('urlChave'), tcStr);
end;

procedure TNFeXmlReader.LerSignature(const ANode: TACBrXmlNode);
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
      NFe.signature.URI             := AuxNode.Attributes.Items['URI'].Content;
      NFE.signature.DigestValue     := ObterConteudo(AuxNode.Childrens.Find('DigestValue'), tcStr);
    end;
  end;

  NFE.signature.SignatureValue  := ObterConteudo(ANode.Childrens.Find('SignatureValue'), tcStr);

  AuxNode := ANode.Childrens.Find('KeyInfo');
  if AuxNode <> nil then
  begin
    NFE.signature.X509Certificate := ObterConteudo(ANode.Childrens.Find('X509Certificate'), tcStr);
  end;
end;

end.

