{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010                                        }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
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
    procedure LerTotal(const ANode: TACBrXmlNode);
    procedure LerTransp(const ANode: TACBrXmlNode);
    procedure LerTranspVol(const ANode: TACBrXmlNode);
    procedure LerCobr(const ANode: TACBrXmlNode);
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
  pcnConversao, pcnConversaoNFe,
  ACBrUtil;

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

  NFe.procNFe.tpAmb    := StrToTpAmb(ok, ProcessarConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  NFe.procNFe.verAplic := ProcessarConteudo(ANode.Childrens.Find('verAplic'), tcStr);
  NFe.procNFe.chNFe    := ProcessarConteudo(ANode.Childrens.Find('chNFe'), tcStr);
  NFe.procNFe.dhRecbto := ProcessarConteudo(ANode.Childrens.Find('dhRecbto'), tcDatHor);
  NFe.procNFe.nProt    := ProcessarConteudo(ANode.Childrens.Find('nProt'), tcStr);
  NFe.procNFe.digVal   := ProcessarConteudo(ANode.Childrens.Find('digVal'), tcStr);
  NFe.procNFe.cStat    := ProcessarConteudo(ANode.Childrens.Find('cStat'), tcInt);
  NFe.procNFe.xMotivo  := ProcessarConteudo(ANode.Childrens.Find('xMotivo'), tcStr);
  NFe.procNFe.cMsg     := ProcessarConteudo(ANode.Childrens.Find('cMsg'), tcInt);
  NFe.procNFe.xMsg     := ProcessarConteudo(ANode.Childrens.Find('xMsg'), tcStr);
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
    NFe.autXML[i].CNPJCPF := ProcessarCNPJCPF(ANodes[i]);
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

procedure TNFeXmlReader.LerIde(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*B02*) NFe.ide.cUF := ProcessarConteudo(ANode.Childrens.Find('cUF'), tcInt);
  (*B03*) NFe.ide.cNF := ProcessarConteudo(ANode.Childrens.Find('cNF'), tcInt);
  if NFe.ide.cNF = 0 then
    NFe.ide.cNF := -2;
  (*B04*) NFe.ide.natOp  := ProcessarConteudo(ANode.Childrens.Find('natOp'), tcStr);
  (*B05*) NFe.ide.indPag := StrToIndpag(ok, ProcessarConteudo(ANode.Childrens.Find('indPag'), tcStr));
  (*B06*) NFe.ide.modelo := ProcessarConteudo(ANode.Childrens.Find('mod'), tcInt);
  (*B07*) NFe.ide.serie  := ProcessarConteudo(ANode.Childrens.Find('serie'), tcInt);
  (*B08*) NFe.ide.nNF    := ProcessarConteudo(ANode.Childrens.Find('nNF'), tcInt);

  if NFe.infNFe.Versao >= 3 then
  begin
    (*B09*) NFe.ide.dEmi    := ProcessarConteudo(ANode.Childrens.Find('dhEmi'), tcDatHor);
    (*B10*) NFe.ide.dSaiEnt := ProcessarConteudo(ANode.Childrens.Find('dhSaiEnt'), tcDatHor);
  end
  else
  begin
    (*B09*) NFe.ide.dEmi    := ProcessarConteudo(ANode.Childrens.Find('dEmi'), tcDat);
    (*B10*) NFe.ide.dSaiEnt := ProcessarConteudo(ANode.Childrens.Find('dSaiEnt'), tcDat);
    (*B10a*)NFe.ide.hSaiEnt := ProcessarConteudo(ANode.Childrens.Find('hSaiEnt'), tcHor);
  end;
  (*B11*) NFe.ide.tpNF := StrToTpNF(ok, ProcessarConteudo(ANode.Childrens.Find('tpNF'), tcStr));

  if NFe.infNFe.Versao >= 3 then
    (*B11a*)NFe.ide.idDest := StrToDestinoOperacao(ok, ProcessarConteudo(ANode.Childrens.Find('idDest'),  tcStr));

  (*B12*) NFe.ide.cMunFG := ProcessarConteudo(ANode.Childrens.Find('cMunFG'), tcInt);
  (*B21*) NFe.Ide.tpImp  := StrToTpImp(ok, ProcessarConteudo(ANode.Childrens.Find('tpImp'), tcStr));
  (*B22*) NFe.Ide.tpEmis := StrToTpEmis(ok, ProcessarConteudo(ANode.Childrens.Find('tpEmis'), tcStr));
  (*B23*) NFe.Ide.cDV    := ProcessarConteudo(ANode.Childrens.Find('cDV'), tcInt);
  (*B24*) NFe.Ide.tpAmb  := StrToTpAmb(ok, ProcessarConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  (*B25*) NFe.Ide.finNFe := StrToFinNFe(ok, ProcessarConteudo(ANode.Childrens.Find('finNFe'), tcStr));

  if NFe.infNFe.Versao >= 3 then
  begin
    (*B25a*)NFe.ide.indFinal := StrToConsumidorFinal(ok, ProcessarConteudo(ANode.Childrens.Find('indFinal'), tcStr));
    (*B25b*)NFe.ide.indPres  := StrToPresencaComprador(ok, ProcessarConteudo(ANode.Childrens.Find('indPres'), tcStr));
  end;

  (*B26*) NFe.Ide.procEmi := StrToProcEmi(ok, ProcessarConteudo(ANode.Childrens.Find('procEmi'), tcStr));
  (*B27*) NFe.Ide.verProc := ProcessarConteudo(ANode.Childrens.Find('verProc'), tcStr);
  (*B28*) NFe.Ide.dhCont  := ProcessarConteudo(ANode.Childrens.Find('dhCont'), tcDatHor);
  (*B29*) NFe.Ide.xJust   := ProcessarConteudo(ANode.Childrens.Find('xJust'), tcStr);

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
  (*B13*) NFe.ide.NFref[i].refNFe := ProcessarConteudo(ANode.Childrens.Find('refNFe'), tcEsp);

  refNode := ANode.Childrens.Find('refNF');
  if refNode <> nil then
  begin
    (*B15*) NFe.Ide.NFref[i].RefNF.cUF    := ProcessarConteudo(refNode.Childrens.Find('cUF'), tcInt);
    (*B16*) NFe.Ide.NFref[i].RefNF.AAMM   := ProcessarConteudo(refNode.Childrens.Find('AAMM'), tcEsp);
    (*B17*) NFe.Ide.NFref[i].RefNF.CNPJ   := ProcessarConteudo(refNode.Childrens.Find('CNPJ'), tcEsp);
    (*B18*) NFe.Ide.NFref[i].RefNF.Modelo := StrToIntDef(ProcessarConteudo(refNode.Childrens.Find('mod'), tcInt),55);
    (*B19*) NFe.ide.NFref[i].RefNF.serie  := ProcessarConteudo(refNode.Childrens.Find('serie'), tcInt);
    (*B20*) NFe.Ide.NFref[i].RefNF.nNF    := ProcessarConteudo(refNode.Childrens.Find('nNF'), tcInt);
  end;

  refNode := ANode.Childrens.Find('refNFP');
  if refNode <> nil then
  begin
    (*B20b*) NFe.Ide.NFref[i].RefNFP.cUF     := ProcessarConteudo(refNode.Childrens.Find('cUF'), tcInt);
    (*B20c*) NFe.Ide.NFref[i].RefNFP.AAMM    := ProcessarConteudo(refNode.Childrens.Find('AAMM'), tcEsp);
    (*B20d/B20e*) NFe.Ide.NFref[i].RefNFP.CNPJCPF := ProcessarCNPJCPF(refNode);
    (*B20f*) NFe.Ide.NFref[i].RefNFP.IE      := ProcessarConteudo(refNode.Childrens.Find('IE'), tcEsp);
    (*B20f*) NFe.Ide.NFref[i].RefNFP.Modelo  := ProcessarConteudo(refNode.Childrens.Find('mod'), tcInt);
    (*B20g*) NFe.ide.NFref[i].RefNFP.serie   := ProcessarConteudo(refNode.Childrens.Find('serie'), tcInt);
    (*B20h*) NFe.Ide.NFref[i].RefNFP.nNF     := ProcessarConteudo(refNode.Childrens.Find('nNF'), tcInt);

  end;

  (*B20i*)NFe.ide.NFref[i].refCTe := ProcessarConteudo(ANode.Childrens.Find('refCTe'), tcEsp);

  refNode := ANode.Childrens.Find('refECF');
  if refNode <> nil then
  begin
  (*B20k*)NFe.Ide.NFref[i].RefECF.modelo := StrToECFModRef(ok,ProcessarConteudo(refNode.Childrens.Find('mod'), tcStr)) ;
  (*B20l*)NFe.ide.NFref[i].RefECF.nECF := ProcessarConteudo(refNode.Childrens.Find('nECF'), tcStr);
  (*B20m*)NFe.Ide.NFref[i].RefECF.nCOO := ProcessarConteudo(refNode.Childrens.Find('nCOO'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerEmit(const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*C02/C02a*)NFe.Emit.CNPJCPF := ProcessarCNPJCPF(ANode);
  (*C03*)NFe.Emit.xNome        := ProcessarConteudo(ANode.Childrens.Find('xNome'), tcStr);
  (*C04*)NFe.Emit.xFant        := ProcessarConteudo(ANode.Childrens.Find('xFant'), tcStr);
  (*C17*)NFe.Emit.IE           := ProcessarConteudo(ANode.Childrens.Find('IE'), tcStr);
  (*C18*)NFe.Emit.IEST         := ProcessarConteudo(ANode.Childrens.Find('IEST'), tcStr);
  (*C19*)NFe.Emit.IM           := ProcessarConteudo(ANode.Childrens.Find('IM'), tcStr);
  (*C20*)NFe.Emit.CNAE         := ProcessarConteudo(ANode.Childrens.Find('CNAE'), tcStr);
  (*C21*)NFe.Emit.CRT          := StrToCRT(ok, ProcessarConteudo(ANode.Childrens.Find('CRT'), tcStr));
  LerEmitEnderEmit(ANode.Childrens.Find('enderEmit'));
end;

procedure TNFeXmlReader.LerEmitEnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*C06*)NFe.Emit.enderEmit.xLgr    := ProcessarConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*C07*)NFe.Emit.enderEmit.nro     := ProcessarConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*C08*)NFe.Emit.enderEmit.xCpl    := ProcessarConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*C09*)NFe.Emit.enderEmit.xBairro := ProcessarConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*C10*)NFe.Emit.EnderEmit.cMun    := ProcessarConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*C11*)NFe.Emit.enderEmit.xMun    := ProcessarConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*C12*)NFe.Emit.enderEmit.UF      := ProcessarConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*C13*)NFe.Emit.enderEmit.CEP     := ProcessarConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*C14*)NFe.Emit.enderEmit.cPais   := ProcessarConteudo(ANode.Childrens.Find('cPais'), tcInt);

  if NFe.Emit.enderEmit.cPais = 0 then
    NFe.Emit.enderEmit.cPais := 1058;

  (*C15*)NFe.Emit.enderEmit.xPais   := ProcessarConteudo(ANode.Childrens.Find('xPais'), tcStr);

  if NFe.Emit.enderEmit.xPais = '' then
    NFe.Emit.enderEmit.xPais := 'BRASIL';

  (*C16*)NFe.Emit.enderEmit.fone    := ProcessarConteudo(ANode.Childrens.Find('fone'), tcStr);
end;

procedure TNFeXmlReader.LerAvulsa(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*D02*)NFe.Avulsa.CNPJ    := ProcessarConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  (*D03*)NFe.Avulsa.xOrgao  := ProcessarConteudo(ANode.Childrens.Find('xOrgao'), tcStr);
  (*D04*)NFe.Avulsa.matr    := ProcessarConteudo(ANode.Childrens.Find('matr'), tcStr);
  (*D05*)NFe.Avulsa.xAgente := ProcessarConteudo(ANode.Childrens.Find('xAgente'), tcStr);
  (*D06*)NFe.Avulsa.fone    := ProcessarConteudo(ANode.Childrens.Find('fone'), tcStr);
  (*D07*)NFe.Avulsa.UF      := ProcessarConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*D08*)NFe.Avulsa.nDAR    := ProcessarConteudo(ANode.Childrens.Find('nDAR'), tcStr);
  (*D09*)NFe.Avulsa.dEmi    := ProcessarConteudo(ANode.Childrens.Find('dEmi'), tcDat);
  (*D10*)NFe.Avulsa.vDAR    := ProcessarConteudo(ANode.Childrens.Find('vDAR'), tcDe2);
  (*D11*)NFe.Avulsa.repEmi  := ProcessarConteudo(ANode.Childrens.Find('repEmi'), tcStr);
  (*D12*)NFe.Avulsa.dPag    := ProcessarConteudo(ANode.Childrens.Find('dPag'), tcDat);
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

  (*E02/E03*)NFe.Dest.CNPJCPF := ProcessarCNPJCPF(ANode);

  if NFe.infNFe.Versao >= 3 then
    (*E03a*)NFe.Dest.idEstrangeiro := ProcessarConteudo(ANode.Childrens.Find('idEstrangeiro'), tcStr);

  (*E04*)NFe.Dest.xNome := ProcessarConteudo(ANode.Childrens.Find('xNome'), tcStr);

  if NFe.infNFe.Versao >= 3 then
    (*E16a*)NFe.Dest.indIEDest := StrToindIEDest(Ok, ProcessarConteudo(ANode.Childrens.Find('indIEDest'), tcStr));

  (*E17*)NFe.Dest.IE := ProcessarConteudo(ANode.Childrens.Find('IE'), tcStr);
  (*E18*)NFe.Dest.ISUF := ProcessarConteudo(ANode.Childrens.Find('ISUF'), tcStr);

  if NFe.infNFe.Versao >= 3 then
    (*E18a*)NFe.Dest.IM := ProcessarConteudo(ANode.Childrens.Find('IM'), tcStr);

  (*E19*)NFe.Dest.Email := ProcessarConteudo(ANode.Childrens.Find('email'), tcStr);

  LerDestEnderDest(ANode.Childrens.Find('enderDest'));
end;

procedure TNFeXmlReader.LerDestEnderDest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*E06*)NFe.Dest.enderDest.xLgr    := ProcessarConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*E07*)NFe.Dest.enderDest.nro     := ProcessarConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*E08*)NFe.Dest.enderDest.xCpl    := ProcessarConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*E09*)NFe.Dest.enderDest.xBairro := ProcessarConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*E10*)NFe.Dest.enderDest.cMun    := ProcessarConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*E11*)NFe.Dest.enderDest.xMun    := ProcessarConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*E12*)NFe.Dest.enderDest.UF      := ProcessarConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*E13*)NFe.Dest.enderDest.CEP     := ProcessarConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*E14*)NFe.Dest.enderDest.cPais   := ProcessarConteudo(ANode.Childrens.Find('cPais'), tcInt);

  if NFe.Dest.enderDest.cPais = 0 then
    NFe.Dest.enderDest.cPais := 1058;

  (*E15*)NFe.Dest.enderDest.xPais   := ProcessarConteudo(ANode.Childrens.Find('xPais'), tcStr);

  if NFe.Dest.enderDest.xPais = '' then
    NFe.Dest.enderDest.xPais := 'BRASIL';

  (*E16*)NFe.Dest.enderDest.fone    := ProcessarConteudo(ANode.Childrens.Find('fone'), tcStr);
end;

procedure TNFeXmlReader.LerRetirada(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*F02/F02a*)NFe.Retirada.CNPJCPF := ProcessarCNPJCPF(ANode);
  (*F02b*)NFe.Retirada.xNome       := ProcessarConteudo(ANode.Childrens.Find('xNome'), tcStr);
  (*F03*)NFe.Retirada.xLgr         := ProcessarConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*F04*)NFe.Retirada.nro          := ProcessarConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*F05*)NFe.Retirada.xCpl         := ProcessarConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*F06*)NFe.Retirada.xBairro      := ProcessarConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*F07*)NFe.Retirada.cMun         := ProcessarConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*F08*)NFe.Retirada.xMun         := ProcessarConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*F09*)NFe.Retirada.UF           := ProcessarConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*F10*)NFe.Retirada.CEP          := ProcessarConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*F11*)NFe.Retirada.cPais        := ProcessarConteudo(ANode.Childrens.Find('cPais'), tcInt);
  (*F12*)NFe.Retirada.xPais        := ProcessarConteudo(ANode.Childrens.Find('xPais'), tcStr);
  (*F13*)NFe.Retirada.fone         := ProcessarConteudo(ANode.Childrens.Find('fone'), tcStr);
  (*F14*)NFe.Retirada.Email        := ProcessarConteudo(ANode.Childrens.Find('email'), tcStr);
  (*F15*)NFe.Retirada.IE           := ProcessarConteudo(ANode.Childrens.Find('IE'), tcStr);
end;

procedure TNFeXmlReader.LerEntrega(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*G02/G02a*)NFe.Entrega.CNPJCPF := ProcessarCNPJCPF(ANode);
  (*G02b*)NFe.Entrega.xNome       := ProcessarConteudo(ANode.Childrens.Find('xNome'), tcStr);
  (*G03*)NFe.Entrega.xLgr         := ProcessarConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  (*G04*)NFe.Entrega.nro          := ProcessarConteudo(ANode.Childrens.Find('nro'), tcStr);
  (*G05*)NFe.Entrega.xCpl         := ProcessarConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  (*G06*)NFe.Entrega.xBairro      := ProcessarConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  (*G07*)NFe.Entrega.cMun         := ProcessarConteudo(ANode.Childrens.Find('cMun'), tcInt);
  (*G08*)NFe.Entrega.xMun         := ProcessarConteudo(ANode.Childrens.Find('xMun'), tcStr);
  (*G09*)NFe.Entrega.UF           := ProcessarConteudo(ANode.Childrens.Find('UF'), tcStr);
  (*G10*)NFe.Entrega.CEP          := ProcessarConteudo(ANode.Childrens.Find('CEP'), tcInt);
  (*G11*)NFe.Entrega.cPais        := ProcessarConteudo(ANode.Childrens.Find('cPais'), tcInt);
  (*G12*)NFe.Entrega.xPais        := ProcessarConteudo(ANode.Childrens.Find('xPais'), tcStr);
  (*G13*)NFe.Entrega.fone         := ProcessarConteudo(ANode.Childrens.Find('fone'), tcStr);
  (*G14*)NFe.Entrega.Email        := ProcessarConteudo(ANode.Childrens.Find('email'), tcStr);
  (*G15*)NFe.Entrega.IE           := ProcessarConteudo(ANode.Childrens.Find('IE'), tcStr);
end;

procedure TNFeXmlReader.LerDet(const ANode: TACBrXmlNode);
Var
  Item: TDetCollectionItem;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := NFe.Det.New;
  (*   *)Item.prod.nItem := NFe.Det.Count;
  (*V01*)Item.infAdProd  := ProcessarConteudo(ANode.Childrens.Find('infAdProd'), tcStr);

  LerDetProd(Item, ANode.Childrens.Find('prod'));
  LerDetImposto(Item, ANode.Childrens.Find('imposto'));

  AuxNode := ANode.Childrens.Find('impostoDevol');
  if (AuxNode <> nil) then
  begin
    (*U51*)Item.pDevol := ProcessarConteudo(AuxNode.Childrens.Find('pDevol'), tcDe2);

    AuxNode := AuxNode.Childrens.Find('IPI');
    if (AuxNode <> nil) then
    begin
      (*U61*)Item.vIPIDevol := ProcessarConteudo(AuxNode.Childrens.Find('vIPIDevol'), tcDe2);
    end;
  end;
end;

procedure TNFeXmlReader.LerDetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*I02*)Item.Prod.cProd := ProcessarConteudo(ANode.Childrens.Find('cProd'), tcStr);
  (*I03*)Item.Prod.cEAN  := ProcessarConteudo(ANode.Childrens.Find('cEAN'), tcStr);
  (*I04*)Item.Prod.xProd := ProcessarConteudo(ANode.Childrens.Find('xProd'), tcStr);
  (*I05*)Item.Prod.NCM   := ProcessarConteudo(ANode.Childrens.Find('NCM'), tcStr);
  (*I05w*)Item.Prod.CEST := ProcessarConteudo(ANode.Childrens.Find('CEST'), tcStr);
  if NFe.infNFe.Versao >= 4 then
  begin
    (*I05d*)Item.Prod.indEscala := StrToindEscala(ok, ProcessarConteudo(ANode.Childrens.Find('indEscala'), tcStr));
    (*I05e*)Item.Prod.CNPJFab   := ProcessarConteudo(ANode.Childrens.Find('CNPJFab'), tcStr);
    (*I05f*)Item.Prod.cBenef    := ProcessarConteudo(ANode.Childrens.Find('cBenef'), tcStr);
  end;
  (*I06*)Item.Prod.EXTIPI   := ProcessarConteudo(ANode.Childrens.Find('EXTIPI'), tcStr);
  (*I08*)Item.Prod.CFOP     := ProcessarConteudo(ANode.Childrens.Find('CFOP'), tcEsp);
  (*I09*)Item.Prod.uCom     := ProcessarConteudo(ANode.Childrens.Find('uCom'), tcStr);
  (*I10*)Item.Prod.qCom     := ProcessarConteudo(ANode.Childrens.Find('qCom'), tcDe4);
  (*I10a*)Item.Prod.vUnCom  := ProcessarConteudo(ANode.Childrens.Find('vUnCom'), tcDe10);
  (*I11*)Item.Prod.vProd    := ProcessarConteudo(ANode.Childrens.Find('vProd'), tcDe2);
  (*I12*)Item.Prod.cEANTrib := ProcessarConteudo(ANode.Childrens.Find('cEANTrib'), tcStr);
  (*I13*)Item.Prod.uTrib    := ProcessarConteudo(ANode.Childrens.Find('uTrib'), tcStr);
  (*I14*)Item.Prod.qTrib    := ProcessarConteudo(ANode.Childrens.Find('qTrib'), tcDe4);
  (*I14a*)Item.Prod.vUnTrib := ProcessarConteudo(ANode.Childrens.Find('vUnTrib'), tcDe10);
  (*I15*)Item.Prod.vFrete   := ProcessarConteudo(ANode.Childrens.Find('vFrete'), tcDe2);
  (*I16*)Item.Prod.vSeg     := ProcessarConteudo(ANode.Childrens.Find('vSeg'), tcDe2);
  (*I17*)Item.Prod.vDesc    := ProcessarConteudo(ANode.Childrens.Find('vDesc'), tcDe2);
  (*I17a*)Item.Prod.vOutro  := ProcessarConteudo(ANode.Childrens.Find('vOutro'), tcDe2);
  (*I17b*)Item.Prod.IndTot  := StrToindTot(ok,ProcessarConteudo(ANode.Childrens.Find('indTot'), tcDe2));
  (*I30*)Item.Prod.xPed     := ProcessarConteudo(ANode.Childrens.Find('xPed'), tcStr);
  (*I31*)Item.Prod.nItemPed := ProcessarConteudo(ANode.Childrens.Find('nItemPed'), tcStr);
  (*I31*)Item.Prod.nRECOPI  := ProcessarConteudo(ANode.Childrens.Find('nRECOPI'), tcStr);
  (*I70*)Item.Prod.nFCI     := ProcessarConteudo(ANode.Childrens.Find('nFCI'), tcStr);

  ANodes := ANode.Childrens.FindAll('NVE');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Item.Prod.NVE.New;
    Item.Prod.NVE.Items[i].NVE := ProcessarConteudo(ANodes[i].Childrens.Find('NVE'), tcStr);
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
  (*I19*)DIItem.nDI        := ProcessarConteudo(ANode.Childrens.Find('nDI'), tcStr);
  (*I20*)DIItem.dDI        := ProcessarConteudo(ANode.Childrens.Find('dDI'), tcDat);
  (*I21*)DIItem.xLocDesemb := ProcessarConteudo(ANode.Childrens.Find('xLocDesemb'), tcStr);
  (*I22*)DIItem.UFDesemb   := ProcessarConteudo(ANode.Childrens.Find('UFDesemb'), tcStr);
  (*I23*)DIItem.dDesemb    := ProcessarConteudo(ANode.Childrens.Find('dDesemb'), tcDat);

  (*I23a*)DIItem.tpViaTransp  := StrToTipoViaTransp(Ok, ProcessarConteudo(ANode.Childrens.Find('tpViaTransp'), tcInt));
  (*I23b*)DIItem.vAFRMM       := ProcessarConteudo(ANode.Childrens.Find('vAFRMM'), tcDe2);
  (*I23c*)DIItem.tpIntermedio := StrToTipoIntermedio(Ok, ProcessarConteudo(ANode.Childrens.Find('tpIntermedio'), tcInt));
  (*I23d*)DIItem.CNPJ         := ProcessarCNPJCPF(ANode);
  (*I23e*)DIItem.UFTerceiro   := ProcessarConteudo(ANode.Childrens.Find('UFTerceiro'), tcStr);

  (*I24*)DIItem.cExportador   := ProcessarConteudo(ANode.Childrens.Find('cExportador'), tcStr);

  DIItem.adi.Clear;
  ANodes := ANode.Childrens.FindAll('adi');
  for i := 0 to Length(ANodes) - 1 do
  begin
    AdiItem := DIItem.adi.New;
    (*I26*)AdiItem.nAdicao     := ProcessarConteudo(ANodes[i].Childrens.Find('nAdicao'), tcInt);
    (*I27*)AdiItem.nSeqAdi     := ProcessarConteudo(ANodes[i].Childrens.Find('nSeqAdic'), tcInt);
    (*I28*)AdiItem.cFabricante := ProcessarConteudo(ANodes[i].Childrens.Find('cFabricante'), tcStr);
    (*I29*)AdiItem.vDescDI     := ProcessarConteudo(ANodes[i].Childrens.Find('vDescDI'), tcDe2);
    (*I29a*)AdiItem.nDraw      := ProcessarConteudo(ANodes[i].Childrens.Find('nDraw'), tcStr);
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
  (*I51*)DetExportItem.nDraw := ProcessarConteudo(ANode.Childrens.Find('nDraw'), tcStr);

  AuxNode := ANode.Childrens.Find('exportInd');
  if (AuxNode <> nil) then
  begin
    (*I53*)DetExportItem.nRE     := ProcessarConteudo(AuxNode.Childrens.Find('nRE'), tcStr);
    (*I54*)DetExportItem.chNFe   := ProcessarConteudo(AuxNode.Childrens.Find('chNFe'), tcStr);
    (*I55*)DetExportItem.qExport := ProcessarConteudo(AuxNode.Childrens.Find('qExport'), tcDe4);
  end;
end;

procedure TNFeXmlReader.LerDetProdRastro(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  RastroItem: TRastroCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  RastroItem := Item.Prod.rastro.New;
  (*I81*)RastroItem.nLote  := ProcessarConteudo(ANode.Childrens.Find('nLote'), tcStr);
  (*I82*)RastroItem.qLote  := ProcessarConteudo(ANode.Childrens.Find('qLote'), tcDe3);
  (*I83*)RastroItem.dFab   := ProcessarConteudo(ANode.Childrens.Find('dFab'), tcDat);
  (*I84*)RastroItem.dVal   := ProcessarConteudo(ANode.Childrens.Find('dVal'), tcDat);
  (*I85*)RastroItem.cAgreg := ProcessarConteudo(ANode.Childrens.Find('cAgreg'), tcStr);
end;

procedure TNFeXmlReader.LerDetProdVeicProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*J02*)Item.Prod.veicProd.tpOP         := StrToTpOP(ok, ProcessarConteudo(ANode.Childrens.Find('tpOp'), tcStr));
  (*J03*)Item.Prod.veicProd.chassi       := ProcessarConteudo(ANode.Childrens.Find('chassi'), tcStr);
  (*J04*)Item.Prod.veicProd.cCor         := ProcessarConteudo(ANode.Childrens.Find('cCor'), tcStr);
  (*J05*)Item.Prod.veicProd.xCor         := ProcessarConteudo(ANode.Childrens.Find('xCor'), tcStr);
  (*J06*)Item.Prod.veicProd.pot          := ProcessarConteudo(ANode.Childrens.Find('pot'), tcStr);
  (*J07*)Item.Prod.veicProd.Cilin        := ProcessarConteudo(ANode.Childrens.Find('cilin'), tcStr);
  (*J08*)Item.Prod.veicProd.pesoL        := ProcessarConteudo(ANode.Childrens.Find('pesoL'), tcStr);
  (*J09*)Item.Prod.veicProd.pesoB        := ProcessarConteudo(ANode.Childrens.Find(''), tcStr);
  (*J10*)Item.Prod.veicProd.nSerie       := ProcessarConteudo(ANode.Childrens.Find('nSerie'), tcStr);
  (*J11*)Item.Prod.veicProd.tpComb       := ProcessarConteudo(ANode.Childrens.Find('tpComb'), tcStr);
  (*J12*)Item.Prod.veicProd.nMotor       := ProcessarConteudo(ANode.Childrens.Find('nMotor'), tcStr);
  (*J13*)Item.Prod.veicProd.CMT          := ProcessarConteudo(ANode.Childrens.Find('CMT'), tcStr);
  (*J14*)Item.Prod.veicProd.dist         := ProcessarConteudo(ANode.Childrens.Find('dist'), tcStr);
  //(*J15*)Item.Prod.veicProd.RENAVAM := ProcessarConteudo(ANode.Childrens.Find('RENAVAM'), tcEsp);
  (*J16*)Item.Prod.veicProd.anoMod       := ProcessarConteudo(ANode.Childrens.Find('anoMod'), tcInt);
  (*J17*)Item.Prod.veicProd.anoFab       := ProcessarConteudo(ANode.Childrens.Find('anoFab'), tcInt);
  (*J18*)Item.Prod.veicProd.tpPint       := ProcessarConteudo(ANode.Childrens.Find('tpPint'), tcStr);
  (*J19*)Item.Prod.veicProd.tpVeic       := ProcessarConteudo(ANode.Childrens.Find('tpVeic'), tcInt);
  (*J20*)Item.Prod.veicProd.espVeic      := ProcessarConteudo(ANode.Childrens.Find('espVeic'), tcInt);
  (*J21*)Item.Prod.veicProd.VIN          := ProcessarConteudo(ANode.Childrens.Find('VIN'), tcStr);
  (*J22*)Item.Prod.veicProd.condVeic     := StrToCondVeic(ok, ProcessarConteudo(ANode.Childrens.Find('condVeic'), tcStr));
  (*J23*)Item.Prod.veicProd.cMod         := ProcessarConteudo(ANode.Childrens.Find('cMod'), tcStr);
  (*J24*)Item.Prod.veicProd.cCorDENATRAN := ProcessarConteudo(ANode.Childrens.Find('cCorDENATRAN'), tcStr);
  (*J25*)Item.Prod.veicProd.lota         := ProcessarConteudo(ANode.Childrens.Find('lota'), tcInt);
  (*J26*)Item.Prod.veicProd.tpRest       := ProcessarConteudo(ANode.Childrens.Find('tpRest'), tcInt);
end;

procedure TNFeXmlReader.LerDetProdMed(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  MedItem: TMedCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  MedItem := Item.Prod.med.New;
  (*K01a*)MedItem.cProdANVISA := ProcessarConteudo(ANode.Childrens.Find('cProdANVISA'), tcStr);
  (*K01b*)MedItem.xMotivoIsencao := ProcessarConteudo(ANode.Childrens.Find('xMotivoIsencao'), tcStr);
  (*K02*)MedItem.nLote := ProcessarConteudo(ANode.Childrens.Find('nLote'), tcStr);
  (*K03*)MedItem.qLote := ProcessarConteudo(ANode.Childrens.Find('qLote'), tcDe3);
  (*K04*)MedItem.dFab  := ProcessarConteudo(ANode.Childrens.Find('dFab'), tcDat);
  (*K05*)MedItem.dVal  := ProcessarConteudo(ANode.Childrens.Find('dVal'), tcDat);
  (*K06*)MedItem.vPMC  := ProcessarConteudo(ANode.Childrens.Find('vPMC'), tcDe2);
end;

procedure TNFeXmlReader.LerDetProdArma(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  ArmaItem: TArmaCollectionItem;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  ArmaItem := Item.Prod.arma.New;
  (*L02*)ArmaItem.tpArma := StrToTpArma(ok, ProcessarConteudo(ANode.Childrens.Find('tpArma'), tcStr));
  (*L03*)ArmaItem.nSerie := ProcessarConteudo(ANode.Childrens.Find('nSerie'), tcStr);
  (*L04*)ArmaItem.nCano  := ProcessarConteudo(ANode.Childrens.Find('nCano'), tcStr);
  (*L05*)ArmaItem.descr  := ProcessarConteudo(ANode.Childrens.Find('descr'), tcStr);
end;

procedure TNFeXmlReader.LerDetProdComb(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*L102*)Item.Prod.comb.cProdANP := ProcessarConteudo(ANode.Childrens.Find('cProdANP'), tcInt);
  (*L102a*)Item.Prod.comb.pMixGN  := ProcessarConteudo(ANode.Childrens.Find('qMixGN'), tcDe4);
  (*LA03*)Item.Prod.comb.descANP  := ProcessarConteudo(ANode.Childrens.Find('descANP'), tcStr);
  (*LA03a*)Item.Prod.comb.pGLP    := ProcessarConteudo(ANode.Childrens.Find('pGLP'), tcDe4);
  (*LA03b*)Item.Prod.comb.pGNn    := ProcessarConteudo(ANode.Childrens.Find('pGNn'), tcDe4);
  (*LA03c*)Item.Prod.comb.pGNi    := ProcessarConteudo(ANode.Childrens.Find('pGNi'), tcDe4);
  (*LA03d*)Item.Prod.comb.vPart   := ProcessarConteudo(ANode.Childrens.Find('vPart'), tcDe2);
  (*LA04*)Item.Prod.comb.CODIF    := ProcessarConteudo(ANode.Childrens.Find('CODIF'), tcEsp);
  (*LA05*)Item.Prod.comb.qTemp    := ProcessarConteudo(ANode.Childrens.Find('qTemp'), tcDe4);
  (*LA06*)Item.Prod.comb.UFcons   := ProcessarConteudo(ANode.Childrens.Find('UFCons'), tcStr);
  (*L120*)Item.Prod.comb.ICMSCons.UFcons := ProcessarConteudo(ANode.Childrens.Find('UFcons'), tcStr);

  AuxNode := ANode.Childrens.Find('CIDE');
  if (AuxNode <> nil) then
  begin
    (*L106*)Item.Prod.comb.CIDE.qBCprod   := ProcessarConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*L107*)Item.Prod.comb.CIDE.vAliqProd := ProcessarConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    (*L108*)Item.Prod.comb.CIDE.vCIDE     := ProcessarConteudo(AuxNode.Childrens.Find('vCIDE'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('encerrante');
  if (AuxNode <> nil) then
  begin
    (*LA12*)Item.Prod.comb.encerrante.nBico   := ProcessarConteudo(AuxNode.Childrens.Find('nBico'), tcInt);
    (*LA13*)Item.Prod.comb.encerrante.nBomba  := ProcessarConteudo(AuxNode.Childrens.Find('nBomba'), tcInt);
    (*LA14*)Item.Prod.comb.encerrante.nTanque := ProcessarConteudo(AuxNode.Childrens.Find('nTanque'), tcInt);
    (*LA15*)Item.Prod.comb.encerrante.vEncIni := ProcessarConteudo(AuxNode.Childrens.Find('vEncIni'), tcDe3);
    (*LA16*)Item.Prod.comb.encerrante.vEncFin := ProcessarConteudo(AuxNode.Childrens.Find('vEncFin'), tcDe3);
  end;

  AuxNode := ANode.Childrens.Find('ICMSComb');
  if (AuxNode <> nil) then
  begin
    (*L110*)Item.Prod.comb.ICMS.vBCICMS   := ProcessarConteudo(AuxNode.Childrens.Find('vBCICMS'), tcDe2);
    (*L111*)Item.Prod.comb.ICMS.vICMS     := ProcessarConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    (*L112*)Item.Prod.comb.ICMS.vBCICMSST := ProcessarConteudo(AuxNode.Childrens.Find('vBCICMSST'), tcDe2);
    (*L113*)Item.Prod.comb.ICMS.vICMSST   := ProcessarConteudo(AuxNode.Childrens.Find('vICMSST'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ICMSInter');
  if (AuxNode <> nil) then
  begin
    (*L115*)Item.Prod.comb.ICMSInter.vBCICMSSTDest := ProcessarConteudo(AuxNode.Childrens.Find('vBCICMSSTDest'), tcDe2);
    (*L116*)Item.Prod.comb.ICMSInter.vICMSSTDest   := ProcessarConteudo(AuxNode.Childrens.Find('vICMSSTDest'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ICMSCons');
  if (AuxNode <> nil) then
  begin
    (*L118*)Item.Prod.comb.ICMSCons.vBCICMSSTCons := ProcessarConteudo(AuxNode.Childrens.Find('vBCICMSSTCons'), tcDe2);
    (*L119*)Item.Prod.comb.ICMSCons.vICMSSTCons   := ProcessarConteudo(AuxNode.Childrens.Find('vICMSSTCons'), tcDe2);
    (*L119*)Item.Prod.comb.ICMSCons.UFcons        := ProcessarConteudo(AuxNode.Childrens.Find('UFCons'), tcStr);
  end;
end;

procedure TNFeXmlReader.LerDetImposto(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
Var
  ok: Boolean;
  AuxNode, AxNode: TACBrXmlNode;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*M02*)Item.Imposto.vTotTrib := ProcessarConteudo(ANode.Childrens.Find('vTotTrib'), tcDe2);

  AuxNode := ANode.Childrens.Find('ICMS');
  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];
  if (AuxNode <> nil) then
  begin
    (*N11*)Item.Imposto.ICMS.orig        := StrToOrig(ok, ProcessarConteudo(AuxNode.Childrens.Find('orig'), tcStr));
    (*N12*)Item.Imposto.ICMS.CST         := StrToCSTICMS(ok, ProcessarConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    (*N12a*)Item.Imposto.ICMS.CSOSN      := StrToCSOSNIcms( ok,ProcessarConteudo(AuxNode.Childrens.Find('CSOSN'), tcInt));
    (*N13*)Item.Imposto.ICMS.modBC       := StrToModBC(ok, ProcessarConteudo(AuxNode.Childrens.Find('modBC'), tcStr));
    (*N14*)Item.Imposto.ICMS.pRedBC      := ProcessarConteudo(AuxNode.Childrens.Find('pRedBC'), tcDe2);
    (*N15*)Item.Imposto.ICMS.vBC         := ProcessarConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*N16*)Item.Imposto.ICMS.pICMS       := ProcessarConteudo(AuxNode.Childrens.Find('pICMS'), tcDe2);
    (*N16a*)Item.Imposto.ICMS.vICMSOp    := ProcessarConteudo(AuxNode.Childrens.Find('vICMSOp'), tcDe2);
    (*N16b*)Item.Imposto.ICMS.pDif       := ProcessarConteudo(AuxNode.Childrens.Find('pDif'), tcDe4);
    (*N16c*)Item.Imposto.ICMS.vICMSDif   := ProcessarConteudo(AuxNode.Childrens.Find('vICMSDif'), tcDe2);
    (*N17*)Item.Imposto.ICMS.vICMS       := ProcessarConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    (*N17a*)Item.Imposto.ICMS.vBCFCP     := ProcessarConteudo(AuxNode.Childrens.Find('vBCFCP'), tcDe2);
    (*N17b*)Item.Imposto.ICMS.pFCP       := ProcessarConteudo(AuxNode.Childrens.Find('pFCP'), tcDe4);
    (*N17c*)Item.Imposto.ICMS.vFCP       := ProcessarConteudo(AuxNode.Childrens.Find('vFCP'), tcDe2);
    (*N18*)Item.Imposto.ICMS.modBCST     := StrToModBCST(ok, ProcessarConteudo(AuxNode.Childrens.Find('modBCST'), tcStr));
    (*N19*)Item.Imposto.ICMS.pMVAST      := ProcessarConteudo(AuxNode.Childrens.Find('pMVAST'), tcDe2);
    (*N20*)Item.Imposto.ICMS.pRedBCST    := ProcessarConteudo(AuxNode.Childrens.Find('pRedBCST'), tcDe2);
    (*N21*)Item.Imposto.ICMS.vBCST       := ProcessarConteudo(AuxNode.Childrens.Find('vBCST'), tcDe2);
    (*N22*)Item.Imposto.ICMS.pICMSST     := ProcessarConteudo(AuxNode.Childrens.Find('pICMSST'), tcDe2);
    (*N23*)Item.Imposto.ICMS.vICMSST     := ProcessarConteudo(AuxNode.Childrens.Find('vICMSST'), tcDe2);
    (*N23a*)Item.Imposto.ICMS.vBCFCPST   := ProcessarConteudo(AuxNode.Childrens.Find('vBCFCPST'), tcDe2);
    (*N23b*)Item.Imposto.ICMS.pFCPST     := ProcessarConteudo(AuxNode.Childrens.Find('pFCPST'), tcDe4);
    (*N23d*)Item.Imposto.ICMS.vFCPST     := ProcessarConteudo(AuxNode.Childrens.Find('vFCPST'), tcDe2);
    (*N24*)Item.Imposto.ICMS.UFST        := ProcessarConteudo(AuxNode.Childrens.Find('UFST'), tcStr);
    (*N25*)Item.Imposto.ICMS.pBCOp       := ProcessarConteudo(AuxNode.Childrens.Find('pBCOp'), tcDe2);
    (*N26*)Item.Imposto.ICMS.vBCSTRet    := ProcessarConteudo(AuxNode.Childrens.Find('vBCSTRet'), tcDe2);
    (*N27*)Item.Imposto.ICMS.vICMSSTRet  := ProcessarConteudo(AuxNode.Childrens.Find('vICMSSTRet'), tcDe2);
    (*N27a*)Item.Imposto.ICMS.vICMSDeson := ProcessarConteudo(AuxNode.Childrens.Find('vICMSDeson'), tcDe2);
    (*N27a*)Item.Imposto.ICMS.vBCFCPSTRet:= ProcessarConteudo(AuxNode.Childrens.Find('vBCFCPSTRet'), tcDe2);
    (*N27b*)Item.Imposto.ICMS.pFCPSTRet  := ProcessarConteudo(AuxNode.Childrens.Find('pFCPSTRet'), tcDe4);
    (*N27d*)Item.Imposto.ICMS.vFCPSTRet  := ProcessarConteudo(AuxNode.Childrens.Find('vFCPSTRet'), tcDe2);
    (*N27e*)Item.Imposto.ICMS.pST        := ProcessarConteudo(AuxNode.Childrens.Find('pST'), tcDe4);
    (*N28*)Item.Imposto.ICMS.motDesICMS  := StrTomotDesICMS(ok, ProcessarConteudo(AuxNode.Childrens.Find('motDesICMS'), tcStr));
    (*N29*)Item.Imposto.ICMS.pCredSN     := ProcessarConteudo(AuxNode.Childrens.Find('pCredSN'), tcDe2);
    (*N30*)Item.Imposto.ICMS.vCredICMSSN := ProcessarConteudo(AuxNode.Childrens.Find('vCredICMSSN'), tcDe2);
    (*N31*)Item.Imposto.ICMS.vBCSTDest   := ProcessarConteudo(AuxNode.Childrens.Find('vBCSTDest'), tcDe2);
    (*N32*)Item.Imposto.ICMS.vICMSSTDest := ProcessarConteudo(AuxNode.Childrens.Find('vICMSSTDest'), tcDe2);
    (*N34*)Item.Imposto.ICMS.pRedBCEfet  := ProcessarConteudo(AuxNode.Childrens.Find('pRedBCEfet'), tcDe4);
    (*N35*)Item.Imposto.ICMS.vBCEfet     := ProcessarConteudo(AuxNode.Childrens.Find('vBCEfet'), tcDe2);
    (*N36*)Item.Imposto.ICMS.pICMSEfet   := ProcessarConteudo(AuxNode.Childrens.Find('pICMSEfet'), tcDe4);
    (*N37*)Item.Imposto.ICMS.vICMSEfet   := ProcessarConteudo(AuxNode.Childrens.Find('vICMSEfet'), tcDe2);
    (*N26b*)Item.Imposto.ICMS.vICMSSubstituto := ProcessarConteudo(AuxNode.Childrens.Find('vICMSSubstituto'), tcDe2);

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
    (*NA03*)Item.Imposto.ICMSUFDest.vBCUFDest      := ProcessarConteudo(AuxNode.Childrens.Find('vBCUFDest'), tcDe2);
    (*NA04*)Item.Imposto.ICMSUFDest.vBCFCPUFDest   := ProcessarConteudo(AuxNode.Childrens.Find('vBCFCPUFDest'), tcDe2);
    (*NA05*)Item.Imposto.ICMSUFDest.pFCPUFDest     := ProcessarConteudo(AuxNode.Childrens.Find('pFCPUFDest'), tcDe2);
    (*NA07*)Item.Imposto.ICMSUFDest.pICMSUFDest    := ProcessarConteudo(AuxNode.Childrens.Find('pICMSUFDest'), tcDe2);
    (*NA09*)Item.Imposto.ICMSUFDest.pICMSInter     := ProcessarConteudo(AuxNode.Childrens.Find('pICMSInter'), tcDe2);
    (*NA11*)Item.Imposto.ICMSUFDest.pICMSInterPart := ProcessarConteudo(AuxNode.Childrens.Find('pICMSInterPart'), tcDe2);
    (*NA13*)Item.Imposto.ICMSUFDest.vFCPUFDest     := ProcessarConteudo(AuxNode.Childrens.Find('vFCPUFDest'), tcDe2);
    (*NA15*)Item.Imposto.ICMSUFDest.vICMSUFDest    := ProcessarConteudo(AuxNode.Childrens.Find('vICMSUFDest'), tcDe2);
    (*NA17*)Item.Imposto.ICMSUFDest.vICMSUFRemet   := ProcessarConteudo(AuxNode.Childrens.Find('vICMSUFRemet'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('IPI');
  if (AuxNode <> nil) then
  begin
    (*O02*)Item.Imposto.IPI.clEnq    := ProcessarConteudo(AuxNode.Childrens.Find('clEnq'), tcStr);
    (*O03*)Item.Imposto.IPI.CNPJProd := ProcessarConteudo(AuxNode.Childrens.Find('CNPJProd'), tcStr);
    (*O04*)Item.Imposto.IPI.cSelo    := ProcessarConteudo(AuxNode.Childrens.Find('cSelo'), tcStr);
    (*O05*)Item.Imposto.IPI.qSelo    := ProcessarConteudo(AuxNode.Childrens.Find('qSelo'), tcInt);
    (*O06*)Item.Imposto.IPI.cEnq     := ProcessarConteudo(AuxNode.Childrens.Find('cEnq'), tcStr);

    // Inicializa CST com sendo Não tributada e conforme o TIPO entrada ou saida
    // Caso a Tag não seja informada sera gravada com sendo não tributada
    if NFe.ide.tpNF = tnEntrada then
      Item.Imposto.IPI.CST := ipi53;
    if NFe.ide.tpNF = tnSaida then
      Item.Imposto.IPI.CST := ipi03;

    AxNode := AuxNode.Childrens.Find('IPITrib');
    if (AxNode <> nil) then
    begin
      (*O09*)Item.Imposto.IPI.CST   := StrToCSTIPI(ok, ProcessarConteudo(AxNode.Childrens.Find('CST'), tcStr));
      (*O10*)Item.Imposto.IPI.vBC   := ProcessarConteudo(AxNode.Childrens.Find('vBC'), tcDe2);
      (*O11*)Item.Imposto.IPI.qUnid := ProcessarConteudo(AxNode.Childrens.Find('qUnid'), tcDe4);
      (*O12*)Item.Imposto.IPI.vUnid := ProcessarConteudo(AxNode.Childrens.Find('vUnid'), tcDe4);
      (*O13*)Item.Imposto.IPI.pIPI  := ProcessarConteudo(AxNode.Childrens.Find('pIPI'), tcDe2);
      (*O14*)Item.Imposto.IPI.vIPI  := ProcessarConteudo(AxNode.Childrens.Find('vIPI'), tcDe2);
    end;

    AxNode := AuxNode.Childrens.Find('IPINT');
    if (AxNode <> nil) then
    begin
      (*O09*)Item.Imposto.IPI.CST := StrToCSTIPI(ok, ProcessarConteudo(AxNode.Childrens.Find('CST'), tcStr));
    end;
  end;

  AuxNode := ANode.Childrens.Find('II');
  if (AuxNode <> nil) then
  begin
    (*P02*)Item.Imposto.II.vBc      := ProcessarConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*P03*)Item.Imposto.II.vDespAdu := ProcessarConteudo(AuxNode.Childrens.Find('vDespAdu'), tcDe2);
    (*P04*)Item.Imposto.II.vII      := ProcessarConteudo(AuxNode.Childrens.Find('vII'), tcDe2);
    (*P05*)Item.Imposto.II.vIOF     := ProcessarConteudo(AuxNode.Childrens.Find('vIOF'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('PIS');
  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];
  if (AuxNode <> nil) then
  begin
    (*Q06*)Item.Imposto.PIS.CST       := StrToCSTPIS(ok, ProcessarConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    (*Q07*)Item.Imposto.PIS.vBC       := ProcessarConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*Q08*)Item.Imposto.PIS.pPIS      := ProcessarConteudo(AuxNode.Childrens.Find('pPIS'), tcDe2);
    (*Q09*)Item.Imposto.PIS.vPIS      := ProcessarConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    (*Q10*)Item.Imposto.PIS.qBCProd   := ProcessarConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*Q11*)Item.Imposto.PIS.vAliqProd := ProcessarConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
  end;

  AuxNode := ANode.Childrens.Find('PISST');
  if (AuxNode <> nil) then
  begin
    (*R02*)Item.Imposto.PISST.vBc       := ProcessarConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*R03*)Item.Imposto.PISST.pPis      := ProcessarConteudo(AuxNode.Childrens.Find('pPIS'), tcDe2);
    (*R04*)Item.Imposto.PISST.qBCProd   := ProcessarConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*R05*)Item.Imposto.PISST.vAliqProd := ProcessarConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    (*R06*)Item.Imposto.PISST.vPIS      := ProcessarConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('COFINS');
  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];
  if (AuxNode <> nil) then
  begin
    (*S06*)Item.Imposto.COFINS.CST       := StrToCSTCOFINS(ok, ProcessarConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    (*S07*)Item.Imposto.COFINS.vBC       := ProcessarConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*S08*)Item.Imposto.COFINS.pCOFINS   := ProcessarConteudo(AuxNode.Childrens.Find('pCOFINS'), tcDe2);
    (*S09*)Item.Imposto.COFINS.qBCProd   := ProcessarConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*S10*)Item.Imposto.COFINS.vAliqProd := ProcessarConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    (*S11*)Item.Imposto.COFINS.vCOFINS   := ProcessarConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('COFINSST');
  if (AuxNode <> nil) then
  begin
    (*T02*)Item.Imposto.COFINSST.vBC       := ProcessarConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*T03*)Item.Imposto.COFINSST.pCOFINS   := ProcessarConteudo(AuxNode.Childrens.Find('pCOFINS'), tcDe2);
    (*T04*)Item.Imposto.COFINSST.qBCProd   := ProcessarConteudo(AuxNode.Childrens.Find('qBCProd'), tcDe4);
    (*T05*)Item.Imposto.COFINSST.vAliqProd := ProcessarConteudo(AuxNode.Childrens.Find('vAliqProd'), tcDe4);
    (*T06*)Item.Imposto.COFINSST.vCOFINS   := ProcessarConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ISSQN');
  if (AuxNode <> nil) then
  begin
    (*U02*)Item.Imposto.ISSQN.vBC       := ProcessarConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*U03*)Item.Imposto.ISSQN.vAliq     := ProcessarConteudo(AuxNode.Childrens.Find('vAliq'), tcDe2);
    (*U04*)Item.Imposto.ISSQN.vISSQN    := ProcessarConteudo(AuxNode.Childrens.Find('vISSQN'), tcDe2);
    (*U05*)Item.Imposto.ISSQN.cMunFG    := ProcessarConteudo(AuxNode.Childrens.Find('cMunFG'), tcInt);
    (*U06*)Item.Imposto.ISSQN.cListServ := ProcessarConteudo(AuxNode.Childrens.Find('cListServ'), tcStr);
    (*U07*)Item.Imposto.ISSQN.cSitTrib  := StrToISSQNcSitTrib( ok,  ProcessarConteudo(AuxNode.Childrens.Find('cSitTrib'), tcStr) ) ;
    (*U07*)Item.Imposto.ISSQN.vDeducao     := ProcessarConteudo(AuxNode.Childrens.Find('vDeducao'), tcDe2);
    (*U08*)Item.Imposto.ISSQN.vOutro       := ProcessarConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
    (*U09*)Item.Imposto.ISSQN.vDescIncond  := ProcessarConteudo(AuxNode.Childrens.Find('vDescIncond'), tcDe2);
    (*U10*)Item.Imposto.ISSQN.vDescCond    := ProcessarConteudo(AuxNode.Childrens.Find('vDescCond'), tcDe2);
    (*U12*)Item.Imposto.ISSQN.vISSRet      := ProcessarConteudo(AuxNode.Childrens.Find('vISSRet'), tcDe2);
    (*U13*)Item.Imposto.ISSQN.indISS       := StrToindISS(Ok, ProcessarConteudo(AuxNode.Childrens.Find('indISS'), tcStr));
    (*U14*)Item.Imposto.ISSQN.cServico     := ProcessarConteudo(AuxNode.Childrens.Find('cServico'), tcStr);
    (*U15*)Item.Imposto.ISSQN.cMun         := ProcessarConteudo(AuxNode.Childrens.Find('cMun'), tcInt);
    (*U16*)Item.Imposto.ISSQN.cPais        := ProcessarConteudo(AuxNode.Childrens.Find('cPais'), tcInt);
    (*U17*)Item.Imposto.ISSQN.nProcesso    := ProcessarConteudo(AuxNode.Childrens.Find('nProcesso'), tcStr);
    (*U18*)Item.Imposto.ISSQN.indIncentivo := StrToindIncentivo(Ok, ProcessarConteudo(AuxNode.Childrens.Find('indIncentivo'), tcStr));
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
    (*W03*)NFe.Total.ICMSTot.vBC           := ProcessarConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*W04*)NFe.Total.ICMSTot.vICMS         := ProcessarConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    (*W04a*)NFe.Total.ICMSTot.vICMSDeson   := ProcessarConteudo(AuxNode.Childrens.Find('vICMSDeson'), tcDe2);
    (*W04c*)NFe.Total.ICMSTot.vFCPUFDest   := ProcessarConteudo(AuxNode.Childrens.Find('vFCPUFDest'), tcDe2);
    (*W04e*)NFe.Total.ICMSTot.vICMSUFDest  := ProcessarConteudo(AuxNode.Childrens.Find('vICMSUFDest'), tcDe2);
    (*W04g*)NFe.Total.ICMSTot.vICMSUFRemet := ProcessarConteudo(AuxNode.Childrens.Find('vICMSUFRemet'), tcDe2);
    (*W04h*)NFe.Total.ICMSTot.vFCP         := ProcessarConteudo(AuxNode.Childrens.Find('vFCP'), tcDe2);
    (*W05*)NFe.Total.ICMSTot.vBCST         := ProcessarConteudo(AuxNode.Childrens.Find('vBCST'), tcDe2);
    (*W06*)NFe.Total.ICMSTot.vST           := ProcessarConteudo(AuxNode.Childrens.Find('vST'), tcDe2);
    (*W06a*)NFe.Total.ICMSTot.vFCPST       := ProcessarConteudo(AuxNode.Childrens.Find('vFCPST'), tcDe2);
    (*W06b*)NFe.Total.ICMSTot.vFCPSTRet    := ProcessarConteudo(AuxNode.Childrens.Find('vFCPSTRet'), tcDe2);
    (*W07*)NFe.Total.ICMSTot.vProd         := ProcessarConteudo(AuxNode.Childrens.Find('vProd'), tcDe2);
    (*W08*)NFe.Total.ICMSTot.vFrete        := ProcessarConteudo(AuxNode.Childrens.Find('vFrete'), tcDe2);
    (*W09*)NFe.Total.ICMSTot.vSeg          := ProcessarConteudo(AuxNode.Childrens.Find('vSeg'), tcDe2);
    (*W10*)NFe.Total.ICMSTot.vDesc         := ProcessarConteudo(AuxNode.Childrens.Find('vDesc'), tcDe2);
    (*W11*)NFe.Total.ICMSTot.vII           := ProcessarConteudo(AuxNode.Childrens.Find('vII'), tcDe2);
    (*W12*)NFe.Total.ICMSTot.vIPI          := ProcessarConteudo(AuxNode.Childrens.Find('vIPI'), tcDe2);
    (*W12a*)NFe.Total.ICMSTot.vIPIDevol    := ProcessarConteudo(AuxNode.Childrens.Find('vIPIDevol'), tcDe2);
    (*W13*)NFe.Total.ICMSTot.vPIS          := ProcessarConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    (*W14*)NFe.Total.ICMSTot.vCOFINS       := ProcessarConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);
    (*W15*)NFe.Total.ICMSTot.vOutro        := ProcessarConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
    (*W16*)NFe.Total.ICMSTot.vNF           := ProcessarConteudo(AuxNode.Childrens.Find('vNF'), tcDe2);
    (*W16a*)NFe.Total.ICMSTot.vTotTrib     := ProcessarConteudo(AuxNode.Childrens.Find('vTotTrib'), tcDe2);
  end;

  AuxNode := ANode.Childrens.Find('ISSQNtot');
  if (AuxNode <> nil) then
  begin
    (*W18*)NFe.Total.ISSQNtot.vServ   := ProcessarConteudo(AuxNode.Childrens.Find('vServ'), tcDe2);
    (*W19*)NFe.Total.ISSQNtot.vBC     := ProcessarConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    (*W20*)NFe.Total.ISSQNtot.vISS    := ProcessarConteudo(AuxNode.Childrens.Find('vISS'), tcDe2);
    (*W21*)NFe.Total.ISSQNtot.vPIS    := ProcessarConteudo(AuxNode.Childrens.Find('vPIS'), tcDe2);
    (*W22*)NFe.Total.ISSQNtot.vCOFINS := ProcessarConteudo(AuxNode.Childrens.Find('vCOFINS'), tcDe2);

    if NFe.infNFe.Versao >= 3 then
    begin
      (*W22a*)NFe.Total.ISSQNtot.dCompet     := ProcessarConteudo(AuxNode.Childrens.Find('dCompet'), tcDat);
      (*W22b*)NFe.Total.ISSQNtot.vDeducao    := ProcessarConteudo(AuxNode.Childrens.Find('vDeducao'), tcDe2);
      (*W22c*)NFe.Total.ISSQNtot.vOutro      := ProcessarConteudo(AuxNode.Childrens.Find('vOutro'), tcDe2);
      (*W22d*)NFe.Total.ISSQNtot.vDescIncond := ProcessarConteudo(AuxNode.Childrens.Find('vDescIncond'), tcDe2);
      (*W22e*)NFe.Total.ISSQNtot.vDescCond   := ProcessarConteudo(AuxNode.Childrens.Find('vDescCond'), tcDe2);
      (*W22f*)NFe.Total.ISSQNtot.vISSRet     := ProcessarConteudo(AuxNode.Childrens.Find('vISSRet'), tcDe2);
      (*W22g*)NFe.Total.ISSQNtot.cRegTrib    := StrToRegTribISSQN(Ok, ProcessarConteudo(AuxNode.Childrens.Find('cRegTrib'), tcStr));
    end;
  end;

  AuxNode := ANode.Childrens.Find('retTrib');
  if (AuxNode <> nil) then
  begin
    (*W24*)NFe.Total.retTrib.vRetPIS    := ProcessarConteudo(AuxNode.Childrens.Find('vRetPIS'), tcDe2);
    (*W25*)NFe.Total.retTrib.vRetCOFINS := ProcessarConteudo(AuxNode.Childrens.Find('vRetCOFINS'), tcDe2);
    (*W26*)NFe.Total.retTrib.vRetCSLL   := ProcessarConteudo(AuxNode.Childrens.Find('vRetCSLL'), tcDe2);
    (*W27*)NFe.Total.retTrib.vBCIRRF    := ProcessarConteudo(AuxNode.Childrens.Find('vBCIRRF'), tcDe2);
    (*W28*)NFe.Total.retTrib.vIRRF      := ProcessarConteudo(AuxNode.Childrens.Find('vIRRF'), tcDe2);
    (*W29*)NFe.Total.retTrib.vBCRetPrev := ProcessarConteudo(AuxNode.Childrens.Find('vBCRetPrev'), tcDe2);
    (*W30*)NFe.Total.retTrib.vRetPrev   := ProcessarConteudo(AuxNode.Childrens.Find('vRetPrev'), tcDe2);
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

  (*X02*)NFe.Transp.modFrete := StrToModFrete(ok, ProcessarConteudo(ANode.Childrens.Find('modFrete'), tcStr));
  (*X25a*)NFe.Transp.vagao   := ProcessarConteudo(ANode.Childrens.Find('vagao'), tcStr);
  (*X25b*)NFe.Transp.balsa   := ProcessarConteudo(ANode.Childrens.Find('balsa'), tcStr);

  AuxNode := ANode.Childrens.Find('transporta');
  if (AuxNode <> nil) then
  begin
    (*X04/X05*)NFe.Transp.Transporta.CNPJCPF := ProcessarCNPJCPF(AuxNode);
    (*X06*)NFe.Transp.Transporta.xNome       := ProcessarConteudo(AuxNode.Childrens.Find('xNome'), tcStr);
    (*X07*)NFe.Transp.Transporta.IE          := ProcessarConteudo(AuxNode.Childrens.Find('IE'), tcStr);
    (*X08*)NFe.Transp.Transporta.xEnder      := ProcessarConteudo(AuxNode.Childrens.Find('xEnder'), tcStr);
    (*X09*)NFe.Transp.Transporta.xMun        := ProcessarConteudo(AuxNode.Childrens.Find('xMun'), tcStr);
    (*X10*)NFe.Transp.Transporta.UF          := ProcessarConteudo(AuxNode.Childrens.Find('UF'), tcStr);
  end;

  AuxNode := ANode.Childrens.Find('retTransp');
  if (AuxNode <> nil) then
  begin
    (*X12*)NFe.Transp.retTransp.vServ    := ProcessarConteudo(AuxNode.Childrens.Find('vServ'), tcDe2);
    (*X13*)NFe.Transp.retTransp.vBCRet   := ProcessarConteudo(AuxNode.Childrens.Find('vBCRet'), tcDe2);
    (*X14*)NFe.Transp.retTransp.pICMSRet := ProcessarConteudo(AuxNode.Childrens.Find('pICMSRet'), tcDe2);
    (*X15*)NFe.Transp.retTransp.vICMSRet := ProcessarConteudo(AuxNode.Childrens.Find('vICMSRet'), tcDe2);
    (*X16*)NFe.Transp.retTransp.CFOP     := ProcessarConteudo(AuxNode.Childrens.Find('CFOP'), tcEsp);
    (*X17*)NFe.Transp.retTransp.cMunFG   := ProcessarConteudo(AuxNode.Childrens.Find('cMunFG'), tcStr);
  end;

  AuxNode := ANode.Childrens.Find('veicTransp');
  if (AuxNode <> nil) then
  begin
    (*X19*)NFe.Transp.veicTransp.placa := ProcessarConteudo(AuxNode.Childrens.Find('placa'), tcStr);
    (*X20*)NFe.Transp.veicTransp.UF    := ProcessarConteudo(AuxNode.Childrens.Find('UF'), tcStr);
    (*X21*)NFe.Transp.veicTransp.RNTC  := ProcessarConteudo(AuxNode.Childrens.Find('RNTC'), tcStr);
  end;

  NFe.Transp.Reboque.Clear;
  ANodes := ANode.Childrens.FindAll('reboque');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.Transp.Reboque.New;
    (*X23*) NFe.Transp.Reboque[i].placa := ProcessarConteudo(ANodes[i].Childrens.Find('placa'), tcStr);
    (*X24*) NFe.Transp.Reboque[i].UF    := ProcessarConteudo(ANodes[i].Childrens.Find('UF'), tcStr);
    (*X25*) NFe.Transp.Reboque[i].RNTC  := ProcessarConteudo(ANodes[i].Childrens.Find('RNTC'), tcStr);
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
  (*X27*)Item.qVol  := ProcessarConteudo(ANode.Childrens.Find('qVol'), tcInt);
  (*X28*)Item.esp   := ProcessarConteudo(ANode.Childrens.Find('esp'), tcStr);
  (*X29*)Item.marca := ProcessarConteudo(ANode.Childrens.Find('marca'), tcStr);
  (*X30*)Item.nVol  := ProcessarConteudo(ANode.Childrens.Find('nVol'), tcStr);
  (*X31*)Item.pesoL := ProcessarConteudo(ANode.Childrens.Find('pesoL'), tcDe3);
  (*X32*)Item.pesoB := ProcessarConteudo(ANode.Childrens.Find('pesoB'), tcDe3);

  Item.lacres.Clear;
  ANodes := ANode.Childrens.FindAll('lacres');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Item.lacres.New;
    (*X34*)Item.lacres[i].nLacre := ProcessarConteudo(ANodes[i].Childrens.Find('nLacre'), tcStr);
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
    (*Y03*)NFe.Cobr.Fat.nFat  := ProcessarConteudo(AuxNode.Childrens.Find('nFat'), tcStr);
    (*Y04*)NFe.Cobr.Fat.vOrig := ProcessarConteudo(AuxNode.Childrens.Find('vOrig'), tcDe2);
    (*Y05*)NFe.Cobr.Fat.vDesc := ProcessarConteudo(AuxNode.Childrens.Find('vDesc'), tcDe2);
    (*Y06*)NFe.Cobr.Fat.vLiq  := ProcessarConteudo(AuxNode.Childrens.Find('vLiq'), tcDe2);
  end;

  NFe.Cobr.Dup.Clear;
  ANodes := ANode.Childrens.FindAll('dup');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.Cobr.Dup.New;
    (*Y08*)NFe.Cobr.Dup[i].nDup  := ProcessarConteudo(ANodes[i].Childrens.Find('nDup'), tcStr);
    (*Y09*)NFe.Cobr.Dup[i].dVenc := ProcessarConteudo(ANodes[i].Childrens.Find('dVenc'), tcDat);
    (*Y10*)NFe.Cobr.Dup[i].vDup  := ProcessarConteudo(ANodes[i].Childrens.Find('vDup'), tcDe2);
  end;

  if NFe.infNFe.Versao >= 3 then
  begin
    NFe.pag.Clear;
    if NFe.infNFe.Versao >= 4 then
    begin
      AuxNode := ANode.Childrens.Find('pag');
      if (AuxNode <> nil) then
        (*YA09*)NFe.pag.vTroco := ProcessarConteudo(AuxNode.Childrens.Find('vTroco'), tcDe2);

      tagPag := 'detPag';
    end
    else
      tagPag := 'pag';
    end;

   ANodes := ANode.Childrens.FindAll(tagPag);
   for i := 0 to Length(ANodes) - 1 do
   begin
     NFe.pag.New;
     (*YA01b*)NFe.pag[i].indPag := StrToIndpag(Ok, ProcessarConteudo(ANodes[i].Childrens.Find('indPag'), tcStr));
     (*YA02*)NFe.pag[i].tPag := StrToFormaPagamento(ok, ProcessarConteudo(ANodes[i].Childrens.Find('tPag'), tcStr));
     (*YA03*)NFe.pag[i].vPag := ProcessarConteudo(ANodes[i].Childrens.Find('vPag'), tcDe2);

     AuxNode := ANode.Childrens.Find('card');
     if (AuxNode <> nil) then
     begin
       (*YA04a*)NFe.pag[i].tpIntegra := StrTotpIntegra(ok, ProcessarConteudo(AuxNode.Childrens.Find('tpIntegra'), tcStr));
        (*YA05*)NFe.pag[i].CNPJ  := ProcessarConteudo(AuxNode.Childrens.Find('CNPJ'), tcStr);
        (*YA06*)NFe.pag[i].tBand := StrToBandeiraCartao(ok, ProcessarConteudo(AuxNode.Childrens.Find('tBand'), tcStr));
        (*YA07*)NFe.pag[i].cAut  := ProcessarConteudo(AuxNode.Childrens.Find('cAut'), tcStr);
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

  (*Z02*)NFe.InfAdic.infAdFisco := ProcessarConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);
  (*Z03*)NFe.InfAdic.infCpl     := ProcessarConteudo(ANode.Childrens.Find('infCpl'), tcStr);

  NFe.InfAdic.obsCont.Clear;
  ANodes := ANode.Childrens.FindAll('obsCont');
  for i := 0 to Length(ANodes) - 1 do
    begin
      NFe.InfAdic.obsCont.New;
      (*Z05*)NFe.InfAdic.obsCont[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
      (*Z06*)NFe.InfAdic.obsCont[i].xTexto := ProcessarConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
    end;

  NFe.InfAdic.obsCont.Clear;
  ANodes := ANode.Childrens.FindAll('obsFisco');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.InfAdic.obsFisco.New;
    (*Z08*)NFe.InfAdic.obsFisco[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
    (*Z09*)NFe.InfAdic.obsFisco[i].xTexto := ProcessarConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
  end;

  NFe.InfAdic.procRef.Clear;
  ANodes := ANode.Childrens.FindAll('procRef');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.InfAdic.obsCont.New;
     (*Z11*)NFe.InfAdic.procRef[i].nProc   := ProcessarConteudo(ANodes[i].Childrens.Find('nProc'),tcStr);
     (*Z12*)NFe.InfAdic.procRef[i].indProc := StrToIndProc(ok, ProcessarConteudo(ANodes[i].Childrens.Find('indProc'), tcStr));
  end;
end;

procedure TNFeXmlReader.LerExporta(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*ZA02*)NFe.exporta.UFembarq   := ProcessarConteudo(ANode.Childrens.Find('UFEmbarq'), tcStr);
  (*ZA03*)NFe.exporta.xLocEmbarq := ProcessarConteudo(ANode.Childrens.Find('xLocEmbarq'), tcStr);

  // Versao 3.10
  (*ZA02*)NFe.exporta.UFSaidaPais  := ProcessarConteudo(ANode.Childrens.Find('UFSaidaPais'), tcStr);
  (*ZA03*)NFe.exporta.xLocExporta  := ProcessarConteudo(ANode.Childrens.Find('xLocExporta'), tcStr);
  (*ZA04*)NFe.exporta.xLocDespacho := ProcessarConteudo(ANode.Childrens.Find('xLocDespacho'), tcStr);
end;

procedure TNFeXmlReader.LerCompra(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*ZB02*)NFe.compra.xNEmp := ProcessarConteudo(ANode.Childrens.Find('xNEmp'), tcStr);
  (*ZB03*)NFe.compra.xPed  := ProcessarConteudo(ANode.Childrens.Find('xPed'), tcStr);
  (*ZB04*)NFe.compra.xCont := ProcessarConteudo(ANode.Childrens.Find('xCont'), tcStr);
end;

procedure TNFeXmlReader.LerCana(const ANode: TACBrXmlNode);
Var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  (*ZC02*) NFe.cana.safra   := ProcessarConteudo(ANode.Childrens.Find('safra'), tcStr);
  (*ZC03*) NFe.cana.ref     := ProcessarConteudo(ANode.Childrens.Find('ref'), tcStr);
  (*ZC07*) NFe.cana.qTotMes := ProcessarConteudo(ANode.Childrens.Find('qTotMes'), tcDe10);
  (*ZC08*) NFe.cana.qTotAnt := ProcessarConteudo(ANode.Childrens.Find('qTotAnt'), tcDe10);
  (*ZC09*) NFe.cana.qTotGer := ProcessarConteudo(ANode.Childrens.Find('qTotGer'), tcDe10);
  (*ZC13*) NFe.cana.vFor    := ProcessarConteudo(ANode.Childrens.Find('vFor'), tcDe2);
  (*ZC14*) NFe.cana.vTotDed := ProcessarConteudo(ANode.Childrens.Find('vTotDed'), tcDe2);
  (*ZC15*) NFe.cana.vLiqFor := ProcessarConteudo(ANode.Childrens.Find('vLiqFor'), tcDe2);

  NFe.cana.fordia.Clear;
  ANodes := ANode.Childrens.FindAll('forDia');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.cana.fordia.New;
    (*ZC05*) NFe.cana.fordia[i].dia  := StrToInt(ANodes[i].Attributes.Items['dia'].Content);
    (*ZC06*) NFe.cana.fordia[i].qtde := ProcessarConteudo(ANodes[i].Childrens.Find('qtde'), tcDe10);
  end;

  NFe.cana.deduc.Clear;
  ANodes := ANode.Childrens.FindAll('deduc');
  for i := 0 to Length(ANodes) - 1 do
  begin
    NFe.cana.deduc.New;
    (*ZC05*) NFe.cana.deduc[i].xDed  := ProcessarConteudo(ANodes[i].Childrens.Find('xDed'), tcStr);
    (*ZC06*) NFe.cana.deduc[i].vDed := ProcessarConteudo(ANodes[i].Childrens.Find('vDed'), tcDe2);
  end;
end;

procedure TNFeXmlReader.LerInfRespTec(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFe.infRespTec.CNPJ     := ProcessarConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  NFe.infRespTec.xContato := ProcessarConteudo(ANode.Childrens.Find('xContato'), tcStr);
  NFe.infRespTec.email    := ProcessarConteudo(ANode.Childrens.Find('email'), tcStr);
  NFe.infRespTec.fone     := ProcessarConteudo(ANode.Childrens.Find('fone'), tcStr);
  NFe.infRespTec.idCSRT   := ProcessarConteudo(ANode.Childrens.Find('idCSRT'), tcInt);
  NFe.infRespTec.hashCSRT := ProcessarConteudo(ANode.Childrens.Find('hashCSRT'), tcStr);
end;

procedure TNFeXmlReader.LerInfNFeSupl(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  NFe.infNFeSupl.qrCode := ProcessarConteudo(ANode.Childrens.Find('qrCode'), tcStr);
  NFe.infNFeSupl.qrCode := StringReplace(NFe.infNFeSupl.qrCode, '<![CDATA[', '', []);
  NFe.infNFeSupl.qrCode := StringReplace(NFe.infNFeSupl.qrCode, ']]>', '', []);
  NFe.infNFeSupl.urlChave := ProcessarConteudo(ANode.Childrens.Find('urlChave'), tcStr);
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
      NFE.signature.DigestValue     := ProcessarConteudo(AuxNode.Childrens.Find('DigestValue'), tcStr);
    end;
  end;

  NFE.signature.SignatureValue  := ProcessarConteudo(ANode.Childrens.Find('SignatureValue'), tcStr);

  AuxNode := ANode.Childrens.Find('KeyInfo');
  if AuxNode <> nil then
  begin
    NFE.signature.X509Certificate := ProcessarConteudo(ANode.Childrens.Find('X509Certificate'), tcStr);
  end;
end;

end.

