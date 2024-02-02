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

unit ACBrDCeXmlReader;

interface

uses
  Classes, SysUtils,
  ACBrXmlDocument, ACBrXmlReader,
  ACBrDCeClass;

type

  { TDCeXmlReader }
  TDCeXmlReader = class(TACBrXmlReader)
  private
    FDCe: TDCe;

    procedure LerInfDCe(const ANode: TACBrXmlNode);
    procedure LerIde(const ANode: TACBrXmlNode);
    procedure LerEmit(const ANode: TACBrXmlNode);
    procedure LerEmitEnderEmit(const ANode: TACBrXmlNode);
    procedure LerFisco(const ANode: TACBrXmlNode);
    procedure LerMarketplace(const ANode: TACBrXmlNode);
    procedure LerTransportadora(const ANode: TACBrXmlNode);
    procedure LerEmpEmisProp(const ANode: TACBrXmlNode);
    procedure LerDest(const ANode: TACBrXmlNode);
    procedure LerDestEnderDest(const ANode: TACBrXmlNode);
    procedure LerDet(const ANode: TACBrXmlNode);
    procedure LerDetProd(const Item: TDetCollectionItem; const ANode: TACBrXmlNode);
    procedure LerTotal(const ANode: TACBrXmlNode);
    procedure LerTransp(const ANode: TACBrXmlNode);
    procedure LerInfAdic(const ANode: TACBrXmlNode);
    procedure LerInfDec(const ANode: TACBrXmlNode);
    procedure LerInfSolicDCe(const ANode: TACBrXmlNode);

    procedure LerProtDCe(const ANode: TACBrXmlNode);
    procedure LerInfDCeSupl(const ANode: TACBrXmlNode);
    procedure LerSignature(const ANode: TACBrXmlNode);
  public
    constructor Create(AOwner: TDCe); reintroduce;

    function LerXml: Boolean; override;

    property DCe: TDCe read FDCe write FDCe;

  end;

implementation

uses
  ACBrUtil.Base,
  ACBrXmlBase, ACBrDCeConversao;

{ TDCeXmlReader }

constructor TDCeXmlReader.Create(AOwner: TDCe);
begin
  inherited Create;

  FDCe := AOwner;
end;

procedure TDCeXmlReader.LerDest(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.dest.CNPJCPF := ObterCNPJCPF(ANode);

  if DCe.dest.CNPJCPF = '' then
    DCe.dest.idOutros := ObterConteudo(ANode.Childrens.Find('idOutros'), tcStr);

  DCe.dest.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);

  LerDestEnderDest(ANode.Childrens.Find('enderDest'));
end;

procedure TDCeXmlReader.LerDestEnderDest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.dest.enderDest.xLgr := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  DCe.dest.enderDest.nro := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  DCe.dest.enderDest.xCpl := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  DCe.dest.enderDest.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  DCe.dest.enderDest.cMun := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  DCe.dest.enderDest.xMun := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  DCe.dest.enderDest.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  DCe.dest.enderDest.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  DCe.dest.enderDest.cPais := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);

  if DCe.dest.enderDest.cPais = 0 then
    DCe.dest.enderDest.cPais := 1058;

  DCe.dest.enderDest.xPais := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);

  if DCe.dest.enderDest.xPais = '' then
    DCe.dest.enderDest.xPais := 'BRASIL';

  DCe.dest.enderDest.fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  DCe.dest.enderDest.email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
end;

procedure TDCeXmlReader.LerDet(const ANode: TACBrXmlNode);
var
  Item: TDetCollectionItem;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item := DCe.Det.New;
  Item.prod.nItem := DCe.Det.Count;
  Item.infAdProd := ObterConteudo(ANode.Childrens.Find('infAdProd'), tcStr);

  LerDetProd(Item, ANode.Childrens.Find('prod'));
end;

procedure TDCeXmlReader.LerDetProd(const Item: TDetCollectionItem;
  const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(Item) or (Item = nil) then Exit;
  if not Assigned(ANode) or (ANode = nil) then Exit;

  Item.Prod.xProd := ObterConteudo(ANode.Childrens.Find('xProd'), tcStr);
  Item.Prod.NCM := ObterConteudo(ANode.Childrens.Find('NCM'), tcStr);
  Item.Prod.qCom := ObterConteudo(ANode.Childrens.Find('qCom'), tcDe4);
  Item.Prod.vUnCom := ObterConteudo(ANode.Childrens.Find('vUnCom'), tcDe10);
  Item.Prod.vProd := ObterConteudo(ANode.Childrens.Find('vProd'), tcDe2);
end;

procedure TDCeXmlReader.LerEmit(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.emit.CNPJCPF := ObterCNPJCPF(ANode);

  if DCe.emit.CNPJCPF = '' then
    DCe.emit.idOutros := ObterConteudo(ANode.Childrens.Find('idOutros'), tcStr);

  DCe.emit.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);

  LerEmitEnderEmit(ANode.Childrens.Find('enderEmit'));
end;

procedure TDCeXmlReader.LerEmitEnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.Emit.enderEmit.xLgr := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  DCe.Emit.enderEmit.nro := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  DCe.Emit.enderEmit.xCpl := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  DCe.Emit.enderEmit.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  DCe.Emit.EnderEmit.cMun := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  DCe.Emit.enderEmit.xMun := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  DCe.Emit.enderEmit.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  DCe.Emit.enderEmit.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  DCe.Emit.enderEmit.cPais := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);

  if DCe.Emit.enderEmit.cPais = 0 then
    DCe.Emit.enderEmit.cPais := 1058;

  DCe.Emit.enderEmit.xPais := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);

  if DCe.Emit.enderEmit.xPais = '' then
    DCe.Emit.enderEmit.xPais := 'BRASIL';

  DCe.Emit.enderEmit.fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
end;

procedure TDCeXmlReader.LerEmpEmisProp(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.EmpEmisProp.CNPJ := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  DCe.EmpEmisProp.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
end;

procedure TDCeXmlReader.LerFisco(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.Fisco.CNPJ := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  DCe.Fisco.xOrgao := ObterConteudo(ANode.Childrens.Find('xOrgao'), tcStr);
  DCe.Fisco.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
end;

procedure TDCeXmlReader.LerIde(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.ide.cUF := ObterConteudo(ANode.Childrens.Find('cUF'), tcInt);
  DCe.ide.cDC := ObterConteudo(ANode.Childrens.Find('cDC'), tcInt);
  DCe.ide.modelo := ObterConteudo(ANode.Childrens.Find('mod'), tcInt);
  DCe.ide.serie := ObterConteudo(ANode.Childrens.Find('serie'), tcInt);
  DCe.ide.nDC := ObterConteudo(ANode.Childrens.Find('nDC'), tcInt);
  DCe.ide.dhEmi := ObterConteudo(ANode.Childrens.Find('dhEmi'), tcDatHor);
  DCe.Ide.tpEmis := StrToTipoEmissao(ok, ObterConteudo(ANode.Childrens.Find('tpEmis'), tcStr));
  DCe.Ide.tpEmit := StrToEmitenteDCe(ok, ObterConteudo(ANode.Childrens.Find('tpEmit'), tcStr));
  DCe.ide.nSiteAutoriz := ObterConteudo(ANode.Childrens.Find('nSiteAutoriz'), tcInt);
  DCe.ide.cDV := ObterConteudo(ANode.Childrens.Find('cDV'), tcInt);
  DCe.Ide.tpAmb := StrToTipoAmbiente(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  DCe.Ide.verProc := ObterConteudo(ANode.Childrens.Find('verProc'), tcStr);
end;

procedure TDCeXmlReader.LerInfAdic(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.InfAdic.infAdFisco := ObterConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);
  DCe.InfAdic.infCpl := ObterConteudo(ANode.Childrens.Find('infCpl'), tcStr);
  DCe.InfAdic.infAdMarketplace := ObterConteudo(ANode.Childrens.Find('infAdMarketplace'), tcStr);
  DCe.InfAdic.infAdTransp := ObterConteudo(ANode.Childrens.Find('infAdTransp'), tcStr);

  DCe.InfAdic.obsCont.Clear;
  ANodes := ANode.Childrens.FindAll('obsCont');
  for i := 0 to Length(ANodes) - 1 do
    begin
      DCe.InfAdic.obsCont.New;
      DCe.InfAdic.obsCont[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
      DCe.InfAdic.obsCont[i].xTexto := ObterConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
    end;

  DCe.InfAdic.obsMarketplace.Clear;
  ANodes := ANode.Childrens.FindAll('obsMarketplace');
  for i := 0 to Length(ANodes) - 1 do
  begin
    DCe.InfAdic.obsMarketplace.New;
    DCe.InfAdic.obsMarketplace[i].xCampo := ANodes[i].Attributes.Items['xCampo'].Content;
    DCe.InfAdic.obsMarketplace[i].xTexto := ObterConteudo(ANodes[i].Childrens.Find('xTexto'), tcStr);
  end;
end;

procedure TDCeXmlReader.LerInfDCe(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  LerIde(ANode.Childrens.Find('ide'));
  LerEmit(ANode.Childrens.Find('emit'));
  LerFisco(ANode.Childrens.Find('Fisco'));
  LerMarketplace(ANode.Childrens.Find('Marketplace'));
  LerTransportadora(ANode.Childrens.Find('Transportadora'));
  LerEmpEmisProp(ANode.Childrens.Find('EmpEmisProp'));
  LerDest(ANode.Childrens.Find('dest'));

  ANodes := ANode.Childrens.FindAll('autXML');
  for i := 0 to Length(ANodes) - 1 do
  begin
    DCe.autXML.New;
    DCe.autXML[i].CNPJCPF := ObterCNPJCPF(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('det');
  for i := 0 to Length(ANodes) - 1 do
  begin
    LerDet(ANodes[i]);
  end;

  LerTotal(ANode.Childrens.Find('total'));
  LerTransp(ANode.Childrens.Find('transp'));
  LerInfAdic(ANode.Childrens.Find('infAdic'));
  LerInfDec(ANode.Childrens.Find('infDec'));
  LerInfSolicDCe(ANode.Childrens.Find('infSolicDCe'));
end;

procedure TDCeXmlReader.LerInfDCeSupl(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.infDCeSupl.qrCode := ObterConteudo(ANode.Childrens.Find('qrCode'), tcStr);
  DCe.infDCeSupl.qrCode := StringReplace(DCe.infDCeSupl.qrCode, '<![CDATA[', '', []);
  DCe.infDCeSupl.qrCode := StringReplace(DCe.infDCeSupl.qrCode, ']]>', '', []);
  DCe.infDCeSupl.urlChave := ObterConteudo(ANode.Childrens.Find('urlChave'), tcStr);
end;

procedure TDCeXmlReader.LerInfDec(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.infDec.xObs1 := ObterConteudo(ANode.Childrens.Find('xObs1'), tcStr);
  DCe.infDec.xObs2 := ObterConteudo(ANode.Childrens.Find('xObs2'), tcStr);
end;

procedure TDCeXmlReader.LerInfSolicDCe(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.infSolicDCe.xSolic := ObterConteudo(ANode.Childrens.Find('xSolic'), tcStr);
end;

procedure TDCeXmlReader.LerMarketplace(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.Marketplace.CNPJ := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  DCe.Marketplace.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  DCe.Marketplace.Site := ObterConteudo(ANode.Childrens.Find('Site'), tcStr);
end;

procedure TDCeXmlReader.LerProtDCe(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.procDCe.tpAmb    := StrToTipoAmbiente(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  DCe.procDCe.verAplic := ObterConteudo(ANode.Childrens.Find('verAplic'), tcStr);
  DCe.procDCe.chDCe    := ObterConteudo(ANode.Childrens.Find('chDCe'), tcStr);
  DCe.procDCe.dhRecbto := ObterConteudo(ANode.Childrens.Find('dhRecbto'), tcDatHor);
  DCe.procDCe.nProt    := ObterConteudo(ANode.Childrens.Find('nProt'), tcStr);
  DCe.procDCe.digVal   := ObterConteudo(ANode.Childrens.Find('digVal'), tcStr);
  DCe.procDCe.cStat    := ObterConteudo(ANode.Childrens.Find('cStat'), tcInt);
  DCe.procDCe.xMotivo  := ObterConteudo(ANode.Childrens.Find('xMotivo'), tcStr);
  DCe.procDCe.cMsg     := ObterConteudo(ANode.Childrens.Find('cMsg'), tcInt);
  DCe.procDCe.xMsg     := ObterConteudo(ANode.Childrens.Find('xMsg'), tcStr);
end;

procedure TDCeXmlReader.LerSignature(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.Find('SignedInfo');
  if AuxNode <> nil then
  begin

    AuxNode := ANode.Childrens.Find('Reference');
    if AuxNode <> nil then
    begin
      DCe.signature.URI := AuxNode.Attributes.Items['URI'].Content;
      DCe.signature.DigestValue := ObterConteudo(AuxNode.Childrens.Find('DigestValue'), tcStr);
    end;
  end;

  DCe.signature.SignatureValue  := ObterConteudo(ANode.Childrens.Find('SignatureValue'), tcStr);

  AuxNode := ANode.Childrens.Find('KeyInfo');
  if AuxNode <> nil then
  begin
    DCe.signature.X509Certificate := ObterConteudo(ANode.Childrens.Find('X509Certificate'), tcStr);
  end;
end;

procedure TDCeXmlReader.LerTotal(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.Total.vDC := ObterConteudo(AuxNode.Childrens.Find('vDC'), tcDe2);
end;

procedure TDCeXmlReader.LerTransp(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.Transp.modTrans := StrToModTrans(ok, ObterConteudo(ANode.Childrens.Find('modTrans'), tcStr));
  DCe.Transp.CNPJTrans := ObterConteudo(ANode.Childrens.Find('CNPJTrans'), tcStr);
end;

procedure TDCeXmlReader.LerTransportadora(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  DCe.Transportadora.CNPJ := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  DCe.Transportadora.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
end;

function TDCeXmlReader.LerXml: Boolean;
var
  DCeNode, infDCeNode: TACBrXmlNode;
  att: TACBrXmlAttribute;
begin
  if not Assigned(FDCe) or (FDCe = nil) then
    raise Exception.Create('Destino não informado, informe a classe [TDCe] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml da DCe não carregado.');

  Result := False;
  infDCeNode := nil;
  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'DCeProc' then
  begin
    LerProtDCe(Document.Root.Childrens.Find('protDCe'));
    DCeNode := Document.Root.Childrens.Find('DCe');
  end
  else
  begin
    DCeNode := Document.Root;
  end;

  if DCeNode <> nil then
      infDCeNode := DCeNode.Childrens.Find('infDCe');

  if infDCeNode = nil then
    raise Exception.Create('Arquivo xml incorreto.');

  att := infDCeNode.Attributes.Items['Id'];
  if att = nil then
    raise Exception.Create('Não encontrei o atributo: Id');

  DCe.infDCe.Id := att.Content;

  att := infDCeNode.Attributes.Items['versao'];
  if att = nil then
    raise Exception.Create('Não encontrei o atributo: versao');

  DCe.infDCe.Versao := StringToFloat(att.Content);

  LerInfDCe(infDCeNode);
  LerInfDCeSupl(DCeNode.Childrens.Find('infDCeSupl'));
  LerSignature(DCeNode.Childrens.Find('Signature'));

  Result := True;
end;

end.

