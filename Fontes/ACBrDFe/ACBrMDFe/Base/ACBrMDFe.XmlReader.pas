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

unit ACBrMDFe.XmlReader;

interface

uses
  Classes, SysUtils,
  ACBrXmlDocument, ACBrXmlReader,
  pmdfeMDFe,
  pcnConversao;

type
  { TMDFeXmlReader }
  TMDFeXmlReader = class(TACBrXmlReader)
  private
    FMDFe: TMDFe;

    function NodeNaoEncontrado(const ANode: TACBrXmlNode): Boolean;

    procedure Ler_ProtMDFe(const ANode: TACBrXmlNode);
    procedure Ler_InfMDFe(const ANode: TACBrXmlNode);
    procedure Ler_Ide(const ANode: TACBrXmlNode);

    procedure Ler_infMunCarrega(const ANode: TACBrXmlNode);
    procedure Ler_infPercurso(const ANode: TACBrXmlNode);

    procedure Ler_Emit(const ANode: TACBrXmlNode);
    procedure Ler_EnderEmit(const ANode: TACBrXmlNode);

    procedure Ler_infModal(const ANode: TACBrXmlNode);

    procedure Ler_Rodoviario(const ANode: TACBrXmlNode);
    procedure Ler_Rododisp(const ANode: TACBrXmlNode);
    procedure Ler_infANTT(const ANode: TACBrXmlNode);
    procedure Ler_infCIOT(const ANode: TACBrXmlNode);
    procedure Ler_valePed(const ANode: TACBrXmlNode);
    procedure Ler_disp(const ANode: TACBrXmlNode);
    procedure Ler_infContratante(const ANode: TACBrXmlNode);
    procedure Ler_infPag(const ANode: TACBrXmlNode);
    procedure Ler_Comp(const ANode: TACBrXmlNode; Item: TinfPagCollectionItem);
    procedure Ler_infPrazo(const ANode: TACBrXmlNode; Item: TinfPagCollectionItem);

    procedure Ler_veicTracao(const ANode: TACBrXmlNode);
    procedure Ler_condutor(const ANode: TACBrXmlNode);

    procedure Ler_veicReboque(const ANode: TACBrXmlNode);

    procedure Ler_lacRodo(const ANode: TACBrXmlNode);

    procedure Ler_Aereo(const ANode: TACBrXmlNode);

    procedure Ler_Aquaviario(const ANode: TACBrXmlNode);
    procedure Ler_infTermCarreg(const ANode: TACBrXmlNode);
    procedure Ler_infTermDescarreg(const ANode: TACBrXmlNode);
    procedure Ler_infEmbComb(const ANode: TACBrXmlNode);
    procedure Ler_infUnidCargaVazia(const ANode: TACBrXmlNode);
    procedure Ler_infUnidTranspVazia(const ANode: TACBrXmlNode);

    procedure Ler_Ferroviario(const ANode: TACBrXmlNode);
    procedure Ler_trem(const ANode: TACBrXmlNode);
    procedure Ler_vag(const ANode: TACBrXmlNode);

    procedure Ler_infDoc(const ANode: TACBrXmlNode);
    procedure Ler_infMunDescarga(const ANode: TACBrXmlNode);

    procedure Ler_infCTe(const ANode: TACBrXmlNode; Item: TinfMunDescargaCollectionItem);
    procedure Ler_infUnidTranspCTe(const ANode: TACBrXmlNode; Item: TinfCTeCollectionItem);
    procedure Ler_lacUnidTranspCTe(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_infUnidCargaCTe(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_lacUnidCargaCTe(const ANode: TACBrXmlNode; Item: TinfUnidCargaCollectionItem);
    procedure Ler_periCTe(const ANode: TACBrXmlNode; Item: TinfCTeCollectionItem);

    procedure Ler_infCT(const ANode: TACBrXmlNode; Item: TinfMunDescargaCollectionItem);
    procedure Ler_infUnidTranspCT(const ANode: TACBrXmlNode; Item: TinfCTCollectionItem);
    procedure Ler_lacUnidTranspCT(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_infUnidCargaCT(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_lacUnidCargaCT(const ANode: TACBrXmlNode; Item: TinfUnidCargaCollectionItem);

    procedure Ler_infNFe(const ANode: TACBrXmlNode; Item: TinfMunDescargaCollectionItem);
    procedure Ler_infUnidTranspNFe(const ANode: TACBrXmlNode; Item: TinfNFeCollectionItem);
    procedure Ler_lacUnidTranspNFe(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_infUnidCargaNFe(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_lacUnidCargaNFe(const ANode: TACBrXmlNode; Item: TinfUnidCargaCollectionItem);
    procedure Ler_periNFe(const ANode: TACBrXmlNode; Item: TinfNFeCollectionItem);

    procedure Ler_infNF(const ANode: TACBrXmlNode; Item: TinfMunDescargaCollectionItem);
    procedure Ler_infUnidTranspNF(const ANode: TACBrXmlNode; Item: TinfNFCollectionItem);
    procedure Ler_lacUnidTranspNF(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_infUnidCargaNF(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_lacUnidCargaNF(const ANode: TACBrXmlNode; Item: TinfUnidCargaCollectionItem);

    procedure Ler_infMDFeTransp(const ANode: TACBrXmlNode; Item: TinfMunDescargaCollectionItem);
    procedure Ler_infUnidTranspMDFe(const ANode: TACBrXmlNode; Item: TinfMDFeTranspCollectionItem);
    procedure Ler_lacUnidTranspMDFe(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_infUnidCargaMDFe(const ANode: TACBrXmlNode; Item: TinfUnidTranspCollectionItem);
    procedure Ler_lacUnidCargaMDFe(const ANode: TACBrXmlNode; Item: TinfUnidCargaCollectionItem);
    procedure Ler_periMDFe(const ANode: TACBrXmlNode; Item: TinfMDFeTranspCollectionItem);

    procedure Ler_seg(const ANode: TACBrXmlNode);
    procedure Ler_infResp(const ANode: TACBrXmlNode; Item: TSegCollectionItem);
    procedure Ler_infSeg(const ANode: TACBrXmlNode; Item: TSegCollectionItem);
    procedure Ler_nAver(const ANode: TACBrXmlNode; Item: TSegCollectionItem);

    procedure Ler_tot(const ANode: TACBrXmlNode);

    procedure Ler_prodPred(const ANode: TACBrXmlNode);
    procedure Ler_infLotacao(const ANode: TACBrXmlNode);
    procedure Ler_infLocalCarrega(const ANode: TACBrXmlNode);
    procedure Ler_infLocalDescarrega(const ANode: TACBrXmlNode);

    procedure Ler_lacres(const ANode: TACBrXmlNode);

    procedure Ler_autXML(const ANode: TACBrXmlNode);
    procedure Ler_InfAdic(const ANode: TACBrXmlNode);
    procedure Ler_infRespTec(const ANode: TACBrXmlNode);
    procedure Ler_InfMDFeSupl(const ANode: TACBrXmlNode);
  public
    constructor Create(AOwner: TMDFe); reintroduce;

    function LerXml: Boolean; override;

    property MDFe: TMDFe read FMDFe write FMDFe;
  end;

implementation

uses
  ACBrXmlBase, ACBrUtil.Base,
  pmdfeConversaoMDFe;

{ TMDFeXmlReader }

constructor TMDFeXmlReader.Create(AOwner: TMDFe);
begin
  inherited Create;

  FMDFe := AOwner;
end;

function TMDFeXmlReader.NodeNaoEncontrado(const ANode: TACBrXmlNode): Boolean;
begin
  Result := not Assigned(ANode);
end;

function TMDFeXmlReader.LerXml: Boolean;
Var
  MDFeNode, infMDFeNode: TACBrXmlNode;
  att: TACBrXmlAttribute;
begin
  if not Assigned(FMDFe) then
    raise Exception.Create('Destino não informado, informe a classe [TMDFe] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml da MDFe não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if (Document.Root.Name = 'MDFeProc') or (Document.Root.Name = 'mdfeProc') then
  begin
    Ler_ProtMDFe(Document.Root.Childrens.Find('protMDFe').Childrens.Find('infProt'));
    MDFeNode := Document.Root.Childrens.Find('MDFe');
  end
  else
  begin
    MDFeNode := Document.Root;
  end;

  if MDFeNode <> nil then
  begin
    infMDFeNode := MDFeNode.Childrens.Find('infMDFe');

    if infMDFeNode = nil then
      raise Exception.Create('Arquivo xml incorreto.');

    att := infMDFeNode.Attributes.Items['Id'];

    if att = nil then
      raise Exception.Create('Não encontrei o atributo: Id');

    MDFe.infMDFe.Id := att.Content;

    att := infMDFeNode.Attributes.Items['versao'];

    if att = nil then
      raise Exception.Create('Não encontrei o atributo: versao');

    MDFe.infMDFe.Versao := StringToFloat(att.Content);

    Ler_InfMDFe(infMDFeNode);

    LerSignature(MDFeNode.Childrens.Find('Signature'), MDFe.signature);
  end;

  Result := True;
end;

procedure TMDFeXmlReader.Ler_ProtMDFe(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  MDFe.procMDFe.tpAmb := StrToTpAmb(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  MDFe.procMDFe.verAplic := ObterConteudo(ANode.Childrens.Find('verAplic'), tcStr);
  MDFe.procMDFe.chMDFe := ObterConteudo(ANode.Childrens.Find('chMDFe'), tcStr);
  MDFe.procMDFe.dhRecbto := ObterConteudo(ANode.Childrens.Find('dhRecbto'), tcDatHor);
  MDFe.procMDFe.nProt := ObterConteudo(ANode.Childrens.Find('nProt'), tcStr);
  MDFe.procMDFe.digVal := ObterConteudo(ANode.Childrens.Find('digVal'), tcStr);
  MDFe.procMDFe.cStat := ObterConteudo(ANode.Childrens.Find('cStat'), tcInt);
  MDFe.procMDFe.xMotivo := ObterConteudo(ANode.Childrens.Find('xMotivo'), tcStr);
  MDFe.procMDFe.cMsg := ObterConteudo(ANode.Childrens.Find('cMsg'), tcInt);
  MDFe.procMDFe.xMsg := ObterConteudo(ANode.Childrens.Find('xMsg'), tcStr);
end;

procedure TMDFeXmlReader.Ler_InfMDFe(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  Ler_Ide(ANode.Childrens.Find('ide'));
  Ler_Emit(ANode.Childrens.Find('emit'));
  Ler_infModal(ANode.Childrens.Find('infModal'));
  Ler_infDoc(ANode.Childrens.Find('infDoc'));

  ANodes := ANode.Childrens.FindAll('seg');
  MDFe.seg.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_seg(ANodes[i]);
  end;

  Ler_prodPred(ANode.Childrens.Find('prodPred'));
  Ler_tot(ANode.Childrens.Find('tot'));

  ANodes := ANode.Childrens.FindAll('lacres');
  MDFe.lacres.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacres(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('autXML');
  MDFe.autXML.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_autXML(ANodes[i]);
  end;

  Ler_InfAdic(ANode.Childrens.Find('infAdic'));
  Ler_infRespTec(ANode.Childrens.Find('infRespTec'));
  Ler_InfMDFeSupl(ANode.Childrens.Find('infMDFeSupl'));
end;

procedure TMDFeXmlReader.Ler_Ide(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  MDFe.ide.cUF := ObterConteudo(ANode.Childrens.Find('cUF'), tcInt);
  MDFe.Ide.tpAmb := StrToTpAmb(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  MDFe.Ide.tpEmit := StrToTpEmitente(Ok, ObterConteudo(ANode.Childrens.Find('tpEmit'), tcStr));
  MDFe.Ide.tpTransp := StrToTTransportador(Ok, ObterConteudo(ANode.Childrens.Find('tpTransp'), tcStr));
  MDFe.ide.modelo := ObterConteudo(ANode.Childrens.Find('mod'), tcInt);
  MDFe.ide.serie := ObterConteudo(ANode.Childrens.Find('serie'), tcInt);
  MDFe.ide.nMDF := ObterConteudo(ANode.Childrens.Find('nMDF'), tcInt);
  MDFe.ide.cMDF := ObterConteudo(ANode.Childrens.Find('cMDF'), tcInt);
  MDFe.Ide.cDV := ObterConteudo(ANode.Childrens.Find('cDV'), tcInt);
  MDFe.Ide.modal := StrToModal(Ok, ObterConteudo(ANode.Childrens.Find('modal'), tcStr));
  MDFe.ide.dhEmi := ObterConteudo(ANode.Childrens.Find('dhEmi'), tcDatHor);
  MDFe.Ide.tpEmis := StrToTpEmis(ok, ObterConteudo(ANode.Childrens.Find('tpEmis'), tcStr));
  MDFe.Ide.procEmi := StrToprocEmi(ok, ObterConteudo(ANode.Childrens.Find('procEmi'), tcStr));
  MDFe.Ide.verProc := ObterConteudo(ANode.Childrens.Find('verProc'), tcStr);
  MDFe.Ide.UFIni := ObterConteudo(ANode.Childrens.Find('UFIni'), tcStr);
  MDFe.Ide.UFFim := ObterConteudo(ANode.Childrens.Find('UFFim'), tcStr);
  MDFe.Ide.dhIniViagem := ObterConteudo(ANode.Childrens.Find('dhIniViagem'), tcDatHor);

  if MDFe.infMDFe.versao >= 3 then
  begin
    if ObterConteudo(ANode.Childrens.Find('indCanalVerde'), tcStr) = '1' then
      MDFe.ide.indCanalVerde := tiSim
    else
      MDFe.ide.indCanalVerde := tiNao;

    if ObterConteudo(ANode.Childrens.Find('indCarregaPosterior'), tcStr) = '1' then
      MDFe.ide.indCarregaPosterior := tiSim
    else
      MDFe.ide.indCarregaPosterior := tiNao;
  end;

  ANodes := ANode.Childrens.FindAll('infMunCarrega');
  MDFe.Ide.infMunCarrega.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infMunCarrega(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('infPercurso');
  MDFe.Ide.infPercurso.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infPercurso(ANodes[i]);
  end;
end;

procedure TMDFeXmlReader.Ler_infMunCarrega(const ANode: TACBrXmlNode);
var
  Item: TinfMunCarregaCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.Ide.infMunCarrega.New;

  Item.cMunCarrega := ObterConteudo(ANode.Childrens.Find('cMunCarrega'), tcInt);
  Item.xMunCarrega := ObterConteudo(ANode.Childrens.Find('xMunCarrega'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infPercurso(const ANode: TACBrXmlNode);
var
  Item: TinfPercursoCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.Ide.infPercurso.New;

  Item.UFPer := ObterConteudo(ANode.Childrens.Find('UFPer'), tcStr);
end;

procedure TMDFeXmlReader.Ler_Emit(const ANode: TACBrXmlNode);
var
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  MDFe.Emit.CNPJCPF := ObterCNPJCPF(ANode);
  MDFe.Emit.IE := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
  MDFe.Emit.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  MDFe.Emit.xFant := ObterConteudo(ANode.Childrens.Find('xFant'), tcStr);

  Ler_EnderEmit(ANode.Childrens.Find('enderEmit'));
end;

procedure TMDFeXmlReader.Ler_EnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  MDFe.Emit.enderEmit.xLgr := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  MDFe.Emit.enderEmit.nro := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  MDFe.Emit.enderEmit.xCpl := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  MDFe.Emit.enderEmit.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  MDFe.Emit.EnderEmit.cMun := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  MDFe.Emit.enderEmit.xMun := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  MDFe.Emit.enderEmit.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  MDFe.Emit.enderEmit.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  MDFe.Emit.enderEmit.fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  MDFe.Emit.enderEmit.email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infModal(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  case MDFe.Ide.modal of
    moRodoviario: Ler_Rodoviario(ANode.Childrens.Find('rodo'));
    moAereo: Ler_Aereo(ANode.Childrens.Find('aereo'));
    moAquaviario: Ler_Aquaviario(ANode.Childrens.Find('aquav'));
    moFerroviario: Ler_Ferroviario(ANode.Childrens.Find('ferrov'));
  end;
end;

procedure TMDFeXmlReader.Ler_Rodoviario(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  MDFe.Rodo.RNTRC := ObterConteudo(ANode.Childrens.Find('RNTRC'), tcStr);
  MDFe.Rodo.CIOT := ObterConteudo(ANode.Childrens.Find('CIOT'), tcStr);

  Ler_infANTT(ANode.Childrens.Find('infANTT'));
  Ler_veicTracao(ANode.Childrens.Find('veicTracao'));
  Ler_veicTracao(ANode.Childrens.Find('veicPrincipal'));

  ANodes := ANode.Childrens.FindAll('veicReboque');
  MDFe.Rodo.veicReboque.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_veicReboque(ANodes[i]);
  end;

  MDFe.Rodo.codAgPorto := ObterConteudo(ANode.Childrens.Find('codAgPorto'), tcStr);

  AuxNode := ANode.Childrens.Find('valePed');

  if Assigned(AuxNode) then
  begin
    ANodes := AuxNode.Childrens.FindAll('disp');
    MDFe.Rodo.valePed.disp.Clear;
    for i := 0 to Length(ANodes) - 1 do
    begin
      Ler_Rododisp(ANodes[i]);
    end;
  end;

  ANodes := ANode.Childrens.FindAll('lacRodo');
  MDFe.Rodo.lacRodo.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacRodo(ANodes[i]);
  end;
end;

procedure TMDFeXmlReader.Ler_Rododisp(const ANode: TACBrXmlNode);
var
  Item: TdispCollectionItem;
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.Rodo.valePed.disp.New;

  Item.CNPJForn := ObterConteudo(ANode.Childrens.Find('CNPJForn'), tcStr);
  Item.CNPJPg := ObterConteudo(ANode.Childrens.Find('CNPJPg'), tcStr);
  Item.nCompra := ObterConteudo(ANode.Childrens.Find('nCompra'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infANTT(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  MDFe.Rodo.infANTT.RNTRC := ObterConteudo(ANode.Childrens.Find('RNTRC'), tcStr);

  ANodes := ANode.Childrens.FindAll('infCIOT');
  MDFe.Rodo.infANTT.infCIOT.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infCIOT(ANodes[i]);
  end;

  Ler_valePed(ANode.Childrens.Find('valePed'));

  ANodes := ANode.Childrens.FindAll('infContratante');
  MDFe.Rodo.infANTT.infContratante.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infContratante(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('infPag');
  MDFe.Rodo.infANTT.infPag.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infPag(ANodes[i]);
  end;
end;

procedure TMDFeXmlReader.Ler_infCIOT(const ANode: TACBrXmlNode);
var
  Item: TinfCIOTCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.Rodo.infANTT.infCIOT.New;

  Item.CIOT := ObterConteudo(ANode.Childrens.Find('CIOT'), tcStr);
  Item.CNPJCPF := ObterCNPJCPF(ANode);
end;

procedure TMDFeXmlReader.Ler_valePed(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  MDFe.Rodo.infANTT.valePed.categCombVeic := StrTocategCombVeic(ok, ObterConteudo(ANode.Childrens.Find('categCombVeic'), tcStr));

  ANodes := ANode.Childrens.FindAll('disp');
  MDFe.Rodo.infANTT.valePed.disp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_disp(ANodes[i]);
  end;
end;

procedure TMDFeXmlReader.Ler_disp(const ANode: TACBrXmlNode);
var
  Item: TdispCollectionItem;
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.Rodo.infANTT.valePed.disp.New;

  Item.CNPJForn := ObterConteudo(ANode.Childrens.Find('CNPJForn'), tcStr);
  Item.CNPJPg := ObterConteudo(ANode.Childrens.Find('CNPJPg'), tcStr);

  if Item.CNPJPg = '' then
    Item.CNPJPg := ObterConteudo(ANode.Childrens.Find('CPFPg'), tcStr);

  Item.nCompra := ObterConteudo(ANode.Childrens.Find('nCompra'), tcStr);
  Item.vValePed := ObterConteudo(ANode.Childrens.Find('vValePed'), tcDe2);
  Item.tpValePed := StrTotpValePed(ok, ObterConteudo(ANode.Childrens.Find('tpValePed'), tcStr));
end;

procedure TMDFeXmlReader.Ler_infContratante(const ANode: TACBrXmlNode);
var
  Item: TinfContratanteCollectionItem;
  ok: Boolean;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.Rodo.infANTT.infContratante.New;

  Item.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  Item.CNPJCPF := ObterCNPJCPF(ANode);
  Item.idEstrangeiro := ObterConteudo(ANode.Childrens.Find('idEstrangeiro'), tcStr);

  AuxNode := ANode.Childrens.Find('infContrato');

  if Assigned(AuxNode) then
  begin
    Item.infContrato.NroContrato := ObterConteudo(AuxNode.Childrens.Find('NroContrato'), tcStr);
    Item.infContrato.vContratoGlobal := ObterConteudo(AuxNode.Childrens.Find('vContratoGlobal'), tcDe2);
  end;
end;

procedure TMDFeXmlReader.Ler_infPag(const ANode: TACBrXmlNode);
var
  Item: TinfPagCollectionItem;
  ok: Boolean;
  aux: string;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.Rodo.infANTT.infPag.New;

  Item.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  Item.CNPJCPF := ObterCNPJCPF(ANode);
  Item.idEstrangeiro := ObterConteudo(ANode.Childrens.Find('idEstrangeiro'), tcStr);

  ANodes := ANode.Childrens.FindAll('Comp');
  Item.Comp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_Comp(ANodes[i], Item);
  end;

  Item.vContrato := ObterConteudo(ANode.Childrens.Find('vContrato'), tcDe2);
  Item.indAltoDesemp := StrToindAltoDesemp(ok, ObterConteudo(ANode.Childrens.Find('indAltoDesemp'), tcStr));
  Item.indPag := StrToTIndPag(ok, ObterConteudo(ANode.Childrens.Find('indPag'), tcStr));
  Item.vAdiant := ObterConteudo(ANode.Childrens.Find('vAdiant'), tcDe2);

  aux := ObterConteudo(ANode.Childrens.Find('indAntecipaAdiant'), tcStr);

  if aux <> '' then
    Item.indAntecipaAdiant := StrToTIndicador(ok, aux);

  ANodes := ANode.Childrens.FindAll('infPrazo');
  Item.infPrazo.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infPrazo(ANodes[i], Item);
  end;

  Item.tpAntecip := StrTotpAntecip(ok, ObterConteudo(ANode.Childrens.Find('indtpAntecipPag'), tcStr));

  AuxNode := ANode.Childrens.Find('infBanc');

  if Assigned(AuxNode) then
  begin
    Item.infBanc.codBanco := ObterConteudo(AuxNode.Childrens.Find('codBanco'), tcStr);
    Item.infBanc.codAgencia := ObterConteudo(AuxNode.Childrens.Find('codAgencia'), tcStr);
    Item.infBanc.CNPJIPEF := ObterConteudo(AuxNode.Childrens.Find('CNPJIPEF'), tcStr);
    Item.infBanc.PIX := ObterConteudo(AuxNode.Childrens.Find('PIX'), tcStr);
  end;
end;

procedure TMDFeXmlReader.Ler_Comp(const ANode: TACBrXmlNode;
  Item: TinfPagCollectionItem);
var
  ItemComp: TCompCollectionItem;
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  ItemComp := Item.Comp.New;

  ItemComp.tpComp := StrToTComp(ok, ObterConteudo(ANode.Childrens.Find('tpComp'), tcStr));
  ItemComp.vComp := ObterConteudo(ANode.Childrens.Find('vComp'), tcDe2);
  ItemComp.xComp := ObterConteudo(ANode.Childrens.Find('xComp'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infPrazo(const ANode: TACBrXmlNode;
  Item: TinfPagCollectionItem);
var
  ItemPrazo: TInfPrazoCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemPrazo := Item.infPrazo.New;

  ItemPrazo.nParcela := ObterConteudo(ANode.Childrens.Find('nParcela'), tcInt);
  ItemPrazo.dVenc := ObterConteudo(ANode.Childrens.Find('dVenc'), tcDat);
  ItemPrazo.vParcela := ObterConteudo(ANode.Childrens.Find('vParcela'), tcDe2);
end;

procedure TMDFeXmlReader.Ler_veicTracao(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  MDFe.Rodo.veicTracao.cInt := ObterConteudo(ANode.Childrens.Find('cInt'), tcStr);
  MDFe.Rodo.veicTracao.placa := ObterConteudo(ANode.Childrens.Find('placa'), tcStr);
  MDFe.Rodo.veicTracao.RENAVAM := ObterConteudo(ANode.Childrens.Find('RENAVAM'), tcStr);
  MDFe.Rodo.veicTracao.tara := ObterConteudo(ANode.Childrens.Find('tara'), tcInt);
  MDFe.Rodo.veicTracao.capKG := ObterConteudo(ANode.Childrens.Find('capKG'), tcInt);
  MDFe.Rodo.veicTracao.capM3 := ObterConteudo(ANode.Childrens.Find('capM3'), tcInt);

  AuxNode := ANode.Childrens.Find('prop');

  if Assigned(AuxNode) then
  begin
    MDFe.Rodo.veicTracao.prop.CNPJCPF := ObterCNPJCPF(AuxNode);
    MDFe.Rodo.veicTracao.prop.RNTRC := ObterConteudo(AuxNode.Childrens.Find('RNTRC'), tcStr);
    MDFe.Rodo.veicTracao.prop.xNome := ObterConteudo(AuxNode.Childrens.Find('xNome'), tcStr);
    MDFe.Rodo.veicTracao.prop.IE := ObterConteudo(AuxNode.Childrens.Find('IE'), tcStr);
    MDFe.Rodo.veicTracao.prop.UF := ObterConteudo(AuxNode.Childrens.Find('UF'), tcStr);
    MDFe.Rodo.veicTracao.prop.tpProp := StrToTpProp(ok, ObterConteudo(AuxNode.Childrens.Find('tpProp'), tcStr));
  end;

  ANodes := ANode.Childrens.FindAll('condutor');
  MDFe.rodo.veicTracao.condutor.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_condutor(ANodes[i]);
  end;

  MDFe.Rodo.veicTracao.tpRod := StrToTpRodado(ok, ObterConteudo(ANode.Childrens.Find('tpRod'), tcStr));
  MDFe.Rodo.veicTracao.tpCar := StrToTpCarroceria(ok, ObterConteudo(ANode.Childrens.Find('tpCar'), tcStr));
  MDFe.Rodo.veicTracao.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
end;

procedure TMDFeXmlReader.Ler_condutor(const ANode: TACBrXmlNode);
var
  Item: TcondutorCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.rodo.veicTracao.condutor.New;

  Item.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  Item.CPF := ObterConteudo(ANode.Childrens.Find('CPF'), tcStr);
end;

procedure TMDFeXmlReader.Ler_veicReboque(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  AuxNode: TACBrXmlNode;
  Item: TveicReboqueCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.rodo.veicReboque.New;

  Item.cInt := ObterConteudo(ANode.Childrens.Find('cInt'), tcStr);
  Item.placa := ObterConteudo(ANode.Childrens.Find('placa'), tcStr);
  Item.RENAVAM := ObterConteudo(ANode.Childrens.Find('RENAVAM'), tcStr);
  Item.tara := ObterConteudo(ANode.Childrens.Find('tara'), tcInt);
  Item.capKG := ObterConteudo(ANode.Childrens.Find('capKG'), tcInt);
  Item.capM3 := ObterConteudo(ANode.Childrens.Find('capM3'), tcInt);

  AuxNode := ANode.Childrens.Find('prop');

  if Assigned(AuxNode) then
  begin
    Item.prop.CNPJCPF := ObterCNPJCPF(AuxNode);
    Item.prop.RNTRC := ObterConteudo(AuxNode.Childrens.Find('RNTRC'), tcStr);
    Item.prop.xNome := ObterConteudo(AuxNode.Childrens.Find('xNome'), tcStr);
    Item.prop.IE := ObterConteudo(AuxNode.Childrens.Find('IE'), tcStr);
    Item.prop.UF := ObterConteudo(AuxNode.Childrens.Find('UF'), tcStr);
    Item.prop.tpProp := StrToTpProp(ok, ObterConteudo(AuxNode.Childrens.Find('tpProp'), tcStr));
  end;

  Item.tpCar := StrToTpCarroceria(ok, ObterConteudo(ANode.Childrens.Find('tpCar'), tcStr));
  Item.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
end;

procedure TMDFeXmlReader.Ler_lacRodo(const ANode: TACBrXmlNode);
var
  ItemlacRodo: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacRodo := MDFe.Rodo.lacRodo.New;

  ItemlacRodo.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_Aereo(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  MDFe.Aereo.nac := ObterConteudo(ANode.Childrens.Find('nac'), tcStr);
  MDFe.Aereo.matr := ObterConteudo(ANode.Childrens.Find('matr'), tcStr);
  MDFe.Aereo.nVoo := ObterConteudo(ANode.Childrens.Find('nVoo'), tcStr);
  MDFe.Aereo.cAerEmb := ObterConteudo(ANode.Childrens.Find('cAerEmb'), tcStr);
  MDFe.Aereo.cAerDes := ObterConteudo(ANode.Childrens.Find('cAerDes'), tcStr);
  MDFe.Aereo.dVoo := ObterConteudo(ANode.Childrens.Find('dVoo'), tcDat);
end;

procedure TMDFeXmlReader.Ler_Aquaviario(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  MDFe.aquav.CNPJAgeNav := ObterConteudo(ANode.Childrens.Find('CNPJAgeNav'), tcStr);
  MDFe.aquav.irin := ObterConteudo(ANode.Childrens.Find('irin'), tcStr);
  MDFe.aquav.tpEmb := ObterConteudo(ANode.Childrens.Find('tpEmb'), tcStr);
  MDFe.aquav.cEmbar := ObterConteudo(ANode.Childrens.Find('cEmbar'), tcStr);
  MDFe.aquav.xEmbar := ObterConteudo(ANode.Childrens.Find('xEmbar'), tcStr);
  MDFe.aquav.nViagem := ObterConteudo(ANode.Childrens.Find('nViagem'), tcStr);
  MDFe.aquav.cPrtEmb := ObterConteudo(ANode.Childrens.Find('cPrtEmb'), tcStr);
  MDFe.aquav.cPrtDest := ObterConteudo(ANode.Childrens.Find('cPrtDest'), tcStr);
  MDFe.aquav.prtTrans := ObterConteudo(ANode.Childrens.Find('prtTrans'), tcStr);
  MDFe.aquav.tpNav := StrToTpNavegacao(ok, ObterConteudo(ANode.Childrens.Find('tpNav'), tcStr));

  ANodes := ANode.Childrens.FindAll('infTermCarreg');
  MDFe.aquav.infTermCarreg.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infTermCarreg(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('infTermDescarreg');
  MDFe.aquav.infTermDescarreg.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infTermDescarreg(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('infEmbComb');
  MDFe.aquav.infEmbComb.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infEmbComb(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('infUnidCargaVazia');
  MDFe.aquav.infUnidCargaVazia.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidCargaVazia(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('infUnidTranspVazia');
  MDFe.aquav.infUnidTranspVazia.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidTranspVazia(ANodes[i]);
  end;
end;

procedure TMDFeXmlReader.Ler_infTermCarreg(const ANode: TACBrXmlNode);
var
  Item: TinfTermCarregCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.aquav.infTermCarreg.New;

  Item.cTermCarreg := ObterConteudo(ANode.Childrens.Find('cTermCarreg'), tcStr);
  Item.xTermCarreg := ObterConteudo(ANode.Childrens.Find('xTermCarreg'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infTermDescarreg(const ANode: TACBrXmlNode);
var
  Item: TinfTermDescarregCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.aquav.infTermDescarreg.New;

  Item.cTermDescarreg := ObterConteudo(ANode.Childrens.Find('cTermDescarreg'), tcStr);
  Item.xTermDescarreg := ObterConteudo(ANode.Childrens.Find('xTermDescarreg'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infEmbComb(const ANode: TACBrXmlNode);
var
  Item: TinfEmbCombCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.aquav.infEmbComb.New;

  Item.cEmbComb := ObterConteudo(ANode.Childrens.Find('cEmbComb'), tcStr);
  Item.xBalsa := ObterConteudo(ANode.Childrens.Find('xBalsa'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infUnidCargaVazia(const ANode: TACBrXmlNode);
var
  Item: TinfUnidCargaVaziaCollectionItem;
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.aquav.infUnidCargaVazia.New;

  Item.idUnidCargaVazia := ObterConteudo(ANode.Childrens.Find('idUnidCargaVazia'), tcStr);
  Item.tpUnidCargaVazia := StrToUnidCarga(ok, ObterConteudo(ANode.Childrens.Find('tpUnidCargaVazia'), tcStr));
end;

procedure TMDFeXmlReader.Ler_infUnidTranspVazia(const ANode: TACBrXmlNode);
var
  Item: TinfUnidTranspVaziaCollectionItem;
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.aquav.infUnidTranspVazia.New;

  Item.idUnidTranspVazia := ObterConteudo(ANode.Childrens.Find('idUnidTranspVazia'), tcStr);
  Item.tpUnidTranspVazia := StrToUnidTransp(ok, ObterConteudo(ANode.Childrens.Find('tpUnidTranspVazia'), tcStr));
end;

procedure TMDFeXmlReader.Ler_Ferroviario(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  Ler_trem(ANode.Childrens.Find('trem'));

  ANodes := ANode.Childrens.FindAll('vag');
  MDFe.ferrov.vag.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_vag(ANodes[i]);
  end;
end;

procedure TMDFeXmlReader.Ler_trem(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  MDFe.ferrov.xPref := ObterConteudo(ANode.Childrens.Find('xPref'), tcStr);
  MDFe.ferrov.dhTrem := ObterConteudo(ANode.Childrens.Find('dhTrem'), tcDatHor);
  MDFe.ferrov.xOri := ObterConteudo(ANode.Childrens.Find('xOri'), tcStr);
  MDFe.ferrov.xDest := ObterConteudo(ANode.Childrens.Find('xDest'), tcStr);
  MDFe.ferrov.qVag := ObterConteudo(ANode.Childrens.Find('qVag'), tcInt);
end;

procedure TMDFeXmlReader.Ler_vag(const ANode: TACBrXmlNode);
var
  Item: TvagCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.ferrov.vag.New;

  Item.pesoBC := ObterConteudo(ANode.Childrens.Find('pesoBC'), tcDe3);
  Item.pesoR := ObterConteudo(ANode.Childrens.Find('pesoR'), tcDe3);
  Item.tpVag := ObterConteudo(ANode.Childrens.Find('tpVag'), tcStr);
  Item.serie := ObterConteudo(ANode.Childrens.Find('serie'), tcStr);
  Item.nVag := ObterConteudo(ANode.Childrens.Find('nVag'), tcInt);
  Item.nSeq := ObterConteudo(ANode.Childrens.Find('nSeq'), tcInt);
  Item.TU := ObterConteudo(ANode.Childrens.Find('TU'), tcDe3);
end;

procedure TMDFeXmlReader.Ler_infDoc(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ANodes := ANode.Childrens.FindAll('infMunDescarga');
  MDFe.infDoc.infMunDescarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infMunDescarga(ANodes[i]);
  end;
end;

procedure TMDFeXmlReader.Ler_infMunDescarga(const ANode: TACBrXmlNode);
var
  Item: TinfMunDescargaCollectionItem;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.infDoc.infMunDescarga.New;

  Item.cMunDescarga := ObterConteudo(ANode.Childrens.Find('cMunDescarga'), tcInt);
  Item.xMunDescarga := ObterConteudo(ANode.Childrens.Find('xMunDescarga'), tcStr);

  ANodes := ANode.Childrens.FindAll('infCTe');
  Item.infCTe.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infCTe(ANodes[i], Item);
  end;

  ANodes := ANode.Childrens.FindAll('infCT');
  Item.infCT.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infCT(ANodes[i], Item);
  end;

  ANodes := ANode.Childrens.FindAll('infNFe');
  Item.infNFe.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infNFe(ANodes[i], Item);
  end;

  ANodes := ANode.Childrens.FindAll('infNF');
  Item.infNF.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infNF(ANodes[i], Item);
  end;

  ANodes := ANode.Childrens.FindAll('infMDFeTransp');
  Item.infMDFeTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infMDFeTransp(ANodes[i], Item);
  end;
end;

procedure TMDFeXmlReader.Ler_infCTe(const ANode: TACBrXmlNode;
  Item: TinfMunDescargaCollectionItem);
var
  ItemCTe: TinfCTeCollectionItem;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  ItemCTe := Item.infCTe.New;

  ItemCTe.chCTe := ObterConteudo(ANode.Childrens.Find('chCTe'), tcStr);
  ItemCTe.SegCodBarra := ObterConteudo(ANode.Childrens.Find('SegCodBarra'), tcStr);
  ItemCTe.indReentrega := ObterConteudo(ANode.Childrens.Find('indReentrega'), tcStr);

  ANodes := ANode.Childrens.FindAll('infUnidTransp');
  ItemCTe.infUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidTranspCTe(ANodes[i], ItemCTe);
  end;

  ANodes := ANode.Childrens.FindAll('peri');
  ItemCTe.peri.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_periCTe(ANodes[i], ItemCTe);
  end;

  AuxNode := ANode.Childrens.Find('infEntregaParcial');

  if Assigned(AuxNode) then
  begin
    ItemCTe.infEntregaParcial.qtdTotal := ObterConteudo(AuxNode.Childrens.Find('qtdTotal'), tcDe4);
    ItemCTe.infEntregaParcial.qtdParcial := ObterConteudo(AuxNode.Childrens.Find('qtdParcial'), tcDe4);
  end;
end;

procedure TMDFeXmlReader.Ler_infUnidTranspCTe(const ANode: TACBrXmlNode;
  Item: TinfCTeCollectionItem);
var
  ItemUnidTransp: TinfUnidTranspCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ItemUnidTransp := Item.infUnidTransp.New;

  ItemUnidTransp.tpUnidTransp := StrToUnidTransp(ok, ObterConteudo(ANode.Childrens.Find('tpUnidTransp'), tcStr));
  ItemUnidTransp.idUnidTransp := ObterConteudo(ANode.Childrens.Find('idUnidTransp'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidTransp');
  ItemUnidTransp.lacUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidTranspCTe(ANodes[i], ItemUnidTransp);
  end;

  ANodes := ANode.Childrens.FindAll('infUnidCarga');
  ItemUnidTransp.infUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidCargaCTe(ANodes[i], ItemUnidTransp);
  end;

  ItemUnidTransp.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe3);
end;

procedure TMDFeXmlReader.Ler_lacUnidTranspCTe(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  ItemlacUnidTransp: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidTransp := Item.lacUnidTransp.New;

  ItemlacUnidTransp.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infUnidCargaCTe(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  IteminfUnidCarga: TinfUnidCargaCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  IteminfUnidCarga := Item.infUnidCarga.New;

  IteminfUnidCarga.tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(ANode.Childrens.Find('tpUnidCarga'), tcStr));
  IteminfUnidCarga.idUnidCarga := ObterConteudo(ANode.Childrens.Find('idUnidCarga'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidCarga');
  IteminfUnidCarga.lacUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidCargaCTe(ANodes[i], IteminfUnidCarga);
  end;

  IteminfUnidCarga.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe2);
end;

procedure TMDFeXmlReader.Ler_lacUnidCargaCTe(const ANode: TACBrXmlNode;
  Item: TinfUnidCargaCollectionItem);
var
  ItemlacUnidCarga: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidCarga := Item.lacUnidCarga.New;

  ItemlacUnidCarga.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_periCTe(const ANode: TACBrXmlNode;
  Item: TinfCTeCollectionItem);
var
  itemPeri: TPeriCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  itemPeri := Item.peri.New;

  itemPeri.nONU := ObterConteudo(ANode.Childrens.Find('nONU'), tcStr);
  itemPeri.xNomeAE := ObterConteudo(ANode.Childrens.Find('xNomeAE'), tcStr);
  itemPeri.xClaRisco := ObterConteudo(ANode.Childrens.Find('xClaRisco'), tcStr);
  itemPeri.grEmb := ObterConteudo(ANode.Childrens.Find('grEmb'), tcStr);
  itemPeri.qTotProd := ObterConteudo(ANode.Childrens.Find('qTotProd'), tcStr);
  itemPeri.qVolTipo := ObterConteudo(ANode.Childrens.Find('qVolTipo'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infCT(const ANode: TACBrXmlNode;
  Item: TinfMunDescargaCollectionItem);
var
  ItemCT: TinfCTCollectionItem;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ItemCT := Item.infCT.New;

  ItemCT.nCT := ObterConteudo(ANode.Childrens.Find('nCT'), tcStr);
  ItemCT.serie := ObterConteudo(ANode.Childrens.Find('serie'), tcInt);
  ItemCT.subser := ObterConteudo(ANode.Childrens.Find('subser'), tcInt);
  ItemCT.dEmi := ObterConteudo(ANode.Childrens.Find('dEmi'), tcDat);
  ItemCT.vCarga := ObterConteudo(ANode.Childrens.Find('vCarga'), tcDe2);

  ANodes := ANode.Childrens.FindAll('infUnidTransp');
  ItemCT.infUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidTranspCT(ANodes[i], ItemCT);
  end;
end;

procedure TMDFeXmlReader.Ler_infUnidTranspCT(const ANode: TACBrXmlNode;
  Item: TinfCTCollectionItem);
var
  ItemUnidTransp: TinfUnidTranspCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ItemUnidTransp := Item.infUnidTransp.New;

  ItemUnidTransp.tpUnidTransp := StrToUnidTransp(ok, ObterConteudo(ANode.Childrens.Find('tpUnidTransp'), tcStr));
  ItemUnidTransp.idUnidTransp := ObterConteudo(ANode.Childrens.Find('idUnidTransp'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidTransp');
  ItemUnidTransp.lacUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidTranspCT(ANodes[i], ItemUnidTransp);
  end;

  ANodes := ANode.Childrens.FindAll('infUnidCarga');
  ItemUnidTransp.infUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidCargaCT(ANodes[i], ItemUnidTransp);
  end;

  ItemUnidTransp.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe3);
end;

procedure TMDFeXmlReader.Ler_lacUnidTranspCT(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  ItemlacUnidTransp: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidTransp := Item.lacUnidTransp.New;

  ItemlacUnidTransp.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infUnidCargaCT(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  IteminfUnidCarga: TinfUnidCargaCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  IteminfUnidCarga := Item.infUnidCarga.New;

  IteminfUnidCarga.tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(ANode.Childrens.Find('tpUnidCarga'), tcStr));
  IteminfUnidCarga.idUnidCarga := ObterConteudo(ANode.Childrens.Find('idUnidCarga'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidCarga');
  IteminfUnidCarga.lacUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidCargaCT(ANodes[i], IteminfUnidCarga);
  end;

  IteminfUnidCarga.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe2);
end;

procedure TMDFeXmlReader.Ler_lacUnidCargaCT(const ANode: TACBrXmlNode;
  Item: TinfUnidCargaCollectionItem);
var
  ItemlacUnidCarga: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidCarga := Item.lacUnidCarga.New;

  ItemlacUnidCarga.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infNFe(const ANode: TACBrXmlNode;
  Item: TinfMunDescargaCollectionItem);
var
  ItemNFe: TinfNFeCollectionItem;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ItemNFe := Item.infNFe.New;

  ItemNFe.chNFe := ObterConteudo(ANode.Childrens.Find('chNFe'), tcStr);
  ItemNFe.SegCodBarra := ObterConteudo(ANode.Childrens.Find('SegCodBarra'), tcStr);
  ItemNFe.indReentrega := ObterConteudo(ANode.Childrens.Find('indReentrega'), tcStr);

  ANodes := ANode.Childrens.FindAll('infUnidTransp');
  ItemNFe.infUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidTranspNFe(ANodes[i], ItemNFe);
  end;

  ANodes := ANode.Childrens.FindAll('peri');
  ItemNFe.peri.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_periNFe(ANodes[i], ItemNFe);
  end;
end;

procedure TMDFeXmlReader.Ler_infUnidTranspNFe(const ANode: TACBrXmlNode;
  Item: TinfNFeCollectionItem);
var
  ItemUnidTransp: TinfUnidTranspCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ItemUnidTransp := Item.infUnidTransp.New;

  ItemUnidTransp.tpUnidTransp := StrToUnidTransp(ok, ObterConteudo(ANode.Childrens.Find('tpUnidTransp'), tcStr));
  ItemUnidTransp.idUnidTransp := ObterConteudo(ANode.Childrens.Find('idUnidTransp'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidTransp');
  ItemUnidTransp.lacUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidTranspNFe(ANodes[i], ItemUnidTransp);
  end;

  ANodes := ANode.Childrens.FindAll('infUnidCarga');
  ItemUnidTransp.infUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidCargaNFe(ANodes[i], ItemUnidTransp);
  end;

  ItemUnidTransp.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe3);
end;

procedure TMDFeXmlReader.Ler_lacUnidTranspNFe(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  ItemlacUnidTransp: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidTransp := Item.lacUnidTransp.New;

  ItemlacUnidTransp.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infUnidCargaNFe(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  IteminfUnidCarga: TinfUnidCargaCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  IteminfUnidCarga := Item.infUnidCarga.New;

  IteminfUnidCarga.tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(ANode.Childrens.Find('tpUnidCarga'), tcStr));
  IteminfUnidCarga.idUnidCarga := ObterConteudo(ANode.Childrens.Find('idUnidCarga'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidCarga');
  IteminfUnidCarga.lacUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidCargaNFe(ANodes[i], IteminfUnidCarga);
  end;

  IteminfUnidCarga.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe2);
end;

procedure TMDFeXmlReader.Ler_lacUnidCargaNFe(const ANode: TACBrXmlNode;
  Item: TinfUnidCargaCollectionItem);
var
  ItemlacUnidCarga: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidCarga := Item.lacUnidCarga.New;

  ItemlacUnidCarga.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_periNFe(const ANode: TACBrXmlNode;
  Item: TinfNFeCollectionItem);
var
  itemPeri: TPeriCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  itemPeri := Item.peri.New;

  itemPeri.nONU := ObterConteudo(ANode.Childrens.Find('nONU'), tcStr);
  itemPeri.xNomeAE := ObterConteudo(ANode.Childrens.Find('xNomeAE'), tcStr);
  itemPeri.xClaRisco := ObterConteudo(ANode.Childrens.Find('xClaRisco'), tcStr);
  itemPeri.grEmb := ObterConteudo(ANode.Childrens.Find('grEmb'), tcStr);
  itemPeri.qTotProd := ObterConteudo(ANode.Childrens.Find('qTotProd'), tcStr);
  itemPeri.qVolTipo := ObterConteudo(ANode.Childrens.Find('qVolTipo'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infNF(const ANode: TACBrXmlNode;
  Item: TinfMunDescargaCollectionItem);
var
  ItemNF: TinfNFCollectionItem;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ItemNF := Item.infNF.New;

  ItemNF.CNPJ := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  ItemNF.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  ItemNF.nNF := ObterConteudo(ANode.Childrens.Find('nNF'), tcStr);
  ItemNF.serie := ObterConteudo(ANode.Childrens.Find('serie'), tcInt);
  ItemNF.dEmi := ObterConteudo(ANode.Childrens.Find('dEmi'), tcDat);
  ItemNF.vNF := ObterConteudo(ANode.Childrens.Find('vNF'), tcDe2);
  ItemNF.PIN := ObterConteudo(ANode.Childrens.Find('PIN'), tcInt);

  ANodes := ANode.Childrens.FindAll('infUnidTransp');
  ItemNF.infUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidTranspNF(ANodes[i], ItemNF);
  end;
end;

procedure TMDFeXmlReader.Ler_infUnidTranspNF(const ANode: TACBrXmlNode;
  Item: TinfNFCollectionItem);
var
  ItemUnidTransp: TinfUnidTranspCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ItemUnidTransp := Item.infUnidTransp.New;

  ItemUnidTransp.tpUnidTransp := StrToUnidTransp(ok, ObterConteudo(ANode.Childrens.Find('tpUnidTransp'), tcStr));
  ItemUnidTransp.idUnidTransp := ObterConteudo(ANode.Childrens.Find('idUnidTransp'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidTransp');
  ItemUnidTransp.lacUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidTranspNF(ANodes[i], ItemUnidTransp);
  end;

  ANodes := ANode.Childrens.FindAll('infUnidCarga');
  ItemUnidTransp.infUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidCargaNF(ANodes[i], ItemUnidTransp);
  end;

  ItemUnidTransp.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe3);
end;

procedure TMDFeXmlReader.Ler_lacUnidTranspNF(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  ItemlacUnidTransp: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidTransp := Item.lacUnidTransp.New;

  ItemlacUnidTransp.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infUnidCargaNF(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  IteminfUnidCarga: TinfUnidCargaCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  IteminfUnidCarga := Item.infUnidCarga.New;

  IteminfUnidCarga.tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(ANode.Childrens.Find('tpUnidCarga'), tcStr));
  IteminfUnidCarga.idUnidCarga := ObterConteudo(ANode.Childrens.Find('idUnidCarga'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidCarga');
  IteminfUnidCarga.lacUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidCargaNF(ANodes[i], IteminfUnidCarga);
  end;

  IteminfUnidCarga.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe2);
end;

procedure TMDFeXmlReader.Ler_lacUnidCargaNF(const ANode: TACBrXmlNode;
  Item: TinfUnidCargaCollectionItem);
var
  ItemlacUnidCarga: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidCarga := Item.lacUnidCarga.New;

  ItemlacUnidCarga.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infMDFeTransp(const ANode: TACBrXmlNode;
  Item: TinfMunDescargaCollectionItem);
var
  ItemMDFe: TinfMDFeTranspCollectionItem;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ItemMDFe := Item.infMDFeTransp.New;

  ItemMDFe.chMDFe := ObterConteudo(ANode.Childrens.Find('chMDFe'), tcStr);
  ItemMDFe.indReentrega := ObterConteudo(ANode.Childrens.Find('indReentrega'), tcStr);

  ANodes := ANode.Childrens.FindAll('infUnidTransp');
  ItemMDFe.infUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidTranspMDFe(ANodes[i], ItemMDFe);
  end;

  ANodes := ANode.Childrens.FindAll('peri');
  ItemMDFe.peri.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_periMDFe(ANodes[i], ItemMDFe);
  end;
end;

procedure TMDFeXmlReader.Ler_infUnidTranspMDFe(const ANode: TACBrXmlNode;
  Item: TinfMDFeTranspCollectionItem);
var
  ItemUnidTransp: TinfUnidTranspCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  ItemUnidTransp := Item.infUnidTransp.New;

  ItemUnidTransp.tpUnidTransp := StrToUnidTransp(ok, ObterConteudo(ANode.Childrens.Find('tpUnidTransp'), tcStr));
  ItemUnidTransp.idUnidTransp := ObterConteudo(ANode.Childrens.Find('idUnidTransp'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidTransp');
  ItemUnidTransp.lacUnidTransp.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidTranspMDFe(ANodes[i], ItemUnidTransp);
  end;

  ANodes := ANode.Childrens.FindAll('infUnidCarga');
  ItemUnidTransp.infUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_infUnidCargaMDFe(ANodes[i], ItemUnidTransp);
  end;

  ItemUnidTransp.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe3);
end;

procedure TMDFeXmlReader.Ler_lacUnidTranspMDFe(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  ItemlacUnidTransp: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidTransp := Item.lacUnidTransp.New;

  ItemlacUnidTransp.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infUnidCargaMDFe(const ANode: TACBrXmlNode;
  Item: TinfUnidTranspCollectionItem);
var
  IteminfUnidCarga: TinfUnidCargaCollectionItem;
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  IteminfUnidCarga := Item.infUnidCarga.New;

  IteminfUnidCarga.tpUnidCarga := StrToUnidCarga(ok, ObterConteudo(ANode.Childrens.Find('tpUnidCarga'), tcStr));
  IteminfUnidCarga.idUnidCarga := ObterConteudo(ANode.Childrens.Find('idUnidCarga'), tcStr);

  ANodes := ANode.Childrens.FindAll('lacUnidCarga');
  IteminfUnidCarga.lacUnidCarga.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_lacUnidCargaMDFe(ANodes[i], IteminfUnidCarga);
  end;

  IteminfUnidCarga.qtdRat := ObterConteudo(ANode.Childrens.Find('qtdRat'), tcDe2);
end;

procedure TMDFeXmlReader.Ler_lacUnidCargaMDFe(const ANode: TACBrXmlNode;
  Item: TinfUnidCargaCollectionItem);
var
  ItemlacUnidCarga: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemlacUnidCarga := Item.lacUnidCarga.New;

  ItemlacUnidCarga.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_periMDFe(const ANode: TACBrXmlNode;
  Item: TinfMDFeTranspCollectionItem);
var
  itemPeri: TPeriCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  itemPeri := Item.peri.New;

  itemPeri.nONU := ObterConteudo(ANode.Childrens.Find('nONU'), tcStr);
  itemPeri.xNomeAE := ObterConteudo(ANode.Childrens.Find('xNomeAE'), tcStr);
  itemPeri.xClaRisco := ObterConteudo(ANode.Childrens.Find('xClaRisco'), tcStr);
  itemPeri.grEmb := ObterConteudo(ANode.Childrens.Find('grEmb'), tcStr);
  itemPeri.qTotProd := ObterConteudo(ANode.Childrens.Find('qTotProd'), tcStr);
  itemPeri.qVolTipo := ObterConteudo(ANode.Childrens.Find('qVolTipo'), tcStr);
end;

procedure TMDFeXmlReader.Ler_seg(const ANode: TACBrXmlNode);
var
  Item: TSegCollectionItem;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.seg.New;

  Ler_infResp(ANode.Childrens.Find('infResp'), Item);
  Ler_infSeg(ANode.Childrens.Find('infSeg'), Item);

  Item.nApol := ObterConteudo(ANode.Childrens.Find('nApol'), tcStr);

  ANodes := ANode.Childrens.FindAll('nAver');
  Item.aver.Clear;
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_nAver(ANodes[i], Item);
  end;
end;

procedure TMDFeXmlReader.Ler_infResp(const ANode: TACBrXmlNode;
  Item: TSegCollectionItem);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Item.respSeg := StrToRspSeguroMDFe(ok, ObterConteudo(ANode.Childrens.Find('respSeg'), tcStr));
  Item.CNPJCPF := ObterCNPJCPF(ANode);
end;

procedure TMDFeXmlReader.Ler_infSeg(const ANode: TACBrXmlNode;
  Item: TSegCollectionItem);
begin
  if not Assigned(ANode) then Exit;

  Item.xSeg := ObterConteudo(ANode.Childrens.Find('xSeg'), tcStr);
  Item.CNPJ := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
end;

procedure TMDFeXmlReader.Ler_nAver(const ANode: TACBrXmlNode;
  Item: TSegCollectionItem);
var
  ItemAver: TAverCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  ItemAver := Item.aver.New;

  ItemAver.nAver := ANode.Content;
end;

procedure TMDFeXmlReader.Ler_prodPred(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  MDFe.prodPred.tpCarga := StrToTCarga(ok, ObterConteudo(ANode.Childrens.Find('tpCarga'), tcStr));
  MDFe.prodPred.xProd := ObterConteudo(ANode.Childrens.Find('xProd'), tcStr);
  MDFe.prodPred.cEAN := ObterConteudo(ANode.Childrens.Find('cEAN'), tcStr);
  MDFe.prodPred.NCM := ObterConteudo(ANode.Childrens.Find('NCM'), tcStr);

  Ler_infLotacao(ANode.Childrens.Find('infLotacao'));
end;

procedure TMDFeXmlReader.Ler_infLotacao(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  Ler_infLocalCarrega(ANode.Childrens.Find('infLocalCarrega'));
  Ler_infLocalDescarrega(ANode.Childrens.Find('infLocalDescarrega'));
end;

procedure TMDFeXmlReader.Ler_infLocalCarrega(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  MDFe.prodPred.infLocalCarrega.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  MDFe.prodPred.infLocalCarrega.latitude := ObterConteudo(ANode.Childrens.Find('latitude'), tcDe6);
  MDFe.prodPred.infLocalCarrega.longitude := ObterConteudo(ANode.Childrens.Find('longitude'), tcDe6);
end;

procedure TMDFeXmlReader.Ler_infLocalDescarrega(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  MDFe.prodPred.infLocalDescarrega.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  MDFe.prodPred.infLocalDescarrega.latitude := ObterConteudo(ANode.Childrens.Find('latitude'), tcDe6);
  MDFe.prodPred.infLocalDescarrega.longitude := ObterConteudo(ANode.Childrens.Find('longitude'), tcDe6);
end;

procedure TMDFeXmlReader.Ler_tot(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  MDFe.tot.qCTe := ObterConteudo(ANode.Childrens.Find('qCTe'), tcInt);
  MDFe.tot.qCT := ObterConteudo(ANode.Childrens.Find('qCT'), tcInt);
  MDFe.tot.qNFe := ObterConteudo(ANode.Childrens.Find('qNFe'), tcInt);
  MDFe.tot.qNF := ObterConteudo(ANode.Childrens.Find('qNF'), tcInt);
  MDFe.tot.qMDFe := ObterConteudo(ANode.Childrens.Find('qMDFe'), tcInt);
  MDFe.tot.vCarga := ObterConteudo(ANode.Childrens.Find('vCarga'), tcDe2);
  MDFe.tot.cUnid := StrToUnidMed(Ok, ObterConteudo(ANode.Childrens.Find('cUnid'), tcStr));
  MDFe.tot.qCarga := ObterConteudo(ANode.Childrens.Find('qCarga'), tcDe4);
end;

procedure TMDFeXmlReader.Ler_lacres(const ANode: TACBrXmlNode);
var
  Item: TlacresCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.lacres.New;

  Item.nLacre := ObterConteudo(ANode.Childrens.Find('nLacre'), tcStr);
end;

procedure TMDFeXmlReader.Ler_autXML(const ANode: TACBrXmlNode);
var
  Item: TautXMLCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := MDFe.autXML.New;

  Item.CNPJCPF := ObterCNPJCPF(ANode);
end;

procedure TMDFeXmlReader.Ler_InfAdic(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  MDFe.InfAdic.infAdFisco := ObterConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);
  MDFe.InfAdic.infCpl := ObterConteudo(ANode.Childrens.Find('infCpl'), tcStr);
end;

procedure TMDFeXmlReader.Ler_infRespTec(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  MDFe.infRespTec.CNPJ     := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  MDFe.infRespTec.xContato := ObterConteudo(ANode.Childrens.Find('xContato'), tcStr);
  MDFe.infRespTec.email    := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  MDFe.infRespTec.fone     := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  MDFe.infRespTec.idCSRT   := ObterConteudo(ANode.Childrens.Find('idCSRT'), tcInt);
  MDFe.infRespTec.hashCSRT := ObterConteudo(ANode.Childrens.Find('hashCSRT'), tcStr);
end;

procedure TMDFeXmlReader.Ler_InfMDFeSupl(const ANode: TACBrXmlNode);
var
 sQrCode: string;
begin
  if not Assigned(ANode) then Exit;

  sQrCode := ObterConteudo(ANode.Childrens.Find('qrCodMDFe'), tcStr);
  sQrCode := StringReplace(sQrCode, '<![CDATA[', '', []);
  sQrCode := StringReplace(sQrCode, ']]>', '', []);

  MDFe.infMDFeSupl.qrCodMDFe := sQrCode;
end;

end.

