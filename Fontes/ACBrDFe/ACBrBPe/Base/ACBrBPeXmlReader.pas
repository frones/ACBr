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

unit ACBrBPeXmlReader;

interface

uses
  Classes, SysUtils,
  ACBrXmlDocument, ACBrXmlReader,
  ACBrBPeClass;

type
  { TBPeXmlReader }
  TBPeXmlReader = class(TACBrXmlReader)
  private
    FBPe: TBPe;

    function NodeNaoEncontrado(const ANode: TACBrXmlNode): Boolean;

    procedure Ler_ProtBPe(const ANode: TACBrXmlNode);
    procedure Ler_InfBPe(const ANode: TACBrXmlNode);
    procedure Ler_Ide(const ANode: TACBrXmlNode);

    procedure Ler_Emit(const ANode: TACBrXmlNode);
    procedure Ler_EnderEmit(const ANode: TACBrXmlNode);

    procedure Ler_Comp(const ANode: TACBrXmlNode);
    procedure Ler_EnderComp(const ANode: TACBrXmlNode);

    procedure Ler_Agencia(const ANode: TACBrXmlNode);
    procedure Ler_EnderAgencia(const ANode: TACBrXmlNode);

    procedure Ler_infBPeSub(const ANode: TACBrXmlNode);
    procedure Ler_infPassagem(const ANode: TACBrXmlNode);
    procedure Ler_InfPassageiro(const ANode: TACBrXmlNode);

    procedure Ler_infViagem(const ANode: TACBrXmlNode);
    procedure Ler_infValorBPe(const ANode: TACBrXmlNode);
    procedure Ler_CompValor(const ANode: TACBrXmlNode);

    procedure Ler_imp(const ANode: TACBrXmlNode);
    procedure Ler_pag(const ANode: TACBrXmlNode);

    // BPe TM
    procedure Ler_detBPeTM(const ANode: TACBrXmlNode);

    procedure Ler_Total(const ANode: TACBrXmlNode);

    procedure Ler_autXML(const ANode: TACBrXmlNode);
    procedure Ler_InfAdic(const ANode: TACBrXmlNode);
    procedure Ler_infRespTec(const ANode: TACBrXmlNode);

    procedure Ler_InfBPeSupl(const ANode: TACBrXmlNode);
    procedure Ler_Signature(const ANode: TACBrXmlNode);
  public
    constructor Create(AOwner: TBPe); reintroduce;

    function LerXml: Boolean; override;

    property BPe: TBPe read FBPe write FBPe;
  end;

implementation

uses
  ACBrXmlBase, ACBrUtil.Base,
  ACBrBPeConversao;

{ TBPeXmlReader }

constructor TBPeXmlReader.Create(AOwner: TBPe);
begin
  inherited Create;

  FBPe := AOwner;
end;

{
procedure TBPeXmlReader.LergSCEE(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

//  BPe.gSCEE.tpPartComp := StrTotpPartComp(ok, ObterConteudo(ANode.Childrens.Find('tpPartComp'), tcStr));

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
end;
}
function TBPeXmlReader.NodeNaoEncontrado(const ANode: TACBrXmlNode): Boolean;
begin
  Result := not Assigned(ANode);
end;

function TBPeXmlReader.LerXml: Boolean;
Var
  BPeNode, infBPeNode: TACBrXmlNode;
  att: TACBrXmlAttribute;
begin
  if not Assigned(FBPe) or (FBPe = nil) then
    raise Exception.Create('Destino não informado, informe a classe [TBPe] de destino.');

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml da BPe não carregado.');

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  if Document.Root.Name = 'BPeProc' then
  begin
    Ler_ProtBPe(Document.Root.Childrens.Find('protBPe').Childrens.Find('infProt'));
    BPeNode := Document.Root.Childrens.Find('BPe');
  end
  else
  begin
    BPeNode := Document.Root;
  end;

  if BPeNode <> nil then
  begin
    infBPeNode := BPeNode.Childrens.Find('infBPe');

    if infBPeNode = nil then
      raise Exception.Create('Arquivo xml incorreto.');
  end;

  att := infBPeNode.Attributes.Items['Id'];

  if att = nil then
    raise Exception.Create('Não encontrei o atributo: Id');

  BPe.infBPe.Id := att.Content;

  att := infBPeNode.Attributes.Items['versao'];

  if att = nil then
    raise Exception.Create('Não encontrei o atributo: versao');

  BPe.infBPe.Versao := StringToFloat(att.Content);

  Ler_InfBPe(infBPeNode);
  Ler_InfBPeSupl(BPeNode.Childrens.Find('infBPeSupl'));
  Ler_Signature(BPeNode.Childrens.Find('Signature'));

  Result := True;
end;

procedure TBPeXmlReader.Ler_ProtBPe(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  BPe.procBPe.tpAmb    := StrToTipoAmbiente(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  BPe.procBPe.verAplic := ObterConteudo(ANode.Childrens.Find('verAplic'), tcStr);
  BPe.procBPe.chDFe    := ObterConteudo(ANode.Childrens.Find('chBPe'), tcStr);
  BPe.procBPe.dhRecbto := ObterConteudo(ANode.Childrens.Find('dhRecbto'), tcDatHor);
  BPe.procBPe.nProt    := ObterConteudo(ANode.Childrens.Find('nProt'), tcStr);
  BPe.procBPe.digVal   := ObterConteudo(ANode.Childrens.Find('digVal'), tcStr);
  BPe.procBPe.cStat    := ObterConteudo(ANode.Childrens.Find('cStat'), tcInt);
  BPe.procBPe.xMotivo  := ObterConteudo(ANode.Childrens.Find('xMotivo'), tcStr);
  BPe.procBPe.cMsg     := ObterConteudo(ANode.Childrens.Find('cMsg'), tcInt);
  BPe.procBPe.xMsg     := ObterConteudo(ANode.Childrens.Find('xMsg'), tcStr);
end;

procedure TBPeXmlReader.Ler_InfBPe(const ANode: TACBrXmlNode);
var
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  Ler_Ide(ANode.Childrens.Find('ide'));
  Ler_Emit(ANode.Childrens.Find('emit'));

  if BPe.ide.tpBPe = tbBPeTM then
  begin
    ANodes := ANode.Childrens.FindAll('detBPeTM');
    for i := 0 to Length(ANodes) - 1 do
    begin
      Ler_detBPeTM(ANodes[i]);
    end;

    Ler_Total(ANode.Childrens.Find('total'));
  end
  else
  begin
    Ler_Comp(ANode.Childrens.Find('comp'));
    Ler_Agencia(ANode.Childrens.Find('agencia'));
    Ler_infBPeSub(ANode.Childrens.Find('infBPeSub'));
    Ler_infPassagem(ANode.Childrens.Find('infPassagem'));

    ANodes := ANode.Childrens.FindAll('infViagem');
    for i := 0 to Length(ANodes) - 1 do
    begin
      Ler_infViagem(ANodes[i]);
    end;

    Ler_infValorBPe(ANode.Childrens.Find('infValorBPe'));
    Ler_imp(ANode.Childrens.Find('imp'));

    ANodes := ANode.Childrens.FindAll('pag');
    for i := 0 to Length(ANodes) - 1 do
    begin
      Ler_pag(ANodes[i]);
    end;
  end;

  ANodes := ANode.Childrens.FindAll('autXML');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_autXML(ANodes[i]);
  end;

  Ler_InfAdic(ANode.Childrens.Find('infAdic'));

  Ler_infRespTec(ANode.Childrens.Find('infRespTec'));
end;

procedure TBPeXmlReader.Ler_Ide(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  BPe.ide.cUF := ObterConteudo(ANode.Childrens.Find('cUF'), tcInt);
  BPe.Ide.tpAmb := StrToTipoAmbiente(ok, ObterConteudo(ANode.Childrens.Find('tpAmb'), tcStr));
  BPe.ide.modelo := ObterConteudo(ANode.Childrens.Find('mod'), tcInt);
  BPe.ide.serie := ObterConteudo(ANode.Childrens.Find('serie'), tcInt);
  BPe.ide.nBP := ObterConteudo(ANode.Childrens.Find('nBP'), tcInt);
  BPe.ide.cBP := ObterConteudo(ANode.Childrens.Find('cBP'), tcInt);
  BPe.Ide.cDV := ObterConteudo(ANode.Childrens.Find('cDV'), tcInt);
  BPe.Ide.modal := StrToModalBPe(ObterConteudo(ANode.Childrens.Find('modal'), tcStr));
  BPe.ide.dhEmi := ObterConteudo(ANode.Childrens.Find('dhEmi'), tcDatHor);
  BPe.ide.dCompet := ObterConteudo(ANode.Childrens.Find('dCompet'), tcDat);
  BPe.Ide.tpEmis := StrToTipoEmissao(ok, ObterConteudo(ANode.Childrens.Find('tpEmis'), tcStr));
  BPe.Ide.verProc := ObterConteudo(ANode.Childrens.Find('verProc'), tcStr);
  BPe.Ide.tpBPe := StrToTpBPe(ObterConteudo(ANode.Childrens.Find('tpBPe'), tcStr));
  BPe.Ide.indPres := StrToPresencaComprador(ObterConteudo(ANode.Childrens.Find('indPres'), tcStr));
  BPe.Ide.UFIni := ObterConteudo(ANode.Childrens.Find('UFIni'), tcStr);
  BPe.Ide.cMunIni := ObterConteudo(ANode.Childrens.Find('cMunIni'), tcInt);
  BPe.Ide.UFFim := ObterConteudo(ANode.Childrens.Find('UFFim'), tcStr);
  BPe.Ide.cMunFim := ObterConteudo(ANode.Childrens.Find('cMunFim'), tcInt);
  BPe.Ide.dhCont := ObterConteudo(ANode.Childrens.Find('dhCont'), tcDatHor);
  BPe.Ide.xJust := ObterConteudo(ANode.Childrens.Find('xJust'), tcStr);
  BPe.Ide.CFOP := ObterConteudo(ANode.Childrens.Find('CFOP'), tcInt);
end;

procedure TBPeXmlReader.Ler_Emit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.Emit.CNPJ := ObterCNPJCPF(ANode);
  BPe.Emit.IE := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);
  BPe.Emit.IEST := ObterConteudo(ANode.Childrens.Find('IEST'), tcStr);
  BPe.Emit.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  BPe.Emit.xFant := ObterConteudo(ANode.Childrens.Find('xFant'), tcStr);
  BPe.Emit.IM := ObterConteudo(ANode.Childrens.Find('IM'), tcStr);
  BPe.Emit.CNAE := ObterConteudo(ANode.Childrens.Find('CNAE'), tcStr);
  BPe.Emit.CRT := StrToCRT(ObterConteudo(ANode.Childrens.Find('CRT'), tcStr));

  Ler_EnderEmit(ANode.Childrens.Find('enderEmit'));

  BPe.Emit.TAR := ObterConteudo(ANode.Childrens.Find('TAR'), tcStr);
end;

procedure TBPeXmlReader.Ler_EnderEmit(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.Emit.enderEmit.xLgr := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  BPe.Emit.enderEmit.nro := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  BPe.Emit.enderEmit.xCpl := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  BPe.Emit.enderEmit.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  BPe.Emit.EnderEmit.cMun := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  BPe.Emit.enderEmit.xMun := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  BPe.Emit.enderEmit.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  BPe.Emit.enderEmit.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  BPe.Emit.enderEmit.fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  BPe.Emit.enderEmit.email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
end;

procedure TBPeXmlReader.Ler_Comp(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.Comp.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);

  BPe.Comp.CNPJCPF := ObterCNPJCPF(ANode);

  if BPe.Comp.CNPJCPF = '' then
    BPe.Comp.idEstrangeiro := ObterConteudo(ANode.Childrens.Find('idEstrangeiro'), tcStr);

  BPe.Comp.IE := ObterConteudo(ANode.Childrens.Find('IE'), tcStr);

  Ler_EnderComp(ANode.Childrens.Find('enderComp'));
end;

procedure TBPeXmlReader.Ler_EnderComp(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.Comp.EnderComp.xLgr := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  BPe.Comp.EnderComp.nro := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  BPe.Comp.EnderComp.xCpl := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  BPe.Comp.EnderComp.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  BPe.Comp.EnderComp.cMun := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  BPe.Comp.EnderComp.xMun := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  BPe.Comp.EnderComp.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  BPe.Comp.EnderComp.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  BPe.Comp.EnderComp.cPais := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);
  BPe.Comp.EnderComp.xPais := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);
  BPe.Comp.EnderComp.fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  BPe.Comp.EnderComp.email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
end;

procedure TBPeXmlReader.Ler_Agencia(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.Agencia.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);

  BPe.Agencia.CNPJ := ObterCNPJCPF(ANode);

  Ler_EnderAgencia(ANode.Childrens.Find('enderAgencia'));
end;

procedure TBPeXmlReader.Ler_EnderAgencia(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.Agencia.EnderAgencia.xLgr := ObterConteudo(ANode.Childrens.Find('xLgr'), tcStr);
  BPe.Agencia.EnderAgencia.nro := ObterConteudo(ANode.Childrens.Find('nro'), tcStr);
  BPe.Agencia.EnderAgencia.xCpl := ObterConteudo(ANode.Childrens.Find('xCpl'), tcStr);
  BPe.Agencia.EnderAgencia.xBairro := ObterConteudo(ANode.Childrens.Find('xBairro'), tcStr);
  BPe.Agencia.EnderAgencia.cMun := ObterConteudo(ANode.Childrens.Find('cMun'), tcInt);
  BPe.Agencia.EnderAgencia.xMun := ObterConteudo(ANode.Childrens.Find('xMun'), tcStr);
  BPe.Agencia.EnderAgencia.CEP := ObterConteudo(ANode.Childrens.Find('CEP'), tcInt);
  BPe.Agencia.EnderAgencia.UF := ObterConteudo(ANode.Childrens.Find('UF'), tcStr);
  BPe.Agencia.EnderAgencia.cPais := ObterConteudo(ANode.Childrens.Find('cPais'), tcInt);
  BPe.Agencia.EnderAgencia.xPais := ObterConteudo(ANode.Childrens.Find('xPais'), tcStr);
  BPe.Agencia.EnderAgencia.fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  BPe.Agencia.EnderAgencia.email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
end;

procedure TBPeXmlReader.Ler_infBPeSub(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.infBPeSub.chBPe := ObterConteudo(ANode.Childrens.Find('chBPe'), tcStr);
  BPe.infBPeSub.tpSub := StrTotpSubstituicao(ObterConteudo(ANode.Childrens.Find('tpSub'), tcStr));
end;

procedure TBPeXmlReader.Ler_infPassagem(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.infPassagem.cLocOrig := ObterConteudo(ANode.Childrens.Find('cLocOrig'), tcStr);
  BPe.infPassagem.xLocOrig := ObterConteudo(ANode.Childrens.Find('xLocOrig'), tcStr);
  BPe.infPassagem.cLocDest := ObterConteudo(ANode.Childrens.Find('cLocDest'), tcStr);
  BPe.infPassagem.xLocDest := ObterConteudo(ANode.Childrens.Find('xLocDest'), tcStr);
  BPe.infPassagem.dhEmb := ObterConteudo(ANode.Childrens.Find('dhEmb'), tcDatHor);
  BPe.infPassagem.dhValidade := ObterConteudo(ANode.Childrens.Find('dhValidade'), tcDatHor);

  Ler_InfPassageiro(ANode.Childrens.Find('infPassageiro'));
end;

procedure TBPeXmlReader.Ler_InfPassageiro(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.infPassagem.infPassageiro.xNome := ObterConteudo(ANode.Childrens.Find('xNome'), tcStr);
  BPe.infPassagem.infPassageiro.CPF := ObterConteudo(ANode.Childrens.Find('CPF'), tcStr);
  BPe.infPassagem.infPassageiro.tpDoc := StrTotpDocumento(ObterConteudo(ANode.Childrens.Find('tpDoc'), tcStr));
  BPe.infPassagem.infPassageiro.nDoc := ObterConteudo(ANode.Childrens.Find('nDoc'), tcStr);
  BPe.infPassagem.infPassageiro.xDoc := ObterConteudo(ANode.Childrens.Find('xDoc'), tcStr);
  BPe.infPassagem.infPassageiro.dNasc := ObterConteudo(ANode.Childrens.Find('dNasc'), tcDat);
  BPe.infPassagem.infPassageiro.Fone := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  BPe.infPassagem.infPassageiro.Email := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
end;

procedure TBPeXmlReader.Ler_infViagem(const ANode: TACBrXmlNode);
var
  Item: TInfViagemCollectionItem;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  Item := BPe.infViagem.New;

  Item.cPercurso := ObterConteudo(ANode.Childrens.Find('cPercurso'), tcStr);
  Item.xPercurso := ObterConteudo(ANode.Childrens.Find('xPercurso'), tcStr);
  Item.tpViagem := StrTotpViagem(ObterConteudo(ANode.Childrens.Find('tpViagem'), tcStr));
  Item.tpServ := StrTotpServico(ObterConteudo(ANode.Childrens.Find('tpServ'), tcStr));
  Item.tpAcomodacao := StrTotpAcomodacao(ObterConteudo(ANode.Childrens.Find('tpAcomodacao'), tcStr));
  Item.tpTrecho := StrTotpTrecho(ObterConteudo(ANode.Childrens.Find('tpTrecho'), tcStr));

  Item.dhViagem := ObterConteudo(ANode.Childrens.Find('dhViagem'), tcDatHor);
  Item.dhConexao := ObterConteudo(ANode.Childrens.Find('dhConexao'), tcDatHor);
  Item.prefixo := ObterConteudo(ANode.Childrens.Find('prefixo'), tcStr);
  Item.poltrona := ObterConteudo(ANode.Childrens.Find('poltrona'), tcInt);
  Item.plataforma := ObterConteudo(ANode.Childrens.Find('plataforma'), tcStr);

  AuxNode := ANode.Childrens.Find('infTravessia');

  if not (NodeNaoEncontrado(AuxNode)) then
  begin
    Item.infTravessia.tpVeiculo := StrTotpVeiculo(ObterConteudo(AuxNode.Childrens.Find('tpVeiculo'), tcStr));
    Item.infTravessia.sitVeiculo := StrToSitVeiculo(ObterConteudo(AuxNode.Childrens.Find('sitVeiculo'), tcStr));
  end;
end;

procedure TBPeXmlReader.Ler_infValorBPe(const ANode: TACBrXmlNode);
var
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  BPe.infValorBPe.vBP := ObterConteudo(ANode.Childrens.Find('vBP'), tcDe2);
  BPe.infValorBPe.vDesconto := ObterConteudo(ANode.Childrens.Find('vDesconto'), tcDe2);
  BPe.infValorBPe.vPgto := ObterConteudo(ANode.Childrens.Find('vPgto'), tcDe2);
  BPe.infValorBPe.vTroco := ObterConteudo(ANode.Childrens.Find('vTroco'), tcDe2);
  BPe.infValorBPe.tpDesconto := StrTotpDesconto(ObterConteudo(ANode.Childrens.Find('tpDesconto'), tcStr));
  BPe.infValorBPe.xDesconto := ObterConteudo(ANode.Childrens.Find('xDesconto'), tcStr);
  BPe.infValorBPe.cDesconto := ObterConteudo(ANode.Childrens.Find('cDesconto'), tcStr);

  ANodes := ANode.Childrens.FindAll('Comp');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_CompValor(ANodes[i]);
  end;
end;

procedure TBPeXmlReader.Ler_CompValor(const ANode: TACBrXmlNode);
var
  Item: TCompCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := BPe.infValorBPe.Comp.New;

  Item.tpComp := StrTotpComponente(ObterConteudo(ANode.Childrens.Find('tpComp'), tcStr));
  Item.vComp := ObterConteudo(ANode.Childrens.Find('vComp'), tcDe2);
end;

procedure TBPeXmlReader.Ler_imp(const ANode: TACBrXmlNode);
var
  AuxNode, AuxNode2: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  BPe.Imp.vTotTrib := ObterConteudo(ANode.Childrens.Find('vTotTrib'), tcDe2);
  BPe.Imp.infAdFisco := ObterConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);

  AuxNode := ANode.Childrens.Find('ICMS');

  if (AuxNode <> nil) then
    AuxNode := AuxNode.Childrens.Items[0];

  if AuxNode <> nil then
  begin
    BPe.Imp.ICMS.CST := StrToCSTICMS(ObterConteudo(AuxNode.Childrens.Find('CST'), tcStr));
    BPe.Imp.ICMS.vBC := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);

    BPe.Imp.ICMS.pICMS := ObterConteudo(AuxNode.Childrens.Find('pICMS'), tcDe2);
    BPe.Imp.ICMS.vICMS := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
    BPe.Imp.ICMS.pRedBC := ObterConteudo(AuxNode.Childrens.Find('pRedBC'), tcDe2);
    BPe.Imp.ICMS.vCred := ObterConteudo(AuxNode.Childrens.Find('vCred'), tcDe2);
    BPe.Imp.ICMS.pRedBCOutraUF := ObterConteudo(AuxNode.Childrens.Find('pRedBCOutraUF'), tcDe2);
    BPe.Imp.ICMS.vBCOutraUF := ObterConteudo(AuxNode.Childrens.Find('vBCOutraUF'), tcDe2);
    BPe.Imp.ICMS.pICMSOutraUF := ObterConteudo(AuxNode.Childrens.Find('pICMSOutraUF'), tcDe2);
    BPe.Imp.ICMS.vICMSOutraUF := ObterConteudo(AuxNode.Childrens.Find('vICMSOutraUF'), tcDe2);

    AuxNode2 := AuxNode.Childrens.Find('ICMSSN');

    if AuxNode2 <> nil then
      BPe.Imp.ICMS.CST := cstSN;
  end;

  AuxNode := ANode.Childrens.Find('ICMSUFFim');

  if AuxNode <> nil then
  begin
    BPe.Imp.ICMSUFFim.vBCUFFim := ObterConteudo(AuxNode.Childrens.Find('vBCUFFim'), tcDe2);
    BPe.Imp.ICMSUFFim.pFCPUFFim := ObterConteudo(AuxNode.Childrens.Find('pFCPUFFim'), tcDe2);
    BPe.Imp.ICMSUFFim.pICMSUFFim := ObterConteudo(AuxNode.Childrens.Find('pICMSUFFim'), tcDe2);
    BPe.Imp.ICMSUFFim.pICMSInter := ObterConteudo(AuxNode.Childrens.Find('pICMSInter'), tcDe2);
    BPe.Imp.ICMSUFFim.pICMSInterPart := ObterConteudo(AuxNode.Childrens.Find('pICMSInterPart'), tcDe2);
    BPe.Imp.ICMSUFFim.vFCPUFFim := ObterConteudo(AuxNode.Childrens.Find('vFCPUFFim'), tcDe2);
    BPe.Imp.ICMSUFFim.vICMSUFFim := ObterConteudo(AuxNode.Childrens.Find('vICMSUFFim'), tcDe2);
    BPe.Imp.ICMSUFFim.vICMSUFIni := ObterConteudo(AuxNode.Childrens.Find('vICMSUFIni'), tcDe2);
  end;
end;

procedure TBPeXmlReader.Ler_pag(const ANode: TACBrXmlNode);
var
  Item: TpagCollectionItem;
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  Item := BPe.Pag.New;

  Item.tPag := StrToFormaPagamentoBPe(ObterConteudo(ANode.Childrens.Find('tPag'), tcStr));
  Item.xPag := ObterConteudo(ANode.Childrens.Find('xPag'), tcStr);
  Item.nDocPag := ObterConteudo(ANode.Childrens.Find('nDocPag'), tcStr);
  Item.vPag := ObterConteudo(ANode.Childrens.Find('vPag'), tcDe2);

  AuxNode := ANode.Childrens.Find('card');
  if (AuxNode <> nil) then
  begin
    Item.tpIntegra := StrTotpIntegra(ObterConteudo(AuxNode.Childrens.Find('tpIntegra'), tcStr));
    Item.CNPJ := ObterConteudo(AuxNode.Childrens.Find('CNPJ'), tcStr);
    Item.tBand := StrToBandeiraCard(ObterConteudo(AuxNode.Childrens.Find('tBand'), tcStr));
    Item.xBand := ObterConteudo(AuxNode.Childrens.Find('xBand'), tcStr);
    Item.cAut := ObterConteudo(AuxNode.Childrens.Find('cAut'), tcStr);
    Item.nsuTrans := ObterConteudo(AuxNode.Childrens.Find('nsuTrans'), tcStr);
    Item.nsuHost := ObterConteudo(AuxNode.Childrens.Find('nsuHost'), tcStr);
    Item.nParcelas := ObterConteudo(AuxNode.Childrens.Find('nParcelas'), tcInt);
    Item.infAdCard := ObterConteudo(AuxNode.Childrens.Find('infAdCard'), tcStr);
  end;
end;

procedure TBPeXmlReader.Ler_detBPeTM(const ANode: TACBrXmlNode);
var
  Item: TdetBPeTMCollectionItem;
  i: Integer;
  j: Integer;
  ANodeNivel3: TACBrXmlNode;
  ANodeNivel4: TACBrXmlNode;
  ANodeNivel5: TACBrXmlNode;
  ANodeNivel6: TACBrXmlNode;
  ADetNodes: TACBrXmlNodeArray;
  CompNodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  Item := BPe.detBPeTM.New;
  Item.idEqpCont   := StrToIntDef(ObterConteudoTag(ANode.Attributes.Items['idEqpCont']), 0);
  Item.UFIniViagem := ObterConteudo(ANode.Childrens.Find('UFIniViagem'), tcStr);
  Item.UFFimViagem := ObterConteudo(ANode.Childrens.Find('UFFimViagem'), tcStr);
  Item.placa := ObterConteudo(ANode.Childrens.Find('placa'), tcStr);
  Item.prefixo := ObterConteudo(ANode.Childrens.Find('prefixo'), tcStr);

  Item.Det.Clear;
  ADetNodes := ANode.Childrens.FindAll('det');

  for i := 0 to Length(ADetNodes) - 1 do
  begin
    with Item.Det.New do
    begin
      nViagem := StrToInt(ObterConteudoTag(ADetNodes[i].Attributes.Items['nViagem']));
      cMunIni := ObterConteudo(ADetNodes[i].Childrens.Find('cMunIni'), tcInt);
      cMunFim := ObterConteudo(ADetNodes[i].Childrens.Find('cMunFim'), tcInt);
      nContInicio := ObterConteudo(ADetNodes[i].Childrens.Find('nContInicio'), tcStr);
      nContFim := ObterConteudo(ADetNodes[i].Childrens.Find('nContFim'), tcStr);
      qPass := ObterConteudo(ADetNodes[i].Childrens.Find('qPass'), tcStr);
      vBP := ObterConteudo(ADetNodes[i].Childrens.Find('vBP'), tcDe2);

      ANodeNivel3 := ADetNodes[i].Childrens.Find('imp');

      if NodeNaoEncontrado(ANodeNivel3) then
      begin
        Imp.infAdFisco := ObterConteudo(ANodeNivel3.Childrens.Find('infAdFisco'), tcStr);

        ANodeNivel4 := ADetNodes[i].Childrens.Find('imp');

        if NodeNaoEncontrado(ANodeNivel4) then
        begin
          Imp.infAdFisco := ObterConteudo(ANodeNivel4.Childrens.Find('infAdFisco'), tcStr);

          ANodeNivel5 := ANodeNivel4.Childrens.Find('ICMS');

          if ANodeNivel5 <> nil then
          begin
            Imp.ICMS.CST := StrToCSTICMS(ObterConteudo(ANodeNivel5.Childrens.Find('CST'), tcStr));
            Imp.ICMS.vBC := ObterConteudo(ANodeNivel5.Childrens.Find('vBC'), tcDe2);

            Imp.ICMS.pICMS := ObterConteudo(ANodeNivel5.Childrens.Find('pICMS'), tcDe2);
            Imp.ICMS.vICMS := ObterConteudo(ANodeNivel5.Childrens.Find('vICMS'), tcDe2);
            Imp.ICMS.pRedBC := ObterConteudo(ANodeNivel5.Childrens.Find('pRedBC'), tcDe2);
            Imp.ICMS.vCred := ObterConteudo(ANodeNivel5.Childrens.Find('vCred'), tcDe2);
            Imp.ICMS.pRedBCOutraUF := ObterConteudo(ANodeNivel5.Childrens.Find('pRedBCOutraUF'), tcDe2);
            Imp.ICMS.vBCOutraUF := ObterConteudo(ANodeNivel5.Childrens.Find('vBCOutraUF'), tcDe2);
            Imp.ICMS.pICMSOutraUF := ObterConteudo(ANodeNivel5.Childrens.Find('pICMSOutraUF'), tcDe2);
            Imp.ICMS.vICMSOutraUF := ObterConteudo(ANodeNivel5.Childrens.Find('vICMSOutraUF'), tcDe2);

            ANodeNivel6 := ANodeNivel5.Childrens.Find('ICMSSN');

            if ANodeNivel6 <> nil then
              BPe.Imp.ICMS.CST := cstSN;
          end;
        end;
      end;

      Comp.Clear;
      CompNodes := ADetNodes[i].Childrens.FindAll('Comp');

      for j := 0 to Length(CompNodes) - 1 do
      begin
        with Comp.New do
        begin
          xNome := ObterConteudo(CompNodes[j].Childrens.Find('xNome'), tcStr);
          qComp := ObterConteudo(CompNodes[j].Childrens.Find('qComp'), tcInt);
        end;
      end;
    end;
  end;
end;

procedure TBPeXmlReader.Ler_Total(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) then Exit;

  BPe.Total.qPass := ObterConteudo(ANode.Childrens.Find('qPass'), tcInt);
  BPe.Total.vBP := ObterConteudo(ANode.Childrens.Find('vBP'), tcDe2);

  AuxNode := ANode.Childrens.Find('ICMSTot');

  if AuxNode <> nil then
  begin
    BPe.Total.vBC := ObterConteudo(AuxNode.Childrens.Find('vBC'), tcDe2);
    BPe.Total.vICMS := ObterConteudo(AuxNode.Childrens.Find('vICMS'), tcDe2);
  end;
end;

procedure TBPeXmlReader.Ler_autXML(const ANode: TACBrXmlNode);
var
  Item: TautXMLCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := BPe.autXML.New;

  Item.CNPJCPF := ObterCNPJCPF(ANode);
end;

procedure TBPeXmlReader.Ler_InfAdic(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.InfAdic.infAdFisco := ObterConteudo(ANode.Childrens.Find('infAdFisco'), tcStr);
  BPe.InfAdic.infCpl := ObterConteudo(ANode.Childrens.Find('infCpl'), tcStr);
end;

procedure TBPeXmlReader.Ler_infRespTec(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  BPe.infRespTec.CNPJ     := ObterConteudo(ANode.Childrens.Find('CNPJ'), tcStr);
  BPe.infRespTec.xContato := ObterConteudo(ANode.Childrens.Find('xContato'), tcStr);
  BPe.infRespTec.email    := ObterConteudo(ANode.Childrens.Find('email'), tcStr);
  BPe.infRespTec.fone     := ObterConteudo(ANode.Childrens.Find('fone'), tcStr);
  BPe.infRespTec.idCSRT   := ObterConteudo(ANode.Childrens.Find('idCSRT'), tcInt);
  BPe.infRespTec.hashCSRT := ObterConteudo(ANode.Childrens.Find('hashCSRT'), tcStr);
end;

procedure TBPeXmlReader.Ler_InfBPeSupl(const ANode: TACBrXmlNode);
var
 sQrCode: string;
begin
  if not Assigned(ANode) then Exit;

  sQrCode := ObterConteudo(ANode.Childrens.Find('qrCodBPe'), tcStr);
  sQrCode := StringReplace(sQrCode, '<![CDATA[', '', []);
  sQrCode := StringReplace(sQrCode, ']]>', '', []);

  BPe.infBPeSupl.qrCodBPe := sQrCode;
end;

procedure TBPeXmlReader.Ler_Signature(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  LerSignature(ANode, BPe.signature);
end;

end.

