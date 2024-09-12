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

unit ACBrMDFe.RetEnvEvento;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  pcnConversao,
  pcnSignature,
  ACBrMDFe.EventoClass,
  ACBrBase,
  ACBrXmlBase,
  ACBrXmlDocument;

type
  TRetInfeventoCollectionItem = class(TObject)
  private
    FRetInfEvento: TRetInfEvento;
  public
    constructor Create;
    destructor Destroy; override;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  TRetInfEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfEventoCollectionItem);
  public
    function Add: TRetInfEventoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetInfEventoCollectionItem;
    property Items[Index: Integer]: TRetInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  TRetEventoMDFe = class(TObject)
  private
    FidLote: Int64;
    Fversao: string;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FcOrgao: Integer;
    FxMotivo: string;
    FretEvento: TRetInfEventoCollection;
    FInfEvento: TInfEvento;
    Fsignature: Tsignature;
    FXML: AnsiString;
    FXmlRetorno: string;

  protected
    procedure Ler_InfEventos(const ANode: TACBrXmlNode);
//    procedure Ler_RetEvento(const ANode: TACBrXmlNode);
    procedure Ler_InfEvento(const ANode: TACBrXmlNode);
    procedure Ler_DetEvento(const ANode: TACBrXmlNode);
    procedure Ler_InfViagens(const ANode: TACBrXmlNode);
    procedure Ler_InfDoc(const ANode: TACBrXmlNode);
    procedure Ler_InfPag(const ANode: TACBrXmlNode);
    procedure Ler_Comp(const ANode: TACBrXmlNode; Idx: Integer);
    procedure Ler_InfPrazo(const ANode: TACBrXmlNode; Idx: Integer);
    procedure Ler_InfBanc(const ANode: TACBrXmlNode; Idx: Integer);

  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    property idLote: Int64                      read FidLote    write FidLote;
    property versao: string                     read Fversao    write Fversao;
    property tpAmb: TpcnTipoAmbiente            read FtpAmb     write FtpAmb;
    property verAplic: string                   read FverAplic  write FverAplic;
    property cOrgao: Integer                    read FcOrgao    write FcOrgao;
    property cStat: Integer                     read FcStat     write FcStat;
    property xMotivo: string                    read FxMotivo   write FxMotivo;
    property InfEvento: TInfEvento              read FInfEvento write FInfEvento;
    property signature: Tsignature              read Fsignature write Fsignature;
    property retEvento: TRetInfEventoCollection read FretEvento write FretEvento;
    property XML: AnsiString                    read FXML       write FXML;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;


implementation

uses
  pmdfeConversaoMDFe,
  pmdfeMDFe,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML;

{ TRetInfEventoCollection }

function TRetInfEventoCollection.Add: TRetInfEventoCollectionItem;
begin
  Result := Self.New;
end;

function TRetInfEventoCollection.GetItem(Index: Integer): TRetInfEventoCollectionItem;
begin
  Result := TRetInfEventoCollectionItem(inherited Items[Index]);
end;

procedure TRetInfEventoCollection.SetItem(Index: Integer;
  Value: TRetInfEventoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRetInfEventoCollection.New: TRetInfEventoCollectionItem;
begin
  Result := TRetInfEventoCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetInfEventoCollectionItem }

constructor TRetInfEventoCollectionItem.Create;
begin
  inherited Create;
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TRetInfEventoCollectionItem.Destroy;
begin
  FRetInfEvento.Free;
  inherited;
end;

{ TRetEventoMDFe }

constructor TRetEventoMDFe.Create;
begin
  inherited Create;

  FretEvento := TRetInfEventoCollection.Create;
  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
end;

destructor TRetEventoMDFe.Destroy;
begin
  FretEvento.Free;
  FInfEvento.Free;
  Fsignature.Free;

  inherited;
end;

procedure TRetEventoMDFe.Ler_DetEvento(const ANode: TACBrXmlNode);
var
  sAux: string;
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  InfEvento.VersaoEvento := ObterConteudoTag(ANode.Attributes.Items['versaoEvento']);

  case InfEvento.tpEvento of
    teCancelamento: AuxNode := ANode.Childrens.FindAnyNs('evCancMDFe');

    teEncerramento: AuxNode := ANode.Childrens.FindAnyNs('evEncMDFe');

    teInclusaoCondutor: AuxNode := ANode.Childrens.FindAnyNs('evIncCondutorMDFe');

    teInclusaoDFe: AuxNode := ANode.Childrens.FindAnyNs('evIncDFeMDFe');

    tePagamentoOperacao: AuxNode := ANode.Childrens.FindAnyNs('evPagtoOperMDFe');

    teAlteracaoPagtoServMDFe: AuxNode := ANode.Childrens.FindAnyNs('evAlteracaoPagtoServMDFe');

    teConfirmaServMDFe: AuxNode := ANode.Childrens.FindAnyNs('evConfirmaServMDFe');
  else
    AuxNode := nil;
  end;

  if AuxNode <> nil then
  begin
    InfEvento.DetEvento.descEvento := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('descEvento'), tcStr);
    InfEvento.DetEvento.nProt := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nProt'), tcStr);
    InfEvento.DetEvento.dtEnc := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dtEnc'), tcDat);

    InfEvento.detEvento.cUF := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cUF'), tcInt);
    InfEvento.detEvento.cMun := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cMun'), tcInt);
    InfEvento.DetEvento.xJust := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xJust'), tcStr);
    InfEvento.detEvento.xNome := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xNome'), tcStr);
    InfEvento.detEvento.CPF := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CPF'), tcStr);

    sAux := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('indEncPorTerceiro'), tcStr);

    if sAux = '1' then
      InfEvento.detEvento.indEncPorTerceiro := tiSim;

    InfEvento.detEvento.cMunCarrega := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cMunCarrega'), tcInt);
    InfEvento.DetEvento.xMunCarrega := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xMunCarrega'), tcStr);

    Ler_InfViagens(AuxNode.Childrens.FindAnyNs('infViagens'));

    ANodes := AuxNode.Childrens.FindAll('infDoc');
    for i := 0 to Length(ANodes) - 1 do
    begin
      Ler_InfDoc(ANodes[i]);
    end;

    ANodes := AuxNode.Childrens.FindAll('infPag');
    for i := 0 to Length(ANodes) - 1 do
    begin
      Ler_InfPag(ANodes[i]);
    end;
  end;
end;

procedure TRetEventoMDFe.Ler_InfViagens(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  InfEvento.detEvento.infViagens.qtdViagens := ObterConteudoTag(ANode.Childrens.FindAnyNs('qtdViagens'), tcInt);
  InfEvento.detEvento.infViagens.nroViagem := ObterConteudoTag(ANode.Childrens.FindAnyNs('nroViagem'), tcInt);
end;

procedure TRetEventoMDFe.Ler_InfDoc(const ANode: TACBrXmlNode);
var
  Item: TInfDocCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infDoc.New;

  Item.cMunDescarga := ObterConteudoTag(ANode.Childrens.FindAnyNs('cMunDescarga'), tcInt);
  Item.xMunDescarga := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMunDescarga'), tcStr);
  Item.chNFe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFe'), tcStr);
end;

procedure TRetEventoMDFe.Ler_InfPag(const ANode: TACBrXmlNode);
var
  Item: TinfPagCollectionItem;
  ok: Boolean;
  sAux: string;
  ANodes: TACBrXmlNodeArray;
  i, j: Integer;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infPag.New;

  j := InfEvento.detEvento.infPag.Count -1;

  Item.xNome := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  Item.idEstrangeiro := ObterConteudoTag(ANode.Childrens.FindAnyNs('idEstrangeiro'), tcStr);
  Item.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);

  if Item.CNPJCPF = '' then
    Item.CNPJCPF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);

  Item.vContrato := ObterConteudoTag(ANode.Childrens.FindAnyNs('vContrato'), tcDe2);
  Item.indPag := StrToTIndPag(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('indPag'), tcStr));
  Item.vAdiant := ObterConteudoTag(ANode.Childrens.FindAnyNs('vAdiant'), tcDe2);

  sAux := ObterConteudoTag(ANode.Childrens.FindAnyNs('indAntecipaAdiant'), tcStr);

  if sAux <> '' then
    Item.indAntecipaAdiant := StrToTIndicador(ok, sAux);

  Item.tpAntecip := StrTotpAntecip(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAntecip'), tcStr));

  ANodes := ANode.Childrens.FindAll('Comp');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_Comp(ANodes[i], j);
  end;

  if Item.indPag = ipPrazo then
  begin
    ANodes := ANode.Childrens.FindAll('infPrazo');
    for i := 0 to Length(ANodes) - 1 do
    begin
      Ler_InfPrazo(ANodes[i], j);
    end;
  end;

  Ler_InfBanc(ANode.Childrens.FindAnyNs('infBanc'), j);
end;

procedure TRetEventoMDFe.Ler_Comp(const ANode: TACBrXmlNode; Idx: Integer);
var
  Item: TCompCollectionItem;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infPag[Idx].Comp.New;

  Item.tpComp := StrToTComp(Ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpComp'), tcStr));
  Item.vComp := ObterConteudoTag(ANode.Childrens.FindAnyNs('vComp'), tcDe2);
  Item.xComp := ObterConteudoTag(ANode.Childrens.FindAnyNs('xComp'), tcStr);
end;

procedure TRetEventoMDFe.Ler_InfPrazo(const ANode: TACBrXmlNode; Idx: Integer);
var
  Item: TInfPrazoCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infPag[Idx].infPrazo.New;

  Item.nParcela := ObterConteudoTag(ANode.Childrens.FindAnyNs('nParcela'), tcInt);
  Item.dVenc := ObterConteudoTag(ANode.Childrens.FindAnyNs('dVenc'), tcDat);
  Item.vParcela := ObterConteudoTag(ANode.Childrens.FindAnyNs('vParcela'), tcDe2);
end;

procedure TRetEventoMDFe.Ler_InfBanc(const ANode: TACBrXmlNode; Idx: Integer);
var
  Item: TinfBanc;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infPag[Idx].infBanc;

  Item.PIX := ObterConteudoTag(ANode.Childrens.FindAnyNs('PIX'), tcStr);
  Item.CNPJIPEF := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJIPEF'), tcStr);
  Item.codBanco := ObterConteudoTag(ANode.Childrens.FindAnyNs('codBanco'), tcStr);
  Item.codAgencia := ObterConteudoTag(ANode.Childrens.FindAnyNs('codAgencia'), tcStr);
end;

procedure TRetEventoMDFe.Ler_InfEvento(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  infEvento.Id := ObterConteudoTag(ANode.Attributes.Items['Id']);
  infEvento.cOrgao := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgao'), tcInt);
  infEvento.tpAmb := StrToTpAmb(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  infEvento.CNPJCPF := ObterConteudoTagCNPJCPF(ANode);
  infEvento.chMDFe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chMDFe'), tcStr);
  infEvento.dhEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEvento'), tcDatHor);
  infEvento.tpEvento := StrToTpEventoMDFe(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEvento'), tcStr));
  infEvento.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);
  infEvento.VersaoEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('verEvento'), tcStr);

  Ler_DetEvento(ANode.Childrens.FindAnyNs('detEvento'));
end;

procedure TRetEventoMDFe.Ler_InfEventos(const ANode: TACBrXmlNode);
var
  Item: TRetInfEventoCollectionItem;
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Item := retEvento.New;

  Item.RetInfEvento.XML := ANode.OuterXml;

  Item.RetInfEvento.Id := ObterConteudoTag(ANode.Attributes.Items['Id']);
  Item.RetInfEvento.tpAmb := StrToTpAmb(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  Item.RetInfEvento.verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
  Item.RetInfEvento.cOrgao := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgao'), tcInt);
  Item.RetInfEvento.cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
  Item.RetInfEvento.xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);
  Item.RetInfEvento.chMDFe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chMDFe'), tcStr);
  Item.RetInfEvento.tpEvento := StrToTpEventoMDFe(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEvento'), tcStr));
  Item.RetInfEvento.xEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('xEvento'), tcStr);
  Item.RetInfEvento.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);
  Item.RetInfEvento.dhRegEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRegEvento'), tcDatHor);
  Item.RetInfEvento.nProt := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProt'), tcStr);

  tpAmb   := Item.FRetInfEvento.tpAmb;
  cStat   := Item.FRetInfEvento.cStat;
  xMotivo := Item.FRetInfEvento.xMotivo;
end;

{
procedure TRetEventoMDFe.Ler_RetEvento(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
begin
  if not Assigned(ANode) then Exit;

  versao := ObterConteudoTag(ANode.Attributes.Items['versao']);
  idLote := ObterConteudoTag(ANode.Childrens.FindAnyNs('idLote'), tcInt64);
  tpAmb := StrToTpAmb(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
  cOrgao := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgao'), tcInt);
  cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
  xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);

  ANodes := ANode.Childrens.FindAll('retEvento');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_InfEventos(ANodes[i].Childrens.FindAnyNs('infEvento'));
  end;
end;
}
function TRetEventoMDFe.LerXml: Boolean;
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if XmlRetorno = '' then Exit;

      Document.LoadFromXml(XmlRetorno);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        if (ANode.LocalName = 'procEventoMDFe') then
        begin
          versao := ObterConteudoTag(ANode.Attributes.Items['versao']);

          AuxNode := ANode.Childrens.FindAnyNs('eventoMDFe');

          if AuxNode = nil then
            AuxNode := ANode.Childrens.FindAnyNs('evento');

          Ler_InfEvento(AuxNode.Childrens.FindAnyNs('infEvento'));

          AuxNode := ANode.Childrens.FindAnyNs('retEventoMDFe');

          if AuxNode <> nil then
          begin
            ANodes := AuxNode.Childrens.FindAllAnyNs('infEvento');
            for i := 0 to Length(ANodes) - 1 do
            begin
              Ler_InfEventos(ANodes[i]);
            end;
          end;
        end;

        if ANode.LocalName = 'retEventoMDFe' then
        begin
          ANodes := ANode.Childrens.FindAllAnyNs('infEvento');
          for i := 0 to Length(ANodes) - 1 do
          begin
            Ler_InfEventos(ANodes[i]);
          end;
        end;

        if (ANode.LocalName = 'eventoMDFe') or (ANode.LocalName = 'evento') then
          Ler_InfEvento(ANode.Childrens.FindAnyNs('infEvento'));

        LerSignature(ANode.Childrens.FindAnyNs('Signature'), signature);
      end;

      Result := True;
    except
      Result := False;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

end.
