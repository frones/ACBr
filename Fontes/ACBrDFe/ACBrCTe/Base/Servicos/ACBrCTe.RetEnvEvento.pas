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

unit ACBrCTe.RetEnvEvento;

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
  ACBrCTe.EventoClass,
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

  TRetEventoCTe = class(TObject)
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
    procedure Ler_InfCorrecao(const ANode: TACBrXmlNode);
    procedure Ler_InfGTV(const ANode: TACBrXmlNode);
    procedure Ler_InfEspecie(const ANode: TACBrXmlNode; Idx: Integer);
    procedure Ler_Remetente(const ANode: TACBrXmlNode; Idx: Integer);
    procedure Ler_Destinatario(const ANode: TACBrXmlNode; Idx: Integer);
    procedure Ler_InfEntrega(const ANode: TACBrXmlNode);
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
  pcteConversaoCTe,
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

{ TRetEventoCTe }

constructor TRetEventoCTe.Create;
begin
  inherited Create;

  FretEvento := TRetInfEventoCollection.Create;
  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
end;

destructor TRetEventoCTe.Destroy;
begin
  FretEvento.Free;
  FInfEvento.Free;
  Fsignature.Free;

  inherited;
end;

procedure TRetEventoCTe.Ler_InfCorrecao(const ANode: TACBrXmlNode);
var
  Item: TinfCorrecaoCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infCorrecao.New;

  Item.grupoAlterado := ObterConteudoTag(ANode.Childrens.FindAnyNs('grupoAlterado'), tcStr);
  Item.campoAlterado := ObterConteudoTag(ANode.Childrens.FindAnyNs('campoAlterado'), tcStr);
  Item.valorAlterado := ObterConteudoTag(ANode.Childrens.FindAnyNs('valorAlterado'), tcStr);
  Item.nroItemAlterado := ObterConteudoTag(ANode.Childrens.FindAnyNs('nroItemAlterado'), tcInt);
end;

procedure TRetEventoCTe.Ler_InfGTV(const ANode: TACBrXmlNode);
var
  Item: TInfGTVCollectionItem;
  ANodes: TACBrXmlNodeArray;
  i, j: Integer;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infGTV.New;

  j := InfEvento.detEvento.infGTV.Count -1;

  Item.nDoc := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDoc'), tcStr);
  Item.id := ObterConteudoTag(ANode.Childrens.FindAnyNs('id'), tcStr);
  Item.serie := ObterConteudoTag(ANode.Childrens.FindAnyNs('serie'), tcStr);
  Item.subserie := ObterConteudoTag(ANode.Childrens.FindAnyNs('subserie'), tcStr);
  Item.dEmi := ObterConteudoTag(ANode.Childrens.FindAnyNs('dEmi'), tcDat);
  Item.nDV := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDV'), tcInt);
  Item.qCarga := ObterConteudoTag(ANode.Childrens.FindAnyNs('qCarga'), tcDe4);
  Item.placa := ObterConteudoTag(ANode.Childrens.FindAnyNs('placa'), tcStr);
  Item.UF := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  Item.RNTRC := ObterConteudoTag(ANode.Childrens.FindAnyNs('RNTRC'), tcStr);

  ANodes := ANode.Childrens.FindAll('infEspecie');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_InfEspecie(ANodes[i], j);
  end;

  Ler_Remetente(ANode.Childrens.FindAnyNs('rem'), j);

  Ler_Destinatario(ANode.Childrens.FindAnyNs('dest'), j);
end;

procedure TRetEventoCTe.Ler_InfEspecie(const ANode: TACBrXmlNode; Idx: Integer);
var
  Item: TInfEspecieCollectionItem;
  Ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infGTV[Idx].infEspecie.New;

  Item.tpEspecie := StrToTEspecie(Ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEspecie'), tcStr));
  Item.vEspecie := ObterConteudoTag(ANode.Childrens.FindAnyNs('vEspecie'), tcDe2);
end;

procedure TRetEventoCTe.Ler_Remetente(const ANode: TACBrXmlNode; Idx: Integer);
var
  Item: TInfRemDest;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infGTV[Idx].rem;

  Item.CNPJCPF := ObterConteudoTagCNPJCPF(ANode);
  Item.IE := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  Item.UF := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  Item.xNome := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
end;

procedure TRetEventoCTe.Ler_Destinatario(const ANode: TACBrXmlNode;
  Idx: Integer);
var
  Item: TInfRemDest;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infGTV[Idx].dest;

  Item.CNPJCPF := ObterConteudoTagCNPJCPF(ANode);
  Item.IE := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  Item.UF := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  Item.xNome := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
end;

procedure TRetEventoCTe.Ler_InfEntrega(const ANode: TACBrXmlNode);
var
  Item: TInfEntregaCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.infEntrega.New;

  Item.chNFe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFe'), tcStr);
end;

procedure TRetEventoCTe.Ler_DetEvento(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  infEvento.VersaoEvento := ObterConteudoTag(ANode.Attributes.Items['versaoEvento']);
  infEvento.DetEvento.descEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('descEvento'), tcStr);
  infEvento.DetEvento.nProt := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProt'), tcStr);
  infEvento.DetEvento.xJust := ObterConteudoTag(ANode.Childrens.FindAnyNs('xJust'), tcStr);
  infEvento.DetEvento.vICMS := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);
  infEvento.DetEvento.vICMSST := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMSST'), tcDe2);
  infEvento.DetEvento.vTPrest := ObterConteudoTag(ANode.Childrens.FindAnyNs('vTPrest'), tcDe2);
  infEvento.DetEvento.vCarga := ObterConteudoTag(ANode.Childrens.FindAnyNs('vCarga'), tcDe2);
  infEvento.detEvento.toma := StrToTpTomador(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('toma'), tcStr));
  infEvento.detEvento.UF := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  infEvento.detEvento.CNPJCPF := ObterConteudoTagCNPJCPF(ANode);
  infEvento.detEvento.IE := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);
  infEvento.detEvento.modal := StrToTpModal(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('modal'), tcStr));
  infEvento.DetEvento.UFIni := ObterConteudoTag(ANode.Childrens.FindAnyNs('UFIni'), tcStr);
  infEvento.DetEvento.UFFim := ObterConteudoTag(ANode.Childrens.FindAnyNs('UFFim'), tcStr);
  infEvento.DetEvento.xOBS := ObterConteudoTag(ANode.Childrens.FindAnyNs('xOBS'), tcStr);
  // Comprovante de Entrega da CT-e e o Cancelamento do Comprovante
  infEvento.detEvento.dhEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEntrega'), tcDatHor);
  infEvento.detEvento.nDoc := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDoc'), tcStr);
  infEvento.detEvento.xNome := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  infEvento.detEvento.latitude := ObterConteudoTag(ANode.Childrens.FindAnyNs('latitude'), tcDe6);
  infEvento.detEvento.longitude := ObterConteudoTag(ANode.Childrens.FindAnyNs('longitude'), tcDe6);
  infEvento.detEvento.hashEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('hashEntrega'), tcStr);
  infEvento.detEvento.dhHashEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhHashEntrega'), tcDatHor);
  infEvento.detEvento.nProtCE := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProtCE'), tcStr);

  infEvento.detEvento.dhTentativaEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhTentativaEntrega'), tcDatHor);
  infEvento.detEvento.nTentativa := ObterConteudoTag(ANode.Childrens.FindAnyNs('nTentativa'), tcInt);
  infEvento.detEvento.tpMotivo := StrTotpMotivo(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpMotivo'), tcStr));
  infEvento.detEvento.xJustMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xJustMotivo'), tcStr);
  infEvento.detEvento.hashTentativaEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('hashTentativaEntrega'), tcStr);
  infEvento.detEvento.dhHashTentativaEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhHashTentativaEntrega'), tcDatHor);

  infEvento.detEvento.nProtIE := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProtIE'), tcStr);

  ANodes := ANode.Childrens.FindAll('infCorrecao');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_InfCorrecao(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('infGTV');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_InfGTV(ANodes[i]);
  end;

  ANodes := ANode.Childrens.FindAll('infEntrega');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_InfEntrega(ANodes[i]);
  end;
end;

procedure TRetEventoCTe.Ler_InfEvento(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  infEvento.Id := ObterConteudoTag(ANode.Attributes.Items['Id']);
  infEvento.tpAmb := StrToTpAmb(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  infEvento.cOrgao := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgao'), tcInt);
  infEvento.CNPJ := ObterConteudoTagCNPJCPF(ANode);
  infEvento.chCTe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chCTe'), tcStr);
  infEvento.dhEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEvento'), tcDatHor);
  infEvento.tpEvento := StrToTpEventoCTe(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEvento'), tcStr));
  infEvento.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);

  Ler_DetEvento(ANode.Childrens.FindAnyNs('detEvento'));
end;

procedure TRetEventoCTe.Ler_InfEventos(const ANode: TACBrXmlNode);
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

  // Os campos abaixos seram retornados caso o cStat = 134 ou 135 ou 136
  Item.RetInfEvento.chCTe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chCTe'), tcStr);
  Item.RetInfEvento.tpEvento := StrToTpEventoCTe(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEvento'), tcStr));
  Item.RetInfEvento.xEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('xEvento'), tcStr);
  Item.RetInfEvento.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);
  Item.RetInfEvento.CNPJDest := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJDest'), tcStr);
  Item.RetInfEvento.dhRegEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRegEvento'), tcDatHor);
  Item.RetInfEvento.nProt := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProt'), tcStr);

  tpAmb := Item.RetInfEvento.tpAmb;
  cStat := Item.RetInfEvento.cStat;
  xMotivo := Item.RetInfEvento.xMotivo;
end;
{
procedure TRetEventoCTe.Ler_RetEvento(const ANode: TACBrXmlNode);
var
  ok: Boolean;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
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
function TRetEventoCTe.LerXml: Boolean;
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
        if (ANode.LocalName = 'procEventoCTe') then
        begin
          versao := ObterConteudoTag(ANode.Attributes.Items['versao']);

          AuxNode := ANode.Childrens.FindAnyNs('retEnvEvento');

          if AuxNode = nil then
            AuxNode := ANode.Childrens.FindAnyNs('retEventoCTe');

          if AuxNode = nil then
            AuxNode := ANode.Childrens.FindAnyNs('retEvento');

          ANodes := AuxNode.Childrens.FindAll('infEvento');
          for i := 0 to Length(ANodes) - 1 do
          begin
            Ler_InfEventos(ANodes[i]);
          end;
        end;

        if (ANode.LocalName = 'eventoCTe') or (ANode.LocalName = 'evento') then
          Ler_InfEvento(ANode);

        LerSignature(ANode.Childrens.Find('Signature'), signature);
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
