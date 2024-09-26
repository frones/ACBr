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

unit ACBrDCe.RetEnvEvento;

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
  ACBrDCe.EventoClass,
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

  TRetEventoDCe = class(TObject)
  private
    FidLote: Int64;
    Fversao: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FcOrgao: Integer;
    FxMotivo: string;
    FretInfEvento: TRetInfEventoCollection;
    FInfEvento: TInfEvento;
    Fsignature: Tsignature;
    FXML: AnsiString;
    FXmlRetorno: string;

  protected
    procedure Ler_InfEventos(const ANode: TACBrXmlNode);
//    procedure Ler_RetEvento(const ANode: TACBrXmlNode);
    procedure Ler_InfEvento(const ANode: TACBrXmlNode);
    procedure Ler_DetEvento(const ANode: TACBrXmlNode);

  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: Boolean;

    property idLote: Int64                      read FidLote    write FidLote;
    property versao: string                     read Fversao    write Fversao;
    property tpAmb: TACBrTipoAmbiente           read FtpAmb     write FtpAmb;
    property verAplic: string                   read FverAplic  write FverAplic;
    property cOrgao: Integer                    read FcOrgao    write FcOrgao;
    property cStat: Integer                     read FcStat     write FcStat;
    property xMotivo: string                    read FxMotivo   write FxMotivo;
    property InfEvento: TInfEvento              read FInfEvento write FInfEvento;
    property signature: Tsignature              read Fsignature write Fsignature;
    property retInfEvento: TRetInfEventoCollection read FretInfEvento write FretInfEvento;
    property XML: AnsiString                    read FXML       write FXML;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;


implementation

uses
  ACBrDCe.Conversao,
  ACBrDCe,
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

{ TRetEventoDCe }

constructor TRetEventoDCe.Create;
begin
  inherited Create;

  FretInfEvento := TRetInfEventoCollection.Create;
  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
end;

destructor TRetEventoDCe.Destroy;
begin
  FretInfEvento.Free;
  FInfEvento.Free;
  Fsignature.Free;

  inherited;
end;

procedure TRetEventoDCe.Ler_DetEvento(const ANode: TACBrXmlNode);
var
  sAux: string;
  AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  i: Integer;
begin
  if not Assigned(ANode) then Exit;

  InfEvento.DetEvento.Versao := ObterConteudoTag(ANode.Attributes.Items['versaoEvento']);

  case InfEvento.tpEvento of
    teCancelamento: AuxNode := ANode.Childrens.FindAnyNs('evCancDCe');
  else
    AuxNode := nil;
  end;

  if AuxNode <> nil then
  begin
    InfEvento.DetEvento.descEvento := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('descEvento'), tcStr);
    InfEvento.DetEvento.nProt := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nProt'), tcStr);
    InfEvento.DetEvento.xJust := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xJust'), tcStr);
  end;
end;

procedure TRetEventoDCe.Ler_InfEvento(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  infEvento.Id := ObterConteudoTag(ANode.Attributes.Items['Id']);
  infEvento.cOrgao := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgao'), tcInt);
  infEvento.tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  infEvento.CNPJCPF := ObterConteudoTagCNPJCPF(ANode);
  infEvento.chDCe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chDCe'), tcStr);
  infEvento.dhEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEvento'), tcDatHor);
  infEvento.tpEvento := StrToTpEventoDCe(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEvento'), tcStr));
  infEvento.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);

  Ler_DetEvento(ANode.Childrens.FindAnyNs('detEvento'));
end;

procedure TRetEventoDCe.Ler_InfEventos(const ANode: TACBrXmlNode);
var
  Item: TRetInfEventoCollectionItem;
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  Item := FretInfEvento.New;

  Item.RetInfEvento.XML := ANode.OuterXml;

  Item.RetInfEvento.Id := ObterConteudoTag(ANode.Attributes.Items['Id']);
  Item.RetInfEvento.tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  Item.RetInfEvento.verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
  Item.RetInfEvento.cOrgao := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgao'), tcInt);
  Item.RetInfEvento.cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
  Item.RetInfEvento.xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);
  Item.RetInfEvento.chDCe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chDCe'), tcStr);
  Item.RetInfEvento.tpEvento := StrToTpEventoDCe(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEvento'), tcStr));
  Item.RetInfEvento.xEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('xEvento'), tcStr);
  Item.RetInfEvento.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);
  Item.RetInfEvento.dhRegEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRegEvento'), tcDatHor);
  Item.RetInfEvento.nProt := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProt'), tcStr);

  tpAmb   := Item.RetInfEvento.tpAmb;
  cStat   := Item.RetInfEvento.cStat;
  xMotivo := Item.RetInfEvento.xMotivo;
end;

{
procedure TRetEventoDCe.Ler_RetEvento(const ANode: TACBrXmlNode);
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
function TRetEventoDCe.LerXml: Boolean;
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
        if (ANode.LocalName = 'procEventoDCe') then
        begin
          versao := ObterConteudoTag(ANode.Attributes.Items['versao']);

          AuxNode := ANode.Childrens.FindAnyNs('eventoDCe');

          if AuxNode = nil then
            AuxNode := ANode.Childrens.FindAnyNs('evento');

          Ler_InfEvento(AuxNode.Childrens.FindAnyNs('infEvento'));

          AuxNode := ANode.Childrens.FindAnyNs('retEventoDCe');

          if AuxNode <> nil then
          begin
            ANodes := AuxNode.Childrens.FindAllAnyNs('infEvento');
            for i := 0 to Length(ANodes) - 1 do
            begin
              Ler_InfEventos(ANodes[i]);
            end;
          end;
        end;

        if ANode.LocalName = 'retEventoDCe' then
        begin
          ANodes := ANode.Childrens.FindAllAnyNs('infEvento');
          for i := 0 to Length(ANodes) - 1 do
          begin
            Ler_InfEventos(ANodes[i]);
          end;
        end;

        if (ANode.LocalName = 'eventoDCe') or (ANode.LocalName = 'evento') then
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
