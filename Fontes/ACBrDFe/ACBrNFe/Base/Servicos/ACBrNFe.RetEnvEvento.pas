{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrNFe.RetEnvEvento;

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
  ACBrNFe.EventoClass,
  ACBrBase,
  ACBrXmlBase,
  ACBrXmlDocument;

type
  TRetInfEventoCollectionItem = class(TObject)
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

  TRetEventoNFe = class(TObject)
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
    procedure Ler_chNFePend(const ANode: TACBrXmlNode; Idx: Integer);
    procedure Ler_InfEventos(const ANode: TACBrXmlNode);
    procedure Ler_RetEvento(const ANode: TACBrXmlNode);
    procedure Ler_InfEvento(const ANode: TACBrXmlNode);
    procedure Ler_DetEvento(const ANode: TACBrXmlNode);
    procedure Ler_Dest(const ANode: TACBrXmlNode);
    procedure Ler_autXML(const ANode: TACBrXmlNode);
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
  pcnConversaoNFe,
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

{ TRetEventoNFe }

constructor TRetEventoNFe.Create;
begin
  inherited Create;

  FretEvento := TRetInfEventoCollection.Create;
  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
end;

destructor TRetEventoNFe.Destroy;
begin
  FretEvento.Free;
  FInfEvento.Free;
  Fsignature.Free;

  inherited;
end;

procedure TRetEventoNFe.Ler_Dest(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  infEvento.detEvento.dest.UF := ObterConteudoTag(ANode.Childrens.FindAnyNs('UF'), tcStr);
  infEvento.detEvento.dest.CNPJCPF := ObterConteudoTagCNPJCPF(ANode);
  infEvento.detEvento.dest.idEstrangeiro := ObterConteudoTag(ANode.Childrens.FindAnyNs('idEstrangeiro'), tcStr);
  infEvento.detEvento.dest.IE := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);

  // para ficar em conformidade com o Schema
  infEvento.detEvento.vNF := ObterConteudoTag(ANode.Childrens.FindAnyNs('vNF'), tcDe2);
  infEvento.detEvento.vICMS := ObterConteudoTag(ANode.Childrens.FindAnyNs('vICMS'), tcDe2);
  infEvento.detEvento.vST := ObterConteudoTag(ANode.Childrens.FindAnyNs('vST'), tcDe2);
end;

procedure TRetEventoNFe.Ler_autXML(const ANode: TACBrXmlNode);
var
  Item: TautXMLCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := InfEvento.detEvento.autXML.New;

  Item.CNPJCPF := ObterConteudoTagCNPJCPF(ANode);
end;

procedure TRetEventoNFe.Ler_DetEvento(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  infEvento.DetEvento.descEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('descEvento'), tcStr);
  infEvento.DetEvento.xCorrecao := ObterConteudoTag(ANode.Childrens.FindAnyNs('xCorrecao'), tcStr);
  infEvento.DetEvento.xCondUso:= ObterConteudoTag(ANode.Childrens.FindAnyNs('xCondUso'), tcStr);
  infEvento.DetEvento.nProt := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProt'), tcStr);
  infEvento.DetEvento.xJust := ObterConteudoTag(ANode.Childrens.FindAnyNs('xJust'), tcStr);

  InfEvento.detEvento.cOrgaoAutor := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgaoAutor'), tcInt);
  infEvento.detEvento.tpAutor := StrToTipoAutor(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAutor'), tcStr));
  infEvento.detEvento.verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
  infEvento.detEvento.dhEmi := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEmi'), tcDatHor);
  infEvento.detEvento.tpNF := StrToTpNF(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr));
  infEvento.detEvento.IE := ObterConteudoTag(ANode.Childrens.FindAnyNs('IE'), tcStr);

  // Comprovante de Entrega da NF-e e o Cancelamento do Comprovante
  infEvento.detEvento.dhEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEntrega'), tcDatHor);
  infEvento.detEvento.nDoc := ObterConteudoTag(ANode.Childrens.FindAnyNs('nDoc'), tcStr);
  infEvento.detEvento.xNome := ObterConteudoTag(ANode.Childrens.FindAnyNs('xNome'), tcStr);
  infEvento.detEvento.latGPS := ObterConteudoTag(ANode.Childrens.FindAnyNs('latGPS'), tcDe6);
  infEvento.detEvento.longGPS := ObterConteudoTag(ANode.Childrens.FindAnyNs('longGPS'), tcDe6);

  infEvento.detEvento.hashComprovante := ObterConteudoTag(ANode.Childrens.FindAnyNs('hashComprovante'), tcStr);
  infEvento.detEvento.dhHashComprovante := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhHashComprovante'), tcDatHor);
  infEvento.detEvento.nProtEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProtEvento'), tcStr);
  infEvento.detEvento.tpAutorizacao := StrToAutorizacao(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAutorizacao'), tcStr));

  infEvento.detEvento.dhTentativaEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhTentativaEntrega'), tcDatHor);
  infEvento.detEvento.nTentativa := ObterConteudoTag(ANode.Childrens.FindAnyNs('nTentativa'), tcInt);
  infEvento.detEvento.tpMotivo := StrTotpMotivo(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpMotivo'), tcStr));
  infEvento.detEvento.xJustMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xJustMotivo'), tcStr);
  infEvento.detEvento.hashTentativaEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('hashTentativaEntrega'), tcStr);
  infEvento.detEvento.dhHashTentativaEntrega := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhHashTentativaEntrega'), tcDatHor);

  Ler_Dest(ANode.Childrens.FindAnyNs('dest'));
  Ler_autXML(ANode.Childrens.FindAnyNs('autXML'))
end;

procedure TRetEventoNFe.Ler_InfEvento(const ANode: TACBrXmlNode);
var
  ok: Boolean;
begin
  if not Assigned(ANode) then Exit;

  infEvento.Id := ObterConteudoTag(ANode.Attributes.Items['Id']);
  infEvento.cOrgao := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgao'), tcInt);
  infEvento.tpAmb := StrToTpAmb(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  infEvento.CNPJ := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);

  if infEvento.CNPJ = '' then
    infEvento.CNPJ := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPF'), tcStr);

  infEvento.chNFe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFe'), tcStr);
  infEvento.dhEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhEvento'), tcDatHor);
  infEvento.tpEvento := StrToTpEventoNFe(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEvento'), tcStr));
  infEvento.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);
  infEvento.VersaoEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('verEvento'), tcStr);

  Ler_DetEvento(ANode.Childrens.FindAnyNs('detEvento'));
end;

procedure TRetEventoNFe.Ler_chNFePend(const ANode: TACBrXmlNode; Idx: Integer);
var
  Item: TRetchNFePendCollectionItem;
begin
  if not Assigned(ANode) then Exit;

  Item := FretEvento.Items[Idx].FRetInfEvento.chNFePend.New;

  Item.ChavePend := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFePend'), tcStr);
end;

procedure TRetEventoNFe.Ler_InfEventos(const ANode: TACBrXmlNode);
var
  Item: TRetInfEventoCollectionItem;
  ok: Boolean;
  i: Integer;
  ANodes: TACBrXmlNodeArray;
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
  Item.RetInfEvento.chNFe := ObterConteudoTag(ANode.Childrens.FindAnyNs('chNFe'), tcStr);
  Item.RetInfEvento.tpEvento := StrToTpEventoNFe(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpEvento'), tcStr));
  Item.RetInfEvento.xEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('xEvento'), tcStr);
  Item.RetInfEvento.nSeqEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('nSeqEvento'), tcInt);
  Item.RetInfEvento.CNPJDest := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJDest'), tcStr);

  if Item.RetInfEvento.CNPJDest = '' then
    Item.RetInfEvento.CNPJDest := ObterConteudoTag(ANode.Childrens.FindAnyNs('CPFDest'), tcStr);

  Item.RetInfEvento.emailDest := ObterConteudoTag(ANode.Childrens.FindAnyNs('emailDest'), tcStr);
  Item.RetInfEvento.cOrgaoAutor := ObterConteudoTag(ANode.Childrens.FindAnyNs('cOrgaoAutor'), tcInt);
  Item.RetInfEvento.dhRegEvento := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhRegEvento'), tcDatHor);
  Item.RetInfEvento.nProt := ObterConteudoTag(ANode.Childrens.FindAnyNs('nProt'), tcStr);

  ANodes := ANode.Childrens.FindAll('chNFePend');
  for i := 0 to Length(ANodes) - 1 do
  begin
    Ler_chNFePend(ANodes[i], i);
  end;
end;

procedure TRetEventoNFe.Ler_RetEvento(const ANode: TACBrXmlNode);
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

function TRetEventoNFe.LerXml: Boolean;
var
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if XmlRetorno = '' then Exit;

      Document.LoadFromXml(XmlRetorno);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        if (ANode.LocalName = 'procEventoNFe') or (ANode.LocalName = 'envEvento') then
        begin
          versao := ObterConteudoTag(ANode.Attributes.Items['versao']);

          Ler_InfEvento(ANode.Childrens.FindAnyNs('evento').Childrens.FindAnyNs('infEvento'));
        end;

        if ANode.LocalName = 'retEnvEvento' then
          Ler_RetEvento(ANode);

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
