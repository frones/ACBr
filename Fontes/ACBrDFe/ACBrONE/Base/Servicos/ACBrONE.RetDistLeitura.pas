{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrONE.RetDistLeitura;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  pcnConversao,
  ACBrONE.Conversao,
  ACBrONE.EnvRecepcaoLeitura,
  ACBrXmlBase,
  ACBrXmlDocument;

type
  TinfMDFeCollectionItem = class(TObject)
  private
    FchMDFe: string;

  public
    property chMDFe: string read FchMDFe write FchMDFe;
  end;

  TinfMDFeCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TinfMDFeCollectionItem;
    procedure SetItem(Index: Integer; Value: TinfMDFeCollectionItem);
  public
    function New: TinfMDFeCollectionItem;
    property Items[Index: Integer]: TinfMDFeCollectionItem read GetItem write SetItem; default;
  end;

  TinfCompl = class(TObject)
  private
    FtpLeitura: TtpLeitura;
    FxEQP: string;
    Flatitude: Double;
    Flongitude: Double;
    Fplaca: string;
    FtpSentido: TtpSentido;
    FNSULeitura: string;
  public
    property tpLeitura: TtpLeitura read FtpLeitura  write FtpLeitura;
    property xEQP: string          read FxEQP       write FxEQP;
    property latitude: Double      read Flatitude   write Flatitude;
    property longitude: Double     read Flongitude  write Flongitude;
    property placa: string         read Fplaca      write Fplaca;
    property tpSentido: TtpSentido read FtpSentido  write FtpSentido;
    property NSULeitura: string    read FNSULeitura write FNSULeitura;
  end;

  TLeituraCollectionItem = class(TObject)
  private
    // Atributos
    FNSU: string;
    Fschema: TSchemaDFe;

    FXML: string;

    FRecepcaoLeitura: TRecepcaoLeitura;
    FinfMDFe: TinfMDFeCollection;
    FinfCompl: TinfCompl;
    procedure SetinfMDFe(const Value: TinfMDFeCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property NSU: string        read FNSU    write FNSU;
    property schema: TSchemaDFe read Fschema write Fschema;
    property XML: string        read FXML    write FXML;

    property RecepcaoLeitura: TRecepcaoLeitura read FRecepcaoLeitura write FRecepcaoLeitura;
    property infMDFe: TinfMDFeCollection read FinfMDFe  write SetinfMDFe;
    property infCompl: TinfCompl         read FinfCompl write FinfCompl;
  end;

  TLeituraCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TLeituraCollectionItem;
    procedure SetItem(Index: Integer; Value: TLeituraCollectionItem);
  public
    function New: TLeituraCollectionItem;
    property Items[Index: Integer]: TLeituraCollectionItem read GetItem write SetItem; default;
  end;

  TLeituraResumoCollectionItem = class(TObject)
  private
    // Atributos
    FNSU: string;
    Fschema: TSchemaDFe;

    FXML: string;

    FinfLeitura: TinfLeitura;
    FinfMDFe: TinfMDFeCollection;

    procedure SetinfMDFe(const Value: TinfMDFeCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property NSU: string        read FNSU    write FNSU;
    property schema: TSchemaDFe read Fschema write Fschema;
    property XML: string        read FXML    write FXML;

    property infLeitura: TinfLeitura     read FinfLeitura write FinfLeitura;
    property infMDFe: TinfMDFeCollection read FinfMDFe    write SetinfMDFe;
  end;

  TLeituraResumoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TLeituraResumoCollectionItem;
    procedure SetItem(Index: Integer; Value: TLeituraResumoCollectionItem);
  public
    function New: TLeituraResumoCollectionItem;
    property Items[Index: Integer]: TLeituraResumoCollectionItem read GetItem write SetItem; default;
  end;

  TLeituraCompactaCollectionItem = class(TObject)
  private
    // Atributos
    FNSU: string;
    Fschema: TSchemaDFe;

    FXML: string;

    FleituraComp: string;
    FinfMDFe: TinfMDFeCollection;

    procedure SetinfMDFe(const Value: TinfMDFeCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property NSU: string        read FNSU    write FNSU;
    property schema: TSchemaDFe read Fschema write Fschema;
    property XML: string        read FXML    write FXML;

    property leituraComp: string         read FleituraComp write FleituraComp;
    property infMDFe: TinfMDFeCollection read FinfMDFe     write SetinfMDFe;
  end;

  TLeituraCompactaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TLeituraCompactaCollectionItem;
    procedure SetItem(Index: Integer; Value: TLeituraCompactaCollectionItem);
  public
    function New: TLeituraCompactaCollectionItem;
    property Items[Index: Integer]: TLeituraCompactaCollectionItem read GetItem write SetItem; default;
  end;

  TRetDistLeitura = class
  private
    Fversao: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: Integer;
    FxMotivo: string;
    FdhResp: TDateTime;
    FultNSU: string;
    FultNSUONE: string;

    FXML: string;

    FLeitura: TLeituraCollection;
    FleituraCompacta: TLeituraCompactaCollection;
    FleituraResumo: TLeituraResumoCollection;
    FXmlRetorno: string;

    procedure SetLeitura(const Value: TLeituraCollection);
    procedure SetleituraCompacta(const Value: TLeituraCompactaCollection);
    procedure SetleituraResumo(const Value: TLeituraResumoCollection);

    procedure Ler_retOneDistLeitura(ANode: TACBrXmlNode);
    procedure Ler_leitura(ANodes: TACBrXmlNodeArray);
    procedure Ler_leituraResumo(ANodes: TACBrXmlNodeArray);
    procedure Ler_oneRecepLeitura(ANode: TACBrXmlNode; Idx: Integer);
    procedure Ler_infCompl(ANode: TACBrXmlNode; Idx: Integer);
    procedure Ler_infLeitura(ANode: TACBrXmlNode; Idx: Integer);
    procedure Ler_leituraCompacta(ANodes: TACBrXmlNodeArray);
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: boolean;
    function LerXMLFromFile(Const CaminhoArquivo: string): Boolean;

    property XML: string              read FXML       write FXML;
    property versao: string           read Fversao    write Fversao;
    property tpAmb: TACBrTipoAmbiente read FtpAmb     write FtpAmb;
    property verAplic: string         read FverAplic  write FverAplic;
    property cStat: Integer           read FcStat     write FcStat;
    property xMotivo: string          read FxMotivo   write FxMotivo;
    property dhResp: TDateTime        read FdhResp    write FdhResp;
    property ultNSU: string           read FultNSU    write FultNSU;
    property ultNSUONE: string        read FultNSUONE write FultNSUONE;

    property Leitura: TLeituraCollection read FLeitura   write SetLeitura;

    property leituraResumo: TLeituraResumoCollection read FleituraResumo   write SetleituraResumo;

    property leituraCompacta: TLeituraCompactaCollection read FleituraCompacta   write SetleituraCompacta;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  synacode,
  ACBrUtil.Base, ACBrUtil.FilesIO, ACBrUtil.Strings;

{ TLeituraCollection }

function TLeituraCollection.GetItem(Index: Integer): TLeituraCollectionItem;
begin
  Result := TLeituraCollectionItem(inherited Items[Index]);
end;

procedure TLeituraCollection.SetItem(Index: Integer;
  Value: TLeituraCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TLeituraCollection.New: TLeituraCollectionItem;
begin
  Result := TLeituraCollectionItem.Create;
  Add(Result);
end;

{ TLeituraCollectionItem }

constructor TLeituraCollectionItem.Create;
begin
  inherited Create;

  FRecepcaoLeitura := TRecepcaoLeitura.Create;
  FinfMDFe := TinfMDFeCollection.Create();
  FinfCompl := TinfCompl.Create;
end;

destructor TLeituraCollectionItem.Destroy;
begin
  FRecepcaoLeitura.Free;
  FinfMDFe.Free;
  FinfCompl.Free;

  inherited;
end;

procedure TLeituraCollectionItem.SetinfMDFe(const Value: TinfMDFeCollection);
begin
  FinfMDFe := Value;
end;

{ TRetDistLeitura }

constructor TRetDistLeitura.Create;
begin
  inherited Create;

  FLeitura := TLeituraCollection.Create();
  FleituraResumo := TLeituraResumoCollection.Create();
  FleituraCompacta := TLeituraCompactaCollection.Create();
end;

destructor TRetDistLeitura.Destroy;
begin
  FLeitura.Free;
  FleituraResumo.Free;
  FleituraCompacta.Free;

  inherited;
end;

procedure TRetDistLeitura.SetLeitura(const Value: TLeituraCollection);
begin
  FLeitura := Value;
end;

procedure TRetDistLeitura.SetleituraCompacta(
  const Value: TLeituraCompactaCollection);
begin
  FleituraCompacta := Value;
end;

procedure TRetDistLeitura.SetleituraResumo(
  const Value: TLeituraResumoCollection);
begin
  FleituraResumo := Value;
end;

procedure TRetDistLeitura.Ler_oneRecepLeitura(ANode: TACBrXmlNode; Idx: Integer);
var
  AuxNode: TACBrXmlNode;
  ok: Boolean;
  sAux: string;
begin
  if not Assigned(ANode) then Exit;

  Leitura[Idx].RecepcaoLeitura.Versao := ObterConteudoTag(ANode.Attributes.Items['versao']);
  Leitura[Idx].RecepcaoLeitura.tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
  Leitura[Idx].RecepcaoLeitura.verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
  Leitura[Idx].RecepcaoLeitura.tpTransm := StrtotpTransm(ObterConteudoTag(ANode.Childrens.FindAnyNs('tpTransm'), tcStr));
  Leitura[Idx].RecepcaoLeitura.dhTransm := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhTransm'), tcDatHor);

  AuxNode := ANode.Childrens.FindAnyNs('infLeitura');

  if AuxNode <> nil then
  begin
    Leitura[Idx].RecepcaoLeitura.infLeitura.cUF := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cUF'), tcInt);
    Leitura[Idx].RecepcaoLeitura.infLeitura.dhPass := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dhPass'), tcDatHor);
    Leitura[Idx].RecepcaoLeitura.infLeitura.CNPJOper := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CNPJOper'), tcStr);
    Leitura[Idx].RecepcaoLeitura.infLeitura.cEQP := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cEQP'), tcStr);
    Leitura[Idx].RecepcaoLeitura.infLeitura.latitude := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('latitude'), tcDe6);
    Leitura[Idx].RecepcaoLeitura.infLeitura.longitude := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('longitude'), tcDe6);
    Leitura[Idx].RecepcaoLeitura.infLeitura.tpSentido := StrTotpSentido(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpSentido'), tcStr));
    Leitura[Idx].RecepcaoLeitura.infLeitura.placa := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('placa'), tcStr);

    sAux := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('tpVeiculo'), tcStr);

    Leitura[Idx].RecepcaoLeitura.infLeitura.tpVeiculo := tvCarga;
    if sAux <> '' then
      Leitura[Idx].RecepcaoLeitura.infLeitura.tpVeiculo := StrTotpVeiculo(sAux);

    Leitura[Idx].RecepcaoLeitura.infLeitura.velocidade := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('velocidade'), tcInt);
    Leitura[Idx].RecepcaoLeitura.infLeitura.foto := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('foto'), tcStr);
    Leitura[Idx].RecepcaoLeitura.infLeitura.indiceConfianca := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('indiceConfianca'), tcInt);
    Leitura[Idx].RecepcaoLeitura.infLeitura.pesoBrutoTotal := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('pesoBrutoTotal'), tcInt);
    Leitura[Idx].RecepcaoLeitura.infLeitura.nroEixos := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nroEixos'), tcInt);
  end;
end;

procedure TRetDistLeitura.Ler_infCompl(ANode: TACBrXmlNode; Idx: Integer);
var
  Item: TinfCompl;
begin
  if not Assigned(ANode) then Exit;

  Item := Leitura[Idx].infCompl;

  Item.tpLeitura := StrTotpLeitura(ObterConteudoTag(ANode.Childrens.FindAnyNs('tpLeitura'), tcStr));
  Item.xEQP := ObterConteudoTag(ANode.Childrens.FindAnyNs('xEQP'), tcStr);
  Item.latitude := ObterConteudoTag(ANode.Childrens.FindAnyNs('latitude'), tcDe6);
  Item.longitude := ObterConteudoTag(ANode.Childrens.FindAnyNs('longitude'), tcDe6);
  Item.placa := ObterConteudoTag(ANode.Childrens.FindAnyNs('placa'), tcStr);
  Item.tpSentido := StrTotpSentido(ObterConteudoTag(ANode.Childrens.FindAnyNs('tpSentido'), tcStr));
  Item.NSULeitura := ObterConteudoTag(ANode.Childrens.FindAnyNs('NSULeitura'), tcStr);
end;

procedure TRetDistLeitura.Ler_leitura(ANodes: TACBrXmlNodeArray);
var
  i, j: Integer;
  ANodes2: TACBrXmlNodeArray;
begin
  if not Assigned(ANodes) then Exit;

  for i := 0 to Length(ANodes) - 1 do
  begin
    Leitura.New;

    Leitura[i].NSU := ObterConteudoTag(ANodes[i].Attributes.Items['NSU']);
    Leitura[i].schema := StrToSchemaDFe(ObterConteudoTag(ANodes[i].Attributes.Items['schema']));

    Leitura[i].XML := ANodes[i].Content;

    Ler_oneRecepLeitura(ANodes[i].Childrens.FindAnyNs('oneRecepLeitura'), i);

    ANodes2 := ANodes[i].Childrens.FindAllAnyNs('infMDFe');

    for j := 0 to Length(ANodes2) - 1 do
    begin
      Leitura[i].infMDFe.New;
      Leitura[i].infMDFe[j].chMDFe := ObterConteudoTag(ANodes2[j].Childrens.FindAnyNs('chMDFe'), tcStr);
    end;

    Ler_infCompl(ANodes[i].Childrens.FindAnyNs('infCompl'), i);
  end;
end;

procedure TRetDistLeitura.Ler_infLeitura(ANode: TACBrXmlNode; Idx: Integer);
var
  Item: TinfLeitura;
  sAux: string;
begin
  if not Assigned(ANode) then Exit;

  Item := leituraResumo[Idx].infLeitura;

  Item.tpTransm := StrTotpTransm(ObterConteudoTag(ANode.Childrens.FindAnyNs('tpTransm'), tcStr));
  Item.dhTransm := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhTransm'), tcDatHor);
  Item.cUF := ObterConteudoTag(ANode.Childrens.FindAnyNs('cUF'), tcInt);
  Item.dhPass := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhPass'), tcDatHor);
  Item.CNPJOper := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJOper'), tcStr);
  Item.xOper := ObterConteudoTag(ANode.Childrens.FindAnyNs('xOper'), tcStr);
  Item.tpLeitura := StrTotpLeitura(ObterConteudoTag(ANode.Childrens.FindAnyNs('tpLeitura'), tcStr));
  Item.cEQP := ObterConteudoTag(ANode.Childrens.FindAnyNs('cEQP'), tcStr);
  Item.xEQP := ObterConteudoTag(ANode.Childrens.FindAnyNs('xEQP'), tcStr);
  Item.latitude := ObterConteudoTag(ANode.Childrens.FindAnyNs('latitude'), tcDe6);
  Item.longitude := ObterConteudoTag(ANode.Childrens.FindAnyNs('longitude'), tcDe6);
  Item.tpSentido := StrTotpSentido(ObterConteudoTag(ANode.Childrens.FindAnyNs('tpSentido'), tcStr));
  Item.placa := ObterConteudoTag(ANode.Childrens.FindAnyNs('placa'), tcStr);

  sAux := ObterConteudoTag(ANode.Childrens.FindAnyNs('tpVeiculo'), tcStr);

  Item.tpVeiculo := tvCarga;
  if sAux <> '' then
    Item.tpVeiculo := StrTotpVeiculo(sAux);

  Item.velocidade := ObterConteudoTag(ANode.Childrens.FindAnyNs('velocidade'), tcInt);
  Item.pesoBrutoTotal := ObterConteudoTag(ANode.Childrens.FindAnyNs('pesoBrutoTotal'), tcInt);
  Item.nroEixos := ObterConteudoTag(ANode.Childrens.FindAnyNs('nroEixos'), tcInt);
  Item.NSULeitura := ObterConteudoTag(ANode.Childrens.FindAnyNs('NSULeitura'), tcStr);
end;

procedure TRetDistLeitura.Ler_leituraResumo(ANodes: TACBrXmlNodeArray);
var
  i, j: Integer;
  ANodes2: TACBrXmlNodeArray;
begin
  if not Assigned(ANodes) then Exit;

  for i := 0 to Length(ANodes) - 1 do
  begin
    leituraResumo.New;

    leituraResumo[i].NSU := ObterConteudoTag(ANodes[i].Attributes.Items['NSU']);
    leituraResumo[i].schema := StrToSchemaDFe(ObterConteudoTag(ANodes[i].Attributes.Items['schema']));

    leituraResumo[i].XML := ANodes[i].Content;

    Ler_infLeitura(ANodes[i].Childrens.FindAnyNs('infLeitura'), i);

    ANodes2 := ANodes[i].Childrens.FindAllAnyNs('infMDFe');

    for j := 0 to Length(ANodes2) - 1 do
    begin
      leituraResumo[i].infMDFe.New;
      leituraResumo[i].infMDFe[j].chMDFe := ObterConteudoTag(ANodes2[j].Childrens.FindAnyNs('chMDFe'), tcStr);
    end;
  end;
end;

procedure TRetDistLeitura.Ler_leituraCompacta(ANodes: TACBrXmlNodeArray);
var
  i, j: Integer;
  ANodes2: TACBrXmlNodeArray;
  StrAux: AnsiString;
begin
  if not Assigned(ANodes) then Exit;

  for i := 0 to Length(ANodes) - 1 do
  begin
    leituraCompacta.New;

    leituraCompacta[i].NSU := ObterConteudoTag(ANodes[i].Attributes.Items['NSU']);
    leituraCompacta[i].schema := StrToSchemaDFe(ObterConteudoTag(ANodes[i].Attributes.Items['schema']));

    leituraCompacta[i].XML := ANodes[i].Content;

    StrAux := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('leituraComp'), tcStr);
    leituraCompacta[i].leituraComp := UnZip(DecodeBase64(StrAux));

    ANodes2 := ANodes[i].Childrens.FindAllAnyNs('infMDFe');

    for j := 0 to Length(ANodes2) - 1 do
    begin
      leituraCompacta[i].infMDFe.New;
      leituraCompacta[i].infMDFe[j].chMDFe := ObterConteudoTag(ANodes2[j].Childrens.FindAnyNs('chMDFe'), tcStr);
    end;
  end;
end;

procedure TRetDistLeitura.Ler_retOneDistLeitura(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;

  Ler_leitura(ANode.Childrens.FindAllAnyNs('leitura'));
  Ler_leituraResumo(ANode.Childrens.FindAllAnyNs('leituraResumo'));
  Ler_leituraCompacta(ANode.Childrens.FindAllAnyNs('leituraCompacta'));
end;

function TRetDistLeitura.LerXml: boolean;
var
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  ok: Boolean;
begin
  Document := TACBrXmlDocument.Create;

  try
    Document.LoadFromXml(XmlRetorno);

    ANode := Document.Root;

    if ANode <> nil then
    begin
      XML := ANode.OuterXml;

      versao := ObterConteudoTag(ANode.Attributes.Items['versao']);
      tpAmb := StrToTipoAmbiente(ok, ObterConteudoTag(ANode.Childrens.FindAnyNs('tpAmb'), tcStr));
      verAplic := ObterConteudoTag(ANode.Childrens.FindAnyNs('verAplic'), tcStr);
      cStat := ObterConteudoTag(ANode.Childrens.FindAnyNs('cStat'), tcInt);
      xMotivo := ObterConteudoTag(ANode.Childrens.FindAnyNs('xMotivo'), tcStr);
      dhResp := ObterConteudoTag(ANode.Childrens.FindAnyNs('dhResp'), tcDatHor);
      ultNSU := ObterConteudoTag(ANode.Childrens.FindAnyNs('ultNSU'), tcStr);
      ultNSUONE := ObterConteudoTag(ANode.Childrens.FindAnyNs('ultNSUONE'), tcStr);

      Ler_retOneDistLeitura(ANode.Childrens.FindAnyNs('retOneDistLeitura'));
    end;
  finally
    Result := True;
    FreeAndNil(Document);
  end;
end;

function TRetDistLeitura.LerXMLFromFile(Const CaminhoArquivo: string): Boolean;
var
  ArqDist: TStringList;
begin
  ArqDist := TStringList.Create;

  try
    ArqDist.LoadFromFile(CaminhoArquivo);

    Self.XmlRetorno := ArqDist.Text;

    Result := LerXml;
  finally
    ArqDist.Free;
  end;
end;

{ TLeituraCompactaCollectionItem }

constructor TLeituraCompactaCollectionItem.Create;
begin
  inherited Create;

  FinfMDFe := TinfMDFeCollection.Create();
end;

destructor TLeituraCompactaCollectionItem.Destroy;
begin
  FinfMDFe.Free;

  inherited;
end;

procedure TLeituraCompactaCollectionItem.SetinfMDFe(
  const Value: TinfMDFeCollection);
begin
  FinfMDFe := Value;
end;

{ TLeituraCompactaCollection }

function TLeituraCompactaCollection.GetItem(
  Index: Integer): TLeituraCompactaCollectionItem;
begin
  Result := TLeituraCompactaCollectionItem(inherited Items[Index]);
end;

function TLeituraCompactaCollection.New: TLeituraCompactaCollectionItem;
begin
  Result := TLeituraCompactaCollectionItem.Create;
  Add(Result);
end;

procedure TLeituraCompactaCollection.SetItem(Index: Integer;
  Value: TLeituraCompactaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TinfMDFeCollection }

function TinfMDFeCollection.GetItem(Index: Integer): TinfMDFeCollectionItem;
begin
  Result := TinfMDFeCollectionItem(inherited Items[Index]);
end;

function TinfMDFeCollection.New: TinfMDFeCollectionItem;
begin
  Result := TinfMDFeCollectionItem.Create;
  Add(Result);
end;

procedure TinfMDFeCollection.SetItem(Index: Integer;
  Value: TinfMDFeCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TLeituraResumoCollectionItem }

constructor TLeituraResumoCollectionItem.Create;
begin
  inherited Create;

  FinfLeitura := TinfLeitura.Create;
  FinfMDFe := TinfMDFeCollection.Create();
end;

destructor TLeituraResumoCollectionItem.Destroy;
begin
  FinfLeitura.Free;
  FinfMDFe.Free;

  inherited;
end;

procedure TLeituraResumoCollectionItem.SetinfMDFe(
  const Value: TinfMDFeCollection);
begin
  FinfMDFe := Value;
end;

{ TLeituraResumoCollection }

function TLeituraResumoCollection.GetItem(
  Index: Integer): TLeituraResumoCollectionItem;
begin
  Result := TLeituraResumoCollectionItem(inherited Items[Index]);
end;

function TLeituraResumoCollection.New: TLeituraResumoCollectionItem;
begin
  Result := TLeituraResumoCollectionItem.Create;
  Add(Result);
end;

procedure TLeituraResumoCollection.SetItem(Index: Integer;
  Value: TLeituraResumoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.

