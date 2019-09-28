{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010                                        }
{                                                                              }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 18/11/2018: Rafael Dias
|*  - Inicio das classes base.
*******************************************************************************}

{$I ACBr.inc}

unit ACBrXmlDocument;

interface

uses
  Classes, SysUtils, libxml2;

type
  TSaveOption = (xmlNone = 0, xmlFormat = 1, xmlNoDecl = 2, xmlNoEmpty = 4,
    xmlNoXHtml = 8, xmlXHtml = 16, xmlAsXml = 32, xmlAsHtml = 64, xmlWsNonSig = 128);
  TSaveOptions = set of TSaveOption;

  EACBrXmlException = class(Exception);
  TACBrXmlNode = class;
  TACBrXmlNamespace = class;
  TACBrXmlAttribute = class;
  TACBrXMLNodeList = class;
  TACBrXMLNodeListEnumerator = class;
  TACBrXMLNamespaceList = class;
  TACBrXMLNamespaceListEnumerator = class;
  TACBrXMLAttributeList = class;
  TACBrXMLAttributeListEnumerator = class;
  TACBrXmlDocument = class;
  TACBrXmlNodeArray = array of TACBrXmlNode;

  TACBrXmlNode = class
  private
    FXmlNode: xmlNodePtr;
    FXmlCdataNode: xmlNodePtr;
    FXmlDoc: TACBrXmlDocument;
    FNamespaceList: TACBrXMLNamespaceList;
    FNodeList: TACBrXMLNodeList;
    FAttributeList: TACBrXMLAttributeList;
    FNamespaceEnumerator: TACBrXMLNamespaceListEnumerator;
    FChildEnumerator: TACBrXMLNodeListEnumerator;
    FAttributeEnumerator: TACBrXMLAttributeListEnumerator;

    constructor Create(xmlDoc: TACBrXmlDocument; xmlNode: xmlNodePtr);

    function GetName: string;
    function GetContent: string;
    procedure SetName(AName: string);
    procedure SetContent(AContent: string);

  public
    destructor Destroy; override;

    property Document: TACBrXmlDocument read FXmlDoc;
    property Name: string read GetName write SetName;
    property Namespaces: TACBrXMLNamespaceList read FNamespaceList;
    property Childrens: TACBrXMLNodeList read FNodeList;
    property Attributes: TACBrXMLAttributeList read FAttributeList;
    property Content: string read GetContent write SetContent;

    procedure AppendChild(ANode: TACBrXmlNode);
    procedure SetAttribute(AName, AContent: string);
    procedure SetNamespace(AHref: string; APrefix: string = '');

    function AddChild(AName: string; ANamespace: string = ''): TACBrXmlNode;
    function GetNextNamespace(var ANamespace: TACBrXmlNamespace): boolean;
    function GetNextChild(var ANode: TACBrXmlNode): boolean;
    function GetNextAttribute(var AAttribute: TACBrXmlAttribute): boolean;

  end;

  TACBrXmlNamespace = class
  private
    FParentNode: TACBrXmlNode;
    xmlNsInternal: xmlNsPtr;

    procedure ReplaceNamespace(xmlNs: xmlNsPtr);

    function GetPrefixo: string;
    function GetContent: string;
    procedure SetPrefixo(AName: string);
    procedure SetContent(AContent: string);

    constructor Create(ParentNode: TACBrXmlNode; xmlNs: xmlNsPtr);

  public
    destructor Destroy; override;

    property Node: TACBrXmlNode read FParentNode;
    property Prefixo: string read GetPrefixo write SetPrefixo;
    property Content: string read GetContent write SetContent;

  end;

  TACBrXmlAttribute = class
  private
    FParentNode: TACBrXmlNode;
    xmlAttInternal: xmlAttrPtr;

    function GetName: string;
    function GetContent: string;

    procedure SetName(AName: string);
    procedure SetContent(AContent: string);

    constructor Create(ParentNode: TACBrXmlNode; xmlAtt: xmlAttrPtr);

  public
    destructor Destroy; override;

    property Node: TACBrXmlNode read FParentNode;
    property Name: string read GetName write SetName;
    property Content: string read GetContent write SetContent;

  end;

  TACBrXMLNamespaceList = class
  private
    FParent: TACBrXmlNode;
    FItens: array of TACBrXmlNamespace;

    procedure Insert(Item: TACBrXmlNamespace);

    function GetCount: integer;
    function GetItem(Index: integer): TACBrXmlNamespace;

    constructor Create(AParent: TACBrXmlNode);

  public
    destructor Destroy; override;

    property Parent: TACBrXmlNode read FParent;
    property Count: integer read GetCount;
    property Items[Index: integer]: TACBrXmlNamespace read GetItem;

    procedure Remove(Item: TACBrXmlNamespace);
    function GetEnumerator: TACBrXMLNamespaceListEnumerator;

  end;

  TACBrXMLNamespaceListEnumerator = class
  private
    FIndex: integer;
    FList: TACBrXMLNamespaceList;

    constructor Create(aList: TACBrXMLNamespaceList);

  public
    function GetCurrent: TACBrXmlNamespace;
    function MoveNext: boolean;

  public
    property Current: TACBrXmlNamespace read GetCurrent;

  end;

  TACBrXMLNodeList = class
  private
    FParent: TACBrXmlNode;
    FItens: array of TACBrXmlNode;

    procedure Insert(Item: TACBrXmlNode);

    function GetCount: integer;
    function GetItem(Index: integer): TACBrXmlNode;

    constructor Create(AParent: TACBrXmlNode);

  public
    destructor Destroy; override;

    property Parent: TACBrXmlNode read FParent;
    property Count: integer read GetCount;
    property Items[Index: integer]: TACBrXmlNode read GetItem; default;

    function Find(const Name: string): TACBrXmlNode;
    function FindAll(const Name: string): TACBrXmlNodeArray;

    procedure Remove(Item: TACBrXmlNode);
    function GetEnumerator: TACBrXMLNodeListEnumerator;

  end;

  TACBrXMLNodeListEnumerator = class
  private
    FIndex: integer;
    FList: TACBrXMLNodeList;

    constructor Create(aList: TACBrXMLNodeList);

  public
    function GetCurrent: TACBrXmlNode;
    function MoveNext: boolean;

  public
    property Current: TACBrXmlNode read GetCurrent;

  end;

  TACBrXMLAttributeList = class
  private
    FParent: TACBrXmlNode;
    FItens: array of TACBrXmlAttribute;

    procedure Insert(Item: TACBrXmlAttribute);

    function GetCount: integer;
    function GetItem(AName: string): TACBrXmlAttribute;

    constructor Create(AParent: TACBrXmlNode);

  public
    destructor Destroy; override;

    property Parent: TACBrXmlNode read FParent;
    property Count: integer read GetCount;
    property Items[AName: string]: TACBrXmlAttribute read GetItem;

    procedure Remove(Item: TACBrXmlAttribute);
    function GetEnumerator: TACBrXMLAttributeListEnumerator;

  end;

  TACBrXMLAttributeListEnumerator = class
  private
    FIndex: integer;
    FList: TACBrXMLAttributeList;

    constructor Create(aList: TACBrXMLAttributeList);

  public
    function GetCurrent: TACBrXmlAttribute;
    function MoveNext: boolean;

  public
    property Current: TACBrXmlAttribute read GetCurrent;

  end;

  TACBrXmlDocument = class
  private
    xmlDocInternal: xmlDocPtr;
    xmlRootElement: TACBrXmlNode;
    FSaveOptions: TSaveOptions;

    function GetName: string;
    function GetXml: string;
    function GetSaveOptions: integer;

    procedure SetRootElement(ARootNode: TACBrXmlNode);

  public
    constructor Create(AName: string = ''; ANamespace: string = ''; APrefixNamespace: string = '');
    destructor Destroy; override;

    function CreateElement(AName: string; ANamespace: string = ''): TACBrXmlNode;

    procedure Clear();
    procedure SaveToFile(AFilename: string);
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromFile(AFilename: string);
    procedure LoadFromXml(AXmlDocument: string);
    procedure LoadFromStream(AStream: TStream);

    property Name: string read GetName;
    property Root: TACBrXmlNode read xmlRootElement write SetRootElement;
    property Xml: string read GetXml;
    property SaveOptions: TSaveOptions read FSaveOptions write FSaveOptions;

  end;

implementation

uses
  TypInfo, ACBrUtil, ACBrLibXml2, synautil;

{ XmlNode }
constructor TACBrXmlNode.Create(xmlDoc: TACBrXmlDocument; xmlNode: xmlNodePtr);
begin
  if not Assigned(xmlDoc) then
    raise EACBrXmlException.Create('XmlDocument não pode ser nulo.');
  if not Assigned(xmlNode) then
    raise EACBrXmlException.Create('XmlNode não pode ser nulo.');

  FXmlDoc := xmlDoc;
  FXmlNode := xmlNode;
  FXmlCdataNode := nil;
  FNamespaceList := TACBrXMLNamespaceList.Create(Self);
  FNodeList := TACBrXMLNodeList.Create(Self);
  FAttributeList := TACBrXMLAttributeList.Create(Self);
  FNamespaceEnumerator := TACBrXMLNamespaceListEnumerator.Create(FNamespaceList);
  FChildEnumerator := TACBrXMLNodeListEnumerator.Create(FNodeList);
  FAttributeEnumerator := TACBrXMLAttributeListEnumerator.Create(FAttributeList);
end;

destructor TACBrXmlNode.Destroy;
begin
  if FXmlNode <> nil then
  begin
    FNodeList.Destroy;
    FAttributeList.Destroy;

    xmlUnlinkNode(FXmlNode);
    xmlFreeNode(FXmlNode);
  end;

  inherited Destroy;
end;

function TACBrXmlNode.GetName: string;
begin
  Result := string(FXmlNode^.Name);
end;

function TACBrXmlNode.GetContent: string;
begin
  Result := string(xmlNodeGetContent(FXmlNode));
end;

procedure TACBrXmlNode.SetName(AName: string);
begin
  if AName = EmptyStr then
    raise EACBrXmlException.Create('O nome do nó não pode ser vazio.');

  xmlNodeSetName(FXmlNode, PAnsiChar(ansistring(AName)));
end;

procedure TACBrXmlNode.SetContent(AContent: string);
Var
  IsCData: Boolean;
  cdataValue: String;
  cdataNode: xmlNodePtr;
begin
  cdataNode := nil;

  IsCData := pos('CDATA', AContent) > 0;
  if IsCData then
  begin
    cdataValue := RetornarConteudoEntre(AContent, '<![CDATA[', ']]>');
    cdataNode := xmlNewCDataBlock(FXmlDoc.xmlDocInternal,
               PAnsichar(ansistring(cdataValue)), Length(cdataValue));
    xmlAddChild(FXmlNode, FXmlCdataNode);
  end
  else
    xmlNodeSetContent(FXmlNode, PAnsichar(ansistring(AContent)));

  if FXmlCdataNode <> nil then
  begin
    xmlUnlinkNode(FXmlCdataNode);
    xmlFreeNode(FXmlCdataNode);
  end;

  FXmlCdataNode := cdataNode;
end;

function TACBrXmlNode.AddChild(AName: string; ANamespace: string): TACBrXmlNode;
begin
  Result := FXmlDoc.CreateElement(AName, ANamespace);
  AppendChild(Result);
end;

procedure TACBrXmlNode.AppendChild(ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) then Exit;
  if not Assigned(ANode.FXmlNode) then Exit;

  ANode.FXmlNode := xmlAddChild(FXmlNode, ANode.FXmlNode);
  ANode.FXmlDoc := FXmlDoc;
  Childrens.Insert(ANode);
end;

procedure TACBrXmlNode.SetAttribute(AName, AContent: string);
var
  xmlAtt: xmlAttrPtr;
begin
  xmlAtt := xmlSetProp(FXmlNode, PAnsichar(ansistring(AName)),
    PAnsichar(ansistring(AContent)));
  if xmlAtt = nil then
    raise EACBrXmlException.Create('Erro ao adicionar atributo');

  Attributes.Insert(TACBrXmlAttribute.Create(Self, xmlAtt));
end;

procedure TACBrXmlNode.SetNamespace(AHref: string; APrefix: string);
Var
  xmlNs: xmlNsPtr;
begin
  xmlNs := xmlNewNs(FXmlNode, PAnsichar(ansistring(AHref)),
    PAnsichar(ansistring(APrefix)));
  if xmlNs = nil then
    raise EACBrXmlException.Create('Erro ao adicionar namespace');

  Namespaces.Insert(TACBrXmlNamespace.Create(Self, xmlNs));
end;

function TACBrXmlNode.GetNextNamespace(var ANamespace: TACBrXmlNamespace): boolean;
begin
  Result := FNamespaceEnumerator.MoveNext;
  ANamespace := FNamespaceEnumerator.Current;
end;

function TACBrXmlNode.GetNextChild(var ANode: TACBrXmlNode): boolean;
begin
  Result := FChildEnumerator.MoveNext;
  ANode := FChildEnumerator.Current;
end;

function TACBrXmlNode.GetNextAttribute(var AAttribute: TACBrXmlAttribute): boolean;
begin
  Result := FAttributeEnumerator.MoveNext;
  AAttribute := FAttributeEnumerator.Current;
end;

{ TACBrXmlNamespace }
constructor TACBrXmlNamespace.Create(ParentNode: TACBrXmlNode; xmlNs: xmlNsPtr);
begin
  FParentNode := ParentNode;
  xmlNsInternal := xmlNs;
end;

destructor TACBrXmlNamespace.Destroy;
Var
  prev, nsDef: xmlNsPtr;
begin
  if xmlNsInternal <> nil then
  begin
    prev := nil;
    nsDef := FParentNode.FXmlNode^.nsDef;
    while nsDef <> nil do
    begin
      if nsDef = xmlNsInternal then
      begin
        if prev = nil then
          FParentNode.FXmlNode^.nsDef := nsDef^.next
        else
          prev^.next := nsDef^.next;

        break;
      end;

      prev := nsDef;
      nsDef := nsDef^.next;
    end;

    xmlFreeNs(xmlNsInternal);
  end;

  inherited Destroy;
end;

function TACBrXmlNamespace.GetPrefixo: string;
begin
  Result := string(xmlNsInternal^.prefix);
end;

function TACBrXmlNamespace.GetContent: string;
begin
  Result := string(xmlNsInternal^.href);
end;

procedure TACBrXmlNamespace.SetPrefixo(AName: string);
Var
  xmlNs: xmlNsPtr;
begin
  xmlNs := xmlNewNs(FParentNode.FXmlNode, xmlNsInternal^.href,
    PAnsichar(ansistring(AName)));
  ReplaceNamespace(xmlNs);
end;

procedure TACBrXmlNamespace.SetContent(AContent: string);
Var
  xmlNs: xmlNsPtr;
begin
  xmlNs := xmlNewNs(FParentNode.FXmlNode, PAnsichar(ansistring(AContent)),
    xmlNsInternal^.prefix);
  ReplaceNamespace(xmlNs);
end;

procedure TACBrXmlNamespace.ReplaceNamespace(xmlNs: xmlNsPtr);
Var
  prev, nsDef: xmlNsPtr;
begin
  prev := nil;
  nsDef := FParentNode.FXmlNode^.nsDef;
  while nsDef <> nil do
  begin
    if nsDef = xmlNsInternal then
    begin
      xmlNs^.next := nsDef^.next;
      if prev = nil then
        FParentNode.FXmlNode^.nsDef := xmlNs
      else
        prev^.next := xmlNs;
      break;
    end;
    prev := nsDef;
    nsDef := nsDef^.next;
  end;

  xmlFreeNs(xmlNsInternal);
  xmlNsInternal := xmlNs;
end;

{ TACBrXmlAttribute }
constructor TACBrXmlAttribute.Create(ParentNode: TACBrXmlNode; xmlAtt: xmlAttrPtr);
begin
  FParentNode := ParentNode;
  xmlAttInternal := xmlAtt;
end;

destructor TACBrXmlAttribute.Destroy;
begin
  if xmlAttInternal <> nil then
    xmlRemoveProp(xmlAttInternal);

  inherited Destroy;
end;

function TACBrXmlAttribute.GetName: string;
begin
  Result := string(xmlAttInternal^.Name);
end;

function TACBrXmlAttribute.GetContent: string;
begin
  Result := string(xmlGetNoNsProp(FParentNode.FXmlNode, xmlAttInternal^.Name));
end;

procedure TACBrXmlAttribute.SetName(AName: string);
var
  AContent: string;
begin
  if AName = EmptyStr then
    raise EACBrXmlException.Create('O nome do atributo não pode ser vazio.');

  AContent := Content;
  if xmlAttInternal <> nil then
    xmlRemoveProp(xmlAttInternal);

  xmlAttInternal := xmlSetProp(FParentNode.FXmlNode, PAnsiChar(ansistring(AName)),
    PAnsiChar(ansistring(AContent)));
end;

procedure TACBrXmlAttribute.SetContent(AContent: string);
begin
  xmlAttInternal := xmlSetProp(FParentNode.FXmlNode, xmlAttInternal^.Name,
    PAnsiChar(ansistring(AContent)));
end;

{ TACBrXMLNamespaceList }
constructor TACBrXMLNamespaceList.Create(AParent: TACBrXmlNode);
var
  curNs: xmlNsPtr;
begin
  FParent := AParent;
  SetLength(FItens, 0);
  if FParent.FXmlNode.nsDef <> nil then
  begin
    curNs := FParent.FXmlNode.nsDef;
    while curNs <> nil do
    begin
      Insert(TACBrXmlNamespace.Create(FParent, curNs));
      curNs := curNs^.Next;
    end;
  end;
end;

destructor TACBrXMLNamespaceList.Destroy;
var
  i, ACount: integer;
begin
  ACount := Count - 1;
  for i := 0 to ACount do
    FreeAndNil(FItens[i]);

  SetLength(FItens, 0);
  Finalize(FItens);
  FItens := nil;

  inherited Destroy;
end;

function TACBrXMLNamespaceList.GetCount: integer;
begin
  Result := Length(FItens);
end;

function TACBrXMLNamespaceList.GetItem(Index: integer): TACBrXmlNamespace;
begin
  Result := FItens[Index];
end;

procedure TACBrXMLNamespaceList.Insert(Item: TACBrXmlNamespace);
var
  idx: integer;
begin
  idx := Count + 1;
  SetLength(FItens, idx);
  FItens[idx - 1] := Item;
end;

procedure TACBrXMLNamespaceList.Remove(Item: TACBrXmlNamespace);
var
  idx, ALength: integer;
begin
  ALength := Count;
  for idx := 0 to ALength do
  begin
    if FItens[idx] = Item then
    begin
      Item.Destroy;
      SetLength(FItens, ALength - 1);
      Exit;
    end;
  end;

  raise EACBrXmlException.Create('Item não se encontra na lista.');
end;

function TACBrXMLNamespaceList.GetEnumerator: TACBrXMLNamespaceListEnumerator;
begin
  Result := TACBrXMLNamespaceListEnumerator.Create(Self);
end;

{ TACBrXMLNamespaceListEnumerator }
constructor TACBrXMLNamespaceListEnumerator.Create(aList: TACBrXMLNamespaceList);
begin
  inherited Create;

  FIndex := -1;
  FList := aList;
end;

function TACBrXMLNamespaceListEnumerator.GetCurrent: TACBrXmlNamespace;
begin
  Result := FList.FItens[FIndex];
end;

function TACBrXMLNamespaceListEnumerator.MoveNext: boolean;
begin
  Result := (FIndex < FList.Count - 1);
  if Result then
    Inc(FIndex)
  else
    FIndex := -1;
end;

{ TACBrXMLNodeList }
constructor TACBrXMLNodeList.Create(AParent: TACBrXmlNode);
var
  curNode: xmlNodePtr;
begin
  FParent := AParent;
  SetLength(FItens, 0);
  if FParent.FXmlNode.children <> nil then
  begin
    curNode := FParent.FXmlNode.children;
    while curNode <> nil do
    begin
      if curNode^.type_ = XML_ELEMENT_NODE then
        Insert(TACBrXmlNode.Create(FParent.Document, curNode));

      curNode := curNode^.Next;
    end;
  end;
end;

destructor TACBrXMLNodeList.Destroy;
var
  i, ACount: integer;
begin
  ACount := Count - 1;
  for i := 0 to ACount do
    FreeAndNil(FItens[i]);

  SetLength(FItens, 0);
  Finalize(FItens);
  FItens := nil;

  inherited Destroy;
end;

function TACBrXMLNodeList.GetCount: integer;
begin
  Result := Length(FItens);
end;

function TACBrXMLNodeList.GetItem(Index: integer): TACBrXmlNode;
begin
  Result := FItens[Index];
end;

function TACBrXMLNodeList.Find(const Name: string):TACBrXmlNode;
Var
  i, ACount: integer;
  Node: TACBrXmlNode;
begin
  Result := nil;
  ACount := Count - 1;
  for i := 0 to ACount do
  begin
    Node := TACBrXmlNode(FItens[i]);
    if Node.Name <> Name then continue;

    Result := Node;
    Exit;
  end;
end;

function TACBrXMLNodeList.FindAll(const Name: string):TACBrXmlNodeArray;
Var
  Node: TACBrXmlNode;
  i, j, ACount: integer;
begin
  Result := nil;
  SetLength(Result, 0);

  j := 0;
  ACount := Count - 1;
  for i := 0 to ACount do
  begin
    Node := TACBrXmlNode(FItens[i]);
    if Node.Name <> Name then continue;

    SetLength(Result, j+1);
    Result[j] := Node;
    inc(j);
  end;
end;

procedure TACBrXMLNodeList.Insert(Item: TACBrXmlNode);
var
  idx: integer;
begin
  idx := Count + 1;
  SetLength(FItens, idx);
  FItens[idx - 1] := Item;
end;

procedure TACBrXMLNodeList.Remove(Item: TACBrXmlNode);
var
  idx, ALength: integer;
begin
  ALength := Count;
  for idx := 0 to ALength do
  begin
    if FItens[idx] = Item then
    begin
      Item.Destroy;
      SetLength(FItens, ALength - 1);
      Exit;
    end;
  end;

  raise EACBrXmlException.Create('Item não se encontra na lista.');
end;

function TACBrXMLNodeList.GetEnumerator: TACBrXMLNodeListEnumerator;
begin
  Result := TACBrXMLNodeListEnumerator.Create(Self);
end;

{ TACBrXMLNodeListEnumerator }
constructor TACBrXMLNodeListEnumerator.Create(aList: TACBrXMLNodeList);
begin
  inherited Create;

  FIndex := -1;
  FList := aList;
end;

function TACBrXMLNodeListEnumerator.GetCurrent: TACBrXmlNode;
begin
  Result := FList.FItens[FIndex];
end;

function TACBrXMLNodeListEnumerator.MoveNext: boolean;
begin
  Result := (FIndex < FList.Count - 1);
  if Result then
    Inc(FIndex)
  else
    FIndex := -1;
end;

{ TACBrXMLAttributeList }
constructor TACBrXMLAttributeList.Create(AParent: TACBrXmlNode);
var
  curAtt: xmlAttrPtr;
begin
  FParent := AParent;
  SetLength(FItens, 0);
  if FParent.FXmlNode.properties <> nil then
  begin
    curAtt := xmlAttrPtr(FParent.FXmlNode.properties);
    while curAtt <> nil do
    begin
      if curAtt^.type_ = XML_ATTRIBUTE_NODE then
        Insert(TACBrXmlAttribute.Create(FParent, curAtt));

      curAtt := curAtt^.Next;
    end;
  end;
end;

destructor TACBrXMLAttributeList.Destroy;
var
  i, ACount: integer;
begin
  ACount := Count - 1;
  for i := 0 to ACount do
    FreeAndNil(FItens[i]);

  SetLength(FItens, 0);
  Finalize(FItens);
  FItens := nil;

  inherited Destroy;
end;

function TACBrXMLAttributeList.GetCount: integer;
begin
  Result := Length(FItens);
end;

function TACBrXMLAttributeList.GetItem(AName: string): TACBrXmlAttribute;
Var
  i, ACount: integer;
  Att: TACBrXmlAttribute;
begin
  Result := nil;
  ACount := Count - 1;
  for i := 0 to ACount do
  begin
    Att := TACBrXmlAttribute(FItens[i]);
    if Att.Name <> AName then continue;

    Result := Att;
    Exit;
  end;
end;

procedure TACBrXMLAttributeList.Insert(Item: TACBrXmlAttribute);
var
  idx: integer;
begin
  idx := Count + 1;
  SetLength(FItens, idx);
  FItens[idx - 1] := Item;
end;

procedure TACBrXMLAttributeList.Remove(Item: TACBrXmlAttribute);
var
  idx, ALength: integer;
begin
  ALength := Count;
  for idx := 0 to ALength do
  begin
    if FItens[idx] = Item then
    begin
      Item.Destroy;
      SetLength(FItens, ALength - 1);
      Exit;
    end;
  end;

  raise EACBrXmlException.Create('Item não se encontra na lista.');
end;

function TACBrXMLAttributeList.GetEnumerator: TACBrXMLAttributeListEnumerator;
begin
  Result := TACBrXMLAttributeListEnumerator.Create(Self);
end;

{ TACBrXMLNodeListEnumerator }
constructor TACBrXMLAttributeListEnumerator.Create(aList: TACBrXMLAttributeList);
begin
  inherited Create;

  FIndex := -1;
  FList := aList;
end;

function TACBrXMLAttributeListEnumerator.GetCurrent: TACBrXmlAttribute;
begin
  Result := FList.FItens[FIndex];
end;

function TACBrXMLAttributeListEnumerator.MoveNext: boolean;
begin
  Result := (FIndex < FList.Count - 1);
  if Result then
    Inc(FIndex)
  else
    FIndex := -1;
end;

{ XmlDocument }
constructor TACBrXmlDocument.Create(AName: string; ANamespace: string; APrefixNamespace: string);
var
  xmlNode: xmlNodePtr;
begin
  LibXmlInit;
  FSaveOptions := [xmlFormat, xmlAsXml];
  xmlDocInternal := xmlNewDoc(PAnsichar(ansistring('1.0')));

  if AName <> EmptyStr then
  begin
    xmlNode := nil;
    xmlNode := xmlNewDocNode(xmlDocInternal, nil, PAnsichar(ansistring(AName)), nil);
    SetRootElement(TACBrXmlNode.Create(Self, xmlNode));

    if ANamespace <> EmptyStr then
    begin
      xmlRootElement.SetNamespace(ANamespace, APrefixNamespace);
    end;
  end
  else
    xmlRootElement := nil;
end;

destructor TACBrXmlDocument.Destroy;
begin
  if xmlRootElement <> nil then
    xmlRootElement.Free;
  if xmlDocInternal <> nil then
    xmlFreeDoc(xmlDocInternal);

  inherited Destroy;
end;

function TACBrXmlDocument.GetName: string;
begin
  Result := string(xmlDocInternal^.Name);
end;

function TACBrXmlDocument.GetSaveOptions: integer;
var
  i: TSaveOption;
begin
  Result := 0;
  for i := Low(TSaveOption) to High(TSaveOption) do
  begin
    if i in FSaveOptions then
      Result := Result + integer(i);
  end;
end;

function TACBrXmlDocument.GetXml: string;
var
  buffer: xmlBufferPtr;
  xmlSaveCtx: xmlSaveCtxtPtr;
begin
  try
    buffer := xmlBufferCreate();
    xmlSaveCtx := xmlSaveToBuffer(buffer, PAnsiChar(ansistring('UTF-8')), GetSaveOptions);
    xmlSaveDoc(xmlSaveCtx, xmlDocInternal);
    xmlSaveClose(xmlSaveCtx);
    Result := string(buffer.content);
  finally
    if (buffer <> nil) then
      xmlBufferFree(buffer);
  end;
end;

procedure TACBrXmlDocument.SetRootElement(ARootNode: TACBrXmlNode);
begin
  if (xmlRootElement <> nil) or Assigned(xmlRootElement) then
    FreeAndNil(xmlRootElement);

  xmlRootElement := ARootNode;
  xmlDocSetRootElement(xmlDocInternal, xmlRootElement.FXmlNode);
end;

procedure TACBrXmlDocument.Clear();
begin
  if xmlRootElement <> nil then FreeAndNil(xmlRootElement);
end;

function TACBrXmlDocument.CreateElement(AName: string; ANamespace: string): TACBrXmlNode;
var
  Namespace, NodeName: PAnsichar;
begin
  Result := nil;
  NodeName := PAnsichar(ansistring(AName));

  Result := TACBrXmlNode.Create(Self, xmlNewDocNode(xmlDocInternal, nil, NodeName, nil));
  if ANamespace <> EmptyStr then
  begin
    Namespace := PAnsichar(ansistring(ANamespace));
    xmlSetNs(Result.FXmlNode, xmlNewNs(Result.FXmlNode, Namespace, nil));
  end;
end;

procedure TACBrXmlDocument.SaveToFile(AFilename: string);
var
  xmlSaveCtx: xmlSaveCtxtPtr;
  ret: integer;
begin
  xmlSaveCtx := xmlSaveToFilename(PAnsiChar(ansistring(AFilename)),
    PAnsiChar(ansistring('UTF-8')), GetSaveOptions);
  try
    ret := xmlSaveDoc(xmlSaveCtx, xmlDocInternal);
    if ret = -1 then
      raise EACBrXmlException.Create(__xmlLastError()^.message);
  finally
    xmlSaveClose(xmlSaveCtx);
  end;
end;

procedure TACBrXmlDocument.SaveToStream(AStream: TStream);
begin
  WriteStrToStream(AStream, ansistring(Xml));
end;

procedure TACBrXmlDocument.LoadFromFile(AFilename: string);
var
  loadedDoc: xmlDocPtr;
  loadedRoot: xmlNodePtr;
begin
  loadedDoc := xmlParseFile(PAnsiChar(ansistring(AFilename)));

  if loadedDoc <> nil then
  begin
    xmlFreeDoc(xmlDocInternal);
    xmlDocInternal := loadedDoc;

    loadedRoot := xmlDocGetRootElement(xmlDocInternal);
    if loadedRoot <> nil then
    begin
      xmlRootElement.Free;
      xmlRootElement := TACBrXmlNode.Create(Self, loadedRoot);
    end
    else
      raise EACBrXmlException.Create(__xmlLastError()^.message);
  end
  else
    raise EACBrXmlException.Create(__xmlLastError()^.message);
end;

procedure TACBrXmlDocument.LoadFromXml(AXmlDocument: string);
var
  loadedDoc: xmlDocPtr;
  loadedRoot: xmlNodePtr;
begin
  loadedDoc := xmlParseDoc(PAnsiChar(ansistring(AXmlDocument)));

  if loadedDoc <> nil then
  begin
    xmlFreeDoc(xmlDocInternal);
    xmlDocInternal := loadedDoc;

    loadedRoot := xmlDocGetRootElement(xmlDocInternal);
    if loadedRoot <> nil then
    begin
      xmlRootElement.Free;
      xmlRootElement := TACBrXmlNode.Create(Self, loadedRoot);
    end
    else
      raise EACBrXmlException.Create(__xmlLastError()^.message);
  end
  else
    raise EACBrXmlException.Create(__xmlLastError()^.message);
end;

procedure TACBrXmlDocument.LoadFromStream(AStream: TStream);
Var
  Xml: String;
begin
  Xml := ReadStrFromStream(AStream, AStream.Size);
  LoadFromXml(Xml);
end;

end.
