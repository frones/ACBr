{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Rafael Teno Dias                               }
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

{******************************************************************************
|* Historico
|*
|* 18/11/2018: Rafael Dias
|*  - Inicio das classes base.
*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibXml2;

interface

uses
  Classes, SysUtils,
  {$IfDef MSWINDOWS}
  Windows,
  {$EndIf}
  {$IfDef FPC}
  DynLibs,
  {$Else}
  Types,
  {$EndIf}
  SyncObjs,
  ACBrBase;

const
  {$IFDEF MSWINDOWS}
   {$IFDEF USE_MINGW}
  LIBXML2_SO = 'libxml2-2.dll';
   {$ELSE}
  LIBXML2_SO = 'libxml2.dll';
   {$ENDIF}
  {$ELSE}
  LIBXML2_SO = 'libxml2.so';
  {$ENDIF}

  XML_DETECT_IDS = 2;
  XML_COMPLETE_ATTRS = 4;
  XML_SKIP_IDS = 8;

type
  // NEXTGEN legacy compatibility
  {$IfDef NEXTGEN}
    AnsiString = RawByteString;
    AnsiChar = UTF8Char;
    PAnsiChar = PUTF8Char;
    PPAnsiChar = ^PUTF8Char;
  {$EndIf}

  xmlElementType = (
    XML_ELEMENT_NODE = 1,
    XML_ATTRIBUTE_NODE = 2,
    XML_TEXT_NODE = 3,
    XML_CDATA_SECTION_NODE = 4,
    XML_ENTITY_REF_NODE = 5,
    XML_ENTITY_NODE = 6,
    XML_PI_NODE = 7,
    XML_COMMENT_NODE = 8,
    XML_DOCUMENT_NODE = 9,
    XML_DOCUMENT_TYPE_NODE = 10,
    XML_DOCUMENT_FRAG_NODE = 11,
    XML_NOTATION_NODE = 12,
    XML_HTML_DOCUMENT_NODE = 13,
    XML_DTD_NODE = 14,
    XML_ELEMENT_DECL = 15,
    XML_ATTRIBUTE_DECL = 16,
    XML_ENTITY_DECL = 17,
    XML_NAMESPACE_DECL = 18,
    XML_XINCLUDE_START = 19,
    XML_XINCLUDE_END = 20,
    XML_DOCB_DOCUMENT_NODE = 21);

  xmlAttributeType = (
    XML_ATTRIBUTE_CDATA = 1,
    XML_ATTRIBUTE_ID = 2,
    XML_ATTRIBUTE_IDREF = 3,
    XML_ATTRIBUTE_IDREFS = 4,
    XML_ATTRIBUTE_ENTITY = 5,
    XML_ATTRIBUTE_ENTITIES = 6,
    XML_ATTRIBUTE_NMTOKEN = 7,
    XML_ATTRIBUTE_NMTOKENS = 8,
    XML_ATTRIBUTE_ENUMERATION = 9,
    XML_ATTRIBUTE_NOTATION = 10);

  xmlErrorLevel = (
      XML_ERR_NONE = 0,
      XML_ERR_WARNING = 1,
      XML_ERR_ERROR = 2,
      XML_ERR_FATAL = 3);

  xmlBufferAllocationScheme = (
      XML_BUFFER_ALLOC_DOUBLEIT = 1,
      XML_BUFFER_ALLOC_EXACT = 2,
      XML_BUFFER_ALLOC_IMMUTABLE = 3,
      XML_BUFFER_ALLOC_IO = 4);

  xmlCharPtr = PAnsiChar;
  xmlCharPtrPtr = ^xmlCharPtr;

  xmlBufferPtr = ^xmlBuffer;

  xmlDocPtr = ^xmlDoc;
  xmlDtdPtr = ^xmlDtd;
  xmlNodePtr = ^xmlNode;
  xmlNodePtrPtr = ^xmlNodePtr;
  xmlNodeSetPtr = ^xmlNodeSet;
  xmlNsPtr = ^xmlNs;
  xmlDictPtr = ^xmlDict;
  xmlAttrPtr = ^xmlAttr;
  xmlNsType = xmlElementType;

  xmlSchemaPtr = ^xmlSchema;
  xmlSchemaAnnotPtr = ^xmlSchemaAnnot;
  xmlHashTablePtr = ^xmlHashTable;
  xmlSchemaParserCtxtPtr = ^xmlSchemaParserCtxt;
  xmlSchemaValidCtxtPtr = ^xmlSchemaValidCtxt;

  xmlSaveCtxtPtr = ^xmlSaveCtxt;
  xmlErrorPtr = ^xmlError;

  xmlDict = record
  end;

  xmlDoc = record
    _private: Pointer; { application data}
    type_: xmlElementType; { XML_DOCUMENT_NODE, must be second !}
    Name: PAnsiChar; { name/filename/URI of the document}
    children: xmlNodePtr; { the document tree}
    last: xmlNodePtr; { last child link}
    parent: xmlNodePtr; { child->parent link}
    Next: xmlNodePtr; { next sibling link }
    prev: xmlNodePtr; { previous sibling link }
    doc: xmlDocPtr; { autoreference to itself End of common part}
    compression: longint; { level of zlib compression}
    standalone: longint; { standalone document (no external refs)
                           1 if standalone="yes"
                           0 if standalone="no"
                           -1 if there is no XML declaration
                           -2 if there is an XML declaration, but no
                           standalone attribute was specified}
    intSubset: xmlDtdPtr; { the document internal subset}
    extSubset: xmlDtdPtr; { the document external subset}
    oldNs: xmlNsPtr; { Global namespace, the old way}
    version: xmlCharPtr; { the XML version string}
    encoding: xmlCharPtr; { external initial encoding, if any}
    ids: Pointer; { Hash table for ID attributes if any}
    refs: Pointer; { Hash table for IDREFs attributes if any}
    URL: xmlCharPtr; { The URI for that document}
    charset: longint; { encoding of the in-memory content actually an xmlCharEncoding}
    dict: xmlDictPtr; { dict used to allocate names or NULL}
    psvi: Pointer; { for type/PSVI informations}
  end;

  xmlDtd = record
    _private: Pointer; { application data}
    type_: xmlElementType; { XML_DTD_NODE, must be second !}
    Name: xmlCharPtr; { Name of the DTD}
    children: xmlNodePtr; { the value of the property link}
    last: xmlNodePtr; { last child link}
    parent: xmlDocPtr; { child->parent link}
    Next: xmlNodePtr; { next sibling link }
    prev: xmlNodePtr; { previous sibling link }
    doc: xmlDocPtr; { the containing document End of common part}
    notations: Pointer; { Hash table for notations if any}
    elements: Pointer; { Hash table for elements if any}
    attributes: Pointer; { Hash table for attributes if any}
    entities: Pointer; { Hash table for entities if any}
    ExternalID: xmlCharPtr; { External identifier for PUBLIC DTD}
    SystemID: xmlCharPtr; { URI for a SYSTEM or PUBLIC DTD}
    pentities: Pointer; { Hash table for param entities if any}
  end;

  xmlNode = record
    _private: Pointer; { application data}
    type_: xmlElementType; { type number, must be second !}
    Name: xmlCharPtr; { the name of the node, or the entity}
    children: xmlNodePtr; { parent->childs link}
    last: xmlNodePtr; { last child link}
    parent: xmlNodePtr; { child->parent link}
    Next: xmlNodePtr; { next sibling link }
    prev: xmlNodePtr; { previous sibling link }
    doc: xmlDocPtr; { the containing document End of common part}
    ns: xmlNsPtr; { pointer to the associated namespace}
    content: xmlCharPtr; { the content}
    properties: xmlAttrPtr; { properties list}
    nsDef: xmlNsPtr; { namespace definitions on this node}
    psvi: Pointer; { for type/PSVI informations}
    line: word; { line number}
    extra: word; { extra data for XPath/XSLT}
  end;

  xmlNodeSet = record
    nodeNr: longint; { number of nodes in the set}
    nodeMax: longint; { size of the array as allocated}
    nodeTab: xmlNodePtrPtr; { array of nodes in no particular order @@ with_ns to check wether namespace nodes should be looked at @@}
  end;

  xmlNs = record
    Next: xmlNsPtr; { next Ns link for this node }
    type_: xmlNsType; { global or local}
    href: xmlCharPtr; { URL for the namespace}
    prefix: xmlCharPtr; { prefix for the namespace}
    _private: Pointer; { application data}
    context: xmlDocPtr; { normally an xmlDoc}
  end;

  xmlAttr = record
    _private: Pointer; { application data}
    type_: xmlElementType; { XML_ATTRIBUTE_NODE, must be second !}
    Name: xmlCharPtr; { the name of the property}
    children: xmlNodePtr; { the value of the property}
    last: xmlNodePtr; { NULL}
    parent: xmlNodePtr; { child->parent link}
    Next: xmlAttrPtr; { next sibling link }
    prev: xmlAttrPtr; { previous sibling link }
    doc: xmlDocPtr; { the containing document}
    ns: xmlNsPtr; { pointer to the associated namespace}
    atype: xmlAttributeType; { the a       xmlHashTablePtr = ^xmlHashTable;
ttribute type if validating}
    psvi: Pointer; { for type/PSVI informations}
    parseFlags: longint; { set of xmlParserOption used to parse the document}
    properties: longint; { set of xmlDocProperties for this document set at the end of parsing}
  end;

  xmlSchema = record
    name : xmlCharPtr; { schema name}
    targetNamespace : xmlCharPtr; { the target namespace}
    version : xmlCharPtr; {}
    id : xmlCharPtr; { Obsolete}
    doc : xmlDocPtr; {}
    annot : xmlSchemaAnnotPtr; {}
    flags : Longint; {}
    typeDecl : xmlHashTablePtr; {}
    attrDecl : xmlHashTablePtr; {}
    attrgrpDecl : xmlHashTablePtr; {}
    elemDecl : xmlHashTablePtr; {}
    notaDecl : xmlHashTablePtr; {}
    schemasImports : xmlHashTablePtr; {}
    _private : Pointer; { unused by the library for users or bindings}
    groupDecl : xmlHashTablePtr; {}
    dict : xmlDictPtr; {}
    includes : Pointer; { the includes, this is opaque for now}
    preserve : Longint; { whether to free the document}
    counter : Longint; { used to give ononymous components unique names}
    idcDef : xmlHashTablePtr; { All identity-constraint defs.}
    volatiles : Pointer; { Obsolete}
  end;

  xmlSchemaAnnot = record
    next : xmlSchemaAnnotPtr; {}
    content : xmlNodePtr; { the annotation}
  end;

  xmlHashTable = record
  end;

  xmlSchemaValidCtxt = record
  end;

  xmlSchemaParserCtxt = record
  end;

  xmlError = record
    domain : Longint; { What part of the library raised this error}
    code : Longint; { The error code, e.g. an xmlParserError}
    message : PAnsiChar; { human-readable informative error message}
    level : xmlErrorLevel; { how consequent is the error}
    file_ : PAnsiChar; { the filename}
    line : Longint; { the line number if available}
    str1 : PAnsiChar; { extra string information}
    str2 : PAnsiChar; { extra string information}
    str3 : PAnsiChar; { extra string information}
    int1 : Longint; { extra number information}
    int2 : Longint; { column number of the error or 0 if N/A (todo: rename this field when we would break ABI)}
    ctxt : Pointer; { the parser context if available}
    node : Pointer; { the node in the tree}
  end;

  xmlSaveCtxt = record
  end;

  xmlBuffer = record
    content : xmlCharPtr; { The buffer content UTF8}
    use : Cardinal; { The buffer size used}
    size : Cardinal; { The buffer size}
    alloc : xmlBufferAllocationScheme; { The realloc method}
    contentIO : xmlCharPtr; { in IO mode we may have a different base}
  end;


type
  TProcedureCdecl = procedure(); cdecl;

  TxmlParseDoc = function(const cur: xmlCharPtr): xmlDocPtr; cdecl;
  TxmlParseFile = function(const filename: PAnsiChar): xmlDocPtr; cdecl;

  TxmlDocGetRootElement = function(doc: xmlDocPtr): xmlNodePtr; cdecl;
  TxmlDocSetRootElement = function(doc: xmlDocPtr; root: xmlNodePtr): xmlNodePtr; cdecl;
  TxmlDocCopyNode = function(const node: xmlNodePtr; doc: xmlDocPtr; extended: Longint): xmlNodePtr; cdecl;
  TxmlNewDocNode = function(doc: xmlDocPtr; ns: xmlNsPtr; const name: xmlCharPtr; const content: xmlCharPtr): xmlNodePtr; cdecl;
  TxmlNewCDataBlock = function(doc: xmlDocPtr; const content: xmlCharPtr; len: Longint): xmlNodePtr; cdecl;
  TxmlDocDumpMemory = procedure(cur: xmlDocPtr; mem: xmlCharPtrPtr; size: PInteger); cdecl;
  TxmlAddChild = function(parent: xmlNodePtr; cur: xmlNodePtr): xmlNodePtr; cdecl;
  TxmlAddChildList =function(parent: xmlNodePtr; cur: xmlNodePtr): xmlNodePtr; cdecl;
  TxmlC14NDocDumpMemory = function(doc: xmlDocPtr; nodes: xmlNodeSetPtr; exclusive: longint;
    inclusive_ns_prefixes: xmlCharPtrPtr; with_comments: longint; doc_txt_ptr: xmlCharPtrPtr): longint; cdecl;
  TxmlNewDoc = function(const version: xmlCharPtr): xmlDocPtr; cdecl;
  TxmlSaveDoc = function(ctxt: xmlSaveCtxtPtr; doc: xmlDocPtr): Longint; cdecl;
  TxmlSaveClose = function(ctxt: xmlSaveCtxtPtr): Longint; cdecl;
  TxmlSaveToFilename = function(const filename: PAnsiChar; const encoding: PAnsiChar; options: Longint): xmlSaveCtxtPtr; cdecl;
  TxmlCopyNode = function(const node: xmlNodePtr; extended: longint): xmlNodePtr; cdecl;
  TxmlNodeGetContent = function(cur: xmlNodePtr): xmlCharPtr; cdecl;
  TxmlNodeSetContent = procedure(cur: xmlNodePtr; const content: xmlCharPtr); cdecl;
  TxmlNodeAddContent = procedure(cur: xmlNodePtr; const content: xmlCharPtr); cdecl;
  TxmlNodeSetName = procedure(cur: xmlNodePtr; const name: xmlCharPtr); cdecl;
  TxmlNewNs = function(node: xmlNodePtr; const href: xmlCharPtr; const prefix: xmlCharPtr): xmlNsPtr; cdecl;
  TxmlSetNs = procedure(node: xmlNodePtr; ns: xmlNsPtr); cdecl;
  TxmlGetNoNsProp = function(node: xmlNodePtr; const name: xmlCharPtr): xmlCharPtr; cdecl;
  TxmlFreeNs = procedure(cur: xmlNsPtr); cdecl;
  TxmlSetProp = function(node: xmlNodePtr; const name: xmlCharPtr; const value: xmlCharPtr): xmlAttrPtr; cdecl;
  TxmlRemoveProp = function(cur: xmlAttrPtr): Longint; cdecl;
  TxmlUnsetNsProp = function(node: xmlNodePtr; ns: xmlNsPtr; const name: xmlCharPtr): Longint; cdecl;
  TxmlUnlinkNode = procedure(cur: xmlNodePtr); cdecl;
  TxmlReadMemory = function(const buffer: PAnsiChar; size: Longint; const URL: PAnsiChar;
    const encoding: PAnsiChar; options: Longint) : xmlDocPtr; cdecl;
  TxmlFreeNode = procedure(cur: xmlNodePtr); cdecl;
  TxmlFreeDoc = procedure(cur: xmlDocPtr); cdecl;

  TxmlSchemaNewParserCtxt = function(const URL: PAnsiChar): xmlSchemaParserCtxtPtr; cdecl;
  TxmlSchemaParse = function(ctxt: xmlSchemaParserCtxtPtr): xmlSchemaPtr; cdecl;
  TxmlSchemaNewValidCtxt = function(schema: xmlSchemaPtr): xmlSchemaValidCtxtPtr; cdecl;
  TxmlSchemaValidateDoc = function(ctxt: xmlSchemaValidCtxtPtr; doc: xmlDocPtr): Longint; cdecl;
  TxmlSchemaFreeParserCtxt = procedure(ctxt: xmlSchemaParserCtxtPtr); cdecl;
  TxmlSchemaFreeValidCtxt = procedure(ctxt: xmlSchemaValidCtxtPtr); cdecl;
  TxmlSchemaFree = procedure(schema: xmlSchemaPtr); cdecl;

  TxmlBufferCreate = function(): xmlBufferPtr; cdecl;
  TxmlSaveToBuffer = function(buffer: xmlBufferPtr; const encoding: PAnsiChar; options: Longint): xmlSaveCtxtPtr; cdecl;
  TxmlNodeDump = function(buf: xmlBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr;
    level: Longint; format: Longint) : Longint; cdecl;
  TxmlBufferFree = procedure(buf: xmlBufferPtr); cdecl;

  TxmlGetLastError = function(): xmlErrorPtr; cdecl;

  TxmlFree = procedure(mem: Pointer); cdecl;
  xmlFreeFuncPtr = ^TxmlFree;
  TxmlSubstituteEntitiesDefault = function(val: Longint): Longint; cdecl;
  TFunctionPInteger = function(): PInteger; cdecl;


function InitLibXml2Interface: Boolean;
function DestroyLibXml2Interface: Boolean;

procedure xmlInitCharEncodingHandlers();
procedure xmlInitGlobals();
procedure xmlInitThreads();
procedure xmlInitParser();

procedure xmlCleanupParser();
procedure xmlCleanupThreads();

function xmlParseDoc(const cur: xmlCharPtr): xmlDocPtr;
function xmlParseFile(const filename: PAnsiChar): xmlDocPtr;

function xmlNewDoc(const version: xmlCharPtr): xmlDocPtr;
function xmlSaveDoc(ctxt: xmlSaveCtxtPtr; doc: xmlDocPtr): Longint;
function xmlSaveClose(ctxt: xmlSaveCtxtPtr): Longint;
function xmlSaveToFilename(const filename: PAnsiChar; const encoding: PAnsiChar; options: Longint): xmlSaveCtxtPtr;
function xmlDocGetRootElement(doc: xmlDocPtr): xmlNodePtr;
function xmlDocSetRootElement(doc: xmlDocPtr; root: xmlNodePtr): xmlNodePtr;
function xmlDocCopyNode(const node: xmlNodePtr; doc: xmlDocPtr; extended: Longint): xmlNodePtr;
function xmlNewDocNode(doc: xmlDocPtr; ns: xmlNsPtr; const name: xmlCharPtr; const content: xmlCharPtr): xmlNodePtr;
function xmlNewCDataBlock(doc: xmlDocPtr; const content: xmlCharPtr; len: Longint): xmlNodePtr;
procedure xmlDocDumpMemory(cur: xmlDocPtr; mem: xmlCharPtrPtr; size: PInteger);
function xmlAddChild(parent: xmlNodePtr; cur: xmlNodePtr): xmlNodePtr;
function xmlAddChildList(parent: xmlNodePtr; cur: xmlNodePtr): xmlNodePtr;
function xmlC14NDocDumpMemory(doc: xmlDocPtr; nodes: xmlNodeSetPtr; exclusive: longint;
  inclusive_ns_prefixes: xmlCharPtrPtr; with_comments: longint; doc_txt_ptr: xmlCharPtrPtr): longint;
function xmlCopyNode(const node: xmlNodePtr; extended: longint): xmlNodePtr;
function xmlNodeGetContent(cur: xmlNodePtr): xmlCharPtr;
procedure xmlNodeSetContent(cur: xmlNodePtr; const content: xmlCharPtr);
procedure xmlNodeAddContent(cur: xmlNodePtr; const content: xmlCharPtr);
procedure xmlNodeSetName(cur: xmlNodePtr; const name: xmlCharPtr);
function xmlNewNs(node: xmlNodePtr; const href: xmlCharPtr; const prefix: xmlCharPtr): xmlNsPtr;
procedure xmlSetNs(node: xmlNodePtr; ns: xmlNsPtr);
function xmlGetNoNsProp(node: xmlNodePtr; const name: xmlCharPtr): xmlCharPtr;
procedure xmlFreeNs(cur: xmlNsPtr);
function xmlSetProp(node: xmlNodePtr; const name: xmlCharPtr; const value: xmlCharPtr): xmlAttrPtr;
function xmlRemoveProp(cur: xmlAttrPtr): Longint;
function xmlUnsetNsProp(node: xmlNodePtr; ns: xmlNsPtr; const name: xmlCharPtr): Longint;
procedure xmlUnlinkNode(cur: xmlNodePtr);
function xmlReadMemory(const buffer: PAnsiChar; size: Longint; const URL: PAnsiChar;
  const encoding: PAnsiChar; options: Longint): xmlDocPtr;
procedure xmlFreeNode(cur: xmlNodePtr);
procedure xmlFreeDoc(cur: xmlDocPtr);

function xmlSchemaNewParserCtxt(const URL: PAnsiChar): xmlSchemaParserCtxtPtr;
function xmlSchemaParse(ctxt: xmlSchemaParserCtxtPtr): xmlSchemaPtr;
function xmlSchemaNewValidCtxt(schema: xmlSchemaPtr): xmlSchemaValidCtxtPtr;
function xmlSchemaValidateDoc (ctxt: xmlSchemaValidCtxtPtr; doc: xmlDocPtr) : Longint;
procedure xmlSchemaFreeParserCtxt(ctxt: xmlSchemaParserCtxtPtr);
procedure xmlSchemaFreeValidCtxt (ctxt: xmlSchemaValidCtxtPtr);
procedure xmlSchemaFree(schema: xmlSchemaPtr);

function xmlBufferCreate(): xmlBufferPtr;
function xmlSaveToBuffer(buffer: xmlBufferPtr; const encoding: PAnsiChar; options: Longint): xmlSaveCtxtPtr;
function xmlNodeDump(buf: xmlBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr;
  level: Longint; format: Longint) : Longint;
procedure xmlBufferFree(buf: xmlBufferPtr);

function xmlGetLastError(): xmlErrorPtr;
procedure xmlFree(mem: Pointer);
function xmlSubstituteEntitiesDefault(const val: Longint): Longint;
procedure xmlLoadExtDtdDefaultValue(const val: LongInt);
procedure xmlIndentTreeOutput(const val: LongInt);
procedure xmlSaveNoEmptyTags(const val: LongInt);

var
  LibXml2LoadVerbose: Boolean = False;
  LibXml2UnavailableFunctions: String = '';
  LibXml2Path: String = '';
  LibXml2Handle: TLibHandle = 0;
  LibXml2File: String = '';

implementation

uses
  TypInfo, strutils
  {$IfDef ANDROID}
   ,System.IOUtils
  {$EndIf};

var
  _xmlInitCharEncodingHandlers: TProcedureCdecl = nil;
  _xmlInitGlobals: TProcedureCdecl = nil;
  _xmlInitThreads: TProcedureCdecl = nil;
  _xmlInitParser: TProcedureCdecl = nil;
  _xmlCleanupParser: TProcedureCdecl = nil;
  _xmlCleanupThreads: TProcedureCdecl = nil;

  _xmlParseDoc: TxmlParseDoc = nil;
  _xmlParseFile: TxmlParseFile = nil;
  _xmlNewDoc: TxmlNewDoc = nil;
  _xmlSaveDoc: TxmlSaveDoc = nil;
  _xmlSaveClose: TxmlSaveClose = nil;
  _xmlSaveToFilename: TxmlSaveToFilename = nil;
  _xmlDocGetRootElement: TxmlDocGetRootElement = nil;
  _xmlDocSetRootElement: TxmlDocSetRootElement = nil;
  _xmlDocCopyNode: TxmlDocCopyNode = nil;
  _xmlNewDocNode: TxmlNewDocNode = nil;
  _xmlNewCDataBlock: TxmlNewCDataBlock = nil;
  _xmlDocDumpMemory: TxmlDocDumpMemory = nil;
  _xmlAddChild: TxmlAddChild = nil;
  _xmlAddChildList: TxmlAddChildList = nil;
  _xmlC14NDocDumpMemory: TxmlC14NDocDumpMemory = nil;
  _xmlCopyNode: TxmlCopyNode = nil;
  _xmlNodeGetContent: TxmlNodeGetContent = nil;
  _xmlNodeSetContent: TxmlNodeSetContent = nil;
  _xmlNodeAddContent: TxmlNodeAddContent = nil;
  _xmlNodeSetName: TxmlNodeSetName = nil;
  _xmlNewNs: TxmlNewNs = nil;
  _xmlSetNs: TxmlSetNs = nil;
  _xmlGetNoNsProp: TxmlGetNoNsProp = nil;
  _xmlFreeNs: TxmlFreeNs = nil;
  _xmlSetProp: TxmlSetProp = nil;
  _xmlRemoveProp: TxmlRemoveProp = nil;
  _xmlUnsetNsProp: TxmlUnsetNsProp = nil;
  _xmlUnlinkNode: TxmlUnlinkNode = nil;
  _xmlReadMemory: TxmlReadMemory = nil;
  _xmlFreeNode: TxmlFreeNode = nil;
  _xmlFreeDoc: TxmlFreeDoc = nil;

  _xmlSchemaNewParserCtxt: TxmlSchemaNewParserCtxt = nil;
  _xmlSchemaParse: TxmlSchemaParse = nil;
  _xmlSchemaNewValidCtxt: TxmlSchemaNewValidCtxt = nil;
  _xmlSchemaValidateDoc: TxmlSchemaValidateDoc = nil;
  _xmlSchemaFreeParserCtxt: TxmlSchemaFreeParserCtxt = nil;
  _xmlSchemaFreeValidCtxt: TxmlSchemaFreeValidCtxt = nil;
  _xmlSchemaFree: TxmlSchemaFree = nil;

  _xmlBufferCreate: TxmlBufferCreate = nil;
  _xmlSaveToBuffer: TxmlSaveToBuffer = nil;
  _xmlNodeDump: TxmlNodeDump = nil;
  _xmlBufferFree: TxmlBufferFree = nil;

  _xmlGetLastError: TxmlGetLastError = nil;
  _xmlSubstituteEntitiesDefault: TxmlSubstituteEntitiesDefault = nil;
  pxmlFree: xmlFreeFuncPtr = nil;
  _xmlLoadExtDtdDefaultValue: TFunctionPInteger = nil;
  _xmlIndentTreeOutput: TFunctionPInteger = nil;
  _xmlSaveNoEmptyTags: TFunctionPInteger = nil;

var
  LibXML2CS: TCriticalSection;
  LibXml2Loaded: Boolean = False;

{ ACBrLibXml2 }

function LoadLib(const Value: String): HModule;
begin
  if (LibXml2Path <> '') then
  begin
    if (RightStr(LibXml2Path, Length(PathDelim)) <> PathDelim) then
      LibXml2Path := LibXml2Path + PathDelim;
  end;

 {$IfDef FPC}
  Result := dynlibs.LoadLibrary(LibXml2Path + Value);
 {$Else}
  Result := LoadLibrary(PChar(LibXml2Path + Value));
 {$ENDIF}
end;

function LoadLibraryLibXml2: Boolean;
{$IfDef MSWINDOWS}
var
  x: Integer;
  s: String;
{$EndIf}
begin
  LibXml2Handle := LoadLib(LIBXML2_SO);

  {$IfDef MSWINDOWS}
  if (LibXml2Handle <> 0) then
  begin
    SetLength(s, 1024);
    x := GetModuleFilename(LibXml2Handle, PChar(s), Length(s));
    SetLength(s, x);
    LibXml2File := s;
  end;
  {$EndIf}

  Result := (LibXml2Handle <> 0);
end;

procedure UnloadLibraryLibXml2;
begin
  LibXml2Loaded := False;
  if LibXml2Handle <> 0 then
  begin
    FreeLibrary(LibXml2Handle);
    LibXml2Handle := 0;
  end;
end;

function GetProcAddr(module: HModule; const ProcName: String): Pointer;
begin
  Result := GetProcAddress(module, PChar(ProcName));
  if LibXml2LoadVerbose and (Result = nil) then
    LibXml2UnavailableFunctions := LibXml2UnavailableFunctions + ProcName + sLineBreak;
end;

procedure LoadLibXML2EntryPoints;
begin
  LibXml2UnavailableFunctions := '';

  _xmlInitCharEncodingHandlers := GetProcAddr(LibXml2Handle, 'xmlInitCharEncodingHandlers');
  _xmlInitGlobals := GetProcAddr(LibXml2Handle, 'xmlInitGlobals');
  _xmlInitThreads := GetProcAddr(LibXml2Handle, 'xmlInitThreads');
  _xmlInitParser := GetProcAddr(LibXml2Handle, 'xmlInitParser');
  _xmlCleanupParser := GetProcAddr(LibXml2Handle, 'xmlCleanupParser');
  _xmlCleanupThreads := GetProcAddr(LibXml2Handle, 'xmlCleanupThreads');

  _xmlParseDoc := GetProcAddr(LibXml2Handle, 'xmlParseDoc');
  _xmlParseFile := GetProcAddr(LibXml2Handle, 'xmlParseFile');
  _xmlNewDoc := GetProcAddr(LibXml2Handle, 'xmlNewDoc');
  _xmlSaveDoc := GetProcAddr(LibXml2Handle, 'xmlSaveDoc');
  _xmlSaveClose := GetProcAddr(LibXml2Handle, 'xmlSaveClose');
  _xmlSaveToFilename := GetProcAddr(LibXml2Handle, 'xmlSaveToFilename');
  _xmlDocGetRootElement := GetProcAddr(LibXml2Handle, 'xmlDocGetRootElement');
  _xmlDocSetRootElement := GetProcAddr(LibXml2Handle, 'xmlDocSetRootElement');
  _xmlDocCopyNode := GetProcAddr(LibXml2Handle, 'xmlDocCopyNode');
  _xmlNewDocNode := GetProcAddr(LibXml2Handle, 'xmlNewDocNode');
  _xmlNewCDataBlock := GetProcAddr(LibXml2Handle, 'xmlNewCDataBlock');
  _xmlDocDumpMemory := GetProcAddr(LibXml2Handle, 'xmlDocDumpMemory');
  _xmlAddChild := GetProcAddr(LibXml2Handle, 'xmlAddChild');
  _xmlAddChildList := GetProcAddr(LibXml2Handle, 'xmlAddChildList');
  _xmlC14NDocDumpMemory := GetProcAddr(LibXml2Handle, 'xmlC14NDocDumpMemory');
  _xmlCopyNode := GetProcAddr(LibXml2Handle, 'xmlCopyNode');
  _xmlNodeGetContent := GetProcAddr(LibXml2Handle, 'xmlNodeGetContent');
  _xmlNodeSetContent := GetProcAddr(LibXml2Handle, 'xmlNodeSetContent');
  _xmlNodeAddContent := GetProcAddr(LibXml2Handle, 'xmlNodeAddContent');
  _xmlNodeSetName := GetProcAddr(LibXml2Handle, 'xmlNodeSetName');
  _xmlNewNs := GetProcAddr(LibXml2Handle, 'xmlNewNs');
  _xmlSetNs := GetProcAddr(LibXml2Handle, 'xmlSetNs');
  _xmlGetNoNsProp := GetProcAddr(LibXml2Handle, 'xmlGetNoNsProp');
  _xmlFreeNs := GetProcAddr(LibXml2Handle, 'xmlFreeNs');
  _xmlSetProp := GetProcAddr(LibXml2Handle, 'xmlSetProp');
  _xmlRemoveProp := GetProcAddr(LibXml2Handle, 'xmlRemoveProp');
  _xmlUnsetNsProp := GetProcAddr(LibXml2Handle, 'xmlUnsetNsProp');
  _xmlUnlinkNode := GetProcAddr(LibXml2Handle, 'xmlUnlinkNode');
  _xmlReadMemory := GetProcAddr(LibXml2Handle, 'xmlReadMemory');
  _xmlFreeNode := GetProcAddr(LibXml2Handle, 'xmlFreeNode');
  _xmlFreeDoc := GetProcAddr(LibXml2Handle, 'xmlFreeDoc');

  _xmlSchemaNewParserCtxt := GetProcAddr(LibXml2Handle, 'xmlSchemaNewParserCtxt');
  _xmlSchemaParse := GetProcAddr(LibXml2Handle, 'xmlSchemaParse');
  _xmlSchemaNewValidCtxt := GetProcAddr(LibXml2Handle, 'xmlSchemaNewValidCtxt');
  _xmlSchemaValidateDoc := GetProcAddr(LibXml2Handle, 'xmlSchemaValidateDoc');
  _xmlSchemaFreeParserCtxt := GetProcAddr(LibXml2Handle, 'xmlSchemaFreeParserCtxt');
  _xmlSchemaFreeValidCtxt := GetProcAddr(LibXml2Handle, 'xmlSchemaFreeValidCtxt');
  _xmlSchemaFree := GetProcAddr(LibXml2Handle, 'xmlSchemaFree');

  _xmlBufferCreate := GetProcAddr(LibXml2Handle, 'xmlBufferCreate');
  _xmlSaveToBuffer := GetProcAddr(LibXml2Handle, 'xmlSaveToBuffer');
  _xmlNodeDump := GetProcAddr(LibXml2Handle, 'xmlNodeDump');
  _xmlBufferFree := GetProcAddr(LibXml2Handle, 'xmlBufferFree');

  _xmlGetLastError := GetProcAddr(LibXml2Handle, 'xmlGetLastError');
  _xmlSubstituteEntitiesDefault := GetProcAddr(LibXml2Handle, 'xmlSubstituteEntitiesDefault');

  pxmlFree := xmlFreeFuncPtr(GetProcAddr(LibXml2Handle, 'xmlFree'));
  _xmlLoadExtDtdDefaultValue := GetProcAddr(LibXml2Handle, '__xmlLoadExtDtdDefaultValue');
  _xmlIndentTreeOutput := GetProcAddr(LibXml2Handle, '__xmlIndentTreeOutput');
  _xmlSaveNoEmptyTags := GetProcAddr(LibXml2Handle, '__xmlSaveNoEmptyTags');
end;

function InitLibXml2Interface: Boolean;
begin
  Result := LibXml2Loaded;
  if LibXml2Loaded then
    Exit;

  LibXML2CS.Enter;
  try
    {$IfDef ANDROID}
    if (LibXml2Path = '') then     // Try to load from "./assets/internal/" first
      LibXml2Path := TPath.GetDocumentsPath;

    Result := LoadLibraryLibXml2;
    if (not Result) then         // Try System Default Lib
    begin
      LibXml2Path := '';
      Result := LoadLibraryLibXml2;
    end;
    {$Else}
    Result := LoadLibraryLibXml2;
    {$EndIf}


    if Not Result then
    begin
      UnloadLibraryLibXml2;
      Exit;
    end;

    LoadLibXML2EntryPoints;

    // LibXML2 initialization
    if Assigned(_xmlInitCharEncodingHandlers) then
      _xmlInitCharEncodingHandlers;
    if Assigned(_xmlInitGlobals) then
      _xmlInitGlobals;
    if Assigned(_xmlInitThreads) then
      _xmlInitThreads;
    if Assigned(_xmlInitParser) then
      _xmlInitParser;

    LibXml2Loaded := True;
  finally
    LibXML2CS.Leave;
  end;
end;

procedure ClearLibXml2EntryPoints;
begin
  _xmlInitCharEncodingHandlers := nil;
  _xmlInitGlobals := nil;
  _xmlInitThreads := nil;
  _xmlInitParser := nil;
  _xmlCleanupParser := nil;
  _xmlCleanupThreads := nil;
  _xmlSubstituteEntitiesDefault := nil;
  _xmlParseDoc := nil;
  _xmlParseFile := nil;
  _xmlNewDoc := nil;
  _xmlSaveDoc := nil;
  _xmlSaveClose := nil;
  _xmlSaveToFilename := nil;
  _xmlDocGetRootElement := nil;
  _xmlDocSetRootElement := nil;
  _xmlDocCopyNode := nil;
  _xmlNewDocNode := nil;
  _xmlNewCDataBlock := nil;
  _xmlDocDumpMemory := nil;
  _xmlAddChild := nil;
  _xmlAddChildList := nil;
  _xmlC14NDocDumpMemory := nil;
  _xmlCopyNode := nil;
  _xmlNodeGetContent := nil;
  _xmlNodeSetContent := nil;
  _xmlNodeAddContent := nil;
  _xmlNodeSetName := nil;
  _xmlNewNs := nil;
  _xmlSetNs := nil;
  _xmlGetNoNsProp := nil;
  _xmlFreeNs := nil;
  _xmlSetProp := nil;
  _xmlRemoveProp := nil;
  _xmlUnsetNsProp := nil;
  _xmlUnlinkNode := nil;
  _xmlReadMemory := nil;
  _xmlFreeNode := nil;
  _xmlFreeDoc := nil;
  _xmlSchemaNewParserCtxt := nil;
  _xmlSchemaParse := nil;
  _xmlSchemaNewValidCtxt := nil;
  _xmlSchemaValidateDoc := nil;
  _xmlSchemaFreeParserCtxt := nil;
  _xmlSchemaFreeValidCtxt := nil;
  _xmlSchemaFree := nil;
  _xmlBufferCreate := nil;
  _xmlSaveToBuffer := nil;
  _xmlNodeDump := nil;
  _xmlBufferFree := nil;
  _xmlGetLastError := nil;
  pxmlFree := nil;
  _xmlLoadExtDtdDefaultValue := nil;
  _xmlIndentTreeOutput := nil;
  _xmlSaveNoEmptyTags := nil;
end;

function DestroyLibXml2Interface: Boolean;
begin
  Result := not LibXml2Loaded;
  if (not LibXml2Loaded) then
    Exit;

  LibXML2CS.Enter;
  try
    { Shutdown libxslt/libxml }
    xmlCleanupParser();
    xmlCleanupThreads();

    ClearLibXml2EntryPoints;
    UnloadLibraryLibXml2;
  finally
    LibXML2CS.Leave;
  end;
end;


procedure xmlInitCharEncodingHandlers();
begin
  if InitLibXml2Interface and Assigned(_xmlInitCharEncodingHandlers) then
    _xmlInitCharEncodingHandlers;
end;

procedure xmlInitGlobals();
begin
  if InitLibXml2Interface and Assigned(_xmlInitGlobals) then
    _xmlInitGlobals;
end;

procedure xmlInitThreads();
begin
  if InitLibXml2Interface and Assigned(_xmlInitThreads) then
    _xmlInitThreads;
end;

procedure xmlInitParser();
begin
  if InitLibXml2Interface and Assigned(_xmlInitParser) then
    _xmlInitParser;
end;

procedure xmlCleanupParser();
begin
  if InitLibXml2Interface and Assigned(_xmlCleanupParser) then
    _xmlCleanupParser;
end;

procedure xmlCleanupThreads();
begin
  if InitLibXml2Interface and Assigned(_xmlCleanupThreads) then
    _xmlCleanupThreads;
end;

function xmlSubstituteEntitiesDefault(const val: Longint): Longint;
begin
  if InitLibXml2Interface and Assigned(_xmlSubstituteEntitiesDefault) then
    Result := _xmlSubstituteEntitiesDefault(val)
  else
    Result := -1;
end;

function xmlParseDoc(const cur: xmlCharPtr): xmlDocPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlParseDoc) then
    Result := _xmlParseDoc(cur)
  else
    Result := nil;
end;

function xmlParseFile(const filename: PAnsiChar): xmlDocPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlParseFile) then
    Result := _xmlParseFile(filename)
  else
    Result := nil;
end;

function xmlNewDoc(const version: xmlCharPtr): xmlDocPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlNewDoc) then
    Result := _xmlNewDoc(version)
  else
    Result := nil;
end;

function xmlSaveDoc(ctxt: xmlSaveCtxtPtr; doc: xmlDocPtr): Longint;
begin
  if InitLibXml2Interface and Assigned(_xmlSaveDoc) then
    Result := _xmlSaveDoc(ctxt, doc)
  else
    Result := -1;
end;

function xmlSaveClose(ctxt: xmlSaveCtxtPtr): Longint;
begin
  if InitLibXml2Interface and Assigned(_xmlSaveClose) then
    Result := _xmlSaveClose(ctxt)
  else
    Result := -1;
end;

function xmlSaveToFilename(const filename: PAnsiChar;
  const encoding: PAnsiChar; options: Longint): xmlSaveCtxtPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlSaveToFilename) then
    Result := _xmlSaveToFilename(filename, encoding, options)
  else
    Result := nil;
end;

function xmlDocGetRootElement(doc: xmlDocPtr): xmlNodePtr;
begin
  if InitLibXml2Interface and Assigned(_xmlDocGetRootElement) then
    Result := _xmlDocGetRootElement(doc)
  else
    Result := nil;
end;

function xmlDocSetRootElement(doc: xmlDocPtr; root: xmlNodePtr): xmlNodePtr;
begin
  if InitLibXml2Interface and Assigned(_xmlDocSetRootElement) then
    Result := _xmlDocSetRootElement(doc, root)
  else
    Result := nil;
end;

function xmlDocCopyNode(const node: xmlNodePtr; doc: xmlDocPtr;
  extended: Longint): xmlNodePtr;
begin
  if InitLibXml2Interface and Assigned(_xmlDocCopyNode) then
    Result := _xmlDocCopyNode(node, doc, extended)
  else
    Result := nil;
end;

function xmlNewDocNode(doc: xmlDocPtr; ns: xmlNsPtr; const name: xmlCharPtr;
  const content: xmlCharPtr): xmlNodePtr;
begin
  if InitLibXml2Interface and Assigned(_xmlNewDocNode) then
    Result := _xmlNewDocNode(doc, ns, name, content)
  else
    Result := nil;
end;

function xmlNewCDataBlock(doc: xmlDocPtr; const content: xmlCharPtr;
  len: Longint): xmlNodePtr;
begin
  if InitLibXml2Interface and Assigned(_xmlNewCDataBlock) then
    Result := _xmlNewCDataBlock(doc, content, len)
  else
    Result := nil;
end;

procedure xmlDocDumpMemory(cur: xmlDocPtr; mem: xmlCharPtrPtr; size: PInteger);
begin
  if InitLibXml2Interface and Assigned(_xmlDocDumpMemory) then
    _xmlDocDumpMemory(cur, mem, size);
end;

function xmlAddChild(parent: xmlNodePtr; cur: xmlNodePtr): xmlNodePtr;
begin
  if InitLibXml2Interface and Assigned(_xmlAddChild) then
    Result := _xmlAddChild(parent, cur)
  else
    Result := nil;
end;

function xmlAddChildList(parent: xmlNodePtr; cur: xmlNodePtr): xmlNodePtr;
begin
  if InitLibXml2Interface and Assigned(_xmlAddChildList) then
    Result := _xmlAddChildList(parent, cur)
  else
    Result := nil;
end;

function xmlC14NDocDumpMemory(doc: xmlDocPtr; nodes: xmlNodeSetPtr; exclusive: longint; inclusive_ns_prefixes: xmlCharPtrPtr; with_comments: longint; doc_txt_ptr: xmlCharPtrPtr): longint;
begin
  if InitLibXml2Interface and Assigned(_xmlC14NDocDumpMemory) then
    Result := _xmlC14NDocDumpMemory(doc, nodes, exclusive, inclusive_ns_prefixes, with_comments, doc_txt_ptr)
  else
    Result := -1;
end;

function xmlCopyNode(const node: xmlNodePtr; extended: longint): xmlNodePtr;
begin
  if InitLibXml2Interface and Assigned(_xmlCopyNode) then
    Result := _xmlCopyNode(node, extended)
  else
    Result := nil;
end;

function xmlNodeGetContent(cur: xmlNodePtr): xmlCharPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlNodeGetContent) then
    Result := _xmlNodeGetContent(cur)
  else
    Result := nil;
end;

procedure xmlNodeSetContent(cur: xmlNodePtr; const content: xmlCharPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlNodeSetContent) then
    _xmlNodeSetContent(cur, content);
end;

procedure xmlNodeAddContent(cur: xmlNodePtr; const content: xmlCharPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlNodeAddContent) then
    _xmlNodeAddContent(cur, content);
end;

procedure xmlNodeSetName(cur: xmlNodePtr; const name: xmlCharPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlNodeSetName) then
    _xmlNodeSetName(cur, name);
end;

function xmlNewNs(node: xmlNodePtr; const href: xmlCharPtr; const prefix: xmlCharPtr): xmlNsPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlNewNs) then
    Result := _xmlNewNs(node, href, prefix)
  else
    Result := nil;
end;

procedure xmlSetNs(node: xmlNodePtr; ns: xmlNsPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlSetNs) then
    _xmlSetNs(node, ns);
end;

function xmlGetNoNsProp(node: xmlNodePtr; const name: xmlCharPtr): xmlCharPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlGetNoNsProp) then
    Result := _xmlGetNoNsProp(node, name)
  else
    Result := nil;
end;

procedure xmlFreeNs(cur: xmlNsPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlFreeNs) then
    _xmlFreeNs(cur);
end;

function xmlSetProp(node: xmlNodePtr; const name: xmlCharPtr;
  const value: xmlCharPtr): xmlAttrPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlSetProp) then
    Result := _xmlSetProp(node, name, value)
  else
    Result := nil;
end;

function xmlRemoveProp(cur: xmlAttrPtr): Longint;
begin
  if InitLibXml2Interface and Assigned(_xmlRemoveProp) then
    Result := _xmlRemoveProp(cur)
  else
    Result := -1;
end;

function xmlUnsetNsProp(node: xmlNodePtr; ns: xmlNsPtr; const name: xmlCharPtr): Longint;
begin
  if InitLibXml2Interface and Assigned(_xmlUnsetNsProp) then
    Result := _xmlUnsetNsProp(node, ns, name)
  else
    Result := -1;
end;

procedure xmlUnlinkNode(cur: xmlNodePtr);
begin
  if InitLibXml2Interface and Assigned(_xmlUnlinkNode) then
    _xmlUnlinkNode(cur);
end;

function xmlReadMemory(const buffer: PAnsiChar; size: Longint;
  const URL: PAnsiChar; const encoding: PAnsiChar; options: Longint): xmlDocPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlReadMemory) then
    Result := _xmlReadMemory(buffer, size, URL, encoding, options)
  else
    Result := nil;
end;

procedure xmlFreeNode(cur: xmlNodePtr);
begin
  if InitLibXml2Interface and Assigned(_xmlFreeNode) then
    _xmlFreeNode(cur);
end;

procedure xmlFreeDoc(cur: xmlDocPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlFreeDoc) then
    _xmlFreeDoc(cur);
end;

procedure xmlFree(mem: Pointer);
begin
  if InitLibXml2Interface and Assigned(pxmlFree) then
    pxmlFree^(mem);
end;

function xmlGetLastError(): xmlErrorPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlGetLastError) then
    Result := _xmlGetLastError
  else
    Result := nil;
end;

procedure xmlLoadExtDtdDefaultValue(const val: LongInt);
begin
  if InitLibXml2Interface and Assigned(_xmlLoadExtDtdDefaultValue) then
    _xmlLoadExtDtdDefaultValue^ := val;
end;

procedure xmlIndentTreeOutput(const val: LongInt);
begin
  if InitLibXml2Interface and Assigned(_xmlIndentTreeOutput) then
    _xmlIndentTreeOutput^ := val;
end;

procedure xmlSaveNoEmptyTags(const val: LongInt);
begin
  if InitLibXml2Interface and Assigned(_xmlSaveNoEmptyTags) then
    _xmlSaveNoEmptyTags^ := val;
end;

function xmlSchemaNewParserCtxt(const URL: PAnsiChar): xmlSchemaParserCtxtPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlSchemaNewParserCtxt) then
    Result := _xmlSchemaNewParserCtxt(URL)
  else
    Result := nil;
end;

function xmlSchemaParse(ctxt: xmlSchemaParserCtxtPtr): xmlSchemaPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlSchemaParse) then
    Result := _xmlSchemaParse(ctxt)
  else
    Result := nil;
end;

function xmlSchemaNewValidCtxt(schema: xmlSchemaPtr): xmlSchemaValidCtxtPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlSchemaNewValidCtxt) then
    Result := _xmlSchemaNewValidCtxt(schema)
  else
    Result := nil;
end;

function xmlSchemaValidateDoc(ctxt: xmlSchemaValidCtxtPtr; doc: xmlDocPtr
  ): Longint;
begin
  if InitLibXml2Interface and Assigned(_xmlSchemaValidateDoc) then
    Result := _xmlSchemaValidateDoc(ctxt, doc)
  else
    Result := -1;
end;

procedure xmlSchemaFreeParserCtxt(ctxt: xmlSchemaParserCtxtPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlSchemaFreeParserCtxt) then
    _xmlSchemaFreeParserCtxt(ctxt);
end;

procedure xmlSchemaFreeValidCtxt(ctxt: xmlSchemaValidCtxtPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlSchemaFreeValidCtxt) then
    _xmlSchemaFreeValidCtxt(ctxt);
end;

procedure xmlSchemaFree(schema: xmlSchemaPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlSchemaFree) then
    _xmlSchemaFree(schema);
end;

function xmlBufferCreate(): xmlBufferPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlBufferCreate) then
    Result := _xmlBufferCreate
  else
    Result := nil;
end;

function xmlSaveToBuffer(buffer: xmlBufferPtr; const encoding: PAnsiChar;
  options: Longint): xmlSaveCtxtPtr;
begin
  if InitLibXml2Interface and Assigned(_xmlSaveToBuffer) then
    Result := _xmlSaveToBuffer(buffer, encoding, options)
  else
    Result := nil;
end;

function xmlNodeDump(buf: xmlBufferPtr; doc: xmlDocPtr; cur: xmlNodePtr;
  level: Longint; format: Longint): Longint;
begin
  if InitLibXml2Interface and Assigned(_xmlNodeDump) then
    Result := _xmlNodeDump(buf, doc, cur, level, format)
  else
    Result := -1;
end;

procedure xmlBufferFree(buf: xmlBufferPtr);
begin
  if InitLibXml2Interface and Assigned(_xmlBufferFree) then
    _xmlBufferFree(buf);
end;


initialization
  LibXML2CS := TCriticalSection.Create;
  LibXml2LoadVerbose := False;
  LibXml2Loaded := False;
  LibXml2UnavailableFunctions := '';
  LibXml2Path := '';
  LibXml2Handle := 0;
  LibXml2File := '';

finalization
  DestroyLibXml2Interface;
  LibXML2CS.Free;

end.
