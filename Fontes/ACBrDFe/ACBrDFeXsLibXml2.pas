{ ****************************************************************************** }
{ Projeto: Componente ACBrNFe }
{ Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal }
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br }
{ Direitos Autorais Reservados (c) 2015 Daniel Simoes de Almeida }
{ André Ferreira de Moraes }
{ Colaboradores nesse arquivo: }
{ Você pode obter a última versão desse arquivo na pagina do Projeto ACBr }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr }
{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior. }
{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT) }
{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc., }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA. }
{ Você também pode obter uma copia da licença em: }
{ http://www.opensource.org/licenses/lgpl-license.php }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br }
{ Praça Anita Costa, 34 - Tatuí - SP - 18270-410 }
{ ****************************************************************************** }

{$I ACBr.inc}
unit ACBrDFeXsLibXml2;

interface

uses
  Classes, SysUtils,
  ACBrDFeSSL, libxml2;

const
  cErrParseDoc = 'Erro: Falha ao interpretar o XML "xmlParseDoc"';
  cErrFindSignNode = 'Erro: Falha ao localizar o nó de Assinatura';
  cErrFindRootNode = 'Erro: Falha ao localizar o nó Raiz';

type

  { TDFeSSLXmlSignLibXml2 }

  TDFeSSLXmlSignLibXml2 = class(TDFeSSLXmlSignClass)
  private
    function SelectElements(const aDoc: xmlDoc; const infElement: AnsiString)
      : xmlNodeSetPtr;
    function CanonC14n(const aXML, docElement, infElement: AnsiString)
      : AnsiString;
    function LibXmlFindSignatureNode(aDoc: xmlDocPtr;
      SignatureNode, SelectionNamespaces, infElement: AnsiString): xmlNodePtr;
    function LibXmlLookUpNode(ParentNode: xmlNodePtr; NodeName: AnsiString;
      NameSpace: AnsiString = ''): xmlNodePtr;
    function LibXmlNodeWasFound(ANode: xmlNodePtr; NodeName: AnsiString;
      NameSpace: AnsiString): Boolean;
  protected
    procedure VerificarValoresPadrao(var SignatureNode: AnsiString;
      var SelectionNamespaces: AnsiString);
  public
    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''; IdAttr: String = ''): String; override;
    function Validar(const ConteudoXML, ArqSchema: String; out MsgErro: String)
      : Boolean; override;
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''; IdSignature: String = '';
      IdAttr: String = ''): Boolean; override;
  end;

implementation

uses
  StrUtils,
  synacode,
  ACBrUtil, ACBrDFeUtil, ACBrConsts, ACBrDFeException;

{ TDFeSSLXmlSignLibXml2 }

function TDFeSSLXmlSignLibXml2.Assinar(const ConteudoXML, docElement,
  infElement: String; SignatureNode: String = '';
  SelectionNamespaces: String = ''; IdSignature: String = '';
  IdAttr: String = ''): String;
var
  aDoc: xmlDocPtr;
  XmlNode: xmlNodePtr;
  buffer: PAnsiChar;
  aXML, XmlAss: String;
  TemDeclaracao: Boolean;
  XmlLength: integer;
  Canon, DigestValue, Signaturevalue: AnsiString;
begin

  { Init libxml and libxslt libraries }
  __xmlSaveNoEmptyTags^ := 1;

  XmlAss := '';
  // Verificando se possui a Declaração do XML, se não possuir,
  // adiciona para libXml2 compreender o Encoding
  TemDeclaracao := XmlEhUTF8(ConteudoXML);
  if not TemDeclaracao then
    aXML := CUTF8DeclaracaoXML + RemoverDeclaracaoXML(ConteudoXML)
  else
    aXML := ConteudoXML;

  // Inserindo Template da Assinatura digital
  if (not XmlEstaAssinado(aXML)) or (SignatureNode <> '') then
    aXML := AdicionarSignatureElement(aXML, False, docElement,
      IdSignature, IdAttr);

  // DEBUG
  //WriteToTXT('C:\TEMP\XmlSign.xml', AXml, False, False);

  // Aplica a transformação c14n no node infElement
  Canon := CanonC14n(aXML, docElement, infElement);

  // DEBUG
  //WriteToTXT('C:\TEMP\CanonDigest.xml', Canon, False, False);

  // gerar o hash
  DigestValue := FpDFeSSL.CalcHash(Canon, FpDFeSSL.SSLDgst, outBase64);

  aDoc := nil;
  XmlNode := nil;

  try
    aDoc := xmlParseDoc(PAnsiChar(AnsiString(aXML)));
    if (aDoc = nil) then
      raise EACBrDFeException.Create(ACBrStr(cErrParseDoc));

    XmlNode := LibXmlFindSignatureNode(aDoc, SignatureNode, SelectionNamespaces,
      infElement);

    XmlNode := LibXmlLookUpNode(XmlNode, 'DigestValue');
    if (XmlNode = nil) then
      raise EACBrDFeException.Create(ACBrStr('Node DigestValue não encontrado!'));

    xmlNodeSetContent(XmlNode, PAnsiChar(DigestValue));

    buffer := nil;
    xmlDocDumpMemory(ADoc, @buffer, @XmlLength);
    aXML := buffer;

    // DEBUG
    //WriteToTXT('C:\TEMP\DigestXml.xml', DigestXml, False, False);
  finally
    xmlFreeDoc(aDoc);
  end;

  // Aplica a transformação c14n o node SignedInfo
  Canon := CanonC14n(aXML, docElement, 'SignedInfo');

  // DEBUG
  // WriteToTXT('C:\TEMP\CanonGeracao.xml', Canon, False, False);

  // Assina o node SignedInfo já transformado
  Signaturevalue := FpDFeSSL.CalcHash(Canon, FpDFeSSL.SSLDgst, outBase64, True);

  aDoc := nil;
  XmlNode := nil;

  try
    aDoc := xmlParseDoc(PAnsiChar(AnsiString(aXML)));
    if (aDoc = nil) then
      raise EACBrDFeException.Create(ACBrStr(cErrParseDoc));

    XmlNode := LibXmlFindSignatureNode(aDoc, SignatureNode, SelectionNamespaces,
      infElement);

    XmlNode := LibXmlLookUpNode(XmlNode, 'SignatureValue');
    if (XmlNode = nil) then
      raise EACBrDFeException.Create(ACBrStr('Node SignatureValue não encontrado!'));

    xmlNodeSetContent(XmlNode, PAnsiChar(Signaturevalue));

    buffer := nil;
    xmlDocDumpMemory(ADoc, @buffer, @XmlLength);
    XmlAss := buffer;

    // DEBUG
    //WriteToTXT('C:\TEMP\XmlSigned.xml', XmlAss, False, False);
  finally
    xmlFreeDoc(aDoc);
  end;

  if not TemDeclaracao then
    XmlAss := RemoverDeclaracaoXML(XmlAss);

  // ajusta o xml e adiciona os dados do certificado
  XmlAss := AjustarXMLAssinado(XmlAss, FpDFeSSL.DadosCertificado.DERBase64);

  // DEBUG
  WriteToTXT('C:\TEMP\XmlSigned2.xml', XmlAss, False, False);

  Result := XmlAss;
end;

function TDFeSSLXmlSignLibXml2.CanonC14n(const aXML, docElement,
  infElement: AnsiString): AnsiString;
var
  doc: xmlDocPtr;
  Elements: xmlNodeSetPtr;
  buffer: PAnsiChar;
  inclusive: xmlCharPtrPtr;
begin
  // carrega o xml
  doc := xmlParseDoc(PAnsiChar(aXML));
  if (doc = nil) then
    raise EACBrDFeException.Create(ACBrStr(cErrParseDoc));

  try
    // seleciona os elementos a serem transformados e inclui os devidos namespaces
    Elements := SelectElements(doc^, infElement);
    try
      // aplica a transformação C14N
      buffer := nil;
      inclusive := nil;
      if xmlC14NDocDumpMemory(doc, Elements, 0, inclusive, 0, @buffer) < 0 then
        raise EACBrDFeException.Create(ACBrStr('Erro ao aplicar transformação C14N!'));

      if buffer = nil then
        raise EACBrDFeException.Create(ACBrStr('Erro ao aplicar transformação C14N!'));
    finally
      xmlXPathFreeNodeSet(Elements);
    end;

    Result := AnsiString(buffer);
  finally
    xmlFreeDoc(doc);
  end;
end;

function TDFeSSLXmlSignLibXml2.SelectElements(const aDoc: xmlDoc;
  const infElement: AnsiString): xmlNodeSetPtr;
var
  xpathCtx: xmlXPathContextPtr;
  xpathExpr: AnsiString;
  xpathObj: xmlXPathObjectPtr;
begin
  // Cria o contexdo o XPath
  xpathCtx := xmlXPathNewContext(@aDoc);
  try
    if (xpathCtx = nil) then
      raise EACBrDFeException.Create(ACBrStr('Erro ao obter o contexto do XPath'));

    // express?o para selecionar os elementos n: ? o namespace selecionado
    xpathExpr := '(//.|//@*|//namespace::*)[ancestor-or-self::*[local-name()='''
      + infElement + ''']]';

    // seleciona os elementos baseados na express?o(retorna um objeto XPath com os elementos)
    xpathObj := xmlXPathEvalExpression(PAnsiChar(xpathExpr), xpathCtx);

    if (xpathObj = nil) then
      raise EACBrDFeException.Create(ACBrStr('Erro ao selecionar os elementos do XML.'));

    if (xpathObj.nodesetval.nodeNr > 0) then
      Result := xpathObj.nodesetval
    else
      raise EACBrDFeException.Create(ACBrStr('Nenhum elemento encontrado.'));

  finally
    xmlXPathFreeContext(xpathCtx);
  end;
end;

function TDFeSSLXmlSignLibXml2.Validar(const ConteudoXML, ArqSchema: String;
  out MsgErro: String): Boolean;
var
  doc, schema_doc: xmlDocPtr;
  parser_ctxt: xmlSchemaParserCtxtPtr;
  schema: xmlSchemaPtr;
  valid_ctxt: xmlSchemaValidCtxtPtr;
  schemError: xmlErrorPtr;
  aXML: AnsiString;
begin
  Result := False;
  doc := nil;
  schema_doc := nil;
  parser_ctxt := nil;
  schema := nil;
  valid_ctxt := nil;

  try
    aXML := AnsiString(ConteudoXML);
    doc := xmlParseDoc(PAnsiChar(aXML));
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
    begin
      MsgErro := cErrParseDoc;
      exit;
    end;

    schema_doc := xmlReadFile(PAnsiChar(AnsiString(ArqSchema)), nil,
      XML_DETECT_IDS);
    // the schema cannot be loaded or is not well-formed
    if (schema_doc = nil) then
    begin
      MsgErro := 'Erro: schema não pode ser carregado ou está corrompido';
      exit;
    end;

    parser_ctxt := xmlSchemaNewDocParserCtxt(schema_doc);
    // unable to create a parser context for the schema */
    if (parser_ctxt = nil) then
    begin
      MsgErro := 'Erro: não foi possivel criar um contexto para o schema';
      exit;
    end;

    schema := xmlSchemaParse(parser_ctxt);
    // the schema itself is not valid
    if (schema = nil) then
    begin
      MsgErro := 'Erro: schema inválido';
      exit;
    end;

    valid_ctxt := xmlSchemaNewValidCtxt(schema);
    // unable to create a validation context for the schema */
    if (valid_ctxt = nil) then
    begin
      MsgErro :=
        'Error: não foi possivel criar um contexto de validação para o schema';
      exit;
    end;

    if (xmlSchemaValidateDoc(valid_ctxt, doc) <> 0) then
    begin
      schemError := xmlGetLastError();
      if (schemError <> nil) then
        MsgErro := IntToStr(schemError^.code) + ' - ' + schemError^.message;
    end
    else
      Result := True;

  finally
    { cleanup }
    if (doc <> nil) then
      xmlFreeDoc(doc);

    if (schema_doc <> nil) then
      xmlFreeDoc(schema_doc);

    if (parser_ctxt <> nil) then
      xmlSchemaFreeParserCtxt(parser_ctxt);

    if (valid_ctxt <> nil) then
      xmlSchemaFreeValidCtxt(valid_ctxt);

    if (schema <> nil) then
      xmlSchemaFree(schema);
  end;
end;

function TDFeSSLXmlSignLibXml2.VerificarAssinatura(const ConteudoXML: String;
  out MsgErro: String; const infElement: String; SignatureNode: String;
  SelectionNamespaces: String; IdSignature: String; IdAttr: String): Boolean;
var
  aDoc: xmlDocPtr;
  X509Certificate: String;
  signBuffer: xmlBufferPtr;
  SignElement, XmlSign, docElement, aXML: AnsiString;
  Digest: TSSLDgst;
  rootNode, SignNode: xmlNodePtr;
begin

  aDoc := nil;

  try
    aDoc := xmlParseDoc(PAnsiChar(AnsiString(ConteudoXML)));
    if (aDoc = nil) then
      raise EACBrDFeException.Create(ACBrStr(cErrParseDoc));

    rootNode := xmlDocGetRootElement(aDoc);
    if (rootNode = nil) then
      raise EACBrDFeException.Create(ACBrStr(cErrFindRootNode));

    SignNode := LibXmlFindSignatureNode(aDoc, SignatureNode,
      SelectionNamespaces, infElement);

    if (SignNode.name <> 'Signature') then
      raise EACBrDFeException.Create(ACBrStr(cErrFindSignNode));

    signBuffer := xmlBufferCreate();
    xmlNodeDump(signBuffer, aDoc, SignNode, 0, 0);

    SignElement := signBuffer.content;
    docElement := rootNode.name;
  finally
    xmlBufferFree(signBuffer);
    xmlFreeDoc(aDoc);
  end;

  Digest := GetSignDigestAlgorithm(SignElement);
  XmlSign := DecodeBase64(LerTagXML(SignElement, 'SignatureValue'));
  X509Certificate := LerTagXML(SignElement, 'X509Certificate');
  FpDFeSSL.CarregarCertificadoPublico(X509Certificate);

  aXML := CanonC14n(ConteudoXML, docElement, 'SignedInfo');

  Result := FpDFeSSL.ValidarHash(aXML, Digest, XmlSign, True);

  // Descarrega o Certificado Publico //
  FpDFeSSL.DescarregarCertificado;
end;

procedure TDFeSSLXmlSignLibXml2.VerificarValoresPadrao(var SignatureNode
  : AnsiString; var SelectionNamespaces: AnsiString);
var
  DSigNs, DSign: AnsiString;
begin
  if SignatureNode = '' then
    SignatureNode := 'Signature'
  else
    SignatureNode := copy(SignatureNode, Pos(':', SignatureNode) + 1,
      Length(SignatureNode));

  DSigNs := 'http://www.w3.org/2000/09/xmldsig#';

  if SelectionNamespaces = '' then
    SelectionNamespaces := DSigNs
  else
  begin
    SelectionNamespaces := RetornarConteudoEntre(SelectionNamespaces, '"', '"');

    if StrUtils.LeftStr(SelectionNamespaces, Length(DSigNs)) <> DSigNs then
      SelectionNamespaces := DSigNs + ' ' + SelectionNamespaces;
  end;
end;

function TDFeSSLXmlSignLibXml2.LibXmlFindSignatureNode(aDoc: xmlDocPtr;
  SignatureNode, SelectionNamespaces, infElement: AnsiString): xmlNodePtr;
var
  rootNode, infNode, SignNode: xmlNodePtr;
begin
  { Encontra o elemento Raiz }
  rootNode := xmlDocGetRootElement(aDoc);
  if (rootNode = nil) then
    raise EACBrDFeException.Create(cErrFindRootNode);

  VerificarValoresPadrao(SignatureNode, SelectionNamespaces);

  { Se infElement possui prefixo o mesmo tem que ser removido }
  if Pos(':', infElement) > 0 then
    infElement := copy(infElement, Pos(':', infElement) + 1,
      Length(infElement));

  { Se tem InfElement, procura pelo mesmo. Isso permitirá acharmos o nó de
    assinatura, relacionado a ele (mesmo pai) }
  if (infElement <> '') then
  begin
    { Procura InfElement em todos os nós, filhos de Raiz, usando LibXml }
    infNode := LibXmlLookUpNode(rootNode, infElement);

    { Não achei o InfElement em nenhum nó :( }
    if (infNode = nil) then
      raise EACBrDFeException.Create(cErrFindRootNode);

    { Vamos agora, achar o pai desse Elemento, pois com ele encontraremos a assinatura }
    if (infNode^.name = infElement) and Assigned(infNode^.parent) and
      (infNode^.parent^.name <> '') then
      infNode := infNode^.parent;
  end
  else
  begin
    { InfElement não foi informado... vamos usar o nó raiz, para pesquisar pela assinatura }
    infNode := rootNode;
  end;

  if (infNode = nil) then
    raise EACBrDFeException.Create(cErrFindRootNode);

  { Procurando pelo nó de assinatura...
    Primeiro vamos verificar manualmente se é o último no do nosso infNode atual };
  SignNode := infNode^.last;
  if not LibXmlNodeWasFound(SignNode, SignatureNode, SelectionNamespaces) then
  begin
    { Não é o ultimo nó do infNode... então, vamos procurar por um Nó dentro de infNode }
    SignNode := LibXmlLookUpNode(infNode, SignatureNode, SelectionNamespaces);

    { Se ainda não achamos, vamos procurar novamente a partir do elemento Raiz }
    if (SignNode = nil) then
    begin
      SignNode := rootNode^.last;
      if not LibXmlNodeWasFound(SignNode, SignatureNode, SelectionNamespaces)
      then
        SignNode := LibXmlLookUpNode(rootNode, SignatureNode,
          SelectionNamespaces);
    end;
  end;

  if (SignNode = nil) then
    raise EACBrDFeException.Create(cErrFindSignNode);

  Result := SignNode;
end;

function TDFeSSLXmlSignLibXml2.LibXmlNodeWasFound(ANode: xmlNodePtr;
  NodeName: AnsiString; NameSpace: AnsiString): Boolean;
begin
  Result := (ANode <> nil) and (ANode^.name = NodeName) and
    ((NameSpace = '') or (ANode^.ns^.href = NameSpace));
end;

function TDFeSSLXmlSignLibXml2.LibXmlLookUpNode(ParentNode: xmlNodePtr;
  NodeName: AnsiString; NameSpace: AnsiString): xmlNodePtr;

  function _LibXmlLookUpNode(ParentNode: xmlNodePtr; NodeName: AnsiString;
    NameSpace: AnsiString): xmlNodePtr;
  var
    NextNode, ChildNode, FoundNode: xmlNodePtr;
  begin
    Result := ParentNode;
    if (ParentNode = Nil) then
      exit;

    FoundNode := ParentNode;

    while (FoundNode <> Nil) and
      (not LibXmlNodeWasFound(FoundNode, NodeName, NameSpace)) do
    begin
      ChildNode := FoundNode^.children;
      NextNode := FoundNode^.next;
      { Faz Chamada recursiva para o novo Filho }
      FoundNode := _LibXmlLookUpNode(ChildNode, NodeName, NameSpace);

      if FoundNode = Nil then
        FoundNode := NextNode;
    end;

    Result := FoundNode;
  end;

begin
  Result := ParentNode;
  if (ParentNode = Nil) or (Trim(NodeName) = '') then
    exit;

  { Primeiro vamos ver se o nó Raiz já não é o que precisamos }
  if LibXmlNodeWasFound(ParentNode, NodeName, NameSpace) then
    exit;

  { Chama função auxiliar, que usa busca recursiva em todos os nós filhos }
  Result := _LibXmlLookUpNode(ParentNode^.children, NodeName, NameSpace);
end;

end.
