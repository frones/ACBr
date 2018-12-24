{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{ Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal }
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2015 Daniel Simoes de Almeida               }
{ André Ferreira de Moraes                                                     }

{ Colaboradores nesse arquivo:                                                 }

{ Você pode obter a última versão desse arquivo na pagina do Projeto ACBr      }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }

{ Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{ Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{ Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{ Praça Anita Costa, 34 - Tatuí - SP - 18270-410                               }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeXsLibXml2;

interface

uses
  Classes, SysUtils,
  ACBrDFeSSL, libxml2;

resourcestring
  cErrParseDoc = 'Erro: Falha ao interpretar o XML "xmlParseDoc"';
  cErrFindSignNode = 'Erro: Falha ao localizar o nó de Assinatura';
  cErrFindRootNode = 'Erro: Falha ao localizar o nó Raiz';
  cErrC14NTransformation = 'Erro ao aplicar transformação C14N';
  cErrSelecionarElements = 'Erro ao selecionar os elementos do XML.';
  cErrElementsNotFound = 'Nenhum elemento encontrado';
  cErrEX509CertificateNotFound = 'X509Certificate não encontrado';
  cErrDigestValueNode = 'Node DigestValue não encontrado';
  cErrSignatureValueNode = 'Node SignatureValue não encontrado';
  cErrDigestValueNaoConfere = 'DigestValue não confere. Conteúdo de "%s" foi alterado';
  cErrInvalidSchema = 'Erro: Schema inválido';
  cErrCreateSchemaContext = 'Erro: Não foi possivel criar um contexto para o Schema';
  cErrNotIdentfiedSchema = 'Erro indefinido, ao validar o Documento com o Schema';
  cErrSchemaValidationContext = 'Error: não foi possivel criar um contexto de validação para o Schema';

const
  cSignatureNode = 'Signature';
  cSignatureNameSpace = 'http://www.w3.org/2000/09/xmldsig#';
  cDigestValueNode = 'DigestValue';
  cSignedInfoNode = 'SignedInfo';
  cSignatureValueNode = 'SignatureValue';
  cX509CertificateNode = 'X509Certificate';

type

  { TDFeSSLXmlSignLibXml2 }

  TDFeSSLXmlSignLibXml2 = class(TDFeSSLXmlSignClass)
  private
  protected
    procedure VerificarValoresPadrao(var SignatureNode: String;
      var SelectionNamespaces: String); virtual;
    function SelectElements(const aDoc: xmlDocPtr; const infElement: String)
      : xmlNodeSetPtr;
    function CanonC14n(const aDoc: xmlDocPtr; const infElement: String): Ansistring;
    function LibXmlFindSignatureNode(aDoc: xmlDocPtr;
      var SignatureNode: String; var SelectionNamespaces: String;
      infElement: String): xmlNodePtr;
    function LibXmlLookUpNode(ParentNode: xmlNodePtr; NodeName: String;
      NameSpace: String = ''): xmlNodePtr;
    function LibXmlNodeWasFound(ANode: xmlNodePtr; NodeName: String;
      NameSpace: String): boolean;
    function AdicionarNode(var aDoc: xmlDocPtr; ConteudoXML: String): xmlNodePtr;
  public
    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''; IdAttr: String = ''): String; override;
    function Validar(const ConteudoXML, ArqSchema: String; out MsgErro: String)
      : boolean; override;
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''; IdSignature: String = '';
      IdAttr: String = ''): boolean; override;
  end;

procedure LibXmlInit();
procedure LibXmlShutDown();

implementation

uses
  synacode,
  ACBrUtil, ACBrDFeUtil, ACBrConsts, ACBrDFeException;

var
  LibXMLLoaded: boolean;

  { TDFeSSLXmlSignLibXml2 }

procedure LibXmlInit;
begin
  if (LibXMLLoaded) then
    Exit;

  // --Inicializar funções das units do libxml2
  libxml2.Init;

  { Init libxml and libxslt libraries }
  xmlInitThreads();
  xmlInitParser();
  xmlSubstituteEntitiesDefault(1);
  __xmlLoadExtDtdDefaultValue^ := XML_DETECT_IDS or XML_COMPLETE_ATTRS;
  __xmlIndentTreeOutput^ := 1;
  __xmlSaveNoEmptyTags^ := 1;

  LibXMLLoaded := True;
end;

procedure LibXmlShutDown();
begin
  if (not LibXMLLoaded) then
    Exit;

  { Shutdown libxslt/libxml }
  xmlCleanupParser();

  LibXMLLoaded := False;
end;

function TDFeSSLXmlSignLibXml2.Assinar(const ConteudoXML, docElement,
  infElement: String; SignatureNode: String; SelectionNamespaces: String;
  IdSignature: String; IdAttr: String): String;
var
  aDoc: xmlDocPtr;
  SignNode, XmlNode: xmlNodePtr;
  buffer: PAnsiChar;
  aXML, XmlAss, URI: String;
  Canon, DigestValue, Signaturevalue: AnsiString;
  TemDeclaracao: Boolean;
  XmlLength: Integer;
begin
  LibXmlInit;

  XmlAss := '';
  // Verificando se possui a Declaração do XML, se não possuir,
  // adiciona para libXml2 compreender o Encoding
  TemDeclaracao := XmlEhUTF8(ConteudoXML);
  if not TemDeclaracao then
    aXML := CUTF8DeclaracaoXML + RemoverDeclaracaoXML(ConteudoXML)
  else
    aXML := ConteudoXML;

  URI := ExtraiURI(aXML, IdAttr);

  // DEBUG
  //WriteToTXT('C:\TEMP\XmlOriginal.xml', aXML, False, False, True);

  aDoc := nil;
  buffer := nil;
  XmlLength := 0;
  try
    aDoc := xmlParseDoc(PAnsiChar(AnsiString(aXML)));
    if (aDoc = nil) then
      raise EACBrDFeException.Create(cErrParseDoc);

    SignNode := LibXmlFindSignatureNode(aDoc, SignatureNode, SelectionNamespaces, infElement);
    if (SignNode = Nil) then
      SignNode := AdicionarNode(aDoc, SignatureElement(URI, True, IdSignature, FpDFeSSL.SSLDgst));

    // DEBUG
    // WriteToTXT('C:\TEMP\XmlSign.xml', aXML, False, False, True);

    // Aplica a transformação c14n no node infElement/docElement
    if URI = '' then
      Canon := CanonC14n(aDoc, '')
    else
      Canon := CanonC14n(aDoc, infElement);

    // DEBUG
    //WriteToTXT('C:\TEMP\CanonDigest.xml', Canon, False, False, True);

    // gerar o hash
    DigestValue := FpDFeSSL.CalcHash(Canon, FpDFeSSL.SSLDgst, outBase64);

    XmlNode := LibXmlLookUpNode(SignNode, cDigestValueNode);
    if (XmlNode = nil) then
      raise EACBrDFeException.Create(cErrDigestValueNode);

    xmlNodeSetContent(XmlNode, PAnsiChar(DigestValue));

    // DEBUG
    // buffer := nil;
    // xmlDocDumpMemory(aDoc, @buffer, @XmlLength);
    //WriteToTXT('C:\TEMP\DigestXml.xml', buffer, False, False, True);

    // Aplica a transformação c14n o node SignedInfo
    Canon := CanonC14n(aDoc, cSignedInfoNode);

    // DEBUG
    //WriteToTXT('C:\TEMP\CanonGeracao.xml', Canon, False, False, True);

    // Assina o node SignedInfo já transformado
    Signaturevalue := FpDFeSSL.CalcHash(Canon, FpDFeSSL.SSLDgst, outBase64, True);

    XmlNode := LibXmlLookUpNode(SignNode, cSignatureValueNode);
    if (XmlNode = nil) then
      raise EACBrDFeException.Create(cErrSignatureValueNode);

    xmlNodeSetContent(XmlNode, PAnsiChar(Signaturevalue));

    XmlNode := LibXmlLookUpNode(SignNode, cX509CertificateNode);
    if (XmlNode = nil) then
      raise EACBrDFeException.Create(cErrEX509CertificateNotFound);

    xmlNodeSetContent(XmlNode, PAnsiChar(AnsiString(FpDFeSSL.DadosCertificado.DERBase64)));

    xmlDocDumpMemory(aDoc, @buffer, @XmlLength);
    XmlAss := String(buffer);

    // DEBUG
    //WriteToTXT('C:\TEMP\XmlSigned.xml', XmlAss, False, False, True);
  finally
    if (buffer <> nil) then
      xmlFree(buffer);

    if (aDoc <> nil) then
      xmlFreeDoc(aDoc);
  end;

  if not TemDeclaracao then
    XmlAss := RemoverDeclaracaoXML(XmlAss);

  // Removendo quebras de linha //
  XmlAss := ChangeLineBreak(XmlAss, '');

  // DEBUG
  //WriteToTXT('C:\TEMP\XmlSigned2.xml', XmlAss, False, False, True);

  Result := XmlAss;
end;

function TDFeSSLXmlSignLibXml2.CanonC14n(const aDoc: xmlDocPtr;
  const infElement: String): Ansistring;
var
  Elements: xmlNodeSetPtr;
  buffer: PAnsiChar;
begin
  Result := '';
  buffer := Nil;
  Elements := Nil;

  try
    // Estamos aplicando a versão 1.0 do c14n, mas existe a versão 1.1
    // Talvez seja necessario no futuro checar a versão do c14n
    // aplica a transformação C14N
    if (infElement <> '') then
    begin
      Elements := SelectElements(aDoc, infElement);
      if xmlC14NDocDumpMemory(aDoc, Elements, 0, nil, 0, @buffer) < 0 then
        raise EACBrDFeException.Create(cErrC14NTransformation);
    end
    else
    begin
      if xmlC14NDocDumpMemory(aDoc, nil, 0, nil, 0, @buffer) < 0 then
        raise EACBrDFeException.Create(cErrC14NTransformation);
    end;

    if buffer = Nil then
      raise EACBrDFeException.Create(cErrC14NTransformation);

    Result := Ansistring(buffer);
    // DEBUG
    // WriteToTXT('C:\TEMP\CanonC14n.xml', Result, False, False);
  finally
    if (Elements <> Nil) then
      xmlXPathFreeNodeSet(Elements);

    if (buffer <> Nil) then
      xmlFree(buffer);
  end;
end;

function TDFeSSLXmlSignLibXml2.SelectElements(const aDoc: xmlDocPtr;
  const infElement: String): xmlNodeSetPtr;
var
  xpathCtx: xmlXPathContextPtr;
  xpathExpr: AnsiString;
  xpathObj: xmlXPathObjectPtr;
begin
  // Cria o contexto XPath
  xpathCtx := xmlXPathNewContext(aDoc);
  try
    if (xpathCtx = nil) then
      raise EACBrDFeException.Create('Erro ao obter o contexto do XPath');

    // expressão para selecionar os elementos n: e o namespace selecionado
    xpathExpr := '(//.|//@*|//namespace::*)[ancestor-or-self::*[local-name()='''
      + Copy(infElement, pos(':', infElement) + 1, Length(infElement)) + ''']]';

    // seleciona os elementos baseados na expressão(retorna um objeto XPath com os elementos)
    xpathObj := xmlXPathEvalExpression(PAnsiChar(xpathExpr), xpathCtx);

    if (xpathObj = nil) then
      raise EACBrDFeException.Create(cErrSelecionarElements);

    if (xpathObj.nodesetval.nodeNr > 0) then
      Result := xpathObj.nodesetval
    else
      raise EACBrDFeException.Create(cErrElementsNotFound);

  finally
    if (xpathCtx <> nil) then
      xmlXPathFreeContext(xpathCtx);
  end;
end;

function TDFeSSLXmlSignLibXml2.Validar(const ConteudoXML, ArqSchema: String;
  out MsgErro: String): boolean;
var
  doc: xmlDocPtr;
  parser_ctxt: xmlSchemaParserCtxtPtr;
  schema: xmlSchemaPtr;
  valid_ctxt: xmlSchemaValidCtxtPtr;
  schemError: xmlErrorPtr;
begin
  LibXmlInit;

  Result := False;
  MsgErro := '';

  doc := nil;
  parser_ctxt := nil;
  schema := nil;
  valid_ctxt := nil;

  try
    doc := xmlParseDoc(PAnsiChar(AnsiString(ConteudoXML)));
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
    begin
      MsgErro := ACBrStr(cErrParseDoc);
      Exit;
    end;

    parser_ctxt := xmlSchemaNewParserCtxt(PAnsiChar(ansistring(ArqSchema)));
    // unable to create a parser context for the schema */
    if (parser_ctxt = nil) then
    begin
      MsgErro := cErrSchemaValidationContext;
      Exit;
    end;

    schema := xmlSchemaParse(parser_ctxt);
    // the schema itself is not valid
    if (schema = nil) then
    begin
      MsgErro := cErrInvalidSchema;
      Exit;
    end;

    valid_ctxt := xmlSchemaNewValidCtxt(schema);
    // unable to create a validation context for the schema */
    if (valid_ctxt = nil) then
    begin
      MsgErro := cErrCreateSchemaContext;
      Exit;
    end;

    if (xmlSchemaValidateDoc(valid_ctxt, doc) <> 0) then
    begin
      schemError := xmlGetLastError();
      if (schemError <> nil) then
        MsgErro := IntToStr(schemError^.code) + ' - ' + schemError^.message
      else
        MsgErro := cErrNotIdentfiedSchema;
    end
    else
      Result := True;

  finally
    { cleanup }
    if (doc <> nil) then
      xmlFreeDoc(doc);

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
  SelectionNamespaces: String; IdSignature: String; IdAttr: String): boolean;
var
  aDoc: xmlDocPtr;
  SignElement, URI: String;
  DigestXML,  XmlSign, X509Certificate, CanonXML: AnsiString;
  signBuffer: xmlBufferPtr;
  DigestAlg: TSSLDgst;
  rootNode, SignNode: xmlNodePtr;
begin
  LibXmlInit;
  Result := False;
  signBuffer := nil;
  aDoc := nil;

  try
    aDoc := xmlParseDoc(PAnsiChar(AnsiString(ConteudoXML)));
    if (aDoc = nil) then
    begin
       MsgErro := ACBrStr(cErrParseDoc);
       Exit;
    end;

    rootNode := xmlDocGetRootElement(aDoc);
    if (rootNode = nil) then
    begin
       MsgErro := ACBrStr(cErrFindRootNode);
       Exit;
    end;

    SignNode := LibXmlFindSignatureNode(aDoc, SignatureNode, SelectionNamespaces, infElement);
    if (SignNode = nil) or (SignNode.Name <> SignatureNode) then
    begin
       MsgErro := ACBrStr(cErrFindSignNode);
       Exit;
    end;

    signBuffer := xmlBufferCreate();
    xmlNodeDump(signBuffer, aDoc, SignNode, 0, 0);
    SignElement := String(signBuffer.content);

    URI := RetornarConteudoEntre(SignElement, '<Reference URI="', '">');
    DigestXML := DecodeBase64(AnsiString(LerTagXML(SignElement, cDigestValueNode)));
    DigestAlg := GetSignDigestAlgorithm(SignElement);

    // Estamos aplicando a versão 1.0 do c14n, mas existe a versão 1.1
    // Talvez seja necessario no futuro checar a versão do c14n
    // para poder validar corretamente
    // Caso a URI Seja vazia precisa calcular o hash para o documento todo sem a tag assinatura.
    // Recalculando o DigestValue do XML e comparando com o atual
    if URI = '' then
    begin
      xmlUnlinkNode(SignNode);
      SignNode.children := nil;
      SignNode.last := nil;
      xmlFreeNode(SignNode);
      CanonXML := AnsiString(CanonC14n(aDoc, rootNode.name));
      AdicionarNode(aDoc, SignElement);
    end
    else
      CanonXML := AnsiString(CanonC14n(aDoc, infElement));

    if(not FpDFeSSL.ValidarHash(CanonXML, DigestAlg, DigestXML)) then
    begin
       MsgErro := Format(ACBrStr(cErrDigestValueNaoConfere), [infElement]);
       Exit;
    end;

    X509Certificate := AnsiString(LerTagXML(SignElement, cX509CertificateNode));
    FpDFeSSL.CarregarCertificadoPublico(X509Certificate);

    CanonXML := AnsiString(CanonC14n(aDoc, cSignedInfoNode));

    XmlSign := DecodeBase64(AnsiString(LerTagXML(SignElement, cSignatureValueNode)) );
    Result := FpDFeSSL.ValidarHash(CanonXML, DigestAlg, XmlSign, True);
  finally
    { cleanup }
    if (aDoc <> nil) then
      xmlFreeDoc(aDoc);

    if (signBuffer <> nil) then
      xmlBufferFree(signBuffer);

    // Descarrega o Certificado Publico //
    FpDFeSSL.DescarregarCertificado;
  end;
end;

procedure TDFeSSLXmlSignLibXml2.VerificarValoresPadrao(var SignatureNode
  : String; var SelectionNamespaces: String);
begin
  if (SignatureNode <> cSignatureNode) then
    SignatureNode := cSignatureNode;

  if (SelectionNamespaces <> cSignatureNameSpace) then
    SelectionNamespaces := cSignatureNameSpace;
end;

function TDFeSSLXmlSignLibXml2.LibXmlFindSignatureNode(aDoc: xmlDocPtr;
  var SignatureNode: String; var SelectionNamespaces: String; infElement: String
  ): xmlNodePtr;
var
  rootNode, infNode, SignNode: xmlNodePtr;
begin
  Result := nil;

  { Encontra o elemento Raiz }
  rootNode := xmlDocGetRootElement(aDoc);
  if (rootNode = nil) then
    Exit;

  VerificarValoresPadrao(SignatureNode, SelectionNamespaces);

  { Se infElement possui prefixo o mesmo tem que ser removido }
  if Pos(':', infElement) > 0 then
    infElement := copy(infElement, Pos(':', infElement) + 1, Length(infElement));

  { Se tem InfElement, procura pelo mesmo. Isso permitirá acharmos o nó de
    assinatura, relacionado a ele (mesmo pai) }
  if (infElement <> '') then
  begin
    { Procura InfElement em todos os nós, filhos de Raiz, usando LibXml }
    infNode := LibXmlLookUpNode(rootNode, infElement);

    { Não achei o InfElement em nenhum nó :( }
    if (infNode = nil) then
      Exit;

    { Vamos agora, achar o pai desse Elemento, pois com ele encontraremos a assinatura }
    if (infNode^.Name = infElement) and
       Assigned(infNode^.parent) and
       (infNode^.parent^.Name <> '') then
    begin
      infNode := infNode^.parent;
    end;
  end
  else
  begin
    { InfElement não foi informado... vamos usar o nó raiz, para pesquisar pela assinatura }
    infNode := rootNode;
  end;

  if (infNode = nil) then
    Exit;

  { Procurando pelo nó de assinatura...
    Primeiro vamos verificar manualmente se é o último no do nosso infNode atual };
  SignNode := infNode^.last;
  if not LibXmlNodeWasFound(SignNode, SignatureNode, SelectionNamespaces) then
  begin
    { Não é o ultimo nó do infNode... então, vamos procurar por um Nó dentro de infNode }
    SignNode := infNode^.next;
    while (SignNode <> nil)  and
      (not LibXmlNodeWasFound(SignNode, SignatureNode, SelectionNamespaces)) do
    begin
      SignNode := SignNode^.next;
    end;

    { Se ainda não achamos, vamos procurar novamente a partir do elemento Raiz }
    if (SignNode = nil) then
    begin
      SignNode := rootNode^.last;
      if not LibXmlNodeWasFound(SignNode, SignatureNode, SelectionNamespaces) then
      begin
        SignNode := rootNode^.next;
        while (SignNode <> nil)  and (not LibXmlNodeWasFound(SignNode, SignatureNode, SelectionNamespaces)) do
        begin
          SignNode := SignNode^.next;
        end;
      end;
    end;
  end;

  Result := SignNode;
end;

function TDFeSSLXmlSignLibXml2.LibXmlNodeWasFound(ANode: xmlNodePtr;
  NodeName: String; NameSpace: String): boolean;
begin
  Result := (ANode <> nil) and (ANode^.Name = NodeName) and
    ((NameSpace = '') or (ANode^.ns^.href = NameSpace));
end;

function TDFeSSLXmlSignLibXml2.LibXmlLookUpNode(ParentNode: xmlNodePtr;
  NodeName: String; NameSpace: String): xmlNodePtr;

  function _LibXmlLookUpNode(ParentNode: xmlNodePtr; NodeName: String;
    NameSpace: String): xmlNodePtr;
  var
    NextNode, ChildNode, FoundNode: xmlNodePtr;
  begin
    Result := ParentNode;
    if (ParentNode = nil) then
      Exit;

    FoundNode := ParentNode;

    while (FoundNode <> nil) and
      (not LibXmlNodeWasFound(FoundNode, NodeName, NameSpace)) do
    begin
      ChildNode := FoundNode^.children;
      NextNode := FoundNode^.Next;

      { Faz Chamada recursiva para o novo Filho }
      FoundNode := _LibXmlLookUpNode(ChildNode, NodeName, NameSpace);

      if FoundNode = nil then
        FoundNode := NextNode;
    end;

    Result := FoundNode;
  end;

begin
  Result := ParentNode;
  if (ParentNode = nil) or (Trim(NodeName) = '') then
    Exit;

  { Primeiro vamos ver se o nó Raiz já não é o que precisamos }
  if LibXmlNodeWasFound(ParentNode, NodeName, NameSpace) then
    Exit;

  { Chama função auxiliar, que usa busca recursiva em todos os nós filhos }
  Result := _LibXmlLookUpNode(ParentNode^.children, NodeName, NameSpace);
end;

function TDFeSSLXmlSignLibXml2.AdicionarNode(var aDoc: xmlDocPtr; ConteudoXML: String): xmlNodePtr;
Var
  NewNode: xmlNodePtr;
  memDoc: xmlDocPtr;
  NewNodeXml: String;
begin
{$IFNDEF COMPILER23_UP}
  Result := nil;
{$ENDIF}
  NewNode := nil;
  memDoc := nil;
  try
    NewNodeXml := '<a>' + ConteudoXML + '</a>';
    memDoc := xmlReadMemory(PAnsiChar(AnsiString(NewNodeXml)), Length(NewNodeXml), nil, nil, 0);
    NewNode := xmlDocCopyNode(xmlDocGetRootElement(memDoc), aDoc.doc, 1);
    Result := xmlAddChildList(xmlDocGetRootElement(aDoc), NewNode.children);
  finally
    if NewNode <> nil then
    begin
      NewNode.children := nil;
      NewNode.last := nil;
      xmlFreeNode(NewNode);
    end;

    if (memDoc <> nil) then
      xmlFreeDoc(memDoc);
  end;
end;

initialization

LibXMLLoaded := False;

finalization

LibXmlShutDown;

end.
