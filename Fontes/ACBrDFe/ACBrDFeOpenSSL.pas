{******************************************************************************}
{ Projeto: Componente ACBrNFe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{ eletrônica - NFe - http://www.nfe.fazenda.gov.br                             }

{ Direitos Autorais Reservados (c) 2015 Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }


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

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeOpenSSL;

interface

uses
  {$IFDEF DELPHIXE4_UP}
   AnsiStrings,
  {$ENDIF}
  Classes, SysUtils,
  ACBrDFeSSL,
  HTTPSend, ssl_openssl,
  libxmlsec, libxslt, libxml2,
  {$IFDEF USE_libeay32}libeay32{$ELSE} OpenSSLExt{$ENDIF}  ;

const
  cDTD = '<!DOCTYPE test [<!ATTLIST &infElement& Id ID #IMPLIED>]>';

  cErrMngrCreate = 'Erro: Falha ao criar Gerenciador de Chaves "xmlSecKeysMngrCreate"';
  cErrMngrInit = 'Erro: Falha ao inicializar o Gerenciador de Chaves "xmlSecCryptoAppDefaultKeysMngrInit"';
  cErrCertLoad = 'Erro: Falha ao ler informação do Certificado no Gerenciador de Chaves';
  cErrParseDoc = 'Erro: Falha ao interpretar o XML "xmlParseDoc"';
  cErrFindSignNode = 'Erro: Falha ao localizar o nó de Assinatura';
  cErrFindRootNode = 'Erro: Falha ao localizar o nó Raiz';
  cErrCtxCreate = 'Erro: Falha ao criar Ctx "xmlSecDSigCtxCreate"';
  cErrPrivKeyLoad = 'Erro: Falha ao ler a Chave Privada de DadosPFX';
  cErrPubKeyLoad = 'Erro: Falha ao ler a Chave Publica do Certificado';
  cErrDSigSign = 'Erro %d: Falha ao assinar o Documento';
  cErrDSigVerify = 'Erro: Falha na verificação da Assinatura';
  cErrDSigInvalid = 'Erro: Assinatura é inválida';


type
  { TDFeOpenSSL }

  TDFeOpenSSL = class(TDFeSSLClass)
  private
    FHTTP: THTTPSend;
    FdsigCtx: xmlSecDSigCtxPtr;
    FCNPJ: String;
    FRazaoSocial: String;
    FCertificadora: String;
    FPrivKey: pEVP_PKEY;
    FNumSerie: String;
    FCertAsDER: String;
    FValidade: TDateTime;
    FSubjectName: String;
    FIssuerName: String;

    procedure Clear;
    procedure ConfiguraHTTP(const URL, SoapAction: String; MimeType: String);
    function InserirDTD(AXml: String; const DTD: String): String;
    function LerPFXInfo(pfxdata: Ansistring): Boolean;

    procedure VerificarValoresPadrao(var SignatureNode: AnsiString;
      var SelectionNamespaces: AnsiString);
    function XmlSecSign(const ConteudoXML: AnsiString;
      SignatureNode, SelectionNamespaces, InfElement: AnsiString): AnsiString;
    function XmlSecNodeWasFound(ANode: xmlNodePtr; NodeName: AnsiString;
      NameSpace: AnsiString): Boolean;
    function XmlSecFindSignatureNode( ADoc: xmlDocPtr; SignatureNode,
      SelectionNamespaces, InfElement: AnsiString): xmlNodePtr;
    function XmlSecLookUpNode(ParentNode: xmlNodePtr; NodeName: AnsiString;
      NameSpace: AnsiString = ''): xmlNodePtr;
    function _XmlSecLookUpNode(ParentNode: xmlNodePtr; NodeName: AnsiString;
      NameSpace: AnsiString = ''): xmlNodePtr;

    procedure CreateCtx;
    procedure DestroyCtx;
    procedure DestroyKey;
  protected

    function GetCertDataVenc: TDateTime; override;
    function GetCertNumeroSerie: String; override;
    function GetCertSubjectName: String; override;
    function GetCertRazaoSocial: String; override;
    function GetCertCNPJ: String; override;
    function GetCertIssuerName: String; override;
    function GetCertCertificadora: String; override;
    function GetHTTPResultCode: Integer; override;
    function GetInternalErrorCode: Integer; override;

  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''): String; override;
    function Enviar(const ConteudoXML: String; const URL: String;
      const SoapAction: String; const MimeType: String = ''): String; override;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; override;
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''): Boolean; override;

    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Assinar: Boolean =  False): AnsiString; override;

    procedure CarregarCertificado; override;
    procedure DescarregarCertificado; override;
  end;

procedure InitXmlSec;
procedure ShutDownXmlSec;

var
  XMLSecLoaded: boolean;

implementation

uses Math, strutils, dateutils,
  ACBrUtil, ACBrDFeException, ACBrDFeUtil, ACBrConsts,
  pcnAuxiliar,
  synautil, synacode;

procedure InitXmlSec;
begin
  if XMLSecLoaded then exit;
  
  //--Inicializar funções das units do OpenSSL
  libxml2.Init;
  libxslt.Init;
  libxmlsec.Init;
  //libexslt.Init;
  //--

  { Init libxml and libxslt libraries }
  xmlInitThreads();
  xmlInitParser();
  __xmlLoadExtDtdDefaultValue^ := XML_DETECT_IDS or XML_COMPLETE_ATTRS;
  xmlSubstituteEntitiesDefault(1);
  __xmlIndentTreeOutput^ := 1;

  { Init xmlsec library }
  if (xmlSecInit() < 0) then
    raise EACBrDFeException.Create('Error: xmlsec initialization failed.');

  { Check loaded library version }
  if (xmlSecCheckVersionExt(1, 2, 18, xmlSecCheckVersionABICompatible) <> 1) then
    raise EACBrDFeException.Create(
      'Error: loaded xmlsec library version is not compatible.');

  (* Load default crypto engine if we are supporting dynamic
   * loading for xmlsec-crypto libraries. Use the crypto library
   * name ("openssl", "nss", etc.) to load corresponding
   * xmlsec-crypto library.
   *)
  if (xmlSecCryptoDLLoadLibrary('openssl') < 0) then
    raise EACBrDFeException.Create(
      'Error: unable to load default xmlsec-crypto library. Make sure'#10 +
      'that you have it installed and check shared libraries path'#10 +
      '(LD_LIBRARY_PATH) environment variable.');

  { Init crypto library }
  if (xmlSecCryptoAppInit(nil) < 0) then
    raise EACBrDFeException.Create('Error: crypto initialization failed.');

  { Init xmlsec-crypto library }
  if (xmlSecCryptoInit() < 0) then
    raise EACBrDFeException.Create('Error: xmlsec-crypto initialization failed.');

  XMLSecLoaded := True;
end;

procedure ShutDownXmlSec;
begin
  if not XMLSecLoaded then Exit;

  { Shutdown xmlsec-crypto library }
  xmlSecCryptoShutdown();

  { Shutdown crypto library }
  xmlSecCryptoAppShutdown();

  { Shutdown xmlsec library }
  xmlSecShutdown();

  { Shutdown libxslt/libxml }
  xsltCleanupGlobals();
  xmlCleanupParser();

  XMLSecLoaded := False;
end;


{ TDFeOpenSSL }

constructor TDFeOpenSSL.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create(ADFeSSL);

  FHTTP := THTTPSend.Create;
  FdsigCtx := nil;
  Clear;
end;

destructor TDFeOpenSSL.Destroy;
begin
  DescarregarCertificado;
  FHTTP.Free;

  inherited Destroy;
end;

function TDFeOpenSSL.Assinar(const ConteudoXML, docElement, infElement: String;
  SignatureNode: String; SelectionNamespaces: String; IdSignature: String
  ): String;
var
  AXml, XmlAss, DTD: String;
begin
  // Nota: "ConteudoXML" já deve estar convertido para UTF8 //
  XmlAss := '';
  if infElement <> '' then
  begin
    DTD := StringReplace(cDTD, '&infElement&', infElement, []);
    AXml := InserirDTD(ConteudoXML, DTD);
  end
  else
    AXml := ConteudoXML;

  // Inserindo Template da Assinatura digital //
  if (not XmlEstaAssinado(AXml)) or (SignatureNode <> '') then
    AXml := AdicionarSignatureElement(AXml, True, docElement, IdSignature);

  // Assinando com XMLSec //
  //DEBUG
  //WriteToTXT('C:\TEMP\XmlToSign.xml', AXml, False, False);

  XmlAss := XmlSecSign(AXml, AnsiString(SignatureNode),
                             AnsiString(SelectionNamespaces),
                             AnsiString(infElement));

  XmlAss := AjustarXMLAssinado(XmlAss, FCertAsDER);

  // Removendo DTD //
  Result := StringReplace(XmlAss, DTD, '', []);
end;

function TDFeOpenSSL.Enviar(const ConteudoXML: String; const URL: String;
  const SoapAction: String; const MimeType: String): String;
var
  OK: Boolean;
  RetornoWS: AnsiString;
begin
  RetornoWS := '';

  // Configurando o THTTPSend //
  ConfiguraHTTP(URL, SoapAction, MimeType);

  // Gravando no Buffer de Envio //
  WriteStrToStream(FHTTP.Document, AnsiString(ConteudoXML)) ;

  // DEBUG //
  //FHTTP.Document.SaveToFile( 'c:\temp\HttpSendDocument.xml' );
  //FHTTP.Headers.SaveToFile( 'c:\temp\HttpSendHeader.xml' );

  // Transmitindo //
  OK := FHTTP.HTTPMethod('POST', URL);

  // Provedor Agili (RESTFul) retorna 202 em vez de 200 //
  OK := OK and (FHTTP.ResultCode in [200, 202]);
  if not OK then
    raise EACBrDFeException.CreateFmt( cACBrDFeSSLEnviarException,
                                       [InternalErrorCode, HTTPResultCode] );

  // Lendo a resposta //
  FHTTP.Document.Position := 0;
  RetornoWS := ReadStrFromStream(FHTTP.Document, FHTTP.Document.Size);

  // DEBUG //
  //HTTP.Document.SaveToFile('c:\temp\ReqResp.xml');

  Result := String( RetornoWS );
end;

function TDFeOpenSSL.Validar(const ConteudoXML, ArqSchema: String;
  out MsgErro: String): Boolean;
var
  doc, schema_doc: xmlDocPtr;
  parser_ctxt: xmlSchemaParserCtxtPtr;
  schema: xmlSchemaPtr;
  valid_ctxt: xmlSchemaValidCtxtPtr;
  schemError: xmlErrorPtr;
  AXml: AnsiString;
begin
  InitXmlSec;

  Result := False;
  doc := Nil;
  schema_doc := Nil;
  parser_ctxt := Nil;
  schema := Nil;
  valid_ctxt := Nil;

  try
    AXml := AnsiString(ConteudoXML);
    doc := xmlParseDoc(PAnsiChar(AXml));
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
    begin
      MsgErro := 'Erro: unable to parse';
      exit;
    end;

    schema_doc := xmlReadFile(PAnsiChar(AnsiString(ArqSchema)), nil, XML_DETECT_IDS);
    // the schema cannot be loaded or is not well-formed
    if (schema_doc = nil) then
    begin
      MsgErro := 'Erro: Schema não pode ser carregado ou está corrompido';
      exit;
    end;

    parser_ctxt := xmlSchemaNewDocParserCtxt(schema_doc);
    // unable to create a parser context for the schema */
    if (parser_ctxt = nil) then
    begin
      MsgErro := 'Erro: unable to create a parser context for the schema';
      exit;
    end;

    schema := xmlSchemaParse(parser_ctxt);
    // the schema itself is not valid
    if (schema = nil) then
    begin
      MsgErro := 'Error: the schema itself is not valid';
      exit;
    end;

    valid_ctxt := xmlSchemaNewValidCtxt(schema);
    // unable to create a validation context for the schema */
    if (valid_ctxt = nil) then
    begin
      MsgErro := 'Error: unable to create a validation context for the schema';
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

function TDFeOpenSSL.VerificarAssinatura(const ConteudoXML: String; out
  MsgErro: String; const infElement: String; SignatureNode: String;
  SelectionNamespaces: String): Boolean;
var
  doc: xmlDocPtr;
  SignNode: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  mngr: xmlSecKeysMngrPtr;
  X509Certificate, DTD: String;
  AXml, asSignatureNode, asSelectionNamespaces, asInfElement: AnsiString;
  MS: TMemoryStream;

const
  xmlSecKeyDataTypeTrusted = $0100 ;

begin
  InitXmlSec;

  asSignatureNode       := AnsiString(SignatureNode);
  asSelectionNamespaces := AnsiString(SelectionNamespaces);
  asInfElement          := AnsiString(infElement);
  VerificarValoresPadrao(asSignatureNode, asSelectionNamespaces);

  Result := False;

  X509Certificate := LerTagXML(ConteudoXML, 'X509Certificate' );
  DTD  := StringReplace(cDTD, '&infElement&', infElement, []);
  AXml := AnsiString(InserirDTD(ConteudoXML, DTD));

  doc := nil;
  dsigCtx := nil;

  MS := TMemoryStream.Create;
  try
    WriteStrToStream(MS, DecodeBase64(X509Certificate) );
    //DEBUG
    //MS.Position := 0;
    //MS.SaveToFile('c:\temp\cert.der');

    mngr := xmlSecKeysMngrCreate();
    if (mngr = nil) then
    begin
      MsgErro := ACBrStr(cErrMngrCreate);
      exit;
    end;

    if xmlSecCryptoAppDefaultKeysMngrInit(mngr) < 0 then
    begin
      MsgErro := ACBrStr(cErrMngrInit);
      exit;
    end;

    { Load the Certificate }
    MS.Position := 0;
    if (xmlSecCryptoAppKeysMngrCertLoadMemory(mngr, MS.Memory, MS.Size,
      xmlSecKeyDataFormatCertDer, xmlSecKeyDataTypeTrusted) < 0) then
    begin
      MsgErro := ACBrStr(cErrCertLoad);
      exit;
    end;

    doc := xmlParseDoc(PAnsiChar(AXml));
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
    begin
      MsgErro := ACBrStr(cErrParseDoc);
      exit;
    end;

    { Achando o nó da Assinatura }
    try
      SignNode := XmlSecFindSignatureNode(doc, SignatureNode, SelectionNamespaces, infElement);
    except
      On E: Exception do
      begin
        MsgErro := E.Message;
        exit;
      end
    end;

    dsigCtx := xmlSecDSigCtxCreate(mngr);
    if (dsigCtx = nil) then
    begin
      MsgErro := ACBrStr(cErrCtxCreate);
      exit;
    end;

    MS.Position := 0;
    dsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory(MS.Memory, MS.Size,
      xmlSecKeyDataFormatCertDer, nil, nil, nil);
    if (dsigCtx^.signKey = nil) then
    begin
      MsgErro := ACBrStr(cErrPubKeyLoad);
      exit;
    end;

    { Verify signature }
    if (xmlSecDSigCtxVerify(dsigCtx, SignNode) < 0) then
    begin
      MsgErro := ACBrStr(cErrDSigVerify);
      exit;
    end;

    Result := (dsigCtx.status = xmlSecDSigStatusSucceeded);
    if not Result then
      MsgErro := ACBrStr(cErrDSigInvalid);

  finally
    { cleanup }
    MS.Free;

    if (doc <> nil) then
      xmlFreeDoc(doc);

    if (dsigCtx <> nil) then
      xmlSecDSigCtxDestroy(dsigCtx);
  end;
end;

{ Método clonado de ACBrEAD }
function TDFeOpenSSL.CalcHash(const AStream: TStream; const Digest: TSSLDgst;
  const Assinar: Boolean): AnsiString;
Var
  md : PEVP_MD ;
  md_len: cardinal;
  md_ctx: EVP_MD_CTX;
  md_value_bin : array [0..1023] of AnsiChar;
  NameDgst : PAnsiChar;
  ABinStr: AnsiString;
  Memory: Pointer;
  PosStream: Int64;
  BytesRead: LongInt;
begin
  NameDgst := '';
  case Digest of
    dgstMD2    : NameDgst := 'md2';
    dgstMD4    : NameDgst := 'md4';
    dgstMD5    : NameDgst := 'md5';
    dgstRMD160 : NameDgst := 'rmd160';
    dgstSHA    : NameDgst := 'sha';
    dgstSHA1   : NameDgst := 'sha1';
    dgstSHA256 : NameDgst := 'sha256';
    dgstSHA512 : NameDgst := 'sha512';
  end ;

  if Assinar and (FPrivKey = Nil) then
    CarregarCertificado;

  PosStream := 0;
  AStream.Position := 0;
  GetMem(Memory, CBufferSize);
  try
    md_len := 0;
    md := EVP_get_digestbyname( NameDgst );
    EVP_DigestInit( @md_ctx, md );

    while (PosStream < AStream.Size) do
    begin
       BytesRead := AStream.Read(Memory^, CBufferSize);
       if BytesRead <= 0 then
          Break;

       EVP_DigestUpdate( @md_ctx, Memory, BytesRead ) ;
       PosStream := PosStream + BytesRead;
    end;

    if Assinar then
       EVP_SignFinal( @md_ctx, @md_value_bin, md_len, FPrivKey)
    else
       EVP_DigestFinal( @md_ctx, @md_value_bin, {$IFNDEF USE_libeay32}@{$ENDIF}md_len);

    SetString( ABinStr, md_value_bin, md_len);
    Result := ABinStr;
  finally
    Freemem(Memory);
  end;
end;

function TDFeOpenSSL.XmlSecSign(const ConteudoXML: AnsiString; SignatureNode,
  SelectionNamespaces, InfElement: AnsiString): AnsiString;
var
  doc: xmlDocPtr;
  SignNode: xmlNodePtr;
  buffer: PAnsiChar;
  bufSize, SignResult: integer;
begin
  InitXmlSec;

  doc := Nil;
  Result := '';

  if Trim(ConteudoXML) = '' then
    Exit;

  CreateCtx;
  try
    { load template }
    doc := xmlParseDoc(PAnsiChar(ConteudoXML));
    if (doc = nil) then
      raise EACBrDFeException.Create(cErrParseDoc);

    { Dispara Exception se não encontrar o SignNode }
    SignNode := XmlSecFindSignatureNode(doc, SignatureNode, SelectionNamespaces, InfElement);

    { sign the template }
    SignResult := xmlSecDSigCtxSign(FdsigCtx, SignNode);
    if (SignResult < 0) then
      raise EACBrDFeException.CreateFmt(cErrDSigSign, [SignResult]);

    { print signed document to stdout }
    // xmlDocDump(stdout, doc);
    // Can't use "stdout" from Delphi, so we'll use xmlDocDumpMemory instead...
    buffer := nil;
    xmlDocDumpMemory(doc, @buffer, @bufSize);
    if (buffer <> nil) then
      { success }
      Result := buffer;
  finally
    { cleanup }
    if (doc <> nil) then
      xmlFreeDoc(doc);

    DestroyCtx ;
  end;
end;

function TDFeOpenSSL.XmlSecFindSignatureNode(ADoc: xmlDocPtr; SignatureNode,
  SelectionNamespaces, InfElement: AnsiString): xmlNodePtr;
var
  rootNode, infNode, SignNode: xmlNodePtr;
begin
  { Encontra o elemento Raiz }
  rootNode := xmlDocGetRootElement(ADoc);
  if (rootNode = nil) then
    raise EACBrDFeException.Create(cErrFindRootNode);

  VerificarValoresPadrao(SignatureNode, SelectionNamespaces);

  { Se tem InfElement, procura pelo mesmo. Isso permitirá acharmos o nó de
    assinatura, relacionado a ele (mesmo pai) }
  if (InfElement <> '') then
  begin
    { Procura InfElement em todos os nós, filhos de Raiz, usando XMLSec }
    infNode := XmlSecLookUpNode(rootNode, InfElement );

    { Não achei o InfElement em nenhum nó :( }
    if (infNode = nil) then
      raise EACBrDFeException.Create(cErrFindRootNode);

    { Vamos agora, achar o pai desse Elemento, pois com ele encontraremos a assinatura }
    if (infNode^.name = InfElement) and Assigned(infNode.parent) and (infNode.parent^.name <> '') then
      infNode := infNode.parent;
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
  SignNode := infNode.last;
  if not XmlSecNodeWasFound(SignNode, SignatureNode, SelectionNamespaces) then
  begin
    { Não é o ultimo nó do infNode... então, vamos procurar por um Nó dentro de infNode }
    SignNode := XmlSecLookUpNode(infNode, SignatureNode, SelectionNamespaces );

    { Se ainda não achamos, vamos procurar novamente a partir do elemento Raiz  }
    if (SignNode = nil) then
    begin
      SignNode := rootNode.last;
      if not XmlSecNodeWasFound(SignNode, SignatureNode, SelectionNamespaces) then
        SignNode := XmlSecLookUpNode(rootNode, SignatureNode, SelectionNamespaces );
    end;
  end;

  if (SignNode = nil) then
    raise EACBrDFeException.Create(cErrFindSignNode);

  Result := SignNode;
end;

function TDFeOpenSSL.XmlSecNodeWasFound(ANode: xmlNodePtr;
  NodeName: AnsiString; NameSpace: AnsiString): Boolean;
begin
  Result := (ANode <> nil) and
            (ANode.name = NodeName) and
            ( (NameSpace = '') or (ANode.ns.href = NameSpace) );
end;

function TDFeOpenSSL.XmlSecLookUpNode(ParentNode: xmlNodePtr;
  NodeName: AnsiString; NameSpace: AnsiString): xmlNodePtr;
begin
  Result := ParentNode;
  if (ParentNode = Nil) or (Trim(NodeName) = '') then
    Exit;

  { Primeiro vamos ver se o nó Raiz já não é o que precisamos }
  if XmlSecNodeWasFound(ParentNode, NodeName, NameSpace) then
    Exit;

  { Chama função auxiliar, que usa busca recursiva em todos os nós filhos }
  Result := _XmlSecLookUpNode(ParentNode.children, NodeName, NameSpace);
end;

function TDFeOpenSSL._XmlSecLookUpNode(ParentNode: xmlNodePtr;
  NodeName: AnsiString; NameSpace: AnsiString): xmlNodePtr;
var
  NextNode, ChildNode, FoundNode: xmlNodePtr;
begin
  Result := ParentNode;
  if (ParentNode = Nil) then
    Exit;

  FoundNode := ParentNode;

  while (FoundNode <> Nil) and
        (not XmlSecNodeWasFound(FoundNode, NodeName, NameSpace)) do
  begin
    ChildNode := FoundNode.children;
    NextNode  := FoundNode.next;
    { Faz Chamada recursiva para o novo Filho }
    FoundNode := _XmlSecLookUpNode(ChildNode, NodeName, NameSpace);

    if FoundNode = Nil then
      FoundNode := NextNode;
  end;

  Result := FoundNode;
end;

procedure TDFeOpenSSL.CreateCtx;
var
  MS: TMemoryStream;
begin
  InitXmlSec;
  // Se FdsigCtx já existia, destrua e crie um novo //
  DestroyCtx;

  with FpDFeSSL do
  begin
    if EstaVazio(DadosPFX) then
      Self.CarregarCertificado;

    { create signature context }
    FdsigCtx := xmlSecDSigCtxCreate(nil);
    if (FdsigCtx = nil) then
      raise EACBrDFeException.Create(cErrCtxCreate);

    MS := TMemoryStream.Create;
    try
      WriteStrToStream(MS, DadosPFX);

      MS.Position := 0;
      FdsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory(
        MS.Memory, MS.Size, xmlSecKeyDataFormatPkcs12,
        PAnsiChar(Senha), nil, nil);

      if (FdsigCtx^.signKey = nil) then
        raise EACBrDFeException.Create(cErrPrivKeyLoad);

    finally
      MS.Free;
    end;
  end;
end;

procedure TDFeOpenSSL.DestroyCtx;
begin
  if (FdsigCtx <> nil) then
  begin
    InitXmlSec;
    xmlSecDSigCtxDestroy(FdsigCtx);
    FdsigCtx := nil;
  end;
end;

procedure TDFeOpenSSL.DestroyKey;
begin
  if (FPrivKey <> Nil) then
  begin
    {$IFDEF USE_libeay32}
     EVP_PKEY_free(FPrivKey);
    {$ELSE}
     EvpPkeyFree(FPrivKey);
    {$ENDIF}
    FPrivKey := nil;
  end;
end;

procedure TDFeOpenSSL.CarregarCertificado;
var
  LoadFromFile, LoadFromData: Boolean;
  FS: TFileStream;
begin
  with FpDFeSSL do
  begin
    // Verificando se possui parâmetros necessários //
    if EstaVazio(ArquivoPFX) and EstaVazio(DadosPFX) then
    begin
      if not EstaVazio(NumeroSerie) then
        raise EACBrDFeException.Create(ClassName +
          ' não suporta carga de Certificado pelo número de série.' +
          sLineBreak + 'Utilize "ArquivoPFX" ou "DadosPFX"')
      else
        raise EACBrDFeException.Create('Certificado não informado.' +
          sLineBreak + 'Utilize "ArquivoPFX" ou "DadosPFX"');
    end;

    LoadFromFile := (not EstaVazio(ArquivoPFX)) and FileExists(ArquivoPFX);
    LoadFromData := (not EstaVazio(DadosPFX));

    if not (LoadFromFile or LoadFromData) then
      raise EACBrDFeException.Create('Arquivo: ' + ArquivoPFX + ' não encontrado, e DadosPFX não informado');

    if LoadFromFile then
    begin
      FS := TFileStream.Create(ArquivoPFX, fmOpenRead or fmShareDenyNone);
      try
        DadosPFX := ReadStrFromStream(FS, FS.Size);
      finally
        FS.Free;
      end;
    end;

    if EstaVazio(DadosPFX) then
      raise EACBrDFeException.Create('Erro ao Carregar Certificado');

    FHTTP.Sock.SSL.PFX := DadosPFX;
    FHTTP.Sock.SSL.KeyPassword := Senha;

    if not LerPFXInfo(DadosPFX) then
      raise EACBrDFeException.Create('Erro ao ler informações do Certificado.'+sLineBreak+
                                     'Provavelmente a senha está errada' );
  end;

  FpCertificadoLido := True;
end;

procedure TDFeOpenSSL.DescarregarCertificado;
begin
  DestroyCtx;
  DestroyKey;
  Clear;
  FpCertificadoLido := False;
end;

function TDFeOpenSSL.LerPFXInfo(pfxdata: Ansistring): Boolean;

  function GetNotAfter( cert: pX509 ): TDateTime;
  var
    Validade: String;
    notAfter: PASN1_TIME;
  begin
    notAfter := cert.cert_info^.validity^.notAfter;
    Validade := {$IFDEF DELPHIXE4_UP}AnsiStrings.{$ENDIF}StrPas( PAnsiChar(notAfter^.data) );
    SetLength(Validade, notAfter^.length);
    Validade := OnlyNumber(Validade);

    if notAfter^.asn1_type = V_ASN1_UTCTIME then  // anos com 2 dígitos
      Validade :=  LeftStr(IntToStrZero(YearOf(Now),4),2) + Validade;

    Result := StoD(Validade);
  end;

  function GetSubjectName( cert: pX509 ): String;
  var
    s: AnsiString;
  begin
    setlength(s, 4096);
    {$IFDEF USE_libeay32}
     Result := X509_NAME_oneline(X509_get_subject_name(cert), PAnsiChar(s), Length(s));
    {$ELSE}
     Result := X509NameOneline(X509GetSubjectName(cert), s, Length(s));
    {$ENDIF}
    if copy(Result,1,1) = '/' then
      Result := Copy(Result,2,Length(Result));

    Result := StringReplace(Result, '/', ', ', [rfReplaceAll]);
  end;

  function GetIssuerName( cert: pX509 ): String;
  var
    s: AnsiString;
  begin
    setlength(s, 4096);
    {$IFDEF USE_libeay32}
     Result := X509_NAME_oneline(X509_get_issuer_name(cert), PAnsiChar(s), Length(s));
    {$ELSE}
     Result := X509NameOneline(X509GetIssuerName(cert), s, Length(s));
    {$ENDIF}
    if copy(Result,1,1) = '/' then
      Result := Copy(Result,2,Length(Result));

    Result := StringReplace(Result, '/', ', ', [rfReplaceAll]);
  end;

  function GetCNPJExt( cert: pX509): String;
  var
    ext: pX509_EXTENSION;
    I, P: Integer;
    prop: PASN1_STRING;
    propStr: AnsiString;
  begin
    Result := '';
    I := 0;
   {$IFDEF USE_libeay32}
    ext := X509_get_ext( cert, I);
   {$ELSE}
    ext := X509GetExt( cert, I);
   {$ENDIF}
    while (ext <> nil) do
    begin
      prop := ext.value;
      propStr := PAnsiChar(prop^.data);
      SetLength(propStr, prop^.length);

      P := pos(#1#3#3#160#16, propStr);;
      if P > 0 then
      begin
        Result := LeftStr(OnlyNumber(copy(propStr,P+5,16)),14);
        exit;
      end;

      inc( I );
      {$IFDEF USE_libeay32}
       ext := X509_get_ext( cert, I);
      {$ELSE}
       ext := X509GetExt( cert, I);
      {$ENDIF}
    end;
  end;

  function GetSerialNumber( cert: pX509): String;
  var
    SN: PASN1_STRING;
    s: AnsiString;
  begin
    {$IFDEF USE_libeay32}
     SN := X509_get_serialNumber(cert);
    {$ELSE}
     SN := X509GetSerialNumber(cert);
    {$ENDIF}
    s := {$IFDEF DELPHIXE4_UP}AnsiStrings.{$ENDIF}StrPas( PAnsiChar(SN^.data) );
    SetLength(s,SN.length);
    Result := AsciiToHex(s);
  end;

  { Método clonado de ACBrEAD }
  function BioToStr(ABio : pBIO) : AnsiString ;
  Var
    {$IFDEF USE_libeay32}
     Buf : array [0..1023] of AnsiChar;
    {$ENDIF}
    Ret : Integer ;
    Lin : String ;
  begin
    Result := '';

    {$IFDEF USE_libeay32}
     while BIO_eof( ABio ) = 0 do
     begin
       Ret := BIO_gets( ABio, Buf, 1024 );
       SetString( Lin, Buf, Ret);
       Result := Result + Lin;
     end ;
    {$ELSE}
     repeat
        SetLength(Lin,1024);
        Ret := BioRead( ABio, Lin, 1024);
        if Ret > 0 then
        begin
           Lin := copy(Lin,1,Ret) ;
           Result := Result + Lin;
        end ;
     until (Ret <= 0);
    {$ENDIF}
  end ;

  function CertToDER( cert: pX509): String;
  var
    MemBio: PBIO;
    Buff: AnsiString;
  begin
    {$IFDEF USE_libeay32}
     MemBio := Bio_New(Bio_S_Mem());
     try
       i2d_X509_bio(MemBio, cert);
       Buff := BioToStr( MemBio );
     finally
       BIO_free_all( MemBio );
     end;
    {$ELSE}
     MemBio := BioNew(BioSMem());
     try
       i2dX509bio(MemBio, cert);
       Buff := BioToStr( MemBio );
     finally
       BioFreeAll( MemBio );
     end;
    {$ENDIF}

    Result := EncodeBase64(Buff);
  end;

var
  cert: pX509;
  ca, p12: Pointer;
  b: PBIO;
begin
  Result := False;
  DestroyKey;

  {$IFDEF USE_libeay32}
   b := Bio_New(BIO_s_mem);
  {$ELSE}
   b := BioNew(BioSMem);
  {$ENDIF}
  try
    {$IFDEF USE_libeay32}
     BIO_write(b, PAnsiChar(pfxdata), Length(PfxData));
     p12 := d2i_PKCS12_bio(b, nil);
    {$ELSE}
     BioWrite(b, pfxdata, Length(PfxData));
     p12 := d2iPKCS12bio(b, nil);
    {$ENDIF}
    if not Assigned(p12) then
      Exit;

    try
      cert := nil;
      FPrivKey := nil;
      ca := nil;
      try
        {$IFDEF USE_libeay32}
        if PKCS12_parse(p12, PAnsiChar(FpDFeSSL.Senha), FPrivKey, cert, ca) > 0 then
        {$ELSE}
        if PKCS12parse(p12, FpDFeSSL.Senha, FPrivKey, cert, ca) > 0 then
        {$ENDIF}
        begin
          Result := True;
          FValidade := GetNotAfter( cert );
          FSubjectName := GetSubjectName( cert );
          FRazaoSocial := GetRazaoSocialFromSubjectName( FSubjectName );
          FIssuerName := GetIssuerName( cert );
          FCertificadora := GetCertificadoraFromSubjectName( FIssuerName );
          FCNPJ := GetCNPJFromSubjectName( FSubjectName );
          if FCNPJ = '' then  // Não tem CNPJ no SubjectName, lendo das Extensões
            FCNPJ := GetCNPJExt( cert );

          FNumSerie := GetSerialNumber( cert );
          FCertAsDER := CertToDER( cert );
        end;
      finally
        {$IFDEF USE_libeay32}
         X509_free(cert);
        {$ELSE}
         X509free(cert);
        {$ENDIF}
      end;
    finally
      {$IFDEF USE_libeay32}
       PKCS12_free(p12);
      {$ELSE}
       PKCS12free(p12);
      {$ENDIF}
    end;
  finally
    {$IFDEF USE_libeay32}
     BIO_free_all(b);
    {$ELSE}
     BioFreeAll(b);
    {$ENDIF}
  end;
end;

procedure TDFeOpenSSL.VerificarValoresPadrao(var SignatureNode: AnsiString;
  var SelectionNamespaces: AnsiString);
var
  DSigNs: AnsiString;
begin
  if SignatureNode = '' then
    SignatureNode := xmlSecNodeSignature()
  else
    SignatureNode := copy(SignatureNode, Pos(':',SignatureNode)+1, Length(SignatureNode));

  DSigNs := xmlSecDSigNs();

  if SelectionNamespaces = '' then
    SelectionNamespaces := DSigNs
  else
  begin
    SelectionNamespaces := RetornarConteudoEntre( SelectionNamespaces, '"', '"');

    if strutils.LeftStr(SelectionNamespaces, Length(DSigNs)) <> DSigNs then
      SelectionNamespaces := DSigNs + ' ' + SelectionNamespaces;
  end;
end;


function TDFeOpenSSL.GetCertDataVenc: TDateTime;
begin
  if FValidade = 0 then
    CarregarCertificado;

  Result := FValidade;
end;

function TDFeOpenSSL.GetCertNumeroSerie: String;
begin
  if EstaVazio(FNumSerie) then
    CarregarCertificado;

  Result := FNumSerie;
end;

function TDFeOpenSSL.GetCertSubjectName: String;
begin
  if EstaVazio(FSubjectName) then
    CarregarCertificado;

  Result := FSubjectName;
end;

function TDFeOpenSSL.GetCertRazaoSocial: String;
begin
  if EstaVazio(FRazaoSocial) then
    CarregarCertificado;

  Result := FRazaoSocial;
end;

function TDFeOpenSSL.GetCertCNPJ: String;
begin
  if EstaVazio(FCNPJ) then
    CarregarCertificado;

  Result := FCNPJ;
end;

function TDFeOpenSSL.GetCertIssuerName: String;
begin
  if EstaVazio(FIssuerName) then
    CarregarCertificado;

  Result := FIssuerName;
end;

function TDFeOpenSSL.GetCertCertificadora: String;
begin
  if EstaVazio(FCertificadora) then
    CarregarCertificado;

  Result := FCertificadora;
end;

function TDFeOpenSSL.GetHTTPResultCode: Integer;
begin
  Result := FHTTP.ResultCode;
end;

function TDFeOpenSSL.GetInternalErrorCode: Integer;
begin
  Result := FHTTP.Sock.LastError;
end;

procedure TDFeOpenSSL.Clear;
begin
  FCNPJ := '';
  FRazaoSocial := '';
  FNumSerie := '';
  FValidade := 0;
  FSubjectName := '';
  FIssuerName := '';
  FCertificadora := '';
  FHTTP.Sock.SSL.PFX := '';
  FHTTP.Sock.SSL.KeyPassword := '';
end;

procedure TDFeOpenSSL.ConfiguraHTTP(const URL, SoapAction: String;
  MimeType: String);
begin
  FHTTP.Clear;

  if not FpDFeSSL.UseCertificateHTTP then
  begin
    FHTTP.Sock.SSL.PFX := '';
    FHTTP.Sock.SSL.KeyPassword := '';
  end
  else
  begin
    if (FHTTP.Sock.SSL.PFX = '') then
      CarregarCertificado;
  end;

  FHTTP.Timeout   := FpDFeSSL.TimeOut;
  FHTTP.ProxyHost := FpDFeSSL.ProxyHost;
  FHTTP.ProxyPort := FpDFeSSL.ProxyPort;
  FHTTP.ProxyUser := FpDFeSSL.ProxyUser;
  FHTTP.ProxyPass := FpDFeSSL.ProxyPass;

  if MimeType = '' then
    MimeType := 'application/soap+xml';

  FHTTP.MimeType := MimeType + '; charset=utf-8';     // Todos DFes usam UTF8

  FHTTP.UserAgent := '';
  FHTTP.Protocol  := '1.1';
  FHTTP.AddPortNumberToHost := False;

  if SoapAction <> '' then
    FHTTP.Headers.Add('SOAPAction: "' + SoapAction + '"');
end;

function TDFeOpenSSL.InserirDTD(AXml: String; const DTD: String): String;
var
  I: integer;
begin
  // Adicionando Cabeçalho DTD, necessário para xmlsec encontrar o ID //
  I := pos('?>', AXml);
  Result := Copy(AXml, 1, IfThen(I > 0, I + 1, I)) +
            DTD +
            Copy(AXml, IfThen(I > 0, I + 2, I), Length(AXml));
end;

initialization
  XMLSecLoaded := False;

finalization;
  ShutDownXmlSec;

end.

