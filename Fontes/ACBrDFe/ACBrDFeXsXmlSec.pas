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

{.$Define USE_MSCRYPO}
{ Ligue a diretiva acima, para usar libxmlsec-mscrypto.dll, ou seja, a XMLSec
  usará a MSCrypto ao invez do OpenSSL, para efeturar a assintura digital do XML.
  (ainda em desenvolvimento)
  Isso permitirá que a XMLSec, acesse certificados A3, por exemplo...
}

{$IfNDef MSWINDOWS}
 {$UnDef USE_MSCRYPO}
{$EndIf}

unit ACBrDFeXsXmlSec;

interface

uses
  {$IfDef DELPHIXE4_UP}
   AnsiStrings,
  {$EndIf}
  {$IfDef USE_MSCRYPO}
   windows, ACBr_WinCrypt,
   {$IfDef FPC}
    DynLibs,
   {$EndIf}
  {$EndIf}
  Classes, SysUtils,
  ACBrDFeSSL, ACBrDFeException,
  libxmlsec, libxml2;

const
  {$IfDef USE_MSCRYPO}
   {$IfDef USE_MINGW}
    LIBXMLSEC_MSCRYPTO_SO = 'libxmlsec1-mscrypto.dll';
   {$Else}
    LIBXMLSEC_MSCRYPTO_SO = 'libxmlsec-mscrypto.dll';
   {$EndIf}
  {$EndIf}

  cDTD = '<!DOCTYPE test [<!ATTLIST &infElement& &IdAttribute& ID #IMPLIED>]>';

  cCryptLibMSCrypto = 'mscrypto';
  cCryptLibOpenSSL = 'openssl';

  cErrMngrCreate = 'Erro: Falha ao criar Gerenciador de Chaves "xmlSecKeysMngrCreate"';
  cErrMngrInit = 'Erro: Falha ao inicializar o Gerenciador de Chaves "xmlSecCryptoAppDefaultKeysMngrInit"';
  cErrCertLoad = 'Erro: Falha ao ler informação do Certificado no Gerenciador de Chaves';
  cErrParseDoc = 'Erro: Falha ao interpretar o XML "xmlParseDoc"';
  cErrFindSignNode = 'Erro: Falha ao localizar o nó de Assinatura';
  cErrFindRootNode = 'Erro: Falha ao localizar o nó Raiz';
  cErrCtxCreate = 'Erro: Falha ao criar Ctx "xmlSecDSigCtxCreate"';
  cErrNoPFxData = 'Erro: Falha ao ler DadosPFX';
  cErrPrivKeyLoad = 'Erro: Falha ao ler a Chave Privada de DadosPFX';
  cErrPubKeyLoad = 'Erro: Falha ao ler a Chave Publica do Certificado';
  cErrDSigSign = 'Erro %d: Falha ao assinar o Documento';
  cErrDSigVerify = 'Erro: Falha na verificação da Assinatura';
  cErrDSigInvalid = 'Erro: Assinatura é inválida';

  cErrXmlSecInit = 'Falha ao inicializar Biblioteca XMLSec.';
  cErrXmlSecWrongVersion = 'Essa version da Biblioteca XMLSec não é compatível com o ACBr.';
  cErrXmlSecLoadCriptoLib = 'Falha ao carregar biblioteca de Criptografia do XMLSec [%s]';
  cErrXmlSecInitCriptoLib = 'Falha ao inicializar a Biblioteca de Criptografia do XMLSec [%s]';
type

  { TDFeSSLXmlSignXmlSec }

  TDFeSSLXmlSignXmlSec = class( TDFeSSLXmlSignClass )
  private
    FdsigCtx: xmlSecDSigCtxPtr;

    function InserirDTD(AXml: String; const DTD: String): String;
    {$IfDef USE_MSCRYPO}
    function UseMSCrypto: Boolean;
    function SSLUsaMSCrypto: Boolean;
    {$EndIf}

    procedure InitXmlSec;
    procedure CreateCtx;
    procedure DestroyCtx;
    function XmlSecSign(const ConteudoXML: AnsiString; SignatureNode,
      SelectionNamespaces, InfElement: AnsiString): AnsiString;
  protected
  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''; IdAttr: String = ''): String; override;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; override;
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''; IdSignature: String = '';
      IdAttr: String = ''): Boolean;
      override;
  end;

procedure InitXmlSec( XMLSecCryptoLib: String );
procedure ShutDownXmlSec;

{$IfDef USE_MSCRYPO}
type
   TxmlSecMSCryptoAppInit = function (config: PAnsiChar): LongInt; cdecl;
   TxmlSecMSCryptoCertAdopt = function (pCert: PCCERT_CONTEXT; type_: xmlSecKeyDataType): xmlSecKeyDataPtr; cdecl;
   TxmlSecMSCryptoKeyDataX509AdoptCert = function (data: xmlSecKeyDataPtr; pCert: PCCERT_CONTEXT): Longint; cdecl;
   TxmlSecMSCryptoKeyDataX509AdoptKeyCert = function (data: xmlSecKeyDataPtr; pCert: PCCERT_CONTEXT): Longint; cdecl;

var
  libXmlSecMsCryptoHandle: TLibHandle;
   _xmlSecMSCryptoAppInit: TxmlSecMSCryptoAppInit = nil;
   _xmlSecMSCryptoCertAdopt: TxmlSecMSCryptoCertAdopt = nil;
   _xmlSecMSCryptoKeyDataX509AdoptCert: TxmlSecMSCryptoKeyDataX509AdoptCert = nil;
   _xmlSecMSCryptoKeyDataX509AdoptKeyCert: TxmlSecMSCryptoKeyDataX509AdoptKeyCert = nil;

function xmlSecMSCryptoAppInit(config: PAnsiChar): LongInt; cdecl;
function xmlSecMSCryptoCertAdopt(pCert: PCCERT_CONTEXT; type_: xmlSecKeyDataType): xmlSecKeyDataPtr; cdecl;
function xmlSecMSCryptoKeyDataX509AdoptCert(data: xmlSecKeyDataPtr; pCert: PCCERT_CONTEXT): LongInt; cdecl;
function xmlSecMSCryptoKeyDataX509AdoptKeyCert(data: xmlSecKeyDataPtr; pCert: PCCERT_CONTEXT): LongInt; cdecl;

function xmlSecUseMSCrypto: Boolean;
{$EndIf}

procedure VerificarValoresPadrao(var SignatureNode: AnsiString;
  var SelectionNamespaces: AnsiString);
function XmlSecNodeWasFound(ANode: xmlNodePtr; NodeName: AnsiString;
  NameSpace: AnsiString): Boolean;
function XmlSecFindSignatureNode( ADoc: xmlDocPtr; SignatureNode,
  SelectionNamespaces, InfElement: AnsiString): xmlNodePtr;
function XmlSecLookUpNode(ParentNode: xmlNodePtr; NodeName: AnsiString;
  NameSpace: AnsiString = ''): xmlNodePtr;


implementation

Uses
  strutils, math,
  ACBrUtil, ACBrDFeUtil, ACBrConsts,
  pcnAuxiliar,
  synautil, synacode;

var
  XMLSecLoaded: String;

procedure InitXmlSec(XMLSecCryptoLib: String);
begin
  if (XMLSecLoaded <> '') then Exit;

  //--Inicializar funções das units do OpenSSL
  libxml2.Init;

  {$IfDef USE_MSCRYPO}
  if XMLSecCryptoLib = cCryptLibMSCrypto then
  begin
    libXmlSecMsCryptoHandle := LoadLibrary(LIBXMLSEC_MSCRYPTO_SO);

    if libXmlSecMsCryptoHandle <> 0 then
    begin
      _xmlSecMSCryptoAppInit := GetProcAddress(libXmlSecMsCryptoHandle, 'xmlSecMSCryptoAppInit');
      _xmlSecMSCryptoCertAdopt := GetProcAddress(libXmlSecMsCryptoHandle, 'xmlSecMSCryptoCertAdopt');
      _xmlSecMSCryptoKeyDataX509AdoptCert := GetProcAddress(libXmlSecMsCryptoHandle, 'xmlSecMSCryptoKeyDataX509AdoptCert');
      _xmlSecMSCryptoKeyDataX509AdoptKeyCert := GetProcAddress(libXmlSecMsCryptoHandle, 'xmlSecMSCryptoKeyDataX509AdoptKeyCert');
    end;

    xmlSecMSCryptoAppInit(nil);
  end;
  {$Else}
  if XMLSecCryptoLib = cCryptLibMSCrypto then
    XMLSecCryptoLib := cCryptLibOpenSSL;
  {$EndIf}

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
    raise EACBrDFeException.Create( cErrXmlSecInit );

  { Check loaded library version }
  if (xmlSecCheckVersionExt(1, 2, 20, xmlSecCheckVersionABICompatible) <> 1) then
    raise EACBrDFeException.Create( cErrXmlSecWrongVersion );

  (* Load default crypto engine if we are supporting dynamic
   * loading for xmlsec-crypto libraries. Use the crypto library
   * name ("openssl", "nss", etc.) to load corresponding
   * xmlsec-crypto library.
   *)
  if (xmlSecCryptoDLLoadLibrary(PAnsiChar(AnsiString(XMLSecCryptoLib))) < 0) then
    raise EACBrDFeException.CreateFmt( cErrXmlSecLoadCriptoLib, [XMLSecCryptoLib] );

  { Init crypto library }
  if (xmlSecCryptoAppInit(nil) < 0) then
    raise EACBrDFeException.CreateFmt( cErrXmlSecInitCriptoLib, [XMLSecCryptoLib] );

  { Init xmlsec-crypto library }
  if (xmlSecCryptoInit() < 0) then
    raise EACBrDFeException.CreateFmt( cErrXmlSecInitCriptoLib, [XMLSecCryptoLib] );

  XMLSecLoaded := XMLSecCryptoLib;
end;

procedure ShutDownXmlSec;
begin
  if (XMLSecLoaded = '') then Exit;

  { Shutdown xmlsec-crypto library }
  xmlSecCryptoShutdown();

  { Shutdown crypto library }
  xmlSecCryptoAppShutdown();

  { Shutdown xmlsec library }
  xmlSecShutdown();

  { Shutdown libxslt/libxml }
  xmlCleanupParser();

  XMLSecLoaded := '';

  {$IfDef USE_MSCRYPO}
   if libXmlSecMsCryptoHandle <> 0 then
   begin
     FreeLibrary(libXmlSecMsCryptoHandle);
     libXmlSecMsCryptoHandle := 0;
   end;
  {$EndIf}
end;


{$IfDef USE_MSCRYPO}
function xmlSecMSCryptoAppInit(config: PAnsiChar): LongInt; cdecl;
begin
  if not Assigned(_xmlSecMSCryptoAppInit) then
    Result := -1
  else
    Result := _xmlSecMSCryptoAppInit( config );
end;

function xmlSecMSCryptoCertAdopt(pCert: PCCERT_CONTEXT; type_: xmlSecKeyDataType
  ): xmlSecKeyDataPtr; cdecl;
begin
  if not Assigned(_xmlSecMSCryptoCertAdopt) then
    Result := Nil
  else
    Result := _xmlSecMSCryptoCertAdopt( pCert, type_);
end;

function xmlSecMSCryptoKeyDataX509AdoptCert(data: xmlSecKeyDataPtr;
  pCert: PCCERT_CONTEXT): LongInt; cdecl;
begin
  if not Assigned(_xmlSecMSCryptoKeyDataX509AdoptCert) then
    Result := -1
  else
    Result := _xmlSecMSCryptoKeyDataX509AdoptCert( data, pCert);
end;

function xmlSecMSCryptoKeyDataX509AdoptKeyCert(data: xmlSecKeyDataPtr;
  pCert: PCCERT_CONTEXT): LongInt; cdecl;
begin
  if not Assigned(_xmlSecMSCryptoKeyDataX509AdoptKeyCert) then
    Result := -1
  else
    Result := _xmlSecMSCryptoKeyDataX509AdoptKeyCert( data, pCert);
end;

function xmlSecUseMSCrypto: Boolean;
begin
  Result := (libXmlSecMsCryptoHandle <> 0);
end;
{$EndIf}

function XmlSecFindSignatureNode(ADoc: xmlDocPtr; SignatureNode,
  SelectionNamespaces, InfElement: AnsiString): xmlNodePtr;
var
  rootNode, infNode, SignNode: xmlNodePtr;
begin
  { Encontra o elemento Raiz }
  rootNode := xmlDocGetRootElement(ADoc);
  if (rootNode = nil) then
    raise EACBrDFeException.Create(cErrFindRootNode);

  VerificarValoresPadrao(SignatureNode, SelectionNamespaces);

  { Se infElement possui prefixo o mesmo tem que ser removido }
  if Pos(':', infElement) > 0 then
    infElement := Copy(infElement, Pos(':', infElement) +1, Length(infElement));

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
    if (infNode^.name = InfElement) and Assigned(infNode^.parent) and (infNode^.parent^.name <> '') then
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
  if not XmlSecNodeWasFound(SignNode, SignatureNode, SelectionNamespaces) then
  begin
    { Não é o ultimo nó do infNode... então, vamos procurar por um Nó dentro de infNode }
    SignNode := XmlSecLookUpNode(infNode, SignatureNode, SelectionNamespaces );

    { Se ainda não achamos, vamos procurar novamente a partir do elemento Raiz  }
    if (SignNode = nil) then
    begin
      SignNode := rootNode^.last;
      if not XmlSecNodeWasFound(SignNode, SignatureNode, SelectionNamespaces) then
        SignNode := XmlSecLookUpNode(rootNode, SignatureNode, SelectionNamespaces );
    end;
  end;

  if (SignNode = nil) then
    raise EACBrDFeException.Create(cErrFindSignNode);

  Result := SignNode;
end;

procedure VerificarValoresPadrao(var SignatureNode: AnsiString;
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

function XmlSecNodeWasFound(ANode: xmlNodePtr;
  NodeName: AnsiString; NameSpace: AnsiString): Boolean;
begin
  Result := (ANode <> nil) and
            (ANode^.name = NodeName) and
            ( (NameSpace = '') or (ANode^.ns^.href = NameSpace) );
end;

function XmlSecLookUpNode(ParentNode: xmlNodePtr;
  NodeName: AnsiString; NameSpace: AnsiString): xmlNodePtr;

  function _XmlSecLookUpNode(ParentNode: xmlNodePtr;
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
      ChildNode := FoundNode^.children;
      NextNode  := FoundNode^.next;
      { Faz Chamada recursiva para o novo Filho }
      FoundNode := _XmlSecLookUpNode(ChildNode, NodeName, NameSpace);

      if FoundNode = Nil then
        FoundNode := NextNode;
    end;

    Result := FoundNode;
  end;

begin
  Result := ParentNode;
  if (ParentNode = Nil) or (Trim(NodeName) = '') then
    Exit;

  { Primeiro vamos ver se o nó Raiz já não é o que precisamos }
  if XmlSecNodeWasFound(ParentNode, NodeName, NameSpace) then
    Exit;

  { Chama função auxiliar, que usa busca recursiva em todos os nós filhos }
  Result := _XmlSecLookUpNode(ParentNode^.children, NodeName, NameSpace);
end;


{ TDFeSSLXmlSignXmlSec }

constructor TDFeSSLXmlSignXmlSec.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create(ADFeSSL);
  FdsigCtx := nil;
end;

destructor TDFeSSLXmlSignXmlSec.Destroy;
begin
  DestroyCtx;
  inherited Destroy;
end;

procedure TDFeSSLXmlSignXmlSec.CreateCtx;
var
  MS: TMemoryStream;
  UsarDadosPFX: Boolean;
  PfxData: String;
  {$IfDef USE_MSCRYPO}
  Ret: LongInt;
  xKeyDataPtr, x509Data: xmlSecKeyDataPtr;
  CertContext, CertContext2: PCCERT_CONTEXT;
  phCryptProvOrNCryptKey: HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
  {$EndIf}
begin
  InitXmlSec;
  // Se FdsigCtx já existia, destrua e crie um novo //
  DestroyCtx;

  with FpDFeSSL do
  begin
    FpDFeSSL.CarregarCertificadoSeNecessario;

    { create signature context }
    FdsigCtx := xmlSecDSigCtxCreate(nil);
    if (FdsigCtx = nil) then
      raise EACBrDFeException.Create(cErrCtxCreate);

    UsarDadosPFX := True;

    {$IfDef USE_MSCRYPO}
     if (FpDFeSSL.DadosPFX = '') and UseMSCrypto then
     begin
       try
         CertContext := PCCERT_CONTEXT( CertContextWinApi );

         x509Data := xmlSecKeyDataCreate(xmlSecKeyDataX509GetKlass);
         CertContext2 := CertDuplicateCertificateContext(CertContext);
         Ret := xmlSecMSCryptoKeyDataX509AdoptCert(x509Data, CertContext2);
         CertContext2 := nil;

         CertContext2 := CertDuplicateCertificateContext(CertContext);
         Ret := xmlSecMSCryptoKeyDataX509AdoptKeyCert(x509Data, CertContext2);
         CertContext2 := nil;

         CertContext2 := CertDuplicateCertificateContext(CertContext);
         xKeyDataPtr := nil;
         xKeyDataPtr := xmlSecMSCryptoCertAdopt(CertContext2, xmlSecKeyDataTypePrivate);
         CertContext2 := Nil;

         if Assigned(xKeyDataPtr) then
         begin
           FdsigCtx^.signKey := xmlSecKeyCreate();
           Ret := xmlSecKeySetValue(FdsigCtx^.signKey, xKeyDataPtr);
           Ret := xmlSecKeyAdoptData(FdsigCtx^.signKey, x509Data);
           Ret := xmlSecKeySetName(FdsigCtx^.signKey, PAnsiChar('ACBrKey') );
           UsarDadosPFX := False;
         end;
       except
         UsarDadosPFX := True;
         raise;
       end;
     end;
    {$EndIf}

    if UsarDadosPFX then
    begin
      PfxData := FpDFeSSL.SSLCryptClass.CertPFXData;
      if PfxData = '' then
        raise EACBrDFeException.Create(cErrNoPFxData);

      MS := TMemoryStream.Create;
      try
        WriteStrToStream(MS, PfxData );

        // Aparentemente o método abaixo não funciona com "mscrypto" //
        MS.Position := 0;
        FdsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory( MS.Memory, MS.Size,
                                                           xmlSecKeyDataFormatPkcs12,
                                                           PAnsiChar(Senha), nil, nil);

        if (FdsigCtx^.signKey = nil) then
          raise EACBrDFeException.Create(cErrPrivKeyLoad);

      finally
        MS.Free;
      end;
    end;
  end;
end;

procedure TDFeSSLXmlSignXmlSec.DestroyCtx;
begin
  if (FdsigCtx <> nil) then
  begin
    InitXmlSec;
    xmlSecDSigCtxDestroy(FdsigCtx);
    FdsigCtx := nil;
  end;
end;

function TDFeSSLXmlSignXmlSec.XmlSecSign(const ConteudoXML: AnsiString; SignatureNode,
  SelectionNamespaces, InfElement: AnsiString): AnsiString;
var
  doc: xmlDocPtr;
  SignNode: xmlNodePtr;
  buffer: PAnsiChar;
  bufSize, SignResult: integer;
  xmlsecMsg: PAnsiChar;
begin
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
    begin
      xmlsecMsg := xmlSecErrorsGetMsg(2);
      raise EACBrDFeException.CreateFmt(cErrDSigSign + sLineBreak + xmlsecMsg, [SignResult]);
    end;

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


function TDFeSSLXmlSignXmlSec.Assinar(const ConteudoXML, docElement,
  infElement: String; SignatureNode: String; SelectionNamespaces: String;
  IdSignature: String; IdAttr: String): String;
var
  AXml, XmlAss, DTD: String;
  TemDeclaracao: Boolean;
begin
  // Nota: "ConteudoXML" já deve estar convertido para UTF8 //
  XmlAss := '';

  // Verificando se possui a Declaração do XML, se não possuir, adiciona para OpenSSL compreender o Encoding
  TemDeclaracao := XmlEhUTF8(ConteudoXML);
  if not TemDeclaracao then
    AXml := CUTF8DeclaracaoXML + RemoverDeclaracaoXML(ConteudoXML)
  else
    AXml := ConteudoXML;

  if infElement <> '' then
  begin
    IdAttr := IfEmptyThen(IdAttr, 'Id');

    DTD := StringReplace(cDTD, '&infElement&', infElement, []);
    DTD := StringReplace( DTD, '&IdAttribute&', IdAttr, []);

    AXml := InserirDTD(AXml, DTD);
  end;

  // Inserindo Template da Assinatura digital //
  if (not XmlEstaAssinado(AXml)) or (SignatureNode <> '') then
    AXml := AdicionarSignatureElement(AXml, True, docElement, IdSignature, IdAttr);

  // Assinando com XMLSec //
  //DEBUG
  //WriteToTXT('C:\TEMP\XmlToSign.xml', AXml, False, False);

  XmlAss := XmlSecSign(AXml, AnsiString(SignatureNode),
                             AnsiString(SelectionNamespaces),
                             AnsiString(infElement));

  //DEBUG
  //WriteToTXT('C:\TEMP\XmlSigned1.xml', XmlAss, False, False);

  if not TemDeclaracao then
    XmlAss := RemoverDeclaracaoXML(XmlAss);

  XmlAss := AjustarXMLAssinado(XmlAss, FpDFeSSL.DadosCertificado.DERBase64);

  //DEBUG
  //WriteToTXT('C:\TEMP\XmlSigned2.xml', XmlAss, False, False);

  // Removendo DTD //
  Result := StringReplace(XmlAss, DTD, '', []);
end;

function TDFeSSLXmlSignXmlSec.Validar(const ConteudoXML, ArqSchema: String; out
  MsgErro: String): Boolean;
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

function TDFeSSLXmlSignXmlSec.VerificarAssinatura(const ConteudoXML: String;
  out MsgErro: String; const infElement: String; SignatureNode: String;
  SelectionNamespaces: String; IdSignature: String;
  IdAttr: String): Boolean;
var
  doc: xmlDocPtr;
  SignNode: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  mngr: xmlSecKeysMngrPtr;
  AXml, X509Certificate, DTD: String;
  asSignatureNode, asSelectionNamespaces: AnsiString;
  MS: TMemoryStream;
begin
  InitXmlSec;

  asSignatureNode       := AnsiString(SignatureNode);
  asSelectionNamespaces := AnsiString(SelectionNamespaces);
  VerificarValoresPadrao(asSignatureNode, asSelectionNamespaces);

  Result := False;
  AXml := ConteudoXML;
  X509Certificate := LerTagXML(AXml, 'X509Certificate' );

  if infElement <> '' then
  begin
    IdAttr := IfEmptyThen(IdAttr, 'Id');

    DTD := StringReplace(cDTD, '&infElement&', infElement, []);
    DTD := StringReplace( DTD, '&IdAttribute&', IdAttr, []);

    AXml := InserirDTD(AXml, DTD);
  end;

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

    Result := (dsigCtx^.status = xmlSecDSigStatusSucceeded);
    if not Result then
      MsgErro := ACBrStr(cErrDSigInvalid);

  finally
    { cleanup }
    MS.Free;

    if (dsigCtx <> nil) then
      xmlSecDSigCtxDestroy(dsigCtx);

    if (doc <> nil) then
      xmlFreeDoc(doc);
  end;
end;

function TDFeSSLXmlSignXmlSec.InserirDTD(AXml: String; const DTD: String): String;
var
  I: integer;
begin
  // Adicionando Cabeçalho DTD, necessário para xmlsec encontrar o ID //
  I := pos('?>', AXml);
  Result := Copy(AXml, 1, IfThen(I > 0, I + 1, I)) +
            DTD +
            Copy(AXml, IfThen(I > 0, I + 2, I), Length(AXml));
end;

{$IfDef USE_MSCRYPO}
function TDFeSSLXmlSignXmlSec.UseMSCrypto: Boolean;
begin
   Result := SSLUsaMSCrypto and xmlSecUseMSCrypto and (XMLSecLoaded = cCryptLibMSCrypto)
end;

function TDFeSSLXmlSignXmlSec.SSLUsaMSCrypto: Boolean;
begin
  Result := (FpDFeSSL.SSLCryptLib in [cryWinCrypt, cryCapicom]);
end;
{$EndIf}

procedure TDFeSSLXmlSignXmlSec.InitXmlSec;
var
  XMLSecCryptoLib: String;
begin
  if XMLSecLoaded <> '' then Exit;

  {$IfDef USE_MSCRYPO}
   if SSLUsaMSCrypto then
     XMLSecCryptoLib := cCryptLibMSCrypto
   else
     XMLSecCryptoLib := cCryptLibOpenSSL;
  {$Else}
   XMLSecCryptoLib := cCryptLibOpenSSL;
  {$EndIf}

  try
    ACBrDFeXsXmlSec.InitXmlSec( XMLSecCryptoLib );
    XMLSecCryptoLib := '';
  except
    On E: Exception do
    begin
      if (pos( cCryptLibMSCrypto, E.Message ) > 0) then
        XMLSecCryptoLib := cCryptLibOpenSSL
      else
        raise;
    end;
  end;

  if XMLSecCryptoLib <> '' then
    ACBrDFeXsXmlSec.InitXmlSec( XMLSecCryptoLib );
end;


initialization
  XMLSecLoaded := '';
  {$IfDef USE_MSCRYPO}
   libXmlSecMsCryptoHandle := 0;
  {$EndIf}

finalization;
  ShutDownXmlSec;

end.


