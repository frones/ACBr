{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  André Ferreira de Moraes                       }
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
{ .$Define USE_MSCRYPO }
{ Ligue a diretiva acima, para usar libxmlsec-mscrypto.dll, ou seja, a XMLSec
  usará a MSCrypto ao invez do OpenSSL, para efeturar a assintura digital do XML.
  (ainda em desenvolvimento)
  Isso permitirá que a XMLSec, acesse certificados A3, por exemplo...
}

{$IFNDEF MSWINDOWS}
{$UNDEF USE_MSCRYPO}
{$ENDIF}
unit ACBrDFeXsXmlSec;

interface

uses
{$IFDEF DELPHIXE4_UP}
  AnsiStrings,
{$ENDIF}
{$IFDEF USE_MSCRYPO}
  windows, ACBr_WinCrypt,
{$IFDEF FPC}
  DynLibs,
{$ENDIF}
{$ENDIF}
  Classes, SysUtils,
  ACBrDFeSSL, ACBrDFeException,
  libxmlsec, libxml2;

const
{$IFDEF USE_MSCRYPO}
{$IFDEF USE_MINGW}
  LIBXMLSEC_MSCRYPTO_SO = 'libxmlsec1-mscrypto.dll';
{$ELSE}
  LIBXMLSEC_MSCRYPTO_SO = 'libxmlsec-mscrypto.dll';
{$ENDIF}
{$ENDIF}
  cDTD = '<!DOCTYPE test [<!ATTLIST &infElement& &IdAttribute& ID #IMPLIED>]>';

  cCryptLibMSCrypto = 'mscrypto';
  cCryptLibOpenSSL = 'openssl';

  cErrMngrCreate = 'Erro: Falha ao criar Gerenciador de Chaves "xmlSecKeysMngrCreate"';
  cErrMngrInit = 'Erro: Falha ao inicializar o Gerenciador de Chaves "xmlSecCryptoAppDefaultKeysMngrInit"';
  cErrCertLoad = 'Erro: Falha ao ler informação do Certificado no Gerenciador de Chaves';
  cErrCtxCreate = 'Erro: Falha ao criar Ctx "xmlSecDSigCtxCreate"';
  cErrNoPFxData = 'Erro: Falha ao ler DadosPFX';
  cErrPrivKeyLoad = 'Erro: Falha ao ler a Chave Privada de DadosPFX';
  cErrPubKeyLoad = 'Erro: Falha ao ler a Chave Publica do Certificado';
  cErrDSigSign = 'Erro %d: Falha ao assinar o Documento';
  cErrDSigVerify = 'Erro: Falha na verificação da Assinatura';
  cErrDSigInvalid = 'Erro: Assinatura é inválida';

  cErrXmlSecInit = 'Falha ao inicializar Biblioteca XMLSec.';
  cErrXmlSecWrongVersion = 'Essa versão da Biblioteca XMLSec não é compatível com o ACBr.';
  cErrXmlSecLoadCriptoLib = 'Falha ao carregar biblioteca de Criptografia do XMLSec [%s]';
  cErrXmlSecInitCriptoLib = 'Falha ao inicializar a Biblioteca de Criptografia do XMLSec [%s]';

  cErrParseDoc = 'Erro: Falha ao interpretar o XML "xmlParseDoc"';
  cErrFindSignNode = 'Erro: Falha ao localizar o nó de Assinatura';
  cErrElementsNotFound = 'Nenhum elemento encontrado';

type

  { TDFeSSLXmlSignXmlSec }

  TDFeSSLXmlSignXmlSec = class(TDFeSSLXmlSignClass)
  private
    FdsigCtx: xmlSecDSigCtxPtr;

    function AdicionarNode(var aDoc: xmlDocPtr; const ConteudoXML: String;
      docElement: String): xmlNodePtr;
    function InserirDTD(const AXml: String; const DTD: String): String;
{$IFDEF USE_MSCRYPO}
    function UseMSCrypto: Boolean;
    function SSLUsaMSCrypto: Boolean;
{$ENDIF}
    procedure InitXmlSec;
    procedure CreateCtx;
    procedure DestroyCtx;

    function LibXmlFindSignatureNode(aDoc: xmlDocPtr;
      const SignatureNode: String; const SelectionNamespaces: String;
      infElement: String): xmlNodePtr;
    function LibXmlLookUpNode(ParentNode: xmlNodePtr; const NodeName: String;
      const NameSpace: String = ''): xmlNodePtr;
    function LibXmlNodeWasFound(ANode: xmlNodePtr; const NodeName: String;
      const NameSpace: String): boolean;
    function XmlSecSign(aDoc: xmlDocPtr;
      const SignatureNode, SelectionNamespaces: string;
      const InfElement, URI, IdSignature, docElement: String;
      const IdSignatureValue: string = ''): String;
    procedure VerificarValoresPadrao(var SignatureNode: String;
      var SelectionNamespaces: String);
  public
    constructor Create(ADFeSSL: TDFeSSL); override;
    destructor Destroy; override;

    function Assinar(const ConteudoXML, docElement, InfElement: String;
      const SignatureNode: String = ''; const SelectionNamespaces: String = '';
      const IdSignature: String = ''; const IdAttr: String = '';
      const IdSignatureValue: string = ''): String; override;
    function Validar(const ConteudoXML, ArqSchema: String; out MsgErro: String)
      : Boolean; override;
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const InfElement: String; const SignatureNode: String = '';
      const SelectionNamespaces: String = ''; const IdSignature: String = '';
      const IdAttr: String = ''): Boolean; override;
  end;

procedure InitXmlSec(XMLSecCryptoLib: String);
procedure ShutDownXmlSec;

{$IFDEF USE_MSCRYPO}

type
  TxmlSecMSCryptoAppInit = function(config: PAnsiChar): LongInt; cdecl;
  TxmlSecMSCryptoCertAdopt = function(pCert: PCCERT_CONTEXT;
    type_: xmlSecKeyDataType): xmlSecKeyDataPtr; cdecl;
  TxmlSecMSCryptoKeyDataX509AdoptCert = function(data: xmlSecKeyDataPtr;
    pCert: PCCERT_CONTEXT): LongInt; cdecl;
  TxmlSecMSCryptoKeyDataX509AdoptKeyCert = function(data: xmlSecKeyDataPtr;
    pCert: PCCERT_CONTEXT): LongInt; cdecl;

var
  libXmlSecMsCryptoHandle: TLibHandle;
  _xmlSecMSCryptoAppInit: TxmlSecMSCryptoAppInit = nil;
  _xmlSecMSCryptoCertAdopt: TxmlSecMSCryptoCertAdopt = nil;
  _xmlSecMSCryptoKeyDataX509AdoptCert
    : TxmlSecMSCryptoKeyDataX509AdoptCert = nil;
  _xmlSecMSCryptoKeyDataX509AdoptKeyCert
    : TxmlSecMSCryptoKeyDataX509AdoptKeyCert = nil;

function xmlSecMSCryptoAppInit(config: PAnsiChar): LongInt; cdecl;
function xmlSecMSCryptoCertAdopt(pCert: PCCERT_CONTEXT;
  type_: xmlSecKeyDataType): xmlSecKeyDataPtr; cdecl;
function xmlSecMSCryptoKeyDataX509AdoptCert(data: xmlSecKeyDataPtr;
  pCert: PCCERT_CONTEXT): LongInt; cdecl;
function xmlSecMSCryptoKeyDataX509AdoptKeyCert(data: xmlSecKeyDataPtr;
  pCert: PCCERT_CONTEXT): LongInt; cdecl;

function xmlSecUseMSCrypto: Boolean;
{$ENDIF}

implementation

Uses
  strutils, math,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrConsts,
  synautil, synacode,
  ACBrDFeUtil;

var
  XMLSecLoaded: String;

procedure InitXmlSec(XMLSecCryptoLib: String);
begin
  if (XMLSecLoaded <> '') then
    Exit;

  { Init libxml and libxslt libraries }
  libxml2.Init;

{$IFDEF USE_MSCRYPO}
  if XMLSecCryptoLib = cCryptLibMSCrypto then
  begin
    libXmlSecMsCryptoHandle := LoadLibrary(LIBXMLSEC_MSCRYPTO_SO);

    if libXmlSecMsCryptoHandle <> 0 then
    begin
      _xmlSecMSCryptoAppInit := GetProcAddress(libXmlSecMsCryptoHandle,
        'xmlSecMSCryptoAppInit');
      _xmlSecMSCryptoCertAdopt := GetProcAddress(libXmlSecMsCryptoHandle,
        'xmlSecMSCryptoCertAdopt');
      _xmlSecMSCryptoKeyDataX509AdoptCert :=
        GetProcAddress(libXmlSecMsCryptoHandle,
        'xmlSecMSCryptoKeyDataX509AdoptCert');
      _xmlSecMSCryptoKeyDataX509AdoptKeyCert :=
        GetProcAddress(libXmlSecMsCryptoHandle,
        'xmlSecMSCryptoKeyDataX509AdoptKeyCert');
    end;

    xmlSecMSCryptoAppInit(nil);
  end;
{$ELSE}
  if XMLSecCryptoLib = cCryptLibMSCrypto then
    XMLSecCryptoLib := cCryptLibOpenSSL;
{$ENDIF}
  libxmlsec.Init;

  { Init xmlsec library }
  if (xmlSecInit() < 0) then
    raise EACBrDFeException.Create(cErrXmlSecInit);

  { Check loaded library version }
  if (xmlSecCheckVersionExt(1, 2, 20, xmlSecCheckVersionABICompatible) <> 1)
  then
    raise EACBrDFeException.Create(cErrXmlSecWrongVersion);

  (* Load default crypto engine if we are supporting dynamic
    * loading for xmlsec-crypto libraries. Use the crypto library
    * name ("openssl", "nss", etc.) to load corresponding
    * xmlsec-crypto library.
  *)
  if (xmlSecCryptoDLLoadLibrary(PAnsiChar(AnsiString(XMLSecCryptoLib))) < 0)
  then
    raise EACBrDFeException.CreateFmt(cErrXmlSecLoadCriptoLib,
      [XMLSecCryptoLib]);

  { Init crypto library }
  if (xmlSecCryptoAppInit(nil) < 0) then
    raise EACBrDFeException.CreateFmt(cErrXmlSecInitCriptoLib,
      [XMLSecCryptoLib]);

  { Init xmlsec-crypto library }
  if (xmlSecCryptoInit() < 0) then
    raise EACBrDFeException.CreateFmt(cErrXmlSecInitCriptoLib,
      [XMLSecCryptoLib]);

  XMLSecLoaded := XMLSecCryptoLib;
end;

procedure ShutDownXmlSec;
begin
  if (XMLSecLoaded = '') then
    Exit;

  { Shutdown xmlsec-crypto library }
  xmlSecCryptoShutdown();

  { Shutdown crypto library }
  xmlSecCryptoAppShutdown();

  { Shutdown xmlsec library }
  xmlSecShutdown();

  { Shutdown libxslt/libxml }
  //LibXmlShutDown();

  XMLSecLoaded := '';

{$IFDEF USE_MSCRYPO}
  if libXmlSecMsCryptoHandle <> 0 then
  begin
    FreeLibrary(libXmlSecMsCryptoHandle);
    libXmlSecMsCryptoHandle := 0;
  end;
{$ENDIF}
end;

{$IFDEF USE_MSCRYPO}

function xmlSecMSCryptoAppInit(config: PAnsiChar): LongInt; cdecl;
begin
  if not Assigned(_xmlSecMSCryptoAppInit) then
    Result := -1
  else
    Result := _xmlSecMSCryptoAppInit(config);
end;

function xmlSecMSCryptoCertAdopt(pCert: PCCERT_CONTEXT;
  type_: xmlSecKeyDataType): xmlSecKeyDataPtr; cdecl;
begin
  if not Assigned(_xmlSecMSCryptoCertAdopt) then
    Result := Nil
  else
    Result := _xmlSecMSCryptoCertAdopt(pCert, type_);
end;

function xmlSecMSCryptoKeyDataX509AdoptCert(data: xmlSecKeyDataPtr;
  pCert: PCCERT_CONTEXT): LongInt; cdecl;
begin
  if not Assigned(_xmlSecMSCryptoKeyDataX509AdoptCert) then
    Result := -1
  else
    Result := _xmlSecMSCryptoKeyDataX509AdoptCert(data, pCert);
end;

function xmlSecMSCryptoKeyDataX509AdoptKeyCert(data: xmlSecKeyDataPtr;
  pCert: PCCERT_CONTEXT): LongInt; cdecl;
begin
  if not Assigned(_xmlSecMSCryptoKeyDataX509AdoptKeyCert) then
    Result := -1
  else
    Result := _xmlSecMSCryptoKeyDataX509AdoptKeyCert(data, pCert);
end;

function xmlSecUseMSCrypto: Boolean;
begin
  Result := (libXmlSecMsCryptoHandle <> 0);
end;
{$ENDIF}
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
{$IFDEF USE_MSCRYPO}
  Ret: LongInt;
  xKeyDataPtr, x509Data: xmlSecKeyDataPtr;
  CertContext, CertContext2: PCCERT_CONTEXT;
  phCryptProvOrNCryptKey: HCRYPTPROV_OR_NCRYPT_KEY_HANDLE;
{$ENDIF}
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

{$IFDEF USE_MSCRYPO}
    if (FpDFeSSL.DadosPFX = '') and UseMSCrypto then
    begin
      try
        CertContext := PCCERT_CONTEXT(CertContextWinApi);

        x509Data := xmlSecKeyDataCreate(xmlSecKeyDataX509GetKlass);
        CertContext2 := CertDuplicateCertificateContext(CertContext);
        Ret := xmlSecMSCryptoKeyDataX509AdoptCert(x509Data, CertContext2);
        CertContext2 := nil;

        CertContext2 := CertDuplicateCertificateContext(CertContext);
        Ret := xmlSecMSCryptoKeyDataX509AdoptKeyCert(x509Data, CertContext2);
        CertContext2 := nil;

        CertContext2 := CertDuplicateCertificateContext(CertContext);
        xKeyDataPtr := nil;
        xKeyDataPtr := xmlSecMSCryptoCertAdopt(CertContext2,
          xmlSecKeyDataTypePrivate);
        CertContext2 := Nil;

        if Assigned(xKeyDataPtr) then
        begin
          FdsigCtx^.signKey := xmlSecKeyCreate();
          Ret := xmlSecKeySetValue(FdsigCtx^.signKey, xKeyDataPtr);
          Ret := xmlSecKeyAdoptData(FdsigCtx^.signKey, x509Data);
          Ret := xmlSecKeySetName(FdsigCtx^.signKey, PAnsiChar('ACBrKey'));
          UsarDadosPFX := False;
        end;
      except
        UsarDadosPFX := True;
        raise;
      end;
    end;
{$ENDIF}
    if UsarDadosPFX then
    begin
      PfxData := FpDFeSSL.SSLCryptClass.CertPFXData;
      if PfxData = '' then
        raise EACBrDFeException.Create(cErrNoPFxData);

      MS := TMemoryStream.Create;
      try
        WriteStrToStream(MS, PfxData);

        // Aparentemente o método abaixo não funciona com "mscrypto" //
        MS.Position := 0;
        FdsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory(MS.Memory, MS.Size,
          xmlSecKeyDataFormatPkcs12, PAnsiChar(Senha), nil, nil);

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

function TDFeSSLXmlSignXmlSec.XmlSecSign(aDoc: xmlDocPtr;
  const SignatureNode, SelectionNamespaces: string;
  const InfElement, URI, IdSignature, docElement: String;
  const IdSignatureValue: string): String;
var
  SignNode: xmlNodePtr;
  buffer: PAnsiChar;
  bufSize, SignResult: integer;
  xmlsecMsg: PAnsiChar;
begin
  Result := '';

  CreateCtx;
  try
    // Inserindo Template da Assinatura digital //
    SignNode := LibXmlFindSignatureNode(aDoc, SignatureNode, SelectionNamespaces,
                                        infElement);

    if (SignNode = nil) then
      SignNode := AdicionarNode(aDoc, SignatureElement(URI, True, IdSignature,
                               FpDFeSSL.SSLDgst, IdSignatureValue), docElement);

    { sign the template }
    SignResult := xmlSecDSigCtxSign(FdsigCtx, SignNode);
    if (SignResult < 0) then
    begin
      xmlsecMsg := xmlSecErrorsGetMsg(2);
      raise EACBrDFeException.CreateFmt(cErrDSigSign + sLineBreak + xmlsecMsg,
        [SignResult]);
    end;

    { print signed document to stdout }
    // xmlDocDump(stdout, doc);
    // Can't use "stdout" from Delphi, so we'll use xmlDocDumpMemory instead...
    buffer := nil;
    xmlDocDumpMemory(aDoc, @buffer, @bufSize);
    if (buffer <> nil) then
      { success }
      Result := String(buffer);
  finally
    { cleanup }
    if (buffer <> nil) then
      xmlFree(buffer);

    DestroyCtx;
  end;
end;

function TDFeSSLXmlSignXmlSec.Assinar(const ConteudoXML, docElement,
  InfElement: String; const SignatureNode: String; const SelectionNamespaces: String;
  const IdSignature: String; const IdAttr: String;
  const IdSignatureValue: string): String;
var
  doc: xmlDocPtr;
  AXml, XmlAss, DTD, URI: String;
  TemDeclaracao: Boolean;
  IdAttr_temp: string;
begin
  InitXmlSec;

  // Nota: "ConteudoXML" já deve estar convertido para UTF8 //
  XmlAss := '';
  IdAttr_temp := IfEmptyThen(IdAttr, 'Id');

  // Verificando se possui a Declaração do XML, se não possuir, adiciona para OpenSSL compreender o Encoding
  TemDeclaracao := XmlEhUTF8(ConteudoXML);
  if not TemDeclaracao then
    AXml := CUTF8DeclaracaoXML + RemoverDeclaracaoXML(ConteudoXML)
  else
    AXml := ConteudoXML;

  if (InfElement <> '') then
  begin
    DTD := StringReplace(cDTD, '&infElement&', InfElement, []);
    DTD := StringReplace(DTD, '&IdAttribute&', IdAttr_temp, []);

    AXml := InserirDTD(AXml, DTD);
  end;

  URI := EncontrarURI(aXML, docElement, IdAttr_temp);

  { load template }
  doc := xmlParseDoc(PAnsiChar(AnsiString(AXml)));
  if (doc = nil) then
    raise EACBrDFeException.Create(cErrParseDoc);

  try
    // Assinando com XMLSec //
    // DEBUG
    // WriteToTXT('C:\TEMP\XmlToSign.xml', AXml, False, False);

    XmlAss := XmlSecSign(doc, SignatureNode, SelectionNamespaces, InfElement,
                         URI, IdSignature, docElement, IdSignatureValue);

    // DEBUG
    // WriteToTXT('C:\TEMP\XmlSigned1.xml', XmlAss, False, False);

    if not TemDeclaracao then
      XmlAss := RemoverDeclaracaoXML(XmlAss);

    XmlAss := AjustarXMLAssinado(XmlAss, FpDFeSSL.DadosCertificado.DERBase64);

    // DEBUG
    // WriteToTXT('C:\TEMP\XmlSigned2.xml', XmlAss, False, False);

    // Removendo DTD //
    Result := StringReplace(XmlAss, DTD, '', []);
  finally
    xmlFreeDoc(doc);
  end;
end;

function TDFeSSLXmlSignXmlSec.Validar(const ConteudoXML, ArqSchema: String;
  out MsgErro: String): Boolean;
var
  doc, schema_doc: xmlDocPtr;
  parser_ctxt: xmlSchemaParserCtxtPtr;
  schema: xmlSchemaPtr;
  valid_ctxt: xmlSchemaValidCtxtPtr;
  schemError: xmlErrorPtr;
  AXml: String;
begin
  InitXmlSec;

  Result := False;
  doc := Nil;
  schema_doc := Nil;
  parser_ctxt := Nil;
  schema := Nil;
  valid_ctxt := Nil;

  try
    AXml := ConteudoXML;
    doc := xmlParseDoc(PAnsiChar(AnsiString(AXml)));
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
    begin
      MsgErro := 'Erro: unable to parse';
      Exit;
    end;

    schema_doc := xmlReadFile(PAnsiChar(AnsiString(ArqSchema)), nil,
      XML_DETECT_IDS);
    // the schema cannot be loaded or is not well-formed
    if (schema_doc = nil) then
    begin
      MsgErro := 'Erro: Schema não pode ser carregado ou está corrompido';
      Exit;
    end;

    parser_ctxt := xmlSchemaNewDocParserCtxt(schema_doc);
    // unable to create a parser context for the schema */
    if (parser_ctxt = nil) then
    begin
      MsgErro := 'Erro: unable to create a parser context for the schema';
      Exit;
    end;

    schema := xmlSchemaParse(parser_ctxt);
    // the schema itself is not valid
    if (schema = nil) then
    begin
      MsgErro := 'Error: the schema itself is not valid';
      Exit;
    end;

    valid_ctxt := xmlSchemaNewValidCtxt(schema);
    // unable to create a validation context for the schema */
    if (valid_ctxt = nil) then
    begin
      MsgErro := 'Error: unable to create a validation context for the schema';
      Exit;
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
  out MsgErro: String; const InfElement: String; const SignatureNode: String;
  const SelectionNamespaces: String; const IdSignature: String; const IdAttr: String): Boolean;
var
  doc: xmlDocPtr;
  SignNode: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  mngr: xmlSecKeysMngrPtr;
  AXml, X509Certificate, DTD: String;
  asSignatureNode, asSelectionNamespaces: String;
  MS: TMemoryStream;
  IdAttr_temp: string;
begin
  InitXmlSec;

  asSignatureNode := SignatureNode;
  asSelectionNamespaces := SelectionNamespaces;
  VerificarValoresPadrao(asSignatureNode, asSelectionNamespaces);

  Result := False;
  AXml := ConteudoXML;
  X509Certificate := LerTagXML(AXml, 'X509Certificate');

  if InfElement <> '' then
  begin
    IdAttr_temp := IfEmptyThen(IdAttr, 'Id');

    DTD := StringReplace(cDTD, '&infElement&', InfElement, []);
    DTD := StringReplace(DTD, '&IdAttribute&', IdAttr_temp, []);

    AXml := InserirDTD(AXml, DTD);
  end;

  doc := nil;
  dsigCtx := nil;

  MS := TMemoryStream.Create;
  try
    WriteStrToStream(MS, DecodeBase64(X509Certificate));
    // DEBUG
    // MS.Position := 0;
    // MS.SaveToFile('c:\temp\cert.der');

    mngr := xmlSecKeysMngrCreate();
    if (mngr = nil) then
    begin
      MsgErro := ACBrStr(cErrMngrCreate);
      Exit;
    end;

    if xmlSecCryptoAppDefaultKeysMngrInit(mngr) < 0 then
    begin
      MsgErro := ACBrStr(cErrMngrInit);
      Exit;
    end;

    { Load the Certificate }
    MS.Position := 0;
    if (xmlSecCryptoAppKeysMngrCertLoadMemory(mngr, MS.Memory, MS.Size,
      xmlSecKeyDataFormatCertDer, xmlSecKeyDataTypeTrusted) < 0) then
    begin
      MsgErro := ACBrStr(cErrCertLoad);
      Exit;
    end;

    doc := xmlParseDoc(PAnsiChar(AnsiString(AXml)));
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
    begin
      MsgErro := ACBrStr(cErrParseDoc);
      Exit;
    end;

    { Achando o nó da Assinatura }
    SignNode := LibXmlFindSignatureNode(doc, asSignatureNode, asSelectionNamespaces, InfElement);
    if (SignNode = nil) or (SignNode.Name <> asSignatureNode) then
    begin
       MsgErro := ACBrStr(cErrFindSignNode);
       Exit;
    end;

    dsigCtx := xmlSecDSigCtxCreate(mngr);
    if (dsigCtx = nil) then
    begin
      MsgErro := ACBrStr(cErrCtxCreate);
      Exit;
    end;

    MS.Position := 0;
    dsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory(MS.Memory, MS.Size,
      xmlSecKeyDataFormatCertDer, nil, nil, nil);
    if (dsigCtx^.signKey = nil) then
    begin
      MsgErro := ACBrStr(cErrPubKeyLoad);
      Exit;
    end;

    { Verify signature }
    if (xmlSecDSigCtxVerify(dsigCtx, SignNode) < 0) then
    begin
      MsgErro := ACBrStr(cErrDSigVerify);
      Exit;
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

function TDFeSSLXmlSignXmlSec.InserirDTD(const AXml: String;
  const DTD: String): String;
var
  I: integer;
begin
  // Adicionando Cabeçalho DTD, necessário para xmlsec encontrar o ID //
  I := pos('?>', AXml);
  Result := Copy(AXml, 1, IfThen(I > 0, I + 1, I)) + DTD +
    Copy(AXml, IfThen(I > 0, I + 2, I), Length(AXml));
end;

function TDFeSSLXmlSignXmlSec.LibXmlFindSignatureNode(aDoc: xmlDocPtr;
  const SignatureNode: String; const SelectionNamespaces: String; infElement: String
  ): xmlNodePtr;
var
  rootNode, infNode, infNodeParent, SignNode: xmlNodePtr;
  vSignatureNode, vSelectionNamespaces, vinfElement: String;
begin
  Result := nil;

  { Encontra o elemento Raiz }
  rootNode := xmlDocGetRootElement(aDoc);
  if (rootNode = nil) then
    Exit;

  vSignatureNode       := SignatureNode;
  vSelectionNamespaces := SelectionNamespaces;
  VerificarValoresPadrao(vSignatureNode, vSelectionNamespaces);
  infNode := nil;
  infNodeParent := nil;
  SignNode := nil;

  { Se infElement possui prefixo o mesmo tem que ser removido }
  vinfElement := copy(infElement, Pos(':', infElement) + 1, Length(infElement));

  { Se tem vinfElement, procura pelo mesmo. Isso permitirá acharmos o nó de
    assinatura, relacionado a ele (mesmo pai) }
  if (vinfElement <> '') then
  begin
    { Procura vinfElement em todos os nós, filhos de Raiz, usando LibXml }
    infNode := LibXmlLookUpNode(rootNode, vinfElement);

    if (infNode <> nil) then
    begin
      { Vamos achar o pai desse Elemento, pois com ele encontraremos a assinatura no final }
      if (infNode^.Name = vinfElement) and
         Assigned(infNode^.parent) and
         (infNode^.parent^.Name <> '') then
      begin
        infNodeParent := infNode^.parent;
      end;
    end;
  end

  { Procurando pelo nó de assinatura...}

  { Primeiro vamos verificar manualmente se é o último nó do Parent do infNode atual };
  if (infNodeParent <> nil) then
  begin
    SignNode := infNodeParent^.last;
    while (SignNode <> nil) and (SignNode <> infNode) and
      (not LibXmlNodeWasFound(SignNode, vSignatureNode, vSelectionNamespaces)) do
    begin
      SignNode := SignNode^.prev;
    end;

    if not LibXmlNodeWasFound(SignNode, vSignatureNode, vSelectionNamespaces) then
      SignNode := nil;
  end;

  { Não é o ultimo nó do infNode... então, vamos procurar por um Nó dentro de infNode }
  if (SignNode = nil) and (infNode <> nil) then
  begin
    SignNode := infNode^.next;
    while (SignNode <> nil)  and
      (not LibXmlNodeWasFound(SignNode, vSignatureNode, vSelectionNamespaces)) do
    begin
      SignNode := SignNode^.next;
    end;

    if not LibXmlNodeWasFound(SignNode, vSignatureNode, vSelectionNamespaces) then
      SignNode := nil;
  end;

  { Se ainda não achamos, vamos procurar novamente a partir do elemento Raiz }
  if (SignNode = nil) then
  begin
    SignNode := rootNode^.last;
    if not LibXmlNodeWasFound(SignNode, vSignatureNode, vSelectionNamespaces) then
    begin
      SignNode := rootNode^.next;
      while (SignNode <> nil)  and (not LibXmlNodeWasFound(SignNode, vSignatureNode, vSelectionNamespaces)) do
      begin
        SignNode := SignNode^.next;
      end;
    end;
  end;

  Result := SignNode;
end;

function TDFeSSLXmlSignXmlSec.LibXmlNodeWasFound(ANode: xmlNodePtr;
  const NodeName: String; const NameSpace: String): boolean;
begin
  Result := (ANode <> nil) and (ANode^.Name = NodeName) and
    ((NameSpace = '') or (ANode^.ns^.href = NameSpace));
end;

function TDFeSSLXmlSignXmlSec.LibXmlLookUpNode(ParentNode: xmlNodePtr;
  const NodeName: String; const NameSpace: String): xmlNodePtr;

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

function TDFeSSLXmlSignXmlSec.AdicionarNode(var aDoc: xmlDocPtr;
  const ConteudoXML: String; docElement: String): xmlNodePtr;
Var
  NewNode, DocNode: xmlNodePtr;
  memDoc: xmlDocPtr;
  NewNodeXml, vdocElement: String;
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
    DocNode := xmlDocGetRootElement(aDoc);

    { Se docElement possui prefixo o mesmo tem que ser removido }
    vdocElement := copy(docElement, Pos(':', docElement) + 1, Length(docElement));

    if (vdocElement <> '') then
      DocNode := LibXmlLookUpNode(DocNode, vdocElement);

    if (DocNode = nil) then
      raise EACBrDFeException.Create(cErrElementsNotFound);

    Result := xmlAddChildList(DocNode, NewNode.children);
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



{$IFDEF USE_MSCRYPO}

function TDFeSSLXmlSignXmlSec.UseMSCrypto: Boolean;
begin
  Result := SSLUsaMSCrypto and xmlSecUseMSCrypto and
    (XMLSecLoaded = cCryptLibMSCrypto)
end;

function TDFeSSLXmlSignXmlSec.SSLUsaMSCrypto: Boolean;
begin
  Result := (FpDFeSSL.SSLCryptLib in [cryWinCrypt, cryCapicom]);
end;
{$ENDIF}

procedure TDFeSSLXmlSignXmlSec.InitXmlSec;
var
  XMLSecCryptoLib: String;
begin
  if XMLSecLoaded <> '' then
    Exit;

{$IFDEF USE_MSCRYPO}
  if SSLUsaMSCrypto then
    XMLSecCryptoLib := cCryptLibMSCrypto
  else
    XMLSecCryptoLib := cCryptLibOpenSSL;
{$ELSE}
  XMLSecCryptoLib := cCryptLibOpenSSL;
{$ENDIF}
  try
    ACBrDFeXsXmlSec.InitXmlSec(XMLSecCryptoLib);
    XMLSecCryptoLib := '';
  except
    On E: Exception do
    begin
      if (pos(cCryptLibMSCrypto, E.message) > 0) then
        XMLSecCryptoLib := cCryptLibOpenSSL
      else
        raise;
    end;
  end;

  if XMLSecCryptoLib <> '' then
    ACBrDFeXsXmlSec.InitXmlSec(XMLSecCryptoLib);
end;

procedure TDFeSSLXmlSignXmlSec.VerificarValoresPadrao(var SignatureNode
  : String; var SelectionNamespaces: String);
var
  DSigNs: String;
begin
  if SignatureNode = '' then
    SignatureNode := xmlSecNodeSignature()
  else
    SignatureNode := Copy(SignatureNode, pos(':', SignatureNode) + 1,
      Length(SignatureNode));

  DSigNs := xmlSecDSigNs();

  if SelectionNamespaces = '' then
    SelectionNamespaces := DSigNs
  else
  begin
    SelectionNamespaces := RetornarConteudoEntre(SelectionNamespaces, '"', '"');

    if strutils.LeftStr(SelectionNamespaces, Length(DSigNs)) <> DSigNs then
      SelectionNamespaces := Trim(DSigNs + ' ' + SelectionNamespaces);
  end;
end;

initialization

XMLSecLoaded := '';
{$IFDEF USE_MSCRYPO}
libXmlSecMsCryptoHandle := 0;
{$ENDIF}

finalization

ShutDownXmlSec;

end.
