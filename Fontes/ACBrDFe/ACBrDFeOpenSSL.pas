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
  Classes, SysUtils,
  ACBrDFeConfiguracoes, ACBrDFeSSL,
  HTTPSend, ssl_openssl,
  libxmlsec, libxslt, libxml2;

const
  cDTD = '<!DOCTYPE test [<!ATTLIST &infElement& Id ID #IMPLIED>]>';

type
  { TDFeOpenSSL }

  TDFeOpenSSL = class(TDFeSSLClass)
  private
    FHTTP: THTTPSend;
    FdsigCtx: xmlSecDSigCtxPtr;
    FCNPJ: String;
    FNumSerie: String;
    FValidade: TDateTime;
    FSubjectName: String;

    procedure Clear;
    procedure ConfiguraHTTP(const URL, SoapAction: String);
    procedure CarregarCertificadoSeNecessario;
    function LerPFXInfo(pfxdata: Ansistring): Boolean;

    procedure InitXmlSec;
    procedure ShutDownXmlSec;
    function XmlSecSign(const Axml: PAnsiChar): AnsiString;
  protected

    function GetCertDataVenc: TDateTime; override;
    function GetCertNumeroSerie: String; override;
    function GetCertSubjectName: String; override;
    function GetCertCNPJ: String; override;

  public
    constructor Create(AConfiguracoes: TConfiguracoes);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    function Assinar(const ConteudoXML, docElement, infElement: String): String;
      override;
    function Enviar(const ConteudoXML: String; const URL: String;
      const SoapAction: String): String; override;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; override;
    function VerificarAssinatura(const ConteudoXML: String;
      out MsgErro: String): Boolean; override;

    procedure CarregarCertificado; override;
  end;

implementation

uses Math, strutils, dateutils,
  ACBrUtil, ACBrDFe, ACBrDFeUtil, ACBrConsts,
  synautil,
  {$IFDEF USE_libeay32}libeay32{$ELSE} OpenSSLExt{$ENDIF};

{ TDFeOpenSSL }

constructor TDFeOpenSSL.Create(AConfiguracoes: TConfiguracoes);
begin
  inherited Create(AConfiguracoes);

  FHTTP := THTTPSend.Create;
  FdsigCtx := nil;
  Clear;
end;

destructor TDFeOpenSSL.Destroy;
begin
  FHTTP.Free;
  DesInicializar;

  inherited Destroy;
end;

procedure TDFeOpenSSL.Inicializar;
begin
  if FpInicializado then
    exit;

  InitXmlSec;
  Clear;
  FpInicializado := True;
end;

procedure TDFeOpenSSL.DesInicializar;
begin
  if (FdsigCtx <> nil) then
  begin
    xmlSecDSigCtxDestroy(FdsigCtx);
    FdsigCtx := nil;
  end;

  Clear;

  if FpInicializado and Configuracoes.Geral.UnloadSSLLib then
    ShutDownXmlSec;

  FpInicializado := False;
end;

function TDFeOpenSSL.Assinar(const ConteudoXML, docElement, infElement: String): String;
var
  I, PosIni, PosFim: integer;
  URI, AXml, XmlAss, DTD, TagEndDocElement: String;
begin
  // Nota: "ConteudoXML" já deve estar convertido para UTF8 //
  AXml := ConteudoXML;
  XmlAss := '';

  URI := ExtraiURI(AXml);

  //// Adicionando Cabeçalho DTD, necessário para xmlsec encontrar o ID ////
  I := pos('?>', AXml);
  DTD := StringReplace(cDTD, '&infElement&', infElement, []);

  AXml := Copy(AXml, 1, IfThen(I > 0, I + 1, I)) + DTD +
    Copy(AXml, IfThen(I > 0, I + 2, I), Length(AXml));

  //// Inserindo Template da Assinatura digital ////
  TagEndDocElement := '</' + docElement + '>';
  I := pos('<signature', lowercase(AXml));
  if I = 0 then
    I := pos(TagEndDocElement, AXml);

  if I = 0 then
    raise EACBrDFeException.Create('Não encontrei final do elemento: ' +
      TagEndDocElement);

  AXml := copy(AXml, 1, I - 1) + SignatureElement(URI, True) + TagEndDocElement;

  // Assinando com XMLSec //
  XmlAss := XmlSecSign(PAnsiChar(AXml));

  // Removendo quebras de linha //
  XmlAss := StringReplace(XmlAss, #10, '', [rfReplaceAll]);
  XmlAss := StringReplace(XmlAss, #13, '', [rfReplaceAll]);

  // Removendo DTD //
  XmlAss := StringReplace(XmlAss, DTD, '', []);

  // Considerando apenas o último Certificado //
  PosIni := Pos('<X509Certificate>', XmlAss) - 1;
  PosFim := PosLast('<X509Certificate>', XmlAss);
  XmlAss := copy(XmlAss, 1, PosIni) + copy(XmlAss, PosFim, length(XmlAss));

  // Removendo cabecalho de versao XML
  XmlAss := StringReplace(XmlAss, '<?xml version="1.0"?>', '', []);

  Result := XmlAss;
end;

function TDFeOpenSSL.Enviar(const ConteudoXML: String; const URL: String;
  const SoapAction: String): String;
var
  OK: Boolean;
  RetornoWS: String;
begin
  RetornoWS := '';

  // Configurando o THTTPSend //
  ConfiguraHTTP(URL, 'SOAPAction: "' + SoapAction + '"');

  // Gravando no Buffer de Envio //
  FHTTP.Document.WriteBuffer(ConteudoXML[1], Length(ConteudoXML));

  // DEBUG //
  //FHTTP.Document.SaveToFile( 'c:\temp\HttpSend.xml' );

  // Transmitindo //
  OK := FHTTP.HTTPMethod('POST', URL);
  OK := OK and (FHTTP.ResultCode = 200);
  if not OK then
    raise EACBrDFeException.Create('Cod.Erro HTTP: ' + IntToStr(FHTTP.ResultCode) +
      ' ' + FHTTP.ResultString);

  // Lendo a resposta //
  FHTTP.Document.Position := 0;
  SetLength(RetornoWS, FHTTP.Document.Size);
  FHTTP.Document.ReadBuffer(RetornoWS[1], FHTTP.Document.Size);

  // DEBUG //
  //HTTP.Document.SaveToFile('c:\temp\ReqResp.xml');

  Result := RetornoWS;
end;

function TDFeOpenSSL.Validar(const ConteudoXML, ArqSchema: String;
  out MsgErro: String): Boolean;
var
  doc, schema_doc: xmlDocPtr;
  parser_ctxt: xmlSchemaParserCtxtPtr;
  schema: xmlSchemaPtr;
  valid_ctxt: xmlSchemaValidCtxtPtr;
  schemError: xmlErrorPtr;
begin
  CarregarCertificadoSeNecessario;
  Result := False;
  doc := nil;
  schema_doc := nil;
  parser_ctxt := nil;

  try
    doc := xmlParseDoc(PAnsiChar(ConteudoXML));
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
    begin
      MsgErro := 'Erro: unable to parse';
      exit;
    end;

    schema_doc := xmlReadFile(PAnsiChar(ArqSchema), nil, XML_DETECT_IDS);
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

function TDFeOpenSSL.VerificarAssinatura(const ConteudoXML: String;
  out MsgErro: String): Boolean;
var
  doc: xmlDocPtr;
  node: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  mngr: xmlSecKeysMngrPtr;
  Publico: String;
  MS: TMemoryStream;
begin
  CarregarCertificadoSeNecessario;
  Result := False;
  Publico := copy(ConteudoXML, pos('<X509Certificate>', ConteudoXML) + 17,
                  pos('</X509Certificate>', ConteudoXML) -
                  (pos('<X509Certificate>', ConteudoXML) + 17));

  MS := TMemoryStream.Create;
  try
    MS.WriteBuffer(Publico[1], Length(Publico));

    //xmlSecCryptoAppKeyCertLoadMemory;
    MS.Position := 0;
    mngr := Nil;
    if (xmlSecCryptoAppKeysMngrCertLoadMemory(mngr, MS.Memory, MS.Size,
      xmlSecKeyDataFormatUnknown, 1) < 0) then
    begin
      MsgErro := 'Error: failed to load certificate';
      exit;
    end;

    //xmlSecOpenSSLAppKeyCertLoadMemory;
    doc := xmlParseDoc(PAnsiChar(ConteudoXML));
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
    begin
      MsgErro := 'Error: unable to parse';
      exit;
    end;

    node := xmlSecFindNode(xmlDocGetRootElement(doc), xmlSecNodeSignature, xmlSecDSigNs);
    if (node = nil) then
    begin
      MsgErro := 'Error: start node not found';
      exit;
    end;

    dsigCtx := xmlSecDSigCtxCreate(nil);
    if (dsigCtx = nil) then
    begin
      MsgErro := 'Error :failed to create signature context';
      exit;
    end;

    MS.Position := 0;
    dsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory(MS.Memory, MS.Size,
      xmlSecKeyDataFormatPem, '', nil, nil);
    if (dsigCtx^.signKey = nil) then
    begin
      MsgErro := 'Error: failed to load public pem key from XML';
      exit;
    end;

    { Verify signature }
    if (xmlSecDSigCtxVerify(dsigCtx, node) < 0) then
    begin
      MsgErro := 'Error: signature verify';
      exit;
    end;

    Result := (dsigCtx.status = xmlSecDSigStatusSucceeded);
  finally
    { cleanup }
    MS.Free;

    if (doc <> nil) then
      xmlFreeDoc(doc);

    if (node <> nil) then
      xmlFreeNode(node);

    if (dsigCtx <> nil) then
      xmlSecDSigCtxDestroy(dsigCtx);
  end;
end;

function TDFeOpenSSL.XmlSecSign(const Axml: PAnsiChar): AnsiString;
var
  doc: xmlDocPtr;
  node: xmlNodePtr;
  buffer: PAnsiChar;
  bufSize: integer;
begin
  doc := nil;
  Result := '';

  if (Axml = nil) then
    Exit;

  CarregarCertificadoSeNecessario;

  try
    { load template }
    doc := xmlParseDoc(Axml);
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
      raise EACBrDFeException.Create('Error: unable to parse');

    { find start node }
    node := xmlSecFindNode(xmlDocGetRootElement(doc),
      PAnsiChar(xmlSecNodeSignature), PAnsiChar(xmlSecDSigNs));
    if (node = nil) then
      raise EACBrDFeException.Create('Error: start node not found');

    { sign the template }
    if (xmlSecDSigCtxSign(FdsigCtx, node) < 0) then
      raise EACBrDFeException.Create('Error: signature failed');

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
  end;
end;

procedure TDFeOpenSSL.CarregarCertificado;
var
  LoadFromFile, LoadFromData: Boolean;
  MS: TMemoryStream;
  FS: TFileStream;
begin
  with Configuracoes.Certificados do
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
      raise EACBrDFeException.Create('Arquivo: ' + ArquivoPFX + ' não encontrado');

    // Se FdsigCtx já existia, destrua e crie um novo //
    if (FdsigCtx <> nil) then
    begin
      xmlSecDSigCtxDestroy(FdsigCtx);
      FdsigCtx := nil;
    end;

    { create signature context }
    FdsigCtx := xmlSecDSigCtxCreate(nil);
    if (FdsigCtx = nil) then
      raise EACBrDFeException.Create('Error :failed to create signature context');

    if LoadFromFile then
    begin
      FHTTP.Sock.SSL.PFXfile := ArquivoPFX;

      FdsigCtx^.signKey := xmlSecCryptoAppKeyLoad(
        PAnsiChar(ArquivoPFX), xmlSecKeyDataFormatPkcs12,
        PAnsiChar(Senha), nil, nil);

      if (FdsigCtx^.signKey = nil) then
        raise EACBrDFeException.Create('Error: failed to load private pem key from "' +
          ArquivoPFX + '"');

      FS := TFileStream.Create(ArquivoPFX, fmOpenRead or fmShareDenyNone);
      try
        DadosPFX := ReadStrFromStream(FS, FS.Size);
      finally
        FS.Free;
      end;

    end
    else if LoadFromData then
    begin
      FHTTP.Sock.SSL.PFX := DadosPFX;

      MS := TMemoryStream.Create;
      try
        MS.WriteBuffer(DadosPFX[1], Length(DadosPFX));
        FdsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory(
          MS.Memory, MS.Size, xmlSecKeyDataFormatPkcs12,
          PAnsiChar(Senha), nil, nil);

        if (FdsigCtx^.signKey = nil) then
          raise EACBrDFeException.Create('Error: failed to load private pem key from "' +
            DadosPFX + '"');
      finally
        MS.Free;
      end;
    end;

    FHTTP.Sock.SSL.KeyPassword := Senha;

    if NaoEstaVazio(DadosPFX) then
      LerPFXInfo(DadosPFX);

    { set key name to the file name, this is just an example! }
    if (xmlSecKeySetName(FdsigCtx^.signKey, PAnsiChar(ArquivoPFX)) < 0) then
      raise EACBrDFeException.Create('Error: failed to set key name for key from "' +
        ArquivoPFX + '"');
  end;
end;

function TDFeOpenSSL.LerPFXInfo(pfxdata: Ansistring): Boolean;

  function GetNotAfter( cert: pX509 ): TDateTime;
  var
    Validade: String;
    notAfter: PASN1_TIME;
  begin
    notAfter := cert.cert_info^.validity^.notAfter;
    Validade := StrPas( PAnsiChar(notAfter^.data) );
    SetLength(Validade, notAfter^.length);
    Validade := OnlyNumber(Validade);

    if notAfter^.asn1_type = V_ASN1_UTCTIME then  // anos com 2 dígitos
      Validade :=  LeftStr(IntToStrZero(YearOf(Now),4),2) + Validade;

    Result := StoD(Validade);
  end;

  function GetSubjectName( cert: pX509 ): String;
  var
    s: String;
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

  function GetCNPJ( SubjectName: String ): String;
  var
    P: Integer;
  begin
    Result := '';
    P := pos('CN=',SubjectName);
    if P > 0 then
    begin
      P := PosEx(':', SubjectName, P);
      if P > 0 then
      begin
        Result := OnlyNumber(copy(SubjectName, P+1, 14));
      end;
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
    s := StrPas( PAnsiChar(SN^.data) );
    SetLength(s,SN.length);
    Result := AsciiToHex(s);
  end;

var
  cert: pX509;
  pkey: pEVP_PKEY;
  ca, p12: Pointer;
  b: PBIO;
begin
  Result := False;
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
      pkey := nil;
      ca := nil;
      try
        {$IFDEF USE_libeay32}
        if PKCS12_parse(p12, PAnsiChar(Configuracoes.Certificados.Senha), pkey, cert, ca) > 0 then
        {$ELSE}
        if PKCS12parse(p12, Configuracoes.Certificados.Senha, pkey, cert, ca) > 0 then
        {$ENDIF}
        begin
          FValidade := GetNotAfter( cert );
          FSubjectName := GetSubjectName( cert );
          FCNPJ := GetCNPJ( FSubjectName );
          FNumSerie := GetSerialNumber( cert );
        end;
      finally
        {$IFDEF USE_libeay32}
         EVP_PKEY_free(pkey);
         X509_free(cert);
        {$ELSE}
         EvpPkeyFree(pkey);
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


function TDFeOpenSSL.GetCertDataVenc: TDateTime;
begin
  CarregarCertificadoSeNecessario;
  Result := FValidade;
end;

function TDFeOpenSSL.GetCertNumeroSerie: String;
begin
  CarregarCertificadoSeNecessario;
  Result := FNumSerie;
end;

function TDFeOpenSSL.GetCertSubjectName: String;
begin
  CarregarCertificadoSeNecessario;
  Result := FSubjectName;
end;

function TDFeOpenSSL.GetCertCNPJ: String;
begin
  CarregarCertificadoSeNecessario;
  Result := FCNPJ;
end;

procedure TDFeOpenSSL.Clear;
begin
  FCNPJ := '';
  FNumSerie := '';
  FValidade := 0;
  FSubjectName := '';
end;

procedure TDFeOpenSSL.InitXmlSec;
begin
  { Init libxml and libxslt libraries }
  xmlInitParser();
  __xmlLoadExtDtdDefaultValue^ := XML_DETECT_IDS or XML_COMPLETE_ATTRS;
  xmlSubstituteEntitiesDefault(1);
  __xmlIndentTreeOutput^ := 1;

  { Init xmlsec library }
  if (xmlSecInit() < 0) then
    raise EACBrDFeException.Create('Error: xmlsec initialization failed.');

  { Check loaded library version }
  if (xmlSecCheckVersionExt(1, 2, 8, xmlSecCheckVersionABICompatible) <> 1) then
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
end;

procedure TDFeOpenSSL.ShutDownXmlSec;
begin
  { Shutdown xmlsec-crypto library }
  xmlSecCryptoShutdown();

  { Shutdown crypto library }
  xmlSecCryptoAppShutdown();

  { Shutdown xmlsec library }
  xmlSecShutdown();

  { Shutdown libxslt/libxml }
  xsltCleanupGlobals();
  xmlCleanupParser();
end;

procedure TDFeOpenSSL.ConfiguraHTTP(const URL, SoapAction: String);
begin
  FHTTP.Clear;

  CarregarCertificadoSeNecessario;

  FHTTP.ProxyHost := Configuracoes.WebServices.ProxyHost;
  FHTTP.ProxyPort := Configuracoes.WebServices.ProxyPort;
  FHTTP.ProxyUser := Configuracoes.WebServices.ProxyUser;
  FHTTP.ProxyPass := Configuracoes.WebServices.ProxyPass;

  if (pos('SCERECEPCAORFB', UpperCase(URL)) <= 0) and
    (pos('SCECONSULTARFB', UpperCase(URL)) <= 0) then
    FHTTP.MimeType := 'application/soap+xml; charset=utf-8'
  else
    FHTTP.MimeType := 'text/xml; charset=utf-8';

  FHTTP.UserAgent := '';
  FHTTP.Protocol := '1.1';
  FHTTP.AddPortNumberToHost := False;
  FHTTP.Headers.Add(SoapAction);
end;

procedure TDFeOpenSSL.CarregarCertificadoSeNecessario;
var
  CertificadoLido: Boolean;
begin
  CertificadoLido := (FdsigCtx <> nil) and (FdsigCtx^.signKey <> nil);

  if not CertificadoLido then
    CarregarCertificado;
end;

end.
