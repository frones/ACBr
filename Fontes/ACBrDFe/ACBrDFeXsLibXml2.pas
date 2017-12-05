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

unit ACBrDFeXsLibXml2;

interface

uses
  Classes, SysUtils,
  ACBrDFeSSL, libxml2;

type

  { TDFeSSLXmlSignLibXml2 }

  TDFeSSLXmlSignLibXml2 = class(TDFeSSLXmlSignClass)
  private
    function SelectElements(const aDoc: xmlDoc;
      const infElement: AnsiString): xmlNodeSetPtr;
    function CanonC14n(const aXML, docElement, infElement: AnsiString): AnsiString;
  protected
    procedure VerificarValoresPadrao(var SignatureNode: String;
      var SelectionNamespaces: String);
  public
    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''; IdAttr: String = ''): String; override;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; override;
    function VerificarAssinatura(const ConteudoXML: String;
      out MsgErro: String; const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''; IdSignature: String = '';
      IdAttr: String = ''): Boolean; override;
  end;

implementation

uses
  StrUtils,
  synacode,
  ACBrUtil, ACBrDFeUtil, ACBrConsts, ACBrDFeException;

{ TDFeSSLXmlSignLibXml2 }

function TDFeSSLXmlSignLibXml2.Assinar(
  const ConteudoXML, docElement, infElement: String;
  SignatureNode: String = ''; SelectionNamespaces: String = '';
  IdSignature: String = ''; IdAttr: String = ''): String;
var
  aXML, XmlAss: String;
  TemDeclaracao: boolean;
  PosIni, PosFim: integer;
  Canon, DigestXml, DigestValue, Signaturevalue: AnsiString;
  Digest: TSSLDgst;
begin

  XmlAss := '';
  // Verificando se possui a Declaração do XML, se não possuir,
  // adiciona para libXml2 compreender o Encoding
  TemDeclaracao := XmlEhUTF8(ConteudoXML);
  if not TemDeclaracao then
    AXml := CUTF8DeclaracaoXML + RemoverDeclaracaoXML(ConteudoXML)
  else
    AXml := ConteudoXML;

  // Usa valores default, se não foram informados
  VerificarValoresPadrao(SignatureNode, SelectionNamespaces);

  // Inserindo Template da Assinatura digital
  if (not XmlEstaAssinado(AXml)) or (SignatureNode <> '') then
    AXml := AdicionarSignatureElement(AXml, False, docElement, IdSignature, IdAttr);

  // DEBUG
  //WriteToTXT('C:\TEMP\XmlSign.xml', AXml, False, False);

  Digest := GetSignDigestAlgorithm(AXml);

  // Aplica a transformação c14n no node infElement
  Canon := CanonC14n(AXml, docElement, infElement);

  // DEBUG
  //WriteToTXT('C:\TEMP\CanonDigest.xml', Canon, False, False);

  // gera o rsa-sha1 hash
  DigestValue := FpDFeSSL.CalcHash(Canon, Digest, outBase64);

  PosIni := 0;
  PosFim := 0;

  // adiciona o digestvalue em SignedInfo
  EncontrarInicioFinalTag(AXml, 'DigestValue', PosIni, PosFim);
  if (PosIni > 0) and (PosFim > 0) then
  begin
    DigestXml := copy(AXml, 1, PosIni) + DigestValue + copy(AXml, PosFim, Length(AXml));
  end
  else
    raise EACBrDFeException.Create('Node DigestValue não encontrado!');

  // Aplica a transformação c14n o node SignedInfo

  Canon := CanonC14n(DigestXml, docElement, 'SignedInfo');

  // DEBUG
  //WriteToTXT('C:\TEMP\CanonGeracao.xml', Canon, False, False);

  // Assina o node SignedInfo já transformado
  Signaturevalue := FpDFeSSL.CalcHash(Canon, Digest, outBase64, True);

  // Adiciona o SignatureValue na Tag SignatureValue
  EncontrarInicioFinalTag(DigestXml, 'SignatureValue', PosIni, PosFim);
  if (PosIni > 0) and (PosFim > 0) then
  begin
    XmlAss := copy(DigestXml, 1, PosIni) + Signaturevalue + copy(DigestXml, PosFim, Length(DigestXml));
  end
  else
    raise EACBrDFeException.Create('Node SignatureValue não encontrado!');

  if not TemDeclaracao then
    XmlAss := RemoverDeclaracaoXML(XmlAss);

  // ajusta o xml e adiciona os dados do certificado
  XmlAss := AjustarXMLAssinado(XmlAss, FpDFeSSL.DadosCertificado.DERBase64);

  // DEBUG
  //WriteToTXT('C:\TEMP\XmlSigned2.xml', XmlAss, False, False);

  Result := XmlAss;
end;

function TDFeSSLXmlSignLibXml2.CanonC14n(
  const aXML, docElement, infElement: AnsiString): AnsiString;
var
  doc: xmlDocPtr;
  Elements: xmlNodeSetPtr;
  buffer: PAnsiChar;
  inclusive: xmlCharPtrPtr;
begin
  // carrega o xml
  doc := xmlParseDoc(PAnsiChar(aXML));
  if (doc = nil) then
    raise EACBrDFeException.Create('Erro ao carregar xml C14N!');

  try
    // seleciona os elementos a serem transformados e inclui os devidos namespaces
    Elements := SelectElements(doc^, infElement);
    try
      // aplica a transformação C14N
      buffer := nil;
      inclusive := nil;
      if xmlC14NDocDumpMemory(doc, Elements, 0, inclusive, 0, @buffer) < 0 then
        raise EACBrDFeException.Create('Erro ao aplicar transformação C14N!');

      if buffer = nil then
        raise EACBrDFeException.Create('Erro ao aplicar transformação C14N!');
    finally
      xmlXPathFreeNodeSet(Elements);
    end;

    Result := AnsiString( buffer );
  finally
    xmlFreeDoc(doc);
  end;
end;

function TDFeSSLXmlSignLibXml2.SelectElements(const aDoc: xmlDoc;
  const infElement: AnsiString): xmlNodeSetPtr;
var
  xpathCtx: xmlXPathContextPtr;
  xpathExpr: ansistring;
  xpathObj: xmlXPathObjectPtr;
begin
  // Cria o contexdo o XPath
  xpathCtx := xmlXPathNewContext(@aDoc);
  try
    if (xpathCtx = nil) then
      raise EACBrDFeException.Create('Erro ao obter o contexto do XPath');

    // express?o para selecionar os elementos n: ? o namespace selecionado
    xpathExpr := '(//.|//@*|//namespace::*)[ancestor-or-self::*[local-name()=''' +
      infElement + ''']]';

    // seleciona os elementos baseados na express?o(retorna um objeto XPath com os elementos)
    xpathObj := xmlXPathEvalExpression(PAnsiChar(xpathExpr), xpathCtx);

    if (xpathObj = nil) then
      raise EACBrDFeException.Create('Erro ao selecionar os elementos do XML.');

    if (xpathObj.nodesetval.nodeNr > 0) then
      Result := xpathObj.nodesetval
    else
      raise EACBrDFeException.Create('Nenhum elemento encontrado.');

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
  aXML: ansistring;
begin
  Result := False;
  doc := nil;
  schema_doc := nil;
  parser_ctxt := nil;
  schema := nil;
  valid_ctxt := nil;

  try
    aXML := ansistring(ConteudoXML);
    doc := xmlParseDoc(PAnsiChar(aXML));
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
    begin
      MsgErro := 'Erro: não foi possivel carregar o xml';
      exit;
    end;

    schema_doc := xmlReadFile(PAnsiChar(ansistring(ArqSchema)), nil,
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
  doc: xmlDocPtr;
  X509Certificate: String;
  XmlSign, docElement, AXml: AnsiString;
  Digest: TSSLDgst;
begin
  Digest := GetSignDigestAlgorithm(ConteudoXML);
  doc := nil;
  try
    doc := xmlParseDoc(PAnsiChar(AnsiString(ConteudoXML)));
    if (doc = nil) then
      raise EACBrDFeException.Create('Erro ao carregar xml C14N!');

    docElement := xmlDocGetRootElement(doc).Name;
    if (docElement = '') then
      raise EACBrDFeException.Create('Erro ao carregar o elemento root do xml!');
  finally
    xmlFreeDoc(doc);
  end;

  XmlSign := DecodeBase64(LerTagXML(ConteudoXML, 'SignatureValue'));
  X509Certificate := LerTagXML(ConteudoXML, 'X509Certificate');
  FpDFeSSL.CarregarCertificadoPublico(X509Certificate);

  AXml := CanonC14n(ConteudoXML, docElement, 'SignedInfo');

  Result := FpDFeSSL.ValidarHash(AXml, Digest, XmlSign, True);

  // Descarrega o Certificado Publico //
  FpDFeSSL.DescarregarCertificado;
end;

procedure TDFeSSLXmlSignLibXml2.VerificarValoresPadrao(
  var SignatureNode: String; var SelectionNamespaces: String);
begin
  if SignatureNode = '' then
    SignatureNode := CSIGNATURE_NODE;

  if SelectionNamespaces = '' then
    SelectionNamespaces := CDSIGNS
  else
  begin
    if LeftStr(SelectionNamespaces, Length(CDSIGNS)) <> CDSIGNS then
      SelectionNamespaces := CDSIGNS + ' ' + SelectionNamespaces;
  end;
end;

end.
