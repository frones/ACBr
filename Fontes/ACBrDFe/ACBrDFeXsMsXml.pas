{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: André Ferreira de Moraes                        }
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

unit ACBrDFeXsMsXml;

interface

uses
  Classes, SysUtils,
  ACBrDFeSSL,
  ACBrMSXML2_TLB,
  Windows, ActiveX, ComObj;

type

  { TDFeSSLXmlSignMsXml }

  TDFeSSLXmlSignMsXml = class( TDFeSSLXmlSignClass )
   private
   protected
     procedure VerificarValoresPadrao(var SignatureNode: String;
       var SelectionNamespaces: String);
   public
     function Assinar(const ConteudoXML, docElement, infElement: String;
       const SignatureNode: String = ''; const SelectionNamespaces: String = '';
       const IdSignature: String = ''; const IdAttr: String = '';
       const IdSignatureValue: string = ''): String; override;
     function Validar(const ConteudoXML, ArqSchema: String;
       out MsgErro: String): Boolean; override;
     function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
       const infElement: String; const SignatureNode: String = '';
       const SelectionNamespaces: String = ''; const IdSignature: String = '';
       const IdAttr: String = ''): Boolean;
       override;
   end;

implementation

uses
  strutils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrDFeUtil, ACBrDFeException,
  ACBr_WinCrypt, ACBrDFeWinCrypt;

{ TDFeSSLXmlSignMsXml }

procedure TDFeSSLXmlSignMsXml.VerificarValoresPadrao(var SignatureNode: String;
  var SelectionNamespaces: String);
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

function TDFeSSLXmlSignMsXml.Assinar(const ConteudoXML, docElement,
  infElement: String; const SignatureNode: String; const SelectionNamespaces: String;
  const IdSignature: String; const IdAttr: String;
  const IdSignatureValue: string): String;
var
  AXml, XmlAss: AnsiString;
  xmldoc: IXMLDOMDocument3;
  signedKey: IXMLDSigKey;
  ResultInitialize: HRESULT;
  Inicializado: Boolean;
  // Código Compatíval com TDFeSSLXmlSignMsXmlCapicom
  xmldsig: IXMLDigitalSignature;
  dsigKey: IXMLDSigKey;
  ProviderType: DWORD;
  ProviderName, ContainerName, Erro: String;
  { // Nova implementação usando IXMLDigitalSignatureEx. porem, falha em algumas raras situações
  xmldsigEx: IXMLDigitalSignatureEx;
  dsigKeyEx: IXMLDSigKeyEx;}
  var vSignatureNode, vSelectionNamespaces: string;
begin
  Result := '';
  ResultInitialize := CoInitialize(nil);
  if (ResultInitialize = E_FAIL) then
    raise EACBrDFeException.Create('Erro ao inicializar biblioteca COM');

  Inicializado := (ResultInitialize in [ S_OK, S_FALSE ]);
  try
    FpDFeSSL.CarregarCertificadoSeNecessario;

    // IXMLDOMDocument3 deve usar a String Nativa da IDE //
    {$IfDef FPC2}
     AXml := ACBrUTF8ToAnsi(ConteudoXML);
    {$Else}
     AXml := UTF8ToNativeString(ConteudoXML);
    {$EndIf}
    XmlAss := '';

    // Usa valores default, se não foram informados //
    vSignatureNode       := SignatureNode;
    vSelectionNamespaces := SelectionNamespaces;
    VerificarValoresPadrao(vSignatureNode, vSelectionNamespaces);

    // Inserindo Template da Assinatura digital //
    if (not XmlEstaAssinado(AXml)) or (vSignatureNode <> CSIGNATURE_NODE) then
      AXml := AdicionarSignatureElement(AXml, False, docElement, IdSignature, IdAttr, IdSignatureValue);

    // DEBUG
    //WriteToFile('c:\temp\XmlDocWithSignatureTag.xml', AXml);

    try
      // Criando XMLDOC //
      xmldoc := CoDOMDocument50.Create;
      xmldoc.async := False;
      xmldoc.validateOnParse := False;
      xmldoc.preserveWhiteSpace := True;

      // Carregando o AXml em XMLDOC
      if (not xmldoc.loadXML( WideString(AXml) )) then
        raise EACBrDFeException.Create('Não foi possível carregar XML'+sLineBreak+ AXml);

      xmldoc.setProperty('SelectionNamespaces', vSelectionNamespaces);

      //DEBUG
      //xmldoc.save('c:\temp\xmldoc.xml');

      // Criando Elemento de assinatura //
      // Código Compatíval com TDFeSSLXmlSignMsXmlCapicom
      xmldsig := CreateComObject(CLASS_MXDigitalSignature50) as IXMLDigitalSignature;
      { // Nova implementação usando IXMLDigitalSignatureEx
      xmldsigEx := CreateComObject(CLASS_MXDigitalSignature50) as IXMLDigitalSignatureEx;}
      if (xmldsig = nil) then
        raise EACBrDFeException.Create('Erro ao criar Elemento para Assinatura');

      // Lendo elemento de Assinatura de XMLDOC //
      xmldsig.signature := xmldoc.selectSingleNode( WideString(vSignatureNode) );
      if (xmldsig.signature = nil) then
        raise EACBrDFeException.Create('Erro ao encontrar nó para Assinatura');

      // Criando Objeto para manipular a Chave Privada //
      dsigKey := Nil;
      // Código Compatíval com TDFeSSLXmlSignMsXmlCapicom
      ProviderType  := 0;
      ProviderName  := '';
      ContainerName := '';
      GetProviderInfo( PCCERT_CONTEXT(FpDFeSSL.CertContextWinApi),
                       ProviderType,
                       ProviderName,
                       ContainerName);

      try
        dsigKey := xmldsig.createKeyFromCSP( ProviderType,
                                             WideString(ProviderName),
                                             WideString(ContainerName), 0);
      except
        on E: Exception do
        begin
          Erro := LowerCase(E.Message);
          if (pos('provider type', Erro) > 0) and
             (pos('not supported', Erro) > 0) then
          begin
            ProviderType := 1;
            ProviderName := 'Microsoft Enhanced Cryptographic Provider v1.0';
            dsigKey := xmldsig.createKeyFromCSP( ProviderType,
                                                 WideString(ProviderName),
                                                 WideString(ContainerName), 0);
          end;
        end;
      end;
      { // Nova implementação usando IXMLDigitalSignatureEx
      dsigKeyEx := Nil;
      xmldsigEx.setStoreHandle( PCCERT_CONTEXT(FpDFeSSL.CertContextWinApi)^.hCertStore );
      xmldsigEx.createKeyFromCertContext( FpDFeSSL.CertContextWinApi, dsigKeyEx);}

      if (dsigKey = nil) then
        raise EACBrDFeException.Create('Falha ao obter a Chave Privada do Certificado para Assinatura.');

      // Assinando com MSXML e CryptoLib //
      signedKey := xmldsig.sign(dsigKey, CERTIFICATES);
      if (signedKey = nil) then
        raise EACBrDFeException.Create('Assinatura Falhou.');

      //DEBUG
      //xmldoc.save('c:\temp\ass.xml');
      XmlAss := AnsiString(xmldoc.xml);

      // Convertendo novamente para UTF8
      {$IfDef FPC2}
       XmlAss := ACBrAnsiToUTF8( XmlAss );
      {$Else}
       XmlAss := NativeStringToUTF8( String(XmlAss) );
      {$EndIf}

      // Ajustando o XML... MsXml, insere um cabeçalho inválido
      XmlAss := AjustarXMLAssinado(XmlAss, FpDFeSSL.DadosCertificado.DERBase64);
    finally
      xmldoc := nil;
      //xmldsig := nil;
      //signedKey := nil;
      //dsigKey := nil;
    end;

    Result := XmlAss;
  finally
    if Inicializado then
      CoUninitialize;
  end;
end;

function TDFeSSLXmlSignMsXml.Validar(const ConteudoXML, ArqSchema: String; out
  MsgErro: String): Boolean;
var
  DOMDocument: IXMLDOMDocument2;
  ParseError: IXMLDOMParseError;
  Schema: XMLSchemaCache50;
  AXml: String;
  ResultInitialize: HRESULT;
  Inicializado: Boolean;
begin
  Result := False;
  ResultInitialize := CoInitialize(nil);
  if (ResultInitialize = E_FAIL) then
    raise EACBrDFeException.Create('Erro ao inicializar biblioteca COM');

  Inicializado := (ResultInitialize in [ S_OK, S_FALSE ]);
  try
    DOMDocument := CoDOMDocument50.Create;
    Schema := CoXMLSchemaCache50.Create;
    try
      DOMDocument.async := False;
      DOMDocument.resolveExternals := False;
      DOMDocument.validateOnParse := True;

      // Carregando ConteudoXML em XMLDOC. Nota: IXMLDOMDocument2 deve usar a String Nativa da IDE //
      {$IfDef FPC2}
       AXml := ACBrUTF8ToAnsi(ConteudoXML);
      {$Else}
       AXml := UTF8ToNativeString(ConteudoXML);
      {$EndIf}

      if (not DOMDocument.loadXML(WideString(AXml))) then
      begin
        ParseError := DOMDocument.parseError;
        MsgErro := ACBrStr('Não foi possível carregar o arquivo.')+sLineBreak+
                   'Err: '+IntToStr(ParseError.errorCode) + ', ' +
                   'Lin: '+IntToStr(ParseError.line) + ', ' +
                   'Pos: '+IntToStr(ParseError.linepos) + ' - ' +
                   String(ParseError.reason);
        exit;
      end;

      Schema.add(WideString(FpDFeSSL.NameSpaceURI), ArqSchema);

      DOMDocument.schemas := Schema;
      ParseError := DOMDocument.validate;

      Result := (ParseError.errorCode = 0);
      MsgErro := String(ParseError.reason);
    finally
      ParseError := nil;
      DOMDocument := nil;
      Schema := nil;
    end;
  finally
    if Inicializado then
      CoUninitialize;
  end;
end;

function TDFeSSLXmlSignMsXml.VerificarAssinatura(const ConteudoXML: String; out
  MsgErro: String; const infElement: String; const SignatureNode: String;
  const SelectionNamespaces: String; const IdSignature: String;
  const IdAttr: String): Boolean;
var
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  certNode: IXMLDOMNode;
  pKey, pKeyOut: IXMLDSigKey;
  AXml: String;
  ResultInitialize: HRESULT;
  Inicializado: Boolean;
  SignatureNode_temp, SelectionNamespaces_temp: string;
begin
{$IFNDEF COMPILER25_UP}
  Result := False;
{$ENDIF}
  // Usa valores default, se não foram informados //
  SignatureNode_temp       := SignatureNode;
  SelectionNamespaces_temp := SelectionNamespaces;
  VerificarValoresPadrao(SignatureNode_temp, SelectionNamespaces_temp);

  ResultInitialize := CoInitialize(nil);
  if (ResultInitialize = E_FAIL) then
    raise EACBrDFeException.Create('Erro ao inicializar biblioteca COM');

  Inicializado := (ResultInitialize in [ S_OK, S_FALSE ]);
  try
    xmldoc := CoDOMDocument50.Create;
    xmldsig := CoMXDigitalSignature50.Create;
    try
      xmldoc.async := False;
      xmldoc.validateOnParse := False;
      xmldoc.preserveWhiteSpace := True;

      // Carregando ConteudoXML em XMLDOC. Nota: IXMLDOMDocument2 deve usar a String Nativa da IDE //
      {$IfDef FPC2}
       AXml := ACBrUTF8ToAnsi(ConteudoXML);
      {$Else}
       AXml := UTF8ToNativeString(ConteudoXML);
      {$EndIf}

      if (not xmldoc.loadXML(WideString(AXml))) then
      begin
        MsgErro := ACBrStr('Não foi possível carregar o arquivo.');
        exit;
      end;

      xmldoc.setProperty('SelectionNamespaces', SelectionNamespaces_temp);
      xmldsig.signature := xmldoc.selectSingleNode( WideString(SignatureNode_temp));
      if (xmldsig.signature = nil) then
      begin
        MsgErro := ACBrStr('Não foi possível ler o nó de Assinatura (')+SignatureNode_temp+')';
        exit;
      end;

      certNode := xmldsig.signature.selectSingleNode( WideString('.//ds:KeyInfo/ds:X509Data/ds:X509Certificate'));
      if (certNode = nil) then
      begin
        MsgErro := 'Erro ao ler <X509Data> da Assinatura';
        exit;
      end;

      pKey := xmldsig.createKeyFromNode(certNode);
      if (pKey = nil) then
      begin
        MsgErro := 'Erro obter a Chave Publica de <X509Certificate>';
        exit;
      end;

      try
        pKeyOut := xmldsig.verify(pKey);
      except
        on E: Exception do
          MsgErro := 'Erro ao verificar assinatura do arquivo: ' + E.Message;
      end;
    finally
      Result := (pKeyOut <> nil);

      pKeyOut := nil;
      pKey := nil;
      certNode := nil;
      xmldsig := nil;
      xmldoc := nil;
    end;
  finally
    if Inicializado then
      CoUninitialize;
  end;
end;

end.

