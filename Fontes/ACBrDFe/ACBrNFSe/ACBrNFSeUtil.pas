{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrNFSeUtil;

interface

uses
  {$IFNDEF ACBrNFSeOpenSSL}
    ACBrCAPICOM_TLB, ACBrMSXML2_TLB,
  {$ENDIF}
  Classes, Forms,
  {$IFDEF FPC}
    LResources, Controls, Graphics, Dialogs,
  {$ELSE}
    StrUtils, Activex,
  {$ENDIF}
  Sysutils, ACBrNFSeConfiguracoes, pnfsConversao, pnfsNFSe, pcnAuxiliar, ACBrDFeUtil;

  {$IFDEF ACBrNFSeOpenSSL}
    const
     cDTDLote = '<!DOCTYPE test [<!ATTLIST LoteRps Id ID #IMPLIED>]>';
     cDTDRps  = '<!DOCTYPE test [<!ATTLIST InfRps Id ID #IMPLIED>]>';
  {$ELSE}
    const
     DSIGNS = 'xmlns:ds="http://www.w3.org/2000/09/xmldsig#"';
  {$ENDIF}

 {$IFNDEF ACBrNFSeOpenSSL}
   var
    CertStore     : IStore3;
    CertStoreMem  : IStore3;
    PrivateKey    : IPrivateKey;
    Certs         : ICertificates2;
    Cert          : ICertificate2;
    NumCertCarregado : String;
 {$ENDIF}

type

 NotaUtil = class
  private
  protected

  public
    {$IFDEF ACBrNFSeOpenSSL}
      class function sign_file(const Axml: PAnsiChar; const key_file: PAnsiChar; const senha: PAnsiChar): AnsiString;
      class function sign_memory(const Axml: PAnsiChar; const key_file: PAnsichar; const senha: PAnsiChar; Size: Cardinal; Ponteiro: Pointer): AnsiString;
      class Procedure InitXmlSec;
      class Procedure ShutDownXmlSec;
    {$ENDIF}

    {$IFDEF ACBrNFSeOpenSSL}
      class function Assinar(const AXML, ArqPFX, PFXSenha: AnsiString;
                             out AXMLAssinado, FMensagem: AnsiString;
                             ALote: Boolean = False;
                             APrefixo3: string = '';
                             APrefixo4: string = '';
                             AProvedor: TnfseProvedor = proNenhum;
                             ASincrono: Boolean = False): Boolean;
    {$ELSE}
      // Alterado por Italo em 12/07/2012
      class function Assinar(const AXML: AnsiString;
                             Certificado : ICertificate2;
                             out AXMLAssinado, FMensagem: AnsiString;
                             ALote: Boolean = False;
                             APrefixo3: string = '';
                             APrefixo4: string = '';
                             AProvedor: TnfseProvedor = proNenhum;
                             ASincrono: Boolean = False): Boolean;
    {$ENDIF}

    // Alterado por Italo em 29/10/2012
    {$IFDEF ACBrNFSeOpenSSL}
     class function AssinarXML(AXML, FURISig, FURIRef, FTagI, FTagF, ArqPFX, PFXSenha: AnsiString;
                               out AXMLAssinado, FMensagem: AnsiString;
                               AProvedor: TnfseProvedor = proNenhum;
                               ASincrono: Boolean = False): Boolean;
    {$ELSE}
     class function AssinarXML(AXML, FURISig, FURIRef, FTagI, FTagF: AnsiString; Certificado : ICertificate2;
                               out AXMLAssinado, FMensagem: AnsiString;
                               AProvedor: TnfseProvedor = proNenhum;
                               ASincrono: Boolean = False): Boolean;
    {$ENDIF}

    class function Valida(const AXML: AnsiString;
                          var AMsg: AnsiString;
                          const APathSchemas: string = '';
                          AURL: string = '';
                          AServico: string = '';
                          APrefixo: string = ''): Boolean;

    class function RetirarPrefixos(const AXML: String): String;
    class function VersaoXML(AXML: String): String;
    class function PathWithDelim( const APath : String ) : String;
    class function RetornarConteudoEntre(const Frase, Inicio, Fim: string): string;
    class function ChaveAcesso(AUF:Integer; ADataEmissao:TDateTime; ACNPJ:String; ASerie:Integer;
                               ANumero,ACodigo: Integer; AModelo:Integer=56): String;
    class function ObterNomeMunicipio(const xMun, xUF: string; const cMun: integer): string;
    class function ObterCodigoMunicipio(const xMun, xUF: string): integer;
{$IFDEF DELPHI2009_UP}
    class function LoadXML(CaminhoArquivo: string): TEncoding;
{$ENDIF}
    class function GerarNomeNFSe(AUF: Integer; ADataEmissao: TDateTime; ACNPJ: String;
                                 ANumero:Integer; AModelo: Integer = 56): String;
    class function ObterDescricaoServico(cCodigo: String): AnsiString;
  published

  end;

implementation

uses
 {$IFDEF ACBrNFSeOpenSSL}
   libxml2, libxmlsec, libxslt,
 {$ELSE}
   ComObj,
 {$ENDIF}
 IniFiles, Variants, ACBrUtil, ACBrNFSe;

{ NotaUtil }

{$IFDEF ACBrNFSeOpenSSL}
class function NotaUtil.sign_file(const Axml: PAnsiChar; const key_file: PAnsiChar; const senha: PAnsiChar): AnsiString;
var
  doc: xmlDocPtr;
  node: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  buffer: PAnsiChar;
  bufSize: integer;
label done;
begin
    doc := nil;
    //node := nil;
    dsigCtx := nil;
    result := '';

    if (Axml = nil) or (key_file = nil) then Exit;

    try
       // load template
       doc := xmlParseDoc(Axml);
       if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
         raise Exception.Create('Error: unable to parse');

       // find start node
       node := xmlSecFindNode(xmlDocGetRootElement(doc), PAnsiChar(xmlSecNodeSignature), PAnsiChar(xmlSecDSigNs));
       if (node = nil) then
         raise Exception.Create('Error: start node not found');

       // create signature context, we don't need keys manager in this example
       dsigCtx := xmlSecDSigCtxCreate(nil);
       if (dsigCtx = nil) then
         raise Exception.Create('Error: failed to create signature context');

       //  load private key
       dsigCtx^.signKey := xmlSecCryptoAppKeyLoad(key_file, xmlSecKeyDataFormatPkcs12, senha, nil, nil);
       if (dsigCtx^.signKey = nil) then
          raise Exception.Create('Error: failed to load private pem key from "' + key_file + '"');

       // set key name to the file name, this is just an example!
       if (xmlSecKeySetName(dsigCtx^.signKey, PAnsiChar(key_file)) < 0) then
         raise Exception.Create('Error: failed to set key name for key from "' + key_file + '"');

       // sign the template
       if (xmlSecDSigCtxSign(dsigCtx, node) < 0) then
         raise Exception.Create('Error: signature failed');

       // print signed document to stdout
       // xmlDocDump(stdout, doc);
       // Can't use "stdout" from Delphi, so we'll use xmlDocDumpMemory instead...
       buffer := nil;
       xmlDocDumpMemory(doc, @buffer, @bufSize);
       if (buffer <> nil) then
          // success
          result := buffer;

   finally
       // cleanup
       if (dsigCtx <> nil) then
         xmlSecDSigCtxDestroy(dsigCtx);

       if (doc <> nil) then
         xmlFreeDoc(doc);
   end;
end;

class function NotaUtil.sign_memory(const Axml: PAnsiChar; const key_file: PAnsichar; const senha: PAnsiChar; Size: Cardinal; Ponteiro: Pointer): AnsiString;
var
  doc: xmlDocPtr;
  node: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  buffer: PAnsiChar;
  bufSize: integer;
label done;
begin
    doc := nil;
    //node := nil;
    dsigCtx := nil;
    result := '';

    if (Axml = nil) or (key_file = nil) then Exit;
    try
       // load template
       doc := xmlParseDoc(Axml);
       if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
         raise Exception.Create('Error: unable to parse');

       // find start node
       node := xmlSecFindNode(xmlDocGetRootElement(doc), PAnsiChar(xmlSecNodeSignature), PAnsiChar(xmlSecDSigNs));
       if (node = nil) then
         raise Exception.Create('Error: start node not found');

       // create signature context, we don't need keys manager in this example
       dsigCtx := xmlSecDSigCtxCreate(nil);
       if (dsigCtx = nil) then
         raise Exception.Create('Error: failed to create signature context');

       //  load private key, assuming that there is not password
       dsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory(Ponteiro, size, xmlSecKeyDataFormatPkcs12, senha, nil, nil);

       if (dsigCtx^.signKey = nil) then
          raise Exception.Create('Error: failed to load private pem key from "' + key_file + '"');

       // set key name to the file name, this is just an example!
       if (xmlSecKeySetName(dsigCtx^.signKey, key_file) < 0) then
         raise Exception.Create('Error: failed to set key name for key from "' + key_file + '"');

       // sign the template
       if (xmlSecDSigCtxSign(dsigCtx, node) < 0) then
         raise Exception.Create('Error: signature failed');

       // print signed document to stdout
       // xmlDocDump(stdout, doc);
       // Can't use "stdout" from Delphi, so we'll use xmlDocDumpMemory instead...
       buffer := nil;
       xmlDocDumpMemory(doc, @buffer, @bufSize);
       if (buffer <> nil) then
          // success
          result := buffer;
   finally
       // cleanup
       if (dsigCtx <> nil) then
         xmlSecDSigCtxDestroy(dsigCtx);

       if (doc <> nil) then
         xmlFreeDoc(doc);
   end;
end;

class Procedure NotaUtil.InitXmlSec;
begin
    // Init libxml and libxslt libraries
    xmlInitParser();
    __xmlLoadExtDtdDefaultValue^ := XML_DETECT_IDS or XML_COMPLETE_ATTRS;
    xmlSubstituteEntitiesDefault(1);
    __xmlIndentTreeOutput^ := 1;


    // Init xmlsec library
    if (xmlSecInit() < 0) then
       raise Exception.Create('Error: xmlsec initialization failed.');

    // Check loaded library version
    if (xmlSecCheckVersionExt(1, 2, 8, xmlSecCheckVersionABICompatible) <> 1) then
       raise Exception.Create('Error: loaded xmlsec library version is not compatible.');

    (* Load default crypto engine if we are supporting dynamic
     * loading for xmlsec-crypto libraries. Use the crypto library
     * name ("openssl", "nss", etc.) to load corresponding
     * xmlsec-crypto library.
     *)
    if (xmlSecCryptoDLLoadLibrary('openssl') < 0) then
       raise Exception.Create( 'Error: unable to load default xmlsec-crypto library. Make sure'#10 +
                          			'that you have it installed and check shared libraries path'#10 +
                          			'(LD_LIBRARY_PATH) environment variable.');

    // Init crypto library
    if (xmlSecCryptoAppInit(nil) < 0) then
       raise Exception.Create('Error: crypto initialization failed.');

    // Init xmlsec-crypto library
    if (xmlSecCryptoInit() < 0) then
       raise Exception.Create('Error: xmlsec-crypto initialization failed.');
end;

class Procedure NotaUtil.ShutDownXmlSec;
begin
    // Shutdown xmlsec-crypto library
    xmlSecCryptoShutdown();

    // Shutdown crypto library
    xmlSecCryptoAppShutdown();

    // Shutdown xmlsec library
    xmlSecShutdown();

    // Shutdown libxslt/libxml
    xsltCleanupGlobals();
    xmlCleanupParser();
end;
{$ENDIF}

{$IFDEF ACBrNFSeOpenSSL}
function AssinarLibXML(const AXML,
                       ArqPFX, PFXSenha : AnsiString;
                       out AXMLAssinado, FMensagem: AnsiString;
                       ALote: Boolean = False;
                       APrefixo3: string = '';
                       APrefixo4: string = '';
                       AProvedor: TnfseProvedor = proNenhum;
                       ASincrono: Boolean = False): Boolean;
var
 I, J, PosIni, PosFim, PosIniAssLote : Integer;
 URI, AID, Identificador, NameSpaceLote, EnviarLoteRps : String;
 AStr, XmlAss, Assinatura : AnsiString;
 Cert: TMemoryStream;
 Cert2: TStringStream;
begin
 AStr := AXML;

 if ASincrono
  then EnviarLoteRps := 'EnviarLoteRpsSincronoEnvio'
  else EnviarLoteRps := 'EnviarLoteRpsEnvio';

 if ALote
  then begin
   I := pos(EnviarLoteRps +' xmlns=', AStr);
   if I = 0
    then NameSpaceLote := ' '
    else begin
     I := I + 25;
     J := pos('>', AStr);
     NameSpaceLote := ' xmlns:ds1=' + Copy(AStr, I, J - I);
    end;

   Identificador := 'Id';
   I             := pos('LoteRps Id=', AStr);
   if I = 0
    then begin
     Identificador := 'id';
     I             := pos('LoteRps id=', AStr);
    end;
   if I = 0
    then begin
     Identificador := '';
     URI           := '';
    end
    else begin
     I := PosEx('"', AStr, I + 2);
     if I = 0
      then raise Exception.Create('Não encontrei inicio do URI: aspas inicial');
     J := PosEx('"', AStr, I + 1);
     if J = 0
      then raise Exception.Create('Não encontrei inicio do URI: aspas final');

     URI := copy(AStr, I + 1, J - I - 1);
    end;

   AStr := copy(AStr, 1, pos('</'+ APrefixo3 + EnviarLoteRps + '>', AStr) - 1);

   if (URI = '') or (AProvedor in [proRecife, proRJ, proAbaco, proIssDSF, proIssCuritiba, proFISSLex, proGovBR])
    then AID := '>'
    else AID := ' ' + Identificador + '="AssLote_' + URI + '">';

   // Incluido por Italo em 23/04/2014
   if AProvedor in [proAbaco, proIssCuritiba, proFISSLex]
    then URI := '';

   //// Adicionando Cabeçalho DTD, necessário para xmlsec encontrar o ID ////
   I    := pos('?>', AStr);
   AStr := copy(AStr, 1, StrToInt(VarToStr(SeSenao(I > 0, I + 1, I)))) +
           cDTDLote +
           copy(AStr, StrToInt(VarToStr(SeSenao(I > 0, I + 2, I))), Length(AStr));

   AStr := AStr + '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"'+ AID +
                 '<SignedInfo>'+
                   SeSenao((AProvedor in [proActcon, proNatal, proTinus]),
                    '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315#WithComments" />',
                    '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />') +
                  '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />'+
                  '<Reference URI="' + SeSenao(URI = '', '">', '#' + URI + '">') +
                   '<Transforms>'+
                    '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />'+
                      SeSenao((AProvedor in [proActcon, profintelISS, proGovBr,
                              proGovDigital, proISSNet, proNatal, proTinus]), '',
                    '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />') +
                   '</Transforms>'+
                   '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />'+
                   '<DigestValue></DigestValue>'+
                  '</Reference>'+
                 '</SignedInfo>'+
                 '<SignatureValue></SignatureValue>'+
                 '<KeyInfo>'+
                  '<X509Data>'+
//                    SeSenao((AProvedor in [proGovDigital]),
//                                    '<X509SubjectName></X509SubjectName>',
//                                    '') +
                    '<X509Certificate></X509Certificate>'+
                  '</X509Data>'+
                 '</KeyInfo>'+
                '</Signature>';

   AStr := AStr + '</'+ APrefixo3 + EnviarLoteRps + '>';

  end
  else begin

   // Ao assinar um RPS a tag não possui prefixo
   APrefixo3 := '';

   if Pos('<Signature', AStr) <= 0
    then begin
     Identificador := 'Id';
     I             := pos('Id=', AStr);
     if I = 0
      then begin
       Identificador := 'id';
       I             := pos('id=', AStr);
       if I = 0
        then Identificador := '';
  //      raise Exception.Create('Não encontrei inicio do URI: Id=');
      end;
     if I <> 0
      then begin
       I := PosEx('"', AStr, I + 2);
       if I = 0
        then raise Exception.Create('Não encontrei inicio do URI: aspas inicial');
       J := PosEx('"', AStr, I + 1);
       if J = 0
        then raise Exception.Create('Não encontrei inicio do URI: aspas final');

       URI := copy(AStr, I + 1, J - I - 1);
      end
      else URI := '';

     if (URI = '') or (AProvedor in [proActcon, profintelISS, proRecife, proNatal,
                 proRJ, proGovBR, proTecnos, proTinus])
      then AID := '>'
      else AID := ' ' + Identificador + '="Ass_' + URI + '">';

     //// Adicionando Cabeçalho DTD, necessário para xmlsec encontrar o ID ////
     I    := pos('?>', AStr);
     AStr := copy(AStr, 1, StrToInt(VarToStr(SeSenao(I>0, I+1, I)))) +
             cDTDRps +
             copy(AStr, StrToInt(VarToStr(SeSenao(I>0, I+2, I))), Length(AStr));

     Assinatura := '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"' + AID +
                    '<SignedInfo>' +
                      SeSenao((AProvedor in [proActcon, proNatal, proTinus]),
                       '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315#WithComments" />',
                       '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />') +
                     '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />' +
                     '<Reference URI="' + SeSenao(URI = '', '">', '#' + URI + '">') +
                      '<Transforms>' +
                       '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />' +
                       SeSenao((AProvedor in [proActcon, profintelISS, proGovBr,
                                proGovDigital, proISSNet, proNatal, proTinus]), '',
                       '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />') +
                      '</Transforms>' +
                      '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />' +
                      '<DigestValue></DigestValue>' +
                     '</Reference>' +
                    '</SignedInfo>' +
                    '<SignatureValue></SignatureValue>' +
                    '<KeyInfo>' +
                     '<X509Data>' +
//                       SeSenao((AProvedor in [proGovDigital]),
//                                       '<X509SubjectName></X509SubjectName>',
//                                       '') +
                       '<X509Certificate></X509Certificate>' +
                     '</X509Data>' +
                    '</KeyInfo>' +
                   '</Signature>';

     case AProvedor of
      profintelISS,
      proGoiania,
      proDigifred,
      proISSDigital,
      proISSe,
      pro4R,
      proFiorilli,
      proCoplan,
      proProdata,
      proVitoria,
      proPVH,
      proAgili,
      ProVirtual,
      proFreire,
      proLink3,
      proMitra,
      proGovDigital,
      proSisPMJP,
      proSystemPro,
      proNFSeBrasil,
      proSaatri:    begin
                     AStr := copy(AStr, 1, pos('</InfDeclaracaoPrestacaoServico>', AStr) - 1);
                     AStr := AStr + '</InfDeclaracaoPrestacaoServico>';
                     AStr := AStr + Assinatura;
                     AStr := AStr + '</Rps>';
                     // Alterado por Cleiver em 26/02/2013
                     if (AProvedor in [proGoiania, proProdata, proVitoria, proFiorilli, proVirtual{, proSystemPro}])
                        or ((AProvedor = proGovDigital) and (not ASincrono)) then // Alterado por Nilton Olher - 11/02/2015
                       AStr := AStr + '</GerarNfseEnvio>';
                    end;
      proTecnos:    begin
                     AStr := copy(AStr, 1, pos('</tcDeclaracaoPrestacaoServico>', AStr) - 1);
                     AStr := AStr + '</tcDeclaracaoPrestacaoServico>';
                     AStr := AStr + Assinatura;
                     AStr := AStr + '</Rps>';
                    end;
      else begin
            AStr := copy(AStr, 1, pos('</Rps>', AStr) - 1);
            AStr := AStr + Assinatura;
            AStr := AStr + '</Rps>';
            // Alterado por Cleiver em 26/02/2013
            if (AProvedor in [proGoiania, proProdata, proVitoria])
               or ((AProvedor = proGovDigital) and (not ASincrono)) then // Alterado por Nilton Olher - 11/02/2015
              AStr := AStr + '</GerarNfseEnvio>';
           end;
     end;
    end;
  end;

  if FileExists(ArqPFX) then
    XmlAss := NotaUtil.sign_file(PAnsiChar(AStr), PAnsiChar(ArqPFX), PAnsiChar(PFXSenha))
  else
   begin
    Cert  := TMemoryStream.Create;
    Cert2 := TStringStream.Create(ArqPFX);
    try
      Cert.LoadFromStream(Cert2);
      XmlAss := NotaUtil.sign_memory(PAnsiChar(AStr), PAnsiChar(ArqPFX), PAnsiChar(PFXSenha), Cert.Size, Cert.Memory);
    finally
      Cert2.Free;
      Cert.Free;
    end;
  end;

  // Removendo quebras de linha //
  XmlAss := StringReplace( XmlAss, #10, '', [rfReplaceAll] );
  XmlAss := StringReplace( XmlAss, #13, '', [rfReplaceAll] );

  // Removendo DTD //
  if ALote
   then XmlAss := StringReplace( XmlAss, cDTDLote, '', [] )
   else XmlAss := StringReplace( XmlAss, cDTDRps , '', [] );

  PosIni := Pos('<X509Certificate>', XmlAss) -1;
  PosFim := PosLast('<X509Certificate>', XmlAss);

  XmlAss := copy(XmlAss, 1, PosIni) + copy(XmlAss, PosFim, length(XmlAss));

  AXMLAssinado := XmlAss;

  //Não é possível assinar por delphi quando o id é minúsculo... mas se manter o id maiúsculo
  //não será validado o schema... uma função mais legível como stringreplace faz a assinatura sumir.
  if {(not ALote) and }(Aprovedor in [proPublica, proPronim,
                                      proLexsom, proSalvador])
   then begin
     I := pos('Id=', AXMLAssinado);
     if i>0 then
       AXMLAssinado[I] := 'i';
     I := pos('InfRps Id=', AXMLAssinado);
     if i>0 then
       AXMLAssinado[I + 7] := 'i';
   end;

  Result := True;
end;

{$ELSE}

// Alterado por Italo em 12/07/2012
function AssinarMSXML(XML : AnsiString;
                      Certificado : ICertificate2;
                      out XMLAssinado : AnsiString;
                      ALote: Boolean = False;
                      APrefixo3: string = '';
                      APrefixo4: string = '';
                      AProvedor: TnfseProvedor = proNenhum;
                      ASincrono: Boolean = False): Boolean;
var
 I, J, PosIni, PosFim, PosIniAssLote : Integer;
 LoteURI, URI, AID, Identificador, NameSpaceLote, EnviarLoteRps : String;
 AXML, Assinatura, xmlHeaderAntes, xmlHeaderDepois : AnsiString;
 xmldoc    : IXMLDOMDocument3;
 xmldsig   : IXMLDigitalSignature;
 dsigKey   : IXMLDSigKey;
 signedKey : IXMLDSigKey;
 xmlRoot   : IXMLDOMElement;
begin
  CoInitialize(nil);
  try
   AXML := XML;

   case AProvedor of
    proActcon:    begin
                    EnviarLoteRps := 'EnviarLoteRpsEnvio';
                    LoteURI := 'LoteRps';
                  end;
    proIssDsf:    begin
                    EnviarLoteRps := 'ReqEnvioLoteRPS';
                    LoteURI := 'Lote';
                  end;
    proInfisc:    begin
                    EnviarLoteRps := 'envioLote';
                    LoteURI := 'Lote';
                  end;
    proEquiplano: begin
                    EnviarLoteRps := 'enviarLoteRpsEnvio';
                    LoteURI := 'lote';
                  end;
    else          begin
                    // Tecnos utiliza apenas metodo sincrono com mesmo mais de 3 notas
                    if (ASincrono) or (AProvedor = proTecnos)
                     then EnviarLoteRps := 'EnviarLoteRpsSincronoEnvio'
                     else EnviarLoteRps := 'EnviarLoteRpsEnvio';
                    LoteURI := 'LoteRps';
                  end;
   end;

   if ALote
    then begin
     if AProvedor = proBetha then
       I := pos(EnviarLoteRps +' xmlns:ns3=', AXML)
     else I := pos(EnviarLoteRps +' xmlns=', AXML);
     if I = 0
      then NameSpaceLote := ' '
      else begin
       // Diego Gonçalves -- Correção pois estava duplicando o campo xmlns
       if AProvedor = proBetha then
         I := I + {25} Length(EnviarLoteRps + ' xmlns:ns3=')
       else I := I + {25} Length(EnviarLoteRps + ' xmlns=');
       J := pos('>', AXML);
       if AProvedor = proBetha then
         NameSpaceLote := ' xmlns:ns3=' + Copy(AXML, I, J - I)
       else NameSpaceLote := ' xmlns:ds1=' + Copy(AXML, I, J - I);
      end;

     if AProvedor = proIssDsf then begin
        I := I + {25} Length(EnviarLoteRps);
        J := pos('>', AXML);
        NameSpaceLote := Copy(AXML, I, J - I);
        I := Pos('xmlns:ns1=', NameSpaceLote);
        NameSpaceLote := ' ' + Copy(NameSpaceLote, I, J - I);
     end;

     if AProvedor = proInfisc then begin
        I := I + {25} Length(EnviarLoteRps);
        J := pos('>', AXML);
        NameSpaceLote := Copy(AXML, I, J - I);
        I := Pos('xmlns:ns1=', NameSpaceLote);
        NameSpaceLote := ' ' + Copy(NameSpaceLote, I, J - I);
        NameSpaceLote := ' envioLote versao="1.0"';
     end;

     Identificador := 'Id';
     I             := pos(LoteURI + ' Id=', AXML);
     if I = 0
      then begin
       Identificador := 'id';
       I             := pos(LoteURI + ' id=', AXML);
      end;
     if I = 0
      then begin
       Identificador := '';
       URI           := '';
      end
      else begin
       I := PosEx('"', AXML, I + 2);
       if I = 0
        then raise Exception.Create('Não encontrei inicio do URI: aspas inicial');
       J := PosEx('"', AXML, I + 1);
       if J = 0
        then raise Exception.Create('Não encontrei inicio do URI: aspas final');

       URI := copy(AXML, I + 1, J - I - 1);
      end;

     AXML := copy(AXML, 1, pos('</'+ APrefixo3 + EnviarLoteRps + '>', AXML) - 1);

     if (URI = '') or (AProvedor in [proRecife, proRJ, proAbaco, proIssDSF, proInfisc,
                                     proIssCuritiba, proFISSLex, proGovBR,
                                     proPublica, proPronim])
      then AID := '>'
//      else if AProvedor = proNatal then AID := ' ' + Identificador + '="Ass_lote">'
      else AID := ' ' + Identificador + '="AssLote_' + URI + '">';

     if AProvedor in [proAbaco, proIssCuritiba, proFISSLex, proPublica]
      then URI := '';

     AXML := AXML + '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"' + AID +
                     '<SignedInfo>' +
                      SeSenao((AProvedor in [proActcon, proNatal, proTinus]),
                       '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315#WithComments" />',
                       '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />') +
                      '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />' +
                     '<Reference URI="' + SeSenao(URI = '', '">', '#' + URI + '">') +
                      '<Transforms>' +
                       '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />' +
                        SeSenao((AProvedor in [proActcon, profintelISS, proGovBr, proGovDigital, proPronim,
                                                       proISSNet, proNatal, proIssDSF, proInfisc, proTinus]),
                                        '',
                                        '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />') +
                        SeSenao((AProvedor in [proIssDSF]),
                                        '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315#WithComments"/>',
                                        '') +
                      '</Transforms>' +
                      '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />' +
                      '<DigestValue></DigestValue>' +
                     '</Reference>' +
                     '</SignedInfo>' +
                     '<SignatureValue></SignatureValue>' +
                     '<KeyInfo>' +
                      '<X509Data>' +
//                       SeSenao((AProvedor in [proGovDigital]),
//                                       '<X509SubjectName></X509SubjectName>',
//                                       '') +
                       '<X509Certificate></X509Certificate>' +
                      '</X509Data>' +
                     '</KeyInfo>' +
                    '</Signature>';

     AXML := AXML + '</'+ APrefixo3 + EnviarLoteRps + '>';

    end
    else begin

     // Ao assinar um RPS a tag não possui prefixo
     APrefixo3 := '';

     if Pos('<Signature', AXML) <= 0
      then begin
       Identificador := 'Id';
       I             := pos('Id=', AXML);
       if I = 0
        then begin
         Identificador := 'id';
         I             := pos('id=', AXML);
         if I = 0
          then Identificador := '';
    //      raise Exception.Create('Não encontrei inicio do URI: Id=');
        end;
       if I <> 0
        then begin
         I := PosEx('"', AXML, I + 2);
         if I = 0
          then raise Exception.Create('Não encontrei inicio do URI: aspas inicial');
         J := PosEx('"', AXML, I + 1);
         if J = 0
          then raise Exception.Create('Não encontrei inicio do URI: aspas final');

         URI := copy(AXML, I + 1, J - I - 1);
        end
        else URI := '';

       // Alterado por Italo em 10/05/2013 - incluido na lista o proRJ
       if (URI = '') or (AProvedor in [proActcon, profintelISS, proRecife, proNatal,
             proRJ, proGovBR, proPronim{Dalvan}, proTecnos, proPublica, proTinus])
        then AID := '>'
        else AID := ' ' + Identificador + '="Ass_' + URI + '">';

       // Incluido por Italo em 23/04/2013
       if AProvedor in [proAbaco, proIssCuritiba, proFISSLex, proPublica]
        then URI := '';

       Assinatura := '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"' + AID +
                      '<SignedInfo>' +
                        SeSenao((AProvedor in [proActcon, proNatal, proTinus]),
                         '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315#WithComments" />',
                         '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />') +
                       '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />' +
                       '<Reference URI="' + SeSenao(URI = '', '">', '#' + URI + '">') +
                        '<Transforms>' +
                         '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />' +
                         SeSenao((AProvedor in [proActcon, profintelISS, proGovBr, proGovDigital, proPronim,
                                                        proISSNet, proNatal, proIssDSF, proInfisc, proTinus]),
                                         '',
                                         '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />') +
                         SeSenao((AProvedor in [proIssDSF]),
                                         '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315#WithComments"/>',
                                         '') +
                        '</Transforms>' +
                        '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />' +
                        '<DigestValue></DigestValue>' +
                       '</Reference>' +
                      '</SignedInfo>' +
                      '<SignatureValue></SignatureValue>' +
                      '<KeyInfo>' +
                       '<X509Data>' +
//                         SeSenao((AProvedor in [proGovDigital]),
//                                         '<X509SubjectName></X509SubjectName>',
//                                         '') +
                         '<X509Certificate></X509Certificate>' +
                       '</X509Data>' +
                      '</KeyInfo>' +
                     '</Signature>';

       case AProvedor of
        profintelISS,
        proGoiania,
        proDigifred,
        proISSDigital,
        proISSe,
        pro4R,
        proFiorilli,
        proCoplan,
        proProdata,
        proVitoria,
        proPVH,
        proAgili,
        proVirtual,
        proFreire,
        proLink3,
        proMitra,
        proGovDigital,
        proSisPMJP,
        proSystemPro,
        proSaatri:    begin
                       AXML := copy(AXML, 1, pos('</InfDeclaracaoPrestacaoServico>', AXML) - 1);
                       AXML := AXML + '</InfDeclaracaoPrestacaoServico>';
                       AXML := AXML + Assinatura;
                       AXML := AXML + '</Rps>';
                       // Alterado por Cleiver em 26/02/2013
                       if (AProvedor in [proGoiania, proProdata, proVitoria, proFiorilli, proVirtual{, proSystemPro}])
                          or ((AProvedor = proGovDigital) and (not ASincrono)) then // Alterado por Nilton Olher - 11/02/2015
                          AXML := AXML + '</GerarNfseEnvio>';
                      end;
        proTecnos:    begin
                       AXML := copy(AXML, 1, pos('</InfDeclaracaoPrestacaoServico>', AXML) - 1);
                       AXML := AXML + '</InfDeclaracaoPrestacaoServico>';
                       AXML := AXML + Assinatura;
                       AXML := AXML + '</tcDeclaracaoPrestacaoServico></Rps>';
                      end;
        else begin
              AXML := copy(AXML, 1, pos('</Rps>', AXML) - 1);
              AXML := AXML + Assinatura;
              AXML := AXML + '</Rps>';
              // Alterado por Cleiver em 26/02/2013
              if (AProvedor in [proGoiania, proProdata, proVitoria, proPublica{, proSystemPro}])
                 or ((AProvedor = proGovDigital) and (not ASincrono)) then // Alterado por Nilton Olher - 11/02/2015
                AXML := AXML + '</GerarNfseEnvio>';
             end;
       end;
      end;
    end;

   // Lendo Header antes de assinar //
   xmlHeaderAntes := '';

   I := pos('?>', AXML);
   if I > 0
    then xmlHeaderAntes := copy(AXML, 1, I + 1);

   xmldoc := CoDOMDocument50.Create;

   xmldoc.async              := False;
   xmldoc.validateOnParse    := False;
   xmldoc.preserveWhiteSpace := True;

   xmldsig := CoMXDigitalSignature50.Create;

   if (not xmldoc.loadXML(AXML))
    then raise Exception.Create('Não foi possível carregar o arquivo: ' + AXML);

   if AProvedor = proIssDSF then
   begin
     NameSpaceLote:=DSIGNS + ' xmlns:ns1="http://localhost:8080/WsNFe2/lote" xmlns:tipos="http://localhost:8080/WsNFe2/tp" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';
     xmldoc.setProperty('SelectionNamespaces', NameSpaceLote );
     xmlRoot := xmldoc.documentElement;
     xmlRoot.setAttribute('xsi:schemaLocation','http://localhost:8080/WsNFe2/lote http://localhost:8080/WsNFe2/xsd/ReqEnvioLoteRPS.xsd');
   end
   else if AProvedor = proInfisc then
   begin
     NameSpaceLote:=DSIGNS;
     xmldoc.setProperty('SelectionNamespaces', NameSpaceLote );
     xmlRoot := xmldoc.documentElement;
   end
   else
   begin
     xmldoc.setProperty('SelectionNamespaces', DSIGNS + NameSpaceLote);
   end;

   if ALote
    then begin
     if (AProvedor in [proEquiplano, proPronim, proIssDSF, proInfisc])
      then xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature')
     else if (URI <> '') and not (AProvedor in [proRecife, proRJ, proAbaco, proIssCuritiba, proFISSLex, proBetha, proPublica])
      then xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature[@' + Identificador + '="AssLote_' + URI + '"]')
//     else if AProvedor = proNatal
//      then xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature[@' + Identificador + '="Ass_' + URI + '"]')
     else if (URI <> '') and (AProvedor = proBetha)
      then xmldsig.signature := xmldoc.selectSingleNode('.//ns3:' + EnviarLoteRps + '/ds:Signature')
     else begin
       xmldsig.signature := xmldoc.selectSingleNode('.//ds1:' + EnviarLoteRps + '/ds:Signature');
      end;
    end
    else xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');

   if (xmldsig.signature = nil)
   then raise Exception.Create('É preciso carregar o template antes de assinar.');

   if NumCertCarregado <> Certificado.SerialNumber
    then CertStoreMem := nil;

   if  CertStoreMem = nil
    then begin
     CertStore := CoStore.Create;
     CertStore.Open(CAPICOM_CURRENT_USER_STORE, 'My', CAPICOM_STORE_OPEN_READ_ONLY);

     CertStoreMem := CoStore.Create;
     CertStoreMem.Open(CAPICOM_MEMORY_STORE, 'Memoria', CAPICOM_STORE_OPEN_READ_ONLY);

     Certs := CertStore.Certificates as ICertificates2;
     for i := 1 to Certs.Count do
      begin
       Cert := IInterface(Certs.Item[i]) as ICertificate2;
       if Cert.SerialNumber = Certificado.SerialNumber
        then begin
         CertStoreMem.Add(Cert);
          NumCertCarregado := Certificado.SerialNumber;
        end;
      end;
   end;

   OleCheck(IDispatch(Certificado.PrivateKey).QueryInterface(IPrivateKey,PrivateKey));
   xmldsig.store := CertStoreMem;

   dsigKey := xmldsig.createKeyFromCSP(PrivateKey.ProviderType, PrivateKey.ProviderName, PrivateKey.ContainerName, 0);
   if (dsigKey = nil)
    then raise Exception.Create('Erro ao criar a chave do CSP.');

   signedKey := xmldsig.sign(dsigKey, $00000002);
   if (signedKey <> nil)
    then begin
     XMLAssinado := xmldoc.xml;
     XMLAssinado := StringReplace( XMLAssinado, #10, '', [rfReplaceAll] );
     XMLAssinado := StringReplace( XMLAssinado, #13, '', [rfReplaceAll] );

     case AProvedor of
      profintelISS,
      proGoiania,
      proDigifred,
      proISSDigital,
      proISSe,
      pro4R,
      proCoplan,
      proProdata,
      proVitoria,
      proPVH,
      proAgili,
      proVirtual,
      proFreire,
      proLink3,
      proMitra,
      proGovDigital,
      proSisPMJP,
      proSystemPro,
      proSaatri: begin
                   //By Akai - L. Massao Aihara ==================================
                   //MUDA A ASSINATURA...
                   //para não criar uma variavel usei a XMLAssinado mesmo...
                   //como o acbr estava gerando algumas coisas que não tinha no exemplo da Prefeitura de PG
                   //so fiz um processo para remover o "excesso"!!!
                   if not ALote then
                    begin
                     AXML := copy(XML, 1, pos('</'+ APrefixo3 + 'InfDeclaracaoPrestacaoServico>', XML) - 1);
                     AXML := AXML + '</'+ APrefixo3 + 'InfDeclaracaoPrestacaoServico>';
                     I    := pos('<Signature', XMLAssinado);

                     XMLAssinado := copy(XMLAssinado, I, pos('</Signature>', XMLAssinado) - I);
                     I           := pos('>', XMLAssinado);
                     XMLAssinado := StringReplace(XMLAssinado, Copy(XMLAssinado, 54, I - 54), '', []);

  //                   I           := pos('<KeyInfo>', XMLAssinado);
  //                   XMLAssinado := StringReplace(XMLAssinado, copy(XMLAssinado, I, pos('</KeyInfo>', XMLAssinado) - I), '', []);
  //                   XMLAssinado := StringReplace(XMLAssinado, '</KeyInfo>', '', []);

                     AXML := AXML + XMLAssinado;
                     AXML := AXML + '</Signature>';
                     AXML := AXML + '</Rps>';

                     // Alterado por Cleiver em 26/02/2013
                     if (AProvedor in [proGoiania, proProdata, proVitoria, proVirtual{, proSystemPro}])
                        or ((AProvedor = proGovDigital) and (not ASincrono)) then // Alterado por Nilton Olher - 11/02/2015
                       AXML := AXML + '</GerarNfseEnvio>';

                     XMLAssinado := AXML;
                    end
                    else
                    //Se for o LOTE...
                    //essa parte não precisava, ja a prefeitura ja estava retornando a msg "correta"...
                    //mas como no exemplo da prefeitura estava diferente, alterei tbm...
                    begin
                     AXML := copy(XML, 1, pos('</LoteRps>', XML) - 1);
                     AXML := AXML + '</LoteRps>';
                     I    := pos('</LoteRps>', XMLAssinado);

                     XMLAssinado := copy(XMLAssinado, I, pos('</' + EnviarLoteRps + '>', XMLAssinado) - I);
                     XMLAssinado := StringReplace(XMLAssinado, '</LoteRps>', '', []);
                     I           := pos('>', XMLAssinado);
                     XMLAssinado := StringReplace(XMLAssinado, Copy(XMLAssinado, 54, I - 54), '', []);

  //                   I           := pos('<KeyInfo>', XMLAssinado);
  //                   XMLAssinado := StringReplace(XMLAssinado, copy(XMLAssinado, I, pos('</KeyInfo>', XMLAssinado) - I), '', []);
  //                   XMLAssinado := StringReplace(XMLAssinado, '</KeyInfo>', '', []);

                     AXML := AXML + XMLAssinado;
                     AXML := AXML + '</' + EnviarLoteRps + '>';

                     XMLAssinado := AXML;
                    end;
                   //Fim alteração Akai ==========================================

                   (*
                   // Variavel XML contem o XML original de entrada na função
                   // sem assinatura.
                   AXML := copy(XML, 1, pos('</'+ APrefixo3 + 'InfDeclaracaoPrestacaoServico>', XML) - 1);
                   AXML := AXML + '</'+ APrefixo3 + 'InfDeclaracaoPrestacaoServico>';
                   I    := pos('<Signature', XMLAssinado);
                   AXML := AXML + copy(XMLAssinado, I, pos('</Signature>', XMLAssinado) - I);
                   AXML := AXML + '</Signature>';
                   AXML := AXML + '</Rps>';

                   XMLAssinado := AXML;
                   *)
                 end;
     end; // fim do case

     if ALote
      then begin
       // Sugestão de Rodrigo Cantelli
       PosIniAssLote := Pos('</'+ APrefixo3 + 'LoteRps>', XMLAssinado);

       if PosIniAssLote = 0
        then PosIniAssLote := Pos('</LoteRps>', XMLAssinado) + length('</LoteRps>')
        else PosIniAssLote := PosIniAssLote + length('</'+ APrefixo3 + 'LoteRps>');

       PosIni      := PosEx('<SignatureValue>', XMLAssinado, PosIniAssLote) + length('<SignatureValue>');
       XMLAssinado := copy(XMLAssinado, 1, PosIni - 1) +
                      StringReplace( copy(XMLAssinado, PosIni, length(XMLAssinado)), ' ', '', [rfReplaceAll] );

       // Sugestão de Rodrigo Cantelli
       PosIniAssLote := Pos('</'+ APrefixo3 + 'LoteRps>', XMLAssinado);

       if PosIniAssLote = 0
        then PosIniAssLote := Pos('</LoteRps>', XMLAssinado) + length('</LoteRps>')
        else PosIniAssLote := PosIniAssLote + length('</'+ APrefixo3 + 'LoteRps>');

       PosIni      := PosEx('<X509Certificate>', XMLAssinado, PosIniAssLote) - 1;
       PosFim      := PosLast('<X509Certificate>', XMLAssinado);
       XMLAssinado := copy(XMLAssinado, 1, PosIni) +
                      copy(XMLAssinado, PosFim, length(XMLAssinado));
      end
      else begin
       PosIni      := Pos('<SignatureValue>', XMLAssinado) + length('<SignatureValue>');
       XMLAssinado := copy(XMLAssinado, 1, PosIni - 1) +
                      StringReplace( copy(XMLAssinado, PosIni, length(XMLAssinado)), ' ', '', [rfReplaceAll] );
       PosIni      := Pos('<X509Certificate>', XMLAssinado) - 1;
       PosFim      := PosLast('<X509Certificate>', XMLAssinado);
       XMLAssinado := copy(XMLAssinado, 1, PosIni) +
                      copy(XMLAssinado, PosFim, length(XMLAssinado));
      end;
    end
    else raise Exception.Create('Assinatura Falhou.');

   if xmlHeaderAntes <> ''
    then begin
     I := pos('?>',XMLAssinado);
     if I > 0
      then begin
       xmlHeaderDepois := copy(XMLAssinado,1,I+1);
       if xmlHeaderAntes <> xmlHeaderDepois
        then XMLAssinado := StuffString(XMLAssinado,1,length(xmlHeaderDepois),xmlHeaderAntes);
      end
      else XMLAssinado := xmlHeaderAntes + XMLAssinado;
    end;

    //Não é possível assinar por delphi quando o id é minúsculo... mas se manter o id maiúsculo
    //não será validado o schema... uma função mais legível como stringreplace faz a assinatura sumir.
    if {(not ALote) and }(Aprovedor in [proPublica, proPronim,
                                        proLexsom, proSalvador])
     then begin
       I := pos('Id=', XMLAssinado);
       if i>0 then
         XMLAssinado[I] := 'i';
       I := pos('InfRps Id=', XMLAssinado);
       if i>0 then
         XMLAssinado[I + 7] := 'i';
     end;

   dsigKey   := nil;
   signedKey := nil;
   xmldoc    := nil;
   xmldsig   := nil;

   Result := True;
  finally
   CoUninitialize;
  end;
end;
{$ENDIF}

{$IFDEF ACBrNFSeOpenSSL}
class function NotaUtil.Assinar(const AXML, ArqPFX, PFXSenha: AnsiString;
                                out AXMLAssinado, FMensagem: AnsiString;
                                ALote: Boolean = False;
                                APrefixo3: string = '';
                                APrefixo4: string = '';
                                AProvedor: TnfseProvedor = proNenhum;
                                ASincrono: Boolean = False): Boolean;
{$ELSE}
// Alterado por Italo em 12/07/2012
class function NotaUtil.Assinar(const AXML: AnsiString;
                                Certificado : ICertificate2;
                                out AXMLAssinado, FMensagem: AnsiString;
                                ALote: Boolean = False;
                                APrefixo3: string = '';
                                APrefixo4: string = '';
                                AProvedor: TnfseProvedor = proNenhum;
                                ASincrono: Boolean = False): Boolean;
{$ENDIF}
begin
{$IFDEF ACBrNFSeOpenSSL}
  Result := AssinarLibXML(AXML, ArqPFX, PFXSenha, AXMLAssinado, FMensagem,
                          ALote, APrefixo3, APrefixo4, AProvedor, ASincrono);
{$ELSE}
  // Alterado por Italo em 12/07/2012
  Result := AssinarMSXML(AXML, Certificado, AXMLAssinado,
                         ALote, APrefixo3, APrefixo4, AProvedor, ASincrono);
{$ENDIF}
end;

{$IFDEF ACBrNFSeOpenSSL}
class function NotaUtil.AssinarXML(AXML, FURISig, FURIRef, FTagI, FTagF, ArqPFX, PFXSenha: AnsiString;
                             out AXMLAssinado, FMensagem: AnsiString;
                             AProvedor: TnfseProvedor = proNenhum;
                             ASincrono: Boolean = False): Boolean;
var
 I, Tipo, PosIni, PosFim : Integer;
 XmlAss : AnsiString;
 Cert: TMemoryStream;
 Cert2: TStringStream;
 EnviarLoteRps: String; 
begin
  Tipo := 1;

 if ASincrono
  then EnviarLoteRps := 'EnviarLoteRpsSincronoEnvio'
  else EnviarLoteRps := 'EnviarLoteRpsEnvio';

  I    := pos( '<' + EnviarLoteRps, AXML );

  if I = 0  then
   begin
     I := pos( '<Rps', AXML );
     if I > 0 then
        Tipo := 2;
   end;

  AXML := AXML + '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"'+
                       SeSenao(FURISig = '', '',' Id="Ass_'+ FURISig +'"')+'>'+
                  '<SignedInfo>'+
                   '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />'+
                   '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />'+
                   '<Reference URI="'+SeSenao(FURIRef = '', '','#'+FURIRef)+'">'+
                    '<Transforms>'+
                     '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />'+
                     SeSenao((AProvedor in [profintelISS, proGovBr, proGovDigital, proPronim{Dalvan}, proISSNet]), '',
                     '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />') +
                    '</Transforms>'+
                    '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />'+
                    '<DigestValue></DigestValue>'+
                   '</Reference>'+
                  '</SignedInfo>'+
                  '<SignatureValue></SignatureValue>'+
                  '<KeyInfo>'+
                   '<X509Data>'+
//                     SeSenao((AProvedor in [proGovDigital]),
//                                     '<X509SubjectName></X509SubjectName>',
//                                     '') +
                     '<X509Certificate></X509Certificate>'+
                   '</X509Data>'+
                  '</KeyInfo>'+
                 '</Signature>';

  AXML := FTagI + AXML + FTagF;

  //// Adicionando Cabeçalho DTD, necessário para xmlsec encontrar o ID ////
  I := pos( '?>', AXML );

  case Tipo of
   1: AXML := copy(AXML, 1, StrToInt(VarToStr(SeSenao(I>0, I+1, I)))) +
              cDTDLote +
              copy(AXML, StrToInt(VarToStr(SeSenao(I>0, I+2, I))), Length(AXML));
   2: AXML := copy(AXML, 1, StrToInt(VarToStr(SeSenao(I>0, I+1, I)))) +
              cDTDRps +
              copy(AXML, StrToInt(VarToStr(SeSenao(I>0, I+2, I))), Length(AXML));
  end;

  if FileExists(ArqPFX) then
    XmlAss := NotaUtil.sign_file( PAnsiChar(AXML), PAnsiChar(ArqPFX), PAnsiChar(PFXSenha) )
  else
   begin
    Cert  := TMemoryStream.Create;
    Cert2 := TStringStream.Create(ArqPFX);
    try
      Cert.LoadFromStream(Cert2);
      XmlAss := NotaUtil.sign_memory(PAnsiChar(AXML), PAnsiChar(ArqPFX), PAnsiChar(PFXSenha), Cert.Size, Cert.Memory);
    finally
      Cert2.Free;
      Cert.Free;
    end;
  end;

  // Removendo quebras de linha //
  XmlAss := StringReplace( XmlAss, #10, '', [rfReplaceAll] );
  XmlAss := StringReplace( XmlAss, #13, '', [rfReplaceAll] );

  // Removendo DTD //
  case Tipo of
   1: XmlAss := StringReplace( XmlAss, cDTDLote, '', [] );
   2: XmlAss := StringReplace( XmlAss, cDTDRps , '', [] );
  end;

  PosIni := Pos( '<X509Certificate>', XmlAss ) -1;
  PosFim := PosLast( '<X509Certificate>', XmlAss );

  XmlAss := copy( XmlAss, 1, PosIni ) + copy( XmlAss, PosFim, length(XmlAss) );

  AXMLAssinado := XmlAss;

  //Não é possível assinar por delphi quando o id é minúsculo... mas se manter o id maiúsculo
  //não será validado o schema... uma função mais legível como stringreplace faz a assinatura sumir.
  if Aprovedor in [proPronim, proPublica, proLexsom, proSalvador] then
  begin
    I := pos('Id=', AXMLAssinado);
    if i>0 then
      AXMLAssinado[I] := 'i';
  end;

  Result := True;
end;
{$ELSE}
class function NotaUtil.AssinarXML(AXML, FURISig, FURIRef, FTagI, FTagF: AnsiString; Certificado : ICertificate2;
                                   out AXMLAssinado, FMensagem: AnsiString;
                                   AProvedor: TnfseProvedor = proNenhum;
                                   ASincrono: Boolean = False): Boolean;
var
 I, PosIni, PosFim : Integer;
 Numero, EnviarLoteRps: String;

 xmlHeaderAntes, xmlHeaderDepois : AnsiString;

 xmldoc    : IXMLDOMDocument3;
 xmldsig   : IXMLDigitalSignature;
 dsigKey   : IXMLDSigKey;
 signedKey : IXMLDSigKey;
begin
   CoInitialize(nil);
   Try
   if ASincrono
    then EnviarLoteRps := 'EnviarLoteRpsSincronoEnvio'
    else EnviarLoteRps := 'EnviarLoteRpsEnvio';

   if Copy(FURIRef, 1, 4) = 'http'
    then Numero := ''
    else Numero := '#';

   AXML := AXML + '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"';

   if AProvedor = proFiorilli then
      AXML := AXML + SeSenao(FURISig = '', '',' Id="Ass_'+ StringReplace(FURISig, '/', '', [rfReplaceAll]) +'"')+'>'
   else
      AXML := AXML + SeSenao(FURISig = '', '',' Id="Ass_'+ FURISig +'"')+'>';

   AXML := AXML +  '<SignedInfo>'+
                    '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />'+
                    '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />'+
                    '<Reference URI="'+SeSenao(FURIRef = '', '',Numero+FURIRef)+'">'+
                     '<Transforms>'+
                      '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />'+
                      SeSenao((AProvedor in [profintelISS, proGovBr, proGovDigital, proPronim{Dalvan}, proISSNet, proInfisc]), '',
                      '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315' +
                      SeSenao(AProvedor in [proISSDSF,proInfisc], '#WithComments', '') + '" />') +
                     '</Transforms>'+
                     '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />'+
                     '<DigestValue></DigestValue>'+
                    '</Reference>'+
                   '</SignedInfo>'+
                   '<SignatureValue></SignatureValue>'+
                   '<KeyInfo>'+
                    '<X509Data>'+
//                      SeSenao((AProvedor in [proGovDigital]),
//                                      '<X509SubjectName></X509SubjectName>',
//                                      '') +
                      '<X509Certificate></X509Certificate>'+
                    '</X509Data>'+
                   '</KeyInfo>'+
                  '</Signature>';

 AXML := FTagI + AXML + FTagF;

 // Lendo Header antes de assinar //
 xmlHeaderAntes := '';
 I              := pos('?>',AXML);
 if I > 0
  then xmlHeaderAntes := copy(AXML,1,I+1);

 xmldoc := CoDOMDocument50.Create;

 xmldoc.async              := False;
 xmldoc.validateOnParse    := False;
 xmldoc.preserveWhiteSpace := True;

 xmldsig := CoMXDigitalSignature50.Create;

 if (not xmldoc.loadXML(AXML) )
  then raise Exception.Create('Não foi possível carregar o arquivo: '+AXML);

 xmldoc.setProperty('SelectionNamespaces', DSIGNS);

 xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');

 if (xmldsig.signature = nil)
  then raise Exception.Create('É preciso carregar o template antes de assinar.');

 if NumCertCarregado <> Certificado.SerialNumber
  then CertStoreMem := nil;

 if  CertStoreMem = nil
  then begin
   CertStore := CoStore.Create;
   CertStore.Open(CAPICOM_CURRENT_USER_STORE, 'My', CAPICOM_STORE_OPEN_READ_ONLY);

   CertStoreMem := CoStore.Create;
   CertStoreMem.Open(CAPICOM_MEMORY_STORE, 'Memoria', CAPICOM_STORE_OPEN_READ_ONLY);

   Certs := CertStore.Certificates as ICertificates2;
   for i := 1 to Certs.Count do
    begin
     Cert := IInterface(Certs.Item[i]) as ICertificate2;
     if Cert.SerialNumber = Certificado.SerialNumber
      then begin
       CertStoreMem.Add(Cert);
        NumCertCarregado := Certificado.SerialNumber;
      end;
    end;
 end;

 OleCheck(IDispatch(Certificado.PrivateKey).QueryInterface(IPrivateKey,PrivateKey));
 xmldsig.store := CertStoreMem;

 dsigKey := xmldsig.createKeyFromCSP(PrivateKey.ProviderType, PrivateKey.ProviderName, PrivateKey.ContainerName, 0);
 if (dsigKey = nil)
  then raise Exception.Create('Erro ao criar a chave do CSP.');

 signedKey := xmldsig.sign(dsigKey, $00000002);
 if (signedKey <> nil)
  then begin
   AXMLAssinado := xmldoc.xml;
   AXMLAssinado := StringReplace( AXMLAssinado, #10, '', [rfReplaceAll] );
   AXMLAssinado := StringReplace( AXMLAssinado, #13, '', [rfReplaceAll] );

   PosIni       := Pos('<SignatureValue>',AXMLAssinado)+length('<SignatureValue>');
   AXMLAssinado := copy(AXMLAssinado,1,PosIni-1)+StringReplace( copy(AXMLAssinado,PosIni,length(AXMLAssinado)), ' ', '', [rfReplaceAll] );
   PosIni       := Pos('<X509Certificate>',AXMLAssinado)-1;
   PosFim       := PosLast('<X509Certificate>',AXMLAssinado);
   AXMLAssinado := copy(AXMLAssinado, 1, PosIni) +
                   copy(AXMLAssinado, PosFim, length(AXMLAssinado));
  end
  else raise Exception.Create('Assinatura Falhou.');

 if xmlHeaderAntes <> ''
  then begin
   I := pos('?>', AXMLAssinado);
   if I > 0
    then begin
     xmlHeaderDepois := copy(AXMLAssinado,1,I+1);
     if xmlHeaderAntes <> xmlHeaderDepois
      then AXMLAssinado := StuffString(AXMLAssinado,1,length(xmlHeaderDepois),xmlHeaderAntes);
    end
    else AXMLAssinado := xmlHeaderAntes + AXMLAssinado;
  end;

  //Não é possível assinar por delphi quando o id é minúsculo... mas se manter o id maiúsculo
  //não será validado o schema... uma função mais legível como stringreplace faz a assinatura sumir.
  if Aprovedor in [proPronim,proPublica, proLexsom, proSalvador] then
  begin
    I := pos('Id=', AXMLAssinado);
    if i>0 then
      AXMLAssinado[I] := 'i';
  end;

 dsigKey   := nil;
 signedKey := nil;
 xmldoc    := nil;
 xmldsig   := nil;

 Result := True;
   Finally
      CoUninitialize;
   End;
end;
{$ENDIF}

{$IFDEF ACBrNFSeOpenSSL}
function ValidaLibXML(const AXML: AnsiString;
                      var AMsg: AnsiString;
                      const APathSchemas: string = '';
                      URL: string = '';
                      Servico: string = ''): Boolean;
var
 doc, schema_doc : xmlDocPtr;
 parser_ctxt : xmlSchemaParserCtxtPtr;
 schema : xmlSchemaPtr;
 valid_ctxt : xmlSchemaValidCtxtPtr;
 schemError : xmlErrorPtr;
 schema_filename : AnsiString;

 // schema_filename : PChar;
// filename : String;
// Tipo, I : Integer;
begin
 if not DirectoryExists(SeSenao(EstaVazio(APathSchemas),
                        PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas',
                        PathWithDelim(APathSchemas)))
  then raise Exception.Create('Diretório de Schemas não encontrado' + sLineBreak +
                              SeSenao(EstaVazio(APathSchemas),
                              PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas',
                              PathWithDelim(APathSchemas)));

 if EstaVazio(APathSchemas)
  then schema_filename := PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\' + Servico
  else schema_filename := PathWithDelim(APathSchemas) + Servico;

 if not FilesExists(schema_filename)
  then raise Exception.Create('Arquivo [' + schema_filename + '] não encontrado.');

// schema_filename := pchar(filename);

// if RightStr(URL, 1) = '/'
//  then Schema.add( URL + Servico, schema_filename )
//  else Schema.add( URL, schema_filename );

// doc         := nil;
// schema_doc  := nil;
// parser_ctxt := nil;
// schema      := nil;
// valid_ctxt  := nil;

 doc := xmlParseDoc(PAnsiChar(Axml));
 if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
  begin
    AMsg   := 'Erro: unable to parse';
    Result := False;
    exit;
  end;

 schema_doc := xmlReadFile(Pansichar(ACBrStr(schema_filename)), nil, XML_DETECT_IDS);

//  the schema cannot be loaded or is not well-formed
 if (schema_doc = nil) then
  begin
    AMsg   := 'Erro: Schema não pode ser carregado ou está corrompido';
    Result := False;
    exit;
  end;

 parser_ctxt := xmlSchemaNewDocParserCtxt(schema_doc);
// unable to create a parser context for the schema */
 if (parser_ctxt = nil) then
  begin
    xmlFreeDoc(schema_doc);
    AMsg   := 'Erro: unable to create a parser context for the schema';
    Result := False;
    exit;
  end;

 schema := xmlSchemaParse(parser_ctxt);
// the schema itself is not valid
 if (schema = nil) then
  begin
    xmlSchemaFreeParserCtxt(parser_ctxt);
    xmlFreeDoc(schema_doc);
    AMsg   := 'Error: the schema itself is not valid ['+schema_filename+']';
    Result := False;
    exit;
  end;

 valid_ctxt := xmlSchemaNewValidCtxt(schema);
//   unable to create a validation context for the schema */
 if (valid_ctxt = nil) then
  begin
    xmlSchemaFree(schema);
    xmlSchemaFreeParserCtxt(parser_ctxt);
    xmlFreeDoc(schema_doc);
    AMsg   := 'Error: unable to create a validation context for the schema';
    Result := False;
    exit;
  end;

 if (xmlSchemaValidateDoc(valid_ctxt, doc) <> 0) then
  begin
    schemError := xmlGetLastError();
    AMsg       := IntToStr(schemError^.code)+' - '+schemError^.message;
    Result     := False;
    exit;
  end;

 xmlSchemaFreeValidCtxt(valid_ctxt);
 xmlSchemaFree(schema);
 xmlSchemaFreeParserCtxt(parser_ctxt);
 xmlFreeDoc(schema_doc);
 Result := True;
end;

{$ELSE}

function ValidaMSXML(XML: AnsiString;
                     out Msg: AnsiString;
                     const APathSchemas: string = '';
                     URL: string = '';
                     Servico: string = ''): Boolean;
var
 DOMDocument     : IXMLDOMDocument3;
 ParseError      : IXMLDOMParseError;
 Schema          : XMLSchemaCache;
 schema_filename : String;
begin
  CoInitialize(nil);
  try
    DOMDocument                  := CoDOMDocument50.Create;
    DOMDocument.async            := False;
    DOMDocument.resolveExternals := False;
    DOMDocument.validateOnParse  := True;
    DOMDocument.loadXML(XML);

    Schema := CoXMLSchemaCache50.Create;

    if not DirectoryExists(SeSenao(EstaVazio(APathSchemas),
                           PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas',
                           PathWithDelim(APathSchemas)))
     then raise Exception.Create('Diretório de Schemas não encontrado' + sLineBreak +
                                 SeSenao(EstaVazio(APathSchemas),
                                 PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas',
                                 PathWithDelim(APathSchemas)));

    schema_filename := SeSenao(EstaVazio(APathSchemas),
                       PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                       PathWithDelim(APathSchemas)) + Servico;

    if not FilesExists(schema_filename)
     then raise Exception.Create('Arquivo ' + schema_filename + ' não encontrado.');

    if RightStr(URL, 1) = '/'
     then Schema.add( URL + Servico, schema_filename )
     else Schema.add( URL, schema_filename );

    DOMDocument.schemas := Schema;

    ParseError := DOMDocument.validate;
    Result     := (ParseError.errorCode = 0);
    Msg        := ParseError.reason;

    DOMDocument := nil;
    ParseError  := nil;
    Schema      := nil;
  finally
    CoUninitialize;
  end;
end;
{$ENDIF}

class function NotaUtil.Valida(const AXML: AnsiString;
                               var AMsg: AnsiString;
                               const APathSchemas: string = '';
                               AURL: string = '';
                               AServico: string = '';
                               APrefixo: string = ''): Boolean;
begin
{$IFDEF ACBrNFSeOpenSSL}
  Result := ValidaLibXML(AXML, AMsg, APathSchemas, AURL, AServico);
{$ELSE}
  Result := ValidaMSXML(AXML, AMsg, APathSchemas, AURL, AServico);
{$ENDIF}
end;

class function NotaUtil.RetirarPrefixos(const AXML: String): String;
var
 XML: string;
begin
 XML := StringReplace( AXML, 'ns1:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'ns2:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'ns3:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'ns4:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'ns5:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'tc:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'ii:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'p1:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'nfse:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'soap:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'soap12:', '', [rfReplaceAll] );
 XML := StringReplace( XML, 'SOAP-ENV:', '', [rfReplaceAll] );

 result := XML;
end;

class function NotaUtil.VersaoXML(AXML: String): String;
var
 i: Integer;
begin
 i := Pos( '<Cidade>', AXML );
 if i > 0
  then result := '1'
  else result := '2';
end;

class function NotaUtil.PathWithDelim( const APath : String ) : String;
begin
  Result := Trim(APath);
  if Result <> '' then
     if RightStr(Result,1) <> PathDelim then   { Tem delimitador no final ? }
        Result := Result + PathDelim;
end;

class function NotaUtil.RetornarConteudoEntre(const Frase, Inicio, Fim: string): string;
var
  i: integer;
  s: string;
begin
  result := '';
  i      := pos(Inicio, Frase);
  if i = 0 then
    exit;
  s      := Copy(Frase, i + length(Inicio), maxInt);
  result := Copy(s, 1, pos(Fim, s) - 1);
end;

class function NotaUtil.ChaveAcesso(AUF: Integer; ADataEmissao: TDateTime;
  ACNPJ: String; ASerie, ANumero, ACodigo: Integer; AModelo: Integer): String;
var
  vUF, vDataEmissao, vSerie, vNumero,
  vCodigo, vModelo: String;
begin
  vUF          := Poem_Zeros(AUF, 2);
  vDataEmissao := FormatDateTime('YYMM', ADataEmissao);
  vModelo      := Poem_Zeros(AModelo, 2);
  vSerie       := Poem_Zeros(ASerie, 3);
  vNumero      := Poem_Zeros(ANumero, 9);
  vCodigo      := Poem_Zeros(ACodigo, 9);

  Result := vUF+vDataEmissao+ACNPJ+vModelo+vSerie+vNumero+vCodigo;
//  Result := Result+NotaUtil.Modulo11(Result);
end;

class function NotaUtil.ObterNomeMunicipio(const xMun, xUF: string; const cMun: integer): string;
var
  i: integer;
  PathArquivo, Codigo, Cidade: string;
  List: TstringList;
begin
  result := '';
  PathArquivo := ExtractFilePath(Application.ExeName) + 'MunIBGE\MunIBGE-UF' + InttoStr(UFparaCodigo(xUF)) + '.txt';
  if FileExists(PathArquivo) then
   begin
     List := TstringList.Create;
     List.LoadFromFile(PathArquivo);
     Codigo := IntToStr(cMun);
     Cidade := '';
     i := 0;
     while (i < list.count) and (Cidade = '') do
      begin
        if pos(Codigo, List[i]) > 0 then
         begin
          Cidade := List[i];
          Cidade := copy(Cidade,9,Length(Cidade));
          if Utf8ToAnsi(Trim(Cidade)) <> '' then
             Cidade := Utf8ToAnsi(Trim(Cidade));
         end;
        inc(i);
      end;
     List.free;
   end;

  Result := Cidade;

  if result = '' then
    result := xMun;
end;

class function NotaUtil.ObterCodigoMunicipio(const xMun, xUF: string): integer;
var
  i: integer;
  PathArquivo: string;
  List: TstringList;
begin
  result := 0;
  PathArquivo :=  PathWithDelim(ExtractFilePath(Application.ExeName))+ 'MunIBGE\MunIBGE-UF' + InttoStr(UFparaCodigo(xUF)) + '.txt';
  if FileExists(PathArquivo) then
   begin
     List := TstringList.Create;
     List.LoadFromFile(PathArquivo);
     i := 0;
     while (i < list.count) and (result = 0) do
      begin
       if pos(UpperCase(TiraAcentos(xMun)), UpperCase(TiraAcentos(List[i]))) > 0 then
          result := StrToInt(Trim(copy(list[i],1,7)));
       inc(i);
      end;
     List.free;
   end;
end;

{$IFDEF DELPHI2009_UP}
class function NotaUtil.LoadXML(CaminhoArquivo: string): TEncoding;
var
 ArquivoXML: TStringList;
begin
   ArquivoXML := TStringList.Create;
   ArquivoXML.LoadFromFile(CaminhoArquivo, TEncoding.UTF8);
   if ArquivoXML.Text <> '' then
     Result := TEncoding.UTF8
   else begin
     {$IFDEF VER230}
     ArquivoXML.LoadFromFile(CaminhoArquivo ,TEncoding.ANSI);
     if ArquivoXML.Text <> '' then
       Result := TEncoding.ANSI
     else begin
     {$ENDIF}
       ArquivoXML.LoadFromFile(CaminhoArquivo, TEncoding.ASCII);
       if ArquivoXML.Text <> '' then
         Result := TEncoding.ASCII
       else begin
         ArquivoXML.LoadFromFile(CaminhoArquivo,TEncoding.Unicode);
         if ArquivoXML.Text <> '' then
           Result := TEncoding.Unicode
         else begin
           ArquivoXML.LoadFromFile(CaminhoArquivo ,TEncoding.UTF7);
           if ArquivoXML.Text <> '' then
             Result := TEncoding.UTF7
           else
             Result := TEncoding.Default;
         end;
       end;
     {$IFDEF VER230}
     end;
     {$ENDIF}
   end;
   ArquivoXML.Free;
end;
{$ENDIF}

class function NotaUtil.GerarNomeNFSe(AUF: Integer; ADataEmissao: TDateTime; ACNPJ: String;
                                 ANumero:Integer; AModelo: Integer = 56): String;
var
  vUF, vDataEmissao, vNumero, vModelo: String;
begin
  vUF          := Poem_Zeros(AUF, 2);
  vDataEmissao := FormatDateTime('YYMM', ADataEmissao);
  vModelo      := Poem_Zeros(AModelo, 2);
  vNumero      := Poem_Zeros(ANumero, 9);

  Result := vUF + vDataEmissao + ACNPJ + vModelo + vNumero;
end;

class function NotaUtil.ObterDescricaoServico(cCodigo: String): AnsiString;
var
 i           : Integer;
 PathArquivo : String;
 List        : TstringList;
begin
 result := '';
 PathArquivo :=  PathWithDelim(ExtractFilePath(Application.ExeName))+ 'TabServicos.txt';
 if (FileExists(PathArquivo)) and (cCodigo <> '')
  then begin
   List := TstringList.Create;
   try
    List.LoadFromFile(PathArquivo);
    i := 0;
    while (i < list.count) and (result = '') do
     begin
      if pos(cCodigo, List[i]) > 0
       then result := Trim(stringReplace(list[i], ccodigo, '', []));
      inc(i);
    end;
   finally
    List.free;
   end;
  end;
end;

end.
