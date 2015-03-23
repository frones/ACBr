{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeUtil;

interface

uses
{$IFNDEF ACBrMDFeOpenSSL}
  ACBrCAPICOM_TLB, ACBrMSXML2_TLB, JwaWinCrypt,
{$ENDIF}
  Classes, Forms,
{$IFDEF FPC}
  LResources, Controls, Graphics, Dialogs,
{$ELSE}
  StrUtils, Activex,
{$ENDIF}
  ACBrMDFeConfiguracoes, pmdfeConversao, pcnConversao, pmdfeMDFe;

{$IFDEF ACBrMDFeOpenSSL}
const
  cDTD     = '<!DOCTYPE test [<!ATTLIST infMDFe Id ID #IMPLIED>]>';
  cDTDEven = '<!DOCTYPE test [<!ATTLIST infEvento Id ID #IMPLIED>]>';
{$ELSE}
const
  DSIGNS = 'xmlns:ds="http://www.w3.org/2000/09/xmldsig#"';
{$ENDIF}
{$IFNDEF ACBrMDFeOpenSSL}
var
  CertStore: IStore3;
  CertStoreMem: IStore3;
  PrivateKey: IPrivateKey;
  Certs: ICertificates2;
  Cert: ICertificate2;
  NumCertCarregado: String;
{$ENDIF}

type
  MDFeUtil = class
  private
    // Estados Emissores pela Sefaz Virtual RS (Rio Grande do Sul):
    // AC, AL, AP, AM, BA, CE, DF, ES, GO, MA, PA, PB, PR, PE, PI, RJ, RN,
    // RO, RR, SC, SE, TO.
    class function GetURLSVRS(AAmbiente: Integer; ALayOut: TLayOutMDFe): WideString;

    class function GetURLMG(AAmbiente: Integer; ALayOut: TLayOutMDFe): WideString;
    class function GetURLRS(AAmbiente: Integer; ALayOut: TLayOutMDFe): WideString;
    class function GetURLSP(AAmbiente: Integer; ALayOut: TLayOutMDFe): WideString;
    class function GetURLMT(AAmbiente: Integer; ALayOut: TLayOutMDFe): WideString;
    class function GetURLMS(AAmbiente: Integer; ALayOut: TLayOutMDFe): WideString;

  protected

  public
{$IFDEF ACBrMDFeOpenSSL}
    class function sign_file(const Axml: PAnsiChar; const key_file: PAnsiChar; const senha: PAnsiChar): AnsiString;
    class function sign_memory(const Axml: PAnsiChar; const key_file: PAnsichar; const senha: PAnsiChar; Size: Cardinal; Ponteiro: Pointer): AnsiString;
    class procedure InitXmlSec;
    class procedure ShutDownXmlSec;
{$ENDIF}
    class function GetURL(const AUF, AAmbiente, FormaEmissao: Integer; ALayOut: TLayOutMDFe): WideString;
    class function Valida(const AXML: AnsiString; var AMsg: AnsiString; const APathSchemas: string = ''): Boolean;
    class function FormatarChaveAcesso(AValue: String; Mascara: Boolean = False): String;
    class function FormatarNumMDFe(const AValue: Integer): string;
    class function FormatarValor(mask: TpcteMask; const AValue: real): string;
    class function GerarChaveContingencia(FMDFe:TMDFe): String;
    class function FormatarChaveContingencia(AValue: String): String;
    class function ValidaAssinatura(const AXML: AnsiString;  var AMsg: AnsiString): Boolean;
{$IFDEF ACBrMDFeOpenSSL}
    class function Assinar(const AXML, ArqPFX, PFXSenha: AnsiString; out AXMLAssinado, FMensagem: AnsiString): Boolean;
{$ELSE}
    class function Assinar(const AXML: AnsiString; Certificado: ICertificate2; out AXMLAssinado, FMensagem: AnsiString): Boolean;
{$ENDIF}
    class function UFtoCUF(UF: String): Integer;
    class function IdentificaTipoSchema(Const AXML: AnsiString; var I: Integer): Integer;
  end;

implementation

uses
 {$IFDEF ACBrMDFeOpenSSL}
  libxml2, libxmlsec, libxslt,
 {$ELSE}
  ComObj,
 {$ENDIF}
  Sysutils, Variants, ACBrUtil, ACBrDFeUtil, pcnAuxiliar;

{ MDFeUtil }

{$IFDEF ACBrMDFeOpenSSL}
class procedure MInitXmlSec;
begin
  { Init libxml and libxslt libraries }
  xmlInitParser();
  __xmlLoadExtDtdDefaultValue^ := XML_DETECT_IDS or XML_COMPLETE_ATTRS;
  xmlSubstituteEntitiesDefault(1);
  __xmlIndentTreeOutput^ := 1;

  { Init xmlsec library }
  if (xmlSecInit() < 0) then
    raise Exception.Create('Error: xmlsec initialization failed.');

  { Check loaded library version }
  if (xmlSecCheckVersionExt(1, 2, 8, xmlSecCheckVersionABICompatible) <> 1) then
    raise Exception.Create('Error: loaded xmlsec library version is not compatible.');

  (* Load default crypto engine if we are supporting dynamic
   * loading for xmlsec-crypto libraries. Use the crypto library
   * name ("openssl", "nss", etc.) to load corresponding
   * xmlsec-crypto library.
   *)
  if (xmlSecCryptoDLLoadLibrary('openssl') < 0) then
    raise Exception.Create('Error: unable to load default xmlsec-crypto library. Make sure'#10 +
      'that you have it installed and check shared libraries path'#10 +
      '(LD_LIBRARY_PATH) environment variable.');

  { Init crypto library }
  if (xmlSecCryptoAppInit(nil) < 0) then
    raise Exception.Create('Error: crypto initialization failed.');

  { Init xmlsec-crypto library }
  if (xmlSecCryptoInit() < 0) then
    raise Exception.Create('Error: xmlsec-crypto initialization failed.');
end;

class procedure MShutDownXmlSec;
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
{$ENDIF}

class function MGetURL(const AUF, AAmbiente, FormaEmissao: Integer;
  ALayOut: TLayOutMDFe): WideString;
begin
  //  (AC,AL,AP,AM,BA,CE,DF,ES,GO,MA,MT,MS,MG,PA,PB,PR,PE,PI,RJ,RN,RS,RO,RR,SC,SP,SE,TO);
  //  (12,27,16,13,29,23,53,32,52,21,51,50,31,15,25,41,26,22,33,24,43,11,14,42,35,28,17);

 case FormaEmissao of
  1,2,4,5 : begin
             case AUF of
              12: Result := MGetURLSVRS(AAmbiente, ALayOut);             //AC - Acre
              27: Result := MGetURLSVRS(AAmbiente, ALayOut);             //AL - Alagoas
              16: Result := MGetURLSVRS(AAmbiente, ALayOut);             //AP - Amapá
              13: Result := MGetURLSVRS(AAmbiente, ALayOut);             //AM - Amazonas
              29: Result := MGetURLSVRS(AAmbiente, ALayOut);             //BA - Bahia
              23: Result := MGetURLSVRS(AAmbiente, ALayOut);             //CE - Ceará
              53: Result := MGetURLSVRS(AAmbiente, ALayOut);             //DF - Distrito Federal
              32: Result := MGetURLSVRS(AAmbiente, ALayOut);             //ES - Espirito Santo
              52: Result := MGetURLSVRS(AAmbiente, ALayOut);             //GO - Goiás
              21: Result := MGetURLSVRS(AAmbiente, ALayOut);             //MA - Maranhão

              51: Result := MGetURLMT(AAmbiente, ALayOut);               //MT - Mato Grosso
              50: Result := MGetURLMS(AAmbiente, ALayOut);               //MS - Mato Grosso do Sul
              31: Result := MGetURLMG(AAmbiente, ALayOut);               //MG - Minas Gerais

              15: Result := MGetURLSVRS(AAmbiente, ALayOut);             //PA - Pará
              25: Result := MGetURLSVRS(AAmbiente, ALayOut);             //PB - Paraibá
              41: Result := MGetURLSVRS(AAmbiente, ALayOut);             //PR - Paraná

              26: Result := MGetURLSP(AAmbiente, ALayOut);               //PE - Pernambuco

              22: Result := MGetURLSVRS(AAmbiente, ALayOut);             //PI - Piauí
              33: Result := MGetURLSVRS(AAmbiente, ALayOut);             //RJ - Rio de Janeiro
              24: Result := MGetURLSVRS(AAmbiente, ALayOut);             //RN - Rio Grande do Norte

              43: Result := MGetURLRS(AAmbiente, ALayOut);               //RS - Rio Grande do Sul

              11: Result := MGetURLSVRS(AAmbiente, ALayOut);             //RO - Rondônia
              14: Result := MGetURLSVRS(AAmbiente, ALayOut);             //RR - Roraima
              42: Result := MGetURLSVRS(AAmbiente, ALayOut);             //SC - Santa Catarina

              35: Result := MGetURLSP(AAmbiente, ALayOut);               //SP - São Paulo

              28: Result := MGetURLSVRS(AAmbiente, ALayOut);             //SE - Sergipe
              17: Result := MGetURLSVRS(AAmbiente, ALayOut);             //TO - Tocantins
             end;
            end;
 end;
 if Result = '' then
     raise Exception.Create('URL não disponível para o Estado solicitado.');
end;

class function MGetURLSVRS(AAmbiente: Integer;
  ALayOut: TLayOutMDFe): WideString;
begin
  case ALayOut of
    LayMDFeRecepcao:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx');
    LayMDFeRetRecepcao:   Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx'      , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx');
    LayMDFeEvento:        Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx', 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx');
    LayMDFeConsulta:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx');
    LayMDFeStatusServico: Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx'  , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx');
    LayMDFeConsNaoEnc:    Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx'        , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx');
  end;
end;

class function MGetURLMG(AAmbiente: Integer;
  ALayOut: TLayOutMDFe): WideString;
begin
  case ALayOut of
    LayMDFeRecepcao:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx');
    LayMDFeRetRecepcao:   Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx'      , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx');
    LayMDFeEvento:        Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx', 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx');
    LayMDFeConsulta:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx');
    LayMDFeStatusServico: Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx'  , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx');
    LayMDFeConsNaoEnc:    Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx'        , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx');
  end;
end;

class function MGetURLRS(AAmbiente: Integer;
  ALayOut: TLayOutMDFe): WideString;
begin
  case ALayOut of
    LayMDFeRecepcao:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx');
    LayMDFeRetRecepcao:   Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx'      , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx');
    LayMDFeEvento:        Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx', 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx');
    LayMDFeConsulta:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx');
    LayMDFeStatusServico: Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx'  , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx');
    LayMDFeConsNaoEnc:    Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx'        , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx');
  end;
end;

class function MGetURLSP(AAmbiente: Integer;
  ALayOut: TLayOutMDFe): WideString;
begin
  case ALayOut of
    LayMDFeRecepcao:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx');
    LayMDFeRetRecepcao:   Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx'      , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx');
    LayMDFeEvento:        Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx', 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx');
    LayMDFeConsulta:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx');
    LayMDFeStatusServico: Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx'  , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx');
    LayMDFeConsNaoEnc:    Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx'        , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx');
  end;
end;

class function MGetURLMS(AAmbiente: Integer;
  ALayOut: TLayOutMDFe): WideString;
begin
  case ALayOut of
    LayMDFeRecepcao:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx');
    LayMDFeRetRecepcao:   Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx'      , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx');
    LayMDFeEvento:        Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx', 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx');
    LayMDFeConsulta:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx');
    LayMDFeStatusServico: Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx'  , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx');
    LayMDFeConsNaoEnc:    Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx'        , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx');
  end;
end;

class function MGetURLMT(AAmbiente: Integer;
  ALayOut: TLayOutMDFe): WideString;
begin
  case ALayOut of
    LayMDFeRecepcao:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFerecepcao/MDFeRecepcao.asmx');
    LayMDFeRetRecepcao:   Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx'      , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRetRecepcao/MDFeRetRecepcao.asmx');
    LayMDFeEvento:        Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx', 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeRecepcaoEvento/MDFeRecepcaoEvento.asmx');
    LayMDFeConsulta:      Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx'            , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsulta/MDFeConsulta.asmx');
    LayMDFeStatusServico: Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx'  , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeStatusServico/MDFeStatusServico.asmx');
    LayMDFeConsNaoEnc:    Result := SeSenao(AAmbiente = 1, 'https://mdfe.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx'        , 'https://mdfe-hml.sefaz.rs.gov.br/ws/MDFeConsNaoEnc/MDFeConsNaoEnc.asmx');
  end;
end;

class function MFormatarNumMDFe(const AValue: Integer): string;
begin
  result := FormatFloat('000000000', AValue);
end;

class function MFormatarValor(mask: TpcteMask; const AValue: real): string;
begin
  result := FormatFloat(TpMaskToStrText(mask), AValue);
end;

class function MFormatarChaveAcesso(AValue: String; Mascara: Boolean = False): String;
begin
  AValue := LimpaNumero(AValue);
  if Mascara
   then Result := copy(AValue,1,2)  + '-' + copy(AValue,3,2)  + '/' +
                  copy(AValue,5,2)  + '-' + copy(AValue,7,2)  + '.' +
                  copy(AValue,9,3)  + '.' + copy(AValue,12,3) + '/' +
                  copy(AValue,15,4) + '-' + copy(AValue,19,2) + '-' +
                  copy(AValue,21,2) + '-' + copy(AValue,23,3) + '-' +
                  copy(AValue,26,3) + '.' + copy(AValue,29,3) + '.' +
                  copy(AValue,32,3) + '-' + copy(AValue,35,1) + '-' +
                  copy(AValue,36,2) + '.' + copy(AValue,38,3) + '.' +
                  copy(AValue,41,3) + '-' + copy(AValue,44,1)
   else Result := copy(AValue,1,4)  + ' ' + copy(AValue,5,4)  + ' ' +
                  copy(AValue,9,4)  + ' ' + copy(AValue,13,4) + ' ' +
                  copy(AValue,17,4) + ' ' + copy(AValue,21,4) + ' ' +
                  copy(AValue,25,4) + ' ' + copy(AValue,29,4) + ' ' +
                  copy(AValue,33,4) + ' ' + copy(AValue,37,4) + ' ' +
                  copy(AValue,41,4);
end;

{$IFDEF ACBrMDFeOpenSSL}
function ValidaLibXML(const AXML: AnsiString;
  var AMsg: AnsiString; const APathSchemas: string = ''): Boolean;
var
  doc, schema_doc: xmlDocPtr;
  parser_ctxt: xmlSchemaParserCtxtPtr;
  schema: xmlSchemaPtr;
  valid_ctxt: xmlSchemaValidCtxtPtr;
  schemError: xmlErrorPtr;
  schema_filename: PChar;
  Tipo, I: Integer;
begin
 Tipo := MIdentificaTipoSchema(AXML, I);

 if not DirectoryExists(SeSenao(EstaVazio(APathSchemas),
                 PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas',
                 PathWithDelim(APathSchemas))) then
    raise Exception.Create('Diretório de Schemas não encontrado'+sLineBreak+
                           SeSenao(EstaVazio(APathSchemas),
                           PathWithDelim(ExtractFileDir(application.ExeName))+
                           'Schemas',PathWithDelim(APathSchemas)));

 case Tipo of
  1: begin
      if EstaVazio(APathSchemas)
       then schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\MDFe_v' + MDFeEnviMDFe + '.xsd')
       else schema_filename := pchar(PathWithDelim(APathSchemas)+'MDFe_v' + MDFeEnviMDFe + '.xsd');
     end;
  2..4:
     begin
      if EstaVazio(APathSchemas) then
        schema_filename := pchar(PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\eventoMDFe_v' + MDFeEventoMDFe + '.xsd')
       else
        schema_filename := pchar(PathWithDelim(APathSchemas)+'eventoMDFe_v' + MDFeEventoMDFe + '.xsd');
     end;
 end;

  doc         := nil;
  schema_doc  := nil;
  parser_ctxt := nil;
  schema      := nil;
  valid_ctxt  := nil;
  doc         := xmlParseDoc(PAnsiChar(Axml));

  if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
  begin
    AMsg   := 'Erro: unable to parse';
    Result := False;
    exit;
  end;

  schema_doc := xmlReadFile(Pansichar(AnsiString(ACBrStr(schema_filename))), nil, XML_DETECT_IDS);

  //  the schema cannot be loaded or is not well-formed
  if (schema_doc = nil) then
  begin
    AMsg := 'Erro: Schema não pode ser carregado ou está corrompido';
    Result := False;
    exit;
  end;

  parser_ctxt := xmlSchemaNewDocParserCtxt(schema_doc);
  // unable to create a parser context for the schema */
  if (parser_ctxt = nil) then
  begin
    xmlFreeDoc(schema_doc);
    AMsg := 'Erro: unable to create a parser context for the schema';
    Result := False;
    exit;
  end;

  schema := xmlSchemaParse(parser_ctxt);
  // the schema itself is not valid
  if (schema = nil) then
  begin
    xmlSchemaFreeParserCtxt(parser_ctxt);
    xmlFreeDoc(schema_doc);
    AMsg := 'Error: the schema itself is not valid';
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
    AMsg := 'Error: unable to create a validation context for the schema';
    Result := False;
    exit;
  end;

  if (xmlSchemaValidateDoc(valid_ctxt, doc) <> 0) then
  begin
    schemError := xmlGetLastError();
    AMsg := IntToStr(schemError^.code) + ' - ' + schemError^.message;
    Result := False;
    exit;
  end;

  xmlSchemaFreeValidCtxt(valid_ctxt);
  xmlSchemaFree(schema);
  xmlSchemaFreeParserCtxt(parser_ctxt);
  xmlFreeDoc(schema_doc);
  Result := True;
end;

function ValidaModalLibXML(XML: AnsiString; var Msg: AnsiString;
 const APathSchemas: string = ''): Boolean;
var
  doc, schema_doc: xmlDocPtr;
  parser_ctxt: xmlSchemaParserCtxtPtr;
  schema: xmlSchemaPtr;
  valid_ctxt: xmlSchemaValidCtxtPtr;
  schemError: xmlErrorPtr;
  schema_filename: String; // PChar;
  Tipo: Integer;
  AXML: AnsiString;
begin
  Tipo := 0;

  if pos('<aereo>', XML) <> 0
   then begin
    Tipo := 1;
    AXML := SeparaDados(XML, 'aereo');
    AXML := '<aereo xmlns="http://www.portalfiscal.inf.br/mdfe">' +
            AXML +
           '</aereo>';
   end;
  if pos('<aquav>', XML) <> 0
   then begin
    Tipo := 2;
    AXML := SeparaDados(XML, 'aquav');
    AXML := '<aquav xmlns="http://www.portalfiscal.inf.br/mdfe">' +
            AXML +
           '</aquav>';
   end;
  if pos('<duto>', XML) <> 0
   then begin
    Tipo := 3;
    AXML := SeparaDados(XML, 'duto');
    AXML := '<duto xmlns="http://www.portalfiscal.inf.br/mdfe">' +
            AXML +
           '</duto>';
   end;
  if pos('<ferrov>', XML) <> 0
   then begin
    Tipo := 4;
    AXML := SeparaDados(XML, 'ferrov');
    AXML := '<ferrov xmlns="http://www.portalfiscal.inf.br/mdfe">' +
            AXML +
           '</ferrov>';
   end;
  if pos('<rodo>', XML) <> 0
   then begin
    Tipo := 5;
    AXML := SeparaDados(XML, 'rodo');
    AXML := '<rodo xmlns="http://www.portalfiscal.inf.br/mdfe">' +
            AXML +
           '</rodo>';
   end;

  // Eventos
  if pos('<evEncMDFe>', XML) <> 0
   then begin
    Tipo := 6;
    AXML := SeparaDados(XML, 'evEncMDFe');
    AXML := '<evEncMDFe xmlns="http://www.portalfiscal.inf.br/mdfe">' +
            AXML +
           '</evEncMDFe>';
   end;
  if pos('<evCancMDFe>', XML) <> 0
   then begin
    Tipo := 7;
    AXML := SeparaDados(XML, 'evCancMDFe');
    AXML := '<evCancMDFe xmlns="http://www.portalfiscal.inf.br/mdfe">' +
            AXML +
           '</evCancMDFe>';
   end;
  if pos('<evIncCondutorMDFe>', XML) <> 0
   then begin
    Tipo := 8;
    AXML := SeparaDados(XML, 'evIncCondutorMDFe');
    AXML := '<evIncCondutorMDFe xmlns="http://www.portalfiscal.inf.br/mdfe">' +
            AXML +
           '</evIncCondutorMDFe>';
   end;

  AXML := '<?xml version="1.0" encoding="UTF-8" ?>' + AXML;

  if Tipo = 0 then
    raise Exception.Create('Modal não encontrado no XML.');

  if not DirectoryExists(SeSenao(EstaVazio(APathSchemas),
                  PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas',
                  PathWithDelim(APathSchemas))) then
    raise Exception.Create('Diretório de Schemas não encontrado'+sLineBreak+
                            SeSenao(EstaVazio(APathSchemas),
                            PathWithDelim(ExtractFileDir(application.ExeName))+
                            'Schemas',PathWithDelim(APathSchemas)));

  case Tipo of
   1: begin
       schema_filename := (SeSenao(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'MDFeModalAereo_v' + MDFeModalAereo + '.xsd');
      end;
   2: begin
       schema_filename := (SeSenao(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'MDFeModalAquaviario_v' + MDFeModalAqua + '.xsd');
      end;
   3: begin
       schema_filename := (SeSenao(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'MDFeModalDutoviario_v' + MDFeModalDuto + '.xsd');
      end;
   4: begin
       schema_filename := (SeSenao(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'MDFeModalFerroviario_v' + MDFeModalFerro + '.xsd');
      end;
   5: begin
       schema_filename := (SeSenao(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'MDFeModalRodoviario_v' + MDFeModalRodo + '.xsd');
      end;
   6: begin
       schema_filename := (SeSenao(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'evEncMDFe_v' + MDFeModalRodo + '.xsd');
      end;
   7: begin
       schema_filename := (SeSenao(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'evCancMDFe_v' + MDFeModalRodo + '.xsd');
      end;
   8: begin
       schema_filename := (SeSenao(EstaVazio(APathSchemas),
          PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
          PathWithDelim(APathSchemas))+'evIncCondutorMDFe_v' + MDFeModalRodo + '.xsd');
      end;
  end;

  doc         := nil;
  schema_doc  := nil;
  parser_ctxt := nil;
  schema      := nil;
  valid_ctxt  := nil;
  doc         := xmlParseDoc(PAnsiChar(AXML));

  if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
  begin
    Msg := 'Erro: unable to parse';
    Result := False;
    exit;
  end;

  schema_doc := xmlReadFile(Pansichar(AnsiString(ACBrStr(schema_filename))), nil, XML_DETECT_IDS);

  //  the schema cannot be loaded or is not well-formed
  if (schema_doc = nil) then
  begin
    Msg := 'Erro: Schema não pode ser carregado ou está corrompido';
    Result := False;
    exit;
  end;

  parser_ctxt := xmlSchemaNewDocParserCtxt(schema_doc);
  // unable to create a parser context for the schema */
  if (parser_ctxt = nil) then
  begin
    xmlFreeDoc(schema_doc);
    Msg := 'Erro: unable to create a parser context for the schema';
    Result := False;
    exit;
  end;

  schema := xmlSchemaParse(parser_ctxt);
  // the schema itself is not valid
  if (schema = nil) then
  begin
    xmlSchemaFreeParserCtxt(parser_ctxt);
    xmlFreeDoc(schema_doc);
    Msg := 'Error: the schema itself is not valid';
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
    Msg := 'Error: unable to create a validation context for the schema';
    Result := False;
    exit;
  end;

  if (xmlSchemaValidateDoc(valid_ctxt, doc) <> 0) then
  begin
    schemError := xmlGetLastError();
    Msg := IntToStr(schemError^.code) + ' - ' + schemError^.message;
    Result := False;
    exit;
  end;

  xmlSchemaFreeValidCtxt(valid_ctxt);
  xmlSchemaFree(schema);
  xmlSchemaFreeParserCtxt(parser_ctxt);
  xmlFreeDoc(schema_doc);
  Result := True;
end;
{$ELSE}
function ValidaMSXML(XML: AnsiString; out Msg: AnsiString;
 const APathSchemas: string = ''): Boolean;
var
  DOMDocument: IXMLDOMDocument2;
  ParseError: IXMLDOMParseError;
  Schema: XMLSchemaCache;
  Tipo, I: Integer;
  schema_filename: String;
begin
  Result := False;
  CoInitialize(nil);
  try
    Tipo := MIdentificaTipoSchema(XML, I);

    DOMDocument                  := CoDOMDocument50.Create;
    DOMDocument.async            := False;
    DOMDocument.resolveExternals := False;
    DOMDocument.validateOnParse  := True;
    DOMDocument.loadXML(XML);

    Schema := CoXMLSchemaCache50.Create;

    if not DirectoryExists(SeSenao(EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas',
                    PathWithDelim(APathSchemas))) then
      raise Exception.Create('Diretório de Schemas não encontrado'+sLineBreak+
                              SeSenao(EstaVazio(APathSchemas),
                              PathWithDelim(ExtractFileDir(application.ExeName))+
                              'Schemas',PathWithDelim(APathSchemas)));

    case Tipo of
      1: schema_filename := SeSenao(EstaVazio(APathSchemas),
                                             PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                                             PathWithDelim(APathSchemas)) + 'MDFe_v' + MDFeEnviMDFe + '.xsd';
      2..4: schema_filename := SeSenao(EstaVazio(APathSchemas),
                                               PathWithDelim(ExtractFileDir(application.ExeName)) + 'Schemas\',
                                               PathWithDelim(APathSchemas)) + 'eventoMDFe_v' + MDFeEventoMDFe + '.xsd';
      else schema_filename := '';
    end;

    if not FilesExists(schema_filename) then
       raise Exception.Create('Arquivo ' + schema_filename + ' não encontrado');

    Schema.add('http://www.portalfiscal.inf.br/mdfe', schema_filename);

    DOMDocument.schemas := Schema;
    ParseError          := DOMDocument.validate;
    Result              := (ParseError.errorCode = 0);
    Msg                 := ParseError.reason;
    DOMDocument         := nil;
    ParseError          := nil;
    Schema              := nil;
  finally
    CoUninitialize;
  end;
end;

function ValidaModalMSXML(XML: AnsiString; out Msg: AnsiString;
 const APathSchemas: string = ''): Boolean;
var
  DOMDocument: IXMLDOMDocument2;
  ParseError: IXMLDOMParseError;
  Schema: XMLSchemaCache;
  Tipo: Integer;
  schema_filename: String;
begin
  try
    Tipo := 0;
    CoInitialize(nil);
    if pos('<aereo>', XML) <> 0
     then begin
      Tipo := 1;
      XML := SeparaDados(XML, 'aereo');
      XML := '<aereo xmlns="http://www.portalfiscal.inf.br/mdfe">' +
              XML +
             '</aereo>';
     end;
    if pos('<aquav>', XML) <> 0
     then begin
      Tipo := 2;
      XML := SeparaDados(XML, 'aquav');
      XML := '<aquav xmlns="http://www.portalfiscal.inf.br/mdfe">' +
              XML +
             '</aquav>';
     end;
    if pos('<duto>', XML) <> 0
     then begin
      Tipo := 3;
      XML := SeparaDados(XML, 'duto');
      XML := '<duto xmlns="http://www.portalfiscal.inf.br/mdfe">' +
              XML +
             '</duto>';
     end;
    if pos('<ferrov>', XML) <> 0
     then begin
      Tipo := 4;
      XML := SeparaDados(XML, 'ferrov');
      XML := '<ferrov xmlns="http://www.portalfiscal.inf.br/mdfe">' +
              XML +
             '</ferrov>';
     end;
    if pos('<rodo>', XML) <> 0
     then begin
      Tipo := 5;
      XML := SeparaDados(XML, 'rodo');
      XML := '<rodo xmlns="http://www.portalfiscal.inf.br/mdfe">' +
              XML +
             '</rodo>';
     end;

    // Eventos
    if pos('<evEncMDFe>', XML) <> 0
     then begin
      Tipo := 6;
      XML := SeparaDados(XML, 'evEncMDFe');
      XML := '<evEncMDFe xmlns="http://www.portalfiscal.inf.br/mdfe">' +
              XML +
             '</evEncMDFe>';
     end;
    if pos('<evCancMDFe>', XML) <> 0
     then begin
      Tipo := 7;
      XML := SeparaDados(XML, 'evCancMDFe');
      XML := '<evCancMDFe xmlns="http://www.portalfiscal.inf.br/mdfe">' +
              XML +
             '</evCancMDFe>';
     end;
    if pos('<evIncCondutorMDFe>', XML) <> 0
     then begin
      Tipo := 8;
      XML := SeparaDados(XML, 'evIncCondutorMDFe');
      XML := '<evIncCondutorMDFe xmlns="http://www.portalfiscal.inf.br/mdfe">' +
              XML +
             '</evIncCondutorMDFe>';
     end;

    XML := '<?xml version="1.0" encoding="UTF-8" ?>' + XML;

    if Tipo = 0 then
      raise Exception.Create('Modal não encontrado no XML.');

    DOMDocument                  := CoDOMDocument50.Create;
    DOMDocument.async            := False;
    DOMDocument.resolveExternals := False;
    DOMDocument.validateOnParse  := True;
    DOMDocument.loadXML(XML);

    Schema := CoXMLSchemaCache50.Create;

    if not DirectoryExists(DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
                    PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas',
                    PathWithDelim(APathSchemas))) then
      raise Exception.Create('Diretório de Schemas não encontrado'+sLineBreak+
                              DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
                              PathWithDelim(ExtractFileDir(application.ExeName))+
                              'Schemas',PathWithDelim(APathSchemas)));

    Schema.remove('http://www.portalfiscal.inf.br/mdfe');

    case Tipo of
     1: begin
         schema_filename := DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
            PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
            PathWithDelim(APathSchemas))+'MDFeModalAereo_v' + MDFeModalAereo + '.xsd';
        end;
     2: begin
         schema_filename := DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
            PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
            PathWithDelim(APathSchemas))+'MDFeModalAquaviario_v' + MDFeModalAqua + '.xsd';
        end;
     3: begin
         schema_filename := DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
            PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
            PathWithDelim(APathSchemas))+'MDFeModalDutoviario_v' + MDFeModalDuto + '.xsd';
        end;
     4: begin
         schema_filename := DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
            PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
            PathWithDelim(APathSchemas))+'MDFeModalFerroviario_v' + MDFeModalFerro + '.xsd';
        end;
     5: begin
         schema_filename := DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
            PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
            PathWithDelim(APathSchemas))+'MDFeModalRodoviario_v' + MDFeModalRodo + '.xsd';
        end;
     6: begin
         schema_filename := DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
            PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
            PathWithDelim(APathSchemas))+'evEncMDFe_v' + MDFeModalRodo + '.xsd';
        end;
     7: begin
         schema_filename := DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
            PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
            PathWithDelim(APathSchemas))+'evCancMDFe_v' + MDFeModalRodo + '.xsd';
        end;
     8: begin
         schema_filename := DFeUtil.SeSenao(DFeUtil.EstaVazio(APathSchemas),
            PathWithDelim(ExtractFileDir(application.ExeName))+'Schemas\',
            PathWithDelim(APathSchemas))+'evIncCondutorMDFe_v' + MDFeModalRodo + '.xsd';
        end;
    end;

   if not FilesExists(schema_filename) then
      raise Exception.Create('Arquivo ' + schema_filename + ' não encontrado');

    Schema.add('http://www.portalfiscal.inf.br/mdfe', schema_filename);

    DOMDocument.schemas := Schema;
    ParseError          := DOMDocument.validate;
    Result              := (ParseError.errorCode = 0);
    Msg                 := ParseError.reason;
    DOMDocument         := nil;
    ParseError          := nil;
    Schema              := nil;
  finally
    CoUninitialize;
  end;
end;

function ValidaAssinaturaMSXML(XML: AnsiString; out Msg: AnsiString): Boolean;
var
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  pKeyInfo: IXMLDOMNode;
  pKey, pKeyOut: IXMLDSigKey;
begin
  xmldoc  := CoDOMDocument50.Create;
  xmldsig := CoMXDigitalSignature50.Create;

  xmldoc.async              := False;
  xmldoc.validateOnParse    := False;
  xmldoc.preserveWhiteSpace := True;

   if (not xmldoc.loadXML(XML)) then
      raise Exception.Create('Não foi possível carregar o arquivo: '+XML);
  try
    xmldoc.setProperty('SelectionNamespaces', DSIGNS);
    xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');

   if (xmldsig.signature = nil) then
      raise Exception.Create('Não foi possível carregar o ler a assinatura: '+XML);

    pKeyInfo := xmldoc.selectSingleNode('.//ds:KeyInfo/ds:X509Data');

    pKey := xmldsig.createKeyFromNode(pKeyInfo);

    try
      pKeyOut := xmldsig.verify(pKey);
    except
       on E: Exception do
          Msg := 'Erro ao verificar assinatura do arquivo: '+ E.Message;
    end;
  finally
    Result := (pKeyOut <> nil);

    pKeyOut  := nil;
    pKey     := nil;
    pKeyInfo := nil;
    xmldsig  := nil;
    xmldoc   := nil;
  end;
end;
{$ENDIF}

class function MValida(const AXML: AnsiString;
  var AMsg: AnsiString; const APathSchemas: string = ''): Boolean;
begin
{$IFDEF ACBrMDFeOpenSSL}
  Result := ValidaLibXML(AXML, AMsg, APathSchemas) and
            ValidaModalLibXML(AXML, AMsg, APathSchemas);
{$ELSE}
  Result := ValidaMSXML(AXML, AMsg, APathSchemas) and
            ValidaModalMSXML(AXML, AMsg, APathSchemas)
{$ENDIF}
end;

class function MValidaAssinatura(const AXML: AnsiString;
  var AMsg: AnsiString): Boolean;
begin
{$IFDEF ACBrMDFeOpenSSL}
  Result := False;
{$ELSE}
  Result := ValidaAssinaturaMSXML(AXML,AMsg);
{$ENDIF}
end;

{$IFDEF ACBrMDFeOpenSSL}
function AssinarLibXML(const AXML, ArqPFX, PFXSenha: AnsiString;
  out AXMLAssinado, FMensagem: AnsiString): Boolean;
var
  I, J, PosIni, PosFim: Integer;
  URI, AStr, XmlAss: AnsiString;
  Tipo: Integer;
  Cert: TMemoryStream;
  Cert2: TStringStream;
begin
  AStr := AXML;

  //// Encontrando o URI ////
  Tipo := MIdentificaTipoSchema(AStr,I);

  if I = 0 then
    raise Exception.Create('Não encontrei inicio do URI: <infMDFe');
  I := PosEx('Id=', AStr, I + 6);
  if I = 0 then
    raise Exception.Create('Não encontrei inicio do URI: Id=');
  I := PosEx('"', AStr, I + 2);
  if I = 0 then
    raise Exception.Create('Não encontrei inicio do URI: aspas inicial');
  J := PosEx('"', AStr, I + 1);
  if J = 0 then
    raise Exception.Create('Não encontrei inicio do URI: aspas final');

  URI := copy(AStr, I + 1, J - I - 1);

  //// Adicionando Cabeçalho DTD, necessário para xmlsec encontrar o ID ////
  I := pos('?>', AStr);

  case Tipo of
    1: AStr := Copy(AStr,1,StrToInt(VarToStr(SeSenao(I>0,I+1,I)))) + cDTD +
               Copy(AStr,StrToInt(VarToStr(SeSenao(I>0,I+2,I))),Length(AStr));
    2..4: AStr := Copy(AStr,1,StrToInt(VarToStr(SeSenao(I>0,I+1,I)))) + cDTDEven +
                  Copy(AStr,StrToInt(VarToStr(SeSenao(I>0,I+2,I))),Length(AStr));
    else AStr := '';
  end;

  case Tipo of
    1:
    begin
      I := pos('</MDFe>',AStr);
      if I = 0 then
        raise Exception.Create('Não encontrei final do XML: </MDFe>');
    end;
    2..4:
    begin
      I := pos('</eventoMDFe>',AStr);
      if I = 0 then
        raise Exception.Create('Não encontrei final do XML: </eventoMDFe>');
    end;
    else
      raise Exception.Create('Template de Tipo não implementado.');
  end;

  if pos('<Signature', AStr) > 0 then
    I := pos('<Signature', AStr);
  AStr := copy(AStr, 1, I - 1) +
    '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">' +
      '<SignedInfo>' +
        '<CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
        '<SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />' +
        '<Reference URI="#' + URI + '">' +
          '<Transforms>' +
            '<Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" />' +
            '<Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" />' +
          '</Transforms>' +
          '<DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />' +
          '<DigestValue></DigestValue>' +
        '</Reference>' +
      '</SignedInfo>' +
      '<SignatureValue></SignatureValue>' +
      '<KeyInfo>' +
        '<X509Data>' +
          '<X509Certificate></X509Certificate>' +
        '</X509Data>' +
      '</KeyInfo>' +
    '</Signature>';

  case Tipo of
       1: AStr := AStr + '</MDFe>';
    2..4: AStr := AStr + '</eventoMDFe>';
    else  AStr := '';
  end;

  if FileExists(ArqPFX) then
    XmlAss := Msign_file(PAnsiChar(AStr), PAnsiChar(ArqPFX), PAnsiChar(PFXSenha))
  else
   begin
    Cert  := TMemoryStream.Create;
    Cert2 := TStringStream.Create(ArqPFX);
    try
      Cert.LoadFromStream(Cert2);
      XmlAss := Msign_memory(PAnsiChar(AStr), PAnsiChar(ArqPFX), PAnsiChar(PFXSenha), Cert.Size, Cert.Memory);
    finally
      Cert2.Free;
      Cert.Free;
    end;
  end;
  // Removendo quebras de linha //
  XmlAss := StringReplace(XmlAss, #10, '', [rfReplaceAll]);
  XmlAss := StringReplace(XmlAss, #13, '', [rfReplaceAll]);

  // Removendo DTD //
  case Tipo of
       1: XmlAss := StringReplace(XmlAss, cDTD, '', []);
    2..4: XmlAss := StringReplace(XmlAss, cDTDEven, '', []);
    else  XmlAss := '';
  end;

  PosIni := Pos('<X509Certificate>', XmlAss) - 1;
  PosFim := PosLast('<X509Certificate>', XmlAss);

  XmlAss := copy(XmlAss, 1, PosIni) + copy(XmlAss, PosFim, length(XmlAss));

  AXMLAssinado := XmlAss;

  Result := True;
end;
{$ELSE}
function AssinarMSXML(XML: AnsiString; Certificado: ICertificate2; out XMLAssinado: AnsiString): Boolean;
var
  I, J, PosIni, PosFim: Integer;
  URI: string;
  Tipo: Integer;
  xmlHeaderAntes, xmlHeaderDepois: AnsiString;
  xmldoc: IXMLDOMDocument3;
  xmldsig: IXMLDigitalSignature;
  dsigKey: IXMLDSigKey;
  signedKey: IXMLDSigKey;
begin
  Result := False;
  CoInitialize(nil);
  try
   if Pos('<Signature', XML) <= 0 then
   begin
     Tipo := MIdentificaTipoSchema(XML, I);

     I := PosEx('Id=', XML, 6);
     if I = 0 then
       raise Exception.Create('Não encontrei inicio do URI: Id=');
     I := PosEx('"', XML, I + 2);
     if I = 0 then
       raise Exception.Create('Não encontrei inicio do URI: aspas inicial');
     J := PosEx('"', XML, I + 1);
     if J = 0 then
       raise Exception.Create('Não encontrei inicio do URI: aspas final');

     URI := copy(XML, I + 1, J - I - 1);

     case Tipo of
          1: XML := copy(XML,1,pos('</MDFe>',XML)-1);
       2..4: XML := copy(XML,1,pos('</eventoMDFe>',XML)-1);
       else  XML := '';
     end;

     XML := XML + '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#"><SignedInfo><CanonicalizationMethod Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" /><SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#rsa-sha1" />';
     XML := XML + '<Reference URI="#' + URI + '">';
     XML := XML + '<Transforms><Transform Algorithm="http://www.w3.org/2000/09/xmldsig#enveloped-signature" /><Transform Algorithm="http://www.w3.org/TR/2001/REC-xml-c14n-20010315" /></Transforms><DigestMethod Algorithm="http://www.w3.org/2000/09/xmldsig#sha1" />';
     XML := XML + '<DigestValue></DigestValue></Reference></SignedInfo><SignatureValue></SignatureValue><KeyInfo></KeyInfo></Signature>';

     case Tipo of
          1: XML := XML + '</MDFe>';
       2..4: XML := XML + '</eventoMDFe>';
       else  XML := '';
     end;
   end;

   // Lendo Header antes de assinar //
   xmlHeaderAntes := '';
   I := pos('?>', XML);
   if I > 0 then
     xmlHeaderAntes := copy(XML, 1, I + 1);

   xmldoc := CoDOMDocument50.Create;

   xmldoc.async := False;
   xmldoc.validateOnParse := False;
   xmldoc.preserveWhiteSpace := True;

   xmldsig := CoMXDigitalSignature50.Create;

   if (not xmldoc.loadXML(XML)) then
     raise Exception.Create('Não foi possível carregar o arquivo: ' + XML);

   xmldoc.setProperty('SelectionNamespaces', DSIGNS);

   xmldsig.signature := xmldoc.selectSingleNode('.//ds:Signature');

   if (xmldsig.signature = nil) then
     raise Exception.Create('Falha ao setar assinatura.');

   if (xmldsig.signature = nil) then
     raise Exception.Create('É preciso carregar o template antes de assinar.');

   if NumCertCarregado <> Certificado.SerialNumber then
     CertStoreMem := nil;

   if CertStoreMem = nil then
   begin
     CertStore := CoStore.Create;
     CertStore.Open(CAPICOM_CURRENT_USER_STORE, 'My', CAPICOM_STORE_OPEN_READ_ONLY);

     CertStoreMem := CoStore.Create;
     CertStoreMem.Open(CAPICOM_MEMORY_STORE, 'Memoria', CAPICOM_STORE_OPEN_READ_ONLY);

     Certs := CertStore.Certificates as ICertificates2;
     for i := 1 to Certs.Count do
     begin
       Cert := IInterface(Certs.Item[i]) as ICertificate2;
       if Cert.SerialNumber = Certificado.SerialNumber then
        begin
          CertStoreMem.Add(Cert);
          NumCertCarregado := Certificado.SerialNumber;
        end;
     end;
   end;

   OleCheck(IDispatch(Certificado.PrivateKey).QueryInterface(IPrivateKey, PrivateKey));
   xmldsig.store := CertStoreMem;

   dsigKey := xmldsig.createKeyFromCSP(PrivateKey.ProviderType, PrivateKey.ProviderName, PrivateKey.ContainerName, 0);
   if (dsigKey = nil) then
     raise Exception.Create('Erro ao criar a chave do CSP.');

   signedKey := xmldsig.sign(dsigKey, $00000002);
   if (signedKey <> nil) then
   begin
     XMLAssinado := xmldoc.xml;
     XMLAssinado := StringReplace(XMLAssinado, #10, '', [rfReplaceAll]);
     XMLAssinado := StringReplace(XMLAssinado, #13, '', [rfReplaceAll]);
     PosIni := Pos('<SignatureValue>', XMLAssinado) + length('<SignatureValue>');
     XMLAssinado := copy(XMLAssinado, 1, PosIni - 1) + StringReplace(copy(XMLAssinado, PosIni, length(XMLAssinado)), ' ', '', [rfReplaceAll]);
     PosIni := Pos('<X509Certificate>', XMLAssinado) - 1;
     PosFim := PosLast('<X509Certificate>', XMLAssinado);

     XMLAssinado := copy(XMLAssinado, 1, PosIni) + copy(XMLAssinado, PosFim, length(XMLAssinado));
   end
   else
     raise Exception.Create('Assinatura Falhou.');

   if xmlHeaderAntes <> '' then
   begin
     I := pos('?>', XMLAssinado);
     if I > 0 then
     begin
       xmlHeaderDepois := copy(XMLAssinado, 1, I + 1);
       if xmlHeaderAntes <> xmlHeaderDepois then
         XMLAssinado := StuffString(XMLAssinado, 1, length(xmlHeaderDepois), xmlHeaderAntes);
     end
     else
       XMLAssinado := xmlHeaderAntes + XMLAssinado;
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

{$IFDEF ACBrMDFeOpenSSL}
 class function Msign_file(const Axml: PAnsiChar; const key_file: PAnsiChar; const senha: PAnsiChar): AnsiString;
var
  doc: xmlDocPtr;
  node: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  buffer: PAnsiChar;
  bufSize: Integer;
label
  done;
begin
  doc     := nil;
  node    := nil;
  dsigCtx := nil;

  result := '';

  if (Axml = nil) or (key_file = nil) then Exit;

  try
    { load template }
    doc := xmlParseDoc(Axml);
    if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
      raise Exception.Create('Error: unable to parse');

    { find start node }
    node := xmlSecFindNode(xmlDocGetRootElement(doc), PAnsiChar(xmlSecNodeSignature), PAnsiChar(xmlSecDSigNs));
    if (node = nil) then
      raise Exception.Create('Error: start node not found');

    { create signature context, we don't need keys manager in this example }
    dsigCtx := xmlSecDSigCtxCreate(nil);
    if (dsigCtx = nil) then
      raise Exception.Create('Error :failed to create signature context');

    // { load private key}
    dsigCtx^.signKey := xmlSecCryptoAppKeyLoad(key_file, xmlSecKeyDataFormatPkcs12, senha, nil, nil);
    if (dsigCtx^.signKey = nil) then
      raise Exception.Create('Error: failed to load private pem key from "' + key_file + '"');

    { set key name to the file name, this is just an example! }
    if (xmlSecKeySetName(dsigCtx^.signKey, PAnsiChar(key_file)) < 0) then
      raise Exception.Create('Error: failed to set key name for key from "' + key_file + '"');

    { sign the template }
    if (xmlSecDSigCtxSign(dsigCtx, node) < 0) then
      raise Exception.Create('Error: signature failed');

    { print signed document to stdout }
    // xmlDocDump(stdout, doc);
    // Can't use "stdout" from Delphi, so we'll use xmlDocDumpMemory instead...
    buffer := nil;
    xmlDocDumpMemory(doc, @buffer, @bufSize);
    if (buffer <> nil) then
      { success }
      result := buffer;
  finally
    { cleanup }
    if (dsigCtx <> nil) then
      xmlSecDSigCtxDestroy(dsigCtx);

    if (doc <> nil) then
      xmlFreeDoc(doc);
  end;
end;

class function Msign_memory(const Axml: PAnsiChar; const key_file: PAnsichar; const senha: PAnsiChar; Size: Cardinal; Ponteiro: Pointer): AnsiString;
var
  doc: xmlDocPtr;
  node: xmlNodePtr;
  dsigCtx: xmlSecDSigCtxPtr;
  buffer: PAnsiChar;
  bufSize: Integer;
label
 done;
begin
    doc     := nil;
    //node := nil;
    dsigCtx := nil;
    
    result := '';

    if (Axml = nil) or (key_file = nil) then Exit;
    try
       { load template }
       doc := xmlParseDoc(Axml);
       if ((doc = nil) or (xmlDocGetRootElement(doc) = nil)) then
         raise Exception.Create('Error: unable to parse');

       { find start node }
       node := xmlSecFindNode(xmlDocGetRootElement(doc), PAnsiChar(xmlSecNodeSignature), PAnsiChar(xmlSecDSigNs));
       if (node = nil) then
         raise Exception.Create('Error: start node not found');

       { create signature context, we don't need keys manager in this example }
       dsigCtx := xmlSecDSigCtxCreate(nil);
       if (dsigCtx = nil) then
         raise Exception.Create('Error :failed to create signature context');

       // { load private key, assuming that there is not password }
       dsigCtx^.signKey := xmlSecCryptoAppKeyLoadMemory(Ponteiro, size, xmlSecKeyDataFormatPkcs12, senha, nil, nil);

       if (dsigCtx^.signKey = nil) then
          raise Exception.Create('Error: failed to load private pem key from "' + key_file + '"');

       { set key name to the file name, this is just an example! }
       if (xmlSecKeySetName(dsigCtx^.signKey, key_file) < 0) then
         raise Exception.Create('Error: failed to set key name for key from "' + key_file + '"');

       { sign the template }
       if (xmlSecDSigCtxSign(dsigCtx, node) < 0) then
         raise Exception.Create('Error: signature failed');

       { print signed document to stdout }
       // xmlDocDump(stdout, doc);
       // Can't use "stdout" from Delphi, so we'll use xmlDocDumpMemory instead...
       buffer := nil;
       xmlDocDumpMemory(doc, @buffer, @bufSize);
       if (buffer <> nil) then
          { success }
          result := buffer;
   finally
       { cleanup }
       if (dsigCtx <> nil) then
         xmlSecDSigCtxDestroy(dsigCtx);

       if (doc <> nil) then
         xmlFreeDoc(doc);
   end;
end;
{$ENDIF}

{$IFDEF ACBrMDFeOpenSSL}
class function MAssinar(const AXML, ArqPFX, PFXSenha: AnsiString; out AXMLAssinado, FMensagem: AnsiString): Boolean;
{$ELSE}
class function MAssinar(const AXML: AnsiString; Certificado: ICertificate2; out AXMLAssinado, FMensagem: AnsiString): Boolean;
{$ENDIF}
begin
{$IFDEF ACBrMDFeOpenSSL}
  Result := AssinarLibXML(AXML, ArqPFX, PFXSenha, AXMLAssinado, FMensagem);
{$ELSE}
  Result := AssinarMSXML(AXML, Certificado, AXMLAssinado);
{$ENDIF}
end;

class function MUFtoCUF(UF: String): Integer;
var
  Codigo, i: Integer;
begin
  Codigo := -1;
  for i:= 0 to High(NFeUF) do
  begin
    if NFeUF[I] = UF then
      Codigo := NFeUFCodigo[I];
  end;

  if Codigo < 0 then
     Result := -1
  else
     Result := Codigo;
end;

class function MGerarChaveContingencia(FMDFe:TMDFe): string;

   function GerarDigito_Contingencia(var Digito: Integer; chave: string): boolean;
   var
     i, j: Integer;
   const
     PESO = '43298765432987654329876543298765432';
   begin
     chave  := LimpaNumero(chave);
     j      := 0;
     Digito := 0;
     result := True;
     try
       for i := 1 to 35 do
         j := j + StrToInt(copy(chave, i, 1)) * StrToInt(copy(PESO, i, 1));
       Digito := 11 - (j mod 11);
       if (j mod 11) < 2 then
         Digito := 0;
     except
       result := False;
     end;
     if length(chave) <> 35 then
       result := False;
   end;

var
   wchave: string;
//   wicms_s, wicms_p: string;
//   wd,wm,wa: word;
//   Digito: Integer;
begin
   wchave := '';
   //UF
   (*
   if FMDFe.Dest.EnderDest.UF = 'EX'
    then wchave := '99' //exterior
    else wchave := copy(inttostr(FMDFe.Dest.EnderDest.cMun),1,2);

   //TIPO DE EMISSAO
   if FMDFe.Ide.tpEmis = teContingencia
    then wchave := wchave + '2'
    else if FMDFe.Ide.tpEmis = teFSDA
          then wchave := wchave + '5'
          else wchave := wchave + '0'; //esta valor caracteriza ERRO, valor tem q ser  2 ou 5

   //CNPJ OU CPF
   if (FMDFe.Dest.EnderDest.UF='EX')
    then wchave:=wchave+MPoem_Zeros('0',14)
    else wchave:=wchave+MPoem_Zeros(FMDFe.Dest.CNPJCPF,14);

   //VALOR DA CT-e
   wchave := wchave + MPoem_Zeros(MLimpaNumero(FloatToStrf(FMDFe.vPrest.vTPrest, ffFixed,18,2)),14);

   //DESTAQUE ICMS PROPRIO E ST
   wicms_p := '2';
   wicms_s := '2';

   // Checar esse trecho

   if (MNaoEstaZerado(FMDFe.Imp.ICMS.ICMS00.vICMS))
    then wicms_p := '1';
   if (MNaoEstaZerado(FMDFe.Imp.ICMS.ICMSOutraUF.vICMSOutraUF))
    then wicms_s := '1';

   wchave := wchave + wicms_p + wicms_s;

   //DIA DA EMISSAO
   decodedate(FMDFe.Ide.dhEmi, wa, wm, wd);
   wchave := wchave + MPoem_Zeros(inttostr(wd), 2);

   //DIGITO VERIFICADOR
   GerarDigito_Contingencia(Digito, wchave);
   wchave := wchave + inttostr(digito);
*)
   //RETORNA A CHAVE DE CONTINGENCIA
   result := wchave;
end;

class function MFormatarChaveContingencia(AValue: String): String;
begin
  AValue := LimpaNumero(AValue);
  Result := copy(AValue,  1, 4) + ' ' + copy(AValue,  5, 4) + ' ' +
            copy(AValue,  9, 4) + ' ' + copy(AValue, 13, 4) + ' ' +
            copy(AValue, 17, 4) + ' ' + copy(AValue, 21, 4) + ' ' +
            copy(AValue, 25, 4) + ' ' + copy(AValue, 29, 4) + ' ' +
            copy(AValue, 33, 4);
end;

class function MIdentificaTipoSchema(const AXML: AnsiString; var I: Integer): Integer;
var
 lTipoEvento: String;
begin
  I := pos('<infMDFe', AXML);
  Result := 1;
  if I = 0  then
   begin
     I := pos('<infEvento', AXML);
     if I > 0 then
      begin
       lTipoEvento := Trim(RetornarConteudoEntre(AXML, '<tpEvento>', '</tpEvento>'));
       if lTipoEvento = '110111'
        then Result := 2 // Cancelamento
        else begin
         if lTipoEvento = '110112'
          then Result := 3 // Encerramento
          else begin
           if lTipoEvento = '110114'
            then Result := 4 // Inclusao de Condutor
            else Result := 5;
          end;
        end;
      end;
   end;
end;

end.

