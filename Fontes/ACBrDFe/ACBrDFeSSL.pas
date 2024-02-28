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

unit ACBrDFeSSL;

interface

uses
  Classes, SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  blcksock, syncobjs,
  ACBrBase;

Const
  CBufferSize = 32768;  // 32k
  CDiasRestantesBuscarNovoCeriticado = 15;

  CDSIGNS = 'xmlns:ds="http://www.w3.org/2000/09/xmldsig#"';
  CSIGNATURE_NODE = './/ds:Signature';
  CDEFAULT_STORE_NAME = 'My'; //My CA Root AddressBook
  CACBR_STORE_NAME = 'ACBrStore';

type

  TSSLLib = (libNone, libOpenSSL, libCapicom, libCapicomDelphiSoap, libWinCrypt, libCustom);
  TSSLCryptLib = (cryNone, cryOpenSSL, cryCapicom, cryWinCrypt);
  TSSLHttpLib = (httpNone, httpWinINet, httpWinHttp, httpOpenSSL, httpIndy);
  TSSLXmlSignLib = (xsNone, xsXmlSec, xsMsXml, xsMsXmlCapicom, xsLibXml2);

  TSSLTipoCertificado = (tpcDesconhecido, tpcA1, tpcA3);
  TSSLDgst = (dgstMD2, dgstMD4, dgstMD5, dgstRMD160, dgstSHA, dgstSHA1, dgstSHA256, dgstSHA512) ;
  TSSLHashOutput = (outHexa, outBase64, outBinary) ;
  TSSLStoreLocation = (slMemory, slLocalMachine, slCurrentUser, slActiveDirectory, slSmartCard);

  TDFeSSL = class;

  { TDadosCertificado }

  TDadosCertificado = class
  private
    FThumbPrint: String;
    FCertificadora: String;
    FCNPJ: String;
    FDataVenc: TDateTime;
    FIssuerName: String;
    FNumeroSerie: String;
    FRazaoSocial: String;
    FSubjectName: String;
    FTipo: TSSLTipoCertificado;
    FDER64base: String;
    FDataInicioValidade: TDateTime;
    procedure SetIssuerName(const AValue: String);
    procedure SetSubjectName(const AValue: String);
  public
    constructor Create;
    procedure Clear;

    function GetCertificadoraFromIssuerName(const SubjectName: String): String;
    function GetCNPJFromSubjectName(const SubjectName: String): String;
    function GetRazaoSocialFromSubjectName(const SubjectName: String): String;

    property NumeroSerie: String read FNumeroSerie write FNumeroSerie;
    property IssuerName: String read FIssuerName write SetIssuerName;
    property Certificadora: String read FCertificadora write FCertificadora;
    property DataVenc: TDateTime read FDataVenc write FDataVenc;
    property DataInicioValidade : TDateTime read FDataInicioValidade write FDataInicioValidade;
    property SubjectName: String read FSubjectName write SetSubjectName;
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
    property CNPJ: String read FCNPJ write FCNPJ;
    property Tipo: TSSLTipoCertificado read FTipo write FTipo;
    property DERBase64: String read FDER64base write FDER64base;
    property ThumbPrint: String read FThumbPrint write FThumbPrint;
  end;


  { TListaCertificados }

  TListaCertificados = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TDadosCertificado>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TDadosCertificado);
    function GetObject (Index: Integer): TDadosCertificado;
    procedure Insert (Index: Integer; Obj: TDadosCertificado);
  public
    function New: TDadosCertificado;
    function Add (Obj: TDadosCertificado): Integer;
    property Objects [Index: Integer]: TDadosCertificado
      read GetObject write SetObject; default;
  end;

  { TDFeSSLCryptClass }

  TDFeSSLCryptClass = class
  private
    procedure CarregarCertificadoSeVazio;

    function GetCertDataVenc: TDateTime;
    function GetCertNumeroSerie: String;
    function GetCertIssuerName: String;
    function GetCertCertificadora: String;
    function GetCertSubjectName: String;
    function GetCertRazaoSocial: String;
    function GetCertCNPJ: String;
    function GetCertTipo: TSSLTipoCertificado;
  protected
    FpDadosCertificado: TDadosCertificado;
    FpListaCertificados: TListaCertificados;
    FpDFeSSL: TDFeSSL;
    FpCertificadoLido: Boolean;

    procedure CarregarCertificadoSeNecessario;

    function GetCertContextWinApi: Pointer; virtual;
    function GetCertPFXData: AnsiString; virtual;

    procedure CarregarCertificadoDeArquivoPFX; virtual;
    procedure CarregarCertificadoDeURLPFX; virtual;
    procedure CarregarCertificadoDeDadosPFX; virtual;
    procedure CarregarCertificadoDeNumeroSerie; virtual;
    procedure LerInfoCertificadoCarregado; virtual;

  public
    constructor Create(ADFeSSL: TDFeSSL); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    function Versao: String; virtual;
    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Assina: Boolean =  False): AnsiString; virtual;

    function ValidarHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean; virtual;

    procedure CarregarCertificado;
    procedure DescarregarCertificado; virtual;
    function SelecionarCertificado: String; virtual;
    procedure LerCertificadosStore; virtual;
    function CarregarCertificadoPublico(const DadosX509Base64: Ansistring): Boolean; virtual;

    property CertContextWinApi: Pointer read GetCertContextWinApi;
    property CertPFXData: AnsiString read GetCertPFXData;

    property CertificadoLido: Boolean read FpCertificadoLido;
    property CertNumeroSerie: String read GetCertNumeroSerie;
    property CertIssuerName: String read GetCertIssuerName;
    property CertCertificadora: String read GetCertCertificadora;
    property CertDataVenc: TDateTime read GetCertDataVenc;
    property CertSubjectName: String read GetCertSubjectName;
    property CertRazaoSocial: String read GetCertRazaoSocial;
    property CertCNPJ: String read GetCertCNPJ;
    property CertTipo: TSSLTipoCertificado read GetCertTipo;
    property DadosCertificado: TDadosCertificado read FpDadosCertificado;
    property ListaCertificados: TListaCertificados read FpListaCertificados;
  end;

  TDFeSSLHttpClassOf = class of TDFeSSLHttpClass;

  { TDFeSSLHttpClass }

  TDFeSSLHttpClass = class
  private
    FDataResp: TMemoryStream;
    FDataReq: TMemoryStream;
    FHeaderReq: THttpHeader;
    FHeaderResp: THttpHeader;
    FURL: String;
    FMethod: String;
    FMimeType: String;
    FSoapAction: String;
  protected
    FpDFeSSL: TDFeSSL;
    FpHTTPResultCode: Integer;
    FpInternalErrorCode: Integer;

    procedure ConfigConnection; virtual;
    function GetLastErrorDesc: String; virtual;
  public
    constructor Create(ADFeSSL: TDFeSSL); virtual;
    destructor Destroy; override;

    function Enviar(const ConteudoXML: String; const AURL: String;
      const ASoapAction: String; const AMimeType: String = '';
      const AAuthorizationHeader : String = ''; AValidateReturnCode: Boolean = True): String;
    procedure HTTPMethod(const AMethod, AURL: String);

    procedure Execute; virtual;
    procedure Abortar; virtual;
    procedure Clear;

    property DataReq: TMemoryStream read FDataReq;
    property HeaderReq: THttpHeader read FHeaderReq;
    property DataResp: TMemoryStream read FDataResp;
    property HeaderResp: THttpHeader read FHeaderResp;

    property URL: String read FURL write FURL;
    property Method: String read FMethod write FMethod;
    property SoapAction: String read FSoapAction write FSoapAction;
    property MimeType: String read FMimeType write FMimeType;

    property HTTPResultCode: Integer read FpHTTPResultCode;
    property InternalErrorCode: Integer read FpInternalErrorCode;
    property LastErrorDesc: String read GetLastErrorDesc;
  end;

  { TDFeSSLXmlSignClass }

  TDFeSSLXmlSignClass = class
  private

  protected
    FpDFeSSL: TDFeSSL;

    function AdicionarSignatureElement( const ConteudoXML: String; AddX509Data: Boolean;
      const docElement, IdSignature: String; const IdAttr: String = '';
      const IdSignatureValue: string = ''): String;
    function AjustarXMLAssinado(const ConteudoXML: String; const X509DER: String = ''): String;
    function GetSignDigestAlgorithm(const SignatureNode: String): TSSLDgst;
  public
    constructor Create(ADFeSSL: TDFeSSL); virtual;

    function Assinar(const ConteudoXML, docElement, infElement: String;
      const SignatureNode: String = ''; const SelectionNamespaces: String = '';
      const IdSignature: String = ''; const IdAttr: String = '';
      const IdSignatureValue: string = ''): String; virtual;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; virtual;
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; const SignatureNode: String = '';
      const SelectionNamespaces: String = ''; const IdSignature: String = '';
      const IdAttr: String = ''): Boolean; virtual;
  end;

  TDFeSSLAntesDeAssinar = procedure (var ConteudoXML: String;
     const docElement, infElement, SignatureNode, SelectionNamespaces,
     IdSignature: String) of object;

  { TDFeSendThread }

  TDFeSendThread = class(TThread)
  private
    FSSLHttp: TDFeSSLHttpClass;
    FHttpSendCriticalSection: TCriticalSection;
    FConteudoXML: String;
    FURL: String;
    FSoapAction: String;
    FMimeType: String;
    FResponse: String;
    FHttpDone: Boolean;
    FExceptMessage: String;

    function GetDone: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(ADFeSSL: TDFeSSL; SSLHttpClass: TDFeSSLHttpClassOf; const AConteudoXML, AURL,
       ASoapAction, AMimeType: String); reintroduce;
    destructor Destroy; override;

    procedure Abort;
    property Response: String read FResponse;
    property ExceptMessage: String read FExceptMessage;
    property Done: Boolean read GetDone;
  end;

  { TDFeSSL }

  TDFeSSL = class(TPersistent)
  private
    FAntesDeAssinar: TDFeSSLAntesDeAssinar;
    FURLPFX: String;
    FArquivoPFX: String;
    FDadosPFX: AnsiString;
    FNameSpaceURI: String;
    FNumeroSerie: String;
    FProxyHost: String;
    FProxyPass: String;
    FProxyPort: String;
    FProxyUser: String;
    FSenha: AnsiString;
    FK: String;
    FSSLCryptClass: TDFeSSLCryptClass;
    FSSLCryptLib: TSSLCryptLib;
    FSSLHttpClass: TDFeSSLHttpClass;
    FSSLHttpLib: TSSLHttpLib;
    FSSLXmlSignClass: TDFeSSLXmlSignClass;
    FSSLXmlSignLib: TSSLXmlSignLib;
    FSSLType: TSSLType;
    FStoreLocation: TSSLStoreLocation;
    FStoreName: String;
    FTimeOut: Integer;
    FTimeOutPorThread: Boolean;
    FUseCertificateHTTP: Boolean;
    FSSLDgst: TSSLDgst;

    function GetCertCNPJ: String;
    function GetCertContextWinApi: Pointer;
    function GetCertDataVenc: TDateTime;
    function GetCertIssuerName: String;
    function GetCertCertificadora: String;
    function GetCertificadoLido: Boolean;
    function GetCertNumeroSerie: String;
    function GetCertRazaoSocial: String;
    function GetCertSubjectName: String;
    function GetCertTipo: TSSLTipoCertificado;
    function GetDadosCertificado: TDadosCertificado;
    function GetListaCertificados: TListaCertificados;

    function GetHTTPResultCode: Integer;
    function GetInternalErrorCode: Integer;
    function GetSenha: AnsiString;

    procedure SetArquivoPFX(const AValue: String);
    procedure SetDadosPFX(const AValue: AnsiString);
    procedure SetURLPFX(AValue: String);
    procedure SetNumeroSerie(const AValue: String);
    procedure SetSenha(const AValue: AnsiString);

    procedure SetSSLCryptLib(ASSLCryptLib: TSSLCryptLib);
    procedure SetSSLHttpLib(ASSLHttpLib: TSSLHttpLib);
    procedure SetSSLXmlSignLib(ASSLXmlSignLib: TSSLXmlSignLib);

  protected
    FHttpSendCriticalSection: TCriticalSection;

  public
    constructor Create;
    procedure Clear;
    destructor Destroy; override;

    // Nota: ConteudoXML, DEVE estar em UTF8 //
    function Assinar(const ConteudoXML, docElement, infElement: String;
      const SignatureNode: String = ''; const SelectionNamespaces: String = '';
      const IdSignature: String = ''; const IdAttr: String = '';
      const IdSignatureValue: string = ''): String;
    // Envia por SoapAction o ConteudoXML (em UTF8) para URL. Retorna a resposta do Servico //
    function Enviar(var ConteudoXML: String; const AURL: String;
      const ASoapAction: String; AMimeType: String = ''; 
      const AAuthorizationHeader : String = ''; AValidateReturnCode: Boolean = True): String;
    // Valida um Arquivo contra o seu Schema. Retorna True se OK, preenche MsgErro se False //
    // ConteudoXML, DEVE estar em UTF8
    procedure HTTPMethod(const AMethod, AURL: String);
    function HTTPGet(const AURL: String): AnsiString;
    function HTTPPost(const ADataToSend: AnsiString; const AURL: String): AnsiString;

    function Validar(const ConteudoXML: String; const ArqSchema: String;
      out MsgErro: String): Boolean;
    // Verifica se assinatura de um XML é válida. Retorna True se OK, preenche MsgErro se False //
    // ConteudoXML, DEVE estar em UTF8
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; const SignatureNode: String = '';
      const SelectionNamespaces: String = ''; const IdSignature: String = '';
      const IdAttr: String = ''): Boolean;

    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const ModoSaida: TSSLHashOutput = outHexa;
       const Assina: Boolean =  False): AnsiString; overload;
    function CalcHashArquivo( const NomeArquivo : String;
       const Digest: TSSLDgst;
       const ModoSaida: TSSLHashOutput = outHexa;
       const Assina: Boolean =  False): AnsiString; overload;
    function CalcHash( const BinaryString : AnsiString;
       const Digest: TSSLDgst;
       const ModoSaida: TSSLHashOutput = outHexa;
       const Assina: Boolean =  False): AnsiString; overload;
    function CalcHash( const AStringList : TStringList;
       const Digest: TSSLDgst;
       const ModoSaida: TSSLHashOutput = outHexa;
       const Assina: Boolean =  False): AnsiString; overload;

   function ValidarHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean; overload;
   function ValidarHash( const SourceString : AnsiString;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean; overload;
   function ValidarHash( const AStringList : TStringList;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean; overload;

   function CalcHMAC( const BinaryString: AnsiString;
      const AKey: AnsiString;
      const Digest: TSSLDgst): AnsiString;

    procedure CarregarCertificado;
    procedure CarregarCertificadoSeNecessario;
    procedure DescarregarCertificado;
    procedure LerCertificadosStore;
    function SelecionarCertificado: String;
    function CarregarCertificadoPublico(const DadosX509Base64: Ansistring): Boolean; virtual;

    procedure ValidarCNPJCertificado(CNPJDocumento: String);

    property CertificadoLido: Boolean read GetCertificadoLido;
    property CertContextWinApi: Pointer read GetCertContextWinApi;

    property CertNumeroSerie: String read GetCertNumeroSerie;
    property CertDataVenc: TDateTime read GetCertDataVenc;
    property CertCertificadora: String read GetCertCertificadora;
    property CertIssuerName: String read GetCertIssuerName;
    property CertSubjectName: String read GetCertSubjectName;
    property CertRazaoSocial: String read GetCertRazaoSocial;
    property CertCNPJ: String read GetCertCNPJ;
    property CertTipo: TSSLTipoCertificado read GetCertTipo;

    property DadosCertificado: TDadosCertificado read GetDadosCertificado;
    property ListaCertificados: TListaCertificados read GetListaCertificados;

    property HTTPResultCode: Integer read GetHTTPResultCode;
    property InternalErrorCode: Integer read GetInternalErrorCode;

    property SSLCryptClass: TDFeSSLCryptClass read FSSLCryptClass;
    property SSLHttpClass: TDFeSSLHttpClass read FSSLHttpClass;
    property SSLXmlSignClass: TDFeSSLXmlSignClass read FSSLXmlSignClass;
  published
    property SSLCryptLib: TSSLCryptLib read FSSLCryptLib write SetSSLCryptLib
      default cryNone;
    property SSLHttpLib: TSSLHttpLib read FSSLHttpLib write SetSSLHttpLib
      default httpNone;
    property SSLXmlSignLib: TSSLXmlSignLib read FSSLXmlSignLib write SetSSLXmlSignLib
      default xsNone;
    property SSLType: TSSLType read FSSLType write FSSLType default LT_TLSv1_2;
    property SSLDgst: TSSLDgst read FSSLDgst write FSSLDgst default dgstSHA1;

    property StoreLocation: TSSLStoreLocation read FStoreLocation
      write FStoreLocation default slCurrentUser;
    property StoreName: String read FStoreName write FStoreName;

    property ArquivoPFX: String read FArquivoPFX write SetArquivoPFX;
    property DadosPFX: AnsiString read FDadosPFX write SetDadosPFX;
    property URLPFX: String read FURLPFX write SetURLPFX;
    property NumeroSerie: String read FNumeroSerie write SetNumeroSerie;
    property Senha: AnsiString read GetSenha write SetSenha;

    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: String read FProxyPort write FProxyPort;
    property ProxyUser: String read FProxyUser write FProxyUser;
    property ProxyPass: String read FProxyPass write FProxyPass;

    property TimeOut: Integer read FTimeOut write FTimeOut default 5000;
    property TimeOutPorThread: Boolean read FTimeOutPorThread write FTimeOutPorThread default False;
    property NameSpaceURI: String read FNameSpaceURI write FNameSpaceURI;

    property UseCertificateHTTP: Boolean read FUseCertificateHTTP write FUseCertificateHTTP default True;

    property AntesDeAssinar: TDFeSSLAntesDeAssinar read FAntesDeAssinar write FAntesDeAssinar;
  end;


implementation

uses
  strutils, dateutils,
  synacode, synautil,
  ACBrDFeUtil, ACBrValidador,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.XMLHTML,
  ACBrUtil.Math,
  ACBrDFeException, ACBrConsts
  {$IfNDef DFE_SEM_OPENSSL}
   ,ACBrDFeOpenSSL, ACBrDFeHttpOpenSSL
   {$IfNDef DFE_SEM_XMLSEC}
    ,ACBrDFeXsXmlSec
   {$EndIf}
   {$IfNDef DFE_SEM_LIBXML2}
    ,ACBrDFeXsLibXml2
   {$EndIf}
  {$EndIf}
  {$IfNDef DFE_SEM_CAPICOM}
   ,ACBrDFeCapicom
   {$IfNDef DFE_SEM_MSXML}
    ,ACBrDFeXsMsXmlCapicom
   {$EndIf}
  {$EndIf}
  {$IfNDef DFE_SEM_INDY}
   ,ACBrDFeHttpIndy
  {$EndIf}
  {$IfDef MSWINDOWS}
   ,ACBrDFeWinCrypt, ACBrDFeHttpWinApi
   {$IfNDef DFE_SEM_MSXML}
    ,ACBrDFeXsMsXml
   {$EndIf}
  {$EndIf};

{ TDFeSendThread }

constructor TDFeSendThread.Create(ADFeSSL: TDFeSSL;
  SSLHttpClass: TDFeSSLHttpClassOf; const AConteudoXML, AURL, ASoapAction,
  AMimeType: String);
begin
  FreeOnTerminate := False; // Sem liberação automática da Thread

  if (not Assigned(ADFeSSL)) or EstaVazio(AConteudoXML) or EstaVazio(AURL) then
    raise EACBrDFeException.Create('TDFeSendThread, parâmetros inválidos');

  FHttpSendCriticalSection := ADFeSSL.FHttpSendCriticalSection;
  FSSLHttp      := SSLHttpClass.Create(ADFeSSL);
  FConteudoXML  := AConteudoXML;
  FURL          := AURL;
  FSoapAction   := ASoapAction;
  FMimeType     := AMimeType;
  FHttpDone     := True;

  inherited Create(False);  // Executa agora

  {$IfNDef POSIX}
  Priority := tpNormal;
  {$EndIf}
end;

destructor TDFeSendThread.Destroy;
begin
  FHttpDone := True;
  FSSLHttp.Free;

  inherited Destroy;
end;

function TDFeSendThread.GetDone: Boolean;
begin
  Result := Self.Terminated or (FResponse <> '') or (FExceptMessage <> '');
end;

procedure TDFeSendThread.Execute;
begin
  FHttpDone      := False;
  FResponse      := '';
  FExceptMessage := '';

  try
    FHttpSendCriticalSection.Acquire;
    try
      FResponse := FSSLHttp.Enviar(FConteudoXML, FURL, FSoapAction, FMimeType);
      FHttpDone := True;
    finally
      FHttpSendCriticalSection.Release;
    end;
  except
    on E: Exception do
    begin
      FExceptMessage := E.Message;
    end;
  end;

  Terminate;
end;

procedure TDFeSendThread.Abort;
begin
  FreeOnTerminate := True;

  if (not FHttpDone) then
  begin
    FHttpDone := True;
    FSSLHttp.Abortar;
  end;
end;

{ TDadosCertificado }

constructor TDadosCertificado.Create;
begin
  inherited;
  Clear;
end;

procedure TDadosCertificado.Clear;
begin
  FCertificadora := '';
  FCNPJ          := '';
  FDataVenc      := 0;
  FIssuerName    := '';
  FNumeroSerie   := '';
  FRazaoSocial   := '';
  FSubjectName   := '';
  FTipo          := tpcDesconhecido;
  FDER64base     := '';
end;

procedure TDadosCertificado.SetSubjectName(const AValue: String);
begin
  if FSubjectName = AValue then Exit;
  FSubjectName := AValue;

  FRazaoSocial := GetRazaoSocialFromSubjectName(FSubjectName);
  FCNPJ := GetCNPJFromSubjectName(FSubjectName);
end;

procedure TDadosCertificado.SetIssuerName(const AValue: String);
begin
  if FIssuerName = AValue then Exit;
  FIssuerName := AValue;

  FCertificadora := GetCertificadoraFromIssuerName( FIssuerName );
end;

function TDadosCertificado.GetCNPJFromSubjectName( const SubjectName: String ): String;
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
      // Evita pegar CPF ou outro Documento, do SubjectName (comuns em EPP)
      if (ValidarCNPJ(Result) <> '') and (ValidarCPF(Result) <> '') then
        Result := '';
    end;
  end;
end;

function TDadosCertificado.GetRazaoSocialFromSubjectName( const SubjectName: String ): String;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := pos('CN=',SubjectName);
  if P1 > 0 then
  begin
    P2 := PosEx(':', SubjectName, P1);
    if P2 <= 0 then
      P2 := PosEx(',', SubjectName, P1);
    if P2 <= 0 then
      P2 := Length(SubjectName)+1;

    Result := copy(SubjectName, P1+3, P2-P1-3);
  end;
end;

function TDadosCertificado.GetCertificadoraFromIssuerName( const SubjectName: String ): String;
var
  P1, P2: Integer;
begin
  Result := '';
  P1 := pos('CN=',SubjectName);
  if P1 > 0 then
  begin
    P2 := PosEx('RFB', SubjectName, P1);
    if P2 <= 0 then
      P2 := PosEx(',', SubjectName, P1);
    if P2 <= 0 then
      P2 := Length(SubjectName)+1;

    Result := trim(copy(SubjectName, P1+3, P2-P1-3));
  end;
end;

{ TListaCertificados }

procedure TListaCertificados.SetObject(Index: Integer; Item: TDadosCertificado);
begin
  inherited Items[Index] := Item;
end;

function TListaCertificados.GetObject(Index: Integer): TDadosCertificado;
begin
  Result := TDadosCertificado(inherited Items[Index]);
end;

procedure TListaCertificados.Insert(Index: Integer; Obj: TDadosCertificado);
begin
  inherited Insert(Index, Obj);
end;

function TListaCertificados.New: TDadosCertificado;
begin
  Result := TDadosCertificado.Create;
  Add(Result);
end;

function TListaCertificados.Add(Obj: TDadosCertificado): Integer;
begin
  Result := inherited Add(Obj) ;
end;

{ TDFeSSLCryptClass }

constructor TDFeSSLCryptClass.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create;

  FpDadosCertificado := TDadosCertificado.Create;
  FpListaCertificados := TListaCertificados.Create;
  FpDFeSSL := ADFeSSL;
  FpCertificadoLido := False;
end;

destructor TDFeSSLCryptClass.Destroy;
begin
  FpDadosCertificado.Free;
  FpListaCertificados.Free;
  inherited Destroy;
end;

procedure TDFeSSLCryptClass.Clear;
begin
  FpDadosCertificado.Clear;
  FpListaCertificados.Clear;
end;

function TDFeSSLCryptClass.Versao: String;
begin
  Result := '';
end;

function TDFeSSLCryptClass.CalcHash(const AStream: TStream; const Digest: TSSLDgst;
  const Assina: Boolean): AnsiString;
begin
  {$IfDef FPC}Result := '';{$EndIf}
  raise EACBrDFeException.Create('"CalcHash" não suportado em: ' + ClassName);
end;

function TDFeSSLCryptClass.ValidarHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean;
begin
 {$IfDef FPC}Result := False;{$EndIf}
  raise EACBrDFeException.Create('"ValidarHash" não suportado em: ' + ClassName);
end;

function TDFeSSLCryptClass.SelecionarCertificado: String;
begin
  {$IfDef FPC}Result := '';{$EndIf}
  raise EACBrDFeException.Create('"SelecionarCertificado" não suportado em: ' +ClassName);
end;

procedure TDFeSSLCryptClass.LerCertificadosStore;
begin
  raise EACBrDFeException.Create('"LerCertificadosStore" não suportado em: ' +ClassName);
end;

function TDFeSSLCryptClass.CarregarCertificadoPublico(const DadosX509Base64: Ansistring): Boolean;
begin
  {$IfDef FPC}Result := False;{$EndIf}
  raise EACBrDFeException.Create('"CarregarCertificadoPublico" não suportado em: ' +ClassName);
end;

procedure TDFeSSLCryptClass.CarregarCertificado;
begin
  DescarregarCertificado;

  Clear;
  if not EstaVazio(FpDFeSSL.URLPFX) then
    CarregarCertificadoDeURLPFX

  else if (not EstaVazio(FpDFeSSL.DadosPFX)) then
      CarregarCertificadoDeDadosPFX

  else if not EstaVazio(FpDFeSSL.ArquivoPFX) then
    CarregarCertificadoDeArquivoPFX

  else if NaoEstaVazio(FpDFeSSL.NumeroSerie) then
    CarregarCertificadoDeNumeroSerie

  else
    raise EACBrDFeException.Create( 'DadosPFX, ArquivoPFX, URLPFX ou NumeroSerie não especificados !');

  LerInfoCertificadoCarregado;
  FpCertificadoLido := True;
end;

procedure TDFeSSLCryptClass.CarregarCertificadoDeArquivoPFX;
var
  PFXStream: TFileStream;
begin
  if not FileExists(FpDFeSSL.ArquivoPFX) then
    raise EACBrDFeException.Create('Arquivo: ' + FpDFeSSL.ArquivoPFX + ' não encontrado');

  PFXStream := TFileStream.Create(FpDFeSSL.ArquivoPFX, fmOpenRead or fmShareDenyNone);
  try
    PFXStream.Position := 0;
    FpDFeSSL.DadosPFX := ReadStrFromStream(PFXStream, PFXStream.Size);
  finally
    PFXStream.Free;
  end;

  CarregarCertificadoDeDadosPFX;
end;

procedure TDFeSSLCryptClass.CarregarCertificadoDeURLPFX;
var
  UsarCertificadoLocal, UsarCertificadoConexao: Boolean;
  DataArquivoPFX: TDateTime;
  OldArquivoPFX, OldURLPFX, OldCNPJ: String;
  OldDadosPFX, DownloadDadosPFX: AnsiString;
  DiasRestantes: Integer;
begin
  OldArquivoPFX := FpDFeSSL.ArquivoPFX;
  OldURLPFX     := FpDFeSSL.URLPFX;
  OldDadosPFX   := FpDFeSSL.DadosPFX;
  DownloadDadosPFX := '';
  OldCNPJ := '';
  UsarCertificadoLocal := (not EstaVazio(OldArquivoPFX)) and FileExists(OldArquivoPFX);

  if UsarCertificadoLocal then
  begin
    CarregarCertificadoDeArquivoPFX;
    LerInfoCertificadoCarregado;
    // Verifica se Certificado está vencendo. Se estiver, deve tentar baixar outro //
    DiasRestantes := Trunc(CertDataVenc) - Trunc(Today);
    OldCNPJ := CertCNPJ;
    UsarCertificadoLocal := (DiasRestantes > CDiasRestantesBuscarNovoCeriticado);

    // Verificando se já baixou Certitificado, hoje. Se SIM, não deve baixar novamente //
    if not UsarCertificadoLocal then
    begin
      DataArquivoPFX := FileDateToDateTime(FileAge(OldArquivoPFX));
      UsarCertificadoLocal := (DateOf(DataArquivoPFX) >= Today);
    end;
  end;

  if not UsarCertificadoLocal then
  begin
    UsarCertificadoConexao := FpDFeSSL.UseCertificateHTTP;
    try
      FpDFeSSL.UseCertificateHTTP := False;
      DownloadDadosPFX := FpDFeSSL.HTTPGet(OldURLPFX);  // Faz Download do PFX
      if not (FpDFeSSL.SSLHttpClass.HTTPResultCode in [200..202]) then  // Erro na resposta
        raise EACBrDFeException.CreateFmt('Http erro %d, baixando Certificado de %s',
                                         [FpDFeSSL.SSLHttpClass.HTTPResultCode,
                                          OldURLPFX]);
    finally
      FpDFeSSL.UseCertificateHTTP := UsarCertificadoConexao;
    end;

    if EstaVazio(DownloadDadosPFX) then
      raise EACBrDFeException.CreateFmt(ACBrStr('Certificado obtido de %s é inválido'),[OldURLPFX]);

    // Testando se Certificado baixado, é válido...
    try
      FpDFeSSL.DadosPFX := DownloadDadosPFX;
      DescarregarCertificado;
      CarregarCertificadoDeDadosPFX;
    except
      raise EACBrDFeException.CreateFmt(ACBrStr('Certificado obtido de %s é inválido'),[OldURLPFX]);
    end;

    if (OldCNPJ <> '') and (CertCNPJ <> OldCNPJ) then
      raise  EACBrDFeException.CreateFmt(ACBrStr('CNPJ do Certificado obtido de %s é diferente do CNPJ atual'),[OldURLPFX]);

    // Certificado lido com sucesso... Devemos salvar uma Copia Local ?
    if (not EstaVazio(DownloadDadosPFX)) and (not EstaVazio(OldArquivoPFX)) then
    begin
      SysUtils.DeleteFile(OldArquivoPFX);
      WriteToFile(OldArquivoPFX, DownloadDadosPFX, True);
    end;
  end;
end;

procedure TDFeSSLCryptClass.CarregarCertificadoDeDadosPFX;
begin
  raise EACBrDFeException.Create('"CarregarCertificadoDeDadosPFX" não implementado em: ' +ClassName);
end;

procedure TDFeSSLCryptClass.CarregarCertificadoDeNumeroSerie;
begin
  raise EACBrDFeException.Create('"CarregarCertificadoDeNumeroSerie" não suportado em: ' +ClassName);
end;

procedure TDFeSSLCryptClass.LerInfoCertificadoCarregado;
begin
  { Sobreescrever apenas se necessário }
end;

procedure TDFeSSLCryptClass.DescarregarCertificado;
begin
  Clear;
  FpCertificadoLido := False;
end;

procedure TDFeSSLCryptClass.CarregarCertificadoSeNecessario;
begin
  if not CertificadoLido then
    CarregarCertificado;
end;

procedure TDFeSSLCryptClass.CarregarCertificadoSeVazio;
begin
  if (FpDadosCertificado.NumeroSerie = '') then
    CarregarCertificadoSeNecessario;
end;

function TDFeSSLCryptClass.GetCertRazaoSocial: String;
begin
  CarregarCertificadoSeVazio;
  Result := FpDadosCertificado.RazaoSocial;
end;

function TDFeSSLCryptClass.GetCertContextWinApi: Pointer;
begin
  Result := Nil;
end;

function TDFeSSLCryptClass.GetCertPFXData: AnsiString;
begin
  Result := FpDFeSSL.DadosPFX;
end;

function TDFeSSLCryptClass.GetCertDataVenc: TDateTime;
begin
  CarregarCertificadoSeVazio;
  Result := FpDadosCertificado.DataVenc;
end;

function TDFeSSLCryptClass.GetCertNumeroSerie: String;
begin
  CarregarCertificadoSeVazio;
  Result := FpDadosCertificado.NumeroSerie;
end;

function TDFeSSLCryptClass.GetCertSubjectName: String;
begin
  CarregarCertificadoSeVazio;
  Result := FpDadosCertificado.SubjectName;
end;

function TDFeSSLCryptClass.GetCertTipo: TSSLTipoCertificado;
begin
  CarregarCertificadoSeVazio;
  Result := FpDadosCertificado.Tipo;
end;

function TDFeSSLCryptClass.GetCertIssuerName: String;
begin
  CarregarCertificadoSeVazio;
  Result := FpDadosCertificado.IssuerName;
end;

function TDFeSSLCryptClass.GetCertCertificadora: String;
begin
  CarregarCertificadoSeVazio;
  Result := FpDadosCertificado.Certificadora;
end;

function TDFeSSLCryptClass.GetCertCNPJ: String;
begin
  CarregarCertificadoSeVazio;
  Result := FpDadosCertificado.CNPJ;
end;

{ TDFeSSLHttpClass }

constructor TDFeSSLHttpClass.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create;
  FpDFeSSL := ADFeSSL;
  FDataReq := TMemoryStream.Create;
  FDataResp := TMemoryStream.Create;
  FHeaderReq := THttpHeader.Create;
  FHeaderResp := THttpHeader.Create;
  Clear;
end;

destructor TDFeSSLHttpClass.Destroy;
begin
  FDataReq.Free;
  FDataResp.Free;
  FHeaderReq.Free;
  FHeaderResp.Free;

  inherited Destroy;
end;

procedure TDFeSSLHttpClass.Clear;
begin
  FDataResp.Clear;
  FDataReq.Clear;
  FHeaderReq.Clear;
  FHeaderResp.Clear;
  FpHTTPResultCode := 0;
  FpInternalErrorCode := 0;
  FURL := '';
  FMethod := '';
  FSoapAction := '';
  FMimeType := '';
end;

function TDFeSSLHttpClass.GetLastErrorDesc: String;
begin
  Result := '';
end;

procedure TDFeSSLHttpClass.ConfigConnection;
begin
  FDataResp.Clear;
  FpHTTPResultCode := 0;
  FpInternalErrorCode := 0;
end;

function TDFeSSLHttpClass.Enviar(const ConteudoXML: String; const AURL: String;
  const ASoapAction: String; const AMimeType: String = '';
  const AAuthorizationHeader : String = ''; AValidateReturnCode: Boolean = True): String;
var
  AMethod: String;
begin
  FDataReq.Clear;
  if (ConteudoXML <> '') then
  begin
    AMethod := 'POST';
    WriteStrToStream(FDataReq, AnsiString(ConteudoXML));
  end
  else
    AMethod := 'GET';

  HeaderReq.Clear; // Para informar Haders na requisição, use HTTPMethod();
  if (AAuthorizationHeader <> '') then
    HeaderReq.AddHeader('Authorization', AAuthorizationHeader);

  FSoapAction := ASoapAction;
  FMimeType := AMimeType;
  Result := '';
  try
    HTTPMethod( AMethod, AURL ) ;

    FDataResp.Position := 0;
    Result := ReadStrFromStream(FDataResp, FDataResp.Size);

    // Verifica se o ResultCode é: 200 OK; 201 Created; 202 Accepted
    // https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html
    if (not (FpHTTPResultCode in [200..202])) and AValidateReturnCode then
      raise EACBrDFeException.Create('');
  except
    on E:Exception do
    begin
      raise EACBrDFeException.CreateDef( Format(ACBrStr(cACBrDFeSSLEnviarException),
                                         [FpInternalErrorCode, FpHTTPResultCode, FURL] )
                                         + sLineBreak + LastErrorDesc + sLineBreak + Result);
    end;
  end;

end;

procedure TDFeSSLHttpClass.HTTPMethod(const AMethod, AURL: String);
begin
  FMethod := AMethod;
  FURL := AURL;
  Execute;
end;

procedure TDFeSSLHttpClass.Execute;
begin
  ConfigConnection;
  { Implementar restante, com Override nas classes filhas }
end;

procedure TDFeSSLHttpClass.Abortar;
begin
  {}
end;


{ TDFeSSLXmlSignClass }

constructor TDFeSSLXmlSignClass.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create;
  FpDFeSSL := ADFeSSL;
end;

function TDFeSSLXmlSignClass.AdicionarSignatureElement(const ConteudoXML: String;
  AddX509Data: Boolean; const docElement, IdSignature: String;
  const IdAttr: String = ''; const IdSignatureValue: string = ''): String;
var
  URI, TagEndDocElement: String;
  I: Integer;
begin
  URI := EncontrarURI(ConteudoXML, docElement, IdAttr);

  TagEndDocElement := '</' + docElement + '>';
  I := PosLast(TagEndDocElement, ConteudoXML);
  if I = 0 then
    raise EACBrDFeException.Create('Não encontrei final do elemento: ' + TagEndDocElement);

  Result := copy(ConteudoXML, 1, I - 1) +
            SignatureElement(URI, AddX509Data, IdSignature, FpDFeSSL.SSLDgst,
                             IdSignatureValue) +
            copy(ConteudoXML, I, Length(ConteudoXML));
end;

function TDFeSSLXmlSignClass.AjustarXMLAssinado(const ConteudoXML: String;
  const X509DER: String): String;
var
  XmlAss: String;
  PosSig, PosIni, PosFim: Integer;

  function RemoveEspacos( const AXML, TagIni, TagFim : String): String;
  begin
    Result := '';
    PosIni := PosLast(TagIni, AXML);
    if PosIni > 0 then
    begin
      PosFim := PosEx(TagFim, AXML, PosIni + 1);
      if PosFim > 0 then
        Result := copy(AXML, 1, PosIni - 1) +
                  StringReplace(copy(AXML, PosIni, PosFim-PosIni), ' ', '', [rfReplaceAll])+
                  copy(AXML, PosFim, Length(AXML));
    end;

    if Result = '' then
      Result := AXML;
  end;

  procedure EncontrarInicioFinalTag(ATag: String; var PosIni, PosFim: Integer);
  begin
    PosFim := 0;
    PosIni := PosEx('<'+ATag+'>', XmlAss, PosSig);
    if (PosIni > 0) then
    begin
      PosIni := PosIni + Length(ATag)+1;
      PosFim := PosLast('</'+ATag+'>', XmlAss);
      if PosFim < PosIni then
        PosFim := 0;
    end;
  end;

begin
  XmlAss := ConteudoXML;

  // Removendo Declaração sem UTF8 Ex: <?xml version="1.0"?> //
  if not XmlEhUTF8(ObtemDeclaracaoXML(XmlAss)) then
    XmlAss := RemoverDeclaracaoXML(XmlAss);

  // Removendo quebras de linha //
  XmlAss := StringReplace(XmlAss, #10, '', [rfReplaceAll]);
  XmlAss := StringReplace(XmlAss, #13, '', [rfReplaceAll]);

  PosSig := PosLast('<SignatureValue>', XmlAss);
  if PosSig > 0 then
  begin
    PosFim := 0;
    if X509DER = '' then
    begin
      // Considerando apenas o último Certificado X509, da assinatura //
      PosIni := PosEx('<X509Certificate>', XmlAss, PosSig)-1;
      if PosIni >= 0 then
      begin
        PosFim := PosLast('<X509Certificate>', XmlAss);
        XmlAss := copy(XmlAss, 1, PosIni) + copy(XmlAss, PosFim, length(XmlAss));
      end;
    end
    else
    begin
      // Remove todos Certificados adicionados, e Adiciona o X509DER informado //
      EncontrarInicioFinalTag('X509Data', PosIni, PosFim);
      if (PosIni > 0) and (PosFim > 0) then
      begin
        XmlAss := copy(XmlAss, 1, PosIni) +
                  '<X509Certificate>' + X509DER + '</X509Certificate>' +
                  copy(XmlAss, PosFim, length(XmlAss));
      end
      else
      begin
        EncontrarInicioFinalTag('KeyInfo', PosIni, PosFim);
        if (PosIni > 0) and (PosFim > 0) then
        begin
          XmlAss := copy(XmlAss, 1, PosIni) +
                    '<X509Data><X509Certificate>' + X509DER + '</X509Certificate></X509Data>'+
                    copy(XmlAss, PosFim, length(XmlAss));
        end
      end;
    end;
  end;

  // CAPICOM insere espaços em alguns Elementos da Assinatura //
  XmlAss := RemoveEspacos(XmlAss, '<SignatureValue>', '</KeyInfo>');

  Result := XmlAss;
end;

function TDFeSSLXmlSignClass.GetSignDigestAlgorithm(const SignatureNode: String
  ): TSSLDgst;
var
  HashAlg: string;
begin
  HashAlg := LowerCase(RetornarConteudoEntre(SignatureNode,
          'SignatureMethod Algorithm="http://www.w3.org/2000/09/xmldsig#', '"'));

  if HashAlg = '' then
    HashAlg := LowerCase(RetornarConteudoEntre(SignatureNode,
          'SignatureMethod Algorithm="http://www.w3.org/2001/04/xmldsig-more#', '"'));

  if HashAlg = '' then
    raise EACBrDFeException.Create(ACBrStr('Não foi possivel recuperar o "Digest Algorithm" do XML'));

  if (HashAlg = 'rsa-sha1') then
    Result := dgstSHA1
  else if (HashAlg = 'rsa-sha256') then
    Result := dgstSHA256
  else if (HashAlg = 'rsa-sha512') then
    Result := dgstSHA512
  else
    raise EACBrDFeException.Create(ACBrStr('Digest Algorithm, "'+HashAlg +'" não suportado'));
end;

function TDFeSSLXmlSignClass.Assinar(const ConteudoXML, docElement,
  infElement: String; const SignatureNode: String; const SelectionNamespaces: String;
  const IdSignature: String; const IdAttr: String; const IdSignatureValue: string): String;
begin
  {$IFDEF FPC}
  Result := '';
  {$ENDIF}
  raise EACBrDFeException.Create(ClassName + '.Assinar, não implementado');
end;

function TDFeSSLXmlSignClass.Validar(const ConteudoXML, ArqSchema: String; out
  MsgErro: String): Boolean;
begin
  {$IFDEF FPC}
  Result := False;
  {$ENDIF}
  raise EACBrDFeException.Create('"Validar" não suportado em: ' + ClassName);
end;

function TDFeSSLXmlSignClass.VerificarAssinatura(const ConteudoXML: String; out
  MsgErro: String; const infElement: String; const SignatureNode: String;
  const SelectionNamespaces: String; const IdSignature: String; const IdAttr: String): Boolean;
begin
  {$IFDEF FPC}
  Result := False;
  {$ENDIF}
  raise EACBrDFeException.Create('"ValidarAssinatura" não suportado em: ' + ClassName);
end;

{ TDFeSSL }

constructor TDFeSSL.Create;
begin
  inherited Create;

  FAntesDeAssinar := Nil;
  FHttpSendCriticalSection := TCriticalSection.Create;

  Clear;
end;

procedure TDFeSSL.Clear;
begin
  FArquivoPFX  := '';
  FDadosPFX    := '';
  FURLPFX      := '';
  FNumeroSerie := '';
  FProxyHost   := '';
  FProxyPass   := '';
  FProxyPort   := '';
  FProxyUser   := '';
  FSenha       := '';
  FK           := '';
  FSSLCryptLib := cryNone;
  FSSLHttpLib  := httpNone;
  FTimeOut     := 5000;
  FTimeOutPorThread := False;
  FNameSpaceURI:= '';

  FSSLType       := LT_TLSv1_2;
  FSSLDgst       := dgstSHA1;
  FStoreLocation := slCurrentUser;
  FStoreName     := CDEFAULT_STORE_NAME;

  // Para emissão de NFS-e essas propriedades podem ter valores diferentes dos
  // atribuidos abaixo, dependendo do provedor...
  FUseCertificateHTTP := True;

  if Assigned(FSSLCryptClass) then
    FSSLCryptClass.Free;

  FSSLCryptClass := TDFeSSLCryptClass.Create(Self);

  if Assigned(FSSLHttpClass) then
    FSSLHttpClass.Free;

  FSSLHttpClass := TDFeSSLHttpClass.Create(Self);

  if Assigned(FSSLXmlSignClass) then
    FSSLXmlSignClass.Free;

  FSSLXmlSignClass := TDFeSSLXmlSignClass.Create(Self);
end;

destructor TDFeSSL.Destroy;
begin
  FHttpSendCriticalSection.Free;

  if Assigned(FSSLCryptClass) then
  begin
    DescarregarCertificado;
    FreeAndNil(FSSLCryptClass);
  end;

  if Assigned(FSSLHttpClass) then
    FreeAndNil(FSSLHttpClass);

  if Assigned(FSSLXmlSignClass) then
    FreeAndNil(FSSLXmlSignClass);

  inherited Destroy;
end;

function TDFeSSL.Assinar(const ConteudoXML, docElement, infElement: String;
  const SignatureNode: String; const SelectionNamespaces: String; const IdSignature: String;
  const IdAttr: String; const IdSignatureValue: string): String;
Var
  XmlAss, DeclaracaoXMLAntes, DeclaracaoXMLDepois: String;
  Assinado: Boolean;
begin
  // Nota: ConteudoXML, DEVE estar em UTF8 //
  // Lendo Header antes de assinar, Se Header não for UTF8 não usa... //
  if XmlEhUTF8(ConteudoXML) then
    DeclaracaoXMLAntes := ObtemDeclaracaoXML(ConteudoXML)
  else
    DeclaracaoXMLAntes := '';

  Assinado := False;

  if Assigned(FAntesDeAssinar) then
  begin
    XmlAss := ConteudoXML;
    FAntesDeAssinar( XmlAss, docElement, infElement, SignatureNode,
                     SelectionNamespaces, IdSignature);
    Assinado := (XmlAss <> ConteudoXML);
  end;

  if not Assinado then
  begin
    XmlAss := FSSLXmlSignClass.Assinar( ConteudoXML, docElement, infElement,
                                        SignatureNode, SelectionNamespaces,
                                        IdSignature, IdAttr, IdSignatureValue);

    // Verificando se modificou o Header do XML assinado, e voltando para o anterior //
    if (DeclaracaoXMLAntes <> '') then
    begin
      DeclaracaoXMLDepois := ObtemDeclaracaoXML(XmlAss);

      if (DeclaracaoXMLDepois = '') then
        XmlAss := DeclaracaoXMLAntes + XmlAss
      else if (DeclaracaoXMLAntes <> DeclaracaoXMLDepois) then
        XmlAss := StringReplace(XmlAss, DeclaracaoXMLAntes, DeclaracaoXMLDepois, []);
    end;
  end;

  Result := XmlAss;
end;

function TDFeSSL.Enviar(var ConteudoXML: String; const AURL: String;
  const ASoapAction: String; AMimeType: String;
  const AAuthorizationHeader: String; AValidateReturnCode: Boolean): String;
var
  SendThread : TDFeSendThread;
  EndTime : TDateTime ;
  ErrorMsg: String;
begin
  // Nota: ConteudoXML, DEVE estar em UTF8 //
  Result := '';

  if UseCertificateHTTP then
    CarregarCertificadoSeNecessario;

  if AMimeType = '' then
    AMimeType := 'application/soap+xml; charset=utf-8';

  if TimeOutPorThread then
  begin
    EndTime := IncSecond(now,TruncFix(TimeOut/1000));
    SendThread := TDFeSendThread.Create( Self,
                                         TDFeSSLHttpClassOf(FSSLHttpClass.ClassType),
                                         ConteudoXML, AURL, ASoapAction, AMimeType);
    try
      while (not SendThread.Done) and (Now <= EndTime) do
        Sleep(50);
    finally
      Result := SendThread.Response;
      ErrorMsg := SendThread.ExceptMessage;

      if not SendThread.Done then
        SendThread.Abort  // Isso forçará a Thread terminar, e morrer por si...
      else
        SendThread.Free;
    end;

    if not EstaVazio(ErrorMsg) then
      raise EACBrDFeException.Create('Erro na thread de envio: ' + ErrorMsg);

    if EstaVazio(Result) then
      raise EACBrDFeException.Create('Timeout - Não foi possível obter a resposta do servidor');
  end
  else
  begin
    FHttpSendCriticalSection.Acquire;
    try
      Result := FSSLHttpClass.Enviar(ConteudoXML, AURL, ASoapAction,
             AMimeType, AAuthorizationHeader, AValidateReturnCode);
    finally
      FHttpSendCriticalSection.Release;
    end;
  end;
end;

procedure TDFeSSL.HTTPMethod(const AMethod, AURL: String);
begin
  FSSLHttpClass.HTTPMethod(AMethod, AURL);
end;

function TDFeSSL.HTTPGet(const AURL: String): AnsiString;
begin
  FSSLHttpClass.Clear;
  FSSLHttpClass.HTTPMethod('GET', AURL);
  FSSLHttpClass.DataResp.Position := 0;
  Result := ReadStrFromStream(FSSLHttpClass.DataResp, FSSLHttpClass.DataResp.Size);
end;

function TDFeSSL.HTTPPost(const ADataToSend: AnsiString; const AURL: String
  ): AnsiString;
begin
  FSSLHttpClass.Clear;
  WriteStrToStream(FSSLHttpClass.DataReq, ADataToSend);
  FSSLHttpClass.HTTPMethod('POST', AURL);
  FSSLHttpClass.DataResp.Position := 0;
  Result := ReadStrFromStream(FSSLHttpClass.DataResp, FSSLHttpClass.DataResp.Size);
end;

function TDFeSSL.Validar(const ConteudoXML: String; const ArqSchema: String;
  out MsgErro: String): Boolean;
begin
  if EstaVazio(ArqSchema) then
    raise EACBrDFeException.Create('Arquivo de Schema não especificado');

  // ArqSchema deve vir com o Path Completo
  if not FileExists(ArqSchema) then
    raise EACBrDFeException.Create('Arquivo ' + sLineBreak + ArqSchema +
      sLineBreak + 'Não encontrado');

  Result := FSSLXmlSignClass.Validar(ConteudoXML, ArqSchema, MsgErro);
end;

function TDFeSSL.VerificarAssinatura(const ConteudoXML: String; out
  MsgErro: String; const infElement: String; const SignatureNode: String;
  const SelectionNamespaces: String; const IdSignature: String; const IdAttr: String): Boolean;
begin
  Result := FSSLXmlSignClass.VerificarAssinatura(ConteudoXML, MsgErro,
                              infElement, SignatureNode, SelectionNamespaces,
                              IdSignature, IdAttr);
end;

function TDFeSSL.CalcHash(const AStream: TStream; const Digest: TSSLDgst;
  const ModoSaida: TSSLHashOutput; const Assina: Boolean): AnsiString;
var
  ABinStr: AnsiString;
begin
  if not Assigned(AStream) then
    raise EACBrDFeException.Create('Stream inválido');

  if AStream.Size <= 0 then
    raise EACBrDFeException.Create('Stream vazio');

  ABinStr := FSSLCryptClass.CalcHash(AStream, Digest, Assina);

  case ModoSaida of
    outBase64 : Result := Trim(EncodeBase64( ABinStr ));
    outHexa   : Result := AsciiToHex( ABinStr ); // TESTAR
  else
    Result := ABinStr;
  end;
end;

function TDFeSSL.CalcHashArquivo(const NomeArquivo: String;
  const Digest: TSSLDgst; const ModoSaida: TSSLHashOutput; const Assina: Boolean
  ): AnsiString;
Var
   FS : TFileStream ;
begin
  FS := TFileStream.Create(NomeArquivo, fmOpenRead or fmShareDenyWrite);
  try
    Result := CalcHash( FS, Digest, ModoSaida, Assina );
  finally
    FS.Free ;
  end ;
end;

function TDFeSSL.CalcHash(const BinaryString: AnsiString;
  const Digest: TSSLDgst; const ModoSaida: TSSLHashOutput; const Assina: Boolean
  ): AnsiString;
Var
   MS : TMemoryStream ;
begin
  MS := TMemoryStream.Create;
  try
    MS.Write( Pointer(BinaryString)^, Length(BinaryString) );
    Result := CalcHash( MS, Digest, ModoSaida, Assina );
  finally
    MS.Free ;
  end ;
end;

function TDFeSSL.CalcHash(const AStringList: TStringList;
  const Digest: TSSLDgst; const ModoSaida: TSSLHashOutput; const Assina: Boolean
  ): AnsiString;
Var
  MS : TMemoryStream ;
begin
  MS := TMemoryStream.Create;
  try
    AStringList.SaveToStream( MS );
    Result := CalcHash( MS, Digest, ModoSaida, Assina );
  finally
    MS.Free ;
  end ;
end;

function TDFeSSL.ValidarHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean;
begin
 if not Assigned(AStream) then
    raise EACBrDFeException.Create('Stream inválido');

  if AStream.Size <= 0 then
    raise EACBrDFeException.Create('Stream vazio');

  if Hash = '' then
    raise EACBrDFeException.Create('Hash vazio');

  Result := FSSLCryptClass.ValidarHash(AStream, Digest, Hash, Assinado);
end;

function TDFeSSL.ValidarHash( const SourceString : AnsiString;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean;
Var
   MS : TMemoryStream ;
begin
  MS := TMemoryStream.Create;
  try
    MS.Write( Pointer(SourceString)^, Length(SourceString) );
    Result := ValidarHash( MS, Digest, Hash, Assinado );
  finally
    MS.Free ;
  end ;
end;

function TDFeSSL.ValidarHash( const AStringList: TStringList;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean;
Var
   MS : TMemoryStream ;
begin
  MS := TMemoryStream.Create;
  try
    AStringList.SaveToStream(MS);
    Result := ValidarHash( MS, Digest, Hash, Assinado );
  finally
    MS.Free ;
  end ;
end;

// Baseado no método synacode.HMAC_SHA1 //
function TDFeSSL.CalcHMAC(const BinaryString: AnsiString;
  const AKey: AnsiString; const Digest: TSSLDgst): AnsiString;
var
  ipad, opad, s, k: AnsiString;
  n: Integer;
begin
  if Length(AKey) > 64 then
    k := CalcHash(AKey, Digest, outBinary, False)
  else
    k := AKey;

  ipad := StringOfChar(#$36, 64);
  opad := StringOfChar(#$5C, 64);
  for n := 1 to Length(k) do
  begin
    ipad[n] := AnsiChar(Byte(ipad[n]) xor Byte(k[n]));
    opad[n] := AnsiChar(Byte(opad[n]) xor Byte(k[n]));
  end;

  s      := CalcHash(ipad + BinaryString, Digest, outBinary, False);
  Result := CalcHash(opad + s, Digest, outBinary, False);
end;

procedure TDFeSSL.CarregarCertificado;
begin
  FSSLCryptClass.CarregarCertificado;
end;

procedure TDFeSSL.CarregarCertificadoSeNecessario;
begin
  FSSLCryptClass.CarregarCertificadoSeNecessario;
end;

procedure TDFeSSL.DescarregarCertificado;
begin
  if Assigned(FSSLCryptClass) then
    FSSLCryptClass.DescarregarCertificado;
end;

procedure TDFeSSL.LerCertificadosStore;
begin
  FSSLCryptClass.LerCertificadosStore;
end;

function TDFeSSL.SelecionarCertificado: String;
begin
  Result := FSSLCryptClass.SelecionarCertificado;

  if NaoEstaVazio(Result) then
    FSSLCryptClass.CarregarCertificado;
end;

function TDFeSSL.CarregarCertificadoPublico(const DadosX509Base64: Ansistring
  ): Boolean;
begin
  DescarregarCertificado;
  if (DadosX509Base64 = '') then
    Result := False
  else
    Result := FSSLCryptClass.CarregarCertificadoPublico(DadosX509Base64);
end;

{ Verifica se o "CNPJDocumento", é da mesma raiz do CNPJ do Certificado }
procedure TDFeSSL.ValidarCNPJCertificado(CNPJDocumento: String);
var
  ErroCNPJ, CNPJCertificado: String;
begin
  CNPJDocumento := OnlyNumber(CNPJDocumento);
  if (CNPJDocumento = '') or              // Informou vazio
     (Length(CNPJDocumento) <> 14) then   // Não é CNPJ
    exit;

  CNPJCertificado := OnlyNumber(CertCNPJ);  // Lendo CNPJ do Certificado...
  if (CNPJCertificado = '') or              // Não foi capaz de ler CNPJ do Certificado (Senha, NumSerie, Path... há algo errado na configuração)
     (Length(CNPJCertificado) <> 14) then   // Não é CNPJ (estranho.. pode ser um eCPF)
    exit;

  ErroCNPJ := ValidarCNPJ(CNPJDocumento);
  if (ErroCNPJ <> '') then
    raise EACBrDFeException.CreateDef('Erro CNPJ Documento: '+ErroCNPJ);

  { Deve verificar somente os 8 primeiros digitos, para evitar problemas quando
    a filial estiver utilizando o certificado da matriz }
  if (Copy(CNPJDocumento, 1, 8) <> Copy(CNPJCertificado, 1, 8)) then
    raise EACBrDFeException.Create('O CNPJ do Documento é diferente do CNPJ do Certificado Digital' );
end;

function TDFeSSL.GetCertDataVenc: TDateTime;
begin
  Result := FSSLCryptClass.CertDataVenc;
end;

function TDFeSSL.GetCertificadoLido: Boolean;
begin
  if Assigned(FSSLCryptClass) then
    Result := FSSLCryptClass.CertificadoLido
  else
    Result := False;
end;

function TDFeSSL.GetCertIssuerName: String;
begin
  Result := FSSLCryptClass.CertIssuerName;
end;

function TDFeSSL.GetCertCertificadora: String;
begin
  Result := FSSLCryptClass.CertCertificadora;
end;

function TDFeSSL.GetCertCNPJ: String;
begin
  Result := FSSLCryptClass.CertCNPJ;
end;

function TDFeSSL.GetCertContextWinApi: Pointer;
begin
  CarregarCertificadoSeNecessario;
  Result := FSSLCryptClass.CertContextWinApi;
end;

function TDFeSSL.GetCertNumeroSerie: String;
begin
  Result := FSSLCryptClass.CertNumeroSerie;
end;

function TDFeSSL.GetCertRazaoSocial: String;
begin
  Result := FSSLCryptClass.CertRazaoSocial;
end;

function TDFeSSL.GetCertSubjectName: String;
begin
  Result := FSSLCryptClass.CertSubjectName;
end;

function TDFeSSL.GetCertTipo: TSSLTipoCertificado;
begin
  Result := FSSLCryptClass.GetCertTipo;
end;

function TDFeSSL.GetDadosCertificado: TDadosCertificado;
begin
  Result := FSSLCryptClass.DadosCertificado;
end;

function TDFeSSL.GetListaCertificados: TListaCertificados;
begin
  Result := FSSLCryptClass.ListaCertificados;
end;

function TDFeSSL.GetHTTPResultCode: Integer;
begin
  Result := FSSLHttpClass.HTTPResultCode;
end;

function TDFeSSL.GetInternalErrorCode: Integer;
begin
  Result := FSSLHttpClass.InternalErrorCode;
end;

function TDFeSSL.GetSenha: AnsiString;
begin
  Result := StrCrypt(FSenha, FK)  // Descritografa a Senha
end;

procedure TDFeSSL.SetArquivoPFX(const AValue: String);
begin
  if FArquivoPFX = AValue then Exit;
  FArquivoPFX := AValue;

  if FArquivoPFX = '' then Exit;
 
  FNumeroSerie := ''; // Evitar erro ao trocar o tipo de certificado;
  FDadosPFX := '';    // Força a releitura de DadosPFX;
  if CertificadoLido then
    DescarregarCertificado;
end;

procedure TDFeSSL.SetDadosPFX(const AValue: AnsiString);
begin
  if FDadosPFX = AValue then Exit;
  FDadosPFX := AValue;

  if FDadosPFX = '' then Exit;

  FArquivoPFX := '';  // Evitar erro ao trocar o tipo de certificado;
  FURLPFX := '';
  FNumeroSerie := '';
  if CertificadoLido then
    DescarregarCertificado;
end;

procedure TDFeSSL.SetURLPFX(AValue: String);
begin
  if FURLPFX = AValue then Exit;
  FURLPFX := AValue;
  if FURLPFX = '' then Exit;
  
  // Não Zera ArquivoPFX, pois se ele estiver preenchido (recomendado), será usado como Cache Local
  FNumeroSerie := ''; // Evitar erro ao trocar o tipo de certificado;
  FDadosPFX := '';    // Força a releitura de DadosPFX;
  if CertificadoLido then
    DescarregarCertificado;
end;

procedure TDFeSSL.SetNumeroSerie(const AValue: String);
begin
  if FNumeroSerie = AValue then Exit;
  FNumeroSerie := Trim(UpperCase(StringReplace(AValue, ' ', '', [rfReplaceAll])));
  if FNumeroSerie = '' then Exit;
  
  FArquivoPFX := '';   // Evitar erro ao trocar o tipo de certificado;
  FDadosPFX := '';
  FURLPFX := '';
  if CertificadoLido then
    DescarregarCertificado;
end;

procedure TDFeSSL.SetSenha(const AValue: AnsiString);
begin
  if (FK <> '') and (FSenha = StrCrypt(AValue, FK)) then
    Exit;

  FK := FormatDateTime('hhnnsszzz',Now);
  FSenha := StrCrypt(AValue, FK);  // Salva Senha de forma Criptografada, para evitar "Inspect"

  if CertificadoLido then
    DescarregarCertificado;
end;

procedure TDFeSSL.SetSSLCryptLib(ASSLCryptLib: TSSLCryptLib);
  procedure FreeSSLCryptLib;
  begin
    if Assigned(FSSLCryptClass) then
      FreeAndNil(FSSLCryptClass);
  end;
begin
  if ASSLCryptLib = FSSLCryptLib then
    exit;

  case ASSLCryptLib of
    cryOpenSSL:
    begin
      {$IfNDef DFE_SEM_OPENSSL}
       FreeSSLCryptLib;
       FSSLCryptClass := TDFeOpenSSL.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a libOpenSSL foi desativado por compilação {$DEFINE DFE_SEM_OPENSSL}');
      {$EndIf}
    end;

    cryCapicom:
    begin
      {$IfNDef DFE_SEM_CAPICOM}
       FreeSSLCryptLib;
       FSSLCryptClass := TDFeCapicom.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a libCapicom foi desativado por compilação {$DEFINE DFE_SEM_CAPICOM}');
      {$EndIf}
    end;

    cryWinCrypt:
    begin
      {$IfDef MSWINDOWS}
       FreeSSLCryptLib;
       FSSLCryptClass := TDFeWinCrypt.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a libWinCrypt disponível apenas em MSWINDOWS');
      {$EndIf}
    end;

  else
    FreeSSLCryptLib;
    FSSLCryptClass := TDFeSSLCryptClass.Create(Self);
  end;

  FSSLCryptLib := ASSLCryptLib;
end;

procedure TDFeSSL.SetSSLHttpLib(ASSLHttpLib: TSSLHttpLib);
begin
  if ASSLHttpLib = FSSLHttpLib then
    exit;

  if Assigned(FSSLHttpClass) then
    FreeAndNil(FSSLHttpClass);

  FSSLHttpLib := ASSLHttpLib;

  case ASSLHttpLib of
    httpWinINet, httpWinHttp:
    begin
      {$IfDef MSWINDOWS}
       FSSLHttpClass := TDFeHttpWinHttp.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a "httpWinINet" disponível apenas em MSWINDOWS');
      {$EndIf}
    end;

    httpIndy:
    begin
      {$IfNDef DFE_SEM_INDY}
       FSSLHttpClass := TDFeHttpIndy.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a "httpIndy" disponível quando: Delphi, MSWINDOWS, e sem a diretiva {$DEFINE DFE_SEM_INDY}');
      {$EndIf}
    end;

    httpOpenSSL:
    begin
      {$IfNDef DFE_SEM_OPENSSL}
       FSSLHttpClass := TDFeHttpOpenSSL.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a "httpOpenSSL" foi desativado por compilação {$DEFINE DFE_SEM_OPENSSL}');
      {$EndIf}
    end;

  else
    FSSLHttpClass := TDFeSSLHttpClass.Create(Self);
  end;
end;

procedure TDFeSSL.SetSSLXmlSignLib(ASSLXmlSignLib: TSSLXmlSignLib);
begin
  if ASSLXmlSignLib = FSSLXmlSignLib then
    exit;

  if Assigned(FSSLXmlSignClass) then
    FreeAndNil(FSSLXmlSignClass);

  case ASSLXmlSignLib of
    xsMsXml:
    begin
      {$IfNDef DFE_SEM_MSXML}
       FSSLXmlSignClass := TDFeSSLXmlSignMsXml.Create(Self);
      {$Else}
       {$IfDef MSWINDOWS}
        raise EACBrDFeException.Create('Suporte a "xsMsXml" foi desativado por compilação {$DEFINE DFE_SEM_MSXML}');
       {$Else}
        raise EACBrDFeException.Create('Suporte a "xsMsXml" disponível apenas em MSWINDOWS');
       {$EndIf}
      {$EndIf}
    end;

    xsXmlSec:
    begin
      {$IfNDef DFE_SEM_XMLSEC}
       FSSLXmlSignClass := TDFeSSLXmlSignXmlSec.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a "xsXmlSec" foi desativado por compilação {$DEFINE DFE_SEM_OPENSSL} ou {$DEFINE DFE_SEM_XMLSEC}');
      {$EndIf}
    end;

    xsMsXmlCapicom:
    begin
      {$IfNDef DFE_SEM_MSXML}
       {$IfNDef DFE_SEM_CAPICOM}
        FSSLXmlSignClass := TDFeSSLXmlSignMsXmlCapicom.Create(Self);
       {$Else}
        raise EACBrDFeException.Create('Suporte a "xsMsXmlCapicom" disponível apenas em MSWINDOWS, e sem a diretiva de compilação {$DEFINE DFE_SEM_CAPICOM}');
       {$EndIf}
      {$Else}
       raise EACBrDFeException.Create('Suporte a "xsMsXmlCapicom" disponível apenas em MSWINDOWS, e sem a diretiva de compilação {$DEFINE DFE_SEM_MSXML}');
      {$EndIf}
    end;

    xsLibXml2:
    begin
      {$IfNDef DFE_SEM_LIBXML2}
       FSSLXmlSignClass := TDFeSSLXmlSignLibXml2.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a "xsLibXml2" foi desativado por compilação {$DEFINE DFE_SEM_OPENSSL} ou {$DEFINE DFE_SEM_LIBXML2}');
      {$EndIf}
    end;

  else
    FSSLXmlSignClass := TDFeSSLXmlSignClass.Create(Self);
  end;

  FSSLXmlSignLib := ASSLXmlSignLib;
end;

end.

