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

unit ACBrDFeSSL;

interface

uses
  Classes, SysUtils, Contnrs,
  blcksock;

Const
  CBufferSize = 32768;

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
    procedure SetIssuerName(AValue: String);
    procedure SetSubjectName(AValue: String);
  public
    constructor Create;
    procedure Clear;

    function GetCertificadoraFromIssuerName(SubjectName: String): String;
    function GetCNPJFromSubjectName(SubjectName: String): String;
    function GetRazaoSocialFromSubjectName(SubjectName: String): String;

    property NumeroSerie: String read FNumeroSerie write FNumeroSerie;
    property IssuerName: String read FIssuerName write SetIssuerName;
    property Certificadora: String read FCertificadora write FCertificadora;
    property DataVenc: TDateTime read FDataVenc write FDataVenc;
    property SubjectName: String read FSubjectName write SetSubjectName;
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
    property CNPJ: String read FCNPJ write FCNPJ;
    property Tipo: TSSLTipoCertificado read FTipo write FTipo;
    property DERBase64: String read FDER64base write FDER64base;
    property ThumbPrint: String read FThumbPrint write FThumbPrint;
  end;


  { TListaCertificados }

  TListaCertificados = class(TObjectList)
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
  public
    constructor Create(ADFeSSL: TDFeSSL); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    function CalcHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Assina: Boolean =  False): AnsiString; virtual;

    function ValidarHash( const AStream : TStream;
       const Digest: TSSLDgst;
       const Hash: AnsiString;
       const Assinado: Boolean =  False): Boolean; virtual;

    procedure CarregarCertificado; virtual;
    procedure DescarregarCertificado; virtual;
    function SelecionarCertificado: String; virtual;
    procedure LerCertificadosStore; virtual;
    function CarregarCertificadoPublico(DadosX509Base64: Ansistring): Boolean; virtual;

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

  { TDFeSSLHttpClass }

  TDFeSSLHttpClass = class
  private
  protected
    FpDFeSSL: TDFeSSL;

    function GetHTTPResultCode: Integer; virtual;
    function GetInternalErrorCode: Integer; virtual;
    procedure ConfigurarHTTP(const AURL, ASoapAction: String; AMimeType: String);
      virtual;
  public
    constructor Create(ADFeSSL: TDFeSSL); virtual;
    destructor Destroy; override;

    function Enviar(const ConteudoXML: String; const AURL: String;
      const ASoapAction: String; AMimeType: String = ''): String; virtual;

    property HTTPResultCode: Integer read GetHTTPResultCode;
    property InternalErrorCode: Integer read GetInternalErrorCode;
  end;



  { TDFeSSLXmlSignClass }

  TDFeSSLXmlSignClass = class
  private

  protected
    FpDFeSSL: TDFeSSL;

    function AdicionarSignatureElement( ConteudoXML: String; AddX509Data: Boolean;
      docElement, IdSignature: String; IdAttr: String = ''): String;
    function AjustarXMLAssinado(const ConteudoXML: String; X509DER: String = ''): String;
    function GetSignDigestAlgorithm(const SignatureNode: String): TSSLDgst;
  public
    constructor Create(ADFeSSL: TDFeSSL); virtual;

    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''; IdAttr: String = ''): String; virtual;
    function Validar(const ConteudoXML, ArqSchema: String;
      out MsgErro: String): Boolean; virtual;
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''; IdSignature: String = '';
      IdAttr: String = ''): Boolean; virtual;
  end;

  TDFeSSLAntesDeAssinar = procedure (var ConteudoXML: String;
     const docElement, infElement, SignatureNode, SelectionNamespaces,
     IdSignature: String) of object;

  { TDFeSSL }

  TDFeSSL = class(TPersistent)
  private
    FAntesDeAssinar: TDFeSSLAntesDeAssinar;
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

    procedure SetArquivoPFX(AValue: String);
    procedure SetDadosPFX(AValue: AnsiString);
    procedure SetNumeroSerie(AValue: String);
    procedure SetSenha(AValue: AnsiString);

    procedure SetSSLCryptLib(ASSLCryptLib: TSSLCryptLib);
    procedure SetSSLHttpLib(ASSLHttpLib: TSSLHttpLib);
    procedure SetSSLXmlSignLib(ASSLXmlSignLib: TSSLXmlSignLib);

  public
    constructor Create;
    procedure Clear;
    destructor Destroy; override;

    // Nota: ConteudoXML, DEVE estar em UTF8 //
    function Assinar(const ConteudoXML, docElement, infElement: String;
      SignatureNode: String = ''; SelectionNamespaces: String = '';
      IdSignature: String = ''; IdAttr: String = ''): String;
    // Envia por SoapAction o ConteudoXML (em UTF8) para URL. Retorna a resposta do Servico //
    function Enviar(var ConteudoXML: String; const AURL: String;
      const ASoapAction: String; AMimeType: String = ''): String;
    // Valida um Arquivo contra o seu Schema. Retorna True se OK, preenche MsgErro se False //
    // ConteudoXML, DEVE estar em UTF8
    function Validar(const ConteudoXML: String; ArqSchema: String;
      out MsgErro: String): Boolean;
    // Verifica se assinatura de um XML é válida. Retorna True se OK, preenche MsgErro se False //
    // ConteudoXML, DEVE estar em UTF8
    function VerificarAssinatura(const ConteudoXML: String; out MsgErro: String;
      const infElement: String; SignatureNode: String = '';
      SelectionNamespaces: String = ''; IdSignature: String = '';
      IdAttr: String = ''): Boolean;

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
    function CarregarCertificadoPublico(DadosX509Base64: Ansistring): Boolean; virtual;

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
    property SSLType: TSSLType read FSSLType write FSSLType default LT_all;
    property SSLDgst: TSSLDgst read FSSLDgst write FSSLDgst default dgstSHA1;

    property StoreLocation: TSSLStoreLocation read FStoreLocation
      write FStoreLocation default slCurrentUser;
    property StoreName: String read FStoreName write FStoreName;

    property ArquivoPFX: String read FArquivoPFX write SetArquivoPFX;
    property DadosPFX: AnsiString read FDadosPFX write SetDadosPFX;
    property NumeroSerie: String read FNumeroSerie write SetNumeroSerie;
    property Senha: AnsiString read GetSenha write SetSenha;

    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: String read FProxyPort write FProxyPort;
    property ProxyUser: String read FProxyUser write FProxyUser;
    property ProxyPass: String read FProxyPass write FProxyPass;

    property TimeOut: Integer read FTimeOut write FTimeOut default 5000;
    property NameSpaceURI: String read FNameSpaceURI write FNameSpaceURI;

    property UseCertificateHTTP: Boolean read FUseCertificateHTTP write FUseCertificateHTTP default True;

    property AntesDeAssinar: TDFeSSLAntesDeAssinar read FAntesDeAssinar write FAntesDeAssinar;
  end;


implementation

uses
  strutils,
  synacode,
  ACBrDFeUtil, ACBrValidador, ACBrUtil, ACBrDFeException
  {$IfNDef DFE_SEM_OPENSSL}
   ,ACBrDFeOpenSSL, ACBrDFeHttpOpenSSL, ACBrDFeXsLibXml2
   {$IfNDef DFE_SEM_XMLSEC}
    ,ACBrDFeXsXmlSec
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

procedure TDadosCertificado.SetSubjectName(AValue: String);
begin
  if FSubjectName = AValue then Exit;
  FSubjectName := AValue;

  FRazaoSocial := GetRazaoSocialFromSubjectName(FSubjectName);
  FCNPJ := GetCNPJFromSubjectName(FSubjectName);
end;

procedure TDadosCertificado.SetIssuerName(AValue: String);
begin
  if FIssuerName = AValue then Exit;
  FIssuerName := AValue;

  FCertificadora := GetCertificadoraFromIssuerName( FIssuerName );
end;

function TDadosCertificado.GetCNPJFromSubjectName( SubjectName: String ): String;
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
      if (ValidarCNPJ(Result) <> '') then
        Result := '';
    end;
  end;
end;

function TDadosCertificado.GetRazaoSocialFromSubjectName( SubjectName: String ): String;
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

function TDadosCertificado.GetCertificadoraFromIssuerName( SubjectName: String ): String;
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
  inherited SetItem (Index, Item) ;
end;

function TListaCertificados.GetObject(Index: Integer): TDadosCertificado;
begin
  Result := inherited GetItem(Index) as TDadosCertificado ;
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

function TDFeSSLCryptClass.CarregarCertificadoPublico(DadosX509Base64: Ansistring): Boolean;
begin
  {$IfDef FPC}Result := False;{$EndIf}
  raise EACBrDFeException.Create('"CarregarCertificadoPublico" não suportado em: ' +ClassName);
end;

procedure TDFeSSLCryptClass.CarregarCertificado;
begin
  FpCertificadoLido := True;
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
end;

destructor TDFeSSLHttpClass.Destroy;
begin
  inherited Destroy;
end;

function TDFeSSLHttpClass.GetHTTPResultCode: Integer;
begin
  Result := 0;
end;

function TDFeSSLHttpClass.GetInternalErrorCode: Integer;
begin
  Result := 0;
end;

procedure TDFeSSLHttpClass.ConfigurarHTTP(const AURL, ASoapAction: String;
  AMimeType: String);
begin
  raise EACBrDFeException.Create('Método "ConfigurarHTTP" não implementado em: '+ClassName);
end;

function TDFeSSLHttpClass.Enviar(const ConteudoXML: String; const AURL: String;
  const ASoapAction: String; AMimeType: String): String;
begin
  {$IFDEF FPC}
  Result := '';
  {$ENDIF}
  raise EACBrDFeException.Create('Método "Enviar" não implementado em: '+ClassName);
end;


{ TDFeSSLXmlSignClass }

constructor TDFeSSLXmlSignClass.Create(ADFeSSL: TDFeSSL);
begin
  inherited Create;
  FpDFeSSL := ADFeSSL;
end;

function TDFeSSLXmlSignClass.AdicionarSignatureElement(ConteudoXML: String;
  AddX509Data: Boolean; docElement, IdSignature: String;
  IdAttr: String = ''): String;
var
  URI, TagEndDocElement: String;
  I: Integer;
begin
  URI := ExtraiURI(ConteudoXML, IdAttr);

  TagEndDocElement := '</' + docElement + '>';
  I := PosLast(TagEndDocElement, ConteudoXML);
  if I = 0 then
    raise EACBrDFeException.Create('Não encontrei final do elemento: ' + TagEndDocElement);

  Result := copy(ConteudoXML, 1, I - 1) +
            SignatureElement(URI, AddX509Data, IdSignature, FpDFeSSL.SSLDgst) +
            TagEndDocElement;
end;

function TDFeSSLXmlSignClass.AjustarXMLAssinado(const ConteudoXML: String;
  X509DER: String): String;
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
  infElement: String; SignatureNode: String; SelectionNamespaces: String;
  IdSignature: String; IdAttr: String): String;
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
  MsgErro: String; const infElement: String; SignatureNode: String;
  SelectionNamespaces: String; IdSignature: String; IdAttr: String): Boolean;
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
  Clear;
end;

procedure TDFeSSL.Clear;
begin
  FArquivoPFX  := '';
  FDadosPFX    := '';
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
  FNameSpaceURI:= '';

  FSSLType       := LT_all;
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
  SignatureNode: String; SelectionNamespaces: String; IdSignature: String;
  IdAttr: String ): String;
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
                                        IdSignature, IdAttr);

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
  const ASoapAction: String; AMimeType: String): String;
begin
  // Nota: ConteudoXML, DEVE estar em UTF8 //
  if UseCertificateHTTP then
    CarregarCertificadoSeNecessario;

  if AMimeType = '' then
    AMimeType := 'application/soap+xml; charset=utf-8';

  Result := FSSLHttpClass.Enviar(ConteudoXML, AURL, ASoapAction, AMimeType);
end;

function TDFeSSL.Validar(const ConteudoXML: String; ArqSchema: String;
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
  MsgErro: String; const infElement: String; SignatureNode: String;
  SelectionNamespaces: String; IdSignature: String; IdAttr: String): Boolean;
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

function TDFeSSL.CarregarCertificadoPublico(DadosX509Base64: Ansistring
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

procedure TDFeSSL.SetArquivoPFX(AValue: String);
begin
  if FArquivoPFX = AValue then Exit;
  FArquivoPFX := AValue;
  FDadosPFX := '';   // Força a releitura de DadosPFX;
  if CertificadoLido then
    DescarregarCertificado;
end;

procedure TDFeSSL.SetDadosPFX(AValue: AnsiString);
begin
  if FDadosPFX = AValue then Exit;
  FDadosPFX := AValue;
  if CertificadoLido then
    DescarregarCertificado;
end;

procedure TDFeSSL.SetNumeroSerie(AValue: String);
begin
  if FNumeroSerie = AValue then Exit;
  FNumeroSerie := Trim(UpperCase(StringReplace(AValue, ' ', '', [rfReplaceAll])));
  FDadosPFX := '';   // Força a releitura de DadosPFX;
  if CertificadoLido then
    DescarregarCertificado;
end;

procedure TDFeSSL.SetSenha(AValue: AnsiString);
begin
  if (FK <> '') and (FSenha = StrCrypt(AValue, FK)) then
    Exit;

  FK := FormatDateTime('hhnnsszzz',Now);
  FSenha := StrCrypt(AValue, FK);  // Salva Senha de forma Criptografada, para evitar "Inspect"

  if CertificadoLido then
    DescarregarCertificado;
end;

procedure TDFeSSL.SetSSLCryptLib(ASSLCryptLib: TSSLCryptLib);
begin
  if ASSLCryptLib = FSSLCryptLib then
    exit;

  if Assigned(FSSLCryptClass) then
    FreeAndNil(FSSLCryptClass);

  case ASSLCryptLib of
    cryOpenSSL:
    begin
      {$IfNDef DFE_SEM_OPENSSL}
       FSSLCryptClass := TDFeOpenSSL.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a libOpenSSL foi desativado por compilação {$DEFINE DFE_SEM_OPENSSL}');
      {$EndIf}
    end;

    cryCapicom:
    begin
      {$IfNDef DFE_SEM_CAPICOM}
       FSSLCryptClass := TDFeCapicom.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a libCapicom foi desativado por compilação {$DEFINE DFE_SEM_CAPICOM}');
      {$EndIf}
    end;

    cryWinCrypt:
    begin
      {$IfDef MSWINDOWS}
       FSSLCryptClass := TDFeWinCrypt.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a libWinCrypt disponível apenas em MSWINDOWS');
      {$EndIf}
    end;

  else
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

  case ASSLHttpLib of
    httpWinINet, httpWinHttp:
    begin
      {$IfDef MSWINDOWS}
       FSSLHttpClass := TDFeHttpWinHttp.Create(Self, ASSLHttpLib);
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

  FSSLHttpLib := ASSLHttpLib;
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
      {$IfNDef DFE_SEM_OPENSSL}
       FSSLXmlSignClass := TDFeSSLXmlSignLibXml2.Create(Self);
      {$Else}
       raise EACBrDFeException.Create('Suporte a "xsXmlSec" foi desativado por compilação {$DEFINE DFE_SEM_OPENSSL}');
      {$EndIf}
    end;

  else
    FSSLXmlSignClass := TDFeSSLXmlSignClass.Create(Self);
  end;

  FSSLXmlSignLib := ASSLXmlSignLib;
end;

end.

