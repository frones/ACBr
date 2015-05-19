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

unit ACBrNFSeWebServices;

interface

uses
  {$IFDEF FPC}
    LResources, Controls, Graphics,
  {$ENDIF}
    StrUtils,
    Classes, SysUtils,
  {$IFDEF CLX}
    QDialogs,
  {$ELSE}
    Dialogs,
  {$ENDIF}
  {$IFDEF ACBrNFSeOpenSSL}
    HTTPSend,
  {$ELSE}
    SOAPHTTPClient, SOAPHTTPTrans, SOAPConst,
    JwaWinCrypt, JwaWinType, WinInet,
    ACBrCAPICOM_TLB, ACBrMSXML2_TLB,
  {$ENDIF}
    pcnGerador, pcnConversao, pcnAuxiliar, pnfsNFSe, pnfsNFSeG, pnfsConversao,
    pnfsEnvLoteRpsResposta, pnfsConsSitLoteRpsResposta,
    pnfsConsLoteRpsResposta, pnfsConsNfseporRpsResposta,
    pnfsConsNfseResposta, pnfsCancNfseResposta,
    pnfsGerarNfseResposta, pnfsSubsNfseResposta,
    ACBrNFSeNotasFiscais, ACBrNFSeConfiguracoes,
    ACBrProvedorGinfesV3, ACBrProvedorPublica, ACBrProvedorRJ,
    ACBrProvedorTiplan, ACBrProvedorISSNet, ACBrProvedorWebISS,
    ACBrProvedorProdemge, ACBrProvedorISSIntel, ACBrProvedorGovBR,
    ACBrProvedorRecife, ACBrProvedorSimplISS, ACBrProvedorThema,
    ACBrProvedorEquiplano, ACBrProvedorfintelISS, ACBrProvedorDigifred,
    ACBrProvedorBetha, ACBrProvedorBetim, ACBrProvedorSaatri,
    ACBrProvedorAbaco, ACBrProvedorGoiania, ACBrProvedorIssCuritiba,
    ACBrProvedorBHISS, ACBrProvedorNatal, ACBrProvedorTinus, ACBrProvedorISSDigital,
    ACBrProvedorISSe, ACBrProvedor4R, ACBrProvedorGovDigital,
    ACBrProvedorFiorilli, ACBrProvedorIssDsf, ACBrProvedorInfisc, ACBrProvedorCoplan,
    ACBrProvedorProdata, ACBrProvedorAgili, ACBrProvedorFISSLex,
    ACBrProvedorVirtual, ACBrProvedorPVH, ACBrProvedorFreire,
    ACBrProvedorLink3, ACBrProvedorSpeedGov, ACBrProvedorVitoria,
    ACBrProvedorMitra, ACBrProvedorTecnos, ACBrProvedorPronim,
    ACBrProvedorActcon, ACBrProvedorEL, ACBrProvedorEgoverneISS,
    ACBrProvedorSisPMJP, ACBrProvedorSystemPro, ACBrProvedorSalvador,
    ACBrProvedorDBSeller, ACBrProvedorLexsom, ACBrProvedorABRASFv1,
    ACBrProvedorABRASFv2, ACBrProvedorNFSEBrasil, ACBrProvedorSJP;

type

  TWebServicesBase = Class
  private
    procedure DoNFSeEnviarLoteRPS;
    procedure DoNFSeConsultarSituacaoLoteRPS;
    procedure DoNFSeConsultarLoteRPS;
    procedure DoNFSeConsultarNFSeporRPS;
    procedure DoNFSeConsultarNFSe;
    procedure DoNFSeConsultarSequencialRPS;
    procedure DoNFSeCancelarNFSe;
    procedure DoNFSeGerarNFSe;
    procedure DoNFSeLinkNFSe;
    procedure DoNFSeGerarLote;
    procedure DoNFSeEnviarSincrono;
    procedure DoNFSeSubstituirNFSe;

    {$IFDEF ACBrNFSeOpenSSL}
      procedure ConfiguraHTTP( HTTP : THTTPSend; Action : AnsiString);
    {$ELSE}
      procedure ConfiguraReqResp( ReqResp : THTTPReqResp);
      procedure OnBeforePost(const HTTPReqResp: THTTPReqResp; Data:Pointer);
    {$ENDIF}
  protected
    FProvedorClass: TProvedorClass;

    FCabMsg: WideString;
    FDadosMsg: AnsiString;
    FDadosSenha: AnsiString;
    FRetornoWS: AnsiString;
    FRetWS: AnsiString;
    FMsg: AnsiString;
    FURL: WideString;
    FConfiguracoes: TConfiguracoes;
    FACBrNFSe : TComponent;
    FHTTP_AG: String;
    FvAssinada: AnsiString;
    FTagI: AnsiString;
    FTagF: AnsiString;
    FPrefixo2: AnsiString;
    FPrefixo3: AnsiString;
    FPrefixo4: AnsiString;
    FNameSpaceCab: AnsiString;
    FNameSpaceDad: AnsiString;
    FVersaoLayOut: AnsiString;
    FVersaoDados: AnsiString;
    FVersaoXML: AnsiString;
    FURLNS1: String;
    FProvedor: TnfseProvedor;
    FxProvedor: String;

    FCabecalho: String;
    FServicoEnviar: String;
    FServicoConSit: String;
    FServicoConLot: String;
    FServicoConRps: String;
    FServicoConNfse: String;
    FServicoCancelar: String;
    FServicoGerar: String;
    FServicoConsSeqRPS:String;
    FServicoEnviarSincrono: String;
    FServicoSubstituir: String;
    FDefTipos: String;

    FNomeCidade: String;
    FRecepcaoLoteRps: String;
    FConsultaLoteRps: String;
    FConsultaNFSeRps: String;
    FConsultaSitLoteRps: String;
    FConsultaNFSe: String;
    FCancelaNFSe: String;
    FGerarNFSe: String;
    FConsSeqRPS: String;
    FLinkNFSe: String;
    FGerarLoteRps: String;
    FRecepcaoSincrono: String;
    FSubstituiNFSe: String;

    procedure LoadMsgEntrada;
    procedure LoadURL;
  public
    function Executar: Boolean; virtual;
    constructor Create(AOwner : TComponent); virtual;
    destructor Destroy; override;
    property CabMsg: WideString read FCabMsg;
    property DadosMsg: AnsiString read FDadosMsg;
    property DadosSenha: AnsiString read FDadosSenha;
    property RetWS: AnsiString read FRetWS;
    property Msg: AnsiString read FMsg;
    property HTTP_AG: String read FHTTP_AG;
    property vAssinada: AnsiString read FvAssinada;
    property TagI: AnsiString read FTagI;
    property TagF: AnsiString read FTagF;
    property Prefixo2: AnsiString read FPrefixo2;
    property Prefixo3: AnsiString read FPrefixo3;
    property Prefixo4: AnsiString read FPrefixo4;
    property NameSpaceCab: AnsiString read FNameSpaceCab;
    property NameSpaceDad: AnsiString read FNameSpaceDad;
    property VersaoLayOut: AnsiString read FVersaoLayOut;
    property VersaoDados: AnsiString read FVersaoDados;
    property VersaoXML: AnsiString read FVersaoXML;
    property URLNS1: String read FURLNS1;
    property Provedor: TnfseProvedor read FProvedor;
    property xProvedor: String read FxProvedor;

    property Cabecalho: String read FCabecalho;
    property ServicoEnviar: String read FServicoEnviar;
    property ServicoConSit: String read FServicoConSit;
    property ServicoConLot: String read FServicoConLot;
    property ServicoConRps: String read FServicoConRps;
    property ServicoConNfse: String read FServicoConNfse;
    property ServicoCancelar: String read FServicoCancelar;
    property ServicoGerar: String read FServicoGerar;
    property ServicoEnviarSincrono: String read FServicoEnviarSincrono;
    property ServicoSubstituir: String read FServicoSubstituir;
    property DefTipos: String read FDefTipos;

    property NomeCidade: String read FNomeCidade;
    property RecepcaoLoteRps: String read FRecepcaoLoteRps;
    property ConsultaLoteRps: String read FConsultaLoteRps;
    property ConsultaNFSeRps: String read FConsultaNFSeRps;
    property ConsultaSitLoteRps: String read FConsultaSitLoteRps;
    property ConsultaNFSe: String read FConsultaNFSe;
    property CancelaNFSe: String read FCancelaNFSe;
    property GerarNFSe: String read FGerarNFSe;
    property LinkNFSe: String read FLinkNFSe;
    property GerarLoteRps: String read FGerarLoteRps;
    property RecepcaoSincrono: String read FRecepcaoSincrono;
    property SubstituiNFSe: String read FSubstituiNFSe;
  end;

  TNFSeEnviarLoteRPS = Class(TWebServicesBase)
  private
    FNumeroLote: String;
    FDataRecebimento: TDateTime;
    FProtocolo : String;
    FNFSeRetorno : TretEnvLote;
    FNotasFiscais : TNotasFiscais;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;
    property NumeroLote: String read FNumeroLote;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Protocolo: String read FProtocolo;
    property NFSeRetorno: TretEnvLote read FNFSeRetorno write FNFSeRetorno;
  end;

  TNFSeConsultarSituacaoLoteRPS = Class(TWebServicesBase)
  private
    FCnpj: String;
    FInscricaoMunicipal: String;
    FProtocolo: String;
    FNumeroLote: String;
    FSituacao: String;
    FSenha: String;
    FFraseSecreta: String;
    FNFSeRetorno: TRetSitLote;
    FNotasFiscais : TNotasFiscais;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;
    property Cnpj: String read FCnpj write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property Protocolo: String read FProtocolo write FProtocolo;
    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Situacao: String read FSituacao;
    property Senha: String read FSenha write FSenha;
    property FraseSecreta: String read FFraseSecreta write FFraseSecreta;
    property NFSeRetorno: TRetSitLote read FNFSeRetorno write FNFSeRetorno;
  end;

  TNFSeConsultarLoteRPS = Class(TWebServicesBase)
  private
    FProtocolo: String;
    FNumeroLote: String;
    FNFSeRetorno: TRetLote;
    FNotasFiscais : TNotasFiscais;
    FCNPJ: String;
    FIM: String;
    FSenha: String;
    FFraseSecreta: String;
    FArquivoRetorno: WideString;
    FRazaoSocial: String;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;
    property Protocolo: String read FProtocolo write FProtocolo;
      //usado pelo provedor IssDsf
    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property NFSeRetorno: TRetLote read FNFSeRetorno write FNFSeRetorno;
    property NotasFiscais : TNotasFiscais read FNotasFiscais;
    property CNPJ: String read FCNPJ write FCNPJ;
    property IM: String read FIM write FIM;
    property Senha: String read FSenha write FSenha;
    property FraseSecreta: String read FFraseSecreta write FFraseSecreta;
    property ArquivoRetorno: WideString read FArquivoRetorno write FArquivoRetorno;
    //usado pelo provedor Tecnos
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
  end;

  TNFSeConsultarNfseRPS = Class(TWebServicesBase)
  private
    FNumero: String;
    FSerie: String;
    FTipo: String;
    FCnpj: String;
    FInscricaoMunicipal: String;
    FSenha: String;
    FFraseSecreta: String;
    FNFSeRetorno: TRetNfseRps;
    FNotasFiscais : TNotasFiscais;
    FRazaoSocial: String;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;
    property Numero: String read FNumero write FNumero;
    property Serie: String read FSerie write FSerie;
    property Tipo: String read FTipo write FTipo;
    property Cnpj: String read FCnpj write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property Senha: String read FSenha write FSenha;
    property FraseSecreta: String read FFraseSecreta write FFraseSecreta;
    property NFSeRetorno: TRetNfseRps read FNFSeRetorno write FNFSeRetorno;
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
  end;

  TNFSeConsultarNfse = Class(TWebServicesBase)
  private
    FCnpj: String;
    FInscricaoMunicipal: String;
    FDataInicial: TDateTime;
    FDataFinal: TDateTime;
    FNumeroNFSe: String;
    FNFSeRetorno: TRetNfse;
    FNotasFiscais : TNotasFiscais;
    FPagina: Integer;
    FSenha: String;
    FFraseSecreta: String;
    FCNPJTomador: String;
    FIMTomador: String;
    FNomeInter: String;
    FCNPJInter: String;
    FIMInter: String;
    FSerie: String;

  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;

    property Cnpj: String               read FCnpj               write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property DataInicial: TDateTime     read FDataInicial        write FDataInicial;
    property DataFinal: TDateTime       read FDataFinal          write FDataFinal;
    property NumeroNFSe: String         read FNumeroNFSe         write FNumeroNFSe;
    property NFSeRetorno: TRetNfse      read FNFSeRetorno        write FNFSeRetorno;
    property Pagina: Integer            read FPagina             write FPagina;
    property Senha: String              read FSenha              write FSenha;
    property FraseSecreta: String       read FFraseSecreta       write FFraseSecreta;
    property CNPJTomador: String        read FCNPJTomador        write FCNPJTomador;
    property IMTomador: String          read FIMTomador          write FIMTomador;
    property NomeInter: String          read FNomeInter          write FNomeInter;
    property CNPJInter: String          read FCNPJInter          write FCNPJInter;
    property IMInter: String            read FIMInter            write FIMInter;
    property Serie: String              read FSerie              write FSerie;

  end;

  TNFSeConsultarSequencialRPS = Class(TWebServicesBase)
  private
    FCnpj: String;
    FInscricaoMunicipal: String;
    FCidade: String;
    FSeriePrestacao: String;
    FNFSeRetorno: TRetNfse;
    FNotasFiscais : TNotasFiscais;
    FSequencialRPS: Integer;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;
    property Cnpj: String read FCnpj write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property Cidade: String read FCidade write FCidade;
    property SeriePrestacao: String read FSeriePrestacao write FSeriePrestacao;
    property SequencialRPS : Integer read FSequencialRPS write FSequencialRPS; //Incluido por Ailton Branco em 176/07/2014
    property NFSeRetorno: TRetNfse read FNFSeRetorno write FNFSeRetorno;
  end;

  TNFSeCancelarNfse = Class(TWebServicesBase)
  private
    FCodigoCancelamento: String;
    FMotivoCancelamento: String;
    FDataHora: TDateTime;
    FNFSeRetorno: TretCancNFSe;
    FNotasFiscais : TNotasFiscais;
    FCNPJ: String;
    FIM: String;
    FNumeroNFSe: String;
    FCodigoMunicipio: String;
    FArquivoRetorno: WideString;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;
    property CodigoCancelamento: String read FCodigoCancelamento write FCodigoCancelamento;
    property MotivoCancelamento: String read FMotivoCancelamento write FMotivoCancelamento;
    property DataHora: TDateTime read FDataHora write FDataHora;
    property NFSeRetorno: TretCancNFSe read FNFSeRetorno write FNFSeRetorno;
    property NumeroNFSe: String read FNumeroNFSe write FNumeroNFSe;
    property CNPJ: String read FCNPJ write FCNPJ;
    property IM: String read FIM write FIM;
    property CodigoMunicipio: String read FCodigoMunicipio write FCodigoMunicipio;
    property ArquivoRetorno: WideString read FArquivoRetorno write FArquivoRetorno;
  end;

  TNFSeGerarNFSe = Class(TWebServicesBase)
  private
    FNumeroRps: Integer;
    FNFSeRetorno : TGerarretNfse;
    FNotasFiscais : TNotasFiscais;
    FProtocolo: String;
    FDataRecebimento: TDateTime;
    FSituacao: String;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;
    property NumeroRps: integer read FNumeroRps;
    property NFSeRetorno: TGerarretNfse read FNFSeRetorno write FNFSeRetorno;
    property Protocolo: String read FProtocolo;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Situacao: String read FSituacao;
  end;

  TNFSeEnviarSincrono = Class(TWebServicesBase)
  private
    FNumeroLote: String;
    FNFSeRetorno : TGerarretNfse;
    FNotasFiscais : TNotasFiscais;
    FProtocolo: String;
    FDataRecebimento: TDateTime;
    FSituacao: String;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;
    property NumeroLote: String read FNumeroLote;
    property NFSeRetorno: TGerarretNfse read FNFSeRetorno write FNFSeRetorno;
    property Protocolo: String read FProtocolo;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Situacao: String read FSituacao;
  end;

  TNFSeSubstituirNFSe = Class(TWebServicesBase)
  private
    FNotasFiscais : TNotasFiscais;

    FCodigoCancelamento: String;
    FMotivoCancelamento: String;
    FDataHora: TDateTime;
    FNumeroNFSe: String;
    FCNPJ: String;
    FIM: String;
    FCodigoMunicipio: String;

    FNumeroRps: Integer;
    FProtocolo: String;
    FDataRecebimento: TDateTime;
    FSituacao: String;

    FNFSeRetorno : TretSubsNfse;
    FArquivoRetorno: WideString;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    destructor Destroy; override;

    property CodigoCancelamento: String read FCodigoCancelamento write FCodigoCancelamento;
    property MotivoCancelamento: String read FMotivoCancelamento write FMotivoCancelamento;
    property DataHora: TDateTime        read FDataHora           write FDataHora;
    property NumeroNFSe: String         read FNumeroNFSe         write FNumeroNFSe;
    property CNPJ: String               read FCNPJ               write FCNPJ;
    property IM: String                 read FIM                 write FIM;
    property CodigoMunicipio: String    read FCodigoMunicipio    write FCodigoMunicipio;

    property NumeroRps: integer         read FNumeroRps;
    property Protocolo: String          read FProtocolo;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Situacao: String           read FSituacao;

    property NFSeRetorno: TretSubsNfse  read FNFSeRetorno    write FNFSeRetorno;
    property ArquivoRetorno: WideString read FArquivoRetorno write FArquivoRetorno;
  end;

  TNFSeLinkNFSe = Class(TWebServicesBase)
  private
    FNotasFiscais : TNotasFiscais;
    FNumeroNFSe: integer;
    FCodVerif: String;
    FLink: String;
    FIM: String;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;

    property NumeroNFSe: integer read FNumeroNFSe;
    property CodVerif: String read FCodVerif;
    property Link: String read FLink;
    property IM: String read FIM;
  end;

  TNFSeGerarLoteRPS = Class(TWebServicesBase)
  private
    FNumeroLote: String;
    FNotasFiscais : TNotasFiscais;
  public
    function Executar: Boolean; override;
    constructor Create(AOwner : TComponent; ANotasFiscais : TNotasFiscais); reintroduce;
    property NumeroLote: String read FNumeroLote;
  end;

  TWebServices = Class(TWebServicesBase)
  private
    FACBrNFSe: TComponent;
    FEnviar: TNFSeEnviarLoteRPS;
    FConsSitLote: TNFSeConsultarSituacaoLoteRPS;
    FConsLote: TNFSeConsultarLoteRPS;
    FConsNfseRps: TNFSeConsultarNfseRps;
    FConsNfse: TNFSeConsultarNfse;
    FConsSeqRPS: TNFSeConsultarSequencialRPS;
    FCancNfse: TNFSeCancelarNfse;
    FGerarNfse: TNFSeGerarNfse;
    FLinkNfse: TNFSeLinkNfse;
    FGerarLoteRPS: TNFSeGerarLoteRPS;
    FEnviarSincrono: TNFSeEnviarSincrono;
    FSubNfse: TNFSeSubstituirNfse;
  public
    constructor Create(AFNotaFiscalEletronica: TComponent); reintroduce;
    destructor Destroy; override;
    function Envia(ALote:Integer): Boolean; overload;
    function Envia(ALote:String): Boolean; overload;
    function ConsultaSituacao(ACnpj, AInscricaoMunicipal, AProtocolo: String;
                              const ANumLote: String = ''): Boolean;
    function ConsultaLoteRps(AProtocolo: String;
                             const CarregaProps: boolean = true): Boolean; overload;
    function ConsultaLoteRps(AProtocolo,
                             ACNPJ, AInscricaoMunicipal: String;
                             const ASenha: String = '';
                             const AFraseSecreta: String = '';
                             const ARazaoSocial: String = ''): Boolean; overload;
    function ConsultaNFSeporRps(ANumero, ASerie, ATipo, ACnpj, AInscricaoMunicipal: String;
                                const ASenha: String = '';
                                const AFraseSecreta: String = '';
                                const ARazaoSocial: String = ''): Boolean;
    function ConsultaNFSe(ACnpj,
                          AInscricaoMunicipal: String;
                          ADataInicial,
                          ADataFinal: TDateTime;
                          NumeroNFSe: String = '';
                          APagina: Integer = 1;
                          const ASenha: String = '';
                          const AFraseSecreta: String = '';
                          ACNPJTomador: String = '';
                          AIMTomador: String = '';
                          ANomeInter: String = '';
                          ACNPJInter: String = '';
                          AIMInter: String = '';
                          ASerie: String = ''): Boolean;
                          
    function ConsultaSequencialRPS(ACidade, ACnpj, AInscricaoMunicipal, ASeriePrestacao: String):Boolean;
    function CancelaNFSe(ACodigoCancelamento: String;
                         const CarregaProps: boolean = true): Boolean; overload;
    function CancelaNFSe(ACodigoCancelamento, ANumeroNFSe, ACNPJ, AInscricaoMunicipal,
                         ACodigoMunicipio: String): Boolean; overload;
    function Gera(ARps:Integer): Boolean;
    function LinkNFSeGerada(ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String): String;
    function GeraLote(ALote:Integer): Boolean; overload;
    function GeraLote(ALote:String): Boolean; overload;
    function EnviaSincrono(ALote:Integer): Boolean; overload;
    function EnviaSincrono(ALote:String): Boolean; overload;
    function SubstitiNFSe(ACodigoCancelamento, ANumeroNFSe: String): Boolean; 
  published
    property ACBrNFSe: TComponent read FACBrNFSe write FACBrNFSe;
    property Enviar: TNFSeEnviarLoteRPS read FEnviar write FEnviar;
    property ConsSitLote: TNFSeConsultarSituacaoLoteRPS read FConsSitLote write FConsSitLote;
    property ConsLote: TNFSeConsultarLoteRPS read FConsLote write FConsLote;
    property ConsNfseRps: TNFSeConsultarNfseRps read FConsNfseRps write FConsNfseRps;
    property ConsNfse: TNFSeConsultarNfse read FConsNfse write FConsNfse;
    property ConsSeqRPS: TNFSeConsultarSequencialRPS read FConsSeqRPS write FConsSeqRPS;
    property CancNfse: TNFSeCancelarNfse read FCancNfse write FCancNfse;
    property GerarNfse: TNFSeGerarNfse read FGerarNfse write FGerarNfse;
    property LinkNfse: TNFSeLinkNfse read FLinkNfse write FLinkNfse;
    property GerarLoteRPS: TNFSeGerarLoteRPS read FGerarLoteRPS write FGerarLoteRPS;
    property EnviarSincrono: TNFSeEnviarSincrono read FEnviarSincrono write FEnviarSincrono;
    property SubNfse: TNFSeSubstituirNfse read FSubNfse write FSubNfse;
  end;

implementation

uses
 {$IFDEF ACBrNFSeOpenSSL}
   ssl_openssl,
 {$ENDIF}
 Math, ACBrUtil, ACBrDFeUtil, ACBrNFSe;

{$IFNDEF ACBrNFSeOpenSSL}
const
  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;
{$ENDIF}

{ TWebServicesBase }
constructor TWebServicesBase.Create(AOwner: TComponent);
begin
 FConfiguracoes := TConfiguracoes( TACBrNFSe( AOwner ).Configuracoes );
 FACBrNFSe      := TACBrNFSe( AOwner );
end;

destructor TWebServicesBase.Destroy;
begin
 if Assigned(FProvedorClass) then
  FProvedorClass.Free();
 inherited;
end;

{$IFDEF ACBrNFSeOpenSSL}
procedure TWebServicesBase.ConfiguraHTTP( HTTP : THTTPSend; Action : AnsiString);
begin
 if FileExists(FConfiguracoes.Certificados.Certificado)
  then HTTP.Sock.SSL.PFXfile := FConfiguracoes.Certificados.Certificado
  else HTTP.Sock.SSL.PFX     := FConfiguracoes.Certificados.Certificado;

  HTTP.Sock.SSL.KeyPassword := FConfiguracoes.Certificados.Senha;

  HTTP.ProxyHost  := FConfiguracoes.WebServices.ProxyHost;
  HTTP.ProxyPort  := FConfiguracoes.WebServices.ProxyPort;
  HTTP.ProxyUser  := FConfiguracoes.WebServices.ProxyUser;
  HTTP.ProxyPass  := FConfiguracoes.WebServices.ProxyPass;

//  HTTP.Sock.RaiseExcept := True;

  HTTP.MimeType  := 'text/xml; charset=utf-8';
  HTTP.UserAgent := '';
  HTTP.Protocol  := '1.1' ;

  HTTP.AddPortNumberToHost := False;
  HTTP.Headers.Add(Action);
end;
{$ELSE}
procedure TWebServicesBase.ConfiguraReqResp(ReqResp: THTTPReqResp);
begin
  if FConfiguracoes.WebServices.ProxyHost <> '' then
   begin
     ReqResp.Proxy    := FConfiguracoes.WebServices.ProxyHost+':'+FConfiguracoes.WebServices.ProxyPort;
     ReqResp.UserName := FConfiguracoes.WebServices.ProxyUser;
     ReqResp.Password := FConfiguracoes.WebServices.ProxyPass;
   end;
  ReqResp.OnBeforePost := OnBeforePost;
end;

procedure TWebServicesBase.OnBeforePost(const HTTPReqResp: THTTPReqResp;
  Data: Pointer);

function GetLastErrorText: String;
var
 aMsg: String;
begin
 case GetLastError of
  12030: aMsg := 'A conexão com o servidor foi finalizada.';
  12044: aMsg := 'O Servidor está solicitando autenticação do cliente.';
  12046: aMsg := 'Autorização do cliente não está configurado neste computador.';
  else aMsg := IntToStr(GetLastError);
 end;
 Result := aMsg;
end;

var
 Cert         : ICertificate2;
 CertContext  : ICertContext;
 PCertContext : Pointer;
 ContentHeader: String;
begin
 if EstaVazio( FConfiguracoes.Certificados.NumeroSerie )
  then Exit;

 Cert        := FConfiguracoes.Certificados.GetCertificado;
 CertContext := Cert as ICertContext;
 CertContext.Get_CertContext(Integer(PCertContext));

 // proIssDSF incluido por Ailton Branco 16/07/2014
 if not (FProvedor in [proGovBr, proSimplISS, proAbaco, proISSNet, pro4R, proIssDSF, proInfisc, 
                       proFiorilli, proProdata, proCoplan, proThema, proVirtual,
                       proPVH, proFreire, proTecnos, proPronim, proPublica,
                       proEgoverneISS, proActcon, proDBSeller, proLexsom])
  then begin

    if FProvedor = proSystemPro then
    begin
      if not InternetSetOption(Data, INTERNET_OPTION_CLIENT_CERT_CONTEXT, PCertContext, Sizeof(CERT_CONTEXT)) then
      begin
        if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog) then
          TACBrNFSe( FACBrNFSe ).OnGerarLog('ERRO: Erro OnBeforePost: ' + IntToStr(GetLastError));
        raise Exception.Create( 'Erro OnBeforePost: ' + GetLastErrorText {IntToStr(GetLastError)} );
      end;
    end
    else
    begin
   if not InternetSetOption(Data, INTERNET_OPTION_CLIENT_CERT_CONTEXT, PCertContext, Sizeof(CERT_CONTEXT)*5)
    then begin
     if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
      then TACBrNFSe( FACBrNFSe ).OnGerarLog('ERRO: Erro OnBeforePost: ' + IntToStr(GetLastError));
     raise Exception.Create( 'Erro OnBeforePost: ' + GetLastErrorText {IntToStr(GetLastError)} );
      end;
    end;
  end;

 if trim(FConfiguracoes.WebServices.ProxyUser) <> ''
  then begin
   if not InternetSetOption(Data, INTERNET_OPTION_PROXY_USERNAME, PChar(FConfiguracoes.WebServices.ProxyUser), Length(FConfiguracoes.WebServices.ProxyUser))
    then raise Exception.Create( 'Erro OnBeforePost: ' + IntToStr(GetLastError) );
   end;

 if trim(FConfiguracoes.WebServices.ProxyPass) <> ''
  then begin
   if not InternetSetOption(Data, INTERNET_OPTION_PROXY_PASSWORD, PChar(FConfiguracoes.WebServices.ProxyPass),Length (FConfiguracoes.WebServices.ProxyPass))
    then raise Exception.Create( 'Erro OnBeforePost: ' + IntToStr(GetLastError) );
   end;

  if (pos('SCERECEPCAORFB',UpperCase(FURL)) <= 0) and
     (pos('SCECONSULTARFB',UpperCase(FURL)) <= 0) then
   begin

     if FConfiguracoes.WebServices.VersaoSoap = '1.2'
      then ContentHeader := Format(ContentTypeTemplate, ['application/soap+xml; charset=utf-8'])
      else ContentHeader := Format(ContentTypeTemplate, ['text/xml; charset=utf-8']);

     HttpAddRequestHeaders(Data, PChar(ContentHeader), Length(ContentHeader), HTTP_ADDREQ_FLAG_REPLACE);
   end;
  HTTPReqResp.CheckContentType;
end;
{$ENDIF}

function TWebServicesBase.Executar: Boolean;
begin
 Result := False;
 LoadMsgEntrada;
 LoadURL;
end;

procedure TWebServicesBase.LoadMsgEntrada;
begin
 FxProvedor := FConfiguracoes.WebServices.xProvedor;
 FProvedor  := FConfiguracoes.WebServices.Provedor;

 FProvedorClass.Free;

 case FProvedor of
  proGINFES:      FProvedorClass := TProvedorGinfesV3.Create;
  proPublica:     FProvedorClass := TProvedorPublica.Create;
  proRJ:          FProvedorClass := TProvedorRJ.Create;
  proTiplan:      FProvedorClass := TProvedorTiplan.Create;
  proISSNet:      FProvedorClass := TProvedorISSNet.Create;
  proWebISS:      FProvedorClass := TProvedorWebISS.Create;
  proProdemge:    FProvedorClass := TProvedorProdemge.Create;
  proISSIntel:    FProvedorClass := TProvedorISSIntel.Create;
  proGovBR:       FProvedorClass := TProvedorGovBR.Create;
  proRecife:      FProvedorClass := TProvedorRecife.Create;
  proSimplISS:    FProvedorClass := TProvedorSimplISS.Create;
  proThema:       FProvedorClass := TProvedorThema.Create;
  proEquiplano:   FProvedorClass := TProvedorEquiplano.Create;
  profintelISS:   FProvedorClass := TProvedorfintelISS.Create;
  proDigifred:    FProvedorClass := TProvedorDigifred.Create;
  proBetha:       FProvedorClass := TProvedorBetha.Create;
  proBetim:       FProvedorClass := TProvedorBetim.Create;
  proSaatri:      FProvedorClass := TProvedorSaatri.Create;
  proAbaco:       FProvedorClass := TProvedorAbaco.Create;
  proGoiania:     FProvedorClass := TProvedorGoiania.Create;
  proIssCuritiba: FProvedorClass := TProvedorIssCuritiba.Create;
  proBHISS:       FProvedorClass := TProvedorBHISS.Create;
  proNatal:       FProvedorClass := TProvedorNatal.Create;
  proTinus:        FProvedorClass := TProvedorTinus.Create;
  proISSDigital:  FProvedorClass := TProvedorISSDigital.Create;
  proISSe:        FProvedorClass := TProvedorISSe.Create;
  proSystemPro:   FProvedorClass := TProvedorSystemPro.Create;
  pro4R:          FProvedorClass := TProvedor4R.Create;
  proGovDigital:  FProvedorClass := TProvedorGovDigital.Create;
  proFiorilli:    FProvedorClass := TProvedorFiorilli.Create;
  proIssDsf:      FProvedorClass := TProvedorIssDsf.Create;
  proInfisc:      FProvedorClass := TProvedorInfisc.Create;
  proCoplan:      FProvedorClass := TProvedorCoplan.Create;
  proProdata:     FProvedorClass := TProvedorProdata.Create;
  proAgili:       FProvedorClass := TProvedorAgili.Create;
  proFISSLex:     FProvedorClass := TProvedorFISSLex.Create;
  proVirtual:     FProvedorClass := TProvedorVirtual.Create;
  proPVH:         FProvedorClass := TProvedorPVH.Create;
  proFreire:      FProvedorClass := TProvedorFreire.Create;
  proLink3:       FProvedorClass := TProvedorLink3.Create;
  proSpeedGov:    FProvedorClass := TProvedorSpeedGov.Create;
  proVitoria:     FProvedorClass := TProvedorVitoria.Create;
  proMitra:       FProvedorClass := TProvedorMitra.Create;
  proTecnos:      FProvedorClass := TProvedorTecnos.Create;
  proPronim:      FProvedorClass := TProvedorPronim.Create;
  proActcon:      FProvedorClass := TProvedorActcon.Create;
  proEL:          FProvedorClass := TProvedorEL.Create;
  proEgoverneISS: FProvedorClass := TProvedorEgoverneISS.Create;
  proSisPMJP:     FProvedorClass := TProvedorSisPMJP.Create;
  proSalvador:    FProvedorClass := TProvedorSalvador.Create;
  proDBSeller:    FProvedorClass := TProvedorDBSeller.Create;
  proLexsom:      FProvedorClass := TProvedorLexsom.Create;
  proABRASFv1:    FProvedorClass := TProvedorABRASFv1.Create;
  proABRASFv2:    FProvedorClass := TProvedorABRASFv2.Create;
  proNFSEBrasil:  FProvedorClass := TProvedorNFSEBrasil.Create;
  proSJP:         FProvedorClass := TProvedorSJP.Create;
 end;

 FPrefixo2 := FConfiguracoes.WebServices.Prefixo2;
 FPrefixo3 := FConfiguracoes.WebServices.Prefixo3;
 FPrefixo4 := FConfiguracoes.WebServices.Prefixo4;
 FURLNS1   := FConfiguracoes.WebServices.NameSpace;

 FVersaoLayOut          := FConfiguracoes.WebServices.VersaoCabecalho;
 FVersaoDados           := FConfiguracoes.WebServices.VersaoDados;
 FVersaoXML             := FConfiguracoes.WebServices.VersaoXML;
 FHTTP_AG               := FConfiguracoes.WebServices.URL;
 FCabecalho             := FConfiguracoes.WebServices.Cabecalho;
 FServicoEnviar         := FConfiguracoes.WebServices.ServicoEnviar;
 FServicoConSit         := FConfiguracoes.WebServices.ServicoConSit;
 FServicoConLot         := FConfiguracoes.WebServices.ServicoConLot;
 FServicoConRps         := FConfiguracoes.WebServices.ServicoConRps;
 FServicoConNfse        := FConfiguracoes.WebServices.ServicoConNfse;
 FServicoCancelar       := FConfiguracoes.WebServices.ServicoCancelar;
 FServicoGerar          := FConfiguracoes.WebServices.ServicoGerar;
 FServicoEnviarSincrono := FConfiguracoes.WebServices.ServicoEnviarSincrono;
 FDefTipos              := FConfiguracoes.WebServices.DefTipos;

 if self is TNFSeEnviarLoteRps
  then DoNFSeEnviarLoteRps
  else if self is TNFSeConsultarSituacaoLoteRps
  then DoNFSeConsultarSituacaoLoteRps
  else if self is TNFSeConsultarLoteRps
  then DoNFSeConsultarLoteRps
  else if self is TNFSeConsultarNfseRps
  then DoNFSeConsultarNfseporRps
  else if self is TNFSeConsultarNfse
  then DoNFSeConsultarNfse
  else if self is TNFSeConsultarSequencialRPS
  then DoNFSeConsultarSequencialRPS
  else if self is TNFSeCancelarNfse
  then DoNFSeCancelarNfse
  else if self is TNFSeGerarNfse
  then DoNFSeGerarNfse
  else if self is TNFSeLinkNfse
  then DoNFSeLinkNfse
  else if self is TNFSeGerarLoteRPS
  then DoNFSeGerarLote
  else if self is TNFSeEnviarSincrono
  then DoNFSeEnviarSincrono
  else if self is TNFSeSubstituirNfse
  then DoNFSeSubstituirNfse
end;

procedure TWebServicesBase.LoadURL;
begin
 if FConfiguracoes.WebServices.AmbienteCodigo = 1
  then begin
   FNomeCidade         := FConfiguracoes.WebServices.ProNomeCidade;
   FRecepcaoLoteRps    := FConfiguracoes.WebServices.ProRecepcaoLoteRPS;
   FConsultaLoteRps    := FConfiguracoes.WebServices.ProConsultaLoteRPS;
   FConsultaNFSeRps    := FConfiguracoes.WebServices.ProConsultaNFSeRPS;
   FConsultaSitLoteRps := FConfiguracoes.WebServices.ProConsultaSitLoteRPS;
   FConsultaNFSe       := FConfiguracoes.WebServices.ProConsultaNFSe;
   FCancelaNFSe        := FConfiguracoes.WebServices.ProCancelaNFSe;
   FGerarNFSe          := FConfiguracoes.WebServices.ProGerarNFSe;
   FConsSeqRPS         := FConfiguracoes.WebServices.ProConsultaSeqRPS; // Alterado por Ailton Branco 16/072014
   FRecepcaoSincrono   := FConfiguracoes.WebServices.ProRecepcaoSincrono;
   FSubstituiNFSe      := FConfiguracoes.WebServices.ProSubstituiNFSe;
  end
  else begin
   FNomeCidade         := FConfiguracoes.WebServices.HomNomeCidade;
   FRecepcaoLoteRps    := FConfiguracoes.WebServices.HomRecepcaoLoteRPS;
   FConsultaLoteRps    := FConfiguracoes.WebServices.HomConsultaLoteRPS;
   FConsultaNFSeRps    := FConfiguracoes.WebServices.HomConsultaNFSeRPS;
   FConsultaSitLoteRps := FConfiguracoes.WebServices.HomConsultaSitLoteRPS;
   FConsultaNFSe       := FConfiguracoes.WebServices.HomConsultaNFSe;
   FCancelaNFSe        := FConfiguracoes.WebServices.HomCancelaNFSe;
   FGerarNFSe          := FConfiguracoes.WebServices.HomGerarNFSe;
   FConsSeqRPS         := FConfiguracoes.WebServices.HomConsultaSeqRPS; // Alterado por Ailton Branco 16/072014
   FRecepcaoSincrono   := FConfiguracoes.WebServices.HomRecepcaoSincrono;
   FSubstituiNFSe      := FConfiguracoes.WebServices.HomSubstituiNFSe;
  end;

 if self is TNFSeEnviarLoteRps
  then FURL := FRecepcaoLoteRps
  else if self is TNFSeConsultarSituacaoLoteRps
  then FURL := FConsultaSitLoteRps
  else if self is TNFSeConsultarLoteRps
  then FURL := FConsultaLoteRps
  else if self is TNFSeConsultarNfseRps
  then FURL := FConsultaNFSeRps
  else if self is TNFSeConsultarNfse
  then FURL := FConsultaNFSe
  else if self is TNFSeCancelarNfse
  then FURL := FCancelaNFSe
  else if self is TNFSeGerarNfse
  then FURL := FGerarNFSe
  else if self is TNFSeEnviarSincrono
  then FURL := FRecepcaoSincrono
  else if self is TNFSeConsultarSequencialRPS
  then FURL := FConsSeqRPS
  else if self is TNFSeSubstituirNfse
  then FURL := FSubstituiNFSe;
end;

procedure TWebServicesBase.DoNFSeEnviarLoteRPS;
var
 i         : Integer;
 vNotas    : WideString;
 URI       : String;
 Separador : String;
 DataInicial, DataFinal : TDateTime;
 TotalServicos, TotalDeducoes: Double;
begin
 vNotas := '';

 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoEnviar <> '' then
 begin
   case FProvedor of
    proIssDSF: FNameSpaceDad :=  'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '"'
                    + ' xmlns:tipos="http://localhost:8080/WsNFe2/tp"'
                    + ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
                    + ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote http://localhost:8080/WsNFe2/xsd/ReqEnvioLoteRPS.xsd"';

    proEquiplano: FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="http://www.equiplano.com.br/esnfs" ' +
                          'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                          'xsi:schemaLocation="http://www.equiplano.com.br/enfs esRecepcionarLoteRpsEnvio_v01.xsd"';

    proNFSEBrasil: FNameSpaceDad := FNameSpaceDad + 'xmlns:xs="http://www.nfsebrasil.net.br/nfse/rps/xsd/rps.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';

    proEL: FNameSpaceDad := 'xmlns' + stringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoEnviar + '" ' +
                            'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                            'xmlns:xsd="http://www.w3.org/2001/XMLSchema" '+
                            'xsi:schemaLocation="' + FHTTP_AG + Separador + FServicoEnviar +' '+ FServicoEnviar + ' "';
    else begin
           if (RightStr(FHTTP_AG, 1) = '/') then
           begin
             if Prefixo3 <> '' then
               FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoEnviar + '"'
             else
               FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoEnviar + '"';
           end
           else begin
             if Prefixo3 <> '' then
               FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
             else
               FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
           end;
         end;
   end;
 end
 else
   FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> '' then
     begin
     // Wilker
     if FProvedor = proIssDsf then
       FNameSpaceDad := FNameSpaceDad + '>'
     else if FProvedor = proInfisc then
       FNameSpaceDad := FNameSpaceDad + '>'
     else
       FNameSpaceDad := FNameSpaceDad + ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">'
    end
   else 
	 FNameSpaceDad := FNameSpaceDad + ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 if FConfiguracoes.Certificados.AssinaRPS
  then begin
   for i := 0 to TNFSeEnviarLoteRPS(Self).FNotasFiscais.Count-1 do
    begin
     case FProvedor of
      pro4R,
      proAgili,
      profintelISS,
      proFiorilli,
      proGoiania,
      proISSDigital,
      proISSe,
      proSystemPro,
      proCoplan,
      proProdata,
      proVitoria,
      proPVH,
      proSaatri,
      proSisPMJP,
      proFreire,
      proLink3,
      proGovDigital,
      proVirtual: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      proTecnos: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico', '</' + Prefixo4 + 'tcDeclaracaoPrestacaoServico>') +
                                   '</' + Prefixo4 + 'tcDeclaracaoPrestacaoServico>' +
                              '</' + Prefixo4 + 'Rps>';

      proDigifred: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps ' +
                                 RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'Rps', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      proIssDSF : vNotas :=  vNotas + TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps;//.XML_Rps_Ass;

      proInfisc : vNotas :=  vNotas + TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps;//.XML_Rps_Ass;

      else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
     end;
    end;
  end
  else begin
   for i := 0 to TNFSeEnviarLoteRPS(Self).FNotasFiscais.Count-1 do
   begin
     case FProvedor of
      profintelISS, proSaatri, proSisPMJP, proCoplan, proGoiania, proISSDigital,
      proISSe, proSystemPro, pro4R, proFiorilli, proProdata, proVitoria, proPVH,
      proAgili, proVirtual, proFreire, proLink3,
      proGovDigital: vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                               '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                              '</' + Prefixo4 + 'Rps>';

      proIssDSF,
      proInfisc,
      proEquiplano,
      proEL: vNotas := vNotas + TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps;

      proTecnos: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico', '</tcDeclaracaoPrestacaoServico>') +
                               '</tcDeclaracaoPrestacaoServico>'+
                              '</' + Prefixo4 + 'Rps>';

      proNFSEBrasil: begin
                       vNotas := StringReplace(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps, '</Rps>', '', [rfReplaceAll]) + '</Rps>';
                       vNotas := StringReplace(vNotas, '<Rps>', '', [rfReplaceAll]);
                       vNotas := '<Rps>' + StringReplace(vNotas, '<InfRps>', '', [rfReplaceAll]);
                     end;

      else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
     end;
   end;
  end;

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 URI := '';

 URI := FProvedorClass.GetURI(URI);

 FTagI := FProvedorClass.Gera_TagI(acRecepcionar, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URI);

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);
 FTagF := FProvedorClass.Gera_TagF(acRecepcionar, Prefixo3);

 case FProvedor of
  proIssDSF: begin
              DataInicial := TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.DataEmissao;
              DataFinal   := DataInicial;

              for i := 0 to TNFSeEnviarLoteRPS(Self).FNotasFiscais.Count-1 do
               begin
                if TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[i].NFSe.DataEmissao < dataInicial then
                  DataInicial := TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[i].NFSe.DataEmissao;
                if TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[i].NFSe.DataEmissao > dataFinal then
                  DataFinal := TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[i].NFSe.DataEmissao;
                TotalServicos := TotalServicos + TNFSeEnviarLoteRps(Self).FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorServicos;
                TotalDeducoes := TotalDeducoes + TNFSeEnviarLoteRps(Self).FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorDeducoes;
               end;

              FDadosMsg := TNFSeG.Gera_DadosMsgEnviarLoteDSF(Prefixo3, Prefixo4,
                                                      FConfiguracoes.WebServices.Identificador,
                                                      NameSpaceDad, FVersaoXML,
                                                      TNFSeEnviarLoteRps(Self).NumeroLote,
                                                      CodCidadeToCodSiafi( StrToIntDef(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                      TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                      TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial,
                                                      LowerCase(booltostr(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Transacao, True)),
                                                      IntToStr(TNFSeEnviarLoteRps(Self).FNotasFiscais.Count),
                                                      FormatFloat('0.00', TotalServicos),
                                                      FormatFloat('0.00', TotalDeducoes),
                                                      DataInicial, DataFinal,
                                                      vNotas,
                                                      FTagI, FTagF);
             end;
  proInfisc: begin
              FDadosMsg := TNFSeG.Gera_DadosMsgEnviarLoteInfisc(Prefixo3, Prefixo4,
                                                                FConfiguracoes.WebServices.Identificador,
                                                                NameSpaceDad, FVersaoXML,
                                                                TNFSeEnviarLoteRps(Self).NumeroLote,
                                                                CodCidadeToCodSiafi( StrToIntDef(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                                OnlyNumber(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                                TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                                TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial,
                                                                LowerCase(booltostr(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Transacao, True)),
                                                                IntToStr(TNFSeEnviarLoteRps(Self).FNotasFiscais.Count),
                                                                FormatFloat('0.00', TNFSeEnviarLoteRps(Self).FNotasFiscais.Items[0].NFSe.Servico.Valores.ValorServicos),
                                                                FormatFloat('0.00', TNFSeEnviarLoteRps(Self).FNotasFiscais.Items[0].NFSe.Servico.Valores.ValorDeducoes),
                                                                DataInicial, DataFinal,
                                                                vNotas,
                                                                FTagI, FTagF);
             end;
  proEquiplano: begin
                 FDadosMsg := TNFSeG.Gera_DadosMsgEnviarLoteEquiplano(FVersaoXML,
                                                        TNFSeEnviarLoteRps(Self).NumeroLote,
                                                        IntToStr(TNFSeEnviarLoteRps(Self).FNotasFiscais.Count),
                                                        OnlyNumber(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                        TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                        FConfiguracoes.WebServices.CodigoMunicipio,
                                                        (TNFSeEnviarLoteRps(Self).FNotasFiscais.Items[0].NFSe.OptanteSimplesNacional = snSim),
                                                        vNotas,
                                                        FTagI, FTagF);
                end;
  proEL: begin
                 FDadosMsg := TNFSeG.Gera_DadosMsgEnviarLoteEL(NameSpaceDad,
                                                        TNFSeEnviarLoteRps(Self).NumeroLote,
                                                        IntToStr(TNFSeEnviarLoteRps(Self).FNotasFiscais.Count),
                                                        OnlyNumber(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                        TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                        FConfiguracoes.WebServices.CodigoMunicipio,
                                                        (TNFSeEnviarLoteRps(Self).FNotasFiscais.Items[0].NFSe.OptanteSimplesNacional = snSim),
                                                        TNFSeEnviarLoteRps(Self).FNotasFiscais.Items[0].NFSe.InfID.ID,
                                                        vNotas,
                                                        FTagI, FTagF);
                end;
  else FDadosMsg := TNFSeG.Gera_DadosMsgEnviarLote(Prefixo3, Prefixo4,
                                                   FConfiguracoes.WebServices.Identificador,
                                                   NameSpaceDad, VersaoDados, FVersaoXML,
                                                   TNFSeEnviarLoteRps(Self).NumeroLote,
                                                   OnlyNumber(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                   TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                   IntToStr(TNFSeEnviarLoteRps(Self).FNotasFiscais.Count),
                                                   vNotas,
                                                   FTagI, FTagF, FProvedor);
 end;

 if FDadosMsg <> ''
  then begin
   if FConfiguracoes.WebServices.Salvar
    then FConfiguracoes.Geral.Save('-xxx1.xml', FDadosMsg);

   if FConfiguracoes.Certificados.AssinaLote
    then FDadosMsg := TNFSeEnviarLoteRPS(Self).FNotasFiscais.AssinarLoteRps(TNFSeEnviarLoteRps(Self).NumeroLote, FDadosMSg);

   if FConfiguracoes.WebServices.Salvar
    then FConfiguracoes.Geral.Save('-xxx2.xml', FDadosMsg);

   if FProvedorClass.GetValidarLote
    then begin
     if not(NotaUtil.Valida(FDadosMsg, FMsg,
                            FConfiguracoes.Geral.PathSchemas,
                            FConfiguracoes.WebServices.URL,
                            FConfiguracoes.WebServices.ServicoEnviar,
                            FConfiguracoes.WebServices.Prefixo4))
      then raise Exception.Create('Falha na validação do Lote ' +
                     TNFSeEnviarLoteRps(Self).NumeroLote + sLineBreak + FMsg);
    end;
  end
  else raise Exception.Create('A funcionalidade [Enviar Lote] não foi disponibilizada pelo provedor: ' + FxProvedor);
end;

procedure TWebServicesBase.DoNFSeConsultarSituacaoLoteRPS;
var
 URISig, URIRef, Separador : String;
begin
 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoConSit <> ''
  then begin
   if (FProvedor = proEquiplano)
    then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="http://www.equiplano.com.br/esnfs" ' +
                          'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                          'xsi:schemaLocation="http://www.equiplano.com.br/enfs esConsultarSituacaoLoteRpsEnvio_v01.xsd"'
   else begin
     if RightStr(FHTTP_AG, 1) = '/'
      then begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoConSit + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoConSit + '"';
      end
      else begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
      end;
   end;
  end
  else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">'
    else FNameSpaceDad := FNameSpaceDad +
                        ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 URISig := '';
 URIRef := '';

 URISig := FProvedorClass.GetURI(URISig);
 URIRef := URISig;

 FTagI := FProvedorClass.Gera_TagI(acConsSit, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URISig);

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);

 FTagF := FProvedorClass.Gera_TagF(acConsSit, Prefixo3);

 if FProvedorClass.GetAssinarXML(acConsSit)
  then begin
   case FProvedor of
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgConsSitLoteEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).FCNPJ),
                                                               TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).NumeroLote,
                                                               '', '');
    proInfisc: FDadosMsg := TNFSeG.Gera_DadosMsgConsSitLoteInfisc(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).FCNPJ),
                                                               TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).NumeroLote,
                                                               '', '');
    else FDadosMsg := TNFSeG.Gera_DadosMsgConsSitLote(Prefixo3, Prefixo4,
                                                      NameSpaceDad, FVersaoXML,
                                                      TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                      OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).Cnpj),
                                                      TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                      '', '', FProvedor);
   end;

   if FDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgConsSitLoteEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).FCNPJ),
                                                               TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).NumeroLote,
                                                               FTagI, FTagF);

    proEL: FDadosMsg := TNFSeG.Gera_DadosMsgConsSitLoteEL(FConfiguracoes.WebServices.CodigoMunicipio,
                                                          OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).FCNPJ),
                                                          TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                          TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                          TNFSeConsultarSituacaoLoteRPS(Self).NumeroLote,
                                                          FTagI, FTagF);

    else FDadosMsg := TNFSeG.Gera_DadosMsgConsSitLote(Prefixo3, Prefixo4,
                                                      NameSpaceDad, FVersaoXML,
                                                      TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                      OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).Cnpj),
                                                      TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                      FTagI, FTagF, FProvedor);
   end;
  end;

  if FDadosMsg = '' then
   raise Exception.Create('A funcionalidade [Consultar Situação do Lote] não foi disponibilizada pelo provedor: ' + FxProvedor);
end;

procedure TWebServicesBase.DoNFSeConsultarLoteRPS;
var
 URISig, URIRef, Separador : String;
begin
 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoConLot <> ''
  then begin
   if (FProvedor = proIssDSF)
    then FNameSpaceDad :=  'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '"'
                    + ' xmlns:tipos="http://localhost:8080/WsNFe2/tp"'
                    + ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
                    + ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote  http://localhost:8080/WsNFe2/xsd/ReqConsultaLote.xsd"'
    else if (FProvedor = proEquiplano)
     then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="http://www.equiplano.com.br/esnfs" ' +
                           'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                           'xsi:schemaLocation="http://www.equiplano.com.br/enfs esConsultarLoteRpsEnvio_v01.xsd"'
   else begin
     if RightStr(FHTTP_AG, 1) = '/'
      then begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoConLot + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoConLot + '"';
      end
      else begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
      end;
    end;
  end
  else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">'
    else FNameSpaceDad := FNameSpaceDad +
                        ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 URISig := '';
 URIRef := '';

 URISig := FProvedorClass.GetURI(URISig);
 URIRef := URISig;

 FTagI := FProvedorClass.Gera_TagI(acConsLote, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URISig);

 if (TNFSeConsultarLoteRPS(Self).FCNPJ = '') then
    TNFSeConsultarLoteRPS(Self).FCNPJ:=OnlyNumber(TNFSeConsultarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj);
 if (TNFSeConsultarLoteRPS(Self).FIM = '') then
    TNFSeConsultarLoteRPS(Self).FIM:=TNFSeConsultarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
 if (TNFSeConsultarLoteRPS(Self).FRazaoSocial = '') and (FProvedor = proTecnos) then
    TNFSeConsultarLoteRPS(Self).FRazaoSocial:=TNFSeConsultarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial;

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);

 FTagF := FProvedorClass.Gera_TagF(acConsLote, Prefixo3);

 if FProvedorClass.GetAssinarXML(acConsLote)
  then begin
   case FProvedor of
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgConsLoteEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                            OnlyNumber(TNFSeConsultarLoteRPS(Self).FCNPJ),
                                                            TNFSeConsultarLoteRPS(Self).IM,
                                                            TNFSeConsultarLoteRPS(Self).Protocolo,
                                                            TNFSeConsultarLoteRPS(Self).NumeroLote,
                                                            '', '');
    else FDadosMsg := TNFSeG.Gera_DadosMsgConsLote(Prefixo3, Prefixo4,
                                                   NameSpaceDad, FVersaoXML,
                                                   TNFSeConsultarLoteRPS(Self).Protocolo,
                                                   TNFSeConsultarLoteRPS(Self).FCNPJ,
                                                   TNFSeConsultarLoteRPS(Self).FIM,
                                                   TNFSeConsultarLoteRPS(Self).FSenha,
                                                   TNFSeConsultarLoteRPS(Self).FFraseSecreta,
                                                   '', '', FProvedor,
                                                   TNFSeConsultarLoteRPS(Self).FRazaoSocial);
   end;
   if FDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FDadosMsg := TNFSeG.Gera_DadosMsgConsLoteDSF(Prefixo3, Prefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      CodCidadeToCodSiafi(FConfiguracoes.WebServices.CodigoMunicipio),
                                                      TNFSeConsultarLoteRPS(Self).Cnpj,
                                                      TNFSeConsultarLoteRPS(Self).Protocolo,
                                                      FTagI, FTagF);
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgConsLoteEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                            OnlyNumber(TNFSeConsultarLoteRPS(Self).FCNPJ),
                                                            TNFSeConsultarLoteRPS(Self).IM,
                                                            TNFSeConsultarLoteRPS(Self).Protocolo,
                                                            TNFSeConsultarLoteRPS(Self).NumeroLote,
                                                            FTagI, FTagF);
    proEL: FDadosMsg := TNFSeG.Gera_DadosMsgConsLoteEL(FConfiguracoes.WebServices.CodigoMunicipio,
                                                       OnlyNumber(TNFSeConsultarLoteRPS(Self).FCNPJ),
                                                       TNFSeConsultarLoteRPS(Self).IM,
                                                       TNFSeConsultarLoteRPS(Self).Protocolo,
                                                       TNFSeConsultarLoteRPS(Self).NumeroLote,
                                                       FTagI, FTagF);
    else FDadosMsg := TNFSeG.Gera_DadosMsgConsLote(Prefixo3, Prefixo4,
                                                   NameSpaceDad, FVersaoXML,
                                                   TNFSeConsultarLoteRPS(Self).Protocolo,
                                                   TNFSeConsultarLoteRPS(Self).FCNPJ,
                                                   TNFSeConsultarLoteRPS(Self).FIM,
                                                   TNFSeConsultarLoteRPS(Self).FSenha,
                                                   TNFSeConsultarLoteRPS(Self).FFraseSecreta,
                                                   FTagI, FTagF, FProvedor,
                                                   TNFSeConsultarLoteRPS(Self).FRazaoSocial);
   end;
  end;

  if FDadosMsg = '' then
   raise Exception.Create('A funcionalidade [Consultar Lote] não foi disponibilizada pelo provedor: ' + FxProvedor);
end;

procedure TWebServicesBase.DoNFSeConsultarNFSeporRPS;
var
 i         : Integer;
 vNotas    : WideString;
 URISig, URIRef, Separador : String;
  Gerador: TGerador;
begin
 vNotas := '';

 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoConRps <> ''
  then begin
   if (FProvedor = proIssDSF)
    then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '" ' +
                          'xmlns:tipos="http://localhost:8080/WsNFe2/tp" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                          'xsi:schemaLocation="' + FURLNS1 + ' http://localhost:8080/WsNFe2/xsd/' + FServicoConRps + '"'
   else if (FProvedor = proEquiplano)
    then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="http://www.equiplano.com.br/esnfs" ' +
                          'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                          'xsi:schemaLocation="http://www.equiplano.com.br/enfs esConsultarNfsePorRpsEnvio_v01.xsd"'
   else begin
     if RightStr(FHTTP_AG, 1) = '/'
      then begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoConRps + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoConRps + '"';
      end
      else begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
      end;
    end;
  end
  else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">'
    else FNameSpaceDad := FNameSpaceDad +
                        ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 URISig := '';
 URIRef := '';

 URISig := FProvedorClass.GetURI(URISig);
 URIRef := URISig;

 FTagI := FProvedorClass.Gera_TagI(acConsNFSeRps, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URISig);

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);

 FTagF := FProvedorClass.Gera_TagF(acConsNFSeRps, Prefixo3);

 if (FProvedor = proIssDSF ) then
 begin
   Gerador := TGerador.Create;
   Gerador.ArquivoFormatoXML := '';

   Gerador.wGrupoNFSe('NotaConsulta');
   for i := 0 to TNFSeConsultarNfseRPS(Self).FNotasFiscais.Count-1 do
   begin
     with TNFSeConsultarNfseRPS(Self).FNotasFiscais.Items[I] do
       if NFSe.Numero <> '' then
       begin
         Gerador.wGrupoNFSe('Nota Id="nota:' + NFSe.Numero + '"');
         Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 11,  1, TNFSeConsultarNfseRPS(Self).InscricaoMunicipal, ''); //NFSe.Prestador.InscricaoMunicipal
         Gerador.wCampoNFSe(tcStr, '#1', 'NumeroNota', 01, 12, 1, OnlyNumber(NFSe.Numero), '');
         Gerador.wCampoNFSe(tcStr, '', 'CodigoVerificacao', 01, 255,  1, NFSe.CodigoVerificacao, '');
         Gerador.wGrupoNFSe('/Nota');
      end;
   end;
   Gerador.wGrupoNFSe('/NotaConsulta');

//   VERIFICAR UMA FORMA DE PREENCHER, APENAS SE EXISTIR RPS PARA CONSULTA!
//   Gerador.wGrupoNFSe('RPSConsulta');
//   for i := 0 to TNFSeConsultarNfseRPS(Self).FNotasFiscais.Count-1 do
//   begin
//     with TNFSeConsultarNfseRPS(Self).FNotasFiscais.Items[I] do
//       if (NFSe.Numero = '') and (NFSe.IdentificacaoRps.Numero <> '') then
//       begin
//         Gerador.wGrupoNFSe('RPS Id="rps:' + NFSe.Numero + '"');
//         Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 11,  1, NFSe.Prestador.InscricaoMunicipal, '');
//         Gerador.wCampoNFSe(tcStr, '#1', 'NumeroRPS', 01, 12, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), '');
//         Gerador.wCampoNFSe(tcStr, '', 'SeriePrestacao', 01, 2,  1, NFSe.IdentificacaoRps.Serie, '');
//         Gerador.wGrupoNFSe('/RPS');
//      end;
//   end;
//   Gerador.wGrupoNFSe('/RPSConsulta');

   vNotas := Gerador.ArquivoFormatoXML;
   Gerador.Free;
 end;

 if FProvedorClass.GetAssinarXML(acConsNFSeRps)
  then begin
   case FProvedor of
    proIssDSF: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSDSF(Prefixo3, Prefixo4,
                                                         NameSpaceDad, VersaoXML,
                                                         CodCidadeToCodSiafi(FConfiguracoes.WebServices.CodigoMunicipio),
                                                         OnlyNumber(TNFSeConsultarNfseRPS(Self).Cnpj),
                                                         LowerCase(booltostr(TNFSeConsultarNfseRPS(Self).FNotasFiscais.Transacao, True)),
                                                         TNFSeConsultarNfseRPS(Self).FNotasFiscais.NumeroLote,
                                                         vNotas,
                                                         '', '');
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               TNFSeConsultarNfseRPS(Self).Numero,
                                                               OnlyNumber(TNFSeConsultarNfseRPS(Self).FCnpj),
                                                               TNFSeConsultarNfseRPS(Self).InscricaoMunicipal,
                                                               '', '');
    else FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPS(Prefixo3, Prefixo4,
                                                      NameSpaceDad, FVersaoXML,
                                                      TNFSeConsultarNfseRPS(Self).Numero,
                                                      TNFSeConsultarNfseRPS(Self).Serie,
                                                      TNFSeConsultarNfseRPS(Self).Tipo,
                                                      OnlyNumber(TNFSeConsultarNfseRPS(Self).Cnpj),
                                                      TNFSeConsultarNfseRPS(Self).InscricaoMunicipal,
                                                      TNFSeConsultarNfseRPS(Self).Senha,
                                                      TNFSeConsultarNfseRPS(Self).FraseSecreta,
                                                      '', '', FProvedor,
                                                      TNFSeConsultarNfseRPS(Self).RazaoSocial);
   end;
   if FDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSDSF(Prefixo3, Prefixo4,
                                                         NameSpaceDad, VersaoXML,
                                                         CodCidadeToCodSiafi( strtointDef(TNFSeConsultarNfseRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                         TNFSeConsultarNfseRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj,
                                                         LowerCase(booltostr(TNFSeConsultarNfseRPS(Self).FNotasFiscais.Transacao, True)),
                                                         TNFSeConsultarNfseRPS(Self).FNotasFiscais.NumeroLote,
                                                         vNotas,
                                                         FTagI, FTagF);
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               TNFSeConsultarNfseRPS(Self).Numero,
                                                               OnlyNumber(TNFSeConsultarNfseRPS(Self).FCnpj),
                                                               TNFSeConsultarNfseRPS(Self).InscricaoMunicipal,
                                                               FTagI, FTagF);
    proEL: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSEL(FConfiguracoes.WebServices.CodigoMunicipio,
                                                          TNFSeConsultarNfseRPS(Self).Numero,
                                                          OnlyNumber(TNFSeConsultarNfseRPS(Self).FCnpj),
                                                          TNFSeConsultarNfseRPS(Self).InscricaoMunicipal,
                                                          FTagI, FTagF);
    else FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPS(Prefixo3, Prefixo4,
                                                      NameSpaceDad, FVersaoXML,
                                                      TNFSeConsultarNfseRPS(Self).Numero,
                                                      TNFSeConsultarNfseRPS(Self).Serie,
                                                      TNFSeConsultarNfseRPS(Self).Tipo,
                                                      OnlyNumber(TNFSeConsultarNfseRPS(Self).Cnpj),
                                                      TNFSeConsultarNfseRPS(Self).InscricaoMunicipal,
                                                      TNFSeConsultarNfseRPS(Self).Senha,
                                                      TNFSeConsultarNfseRPS(Self).FraseSecreta,
                                                      FTagI, FTagF, FProvedor,
                                                      TNFSeConsultarNfseRPS(Self).RazaoSocial);
   end;
  end;

  if FDadosMsg = '' then
   raise Exception.Create('A funcionalidade [Consultar NFSe por RPS] não foi disponibilizada pelo provedor: ' + FxProvedor);
end;

procedure TWebServicesBase.DoNFSeConsultarNFSe;
var
 URISig, URIRef, Separador : String;
begin
 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoConNfse <> ''
  then begin
   if (FProvedor = proIssDSF)
    then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '" ' {+
                          'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="' + FURLNS1 + ' ' +
                          FHTTP_AG + FServicoConNfse + '"'}
    else begin
     if RightStr(FHTTP_AG, 1) = '/'
      then begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoConNfse + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoConNfse + '"';
      end
      else begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
      end;
    end;
  end
  else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">'
    else FNameSpaceDad := FNameSpaceDad +
                        ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 URISig := '';
 URIRef := '';

 URISig := FProvedorClass.GetURI(URISig);
 URIRef := URISig;

 FTagI := FProvedorClass.Gera_TagI(acConsNFSe, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URISig);

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);

 FTagF := FProvedorClass.Gera_TagF(acConsNFSe, Prefixo3);

 if FProvedorClass.GetAssinarXML(acConsNFSe)
  then begin
   case FProvedor of
    proIssDSF: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeDSF(Prefixo3, Prefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      CodCidadeToCodSiafi(strtointDef(TNFSeConsultarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                      TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                      TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                      TNFSeConsultarNfse(Self).DataInicial,
                                                      TNFSeConsultarNfse(Self).DataFinal,
                                                      '', '');
    proInfisc: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeInfisc(Prefixo3, Prefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      // Alterado Por Moro em 18/02/2015
                                                      CodCidadeToCodSiafi(strtointDef(IntToStr(TNFSeConsultarNfse(Self).FConfiguracoes.WebServices.CodigoMunicipio), 0)),
                                                      //CodCidadeToCodSiafi(strtointDef(TNFSeConsultarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                      TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                      TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                      TNFSeConsultarNfse(Self).FSerie,
                                                      TNFSeConsultarNfse(Self).DataInicial,
                                                      TNFSeConsultarNfse(Self).DataFinal,
                                                      '', '');
    else FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSe(Prefixo3, Prefixo4,
                                                   NameSpaceDad, FVersaoXML,
                                                   OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                   TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                   TNFSeConsultarNfse(Self).DataInicial,
                                                   TNFSeConsultarNfse(Self).DataFinal,
                                                   '', '',
                                                   TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                   TNFSeConsultarNfse(Self).Senha,
                                                   TNFSeConsultarNfse(Self).FraseSecreta,
                                                   FProvedor,
                                                   TNFSeConsultarNfse(Self).FPagina,
                                                   TNFSeConsultarNfse(Self).FCNPJTomador,
                                                   TNFSeConsultarNfse(Self).FIMTomador,
                                                   TNFSeConsultarNfse(Self).FNomeInter,
                                                   TNFSeConsultarNfse(Self).FCNPJInter,
                                                   TNFSeConsultarNfse(Self).FIMInter);
   end;
   if FDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeDSF(Prefixo3, Prefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      CodCidadeToCodSiafi(strtointDef(TNFSeConsultarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                      TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                      TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                      TNFSeConsultarNfse(Self).DataInicial,
                                                      TNFSeConsultarNfse(Self).DataFinal,
                                                      FTagI, FTagF);
    proInfisc: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeInfisc(Prefixo3, Prefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      CodCidadeToCodSiafi(strtointDef(TNFSeConsultarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                      TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                      TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                      TNFSeConsultarNfse(Self).FSerie,
                                                      TNFSeConsultarNfse(Self).DataInicial,
                                                      TNFSeConsultarNfse(Self).DataFinal,
                                                      FTagI, FTagF);
    proEL: FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeEL(Prefixo3, Prefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                      OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                      TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                      OnlyNumber(TNFSeConsultarNfse(Self).FCNPJTomador),
                                                      OnlyNumber(TNFSeConsultarNfse(Self).FCNPJInter),
                                                      TNFSeConsultarNfse(Self).DataInicial,
                                                      TNFSeConsultarNfse(Self).DataFinal,
                                                      FTagI, FTagF);
    else FDadosMsg := TNFSeG.Gera_DadosMsgConsNFSe(Prefixo3, Prefixo4,
                                                   NameSpaceDad, FVersaoXML,
                                                   OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                   TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                   TNFSeConsultarNfse(Self).DataInicial,
                                                   TNFSeConsultarNfse(Self).DataFinal,
                                                   FTagI, FTagF,
                                                   TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                   TNFSeConsultarNfse(Self).Senha,
                                                   TNFSeConsultarNfse(Self).FraseSecreta,
                                                   FProvedor,
                                                   TNFSeConsultarNfse(Self).FPagina,
                                                   TNFSeConsultarNfse(Self).FCNPJTomador,
                                                   TNFSeConsultarNfse(Self).FIMTomador,
                                                   TNFSeConsultarNfse(Self).FNomeInter,
                                                   TNFSeConsultarNfse(Self).FCNPJInter,
                                                   TNFSeConsultarNfse(Self).FIMInter);
   end;
  end;

  if FDadosMsg = '' then
   raise Exception.Create('A funcionalidade [Consultar NFSe] não foi disponibilizada pelo provedor: ' + FxProvedor);
end;

//Metodo usado apenas no provedor IssDSF
procedure TWebServicesBase.DoNFSeConsultarSequencialRPS;
var
 vNotas    : WideString;
 URISig, URIRef, Separador : String;
begin
 if FProvedor <> proIssDSF then exit;

 vNotas := '';
 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> '' then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end else FNameSpaceCab := '>';

 if FServicoConNfse <> '' then
    FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '" ' {+
                     'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="' + FURLNS1 +
                     ' ' + FHTTP_AG + FServicoCancelar + '"'}
 else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">'
    else FNameSpaceDad := FNameSpaceDad +
                        ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 URISig := '';
 URIRef := '';

 URISig := FProvedorClass.GetURI(URISig);
 URIRef := URISig;

 FTagI := FProvedorClass.Gera_TagI(acConsSecRps {acConsNFSe}, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URISig);

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);

 FTagF := FProvedorClass.Gera_TagF(acConsSecRps {acConsNFSe}, Prefixo3);

 if FProvedor = proEL then
   FDadosMsg := TNFSeG.Gera_DadosMsgConsSeqRPSEL(FTagI, FTagF, VersaoXML,
                                                 CodCidadeToCodSiafi(StrToIntDef(TNFSeConsultarSequencialRPS(Self).FCidade, 0)),
                                                 TNFSeConsultarSequencialRPS(Self).FInscricaoMunicipal,
                                                 TNFSeConsultarSequencialRPS(Self).FCnpj,
                                                 TNFSeConsultarSequencialRPS(Self).FSeriePrestacao)
 else
   FDadosMsg := TNFSeG.Gera_DadosMsgConsSeqRPSDSF(FTagI, FTagF, VersaoXML,
                                                  CodCidadeToCodSiafi(StrToIntDef(TNFSeConsultarSequencialRPS(Self).FCidade, 0)),
                                                  TNFSeConsultarSequencialRPS(Self).FInscricaoMunicipal,
                                                  TNFSeConsultarSequencialRPS(Self).FCnpj,
                                                  TNFSeConsultarSequencialRPS(Self).FSeriePrestacao);

 if FProvedorClass.GetAssinarXML(acConsSecRps)
  then begin
  {$IFDEF ACBrNFSeOpenSSL}
   if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                   FConfiguracoes.Certificados.Certificado,
                   FConfiguracoes.Certificados.Senha,
                   FvAssinada, FMsg, FProvedor))
    then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
    else FDadosMsg := FvAssinada;
  {$ELSE}
   if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                   FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
    then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
    else FDadosMsg := FvAssinada;
  {$ENDIF}
  end;
end;

procedure TWebServicesBase.DoNFSeCancelarNFSe;
var
 i         : Integer;
 vNotas    : WideString;
 URISig, URIRef, Separador : String;
 Gerador: TGerador;
begin
 vNotas := '';

 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoCancelar <> ''
  then begin
   if (FProvedor = proIssDSF)
    then FNameSpaceDad :=  'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '"'
                    + ' xmlns:tipos="http://localhost:8080/WsNFe2/tp"'
                    + ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
                    + ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote  http://localhost:8080/WsNFe2/xsd/ReqCancelamentoNFSe.xsd"'

   else if (FProvedor = proInfisc)
    then FNameSpaceDad :=  'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '"'
                    + ' xmlns:tipos="http://localhost:8080/WsNFe2/tp"'
                    + ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
                    + ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote  http://localhost:8080/WsNFe2/xsd/ReqCancelamentoNFSe.xsd"'

   else if (FProvedor = proEquiplano)
    then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="http://www.equiplano.com.br/esnfs" ' +
                          'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                          'xsi:schemaLocation="http://www.equiplano.com.br/enfs esCancelarNfseEnvio_v01.xsd"'
   else begin
     if RightStr(FHTTP_AG, 1) = '/'
      then begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoCancelar + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoCancelar + '"';
      end
      else begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
      end;
    end;
  end
  else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then begin
     if (FProvedor = proGINFES)
      then begin
       FVersaoLayOut := '2';
       FVersaoDados  := '2';
       FDefTipos     := 'tipos_v02.xsd';
      end;
     FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">';
    end
    else FNameSpaceDad := FNameSpaceDad +
                        ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 if (TNFSeCancelarNfse(Self).FNumeroNFSe = '') then
   TNFSeCancelarNfse(Self).FNumeroNFSe := TNFSeCancelarNfse(Self).FNotasFiscais.Items[0].NFSe.Numero;

 if (TNFSeCancelarNfse(Self).FCNPJ = '') then
 begin
   if (FProvedor = proDigifred) or (FProvedor = pro4R) then
    TNFSeCancelarNfse(Self).FCNPJ := OnlyNumber(TNFSeCancelarNfse(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj)
   else
    TNFSeCancelarNfse(Self).FCNPJ := OnlyNumber(TNFSeCancelarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj);
 end;

 if (TNFSeCancelarNfse(Self).FIM = '') then
 begin
  if (FProvedor = proDigifred) or (FProvedor = pro4R) then
    TNFSeCancelarNfse(Self).FIM := TNFSeCancelarNfse(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal
  else
    TNFSeCancelarNfse(Self).FIM := TNFSeCancelarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
 end;

 if (TNFSeCancelarNfse(Self).MotivoCancelamento = '') then
   TNFSeCancelarNfse(Self).MotivoCancelamento:= TNFSeCancelarNfse(Self).FNotasFiscais.Items[0].NFSe.MotivoCancelamento;

 if (TNFSeCancelarNfse(Self).FCodigoMunicipio = '') then
 begin
  if (FProvedor = proISSNet) and (FConfiguracoes.WebServices.AmbienteCodigo = 2) then
   TNFSeCancelarNfse(Self).FCodigoMunicipio := '999'
  else
  begin
    if FProvedor = proFiorilli then
      TNFSeCancelarNfse(Self).FCodigoMunicipio := TNFSeCancelarNfse(Self).FNotasFiscais.Items[0].NFSe.Servico.CodigoMunicipio
    else
      TNFSeCancelarNfse(Self).FCodigoMunicipio := TNFSeCancelarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio;
  end;
 end;

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 case FProvedor of
  // Alterado por Augusto Fontana - 28/04/2014
  proEquiplano, proPublica: URISig:= '';
  proDigifred:  URISig := 'CANC' + TNFSeCancelarNfse(Self).FNumeroNFSe;
  proSaatri: URISig := 'Cancelamento_' + TNFSeCancelarNfse(Self).FCnpj;
  proIssIntel,
  proISSNet: begin
              URISig := '';
              URIRef := 'http://www.w3.org/TR/2000/REC-xhtml1-20000126/';
             end;
  proTecnos: URISig := '2' + TNFSeCancelarNfse(Self).FCnpj + IntToStrZero(StrToInt(TNFSeCancelarNfse(Self).FNumeroNFSe), 16);
// Alterado por Nilton Olher - 12/02/2015
  proGovDigital: URISig := TNFSeCancelarNfse(Self).FNumeroNFSe;
 else            URISig := 'pedidoCancelamento_' + TNFSeCancelarNfse(Self).FCnpj + TNFSeCancelarNfse(Self).FIM + TNFSeCancelarNfse(Self).FNumeroNFSe;
 end;

 if FProvedor <> proISSNet
  then begin
   URISig := FProvedorClass.GetURI(URISig);
   URIRef := URISig;
  end;

 FTagI := FProvedorClass.Gera_TagI(acCancelar, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URISig);

 FTagF := FProvedorClass.Gera_TagF(acCancelar, Prefixo3);

 if (FProvedor = proIssDSF ) then
 begin
   Gerador := TGerador.Create;
   Gerador.ArquivoFormatoXML := '';

   for i := 0 to TNFSeCancelarNfse(Self).FNotasFiscais.Count-1 do
   begin
     with TNFSeCancelarNfse(Self).FNotasFiscais.Items[I] do
     begin
       Gerador.wGrupoNFSe('Nota Id="nota:' + NFSe.Numero + '"');
       Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 11,  1, TNFSeCancelarNfse(Self).FIM, '');
       Gerador.wCampoNFSe(tcStr, '#1', 'NumeroNota', 01, 12, 1, OnlyNumber(NFSe.Numero), '');
       Gerador.wCampoNFSe(tcStr, '', 'CodigoVerificacao', 01, 255,  1, NFSe.CodigoVerificacao, '');
       Gerador.wCampoNFSe(tcStr, '', 'MotivoCancelamento', 01, 80, 1, NFSe.MotivoCancelamento, '');
       Gerador.wGrupoNFSe('/Nota');
     end;
   end;

   vNotas := Gerador.ArquivoFormatoXML;

   Gerador.Free;
 end;

 if (FProvedor = proInfisc ) then
 begin
   Gerador := TGerador.Create;
   try
     Gerador.ArquivoFormatoXML := '';
     for i := 0 to TNFSeCancelarNfse(Self).FNotasFiscais.Count-1 do
     begin
       with TNFSeCancelarNfse(Self).FNotasFiscais.Items[I] do
       begin
         Gerador.wCampoNFSe(tcStr, '', 'chvAcessoNFS-e', 1, 39, 1, NFSe.ChaveNFSe, '');
         Gerador.wCampoNFSe(tcStr, '', 'motivo', 1, 39, 1, NFSe.MotivoCancelamento, '');
       end;
     end;
     vNotas := Gerador.ArquivoFormatoXML;
   finally
     Gerador.Free;
   end;
 end;

 if FProvedorClass.GetAssinarXML(acCancelar)
  then begin
   case FProvedor of
    proIssDSF: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeDSF(Prefixo3, Prefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeCancelarNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeCancelarNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeCancelarNfse(Self).FCodigoMunicipio)),
                                                          TNFSeCancelarNfse(Self).FNotasFiscais.NumeroLote,
                                                          vNotas,
                                                          '', '');
    proInfisc: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeInfisc(Prefixo3, Prefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeCancelarNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeCancelarNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeCancelarNfse(Self).FCodigoMunicipio)),
                                                          TNFSeCancelarNfse(Self).FNotasFiscais.NumeroLote,
                                                          vNotas,
                                                          '', '');
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEquiplano(StrToInt(TNFSeCancelarNfse(Self).FCodigoMunicipio),
                                                                OnlyNumber(TNFSeCancelarNfse(Self).FCnpj),
                                                                TNFSeCancelarNfse(Self).FIM,
                                                                TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                                TNFSeCancelarNfse(Self).FMotivoCancelamento,
                                                                '', '');
    else FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSe(Prefixo4,
                                                       NameSpaceDad,
                                                       TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                       TNFSeCancelarNfse(Self).FCnpj,
                                                       TNFSeCancelarNfse(Self).FIM,
                                                       TNFSeCancelarNfse(Self).FCodigoMunicipio,
                                                       TNFSeCancelarNfse(Self).FCodigoCancelamento,
                                                       '', '',
                                                       FProvedor,
                                                       TNFSeCancelarNfse(Self).FMotivoCancelamento);
   end;
   if FDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     URIRef := '';
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeDSF(Prefixo3, Prefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeCancelarNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeCancelarNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeCancelarNfse(Self).FCodigoMunicipio)),
                                                          TNFSeCancelarNfse(Self).FNotasFiscais.NumeroLote,
                                                          vNotas,
                                                          FTagI, FTagF);
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEquiplano(StrToInt(TNFSeCancelarNfse(Self).FCodigoMunicipio),
                                                                OnlyNumber(TNFSeCancelarNfse(Self).FCnpj),
                                                                TNFSeCancelarNfse(Self).FIM,
                                                                TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                                TNFSeCancelarNfse(Self).FMotivoCancelamento,
                                                                FTagI, FTagF);
    proEL: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEL(StrToInt(TNFSeCancelarNfse(Self).FCodigoMunicipio),
                                                           OnlyNumber(TNFSeCancelarNfse(Self).FCnpj),
                                                           TNFSeCancelarNfse(Self).FIM,
                                                           TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                           TNFSeCancelarNfse(Self).FMotivoCancelamento,
                                                           FTagI, FTagF);
    proFreire: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeFreire(Prefixo4,
                                                             NameSpaceDad,
                                                             TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                             TNFSeCancelarNfse(Self).FCnpj,
                                                             TNFSeCancelarNfse(Self).FIM,
                                                             TNFSeCancelarNfse(Self).FCodigoMunicipio,
                                                             TNFSeCancelarNfse(Self).FCodigoCancelamento,
                                                             TNFSeCancelarNfse(Self).FMotivoCancelamento,
                                                             FTagI, FTagF);
    else FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSe(Prefixo4,
                                                       NameSpaceDad,
                                                       TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                       TNFSeCancelarNfse(Self).FCnpj,
                                                       TNFSeCancelarNfse(Self).FIM,
                                                       TNFSeCancelarNfse(Self).FCodigoMunicipio,
                                                       TNFSeCancelarNfse(Self).FCodigoCancelamento,
                                                       FTagI, FTagF,
                                                       FProvedor,
                                                       TNFSeCancelarNfse(Self).FMotivoCancelamento);
   end;
  end;

  if DadosMsg = '' then
   raise Exception.Create('A funcionalidade [Cancelar NFSe] não foi disponibilizada pelo provedor: ' + FxProvedor);
end;

procedure TWebServicesBase.DoNFSeGerarNFSe;
var
 i         : Integer;
 vNotas    : WideString;
 URI       : String;
 Separador : String;
begin
 vNotas := '';

 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoGerar <> ''
  then begin
   if RightStr(FHTTP_AG, 1) = '/'
    then begin
     if Prefixo3 <> ''
      then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoGerar + '"'
      else FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoGerar + '"';
    end
    else begin
     if Prefixo3 <> ''
      then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
      else FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
    end;
  end
  else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">'
    else FNameSpaceDad := FNameSpaceDad + ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 if FConfiguracoes.Certificados.AssinaRPS or FProvedorClass.GetAssinarXML(acGerar)
  then begin
   for i := 0 to TNFSeGerarNFSe(Self).FNotasFiscais.Count-1 do
    begin
     case FProvedor of
      profintelISS,
      proSaatri,
      proSisPMJP,
      proISSDigital,
      proISSe,
      pro4R,
      proFiorilli,
      proProdata,
      proVitoria,
      proPVH,
      proAgili,
      proCoplan,
      proVirtual,
      proLink3,
      proGovDigital,
      proGoiania: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      proFreire: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico Id ="'+ TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].NFSe.InfID.ID +'"' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      proDigifred: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps ' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'Rps', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      proSystemPro: vNotas := vNotas + TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XML_Rps_Ass;

      else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
     end;
    end;
  end
  else begin
   for i := 0 to TNFSeGerarNFSe(Self).FNotasFiscais.Count-1 do
    begin
      case FProvedor of
           profintelISS,
           proSaatri,
           proSisPMJP,
           proGoiania,
           proISSDigital,
           proISSe,
           proSystemPro,
           pro4R,
           proFiorilli,
           proProdata,
           proVitoria,
           proPVH,
           proAgili,
           proCoplan,
           proLink3,
           proGovDigital,
           proVirtual: vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                               '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                              '</' + Prefixo4 + 'Rps>';

           proFreire : vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico Id="' + TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].NFSe.InfID.ID + '" '+
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                               '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                              '</' + Prefixo4 + 'Rps>';

           proEgoverneISS: vNotas := vNotas + '<' + Prefixo4 + 'NotaFiscal>' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'NotaFiscal>', '</' + Prefixo4 + 'NotaFiscal>') +
                              '</' + Prefixo4 + 'NotaFiscal>';

           else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
      end;
    end;
  end;

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 URI := '';

 URI := FProvedorClass.GetURI(URI);

 FTagI := FProvedorClass.Gera_TagI(acGerar, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URI);

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);

 FTagF := FProvedorClass.Gera_TagF(acGerar, Prefixo3);

 FDadosMsg := TNFSeG.Gera_DadosMsgGerarNFSe(Prefixo3, Prefixo4,
                                            FConfiguracoes.WebServices.Identificador,
                                            NameSpaceDad, VersaoDados, FVersaoXML,
                                            IntToStr(TNFSeGerarNFSe(Self).NumeroRps),
                                            OnlyNumber(TNFSeGerarNFSe(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                            TNFSeGerarNFSe(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                            IntToStr(TNFSeGerarNFSe(Self).FNotasFiscais.Count),
                                            vNotas,
                                            FTagI, FTagF, FProvedor);

 if FDadosMsg <> ''
  then begin
   if FConfiguracoes.WebServices.Salvar
    then FConfiguracoes.Geral.Save('-xxx1.xml', FDadosMsg);

   if FConfiguracoes.WebServices.Salvar
    then FConfiguracoes.Geral.Save('-xxx2.xml', FDadosMsg);

   if FProvedorClass.GetValidarLote
    then begin
     if not(NotaUtil.Valida(FDadosMsg, FMsg,
                            FConfiguracoes.Geral.PathSchemas,
                            FConfiguracoes.WebServices.URL,
                            FConfiguracoes.WebServices.ServicoGerar,
                            FConfiguracoes.WebServices.Prefixo4))
      then raise Exception.Create('Falha na validação do Lote ' +
                     IntToStr(TNFSeGerarNFSe(Self).NumeroRps) + sLineBreak + FMsg);
    end;
  end
  else raise Exception.Create('A funcionalidade [Gerar NFSe] não foi disponibilizada pelo provedor: ' + FxProvedor);
end;

procedure TWebServicesBase.DoNFSeLinkNFSe;
begin
 TNFSeLinkNFSe(Self).FLink := FProvedorClass.GetLinkNFSe(FConfiguracoes.WebServices.CodigoMunicipio,
                                      TNFSeLinkNFSe(Self).FNumeroNFSe,
                                      TNFSeLinkNFSe(Self).FCodVerif,
                                      TNFSeLinkNFSe(Self).FIM,
                                      FConfiguracoes.WebServices.AmbienteCodigo);
end;

procedure TWebServicesBase.DoNFSeGerarLote;
var
 i         : Integer;
 vNotas    : WideString;
 URI,
 Separador,
 PathSalvar: String;
 DataInicial, DataFinal : TDateTime;
begin
 vNotas := '';

 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoEnviar <> ''
  then begin
   if (FProvedor = proIssDSF)
    then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '" ' {+
                          'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="' + FURLNS1 + ' ' +
                          FHTTP_AG + FServicoEnviar + '"'}
   else if (FProvedor = proInfisc)
    then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '" ' {+
                          'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="' + FURLNS1 + ' ' +
                          FHTTP_AG + FServicoEnviar + '"'}
    else begin
      if (RightStr(FHTTP_AG, 1) = '/')
      then begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoEnviar + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoEnviar + '"';
      end
      else begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
      end;
    end;
  end
  else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">'
    else FNameSpaceDad := FNameSpaceDad + ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 if FConfiguracoes.Certificados.AssinaRPS
  then begin
   for i := 0 to TNFSeGerarLoteRPS(Self).FNotasFiscais.Count-1 do
    begin
     case FProvedor of
      profintelISS,
      proSaatri,
      proSisPMJP,
      proISSDigital,
      proISSe,
      proSystemPro,
      pro4R,
      proFiorilli,
      proProdata,
      proVitoria,
      proPVH,
      proAgili,
      proCoplan,
      proVirtual,
      proFreire,
      proLink3,
      proGovDigital,
      proGoiania: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               // ManutJonatan
//                              '<' + Prefixo4 + 'Rps Id="' +  TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].NFSe.IdentificacaoRps.Numero + '">' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      proMitra: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      proDigifred: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps ' +
                                 RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'Rps', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';
      proIssDSF : vNotas :=  vNotas + TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps;//.XML_Rps_Ass;

      proInfisc : vNotas :=  vNotas + TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps;//.XML_Rps_Ass;

      proTecnos: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';


      else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
     end;
    end;
  end
  else begin
   for i := 0 to TNFSeGerarLoteRPS(Self).FNotasFiscais.Count-1 do
    begin
     case FProvedor of
      profintelISS, proSaatri, proSisPMJP, proGoiania, proISSDigital, proISSe, proSystemPro,
      pro4R, proFiorilli, proProdata, proVitoria, proPVH, proAgili,
      proCoplan, proVirtual, proFreire, proLink3, proMitra, proGovDigital:
       vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                               '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                              '</' + Prefixo4 + 'Rps>';

      proIssDSF, proInfisc:
       vNotas :=  vNotas + TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps;

      proTecnos:
       vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                           '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico' +
                            RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                           '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico', '</tcDeclaracaoPrestacaoServico>') +
                          '</tcDeclaracaoPrestacaoServico>'+
                          '</' + Prefixo4 + 'Rps>';

      else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
     end;
    end;
  end;

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 URI := '';

 URI := FProvedorClass.GetURI(URI);

 FTagI := FProvedorClass.Gera_TagI(acRecepcionar, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URI);

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);
 FTagF := FProvedorClass.Gera_TagF(acRecepcionar, Prefixo3);

 case FProvedor of
  proIssDSF: begin
              DataInicial := TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.DataEmissao;
              DataFinal   := DataInicial;

              for i := 0 to TNFSeEnviarLoteRPS(Self).FNotasFiscais.Count-1 do
               begin
                if TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[i].NFSe.DataEmissao < dataInicial then
                  DataInicial := TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[i].NFSe.DataEmissao;
                if TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[i].NFSe.DataEmissao > dataFinal then
                  DataFinal := TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[i].NFSe.DataEmissao;
               end;

              FDadosMsg := TNFSeG.Gera_DadosMsgEnviarLoteDSF(Prefixo3, Prefixo4,
                                                      FConfiguracoes.WebServices.Identificador,
                                                      NameSpaceDad, FVersaoXML,
                                                      TNFSeGerarLoteRPS(Self).NumeroLote,
                                                      CodCidadeToCodSiafi( StrToIntDef(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                      TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                      TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial,
                                                      LowerCase(booltostr(TNFSeGerarLoteRPS(Self).FNotasFiscais.Transacao, True)),
                                                      IntToStr(TNFSeGerarLoteRPS(Self).FNotasFiscais.Count),
                                                      FormatFloat('0.00', TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Servico.Valores.ValorServicos),
                                                      FormatFloat('0.00', TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Servico.Valores.ValorDeducoes),
                                                      DataInicial, DataFinal,
                                                      vNotas,
                                                      FTagI, FTagF);
             end;  
  proInfisc: begin
              DataInicial := TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.DataEmissao;
              DataFinal   := DataInicial;
              FDadosMsg := TNFSeG.Gera_DadosMsgEnviarLoteInfisc(Prefixo3, Prefixo4,
                                                      FConfiguracoes.WebServices.Identificador,
                                                      NameSpaceDad, FVersaoXML,
                                                      TNFSeGerarLoteRPS(Self).NumeroLote,
                                                      CodCidadeToCodSiafi( StrToIntDef(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                      TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                      TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial,
                                                      LowerCase(booltostr(TNFSeGerarLoteRPS(Self).FNotasFiscais.Transacao, True)),
                                                      IntToStr(TNFSeGerarLoteRPS(Self).FNotasFiscais.Count),
                                                      FormatFloat('0.00', TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Servico.Valores.ValorServicos),
                                                      FormatFloat('0.00', TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Servico.Valores.ValorDeducoes),
                                                      DataInicial, DataFinal,
                                                      vNotas,
                                                      FTagI, FTagF);
             end;
  else FDadosMsg := TNFSeG.Gera_DadosMsgEnviarLote(Prefixo3, Prefixo4,
                                                   FConfiguracoes.WebServices.Identificador,
                                                   NameSpaceDad, VersaoDados, FVersaoXML,
                                                   TNFSeGerarLoteRps(Self).NumeroLote,
                                                   OnlyNumber(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                   TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                   IntToStr(TNFSeGerarLoteRps(Self).FNotasFiscais.Count),
                                                   vNotas,
                                                   FTagI, FTagF, FProvedor);
 end;

 FDadosMsg := TNFSeGerarLoteRPS(Self).FNotasFiscais.AssinarLoteRps(TNFSeGerarLoteRps(Self).NumeroLote, FDadosMSg);

//  if FConfiguracoes.Geral.Salvar
//   then begin
    PathSalvar := FConfiguracoes.Arquivos.GetPathGer;
    TNFSeGerarLoteRps(Self).FNotasFiscais.Items[0].NomeArq := PathWithDelim(PathSalvar) +
                                                              TNFSeGerarLoteRps(Self).NumeroLote+'-lot-rps.xml';
    FConfiguracoes.Geral.Save(TNFSeGerarLoteRps(Self).NumeroLote+'-lot-rps.xml', FDadosMsg, PathSalvar);
//   end;

 if FProvedorClass.GetValidarLote
  then begin
   if not(NotaUtil.Valida(FDadosMsg, FMsg,
                          FConfiguracoes.Geral.PathSchemas,
                          FConfiguracoes.WebServices.URL,
                          FConfiguracoes.WebServices.ServicoEnviar,
                          FConfiguracoes.WebServices.Prefixo4))
    then raise Exception.Create('Falha na validação do Lote ' +
                   TNFSeGerarLoteRps(Self).NumeroLote + sLineBreak + FMsg);
  end;
end;

procedure TWebServicesBase.DoNFSeEnviarSincrono;
var
 i         : Integer;
 vNotas    : WideString;
 URI       : String;
 Separador : String;
begin
 vNotas := '';

 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoEnviarSincrono <> ''
  then begin
   if (RightStr(FHTTP_AG, 1) = '/')
    then begin
     if Prefixo3 <> ''
      then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoEnviarSincrono + '"'
      else FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoEnviarSincrono + '"';
    end
    else begin
     if Prefixo3 <> ''
      then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
      else FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
    end;
  end
  else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">'
    else FNameSpaceDad := FNameSpaceDad + ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 if FConfiguracoes.Certificados.AssinaRPS
  then begin
   for i := 0 to TNFSeEnviarSincrono(Self).FNotasFiscais.Count-1 do
    begin
     case FProvedor of
      profintelISS,
      proSaatri,
      proSisPMJP,
      proISSDigital,
      proISSe,
      proSystemPro,
      pro4R,
      proFiorilli,
      proProdata,
      proVitoria,
      proPVH,
      proAgili,
      proCoplan,
      proVirtual,
      proFreire,
      proLink3,
      proGovDigital,
      proGoiania: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      proTecnos: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico', '</' + Prefixo4 + 'tcDeclaracaoPrestacaoServico>') +
                                   '</' + Prefixo4 + 'tcDeclaracaoPrestacaoServico>' +
                              '</' + Prefixo4 + 'Rps>';

      proDigifred: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps ' +
                                 RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'Rps', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
     end;
    end;
  end
  else begin
   for i := 0 to TNFSeEnviarSincrono(Self).FNotasFiscais.Count-1 do
    begin
     case FProvedor of
      profintelISS,
      proSaatri,
      proSisPMJP,
      proGoiania,
      proISSDigital,
      proISSe,
      proSystemPro,
      pro4R,
      proFiorilli,
      proProdata,
      proVitoria,
      proPVH,
      proAgili,
      proCoplan,
      proVirtual,
      proFreire,
      proLink3,
      proActcon,
      proGovDigital: vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                               '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                              '</' + Prefixo4 + 'Rps>';

      proTecnos: vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico', '</tcDeclaracaoPrestacaoServico>') +
                               '</tcDeclaracaoPrestacaoServico>'+
                              '</' + Prefixo4 + 'Rps>';

      else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
     end;
    end;
  end;

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 URI := '';

 URI := FProvedorClass.GetURI(URI);

 FTagI := FProvedorClass.Gera_TagI(acRecSincrono, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URI);

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);
 FTagF := FProvedorClass.Gera_TagF(acRecSincrono, Prefixo3);

 FDadosMsg := TNFSeG.Gera_DadosMsgEnviarSincrono(Prefixo3, Prefixo4,
                                                 FConfiguracoes.WebServices.Identificador,
                                                 NameSpaceDad, VersaoDados, FVersaoXML,
                                                 TNFSeEnviarSincrono(Self).NumeroLote,
                                                 OnlyNumber(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                 TNFSeEnviarSincrono(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                 IntToStr(TNFSeEnviarSincrono(Self).FNotasFiscais.Count),
                                                 vNotas,
                                                 FTagI, FTagF, FProvedor);

 if FDadosMsg <> ''
  then begin
   if FConfiguracoes.WebServices.Salvar
    then FConfiguracoes.Geral.Save('-xxx1.xml', FDadosMsg);

   FDadosMsg := TNFSeEnviarSincrono(Self).FNotasFiscais.AssinarLoteRps(TNFSeEnviarSincrono(Self).NumeroLote, FDadosMSg, True);

   if FConfiguracoes.WebServices.Salvar
    then FConfiguracoes.Geral.Save('-xxx2.xml', FDadosMsg);

   if FProvedorClass.GetValidarLote
    then begin
     if not(NotaUtil.Valida(FDadosMsg, FMsg,
                            FConfiguracoes.Geral.PathSchemas,
                            FConfiguracoes.WebServices.URL,
                            FConfiguracoes.WebServices.ServicoEnviar,
                            FConfiguracoes.WebServices.Prefixo4))
      then raise Exception.Create('Falha na validação do Lote ' +
                     TNFSeEnviarSincrono(Self).NumeroLote + sLineBreak + FMsg);
    end;
  end
  else raise Exception.Create('A funcionalidade [Enviar Sincrono] não foi disponibilizada pelo provedor: ' + FxProvedor);
end;

procedure TWebServicesBase.DoNFSeSubstituirNFSe;
var
 i         : Integer;
 vNotas    : WideString;
 URISig, URIRef, Separador : String;
 Gerador: TGerador;
begin
 vNotas := '';

 if RightStr(FHTTP_AG, 1) = '/'
  then Separador := ''
  else Separador := '/';

 if FCabecalho <> ''
  then begin
   if Prefixo2 <> ''
    then FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) + '="' + FHTTP_AG + Separador + FCabecalho +'">'
    else FNameSpaceCab := ' xmlns="' + FHTTP_AG + Separador + FCabecalho +'">';
  end
  else FNameSpaceCab := '>';

 if FServicoSubstituir <> ''
  then begin
   (*
   if (FProvedor = proIssDSF)
    then FNameSpaceDad :=  'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FURLNS1 + '"'
                    + ' xmlns:tipos="http://localhost:8080/WsNFe2/tp"'
                    + ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
                    + ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote  http://localhost:8080/WsNFe2/xsd/ReqCancelamentoNFSe.xsd"'

   else if (FProvedor = proEquiplano)
    then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="http://www.equiplano.com.br/esnfs" ' +
                          'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                          'xsi:schemaLocation="http://www.equiplano.com.br/enfs esCancelarNfseEnvio_v01.xsd"'

   else begin
   *)
     if RightStr(FHTTP_AG, 1) = '/'
      then begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + Separador + FServicoSubstituir + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + Separador + FServicoSubstituir + '"';
      end
      else begin
       if Prefixo3 <> ''
        then FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + FHTTP_AG + '"'
        else FNameSpaceDad := 'xmlns="' + FHTTP_AG + '"';
      end;
//    end;
  end
  else FNameSpaceDad := '';

 if (FDefTipos = '') and (FNameSpaceDad <> '')
  then FNameSpaceDad := FNameSpaceDad + '>';

 if FDefTipos <> ''
  then begin
   if Prefixo4 <> ''
    then begin
     if (FProvedor = proGINFES)
      then begin
       FVersaoLayOut := '2';
       FVersaoDados  := '2';
       FDefTipos     := 'tipos_v02.xsd';
      end;
     FNameSpaceDad := FNameSpaceDad +
                        ' xmlns:' + StringReplace(Prefixo4, ':', '', []) + '="' + FHTTP_AG + Separador + FDefTipos + '">';
    end
    else FNameSpaceDad := FNameSpaceDad +
                        ' xmlns="' + FHTTP_AG + Separador + FDefTipos + '">';
  end;

 if FNameSpaceDad = ''
  then FNameSpaceDad := '>'
  else FNameSpaceDad := ' ' + FNameSpaceDad;

 if FConfiguracoes.Certificados.AssinaRPS
  then begin
   for i := 0 to TNFSeSubstituirNFSe(Self).FNotasFiscais.Count-1 do
    begin
     case FProvedor of
      profintelISS,
      proSaatri,
      proSisPMJP,
      proISSDigital,
      proISSe,
      proSystemPro,
      pro4R,
      proFiorilli,
      proProdata,
      proVitoria,
      proPVH,
      proAgili,
      proCoplan,
      proVirtual,
      proFreire,
      proLink3,
      proGovDigital,
      proGoiania: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      proTecnos: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico', '</' + Prefixo4 + 'tcDeclaracaoPrestacaoServico>') +
                                   '</' + Prefixo4 + 'tcDeclaracaoPrestacaoServico>' +
                              '</' + Prefixo4 + 'Rps>';

      proDigifred: vNotas := vNotas +
                              '<' + Prefixo4 + 'Rps ' +
                                 RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'Rps', '</Signature>') +
                               '</Signature>'+
                              '</' + Prefixo4 + 'Rps>';

      else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XML_Rps_Ass,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
     end;
    end;
  end
  else begin
   for i := 0 to TNFSeSubstituirNFSe(Self).FNotasFiscais.Count-1 do
    begin
     case FProvedor of
      profintelISS,
      proSaatri,
      proSisPMJP,
      proGoiania,
      proISSDigital,
      proISSe,
      proSystemPro,
      pro4R,
      proFiorilli,
      proProdata,
      proVitoria,
      proPVH,
      proAgili,
      proCoplan,
      proVirtual,
      proFreire,
      proLink3,
      proActcon,
      proGovDigital: vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                               '</' + Prefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                              '</' + Prefixo4 + 'Rps>';

      proTecnos: vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'tcDeclaracaoPrestacaoServico', '</tcDeclaracaoPrestacaoServico>') +
                               '</tcDeclaracaoPrestacaoServico>'+
                              '</' + Prefixo4 + 'Rps>';

      else vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                               '<' + Prefixo4 + 'InfRps' +
                                 RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XML_Rps,
                                   '<' + Prefixo4 + 'InfRps', '</Rps>') +
                              '</' + Prefixo4 + 'Rps>';
     end;
    end;
  end;

 if (TNFSeSubstituirNfse(Self).FNumeroNFSe = '') then
   TNFSeSubstituirNfse(Self).FNumeroNFSe := TNFSeSubstituirNfse(Self).FNotasFiscais.Items[0].NFSe.Numero;

 if (TNFSeSubstituirNfse(Self).FCNPJ = '') then
 begin
   if (FProvedor = proDigifred) or (FProvedor = pro4R) then
    TNFSeSubstituirNfse(Self).FCNPJ := OnlyNumber(TNFSeSubstituirNfse(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj)
   else
    TNFSeSubstituirNfse(Self).FCNPJ := OnlyNumber(TNFSeSubstituirNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj);
 end;

 if (TNFSeSubstituirNfse(Self).FIM = '') then
 begin
  if (FProvedor = proDigifred) or (FProvedor = pro4R) then
    TNFSeSubstituirNfse(Self).FIM := TNFSeSubstituirNfse(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal
  else
    TNFSeSubstituirNfse(Self).FIM := TNFSeSubstituirNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
 end;

 if (TNFSeSubstituirNfse(Self).MotivoCancelamento = '') then
   TNFSeSubstituirNfse(Self).MotivoCancelamento:= TNFSeSubstituirNfse(Self).FNotasFiscais.Items[0].NFSe.MotivoCancelamento;

 if (TNFSeSubstituirNfse(Self).FCodigoMunicipio = '') then
 begin
  if (FProvedor = proISSNet) and (FConfiguracoes.WebServices.AmbienteCodigo = 2) then
   TNFSeSubstituirNfse(Self).FCodigoMunicipio := '999'
  else
   TNFSeSubstituirNfse(Self).FCodigoMunicipio := TNFSeSubstituirNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio;
 end;

 FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                               FConfiguracoes.WebServices.SenhaWeb);

 FCabMsg := FProvedorClass.Gera_CabMsg(Prefixo2, FVersaoLayOut, FVersaoDados, NameSpaceCab, FConfiguracoes.WebServices.CodigoMunicipio);

 case FProvedor of
  // Alterado por Augusto Fontana - 28/04/2014
  proEquiplano, proPublica: URISig:= '';
  proDigifred:  URISig := 'CANC' + TNFSeSubstituirNfse(Self).FNumeroNFSe;
  proSaatri: URISig := 'Cancelamento_' + TNFSeSubstituirNfse(Self).FCnpj;
  proIssIntel,
  proISSNet: begin
              URISig := '';
              URIRef := 'http://www.w3.org/TR/2000/REC-xhtml1-20000126/';
             end;
  proTecnos: URISig := '2' + TNFSeSubstituirNfse(Self).FCnpj + IntToStrZero(StrToInt(TNFSeSubstituirNfse(Self).FNumeroNFSe), 16);
 else        URISig := 'pedidoCancelamento_' + TNFSeSubstituirNfse(Self).FCnpj +
                    TNFSeSubstituirNfse(Self).FIM + TNFSeSubstituirNfse(Self).FNumeroNFSe;
 end;

 if FProvedor <> proISSNet
  then begin
   URISig := FProvedorClass.GetURI(URISig);
   URIRef := URISig;
  end;

 FTagI := FProvedorClass.Gera_TagI(acSubstituir, Prefixo3, Prefixo4, NameSpaceDad, FConfiguracoes.WebServices.Identificador, URISig);

 FTagF := FProvedorClass.Gera_TagF(acSubstituir, Prefixo3);

 if (FProvedor = proIssDSF ) then
 begin
   Gerador := TGerador.Create;
   Gerador.ArquivoFormatoXML := '';

   for i := 0 to TNFSeSubstituirNfse(Self).FNotasFiscais.Count-1 do
   begin
     with TNFSeSubstituirNfse(Self).FNotasFiscais.Items[I] do
     begin
       Gerador.wGrupoNFSe('Nota Id="nota:' + NFSe.Numero + '"');
       Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 11,  1, TNFSeSubstituirNfse(Self).FIM, '');
       Gerador.wCampoNFSe(tcStr, '#1', 'NumeroNota', 01, 12, 1, OnlyNumber(NFSe.Numero), '');
       Gerador.wCampoNFSe(tcStr, '', 'CodigoVerificacao', 01, 255,  1, NFSe.CodigoVerificacao, '');
       Gerador.wCampoNFSe(tcStr, '', 'MotivoCancelamento', 01, 80, 1, NFSe.MotivoCancelamento, '');
       Gerador.wGrupoNFSe('/Nota');
     end;
   end;

   vNotas := Gerador.ArquivoFormatoXML;

   Gerador.Free;
 end;

 if FProvedorClass.GetAssinarXML(acSubstituir)
  then begin
   case FProvedor of
    proIssDSF: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeDSF(Prefixo3, Prefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeSubstituirNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeSubstituirNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeSubstituirNfse(Self).FCodigoMunicipio)),
                                                          TNFSeSubstituirNfse(Self).FNotasFiscais.NumeroLote,
                                                          vNotas,
                                                          '', '');
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEquiplano(StrToInt(TNFSeSubstituirNfse(Self).FCodigoMunicipio),
                                                                OnlyNumber(TNFSeSubstituirNfse(Self).FCnpj),
                                                                TNFSeSubstituirNfse(Self).FIM,
                                                                TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                                TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                                '', '');
    else FDadosMsg := TNFSeG.Gera_DadosMsgSubstituirNFSe(Prefixo3, Prefixo4,
                                                         FConfiguracoes.WebServices.Identificador, NameSpaceDad,
                                                         TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                         TNFSeSubstituirNfse(Self).FCnpj,
                                                         TNFSeSubstituirNfse(Self).FIM,
                                                         TNFSeSubstituirNfse(Self).FCodigoMunicipio,
                                                         TNFSeSubstituirNfse(Self).FCodigoCancelamento,
                                                         TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                         VersaoDados, FVersaoXML,
                                                         IntToStr(TNFSeSubstituirNfse(Self).NumeroRps),
                                                         IntToStr(TNFSeSubstituirNfse(Self).FNotasFiscais.Count),
                                                         vNotas,
                                                         '', '',
                                                         FProvedor);
   end;
   if FDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     URIRef := '';
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeDSF(Prefixo3, Prefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeSubstituirNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeSubstituirNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeSubstituirNfse(Self).FCodigoMunicipio)),
                                                          TNFSeSubstituirNfse(Self).FNotasFiscais.NumeroLote,
                                                          vNotas,
                                                          FTagI, FTagF);
    proEquiplano: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEquiplano(StrToInt(TNFSeSubstituirNfse(Self).FCodigoMunicipio),
                                                                OnlyNumber(TNFSeSubstituirNfse(Self).FCnpj),
                                                                TNFSeSubstituirNfse(Self).FIM,
                                                                TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                                TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                                FTagI, FTagF);
    proFreire: FDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeFreire(Prefixo4,
                                                             NameSpaceDad,
                                                             TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                             TNFSeSubstituirNfse(Self).FCnpj,
                                                             TNFSeSubstituirNfse(Self).FIM,
                                                             TNFSeSubstituirNfse(Self).FCodigoMunicipio,
                                                             TNFSeSubstituirNfse(Self).FCodigoCancelamento,
                                                             TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                             FTagI, FTagF);
    else FDadosMsg := TNFSeG.Gera_DadosMsgSubstituirNFSe(Prefixo3, Prefixo4,
                                                         FConfiguracoes.WebServices.Identificador, NameSpaceDad,
                                                         TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                         TNFSeSubstituirNfse(Self).FCnpj,
                                                         TNFSeSubstituirNfse(Self).FIM,
                                                         TNFSeSubstituirNfse(Self).FCodigoMunicipio,
                                                         TNFSeSubstituirNfse(Self).FCodigoCancelamento,
                                                         TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                         VersaoDados, FVersaoXML,
                                                         IntToStr(TNFSeSubstituirNfse(Self).NumeroRps),
                                                         IntToStr(TNFSeSubstituirNfse(Self).FNotasFiscais.Count),
                                                         vNotas,
                                                         FTagI, FTagF,
                                                         FProvedor);
   end;
  end;

  if DadosMsg = '' then
   raise Exception.Create('A funcionalidade [Substituir NFSe] não foi disponibilizada pelo provedor: ' + FxProvedor);
end;

{ TWebServices }

constructor TWebServices.Create(AFNotaFiscalEletronica: TComponent);
begin
 inherited Create( AFNotaFiscalEletronica );

 FACBrNFSe       := TACBrNFSe(AFNotaFiscalEletronica);
 FEnviar         := TNFSeEnviarLoteRPS.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FConsSitLote    := TNFSeConsultarSituacaoLoteRPS.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FConsLote       := TNFSeConsultarLoteRPS.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FConsNfseRps    := TNFSeConsultarNfseRPS.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FConsNfse       := TNFSeConsultarNfse.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FCancNfse       := TNFSeCancelarNfse.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FGerarNFSe      := TNFSeGerarNFSe.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FLinkNfse       := TNFSeLinkNFSe.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FGerarLoteRPS   := TNFSeGerarLoteRPS.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FEnviarSincrono := TNFSeEnviarSincrono.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FConsSeqRPS     := TNFSeConsultarSequencialRPS.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
 FSubNfse        := TNFSeSubstituirNfse.Create(AFNotaFiscalEletronica, TACBrNFSe(AFNotaFiscalEletronica).NotasFiscais);
end;

destructor TWebServices.Destroy;
begin
  if Assigned(FEnviar) then //Leandro
     FEnviar.Free;
  if Assigned(FConsSitLote) then //Leandro
     FConsSitLote.Free;
  if Assigned(FConsLote) then //Leandro
     FConsLote.Free;
  if Assigned(FConsNfseRps) then //Leandro
     FConsNfseRps.Free;
  if Assigned(FConsNfse) then //Leandro
     FConsNfse.Free;
  if Assigned(FCancNfse) then //Leandro
     FCancNfse.Free;
  if Assigned(FGerarNFSe) then //Leandro
     FGerarNFSe.Free;
  if Assigned(FLinkNfse) then //Leandro
     FLinkNfse.Free;
  if Assigned(FGerarLoteRPS) then //Leandro
     FGerarLoteRPS.Free;
  if Assigned(FEnviarSincrono) then //Leandro
     FEnviarSincrono.Free;
  if Assigned(FConsSeqRPS) then //Leandro
     FConsSeqRPS.Free;
  if Assigned(FSubNfse) then //Leandro
     FSubNfse.Free;
// FEnviar.Free;
// FConsSitLote.Free;
// FConsLote.Free;
// FConsNfseRps.Free;
// FConsNfse.Free;
// FCancNfse.Free;
// FGerarNFSe.Free;
// FLinkNfse.Free;
// FGerarLoteRPS.Free;
// FEnviarSincrono.Free;
// FConsSeqRPS.Free;

 inherited;
end;

function TWebServices.Envia(ALote:Integer): Boolean;
begin
  Result := Envia(IntToStr(ALote));
end;

function TWebServices.Envia(ALote: String): Boolean;
begin
 self.Enviar.FNumeroLote := ALote;

 Result := Self.Enviar.Executar;

 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.Enviar.Msg);
   if Self.Enviar.Msg <> ''
    then raise Exception.Create(Self.Enviar.Msg)
    else raise Exception.Create('Erro Desconhecido ao Enviar o Lote!')
  end;

 Self.ConsSitLote.Cnpj               := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;
 Self.ConsSitLote.InscricaoMunicipal := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
 Self.ConsSitLote.Protocolo          := Self.Enviar.Protocolo;
 Self.ConsSitLote.NumeroLote         := Self.Enviar.NumeroLote;

 if (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor in [proISSDigital] ) then
 begin
   Self.ConsSitLote.Senha        := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.Senha;
   Self.ConsSitLote.FraseSecreta := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.FraseSecreta;
 end;

 Self.ConsLote.Protocolo := Self.Enviar.Protocolo;

 if (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor in [proISSDigital, proTecnos] ) then
 begin
   Self.ConsLote.Senha        := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.Senha;
   Self.ConsLote.FraseSecreta := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.FraseSecreta;
 end;

 if (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.ConsultaLoteAposEnvio) and (Result) then
 begin
   if not (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor in [proDigifred, proProdata,
          proVitoria, proPVH, profintelISS, proSaatri, proSisPMJP, proCoplan, proISSDigital,
          proISSDSF, proFiorilli, proFreire, proTecnos, proDBSeller]) then //adicionei o provedor DBSeller - Rodrigo Custódio
    begin
     Result := Self.ConsSitLote.Executar;

     if not (Result)
      then begin
       if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
        then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsSitLote.Msg);
       if Self.ConsSitLote.Msg <> ''
        then raise Exception.Create(Self.ConsSitLote.Msg)
        else raise Exception.Create('Erro Desconhecido ao Consultar a Situação do Lote!')
      end;
    end;

   if TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor = proInfisc then
     Result := True // Para Provedor Infisc já retorna a NFS-e ao Consultar Situação Lote
   else
     Result := Self.ConsLote.Executar;

   if not (Result)
    then begin
     if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
      then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsLote.Msg);
     if Self.ConsLote.Msg <> ''
      then raise Exception.Create(Self.ConsLote.Msg)
      else raise Exception.Create('Erro Desconhecido ao Consultar o Lote!')
    end;
 end;
end;

function TWebServices.ConsultaSituacao(ACnpj, AInscricaoMunicipal,
  AProtocolo: String; const ANumLote: String = ''): Boolean;
begin
 ACnpj := OnlyNumber(ACnpj);
 if not ValidarCNPJ(ACnpj) then
  raise Exception.Create('CNPJ '+ACnpj+' inválido.');

 Self.ConsSitLote.Cnpj               := ACnpj;
 Self.ConsSitLote.InscricaoMunicipal := AInscricaoMunicipal;
 Self.ConsSitLote.Protocolo          := AProtocolo;
 Self.ConsSitLote.NumeroLote         := ANumLote;

 Result := Self.ConsSitLote.Executar;

 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsSitLote.Msg);
   if Self.ConsSitLote.Msg <> ''
    then raise Exception.Create(Self.ConsSitLote.Msg)
    else raise Exception.Create('Erro Desconhecido ao Consultar a Situação do Lote!')
  end;
end;

function TWebServices.ConsultaLoteRps(AProtocolo: String;
  const CarregaProps: boolean): Boolean;
begin
  if CarregaProps then
  begin
     Self.ConsLote.CNPJ := '';
     Self.ConsLote.IM   := '';
  end;

 Self.ConsLote.Protocolo := AProtocolo;

 Result := Self.ConsLote.Executar;

 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsLote.Msg);
   if Self.ConsLote.Msg <> ''
    then raise Exception.Create(Self.ConsLote.Msg)
    else raise Exception.Create('Erro Desconhecido ao Consultar o Lote!')
  end;
end;

function TWebServices.ConsultaNFSeporRps(ANumero, ASerie, ATipo, ACnpj,
  AInscricaoMunicipal: String; const ASenha : String = '';
  const AFraseSecreta : String = ''; const ARazaoSocial: String = ''): Boolean;
begin
 ACnpj := OnlyNumber(ACnpj);

 if not ValidarCNPJ(ACnpj) and (Length(ACnpj) = 14) then //Eduardo Silva dos Santos - DRD SISTEMAS, testar somente CNPJ
  raise Exception.Create('CNPJ '+ACnpj+' inválido.');

 if not ValidarCPF(ACnpj) and (Length(ACnpj) = 11) then //Eduardo Silva dos Santos - DRD SISTEMAS, testar somente CPF
  raise Exception.Create('CPF '+ACnpj+' inválido.');

 Self.ConsNfseRps.Numero             := ANumero;
 Self.ConsNfseRps.Serie              := ASerie;
 Self.ConsNfseRps.Tipo               := ATipo;
 Self.ConsNfseRps.Cnpj               := ACnpj;
 Self.ConsNfseRps.InscricaoMunicipal := AInscricaoMunicipal;
 Self.ConsNfseRps.Senha              := ASenha;
 Self.ConsNfseRps.FraseSecreta       := AFraseSecreta;
 Self.ConsNfseRps.RazaoSocial        := ARazaoSocial;

 Result := Self.ConsNfseRps.Executar;

 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsNfseRps.Msg);
   if Self.ConsNfseRps.Msg <> ''
    then raise Exception.Create(Self.ConsNfseRps.Msg)
    else raise Exception.Create('Erro Desconhecido ao Consultar a NFS-e por RPS!')
  end;
end;

function TWebServices.ConsultaLoteRps(AProtocolo, ACNPJ,
  AInscricaoMunicipal: String; const ASenha, AFraseSecreta, ARazaoSocial: String): Boolean;
begin
 Self.ConsLote.CNPJ         := ACNPJ;
 Self.ConsLote.IM           := AInscricaoMunicipal;
 Self.ConsLote.Senha        := ASenha;
 Self.ConsLote.FraseSecreta := AFraseSecreta;
 Self.ConsLote.RazaoSocial  := ARazaoSocial;

 Result := ConsultaLoteRPS(AProtocolo, False);
end;

function TWebServices.ConsultaNFSe(ACnpj,
                                   AInscricaoMunicipal: String;
                                   ADataInicial,
                                   ADataFinal: TDateTime;
                                   NumeroNFSe: String = '';
                                   APagina: Integer = 1;
                                   const ASenha : String = '';
                                   const AFraseSecreta: String = '';
                                   ACNPJTomador: String = '';
                                   AIMTomador: String = '';
                                   ANomeInter: String = '';
                                   ACNPJInter: String = '';
                                   AIMInter: String = '';
                                   ASerie: String = ''): Boolean;
begin
 ACnpj := OnlyNumber(ACnpj);
 if not ValidarCNPJ(ACnpj) then
  raise Exception.Create('CNPJ ' + ACnpj + ' inválido.');

 Self.ConsNfse.Cnpj               := ACnpj;
 Self.ConsNfse.InscricaoMunicipal := AInscricaoMunicipal;
 Self.ConsNfse.DataInicial        := ADataInicial;
 Self.ConsNfse.DataFinal          := ADataFinal;
 Self.ConsNfse.NumeroNFSe         := NumeroNFSe;
 Self.ConsNfse.Pagina             := APagina;
 Self.ConsNfse.Senha              := ASenha;
 Self.ConsNfse.FraseSecreta       := AFraseSecreta;
 Self.ConsNfse.CNPJTomador        := ACNPJTomador;
 Self.ConsNfse.IMTomador          := AIMTomador;
 Self.ConsNfse.NomeInter          := ANomeInter;
 Self.ConsNfse.CNPJInter          := ACNPJInter;
 Self.ConsNfse.IMInter            := AIMInter;
 Self.ConsNfse.Serie              := ASerie;

 Result := Self.ConsNfse.Executar;

 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsNfse.Msg);
   if Self.ConsNfse.Msg <> ''
    then raise Exception.Create(Self.ConsNfse.Msg)
    else raise Exception.Create('Erro Desconhecido ao Consultar a NFS-e!')
  end;
end;

function TWebServices.ConsultaSequencialRPS(ACidade, ACnpj,
  AInscricaoMunicipal, ASeriePrestacao: String): Boolean;
begin
 ACnpj := OnlyNumber(ACnpj);
 if not ValidarCNPJ(ACnpj) then
  raise Exception.Create('CNPJ '+ACnpj+' inválido.');

 // Alterado por Italo em 30/06/2014
 Self.ConsSeqRPS.FCidade             := ACidade;
 Self.ConsSeqRPS.FCnpj               := ACnpj;
 Self.ConsSeqRPS.FInscricaoMunicipal := AInscricaoMunicipal;
 Self.ConsSeqRPS.FSeriePrestacao     := ASeriePrestacao;

 Result := Self.ConsSeqRPS.Executar;

 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsSeqRPS.Msg);
   if Self.ConsSeqRPS.Msg <> ''
    then raise Exception.Create(Self.ConsSeqRPS.Msg)
    else raise Exception.Create('Erro Desconhecido ao Consultar a Sequencia de RPS!')
  end;

end;

function TWebServices.CancelaNFSe(ACodigoCancelamento: String;
  const CarregaProps: boolean): Boolean;
begin
 if CarregaProps then
 begin
   Self.CancNfse.NumeroNFSe := '';
   Self.CancNfse.CNPJ := '';
   Self.CancNfse.IM := '';
   Self.CancNfse.CodigoMunicipio := '';
 end;

 if (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor = proEL) then
 begin
   Self.CancNfse.NumeroNFSe      := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Numero;
   Self.CancNfse.CNPJ            := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
   Self.CancNfse.IM              := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
   Self.CancNfse.CodigoMunicipio := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio;
 end;

 Self.CancNfse.CodigoCancelamento := ACodigoCancelamento;

 Result := Self.CancNfse.Executar;

 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.CancNfse.Msg);
   if Self.CancNfse.Msg <> ''
    then raise Exception.Create(Self.CancNfse.Msg)
    else raise Exception.Create('Erro Desconhecido ao Cancelar a NFS-e!')
  end;

 if not (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor in [proISSNet, proEL])
  then begin

   if TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor in [proSystemPro] then
   begin
     Self.ConsNfse.NumeroNFSe         := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Numero;
     Self.ConsNfse.Cnpj               := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
     Self.ConsNfse.InscricaoMunicipal := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
     Result := Self.ConsNfse.Executar;
   end
   else begin
     Self.ConsNfseRps.Numero             := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero;
     Self.ConsNfseRps.Serie              := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie;
     Self.ConsNfseRps.Tipo               := TipoRPSToStr(TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Tipo);

     Self.ConsNfseRps.Cnpj               := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;
     if Self.ConsNfseRps.Cnpj = '' then
       Self.ConsNfseRps.Cnpj := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;

     Self.ConsNfseRps.InscricaoMunicipal := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
     if Self.ConsNfseRps.InscricaoMunicipal = '' then
       Self.ConsNfseRps.InscricaoMunicipal := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

     Self.ConsNfseRps.RazaoSocial        := '';
     if not (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Provedor in [proDigifred]) then
       Self.ConsNfseRps.RazaoSocial := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial;

     Result := Self.ConsNfseRps.Executar;
   end;

   if not(Result)
    then begin
     if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
      then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.ConsNfseRps.Msg);
     if Self.ConsNfseRps.Msg <> ''
      then raise Exception.Create(Self.ConsNfseRps.Msg)
      else raise Exception.Create('Erro Desconhecido ao Consultar a NFS-e por RPS!')
    end;
  end;
end;

function TWebServices.CancelaNFSe(ACodigoCancelamento, ANumeroNFSe, ACNPJ,
  AInscricaoMunicipal, ACodigoMunicipio: String): Boolean;
begin
  Self.CancNfse.NumeroNFSe      := ANumeroNFSe;
  Self.CancNfse.CNPJ            := ACNPJ;
  Self.CancNfse.IM              := AInscricaoMunicipal;
  Self.CancNfse.CodigoMunicipio := ACodigoMunicipio;

  Result := CancelaNFSe(ACodigoCancelamento, False);
end;

function TWebServices.Gera(ARps:Integer): Boolean;
begin
 self.GerarNfse.FNumeroRps := ARps;

 Result := Self.GerarNfse.Executar;

 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.GerarNfse.Msg);
   if Self.GerarNfse.Msg <> ''
    then raise Exception.Create(Self.GerarNfse.Msg)
    else raise Exception.Create('Erro Desconhecido ao Gerar NFS-e!')
  end;
end;

function TWebServices.LinkNFSeGerada(ANumeroNFSe: Integer;
  ACodVerificacao, AInscricaoM: String): String;
begin
 self.LinkNfse.FNumeroNFSe := ANumeroNFSe;
 self.LinkNFSe.FCodVerif   := ACodVerificacao;
 self.LinkNfse.FIM         := AInscricaoM;

 if not (Self.LinkNfse.Executar)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.LinkNfse.Msg);
   if Self.LinkNfse.Msg <> ''
    then raise Exception.Create(Self.LinkNfse.Msg)
    else raise Exception.Create('Erro Desconhecido ao Gerar o Link da NFS-e!')
  end;

 Result := self.LinkNFSe.FLink;
end;

function TWebServices.GeraLote(ALote: Integer): Boolean;
begin
  Result := GeraLote(IntToStr(ALote));
end;

function TWebServices.GeraLote(ALote: String): Boolean;
begin
 self.GerarLoteRPS.FNumeroLote := ALote;

 Result := Self.GerarLoteRPS.Executar;
 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.GerarLoteRPs.Msg);
   if Self.GerarLoteRPs.Msg <> ''
    then raise Exception.Create(Self.GerarLoteRPs.Msg)
    else raise Exception.Create('Erro Desconhecido ao Gerar o Lote de RPS!')
  end;
end;

function TWebServices.EnviaSincrono(ALote: Integer): Boolean;
begin
  Result := EnviaSincrono(IntToStr(Alote));
end;

function TWebServices.EnviaSincrono(ALote: String): Boolean;
begin
 self.EnviarSincrono.FNumeroLote := ALote;

 Result := Self.EnviarSincrono.Executar;

 if not Result
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.EnviarSincrono.Msg);
   if Self.EnviarSincrono.Msg <> ''
    then raise Exception.Create(Self.EnviarSincrono.Msg)
    else raise Exception.Create('Erro Desconhecido ao Enviar o Lote modo Sincrono!')
  end;
end;

function TWebServices.SubstitiNFSe(ACodigoCancelamento, ANumeroNFSe: String): Boolean;
begin
 Self.SubNfse.FCodigoCancelamento := ACodigoCancelamento;
 Self.SubNfse.FMotivoCancelamento := '';

 Self.SubNfse.FNumeroNFSe      := ANumeroNFSe;
 Self.SubNfse.FCnpj            := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;
 Self.SubNfse.FIM              := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
 Self.SubNfse.FCodigoMunicipio := TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.Servico.CodigoMunicipio;
 Self.SubNfse.FNumeroRps       := StrToInt(TACBrNFSe( FACBrNFSe ).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero);

 Result := Self.SubNfse.Executar;

 if not (Result)
  then begin
   if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
    then TACBrNFSe( FACBrNFSe ).OnGerarLog(Self.SubNfse.Msg);
   if Self.SubNfse.Msg <> ''
    then raise Exception.Create(Self.SubNfse.Msg)
    else raise Exception.Create('Erro Desconhecido ao Substituir a NFS-e!')
  end;
end;

{ TNFSeEnviarLoteRPS }

constructor TNFSeEnviarLoteRPS.Create(AOwner : TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);

 FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeEnviarLoteRPS.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
 inherited;
end;

function TNFSeEnviarLoteRPS.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 HashIdent   : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;
 i           : Integer;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}
begin
  inherited Executar;

 if FProvedor = proEL then begin
   (*Este provedor requer autenticação anterior ao envio do lote, com retorno do Hash Identificacao*)
   Stream    := TMemoryStream.Create;
   Acao      := TStringList.Create;
   Texto     := TiraAcentos(FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb,
                                                           FConfiguracoes.WebServices.SenhaWeb));
   Acao.Text := Texto;
   {$IFDEF ACBrNFSeOpenSSL}
     Acao.SaveToStream(Stream);
     HTTP := THTTPSend.Create;
   {$ELSE}
     ReqResp := THTTPReqResp.Create(nil);
     ConfiguraReqResp( ReqResp );
     ReqResp.URL := FURL;
     ReqResp.UseUTF8InHeader := True;
     ReqResp.SoapAction := FProvedorClass.GetSoapAction(acRecepcionar, FNomeCidade);
   {$ENDIF}

    try
      {$IFDEF ACBrNFSeOpenSSL}
        HTTP.Document.LoadFromStream(Stream);
        ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acRecepcionar, FNomeCidade) +'"');
        HTTP.HTTPMethod('POST', FURL);

        StrStream := TStringStream.Create('');
        StrStream.CopyFrom(HTTP.Document, 0);

        FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
        FRetWS     := FProvedorClass.GetRetornoWS(acRecSincrono, FRetornoWS);

        StrStream.Free;
      {$ELSE}
        ReqResp.Execute(Acao.Text, Stream);
        StrStream := TStringStream.Create('');
        StrStream.CopyFrom(Stream, 0);

        FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

        FRetWS     := FProvedorClass.GetRetornoWS(acRecSincrono, FRetornoWS);

        StrStream.Free;
      {$ENDIF}

      HashIdent   := FRetWS; //hashIdentificador

      FDadosMsg   := FTagI +
                      '<identificacaoPrestador>' + FConfiguracoes.WebServices.UserWeb + '</identificacaoPrestador>' +
                      '<hashIdentificador>' + HashIdent + '</hashIdentificador>' +
                      '<arquivo>' +
                       StringReplace(StringReplace(FDadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                      '</arquivo>' +
                     FTagF;

      Acao.Free;
      Stream.Free;

    except
      on E: Exception do
      begin
       Result := False;
       if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog) then
          TACBrNFSe( FACBrNFSe ).OnGerarLog(E.Message);
       if E.Message<>''
        then raise Exception.Create(E.Message)
        else raise Exception.Create('Erro Desconhecido!');
      end;
    end;
 end;

 // O número do protocolo deve ser inicializado antes do processo de transmissão.
 // Ao se transmitir pode ocorrer erro e este campo ficaria com o número de protocolo
 // do lote anterior
 FDataRecebimento := 0;
 FProtocolo       := '';

 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeRecepcionarLoteRPS(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeRecepcao );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(NumeroLote+'-env-lot-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathGer);

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(NumeroLote+'-env-lot.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathGer);

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );
   ReqResp.URL := FURL;
   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acRecepcionar, FNomeCidade);
 {$ENDIF}

 try
  try
    {$IFDEF ACBrNFSeOpenSSL}
      HTTP.Document.LoadFromStream(Stream);
      ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acRecepcionar, FNomeCidade) +'"');
      HTTP.HTTPMethod('POST', FURL);

      StrStream := TStringStream.Create('');
      StrStream.CopyFrom(HTTP.Document, 0);

       // Luiz Baião 2014.11.28    ACBrProvedorNFSEBrasil
//      if FProvedor = proNFSEBrasil then
//        FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//      else
        FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

      FRetWS := FProvedorClass.GetRetornoWS(acRecepcionar, FRetornoWS);

      StrStream.Free;
    {$ELSE}
      ReqResp.Execute(Acao.Text, Stream);
      StrStream := TStringStream.Create('');
      StrStream.CopyFrom(Stream, 0);

     // Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//      if FProvedor = proNFSEBrasil then
//        FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//      else
        FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

      FRetWS := FProvedorClass.GetRetornoWS(acRecepcionar, FRetornoWS);

      StrStream.Free;
    {$ENDIF}

    if FConfiguracoes.WebServices.Salvar
     then FConfiguracoes.Geral.Save(NumeroLote+'-rec-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathGer);

    if FConfiguracoes.Geral.Salvar
     then FConfiguracoes.Geral.Save(NumeroLote+'-rec.xml', FRetWS, FConfiguracoes.Arquivos.GetPathGer);

    NFSeRetorno := TretEnvLote.Create;

    NFSeRetorno.Leitor.Arquivo := FRetWS;

    case FProvedor of
     proEquiplano: 	NFSeRetorno.LerXML_provedorEquiplano;
     proInfisc:     NFSeRetorno.LerXml_provedorInfisc;
     proISSDSF:    	NFSeRetorno.LerXml_provedorIssDsf;
	   proNFSEBrasil: NFSeRetorno.LerXml_provedorNFSEBrasil;
	   proEL:         NFSeRetorno.LerXml_provedorEL;
     else           NFSeRetorno.LerXml;
    end;

    TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

    FDataRecebimento := NFSeRetorno.InfRec.DataRecebimento;
    FProtocolo       := NFSeRetorno.InfRec.Protocolo;
    FNumeroLote      := NFSeRetorno.InfRec.NumeroLote;

    // Lista de Mensagem de Retorno
    FMsg := '';
    if NFSeRetorno.InfRec.MsgRetorno.Count>0
     then begin
      aMsg:='';
      for i:=0 to NFSeRetorno.InfRec.MsgRetorno.Count - 1 do
       begin
        FMsg := FMsg + NFSeRetorno.infRec.MsgRetorno.Items[i].Mensagem + IfThen(FMsg = '', '', ' / ');

        aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.InfRec.MsgRetorno.Items[i].Codigo + LineBreak +
                       'Mensagem... : ' + NFSeRetorno.infRec.MsgRetorno.Items[i].Mensagem + LineBreak+
                       'Correção... : ' + NFSeRetorno.InfRec.MsgRetorno.Items[i].Correcao + LineBreak+
                       'Provedor... : ' + FxProvedor + LineBreak;
       end;
     end
     else begin
      for i:=0 to FNotasFiscais.Count -1 do
       begin
        FNotasFiscais.Items[i].NFSe.Protocolo     := FProtocolo;
        FNotasFiscais.Items[i].NFSe.dhRecebimento := FDataRecebimento;
       end;
      aMsg := 'Numero do Lote : ' + NFSeRetorno.InfRec.NumeroLote + LineBreak +
              'Recebimento... : ' + ifThen(FDataRecebimento = 0, '', DateTimeToStr(FDataRecebimento)) + LineBreak +
              'Protocolo..... : ' + FProtocolo + LineBreak +
              'Provedor...... : ' + FxProvedor + LineBreak;
     end;

    if FConfiguracoes.WebServices.Visualizar
     then ShowMessage(aMsg);

    if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
     then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

    Result := (NFSeRetorno.InfRec.Protocolo<>'');

    if (FProvedor = proEL) and (Trim(HashIdent)<>'') then begin
     Stream.Free;
     Stream    := TMemoryStream.Create;
     Texto     := TiraAcentos(TProvedorEL(FProvedorClass).FinalizaSessao(HashIdent));
     Acao.Text := Texto;
     {$IFDEF ACBrNFSeOpenSSL}
       Acao.SaveToStream(Stream);
       HTTP := THTTPSend.Create;
     {$ELSE}
       ReqResp := THTTPReqResp.Create(nil);
       ConfiguraReqResp( ReqResp );
       ReqResp.URL := FURL;
       ReqResp.UseUTF8InHeader := True;
       ReqResp.SoapAction := FProvedorClass.GetSoapAction(acRecepcionar, FNomeCidade);
     {$ENDIF}

      try
        {$IFDEF ACBrNFSeOpenSSL}
          HTTP.Document.LoadFromStream(Stream);
          ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acRecepcionar, FNomeCidade) +'"');
          HTTP.HTTPMethod('POST', FURL);

          StrStream := TStringStream.Create('');
          StrStream.CopyFrom(HTTP.Document, 0);

          FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
          FRetWS     := FProvedorClass.GetRetornoWS(acRecepcionar, FRetornoWS);

          StrStream.Free;
        {$ELSE}
          ReqResp.Execute(Acao.Text, Stream);
          StrStream := TStringStream.Create('');
          StrStream.CopyFrom(Stream, 0);

          FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

          FRetWS     := FProvedorClass.GetRetornoWS(acRecepcionar, FRetornoWS);

          StrStream.Free;
        {$ENDIF}

      except
        on E: Exception do
        begin
         Result := False;
         if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog) then
            TACBrNFSe( FACBrNFSe ).OnGerarLog(E.Message);
         if E.Message<>''
          then raise Exception.Create(E.Message)
          else raise Exception.Create('Erro Desconhecido!');
        end;
      end;
    end;

  except
		on E: Exception do
		begin
     Result := False;
     if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog) then
        TACBrNFSe( FACBrNFSe ).OnGerarLog(E.Message);
     if E.Message<>''
      then raise Exception.Create(E.Message)
      else raise Exception.Create('Erro Desconhecido!');
		end;
  end;

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

{ TNFSeConsultarSituacaoLoteRPS }

constructor TNFSeConsultarSituacaoLoteRPS.Create(AOwner: TComponent; ANotasFiscais : TNotasFiscais);
begin
 inherited Create(AOwner);
  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeConsultarSituacaoLoteRPS.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
 inherited;
end;

function TNFSeConsultarSituacaoLoteRPS.Executar: Boolean;

function Processando: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;
 Ok          : Boolean;
 i           : Integer;
 xSituacao   : String;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}
begin
 inherited Executar;

 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeConsultarSituacaoLoteRPS(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeConsulta );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(Protocolo + '-con-sit-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathGer);

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(Protocolo + '-con-sit.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathGer);

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );
   ReqResp.URL := FURL;
   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acConsSit, FNomeCidade);
 {$ENDIF}

 try
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Document.LoadFromStream(Stream);
    ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acConsSit, FNomeCidade) +'"');
    HTTP.HTTPMethod('POST', FURL);

    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(HTTP.Document, 0);

    // FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acConsSit, FRetornoWS);

    StrStream.Free;
  {$ELSE}
    ReqResp.Execute(Acao.Text, Stream);
    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(Stream, 0);

    //FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acConsSit, FRetornoWS);
    
    StrStream.Free;
  {$ENDIF}

  if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(Protocolo + '-sit-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathGer);

  if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(Protocolo + '-sit.xml', FRetWS, FConfiguracoes.Arquivos.GetPathGer);

  NFSeRetorno := TretSitLote.Create;

  NFSeRetorno.Leitor.Arquivo := FRetWS;
  if (FProvedor = proEquiplano) then
    NFSeRetorno.LerXML_provedorEquiplano
  else if (FProvedor = proInfisc) then
    NFSeRetorno.LerXML_provedorInfisc
  else if (FProvedor = proEL) then
    NFSeRetorno.LerXML_provedorEL
  else if (FProvedor = proFissLex) Then
    NFSeRetorno.LerXml_provedorFissLex
  else
    NFSeRetorno.LerXml;

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  FSituacao := NFSeRetorno.InfSit.Situacao;
  // FSituacao: 1 = Não Recebido
  //            2 = Não Processado
  //            3 = Processado com Erro
  //            4 = Processado com Sucesso

  // Lista de Mensagem de Retorno
  FMsg := '';
  if NFSeRetorno.InfSit.MsgRetorno.Count>0
   then begin
    aMsg:='';
    for i:=0 to NFSeRetorno.InfSit.MsgRetorno.Count - 1 do
     begin
      FMsg := FMsg + NFSeRetorno.infSit.MsgRetorno.Items[i].Mensagem + IfThen(FMsg = '', '', ' / ');

      aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.InfSit.MsgRetorno.Items[i].Codigo + LineBreak +
                     'Mensagem... : ' + NFSeRetorno.infSit.MsgRetorno.Items[i].Mensagem + LineBreak+
                     'Correção... : ' + NFSeRetorno.InfSit.MsgRetorno.Items[i].Correcao + LineBreak+
                     'Provedor... : ' + FxProvedor + LineBreak;
     end;
   end
   else begin
    for i:=0 to FNotasFiscais.Count -1 do
      FNotasFiscais.Items[i].NFSe.Situacao := FSituacao;

    if (FProvedor = proEquiplano) then
      begin
        case FSituacao[1] of
          '1' : xSituacao := 'Aguardando processamento';
          '2' : xSituacao := 'Não Processado, lote com erro';
          '3' : xSituacao := 'Processado com sucesso';
          '4' : xSituacao := 'Processado com avisos';
        end;
      end
    else if (FProvedor = proEL) then
      begin
        case FSituacao[1] of
          '1' : xSituacao := 'Aguardando processamento';
          '2' : xSituacao := 'Não Processado, lote com erro';
          '3' : xSituacao := 'Processado com avisos';
          '4' : xSituacao := 'Processado com sucesso';
        end;
      end
    else
      begin
        case StrToSituacaoLoteRPS(Ok, FSituacao) of
         slrNaoRecibo        : xSituacao := 'Não Recebido.';
         slrNaoProcessado    : xSituacao := 'Não Processado.';
         slrProcessadoErro   : xSituacao := 'Processado com Erro.';
         slrProcessadoSucesso: xSituacao := 'Processado com Sucesso.';
        end;
      end;
    aMsg := 'Numero do Lote : ' + NFSeRetorno.InfSit.NumeroLote + LineBreak +
            'Situação...... : ' + FSituacao + '-' + xSituacao + LineBreak;
   end;

  if FConfiguracoes.WebServices.Visualizar
   then ShowMessage(aMsg);

  if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
   then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

  if (FProvedor in [proEquiplano, proEL]) then
    Result := (FSituacao = '1')  // Aguardando processamento
  else
    Result := (FSituacao = '2'); // Não Processado

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  DFeUtil.ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

var
  vCont, qTent: Integer;
begin
  inherited Executar;

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeConsulta );
  Sleep(TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.AguardarConsultaRet);
  vCont := 10000;
  qTent := 1;

  while Processando do  // Enquanto FSituacao = 2 (Não Processado) tenta mais uma vez
  begin
    TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeAguardaProcesso );
    if TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.IntervaloTentativas > 0 then
       sleep(TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.IntervaloTentativas)
    else
       sleep(vCont);

    if qTent > TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.Tentativas then
      break;

    qTent := qTent + 1;  
  end;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  if (FProvedor = proEquiplano) then
    Result := (FSituacao = '2') or (FSituacao = '3') or (FSituacao = '4')
		//1 - Aguardando processamento
		//2 - Não Processado, lote com erro
		//3 - Processado com sucesso
		//4 - Processado com avisos
  else if (FProvedor = proInfisc) then
    Result := (FSituacao = '4')
		//3 - Processado com sucesso
		//4 - Processado com avisos
  else
    Result := (FSituacao = '3') or (FSituacao = '4');
  // FSituacao: 1 = Não Recebido
  //            2 = Não Processado
  //            3 = Processado com Erro
  //            4 = Processado com Sucesso
end;

{ TNFSeConsultarLoteRPS }

constructor TNFSeConsultarLoteRPS.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);
  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeConsultarLoteRPS.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
 inherited;
end;

function TNFSeConsultarLoteRPS.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}

 Prefixo3      : String;
 Prefixo4      : String;
 FRetListaNfse : AnsiString;
 FRetNfse      : AnsiString;
 FRetNfse2     : AnsiString;
 i, j, k, p,
 ii, l, m      : Integer;
 PathSalvar    : String;
 NomeArq       : String;
 iNFRetorno    : Integer;
 iNF           : Integer;
begin
 inherited Executar;

 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeConsultarLoteRPS(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeConsulta );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(Protocolo + '-con-lot-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathGer);

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(Protocolo + '-con-lot.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathGer);

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );
   ReqResp.URL := FURL;
   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acConsLote, FNomeCidade);
 {$ENDIF}

 try
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Document.LoadFromStream(Stream);
    ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acConsLote, FNomeCidade) +'"');
    HTTP.HTTPMethod('POST', FURL);

    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(HTTP.Document, 0);

    // FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	     // Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//   if FProvedor = proNFSEBrasil then
//     FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//   else
     FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acConsLote, FRetornoWS);

    StrStream.Free;
  {$ELSE}
    ReqResp.Execute(Acao.Text, Stream);
    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(Stream, 0);

    // FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	     // Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acConsLote, FRetornoWS);

    StrStream.Free;
  {$ENDIF}

  if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(Protocolo + '-lista-nfse-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathGer);

  if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(Protocolo + '-lista-nfse.xml', {NotaUtil.RetirarPrefixos(FRetWS)} FRetWS, FConfiguracoes.Arquivos.GetPathGer);
  self.ArquivoRetorno := FRetWS;

  NFSeRetorno := TretLote.Create;

  Prefixo3 := FConfiguracoes.WebServices.Prefixo3;
  Prefixo4 := FConfiguracoes.WebServices.Prefixo4;

  case FProvedor of
    proBetha:    Prefixo3 := '';
    proDBSeller: Prefixo3 := 'ii:';
    proSpeedGov: begin
                   Prefixo3 := '';
                   Prefixo4 := '';
                 end;
  end;

  try
   NFSeRetorno.Leitor.Arquivo := FRetWS;
   NFSeRetorno.Provedor       := FProvedor;
   NFSeRetorno.TabServicosExt := FConfiguracoes.Arquivos.TabServicosExt;

   if (FProvedor = proIssDsf )then
     NFSeRetorno.LerXml_provedorIssDsf //falta homologar
   else if (FProvedor = proEquiplano) then
     NFSeRetorno.LerXml_provedorEquiplano
   else if (FProvedor = proEL) then
     NFSeRetorno.LerXml_provedorEL
   else if (FProvedor = proFissLex) Then
     NFSeRetorno.LerXml_provedorFissLex
   else
     NFSeRetorno.LerXml;

   if (FProvedor in [proEquiplano, proIssDsf, proEL]) then
    begin
      FRetListaNfse := '';

      if (FNotasFiscais.Count > 0) then
        begin
          for iNFRetorno:= 0 to NFSeRetorno.ListaNfse.CompNfse.Count - 1 do
            begin
              for iNF:= 0 to FNotasFiscais.Count - 1 do
                begin
                  if (FNotasFiscais.Items[iNF].NFSe.IdentificacaoRps.Numero =
                      NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.IdentificacaoRps.Numero) then
                    begin
                      FNotasFiscais.Items[iNF].NFSe.Numero                   := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.Numero;
                      FNotasFiscais.Items[iNF].NFSe.CodigoVerificacao        := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.CodigoVerificacao;
                      FNotasFiscais.Items[iNF].NFSe.DataEmissao              := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.DataEmissao;
                      FNotasFiscais.Items[iNF].NFSe.IdentificacaoRps.Numero  := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.IdentificacaoRps.Numero;
                      FNotasFiscais.Items[iNF].NFSe.NfseCancelamento.DataHora:= NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.NfseCancelamento.DataHora;
                      FNotasFiscais.Items[iNF].NFSe.MotivoCancelamento       := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.MotivoCancelamento;
                      FNotasFiscais.Items[iNF].NFSe.Status                   := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.Status;

                      Break;
                    end;
                end;
            end;
        end
      else  //Não carregou o arquivo
        begin
          for iNFRetorno:= 0 to NFSeRetorno.ListaNfse.CompNfse.Count - 1 do
            begin
              FNotasFiscais.Add;
              FNotasFiscais.Items[iNFRetorno].NFSe.Numero                   := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.Numero;
              FNotasFiscais.Items[iNFRetorno].NFSe.CodigoVerificacao        := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.CodigoVerificacao;
              FNotasFiscais.Items[iNFRetorno].NFSe.DataEmissao              := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.DataEmissao;
              FNotasFiscais.Items[iNFRetorno].NFSe.IdentificacaoRps.Numero  := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.IdentificacaoRps.Numero;
              FNotasFiscais.Items[iNFRetorno].NFSe.NfseCancelamento.DataHora:= NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.NfseCancelamento.DataHora;
              FNotasFiscais.Items[iNFRetorno].NFSe.MotivoCancelamento       := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.MotivoCancelamento;
              FNotasFiscais.Items[iNFRetorno].NFSe.Status                   := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.Status;
            end;
        end;
    end
   else begin
    if FProvedor = proSisPMJP then
      Prefixo3 := 'nfse:';

    FRetListaNfse := SeparaDados(FRetWS, Prefixo3 + 'ListaNfse');
   end;

//  i := 0;
   while FRetListaNfse <> '' do
    begin
     j := Pos('</' + Prefixo3 +
                    ifThen(FProvedor = proBetha, 'ComplNfse', 'CompNfse') + '>', FRetListaNfse);
     p := Length(trim(Prefixo3));
     if j > 0
      then begin
       for iNFRetorno := 0 to NFSeRetorno.ListaNfse.CompNfse.Count - 1 do
       begin
        for iNF := 0 to FNotasFiscais.Count - 1 do
        begin
         if FNotasFiscais.Items[iNF].NFSe.IdentificacaoRps.Numero = NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.IdentificacaoRps.Numero
          then begin
           FNotasFiscais.Items[iNF].Confirmada             := True;
           FNotasFiscais.Items[iNF].NFSe.CodigoVerificacao := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.CodigoVerificacao;
           FNotasFiscais.Items[iNF].NFSe.Numero            := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.Numero;
           FNotasFiscais.Items[iNF].NFSe.Competencia       := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.Competencia;
           FNotasFiscais.Items[iNF].NFSe.NfseSubstituida   := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.NfseSubstituida;
           FNotasFiscais.Items[iNF].NFSe.OutrasInformacoes := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.OutrasInformacoes;
           FNotasFiscais.Items[iNF].NFSe.DataEmissao       := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.DataEmissao;

           FNotasFiscais.Items[iNF].NFSe.Servico.xItemListaServico := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.Servico.xItemListaServico;

           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.RazaoSocial  := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.RazaoSocial;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.NomeFantasia := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.NomeFantasia;

           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj               := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Endereco.Endereco        := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.Endereco;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Endereco.Numero          := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.Numero;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Endereco.Complemento     := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.Complemento;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Endereco.Bairro          := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.Bairro;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Endereco.CodigoMunicipio := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.CodigoMunicipio;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Endereco.UF              := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.UF;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Endereco.CEP             := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.CEP;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Endereco.xMunicipio      := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.xMunicipio;

           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Contato.Telefone := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Contato.Telefone;
           FNotasFiscais.Items[iNF].NFSe.PrestadorServico.Contato.Email    := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Contato.Email;

           FNotasFiscais.Items[iNF].NFSe.Tomador.Endereco.xMunicipio := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.Tomador.Endereco.xMunicipio;

           if FProvedor = proISSNet then
             FRetNfse := AnsiString(StringReplace(String(NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.XML), '<br>', '', [rfReplaceAll]))
           else
             FRetNfse := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.XML;

//             FRetNfse := ParseText(NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.XML);

           k :=  Pos('<' + Prefixo4 + 'Nfse', FRetNfse);
           FRetNfse := Copy(FRetNfse, k, length(FRetNfse));

           // Recoloca o prefixo4 quando o provedor for ISSNet
           if FProvedor = proISSNet
            then begin
             m := length(FRetNFSe);
             FRetNfse2 := '';
             l := 1;
             while l <= m do
              begin
               if FRetNFSe[l] = '<'
                then begin
                 if FRetNFSe[l+1] = '?'
                  then FRetNfse2 := FRetNfse2 + FRetNFSe[l]
                  else begin
                   if FRetNFSe[l+1] = '/'
                    then begin
                     FRetNfse2 := FRetNfse2 + '</' + Prefixo4;
                     inc(l);
                    end
                    else FRetNfse2 := FRetNfse2 + '<' + Prefixo4;
                  end;
                end
                else FRetNfse2 := FRetNfse2 + FRetNFSe[l];
               inc(l);
              end;
             FRetNFSe := FRetNfse2;
            end;

           if FProvedor = proSisPMJP then
             Prefixo3 := 'nfse:';

           FRetNFSe := FProvedorClass.GeraRetornoNFSe(Prefixo3, FRetNFSe, FNomeCidade);

//          if FConfiguracoes.Geral.Salvar
//           then begin

            if FConfiguracoes.Arquivos.EmissaoPathNFSe then
              PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(FNotasFiscais.Items[iNF].NFSe.DataEmissao)
            else
              PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(0);

            if FConfiguracoes.Arquivos.NomeLongoNFSe then
              NomeArq := NotaUtil.GerarNomeNFSe(UFparaCodigo(NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.UF),
                                                NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.DataEmissao,
                                                NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj,
                                                StrToIntDef(NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.Numero, 0)) + '-nfse.xml'
            else
              NomeArq := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.Numero + '-nfse.xml';

            FConfiguracoes.Geral.Save(NomeArq, FRetNfse, PathSalvar);
//                                      NotaUtil.RetirarPrefixos(FRetNfse), PathSalvar);
            if FNotasFiscais.Count>0
             then FNotasFiscais.Items[iNF].NomeArq := PathWithDelim(PathSalvar) + NomeArq;
//           end;

           FNotasFiscais.Items[iNF].XML_NFSe := FRetNfse;

           break;
          end;
        end;
       end;
       FRetListaNfse := Copy(FRetListaNfse, j + 11 + p, length(FRetListaNfse));
//      inc(i);
      end
      else FRetListaNfse:='';
    end;

   TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

   // Lista de Mensagem de Retorno
   FMsg := '';
   if NFSeRetorno.ListaNfse.MsgRetorno.Count>0
    then begin
     aMsg:='';
     for i := 0 to NFSeRetorno.ListaNfse.MsgRetorno.Count - 1 do
      begin
       if NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo <> 'L000'
        then begin
         FMsg := FMsg + IfThen(FMsg = '', '', ' / ') + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem;

         aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo + LineBreak +
                        'Mensagem... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + LineBreak+
                        'Correção... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Correcao + LineBreak+
                        'Provedor... : ' + FxProvedor + LineBreak;
        end;
      end;
     if FConfiguracoes.WebServices.Visualizar
      then ShowMessage(aMsg);
    end;
  except
  end;

  if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
   then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

   Result := (FMsg = '');

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  DFeUtil.ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

{ TNFSeConsultarNfseRPS }

constructor TNFSeConsultarNfseRPS.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);
  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeConsultarNfseRPS.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
 inherited;
end;

function TNFSeConsultarNfseRPS.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}

 Prefixo3     : String;
 Prefixo4     : String;
 FRetCompNfse : AnsiString;
 FRetNfse     : AnsiString;
 i, j         : Integer;
 PathSalvar   : String;
 NomeArq      : String;
  iNFRetorno: Integer;
  iNF: Integer;
begin
 inherited Executar;

 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeConsultarNFSeporRPS(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeConsulta );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(Numero + Serie + '-con-nfse-rps-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathGer);

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(Numero + Serie + '-con-nfse-rps.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathGer);

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );
   ReqResp.URL := FURL;
   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acConsNFSeRps, FNomeCidade);
 {$ENDIF}

 try
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Document.LoadFromStream(Stream);
    ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acConsNFSeRps, FNomeCidade) +'"');
    HTTP.HTTPMethod('POST', FURL);

    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(HTTP.Document, 0);

    // FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acConsNFSeRps, FRetornoWS);

    StrStream.Free;
  {$ELSE}
    ReqResp.Execute(Acao.Text, Stream);
    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(Stream, 0);

    // FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acConsNFSeRps, FRetornoWS);

    StrStream.Free;
  {$ENDIF}

  if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(Numero + Serie + '-comp-nfse-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathGer);

  if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(Numero + Serie + '-comp-nfse.xml', FRetWS, FConfiguracoes.Arquivos.GetPathGer);

  NFSeRetorno := TretNfseRps.Create;

  Prefixo3 := FConfiguracoes.WebServices.Prefixo3;
  Prefixo4 := FConfiguracoes.WebServices.Prefixo4;

  case FProvedor of
    proBetha:    Prefixo3 := '';
    proDBSeller: Prefixo3 := 'ii:';
  end;

  NFSeRetorno.Leitor.Arquivo := FRetWS;
  NFSeRetorno.Provedor       := FProvedor;
  NFSeRetorno.TabServicosExt := FConfiguracoes.Arquivos.TabServicosExt;

  if (FProvedor = proIssDsf )then
  begin
    Result := NFSeRetorno.LerXml_provedorIssDsf;

    if (FNotasFiscais.Count > 0) then
    begin
      for iNFRetorno:= 0 to NFSeRetorno.ListaNfse.CompNfse.Count - 1 do
      begin
        for iNF:= 0 to FNotasFiscais.Count - 1 do
        begin
          if (FNotasFiscais.Items[iNF].NFSe.IdentificacaoRps.Numero =
              NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.IdentificacaoRps.Numero) then
          begin
            FNotasFiscais.Items[iNF].Confirmada                    := True;
            FNotasFiscais.Items[iNF].NFSe.Numero                   := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.Numero;
            FNotasFiscais.Items[iNF].NFSe.CodigoVerificacao        := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.CodigoVerificacao;
            FNotasFiscais.Items[iNF].NFSe.DataEmissao              := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.DataEmissao;
            FNotasFiscais.Items[iNF].NFSe.IdentificacaoRps.Numero  := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.IdentificacaoRps.Numero;
            FNotasFiscais.Items[iNF].NFSe.NfseCancelamento.DataHora:= NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.NfseCancelamento.DataHora;
            FNotasFiscais.Items[iNF].NFSe.MotivoCancelamento       := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.MotivoCancelamento;
            FNotasFiscais.Items[iNF].NFSe.Status                   := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.Status;
            FNotasFiscais.Items[iNF].XML_NFSe                      := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.XML;


            if FConfiguracoes.Arquivos.EmissaoPathNFSe then
              PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.DataEmissao)
            else
              PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(0);

            if FConfiguracoes.Arquivos.NomeLongoNFSe then
              NomeArq := NotaUtil.GerarNomeNFSe(UFparaCodigo(NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.Endereco.UF),
                                                NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.DataEmissao,
                                                NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj,
                                                StrToIntDef(NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.Numero, 0)) + '-nfse.xml'
            else
              NomeArq := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.Numero + '-nfse.xml';

            if FConfiguracoes.Geral.Salvar then
              FConfiguracoes.Geral.Save(NomeArq, NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.XML, PathSalvar);

            Break;
          end;
        end;
      end;
    end;
  end
  else if (FProvedor = proEquiplano) then
    Result := NFSeRetorno.LerXML_provedorEquiplano
  else if (FProvedor = proEL) then
    Result := NFSeRetorno.LerXML_provedorEL
  else
    Result := NFSeRetorno.LerXml;

  if (FProvedor = proEL) then
  begin
    FRetCompNfse := '';

    if (FNotasFiscais.Count > 0) then
      begin
        for iNFRetorno:= 0 to NFSeRetorno.ListaNfse.CompNfse.Count - 1 do
          begin
            for iNF:= 0 to FNotasFiscais.Count - 1 do
              begin
                if (FNotasFiscais.Items[iNF].NFSe.IdentificacaoRps.Numero =
                    NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].Nfse.IdentificacaoRps.Numero) then
                  begin
                    FNotasFiscais.Items[iNF].NFSe.Numero                   := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.Numero;
                    FNotasFiscais.Items[iNF].NFSe.CodigoVerificacao        := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.CodigoVerificacao;
                    FNotasFiscais.Items[iNF].NFSe.DataEmissao              := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.DataEmissao;
                    FNotasFiscais.Items[iNF].NFSe.IdentificacaoRps.Numero  := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.IdentificacaoRps.Numero;
                    FNotasFiscais.Items[iNF].NFSe.NfseCancelamento.DataHora:= NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.NfseCancelamento.DataHora;
                    FNotasFiscais.Items[iNF].NFSe.MotivoCancelamento       := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.MotivoCancelamento;
                    FNotasFiscais.Items[iNF].NFSe.Status                   := NFSeRetorno.ListaNfse.CompNfse.Items[iNFRetorno].NFSe.Status;

                    Break;
                  end;
              end;
          end;
      end;
  end;

  if FProvedor <> proISSNet
   then begin
    FRetWS := NotaUtil.RetirarPrefixos(FRetWS);
    Prefixo3 := '';
    Prefixo4 := '';
   end;

  if FProvedor = proBetha
   then FRetCompNfse := SeparaDados(FRetWS, Prefixo3 + 'ComplNfse')
   else FRetCompNfse := SeparaDados(FRetWS, Prefixo3 + 'CompNfse');

  i := 0;
  while FRetCompNfse <> '' do
   begin
    j := Pos('</' + Prefixo3 + 'Nfse>', FRetCompNfse);
    if j = 0
     then j := Pos('</' + Prefixo4 + 'Nfse>', FRetCompNfse);

    if j > 0
     then begin
      FRetNfse := FRetCompNfse;
      FRetNFSe := FProvedorClass.GeraRetornoNFSe(Prefixo3, FRetNFSe, FNomeCidade);

//      if FConfiguracoes.Geral.Salvar
//       then begin

        if FConfiguracoes.Arquivos.EmissaoPathNFSe then
          PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(NFSeRetorno.ListaNfse.CompNfse.Items[i].NFSe.DataEmissao)
        else
          PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(0);

        if FConfiguracoes.Arquivos.NomeLongoNFSe then
          NomeArq := NotaUtil.GerarNomeNFSe(UFparaCodigo(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.UF),
                                            NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.DataEmissao,
                                            NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj,
                                            StrToIntDef(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero, 0)) + '-nfse.xml'
        else
          NomeArq := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero + '-nfse.xml';

        FConfiguracoes.Geral.Save(NomeArq, FRetNfse, PathSalvar);
//                                  NotaUtil.RetirarPrefixos(FRetNfse), PathSalvar);
        if FNotasFiscais.Count = 0
         then begin
          with FNotasFiscais.Add do begin
            NFSe.NumeroLote := '0';
            NFSe.NomeArq    := '';
          end;
         end;

        if FNotasFiscais.Count>0
         then FNotasFiscais.Items[i].NomeArq := PathWithDelim(PathSalvar) + NomeArq;
//       end;

      FNotasFiscais.Items[i].Confirmada             := True;
      FNotasFiscais.Items[i].NFSe.Protocolo         := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Protocolo;
      FNotasFiscais.Items[i].NFSe.DataEmissao       := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.DataEmissao;
      //FNotasFiscais.Items[i].NFSe.IdentificacaoRps  := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.IdentificacaoRps;
      FNotasFiscais.Items[i].NFSe.IdentificacaoRps.Numero:= NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.IdentificacaoRps.Numero;
      FNotasFiscais.Items[i].NFSe.IdentificacaoRps.Serie := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.IdentificacaoRps.Serie;
      FNotasFiscais.Items[i].NFSe.IdentificacaoRps.Tipo  := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.IdentificacaoRps.Tipo;
      FNotasFiscais.Items[i].NFSe.CodigoVerificacao := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.CodigoVerificacao;
      FNotasFiscais.Items[i].NFSe.Numero            := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero;
      FNotasFiscais.Items[i].NFSe.OutrasInformacoes := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.OutrasInformacoes;
      FNotasFiscais.Items[i].XML_NFSe               := FRetNfse;
      //Eduardo - DRD, adicionei
      FNotasFiscais.Items[i].NFSe.InfID.ID          := NFSeRetorno.ListaNfse.CompNfse.Items[i].NFSe.InfID.ID +
                                                       NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.IdentificacaoRps.Serie;
      FNotasFiscais.Items[i].XML                    := FRetNfse;

      FNotasFiscais.Items[i].NFSe.NfseCancelamento.DataHora := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.NfseCancelamento.DataHora;

      FNotasFiscais.Items[i].NFSe.NfseCancelamento.Pedido.CodigoCancelamento := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.NfseCancelamento.Pedido.CodigoCancelamento;
      FNotasFiscais.Items[i].NFSe.Status                                     := NFSeRetorno.ListaNfse.CompNfse.Items[i].NFSe.Status;

      FRetCompNfse := '';
      inc(i);
     end
     else FRetCompNfse := '';
   end;

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  // Lista de Mensagem de Retorno
  FMsg := '';
  if NFSeRetorno.ListaNfse.MsgRetorno.Count>0
   then begin
    aMsg:='';
    for i:=0 to NFSeRetorno.ListaNfse.MsgRetorno.Count - 1 do
     begin
      if NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo <> 'L000'
       then begin
        FMsg := FMsg + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + IfThen(FMsg = '', '', ' / ');

        aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo + LineBreak +
                       'Mensagem... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + LineBreak+
                       'Correção... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Correcao + LineBreak+
                       'Provedor... : ' + FxProvedor + LineBreak;
       end;
     end;
    if FConfiguracoes.WebServices.Visualizar
     then ShowMessage(aMsg);
   end;

  if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
   then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

  Result := (Result) and (FMsg = '');

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  DFeUtil.ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

{ TNFSeConsultarNfse }

constructor TNFSeConsultarNfse.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);
  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeConsultarNfse.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
 inherited;
end;

function TNFSeConsultarNfse.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}

 Prefixo3      : String;
 Prefixo4      : String;
 FRetListaNfse : AnsiString;
 FRetNfse      : AnsiString;
 i, j, k, p    : Integer;
 PathSalvar    : String;
 NomeArq       : String;
begin
 inherited Executar;

 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeConsultarNFSe(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeConsulta );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(FormatDateTime('yyyymmdd', DataInicial) +
                                  FormatDateTime('yyyymmdd', DataFinal) + '-con-nfse-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathGer);

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(FormatDateTime('yyyymmdd', DataInicial) +
                                  FormatDateTime('yyyymmdd', DataFinal) + '-con-nfse.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathGer);

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );
   ReqResp.URL := FURL;
   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acConsNFSe, FNomeCidade);
 {$ENDIF}

 try
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Document.LoadFromStream(Stream);
    ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acConsNFSe, FNomeCidade) +'"');
    HTTP.HTTPMethod('POST', FURL);

    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(HTTP.Document, 0);

    //FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acConsNFSe, FRetornoWS);

    StrStream.Free;
  {$ELSE}
    ReqResp.Execute(Acao.Text, Stream);
    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(Stream, 0);

    // FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acConsNFSe, FRetornoWS);

    StrStream.Free;
  {$ENDIF}

  if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(FormatDateTime('yyyymmdd', DataInicial) +
                                  FormatDateTime('yyyymmdd', DataFinal) + '-lista-nfse-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathGer);

  if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(FormatDateTime('yyyymmdd', DataInicial) +
                                  FormatDateTime('yyyymmdd', DataFinal) + '-lista-nfse.xml', NotaUtil.RetirarPrefixos(FRetWS), FConfiguracoes.Arquivos.GetPathGer);

  NFSeRetorno := TretNfse.Create;

  Prefixo3 := FConfiguracoes.WebServices.Prefixo3;
  Prefixo4 := FConfiguracoes.WebServices.Prefixo4;

  case FProvedor of
    proBetha:    Prefixo3 := '';
    proDBSeller: Prefixo3 := 'ii:';
  end;

  NFSeRetorno.Leitor.Arquivo := FRetWS;
  NFSeRetorno.Provedor       := FProvedor;
  NFSeRetorno.TabServicosExt := FConfiguracoes.Arquivos.TabServicosExt;

  if (FProvedor = proIssDsf) then
     NFSeRetorno.LerXml_provedorIssDsf //falta homologar
  else if (FProvedor = proInfisc) then
     NFSeRetorno.LerXml_provedorInfisc
  else begin
     NFSeRetorno.LerXml;
     // Utilizado para realizar as consultas as NFSe
     // quando o provedor por Fiorilli e fintelISS
     FPagina := NFSeRetorno.ListaNfse.Pagina;
  end;

  if FProvedor=proInfisc then
    FRetListaNfse := SeparaDados(FRetWS, Prefixo3 + 'NFS-e')
  else
    FRetListaNfse := SeparaDados(FRetWS, Prefixo3 + 'ListaNfse');
  i := 0;
  while FRetListaNfse <> '' do
   begin
    if FProvedor = proBetha
     then j := Pos('</' + Prefixo3 + 'ComplNfse>', FRetListaNfse)
    else if FProvedor = proInfisc then j := Length(FRetListaNfse)
    else j := Pos('</' + Prefixo3 + 'CompNfse>', FRetListaNfse);

    p := Length(trim(Prefixo3));
    if j > 0
     then begin
      FRetNfse := Copy(FRetListaNfse, 1, j - 1);
      if FProvedor=proInfisc then
        k :=  Pos('<' + Prefixo4 + 'infNFSe', FRetNfse)
      else
        k :=  Pos('<' + Prefixo4 + 'Nfse', FRetNfse);
      FRetNfse := Copy(FRetNfse, k, length(FRetNfse));

      FRetNFSe := FProvedorClass.GeraRetornoNFSe(Prefixo3, FRetNFSe, FNomeCidade);

//      if FConfiguracoes.Geral.Salvar
//       then begin

        if FConfiguracoes.Arquivos.EmissaoPathNFSe then
          PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.DataEmissao)
        else
          PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(0);

        if FConfiguracoes.Arquivos.NomeLongoNFSe then
          NomeArq := NotaUtil.GerarNomeNFSe(UFparaCodigo(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.UF),
                                            NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.DataEmissao,
                                            NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj,
                                            StrToIntDef(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero, 0)) + '-nfse.xml'
        else
          NomeArq := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero + '-nfse.xml';

        FConfiguracoes.Geral.Save(NomeArq, FRetNfse, PathSalvar);
//                                  NotaUtil.RetirarPrefixos(FRetNfse), PathSalvar);
//       end;

        if FNotasFiscais.Count>0
          then FNotasFiscais.Items[i].NomeArq := PathWithDelim(PathSalvar) + NomeArq;

      FRetListaNfse := Copy(FRetListaNfse, j + 11 + p, length(FRetListaNfse));
      inc(i);
     end
     else FRetListaNfse:='';
   end;

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  // Lista de Mensagem de Retorno
  FMsg := '';
  if NFSeRetorno.ListaNfse.MsgRetorno.Count>0
   then begin
    aMsg:='';
    for i:=0 to NFSeRetorno.ListaNfse.MsgRetorno.Count - 1 do
     begin
      if NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo <> 'L000'
       then begin
        FMsg := FMsg + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + IfThen(FMsg = '', '', ' / ');

        aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo + LineBreak +
                       'Mensagem... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + LineBreak+
                       'Correção... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Correcao + LineBreak+
                       'Provedor... : ' + FxProvedor + LineBreak;
       end;
     end;
    if FConfiguracoes.WebServices.Visualizar
     then ShowMessage(aMsg);
   end;

  if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
   then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

  Result := (FMsg = '');

  (*
  // Incluido por Mauro Gomes
  // apaga o retorno anterior no atributo do WebService, se houver instância
  if Self.NFSeRetorno <> Nil then
     Self.NFSeRetorno.Free;

  // Incluido por Mauro Gomes
  // guarda o retorno atual para processamento posterior da lista de Notas Fiscais
  Self.NFSeRetorno := NFSeRetorno;

  // Comentado por Mauro Gomes
//  NFSeRetorno.Free;
  *)

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  DFeUtil.ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

{ TNFSeCancelarNfse }

constructor TNFSeCancelarNfse.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);
  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeCancelarNfse.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
 inherited;
end;

function TNFSeCancelarNfse.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;
 i           : Integer;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}
begin
 inherited Executar;

 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeCancelarNFSe(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeCancelamento );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(TNFSeCancelarNFse(Self).FNotasFiscais.Items[0].NFSe.Numero + '-ped-can-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathCan );

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(TNFSeCancelarNFse(Self).FNotasFiscais.Items[0].NFSe.Numero + '-ped-can.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathCan );

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );

  if FProvedor = proRJ
   then ReqResp.URL := 'https://notacarioca.rio.gov.br/WSNacional/nfse.asmx?op=CancelarNfse'
   else ReqResp.URL := FURL;

   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acCancelar, FNomeCidade);
 {$ENDIF}

 try
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Document.LoadFromStream(Stream);
    ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acCancelar, FNomeCidade) +'"');
    HTTP.HTTPMethod('POST', FURL);

    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(HTTP.Document, 0);

    //FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acCancelar, FRetornoWS);

    StrStream.Free;
  {$ELSE}
    ReqResp.Execute(Acao.Text, Stream);
    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(Stream, 0);

    //FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acCancelar, FRetornoWS);

    StrStream.Free;
  {$ENDIF}

  if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(TNFSeCancelarNFse(Self).FNotasFiscais.Items[0].NFSe.Numero + '-can-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathCan);

  if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(TNFSeCancelarNFse(Self).FNotasFiscais.Items[0].NFSe.Numero + '-can.xml', FRetWS, FConfiguracoes.Arquivos.GetPathCan);

  self.ArquivoRetorno := FRetWS;

  NFSeRetorno := TretCancNfse.Create;

  NFSeRetorno.Leitor.Arquivo := FRetWS;
  if FProvedor = proEquiplano then
    NFSeRetorno.LerXML_provedorEquiplano
  else
  if FProvedor = proIssDSF then
    NFSeRetorno.LerXml_provedorIssDsf
  else if FProvedor = proInfisc then
    NFSeRetorno.LerXml_provedorInfisc(FVersaoXML)
  else
    NFSeRetorno.LerXml;

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  FDataHora := NFSeRetorno.InfCanc.DataHora;

  // Lista de Mensagem de Retorno
  FMsg := '';
  if NFSeRetorno.InfCanc.MsgRetorno.Count>0
   then begin
    aMsg:='';
    for i:=0 to NFSeRetorno.InfCanc.MsgRetorno.Count - 1 do
     begin
      FMsg := FMsg + NFSeRetorno.infCanc.MsgRetorno.Items[i].Mensagem + IfThen(FMsg = '', '', ' / ');

      aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.InfCanc.MsgRetorno.Items[i].Codigo + LineBreak +
                     'Mensagem... : ' + NFSeRetorno.infCanc.MsgRetorno.Items[i].Mensagem + LineBreak+
                     'Correção... : ' + NFSeRetorno.InfCanc.MsgRetorno.Items[i].Correcao + LineBreak+
                     'Provedor... : ' + FxProvedor + LineBreak;
     end;
   end
   else aMsg := 'Numero da NFSe : ' + NFSeRetorno.InfCanc.Pedido.IdentificacaoNfse.Numero + LineBreak +
                'Data Hora..... : ' + ifThen(FDataHora = 0, '', DateTimeToStr(FDataHora)) + LineBreak;

  if FConfiguracoes.WebServices.Visualizar
   then ShowMessage(aMsg);

  if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
   then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

  Result := (FMsg='');

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  DFeUtil.ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

{ TNFSeGerarNFSe }

constructor TNFSeGerarNFSe.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);

 FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeGerarNFSe.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
 inherited;
end;

function TNFSeGerarNFSe.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}

 Prefixo3      : String;
 Prefixo4      : String;
 FRetListaNfse : AnsiString;
 FRetNfse      : AnsiString;
 i, j, k, p    : Integer;
 PathSalvar    : String;
 NomeArq       : String;
begin
 inherited Executar;

 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeGerarNFSe(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeRecepcao );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(IntToStr(NumeroRps)+'-ger-nfse-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathGer);

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(IntToStr(NumeroRps)+'-ger-nfse.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathGer);

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );
   ReqResp.URL := FURL;
   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acGerar, FNomeCidade);
 {$ENDIF}

 try
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Document.LoadFromStream(Stream);
    ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acGerar, FNomeCidade) +'"');
    HTTP.HTTPMethod('POST', FURL);

    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(HTTP.Document, 0);

    //FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acGerar, FRetornoWS);

    StrStream.Free;
  {$ELSE}
    ReqResp.Execute(Acao.Text, Stream);
    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(Stream, 0);

    //FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acGerar, FRetornoWS);

    StrStream.Free;
  {$ENDIF}

  if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(IntToStr(NumeroRps) + '-lista-nfse-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathGer);

  if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(IntToStr(NumeroRps) + '-lista-nfse.xml', NotaUtil.RetirarPrefixos(FRetWS), FConfiguracoes.Arquivos.GetPathGer);

  NFSeRetorno := TGerarretNfse.Create;

  Prefixo3 := FConfiguracoes.WebServices.Prefixo3;
  Prefixo4 := FConfiguracoes.WebServices.Prefixo4;

  case FProvedor of
    proBetha:    Prefixo3 := '';
    proDBSeller: Prefixo3 := 'ii:';
    // Incluido por Italo em 18/03/2014
    proFiorilli: begin
                   Prefixo3 := 'ns2:';
                   Prefixo4 := 'ns2:';
                 end;
    //proFreire: Prefixo3 := 'ns1';
  end;

  NFSeRetorno.Leitor.Arquivo := FRetWS;
  NFSeRetorno.Provedor       := FProvedor;
  NFSeRetorno.TabServicosExt := FConfiguracoes.Arquivos.TabServicosExt;
  
  NFSeRetorno.LerXml;
  //Obter o protocolo após leitura do XML
  FProtocolo := NFseRetorno.Protocolo;

  if NFSeRetorno.ListaNfse.CompNfse.Count > 0 then
  begin
    FDataRecebimento := NFSeRetorno.ListaNfse.CompNfse[0].Nfse.dhRecebimento;
    FProtocolo       := NFSeRetorno.ListaNfse.CompNfse[0].Nfse.Protocolo;
  end;
//  FSituacao        := NFSeRetorno.InfSit.Situacao;
  // FSituacao: 1 = Não Recebido
  //            2 = Não Processado
  //            3 = Processado com Erro
  //            4 = Processado com Sucesso

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  FRetListaNfse := SeparaDados(FRetWS, Prefixo3 + 'ListaNfse');

  // Alterado por Nilton Olher - 11/02/2015
  if FProvedor = proGovDigital then
    FRetListaNfse := StringReplace(FRetListaNfse,'ns2:','',[rfReplaceAll]);

  i := 0;
  while FRetListaNfse <> '' do
   begin
    if FProvedor = proBetha
     then j := Pos('</' + Prefixo3 + 'ComplNfse>', FRetListaNfse)
     else j := Pos('</' + Prefixo3 + 'CompNfse>', FRetListaNfse);

    p := Length(trim(Prefixo3));
    if j > 0
     then begin
      FRetNfse := Copy(FRetListaNfse, 1, j - 1);
      k :=  Pos('<' + Prefixo4 + 'Nfse', FRetNfse);
      FRetNfse := Copy(FRetNfse, k, length(FRetNfse));

      FRetNFSe := FProvedorClass.GeraRetornoNFSe(Prefixo3, FRetNFSe, FNomeCidade);

      FNotasFiscais.Items[i].NFSe.Protocolo         := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Protocolo;
      FNotasFiscais.Items[i].NFSe.DataEmissao       := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.DataEmissao;
      FNotasFiscais.Items[i].NFSe.CodigoVerificacao := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.CodigoVerificacao;
      FNotasFiscais.Items[i].NFSe.Numero            := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero;
      // Alterado por Augusto Fontana - 25/04/2014
      FNotasFiscais.Items[i].Confirmada             := True;
      FNotasFiscais.Items[i].NFSe.Competencia       := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Competencia;
      FNotasFiscais.Items[i].NFSe.NfseSubstituida   := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.NfseSubstituida;
      FNotasFiscais.Items[i].NFSe.OutrasInformacoes := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.OutrasInformacoes;

      FNotasFiscais.Items[i].NFSe.Servico.xItemListaServico := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Servico.xItemListaServico;

      FNotasFiscais.Items[i].NFSe.PrestadorServico.RazaoSocial  := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.RazaoSocial;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.NomeFantasia := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.NomeFantasia;

      FNotasFiscais.Items[i].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj               := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

      FNotasFiscais.Items[i].NFSe.PrestadorServico.Endereco.Endereco        := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.Endereco;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.Endereco.Numero          := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.Numero;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.Endereco.Complemento     := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.Complemento;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.Endereco.Bairro          := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.Bairro;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.Endereco.CodigoMunicipio := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.CodigoMunicipio;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.Endereco.UF              := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.UF;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.Endereco.CEP             := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.CEP;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.Endereco.xMunicipio      := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.xMunicipio;

      FNotasFiscais.Items[i].NFSe.PrestadorServico.Contato.Telefone := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Contato.Telefone;
      FNotasFiscais.Items[i].NFSe.PrestadorServico.Contato.Email    := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Contato.Email;

      FNotasFiscais.Items[i].NFSe.Tomador.Endereco.xMunicipio := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Tomador.Endereco.xMunicipio;

//      if FConfiguracoes.Geral.Salvar
//       then begin
        if FConfiguracoes.Arquivos.EmissaoPathNFSe then
          PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(NFSeRetorno.ListaNfse.CompNfse.Items[i].NFSe.DataEmissao)
        else
          PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(0);

        if FConfiguracoes.Arquivos.NomeLongoNFSe then
          NomeArq := NotaUtil.GerarNomeNFSe(UFparaCodigo(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.UF),
                                            NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.DataEmissao,
                                            NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj,
                                            StrToIntDef(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero, 0)) + '-nfse.xml'
        else
          NomeArq := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero + '-nfse.xml';

        FConfiguracoes.Geral.Save(NomeArq, FRetNfse, PathSalvar);
//                                      NotaUtil.RetirarPrefixos(FRetNfse), PathSalvar);
        if FNotasFiscais.Count>0
         then FNotasFiscais.Items[i].NomeArq := PathWithDelim(PathSalvar) + NomeArq;
//       end;
      FRetListaNfse := Copy(FRetListaNfse, j + 11 + p, length(FRetListaNfse));
      // Alterado por Augusto Fontana - 25/04/2014
      FNotasFiscais.Items[i].XML_NFSe := FRetNfse;
      inc(i);
     end
     else FRetListaNfse:='';
   end;

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  // Lista de Mensagem de Retorno
  FMsg := '';
  if NFSeRetorno.ListaNfse.MsgRetorno.Count>0
   then begin
    aMsg:='';
    for i:=0 to NFSeRetorno.ListaNfse.MsgRetorno.Count - 1 do
     begin
      if NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo <> 'L000'
       then begin
        FMsg := FMsg + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + IfThen(FMsg = '', '', ' / ');

        aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo + LineBreak +
                       'Mensagem... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + LineBreak+
                       'Correção... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Correcao + LineBreak+
                       'Provedor... : ' + FxProvedor + LineBreak;
       end;
     end;
    if FConfiguracoes.WebServices.Visualizar
     then ShowMessage(aMsg);
   end;

  if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
   then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

  Result := (FMsg = '');

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  DFeUtil.ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

{ TNFSeLinkNFSe }

constructor TNFSeLinkNFSe.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);

 FNotasFiscais := ANotasFiscais;
end;

function TNFSeLinkNFSe.Executar: Boolean;
begin
 inherited Executar;

 Result := True;
end;

{ TNFSeGerarLoteRPS }

constructor TNFSeGerarLoteRPS.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);

 FNotasFiscais := ANotasFiscais;
end;

function TNFSeGerarLoteRPS.Executar: Boolean;
begin
 inherited Executar;

 Result := True;
end;

{ TNFSeEnviarSincrono }

constructor TNFSeEnviarSincrono.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);

 FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeEnviarSincrono.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
 inherited;
end;

function TNFSeEnviarSincrono.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}

 Prefixo3      : String;
 Prefixo4      : String;
 FRetListaNfse : AnsiString;
 FRetNfse      : AnsiString;
 i, j, k, p,
 ii            : Integer;
 PathSalvar    : String;
 NomeArq       : String;
begin
 inherited Executar;

 // O número do protocolo deve ser inicializado antes do processo de transmissão.
 // Ao se transmitir pode ocorrer erro e este campo ficaria com o número de protocolo
 // do lote anterior
 FDataRecebimento := 0;
 FProtocolo       := '';

 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeRecepcionarSincrono(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeRecepcao );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(NumeroLote+'-env-lotS-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathGer);

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(NumeroLote+'-env-lotS.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathGer);

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );
   ReqResp.URL := FURL;
   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acRecSincrono, FNomeCidade);
 {$ENDIF}

 try
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Document.LoadFromStream(Stream);
    ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acRecSincrono, FNomeCidade) +'"');
    HTTP.HTTPMethod('POST', FURL);

    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(HTTP.Document, 0);

    //FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acRecSincrono, FRetornoWS);

    StrStream.Free;
  {$ELSE}
    ReqResp.Execute(Acao.Text, Stream);
    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(Stream, 0);

    //FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acRecSincrono, FRetornoWS);

    StrStream.Free;
  {$ENDIF}

  if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(NumeroLote + '-lista-nfse-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathGer);

  if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(NumeroLote + '-lista-nfse.xml', NotaUtil.RetirarPrefixos(FRetWS), FConfiguracoes.Arquivos.GetPathGer);

  NFSeRetorno := TGerarretNfse.Create;

  Prefixo3 := FConfiguracoes.WebServices.Prefixo3;
  Prefixo4 := FConfiguracoes.WebServices.Prefixo4;

  case FProvedor of
    proBetha: Prefixo3 := '';
    proDBSeller: Prefixo3 := 'ii:';
  end;

  NFSeRetorno.Leitor.Arquivo := FRetWS;
  NFSeRetorno.Provedor       := FProvedor;

  NFSeRetorno.LerXml;

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

//  FDataRecebimento := NFSeRetorno.InfRec.DataRecebimento;
//  FProtocolo       := NFSeRetorno.InfRec.Protocolo;
//  FSituacao        := NFSeRetorno.InfSit.Situacao;
  // FSituacao: 1 = Não Recebido
  //            2 = Não Processado
  //            3 = Processado com Erro
  //            4 = Processado com Sucesso

  if FProvedor = proSisPMJP then
    Prefixo3 := 'nfse:';

  FRetListaNfse := NotaUtil.RetirarPrefixos(SeparaDados(FRetWS, Prefixo3 + 'ListaNfse'));

  if FProvedor = proSisPMJP then
    Prefixo3 := '';

  // Alterado por Nilton Olher - 11/02/2015
  if FProvedor = proGovDigital then
    FRetListaNfse := StringReplace(FRetListaNfse,'ns2:','',[rfReplaceAll]);

  i := 0;
  while FRetListaNfse <> '' do
   begin
    if FProvedor = proBetha
     then j := Pos('</' + Prefixo3 + 'ComplNfse>', FRetListaNfse)
     else j := Pos('</' + Prefixo3 + 'CompNfse>', FRetListaNfse);

    p := Length(trim(Prefixo3));
    if j > 0
     then begin
      (*
      FRetNfse := Copy(FRetListaNfse, 1, j - 1);
      k :=  Pos('<' + Prefixo4 + 'Nfse', FRetNfse);
      FRetNfse := Copy(FRetNfse, k, length(FRetNfse));

      FRetNFSe := FProvedorClass.GeraRetornoNFSe(Prefixo3, FRetNFSe, FNomeCidade);

      PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(0);
      FConfiguracoes.Geral.Save(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero + '-nfse.xml',
                                NotaUtil.RetirarPrefixos(FRetNfse), PathSalvar);
      if FNotasFiscais.Count>0
       then FNotasFiscais.Items[i].NomeArq := PathWithDelim(PathSalvar) + NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero + '-nfse.xml';

      FRetListaNfse := Copy(FRetListaNfse, j + 11 + p, length(FRetListaNfse));
      *)
      for ii := 0 to NFSeRetorno.ListaNfse.CompNfse.Count -1 do
       begin
        if FNotasFiscais.Items[ii].NFSe.IdentificacaoRps.Numero = NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.IdentificacaoRps.Numero
         then begin
          FNotasFiscais.Items[ii].Confirmada             := True;
          FNotasFiscais.Items[ii].NFSe.CodigoVerificacao := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.CodigoVerificacao;
          FNotasFiscais.Items[ii].NFSe.Numero            := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero;
          FNotasFiscais.Items[ii].NFSe.Competencia       := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Competencia;
          FNotasFiscais.Items[ii].NFSe.NfseSubstituida   := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.NfseSubstituida;
          FNotasFiscais.Items[ii].NFSe.OutrasInformacoes := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.OutrasInformacoes;
          FNotasFiscais.Items[ii].NFSe.DataEmissao       := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.DataEmissao;

          FNotasFiscais.Items[ii].NFSe.Servico.xItemListaServico := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Servico.xItemListaServico;

          FNotasFiscais.Items[ii].NFSe.PrestadorServico.RazaoSocial  := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.RazaoSocial;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.NomeFantasia := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.NomeFantasia;

          FNotasFiscais.Items[ii].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj               := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.Endereco        := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.Endereco;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.Numero          := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.Numero;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.Complemento     := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.Complemento;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.Bairro          := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.Bairro;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.CodigoMunicipio := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.CodigoMunicipio;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.UF              := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.UF;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.CEP             := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.CEP;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.xMunicipio      := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.xMunicipio;

          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Contato.Telefone := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Contato.Telefone;
          FNotasFiscais.Items[ii].NFSe.PrestadorServico.Contato.Email    := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Contato.Email;

          FNotasFiscais.Items[ii].NFSe.Tomador.Endereco.xMunicipio := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Tomador.Endereco.xMunicipio;

          FRetNfse := Copy(FRetListaNfse, 1, j - 1);
          k :=  Pos('<' + Prefixo4 + 'Nfse', FRetNfse);
          FRetNfse := Copy(FRetNfse, k, length(FRetNfse));

          FRetNFSe := FProvedorClass.GeraRetornoNFSe(Prefixo3, FRetNFSe, FNomeCidade);

//          if FConfiguracoes.Geral.Salvar
//           then begin
          if FConfiguracoes.Arquivos.EmissaoPathNFSe then
            PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(NFSeRetorno.ListaNfse.CompNfse.Items[i].NFSe.DataEmissao)
          else
            PathSalvar := FConfiguracoes.Arquivos.GetPathNFSe(0);

          if FConfiguracoes.Arquivos.NomeLongoNFSe then
            NomeArq := NotaUtil.GerarNomeNFSe(UFparaCodigo(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.Endereco.UF),
                                              NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.DataEmissao,
                                              NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.PrestadorServico.IdentificacaoPrestador.Cnpj,
                                              StrToIntDef(NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero, 0)) + '-nfse.xml'
          else
            NomeArq := NFSeRetorno.ListaNfse.CompNfse.Items[i].Nfse.Numero + '-nfse.xml';

            FConfiguracoes.Geral.Save(NomeArq, FRetNfse, PathSalvar);
//                                      NotaUtil.RetirarPrefixos(FRetNfse), PathSalvar);
            if FNotasFiscais.Count>0
             then FNotasFiscais.Items[ii].NomeArq := PathWithDelim(PathSalvar) + NomeArq;
//           end;

          FRetListaNfse := Copy(FRetListaNfse, j + 11 + p, length(FRetListaNfse));

          FNotasFiscais.Items[ii].XML_NFSe := FRetNfse;

          break;
         end;
       end;

      inc(i);
     end
     else FRetListaNfse:='';
   end;

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  if NFSeRetorno.ListaNfse.CompNfse.Count > 0 then
  begin
    FDataRecebimento := NFSeRetorno.ListaNfse.CompNfse[0].Nfse.dhRecebimento;
    FProtocolo       := NFSeRetorno.ListaNfse.CompNfse[0].Nfse.Protocolo;
  end;
  
  // Lista de Mensagem de Retorno
  FMsg := '';
  if NFSeRetorno.ListaNfse.MsgRetorno.Count>0
   then begin
    aMsg:='';
    for i:=0 to NFSeRetorno.ListaNfse.MsgRetorno.Count - 1 do
     begin
      if (NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo <> 'L000') and
         (NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo <> 'A0000')
       then begin
        FMsg := FMsg + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + IfThen(FMsg = '', '', ' / ');

        aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo + LineBreak +
                       'Mensagem... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + LineBreak+
                       'Correção... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Correcao + LineBreak+
                       'Provedor... : ' + FxProvedor + LineBreak;
       end;
     end;
    if FConfiguracoes.WebServices.Visualizar
     then ShowMessage(aMsg);
   end;

  if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
   then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

  Result := (FMsg = '');

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  DFeUtil.ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

{ TNFSeConsultarSequencialRPS }

constructor TNFSeConsultarSequencialRPS.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);
  FNotasFiscais := ANotasFiscais; 
end;

destructor TNFSeConsultarSequencialRPS.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
 inherited;
end;

//usado apenas pelo provedor IssDSF
function TNFSeConsultarSequencialRPS.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}

 Prefixo3      : String;
 Prefixo4      : String;
 i             : Integer;
// PathSalvar    : String;
begin
 inherited Executar;

 // Incluido por Rodrigo Cantelli
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeConsultarSequencialRps(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeConsulta );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save('-con-seqRPS-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathGer);

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save('-con-seqRPS.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathGer);

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );
   ReqResp.URL := FURL;
   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acConsSecRps, FNomeCidade);
 {$ENDIF}

 try
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Document.LoadFromStream(Stream);
    ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acConsSecRps, FNomeCidade) +'"');
    HTTP.HTTPMethod('POST', FURL);

    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(HTTP.Document, 0);

    FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
    FRetWS     := FProvedorClass.GetRetornoWS(acConsSecRps, FRetornoWS);

    StrStream.Free;
  {$ELSE}
    ReqResp.Execute(Acao.Text, Stream);
    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(Stream, 0);

    FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
    FRetWS     := FProvedorClass.GetRetornoWS(acConsSecRps, FRetornoWS);

    StrStream.Free;
  {$ENDIF}

  if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save('-lista-seqRPS-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathGer);

  if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save('-lista-seqRPS.xml', NotaUtil.RetirarPrefixos(FRetWS), FConfiguracoes.Arquivos.GetPathGer);

  NFSeRetorno := TretNfse.Create;

  Prefixo3 := FConfiguracoes.WebServices.Prefixo3;
  Prefixo4 := FConfiguracoes.WebServices.Prefixo4;

  NFSeRetorno.Leitor.Arquivo := FRetWS;
  NFSeRetorno.Provedor       := FProvedor;

  //Este metodo é usado somente por este provedor.
  if (FProvedor = proIssDsf) then
     NFSeRetorno.LerXml_provedorIssDsf; //falta homologar

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  // Lista de Mensagem de Retorno
  FMsg := '';
  if NFSeRetorno.ListaNfse.MsgRetorno.Count>0
   then begin
    aMsg:='';
    for i:=0 to NFSeRetorno.ListaNfse.MsgRetorno.Count - 1 do
     begin
      if NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo <> 'L000'
       then begin
        FMsg := FMsg + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + IfThen(FMsg = '', '', ' / ');

        aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Codigo + LineBreak +
                       'Mensagem... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Mensagem + LineBreak+
                       'Correção... : ' + NFSeRetorno.ListaNfse.MsgRetorno.Items[i].Correcao + LineBreak+
                       'Provedor... : ' + FxProvedor + LineBreak;
       end;
     end;
    if FConfiguracoes.WebServices.Visualizar
     then ShowMessage(aMsg);
   end;

  if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
   then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

  Result := (FMsg = '');

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  DFeUtil.ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

{ TNFSeSubstituirNFSe }

constructor TNFSeSubstituirNFSe.Create(AOwner: TComponent;
  ANotasFiscais: TNotasFiscais);
begin
 inherited Create(AOwner);
  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeSubstituirNFSe.Destroy;
begin
 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;
  inherited;
end;

function TNFSeSubstituirNFSe.Executar: Boolean;
var
 aMsg        : String;
 Texto       : String;
 Acao        : TStringList;
 Stream      : TMemoryStream;
 StrStream   : TStringStream;
 i           : Integer;

 {$IFDEF ACBrNFSeOpenSSL}
   HTTP    : THTTPSend;
 {$ELSE}
   ReqResp : THTTPReqResp;
 {$ENDIF}
begin
 inherited Executar;

 if Assigned(NFSeRetorno)
  then NFSeRetorno.Free;

 Texto := TiraAcentos(FProvedorClass.GeraEnvelopeSubstituirNFSe(URLNS1, FCabMSg, FDadosMsg, FDadosSenha));

 Acao      := TStringList.Create;
 Stream    := TMemoryStream.Create;
 Acao.Text := Texto;

 TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeSubstituicao );

 if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(TNFSeSubstituirNFse(Self).FNotasFiscais.Items[0].NFSe.Numero + '-ped-sub-soap.xml', Texto, FConfiguracoes.Arquivos.GetPathCan );

 if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(TNFSeSubstituirNFse(Self).FNotasFiscais.Items[0].NFSe.Numero + '-ped-sub.xml', FDadosMsg, FConfiguracoes.Arquivos.GetPathCan );

 {$IFDEF ACBrNFSeOpenSSL}
   Acao.SaveToStream(Stream);
   HTTP := THTTPSend.Create;
 {$ELSE}
   ReqResp := THTTPReqResp.Create(nil);
   ConfiguraReqResp( ReqResp );

  // Veriricar qual é a URL do provedor RJ para a realização da Substituição
  if FProvedor = proRJ
   then ReqResp.URL := 'https://notacarioca.rio.gov.br/WSNacional/nfse.asmx?op=CancelarNfse'
   else ReqResp.URL := FURL;

   ReqResp.UseUTF8InHeader := True;

   ReqResp.SoapAction := FProvedorClass.GetSoapAction(acSubstituir, FNomeCidade);
 {$ENDIF}

 try
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Document.LoadFromStream(Stream);
    ConfiguraHTTP(HTTP, 'SOAPAction: "'+ FProvedorClass.GetSoapAction(acSubstituir, FNomeCidade) +'"');
    HTTP.HTTPMethod('POST', FURL);

    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(HTTP.Document, 0);

//    FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
   // Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acSubstituir, FRetornoWS);

    StrStream.Free;
  {$ELSE}
    ReqResp.Execute(Acao.Text, Stream);
    StrStream := TStringStream.Create('');
    StrStream.CopyFrom(Stream, 0);

//    FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));
	// Luiz Baião 2014.12.02    ACBrProvedorNFSEBrasil
//    if FProvedor = proNFSEBrasil then
//      FRetornoWS := TiraAcentos(CaracterEmTagXML(StrStream.DataString, True))
//    else
      FRetornoWS := TiraAcentos(ParseText(StrStream.DataString, True));

    FRetWS := FProvedorClass.GetRetornoWS(acSubstituir, FRetornoWS);

    StrStream.Free;
  {$ENDIF}

  if FConfiguracoes.WebServices.Salvar
   then FConfiguracoes.Geral.Save(TNFSeSubstituirNFse(Self).FNotasFiscais.Items[0].NFSe.Numero + '-sub-soap.xml', FRetornoWS, FConfiguracoes.Arquivos.GetPathCan);

  if FConfiguracoes.Geral.Salvar
   then FConfiguracoes.Geral.Save(TNFSeSubstituirNFse(Self).FNotasFiscais.Items[0].NFSe.Numero + '-sub.xml', FRetWS, FConfiguracoes.Arquivos.GetPathCan);

  self.ArquivoRetorno := FRetWS;

  NFSeRetorno := TretSubsNfse.Create;

  NFSeRetorno.Leitor.Arquivo := FRetWS;
  if FProvedor = proEquiplano then
    NFSeRetorno.LerXML_provedorEquiplano
  else
  if FProvedor = proIssDSF then
    NFSeRetorno.LerXml_provedorIssDsf
  else
    NFSeRetorno.LerXml;

  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );

  // Lista de Mensagem de Retorno
  FMsg := '';
  if NFSeRetorno.MsgRetorno.Count>0
   then begin
    aMsg:='';
    for i:=0 to NFSeRetorno.MsgRetorno.Count - 1 do
     begin
      FMsg := FMsg + NFSeRetorno.MsgRetorno.Items[i].Mensagem + IfThen(FMsg = '', '', ' / ');

      aMsg := aMsg + 'Código Erro : ' + NFSeRetorno.MsgRetorno.Items[i].Codigo + LineBreak +
                     'Mensagem... : ' + NFSeRetorno.MsgRetorno.Items[i].Mensagem + LineBreak+
                     'Correção... : ' + NFSeRetorno.MsgRetorno.Items[i].Correcao + LineBreak+
                     'Provedor... : ' + FxProvedor + LineBreak;
     end;
   end{
   else aMsg := 'Numero da NFSe : ' + NFSeRetorno.Pedido.IdentificacaoNfse.Numero + LineBreak +
                'Data Hora..... : ' + ifThen(FDataHora = 0, '', DateTimeToStr(FDataHora)) + LineBreak};

  if FConfiguracoes.WebServices.Visualizar
   then ShowMessage(aMsg);

  if Assigned(TACBrNFSe( FACBrNFSe ).OnGerarLog)
   then TACBrNFSe( FACBrNFSe ).OnGerarLog(aMsg);

  Result := (FMsg='');

 finally
  {$IFDEF ACBrNFSeOpenSSL}
    HTTP.Free;
  {$ELSE}
    ReqResp.Free;
  {$ENDIF}
  Acao.Free;
  Stream.Free;

//  DFeUtil.ConfAmbiente;
  TACBrNFSe( FACBrNFSe ).SetStatus( stNFSeIdle );
 end;
end;

end.
