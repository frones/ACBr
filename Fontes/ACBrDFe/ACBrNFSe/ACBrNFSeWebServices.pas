{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Nota Fiscal}
{  de Serviço eletrônica - NFSe                                                }

{ Direitos Autorais Reservados (c) 2008 Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
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

unit ACBrNFSeWebServices;

interface

uses
  Classes, SysUtils, pcnAuxiliar, pcnConversao,
  ACBrDFe, ACBrDFeWebService, ACBrDFeSSL,
  ACBrNFSeNotasFiscais, ACBrNFSeConfiguracoes,
  pnfsNFSe, pnfsNFSeG, pnfsConversao, pnfsLerListaNFSe, pnfsEnvLoteRpsResposta,
  pnfsConsSitLoteRpsResposta, pnfsCancNfseResposta, pnfsSubsNfseResposta,
  pnfsAbrirSessaoResposta, synacode;

type

  { TNFSeWebService }

  TNFSeWebService = class(TDFeWebService)
  private
  protected
    FPConfiguracoesNFSe: TConfiguracoesNFSe;

    FNotasFiscais: TNotasFiscais;

    FProvedor: TNFSeProvedor;
    FPStatus: TStatusACBrNFSe;
    FPLayout: TLayOutNFSe;
    FNameSpaceDad: String;
    FNameSpaceCab: String;
    FURI: String;
    FURISig: String;
    FURIRef: String;
    FTagI: String;
    FTagF: String;
    FDadosSenha: String;
    FDadosEnvelope: String;
    FaMsg: String;
    FSeparador: String;
    FPrefixo2: String;
    FPrefixo3: String;
    FPrefixo4: String;
    FNameSpace: String;
    FDefTipos: String;
    FCabecalho: String;
    FxsdServico: String;
    FVersaoXML: String;
    FVersaoNFSe: TVersaoNFSe;
    FxSignatureNode: String;
    FxDSIGNSLote: String;
    FxIdSignature: String;
    FHashIdent: String;
    FTagGrupo: String;
    FTagElemento: String;
    FDocElemento: String;
    FInfElemento: String;
    FIDLote: String;

    FCabecalhoStr: Boolean;
    FDadosStr: Boolean;

    FvNotas: String;
    FXML_NFSe: String;

    FProtocolo: String;
    FDataRecebimento: TDateTime;

    FRetornoNFSe: TRetornoNFSe;
    FGerarDadosMsg: TNFSeG;

    FLoteNaoProc: Boolean;
    FNameSpaceCan: String;
    FIntegridade: String;

    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirEnvelopeSoap; override;
    procedure InicializarServico; override;
    function GerarVersaoDadosSoap: String; override;
    function GerarCabecalhoSoap: String; override;
    procedure InicializarDadosMsg(AIncluiEncodingCab: Boolean);
    procedure FinalizarServico; override;
    procedure IncluirEncoding(Incluir: Boolean);
    function ExtrairRetorno(const GrupoMsgRet, AGrupo: String): String;
    function ExtrairNotasRetorno: Boolean;
    function GerarRetornoNFSe(const ARetNFSe: String): String;
    procedure DefinirSignatureNode(const TipoEnvio: String);
    procedure GerarLoteRPScomAssinatura(const RPS: String);
    procedure GerarLoteRPSsemAssinatura(const RPS: String);
    procedure InicializarTagITagF;
    procedure InicializarGerarDadosMsg;
    function ExtrairGrupoMsgRet(const AGrupo: String): String;
    function RemoverCharControle(AXML: String): String;
    function DefinirDadosSenha(ATexto: String): String;

  public
    constructor Create(AOwner: TACBrDFe); override;

    property Provedor: TNFSeProvedor read FProvedor;
    property Status: TStatusACBrNFSe read FPStatus;
    property Layout: TLayOutNFSe     read FPLayout;
    property NameSpaceCab: String    read FNameSpaceCab;
    property NameSpaceDad: String    read FNameSpaceDad;
    property URI: String             read FURI;
    property URISig: String          read FURISig;
    property URIRef: String          read FURIRef;
    property TagI: String            read FTagI;
    property TagF: String            read FTagF;
    property DadosSenha: String      read FDadosSenha;
    property DadosEnvelope: String   read FDadosEnvelope;
    property aMsg: String            read FaMsg;
    property Separador: String       read FSeparador;
    property Prefixo2: String        read FPrefixo2;
    property Prefixo3: String        read FPrefixo3;
    property Prefixo4: String        read FPrefixo4;
    property NameSpace: String       read FNameSpace;
    property DefTipos: String        read FDefTipos;
    property Cabecalho: String       read FCabecalho;
    property xsdServico: String      read FxsdServico;
    property VersaoXML: String       read FVersaoXML;
    property VersaoNFSe: TVersaoNFSe read FVersaoNFSe;
    property xSignatureNode: String  read FxSignatureNode;
    property xDSIGNSLote: String     read FxDSIGNSLote;
    property xIdSignature: String    read FxIdSignature;
    property HashIdent: String       read FHashIdent;
    property TagGrupo: String        read FTagGrupo;
    property TagElemento: String     read FTagElemento;
    property DocElemento: String     read FDocElemento;
    property InfElemento: String     read FInfElemento;
    property IDLote: String          read FIDLote;
    property LoteNaoProc: Boolean    read FLoteNaoProc;
    property NameSpaceCan: String    read FNameSpaceCan;
    property Integridade: String     read FIntegridade;

    property vNotas: String   read FvNotas;
    property XML_NFSe: String read FXML_NFSe;

    property DataRecebimento: TDateTime read FDataRecebimento;
    property Protocolo: String          read FProtocolo;

    property RetornoNFSe: TRetornoNFSe read FRetornoNFSe   write FRetornoNFSe;
    property GerarDadosMsg: TNFSeG     read FGerarDadosMsg write FGerarDadosMsg;
  end;

  { TNFSeEnviarLoteRPS }

  TNFSeEnviarLoteRPS = class(TNFSeWebService)
  private
    // Entrada
    FNumeroLote: String;
    FqMaxRps: Integer;
    // Retorno
    FRetEnvLote: TRetEnvLote;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property NumeroLote: String read FNumeroLote;
    property qMaxRps: Integer read FqMaxRps;

    property RetEnvLote: TRetEnvLote read FRetEnvLote write FRetEnvLote;
  end;

  { TNFSeTesteEnvioLoteRPS }

  TNFSeTesteEnvioLoteRPS = class(TNFSeEnviarLoteRPS)
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
  end;

{ TNFSeEnviarSincrono }

  TNFSeEnviarSincrono = Class(TNFSeWebService)
  private
    // Entrada
    FNumeroLote: String;
    // Retorno
    FNotaRetornada: Boolean;
    FRetEnvLote: TRetEnvLote;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property NumeroLote: String read FNumeroLote;

    property NotaRetornada: Boolean  read FNotaRetornada;
    property RetEnvLote: TRetEnvLote read FRetEnvLote write FRetEnvLote;
  end;

{ TNFSeGerarNFSe }

  TNFSeGerarNFSe = Class(TNFSeWebService)
  private
    // Entrada
    FNumeroRps: String;
    FNumeroLote: String;
    // Retorno
    FSituacao: String;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property NumeroRps: String read FNumeroRps;
    property NumeroLote: String read FNumeroLote;
    property Situacao: String   read FSituacao;
  end;

  { TNFSeGerarLoteRPS }

  TNFSeGerarLoteRPS = Class(TNFSeEnviarLoteRPS)
  private
    // Entrada
    FNumeroLote: String;
    FqMaxRps: Integer;
    FSincrono: Boolean;

  protected
    procedure DefinirURL; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;
    function Executar: Boolean; override;

    property NumeroLote: String read FNumeroLote;
    property qMaxRps: Integer read FqMaxRps;
    property Sincrono: Boolean read FSincrono;
  end;

{ TNFSeConsultarSituacaoLoteRPS }

  TNFSeConsultarSituacaoLoteRPS = Class(TNFSeWebService)
  private
    // Entrada
    FNumeroLote: String;
    // Retorno
    FSituacao: String;
    FRetSitLote: TRetSitLote;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    function TratarRespostaFinal: Boolean;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    function Executar: Boolean; override;

    property NumeroLote: String   read FNumeroLote   write FNumeroLote;
    property Situacao: String     read FSituacao;

    property RetSitLote: TRetSitLote read FRetSitLote write FRetSitLote;
  end;

{ TNFSeConsultarLoteRPS }

  TNFSeConsultarLoteRPS = Class(TNFSeWebService)
  private
    FSituacao: String;
    // Entrada
    FNumeroLote: String;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property Situacao: String     read FSituacao;
    //usado pelo provedor IssDsf
    property NumeroLote: String   read FNumeroLote   write FNumeroLote;
  end;

{ TNFSeConsultarNFSeRPS }

  TNFSeConsultarNFSeRPS = Class(TNFSeWebService)
  private
    // Entrada
    FNumeroRps: String;
    FSerie: String;
    FTipo: String;
    FNumeroLote: String;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property NumeroRps: String  read FNumeroRps  write FNumeroRps;
    property Serie: String      read FSerie      write FSerie;
    property Tipo: String       read FTipo       write FTipo;
    //usado pelo provedor IssDsf
    property NumeroLote: String read FNumeroLote write FNumeroLote;
  end;

{ TNFSeConsultarNFSe }

  TNFSeConsultarNFSe = Class(TNFSeWebService)
  private
    // Entrada
    FDataInicial: TDateTime;
    FDataFinal: TDateTime;
    FNumeroNFSe: String;
    FPagina: Integer;
    FCNPJTomador: String;
    FIMTomador: String;
    FNomeInter: String;
    FCNPJInter: String;
    FIMInter: String;
    FSerie: String;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property DataInicial: TDateTime read FDataInicial  write FDataInicial;
    property DataFinal: TDateTime   read FDataFinal    write FDataFinal;
    property NumeroNFSe: String     read FNumeroNFSe   write FNumeroNFSe;
    property Pagina: Integer        read FPagina       write FPagina;
    property CNPJTomador: String    read FCNPJTomador  write FCNPJTomador;
    property IMTomador: String      read FIMTomador    write FIMTomador;
    property NomeInter: String      read FNomeInter    write FNomeInter;
    property CNPJInter: String      read FCNPJInter    write FCNPJInter;
    property IMInter: String        read FIMInter      write FIMInter;
    property Serie: String          read FSerie        write FSerie;
  end;

{ TNFSeCancelarNFSe }

  TNFSeCancelarNFSe = Class(TNFSeWebService)
  private
    // Entrada
    FNumeroNFSe: String;
    FCodigoVerificacao: String;
    FCodigoCancelamento: String;
    FMotivoCancelamento: String;
    FNumeroLote: String;

    // Retorno
    FDataHora: TDateTime;
    FRetCancNFSe: TRetCancNFSe;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure SalvarResposta; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property NumeroNFSe: String         read FNumeroNFSe         write FNumeroNFSe;
    property CodigoVerificacao: String  read FCodigoVerificacao  write FCodigoVerificacao;
    property CodigoCancelamento: String read FCodigoCancelamento write FCodigoCancelamento;
    property MotivoCancelamento: String read FMotivoCancelamento write FMotivoCancelamento;
    //usado pelo provedor IssDsf
    property NumeroLote: String         read FNumeroLote         write FNumeroLote;

    property DataHora: TDateTime        read FDataHora           write FDataHora;

    property RetCancNFSe: TRetCancNFSe read FRetCancNFSe write FRetCancNFSe;
  end;

{ TNFSeSubstituirNFSe }

 TNFSeSubstituirNFSe = Class(TNFSeWebService)
  private
    // Entrada
    FNumeroNFSe: String;
    FCodigoCancelamento: String;
    FMotivoCancelamento: String;
    FNumeroRps: String;
    
    // Retorno
    FDataHora: TDateTime;
    FSituacao: String;

    FNFSeRetorno: TretSubsNFSe;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property CodigoCancelamento: String read FCodigoCancelamento write FCodigoCancelamento;
    property MotivoCancelamento: String read FMotivoCancelamento write FMotivoCancelamento;
    property DataHora: TDateTime        read FDataHora           write FDataHora;
    property NumeroNFSe: String         read FNumeroNFSe         write FNumeroNFSe;

    property NumeroRps: String          read FNumeroRps;
    property Situacao: String           read FSituacao;

    property NFSeRetorno: TretSubsNFSe read FNFSeRetorno write FNFSeRetorno;
  end;

 { TNFSeAbrirSessao }

  TNFSeAbrirSessao = Class(TNFSeWebService)
  private
    // Entrada
    FNumeroLote: String;

    // Retorno
    FRetAbrirSessao: TRetAbrirSessao;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property NumeroLote: String read FNumeroLote;

    property RetAbrirSessao: TRetAbrirSessao read FRetAbrirSessao write FRetAbrirSessao;
  end;

 { TNFSeFecharSessao }

  TNFSeFecharSessao = Class(TNFSeWebService)
  private
    // Entrada
    FNumeroLote: String;

    // Retorno
//    FRetAbrirSessao: TRetAbrirSessao;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;
    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property NumeroLote: String read FNumeroLote;

//    property RetAbrirSessao: TRetAbrirSessao read FRetAbrirSessao write FRetAbrirSessao;
  end;

 { TNFSeEnvioWebService }

  TNFSeEnvioWebService = class(TNFSeWebService)
  private
    FXMLEnvio: String;
    FPURLEnvio: String;
    FVersao: String;
    FSoapActionEnvio: String;

  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    function GerarMsgErro(E: Exception): String; override;
    function GerarVersaoDadosSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    function Executar: Boolean; override;

    property XMLEnvio: String        read FXMLEnvio        write FXMLEnvio;
    property URLEnvio: String        read FPURLEnvio       write FPURLEnvio;
    property SoapActionEnvio: String read FSoapActionEnvio write FSoapActionEnvio;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrNFSe: TACBrDFe;
    
    FGerarLoteRPS: TNFSeGerarLoteRPS;
    FEnviarLoteRPS: TNFSeEnviarLoteRPS;
    FTesteEnvioLoteRPS: TNFSeTesteEnvioLoteRPS;
    FEnviarSincrono: TNFSeEnviarSincrono;
    FGerarNFSe: TNFSeGerarNFSe;
    FConsSitLoteRPS: TNFSeConsultarSituacaoLoteRPS;
    FConsLote: TNFSeConsultarLoteRPS;
    FConsNFSeRps: TNFSeConsultarNFSeRps;
    FConsNFSe: TNFSeConsultarNFSe;
    FCancNFSe: TNFSeCancelarNFSe;
    FSubNFSe: TNFSeSubstituirNFSe;
    FAbrirSessao: TNFSeAbrirSessao;
    FFecharSessao: TNFSeFecharSessao;

    FEnvioWebService: TNFSeEnvioWebService;

  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function GeraLote(ALote: Integer; AqMaxRps: Integer = 50;
      ASincrono: Boolean = False): Boolean; overload;
    function GeraLote(const ALote: String; AqMaxRps: Integer = 50;
      ASincrono: Boolean = False): Boolean; overload;

    function Envia(ALote: Integer): Boolean; overload;
    function Envia(const ALote: String): Boolean; overload;

    function TestaEnvio(const ALote: String): Boolean;

    function EnviaSincrono(ALote:Integer): Boolean; overload;
    function EnviaSincrono(const ALote:String): Boolean; overload;

    function Gera(ARps: Integer; ALote: Integer = 1): Boolean;

    function ConsultaSituacao(const AProtocolo: String;
                              const ANumLote: String = ''): Boolean;
    function ConsultaLoteRps(const ANumLote, AProtocolo: String): Boolean;
    function ConsultaNFSeporRps(const ANumero, ASerie, ATipo: String;
                                const ANumLote: String = ''): Boolean;
    function ConsultaNFSe(ADataInicial,
                          ADataFinal: TDateTime;
                          const NumeroNFSe: String = '';
                          APagina: Integer = 1;
                          const ACNPJTomador: String = '';
                          const AIMTomador: String = '';
                          const ANomeInter: String = '';
                          const ACNPJInter: String = '';
                          const AIMInter: String = '';
                          const ASerie: String = ''): Boolean;

    function CancelaNFSe(const ACodigoCancelamento: String;
                         const ANumeroNFSe: String = '';
                         const AMotivoCancelamento: String = '';
                         const ANumLote: String = ''): Boolean;

    function SubstituiNFSe(const ACodigoCancelamento, ANumeroNFSe: String;
                           const AMotivoCancelamento: String = ''): Boolean;

    property ACBrNFSe: TACBrDFe                            read FACBrNFSe        write FACBrNFSe;
    property GerarLoteRPS: TNFSeGerarLoteRPS               read FGerarLoteRPS    write FGerarLoteRPS;
    property EnviarLoteRPS: TNFSeEnviarLoteRPS             read FEnviarLoteRPS   write FEnviarLoteRPS;
    property TesteEnvioLoteRPS: TNFSeTesteEnvioLoteRPS     read FTesteEnvioLoteRPS   write FTesteEnvioLoteRPS;
    property EnviarSincrono: TNFSeEnviarSincrono           read FEnviarSincrono  write FEnviarSincrono;
    property GerarNFSe: TNFSeGerarNFSe                     read FGerarNFSe       write FGerarNFSe;
    property ConsSitLoteRPS: TNFSeConsultarSituacaoLoteRPS read FConsSitLoteRPS  write FConsSitLoteRPS;
    property ConsLote: TNFSeConsultarLoteRPS               read FConsLote        write FConsLote;
    property ConsNFSeRps: TNFSeConsultarNFSeRps            read FConsNFSeRps     write FConsNFSeRps;
    property ConsNFSe: TNFSeConsultarNFSe                  read FConsNFSe        write FConsNFSe;
    property CancNFSe: TNFSeCancelarNFSe                   read FCancNFSe        write FCancNFSe;
    property SubNFSe: TNFSeSubstituirNFSe                  read FSubNFSe         write FSubNFSe;
    property AbrirSessao: TNFSeAbrirSessao                 read FAbrirSessao     write FAbrirSessao;
    property FecharSessao: TNFSeFecharSessao               read FFecharSessao    write FFecharSessao;

    property EnvioWebService: TNFSeEnvioWebService         read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil, ACBrNFSe,
  pcnGerador, pcnLeitor;

{ TNFSeWebService }

constructor TNFSeWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesNFSe := TConfiguracoesNFSe(FPConfiguracoes);
  FPLayout := LayNFSeRecepcaoLote;
  FPStatus := stNFSeIdle;
  FCabecalhoStr:= False;
  FDadosStr:= False;
end;

procedure TNFSeWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrNFSe(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

procedure TNFSeWebService.DefinirServicoEAction;
var
  Ambiente: String;
begin
  if Pos('%NomeURL_HP%', FPSoapAction) > 0 then
  begin
    if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
      FPSoapAction := StringReplace(FPSoapAction, '%NomeURL_HP%', FPConfiguracoesNFSe.Geral.xNomeURL_H, [rfReplaceAll])
    else
      FPSoapAction := StringReplace(FPSoapAction, '%NomeURL_HP%', FPConfiguracoesNFSe.Geral.xNomeURL_P, [rfReplaceAll]);
  end;

  case FProvedor of
    proTinus:
      begin
        if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
          FPSoapAction := StringReplace(FPSoapAction, 'www.tinus', 'www2.tinus', [rfReplaceAll]);

        if FPConfiguracoesNFSe.Geral.CodigoMunicipio = 2407104 then
        begin
          // Macaiba/RN
          if FPConfiguracoesNFSe.WebServices.Ambiente = taProducao then
            FPSoapAction := StringReplace(FPSoapAction, 'www.tinus.com.br', 'tempuri.org', [rfReplaceAll]);
        end;
      end;

    proEReceita:
      begin
        if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
          FPSoapAction := StringReplace(FPSoapAction, 'https://www.ereceita', 'http://www3.ereceita', [rfReplaceAll]);
      end;

    proActconv202:
      begin
        if FPConfiguracoesNFSe.Geral.CodigoMunicipio = 3167202 then
          Ambiente := 'nfse'
        else
          Ambiente := 'nfseserv';

        if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
          FPSoapAction := StringReplace(FPSoapAction, '%Ambiente%', 'homologacao', [rfReplaceAll])
        else
          FPSoapAction := StringReplace(FPSoapAction, '%Ambiente%', Ambiente, [rfReplaceAll]);
      end;

    proActcon:
      begin
      {
        if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
          FPSoapAction := StringReplace(FPSoapAction, '%Ambiente%', 'homologacao', [rfReplaceAll])
        else
          FPSoapAction := StringReplace(FPSoapAction, '%Ambiente%', 'nfseserv', [rfReplaceAll]);
      }
      end;
  end;
end;

procedure TNFSeWebService.DefinirEnvelopeSoap;
var
  Texto, DadosMsg, CabMsg, NameSpaceTemp, Bound, UsuarioWeb, SenhaWeb: String;
begin
  {$IFDEF FPC}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  Texto := Texto + FDadosEnvelope;

  if FProvedor = proIPM then
  begin
    Bound := IntToHex( Random( MaxInt ), 8 ) + '_Synapse_boundary';

    UsuarioWeb := Trim(FPConfiguracoesNFSe.Geral.Emitente.WebUser);
    if UsuarioWeb = '' then
      UsuarioWeb := Trim(FPConfiguracoesNFSe.Geral.UserWeb);

    SenhaWeb := Trim(FPConfiguracoesNFSe.Geral.Emitente.WebSenha);
    if SenhaWeb = '' then
      SenhaWeb := Trim(FPConfiguracoesNFSe.Geral.SenhaWeb);

    if UsuarioWeb = '' then
      GerarException(ACBrStr('O provedor IPM necessita que a propriedade: Configuracoes.Geral.Emitente.WebUser seja informada.'));

    if SenhaWeb = '' then
      GerarException(ACBrStr('O provedor IPM necessita que a propriedade: Configuracoes.Geral.Emitente.WebSenha seja informada.'));

    Texto := Texto +
      '--' + Bound + sLineBreak +
      'Content-Disposition: form-data; name=' + AnsiQuotedStr( 'login', '"') + sLineBreak +
      sLineBreak +

      UsuarioWeb +

      sLineBreak + '--' + Bound + sLineBreak +
      'Content-Disposition: form-data; name=' + AnsiQuotedStr( 'senha', '"') + sLineBreak +
      sLineBreak +

      SenhaWeb +

      sLineBreak + '--' + Bound + sLineBreak +
      'Content-Disposition: form-data; name=' + AnsiQuotedStr( 'f1', '"' ) + '; ' +
      'filename=' + AnsiQuotedStr( GerarPrefixoArquivo + '-' + FPArqEnv + '.xml', '"') + sLineBreak +
      'Content-Type: text/xml' + sLineBreak +
      sLineBreak +

      FPDadosMsg +

      sLineBreak +
      '--' + Bound + '--' + sLineBreak;

    FPMimeType := 'multipart/form-data; boundary=' + AnsiQuotedStr( Bound, '"' );
  end
  else
  begin
    if FPConfiguracoesNFSe.WebServices.Ambiente = taProducao then
      NameSpaceTemp := FPConfiguracoesNFSe.Geral.ConfigNameSpace.Producao
    else
      NameSpaceTemp := FPConfiguracoesNFSe.Geral.ConfigNameSpace.Homologacao;

    if FProvedor in [proSafeWeb, proTcheInfov2] then
      FPCabMsg := StringReplace(FPCabMsg, '%SenhaMsg%' , FDadosSenha, [rfReplaceAll]);

    CabMsg := FPCabMsg;
    if FCabecalhoStr then
      CabMsg := StringReplace(StringReplace(CabMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

    CabMsg := StringReplace(CabMsg, '%NameSpace%', NameSpaceTemp, [rfReplaceAll]);
    CabMsg := StringReplace(CabMsg, '%VersaoAtrib%', FPConfiguracoesNFSe.Geral.ConfigXML.VersaoAtrib, [rfReplaceAll]);
    CabMsg := StringReplace(CabMsg, '%VersaoDados%', FPConfiguracoesNFSe.Geral.ConfigXML.VersaoDados, [rfReplaceAll]);

    DadosMsg := FPDadosMsg;
    if FDadosStr then
      DadosMsg := StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);

    // Alterações no conteudo de DadosMsg especificas para alguns provedores
    case FProvedor of
      proGinfes:
        begin
          if (FPLayout = LayNfseCancelaNfse) then
            DadosMsg := StringReplace(StringReplace(DadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]);
        end;

      proPronim:
        DadosMsg := StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);

      proPronimV2:
        DadosMsg := StringReplace(DadosMsg, ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', [rfReplaceAll]);

//      proSigep:    DadosMsg := Copy(DadosMsg, (pos('&lt;p:credenciais',DadosMsg)), length(DadosMsg));
    end;

    // %SenhaMsg%  : Representa a Mensagem que contem o usuário e senha
    // %NameSpace% : Representa o NameSpace de Homologação/Produção
    // %CabMsg%    : Representa a Mensagem de Cabeçalho
    // %DadosMsg%  : Representa a Mensagem de Dados

    Texto := StringReplace(Texto, '%SenhaMsg%' , FDadosSenha, [rfReplaceAll]);
    Texto := StringReplace(Texto, '%NameSpace%', NameSpaceTemp  , [rfReplaceAll]);
    Texto := StringReplace(Texto, '%CabMsg%'   , CabMsg     , [rfReplaceAll]);
    Texto := StringReplace(Texto, '%DadosMsg%' , DadosMsg   , [rfReplaceAll]);
    Texto := StringReplace(Texto, '%inscricaoMunicipal%', FPConfiguracoesNFSe.Geral.Emitente.InscMun, [rfReplaceAll]);
  end;

  {Configura Authorization para GIAP}
//  if Provedor = proGiap then
//    FAuthorization := FPConfiguracoesNFSe.Geral.Emitente.WebChaveAcesso;

  FPEnvelopeSoap := Texto;
end;

procedure TNFSeWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  FProvedor := FPConfiguracoesNFSe.Geral.Provedor;

  inherited InicializarServico;

  if FPConfiguracoesNFSe.Geral.ConfigGeral.VersaoSoap = '' then
    FPMimeType := 'application/xml'
  else if FPConfiguracoesNFSe.Geral.ConfigGeral.VersaoSoap = '1.2' then
    FPMimeType := 'application/soap+xml'
  else
    FPMimeType := 'text/xml';

  FPDFeOwner.SSL.UseCertificateHTTP := FPConfiguracoesNFSe.Geral.ConfigGeral.UseCertificateHTTP;

  TACBrNFSe(FPDFeOwner).SetStatus(FPStatus);
end;

function TNFSeWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrNFSe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

function TNFSeWebService.GerarCabecalhoSoap: String;
begin
  Result := FPCabMsg;
end;

procedure TNFSeWebService.InicializarDadosMsg(AIncluiEncodingCab: Boolean);
var
  xmlns2, xmlns3, xmlns4: String;
  Ok: Boolean;
begin
  FvNotas := '';
  FURI    := '';
  FURISig := '';
  FURIRef := '';

  FPConfiguracoesNFSe.WebServices.QuebradeLinha := FPConfiguracoesNFSe.Geral.ConfigGeral.QuebradeLinha;

  FNameSpace  := FPConfiguracoesNFSe.Geral.ConfigXML.NameSpace;

  if FProvedor = proActcon then
  begin
    if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
      FNameSpace := StringReplace(FNameSpace, '%Ambiente%', 'homologacao', [rfReplaceAll])
    else
      FNameSpace := StringReplace(FNameSpace, '%Ambiente%', 'nfseserv', [rfReplaceAll]);
  end;

  if Pos('%NomeURL_HP%', FNameSpace) > 0 then
  begin
    if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
    begin
      FNameSpace := StringReplace(FNameSpace, '%NomeURL_HP%', FPConfiguracoesNFSe.Geral.xNomeURL_H, [rfReplaceAll]);

      if FProvedor in [proActcon, proActconv202] then
      begin
        if FPConfiguracoesNFSe.Geral.CodigoMunicipio = 3167202 then
          FNameSpace := StringReplace(FNameSpace, '//nfse', '//homologacao', [rfReplaceAll])
        else
          FNameSpace := StringReplace(FNameSpace, '/nfseserv/', '/homologacao/', [rfReplaceAll]);
      end;

    end
    else
      FNameSpace := StringReplace(FNameSpace, '%NomeURL_HP%', FPConfiguracoesNFSe.Geral.xNomeURL_P, [rfReplaceAll]);
  end;

  FVersaoXML  := FPConfiguracoesNFSe.Geral.ConfigXML.VersaoXML;
  FVersaoNFSe := StrToVersaoNFSe(Ok, FVersaoXML);
  FDefTipos   := FPConfiguracoesNFSe.Geral.ConfigSchemas.DefTipos;

//  if (FProvedor = proGinfes) and (FPLayout = LayNfseCancelaNfse) then
//    FDefTipos := 'tipos_v02.xsd';

  FCabecalho := FPConfiguracoesNFSe.Geral.ConfigSchemas.Cabecalho;
  FPrefixo2  := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo2;
  FPrefixo3  := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo3;
  FPrefixo4  := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo4;
  FPCabMsg   := FPConfiguracoesNFSe.Geral.ConfigEnvelope.CabecalhoMsg;

  if FPrefixo2 <> '' then
    xmlns2 := 'xmlns:' + StringReplace(FPrefixo2, ':', '', []) + '="'
  else
    xmlns2 := 'xmlns="';

  if FPrefixo3 <> '' then
    xmlns3 := 'xmlns:' + StringReplace(FPrefixo3, ':', '', []) + '="'
  else
    xmlns3 := 'xmlns="';

  if FPrefixo4 <> '' then
    xmlns4 := 'xmlns:' + StringReplace(FPrefixo4, ':', '', []) + '="'
  else
    xmlns4 := 'xmlns="';

//  if FProvedor = proSigep then
//  begin
//    xmlns3 := '';
//    xmlns2 := '';
//    xmlns4 := '';
//  end;

  if AIncluiEncodingCab then
    FPCabMsg := '<' + ENCODING_UTF8 + '>' + FPCabMsg;

  if RightStr(FNameSpace, 1) = '/' then
    FSeparador := ''
  else
    FSeparador := '/';

  if FCabecalho <> '' then
    FNameSpaceCab := ' ' + xmlns2 + FNameSpace + FSeparador + FCabecalho +'">'
  else
    FNameSpaceCab := '>';

  // Seta o NameSpace para realizar a validação do Lote.
  FPDFeOwner.SSL.NameSpaceURI := FNameSpace;

  if FxsdServico <> '' then
  begin
    case FProvedor of
//      proGiss: FNameSpaceDad := 'xmlns="http://www.giss.com.br/' + FxsdServico + '"' +
//                                 ' xmlns:n2="http://www.altova.com/samplexml/other-namespace"' +
//                                 ' xmlns:tipos="http://www.giss.com.br/tipos-v2_04.xsd"' +
//                                 ' xmlns:dsig="http://www.w3.org/2000/09/xmldsig#"' +
//                                 ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';

      // incluido em 23/06/2017 por italo
      proGovBr: FNameSpaceDad := 'xmlns:ns2="http://www.w3.org/2000/09/xmldsig#"' +
                                 ' xmlns:ns3="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"';

//      proMetropolisWeb: FNameSpaceDad := xmlns3 + FNameSpace + '" ' +
//            'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
//            'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';

      proInfisc,
      proInfiscv11,
      proSMARAPD: FNameSpaceDad := xmlns3 + FNameSpace + '"';

      proGiap: FNameSpaceDad := xmlns3 + FNameSpace + '"';

      proIssDSF: FNameSpaceDad := xmlns3 + FNameSpace + '"' +
                                  ' xmlns:tipos="http://localhost:8080/WsNFe2/tp"' +
                                  ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                                  ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote' +
                                  ' http://localhost:8080/WsNFe2/xsd/' + FxsdServico + '"';

      proWebISS,
      proWebISSv2: FNameSpaceDad := xmlns3 + FNameSpace + '"' ;
//                                  ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
//                                  ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"';

      proNFSeBrasil: FNameSpaceDad := ' xmlns:xs="http://www.nfsebrasil.net.br/nfse/rps/xsd/rps.xsd"' +
                                      ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';

      proEL: FNameSpaceDad := 'xmlns="' + FNameSpace + FSeparador + FxsdServico + '" ' +
                              'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                              'xmlns:xsd="http://www.w3.org/2001/XMLSchema" ' +
                              'xsi:schemaLocation="' + FNameSpace + FSeparador + FxsdServico + ' ' + FxsdServico + ' "';

      proSigep: FNameSpaceDad := 'xsi:schemaLocation="http://www.abrasf.org.br/nfse.xsd nfse-v2.xsd" ' +
                                 'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                                 xmlns3 + FNameSpace+ '" ' +
                                 'xmlns:ds="http://www.w3.org/2000/09/xmldsig#"';

      proTiplanv2: FNameSpaceDad := xmlns3 + FNameSpace + '"' +
                                  ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                                  ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';

      else begin
        if (FSeparador = '') then
        begin
          FNameSpaceDad := xmlns3 + FNameSpace + FSeparador + FxsdServico + '"';
          FPDFeOwner.SSL.NameSpaceURI := FNameSpace + FSeparador + FxsdServico;
        end
        else
          FNameSpaceDad := xmlns3 + FNameSpace + '"';

        // Para Manaus
        if FPConfiguracoesNFSe.Geral.CodigoMunicipio = 1302603 then
          FPDFeOwner.SSL.NameSpaceURI := '';

      end;
    end;
  end
  else
    FNameSpaceDad := '';

  if FDefTipos <> '' then
    FNameSpaceDad := FNameSpaceDad + ' ' + xmlns4 + FNameSpace + FSeparador + FDefTipos + '"';

  if FNameSpaceDad <> '' then
    FNameSpaceDad := ' ' + FNameSpaceDad;

  FDadosSenha := DefinirDadosSenha(FPConfiguracoesNFSe.Geral.ConfigGeral.DadosSenha);
end;

procedure TNFSeWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrNFSe(FPDFeOwner).SetStatus(stNFSeIdle);
end;

function TNFSeWebService.RemoverCharControle(AXML: String): String;
begin
  if FPConfiguracoesNFSe.Geral.ConfigRemover.Tabulacao then
    AXML := StringReplace(AXML, '#9', '', [rfReplaceAll]);

  if FPConfiguracoesNFSe.Geral.ConfigRemover.QuebradeLinhaRetorno then
  begin
    AXML := StringReplace(AXML, #10, '', [rfReplaceAll]);
    AXML := StringReplace(AXML, #13, '', [rfReplaceAll]);

    AXML := StringReplace(AXML, '&#xD;', '', [rfReplaceAll]);
    AXML := StringReplace(AXML, '&#xd;', '', [rfReplaceAll]);
  end;

  if FPConfiguracoesNFSe.Geral.ConfigRemover.EComercial then
    AXML := StringReplace(AXML, '&amp;', '', [rfReplaceAll]);

  if FPConfiguracoesNFSe.Geral.ConfigRemover.TagQuebradeLinhaUnica then
    AXML := StringReplace(AXML, 'lt;brgt;', '', [rfReplaceAll]);

  Result := AXML;
end;

function TNFSeWebService.ExtrairRetorno(const GrupoMsgRet, AGrupo: String): String;
var
  AuxXML, XMLRet, aMsgRet: String;
begin
  // Alguns provedores retornam a resposta em String
  // Aplicado a conversão de String para XML
  FPRetornoWS := StringReplace(StringReplace(FPRetornoWS, '&lt;', '<', [rfReplaceAll]), '&gt;', '>', [rfReplaceAll]);

  FPRetornoWS := RemoverCharControle(FPRetornoWS);

  FPRetornoWS := RemoverDeclaracaoXML(FPRetornoWS);

  FPRetornoWS := RemoverIdentacao(FPRetornoWS);

  if (FProvedor in [proNFSeBrasil, proIPM]) then
    AuxXML := ParseText(FPRetornoWS, true, false)
  else
    AuxXML := ParseText(FPRetornoWS);

  if FPConfiguracoesNFSe.Geral.RetirarAcentos then
    AuxXML := TiraAcentos(AuxXML);

  if GrupoMsgRet <> '' then
    XMLRet := SeparaDados(AuxXML, GrupoMsgRet)
  else
  begin
    XMLRet := SeparaDados(AuxXML, 'return');

    if XMLRet = '' then
      XMLRet := SeparaDados(AuxXML, 'ns:return');

    if XMLRet = '' then
      XMLRet := SeparaDados(AuxXML, 'outputXML');

    if XMLRet = '' then
      XMLRet := SeparaDados(AuxXML, 'RetornoXML');

    if XMLRet = '' then
      XMLRet := SeparaDados(AuxXML, 's:Body');

    if XMLRet = '' then
      XMLRet := SeparaDados(AuxXML, 'soap:Body');

    if XMLRet = '' then
      XMLRet := SeparaDados(AuxXML, 'env:Body');

    if XMLRet = '' then
      XMLRet := SeparaDados(AuxXML, 'soapenv:Body');

    if XMLRet = '' then
      XMLRet := SeparaDados(AuxXML, 'SOAP-ENV:Body');
  end;

  // Caso não consiga extrai o retorno, retornar a resposta completa.
  if XMLRet = '' then
    XMLRet := AuxXML;

  // No retorno ao separar os dados do grupo de retorno pode conter uma
  // segunda Declaração XML que também deve ser removida.
  XMLRet := RemoverDeclaracaoXML(XMLRet);

  Result := XMLRet;

  // Remove do Retorno o conteudo de AGrupo
  if AGrupo <> '' then
  begin
    aMsgRet := SeparaDados(XMLRet, AGrupo);

    if aMsgRet <> '' then
      Result := aMsgRet;
  end;
end;

function TNFSeWebService.ExtrairNotasRetorno: Boolean;
var
  FRetNFSe, PathArq, NomeArq, xCNPJ: String;
  i, l, ii: Integer;
  xData: TDateTime;
  NovoRetorno, CondicaoNovoRetorno: Boolean;
  Alerta203, ProcSucesso: Boolean;
begin
  FRetornoNFSe := TRetornoNFSe.Create;

  FRetornoNFSe.Leitor.Arquivo := FPRetWS;
  FRetornoNFSe.Provedor       := FProvedor;
  FRetornoNFSe.TabServicosExt := FPConfiguracoesNFSe.Arquivos.TabServicosExt;
  FRetornoNFSe.PathIniCidades := FPConfiguracoesNFSe.Geral.PathIniCidades;
  FRetornoNFSe.LerXml;

  ii := 0;
  for i := 0 to FRetornoNFSe.ListaNFSe.CompNFSe.Count -1 do
  begin
    // O provedor EGoverneISS não retorna o XML da NFS-e esse é obtido pelo
    // Link retornado e atribuido a propriedade Link, bem como o numero da nota
    // que é atribuido a propriedade Numero
    if (FProvedor = proEGoverneISS) then
    begin
      FNotasFiscais.Items[0].NFSe.Autenticador := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Autenticador;
      FNotasFiscais.Items[0].NFSe.Link         := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Link;
      FNotasFiscais.Items[0].NFSe.Numero       := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Numero;

      break;
    end;

    if (FProvedor = proIPM) then
    begin
      FNotasFiscais.Items[0].NFSe.Autenticador      := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Autenticador;
      FNotasFiscais.Items[0].NFSe.Link              := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Link;
      FNotasFiscais.Items[0].NFSe.Numero            := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Numero;
      FNotasFiscais.Items[0].NFSe.dhRecebimento     := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.dhRecebimento;
      FNotasFiscais.Items[0].NFSe.CodigoVerificacao := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.CodigoVerificacao;
      FNotasFiscais.Items[0].NFSe.Protocolo         := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Protocolo;
      FNotasFiscais.Items[0].NFSe.Competencia       := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Competencia;
      FNotasFiscais.Items[0].NFSe.Cancelada         := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Cancelada;
      FNotasFiscais.Items[0].NFSe.Status            := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.Status;

      if (Trim(FXML_NFSe) <> '') then
      FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.XML := FXML_NFSe;
      FNotasFiscais.Items[0].NFSe.XML               := FRetornoNFSe.ListaNFSe.CompNFSe.Items[0].NFSe.XML;
      Break;
    end;

    // provedor CTA na consulta por lote somente retorna os dados do RPS
    if (FProvedor = proCTA) and (FPLayout = LayNfseConsultaLote) then
    begin
         // todo: tratar provedor CTA
    end;

    // Considerar o retorno sempre como novo, avaliar abaixo se o RPS está na lista
    NovoRetorno := True;
    for l := 0 to FNotasFiscais.Count -1 do
    begin
      // Provedor de goinaia em modo de homologação sempre retorna o mesmo dados
      if (FProvedor = proGoiania) and (FPConfiguracoes.WebServices.Ambiente = taHomologacao) then
      begin
        FNotasFiscais.Items[l].NFSe.IdentificacaoRps.Numero := '14';
        FNotasFiscais.Items[l].NFSe.IdentificacaoRps.Serie  := 'UNICA';
      end;

      // Se o RPS na lista de NFS-e consultado está na lista de FNotasFiscais, então atualiza os dados da mesma. A não existencia, implica em adcionar novo ponteiro em FNotasFiscais
      // foi alterado para testar o Numero, serie e tipo, pois o numero pode voltar ao terminar a seriação.
      if FProvedor in [proNFSeBrasil, proEL, proEquiplano] then
        // Se o provedor for NFSeBrasil ou EL compara apenas o numero do RPS
        CondicaoNovoRetorno := (StrToInt64Def(FNotasFiscais.Items[l].NFSe.IdentificacaoRps.Numero, 0) = StrToInt64Def(FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.IdentificacaoRps.Numero, 0))
      else
        // caso contrario compara se já esta adicionado comparando pelo número, série e (tipo ou InfId.ID)
        CondicaoNovoRetorno := (StrToInt64Def(FNotasFiscais.Items[l].NFSe.IdentificacaoRps.Numero, 0) = StrToInt64Def(FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.IdentificacaoRps.Numero, 0)) and
              (FNotasFiscais.Items[l].NFSe.IdentificacaoRps.Serie = FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.IdentificacaoRps.Serie) and
              ((FNotasFiscais.Items[l].NFSe.IdentificacaoRps.Tipo = FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.IdentificacaoRps.Tipo) or
              (FNotasFiscais.Items[l].NFSe.InfID.ID = FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.InfID.ID));

      if CondicaoNovoRetorno then
      begin
        NovoRetorno := False;
        ii := l;
        break;
      end;
    end;

    if NovoRetorno then
    begin
      FNotasFiscais.Add;
      ii := FNotasFiscais.Count -1;
    end;

    FNotasFiscais.Items[ii].Confirmada := True;

    FNotasFiscais.Items[ii].NFSe.XML := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.XML;

    FNotasFiscais.Items[ii].NFSe.InfID.ID := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.InfID.ID;

    // Retorno do GerarNfse e EnviarLoteRpsSincrono
    if FPLayout in [LayNFSeGerar, LayNFSeRecepcaoLoteSincrono] then
    begin
      FProtocolo := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Protocolo;

      if (Provedor = ProTecnos) then
      begin
        if (FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.NumeroLote <> '0') then
        begin
          FNotasFiscais.Items[ii].NFSe.NumeroLote    := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.NumeroLote;
          FNotasFiscais.Items[ii].NFSe.dhRecebimento := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.dhRecebimento;
          FNotasFiscais.Items[ii].NFSe.Protocolo     := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Protocolo;
        end;
      end
      else
      begin
        FNotasFiscais.Items[ii].NFSe.NumeroLote    := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.NumeroLote;
        FNotasFiscais.Items[ii].NFSe.dhRecebimento := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.dhRecebimento;
        FNotasFiscais.Items[ii].NFSe.Protocolo     := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Protocolo;
      end;
    end;

    // Retorno do GerarNfse e ConsultarLoteRps
    if (FPLayout in [LayNFSeGerar, LayNfseConsultaLote]) or ((FPLayout = LayNfseConsultaNfseRps) and (FProvedor = proISSNET)) then
    begin
      FNotasFiscais.Items[ii].NFSe.Situacao := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Situacao;
      FLoteNaoProc := (FNotasFiscais.Items[ii].NFSe.Situacao = '2');
    end;

    { Márcio - Como a questão do link é tratada no reader do XML, acho que aqui
      pode pegar direto, sem validar provedor }
    FNotasFiscais.Items[ii].NFSe.Link              := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Link;
    FNotasFiscais.Items[ii].NFSe.CodigoVerificacao := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.CodigoVerificacao;
    FNotasFiscais.Items[ii].NFSe.Numero            := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Numero;
    FNotasFiscais.Items[ii].NFSe.Competencia       := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Competencia;
    FNotasFiscais.Items[ii].NFSe.NFSeSubstituida   := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.NFSeSubstituida;
    FNotasFiscais.Items[ii].NFSe.OutrasInformacoes := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.OutrasInformacoes;
    FNotasFiscais.Items[ii].NFSe.DataEmissao       := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.DataEmissao;

    FNotasFiscais.Items[ii].NFSe.ValoresNfse.BaseCalculo      := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.ValoresNfse.BaseCalculo;
    FNotasFiscais.Items[ii].NFSe.ValoresNfse.Aliquota         := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.ValoresNfse.Aliquota;
    FNotasFiscais.Items[ii].NFSe.ValoresNfse.ValorIss         := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.ValoresNfse.ValorIss;
    FNotasFiscais.Items[ii].NFSe.ValoresNfse.ValorLiquidoNfse := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.ValoresNfse.ValorLiquidoNfse;

    FNotasFiscais.Items[ii].NFSe.Servico.xItemListaServico := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.xItemListaServico;

    FNotasFiscais.Items[ii].NFSe.PrestadorServico.RazaoSocial  := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.RazaoSocial;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.NomeFantasia := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.NomeFantasia;

    FNotasFiscais.Items[ii].NFSe.PrestadorServico.IdentificacaoPrestador.CNPJ               := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.IdentificacaoPrestador.CNPJ;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

    FNotasFiscais.Items[ii].NFSe.IdentificacaoRps.Tipo   := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.IdentificacaoRps.Tipo;
    FNotasFiscais.Items[ii].NFSe.IdentificacaoRps.Serie  := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.IdentificacaoRps.Serie;
    FNotasFiscais.Items[ii].NFSe.IdentificacaoRps.Numero := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.IdentificacaoRps.Numero;

    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.Endereco        := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Endereco.Endereco;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.Numero          := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Endereco.Numero;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.Complemento     := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Endereco.Complemento;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.Bairro          := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Endereco.Bairro;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.CodigoMunicipio := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Endereco.CodigoMunicipio;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.UF              := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Endereco.UF;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.CEP             := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Endereco.CEP;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Endereco.xMunicipio      := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Endereco.xMunicipio;

    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Contato.Telefone := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Contato.Telefone;
    FNotasFiscais.Items[ii].NFSe.PrestadorServico.Contato.Email    := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.Contato.Email;

    FNotasFiscais.Items[ii].NFSe.Tomador.IdentificacaoTomador.CpfCnpj            := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.IdentificacaoTomador.CpfCnpj;
    FNotasFiscais.Items[ii].NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.IdentificacaoTomador.InscricaoMunicipal;

    FNotasFiscais.Items[ii].NFSe.Tomador.RazaoSocial := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.RazaoSocial;

    FNotasFiscais.Items[ii].NFSe.Tomador.Endereco.Endereco        := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Endereco.Endereco;
    FNotasFiscais.Items[ii].NFSe.Tomador.Endereco.Numero          := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Endereco.Numero;
    FNotasFiscais.Items[ii].NFSe.Tomador.Endereco.Complemento     := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Endereco.Complemento;
    FNotasFiscais.Items[ii].NFSe.Tomador.Endereco.Bairro          := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Endereco.Bairro;
    FNotasFiscais.Items[ii].NFSe.Tomador.Endereco.xMunicipio      := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Endereco.xMunicipio;
    FNotasFiscais.Items[ii].NFSe.Tomador.Endereco.CodigoMunicipio := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Endereco.CodigoMunicipio;
    FNotasFiscais.Items[ii].NFSe.Tomador.Endereco.UF              := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Endereco.UF;
    FNotasFiscais.Items[ii].NFSe.Tomador.Endereco.CEP             := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Endereco.CEP;

    FNotasFiscais.Items[ii].NFSe.Tomador.Contato.Telefone := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Contato.Telefone;
    FNotasFiscais.Items[ii].NFSe.Tomador.Contato.Email    := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Tomador.Contato.Email;

    // Incluido em 13/10/2017
    FNotasFiscais.Items[ii].NFSe.Servico.ItemListaServico          := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.ItemListaServico;
    FNotasFiscais.Items[ii].NFSe.Servico.xItemListaServico         := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.xItemListaServico;
    FNotasFiscais.Items[ii].NFSe.Servico.CodigoCnae                := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.CodigoCnae;
    FNotasFiscais.Items[ii].NFSe.Servico.CodigoTributacaoMunicipio := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.CodigoTributacaoMunicipio;
    FNotasFiscais.Items[ii].NFSe.Servico.Discriminacao             := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Discriminacao;
    FNotasFiscais.Items[ii].NFSe.Servico.CodigoMunicipio           := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.CodigoMunicipio;
    FNotasFiscais.Items[ii].NFSe.Servico.ExigibilidadeISS          := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.ExigibilidadeISS;

    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorServicos          := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorServicos;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorDeducoes          := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorDeducoes;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorPis               := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorPis;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorCofins            := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorCofins;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorInss              := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorInss;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorIr                := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorIr;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorCsll              := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorCsll;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.IssRetido              := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.IssRetido;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorIss               := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorIss;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.OutrasRetencoes        := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.OutrasRetencoes;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.BaseCalculo            := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.BaseCalculo;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.Aliquota               := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.Aliquota;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorLiquidoNFSe       := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorLiquidoNFSe;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.ValorIssRetido         := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.ValorIssRetido;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.DescontoCondicionado   := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.DescontoCondicionado;
    FNotasFiscais.Items[ii].NFSe.Servico.Valores.DescontoIncondicionado := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Servico.Valores.DescontoIncondicionado;

    FNotasFiscais.Items[ii].NFSe.NfseCancelamento.DataHora := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.NfseCancelamento.DataHora;
    FNotasFiscais.Items[ii].NFSe.NfseCancelamento.Pedido.CodigoCancelamento := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.NfseCancelamento.Pedido.CodigoCancelamento;

    FNotasFiscais.Items[ii].NFSe.Cancelada := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Cancelada;
    FNotasFiscais.Items[ii].NFSe.Status    := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Status;

    FNotasFiscais.Items[ii].NFSe.NfseSubstituidora := FRetornoNFSe.ListaNfse.CompNfse.Items[i].NFSe.NfseSubstituidora;

    FRetNFSe := GerarRetornoNFSe(FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.XML);

    if FPConfiguracoesNFSe.Arquivos.EmissaoPathNFSe then
      xData := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.DataEmissao
    else
      xData := Date;

    xCNPJ := OnlyNumber(FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.PrestadorServico.IdentificacaoPrestador.CNPJ);

    if FPConfiguracoesNFSe.Arquivos.NomeLongoNFSe then
      NomeArq := GerarNomeNFSe(FPConfiguracoesNFSe.WebServices.UFCodigo,
                               FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.DataEmissao,
                               xCNPJ,
                               StrToInt64Def(FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Numero, 0)) + '-nfse.xml'
    else
      NomeArq := FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.Numero +
                 FRetornoNFSe.ListaNFSe.CompNFSe.Items[i].NFSe.IdentificacaoRps.Serie +
                 '-nfse.xml';

    PathArq := PathWithDelim(FPConfiguracoesNFSe.Arquivos.GetPathNFSe(xData, xCNPJ));

    FNotasFiscais.Items[ii].NomeArq := PathArq + NomeArq;
    FNotasFiscais.Items[ii].XMLNFSe := FRetNFSe;

    if FPConfiguracoesNFSe.Arquivos.Salvar then
      FPDFeOwner.Gravar(NomeArq, FRetNFSe, PathArq);

    inc(ii);
  end;

  // todo: tratar provedor CTA

  if FRetornoNFSe.ListaNFSe.CompNFSe.Count > 0 then
  begin
    FDataRecebimento := FRetornoNFSe.ListaNFSe.CompNFSe[0].NFSe.dhRecebimento;

    if FDataRecebimento = 0 then
      FDataRecebimento := FRetornoNFSe.ListaNFSe.CompNFSe[0].NFSe.DataEmissao;

    if (FProvedor = ProTecnos) and (DateToStr(FDataRecebimento) = '01/01/0001') then
      FDataRecebimento := 0;

    if FProvedor in [proGovDigital, proInfisc, proInfiscv11, proNFSeBrasil, proVersaTecnologia] then
      FProtocolo := FRetornoNFSe.ListaNFSe.CompNFSe[0].NFSe.Protocolo;

    { Márcio - O provedor Tecnos está no 'in' acima e eu retirei. Isso estava
      gerando problema, pois FRetornoNFSe.ListaNFSe.CompNFSe[0].NFSe.Protocolo
      estava vindo com '0' e como o protocolo é usado no nome do XML do resultado
      da consulta de lote, acabava que todas as consultas ficavam com o mesmo
      nome, pois o arquivo era sempre sobresvrevido. Temos clientes que usam
      esses aquivos, então tem que ficar com o nome do protocolo.

      O protocolo é usado no XML de envio da consulta, então não sei se existe
      chance de ele estar vazio, pois já no envio ele é necessário. Coloquei
      então uma validação específica para o provedor Tecnos, assim, se em outras
      consultas ele estiver vazio ele será preenchido.
      }
    if (FProtocolo = '') and (FProvedor in [proTecnos, proTiplanv2]) then
      FProtocolo := FRetornoNFSe.ListaNFSe.CompNFSe[0].NFSe.Protocolo;
  end
  else
    FDataRecebimento := 0;

  // Lista de Mensagem de Retorno
  FPMsg := '';
  FaMsg := '';
  Alerta203 := False;

  if FRetornoNFSe.ListaNFSe.MsgRetorno.Count > 0 then
  begin
    ProcSucesso := False;
    for i := 0 to FRetornoNFSe.ListaNFSe.MsgRetorno.Count - 1 do
    begin
      if (FRetornoNFSe.ListaNFSe.MsgRetorno.Items[i].Codigo <> 'L000') and
         (FRetornoNFSe.ListaNFSe.MsgRetorno.Items[i].Codigo <> 'A0000') then
      begin
        Alerta203 := (FRetornoNFSe.ListaNFSe.MsgRetorno.Items[i].Codigo = '203');

        FPMsg := FPMsg + FRetornoNFSe.ListaNFSe.MsgRetorno.Items[i].Mensagem + LineBreak +
                         FRetornoNFSe.ListaNFSe.MsgRetorno.Items[i].Correcao + LineBreak;

        FaMsg := FaMsg + 'Método..... : ' + LayOutToStr(FPLayout) + LineBreak +
                         'Código Erro : ' + FRetornoNFSe.ListaNFSe.MsgRetorno.Items[i].Codigo + LineBreak +
                         'Mensagem... : ' + FRetornoNFSe.ListaNFSe.MsgRetorno.Items[i].Mensagem + LineBreak +
                         'Correção... : ' + FRetornoNFSe.ListaNFSe.MsgRetorno.Items[i].Correcao + LineBreak +
                         'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
      end;
    end;
  end
  else begin
    ProcSucesso := True;
    if FRetornoNFSe.ListaNFSe.CompNFSe.Count > 0 then
    begin
      if FProvedor = proEgoverneISS then
        FaMsg := 'Método........ : ' + LayOutToStr(FPLayout) + LineBreak +
                 'Autenticador.. : ' + FRetornoNFSe.ListaNFSe.CompNFSe[0].NFSe.Autenticador + LineBreak +
                 'Link.......... : ' + FRetornoNFSe.ListaNFSe.CompNFSe[0].NFSe.Link + LineBreak +
                 'Numero........ : ' + FRetornoNFSe.ListaNFSe.CompNFSe[0].NFSe.Numero + LineBreak +
                 'Provedor...... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak
      else
        FaMsg := 'Método........ : ' + LayOutToStr(FPLayout) + LineBreak +
                 'Situação...... : ' + FRetornoNFSe.ListaNFSe.CompNFSe[0].NFSe.Situacao + LineBreak +
                 'Recebimento... : ' + IfThen(FDataRecebimento = 0, '', DateTimeToStr(FDataRecebimento)) + LineBreak +
                 'Protocolo..... : ' + FProtocolo + LineBreak +
                 'Provedor...... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
    end
    else
      FaMsg := 'Método........ : ' + LayOutToStr(FPLayout) + LineBreak +
               'Recebimento... : ' + IfThen(FDataRecebimento = 0, '', DateTimeToStr(FDataRecebimento)) + LineBreak +
               'Protocolo..... : ' + FProtocolo + LineBreak +
               'Provedor...... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
  end;

  case Fprovedor of
    proNotaBlu,
    proGiap: Result := (UpperCase(FRetornoNFSe.ListaNFSe.Sucesso) = UpperCase('true'));

    proISSDSF: Result := Alerta203 or (FDataRecebimento <> 0);

    proEgoverneISS: Result := ProcSucesso;
  else
    Result := (FDataRecebimento <> 0);
  end;
end;

function TNFSeWebService.GerarRetornoNFSe(const ARetNFSe: String): String;
var
  Texto: String;
begin
  Texto := FPConfiguracoesNFSe.Geral.ConfigGeral.RetornoNFSe;

  // %NomeURL_P% : Representa o Nome da cidade na URL
  // %DadosNFSe% : Representa a NFSe

  Texto := StringReplace(Texto, '%NomeURL_P%', FPConfiguracoesNFSe.Geral.xNomeURL_P, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%DadosNFSe%', ARetNFSe, [rfReplaceAll]);

  Result := Texto;
end;

procedure TNFSeWebService.DefinirSignatureNode(const TipoEnvio: String);
var
  TagGrupoTemp, xmlns, xPrefixo, Identificador: String;
  i, j: Integer;
begin
  TagGrupoTemp := RetirarPrefixos('<' + TipoEnvio, FProvedor);
  TagGrupoTemp := Copy(TagGrupoTemp, 2, Length(TagGrupoTemp));

  FxSignatureNode := '';
  FxDSIGNSLote := '';
  FxIdSignature := '';
  Identificador := FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador;

  case FPLayout of
    LayNFSeGerar,
    LayNfseRecepcaoLote,
    LayNfseRecepcaoLoteSincrono:
      begin
        if (FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS) or
           (FPConfiguracoesNFSe.Geral.ConfigAssinar.RpsGerar) then
        begin
          if (URI <> '') then
          begin
            if not (FProvedor in [proAbaco, proBetha, proFISSLex, proIssCuritiba,
                                  proPublica, proRecife, proRJ]) then
            begin
              FxSignatureNode := './/ds:Signature[@' + Identificador +
                                 '="AssLote_' + URI + '"]';

              FxIdSignature := ' ' + Identificador + '="AssLote_' + URI;
            end;
          end
          else
          begin
            if FPrefixo3 = '' then
            begin
              xPrefixo := 'ds1:';
              xmlns := ' xmlns="';
            end
            else begin
              xPrefixo := FPrefixo3;
              xmlns := ' xmlns:' + StringReplace(FPrefixo3, ':', '', []) + '="';
            end;

            if FProvedor in [proInfisc, proInfiscv11] then
            begin
              FxSignatureNode := './/' + 'ds:Signature';
              i := pos(TagGrupoTemp, FPDadosMsg);
              i := i + Length(TagGrupoTemp + xmlns);
            end
            else
            begin
              FxSignatureNode := './/' + xPrefixo + TagGrupoTemp + '/ds:Signature';
              i := pos(TagGrupoTemp + xmlns, FPDadosMsg);
              i := i + Length(TagGrupoTemp + xmlns) - 1;
            end;

            j := Pos('">', FPDadosMsg) + 1;

            if FProvedor = proIssDSF then
              FxDSIGNSLote := 'xmlns:' + StringReplace(xPrefixo, ':', '', []) + '=' +
                            '"' + Trim(FNameSpace) + '"'
            else
              FxDSIGNSLote := 'xmlns:' + StringReplace(xPrefixo, ':', '', []) + '=' +
                              Copy(FPDadosMsg, i, j - i);

            if FProvedor = proSigep then
              FxDSIGNSLote := 'xmlns:ds=';
          end;
        end;
      end;
     (*
    LayNfseCancelaNfse:
      begin
        if FPConfiguracoesNFSe.Geral.ConfigAssinar.Cancelar then
        begin
          if (URI <> '') then
          begin
            if not (FProvedor in [proGINFES {proAbaco, proBetha, proFISSLex, proIssCuritiba,
                                  proRecife, proRJ}]) then
            begin
              if FProvedor = proPublica then
                Identificador := 'Id';

              FxSignatureNode := './/ds:Signature[@' + Identificador +
                                 '="Ass_' + URI + '"]';

              FxIdSignature := ' ' + Identificador + '="Ass_' + URI + '"';
            end;
          end
          else
          begin
            if FPrefixo3 = '' then
            begin
              xPrefixo := 'ds1:';
              xmlns := ' xmlns="';
            end
            else begin
              xPrefixo := FPrefixo3;
              xmlns := ' xmlns:' + StringReplace(FPrefixo3, ':', '', []) + '="';
            end;

            FxSignatureNode := './/' + xPrefixo + TagGrupo + '/ds:Signature';

            i := pos(TagGrupo + xmlns, FPDadosMsg);
            i := i + Length(TagGrupo + xmlns) - 1;
            j := Pos('">', FPDadosMsg) + 1;

            FxDSIGNSLote := 'xmlns:' + StringReplace(xPrefixo, ':', '', []) + '=' +
                            Copy(FPDadosMsg, i, j - i);

            if FProvedor = proSigep then
              FxDSIGNSLote := 'xmlns:ds=';
          end;
        end;
      end;
      *)
  end;
end;

procedure TNFSeWebService.GerarLoteRPScomAssinatura(const RPS: String);
begin
  case FVersaoNFSe of
    // RPS versão 2.00
    ve200: case FProvedor of
             proTecnos: FvNotas := FvNotas +
                         '<' + FPrefixo4 + 'Rps>' +
                          '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                            RetornarConteudoEntre(RPS,
                              '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</Signature>') +
                            '</Signature>'+
                         '</' + FPrefixo4 + 'Rps>';

             proFriburgo: FvNotas := FvNotas +
                       '<' + FPrefixo4 + 'Rps>' +
                        '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico Id="' + RetornarConteudoEntre(RPS, '<Numero>', '</Numero>') + '"' +
                          RetornarConteudoEntre(RPS,
                            '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                          '</Signature>'+
                       '</' + FPrefixo4 + 'Rps>';

           else
             FvNotas := FvNotas +
                       '<' + FPrefixo4 + 'Rps>' +
                        '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                          RetornarConteudoEntre(RPS,
                            '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                          '</Signature>'+
                       '</' + FPrefixo4 + 'Rps>';
           end;

    // RPS versão 1.00
    else begin
      case FProvedor of
        proEgoverneISS: FvNotas := FvNotas +
                          '<rgm:NotaFiscal' +
                            RetornarConteudoEntre(RPS,
                              '<rgm:NotaFiscal', '</Signature>') +
                            '</Signature>' +
                         '</rgm:NotaFiscal>';

        proSMARAPD,
        proGiap,
        proIPM: FvNotas := RPS;
      else
        FvNotas := FvNotas +
                    '<' + FPrefixo4 + 'Rps>' +
                      '<' + FPrefixo4 + 'InfRps' +
                        RetornarConteudoEntre(RPS,
                          '<' + FPrefixo4 + 'InfRps', '</Signature>') +
                        '</Signature>'+
                    '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end;
end;

procedure TNFSeWebService.GerarLoteRPSsemAssinatura(const RPS: String);
var
  SRpsTmp: String;
begin
  case FVersaoNFSe of
    // RPS versão 2.00
    ve200: case FProvedor of
             proTecnos: FvNotas := FvNotas +
                         '<' + FPrefixo4 + 'Rps>' +
                          '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                            RetornarConteudoEntre(RPS,
                              '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>') +
                            '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>'+
                         '</' + FPrefixo4 + 'Rps>';

             proGiap,
             proCONAM: FvNotas := FvNotas + RPS;

             proAgiliv2: FvNotas := FvNotas +
                          '<' + FPrefixo4 + 'DeclaracaoPrestacaoServico' +
                            RetornarConteudoEntre(RPS,
                            '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                          '</' + FPrefixo4 + 'DeclaracaoPrestacaoServico>';

             proSigep: FvNotas := FvNotas +
                      '<' + FPrefixo4 + 'credenciais>' +
                          RetornarConteudoEntre(RPS,
                         '<' + FPrefixo4 + 'credenciais>' , '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                         '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                      '</' + FPrefixo4 + 'Rps>';
           else
             FvNotas := FvNotas +
                      '<' + FPrefixo4 + 'Rps>' +
                       '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                         RetornarConteudoEntre(RPS,
                           '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                         '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                      '</' + FPrefixo4 + 'Rps>';
           end;

    // RPS versão 1.00
    else
    begin
      case FProvedor of
        proAgili: FvNotas := FvNotas +
                   '<' + FPrefixo4 + 'DeclaracaoPrestacaoServico' +
                     RetornarConteudoEntre(RPS,
                     '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                   '</' + FPrefixo4 + 'DeclaracaoPrestacaoServico>';

        proEL,
        proGoverna: FvNotas :=  FvNotas + RPS;

        proCTA: FvNotas := FvNotas + '<RPS xmlns=""' +
                                      RetornarConteudoEntre(RPS, '<RPS', '</RPS>') +
                                     '</RPS>';

        proSP, 
        proNotaBlu: FvNotas :=  FvNotas + '<RPS xmlns=""' +
                                      RetornarConteudoEntre(RPS, '<RPS', '</RPS>') +
                                     '</RPS>';

        proInfisc,
        proInfiscv11: FvNotas := FvNotas +
                                '<NFS-e' +
                                  RetornarConteudoEntre(RPS, '<NFS-e', '</NFS-e>') +
                                '</NFS-e>';

        proIssDSF,
        proEquiplano: FvNotas :=  FvNotas + StringReplace(RPS, '<' + ENCODING_UTF8 + '>', '', [rfReplaceAll]);

        proEgoverneISS: FvNotas := FvNotas +
                                   '<rgm:NotaFiscal>' +
                                     RetornarConteudoEntre(RPS,
                                     '<rgm:NotaFiscal>', '</rgm:NotaFiscal>') +
                                   '</rgm:NotaFiscal>';

        proNFSeBrasil: begin
                         SRpsTmp := StringReplace(RPS, '</Rps>', '', [rfReplaceAll]);
                         SRpsTmp := StringReplace(SRpsTmp, '<Rps>', '', [rfReplaceAll]);
                         FvNotas := FvNotas + '<Rps>' + StringReplace(SRpsTmp, '<InfRps>', '', [rfReplaceAll]) + '</Rps>';
                       end;

        proSMARAPD,
        proIPM: FvNotas := RPS;

        proAssessorPublico: begin
                              SRpsTmp := StringReplace(RPS, '<' + ENCODING_UTF8 + '>', '', [rfReplaceAll]);
                              FvNotas := '<NFSE>';
                              FvNotas := FvNotas + '<IDENTIFICACAO>';
                              FvNotas := FvNotas + '<MESCOMP>'+FormatDateTime('MM',Now)+'</MESCOMP>';
                              FvNotas := FvNotas + '<ANOCOMP>'+FormatDateTime('yyyy',Now)+'</ANOCOMP>';
                              FvNotas := FvNotas + '<INSCRICAO>'+FPConfiguracoesNFSe.Geral.Emitente.InscMun+'</INSCRICAO>';
                              FvNotas := FvNotas + '<VERSAO>'+FVersaoXML+'</VERSAO>';
                              FvNotas := FvNotas + '</IDENTIFICACAO>';
                              FvNotas := FvNotas + '<NOTAS>';
                              FvNotas := FvNotas + SRpsTmp;
                              FvNotas := FvNotas + '</NOTAS>';
                              FvNotas := FvNotas + '</NFSE>';
                            end;
      else
        FvNotas := FvNotas +
                    '<' + FPrefixo4 + 'Rps>' +
                     '<' + FPrefixo4 + 'InfRps' +
                       RetornarConteudoEntre(RPS,
                         '<' + FPrefixo4 + 'InfRps', '</Rps>') +
                    '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end;
end;

procedure TNFSeWebService.InicializarTagITagF;
begin
  // Inicializa a TagI
  case FPLayout of
    LayNfseRecepcaoLote:
       begin
         case FProvedor of
           proAbaco: begin
                       // Manaus
                       if (FPConfiguracoesNFSe.Geral.CodigoMunicipio = 1302603) then
                         FTagI := '<'+FTagGrupo+'>'
                       else // Outros
                         FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
                     end;

           proABase: FTagI := '<' + FTagGrupo + FNameSpaceDad +
                                ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';

           proEquiplano: FTagI := '<' + FTagGrupo +
                                    ' xmlns:es="http://www.equiplano.com.br/esnfs" ' +
                                    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                                    'xsi:schemaLocation="http://www.equiplano.com.br/enfs esRecepcionarLoteRpsEnvio_v01.xsd">';

           proCONAM,
           proEL,
           proFISSLex,
           proGiap,
           proSimplISS: FTagI := '<' + FTagGrupo + '>';

           proCTA: FTagI := '<' + FTagGrupo + ' xmlns:ns1="http://localhost:8080/WsNFe2/lote" '+
                                    'xmlns:tipos="http://localhost:8080/WsNFe2/tp" '+
                                    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" '+
                                    'xsi:schemaLocation="http://localhost:8080/WsNFe2/lote '+
                                    'http://localhost:8080/WsNFe2/xsd/ReqEnvioLoteRPS.xsd">';

           proSaatri: FTagI := '<' + FTagGrupo + FNameSpaceDad +
                                ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                                ' xmsns:xsd="http://www.w3.org/2001/XMLSchema">';

           proAssessorPublico,
           proGoverna,
           proInfisc,
           proInfiscv11,
           proIPM,
           proSMARAPD: FTagI := '';
         else
           FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
         end;
       end;

    LayNfseConsultaSitLoteRps:
       begin
         case FProvedor of
           proAbaco: begin
                       // Manaus
                       if (FPConfiguracoesNFSe.Geral.CodigoMunicipio = 1302603) then
                         FTagI := '<'+FTagGrupo+'>'
                       else // Outros
                         FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
                     end;

           proEquiplano: FTagI := '<' + FTagGrupo +
                                    ' xmlns:es="http://www.equiplano.com.br/esnfs" ' +
                                    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                                    'xsi:schemaLocation="http://www.equiplano.com.br/enfs esConsultarSituacaoLoteRpsEnvio_v01.xsd">';

           proEL,
           proInfisc,
           proInfiscv11,
           proTinus,
           proSimplISS: FTagI := '<' + FTagGrupo + '>';

           proSP: FTagI := '<' + FTagGrupo +
                             ' xmlns="http://www.prefeitura.sp.gov.br/nfe">';

           proNotaBlu: FTagI := '<' + FTagGrupo +
                             ' xmlns="http://nfse.blumenau.sc.gov.br" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';

//           proNotaBlu: FTagI := '<' + FTagGrupo +
//                             ' xmlns:p1="http://nfse.blumenau.sc.gov.br" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';

           proAssessorPublico,
           proFISSLex,
           proIPM,
           proGiap,
           proSMARAPD: FTagI := '';

         else
           FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
         end;
       end;

    LayNfseConsultaLote:
       begin
         case FProvedor of
           proAbaco: begin
                       // Manaus
                       if (FPConfiguracoesNFSe.Geral.CodigoMunicipio = 1302603) then
                         FTagI := '<'+FTagGrupo+'>'
                       else // Outros
                         FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
                     end;

           proAgili: FTagI := '<' + FTagGrupo + FNameSpaceDad + '>' +
                               '<UnidadeGestora>' +
                               OnlyNumber(FPConfiguracoesNFSe.Geral.CNPJPrefeitura) +
                               '</UnidadeGestora>';

//           proAgiliv2: FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';

           proEquiplano: FTagI := '<' + FTagGrupo +
                                    ' xmlns:es="http://www.equiplano.com.br/esnfs" ' +
                                    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                                    'xsi:schemaLocation="http://www.equiplano.com.br/enfs esConsultarLoteRpsEnvio_v01.xsd">';

           proEL,
           proTinus,
           proSimplISS: FTagI := '<' + FTagGrupo + '>';

           proSP: FTagI := '<' + FTagGrupo +
                             ' xmlns="http://www.prefeitura.sp.gov.br/nfe">';

           proNotaBlu: FTagI := '<' + FTagGrupo +
                             ' xmlns="http://nfse.blumenau.sc.gov.br" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';

//           proNotaBlu: FTagI := '<' + FTagGrupo +
//                             ' xmlns:p1="http://nfse.blumenau.sc.gov.br" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';

           proAssessorPublico,
           proFISSLex,
           proIPM,
           proGiap,
           proSMARAPD: FTagI := '';

         else
           FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
         end;
       end;

    LayNfseConsultaNfseRps:
       begin
         case FProvedor of
           proAbaco: begin
                       // Manaus
                       if (FPConfiguracoesNFSe.Geral.CodigoMunicipio = 1302603) then
                         FTagI := '<'+FTagGrupo+'>'
                       else // Outros
                         FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
                     end;

           proDBSeller: FTagI := '<ConsultarNfsePorRps>' +
                                  '<' + FTagGrupo + FNameSpaceDad + '>';

           proEquiplano: FTagI := '<' + FTagGrupo +
                                    ' xmlns:es="http://www.equiplano.com.br/esnfs" ' +
                                    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                                    'xsi:schemaLocation="http://www.equiplano.com.br/enfs esConsultarNfsePorRpsEnvio_v01.xsd">';

           proEL,
           proTinus,
           proSimplISS: FTagI := '<' + FTagGrupo + '>';

           proSP: FTagI := '<' + FTagGrupo +
                             ' xmlns="http://www.prefeitura.sp.gov.br/nfe">';

//           proSP: FTagI := '<' + FTagGrupo +
//                             ' xmlns:p1="http://www.prefeitura.sp.gov.br/nfe" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';

           proNotaBlu: FTagI := '<' + FTagGrupo +
                             ' xmlns="http://nfse.blumenau.sc.gov.br" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';

//           proNotaBlu: FTagI := '<' + FTagGrupo +
//                             ' xmlns:p1="http://nfse.blumenau.sc.gov.br" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';

           proAssessorPublico,
           proGoverna,
           proFISSLex,
           proIPM,
           proGiap,
           proSMARAPD: FTagI := '';

         else
           FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
         end;
       end;

    LayNfseConsultaNfse:
       begin
         case FProvedor of
           proAbaco: begin
                       // Manaus
                       if (FPConfiguracoesNFSe.Geral.CodigoMunicipio = 1302603) then
                         FTagI := '<'+FTagGrupo+'>'
                       else // Outros
                         FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
                     end;

           proEL,
           proInfisc,
           proInfiscv11,
           proSimplISS,
           proSP, 
           proTinus,
           proNotaBlu: FTagI := '<' + FTagGrupo + '>';

           proAssessorPublico,
           proFISSLex,
           proIPM,
           proGiap,
           proSMARAPD: FTagI := '';
         else
           FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
         end;
       end;

    LayNfseCancelaNfse:
       begin
         FNameSpaceCan := '';

         case FProvedor of
           proAbaco: begin
                       // Manaus
                       if (FPConfiguracoesNFSe.Geral.CodigoMunicipio = 1302603) then
                         FTagI := '<'+FTagGrupo+'>'
                       else // Outros
                         FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
                     end;

           proAgili: FTagI := '<' + FTagGrupo + FNameSpaceDad + '>' +
                               '<UnidadeGestora>' +
                                 OnlyNumber(FPConfiguracoesNFSe.Geral.CNPJPrefeitura) +
                               '</UnidadeGestora>';

           proEquiplano: FTagI := '<' + FTagGrupo +
                                    ' xmlns:es="http://www.equiplano.com.br/esnfs" ' +
                                    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" ' +
                                    'xsi:schemaLocation="http://www.equiplano.com.br/enfs esCancelarNfseEnvio_v01.xsd">';

           proGinfes:begin
                       FNameSpaceCan := ' xmlns="http://www.ginfes.com.br/servico_cancelar_nfse_envio"' +
                                 ' xmlns:ns4="http://www.ginfes.com.br/tipos"';

                       FTagI := '<' + FTagGrupo + FNameSpaceCan + '>';
                     end;

           proCONAM,
           proEL,
           proInfisc,
           proInfiscv11,
           proPronimv2,
           proTinus,
           proSimplISS: FTagI := '<' + FTagGrupo + '>';

           proSP: FTagI := '<' + FTagGrupo +
                             ' xmlns="http://www.prefeitura.sp.gov.br/nfe">';

//           proSP,
//           proNotaBlu: FTagI := '<' + FTagGrupo + FNameSpaceDad +
//                             ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">';

           proISSNet: begin
                        FNameSpaceCan := ' xmlns:p1="http://www.issnetonline.com.br/webserviceabrasf/vsd/servico_cancelar_nfse_envio.xsd"' +
                                         ' xmlns:tc="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_complexos.xsd"' +
                                         ' xmlns:ts="http://www.issnetonline.com.br/webserviceabrasf/vsd/tipos_simples.xsd"';

                        FTagI := '<p1:' + FTagGrupo + FNameSpaceCan + '>';
                      end;

           proAssessorPublico,
           proBetha,
           proGoverna,
           proSMARAPD,
           proGiap,
           proIPM: FTagI := '';

         else
           begin
             FNameSpaceCan := FNameSpaceDad;
             FTagI := '<' + FTagGrupo + FNameSpaceCan + '>';
           end;
         end;

         if FProvedor in [proDBSeller] then
           FTagI := '<CancelarNfse>' + FTagI;
       end;

    LayNfseGerar:
       begin
         case FProvedor of
     //      proEGoverneISS: FTagI := '<' + FTagGrupo + ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
     //                                                ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';

//           proRecife:
//             FTagI := '<' + FTagGrupo + //FNameSpaceDad +
//                      ' xmlns="http://nfse.recife.pe.gov.br/WSNacional/XSD/1/nfse_recife_v01.xsd"' +
//                      '>';

           proEGoverneISS,
           proTinus,
           proSimplISS: FTagI := '<' + FTagGrupo + '>';

           proAssessorPublico,
           proSMARAPD,
           proGiap,
           proIPM: FTagI := '';
         else
           FTagI := '<' + FTagGrupo + FNameSpaceDad + '>';
         end;
       end;

    LayNfseRecepcaoLoteSincrono:
       begin
         case FProvedor of
           proAssessorPublico:
             FTagI := '';
         else
           FTagI := '<' + FPrefixo3 + 'EnviarLoteRpsSincronoEnvio' + FNameSpaceDad + '>';
         end;	 
       end;

    LayNfseSubstituiNfse:
       begin
         case FProvedor of
           proAgili: FTagI := '<' + FPrefixo3 + 'SubstituirNfseEnvio' + FNameSpaceDad + '>' +
                               '<UnidadeGestora>' +
                                OnlyNumber(FPConfiguracoesNFSe.Geral.CNPJPrefeitura) +
                               '</UnidadeGestora>'{ +
                              '<' + FPrefixo4 + 'PedidoCancelamento' +
                               ifThen(FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador <> '', ' ' +
                                      FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador + '="' + FURI + '"', '') + '>'};

           proAgiliv2: FTagI := '<' + FPrefixo3 + 'SubstituirNfseEnvio' + FNameSpaceDad + '>'{ +
                                '<' + FPrefixo4 + 'PedidoCancelamento' +
                                 ifThen(FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador <> '', ' ' +
                                        FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador + '="' + FURI + '"', '') + '>'};

           proAssessorPublico,
           proSMARAPD,
           proGiap,
           proIPM: FTagI := '';
         else begin
                FTagI := '<' + FPrefixo3 + 'SubstituirNfseEnvio' + FNameSpaceDad + '>' +
                          '<' + FPrefixo3 + 'SubstituicaoNfse>'{ +
                           '<' + FPrefixo3 + 'Pedido>' +
                            '<' + FPrefixo4 + 'InfPedidoCancelamento' +
                              ifThen(FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador <> '', ' ' +
                                     FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador + '="' + FURI + '"', '') + '>'};
              end;
         end;
       end;

    LayNfseAbrirSessao:
       begin
         FTagI := '';
       end;

    LayNfseFecharSessao:
       begin
         FTagI := '';
       end;
  end;

  // Inicializa a TagF
  case FPLayout of
    LayNfseRecepcaoLote:
       begin
         case FProvedor of
           proAssessorPublico,
           proInfisc,
           proInfiscv11,
           proGoverna,
           proIPM,
           proSMARAPD: FTagF := '';
         else
           FTagF := '</' + FTagGrupo + '>';
         end;
       end;

    LayNfseConsultaSitLoteRps:
       begin
         FTagF := '</' + FTagGrupo + '>';

         if FProvedor in [proAssessorPublico, proFISSLex, proSMARAPD, proIPM, proGiap] then
           FTagF := '';
       end;

    LayNfseConsultaLote:
       begin
         FTagF := '</' + FTagGrupo + '>';

         if FProvedor in [proAssessorPublico, proFISSLex, proSMARAPD, proIPM, proGiap] then
           FTagF := '';
       end;

    LayNfseConsultaNfseRps:
       begin
         FTagF := '</' + FTagGrupo + '>';

         if FProvedor in [proDBSeller] then
           FTagF := FTagF + '</ConsultarNfsePorRps>';

         if FProvedor in [proAssessorPublico, proGoverna, proFISSLex, proSMARAPD, proIPM, proGiap] then
           FTagF := '';
       end;

    LayNfseConsultaNfse:
       begin
         FTagF := '</' + FTagGrupo + '>';

         if FProvedor in [proAssessorPublico, proFISSLex, proSMARAPD, proIPM, proGiap] then
           FTagF := '';
       end;

    LayNfseCancelaNfse:
       begin
         case FProvedor of
           proAssessorPublico,
           proBetha,
           proGoverna,
           proIPM,
           proGiap,
           proSMARAPD: FTagF := '';

           proISSNet: FTagF := '</p1:' + FTagGrupo + '>';
         else
           FTagF := '</' + FTagGrupo + '>';
         end;

         if FProvedor in [proDBSeller] then
           FTagF := FTagF + '</CancelarNfse>';
       end;

    LayNfseGerar:
       begin
         case FProvedor of
           proAssessorPublico,
           proIPM: FTagF := '';
         else
           FTagF := '</' + FTagGrupo + '>';
         end;
       end;

    LayNfseRecepcaoLoteSincrono:
       begin
         if FProvedor = proAssessorPublico then
           FTagF := ''
         else
           FTagF := '</' + FPrefixo3 + 'EnviarLoteRpsSincronoEnvio>';
       end;

    LayNfseSubstituiNfse:
       begin
         case FProvedor of
           proAgili,
           proAgiliv2: FTagF := '</' + FPrefixo3 + 'SubstituirNfseEnvio>';

           proAssessorPublico,
           proSMARAPD,
           proGiap,
           proIPM: FTagF := '';
         else
           FTagF :=  '</' + FPrefixo3 + 'SubstituicaoNfse>' +
                    '</' + FPrefixo3 + 'SubstituirNfseEnvio>';
         end;
       end;

    LayNfseAbrirSessao:
       begin
         FTagF := '';
       end;

    LayNfseFecharSessao:
       begin
         FTagF := '';
       end;  
  end;
end;

procedure TNFSeWebService.InicializarGerarDadosMsg;
begin
  with GerarDadosMsg do
  begin
    Provedor      := FProvedor;
    VersaoNFSe    := FVersaoNFSe;
    Prefixo3      := FPrefixo3;
    Prefixo4      := FPrefixo4;
    NameSpaceDad  := FNameSpaceDad;
    VersaoXML     := FVersaoXML;
    CodMunicipio  := FPConfiguracoesNFSe.Geral.CodigoMunicipio;
    Identificador := FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador;
    VersaoDados   := FPConfiguracoesNFSe.Geral.ConfigXML.VersaoDados;
    IdCanc        := FURI; // URI de Cancelamento

    CNPJPrefeitura := OnlyNumber(FPConfiguracoesNFSe.Geral.CNPJPrefeitura);

    // Dados do Emitente
    CNPJ := FPConfiguracoesNFSe.Geral.Emitente.CNPJ;
    if CNPJ = '' then
      GerarException(ACBrStr('O CNPJ não informado em: Configuracoes.Geral.Emitente.CNPJ'));
    IM := FPConfiguracoesNFSe.Geral.Emitente.InscMun;
    if (IM = '') and (not (Provedor in [proBetha, proBethav2])) then
      GerarException(ACBrStr('A I.M. não informada em: Configuracoes.Geral.Emitente.InscMun'));
    RazaoSocial := FPConfiguracoesNFSe.Geral.Emitente.RazSocial;
    if RazaoSocial = '' then
      GerarException(ACBrStr('A Razão Social não informada em: Configuracoes.Geral.Emitente.RazSocial'));

    UserWeb := Trim(FPConfiguracoesNFSe.Geral.Emitente.WebUser);
    if UserWeb = '' then
      UserWeb := Trim(FPConfiguracoesNFSe.Geral.UserWeb);
    if (UserWeb = '') and (Provedor in [proCONAM]) then
      GerarException(ACBrStr('O provedor ' + FPConfiguracoesNFSe.Geral.xProvedor +
        ' necessita que a propriedade: Configuracoes.Geral.Emitente.WebUser seja informada.'));

    SenhaWeb := Trim(FPConfiguracoesNFSe.Geral.Emitente.WebSenha);
    if SenhaWeb = '' then
      SenhaWeb := Trim(FPConfiguracoesNFSe.Geral.SenhaWeb);
    if (SenhaWeb = '') and (Provedor in [proCONAM, proEL, proISSDigital]) then
      GerarException(ACBrStr('O provedor ' + FPConfiguracoesNFSe.Geral.xProvedor +
        ' necessita que a propriedade: Configuracoes.Geral.Emitente.WebSenha seja informada.'));

    FraseSecreta := Trim(FPConfiguracoesNFSe.Geral.Emitente.WebFraseSecr);
    if (FraseSecreta = '') and (Provedor in [proISSDigital]) then
      GerarException(ACBrStr('O provedor ' + FPConfiguracoesNFSe.Geral.xProvedor +
        ' necessita que a propriedade: Configuracoes.Geral.Emitente.WebFraseSecr seja informada.'));

    // Agili, Agiliv2, CTA, Governa, proEGoverneISS
    ChaveAcessoPrefeitura := FPConfiguracoesNFSe.Geral.Emitente.WebChaveAcesso;
    if (ChaveAcessoPrefeitura = '') and
       (Provedor in [proAgili, proAgiliv2, proCTA, proGoverna, proEgoverneISS,
                     proGiap, proiiBrasilv2]) then
      GerarException(ACBrStr('O provedor ' + FPConfiguracoesNFSe.Geral.xProvedor +
        ' necessita que a propriedade: Configuracoes.Geral.Emitente.WebChaveAcesso seja informada.'));
  end;
end;

function TNFSeWebService.ExtrairGrupoMsgRet(const AGrupo: String): String;
var
  aMsgRet: String;
begin
  Result := FPRetWS;

  if AGrupo <> '' then
  begin
    aMsgRet := SeparaDados(FPRetWS, AGrupo);

    if aMsgRet <> '' then
      Result := aMsgRet;
  end;
end;

procedure TNFSeWebService.IncluirEncoding(Incluir: Boolean);
begin
  FPDadosMsg := StringReplace(FPDadosMsg, '<' + ENCODING_UTF8 + '>', '', [rfReplaceAll]);
  if Incluir then
    FPDadosMsg := '<' + ENCODING_UTF8 + '>' + FPDadosMsg;
end;

function TNFSeWebService.DefinirDadosSenha(ATexto: String): String;
var
  i: Integer;
  UsuarioWeb, SenhaWeb: string;
begin
  UsuarioWeb := Trim(FPConfiguracoesNFSe.Geral.Emitente.WebUser);
  if UsuarioWeb = '' then
    UsuarioWeb := Trim(FPConfiguracoesNFSe.Geral.UserWeb);

  SenhaWeb := Trim(FPConfiguracoesNFSe.Geral.Emitente.WebSenha);
  if SenhaWeb = '' then
    SenhaWeb := Trim(FPConfiguracoesNFSe.Geral.SenhaWeb);

  if (UsuarioWeb = '') and
     (FProvedor in [proCONAM, proFiorilli, proEL, proIPM, proNFSeBrasil, proSafeWeb,
                    proSaatri, proSMARAPD, proSimplISS]) then
    GerarException(ACBrStr('O provedor ' + FPConfiguracoesNFSe.Geral.xProvedor +
      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WebUser seja informada.'));

  // %Usuario% : Representa o nome do usuário ou CNPJ
  // %Senha%   : Representa a senha do usuário
  ATexto := StringReplace(ATexto, '%Usuario%', UsuarioWeb, [rfReplaceAll]);

  if (SenhaWeb = '') and
     (FProvedor in [proCONAM, proFiorilli, proEL, proIPM,
                    proNFSeBrasil, proSaatri, proSMARAPD, proSimplISS]) then
    GerarException(ACBrStr('O provedor ' + FPConfiguracoesNFSe.Geral.xProvedor +
      ' necessita que a propriedade: Configuracoes.Geral.Emitente.WebSenha seja informada.'));

  // Fazer o parse da senha, pois pode ter caracteres especiais
  case FProvedor of
    proSimplISS: ATexto := StringReplace(ATexto, '%Senha%', ParseText(SenhaWeb, False), [rfReplaceAll]);

    proSMARAPD:  ATexto := StringReplace(ATexto, '%Senha%', EncodeBase64(SHA1(SenhaWeb)) , [rfReplaceAll]);

    proIPM:      ATexto := StringReplace(ATexto, '%Senha%', ParseText(SenhaWeb, False), [rfReplaceAll]);
  else
    ATexto := StringReplace(ATexto, '%Senha%', SenhaWeb, [rfReplaceAll]);
  end;

  case FProvedor of
    proDataSmart:
      begin
        if FPConfiguracoesNFSe.WebServices.Ambiente = taProducao then
          ATexto := StringReplace(ATexto, '%Municipio%', FPConfiguracoesNFSe.Geral.Banco_P, [rfReplaceAll])
        else
          ATexto := StringReplace(ATexto, '%Municipio%', FPConfiguracoesNFSe.Geral.Banco_H, [rfReplaceAll]);
      end
  else
    ATexto := StringReplace(ATexto, '%Municipio%', IntToStr(FPConfiguracoesNFSe.Geral.CodigoMunicipio), [rfReplaceAll]);
  end;

  ATexto := StringReplace(ATexto, '%WebChaveAcesso%', FPConfiguracoesNFSe.Geral.Emitente.WebChaveAcesso, [rfReplaceAll]);

  // Parâmetros personalizados
  for i := 0 to FPConfiguracoesNFSe.Geral.Emitente.DadosSenhaParams.Count - 1 do
    ATexto := StringReplace(ATexto,
                            '%' + FPConfiguracoesNFSe.Geral.Emitente.DadosSenhaParams[i].Param + '%',
                            FPConfiguracoesNFSe.Geral.Emitente.DadosSenhaParams[i].Conteudo,
                            [rfReplaceAll]);

  Result := ATexto;
end;

{ TNFSeGerarLoteRPS }

constructor TNFSeGerarLoteRPS.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner, ANotasFiscais);
end;

destructor TNFSeGerarLoteRPS.Destroy;
begin
  inherited Destroy;
end;

procedure TNFSeGerarLoteRPS.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNFSeRecepcaoLote;
  FPArqEnv := 'lot-rps';
  FPArqResp := ''; // O lote é apenas gerado não há retorno de envio.
end;

procedure TNFSeGerarLoteRPS.DefinirURL;
begin
  inherited DefinirURL;
end;

procedure TNFSeGerarLoteRPS.DefinirDadosMsg;
begin
  TNFSeEnviarLoteRPS(Self).FNumeroLote := NumeroLote;
  TNFSeEnviarLoteRPS(Self).FqMaxRps := qMaxRps;

  inherited DefinirDadosMsg;

  if FSincrono then
    FPDadosMsg := StringReplace(FPDadosMsg, 'EnviarLoteRpsEnvio',
                            'EnviarLoteRpsSincronoEnvio', [rfReplaceAll]);
end;

function TNFSeGerarLoteRPS.TratarResposta: Boolean;
begin
  FPMsg := '';
  FaMsg := '';
  FNotasFiscais.Items[0].NomeArq := FPConfiguracoes.Arquivos.PathSalvar +
                                  GerarPrefixoArquivo + '-' + FPArqEnv + '.xml';
  Result := True;
end;

procedure TNFSeGerarLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeGerarLoteRPS.GerarMsgLog: String;
begin
  Result := inherited GerarMsgLog;
end;

function TNFSeGerarLoteRPS.GerarPrefixoArquivo: String;
begin
  Result := inherited GerarPrefixoArquivo;
end;

function TNFSeGerarLoteRPS.Executar: Boolean;
begin
  InicializarServico;
  try
    DefinirDadosMsg;
    DefinirEnvelopeSoap;
    SalvarEnvio;
    TratarResposta;
  finally
    FinalizarServico;
  end;
  Result := True;
end;

{ TNFSeEnviarLoteRPS }

constructor TNFSeEnviarLoteRPS.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeEnviarLoteRPS.Destroy;
begin
  if Assigned(FRetornoNFSe) then
    FRetornoNFSe.Free;

  if Assigned(FRetEnvLote) then
    FRetEnvLote.Free;

  inherited Destroy;
end;

procedure TNFSeEnviarLoteRPS.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNFSeRecepcaoLote;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

  FProtocolo := '';
  
  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeEnviarLoteRPS.DefinirURL;
begin
  FPLayout := LayNFSeRecepcaoLote;
  
  inherited DefinirURL;
end;

procedure TNFSeEnviarLoteRPS.DefinirServicoEAction;
begin
  FPServico := 'EnviarLoteRPS';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.Recepcionar;

  inherited DefinirServicoEAction;
end;

procedure TNFSeEnviarLoteRPS.DefinirDadosMsg;
var
  I, iTributos: Integer;
  dDataInicial, dDataFinal: TDateTime;
  TotalServicos, TotalDeducoes, TotalISS,
  TotalTributos, TotalISSRetido: Double;
begin
  if FNotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if FNotasFiscais.Count > qMaxRps then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de ' +
      IntToStr(qMaxRps) + ' RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(FNotasFiscais.Count)));

  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    if (FTagElemento <> '') and not (Provedor in [proBetha, proIssDSF, proCTA]) then
      FTagElemento := FPrefixo3 + FTagElemento;

    if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
    begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPScomAssinatura(FNotasFiscais.Items[I].XMLAssinado);
    end
    else begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPSsemAssinatura(FNotasFiscais.Items[I].XMLOriginal);
    end;

    InicializarTagITagF;

    dDataInicial   := FNotasFiscais.Items[0].NFSe.DataEmissao;
    dDataFinal     := dDataInicial;
    iTributos      := 0;
    TotalServicos  := 0.0;
    TotalDeducoes  := 0.0;
    TotalISS       := 0.0;
    TotalISSRetido := 0.0;
    TotalTributos  := 0.0;

    for i := 0 to FNotasFiscais.Count-1 do
    begin
      if FNotasFiscais.Items[i].NFSe.DataEmissao < dDataInicial then
        dDataInicial := FNotasFiscais.Items[i].NFSe.DataEmissao;
      if FNotasFiscais.Items[i].NFSe.DataEmissao > dDataFinal then
        dDataFinal := FNotasFiscais.Items[i].NFSe.DataEmissao;

      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaPis > 0 then
        iTributos := iTributos + 1;
      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaCofins > 0 then
        iTributos := iTributos + 1;
      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaCsll > 0 then
        iTributos := iTributos + 1;
      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaInss > 0 then
        iTributos := iTributos + 1;
      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaIr > 0 then
        iTributos := iTributos + 1;

      TotalServicos  := TotalServicos + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorServicos;
      TotalDeducoes  := TotalDeducoes + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorDeducoes;
      TotalISS       := TotalISS + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorIss;
      TotalISSRetido := TotalISSRetido + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorIssRetido;
      TotalTributos  := TotalTributos + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorIr
                                      + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorCofins
                                      + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorPis
                                      + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorInss
                                      + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorCsll;
    end;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      NumeroLote := FNumeroLote;
      QtdeNotas  := FNotasFiscais.Count;
      Notas      := FvNotas;

      // Necessário para o provedor ISSDSF
      Transacao   := FNotasFiscais.Transacao;
      DataInicial := dDataInicial;
      DataFinal   := dDataFinal;

      ValorTotalServicos := TotalServicos;
      ValorTotalDeducoes := TotalDeducoes;

      // Necessário para o provedor Equiplano - EL
      NumeroRps      := FNotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero;
      SerieRps       := FNotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie;
      OptanteSimples := FNotasFiscais.Items[0].NFSe.OptanteSimplesNacional;

      if FProvedor = proCTA then
        ChaveAcessoPrefeitura := FPConfiguracoesNFSe.Geral.Emitente.WebChaveAcesso
      else // Necessário para o provedor Governa
        ChaveAcessoPrefeitura := FNotasFiscais.Items[0].NFSe.Prestador.ChaveAcesso;

      if FProvedor = proCONAM then
      begin
        AliquotaSN     := FNotasFiscais.Items[0].NFSe.Servico.Valores.AliquotaSN;
        AliquotaIss    := FNotasFiscais.Items[0].NFSe.Servico.Valores.Aliquota;
        TipoTributacao := '4';
        QtdTributos    := iTributos;
        ValorNota      := TotalServicos;
        ValorIss       := TotalIss;
        ValorIssRetido := TotalIssRetido;
        ValorTotalDeducoes := TotalDeducoes;
        ValorTotalTributos := TotalTributos;
        {Todo:// Acrescentados estas duas linhas abaixo por masl}
        ExigibilidadeISS := FNotasFiscais.Items[0].NFSe.Servico.ExigibilidadeISS;
        DataOptanteSimples := FNotasFiscais.Items[0].NFSe.DataOptanteSimplesNacional;
      end;
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    case Provedor of
      proEL:
        begin
          FPDadosMsg := GerarDadosMsg.Gera_DadosMsgEnviarLote;
          FPDadosMsg := StringReplace(FPDadosMsg, '<' + ENCODING_UTF8 + '>', '', [rfReplaceAll]);
          FPDadosMsg := FTagI +
                         '<identificacaoPrestador>' +
                           FPConfiguracoesNFSe.Geral.Emitente.CNPJ +
                         '</identificacaoPrestador>' +
                         '<hashIdentificador>' +
                           FHashIdent +
                         '</hashIdentificador>' +
                         '<arquivo>' +
                           StringReplace(StringReplace(FPDadosMsg, '<', '&lt;', [rfReplaceAll]), '>', '&gt;', [rfReplaceAll]) +
                         '</arquivo>' +
                        FTagF;
        end;

      proISSNET:
        begin
          FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgEnviarLote +
                                FDadosSenha +
                        FTagF;
        end;
    else
      FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgEnviarLote + FTagF;
    end;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.Envelope;

  if (FProvedor = proThema) and (FNotasFiscais.Count < 4) then
  begin
    FDadosEnvelope := StringReplace(FDadosEnvelope, 'recepcionarLoteRps', 'recepcionarLoteRpsLimitado', [rfReplaceAll]);
    FPSoapAction := StringReplace(FPSoapAction, 'recepcionarLoteRps', 'recepcionarLoteRpsLimitado', [rfReplaceAll]);
  end;

  if (FPDadosMsg <> '') and (FDadosEnvelope <> '') then
  begin
    DefinirSignatureNode(FTagGrupo);

    FPDadosMsg := FNotasFiscais.AssinarLote(FPDadosMsg, FTagGrupo, FTagElemento,
                                   FPConfiguracoesNFSe.Geral.ConfigAssinar.Lote,
                                   xSignatureNode, xDSIGNSLote, xIdSignature);

    // Incluido a linha abaixo por após realizar a assinatura esta gerando o
    // atributo xmlns vazio.
    if not (FProvedor in [proSP, proNotaBlu]) then
      FPDadosMsg := StringReplace(FPDadosMsg, 'xmlns=""', '', [rfReplaceAll]);

    if FProvedor in [proSMARAPD, proGiap] then
      FPDadosMsg := StringReplace(FPDadosMsg, '<?xml version="1.0" encoding="UTF-8"?>', '', [rfReplaceAll]);

    if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
      FNotasFiscais.ValidarLote(FPDadosMsg,
                         FPConfiguracoes.Arquivos.PathSchemas +
                         FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar);
  end
  else
  begin
    if not (FProvedor in [proIPM]) then
      GerarException(ACBrStr('A funcionalidade [Enviar Lote] não foi disponibilizada pelo provedor: ' +
        FPConfiguracoesNFSe.Geral.xProvedor));
  end;

  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.IncluiEncodingDados);

  case FProvedor of
    proTinus:
      begin
        FPDadosMsg := StringReplace(FPDadosMsg, 'EnviarLoteRpsEnvio', 'Arg', [rfReplaceAll]);

        case FPConfiguracoesNFSe.Geral.CodigoMunicipio of
          2407104:  // Macaiba/RN
            begin
              FPDadosMsg := StringReplace(FPDadosMsg, ' xmlns="http://www.tinus.com.br"', '', [rfReplaceAll]);

//              if FPConfiguracoesNFSe.WebServices.Ambiente = taProducao then
//                FPDadosMsg := StringReplace(FPDadosMsg, 'www.tinus.com.br', 'tempuri.org', [rfReplaceAll]);
            end;
        else
          begin
            if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
              FPDadosMsg := StringReplace(FPDadosMsg, 'www.tinus', 'www2.tinus', [rfReplaceAll]);
          end;
        end;
      end;

    // Italo 25/06/2019 incluido para resolver o problema da cidade: Soledade/RS
    proPronim:
      FPDadosMsg := StringReplace(FPDadosMsg, ' xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd"', '', [rfReplaceAll]);
  end;

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
end;

function TNFSeEnviarLoteRPS.TratarResposta: Boolean;
var
  i: Integer;
begin
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Recepcionar);

  if Assigned(FRetEnvLote) then
    FreeAndNil(FRetEnvLote);
  FRetEnvLote := TRetEnvLote.Create;

  FRetEnvLote.Leitor.Arquivo := FPRetWS;
  FRetEnvLote.Provedor := FProvedor;
  FRetEnvLote.LerXml;

//  FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Recepcionar);

  FDataRecebimento := RetEnvLote.InfRec.DataRecebimento;
  FProtocolo       := RetEnvLote.InfRec.Protocolo;
  if RetEnvLote.InfRec.NumeroLote <> '' then
    FNumeroLote := RetEnvLote.InfRec.NumeroLote;

  // Lista de Mensagem de Retorno
  FPMsg := '';
  FaMsg := '';

  if FProtocolo <> '' then
  begin
    for i := 0 to FNotasFiscais.Count -1 do
    begin
      FNotasFiscais.Items[i].NFSe.Protocolo     := FProtocolo;
      FNotasFiscais.Items[i].NFSe.dhRecebimento := FDataRecebimento;

      case FProvedor of
        proGoverna: FNotasFiscais.Items[i].NFSe.Numero := RetEnvLote.InfRec.ListaChaveNFeRPS[I].ChaveNFeRPS.Numero;

        proIPM: begin
                  FNotasFiscais.Items[i].NFSe.Numero            := RetEnvLote.InfRec.ListaChaveNFeRPS[I].ChaveNFeRPS.Numero;
                  FNotasFiscais.Items[i].NFSe.CodigoVerificacao := RetEnvLote.InfRec.ListaChaveNFeRPS[I].ChaveNFeRPS.CodigoVerificacao;
                  FNotasFiscais.Items[i].NFSe.Link              := RetEnvLote.InfRec.ListaChaveNFeRPS[I].ChaveNFeRPS.Link;
                end;

        proGiap: begin
                  FNotasFiscais.Items[i].NFSe.Numero            := RetEnvLote.InfRec.ListaChaveNFeRPS[I].ChaveNFeRPS.Numero;
                  FNotasFiscais.Items[i].NFSe.CodigoVerificacao := RetEnvLote.InfRec.ListaChaveNFeRPS[I].ChaveNFeRPS.CodigoVerificacao;
                  FNotasFiscais.Items[i].NFSe.Link              := RetEnvLote.InfRec.ListaChaveNFeRPS[I].ChaveNFeRPS.Link;
                  FNotasFiscais.Items[i].GerarXML;
                  FNotasFiscais.Items[i].GravarXML();
                end;
				
        proCTA,
        proSP,
        ProNotaBlu: begin
                      if (FProvedor in [proCTA]) or
                         ((FProvedor in [ProNotaBlu, proSP]) and (RetEnvLote.InfRec.ListaChaveNFeRPS.Count > i)) then
                      begin
                        FNotasFiscais.Items[i].NFSe.Numero := RetEnvLote.InfRec.ListaChaveNFeRPS[I].ChaveNFeRPS.Numero;
                        FNotasFiscais.Items[i].NFSe.CodigoVerificacao := RetEnvLote.InfRec.ListaChaveNFeRPS[I].ChaveNFeRPS.CodigoVerificacao;
                        FNotasFiscais.Items[i].NFSe.NumeroLote := RetEnvLote.InfRec.NumeroLote;
                      end;
                    end;
      end;
    end;

    FaMsg := 'Método........ : ' + LayOutToStr(FPLayout) + LineBreak +
             'Numero do Lote : ' + RetEnvLote.InfRec.NumeroLote + LineBreak +
             'Recebimento... : ' + IfThen(FDataRecebimento = 0, '', DateTimeToStr(FDataRecebimento)) + LineBreak +
             'Protocolo..... : ' + FProtocolo + LineBreak +
             'Provedor...... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
  end;

  if FProvedor in [proSP, ProNotaBlu] then
    Result := UpperCase(RetEnvLote.infRec.Sucesso) = UpperCase('true')
  else if FProvedor in [proGiap] then
    Result := RetEnvLote.InfRec.ListaChaveNFeRPS.Count > 0
  else
    Result := (RetEnvLote.InfRec.Protocolo <> '');

  if RetEnvLote.InfRec.MsgRetorno.Count > 0 then
  begin
    for i := 0 to RetEnvLote.InfRec.MsgRetorno.Count - 1 do
    begin
      if (RetEnvLote.InfRec.MsgRetorno.Items[i].Codigo <> 'L000') and
         (RetEnvLote.InfRec.MsgRetorno.Items[i].Codigo <> 'A0000') then
      begin
        FPMsg := FPMsg + RetEnvLote.infRec.MsgRetorno.Items[i].Mensagem + LineBreak +
                         RetEnvLote.infRec.MsgRetorno.Items[i].Correcao + LineBreak;

        FaMsg := FaMsg + 'Método..... : ' + LayOutToStr(FPLayout) + LineBreak +
                         'Código Erro : ' + RetEnvLote.InfRec.MsgRetorno.Items[i].Codigo + LineBreak +
                         'Mensagem... : ' + RetEnvLote.infRec.MsgRetorno.Items[i].Mensagem + LineBreak +
                         'Correção... : ' + RetEnvLote.InfRec.MsgRetorno.Items[i].Correcao + LineBreak +
                         'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;

        if FProvedor = proDBSeller then
          Result := False;
      end;
    end;
  end;
end;

procedure TNFSeEnviarLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;

//  if Assigned(FRetornoNFSe) then
//    FreeAndNil(FRetornoNFSe);
end;

function TNFSeEnviarLoteRPS.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeEnviarLoteRPS.GerarPrefixoArquivo: String;
begin
  Result := NumeroLote;
end;

{ TNFSeTesteEnvioLoteRPS }

procedure TNFSeTesteEnvioLoteRPS.DefinirDadosMsg;
var
  I, iTributos: Integer;
  dDataInicial, dDataFinal: TDateTime;
  TotalServicos, TotalDeducoes, TotalISS,
  TotalTributos, TotalISSRetido: Double;
begin

  if FNotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if FNotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 50 RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(FNotasFiscais.Count)));

  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Teste.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Teste.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Teste.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Teste.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Teste.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Teste.InfElemento;

  //Para o teste o schema é o mesmo do Envio
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoTeste;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Teste.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    if (FTagElemento <> '') and not (Provedor in [proBetha, proIssDSF, proCTA]) then
      FTagElemento := FPrefixo3 + FTagElemento;

    if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
    begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPScomAssinatura(FNotasFiscais.Items[I].XMLAssinado);
    end
    else begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPSsemAssinatura(FNotasFiscais.Items[I].XMLOriginal);
    end;

    InicializarTagITagF;

    dDataInicial   := FNotasFiscais.Items[0].NFSe.DataEmissao;
    dDataFinal     := dDataInicial;
    iTributos      := 0;
    TotalServicos  := 0.0;
    TotalDeducoes  := 0.0;
    TotalISS       := 0.0;
    TotalISSRetido := 0.0;
    TotalTributos  := 0.0;

    for i := 0 to FNotasFiscais.Count-1 do
    begin
      if FNotasFiscais.Items[i].NFSe.DataEmissao < dDataInicial then
        dDataInicial := FNotasFiscais.Items[i].NFSe.DataEmissao;
      if FNotasFiscais.Items[i].NFSe.DataEmissao > dDataFinal then
        dDataFinal := FNotasFiscais.Items[i].NFSe.DataEmissao;

      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaPis > 0 then
        iTributos := iTributos + 1;
      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaCofins > 0 then
        iTributos := iTributos + 1;
      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaCsll > 0 then
        iTributos := iTributos + 1;
      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaInss > 0 then
        iTributos := iTributos + 1;
      if FNotasFiscais.Items[i].NFSe.Servico.Valores.AliquotaIr > 0 then
        iTributos := iTributos + 1;

      TotalServicos  := TotalServicos + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorServicos;
      TotalDeducoes  := TotalDeducoes + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorDeducoes;
      TotalISS       := TotalISS + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorIss;
      TotalISSRetido := TotalISSRetido + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorIssRetido;
      TotalTributos  := TotalTributos + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorIr
                                      + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorCofins
                                      + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorPis
                                      + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorInss
                                      + FNotasFiscais.Items[i].NFSe.Servico.Valores.ValorCsll;
    end;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      NumeroLote := FNumeroLote;
      QtdeNotas  := FNotasFiscais.Count;
      Notas      := FvNotas;

      // Necessário para o provedor ISSDSF
      Transacao   := FNotasFiscais.Transacao;
      DataInicial := dDataInicial;
      DataFinal   := dDataFinal;

      ValorTotalServicos := TotalServicos;
      ValorTotalDeducoes := TotalDeducoes;

      // Necessário para o provedor Equiplano - EL
      NumeroRps      := FNotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero;
      SerieRps       := FNotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie;
      OptanteSimples := FNotasFiscais.Items[0].NFSe.OptanteSimplesNacional;

      if FProvedor = proCTA then
        ChaveAcessoPrefeitura := FPConfiguracoesNFSe.Geral.Emitente.WebChaveAcesso
      else // Necessário para o provedor Governa
        ChaveAcessoPrefeitura := FNotasFiscais.Items[0].NFSe.Prestador.ChaveAcesso;

      if FProvedor = proCONAM then
      begin
        AliquotaIss    := FNotasFiscais.Items[0].NFSe.Servico.Valores.Aliquota;
        TipoTributacao := '4';
        QtdTributos    := iTributos;
        ValorNota      := TotalServicos;
        ValorIss       := TotalIss;
        ValorIssRetido := TotalIssRetido;
        ValorTotalDeducoes := TotalDeducoes;
        ValorTotalTributos := TotalTributos;
        {Todo:// Acrescentados estas duas linhas abaixo por masl}
        ExigibilidadeISS := FNotasFiscais.Items[0].NFSe.Servico.ExigibilidadeISS;
        DataOptanteSimples := FNotasFiscais.Items[0].NFSe.DataOptanteSimplesNacional;
      end;
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgEnviarLote + FTagF;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Teste.Envelope;

  if (FPDadosMsg <> '') and (FDadosEnvelope <> '') then
  begin
    DefinirSignatureNode(FTagGrupo);

    FPDadosMsg := FNotasFiscais.AssinarLote(FPDadosMsg, FTagGrupo, FTagElemento,
                                   FPConfiguracoesNFSe.Geral.ConfigAssinar.Lote,
                                   xSignatureNode, xDSIGNSLote, xIdSignature);

    if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
      FNotasFiscais.ValidarLote(FPDadosMsg,
                         FPConfiguracoes.Arquivos.PathSchemas +
                         FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar);
  end
  else
    GerarException(ACBrStr('A funcionalidade [Enviar Lote] não foi disponibilizada pelo provedor: ' +
      FPConfiguracoesNFSe.Geral.xProvedor));

  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar.IncluiEncodingDados);

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));

end;

procedure TNFSeTesteEnvioLoteRPS.DefinirServicoEAction;
begin
  FPServico := 'TesteEnvioLoteRPS';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.Teste;

  inherited DefinirServicoEAction;
end;

function TNFSeTesteEnvioLoteRPS.TratarResposta: Boolean;
var
  i : Integer;
begin
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Recepcionar);

  if Assigned(FRetEnvLote) then
    FreeAndNil(FRetEnvLote);
  FRetEnvLote := TRetEnvLote.Create;

  FRetEnvLote.Leitor.Arquivo := FPRetWS;
  FRetEnvLote.Provedor := FProvedor;
  FRetEnvLote.LerXml;

//  FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Recepcionar);

  FDataRecebimento := RetEnvLote.InfRec.DataRecebimento;
  FProtocolo       := RetEnvLote.InfRec.Protocolo;
  if RetEnvLote.InfRec.NumeroLote <> '' then
    FNumeroLote := RetEnvLote.InfRec.NumeroLote;

  // Lista de Mensagem de Retorno
  FPMsg := '';
  FaMsg := '';

  if FProtocolo <> '' then
  begin
    FaMsg := 'Método........ : ' + LayOutToStr(FPLayout) + LineBreak +
             'Numero do Lote : ' + RetEnvLote.InfRec.NumeroLote + LineBreak +
             'Recebimento... : ' + IfThen(FDataRecebimento = 0, '', DateTimeToStr(FDataRecebimento)) + LineBreak +
             'Protocolo..... : ' + FProtocolo + LineBreak +
             'Provedor...... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
  end;

  Result := UpperCase(RetEnvLote.infRec.Sucesso) = UpperCase('true');

  if RetEnvLote.InfRec.MsgRetorno.Count > 0 then
  begin
    for i := 0 to RetEnvLote.InfRec.MsgRetorno.Count - 1 do
    begin
      if (RetEnvLote.InfRec.MsgRetorno.Items[i].Codigo <> 'L000') and
         (RetEnvLote.InfRec.MsgRetorno.Items[i].Codigo <> 'A0000') then
      begin
        FPMsg := FPMsg + RetEnvLote.infRec.MsgRetorno.Items[i].Mensagem + LineBreak +
                         RetEnvLote.infRec.MsgRetorno.Items[i].Correcao + LineBreak;

        FaMsg := FaMsg + 'Método..... : ' + LayOutToStr(FPLayout) + LineBreak +
                         'Código Erro : ' + RetEnvLote.InfRec.MsgRetorno.Items[i].Codigo + LineBreak +
                         'Mensagem... : ' + RetEnvLote.infRec.MsgRetorno.Items[i].Mensagem + LineBreak +
                         'Correção... : ' + RetEnvLote.InfRec.MsgRetorno.Items[i].Correcao + LineBreak +
                         'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;

      end;
    end;
  end;
end;

{ TNFSeEnviarSincrono }

constructor TNFSeEnviarSincrono.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeEnviarSincrono.Destroy;
begin
  if Assigned(FRetornoNFSe) then
    FRetornoNFSe.Free;

  if Assigned(FRetEnvLote) then
    FRetEnvLote.Free;

  inherited Destroy;
end;

procedure TNFSeEnviarSincrono.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNFSeRecepcaoLoteSincrono;
  FPArqEnv := 'env-lotS';
  FPArqResp := 'recS';

  FProtocolo := '';

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeEnviarSincrono.DefinirURL;
begin
  FPLayout := LayNFSeRecepcaoLoteSincrono;

  inherited DefinirURL;
end;

procedure TNFSeEnviarSincrono.DefinirServicoEAction;
begin
  FPServico :=  'NFSeEnviarSincrono';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.RecSincrono;

  inherited DefinirServicoEAction;
end;

procedure TNFSeEnviarSincrono.DefinirDadosMsg;
var
  I: Integer;
begin
  if FNotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'));

  if FNotasFiscais.Count > 50 then
    GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 50 RPS)' +
      ' excedido. Quantidade atual: ' + IntToStr(FNotasFiscais.Count)));

  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.RecSincrono.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.RecSincrono.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.RecSincrono.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.RecSincrono.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.RecSincrono.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.RecSincrono.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviarSincrono;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.RecSincrono.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    if (FTagElemento <> '') and not (Provedor in [proBetha, proIssDSF]) then
      FTagElemento := FPrefixo3 + FTagElemento;

    if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
    begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPScomAssinatura(FNotasFiscais.Items[I].XMLAssinado);
    end
    else begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPSsemAssinatura(FNotasFiscais.Items[I].XMLOriginal);
    end;

    InicializarTagITagF;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      NumeroLote := FNumeroLote; 
      QtdeNotas  := FNotasFiscais.Count;
      Notas      := FvNotas;
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgEnviarSincrono + FTagF;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.RecSincrono.Envelope;

  if (FPDadosMsg <> '') and (FDadosEnvelope <> '') then
  begin
    DefinirSignatureNode(FTagGrupo);

    FPDadosMsg := TNFSeEnviarSincrono(Self).FNotasFiscais.AssinarLote(FPDadosMsg,
                                  FTagGrupo,
                                  FTagElemento,
                                  FPConfiguracoesNFSe.Geral.ConfigAssinar.Lote,
                                  xSignatureNode, xDSIGNSLote, xIdSignature);

    if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
      TNFSeEnviarSincrono(Self).FNotasFiscais.ValidarLote(FPDadosMsg,
                 FPConfiguracoes.Arquivos.PathSchemas +
                 FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviarSincrono);
   end
   else
     GerarException(ACBrStr('A funcionalidade [Enviar Sincrono] não foi disponibilizada pelo provedor: ' +
      FPConfiguracoesNFSe.Geral.xProvedor));

  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.RecSincrono.IncluiEncodingDados);

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
end;

function TNFSeEnviarSincrono.TratarResposta: Boolean;
var
  i: Integer;
begin
  FPMsg := '';
  FaMsg := '';

  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.RecSincrono);

  FNotaRetornada := (Pos('CompNfse', FPRetWS) > 0);

  if FNotaRetornada then
  begin
//    FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.RecSincrono);
    Result := ExtrairNotasRetorno;
  end
  else
  begin
    if Assigned(FRetEnvLote) then
      FreeAndNil(FRetEnvLote);
    FRetEnvLote := TRetEnvLote.Create;

    FRetEnvLote.Leitor.Arquivo := FPRetWS;
    FRetEnvLote.Provedor := FProvedor;
    FRetEnvLote.LerXml;

//    FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.RecSincrono);

    FDataRecebimento := RetEnvLote.InfRec.DataRecebimento;
    FProtocolo       := RetEnvLote.InfRec.Protocolo;
    if RetEnvLote.InfRec.NumeroLote <> '' then
      FNumeroLote := RetEnvLote.InfRec.NumeroLote;

    // Lista de Mensagem de Retorno

    if FProtocolo <> '' then
    begin
      for i := 0 to FNotasFiscais.Count -1 do
      begin
        FNotasFiscais.Items[i].NFSe.Protocolo     := FProtocolo;
        FNotasFiscais.Items[i].NFSe.dhRecebimento := FDataRecebimento;
      end;
      FaMsg := 'Método........ : ' + LayOutToStr(FPLayout) + LineBreak +
               'Numero do Lote : ' + RetEnvLote.InfRec.NumeroLote + LineBreak +
               'Recebimento... : ' + IfThen(FDataRecebimento = 0, '', DateTimeToStr(FDataRecebimento)) + LineBreak +
               'Protocolo..... : ' + FProtocolo + LineBreak +
               'Provedor...... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
    end;

    if RetEnvLote.InfRec.MsgRetorno.Count > 0 then
    begin
      for i := 0 to RetEnvLote.InfRec.MsgRetorno.Count - 1 do
      begin
        if (RetEnvLote.InfRec.MsgRetorno.Items[i].Codigo <> 'L000') and
           (RetEnvLote.InfRec.MsgRetorno.Items[i].Codigo <> 'A0000') then
        begin
          FPMsg := FPMsg + RetEnvLote.infRec.MsgRetorno.Items[i].Mensagem + LineBreak +
                           RetEnvLote.infRec.MsgRetorno.Items[i].Correcao + LineBreak;

          FaMsg := FaMsg + 'Método..... : ' + LayOutToStr(FPLayout) + LineBreak +
                           'Código Erro : ' + RetEnvLote.InfRec.MsgRetorno.Items[i].Codigo + LineBreak +
                           'Mensagem... : ' + RetEnvLote.infRec.MsgRetorno.Items[i].Mensagem + LineBreak +
                           'Correção... : ' + RetEnvLote.InfRec.MsgRetorno.Items[i].Correcao + LineBreak +
                           'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
        end;
      end;
    end;

    Result := (RetEnvLote.InfRec.Protocolo <> '');
  end;
end;

procedure TNFSeEnviarSincrono.FinalizarServico;
begin
  inherited FinalizarServico;

//  if Assigned(FRetornoNFSe) then
//    FreeAndNil(FRetornoNFSe);
end;

function TNFSeEnviarSincrono.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeEnviarSincrono.GerarPrefixoArquivo: String;
begin
  Result := NumeroLote;
end;

{ TNFSeGerarNFSe }

constructor TNFSeGerarNFSe.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeGerarNFSe.Destroy;
begin
  if Assigned(FRetornoNFSe) then
    FRetornoNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeGerarNFSe.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNFSeGerar;
  FPArqEnv := 'ger-nfse';
  FPArqResp := 'lista-nfse';

  FProtocolo := '';
  FSituacao := '';

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeGerarNFSe.DefinirURL;
begin
  FPLayout := LayNFSeGerar;
  
  inherited DefinirURL;
end;

procedure TNFSeGerarNFSe.DefinirServicoEAction;
begin
  FPServico :=  'NFSeGerarNFSe';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.Gerar;

  if (FProvedor = proCenti) and (FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao) then
    FPSoapAction := StringReplace(FPSoapAction, 'GerarNfse' , 'GerarNfseHomologacao', [rfReplaceAll]);

  inherited DefinirServicoEAction;
end;

procedure TNFSeGerarNFSe.DefinirDadosMsg;
var
  I: Integer;
  Gerador: TGerador;
begin
  if FNotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao componente'));

  if FProvedor in [proBHISS, proWebISS, {proWebISSv2,} proTiplanv2] then
  begin
    if FNotasFiscais.Count > 3 then
      GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 3 RPS)' +
        ' excedido. Quantidade atual: ' + IntToStr(FNotasFiscais.Count)));
  end
  else begin
    if FNotasFiscais.Count > 1 then
      GerarException(ACBrStr('ERRO: Conjunto de RPS transmitidos (máximo de 1 RPS)' +
        ' excedido. Quantidade atual: ' + IntToStr(FNotasFiscais.Count)));
  end;

  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Gerar.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Gerar.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Gerar.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Gerar.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Gerar.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Gerar.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoGerar;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Gerar.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    if (FTagElemento <> '') and not (FProvedor in [proEGoverneISS]) then
      FTagElemento := FPrefixo3 + FTagElemento;

    if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS or FPConfiguracoesNFSe.Geral.ConfigAssinar.RpsGerar then
    begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPScomAssinatura(FNotasFiscais.Items[I].XMLAssinado);
    end
    else begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPSsemAssinatura(FNotasFiscais.Items[I].XMLOriginal);
    end;

    // Necessário para o provedor iiBrasil
    if Provedor = proiiBrasilv2 then
    begin
      FIntegridade := TACBrNFSe(FPDFeOwner).GerarIntegridade(FvNotas);

      Gerador := TGerador.Create;
      try
        Gerador.ArquivoFormatoXML := '';

        Gerador.wCampoNFSe(tcStr, '', 'Integridade', 01, 2000, 1, FIntegridade);

        FvNotas := FvNotas + Gerador.ArquivoFormatoXML;
      finally
        Gerador.Free;
      end;
    end;

    InicializarTagITagF;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      NumeroRps  := FNumeroRps;
      NumeroLote := FNumeroLote;
      QtdeNotas  := FNotasFiscais.Count;
      Notas      := FvNotas;
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgGerarNFSe + FTagF;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Gerar.Envelope;

  if (FProvedor = proCenti) and (FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao) then
    FDadosEnvelope := StringReplace(FDadosEnvelope, 'GerarNfse' , 'GerarNfseHomologacao', [rfReplaceAll]);

  if (FPDadosMsg <> '') and (FDadosEnvelope <> '') then
  begin
    DefinirSignatureNode(FTagGrupo);

    case FProvedor of
      proBethav2,
      proPublica: FTagGrupo := FPrefixo3 + 'Rps></GerarNfseEnvio';

      proSigep: FTagGrupo := FPrefixo3 + 'Rps></' + FPrefixo3 + 'GerarNfseEnvio';
    end;

    FPDadosMsg := TNFSeGerarNFSe(Self).FNotasFiscais.AssinarLote(FPDadosMsg,
                              FTagGrupo, FTagElemento,
                              FPConfiguracoesNFSe.Geral.ConfigAssinar.LoteGerar,
                              xSignatureNode, xDSIGNSLote, xIdSignature);

   if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
      TNFSeGerarNFSe(Self).FNotasFiscais.ValidarLote(FPDadosMsg,
                          FPConfiguracoes.Arquivos.PathSchemas +
                          FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoGerar);

   if FProvedor = proRecife then
     FPDadosMsg := StringReplace(FPDadosMsg, 'http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd' ,
       'http://nfse.recife.pe.gov.br/WSNacional/XSD/1/nfse_recife_v01.xsd', [rfReplaceAll]);

   if FProvedor = proRecife then
     FPDadosMsg := StringReplace(FPDadosMsg, '<InfRps' ,
       '<InfRps' + FNameSpaceDad + ' ', [rfReplaceAll]);
  end
  else
  begin
    if not (FProvedor in [proIPM]) then
      GerarException(ACBrStr('A funcionalidade [Gerar NFSe] não foi disponibilizada pelo provedor: ' +
       FPConfiguracoesNFSe.Geral.xProvedor));
  end;

  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Gerar.IncluiEncodingDados);
end;

function TNFSeGerarNFSe.TratarResposta: Boolean;
begin
  FPMsg := '';
  FaMsg := '';
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Gerar);
//  FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Gerar);
  Result := ExtrairNotasRetorno;
end;

procedure TNFSeGerarNFSe.FinalizarServico;
begin
  inherited FinalizarServico;

//  if Assigned(FRetornoNFSe) then
//    FreeAndNil(FRetornoNFSe);
end;

function TNFSeGerarNFSe.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeGerarNFSe.GerarPrefixoArquivo: String;
begin
  Result := NumeroRPS;
end;

{ TNFSeConsultarSituacaoLoteRPS }

constructor TNFSeConsultarSituacaoLoteRPS.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeConsultarSituacaoLoteRPS.Destroy;
begin
  if Assigned(FRetSitLote) then
    FRetSitLote.Free;

  inherited Destroy;
end;

procedure TNFSeConsultarSituacaoLoteRPS.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeConsultaSituacao;
  FPLayout := LayNFSeConsultaSitLoteRps;
  FPArqEnv := 'con-sit';
  FPArqResp := 'sit';

  FSituacao := '';

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeConsultarSituacaoLoteRPS.DefinirURL;
begin
  FPLayout := LayNfseConsultaSitLoteRps;

  inherited DefinirURL;
end;

procedure TNFSeConsultarSituacaoLoteRPS.DefinirServicoEAction;
begin
  FPServico :=  'NFSeConsSitLoteRPS';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.ConsSit;

  inherited DefinirServicoEAction;
end;

procedure TNFSeConsultarSituacaoLoteRPS.DefinirDadosMsg;
var
  i: Integer;
begin
  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsSit.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsSit.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsSit.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsSit.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsSit.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsSit.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoConSit;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsSit.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    if FdocElemento <> '' then
      FdocElemento := FPrefixo3 + FdocElemento;

    InicializarTagITagF;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      Protocolo := FProtocolo;

      // Necessário para o provedor Equiplano / Infisc
      NumeroLote := FNumeroLote; 
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgConsSitLote + FTagF;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  // O procedimento recebe como parametro o XML a ser assinado e retorna o
  // mesmo assinado da propriedade FPDadosMsg

  DefinirSignatureNode(FTagGrupo);

  FPDadosMsg := FNotasFiscais.AssinarXML(FPDadosMsg, FdocElemento, FinfElemento,
                                FPConfiguracoesNFSe.Geral.ConfigAssinar.ConsSit,
                                xSignatureNode, xDSIGNSLote, FxIdSignature);
  (*
  // O procedimento recebe como parametro o XML a ser assinado e retorna o
  // mesmo assinado da propriedade FPDadosMsg
  if (FPConfiguracoesNFSe.Geral.ConfigAssinar.ConsSit) and (FPDadosMsg <> '') then
    AssinarXML(FPDadosMsg, FTagGrupo, FinfElemento, 'Falha ao Assinar - Consultar Situação do Lote: ');
  *)

  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsSit.IncluiEncodingDados);

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsSit.Envelope;

  case FProvedor of

    proPublica:
      begin
        FPDadosMsg := StringReplace(FPDadosMsg,
               '<Protocolo>' + FProtocolo + '</Protocolo>', '', [rfReplaceAll]);

        i := Pos('</Signature>', FPDadosMsg);

        FPDadosMsg := Copy(FPDadosMsg, 1, i -1) + '</Signature>' +
                      '<Protocolo>' + FProtocolo + '</Protocolo>' +
                      '</ConsultarSituacaoLoteRpsEnvio>';
      end;

    proTinus:
      begin
        FPDadosMsg := StringReplace(FPDadosMsg, 'ConsultarSituacaoLoteRpsEnvio', 'Arg', [rfReplaceAll]);

        if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
          FPDadosMsg := StringReplace(FPDadosMsg, 'www.tinus', 'www2.tinus', [rfReplaceAll])
      end;
  end;

  if (FPDadosMsg = '') or (FDadosEnvelope = '') then
    GerarException(ACBrStr('A funcionalidade [Consultar Situação do Lote] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

procedure TNFSeConsultarSituacaoLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeConsultarSituacaoLoteRPS.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeConsultarSituacaoLoteRPS.GerarPrefixoArquivo: String;
begin
  Result := Protocolo;
end;

function TNFSeConsultarSituacaoLoteRPS.Executar: Boolean;
var
  IntervaloTentativas, Tentativas: integer;
begin
  TACBrNFSe(FPDFeOwner).SetStatus(stNFSeConsultaSituacao);
  try
    Sleep(FPConfiguracoesNFSe.WebServices.AguardarConsultaRet);

    Tentativas := 0;
    IntervaloTentativas := max(FPConfiguracoesNFSe.WebServices.IntervaloTentativas, 1000);

    while (inherited Executar) and
      (Tentativas < FPConfiguracoesNFSe.WebServices.Tentativas) do
    begin
      Inc(Tentativas);
      sleep(IntervaloTentativas);
    end;
  finally
    TACBrNFSe(FPDFeOwner).SetStatus(stNFSeIdle);
  end;

  Result := TratarRespostaFinal;
end;

function TNFSeConsultarSituacaoLoteRPS.TratarResposta: Boolean;
begin
  FPMsg := '';
  FaMsg := '';
  FRetSitLote.Free;
  FRetSitLote := TretSitLote.Create;

  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.ConsSit);

  FRetSitLote.Leitor.Arquivo := FPRetWS;
  FRetSitLote.Provedor       := FProvedor;

  RetSitLote.LerXml;

//  FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.ConsSit);

  FSituacao := RetSitLote.InfSit.Situacao;
  // FSituacao: 1 = Não Recebido
  //            2 = Não Processado
  //            3 = Processado com Erro
  //            4 = Processado com Sucesso

  if (FProvedor in [proEquiplano, proEL]) then
    Result := (FSituacao = '1')  // Aguardando processamento
  else
    Result := (FSituacao = '2'); // Lote não Processado
end;

function TNFSeConsultarSituacaoLoteRPS.TratarRespostaFinal: Boolean;
var
  xSituacao: String;
  i: Integer;
  Ok: Boolean;
begin
  FPMsg := '';
  FaMsg := '';
  // Lista de Mensagem de Retorno
  if RetSitLote.InfSit.MsgRetorno.Count > 0 then
  begin
    for i := 0 to RetSitLote.InfSit.MsgRetorno.Count - 1 do
    begin
      FPMsg := FPMsg + RetSitLote.infSit.MsgRetorno.Items[i].Mensagem + LineBreak +
                       RetSitLote.infSit.MsgRetorno.Items[i].Correcao + LineBreak;

      FaMsg := FaMsg + 'Método..... : ' + LayOutToStr(FPLayout) + LineBreak +
                       'Código Erro : ' + RetSitLote.infSit.MsgRetorno.Items[i].Codigo + LineBreak +
                       'Mensagem... : ' + RetSitLote.infSit.MsgRetorno.Items[i].Mensagem + LineBreak +
                       'Correção... : ' + RetSitLote.infSit.MsgRetorno.Items[i].Correcao + LineBreak +
                       'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
    end;
  end
  else begin
    for i:=0 to FNotasFiscais.Count -1 do
      FNotasFiscais.Items[i].NFSe.Situacao := FSituacao;

    case FProvedor of
      proEquiplano: begin
                      case FSituacao[1] of
                        '1' : xSituacao := 'Aguardando processamento';
                        '2' : xSituacao := 'Não Processado, lote com erro';
                        '3' : xSituacao := 'Lote Processado com sucesso';
                        '4' : xSituacao := 'Lote Processado com avisos';
                      end;
                    end;

      proEL: begin
               case FSituacao[1] of
                 '1' : xSituacao := 'Aguardando processamento';
                 '2' : xSituacao := 'Não Processado, lote com erro';
                 '3' : xSituacao := 'Lote Processado com avisos';
                 '4' : xSituacao := 'Lote Processado com sucesso';
               end;
             end;

    else begin
           case StrToSituacaoLoteRPS(Ok, FSituacao) of
            slrNaoRecibo        : xSituacao := 'Lote não Recebido.';
            slrNaoProcessado    : xSituacao := 'Lote não Processado.';
            slrProcessadoErro   : xSituacao := 'Lote Processado com Erro.';
            slrProcessadoSucesso: xSituacao := 'Lote Processado com Sucesso.';
           end;
         end;
    end;

    FaMsg := 'Método........ : ' + LayOutToStr(FPLayout) + LineBreak +
             'Numero do Lote : ' + RetSitLote.InfSit.NumeroLote + LineBreak +
             'Situação...... : ' + FSituacao + '-' + xSituacao + LineBreak;
  end;

  Result := (FPMsg = '');
end;

{ TNFSeConsultarLoteRPS }

constructor TNFSeConsultarLoteRPS.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeConsultarLoteRPS.Destroy;
begin
  if Assigned(FRetornoNFSe) then
    FRetornoNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeConsultarLoteRPS.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeConsulta;
  FPLayout := LayNfseConsultaLote;
  FPArqEnv := 'con-lot';
  FPArqResp := 'lista-nfse';

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeConsultarLoteRPS.DefinirURL;
begin
  FPLayout := LayNfseConsultaLote;

  inherited DefinirURL;
end;

procedure TNFSeConsultarLoteRPS.DefinirServicoEAction;
begin
  FPServico := 'NFSeConsLote';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.ConsLote;

  inherited DefinirServicoEAction;
end;

procedure TNFSeConsultarLoteRPS.DefinirDadosMsg;
begin
  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsLote.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsLote.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsLote.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsLote.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsLote.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsLote.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoConLot;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsLote.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    InicializarTagITagF;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      Protocolo := FProtocolo; 

      // Necessário para o provedor Equiplano - EL
      NumeroLote := FNumeroLote; 
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgConsLote + FTagF;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  if Provedor = proNFSeBrasil then
    FPDadosMsg := FProtocolo;

  // O procedimento recebe como parametro o XML a ser assinado e retorna o
  // mesmo assinado da propriedade FPDadosMsg
  if (FPConfiguracoesNFSe.Geral.ConfigAssinar.ConsLote) and (FPDadosMsg <> '') then
    AssinarXML(FPDadosMsg, FTagGrupo, FinfElemento, 'Falha ao Assinar - Consultar Lote de RPS: ');

  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsLote.IncluiEncodingDados);

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsLote.Envelope;

  if FProvedor = proTinus then
  begin
    FPDadosMsg := StringReplace(FPDadosMsg, 'ConsultarLoteRpsEnvio', 'Arg', [rfReplaceAll]);

    if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
      FPDadosMsg := StringReplace(FPDadosMsg, 'www.tinus', 'www2.tinus', [rfReplaceAll])
  end;

  if ((FPDadosMsg = '') or (FDadosEnvelope = '')) and (not (FProvedor in [proIPM])) then
    GerarException(ACBrStr('A funcionalidade [Consultar Lote] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeConsultarLoteRPS.TratarResposta: Boolean;
begin
  FPMsg := '';
  FaMsg := '';
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.ConsLote);
  Result := ExtrairNotasRetorno;
  FSituacao := FRetornoNFSe.Situacao;

//  FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.ConsLote);
end;

procedure TNFSeConsultarLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeConsultarLoteRPS.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeConsultarLoteRPS.GerarPrefixoArquivo: String;
begin
  Result := TiraPontos(Protocolo);
end;

{ TNFSeConsultarNfseRPS }

constructor TNFSeConsultarNfseRPS.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeConsultarNfseRPS.Destroy;
begin
  if Assigned(FRetornoNFSe) then
    FRetornoNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeConsultarNFSeRPS.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeConsulta;
  FPLayout := LayNfseConsultaNfseRps;
  FPArqEnv := 'con-nfse-rps';
  FPArqResp := 'comp-nfse';

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeConsultarNfseRPS.DefinirURL;
begin
  FPLayout := LayNfseConsultaNfseRps;

  inherited DefinirURL;
end;

procedure TNFSeConsultarNfseRPS.DefinirServicoEAction;
begin
  FPServico := 'NFSeConsNfseRPS';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.ConsNfseRps;

  inherited DefinirServicoEAction;
end;

procedure TNFSeConsultarNfseRPS.DefinirDadosMsg;
var
  i: Integer;
  Gerador: TGerador;
  Consulta: string;
begin
  if (FNotasFiscais.Count <= 0) and (FProvedor in [proGoverna,proIssDSF]) then
    GerarException(ACBrStr('ERRO: Nenhum RPS carregado ao componente'));

  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSeRps.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSeRps.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSeRps.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSeRps.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSeRps.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSeRps.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoConRps;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSeRps.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    InicializarTagITagF;

    if FProvedor in [proIssDSF, proCTA] then
    begin
      Gerador := TGerador.Create;
      try
        Gerador.ArquivoFormatoXML := '';

        if FNotasFiscais.Count > 0 then
        begin
          if FNotasFiscais.Items[0].NFSe.Numero = '' then
          begin
            Gerador.wGrupoNFSe('RPSConsulta');
            for i := 0 to FNotasFiscais.Count-1 do
            begin
              with FNotasFiscais.Items[I] do
                if NFSe.IdentificacaoRps.Numero <> '' then
                begin
                  Gerador.wGrupoNFSe('RPS Id="rps:' + NFSe.IdentificacaoRps.Numero + '"');
                  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 11,  1, NFSe.Prestador.InscricaoMunicipal, '');
                  Gerador.wCampoNFSe(tcStr, '#1', 'NumeroRPS', 01, 12, 1, OnlyNumber(NFSe.IdentificacaoRps.Numero), '');
                  // Roberto godinho - Para o provedor CTA deve enviar a série de prestação (99) e não a série do RPS
                  if FProvedor = proCTA then
                    Gerador.wCampoNFSe(tcStr, '', 'SeriePrestacao', 01, 2,  1, IIf(NFSe.SeriePrestacao='', '99', NFSe.SeriePrestacao), '')
                  else
                    Gerador.wCampoNFSe(tcStr, '', 'SeriePrestacao', 01, 2,  1, NFSe.SeriePrestacao, '');
                  Gerador.wGrupoNFSe('/RPS');
                end;
            end;
            Gerador.wGrupoNFSe('/RPSConsulta');
          end
          else begin
            Gerador.wGrupoNFSe('NotaConsulta');
            for i := 0 to FNotasFiscais.Count-1 do
            begin
              with FNotasFiscais.Items[I] do
                if NFSe.Numero <> '' then
                begin
                  Gerador.wGrupoNFSe('Nota Id="nota:' + NFSe.Numero + '"');
                  Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 11,  1, FPConfiguracoesNFSe.Geral.Emitente.InscMun, '');
                  Gerador.wCampoNFSe(tcStr, '#1', 'NumeroNota', 01, 12, 1, OnlyNumber(NFSe.Numero), '');
                  Gerador.wCampoNFSe(tcStr, '', 'CodigoVerificacao', 01, 255,  1, NFSe.CodigoVerificacao, '');
                  Gerador.wGrupoNFSe('/Nota');
                end;
            end;
            Gerador.wGrupoNFSe('/NotaConsulta');
          end;
        end;

        FvNotas := Gerador.ArquivoFormatoXML;
      finally
        Gerador.Free;
      end;
    end;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      NumeroRps := FNumeroRPS;
      SerieRps  := FSerie;
      TipoRps   := FTipo;

      // Necessário para o provedor ISSDSF e CTA
      if FProvedor in [proIssDSF, proCTA] then
      begin
        NumeroLote := FNumeroLote;
        Transacao  := FNotasFiscais.Transacao;
        Notas      := FvNotas;
      end;

      // Necessário para o provedor Governa
      if FProvedor = proGoverna then
      begin
        ChaveAcessoPrefeitura := FNotasFiscais.Items[0].NFSe.Prestador.ChaveAcesso;
        CodVerificacaoRPS     := FNotasFiscais.Items[0].NFSe.CodigoVerificacao;
      end
      else if FProvedor = proGiap then
      begin
        CodVerificacaoRPS := FNotasFiscais.Items[0].NFSe.CodigoVerificacao;
      end
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    Consulta := GerarDadosMsg.Gera_DadosMsgConsNFSeRPS;

    // Necessário para o provedor iiBrasil
    if Provedor = proiiBrasilv2 then
    begin
      FIntegridade := TACBrNFSe(FPDFeOwner).GerarIntegridade(Consulta);

      Gerador := TGerador.Create;
      try
        Gerador.ArquivoFormatoXML := '';

        Gerador.wCampoNFSe(tcStr, '', 'Integridade', 01, 2000, 1, FIntegridade);

        Consulta := Consulta + Gerador.ArquivoFormatoXML;
      finally
        Gerador.Free;
      end;
    end;

    FPDadosMsg := FTagI + Consulta + FTagF;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  if Provedor = proNFSeBrasil then
    FPDadosMsg := NumeroRps;

  // O procedimento recebe como parametro o XML a ser assinado e retorna o
  // mesmo assinado da propriedade FPDadosMsg
  if (FPConfiguracoesNFSe.Geral.ConfigAssinar.ConsNFSeRps) and (FPDadosMsg <> '') then
    AssinarXML(FPDadosMsg, FTagGrupo, FinfElemento, 'Falha ao Assinar - Consultar NFSe por RPS: ');
    
  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSeRps.IncluiEncodingDados);

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSeRps.Envelope;

  if FProvedor = proTinus then
  begin
    FPDadosMsg := StringReplace(FPDadosMsg, 'ConsultarNfseRpsEnvio', 'Arg', [rfReplaceAll]);

    if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
      FPDadosMsg := StringReplace(FPDadosMsg, 'www.tinus', 'www2.tinus', [rfReplaceAll])
  end;

  if (FPDadosMsg = '') or (FDadosEnvelope = '') then
    GerarException(ACBrStr('A funcionalidade [Consultar NFSe por RPS] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeConsultarNfseRPS.TratarResposta: Boolean;
begin
  FPMsg := '';
  FaMsg := '';
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.ConsNFSeRPS);
  Result := ExtrairNotasRetorno;

//  FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.ConsNFSeRPS);
end;

procedure TNFSeConsultarNfseRPS.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeConsultarNfseRPS.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeConsultarNfseRPS.GerarPrefixoArquivo: String;
begin
  Result := NumeroRps + Serie;
end;

{ TNFSeConsultarNfse }

constructor TNFSeConsultarNfse.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeConsultarNfse.Destroy;
begin
  if Assigned(FRetornoNFSe) then
    FRetornoNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeConsultarNFSe.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeConsulta;
  FPLayout := LayNfseConsultaNfse;
  FPArqEnv := 'con-nfse';
  FPArqResp := 'lista-nfse';

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeConsultarNfse.DefinirURL;
begin
  FPLayout := LayNfseConsultaNfse;

  inherited DefinirURL;
end;

procedure TNFSeConsultarNfse.DefinirServicoEAction;
begin
  FPServico := 'NFSeConsNfse';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.ConsNfse;

  inherited DefinirServicoEAction;
end;

procedure TNFSeConsultarNfse.DefinirDadosMsg;
begin
  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSe.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSe.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSe.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSe.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSe.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSe.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoConNfse;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSe.IncluiEncodingCab);

  if (FProvedor = proSP) and (TNFSeConsultarNfse(Self).DataInicial > 0) and
     (TNFSeConsultarNfse(Self).DataFinal > 0) then
    FTagGrupo := 'PedidoConsultaNFePeriodo';

  GerarDadosMsg := TNFSeG.Create;
  try
    if FProvedor = proGoverna then
      FTagGrupo := FPrefixo4 + FTagGrupo
    else
    begin
      if FTagGrupo <> '' then
        FTagGrupo := FPrefixo3 + FTagGrupo;
    end;

    InicializarTagITagF;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      DataInicial := TNFSeConsultarNfse(Self).DataInicial;
      DataFinal   := TNFSeConsultarNfse(Self).DataFinal;
      NumeroNFSe  := TNFSeConsultarNfse(Self).NumeroNFSe;
      Pagina      := TNFSeConsultarNfse(Self).FPagina;
      CNPJTomador := TNFSeConsultarNfse(Self).FCNPJTomador;
      IMTomador   := TNFSeConsultarNfse(Self).FIMTomador;
      NomeInter   := TNFSeConsultarNfse(Self).FNomeInter;
      CNPJInter   := TNFSeConsultarNfse(Self).FCNPJInter;
      IMInter     := TNFSeConsultarNfse(Self).FIMInter;

      // Necessario para o provedor Infisc
      SerieNFSe := TNFSeConsultarNfse(Self).Serie;
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgConsNFSe + FTagF;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  if (FProvedor = proNFSeBrasil)
    then FPDadosMsg := NumeroNFSe;

  // O procedimento recebe como parametro o XML a ser assinado e retorna o
  // mesmo assinado da propriedade FPDadosMsg
  if (FPConfiguracoesNFSe.Geral.ConfigAssinar.ConsNFSe) and (FPDadosMsg <> '') then
    AssinarXML(FPDadosMsg, FTagGrupo, FinfElemento, 'Falha ao Assinar - Consultar NFSe: ');
    
  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSe.IncluiEncodingDados);

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.ConsNFSe.Envelope;

  if FProvedor = proTinus then
  begin
    FPDadosMsg := StringReplace(FPDadosMsg, 'ConsultarNfseEnvio', 'Arg', [rfReplaceAll]);

    if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
      FPDadosMsg := StringReplace(FPDadosMsg, 'www.tinus', 'www2.tinus', [rfReplaceAll])
  end;

  if (FPDadosMsg = '') or (FDadosEnvelope = '') then
    GerarException(ACBrStr('A funcionalidade [Consultar NFSe] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeConsultarNfse.TratarResposta: Boolean;
begin
  FPMsg := '';
  FaMsg := '';
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.ConsNFSe);
  Result := ExtrairNotasRetorno;

//  FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.ConsNFSe);
end;

procedure TNFSeConsultarNfse.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeConsultarNfse.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeConsultarNfse.GerarPrefixoArquivo: String;
begin
  Result := FormatDateTime('yyyymmdd', DataInicial) +
            FormatDateTime('yyyymmdd', DataFinal);
end;

{ TNFSeCancelarNfse }

constructor TNFSeCancelarNfse.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeCancelarNfse.Destroy;
begin
  if Assigned(FRetCancNFSe) then
    FRetCancNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeCancelarNFSe.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeCancelamento;
  FPLayout := LayNfseCancelaNfse;
  FPArqEnv := 'ped-can';
  FPArqResp := 'can';

  FDataHora := 0;

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeCancelarNfse.DefinirURL;
begin
  FPLayout := LayNfseCancelaNfse;

  inherited DefinirURL;
end;

procedure TNFSeCancelarNfse.DefinirServicoEAction;
begin
  FPServico := 'NFSeCancNfse';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.Cancelar;

  inherited DefinirServicoEAction;
end;

procedure TNFSeCancelarNfse.DefinirDadosMsg;
var
  i, iPos: Integer;
  Gerador: TGerador;
  sAssinatura: String;
begin
  if FNotasFiscais.Count <= 0 then
    GerarException(ACBrStr('ERRO: Nenhuma NFS-e carregada ao componente'));

  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Cancelar.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Cancelar.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Cancelar.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Cancelar.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Cancelar.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Cancelar.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoCancelar;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Cancelar.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if (FTagGrupo <> '') and (FProvedor <> proGinfes) then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    // Removido o provedor proISSDSF para que será incluido o profixo em
    // FdocElemento
    if (FdocElemento <> '') and not (FProvedor in [proBetha, proGinfes]) then
      FdocElemento := FPrefixo3 + FdocElemento;

    if FNotasFiscais.Count > 0 then
    begin
      if (FNumeroNFSe = '') then
        FNumeroNFSe := FNotasFiscais.Items[0].NFSe.Numero;
      if FProvedor = proISSDSF then
        FCodigoVerificacao := FNotasFiscais.Items[0].NFSe.CodigoVerificacao;
    end;

    case FProvedor of
      proCONAM: FURI := 'Sdt_cancelanfe';

      proDigifred,
      proPronimv2,
      proPublica: FURI := 'CANC' + TNFSeCancelarNfse(Self).FNumeroNFSe;

      proEquiplano,
      proISSCuritiba,
      proSP,
      proNotaBlu,
      proSMARAPD,
      proGiap,
      proIPM: FURI := '';

      proGovDigital: FURI := TNFSeCancelarNfse(Self).FNumeroNFSe;

      proIssIntel,
      proISSNet: begin
                   FURI := '';
                   FURIRef := 'http://www.w3.org/TR/2000/REC-xhtml1-20000126/';
                 end;

      proABase,
      proTecnos: FURI := '2' + FPConfiguracoesNFSe.Geral.Emitente.CNPJ +
                  IntToStrZero(StrToInt(TNFSeCancelarNfse(Self).FNumeroNFSe), 16);

      proRecife,
      proRJ,
      proFriburgo: FURI := 'Cancelamento_NF' + TNFSeCancelarNfse(Self).FNumeroNFSe;

      proSaatri: FURI := 'Cancelamento_' + FPConfiguracoesNFSe.Geral.Emitente.CNPJ;
    else
      FURI := 'pedidoCancelamento_' + FPConfiguracoesNFSe.Geral.Emitente.CNPJ +
                                   FPConfiguracoesNFSe.Geral.Emitente.InscMun +
                                   TNFSeCancelarNfse(Self).FNumeroNFSe;
    end;

    InicializarTagITagF;

    if FProvedor in [proIssDSF, proCTA] then
    begin
      Gerador := TGerador.Create;
      try
        Gerador.ArquivoFormatoXML := '';

        for i := 0 to FNotasFiscais.Count-1 do
        begin
          with FNotasFiscais.Items[I] do
          begin
            Gerador.wGrupoNFSe('Nota Id="nota:' + NFSe.Numero + '"');
            Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 11,  1, FPConfiguracoesNFSe.Geral.Emitente.InscMun, '');
            Gerador.wCampoNFSe(tcStr, '#1', 'NumeroNota', 01, 12, 1, OnlyNumber(NFSe.Numero), '');
            Gerador.wCampoNFSe(tcStr, '', 'CodigoVerificacao', 01, 255,  1, NFSe.CodigoVerificacao, '');
            Gerador.wCampoNFSe(tcStr, '', 'MotivoCancelamento', 01, 80, 1, TNFSeCancelarNfse(Self).FMotivoCancelamento, '');
            Gerador.wGrupoNFSe('/Nota');
          end;
        end;

        FvNotas := Gerador.ArquivoFormatoXML;
      finally
        Gerador.Free;
      end;
    end;

    if (FProvedor in [proInfisc, proInfiscv11] ) then
    begin
      Gerador := TGerador.Create;
      try
        Gerador.ArquivoFormatoXML := '';
        for i := 0 to FNotasFiscais.Count-1 do
        begin
          with FNotasFiscais.Items[I] do
          begin
            Gerador.wCampoNFSe(tcStr, '', 'chvAcessoNFS-e', 1, 39, 1, NFSe.ChaveNFSe, '');
            Gerador.wCampoNFSe(tcStr, '', 'motivo', 1, 39, 1, TNFSeCancelarNfse(Self).FCodigoCancelamento, ''); {@/\@}
          end;
        end;

        FvNotas := Gerador.ArquivoFormatoXML;
      finally
        Gerador.Free;
      end;
    end;

    if FProvedor in [proGoverna] then
    begin
      Gerador := TGerador.Create;
      try
        Gerador.ArquivoFormatoXML := '';
        Gerador.Prefixo := Prefixo4;
        for i := 0 to FNotasFiscais.Count-1 do
        begin
          Gerador.wGrupoNFSe('NotCan');
          with FNotasFiscais.Items[I] do
          begin
            Gerador.wGrupoNFSe('InfNotCan');
            Gerador.Prefixo := Prefixo3;
            Gerador.wCampoNFSe(tcStr, '', 'NumNot', 01, 10, 1, OnlyNumber(NFSe.Numero), '');
            Gerador.wCampoNFSe(tcStr, '', 'CodVer', 01, 255,  1, NFSe.CodigoVerificacao, '');
            Gerador.wCampoNFSe(tcStr, '', 'DesMotCan', 01, 80, 1, TNFSeCancelarNfse(Self).FMotivoCancelamento, '');
            Gerador.Prefixo := Prefixo4;
            Gerador.wGrupoNFSe('/InfNotCan');
          end;
          Gerador.wGrupoNFSe('/NotCan');
        end;

        FvNotas := Gerador.ArquivoFormatoXML;
      finally
        Gerador.Free;
      end;
    end;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      if FProvedor in [proSP, proNotaBlu] then
      begin
        sAssinatura := Poem_Zeros(IM, 8) + Poem_Zeros(TNFSeCancelarNfse(Self).NumeroNFSe, 12);
        AssinaturaCan := FPDFeOwner.SSL.CalcHash(sAssinatura, dgstSHA1, outBase64, True);
      end;

      case FProvedor of
        proISSNet: if FPConfiguracoesNFSe.WebServices.AmbienteCodigo = 2 then
                     CodMunicipio := 999;
                     
        proBetha,
        proBethav2: CodMunicipio  := StrToIntDef(FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0);
        proFiorilli: CodMunicipio := FNotasFiscais.Items[0].NFSe.Servico.MunicipioIncidencia;
      else
        CodMunicipio := FPConfiguracoesNFSe.Geral.CodigoMunicipio;
      end;

      NumeroNFSe := TNFSeCancelarNfse(Self).NumeroNFSe;
      CodigoCanc := TNFSeCancelarNfse(Self).FCodigoCancelamento;
      MotivoCanc := TNFSeCancelarNfse(Self).FMotivoCancelamento;

      SerieNFSe  := FNotasFiscais.Items[0].NFSe.SeriePrestacao;
      NumeroRPS  := FNotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero;
      SerieRps   := FNotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie;
      ValorNota  := FNotasFiscais.Items[0].NFSe.ValoresNfse.ValorLiquidoNfse;

      // Necessário para o provedor ISSDSF
      Transacao  := FNotasFiscais.Transacao;
      Notas      := FvNotas;

      NumeroLote := FNotasFiscais.NumeroLote;
      if NumeroLote = '' then
        NumeroLote := FNumeroLote;

      if FProvedor = proEGoverneISS then
        Transacao := (SimNaoToStr(FNotasFiscais.Items[0].NFSe.Producao) = '2');

      if FProvedor = proCTA then
        ChaveAcessoPrefeitura := FPConfiguracoesNFSe.Geral.Emitente.WebChaveAcesso
      else
        ChaveAcessoPrefeitura := FNotasFiscais.Items[0].NFSe.Prestador.ChaveAcesso;
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    case Fprovedor of
      proISSe,
      ProTecnos:
        begin
          FPDadosMsg := GerarDadosMsg.Gera_DadosMsgCancelarNFSe;
          iPos := Pos('><InfPedido', FPDadosMsg);
          FPDadosMsg := Copy(FPDadosMsg, 1, iPos -1) + FNameSpaceCan +
                        Copy(FPDadosMsg, iPos, Length(FPDadosMsg));
        end;

      proISSNET:
        begin
          FPDadosMsg := GerarDadosMsg.Gera_DadosMsgCancelarNFSe;
          iPos := Pos('><tc:InfPedido', FPDadosMsg);
          FPDadosMsg := Copy(FPDadosMsg, 1, iPos -1) + FNameSpaceCan +
                        Copy(FPDadosMsg, iPos, Length(FPDadosMsg));
        end;

      proSigep:
        begin
          Gerador := TGerador.Create;
          try
            Gerador.ArquivoFormatoXML := '';
            Gerador.Prefixo := Prefixo4;
            Gerador.wGrupoNFSe('credenciais');
            Gerador.wCampoNFSe(tcStr, '#01', 'usuario     ', 01, 15, 1, GerarDadosMsg.UserWeb);
            Gerador.wCampoNFSe(tcStr, '#02', 'senha       ', 01, 05, 1, GerarDadosMsg.SenhaWeb);
            Gerador.wCampoNFSe(tcStr, '#03', 'chavePrivada', 01, 01, 1, GerarDadosMsg.ChaveAcessoPrefeitura);
            Gerador.wGrupoNFSe('/credenciais');

            FPDadosMsg := FTagI + Gerador.ArquivoFormatoXML +
                                GerarDadosMsg.Gera_DadosMsgCancelarNFSe + FTagF;
          finally
            Gerador.Free;
          end;
        end;
    else
      FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgCancelarNFSe + FTagF;
    end;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  if (FProvedor = proNFSeBrasil) then
    FPDadosMsg := NumeroNFSe;

  // O procedimento recebe como parametro o XML a ser assinado e retorna o
  // mesmo assinado da propriedade FPDadosMsg

  DefinirSignatureNode(FTagGrupo);

  FPDadosMsg := FNotasFiscais.AssinarXML(FPDadosMsg, FdocElemento, FinfElemento,
         FPConfiguracoesNFSe.Geral.ConfigAssinar.Cancelar,
         xSignatureNode, xDSIGNSLote, FxIdSignature);

//  if (FPConfiguracoesNFSe.Geral.ConfigAssinar.Cancelar) and (FPDadosMsg <> '') then
//    AssinarXML(FPDadosMsg, FdocElemento, FinfElemento,
//               'Falha ao Assinar - Cancelar NFS-e: ');

  case FProvedor of
    proISSe,
    ProTecnos,
    proISSNET:
      begin
        FPDadosMsg := StringReplace(FPDadosMsg, FNameSpaceCan, '', []);
        FPDadosMsg := FTagI + FPDadosMsg + FTagF;
      end;

    proBetha: FPDadosMsg := '<' + FTagGrupo + FNameSpaceDad + '>' +
                                  FPDadosMsg +
                            '</' + FTagGrupo + '>';
  end;

  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Cancelar.IncluiEncodingDados);

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Cancelar.Envelope;

  if FProvedor = proTinus then
  begin
    FPDadosMsg := StringReplace(FPDadosMsg, 'CancelarNfseEnvio', 'Arg', [rfReplaceAll]);

    if FPConfiguracoesNFSe.WebServices.Ambiente = taHomologacao then
      FPDadosMsg := StringReplace(FPDadosMsg, 'www.tinus', 'www2.tinus', [rfReplaceAll])
  end;

  if ((FPDadosMsg = '') or (FDadosEnvelope = '')) and (not (FProvedor in [proIPM])) then
    GerarException(ACBrStr('A funcionalidade [Cancelar NFSe] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeCancelarNfse.TratarResposta: Boolean;
var
  i: Integer;
begin
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Cancelar);

  if Assigned(FRetCancNFSe) then
    FRetCancNFSe.Free;

  FRetCancNFSe := TRetCancNfse.Create;
  FRetCancNFSe.Leitor.Arquivo := FPRetWS;
  FRetCancNFSe.Provedor       := FProvedor;
  FRetCancNFSe.VersaoXML      := FVersaoXML;

  FRetCancNFSe.LerXml;

//  FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Cancelar);

  FDataHora := RetCancNFSe.InfCanc.DataHora;

  // Lista de Mensagem de Retorno
  FPMsg := '';
  FaMsg := '';
  if RetCancNFSe.InfCanc.MsgRetorno.Count > 0 then
  begin
    for i := 0 to RetCancNFSe.InfCanc.MsgRetorno.Count - 1 do
    begin
      FPMsg := FPMsg + RetCancNFSe.infCanc.MsgRetorno.Items[i].Mensagem + LineBreak +
                       RetCancNFSe.InfCanc.MsgRetorno.Items[i].Correcao + LineBreak;

      FaMsg := FaMsg + 'Método..... : ' + LayOutToStr(FPLayout) + LineBreak +
                       'Código Erro : ' + RetCancNFSe.InfCanc.MsgRetorno.Items[i].Codigo + LineBreak +
                       'Mensagem... : ' + RetCancNFSe.infCanc.MsgRetorno.Items[i].Mensagem + LineBreak +
                       'Correção... : ' + RetCancNFSe.InfCanc.MsgRetorno.Items[i].Correcao + LineBreak +
                       'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
    end;
  end
  else 
  begin
    FaMsg := 'Método........ : ' + LayOutToStr(FPLayout) + LineBreak +
                'Numero da NFSe : ' + TNFSeCancelarNfse(Self).FNumeroNFSe + LineBreak +
                'Data Hora..... : ' + ifThen(FDataHora = 0, '', DateTimeToStr(FDataHora)) + LineBreak;

    if Provedor = proGiap then
      FPMsg := RetCancNFSe.InfCanc.MsgCanc;
  end;

  Result := (FDataHora > 0) or (RetCancNFSe.InfCanc.Sucesso='S') or (UpperCase(RetCancNFSe.InfCanc.Sucesso)='TRUE');
end;

procedure TNFSeCancelarNFSe.SalvarResposta;
var
  aPath: String;
begin
  inherited SalvarResposta;

  if FPConfiguracoesNFSe.Arquivos.Salvar then
  begin
    aPath := PathWithDelim(FPConfiguracoesNFSe.Arquivos.GetPathNFSe(0, ''{xData, xCNPJ}));
    FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqResp + '.xml', FPRetWS, aPath);
  end;
end;

procedure TNFSeCancelarNfse.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeCancelarNfse.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeCancelarNfse.GerarPrefixoArquivo: String;
begin
  Result := NumeroNFSe;
end;

{ TNFSeSubstituirNFSe }

constructor TNFSeSubstituirNFSe.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeSubstituirNFSe.Destroy;
begin
  if Assigned(FRetornoNFSe) then
    FRetornoNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeSubstituirNFSe.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeSubstituicao;
  FPLayout := LayNfseSubstituiNfse;
  FPArqEnv := 'ped-sub';
  FPArqResp := 'sub';

  FDataHora := 0;
  FSituacao := '';

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeSubstituirNFSe.DefinirURL;
begin
  FPLayout := LayNfseSubstituiNfse;

  inherited DefinirURL;
end;

procedure TNFSeSubstituirNFSe.DefinirServicoEAction;
begin
  FPServico := 'NFSeSubNfse';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.Substituir;

  inherited DefinirServicoEAction;
end;

procedure TNFSeSubstituirNFSe.DefinirDadosMsg;
var
  i: Integer;
  Gerador: TGerador;
  Identificador: string;
begin
  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Substituir.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Substituir.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Substituir.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Substituir.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Substituir.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Substituir.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoSubstituir;

  Identificador := FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador;

  if Identificador <> '' then
    Identificador := ' ' + Identificador + '="sub' +
                     TNFSeSubstituirNfse(Self).FNumeroNFSe + '"';

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Substituir.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    if FdocElemento <> '' then
      FdocElemento := FPrefixo3 + FdocElemento;

    if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
    begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPScomAssinatura(FNotasFiscais.Items[I].XMLAssinado);
    end
    else begin
      for I := 0 to FNotasFiscais.Count - 1 do
        GerarLoteRPSsemAssinatura(FNotasFiscais.Items[I].XMLOriginal);
    end;

    case FProvedor of
      proEquiplano,
      proPublica: FURI:= '';

      proDigifred:  FURI := 'CANC' + TNFSeSubstituirNfse(Self).FNumeroNFSe;

      proSaatri: FURI := 'Cancelamento_' + FPConfiguracoesNFSe.Geral.Emitente.CNPJ;

      proIssIntel,
      proISSNet: begin
                   FURI := '';
                   FURIRef := 'http://www.w3.org/TR/2000/REC-xhtml1-20000126/';
                 end;

      proTecnos: FURI := '2' + FPConfiguracoesNFSe.Geral.Emitente.CNPJ +
                            IntToStrZero(StrToInt(TNFSeSubstituirNfse(Self).FNumeroNFSe), 16);

    else
      FURI := 'pedidoCancelamento_' + FPConfiguracoesNFSe.Geral.Emitente.CNPJ +
                      FPConfiguracoesNFSe.Geral.Emitente.InscMun +
                      TNFSeSubstituirNfse(Self).FNumeroNFSe;
    end;

    InicializarTagITagF;

    if FProvedor in [proIssDSF] then
    begin
      Gerador := TGerador.Create;
      try
        Gerador.ArquivoFormatoXML := '';

        for i := 0 to FNotasFiscais.Count-1 do
        begin
          with FNotasFiscais.Items[I] do
          begin
            Gerador.wGrupoNFSe('Nota Id="nota:' + NFSe.Numero + '"');
            Gerador.wCampoNFSe(tcStr, '', 'InscricaoMunicipalPrestador', 01, 11,  1, FPConfiguracoesNFSe.Geral.Emitente.InscMun, '');
            Gerador.wCampoNFSe(tcStr, '#1', 'NumeroNota', 01, 12, 1, OnlyNumber(NFSe.Numero), '');
            Gerador.wCampoNFSe(tcStr, '', 'CodigoVerificacao', 01, 255,  1, NFSe.CodigoVerificacao, '');
            Gerador.wCampoNFSe(tcStr, '', 'MotivoCancelamento', 01, 80, 1, TNFSeSubstituirNfse(Self).FMotivoCancelamento, '');
            Gerador.wGrupoNFSe('/Nota');
          end;
        end;

        FvNotas := Gerador.ArquivoFormatoXML;
      finally
        Gerador.Free;
      end;
    end;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      NumeroNFSe := TNFSeSubstituirNfse(Self).NumeroNFSe;
      CodigoCanc := TNFSeSubstituirNfse(Self).FCodigoCancelamento;
      MotivoCanc := TNFSeSubstituirNfse(Self).FMotivoCancelamento;
      NumeroRps  := TNFSeSubstituirNfse(Self).FNumeroRps;
      QtdeNotas  := FNotasFiscais.Count;
      Notas      := FvNotas;

      // Necessário para o provedor ISSDSF - CTA
      NumeroLote := FNotasFiscais.NumeroLote;
      Transacao  := FNotasFiscais.Transacao;
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    FPDadosMsg := {FTagI + }GerarDadosMsg.Gera_DadosMsgSubstituirNFSe{ + FTagF};

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  // O procedimento recebe como parametro o XML a ser assinado e retorna o
  // mesmo assinado da propriedade FPDadosMsg
  if (FPConfiguracoesNFSe.Geral.ConfigAssinar.Substituir) and (FPDadosMsg <> '') then
    AssinarXML(FPDadosMsg, FdocElemento, FinfElemento, 'Falha ao Assinar - Substituir NFS-e: ');
    
  FPDadosMsg := '<' + FPrefixo3 + 'SubstituirNfseEnvio' + FNameSpaceDad + '>' +
                '<' + FPrefixo3 + 'SubstituicaoNfse'+ Identificador + '>' +
                 SeparaDados(FPDadosMsg, FPrefixo3 + 'Pedido', True) +
                 FvNotas  + FTagF;

  if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
    FNotasFiscais.ValidarLote(FPDadosMsg,
                              FPConfiguracoes.Arquivos.PathSchemas +
                              FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoSubstituir);


  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.Substituir.IncluiEncodingDados);

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Substituir.Envelope;

  if (FPDadosMsg = '') or (FDadosEnvelope = '') then
    GerarException(ACBrStr('A funcionalidade [Substituir NFSe] não foi disponibilizada pelo provedor: ' +
                           FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeSubstituirNFSe.TratarResposta: Boolean;
var
  i: Integer;
begin
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Substituir);

  FNFSeRetorno := TRetSubsNfse.Create;
  try
    FNFSeRetorno.Leitor.Arquivo := FPRetWS;
    FNFSeRetorno.Provedor       := FProvedor;

    FNFSeRetorno.LerXml;

//    FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.Substituir);

//      FDataHora := FNFSeRetorno.InfCanc.DataHora;

    // Lista de Mensagem de Retorno
    FPMsg := '';
    FaMsg := '';
    if FNFSeRetorno.MsgRetorno.Count > 0 then
    begin
      for i := 0 to FNFSeRetorno.MsgRetorno.Count - 1 do
      begin
        FPMsg := FPMsg + FNFSeRetorno.MsgRetorno.Items[i].Mensagem + LineBreak +
                         FNFSeRetorno.MsgRetorno.Items[i].Correcao + LineBreak;

        FaMsg := FaMsg + 'Método..... : ' + LayOutToStr(FPLayout) + LineBreak +
                         'Código Erro : ' + FNFSeRetorno.MsgRetorno.Items[i].Codigo + LineBreak +
                         'Mensagem... : ' + FNFSeRetorno.MsgRetorno.Items[i].Mensagem + LineBreak +
                         'Correção... : ' + FNFSeRetorno.MsgRetorno.Items[i].Correcao + LineBreak +
                         'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
      end;
    end;
//    else FaMsg := 'Numero da NFSe : ' + FNFSeRetorno.Pedido.IdentificacaoNfse.Numero + LineBreak +
//                  'Data Hora..... : ' + ifThen(FDataHora = 0, '', DateTimeToStr(FDataHora)) + LineBreak;

    Result := (FPMsg = '');
  finally
    FNFSeRetorno.Free;
  end;
end;

procedure TNFSeSubstituirNFSe.FinalizarServico;
begin
  inherited FinalizarServico;

//  if Assigned(FRetornoNFSe) then
//    FreeAndNil(FRetornoNFSe);
end;

function TNFSeSubstituirNFSe.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeSubstituirNFSe.GerarPrefixoArquivo: String;
begin
  Result := NumeroNFSe;
end;

{ TNFSeAbrirSessao }

constructor TNFSeAbrirSessao.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeAbrirSessao.Destroy;
begin
  if Assigned(FRetornoNFSe) then
    FRetornoNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeAbrirSessao.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeAbrirSessao;
  FPLayout := LayNfseAbrirSessao;
  FPArqEnv := 'abr-ses';
  FPArqResp := 'sesA';

  FHashIdent := '';

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeAbrirSessao.DefinirURL;
begin
  FPLayout := LayNfseAbrirSessao;

  inherited DefinirURL;
end;

procedure TNFSeAbrirSessao.DefinirServicoEAction;
begin
  FPServico := 'NFSeAbrirSessao';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.AbrirSessao;

  inherited DefinirServicoEAction;
end;

procedure TNFSeAbrirSessao.DefinirDadosMsg;
begin
  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.AbrirSessao.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.AbrirSessao.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.AbrirSessao.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.AbrirSessao.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.AbrirSessao.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.AbrirSessao.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoAbrirSessao;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.AbrirSessao.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    InicializarTagITagF;

    InicializarGerarDadosMsg;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgAbrirSessao + FTagF;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  // O procedimento recebe como parametro o XML a ser assinado e retorna o
  // mesmo assinado da propriedade FPDadosMsg
  if (FPConfiguracoesNFSe.Geral.ConfigAssinar.AbrirSessao) and (FPDadosMsg <> '') then
    AssinarXML(FPDadosMsg, FTagGrupo, '', 'Falha ao Assinar - Abrir Sessão: ');
    
  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.AbrirSessao.IncluiEncodingDados);

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.AbrirSessao.Envelope;

  if (FPDadosMsg = '') or (FDadosEnvelope = '') then
    GerarException(ACBrStr('A funcionalidade [Abrir Sessão] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeAbrirSessao.TratarResposta: Boolean;
var
  i: Integer;
begin
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.AbrirSessao);

  FRetAbrirSessao := TRetAbrirSessao.Create;
  try
    FRetAbrirSessao.Leitor.Arquivo := FPRetWS;
    FRetAbrirSessao.Provedor       := FProvedor;

    FRetAbrirSessao.LerXml;

    FHashIdent := FRetAbrirSessao.InfAbrirSessao.HashIdent;

//    FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.AbrirSessao);

    // Lista de Mensagem de Retorno
    FPMsg := '';
    FaMsg := '';
    if FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Count > 0 then
    begin
      for i := 0 to FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Count - 1 do
      begin
        FPMsg := FPMsg + FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Mensagem + LineBreak +
                         FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Correcao + LineBreak;

        FaMsg := FaMsg + 'Método..... : ' + LayOutToStr(FPLayout) + LineBreak +
                         'Código Erro : ' + FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Codigo + LineBreak +
                         'Mensagem... : ' + FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Mensagem + LineBreak +
                         'Correção... : ' + FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Correcao + LineBreak +
                         'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
      end;
    end;
//    else FaMsg := 'Numero da NFSe : ' + FNFSeRetorno.Pedido.IdentificacaoNfse.Numero + LineBreak +
//                  'Data Hora..... : ' + ifThen(FDataHora = 0, '', DateTimeToStr(FDataHora)) + LineBreak;

    Result := (FPMsg = '');
  finally
    FRetAbrirSessao.Free;
  end;
end;

procedure TNFSeAbrirSessao.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeAbrirSessao.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeAbrirSessao.GerarPrefixoArquivo: String;
begin
  Result := NumeroLote;
end;

{ TNFSeFecharSessao }

constructor TNFSeFecharSessao.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFSeFecharSessao.Destroy;
begin
  if Assigned(FRetornoNFSe) then
    FRetornoNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeFecharSessao.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeFecharSessao;
  FPLayout := LayNfseFecharSessao;
  FPArqEnv := 'fec-ses';
  FPArqResp := 'sesF';

  if Assigned(FRetornoNFSe) then
    FreeAndNil(FRetornoNFSe);
end;

procedure TNFSeFecharSessao.DefinirURL;
begin
  FPLayout := LayNfseFecharSessao;

  inherited DefinirURL;
end;

procedure TNFSeFecharSessao.DefinirServicoEAction;
begin
  FPServico := 'NFSeFecharSessao';
  FPSoapAction := FPConfiguracoesNFSe.Geral.ConfigSoapAction.FecharSessao;

  inherited DefinirServicoEAction;
end;

procedure TNFSeFecharSessao.DefinirDadosMsg;
begin
  FCabecalhoStr := FPConfiguracoesNFSe.Geral.ConfigEnvelope.FecharSessao.CabecalhoStr;
  FDadosStr     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.FecharSessao.DadosStr;
  FTagGrupo     := FPConfiguracoesNFSe.Geral.ConfigEnvelope.FecharSessao.TagGrupo;
  FTagElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.FecharSessao.TagElemento;
  FDocElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.FecharSessao.DocElemento;
  FInfElemento  := FPConfiguracoesNFSe.Geral.ConfigEnvelope.FecharSessao.InfElemento;

  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoFecharSessao;

  InicializarDadosMsg(FPConfiguracoesNFSe.Geral.ConfigEnvelope.FecharSessao.IncluiEncodingCab);

  GerarDadosMsg := TNFSeG.Create;
  try
    if FTagGrupo <> '' then
      FTagGrupo := FPrefixo3 + FTagGrupo;

    InicializarTagITagF;

    InicializarGerarDadosMsg;

    with GerarDadosMsg do
    begin
      HashIdent := TNFSeFecharSessao(Self).HashIdent;
    end;

    AjustarOpcoes( GerarDadosMsg.Gerador.Opcoes );

    FPDadosMsg := FTagI + GerarDadosMsg.Gera_DadosMsgFecharSessao + FTagF;

    FIDLote := GerarDadosMsg.IdLote;
  finally
    GerarDadosMsg.Free;
  end;

  // O procedimento recebe como parametro o XML a ser assinado e retorna o
  // mesmo assinado da propriedade FPDadosMsg
  if (FPConfiguracoesNFSe.Geral.ConfigAssinar.FecharSessao) and (FPDadosMsg <> '') then
    AssinarXML(FPDadosMsg, FTagGrupo, '', 'Falha ao Assinar - Fechar Sessão: ');

  IncluirEncoding(FPConfiguracoesNFSe.Geral.ConfigEnvelope.FecharSessao.IncluiEncodingDados);

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.FecharSessao.Envelope;

  if (FPDadosMsg = '') or (FDadosEnvelope = '') then
    GerarException(ACBrStr('A funcionalidade [Fechar Sessão] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeFecharSessao.TratarResposta: Boolean;
//var
//  i: Integer;
begin
  FPRetWS := ExtrairRetorno(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.GrupoMsg,
                            FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.FecharSessao);

//  FRetAbrirSessao := TRetAbrirSessao.Create;
//  try
//    FRetAbrirSessao.Leitor.Arquivo := FPRetWS;
//    FRetAbrirSessao.Provedor       := FProvedor;

//    FRetAbrirSessao.LerXml;

//    FPRetWS := ExtrairGrupoMsgRet(FPConfiguracoesNFSe.Geral.ConfigGrupoMsgRet.FecharSessao);

    // Lista de Mensagem de Retorno
    FPMsg := '';
    FaMsg := '';
    (*
    if FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Count > 0 then
    begin
      for i := 0 to FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Count - 1 do
      begin
        FPMsg := FPMsg + FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Mensagem + LineBreak +
                         FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Correcao + LineBreak;

        FaMsg := FaMsg + 'Método..... : ' + LayOutToStr(FPLayout) + LineBreak +
                         'Código Erro : ' + FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Codigo + LineBreak +
                         'Mensagem... : ' + FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Mensagem + LineBreak +
                         'Correção... : ' + FRetAbrirSessao.InfAbrirSessao.MsgRetorno.Items[i].Correcao + LineBreak +
                         'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
      end;
    end;
//    else FaMsg := 'Numero da NFSe : ' + FNFSeRetorno.Pedido.IdentificacaoNfse.Numero + LineBreak +
//                  'Data Hora..... : ' + ifThen(FDataHora = 0, '', DateTimeToStr(FDataHora)) + LineBreak;
    *)
    Result := (FPMsg = '');
//  finally
//    FRetAbrirSessao.Free;
//  end;
end;

procedure TNFSeFecharSessao.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeFecharSessao.GerarMsgLog: String;
begin
  Result := ACBrStr(FaMsg)
end;

function TNFSeFecharSessao.GerarPrefixoArquivo: String;
begin
  Result := NumeroLote;
end;

{ TNFSeEnvioWebService }

constructor TNFSeEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
end;

destructor TNFSeEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

procedure TNFSeEnvioWebService.Clear;
begin
  inherited Clear;

  FPStatus := stNFSeEnvioWebService;
  FVersao := '';
end;

function TNFSeEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TNFSeEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TNFSeEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;

  inherited DefinirServicoEAction;
end;

procedure TNFSeEnvioWebService.DefinirDadosMsg;
var
  LeitorXML: TLeitor;
begin
  FCabecalhoStr:= FPConfiguracoesNFSe.Geral.ConfigXML.CabecalhoStr;
  FDadosStr:= FPConfiguracoesNFSe.Geral.ConfigXML.DadosStr;

  LeitorXML := TLeitor.Create;
  try
    LeitorXML.Arquivo := FXMLEnvio;
    LeitorXML.Grupo := FXMLEnvio;
    FVersao := LeitorXML.rAtributo('versao')
  finally
    LeitorXML.Free;
  end;

  FPDadosMsg := FXMLEnvio;
end;

function TNFSeEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TNFSeEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TNFSeEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrNFSe := TACBrNFSe(AOwner);

  FGerarLoteRPS   := TNFSeGerarLoteRPS.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FEnviarLoteRPS  := TNFSeEnviarLoteRPS.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FEnviarSincrono := TNFSeEnviarSincrono.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FGerarNfse      := TNFSeGerarNfse.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FConsSitLoteRPS := TNFSeConsultarSituacaoLoteRPS.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FConsLote       := TNFSeConsultarLoteRPS.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FConsNfseRps    := TNFSeConsultarNfseRps.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FConsNfse       := TNFSeConsultarNfse.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FCancNfse       := TNFSeCancelarNfse.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FSubNfse        := TNFSeSubstituirNfse.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FAbrirSessao    := TNFSeAbrirSessao.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FFecharSessao   := TNFSeFecharSessao.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);

  FEnvioWebService := TNFSeEnvioWebService.Create(FACBrNFSe);

  FTesteEnvioLoteRPS := TNFSeTesteEnvioLoteRPS.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
end;

destructor TWebServices.Destroy;
begin
  FGerarLoteRPS.Free;
  FEnviarLoteRPS.Free;
  FEnviarSincrono.Free;
  FGerarNfse.Free;
  FConsSitLoteRPS.Free;
  FConsLote.Free;
  FConsNfseRps.Free;
  FConsNfse.Free;
  FCancNfse.Free;
  FSubNfse.Free;
  FAbrirSessao.Free;
  FFecharSessao.Free;

  FEnvioWebService.Free;

  FTesteEnvioLoteRPS.Free;

  inherited Destroy;
end;

function TWebServices.GeraLote(ALote: Integer; AqMaxRps: Integer;
  ASincrono: Boolean): Boolean;
begin
  Result := GeraLote(IntToStr(ALote), AqMaxRps, ASincrono);
end;

function TWebServices.GeraLote(const ALote: String; AqMaxRps: Integer;
  ASincrono: Boolean): Boolean;
begin
  FGerarLoteRPS.FNumeroLote := ALote;
  FGerarLoteRPS.FqMaxRps    := AqMaxRps;
  FGerarLoteRPS.FSincrono   := ASincrono;

  Result := GerarLoteRPS.Executar;

  if not (Result) then
    GerarLoteRPS.GerarException( GerarLoteRPS.Msg );
end;

function TWebServices.Envia(ALote: Integer): Boolean;
begin
  Result := Envia(IntToStr(ALote));
end;

function TWebServices.Envia(const ALote: String): Boolean;
var
  Tentativas, IntervaloTentativas: Integer;
begin
  if TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor = proEL then
  begin
    FAbrirSessao.FNumeroLote := ALote;

    Result := FAbrirSessao.Executar;

    FEnviarLoteRPS.FHashIdent := FAbrirSessao.FHashIdent;

    if not (Result) then
      FAbrirSessao.GerarException( FAbrirSessao.Msg );
  end;

  FEnviarLoteRPS.FNumeroLote := ALote;
  FEnviarLoteRPS.FqMaxRps    := 50;

  Result := FEnviarLoteRPS.Executar;

  if not (Result) then
    FEnviarLoteRPS.GerarException( FEnviarLoteRPS.Msg );

  if TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor = proEL then
  begin
    FFecharSessao.FNumeroLote := ALote;
    FFecharSessao.FHashIdent := FEnviarLoteRPS.HashIdent;

    Result := FFecharSessao.Executar;

    if not (Result) then
      FFecharSessao.GerarException( FFecharSessao.Msg );
  end;

  FConsSitLoteRPS.FProtocolo  := FEnviarLoteRPS.Protocolo;
  FConsSitLoteRPS.FNumeroLote := FEnviarLoteRPS.NumeroLote;

  FConsLote.FProtocolo := FEnviarLoteRPS.Protocolo;
  FConsLote.FNumeroLote := FEnviarLoteRPS.NumeroLote;

  with TACBrNFSe(FACBrNFSe) do
  begin
    if (Configuracoes.Geral.ConsultaLoteAposEnvio) and (Result) then
    begin
      //==========================================================================
      // Provedores que seguem a versão 1.0 do layout da ABRASF devem primeiro
        // Consultar a Situação do Lote
      if ProvedorToVersaoNFSe(Configuracoes.Geral.Provedor) = ve100 then
      begin
        // Provedores cuja versão é 1.0 mas não possuem o método Consulta
        // a Situação do Lote devem ser relacionados no case abaixo.
        case Configuracoes.Geral.Provedor of
          proGoverna,
          proIPM,
          proIssDSF: Result := True
        else
          Result := FConsSitLoteRPS.Executar;
        end;

        if not (Result) then
          FConsSitLoteRPS.GerarException( FConsSitLoteRPS.Msg );
      end;

      // Provedores que não possuem o método Consultar o Lote devem ser
      // relacionados no case abaixo.
      case Configuracoes.Geral.Provedor of
        proGoverna,
        proIPM,
        proInfisc,
        proInfiscv11: Result := True
      else
        begin
          Sleep(Configuracoes.WebServices.AguardarConsultaRet);

          Result := FConsLote.Executar;

          // O código abaixo tem por objetivo repetir a consulta ao lote
          // quando no retorno constar que o lote ainda se encontra em processamento
          // não sabemos se vai funcionar como o esperado.
          //****************************************************************
          if ProvedorToVersaoNFSe(Configuracoes.Geral.Provedor) = ve200 then
          begin
            try
              Tentativas := 0;
              IntervaloTentativas := max(Configuracoes.WebServices.IntervaloTentativas, 1000);

              while (FConsLote.FLoteNaoProc) and
                      (Tentativas < Configuracoes.WebServices.Tentativas) do
              begin
                Inc(Tentativas);
                sleep(IntervaloTentativas);

                Result := FConsLote.Executar;
              end;
            finally
              SetStatus(stNFSeIdle);
            end;
          end;
          //****************************************************************
        end;
      end;

      if not (Result) then
        FConsLote.GerarException( FConsLote.Msg );
    end;
  end;
end;

function TWebServices.EnviaSincrono(ALote: Integer): Boolean;
begin
  Result := EnviaSincrono(IntToStr(ALote));
end;

function TWebServices.EnviaSincrono(const ALote: String): Boolean;
var
  Tentativas, IntervaloTentativas: Integer;
begin
  FEnviarSincrono.FNumeroLote := ALote;

  Result := FEnviarSincrono.Executar;

  if not (Result) then
    FEnviarSincrono.GerarException( FEnviarSincrono.Msg );

  if not FEnviarSincrono.FNotaRetornada then
  begin
    // Alguns provedores requerem que sejam feitas as consultas para obter o XML
    // da NFS-e
    FConsSitLoteRPS.FProtocolo  := FEnviarSincrono.Protocolo;
    FConsSitLoteRPS.FNumeroLote := FEnviarSincrono.NumeroLote;

    FConsLote.FProtocolo := FEnviarSincrono.Protocolo;

    with TACBrNFSe(FACBrNFSe) do
    begin
      if (Configuracoes.Geral.ConsultaLoteAposEnvio) and (Result) then
      begin
        if ProvedorToVersaoNFSe(Configuracoes.Geral.Provedor) = ve100 then
        begin
          Result := FConsSitLoteRPS.Executar;

          if not (Result) then
            FConsSitLoteRPS.GerarException( FConsSitLoteRPS.Msg );
        end;

        case Configuracoes.Geral.Provedor of
          proInfisc,
          proInfiscv11: Result := True
        else
          begin
            if (Configuracoes.Geral.Provedor = pro4R) and
               (Configuracoes.WebServices.Ambiente = taHomologacao) then
              Result := True
            else
            begin
              Sleep(Configuracoes.WebServices.AguardarConsultaRet);

              Result := FConsLote.Executar;

              // O código abaixo tem por objetivo repetir a consulta ao lote
              // quando no retorno constar que o lote ainda se encontra em processamento
              // não sabemos se vai funcionar como o esperado.
              //****************************************************************
              if (ProvedorToVersaoNFSe(Configuracoes.Geral.Provedor) = ve200) or
                 (Configuracoes.Geral.Provedor in [proIssDSF]) then
              begin
                try
                  Tentativas := 0;
                  IntervaloTentativas := max(Configuracoes.WebServices.IntervaloTentativas, 1000);

                  while (FConsLote.FLoteNaoProc) and
                          (Tentativas < Configuracoes.WebServices.Tentativas) do
                  begin
                    Inc(Tentativas);
                    sleep(IntervaloTentativas);

                    Result := FConsLote.Executar;
                  end;
                finally
                  SetStatus(stNFSeIdle);
                end;
              end;
              //****************************************************************
            end;
          end;
        end;

        if not (Result) then
          FConsLote.GerarException( FConsLote.Msg );
      end;
    end;
  end;
end;

function TWebServices.Gera(ARps: Integer; ALote: Integer): Boolean;
begin
 FGerarNfse.FNumeroRps  := IntToStr(ARps);
 FGerarNfse.FNumeroLote := IntToStr(ALote);

 Result := FGerarNfse.Executar;

 if not (Result) then
   FGerarNfse.GerarException( FGerarNfse.Msg );
end;

function TWebServices.ConsultaSituacao(const AProtocolo: String;
  const ANumLote: String): Boolean;
begin
  FConsSitLoteRPS.FProtocolo  := AProtocolo;
  FConsSitLoteRPS.FNumeroLote := ANumLote;

  Result := FConsSitLoteRPS.Executar;

  if not (Result) then
   FConsSitLoteRPS.GerarException( FConsSitLoteRPS.Msg );
end;

function TWebServices.ConsultaLoteRps(const ANumLote, AProtocolo: String): Boolean;
begin
  FConsLote.FNumeroLote := ANumLote;
  FConsLote.FProtocolo  := AProtocolo;

  Result := FConsLote.Executar;

  if not (Result) then
    FConsLote.GerarException( FConsLote.Msg );
end;

function TWebServices.ConsultaNFSeporRps(const ANumero, ASerie, ATipo: String;
                                         const ANumLote: String = ''): Boolean;
begin
  FConsNfseRps.FNumeroRps  := ANumero;
  FConsNfseRps.FSerie      := ASerie;
  FConsNfseRps.FTipo       := ATipo;
  FConsNfseRps.FNumeroLote := ANumLote;

  Result := FConsNfseRps.Executar;

  if not (Result) then
    FConsNfseRps.GerarException( FConsNfseRps.Msg );
end;

function TWebServices.ConsultaNFSe(ADataInicial, ADataFinal: TDateTime;
  const NumeroNFSe: String; APagina: Integer; const ACNPJTomador, AIMTomador, ANomeInter,
  ACNPJInter, AIMInter, ASerie: String): Boolean;
begin
  FConsNfse.FDataInicial := ADataInicial;
  FConsNfse.FDataFinal   := ADataFinal;
  FConsNfse.FNumeroNFSe  := NumeroNFSe;
  FConsNfse.FPagina      := APagina;
  FConsNfse.FCNPJTomador := ACNPJTomador;
  FConsNfse.FIMTomador   := AIMTomador;
  FConsNfse.FNomeInter   := ANomeInter;
  FConsNfse.FCNPJInter   := ACNPJInter;
  FConsNfse.FIMInter     := AIMInter;
  FConsNfse.FSerie       := ASerie;

  Result := FConsNfse.Executar;

  if not (Result) then
    FConsNfse.GerarException( FConsNfse.Msg );
end;

function TWebServices.CancelaNFSe(const ACodigoCancelamento: String;
  const ANumeroNFSe: String = ''; const AMotivoCancelamento: String = '';
  const ANumLote: String = ''): Boolean;
begin
  FCancNfse.FCodigoCancelamento := ACodigoCancelamento;
  FCancNfse.FNumeroNFSe         := ANumeroNFSe;
  FCancNfse.FMotivoCancelamento := AMotivoCancelamento;
  FCancNfse.FNumeroLote         := ANumLote;

  Result := FCancNfse.Executar;

  if not (Result) then
    FCancNfse.GerarException( FCancNfse.Msg );

  with TACBrNFSe(FACBrNFSe) do
  begin
    if not (Configuracoes.Geral.Provedor in [proABase, proCONAM, proEL, proISSNet,
                                             proSMARAPD, proIPM]) then
    begin
      if Configuracoes.Geral.Provedor in [proSystemPro] then
      begin
        Sleep(Configuracoes.WebServices.AguardarConsultaRet);

        FConsNfse.FNumeroNFSe := NotasFiscais.Items[0].NFSe.Numero;
        // Utilizado por alguns provedores para realizar a consulta de uma NFS-e
        FConsNfse.FPagina     := 1;

        Result := FConsNfse.Executar;

        if not (Result) then
          FConsNfse.GerarException( FConsNfse.Msg );
      end
      else
      begin
        case Configuracoes.Geral.Provedor of
          proInfisc,
          proInfiscv11,
          proSafeWeb,
          proTiplanv2,
          proWebISSv2 : Result := True
        else
          begin
            Sleep(Configuracoes.WebServices.AguardarConsultaRet);

            FConsNfseRps.FNumeroRps := NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero;
            FConsNfseRps.FSerie     := NotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie;
            FConsNfseRps.FTipo      := TipoRPSToStr(NotasFiscais.Items[0].NFSe.IdentificacaoRps.Tipo);

            Result := FConsNfseRps.Executar;
          end;
        end;

        if not (Result) then
          FConsNfseRps.GerarException( FConsNfseRps.Msg );
      end;
    end;
  end;
end;

function TWebServices.SubstituiNFSe(const ACodigoCancelamento, ANumeroNFSe: String;
  const AMotivoCancelamento: String): Boolean;
begin
  Result := False;

  FSubNfse.FNumeroNFSe         := ANumeroNFSe;
  FSubNfse.FCodigoCancelamento := ACodigoCancelamento;

  if TACBrNFSe(FACBrNFSe).NotasFiscais.Count <= 0 then
    FSubNfse.GerarException(ACBrStr('ERRO: Nenhum RPS adicionado ao Lote'))
  else begin
    FSubNfse.FNumeroRps         := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero;
    FSubNfse.MotivoCancelamento := AMotivoCancelamento;

    Result := FSubNfse.Executar;

    if not (Result) then
      FSubNfse.GerarException( FSubNfse.Msg );
  end;
end;

function TWebServices.TestaEnvio(const ALote: String): Boolean;
begin

  FTesteEnvioLoteRPS.FNumeroLote := ALote;
  Result := FTesteEnvioLoteRPS.Executar;

  if not (Result) then
    FTesteEnvioLoteRPS.GerarException( FTesteEnvioLoteRPS.Msg );

  FConsSitLoteRPS.FProtocolo  := FTesteEnvioLoteRPS.Protocolo;
  FConsSitLoteRPS.FNumeroLote := FTesteEnvioLoteRPS.NumeroLote;

  FConsLote.FProtocolo := FTesteEnvioLoteRPS.Protocolo;
  FConsLote.FNumeroLote := FTesteEnvioLoteRPS.NumeroLote;

  if (TACBrNFSe(FACBrNFSe).Configuracoes.Geral.ConsultaLoteAposEnvio) and (Result) then
  begin
    if ProvedorToVersaoNFSe(TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor) = ve100 then
    begin
      if TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor in [proGoverna] then
        Result := True
      else
        Result := FConsSitLoteRPS.Executar;

      if not (Result) then
        FConsSitLoteRPS.GerarException( FConsSitLoteRPS.Msg );
    end;

    case TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor of
      proEL,
      proGoverna,
      proInfisc,
      proInfiscv11: Result := True
    else
      Result := FConsLote.Executar;
    end;

    if not (Result) then
      FConsLote.GerarException( FConsLote.Msg );
  end;
end;

end.
