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
  Classes, SysUtils,
  ACBrDFe, ACBrDFeWebService,
  pnfsNFSe, pcnAuxiliar, pcnConversao, pnfsConversao,
  ACBrNFSeNotasFiscais, ACBrNFSeConfiguracoes,

  pnfsEnvLoteRpsResposta, pnfsConsSitLoteRpsResposta,
  pnfsConsLoteRpsResposta, pnfsConsNfseporRpsResposta,
  pnfsConsNfseResposta, pnfsCancNfseResposta,
  pnfsGerarNfseResposta, pnfsSubsNfseResposta;

type

  { TNFSeWebService }

  TNFSeWebService = class(TDFeWebService)
  private
  protected
    FPConfiguracoesNFSe: TConfiguracoesNFSe;
    FPStatus: TStatusACBrNFSe;
    FPLayout: TLayOutNFSe;
    FNameSpaceDad: String;
    FNameSpaceCab: String;
    FURI: String;
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
    FvNotas: String;

    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    function GerarCabecalhoSoap: String; override;
    procedure FinalizarServico; override;
    procedure DefinirEnvelopeSoap; override;
    procedure InicializarDadosMsg; 

  public
    constructor Create(AOwner: TACBrDFe); override;

    property Status: TStatusACBrNFSe read FPStatus;
    property Layout: TLayOutNFSe read FPLayout;
    property NameSpaceCab: String read FNameSpaceCab;
    property NameSpaceDad: String read FNameSpaceDad;
    property URI: String read FURI;
    property TagI: String read FTagI;
    property TagF: String read FTagF;
    property DadosSenha: String read FDadosSenha;
    property DadosEnvelope: String read FDadosEnvelope;
    property aMsg: String read FaMsg;
    property Separador: String read FSeparador;
    property Prefixo2: String read FPrefixo2;
    property Prefixo3: String read FPrefixo3;
    property Prefixo4: String read FPrefixo4;
    property NameSpace: String read FNameSpace;
    property DefTipos: String read FDefTipos;
    property Cabecalho: String read FCabecalho;
    property xsdServico: String read FxsdServico;
    property vNotas: String read FvNotas;
  end;

  { TNFSeGerarLoteRPS }

  TNFSeGerarLoteRPS = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;

    FNumeroLote: String;
  protected
    procedure EnviarDados; override;
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

    property NumeroLote: String read FNumeroLote;
  end;

  { TNFSeEnviarLoteRPS }

  TNFSeEnviarLoteRPS = class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNFSeRetorno: TretEnvLote;

    FNumeroLote: String;
    FDataRecebimento: TDateTime;
    FProtocolo: String;

    function GetLote: String;
    function GetProtocolo: String;
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

    property NFSeRetorno: TretEnvLote read FNFSeRetorno write FNFSeRetorno;

    property NumeroLote: String read FNumeroLote;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Protocolo: String read FProtocolo;
  end;

{ TNFSeEnviarSincrono }

  TNFSeEnviarSincrono = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNFSeRetorno: TGerarretNfse;

    FNumeroLote: String;
    FProtocolo: String;
    FDataRecebimento: TDateTime;
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

    property NFSeRetorno: TGerarretNfse read FNFSeRetorno write FNFSeRetorno;

    property NumeroLote: String read FNumeroLote;
    property Protocolo: String read FProtocolo;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Situacao: String read FSituacao;
  end;

{ TNFSeGerarNFSe }

  TNFSeGerarNFSe = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNFSeRetorno: TGerarretNfse;

    FNumeroRps: Integer;
    FProtocolo: String;
    FDataRecebimento: TDateTime;
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

    property NFSeRetorno: TGerarretNfse read FNFSeRetorno write FNFSeRetorno;

    property NumeroRps: Integer read FNumeroRps;
    property Protocolo: String read FProtocolo;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Situacao: String read FSituacao;
  end;

{ TNFSeConsultarSituacaoLoteRPS }

  TNFSeConsultarSituacaoLoteRPS = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNFSeRetorno: TRetSitLote;

    FCnpj: String;
    FInscricaoMunicipal: String;
    FProtocolo: String;
    FNumeroLote: String;
    FSituacao: String;
    FSenha: String;
    FFraseSecreta: String;

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

    property NFSeRetorno: TRetSitLote read FNFSeRetorno write FNFSeRetorno;

    property Cnpj: String read FCnpj write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property Protocolo: String read FProtocolo write FProtocolo;
    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Situacao: String read FSituacao;
    property Senha: String read FSenha write FSenha;
    property FraseSecreta: String read FFraseSecreta write FFraseSecreta;
  end;

{ TNFSeConsultarLoteRPS }

  TNFSeConsultarLoteRPS = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNFSeRetorno: TRetLote;

    FProtocolo: String;
    FNumeroLote: String;
    FCNPJ: String;
    FIM: String;
    FSenha: String;
    FFraseSecreta: String;
    FArquivoRetorno: String;
    FRazaoSocial: String;

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

//    property NotasFiscais: TNotasFiscais read FNotasFiscais;
    property NFSeRetorno: TRetLote read FNFSeRetorno write FNFSeRetorno;

    property Protocolo: String read FProtocolo write FProtocolo;
      //usado pelo provedor IssDsf
    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property CNPJ: String read FCNPJ write FCNPJ;
    property IM: String read FIM write FIM;
    property Senha: String read FSenha write FSenha;
    property FraseSecreta: String read FFraseSecreta write FFraseSecreta;
    property ArquivoRetorno: String read FArquivoRetorno write FArquivoRetorno;
    //usado pelo provedor Tecnos
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
  end;

{ TNFSeConsultarNfseRPS }

  TNFSeConsultarNfseRPS = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNFSeRetorno: TRetNfseRps;

    FNumero: String;
    FSerie: String;
    FTipo: String;
    FCnpj: String;
    FInscricaoMunicipal: String;
    FSenha: String;
    FFraseSecreta: String;
    FRazaoSocial: String;

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

    property NFSeRetorno: TRetNfseRps read FNFSeRetorno write FNFSeRetorno;

    property Numero: String read FNumero write FNumero;
    property Serie: String read FSerie write FSerie;
    property Tipo: String read FTipo write FTipo;
    property Cnpj: String read FCnpj write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property Senha: String read FSenha write FSenha;
    property FraseSecreta: String read FFraseSecreta write FFraseSecreta;
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
  end;

{ TNFSeConsultarNfse }

  TNFSeConsultarNfse = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNFSeRetorno: TRetNfse;

    FCnpj: String;
    FInscricaoMunicipal: String;
    FDataInicial: TDateTime;
    FDataFinal: TDateTime;
    FNumeroNFSe: String;
    FPagina: Integer;
    FSenha: String;
    FFraseSecreta: String;
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

    property NFSeRetorno: TRetNfse      read FNFSeRetorno        write FNFSeRetorno;

    property Cnpj: String               read FCnpj               write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property DataInicial: TDateTime     read FDataInicial        write FDataInicial;
    property DataFinal: TDateTime       read FDataFinal          write FDataFinal;
    property NumeroNFSe: String         read FNumeroNFSe         write FNumeroNFSe;
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

{ TNFSeCancelarNfse }

  TNFSeCancelarNfse = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNFSeRetorno: TretCancNFSe;

    FCodigoCancelamento: String;
    FMotivoCancelamento: String;
    FDataHora: TDateTime;
    FCNPJ: String;
    FIM: String;
    FNumeroNFSe: String;
    FCodigoMunicipio: String;
    FArquivoRetorno: String;

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

    property NFSeRetorno: TretCancNFSe read FNFSeRetorno write FNFSeRetorno;

    property CodigoCancelamento: String read FCodigoCancelamento write FCodigoCancelamento;
    property MotivoCancelamento: String read FMotivoCancelamento write FMotivoCancelamento;
    property DataHora: TDateTime read FDataHora write FDataHora;
    property NumeroNFSe: String read FNumeroNFSe write FNumeroNFSe;
    property CNPJ: String read FCNPJ write FCNPJ;
    property IM: String read FIM write FIM;
    property CodigoMunicipio: String read FCodigoMunicipio write FCodigoMunicipio;
    property ArquivoRetorno: String read FArquivoRetorno write FArquivoRetorno;
  end;

{ TNFSeSubstituirNFSe }

 TNFSeSubstituirNFSe = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FNFSeRetorno: TretSubsNfse;

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

    FArquivoRetorno: String;

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

    property NFSeRetorno: TretSubsNfse  read FNFSeRetorno    write FNFSeRetorno;

    property CodigoCancelamento: String read FCodigoCancelamento write FCodigoCancelamento;
    property MotivoCancelamento: String read FMotivoCancelamento write FMotivoCancelamento;
    property DataHora: TDateTime        read FDataHora           write FDataHora;
    property NumeroNFSe: String         read FNumeroNFSe         write FNumeroNFSe;
    property CNPJ: String               read FCNPJ               write FCNPJ;
    property IM: String                 read FIM                 write FIM;
    property CodigoMunicipio: String    read FCodigoMunicipio    write FCodigoMunicipio;

    property NumeroRps: Integer         read FNumeroRps;
    property Protocolo: String          read FProtocolo;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Situacao: String           read FSituacao;

    property ArquivoRetorno: String read FArquivoRetorno write FArquivoRetorno;
  end;

{ TNFSeLinkNFSe }

  TNFSeLinkNFSe = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    
    FNumeroNFSe: Integer;
    FCodVerif: String;
    FLink: String;
    FIM: String;

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

    property NumeroNFSe: Integer read FNumeroNFSe;
    property CodVerif: String read FCodVerif;
    property Link: String read FLink;
    property IM: String read FIM;
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
    function Executar: Boolean; override;

    property XMLEnvio: String read FXMLEnvio write FXMLEnvio;
    property URLEnvio: String read FPURLEnvio write FPURLEnvio;
    property SoapActionEnvio: String read FSoapActionEnvio write FSoapActionEnvio;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrNFSe: TACBrDFe;
    FGerarLoteRPS: TNFSeGerarLoteRPS;
    FEnviarLoteRPS: TNFSeEnviarLoteRPS;
    FEnviarSincrono: TNFSeEnviarSincrono;
    FGerarNfse: TNFSeGerarNfse;
    FConsSitLoteRPS: TNFSeConsultarSituacaoLoteRPS;
    FConsLote: TNFSeConsultarLoteRPS;
    FConsNfseRps: TNFSeConsultarNfseRps;
    FConsNfse: TNFSeConsultarNfse;
    FCancNfse: TNFSeCancelarNfse;
    FSubNfse: TNFSeSubstituirNfse;
    FLinkNfse: TNFSeLinkNfse;
    FEnvioWebService: TNFSeEnvioWebService;

  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function GeraLote(ALote: Integer): Boolean; overload;
    function GeraLote(ALote: String): Boolean; overload;

    function Envia(ALote: Integer): Boolean; overload;
    function Envia(ALote: String): Boolean; overload;

    function EnviaSincrono(ALote:Integer): Boolean; overload;
    function EnviaSincrono(ALote:String): Boolean; overload;

    function Gera(ARps: Integer): Boolean;

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

    function CancelaNFSe(ACodigoCancelamento: String;
                         const CarregaProps: boolean = true): Boolean; overload;
    function CancelaNFSe(ACodigoCancelamento, ANumeroNFSe, ACNPJ, AInscricaoMunicipal,
                         ACodigoMunicipio: String): Boolean; overload;

    function SubstitiNFSe(ACodigoCancelamento, ANumeroNFSe: String): Boolean; 

    function LinkNFSeGerada(ANumeroNFSe: Integer; ACodVerificacao, AInscricaoM: String): String;

    property ACBrNFSe: TACBrDFe read FACBrNFSe write FACBrNFSe;
    property GerarLoteRPS: TNFSeGerarLoteRPS read FGerarLoteRPS write FGerarLoteRPS;
    property EnviarLoteRPS: TNFSeEnviarLoteRPS read FEnviarLoteRPS write FEnviarLoteRPS;
    property EnviarSincrono: TNFSeEnviarSincrono read FEnviarSincrono write FEnviarSincrono;
    property GerarNfse: TNFSeGerarNfse read FGerarNfse write FGerarNfse;
    property ConsSitLoteRPS: TNFSeConsultarSituacaoLoteRPS read FConsSitLoteRPS write FConsSitLoteRPS;
    property ConsLote: TNFSeConsultarLoteRPS read FConsLote write FConsLote;
    property ConsNfseRps: TNFSeConsultarNfseRps read FConsNfseRps write FConsNfseRps;
    property ConsNfse: TNFSeConsultarNfse read FConsNfse write FConsNfse;
    property CancNfse: TNFSeCancelarNfse read FCancNfse write FCancNfse;
    property SubNfse: TNFSeSubstituirNfse read FSubNfse write FSubNfse;
    property LinkNfse: TNFSeLinkNfse read FLinkNfse write FLinkNfse;
    property EnvioWebService: TNFSeEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil, ACBrNFSe, pnfsNFSeG,
  pcnGerador, pcnLeitor;

{ TNFSeWebService }

constructor TNFSeWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesNFSe := TConfiguracoesNFSe(FPConfiguracoes);
  FPLayout := LayNfseRecepcaoLote;
  FPStatus := stNFSeIdle;
end;

procedure TNFSeWebService.DefinirEnvelopeSoap;
var
  Texto: String;
begin
  {$IFDEF UNICODE}
   Texto := '<' + ENCODING_UTF8 + '>';    // Envelope já está sendo montado em UTF8
  {$ELSE}
   Texto := '';  // Isso forçará a conversão para UTF8, antes do envio
  {$ENDIF}

  Texto := FDadosEnvelope;
  // %CabMsg%   : Representa a Mensagem de Cabeçalho
  // %DadosMsg% : Representa a Mensagem de Dados
  Texto := StringReplace(Texto, '%CabMsg%', FPCabMsg, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%DadosMsg%', FPDadosMsg, [rfReplaceAll]);

  FPEnvelopeSoap := Texto;
end;

procedure TNFSeWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrNFSe(FPDFeOwner).SetStatus(FPStatus);
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

function TNFSeWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrNFSe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TNFSeWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrNFSe(FPDFeOwner).SetStatus(stNFSeIdle);
end;

function TNFSeWebService.GerarCabecalhoSoap: String;
begin
  Result := FPCabMsg;
end;

procedure TNFSeWebService.InicializarDadosMsg;
var
  Texto: String;
begin
  FvNotas    := '';
  FURI       := '';
  FNameSpace := FPConfiguracoesNFSe.Geral.ConfigXML.NameSpace;
  FDefTipos  := FPConfiguracoesNFSe.Geral.ConfigSchemas.DefTipos;
  FCabecalho := FPConfiguracoesNFSe.Geral.ConfigSchemas.Cabecalho;
  FPrefixo2  := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo2;
  FPrefixo3  := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo3;
  FPrefixo4  := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo4;
  FPCabMsg   := FPConfiguracoesNFSe.Geral.ConfigEnvelope.CabecalhoMsg;

  if RightStr(FNameSpace, 1) = '/' then
    FSeparador := ''
  else
    FSeparador := '/';

  if FCabecalho <> '' then
  begin
    if FPrefixo2 <> '' then
      FNameSpaceCab := ' xmlns:' + StringReplace(FPrefixo2, ':', '', []) +
                       '="' + FNameSpace + FSeparador + FCabecalho +'">'
    else
      FNameSpaceCab := ' xmlns="' + FNameSpace + FSeparador + FCabecalho +'">';
  end
  else
    FNameSpaceCab := '>';

  if FxsdServico <> '' then
  begin
    case FPConfiguracoesNFSe.Geral.Provedor of
      proIssDSF: FNameSpaceDad := 'xmlns:' + StringReplace(FPrefixo3, ':', '', []) + '="' + FNameSpace + '" ';
      proInfisc: FNameSpaceDad := 'xmlns:' + StringReplace(FPrefixo3, ':', '', []) + '="' + FNameSpace + '" ';
      else begin
        if (FSeparador = '/') then
        begin
          if FPrefixo3 <> '' then
            FNameSpaceDad := 'xmlns:' + StringReplace(FPrefixo3, ':', '', []) + '="' + FNameSpace + FSeparador + FxsdServico + '"'
          else
            FNameSpaceDad := 'xmlns="' + FNameSpace + FSeparador + FxsdServico + '"';
        end
        else begin
          if FPrefixo3 <> '' then
            FNameSpaceDad := 'xmlns:' + StringReplace(FPrefixo3, ':', '', []) + '="' + FNameSpace + '"'
          else
            FNameSpaceDad := 'xmlns="' + FNameSpace + '"';
        end;
      end;
    end;
  end
  else
    FNameSpaceDad := '';

  if (DefTipos = '') and (NameSpaceDad <> '') then
    FNameSpaceDad := FNameSpaceDad + '>';

  if FDefTipos <> '' then
  begin
    if FPrefixo4 <> '' then
      FNameSpaceDad := FNameSpaceDad + ' xmlns:' +
                       StringReplace(FPrefixo4, ':', '', []) + '="' + FNameSpace + FSeparador + FDefTipos + '">'
    else
      FNameSpaceDad := FNameSpaceDad + ' xmlns="' + FNameSpace + FSeparador + FDefTipos + '">';
  end;

  if FNameSpaceDad = '' then
    FNameSpaceDad := '>'
  else
    FNameSpaceDad := ' ' + FNameSpaceDad;

  Texto := FPConfiguracoesNFSe.Geral.ConfigGeral.DadosSenha;
  // %Usuario% : Representa o nome do usuário ou CNPJ
  // %Senha%   : Representa a senha do usuário
  Texto := StringReplace(Texto, '%Usuario%', FPConfiguracoesNFSe.Geral.UserWeb, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%Senha%', FPConfiguracoesNFSe.Geral.SenhaWeb, [rfReplaceAll]);

  FDadosSenha := Texto;
end;

{ TNFSeGerarLoteRPS }

constructor TNFSeGerarLoteRPS.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNfseRecepcaoLote;
  FPArqEnv := 'lot-rps';
  FPArqResp := ''; // O lote é apenas gerado não retorno de envio.
end;

destructor TNFSeGerarLoteRPS.Destroy;
begin
  inherited Destroy;
end;

procedure TNFSeGerarLoteRPS.EnviarDados;
begin
  // O Gerar Lote RPS não ocorre o envio para o Web Service
end;

procedure TNFSeGerarLoteRPS.DefinirURL;
begin
  FPLayout := LayNfseRecepcaoLote;
  inherited DefinirURL;
end;

procedure TNFSeGerarLoteRPS.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeGerarLoteRPS';
  FPSoapAction := FPServico;
end;

procedure TNFSeGerarLoteRPS.DefinirDadosMsg;
var
  I: Integer;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar;

  InicializarDadosMsg;

  if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
  begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        profintelISS, proSaatri, proSisPMJP, proISSDigital, proISSe, proSystemPro,
        pro4R, proFiorilli, proProdata, proVitoria, proPVH, proAgili, proCoplan,
        proVirtual, proFreire, proLink3, proGovDigital, proMitra,
        proGoiania: FvNotas := FvNotas +
                               '<' + FPrefixo4 + 'Rps>' +
                                '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                  RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                                    '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                                '</Signature>'+
                               '</' + FPrefixo4 + 'Rps>';

        proDigifred: FvNotas := FvNotas +
                                '<' + FPrefixo4 + 'Rps ' +
                                   RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                                     '<' + FPrefixo4 + 'Rps', '</Signature>') +
                                 '</Signature>'+
                                '</' + Prefixo4 + 'Rps>';

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                                   '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + FPrefixo4 + 'Rps>';

        else FvNotas := FvNotas +
                        '<' + FPrefixo4 + 'Rps>' +
                         '<' + FPrefixo4 + 'InfRps' +
                           RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                         '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                        '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end
  else begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        profintelISS, proSaatri, proSisPMJP, proGoiania, proISSDigital, proISSe,
        proSystemPro, pro4R, proFiorilli, proProdata, proVitoria, proPVH, proAgili,
        proCoplan, proVirtual, proFreire, proLink3, proMitra,
        proGovDigital: FvNotas := FvNotas +
                                  '<' + FPrefixo4 + 'Rps>' +
                                   '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                     RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                                   '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                                  '</' + FPrefixo4 + 'Rps>';

        proIssDSF,
        proInfisc: FvNotas :=  FvNotas + TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal;

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal,
                                 '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>') +
                               '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>'+
                              '</' + FPrefixo4 + 'Rps>';

        else FvNotas := FvNotas + '<' + FPrefixo4 + 'Rps>' +
                                 '<' + FPrefixo4 + 'InfRps' +
                                   RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                                '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end;

  FTagI := '<' + FPrefixo3 + 'EnviarLoteRpsEnvio' + FNameSpaceDad;
  FTagF := '</' + FPrefixo3 + 'EnviarLoteRpsEnvio>';

  FPDadosMsg := TNFSeG.Gera_DadosMsgEnviarLote(FPrefixo3, FPrefixo4,
                                               FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador,
                                               FNameSpace,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoDados,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoXML,
                                               TNFSeGerarLoteRps(Self).NumeroLote,
                                               OnlyNumber(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                               TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                               IntToStr(TNFSeGerarLoteRps(Self).FNotasFiscais.Count),
                                               FvNotas,
                                               FTagI, FTagF, FPConfiguracoesNFSe.Geral.Provedor);

  if FPDadosMsg <> '' then
  begin
    FPDadosMsg := TNFSeGerarLoteRPS(Self).FNotasFiscais.AssinarLote(FPDadosMsg,
                                  'EnviarLoteRpsEnvio', 'LoteRps',
                                  FPConfiguracoesNFSe.Geral.ConfigAssinar.Lote);

    if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
      TNFSeGerarLoteRPS(Self).FNotasFiscais.ValidarLote(FPDadosMsg,
                         FPConfiguracoes.Arquivos.PathSchemas +
                         FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar);
  end
  else
    GerarException(ACBrStr('A funcionalidade [Gerar Lote] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar;

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
end;

function TNFSeGerarLoteRPS.TratarResposta: Boolean;
begin
  TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NomeArq :=
    FPConfiguracoes.Arquivos.PathSalvar +
    GerarPrefixoArquivo + '-' + FPArqEnv + '.xml';
  Result := True;  
end;

procedure TNFSeGerarLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeGerarLoteRPS.GerarMsgLog: String;
begin
  Result := '';
end;

function TNFSeGerarLoteRPS.GerarPrefixoArquivo: String;
begin
  Result := NumeroLote;
end;

{ TNFSeEnviarLoteRPS }

constructor TNFSeEnviarLoteRPS.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNfseRecepcaoLote;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

  FNFSeRetorno := nil;
end;

destructor TNFSeEnviarLoteRPS.Destroy;
begin
  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

function TNFSeEnviarLoteRPS.GetLote: String;
begin
  Result := Trim(FNumeroLote);
end;

function TNFSeEnviarLoteRPS.GetProtocolo: String;
begin
  Result := Trim(FProtocolo);
end;

procedure TNFSeEnviarLoteRPS.DefinirURL;
begin
  FPLayout := LayNfseRecepcaoLote;
  inherited DefinirURL;
end;

procedure TNFSeEnviarLoteRPS.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeEnviarLoteRPS';
  FPSoapAction := FPServico;
end;

procedure TNFSeEnviarLoteRPS.DefinirDadosMsg;
var
  I: Integer;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar;

  InicializarDadosMsg;

  if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
  begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        pro4R, proAgili, profintelISS, proFiorilli, proGoiania, proISSDigital,
        proISSe, proSystemPro, proCoplan, proProdata, proVitoria, proPVH,
        proSaatri, proSisPMJP, proFreire, proLink3, proGovDigital, proMitra,
        proVirtual: FvNotas := FvNotas +
                               '<' + FPrefixo4 + 'Rps>' +
                                '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                  RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                                    '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                                '</Signature>'+
                               '</' + FPrefixo4 + 'Rps>';

        proDigifred: FvNotas := FvNotas +
                                '<' + FPrefixo4 + 'Rps ' +
                                   RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                                     '<' + FPrefixo4 + 'Rps', '</Signature>') +
                                 '</Signature>'+
                                '</' + Prefixo4 + 'Rps>';

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                                   '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + FPrefixo4 + 'Rps>';

        else FvNotas := FvNotas +
                        '<' + FPrefixo4 + 'Rps>' +
                         '<' + FPrefixo4 + 'InfRps' +
                           RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                         '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                        '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end
  else begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        profintelISS, proSaatri, proSisPMJP, proCoplan, proGoiania, proISSDigital,
        proISSe, proSystemPro, pro4R, proFiorilli, proProdata, proVitoria, proPVH,
        proAgili, proVirtual, proFreire, proLink3,
        proGovDigital: FvNotas := FvNotas +
                                  '<' + FPrefixo4 + 'Rps>' +
                                   '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                     RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                                   '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                                  '</' + FPrefixo4 + 'Rps>';

        proIssDSF,
        proInfisc,
        proEquiplano,
        proEL: FvNotas :=  FvNotas + TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal;

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal,
                                 '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>') +
                               '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>'+
                              '</' + FPrefixo4 + 'Rps>';

        proNFSEBrasil: begin
                         FvNotas := StringReplace(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal, '</Rps>', '', [rfReplaceAll]) + '</Rps>';
                         FvNotas := StringReplace(FvNotas, '<Rps>', '', [rfReplaceAll]);
                         FvNotas := '<Rps>' + StringReplace(FvNotas, '<InfRps>', '', [rfReplaceAll]);
                       end;

        else FvNotas := FvNotas + '<' + FPrefixo4 + 'Rps>' +
                                 '<' + FPrefixo4 + 'InfRps' +
                                   RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                                '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end;

  FTagI := '<' + FPrefixo3 + 'EnviarLoteRpsEnvio' + FNameSpaceDad;
  FTagF := '</' + FPrefixo3 + 'EnviarLoteRpsEnvio>';

  FPDadosMsg := TNFSeG.Gera_DadosMsgEnviarLote(FPrefixo3, FPrefixo4,
                                               FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador,
                                               FNameSpace,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoDados,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoXML,
                                               TNFSeEnviarLoteRps(Self).NumeroLote,
                                               OnlyNumber(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                               TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                               IntToStr(TNFSeEnviarLoteRps(Self).FNotasFiscais.Count),
                                               FvNotas,
                                               FTagI, FTagF, FPConfiguracoesNFSe.Geral.Provedor);

  if FPDadosMsg <> '' then
  begin
    FPDadosMsg := TNFSeEnviarLoteRPS(Self).FNotasFiscais.AssinarLote(FPDadosMsg,
                                  'EnviarLoteRpsEnvio', 'LoteRps',
                                  FPConfiguracoesNFSe.Geral.ConfigAssinar.Lote);

    if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
      TNFSeEnviarLoteRPS(Self).FNotasFiscais.ValidarLote(FPDadosMsg,
                         FPConfiguracoes.Arquivos.PathSchemas +
                         FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar);
  end
  else
    GerarException(ACBrStr('A funcionalidade [Enviar Lote] não foi disponibilizada pelo provedor: ' +
      FPConfiguracoesNFSe.Geral.xProvedor));

  FDadosEnvelope := FPConfiguracoesNFSe.Geral.ConfigEnvelope.Recepcionar;

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
end;

function TNFSeEnviarLoteRPS.TratarResposta: Boolean;
var
  I: Integer;
  chNFSe, NomeArquivo: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'Return');
  if FPRetWS = '' then
    FPRetWS := SeparaDados(FPRetornoWS, 'EnviarLoteRpsResposta');
  if FPRetWS = '' then
    FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body')
  else
    FPRetWS := FPRetWS + '</EnviarLoteRpsResposta>';

  FNFSeRetorno := TretEnvLote.Create;

  FNFSeRetorno.Leitor.Arquivo := FPRetWS;
  FNFSeRetorno.LerXml;

  FDataRecebimento := NFSeRetorno.InfRec.DataRecebimento;
  FProtocolo       := NFSeRetorno.InfRec.Protocolo;
  FNumeroLote      := NFSeRetorno.InfRec.NumeroLote;

  // Lista de Mensagem de Retorno
  FPMsg := '';
  if NFSeRetorno.InfRec.MsgRetorno.Count > 0 then
  begin
    FaMsg:='';
    for i := 0 to NFSeRetorno.InfRec.MsgRetorno.Count - 1 do
    begin
      FPMsg := FPMsg + NFSeRetorno.infRec.MsgRetorno.Items[i].Mensagem + IfThen(FPMsg = '', '', ' / ');

      FaMsg := FaMsg + 'Código Erro : ' + NFSeRetorno.InfRec.MsgRetorno.Items[i].Codigo + LineBreak +
                       'Mensagem... : ' + NFSeRetorno.infRec.MsgRetorno.Items[i].Mensagem + LineBreak +
                       'Correção... : ' + NFSeRetorno.InfRec.MsgRetorno.Items[i].Correcao + LineBreak +
                       'Provedor... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
    end;
  end
  else begin
    for i := 0 to FNotasFiscais.Count -1 do
    begin
      FNotasFiscais.Items[i].NFSe.Protocolo     := FProtocolo;
      FNotasFiscais.Items[i].NFSe.dhRecebimento := FDataRecebimento;
    end;
    FaMsg := 'Numero do Lote : ' + NFSeRetorno.InfRec.NumeroLote + LineBreak +
             'Recebimento... : ' + IfThen(FDataRecebimento = 0, '', DateTimeToStr(FDataRecebimento)) + LineBreak +
             'Protocolo..... : ' + FProtocolo + LineBreak +
             'Provedor...... : ' + FPConfiguracoesNFSe.Geral.xProvedor + LineBreak;
  end;

  Result := (NFSeRetorno.InfRec.Protocolo <> '');
end;

procedure TNFSeEnviarLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeEnviarLoteRPS.GerarMsgLog: String;
begin
  if Assigned(FNFSeRetorno) then
    Result := ACBrStr(FaMsg)
  else
    Result := '';
end;

function TNFSeEnviarLoteRPS.GerarPrefixoArquivo: String;
begin
  Result := NumeroLote;
end;

{ TNFSeEnviarSincrono }

constructor TNFSeEnviarSincrono.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNfseRecepcaoLoteSincrono;
  FPArqEnv := 'env-lotS';
  FPArqResp := 'lista-nfse';

  FNFSeRetorno := nil;
end;

procedure TNFSeEnviarSincrono.DefinirDadosMsg;
var
  i: Integer;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviarSincrono;

  InicializarDadosMsg;

  if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
  begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        profintelISS, proSaatri, proSisPMJP, proISSDigital, proISSe, proSystemPro,
        pro4R, proFiorilli, proProdata, proVitoria, proPVH, proAgili, proCoplan,
        proVirtual, proFreire, proLink3, proGovDigital, proMitra,
        proGoiania: FvNotas := FvNotas +
                               '<' + FPrefixo4 + 'Rps>' +
                                '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                  RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XMLAssinado,
                                    '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                                '</Signature>'+
                               '</' + FPrefixo4 + 'Rps>';

        proDigifred: FvNotas := FvNotas +
                                '<' + FPrefixo4 + 'Rps ' +
                                   RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XMLAssinado,
                                     '<' + FPrefixo4 + 'Rps', '</Signature>') +
                                 '</Signature>'+
                                '</' + Prefixo4 + 'Rps>';

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XMLAssinado,
                                   '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + FPrefixo4 + 'Rps>';

        else FvNotas := FvNotas +
                        '<' + FPrefixo4 + 'Rps>' +
                         '<' + FPrefixo4 + 'InfRps' +
                           RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XMLAssinado,
                         '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                        '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end
  else begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        profintelISS, proSaatri, proSisPMJP, proGoiania, proISSDigital, proISSe,
        proSystemPro, pro4R, proFiorilli, proProdata, proVitoria, proPVH, proAgili,
        proCoplan, proVirtual, proFreire, proLink3, proMitra,
        proGovDigital: FvNotas := FvNotas +
                                  '<' + FPrefixo4 + 'Rps>' +
                                   '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                     RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                                   '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                                  '</' + FPrefixo4 + 'Rps>';

        proIssDSF,
        proInfisc: FvNotas :=  FvNotas + TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XMLOriginal;

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XMLOriginal,
                                 '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>') +
                               '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>'+
                              '</' + FPrefixo4 + 'Rps>';

        else FvNotas := FvNotas + '<' + FPrefixo4 + 'Rps>' +
                                 '<' + FPrefixo4 + 'InfRps' +
                                   RetornarConteudoEntre(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                                '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end;

  FTagI := '<' + FPrefixo3 + 'EnviarLoteRpsSincronoEnvio' + FNameSpaceDad;
  FTagF := '</' + FPrefixo3 + 'EnviarLoteRpsSincronoEnvio>';

  FPDadosMsg := TNFSeG.Gera_DadosMsgEnviarSincrono(FPrefixo3, FPrefixo4,
                                                   FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador,
                                                   FNameSpace,
                                                   FPConfiguracoesNFSe.Geral.ConfigXML.VersaoDados,
                                                   FPConfiguracoesNFSe.Geral.ConfigXML.VersaoXML,
                                                   TNFSeEnviarSincrono(Self).NumeroLote,
                                                   OnlyNumber(TNFSeEnviarSincrono(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                                   TNFSeEnviarSincrono(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                                   IntToStr(TNFSeEnviarSincrono(Self).FNotasFiscais.Count),
                                                   FvNotas,
                                                   FTagI, FTagF,
                                                   FPConfiguracoesNFSe.Geral.Provedor);

  if FPDadosMsg <> '' then
  begin
    FPDadosMsg := TNFSeEnviarSincrono(Self).FNotasFiscais.AssinarLote(FPDadosMsg,
                                  'EnviarLoteRpsSincronoEnvio', 'LoteRps',
                                  FPConfiguracoesNFSe.Geral.ConfigAssinar.Lote);

    if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
      TNFSeEnviarSincrono(Self).FNotasFiscais.ValidarLote(FPDadosMsg,
                 FPConfiguracoes.Arquivos.PathSchemas +
                 FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviarSincrono);
   end
   else
     GerarException(ACBrStr('A funcionalidade [Enviar Sincrono] não foi disponibilizada pelo provedor: ' +
      FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeEnviarSincrono.TratarResposta: Boolean;
begin
{a}
end;

procedure TNFSeEnviarSincrono.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeEnviarSincrono';
  FPSoapAction := FPServico;
end;

procedure TNFSeEnviarSincrono.DefinirURL;
begin
  FPLayout := LayNfseRecepcaoLoteSincrono;
  inherited DefinirURL;
end;

destructor TNFSeEnviarSincrono.Destroy;
begin
  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeEnviarSincrono.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeEnviarSincrono.GerarMsgLog: String;
begin
  if Assigned(FNFSeRetorno) then
    Result := ACBrStr(FaMsg)
  else
    Result := '';
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

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNfseGerar;
  FPArqEnv := 'ger-nfse';
  FPArqResp := 'lista-nfse';

  FNFSeRetorno := nil;
end;

procedure TNFSeGerarNFSe.DefinirDadosMsg;
var
  i: Integer;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoGerar;

  InicializarDadosMsg;

  if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS or FPConfiguracoesNFSe.Geral.ConfigAssinar.Gerar then
  begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        profintelISS, proSaatri, proSisPMJP, proISSDigital, proISSe, pro4R,
        proFiorilli, proProdata, proVitoria, proPVH, proAgili, proCoplan,
        proVirtual, proLink3, proGovDigital, proMitra,
        proGoiania: FvNotas := FvNotas +
                               '<' + FPrefixo4 + 'Rps>' +
                                '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                  RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                                    '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                                '</Signature>'+
                               '</' + FPrefixo4 + 'Rps>';

        proFreire: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico Id ="'+ TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].NFSe.InfID.ID +'"' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                                   '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + FPrefixo4 + 'Rps>';

        proDigifred: FvNotas := FvNotas +
                                '<' + FPrefixo4 + 'Rps ' +
                                   RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                                     '<' + FPrefixo4 + 'Rps', '</Signature>') +
                                 '</Signature>'+
                                '</' + Prefixo4 + 'Rps>';

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                                   '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + FPrefixo4 + 'Rps>';

        proSystemPro: FvNotas := FvNotas + TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLAssinado;

        else FvNotas := FvNotas +
                        '<' + FPrefixo4 + 'Rps>' +
                         '<' + FPrefixo4 + 'InfRps' +
                           RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                         '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                        '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end
  else begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        profintelISS, proSaatri, proSisPMJP, proGoiania, proISSDigital, proISSe,
        proSystemPro, pro4R, proFiorilli, proProdata, proVitoria, proPVH,
        proAgili, proCoplan, proLink3, proGovDigital,
        proVirtual: FvNotas := FvNotas +
                               '<' + FPrefixo4 + 'Rps>' +
                                 '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                   RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLOriginal,
                                   '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                                 '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                                '</' + FPrefixo4 + 'Rps>';

        proFreire : FvNotas := FvNotas +
                               '<' + FPrefixo4 + 'Rps>' +
                                '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico Id="' + TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].NFSe.InfID.ID + '" '+
                                  RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLOriginal,
                                    '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                                '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                               '</' + FPrefixo4 + 'Rps>';

        proEgoverneISS: FvNotas := FvNotas +
                                   '<' + FPrefixo4 + 'NotaFiscal>' +
                                     RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'NotaFiscal>', '</' + FPrefixo4 + 'NotaFiscal>') +
                                   '</' + FPrefixo4 + 'NotaFiscal>';

        proIssDSF,
        proInfisc: FvNotas :=  FvNotas + TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLOriginal;

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLOriginal,
                                 '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>') +
                               '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>'+
                              '</' + FPrefixo4 + 'Rps>';

        else FvNotas := FvNotas + '<' + FPrefixo4 + 'Rps>' +
                                 '<' + FPrefixo4 + 'InfRps' +
                                   RetornarConteudoEntre(TNFSeGerarNFSe(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                                '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end;

  FTagI := '<' + FPrefixo3 + 'GerarNfseEnvio' + FNameSpaceDad;
  FTagF := '</' + FPrefixo3 + 'GerarNfseEnvio>';

  FPDadosMsg := TNFSeG.Gera_DadosMsgGerarNFSe(FPrefixo3, FPrefixo4,
                                              FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador,
                                              FNameSpace,
                                              FPConfiguracoesNFSe.Geral.ConfigXML.VersaoDados,
                                              FPConfiguracoesNFSe.Geral.ConfigXML.VersaoXML,
                                              IntToStr(FNumeroRps),
                                              OnlyNumber(TNFSeGerarNFSe(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                              TNFSeGerarNFSe(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                              IntToStr(TNFSeGerarNFSe(Self).FNotasFiscais.Count),
                                              FvNotas,
                                              FTagI, FTagF,
                                              FPConfiguracoesNFSe.Geral.Provedor);

  if FPDadosMsg <> '' then
  begin
    FPDadosMsg := TNFSeGerarNFSe(Self).FNotasFiscais.AssinarLote(FPDadosMsg,
                                  'GerarNfseEnvio', 'Rps',
                                  FPConfiguracoesNFSe.Geral.ConfigAssinar.Gerar);

    if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
      TNFSeGerarNFSe(Self).FNotasFiscais.ValidarLote(FPDadosMsg,
                          FPConfiguracoes.Arquivos.PathSchemas +
                          FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoGerar);
   end
   else
     GerarException(ACBrStr('A funcionalidade [Gerar NFSe] não foi disponibilizada pelo provedor: ' +
      FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeGerarNFSe.TratarResposta: Boolean;
begin
{a}
end;

procedure TNFSeGerarNFSe.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeGerarNFSe';
  FPSoapAction := FPServico;
end;

procedure TNFSeGerarNFSe.DefinirURL;
begin
  FPLayout := LayNfseGerar;
  inherited DefinirURL;
end;

destructor TNFSeGerarNFSe.Destroy;
begin
  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeGerarNFSe.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeGerarNFSe.GerarMsgLog: String;
begin
  if Assigned(FNFSeRetorno) then
    Result := ACBrStr(FaMsg)
  else
    Result := '';
end;

function TNFSeGerarNFSe.GerarPrefixoArquivo: String;
begin
  Result := inttoStr(NumeroRPS);
end;

{ TNFSeConsultarSituacaoLoteRPS }

constructor TNFSeConsultarSituacaoLoteRPS.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;

  FPStatus := stNFSeConsulta;
  FPLayout := LayNfseConsultaSitLoteRps;
  FPArqEnv := 'con-sit';
  FPArqResp := 'sit';

  FNFSeRetorno := nil;
end;

procedure TNFSeConsultarSituacaoLoteRPS.DefinirDadosMsg;
var
  URISig, URIRef: String;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoConSit;

  InicializarDadosMsg;

(*
 URISig := '';
 URIRef := '';

 URISig := FProvedorClass.GetURI(URISig);
 URIRef := URISig;

  FTagI := '<' + FPrefixo3 + 'ConsultarSituacaoLoteRpsEnvio' + FNameSpaceDad;
  FTagF := '</' + FPrefixo3 + 'ConsultarSituacaoLoteRpsEnvio>';


 if FProvedorClass.GetAssinarXML(acConsSit)
  then begin
   case FProvedor of
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgConsSitLoteEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).FCNPJ),
                                                               TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).NumeroLote,
                                                               '', '');
    proInfisc: FPDadosMsg := TNFSeG.Gera_DadosMsgConsSitLoteInfisc(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).FCNPJ),
                                                               TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).NumeroLote,
                                                               '', '');
    else FPDadosMsg := TNFSeG.Gera_DadosMsgConsSitLote(FPrefixo3, FPrefixo4,
                                                      NameSpaceDad, FVersaoXML,
                                                      TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                      OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).Cnpj),
                                                      TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                      '', '', FProvedor);
   end;

   if FPDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     NotaUtil.InitXmlSec;
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgConsSitLoteEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).FCNPJ),
                                                               TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                               TNFSeConsultarSituacaoLoteRPS(Self).NumeroLote,
                                                               FTagI, FTagF);

    proEL: FPDadosMsg := TNFSeG.Gera_DadosMsgConsSitLoteEL(FConfiguracoes.WebServices.CodigoMunicipio,
                                                          OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).FCNPJ),
                                                          TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                          TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                          TNFSeConsultarSituacaoLoteRPS(Self).NumeroLote,
                                                          FTagI, FTagF);

    else FPDadosMsg := TNFSeG.Gera_DadosMsgConsSitLote(FPrefixo3, FPrefixo4,
                                                      NameSpaceDad, FVersaoXML,
                                                      TNFSeConsultarSituacaoLoteRPS(Self).Protocolo,
                                                      OnlyNumber(TNFSeConsultarSituacaoLoteRPS(Self).Cnpj),
                                                      TNFSeConsultarSituacaoLoteRPS(Self).InscricaoMunicipal,
                                                      FTagI, FTagF, FProvedor);
   end;
  end;

*)
  if FPDadosMsg = '' then
    GerarException(ACBrStr('A funcionalidade [Consultar Situação do Lote] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeConsultarSituacaoLoteRPS.TratarResposta: Boolean;
begin
{a}
end;

procedure TNFSeConsultarSituacaoLoteRPS.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeConsSitLoteRPS';
  FPSoapAction := FPServico;
end;

procedure TNFSeConsultarSituacaoLoteRPS.DefinirURL;
begin
  FPLayout := LayNfseConsultaSitLoteRps;
  inherited DefinirURL;
end;

destructor TNFSeConsultarSituacaoLoteRPS.Destroy;
begin
  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeConsultarSituacaoLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeConsultarSituacaoLoteRPS.GerarMsgLog: String;
begin
  if Assigned(FNFSeRetorno) then
    Result := ACBrStr(FaMsg)
  else
    Result := '';
end;

function TNFSeConsultarSituacaoLoteRPS.GerarPrefixoArquivo: String;
begin
  Result := Protocolo;
end;

{ TNFSeConsultarLoteRPS }

constructor TNFSeConsultarLoteRPS.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;

  FPStatus := stNFSeConsulta;
  FPLayout := LayNfseConsultaLote;
  FPArqEnv := 'con-lot';
  FPArqResp := 'lista-nfse';

  FNFSeRetorno := nil;
end;

procedure TNFSeConsultarLoteRPS.DefinirDadosMsg;
var
  URISig, URIRef: String;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoConLot;

  InicializarDadosMsg;

(*
 URISig := '';
 URIRef := '';

 URISig := FProvedorClass.GetURI(URISig);
 URIRef := URISig;

  FTagI := '<' + FPrefixo3 + 'ConsultarLoteRpsEnvio' + FNameSpaceDad;
  FTagF := '</' + FPrefixo3 + 'ConsultarLoteRpsEnvio>';

 if FProvedor = proEL then
 begin
   if (TNFSeConsultarLoteRPS(Self).FCNPJ = '') then
     TNFSeConsultarLoteRPS(Self).FCNPJ := OnlyNumber(TNFSeConsultarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj);
 end
 else begin
   if (TNFSeConsultarLoteRPS(Self).FCNPJ = '') then
     TNFSeConsultarLoteRPS(Self).FCNPJ := OnlyNumber(TNFSeConsultarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj);
 end;
 if (TNFSeConsultarLoteRPS(Self).FIM = '') then
    TNFSeConsultarLoteRPS(Self).FIM := TNFSeConsultarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
 if (TNFSeConsultarLoteRPS(Self).FRazaoSocial = '') and (FProvedor = proTecnos) then
    TNFSeConsultarLoteRPS(Self).FRazaoSocial := TNFSeConsultarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial;

 if FProvedorClass.GetAssinarXML(acConsLote)
  then begin
   case FProvedor of
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgConsLoteEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                            OnlyNumber(TNFSeConsultarLoteRPS(Self).FCNPJ),
                                                            TNFSeConsultarLoteRPS(Self).IM,
                                                            TNFSeConsultarLoteRPS(Self).Protocolo,
                                                            TNFSeConsultarLoteRPS(Self).NumeroLote,
                                                            '', '');
    else FPDadosMsg := TNFSeG.Gera_DadosMsgConsLote(FPrefixo3, FPrefixo4,
                                                   NameSpaceDad, FVersaoXML,
                                                   TNFSeConsultarLoteRPS(Self).Protocolo,
                                                   TNFSeConsultarLoteRPS(Self).FCNPJ,
                                                   TNFSeConsultarLoteRPS(Self).FIM,
                                                   TNFSeConsultarLoteRPS(Self).FSenha,
                                                   TNFSeConsultarLoteRPS(Self).FFraseSecreta,
                                                   '', '', FProvedor,
                                                   TNFSeConsultarLoteRPS(Self).FRazaoSocial);
   end;
   if FPDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     NotaUtil.InitXmlSec;
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FPDadosMsg := TNFSeG.Gera_DadosMsgConsLoteDSF(FPrefixo3, FPrefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      CodCidadeToCodSiafi(FConfiguracoes.WebServices.CodigoMunicipio),
                                                      TNFSeConsultarLoteRPS(Self).Cnpj,
                                                      TNFSeConsultarLoteRPS(Self).Protocolo,
                                                      FTagI, FTagF);
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgConsLoteEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                            OnlyNumber(TNFSeConsultarLoteRPS(Self).FCNPJ),
                                                            TNFSeConsultarLoteRPS(Self).IM,
                                                            TNFSeConsultarLoteRPS(Self).Protocolo,
                                                            TNFSeConsultarLoteRPS(Self).NumeroLote,
                                                            FTagI, FTagF);
    proEL: FPDadosMsg := TNFSeG.Gera_DadosMsgConsLoteEL(FConfiguracoes.WebServices.CodigoMunicipio,
                                                       OnlyNumber(TNFSeConsultarLoteRPS(Self).FCNPJ),
                                                       TNFSeConsultarLoteRPS(Self).IM,
                                                       TNFSeConsultarLoteRPS(Self).Protocolo,
                                                       TNFSeConsultarLoteRPS(Self).NumeroLote,
                                                       FTagI, FTagF);
    else FPDadosMsg := TNFSeG.Gera_DadosMsgConsLote(FPrefixo3, FPrefixo4,
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

*)
  if FPDadosMsg = '' then
    GerarException(ACBrStr('A funcionalidade [Consultar Lote] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeConsultarLoteRPS.TratarResposta: Boolean;
begin
{a}
end;

procedure TNFSeConsultarLoteRPS.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeConsLote';
  FPSoapAction := FPServico;
end;

procedure TNFSeConsultarLoteRPS.DefinirURL;
begin
  FPLayout := LayNfseConsultaLote;
  inherited DefinirURL;
end;

destructor TNFSeConsultarLoteRPS.Destroy;
begin
  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeConsultarLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeConsultarLoteRPS.GerarMsgLog: String;
begin
  if Assigned(FNFSeRetorno) then
    Result := ACBrStr(FaMsg)
  else
    Result := '';
end;

function TNFSeConsultarLoteRPS.GerarPrefixoArquivo: String;
begin
  Result := Protocolo;
end;

{ TNFSeConsultarNfseRPS }

constructor TNFSeConsultarNfseRPS.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;

  FPStatus := stNFSeConsulta;
  FPLayout := LayNfseConsultaNfseRps;
  FPArqEnv := 'con-nfse-rps';
  FPArqResp := 'comp-nfse';

  FNFSeRetorno := nil;
end;

procedure TNFSeConsultarNfseRPS.DefinirDadosMsg;
var
  i: Integer;
  URISig, URIRef: String;
  Gerador: TGerador;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoConRps;

  InicializarDadosMsg;

(*
 URISig := '';
 URIRef := '';

 URISig := FProvedorClass.GetURI(URISig);
 URIRef := URISig;

  FTagI := '<' + FPrefixo3 + 'ConsultarNfseRpsEnvio' + FNameSpaceDad;
  FTagF := '</' + FPrefixo3 + 'ConsultarNfseRpsEnvio>';

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

   FvNotas := Gerador.ArquivoFormatoXML;
   Gerador.Free;
 end;

 if FProvedorClass.GetAssinarXML(acConsNFSeRps)
  then begin
   case FProvedor of
    proIssDSF: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSDSF(FPrefixo3, FPrefixo4,
                                                         NameSpaceDad, VersaoXML,
                                                         CodCidadeToCodSiafi(FConfiguracoes.WebServices.CodigoMunicipio),
                                                         OnlyNumber(TNFSeConsultarNfseRPS(Self).Cnpj),
                                                         LowerCase(booltostr(TNFSeConsultarNfseRPS(Self).FNotasFiscais.Transacao, True)),
                                                         TNFSeConsultarNfseRPS(Self).FNotasFiscais.NumeroLote,
                                                         FvNotas,
                                                         '', '');
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               TNFSeConsultarNfseRPS(Self).Numero,
                                                               OnlyNumber(TNFSeConsultarNfseRPS(Self).FCnpj),
                                                               TNFSeConsultarNfseRPS(Self).InscricaoMunicipal,
                                                               '', '');
    else FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPS(FPrefixo3, FPrefixo4,
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
   if FPDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     NotaUtil.InitXmlSec;
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSDSF(FPrefixo3, FPrefixo4,
                                                         NameSpaceDad, VersaoXML,
                                                         CodCidadeToCodSiafi( strtointDef(TNFSeConsultarNfseRPS(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                         TNFSeConsultarNfseRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj,
                                                         LowerCase(booltostr(TNFSeConsultarNfseRPS(Self).FNotasFiscais.Transacao, True)),
                                                         TNFSeConsultarNfseRPS(Self).FNotasFiscais.NumeroLote,
                                                         FvNotas,
                                                         FTagI, FTagF);
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSEquiplano(FConfiguracoes.WebServices.CodigoMunicipio,
                                                               TNFSeConsultarNfseRPS(Self).Numero,
                                                               OnlyNumber(TNFSeConsultarNfseRPS(Self).FCnpj),
                                                               TNFSeConsultarNfseRPS(Self).InscricaoMunicipal,
                                                               FTagI, FTagF);
    proEL: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPSEL(FConfiguracoes.WebServices.CodigoMunicipio,
                                                          TNFSeConsultarNfseRPS(Self).Numero,
                                                          OnlyNumber(TNFSeConsultarNfseRPS(Self).FCnpj),
                                                          TNFSeConsultarNfseRPS(Self).InscricaoMunicipal,
                                                          FTagI, FTagF);
    else FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeRPS(FPrefixo3, FPrefixo4,
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

*)
  if FPDadosMsg = '' then
    GerarException(ACBrStr('A funcionalidade [Consultar NFSe por RPS] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeConsultarNfseRPS.TratarResposta: Boolean;
begin
 {a}
end;

procedure TNFSeConsultarNfseRPS.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeConsNfseRPS';
  FPSoapAction := FPServico;
end;

procedure TNFSeConsultarNfseRPS.DefinirURL;
begin
  FPLayout := LayNfseConsultaNfseRps;
  inherited DefinirURL;
end;

destructor TNFSeConsultarNfseRPS.Destroy;
begin
  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeConsultarNfseRPS.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeConsultarNfseRPS.GerarMsgLog: String;
begin
  if Assigned(FNFSeRetorno) then
    Result := ACBrStr(FaMsg)
  else
    Result := '';
end;

function TNFSeConsultarNfseRPS.GerarPrefixoArquivo: String;
begin
  Result := Numero + Serie;
end;

{ TNFSeConsultarNfse }

constructor TNFSeConsultarNfse.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;

  FPStatus := stNFSeConsulta;
  FPLayout := LayNfseConsultaNfse;
  FPArqEnv := 'con-nfse';
  FPArqResp := 'lista-nfse';

  FNFSeRetorno := nil;
end;

procedure TNFSeConsultarNfse.DefinirDadosMsg;
var
  URISig, URIRef: String;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoConNfse;

  InicializarDadosMsg;

(*
 URISig := '';
 URIRef := '';

 URISig := FProvedorClass.GetURI(URISig);
 URIRef := URISig;

  FTagI := '<' + FPrefixo3 + 'ConsultarNfseEnvio' + FNameSpaceDad;
  FTagF := '</' + FPrefixo3 + 'ConsultarNfseEnvio>';

 if FProvedorClass.GetAssinarXML(acConsNFSe)
  then begin
   case FProvedor of
    proIssDSF: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeDSF(FPrefixo3, FPrefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      CodCidadeToCodSiafi(strtointDef(TNFSeConsultarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                      TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                      TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                      TNFSeConsultarNfse(Self).DataInicial,
                                                      TNFSeConsultarNfse(Self).DataFinal,
                                                      '', '');
    proInfisc: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeInfisc(FPrefixo3, FPrefixo4,
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
    else FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSe(FPrefixo3, FPrefixo4,
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
   if FPDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     NotaUtil.InitXmlSec;
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeDSF(FPrefixo3, FPrefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      CodCidadeToCodSiafi(strtointDef(TNFSeConsultarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                      TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                      TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                      TNFSeConsultarNfse(Self).DataInicial,
                                                      TNFSeConsultarNfse(Self).DataFinal,
                                                      FTagI, FTagF);
    proInfisc: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeInfisc(FPrefixo3, FPrefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      CodCidadeToCodSiafi(strtointDef(TNFSeConsultarNfse(Self).FNotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio, 0)),
                                                      OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                      TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                      TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                      TNFSeConsultarNfse(Self).FSerie,
                                                      TNFSeConsultarNfse(Self).DataInicial,
                                                      TNFSeConsultarNfse(Self).DataFinal,
                                                      FTagI, FTagF);
    proEL: FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSeEL(FPrefixo3, FPrefixo4,
                                                      NameSpaceDad, VersaoXML,
                                                      TNFSeConsultarNfse(Self).FNumeroNFSe,
                                                      OnlyNumber(TNFSeConsultarNfse(Self).Cnpj),
                                                      TNFSeConsultarNfse(Self).InscricaoMunicipal,
                                                      OnlyNumber(TNFSeConsultarNfse(Self).FCNPJTomador),
                                                      OnlyNumber(TNFSeConsultarNfse(Self).FCNPJInter),
                                                      TNFSeConsultarNfse(Self).DataInicial,
                                                      TNFSeConsultarNfse(Self).DataFinal,
                                                      FTagI, FTagF);
    else FPDadosMsg := TNFSeG.Gera_DadosMsgConsNFSe(FPrefixo3, FPrefixo4,
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

*)
  if FPDadosMsg = '' then
    GerarException(ACBrStr('A funcionalidade [Consultar NFSe] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeConsultarNfse.TratarResposta: Boolean;
begin
 {a}
end;

procedure TNFSeConsultarNfse.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeConsNfse';
  FPSoapAction := FPServico;
end;

procedure TNFSeConsultarNfse.DefinirURL;
begin
  FPLayout := LayNfseConsultaNfse;
  inherited DefinirURL;
end;

destructor TNFSeConsultarNfse.Destroy;
begin
  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeConsultarNfse.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeConsultarNfse.GerarMsgLog: String;
begin
  if Assigned(FNFSeRetorno) then
    Result := ACBrStr(FaMsg)
  else
    Result := '';
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

  FPStatus := stNFSeCancelamento;
  FPLayout := LayNfseCancelaNfse;
  FPArqEnv := 'ped-can';
  FPArqResp := 'can';

  FNFSeRetorno := nil;
end;

procedure TNFSeCancelarNfse.DefinirDadosMsg;
var
  i: Integer;
  URISig, URIRef: String;
  Gerador: TGerador;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoCancelar;

  InicializarDadosMsg;

(*
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

  FTagI := '<' + FPrefixo3 + 'CancelarNfseEnvio' + FNameSpaceDad +
            '<' + FPrefixo3 + 'Pedido>' +
             '<' + FPrefixo4 + 'InfPedidoCancelamento' +
              ifThen(FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador <> '', ' ' +
                     FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador + '="' + FURI + '"', '') + '>';

  FTagF :=  '</' + FPrefixo3 + 'Pedido>' +
           '</' + FPrefixo3 + 'CancelarNfseEnvio>';

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

   FvNotas := Gerador.ArquivoFormatoXML;

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
     FvNotas := Gerador.ArquivoFormatoXML;
   finally
     Gerador.Free;
   end;
 end;

 if FProvedorClass.GetAssinarXML(acCancelar)
  then begin
   case FProvedor of
    proIssDSF: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeDSF(FPrefixo3, FPrefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeCancelarNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeCancelarNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeCancelarNfse(Self).FCodigoMunicipio)),
                                                          TNFSeCancelarNfse(Self).FNotasFiscais.NumeroLote,
                                                          FvNotas,
                                                          '', '');
    proInfisc: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeInfisc(FPrefixo3, FPrefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeCancelarNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeCancelarNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeCancelarNfse(Self).FCodigoMunicipio)),
                                                          TNFSeCancelarNfse(Self).FNotasFiscais.NumeroLote,
                                                          FvNotas,
                                                          '', '');
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEquiplano(StrToInt(TNFSeCancelarNfse(Self).FCodigoMunicipio),
                                                                OnlyNumber(TNFSeCancelarNfse(Self).FCnpj),
                                                                TNFSeCancelarNfse(Self).FIM,
                                                                TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                                TNFSeCancelarNfse(Self).FMotivoCancelamento,
                                                                '', '');
    else FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSe(FPrefixo4,
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
   if FPDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     URIRef := '';
     NotaUtil.InitXmlSec;
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeDSF(FPrefixo3, FPrefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeCancelarNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeCancelarNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeCancelarNfse(Self).FCodigoMunicipio)),
                                                          TNFSeCancelarNfse(Self).FNotasFiscais.NumeroLote,
                                                          FvNotas,
                                                          FTagI, FTagF);
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEquiplano(StrToInt(TNFSeCancelarNfse(Self).FCodigoMunicipio),
                                                                OnlyNumber(TNFSeCancelarNfse(Self).FCnpj),
                                                                TNFSeCancelarNfse(Self).FIM,
                                                                TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                                TNFSeCancelarNfse(Self).FMotivoCancelamento,
                                                                FTagI, FTagF);
    proEL: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEL(StrToInt(TNFSeCancelarNfse(Self).FCodigoMunicipio),
                                                           OnlyNumber(TNFSeCancelarNfse(Self).FCnpj),
                                                           TNFSeCancelarNfse(Self).FIM,
                                                           TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                           TNFSeCancelarNfse(Self).FMotivoCancelamento,
                                                           FTagI, FTagF);
    proFreire: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeFreire(FPrefixo4,
                                                             NameSpaceDad,
                                                             TNFSeCancelarNfse(Self).FNumeroNFSe,
                                                             TNFSeCancelarNfse(Self).FCnpj,
                                                             TNFSeCancelarNfse(Self).FIM,
                                                             TNFSeCancelarNfse(Self).FCodigoMunicipio,
                                                             TNFSeCancelarNfse(Self).FCodigoCancelamento,
                                                             TNFSeCancelarNfse(Self).FMotivoCancelamento,
                                                             FTagI, FTagF);
    else FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSe(FPrefixo4,
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

*)
  if FPDadosMsg = '' then
    GerarException(ACBrStr('A funcionalidade [Cancelar NFSe] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeCancelarNfse.TratarResposta: Boolean;
begin
 {a}
end;

procedure TNFSeCancelarNfse.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeCancNfse';
  FPSoapAction := FPServico;
end;

procedure TNFSeCancelarNfse.DefinirURL;
begin
  FPLayout := LayNfseCancelaNfse;
  inherited DefinirURL;
end;

destructor TNFSeCancelarNfse.Destroy;
begin
  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeCancelarNfse.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeCancelarNfse.GerarMsgLog: String;
begin
  if Assigned(FNFSeRetorno) then
    Result := ACBrStr(FaMsg)
  else
    Result := '';
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

  FPStatus := stNFSeSubstituicao;
  FPLayout := LayNfseSubstituiNfse;
  FPArqEnv := 'ped-sub';
  FPArqResp := 'sub';

  FNFSeRetorno := nil;
end;

procedure TNFSeSubstituirNFSe.DefinirDadosMsg;
var
  i: Integer;
  URISig, URIRef: String;
  Gerador: TGerador;
begin
  FxsdServico := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoSubstituir;

  InicializarDadosMsg;

  if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
  begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        profintelISS, proSaatri, proSisPMJP, proISSDigital, proISSe, proSystemPro,
        pro4R, proFiorilli, proProdata, proVitoria, proPVH, proAgili, proCoplan,
        proVirtual, proFreire, proLink3, proGovDigital,
        proGoiania: FvNotas := FvNotas +
                               '<' + FPrefixo4 + 'Rps>' +
                                '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                  RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                                    '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                                '</Signature>'+
                               '</' + FPrefixo4 + 'Rps>';

        proMitra: FvNotas := FvNotas +
                             '<' + FPrefixo4 + 'Rps>' +
                              '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                                  '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</Signature>') +
                              '</Signature>'+
                             '</' + FPrefixo4 + 'Rps>';

        proDigifred: FvNotas := FvNotas +
                                '<' + FPrefixo4 + 'Rps ' +
                                   RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                                     '<' + FPrefixo4 + 'Rps', '</Signature>') +
                                 '</Signature>'+
                                '</' + Prefixo4 + 'Rps>';

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                                   '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</Signature>') +
                               '</Signature>'+
                              '</' + FPrefixo4 + 'Rps>';

        else FvNotas := FvNotas +
                        '<' + FPrefixo4 + 'Rps>' +
                         '<' + FPrefixo4 + 'InfRps' +
                           RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XMLAssinado,
                         '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                        '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end
  else begin
    for I := 0 to FNotasFiscais.Count - 1 do
    begin
      case FPConfiguracoesNFSe.Geral.Provedor of

        profintelISS, proSaatri, proSisPMJP, proGoiania, proISSDigital, proISSe,
        proSystemPro, pro4R, proFiorilli, proProdata, proVitoria, proPVH,
        proAgili, proCoplan, proVirtual, proFreire, proLink3, proActcon,
        proGovDigital: FvNotas := FvNotas +
                                  '<' + FPrefixo4 + 'Rps>' +
                                   '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico' +
                                     RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>') +
                                   '</' + FPrefixo4 + 'InfDeclaracaoPrestacaoServico>'+
                                  '</' + FPrefixo4 + 'Rps>';

        proIssDSF,
        proInfisc: FvNotas :=  FvNotas + TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XMLOriginal;

        proTecnos: FvNotas := FvNotas +
                              '<' + FPrefixo4 + 'Rps>' +
                               '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico' +
                                 RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XMLOriginal,
                                 '<' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico', '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>') +
                               '</' + FPrefixo4 + 'tcDeclaracaoPrestacaoServico>'+
                              '</' + FPrefixo4 + 'Rps>';

        else FvNotas := FvNotas + '<' + FPrefixo4 + 'Rps>' +
                                 '<' + FPrefixo4 + 'InfRps' +
                                   RetornarConteudoEntre(TNFSeSubstituirNFSe(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + FPrefixo4 + 'InfRps', '</' + FPrefixo4 + 'Rps>') +
                                '</' + FPrefixo4 + 'Rps>';
      end;
    end;
  end;

(*
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

  FTagI := '<' + FPrefixo3 + 'SubstituirNfseEnvio' + FNameSpaceDad +
            '<' + FPrefixo3 + 'SubstituicaoNfse>' +
             '<' + FPrefixo3 + 'Pedido>' +
              '<' + FPrefixo4 + 'InfPedidoCancelamento' +
                ifThen(FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador <> '', ' ' +
                       FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador + '="' + FURI + '"', '') + '>';

  FTagF :=  '</' + FPrefixo3 + 'SubstituicaoNfse>' +
           '</' + FPrefixo3 + 'SubstituirNfseEnvio>';

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

   FvNotas := Gerador.ArquivoFormatoXML;

   Gerador.Free;
 end;

 if FProvedorClass.GetAssinarXML(acSubstituir)
  then begin
   case FProvedor of
    proIssDSF: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeDSF(FPrefixo3, FPrefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeSubstituirNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeSubstituirNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeSubstituirNfse(Self).FCodigoMunicipio)),
                                                          TNFSeSubstituirNfse(Self).FNotasFiscais.NumeroLote,
                                                          FvNotas,
                                                          '', '');
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEquiplano(StrToInt(TNFSeSubstituirNfse(Self).FCodigoMunicipio),
                                                                OnlyNumber(TNFSeSubstituirNfse(Self).FCnpj),
                                                                TNFSeSubstituirNfse(Self).FIM,
                                                                TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                                TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                                '', '');
    else FPDadosMsg := TNFSeG.Gera_DadosMsgSubstituirNFSe(FPrefixo3, FPrefixo4,
                                                         FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador, NameSpaceDad,
                                                         TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                         TNFSeSubstituirNfse(Self).FCnpj,
                                                         TNFSeSubstituirNfse(Self).FIM,
                                                         TNFSeSubstituirNfse(Self).FCodigoMunicipio,
                                                         TNFSeSubstituirNfse(Self).FCodigoCancelamento,
                                                         TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                         VersaoDados, FVersaoXML,
                                                         IntToStr(TNFSeSubstituirNfse(Self).NumeroRps),
                                                         IntToStr(TNFSeSubstituirNfse(Self).FNotasFiscais.Count),
                                                         FvNotas,
                                                         '', '',
                                                         FProvedor);
   end;
   if FPDadosMsg <> ''
    then begin
    {$IFDEF ACBrNFSeOpenSSL}
     URIRef := '';
     NotaUtil.InitXmlSec;
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.Certificado,
                     FConfiguracoes.Certificados.Senha,
                     FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ELSE}
     if not(NotaUtil.AssinarXML(FPDadosMsg, URISig, URIRef, FTagI, FTagF,
                     FConfiguracoes.Certificados.GetCertificado, FvAssinada, FMsg, FProvedor))
      then raise Exception.Create('Falha ao assinar o XML ' + FMsg)
      else FPDadosMsg := FvAssinada;
    {$ENDIF}
    end;
  end
  else begin
   case FProvedor of
    proIssDSF: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeDSF(FPrefixo3, FPrefixo4,
                                                          NameSpaceDad, VersaoXML,
                                                          TNFSeSubstituirNfse(Self).FCnpj,
                                                          LowerCase(booltostr(TNFSeSubstituirNfse(Self).FNotasFiscais.Transacao, True)),
                                                          CodCidadeToCodSiafi(strtoint64(TNFSeSubstituirNfse(Self).FCodigoMunicipio)),
                                                          TNFSeSubstituirNfse(Self).FNotasFiscais.NumeroLote,
                                                          FvNotas,
                                                          FTagI, FTagF);
    proEquiplano: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeEquiplano(StrToInt(TNFSeSubstituirNfse(Self).FCodigoMunicipio),
                                                                OnlyNumber(TNFSeSubstituirNfse(Self).FCnpj),
                                                                TNFSeSubstituirNfse(Self).FIM,
                                                                TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                                TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                                FTagI, FTagF);
    proFreire: FPDadosMsg := TNFSeG.Gera_DadosMsgCancelarNFSeFreire(FPrefixo4,
                                                             NameSpaceDad,
                                                             TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                             TNFSeSubstituirNfse(Self).FCnpj,
                                                             TNFSeSubstituirNfse(Self).FIM,
                                                             TNFSeSubstituirNfse(Self).FCodigoMunicipio,
                                                             TNFSeSubstituirNfse(Self).FCodigoCancelamento,
                                                             TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                             FTagI, FTagF);
    else FPDadosMsg := TNFSeG.Gera_DadosMsgSubstituirNFSe(FPrefixo3, FPrefixo4,
                                                         FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador, NameSpaceDad,
                                                         TNFSeSubstituirNfse(Self).FNumeroNFSe,
                                                         TNFSeSubstituirNfse(Self).FCnpj,
                                                         TNFSeSubstituirNfse(Self).FIM,
                                                         TNFSeSubstituirNfse(Self).FCodigoMunicipio,
                                                         TNFSeSubstituirNfse(Self).FCodigoCancelamento,
                                                         TNFSeSubstituirNfse(Self).FMotivoCancelamento,
                                                         VersaoDados, FVersaoXML,
                                                         IntToStr(TNFSeSubstituirNfse(Self).NumeroRps),
                                                         IntToStr(TNFSeSubstituirNfse(Self).FNotasFiscais.Count),
                                                         FvNotas,
                                                         FTagI, FTagF,
                                                         FProvedor);
   end;
  end;

*)
  if FPDadosMsg = '' then
    GerarException(ACBrStr('A funcionalidade [Substituir NFSe] não foi disponibilizada pelo provedor: ' +
     FPConfiguracoesNFSe.Geral.xProvedor));
end;

function TNFSeSubstituirNFSe.TratarResposta: Boolean;
begin
 {a}
end;

procedure TNFSeSubstituirNFSe.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeSubNfse';
  FPSoapAction := FPServico;
end;

procedure TNFSeSubstituirNFSe.DefinirURL;
begin
  FPLayout := LayNfseSubstituiNfse;
  inherited DefinirURL;
end;

destructor TNFSeSubstituirNFSe.Destroy;
begin
  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeSubstituirNFSe.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeSubstituirNFSe.GerarMsgLog: String;
begin
  if Assigned(FNFSeRetorno) then
    Result := ACBrStr(FaMsg)
  else
    Result := '';
end;

function TNFSeSubstituirNFSe.GerarPrefixoArquivo: String;
begin
  Result := NumeroNFSe;
end;

{ TNFSeLinkNFSe }

constructor TNFSeLinkNFSe.Create(AOwner: TACBrDFe;
  ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNfseRecepcaoLote;
  FPArqEnv := '';
  FPArqResp := '';
end;

procedure TNFSeLinkNFSe.DefinirDadosMsg;
var
  Texto, xNumeroNFSe, xNomeMunic: String;
begin
 if FPConfiguracoesNFSe.WebServices.Ambiente = taProducao then
   Texto := FPConfiguracoesNFSe.Geral.ConfigGeral.ProLinkNFSe
 else
   Texto := FPConfiguracoesNFSe.Geral.ConfigGeral.HomLinkNFSe;

  // %CodVerif%      : Representa o Código de Verificação da NFS-e
  // %NumeroNFSe%    : Representa o Numero da NFS-e
  // %NomeMunicipio% : Representa o Nome do Municipio
  // %InscMunic%     : Representa a Inscrição Municipal do Emitente

  xNumeroNFSe := inttostr(FNumeroNFSe);
  xNomeMunic := FPConfiguracoesNFSe.Geral.xMunicipio;
  // Remove os acentos, espaços em branco e converte tudo para minusculo
  xNomeMunic := TiraAcentos(xNomeMunic);
  xNomeMunic := StringReplace(xNomeMunic, ' ', '', [rfReplaceAll]);
  xNomeMunic := LowerCase(xNomeMunic);

  Texto := StringReplace(Texto, '%CodVerif%', FCodVerif, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%NumeroNFSe%', xNumeroNFSe, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%NomeMunicipio%', xNomeMunic, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%InscMunic%', FIM, [rfReplaceAll]);

  FLink := Texto;
end;

function TNFSeLinkNFSe.TratarResposta: Boolean;
begin
 {a}
end;

procedure TNFSeLinkNFSe.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeLinkNfse';
  FPSoapAction := FPServico;
end;

procedure TNFSeLinkNFSe.DefinirURL;
begin
  FPLayout := LayNfseRecepcaoLote;
  inherited DefinirURL;
end;

destructor TNFSeLinkNFSe.Destroy;
begin
  inherited Destroy;
end;

procedure TNFSeLinkNFSe.FinalizarServico;
begin
  inherited FinalizarServico;
end;

function TNFSeLinkNFSe.GerarMsgLog: String;
begin
  Result := '';
end;

function TNFSeLinkNFSe.GerarPrefixoArquivo: String;
begin
  Result := '';
end;

// =============================================================================

{ TNFSeEnvioWebService }

constructor TNFSeEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stNFSeEnvioWebService;
  FVersao := '';
end;

destructor TNFSeEnvioWebService.Destroy;
begin
  inherited Destroy;
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
end;

procedure TNFSeEnvioWebService.DefinirDadosMsg;
var
  LeitorXML: TLeitor;
begin
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
  FLinkNfse       := TNFSeLinkNfse.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  
  FEnvioWebService := TNFSeEnvioWebService.Create(FACBrNFSe);
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
  FLinkNfse.Free;
  FEnvioWebService.Free;

  inherited Destroy;
end;

function TWebServices.GeraLote(ALote: Integer): Boolean;
begin
  Result := GeraLote(IntToStr(ALote));
end;

function TWebServices.GeraLote(ALote: String): Boolean;
begin
  FGerarLoteRPS.FNumeroLote := ALote;

  Result := GerarLoteRPS.Executar;

  if not (Result) then
    GerarLoteRPS.GerarException( GerarLoteRPS.Msg );
end;

function TWebServices.Envia(ALote: Integer): Boolean;
begin
  Result := Envia(IntToStr(ALote));
end;

function TWebServices.Envia(ALote: String): Boolean;
begin
  FEnviarLoteRPS.FNumeroLote := ALote;

  Result := FEnviarLoteRPS.Executar;

  if not (Result) then
    FEnviarLoteRPS.GerarException( FEnviarLoteRPS.Msg );

  FConsSitLoteRPS.FCnpj               := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;
  FConsSitLoteRPS.FInscricaoMunicipal := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
  FConsSitLoteRPS.FProtocolo          := FEnviarLoteRPS.Protocolo;
  FConsSitLoteRPS.FNumeroLote         := FEnviarLoteRPS.NumeroLote;

  if TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor in [proISSDigital] then
  begin
    FConsSitLoteRPS.FSenha        := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.Senha;
    FConsSitLoteRPS.FFraseSecreta := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.FraseSecreta;
  end;

  FConsLote.FProtocolo := FEnviarLoteRPS.Protocolo;

  if TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor in [proISSDigital, proTecnos] then
  begin
    FConsLote.FSenha        := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.Senha;
    FConsLote.FFraseSecreta := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.FraseSecreta;
  end;

  if (TACBrNFSe(FACBrNFSe).Configuracoes.Geral.ConsultaLoteAposEnvio) and (Result) then
  begin
    if not (TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor in [proDigifred, proProdata,
           proVitoria, proPVH, profintelISS, proSaatri, proSisPMJP, proCoplan,
           proISSDigital, proISSDSF, proFiorilli, proFreire, proTecnos, proDBSeller]) then
     begin
       Result := FConsSitLoteRPS.Executar;

       if not (Result) then
         FConsSitLoteRPS.GerarException( FConsSitLoteRPS.Msg );
     end;

     if TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor = proInfisc then
       Result := True
     else
       Result := FConsLote.Executar;

     if not (Result) then
       FConsLote.GerarException( FConsLote.Msg );
  end;
end;

function TWebServices.EnviaSincrono(ALote: Integer): Boolean;
begin
  Result := EnviaSincrono(IntToStr(ALote));
end;

function TWebServices.EnviaSincrono(ALote: String): Boolean;
begin
  FEnviarSincrono.FNumeroLote := ALote;

  Result := FEnviarSincrono.Executar;

  if not (Result) then
    FEnviarSincrono.GerarException( FEnviarSincrono.Msg );
end;

function TWebServices.Gera(ARps: Integer): Boolean;
begin
 FGerarNfse.FNumeroRps := ARps;

 Result := FGerarNfse.Executar;

 if not (Result) then
   FGerarNfse.GerarException( FGerarNfse.Msg );
end;

function TWebServices.ConsultaSituacao(ACnpj, AInscricaoMunicipal,
  AProtocolo: String; const ANumLote: String): Boolean;
begin
  ACnpj := OnlyNumber(ACnpj);
  if not ValidarCNPJ(ACnpj) then
    FConsSitLoteRPS.GerarException( 'CNPJ ' + ACnpj + ' inválido.' );

  FConsSitLoteRPS.FCnpj               := ACnpj;
  FConsSitLoteRPS.FInscricaoMunicipal := AInscricaoMunicipal;
  FConsSitLoteRPS.FProtocolo          := AProtocolo;
  FConsSitLoteRPS.FNumeroLote         := ANumLote;

  Result := FConsSitLoteRPS.Executar;

  if not (Result) then
   FConsSitLoteRPS.GerarException( FConsSitLoteRPS.Msg );
end;

function TWebServices.ConsultaLoteRps(AProtocolo: String;
  const CarregaProps: boolean): Boolean;
begin
  if CarregaProps then
  begin
     FConsLote.FCNPJ := '';
     FConsLote.FIM   := '';
  end;

  FConsLote.FProtocolo := AProtocolo;

  Result := FConsLote.Executar;

  if not (Result) then
    FConsLote.GerarException( FConsLote.Msg );
end;

function TWebServices.ConsultaLoteRps(AProtocolo, ACNPJ,
  AInscricaoMunicipal: String; const ASenha, AFraseSecreta,
  ARazaoSocial: String): Boolean;
begin
  FConsLote.FCNPJ         := ACNPJ;
  FConsLote.FIM           := AInscricaoMunicipal;
  FConsLote.FSenha        := ASenha;
  FConsLote.FFraseSecreta := AFraseSecreta;
  FConsLote.FRazaoSocial  := ARazaoSocial;

  Result := ConsultaLoteRPS(AProtocolo, False);
end;

function TWebServices.ConsultaNFSeporRps(ANumero, ASerie, ATipo, ACnpj,
  AInscricaoMunicipal: String; const ASenha, AFraseSecreta,
  ARazaoSocial: String): Boolean;
begin
  ACnpj := OnlyNumber(ACnpj);

  if not ValidarCNPJ(ACnpj) and (Length(ACnpj) = 14) then
    FConsNfseRps.GerarException( 'CNPJ ' + ACnpj + ' inválido.' );

  if not ValidarCPF(ACnpj) and (Length(ACnpj) = 11) then
    FConsNfseRps.GerarException( 'CPF ' + ACnpj + ' inválido.' );

  FConsNfseRps.FNumero             := ANumero;
  FConsNfseRps.FSerie              := ASerie;
  FConsNfseRps.FTipo               := ATipo;
  FConsNfseRps.FCnpj               := ACnpj;
  FConsNfseRps.FInscricaoMunicipal := AInscricaoMunicipal;
  FConsNfseRps.FSenha              := ASenha;
  FConsNfseRps.FFraseSecreta       := AFraseSecreta;
  FConsNfseRps.FRazaoSocial        := ARazaoSocial;

  Result := FConsNfseRps.Executar;

  if not (Result) then
    FConsNfseRps.GerarException( FConsNfseRps.Msg );
end;

function TWebServices.ConsultaNFSe(ACnpj, AInscricaoMunicipal: String;
  ADataInicial, ADataFinal: TDateTime; NumeroNFSe: String;
  APagina: Integer; const ASenha, AFraseSecreta: String; ACNPJTomador,
  AIMTomador, ANomeInter, ACNPJInter, AIMInter, ASerie: String): Boolean;
begin
  ACnpj := OnlyNumber(ACnpj);
  if not ValidarCNPJ(ACnpj) then
    FConsNfse.GerarException( 'CNPJ ' + ACnpj + ' inválido.' );

  FConsNfse.FCnpj               := ACnpj;
  FConsNfse.FInscricaoMunicipal := AInscricaoMunicipal;
  FConsNfse.FDataInicial        := ADataInicial;
  FConsNfse.FDataFinal          := ADataFinal;
  FConsNfse.FNumeroNFSe         := NumeroNFSe;
  FConsNfse.FPagina             := APagina;
  FConsNfse.FSenha              := ASenha;
  FConsNfse.FFraseSecreta       := AFraseSecreta;
  FConsNfse.FCNPJTomador        := ACNPJTomador;
  FConsNfse.FIMTomador          := AIMTomador;
  FConsNfse.FNomeInter          := ANomeInter;
  FConsNfse.FCNPJInter          := ACNPJInter;
  FConsNfse.FIMInter            := AIMInter;
  FConsNfse.FSerie              := ASerie;

  Result := FConsNfse.Executar;

  if not (Result) then
    FConsNfse.GerarException( FConsNfse.Msg );
end;

function TWebServices.CancelaNFSe(ACodigoCancelamento: String;
  const CarregaProps: boolean): Boolean;
begin
  if CarregaProps then
  begin
    FCancNfse.FNumeroNFSe := '';
    FCancNfse.FCNPJ := '';
    FCancNfse.FIM := '';
    FCancNfse.FCodigoMunicipio := '';
  end;

  if TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor = proEL then
  begin
    FCancNfse.FNumeroNFSe      := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Numero;
    FCancNfse.FCNPJ            := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
    FCancNfse.FIM              := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
    FCancNfse.FCodigoMunicipio := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio;
  end;

  FCancNfse.FCodigoCancelamento := ACodigoCancelamento;

  Result := FCancNfse.Executar;

  if not (Result) then
    FCancNfse.GerarException( FCancNfse.Msg );

  if not (TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor in [proISSNet, proEL]) then
  begin
    if TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor in [proSystemPro] then
    begin
      FConsNfse.FNumeroNFSe         := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Numero;
      FConsNfse.FCnpj               := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
      FConsNfse.FInscricaoMunicipal := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

      Result := FConsNfse.Executar;
    end
    else begin
      FConsNfseRps.FNumero := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero;
      FConsNfseRps.FSerie  := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie;
      FConsNfseRps.FTipo   := TipoRPSToStr(TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Tipo);
      FConsNfseRps.FCnpj   := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;

      if FConsNfseRps.Cnpj = '' then
        FConsNfseRps.FCnpj := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;

      FConsNfseRps.FInscricaoMunicipal := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;

      if FConsNfseRps.InscricaoMunicipal = '' then
        FConsNfseRps.FInscricaoMunicipal := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

      FConsNfseRps.RazaoSocial := '';

      if not (TACBrNFSe(FACBrNFSe).Configuracoes.Geral.Provedor in [proDigifred]) then
        FConsNfseRps.FRazaoSocial := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial;

      Result := FConsNfseRps.Executar;
    end;

    if not(Result) then
      FConsNfseRps.GerarException( FConsNfseRps.Msg );
  end;
end;

function TWebServices.CancelaNFSe(ACodigoCancelamento, ANumeroNFSe, ACNPJ,
  AInscricaoMunicipal, ACodigoMunicipio: String): Boolean;
begin
  FCancNfse.FNumeroNFSe      := ANumeroNFSe;
  FCancNfse.FCNPJ            := ACNPJ;
  FCancNfse.FIM              := AInscricaoMunicipal;
  FCancNfse.FCodigoMunicipio := ACodigoMunicipio;

  Result := CancelaNFSe(ACodigoCancelamento, False);
end;

function TWebServices.SubstitiNFSe(ACodigoCancelamento,
  ANumeroNFSe: String): Boolean;
begin
  FSubNfse.FCodigoCancelamento := ACodigoCancelamento;
  FSubNfse.FMotivoCancelamento := '';

  FSubNfse.FNumeroNFSe      := ANumeroNFSe;
  FSubNfse.FCnpj            := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;
  FSubNfse.FIM              := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
  FSubNfse.FCodigoMunicipio := TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.Servico.CodigoMunicipio;
  FSubNfse.FNumeroRps       := StrToInt(TACBrNFSe(FACBrNFSe).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero);

  Result := FSubNfse.Executar;

  if not (Result) then
    FSubNfse.GerarException( FSubNfse.Msg );
end;

function TWebServices.LinkNFSeGerada(ANumeroNFSe: Integer; ACodVerificacao,
  AInscricaoM: String): String;
begin
  FLinkNfse.FNumeroNFSe := ANumeroNFSe;
  FLinkNFSe.FCodVerif   := ACodVerificacao;
  FLinkNfse.FIM         := AInscricaoM;

  Result := '';

  if not (FLinkNfse.Executar) then
    FLinkNfse.GerarException( FLinkNfse.Msg );

  Result := FLinkNFSe.FLink;
end;

end.

