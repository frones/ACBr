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

    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    function GerarCabecalhoSoap: String; override;
    procedure FinalizarServico; override;
    procedure DefinirEnvelopeSoap; override;

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

    property NumeroRps: integer read FNumeroRps;
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

    property NumeroRps: integer         read FNumeroRps;
    property Protocolo: String          read FProtocolo;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Situacao: String           read FSituacao;

    property ArquivoRetorno: String read FArquivoRetorno write FArquivoRetorno;
  end;

{ TNFSeLinkNFSe }

  TNFSeLinkNFSe = Class(TNFSeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    
    FNumeroNFSe: integer;
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

    property NumeroNFSe: integer read FNumeroNFSe;
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

    function Gera(ARps: Integer): Boolean; overload;
    function Gera(ARps: String): Boolean; overload;

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
  Texto := stringReplace(Texto, '%CabMsg%', FPCabMsg, [rfReplaceAll]);
  Texto := stringReplace(Texto, '%DadosMsg%', FPDadosMsg, [rfReplaceAll]);

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
  I: integer;
  URI,
  Separador,
  vNotas,
  NameSpace,
  ServicoEnviar,
  DefTipos,
  Cabecalho,
  Prefixo2,
  Prefixo3,
  Prefixo4: String;
begin
  vNotas := '';

  NameSpace := FPConfiguracoesNFSe.Geral.ConfigXML.NameSpace;
  DefTipos := FPConfiguracoesNFSe.Geral.ConfigSchemas.DefTipos;
  ServicoEnviar := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar;
  Cabecalho := FPConfiguracoesNFSe.Geral.ConfigSchemas.Cabecalho;
  Prefixo2 := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo2;
  Prefixo3 := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo3;
  Prefixo4 := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo4;

  if RightStr(NameSpace, 1) = '/' then
    Separador := ''
  else
    Separador := '/';

  if Cabecalho <> '' then
  begin
    if Prefixo2 <> '' then
      FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) +
                       '="' + NameSpace + Separador + Cabecalho +'">'
    else
      FNameSpaceCab := ' xmlns="' + NameSpace + Separador + Cabecalho +'">';
  end
  else
    FNameSpaceCab := '>';

  if FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar <> '' then
  begin
    if (FPConfiguracoesNFSe.Geral.Provedor = proIssDSF) then
      FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + '" '
    else
      if (FPConfiguracoesNFSe.Geral.Provedor = proInfisc) then
        FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + '" '
      else begin
        if (RightStr(NameSpace, 1) = '/') then
        begin
          if Prefixo3 <> '' then
            FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + Separador + ServicoEnviar + '"'
          else
            FNameSpaceDad := 'xmlns="' + NameSpace + Separador + ServicoEnviar + '"';
        end
        else begin
          if Prefixo3 <> '' then
            FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + '"'
          else
            FNameSpaceDad := 'xmlns="' + NameSpace + '"';
        end;
      end;
  end
  else
    FNameSpaceDad := '';

  if (DefTipos = '') and (NameSpaceDad <> '') then
    FNameSpaceDad := FNameSpaceDad + '>';

  if DefTipos <> '' then
  begin
    if Prefixo4 <> '' then
      FNameSpaceDad := FNameSpaceDad + ' xmlns:' +
                       StringReplace(Prefixo4, ':', '', []) + '="' + NameSpace + Separador + DefTipos + '">'
    else
      FNameSpaceDad := FNameSpaceDad + ' xmlns="' + NameSpace + Separador + DefTipos + '">';
  end;

  if FNameSpaceDad = '' then
    FNameSpaceDad := '>'
  else
    FNameSpaceDad := ' ' + FNameSpaceDad;

  if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
  begin
    for I := 0 to FNotasFiscais.Count - 1 do
       vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                             '<' + Prefixo4 + 'InfRps' +
                                RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                                     '<' + Prefixo4 + 'InfRps', '</Rps>') +
                          '</' + Prefixo4 + 'Rps>';
  end
  else begin
    for I := 0 to FNotasFiscais.Count - 1 do
       vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                             '<' + Prefixo4 + 'InfRps' +
                                RetornarConteudoEntre(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + Prefixo4 + 'InfRps', '</Rps>') +
                          '</' + Prefixo4 + 'Rps>';
  end;

  FPCabMsg := FPConfiguracoesNFSe.Geral.ConfigEnvelope.CabecalhoMsg;
  FURI := '';
//  FURI := FProvedorClass.GetURI(URI);
  FTagI := '<' + Prefixo3 + 'EnviarLoteEnvio' + FNameSpaceDad;
  FTagF := '</' + Prefixo3 + 'EnviarLoteEnvio>';
  FDadosSenha := '';
//  FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb, FConfiguracoes.WebServices.SenhaWeb);

  FPDadosMsg := TNFSeG.Gera_DadosMsgEnviarLote(Prefixo3, Prefixo4,
                                               FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador,
                                               NameSpace,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoDados,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoXML,
                                               TNFSeGerarLoteRps(Self).NumeroLote,
                                               OnlyNumber(TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                               TNFSeGerarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                               IntToStr(TNFSeGerarLoteRps(Self).FNotasFiscais.Count),
                                               vNotas,
                                               FTagI, FTagF, FPConfiguracoesNFSe.Geral.Provedor);

  if FPDadosMsg <> '' then
  begin
//    if FPConfiguracoesNFSe.Geral.ConfigAssinar.Lote then
//      FPDadosMsg := TNFSeEnviarLoteRPS(Self).FNotasFiscais.AssinarLoteRps(TNFSeGerarLoteRps(Self).NumeroLote, FPDadosMSg);

//    if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
//    begin
//      if not(NotaUtil.Valida(FDadosMsg, FMsg, FPConfiguracoes.Geral.PathSchemas,
//                             FPConfiguracoes.WebServices.URL,
//                             FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar,
//                             Prefixo4)) then
//        GerarException(ACBrStr('Falha na validação do Lote ' +
//                               TNFSeGerarLoteRps(Self).NumeroLote + sLineBreak + FMsg));
//    end;
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
  I: integer;
  URI,
  Separador,
  vNotas,
  NameSpace,
  ServicoEnviar,
  DefTipos,
  Cabecalho,
  Prefixo2,
  Prefixo3,
  Prefixo4: String;
begin
  vNotas := '';

  NameSpace := FPConfiguracoesNFSe.Geral.ConfigXML.NameSpace;
  DefTipos := FPConfiguracoesNFSe.Geral.ConfigSchemas.DefTipos;
  ServicoEnviar := FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar;
  Cabecalho := FPConfiguracoesNFSe.Geral.ConfigSchemas.Cabecalho;
  Prefixo2 := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo2;
  Prefixo3 := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo3;
  Prefixo4 := FPConfiguracoesNFSe.Geral.ConfigGeral.Prefixo4;

  if RightStr(NameSpace, 1) = '/' then
    Separador := ''
  else
    Separador := '/';

  if Cabecalho <> '' then
  begin
    if Prefixo2 <> '' then
      FNameSpaceCab := ' xmlns:' + StringReplace(Prefixo2, ':', '', []) +
                       '="' + NameSpace + Separador + Cabecalho +'">'
    else
      FNameSpaceCab := ' xmlns="' + NameSpace + Separador + Cabecalho +'">';
  end
  else
    FNameSpaceCab := '>';

  if FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar <> '' then
  begin
    if (FPConfiguracoesNFSe.Geral.Provedor = proIssDSF) then
      FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + '" '
    else
      if (FPConfiguracoesNFSe.Geral.Provedor = proInfisc) then
        FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + '" '
      else begin
        if (RightStr(NameSpace, 1) = '/') then
        begin
          if Prefixo3 <> '' then
            FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + Separador + ServicoEnviar + '"'
          else
            FNameSpaceDad := 'xmlns="' + NameSpace + Separador + ServicoEnviar + '"';
        end
        else begin
          if Prefixo3 <> '' then
            FNameSpaceDad := 'xmlns:' + StringReplace(Prefixo3, ':', '', []) + '="' + NameSpace + '"'
          else
            FNameSpaceDad := 'xmlns="' + NameSpace + '"';
        end;
      end;
  end
  else
    FNameSpaceDad := '';

  if (DefTipos = '') and (NameSpaceDad <> '') then
    FNameSpaceDad := FNameSpaceDad + '>';

  if DefTipos <> '' then
  begin
    if Prefixo4 <> '' then
      FNameSpaceDad := FNameSpaceDad + ' xmlns:' +
                       StringReplace(Prefixo4, ':', '', []) + '="' + NameSpace + Separador + DefTipos + '">'
    else
      FNameSpaceDad := FNameSpaceDad + ' xmlns="' + NameSpace + Separador + DefTipos + '">';
  end;

  if FNameSpaceDad = '' then
    FNameSpaceDad := '>'
  else
    FNameSpaceDad := ' ' + FNameSpaceDad;

  if FPConfiguracoesNFSe.Geral.ConfigAssinar.RPS then
  begin
    for I := 0 to FNotasFiscais.Count - 1 do
       vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                             '<' + Prefixo4 + 'InfRps' +
                                RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLAssinado,
                                     '<' + Prefixo4 + 'InfRps', '</Rps>') +
                          '</' + Prefixo4 + 'Rps>';
  end
  else begin
    for I := 0 to FNotasFiscais.Count - 1 do
       vNotas := vNotas + '<' + Prefixo4 + 'Rps>' +
                             '<' + Prefixo4 + 'InfRps' +
                                RetornarConteudoEntre(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[I].XMLOriginal,
                                     '<' + Prefixo4 + 'InfRps', '</Rps>') +
                          '</' + Prefixo4 + 'Rps>';
  end;

  FPCabMsg := FPConfiguracoesNFSe.Geral.ConfigEnvelope.CabecalhoMsg;
  FURI := '';
//  FURI := FProvedorClass.GetURI(URI);
  FTagI := '<' + Prefixo3 + 'EnviarLoteEnvio' + FNameSpaceDad;
  FTagF := '</' + Prefixo3 + 'EnviarLoteEnvio>';
  FDadosSenha := '';
//  FDadosSenha := FProvedorClass.Gera_DadosSenha(FConfiguracoes.WebServices.UserWeb, FConfiguracoes.WebServices.SenhaWeb);

  FPDadosMsg := TNFSeG.Gera_DadosMsgEnviarLote(Prefixo3, Prefixo4,
                                               FPConfiguracoesNFSe.Geral.ConfigGeral.Identificador,
                                               NameSpace,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoDados,
                                               FPConfiguracoesNFSe.Geral.ConfigXML.VersaoXML,
                                               TNFSeEnviarLoteRps(Self).NumeroLote,
                                               OnlyNumber(TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.Cnpj),
                                               TNFSeEnviarLoteRPS(Self).FNotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal,
                                               IntToStr(TNFSeEnviarLoteRps(Self).FNotasFiscais.Count),
                                               vNotas,
                                               FTagI, FTagF, FPConfiguracoesNFSe.Geral.Provedor);

  if FPDadosMsg <> '' then
  begin
//    if FPConfiguracoesNFSe.Geral.ConfigAssinar.Lote then
//      FPDadosMsg := TNFSeEnviarLoteRPS(Self).FNotasFiscais.AssinarLoteRps(TNFSeEnviarLoteRps(Self).NumeroLote, FPDadosMSg);

//    if FPConfiguracoesNFSe.Geral.ConfigSchemas.Validar then
//    begin
//      if not(NotaUtil.Valida(FDadosMsg, FMsg, FConfiguracoes.Geral.PathSchemas,
//                             FConfiguracoes.WebServices.URL,
//                             FPConfiguracoesNFSe.Geral.ConfigSchemas.ServicoEnviar,
//                             Prefixo4)) then
//        GerarException(ACBrStr('Falha na validação do Lote ' +
//                               TNFSeEnviarLoteRps(Self).NumeroLote + sLineBreak + FMsg));
//    end;
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
begin
  inherited;
{a}
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
begin
  inherited;
{a}
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
  Result := NumeroRPS;
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
begin
  inherited;
{a}
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
begin
  inherited;
{a}
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
begin
  inherited;
 {a}
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
begin
  inherited;
 {a}
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
begin
  inherited;
 {a}
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
  Result := Numero;
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
begin
  inherited;
 {a}
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
  Result := Numero;
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

  FNFSeRetorno := nil;
end;

procedure TNFSeLinkNFSe.DefinirDadosMsg;
begin
  inherited;
 {a}
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

  FConsSitLoteRPS.FCnpj               := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;
  FConsSitLoteRPS.FInscricaoMunicipal := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
  FConsSitLoteRPS.FProtocolo          := FEnviarLoteRPS.Protocolo;
  FConsSitLoteRPS.FNumeroLote         := FEnviarLoteRPS.NumeroLote;

  if FPConfiguracoesNFSe.Geral.Provedor in [proISSDigital] ) then
  begin
    FConsSitLoteRPS.FSenha        := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.Senha;
    FConsSitLoteRPS.FFraseSecreta := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.FraseSecreta;
  end;

  FConsLote.FProtocolo := FEnviarLoteRPS.Protocolo;

  if FPConfiguracoesNFSe.Geral.Provedor in [proISSDigital, proTecnos] ) then
  begin
    FConsLote.FSenha        := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.Senha;
    FConsLote.FFraseSecreta := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.FraseSecreta;
  end;

  if (TACBrNFSe( FACBrNFSe ).Configuracoes.WebServices.ConsultaLoteAposEnvio) and (Result) then
  begin
    if not FPConfiguracoesNFSe.Geral.Provedor in [proDigifred, proProdata,
           proVitoria, proPVH, profintelISS, proSaatri, proSisPMJP, proCoplan,
           proISSDigital, proISSDSF, proFiorilli, proFreire, proTecnos, proDBSeller]) then
     begin
       Result := FConsSitLoteRPS.Executar;

       if not (Result) then
         FConsSitLoteRPS.GerarException( FConsSitLoteRPS.Msg );
     end;

     if FPConfiguracoesNFSe.Geral.Provedor = proInfisc then
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
  Result := Gera(IntToStr(ARps));
end;

function TWebServices.Gera(ARps: String): Boolean;
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

  if FPConfiguracoesNFSe.Geral.Provedor = proEL) then
  begin
    FSelf.CancNfse.FNumeroNFSe      := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Numero;
    FSelf.CancNfse.FCNPJ            := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
    FSelf.CancNfse.FIM              := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;
    FSelf.CancNfse.FCodigoMunicipio := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.PrestadorServico.Endereco.CodigoMunicipio;
  end;

  FCancNfse.FCodigoCancelamento := ACodigoCancelamento;

  Result := FCancNfse.Executar;

  if not (Result) then
    FCancNfse.GerarException( FCancNfse.Msg );

  if not (TFPConfiguracoesNFSe.Geral.Provedor in [proISSNet, proEL]) then
  begin
    if FPConfiguracoesNFSe.Geral.Provedor in [proSystemPro] then
    begin
      FConsNfse.FNumeroNFSe         := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Numero;
      FConsNfse.FCnpj               := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;
      FConsNfse.FInscricaoMunicipal := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

      Result := FConsNfse.Executar;
    end
    else begin
      FConsNfseRps.FNumero := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero;
      FConsNfseRps.FSerie  := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Serie;
      FConsNfseRps.FTipo   := TipoRPSToStr(TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Tipo);
      FConsNfseRps.FCnpj   := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;

      if FConsNfseRps.Cnpj = '' then
        FConsNfseRps.FCnpj := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.Cnpj;

      FConsNfseRps.FInscricaoMunicipal := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;

      if FConsNfseRps.InscricaoMunicipal = '' then
        FConsNfseRps.FInscricaoMunicipal := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.PrestadorServico.IdentificacaoPrestador.InscricaoMunicipal;

      FConsNfseRps.RazaoSocial := '';

      if not (FPConfiguracoesNFSe.Geral.Provedor in [proDigifred]) then
        FConsNfseRps.FRazaoSocial := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.PrestadorServico.RazaoSocial;

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
  FSubNfse.FCnpj            := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.Cnpj;
  FSubNfse.FIM              := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Prestador.InscricaoMunicipal;
  FSubNfse.FCodigoMunicipio := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.Servico.CodigoMunicipio;
  FSubNfse.FNumeroRps       := StrToInt(TACBrNFSe(FPDFeOwner).NotasFiscais.Items[0].NFSe.IdentificacaoRps.Numero);

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

