{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

(*

  Documentação:
  https://github.com/bacen/pix-api

*)
{$I ACBr.inc}

unit ACBrPIXCD;

interface

uses
  Classes, SysUtils, httpsend, ssl_openssl,
  ACBrOpenSSLUtils, ACBrBase, ACBrPIXBase, ACBrPIXBRCode,
  ACBrPIXSchemasPix,
  ACBrPIXSchemasRec,
  ACBrPIXSchemasCob,
  ACBrPIXSchemasCobV,
  ACBrPIXSchemasCobR,
  ACBrPIXSchemasWebhook,
  ACBrPIXSchemasSolicRec,
  ACBrPIXSchemasLocation,
  ACBrPIXSchemasProblema,
  ACBrPIXSchemasDevolucao,
  ACBrPIXSchemasPixConsultados,
  ACBrPIXSchemasCobsConsultadas,
  ACBrPIXSchemasCobsVConsultadas;

const
  ChttpTimeOutDef = 90000;

  ChttpMethodGET = 'GET';
  ChttpMethodPOST = 'POST';
  ChttpMethodPUT = 'PUT';
  ChttpMethodPATCH = 'PATCH';
  ChttpMethodDELETE = 'DELETE';

  cEndPointPix = '/pix';
  cEndPointRec = '/rec';
  cEndPointCob = '/cob';
  cEndPointCobV = '/cobv';
  cEndPointCobR = '/cobr';
  cEndPointLocRec = '/locrec';
  cEndPointSolicRec = '/solicrec';
  cEndPointWebhook = '/webhook';
  cEndPointWebhookRec = '/webhookrec';
  cEndPointWebhookCobR = '/webhookcobr';

  CContentTypeUTF8 = 'charset=utf-8';
  CContentTypeTextPlain = 'text/plain';
  CContentTypeApplicationPDF = 'application/pdf';
  CContentTypeApplicationJSon = 'application/json';
  CContentTypeApplicationWwwFormUrlEncoded = 'application/x-www-form-urlencoded';

  ChttpHeaderAuthorization = 'Authorization:';
  ChttpHeaderContentType = 'Content-Type:';
  ChttpHeaderContentEncoding = 'Content-Encoding:';
  ChhtpHeaderAcceptEncoding = 'Accept-Encoding:';

  ChttpAuthorizationBearer = 'Bearer';
  ChttpAuthorizationBasic = 'Basic';
  ChttpContentEncodingCompress: array[0..2] of String = ('gzip', 'compress', 'deflate');

  HTTP_CONTINUE                     = 100;
  HTTP_SWITCHING_PROTOCOLS          = 101;
  HTTP_PROCESSING                   = 102;
  HTTP_OK                           = 200;
  HTTP_CREATED                      = 201;
  HTTP_ACCEPTED                     = 202;
  HTTP_NON_AUTHORITATIVE            = 203;
  HTTP_NO_CONTENT                   = 204;
  HTTP_RESET_CONTENT                = 205;
  HTTP_PARTIAL_CONTENT              = 206;
  HTTP_MULTI_STATUS                 = 207;
  HTTP_MULTIPLE_CHOICES             = 300;
  HTTP_MOVED_PERMANENTLY            = 301;
  HTTP_MOVED_TEMPORARILY            = 302;
  HTTP_SEE_OTHER                    = 303;
  HTTP_NOT_MODIFIED                 = 304;
  HTTP_USE_PROXY                    = 305;
  HTTP_TEMPORARY_REDIRECT           = 307;
  HTTP_BAD_REQUEST                  = 400;
  HTTP_UNAUTHORIZED                 = 401;
  HTTP_PAYMENT_REQUIRED             = 402;
  HTTP_FORBIDDEN                    = 403;
  HTTP_NOT_FOUND                    = 404;
  HTTP_METHOD_NOT_ALLOWED           = 405;
  HTTP_NOT_ACCEPTABLE               = 406;
  HTTP_PROXY_AUTHENTICATION_REQUIRED= 407;
  HTTP_REQUEST_TIME_OUT             = 408;
  HTTP_CONFLICT                     = 409;
  HTTP_GONE                         = 410;
  HTTP_LENGTH_REQUIRED              = 411;
  HTTP_PRECONDITION_FAILED          = 412;
  HTTP_REQUEST_ENTITY_TOO_LARGE     = 413;
  HTTP_REQUEST_URI_TOO_LARGE        = 414;
  HTTP_UNSUPPORTED_MEDIA_TYPE       = 415;
  HTTP_RANGE_NOT_SATISFIABLE        = 416;
  HTTP_EXPECTATION_FAILED           = 417;
  HTTP_UNPROCESSABLE_ENTITY         = 422;
  HTTP_LOCKED                       = 423;
  HTTP_FAILED_DEPENDENCY            = 424;
  HTTP_INTERNAL_SERVER_ERROR        = 500;
  HTTP_NOT_IMPLEMENTED              = 501;
  HTTP_BAD_GATEWAY                  = 502;
  HTTP_SERVICE_UNAVAILABLE          = 503;
  HTTP_GATEWAY_TIME_OUT             = 504;
  HTTP_VERSION_NOT_SUPPORTED        = 505;
  HTTP_VARIANT_ALSO_VARIES          = 506;
  HTTP_INSUFFICIENT_STORAGE         = 507;
  HTTP_NOT_EXTENDED                 = 510;

resourcestring
  sErroMetodoNaoImplementado = 'Método %s não implementado na Classe %s';
  sErroParametroInvalido = 'Parâmetros %s inválido ou não informado';
  sErroObjetoNaoPrenchido = 'Objeto %s não preenchido';
  sErroRecebedorNome = 'Nome do Recebedor não informado';
  sErroRecebedorCidade = 'Cidade do Recebedor não informada';
  sErroPSPNaoAtribuido = 'Componente ACBrPSP não atribuido';
  sErroPSPChavePIX = 'Chave Pix não informada';
  sErroPSPTipoChave = 'Chave Pix inválida';
  sErroHttp = 'Erro HTTP: %d, Metodo: %s, URL: %s';
  sErroAutenticacao = 'Erro de Autenticação';
  sErroPropriedadeNaoDefinida = 'Propriedade %s não atribuida';
  sErroCertificadoAmbiente = 'Rotina disponível apenas em ambiente de produção';
  sErroEndpointNaoImplementado = 'Endpoint não implementado pelo PSP';

type

  TACBrPixCD = class;
  TACBrPSP = class;

  EACBrPixHttpException = class(EACBrPixException);
  EACBrPSPException = class(EACBrPixException);

  TACBrPixCDAmbiente = (ambTeste, ambProducao, ambPreProducao);

  TACBrPSPScope =
    (scCobWrite, scCobRead, scCobVWrite, scCobVRead, scLoteCobVWrite,
     scLoteCobVRead, scPixWrite, scPixRead, scWebhookWrite, scWebhookRead,
     scPayloadLocationWrite, scPayloadLocationRead, scRecWrite, scRecRead,
     scSolicRecWrite, scSolicRecRead, scCobRWrite, scCobRRead, scWebhookRecWrite,
     scWebhookRecRead, scWebhookCobRWrite, scWebhookCobRRead,
     scPayloadLocationRecWrite, scPayloadLocationRecRead);

  TACBrPSPScopes = set of TACBrPSPScope;

  { TACBrPixEndPoint - Classe com comandos básicos, para EndPoints}

  TACBrPixEndPoint = class
  private
    fPSP: TACBrPSP;
    fHTTP: THTTPSend;
    fProblema: TACBrPIXProblema;
    function GetNivelLog: Byte;
  protected
    fpEndPoint: String;
    procedure RegistrarLog(const ALinha: String; aNivelMin: Integer = 1);
    property NivelLog: Byte read GetNivelLog;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    property Problema: TACBrPIXProblema read fProblema;
    property EndPoint: String read fpEndPoint;
  end;

  { TACBrPixEndPointPix - EndPoint /pix }

  TACBrPixEndPointPix = class(TACBrPixEndPoint)
  private
    fDevolucao: TACBrPIXDevolucao;
    fDevolucaoSolicitada: TACBrPIXDevolucaoSolicitada;
    fPix: TACBrPIX;
    fPixConsultados: TACBrPIXConsultados;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    function ConsultarPix(const e2eid: String): Boolean;
    function ConsultarPixRecebidos(Inicio: TDateTime; Fim: TDateTime;
      const TxId: String = ''; const CpfCnpj: String = '';
      PagAtual: Integer = 0; ItensPorPagina: Integer = 100): Boolean;
    function SolicitarDevolucaoPix(const e2eid, idDevolucao: String): Boolean;
    function ConsultarDevolucaoPix(const e2eid, idDevolucao: String): Boolean;

    property PixConsultados: TACBrPIXConsultados read fPixConsultados;
    property Pix: TACBrPIX read fPix;
    property Devolucao: TACBrPIXDevolucao read fDevolucao;
    property DevolucaoSolicitada: TACBrPIXDevolucaoSolicitada read fDevolucaoSolicitada;
  end;

  { TACBrPixEndPointCob - EndPoint /cob }

  TACBrPixEndPointCob = class(TACBrPixEndPoint)
  private
    fCobCompleta: TACBrPIXCobCompleta;
    fCobRevisada: TACBrPIXCobRevisada;
    fCobsConsultadas: TACBrPIXCobsConsultadas;
    fCobSolicitada: TACBrPIXCobSolicitada;
    fCobGerada: TACBrPIXCobGerada;
    procedure VerificarCopiaECola;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    function CriarCobrancaImediata(const TxId: String = ''): Boolean;
    function RevisarCobrancaImediata(const TxId: String): Boolean;
    function ConsultarCobrancaImediata(const TxId: String; Revisao: Integer = 0) : Boolean;
    function ConsultarCobrancas(Inicio: TDateTime; Fim: TDateTime;
      const CpfCnpj: String = ''; LocationPresente: Boolean = False;
      AStatus: TACBrPIXStatusCobranca = stcNENHUM;
      PagAtual: Integer = 0; ItensPorPagina: Integer = 100): Boolean;

    property CobsConsultadas: TACBrPIXCobsConsultadas read fCobsConsultadas;
    property CobSolicitada: TACBrPIXCobSolicitada read fCobSolicitada;
    property CobGerada: TACBrPIXCobGerada read fCobGerada;
    property CobRevisada: TACBrPIXCobRevisada read fCobRevisada;
    property CobCompleta: TACBrPIXCobCompleta read fCobCompleta;
  end;

  { TACBrPixEndPointCobV - EndPoint /cobv }

  TACBrPixEndPointCobV = class(TACBrPixEndPoint)
  private
    fCobVCompleta: TACBrPIXCobVCompleta;
    fCobVRevisada: TACBrPIXCobVRevisada;
    fCobsVConsultadas: TACBrPIXCobsVConsultadas;
    fCobVSolicitada: TACBrPIXCobVSolicitada;
    fCobVGerada: TACBrPIXCobVGerada;
    procedure VerificarCopiaECola;
  public
    constructor Create(aOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    function CriarCobranca(const TxId: String): Boolean;
    function RevisarCobranca(const TxId: String): Boolean;
    function ConsultarCobranca(const TxId: String; Revisao: Integer = 0) : Boolean;
    function ConsultarCobrancas(aInicio: TDateTime; aFim: TDateTime;
      const aCpfCnpj: String = ''; aLocationPresente: Boolean = False;
      aStatus: TACBrPIXStatusCobranca = stcNENHUM; aPagAtual: Integer = 0;
      aItensPorPagina: Integer = 100): Boolean;

    property CobsVConsultadas: TACBrPIXCobsVConsultadas read fCobsVConsultadas;
    property CobVSolicitada: TACBrPIXCobVSolicitada read fCobVSolicitada;
    property CobVGerada: TACBrPIXCobVGerada read fCobVGerada;
    property CobVRevisada: TACBrPIXCobVRevisada read fCobVRevisada;
    property CobVCompleta: TACBrPIXCobVCompleta read fCobVCompleta;
  end;

  { TACBrPixEndPointRec - EndPoint /rec }

  TACBrPixEndPointRec = class(TACBrPixEndPoint)
  private
    fRecorrenciaGerada: TACBrPIXRecGerada;
    fRecorrenciaCompleta: TACBrPIXRecCompleta;
    fRecorrenciaRevisada: TACBrPIXRecRevisada;
    fRecorrenciaSolicitada: TACBrPIXRecSolicitada;
    fRecorrenciasConsultadas: TACBrPIXRecsConsultadas;
    function GetRecorrenciaGerada: TACBrPIXRecGerada;
    function GetRecorrenciaCompleta: TACBrPIXRecCompleta;
    function GetRecorrenciaRevisada: TACBrPIXRecRevisada;
    function GetRecorrenciaSolicitada: TACBrPIXRecSolicitada;
    function GetRecorrenciasConsultadas: TACBrPIXRecsConsultadas;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    function CriarRecorrencia: Boolean;
    function RevisarRecorrencia(const aIdRec: String): Boolean;
    function ConsultarRecorrencia(aIdRec: String): Boolean;
    function ConsultarRecorrencias(aInicio: TDateTime; aFim: TDateTime;
      const aCpfCnpj: String = ''; aLocationPresente: Boolean = False;
      aStatus: TACBrPIXStatusRecorrencia = strNENHUM; aConvenio: String = '';
      aPagAtual: Integer = 0; aItensPorPagina: Integer = 100): Boolean;

    property RecorrenciaGerada: TACBrPIXRecGerada read GetRecorrenciaGerada;
    property RecorrenciaCompleta: TACBrPIXRecCompleta read GetRecorrenciaCompleta;
    property RecorrenciaRevisada: TACBrPIXRecRevisada read GetRecorrenciaRevisada;
    property RecorrenciaSolicitada: TACBrPIXRecSolicitada read GetRecorrenciaSolicitada;
    property RecorrenciasConsultadas: TACBrPIXRecsConsultadas read GetRecorrenciasConsultadas;
  end;

  { TACBrPixEndPointSolicRec - EndPoint /solicrec }

  TACBrPixEndPointSolicRec = class(TACBrPixEndPoint)
  private
    fSolicitacaoCompleta: TACBrPIXSolicitacaoRecCompleta;
    fSolicitacaoGerada: TACBrPIXSolicitacaoRecCompleta;
    fSolicitacaoRevisada: TACBrPIXSolicitacaoRecRevisada;
    fSolicitacaoSolicitada: TACBrPIXSolicitacaoRecSolicitada;
    function GetSolicitacaoCompleta: TACBrPIXSolicitacaoRecCompleta;
    function GetSolicitacaoGerada: TACBrPIXSolicitacaoRecCompleta;
    function GetSolicitacaoRevisada: TACBrPIXSolicitacaoRecRevisada;
    function GetSolicitacaoSolicitada: TACBrPIXSolicitacaoRecSolicitada;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    function CriarSolicitacaoConfirmacao: Boolean;
    function RevisarSolicitacao(const aIdSolicRec: String): Boolean;
    function ConsultarSolicitacao(aIdSolicRec: String): Boolean;

    property SolicitacaoGerada: TACBrPIXSolicitacaoRecCompleta read GetSolicitacaoGerada;
    property SolicitacaoCompleta: TACBrPIXSolicitacaoRecCompleta read GetSolicitacaoCompleta;
    property SolicitacaoRevisada: TACBrPIXSolicitacaoRecRevisada read GetSolicitacaoRevisada;
    property SolicitacaoSolicitada: TACBrPIXSolicitacaoRecSolicitada read GetSolicitacaoSolicitada;
  end;

  { TACBrPixEndPointLocRec - EndPoint /locrec }

  TACBrPixEndPointLocRec = class(TACBrPixEndPoint)
  private  
    fLocationCompleta: TACBrPIXRecLocation;
    fLocationsConsultadas: TACBrPIXRecLocConsultadas;
    fLocationGerada: TACBrPIXRecLocationGerada;
    function GetLocationCompleta: TACBrPIXRecLocation;
    function GetLocationsConsultadas: TACBrPIXRecLocConsultadas;
    function GetLocationGerada: TACBrPIXRecLocationGerada;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    function CriarLocation: Boolean;
    function DesvincularLocation(aId: Integer): Boolean;
    function ConsultarLocation(const aId: Integer): Boolean;
    function ConsultarLocations(const aInicio: TDateTime; const aFim: TDateTime;
      const idRecPresente: Boolean = True; const aConvenio: String = '';
      const aPagAtual: Integer = 0; const aItensPorPagina: Integer = 100): Boolean;

    property LocationGerada: TACBrPIXRecLocationGerada read GetLocationGerada;
    property LocationCompleta: TACBrPIXRecLocation read GetLocationCompleta;
    property LocationsConsultadas: TACBrPIXRecLocConsultadas read GetLocationsConsultadas;
  end;

  { TACBrPixEndPointCobR - EndPoint /cobr }

  TACBrPixEndPointCobR = class(TACBrPixEndPoint)
  private
    fCobRCompleta: TACBrPIXCobRCompleta;
    fCobRRevisada: TACBrPIXCobRRevisada;
    fCobsRConsultadas: TACBrPIXCobsRConsultadas;
    fCobRSolicitada: TACBrPIXCobRSolicitada;
    fCobRGerada: TACBrPIXCobRGerada;
    function GetCobRCompleta: TACBrPIXCobRCompleta;
    function GetCobRGerada: TACBrPIXCobRGerada;
    function GetCobRRevisada: TACBrPIXCobRRevisada;
    function GetCobRSolicitada: TACBrPIXCobRSolicitada;
    function GetCobsRConsultadas: TACBrPIXCobsRConsultadas;
  public
    constructor Create(aOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    function CriarCobranca(const TxId: String = ''): Boolean;
    function RevisarCobranca(const TxId: String): Boolean;
    function ConsultarCobranca(const TxId: String): Boolean;
    function SolicitarRetentativa(const TxId: String;
      const aDataLiquidacao: TDateTime): Boolean;
    function ConsultarCobrancas(aInicio: TDateTime; aFim: TDateTime;
      const idRec: String = ''; const aCpfCnpj: String = '';
      const aStatus: TACBrPIXStatusRegistroCobranca = srcNENHUM;
      const aConvenio: String = ''; const aPagAtual: Integer = 0;
      const aItensPorPagina: Integer = 100): Boolean;

    property CobsRConsultadas: TACBrPIXCobsRConsultadas read GetCobsRConsultadas;
    property CobRSolicitada: TACBrPIXCobRSolicitada read GetCobRSolicitada;
    property CobRGerada: TACBrPIXCobRGerada read GetCobRGerada;
    property CobRRevisada: TACBrPIXCobRRevisada read GetCobRRevisada;
    property CobRCompleta: TACBrPIXCobRCompleta read GetCobRCompleta;
  end;

  { TACBrPixEndPointWebhook - EndPoint /webhook }

  TACBrPixEndPointWebhook = class(TACBrPixEndPoint)
  private
    fWebhookSolicitado: TACBrPIXWebhookRequest;
    fWebhookConsultado: TACBrPIXWebhookCobResponse;
    fWebhooksConsultados: TACBrPIXWebhookConsultados;
    function GetWebhookConsultado: TACBrPIXWebhookCobResponse;
    function GetWebhookSolicitado: TACBrPIXWebhookRequest;
    function GetWebhooksConsultados: TACBrPIXWebhookConsultados;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;
    
    function CancelarWebhook(aChave: String): Boolean;
    function ConfigurarWebhook(aChave: String): Boolean;

    function ConsultarWebhook(aChave: String): Boolean;
    function ConsultarWebhooks(aInicio: TDateTime; aFim: TDateTime;
      const aPagAtual: Integer = 0; const aItensPorPagina: Integer = 100): Boolean;
                                                                            
    property WebhookSolicitado: TACBrPIXWebhookRequest read GetWebhookSolicitado;
    property WebhookConsultado: TACBrPIXWebhookCobResponse read GetWebhookConsultado;
    property WebhooksConsultados: TACBrPIXWebhookConsultados read GetWebhooksConsultados;
  end;

  { TACBrPixEndPointWebhookRec - EndPoint /webhookrec }

  TACBrPixEndPointWebhookRec = class(TACBrPixEndPoint)
  private
    fWebhookRecSolicitado: TACBrPIXWebhookRequest;
    fWebhookRecConsultado: TACBrPIXWebhookResponse;
    function GetWebhookRecConsultado: TACBrPIXWebhookResponse;
    function GetWebhookRecSolicitado: TACBrPIXWebhookRequest;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    function CancelarWebhookRec: Boolean;
    function ConsultarWebhookRec: Boolean;
    function ConfigurarWebhookRec: Boolean;

    property WebhookRecSolicitado: TACBrPIXWebhookRequest read GetWebhookRecSolicitado;
    property WebhookRecConsultado: TACBrPIXWebhookResponse read GetWebhookRecConsultado;
  end;

  { TACBrPixEndPointWebhookCobR - EndPoint /webhookcobr }

  TACBrPixEndPointWebhookCobR = class(TACBrPixEndPoint)
  private
    fWebhookCobRSolicitado: TACBrPIXWebhookRequest;
    fWebhookCobRConsultado: TACBrPIXWebhookResponse;
    function GetWebhookCobRConsultado: TACBrPIXWebhookResponse;
    function GetWebhookCobRSolicitado: TACBrPIXWebhookRequest;
  public
    constructor Create(AOwner: TACBrPSP);
    destructor Destroy; override;
    procedure Clear;

    function CancelarWebhookCobR: Boolean;
    function ConsultarWebhookCobR: Boolean;
    function ConfigurarWebhookCobR: Boolean;

    property WebhookCobRSolicitado: TACBrPIXWebhookRequest read GetWebhookCobRSolicitado;
    property WebhookCobRConsultado: TACBrPIXWebhookResponse read GetWebhookCobRConsultado;
  end;

  TACBrQuandoAcessarEndPoint = procedure(const AEndPoint: String;
    var AURL: String; var AMethod: String) of object;

  TACBrQuandoReceberRespostaEndPoint = procedure(const AEndPoint, URL, AMethod: String;
    var AResultCode: Integer; var RespostaHttp: AnsiString) of object;

  TACBrQuandoTransmitirHttp = procedure(var AURL: String; var AMethod: String;
    ReqHeaders: TStrings; ReqBody: AnsiString) of object;

  TACBrQuandoReceberRespostaHttp = procedure(const AURL: String; const AMethod: String;
    RespHeaders: TStrings; var AResultCode: Integer; var RespostaHttp: AnsiString) of object;

  TACBrQuandoNecessitarCredencial = procedure(const TipoCredencial: TACBrOpenSSLCredential;
    var Resposta: AnsiString) of object;

  TACBrOnAntesAutenticar = procedure(var aToken: String; var aValidadeToken: TDateTime) of object;

  TACBrOnDepoisAutenticar = procedure(const aToken: String; const aValidadeToken: TDateTime) of object;

  { TACBrQueryParams }

  TACBrQueryParams = class(TStringList)
  private
    function GetAsURL: String;
    procedure SetAsURL(const AValue: String);
  public
    property AsURL: String read GetAsURL write SetAsURL;
  end;


  { TACBrPSP - O Componente Base, para os PSPs, deve ser conectado em TACBrPixCD}

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSP = class(TACBrComponent)
  private
    fAPIVersion: TACBrPIXAPIVersion;
    fChavePIX: String;
    fepLocRec: TACBrPixEndPointLocRec;
    fScopes: TACBrPSPScopes;
    fURLPathParams: TStringList;
    fURLQueryParams: TACBrQueryParams;
    fQuandoReceberRespostaHttp: TACBrQuandoReceberRespostaHttp;
    fQuandoTransmitirHttp: TACBrQuandoTransmitirHttp;
    fClientID: AnsiString;
    fClientSecret: AnsiString;
    fk1, fk2: String;
    fTipoChave: TACBrPIXTipoChave;

    fepPix: TACBrPixEndPointPix;
    fepRec: TACBrPixEndPointRec;
    fepCob: TACBrPixEndPointCob;
    fepCobV: TACBrPixEndPointCobV;
    fepCobR: TACBrPixEndPointCobR;
    fepSolicRec: TACBrPixEndPointSolicRec;
    fepWebhook: TACBrPixEndPointWebhook;
    fepWebhookRec: TACBrPixEndPointWebhookRec;
    fepWebhookCobR: TACBrPixEndPointWebhookCobR;
    fPixCD: TACBrPixCD;
    fHttpSend: THTTPSend;
    fHttpRespStream: TMemoryStream;

    function GetClientID: String;
    function GetNivelLog: Byte;
    procedure SetClientID(AValue: String);
    function GetClientSecret: String;
    procedure SetClientSecret(AValue: String);
    procedure SetTipoChave(AValue: TACBrPIXTipoChave);
    procedure SetChavePIX(AValue: String);
    procedure SetACBrPixCD(AValue: TACBrPixCD);
  protected
    fpIsBacen: Boolean;
    fpAutenticado: Boolean;
    fpAutenticouManual:Boolean;
    fpToken: String;
    fpRefreshToken: String;
    fpValidadeToken: TDateTime;
    fpQuandoAcessarEndPoint: TACBrQuandoAcessarEndPoint;
    fpQuandoReceberRespostaEndPoint: TACBrQuandoReceberRespostaEndPoint;
    fpOnAntesAutenticar: TACBrOnAntesAutenticar;
    fpOnDepoisAutenticar: TACBrOnDepoisAutenticar;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure VerificarPIXCDAtribuido;

    procedure DispararExcecao(E: Exception);
    procedure RegistrarLog(const ALinha: String; aNivelLog: Integer = 1);
    property NivelLog: Byte read GetNivelLog;

    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; virtual;
    procedure ConfigurarHTTP; virtual;
    procedure ConfigurarProxy; virtual;
    procedure ConfigurarTimeOut; virtual;
    procedure ConfigurarHeaders(const Method, AURL: String); virtual;
    procedure ConfigurarAutenticacao(const Method, EndPoint: String); virtual;
    procedure ConfigurarPathParameters(const Method, EndPoint: String); virtual;
    procedure ConfigurarQueryParameters(const Method, EndPoint: String); virtual;
    procedure ConfigurarBody(const aMethod, aEndPoint: String; var aBody: String); virtual;

    procedure LimparHTTP; virtual;
    procedure PrepararHTTP; virtual;
    function AcessarEndPoint(const Method, EndPoint: String;
      out ResultCode: Integer; out RespostaHttp: AnsiString): Boolean; virtual;
    function CalcularURLEndPoint(const Method, EndPoint: String): String; virtual;
    function CalcularEndPointPath(const Method, EndPoint: String): String; virtual;
    function EfetuarAutenticacaoManual: Boolean;
    function ScopeToString(aScope: TACBrPSPScope): String; virtual;
    function ScopesToString(aScopes: TACBrPSPScopes): String;

    procedure ChamarEventoQuandoAcessarEndPoint(const AEndPoint: String;
      var AURL: String; var AMethod: String);
    procedure ChamarEventoQuandoReceberRespostaEndPoint( const AEndPoint, AURL,
      AMethod: String; var ResultCode: Integer; var RespostaHttp: AnsiString);

    function TransmitirHttp(const AMethod, AURL: String; out ResultCode: Integer; out
      RespostaHttp: AnsiString): Boolean; virtual;
    procedure ChamarEventoQuandoTransmitirHttp(var AURL: String; var AMethod: String);
    procedure ChamarEventoQuandoReceberRespostaHttp(const AURL: String;
      const AMethod: String; var ResultCode: Integer; var RespostaHttp: AnsiString);
    procedure TratarRetornoComErro(ResultCode: Integer; const RespostaHttp: AnsiString;
      Problema: TACBrPIXProblema); virtual;
    procedure AtribuirErroHTTPProblema(Problema: TACBrPIXProblema); virtual;

    property URLQueryParams: TACBrQueryParams read fURLQueryParams;
    property URLPathParams: TStringList read fURLPathParams;
  protected
    property APIVersion: TACBrPIXAPIVersion read fAPIVersion write fAPIVersion default ver262;

    property ClientID: String read GetClientID write SetClientID;
    property ClientSecret: String read GetClientSecret write SetClientSecret;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; virtual;

    procedure Autenticar; virtual;
    procedure VerificarValidadeToken; virtual;
    procedure RenovarToken; virtual;
    procedure VerificarAutenticacao; virtual;
    property Autenticado: Boolean read fpAutenticado;
    property ValidadeToken: TDateTime read fpValidadeToken;

    property epPix: TACBrPixEndPointPix read fepPix;
    property epRec: TACBrPixEndPointRec read fepRec;
    property epCob: TACBrPixEndPointCob read fepCob;
    property epCobV: TACBrPixEndPointCobV read fepCobV;
    property epCobR: TACBrPixEndPointCobR read fepCobR;
    property epLocRec: TACBrPixEndPointLocRec read fepLocRec;
    property epSolicRec: TACBrPixEndPointSolicRec read fepSolicRec;
    property epWebhook: TACBrPixEndPointWebhook read fepWebhook;
    property epWebhookRec: TACBrPixEndPointWebhookRec read fepWebhookRec;
    property epWebhookCobR: TACBrPixEndPointWebhookCobR read fepWebhookCobR;

    property Http: THTTPSend read fHttpSend;
    property IsBacen: Boolean read fpIsBacen;
  published
    property ACBrPixCD: TACBrPixCD read fPixCD write SetACBrPixCD;

    property ChavePIX: String read fChavePIX write SetChavePIX;
    property TipoChave: TACBrPIXTipoChave read fTipoChave write SetTipoChave stored false;
    property Scopes: TACBrPSPScopes read fScopes write fScopes;

    property QuandoTransmitirHttp: TACBrQuandoTransmitirHttp read fQuandoTransmitirHttp write fQuandoTransmitirHttp;
    property QuandoReceberRespostaHttp: TACBrQuandoReceberRespostaHttp read fQuandoReceberRespostaHttp write fQuandoReceberRespostaHttp;
    property OnAntesAutenticar: TACBrOnAntesAutenticar read fpOnAntesAutenticar write fpOnAntesAutenticar;
    property OnDepoisAutenticar: TACBrOnDepoisAutenticar read fpOnDepoisAutenticar write fpOnDepoisAutenticar;
  end;

  { TACBrPSPCertificate }

  TACBrPSPCertificate = class(TACBrPSP)
  private
    fK: String;
    fArquivoCertificado: String;
    fArquivoChavePrivada: String;
    fArquivoPFX: String;
    fSenhaPFX: AnsiString;
    fCertificado: AnsiString;
    fChavePrivada: AnsiString;
    fPFX: AnsiString;
                                                
    function GetSenhaPFX: AnsiString;
    procedure SetArquivoCertificado(aValue: String);
    procedure SetArquivoChavePrivada(aValue: String);
    procedure SetArquivoPFX(const aValue: String);
    procedure SetCertificado(aValue: AnsiString);
    procedure SetChavePrivada(aValue: AnsiString);
    procedure SetPFX(aValue: AnsiString);
    procedure SetSenhaPFX(const aValue: AnsiString);

  protected
    procedure ConfigurarHeaders(const Method, AURL: String); override;

    function VerificarSeIncluiPFX(const Method, AURL: String): Boolean;  virtual;
    function VerificarSeIncluiCertificado(const Method, AURL: String): Boolean; virtual;
    function VerificarSeIncluiChavePrivada(const Method, AURL: String): Boolean;  virtual;
  published
    property ClientID;
    property ClientSecret;
  public
    constructor Create(AOwner: TComponent); override;

    property ArquivoCertificado: String read fArquivoCertificado write SetArquivoCertificado;
    property ArquivoChavePrivada: String read fArquivoChavePrivada write SetArquivoChavePrivada;
    property ArquivoPFX: String read fArquivoPFX write SetArquivoPFX;

    property Certificado: AnsiString read fCertificado write SetCertificado;
    property ChavePrivada: AnsiString read fChavePrivada write SetChavePrivada;
    property PFX: AnsiString read fPFX write SetPFX;
    property SenhaPFX: AnsiString read GetSenhaPFX write SetSenhaPFX;
  end;

  { TACBrPixRecebedor }

  TACBrPixRecebedor = class(TPersistent)
  private
    fCEP: String;
    fCidade: String;
    fCodCategoriaComerciante: Integer;
    fNome: String;
    fUF: String;

    procedure SetCEP(AValue: String);
    procedure SetCidade(AValue: String);
    procedure SetCodCategoriaComerciante(AValue: Integer);
    procedure SetNome(AValue: String);
    procedure SetUF(AValue: String);
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPixRecebedor); reintroduce;
  published
    property Nome: String read fNome write SetNome;
    property Cidade: String read fCidade write SetCidade;
    property UF: String read fUF write SetUF;
    property CEP: String read fCEP write SetCEP;
    property CodCategoriaComerciante: Integer read fCodCategoriaComerciante  // https://classification.codes/classifications/industry/mcc/
      write SetCodCategoriaComerciante;
  end;

  { TACBrPixDadosAutomacao }

  TACBrPixDadosAutomacao = class(TPersistent)
  private
    fCNPJSoftwareHouse: String;
    fNomeAplicacao: String;
    fNomeSoftwareHouse: String;
    fVersaoAplicacao: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPixDadosAutomacao); reintroduce;
  published
    property NomeSoftwareHouse: String read fNomeSoftwareHouse write fNomeSoftwareHouse;
    property CNPJSoftwareHouse: String read fCNPJSoftwareHouse write fCNPJSoftwareHouse;
    property NomeAplicacao: String read fNomeAplicacao write fNomeAplicacao ;
    property VersaoAplicacao: String read fVersaoAplicacao write fVersaoAplicacao ;
  end;


  { TACBrHttpProxy }

  TACBrHttpProxy = class(TPersistent)
  private
    fHost: String;
    fPass: String;
    fPort: String;
    fUser: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrHttpProxy); reintroduce;
  published
    property Host: String read fHost write fHost;
    property Port: String read fPort write fPort;
    property User: String read fUser write fUser;
    property Pass: String read fPass write fPass;
  end;

  { TACBrPixCD - O Componente em si...}

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPixCD = class(TACBrComponent)
  private
    fAmbiente: TACBrPixCDAmbiente;
    fArqLOG: String;
    fDadosAutomacao: TACBrPixDadosAutomacao;
    fNivelLog: Byte;
    fProxy: TACBrHttpProxy;
    fPSP: TACBrPSP;
    fQuandoGravarLog: TACBrGravarLog;
    fRecebedor: TACBrPixRecebedor;
    fTimeOut: Integer;

    procedure SetACBrPSP(AValue: TACBrPSP);
    procedure SetDadosAutomacao(AValue: TACBrPixDadosAutomacao);
    procedure SetProxy(AValue: TACBrHttpProxy);
    procedure SetRecebedor(AValue: TACBrPixRecebedor);

    procedure VerificarPSPAtribuido;
    procedure GravarLogEmArquivo(const ALinha: String) ;
    procedure DispararExcecao(E: Exception);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegistrarLog(const ALinha: String; aNivelLog: Integer = 1);

    function GerarQRCodeEstatico(Valor: Currency; const infoAdicional: String = '';
      const TxId: String = ''): String; overload;
    function GerarQRCodeEstatico(const ChavePix: String; Valor: Currency;
      const infoAdicional: String = ''; const TxId: String = ''): String; overload;
    function GerarQRCodeDinamico(const Location: String; const TxID: String = '';
      const Valor: Currency = 0): String;

  published
    property Recebedor: TACBrPixRecebedor read fRecebedor write SetRecebedor;
    property DadosAutomacao: TACBrPixDadosAutomacao read fDadosAutomacao write SetDadosAutomacao;
    property Proxy: TACBrHttpProxy read fProxy write SetProxy;
    property TimeOut: Integer read fTimeOut write fTimeOut default ChttpTimeOutDef;
    property Ambiente: TACBrPixCDAmbiente read fAmbiente write fAmbiente default ambTeste;

    property PSP: TACBrPSP read fPSP write SetACBrPSP;

    property ArqLOG: String read fArqLOG write fArqLOG;
    property NivelLog: Byte read fNivelLog write fNivelLog default 1;
    property QuandoGravarLog: TACBrGravarLog read fQuandoGravarLog write fQuandoGravarLog;
  end;

function StreamToAnsiString(AStream: TStream): AnsiString;
function URLComDelimitador(AURL: String): String;
function URLSemDelimitador(AURL: String): String;
function GetHeaderValue(const AValue : String; AStringList: TStringList) : String ;
function ContentIsCompressed(AHeader: TStringList): Boolean;
function DecompressStream(AStream: TStream): AnsiString;

implementation

uses
  StrUtils,
  synacode, synautil,
  ACBrUtil.FilesIO,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.Base,
  ACBrCompress, ACBrValidador,
  ACBrPIXUtil;

function StreamToAnsiString(AStream: TStream): AnsiString;
begin
  AStream.Position := 0;
  Result := ReadStrFromStream(AStream, AStream.Size);
end;

function URLComDelimitador(AURL: String): String;
begin
  Result := Trim(AURL);
  if (RightStr(Result, 1) <> '/') then
    Result := Result + '/'
end;

function URLSemDelimitador(AURL: String): String;
begin
  Result := Trim(AURL);
  while (Result <> '') and (RightStr(Result, 1) = '/') do
    Delete(Result, Length(Result), 1);
end;

function GetHeaderValue(const AValue : String; AStringList: TStringList) : String ;
var
  i: Integer;
  u, LinhaHeader: String;
begin
  Result := '';
  u := UpperCase(Trim(AValue));
  if (u = '') then
    Exit;

  i := 0;
  while (Result = '') and (i < AStringList.Count) do
  begin
    LinhaHeader := AStringList[i];
    if (pos(u, UpperCase(LinhaHeader)) = 1) then
      Result := Trim(copy(LinhaHeader, Length(u)+1, Length(LinhaHeader)));
    Inc(i);
  end;
end;

function ContentIsCompressed(AHeader: TStringList): Boolean;
var
  ce: String;
  i: Integer;
begin
  ce := GetHeaderValue(ChttpHeaderContentEncoding, AHeader);
  i := Low(ChttpContentEncodingCompress);
  Result := False;
  while (not Result) and (i <= High(ChttpContentEncodingCompress)) do
  begin
    Result := (pos(ChttpContentEncodingCompress[i], ce) > 0);
    Inc(i);
  end;
end;

function DecompressStream(AStream: TStream): AnsiString;
var
  zt: TCompressType;
begin
  zt := DetectCompressType(AStream);
  if (zt = ctUnknown) then  // Not compressed...
  begin
    AStream.Position := 0;
    Result := ReadStrFromStream(AStream, AStream.Size);
  end
  else
    Result := UnZip(AStream);
end;

{ TACBrPixEndPointCobV }

procedure TACBrPixEndPointCobV.VerificarCopiaECola;
begin
  try
    if EstaVazio(Trim(CobVGerada.pixCopiaECola)) and Assigned(fPSP.ACBrPixCD) and
       NaoEstaVazio(CobVGerada.loc.location) and
       NaoEstaVazio(fPSP.ACBrPixCD.Recebedor.Nome) and
       NaoEstaVazio(fPSP.ACBrPixCD.Recebedor.Cidade) then
      CobVGerada.pixCopiaECola := fPSP.ACBrPixCD.GerarQRCodeDinamico(CobVGerada.loc.location);
  except
  end;
end;

constructor TACBrPixEndPointCobV.Create(aOwner: TACBrPSP);
begin
  if (aOwner = nil) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));

  inherited Create(AOwner);
  fpEndPoint := cEndPointCobV;

  fCobVGerada := TACBrPIXCobVGerada.Create;
  fCobVRevisada := TACBrPIXCobVRevisada.Create;
  fCobVCompleta := TACBrPIXCobVCompleta.Create;
  fCobVSolicitada := TACBrPIXCobVSolicitada.Create;
  fCobsVConsultadas := TACBrPIXCobsVConsultadas.Create;
end;

destructor TACBrPixEndPointCobV.Destroy;
begin
  fCobVGerada.Free;
  fCobVRevisada.Free;
  fCobVCompleta.Free;
  fCobVSolicitada.Free;
  fCobsVConsultadas.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointCobV.Clear;
begin
  inherited Clear;
  fCobVGerada.Clear;
  fCobVRevisada.Clear;
  fCobVCompleta.Clear;
  fCobVSolicitada.Clear;
  fCobsVConsultadas.Clear;
end;

function TACBrPixEndPointCobV.CriarCobranca(const TxId: String): Boolean;
var
  Body, ep: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog('CriarCobrancaVencimento( '+TxId+' )');
  if EstaVazio(Trim(TxId)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['txid']);

  Body := Trim(fCobVSolicitada.AsJSON);
  if EstaVazio(Body) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['CobVSolicitada']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(TxId);
  ep := ChttpMethodPUT;

  fPSP.ConfigurarBody(ep, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ep, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_CREATED);

  if Result then
  begin
    fCobVGerada.AsJSON := String(RespostaHttp);
    VerificarCopiaECola;
  end
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCobV.RevisarCobranca(const TxId: String): Boolean;
var
  Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog('RevisarCobranca('+TxId+')');

  if EstaVazio(Trim(TxId)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['txid']);

  Body := Trim(fCobVRevisada.AsJSON);
  if EstaVazio(Body) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['CobVRevisada']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(TxId);
  fPSP.ConfigurarBody(ChttpMethodPATCH, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ChttpMethodPATCH, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    fCobVGerada.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCobV.ConsultarCobranca(const TxId: String;
  Revisao: Integer): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog('ConsultarCobrancaVencimento( '+TxId+', '+IntToStr(Revisao)+' )');
  if EstaVazio(Trim(TxId)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['txid']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(TxId);
  if (Revisao <> 0) then
    fPSP.URLQueryParams.Values['revisao'] := IntToStr(Revisao);

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    fCobVCompleta.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCobV.ConsultarCobrancas(aInicio: TDateTime;
  aFim: TDateTime; const aCpfCnpj: String; aLocationPresente: Boolean;
  aStatus: TACBrPIXStatusCobranca; aPagAtual: Integer; aItensPorPagina: Integer): Boolean;
var
  s, e: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog(Format('ConsultarCobrancas(%s, %s, %s, %s, %s, %s, %s)',
      [FormatDateTimeBr(aInicio), FormatDateTimeBr(aFim), aCpfCnpj,
       BoolToStr(aLocationPresente, True), PIXStatusCobrancaToString(aStatus),
       IntToStr(aPagAtual), IntToStr(aItensPorPagina)]));

  Clear;
  fPSP.PrepararHTTP;
  with fPSP.URLQueryParams do
  begin
    Values['inicio'] := DateTimeToIso8601(aInicio);
    Values['fim'] := DateTimeToIso8601(aFim);

    s := OnlyNumber(aCpfCnpj);
    if NaoEstaVazio(s) then
    begin
      e := ValidarCNPJouCPF(s);
      if NaoEstaVazio(e) then
        raise EACBrPixException.Create(ACBrStr(e));

      if (Length(s) < 12) then
        Values['cpf'] := s
      else
        Values['cnpj'] := s;
    end;

    Values['locationPresente'] := IfThen(aLocationPresente, 'true', 'false');
    if (aStatus <> stcNENHUM) then
      Values['status'] := PIXStatusCobrancaToString(aStatus);

    if (aPagAtual > 0) then
      Values['paginacao.paginaAtual'] := IntToStr(aPagAtual);

    if (aItensPorPagina > 0) then
      Values['paginacao.itensPorPagina'] := IntToStr(aItensPorPagina);
  end;

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    fCobsVConsultadas.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrPixEndPointRec }

function TACBrPixEndPointRec.GetRecorrenciaSolicitada: TACBrPIXRecSolicitada;
begin
  if (not Assigned(fRecorrenciaSolicitada)) then
    fRecorrenciaSolicitada := TACBrPIXRecSolicitada.Create;
  Result := fRecorrenciaSolicitada;
end;

function TACBrPixEndPointRec.GetRecorrenciaGerada: TACBrPIXRecGerada;
begin
  if (not Assigned(fRecorrenciaGerada)) then
    fRecorrenciaGerada := TACBrPIXRecGerada.Create;
  Result := fRecorrenciaGerada;
end;

function TACBrPixEndPointRec.GetRecorrenciaCompleta: TACBrPIXRecCompleta;
begin
  if (not Assigned(fRecorrenciaCompleta)) then
    fRecorrenciaCompleta := TACBrPIXRecCompleta.Create;
  Result := fRecorrenciaCompleta;
end;

function TACBrPixEndPointRec.GetRecorrenciaRevisada: TACBrPIXRecRevisada;
begin
  if (not Assigned(fRecorrenciaRevisada)) then
    fRecorrenciaRevisada := TACBrPIXRecRevisada.Create;
  Result := fRecorrenciaRevisada;
end;

function TACBrPixEndPointRec.GetRecorrenciasConsultadas: TACBrPIXRecsConsultadas;
begin
  if (not Assigned(fRecorrenciasConsultadas)) then
    fRecorrenciasConsultadas := TACBrPIXRecsConsultadas.Create;
  Result := fRecorrenciasConsultadas;
end;

constructor TACBrPixEndPointRec.Create(AOwner: TACBrPSP);
begin
  if (aOwner = nil) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));

  inherited Create(AOwner);
  fpEndPoint := cEndPointRec;
end;

destructor TACBrPixEndPointRec.Destroy;
begin
  if Assigned(fRecorrenciaSolicitada) then
    fRecorrenciaSolicitada.Free;
  if Assigned(fRecorrenciaCompleta) then
    fRecorrenciaCompleta.Free;
  if Assigned(fRecorrenciasConsultadas) then
    fRecorrenciasConsultadas.Free;
  if Assigned(fRecorrenciaGerada) then
    fRecorrenciaGerada.Free;
  if Assigned(fRecorrenciaRevisada) then
    fRecorrenciaRevisada.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointRec.Clear;
begin
  if Assigned(fRecorrenciaSolicitada) then
    fRecorrenciaSolicitada.Clear;
  if Assigned(fRecorrenciasConsultadas) then
    fRecorrenciasConsultadas.Clear;
  if Assigned(fRecorrenciaGerada) then
    fRecorrenciaGerada.Clear;
  if Assigned(fRecorrenciaCompleta) then
    fRecorrenciaCompleta.Clear;
  if Assigned(fRecorrenciaRevisada) then
    fRecorrenciaRevisada.Clear;
end;

function TACBrPixEndPointRec.CriarRecorrencia: Boolean;
var
  wBody, ep: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog('CriarRecorrencia', 2);

  if (not Assigned(fRecorrenciaSolicitada)) or fRecorrenciaSolicitada.IsEmpty then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['RecorrenciaSolicitada']);

  wBody := Trim(fRecorrenciaSolicitada.AsJSON);
  Clear;
  fPSP.PrepararHTTP;
  ep := ChttpMethodPOST;

  fPSP.ConfigurarBody(ep, EndPoint, wBody);
  WriteStrToStream(fPSP.Http.Document, wBody);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ep, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_CREATED);

  if Result then
    RecorrenciaGerada.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointRec.RevisarRecorrencia(const aIdRec: String): Boolean;
var
  wBody: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('RevisarRecorrencia(idRec: %s)', [aIdRec]), 2);
  if EstaVazio(Trim(aIdRec)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['idRec']);

  wBody := EmptyStr;
  if Assigned(fRecorrenciaRevisada) and (not fRecorrenciaRevisada.IsEmpty) then
    wBody := Trim(fRecorrenciaRevisada.AsJSON);
  if EstaVazio(wBody) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['RecorrenciaRevisada']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(aIdRec);
  fPSP.ConfigurarBody(ChttpMethodPATCH, EndPoint, wBody);
  WriteStrToStream(fPSP.Http.Document, wBody);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ChttpMethodPATCH, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    RecorrenciaGerada.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointRec.ConsultarRecorrencia(aIdRec: String): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('ConsultarRecorrencia(%s)', [aIdRec]), 2);
  if EstaVazio(Trim(aIdRec)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['idRec']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(aIdRec);
  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    RecorrenciaCompleta.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointRec.ConsultarRecorrencias(aInicio: TDateTime; aFim: TDateTime;
  const aCpfCnpj: String; aLocationPresente: Boolean; aStatus: TACBrPIXStatusRecorrencia;
  aConvenio: String; aPagAtual: Integer; aItensPorPagina: Integer): Boolean;
var
  s, e: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('ConsultarRecorrencias(%s, %s, %s, %s, %s, %s, %s, %s)',
    [FormatDateTimeBr(aInicio), FormatDateTimeBr(aFim), aCpfCnpj,
     BoolToStr(aLocationPresente, True), PIXStatusRecorrenciaToString(aStatus),
     aConvenio, IntToStr(aPagAtual), IntToStr(aItensPorPagina)]), 2);

  Clear;
  fPSP.PrepararHTTP;
  with fPSP.URLQueryParams do
  begin
    Values['inicio'] := DateTimeToIso8601(aInicio);
    Values['fim'] := DateTimeToIso8601(aFim);

    s := OnlyNumber(aCpfCnpj);
    if NaoEstaVazio(s) then
    begin
      e := ValidarCNPJouCPF(s);
      if NaoEstaVazio(e) then
        raise EACBrPixException.Create(ACBrStr(e));

      if (Length(s) < 12) then
        Values['cpf'] := s
      else
        Values['cnpj'] := s;
    end;

    Values['locationPresente'] := IfThen(aLocationPresente, 'true', 'false');
    if (aStatus <> strNENHUM) then
      Values['status'] := PIXStatusRecorrenciaToString(aStatus);

    if (aPagAtual > 0) then
      Values['paginacao.paginaAtual'] := IntToStr(aPagAtual);

    if (aItensPorPagina > 0) then
      Values['paginacao.itensPorPagina'] := IntToStr(aItensPorPagina);
  end;

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    RecorrenciasConsultadas.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrPixEndPointSolicRec }

function TACBrPixEndPointSolicRec.GetSolicitacaoCompleta: TACBrPIXSolicitacaoRecCompleta;
begin
  if (not Assigned(fSolicitacaoCompleta)) then
    fSolicitacaoCompleta := TACBrPIXSolicitacaoRecCompleta.Create;
  Result := fSolicitacaoCompleta;
end;

function TACBrPixEndPointSolicRec.GetSolicitacaoGerada: TACBrPIXSolicitacaoRecCompleta;
begin
  if (not Assigned(fSolicitacaoGerada)) then
    fSolicitacaoGerada := TACBrPIXSolicitacaoRecCompleta.Create;
  Result := fSolicitacaoGerada;
end;

function TACBrPixEndPointSolicRec.GetSolicitacaoRevisada: TACBrPIXSolicitacaoRecRevisada;
begin
  if (not Assigned(fSolicitacaoRevisada)) then
    fSolicitacaoRevisada := TACBrPIXSolicitacaoRecRevisada.Create;
  Result := fSolicitacaoRevisada;
end;

function TACBrPixEndPointSolicRec.GetSolicitacaoSolicitada: TACBrPIXSolicitacaoRecSolicitada;
begin
  if (not Assigned(fSolicitacaoSolicitada)) then
    fSolicitacaoSolicitada := TACBrPIXSolicitacaoRecSolicitada.Create;
  Result := fSolicitacaoSolicitada;
end;

constructor TACBrPixEndPointSolicRec.Create(AOwner: TACBrPSP);
begin
  if (aOwner = nil) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));

  inherited Create(AOwner);
  fpEndPoint := cEndPointSolicRec;
end;

destructor TACBrPixEndPointSolicRec.Destroy;
begin
  if Assigned(fSolicitacaoCompleta) then
    fSolicitacaoCompleta.Free;
  if Assigned(fSolicitacaoGerada) then
    fSolicitacaoGerada.Free;
  if Assigned(fSolicitacaoRevisada) then
    fSolicitacaoRevisada.Free;
  if Assigned(fSolicitacaoSolicitada) then
    fSolicitacaoSolicitada.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointSolicRec.Clear;
begin
  if Assigned(fSolicitacaoCompleta) then
    fSolicitacaoCompleta.Clear;
  if Assigned(fSolicitacaoGerada) then
    fSolicitacaoGerada.Clear;
  if Assigned(fSolicitacaoRevisada) then
    fSolicitacaoRevisada.Clear;
  if Assigned(fSolicitacaoSolicitada) then
    fSolicitacaoSolicitada.Clear;
end;

function TACBrPixEndPointSolicRec.CriarSolicitacaoConfirmacao: Boolean;
var
  wBody, ep: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  wBody := EmptyStr;
  RegistrarLog('CriarSolicitacaoConfirmacao', 2);

  if (not Assigned(fSolicitacaoSolicitada)) or fSolicitacaoSolicitada.IsEmpty then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['SolicitacaoSolicitada']);

  wBody := Trim(fSolicitacaoSolicitada.AsJSON);
  Clear;
  fPSP.PrepararHTTP;
  ep := ChttpMethodPOST;

  fPSP.ConfigurarBody(ep, EndPoint, wBody);
  WriteStrToStream(fPSP.Http.Document, wBody);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ep, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_CREATED);

  if Result then
    SolicitacaoGerada.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointSolicRec.RevisarSolicitacao(
  const aIdSolicRec: String): Boolean;
var
  wBody: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('RevisarSolicitacao(idSolicRec: %s)', [aIdSolicRec]), 2);
  if EstaVazio(Trim(aIdSolicRec)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['idSolicRec']);

  if (not Assigned(fSolicitacaoRevisada)) or fSolicitacaoRevisada.IsEmpty then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['SolicitacaoRevisada']);

  wBody := Trim(fSolicitacaoRevisada.AsJSON);
  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(aIdSolicRec);
  fPSP.ConfigurarBody(ChttpMethodPATCH, EndPoint, wBody);
  WriteStrToStream(fPSP.Http.Document, wBody);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ChttpMethodPATCH, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_CREATED);

  if Result then
    SolicitacaoGerada.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointSolicRec.ConsultarSolicitacao(aIdSolicRec: String): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('ConsultarSolicitacao(%s)', [aIdSolicRec]), 2);
  if EstaVazio(Trim(aIdSolicRec)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['idSolicRec']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(aIdSolicRec);
  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    SolicitacaoCompleta.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrPixEndPointLocRec }

function TACBrPixEndPointLocRec.GetLocationCompleta: TACBrPIXRecLocation;
begin
  if (not Assigned(fLocationCompleta)) then
    fLocationCompleta := TACBrPIXRecLocation.Create;
  Result := fLocationCompleta;
end;

function TACBrPixEndPointLocRec.GetLocationsConsultadas: TACBrPIXRecLocConsultadas;
begin
  if (not Assigned(fLocationsConsultadas)) then
    fLocationsConsultadas := TACBrPIXRecLocConsultadas.Create;
  Result := fLocationsConsultadas;
end;

function TACBrPixEndPointLocRec.GetLocationGerada: TACBrPIXRecLocationGerada;
begin
  if (not Assigned(fLocationGerada)) then
    fLocationGerada := TACBrPIXRecLocationGerada.Create;
  Result := fLocationGerada;
end;

constructor TACBrPixEndPointLocRec.Create(AOwner: TACBrPSP);
begin
  if (aOwner = nil) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));

  inherited Create(AOwner);
  fpEndPoint := cEndPointLocRec;
end;

destructor TACBrPixEndPointLocRec.Destroy;
begin
  if Assigned(fLocationGerada) then
    fLocationGerada.Free;
  if Assigned(fLocationCompleta) then
    fLocationCompleta.Free;
  if Assigned(fLocationsConsultadas) then
    fLocationsConsultadas.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointLocRec.Clear;
begin
  if Assigned(fLocationGerada) then
    fLocationGerada.Clear;
  if Assigned(fLocationCompleta) then
    fLocationCompleta.Clear;
  if Assigned(fLocationsConsultadas) then
    fLocationsConsultadas.Clear;
end;

function TACBrPixEndPointLocRec.CriarLocation: Boolean;
var
  ep: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog('CriarLocation', 2);

  Clear;
  fPSP.PrepararHTTP;
  ep := ChttpMethodPOST;
  fPSP.AcessarEndPoint(ep, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_CREATED);

  if Result then
    LocationGerada.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointLocRec.DesvincularLocation(aId: Integer): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if EstaZerado(aId) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['Id']);
    
  Clear;
  RegistrarLog(Format('DesvincularLocation(Id: %d)', [aId]), 2);
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(IntToStr(aId));
  fPSP.URLPathParams.Add('idRec');
  fPSP.AcessarEndPoint(ChttpMethodDELETE, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    LocationCompleta.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointLocRec.ConsultarLocation(const aId: Integer): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if EstaZerado(aId) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['Id']);

  Clear; 
  RegistrarLog(Format('ConsultarLocation(%d)', [aId]), 2);
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(IntToStr(aId));
  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    LocationCompleta.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointLocRec.ConsultarLocations(const aInicio: TDateTime;
  const aFim: TDateTime; const idRecPresente: Boolean; const aConvenio: String;
  const aPagAtual: Integer; const aItensPorPagina: Integer): Boolean;
var
  s, e: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('ConsultarLocations(%s, %s, %s, %s, %s, %s)',
    [FormatDateTimeBr(aInicio), FormatDateTimeBr(aFim), BoolToStr(idRecPresente),
     aConvenio, IntToStr(aPagAtual), IntToStr(aItensPorPagina)]), 2);

  Clear;
  fPSP.PrepararHTTP;
  with fPSP.URLQueryParams do
  begin
    Values['inicio'] := DateTimeToIso8601(aInicio);
    Values['fim'] := DateTimeToIso8601(aFim);
    //Values['idRecPresente'] := IfThen(idRecPresente, 'true', 'false');

    if NaoEstaVazio(aConvenio) then
        Values['convenio'] := aConvenio;

    if (aPagAtual > 0) then
      Values['paginacao.paginaAtual'] := IntToStr(aPagAtual);

    if (aItensPorPagina > 0) then
      Values['paginacao.itensPorPagina'] := IntToStr(aItensPorPagina);
  end;

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    LocationsConsultadas.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrPixEndPointCobR }

function TACBrPixEndPointCobR.GetCobRCompleta: TACBrPIXCobRCompleta;
begin
  if not Assigned(fCobRCompleta) then
    fCobRCompleta := TACBrPIXCobRCompleta.Create;
  Result := fCobRCompleta;
end;

function TACBrPixEndPointCobR.GetCobRGerada: TACBrPIXCobRGerada;
begin
  if not Assigned(fCobRGerada) then
    fCobRGerada := TACBrPIXCobRGerada.Create;
  Result := fCobRGerada;
end;

function TACBrPixEndPointCobR.GetCobRRevisada: TACBrPIXCobRRevisada;
begin
  if not Assigned(fCobRRevisada) then
    fCobRRevisada := TACBrPIXCobRRevisada.Create;
  Result := fCobRRevisada;
end;

function TACBrPixEndPointCobR.GetCobRSolicitada: TACBrPIXCobRSolicitada;
begin
  if not Assigned(fCobRSolicitada) then
    fCobRSolicitada := TACBrPIXCobRSolicitada.Create;
  Result := fCobRSolicitada;
end;

function TACBrPixEndPointCobR.GetCobsRConsultadas: TACBrPIXCobsRConsultadas;
begin
  if not Assigned(fCobsRConsultadas) then
    fCobsRConsultadas := TACBrPIXCobsRConsultadas.Create;
  Result := fCobsRConsultadas;
end;

constructor TACBrPixEndPointCobR.Create(aOwner: TACBrPSP);
begin
  if (aOwner = nil) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));

  inherited Create(AOwner);
  fpEndPoint := cEndPointCobR;
end;

destructor TACBrPixEndPointCobR.Destroy;
begin
  if Assigned(fCobRGerada) then
    fCobRGerada.Free;
  if Assigned(fCobRRevisada) then
    fCobRRevisada.Free;
  if Assigned(fCobRCompleta) then
    fCobRCompleta.Free;
  if Assigned(fCobRSolicitada) then
    fCobRSolicitada.Free;
  if Assigned(fCobsRConsultadas) then
    fCobsRConsultadas.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointCobR.Clear;
begin
  inherited Clear;
  if Assigned(fCobRGerada) then
    fCobRGerada.Clear;
  if Assigned(fCobRRevisada) then
    fCobRRevisada.Clear;
  if Assigned(fCobRCompleta) then
    fCobRCompleta.Clear;
  if Assigned(fCobRSolicitada) then
    fCobRSolicitada.Clear;
  if Assigned(fCobsRConsultadas) then
    fCobsRConsultadas.Clear;
end;

function TACBrPixEndPointCobR.CriarCobranca(const TxId: String): Boolean;
var
  Body, ep: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('CriarCobrancaRecorrente(txid: %s)', [TxId]), 2);

  if (not Assigned(fCobRSolicitada)) or fCobRSolicitada.IsEmpty then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['CobRSolicitada']);

  Body := Trim(CobRSolicitada.AsJSON);
  Clear;
  fPSP.PrepararHTTP;
  if NaoEstaVazio(TxId) then
  begin
    ep := ChttpMethodPUT;
    fPSP.URLPathParams.Add(TxId);
  end
  else
    ep := ChttpMethodPOST;

  fPSP.ConfigurarBody(ep, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ep, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_CREATED);

  if Result then
    CobRGerada.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCobR.RevisarCobranca(const TxId: String): Boolean;
var
  Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('RevisarCobrancaRecorrente(txId: %s)', [TxId]), 2);

  if EstaVazio(Trim(TxId)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['txid']);

  if (not Assigned(fCobRRevisada)) or fCobRRevisada.IsEmpty then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['CobRRevisada']);

  Body := Trim(CobRRevisada.AsJSON);
  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(TxId);
  fPSP.ConfigurarBody(ChttpMethodPATCH, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ChttpMethodPATCH, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    CobRGerada.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCobR.ConsultarCobranca(const TxId: String): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('ConsultarCobrancaRecorrente(txId: %s)', [TxId]), 2);
  if EstaVazio(Trim(TxId)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['txid']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(TxId);

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    CobRCompleta.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCobR.SolicitarRetentativa(const TxId: String; const aDataLiquidacao: TDateTime): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('SolicitarRetentativa(txId: %s; data: %s)', [TxId, FormatDateBr(aDataLiquidacao)]), 2);
  if EstaVazio(Trim(TxId)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['txid']);
  if EstaZerado(aDataLiquidacao) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['DataLiquidacao']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(TxId);
  fPSP.URLPathParams.Add('retentativa');
  fPSP.URLPathParams.Add(FormatDateBr(aDataLiquidacao, 'YYYY-MM-DD'));

  fPSP.AcessarEndPoint(ChttpMethodPOST, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_CREATED);

  if Result then
    CobRCompleta.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCobR.ConsultarCobrancas(aInicio: TDateTime; aFim: TDateTime;
  const idRec: String; const aCpfCnpj: String;
  const aStatus: TACBrPIXStatusRegistroCobranca; const aConvenio: String;
  const aPagAtual: Integer; const aItensPorPagina: Integer): Boolean;
var
  s, e: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('ConsultarCobrancasRecorrentes(%s, %s, %s, %s, %s, %s, %s, %s)',
    [FormatDateTimeBr(aInicio), FormatDateTimeBr(aFim), idRec, aCpfCnpj,
     PIXStatusRegistroCobrancaToString(aStatus), aConvenio, IntToStr(aPagAtual),
     IntToStr(aItensPorPagina)]), 2);

  Clear;
  fPSP.PrepararHTTP;
  with fPSP.URLQueryParams do
  begin
    Values['inicio'] := DateTimeToIso8601(aInicio);
    Values['fim'] := DateTimeToIso8601(aFim);

    if NaoEstaVazio(idRec) then
      Values['idRec'] := idRec;

    s := OnlyNumber(aCpfCnpj);
    if NaoEstaVazio(s) then
    begin
      e := ValidarCNPJouCPF(s);
      if NaoEstaVazio(e) then
        raise EACBrPixException.Create(ACBrStr(e));

      if (Length(s) < 12) then
        Values['cpf'] := s
      else
        Values['cnpj'] := s;
    end;

    if NaoEstaVazio(aConvenio) then
        Values['convenio'] := aConvenio;

    if (aStatus <> srcNENHUM) then
      Values['status'] := PIXStatusRegistroCobrancaToString(aStatus);

    if (aPagAtual > 0) then
      Values['paginacao.paginaAtual'] := IntToStr(aPagAtual);

    if (aItensPorPagina > 0) then
      Values['paginacao.itensPorPagina'] := IntToStr(aItensPorPagina);
  end;

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    CobsRConsultadas.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrPixEndPointWebhook }

function TACBrPixEndPointWebhook.GetWebhookConsultado: TACBrPIXWebhookCobResponse;
begin
  if not Assigned(fWebhookConsultado) then
    fWebhookConsultado := TACBrPIXWebhookCobResponse.Create;
  Result := fWebhookConsultado;
end;

function TACBrPixEndPointWebhook.GetWebhooksConsultados: TACBrPIXWebhookConsultados;
begin
  if not Assigned(fWebhooksConsultados) then
    fWebhooksConsultados := TACBrPIXWebhookConsultados.Create;
  Result := fWebhooksConsultados;
end;

function TACBrPixEndPointWebhook.GetWebhookSolicitado: TACBrPIXWebhookRequest;
begin
  if not Assigned(fWebhookSolicitado) then
    fWebhookSolicitado := TACBrPIXWebhookRequest.Create;
  Result := fWebhookSolicitado;
end;

constructor TACBrPixEndPointWebhook.Create(AOwner: TACBrPSP);
begin
  inherited Create(AOwner);
  fpEndPoint := cEndPointWebhook;
end;

destructor TACBrPixEndPointWebhook.Destroy;
begin
  if Assigned(fWebhookConsultado) then
    fWebhookConsultado.Free;
  if Assigned(fWebhookSolicitado) then
    fWebhookSolicitado.Free;
  if Assigned(fWebhooksConsultados) then
    fWebhooksConsultados.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointWebhook.Clear;
begin
  inherited Clear;
  if Assigned(fWebhookConsultado) then
    fWebhookConsultado.Clear;
  if Assigned(fWebhookSolicitado) then
    fWebhookSolicitado.Clear;
  if Assigned(fWebhooksConsultados) then
    fWebhooksConsultados.Clear;
end;

function TACBrPixEndPointWebhook.CancelarWebhook(aChave: String): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('CancelarWebhook(aChave: %s)', [aChave]), 2);
  if EstaVazio(aChave) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['Chave']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(aChave);
  fPSP.AcessarEndPoint(ChttpMethodDELETE, EndPoint, ResultCode, RespostaHttp);

  Result := (ResultCode = HTTP_NO_CONTENT);

  if not Result then
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointWebhook.ConsultarWebhook(aChave: String): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('ConsultarWebhook(aChave: %s)', [aChave]), 2);
  if EstaVazio(aChave) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['Chave']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(aChave);
  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    WebhookConsultado.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointWebhook.ConsultarWebhooks(aInicio: TDateTime; aFim: TDateTime;
  const aPagAtual: Integer; const aItensPorPagina: Integer): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('ConsultarWebhooks(Inicio: %s, Fim: %s, PagAtual: %d, ItensPorPag: %d)',
    [FormatDateTimeBr(aInicio), FormatDateTimeBr(aFim), aPagAtual, aItensPorPagina]), 2);

  Clear;
  fPSP.PrepararHTTP;
  with fPSP.URLQueryParams do
  begin
    Values['inicio'] := DateTimeToIso8601(aInicio);
    Values['fim'] := DateTimeToIso8601(aFim);

    if NaoEstaZerado(aPagAtual) then
      Values['paginacao.paginaAtual'] := IntToStr(aPagAtual);

    if NaoEstaZerado(aItensPorPagina) then
      Values['paginacao.itensPorPagina'] := IntToStr(aItensPorPagina);
  end;

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    WebhooksConsultados.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointWebhook.ConfigurarWebhook(aChave: String): Boolean;
var
  Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog(Format('ConfigurarWebhook(aChave: %s)', [aChave]), 2);
  if EstaVazio(aChave) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['Chave']);

  if (not Assigned(fWebhookSolicitado)) or fWebhookSolicitado.IsEmpty then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['WebhookSolicitado']);

  Body := Trim(WebhookSolicitado.AsJSON);
  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(aChave);
  fPSP.ConfigurarBody(ChttpMethodPUT, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ChttpMethodPUT, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if not Result then
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrPixEndPointWebhookRec }

function TACBrPixEndPointWebhookRec.GetWebhookRecConsultado: TACBrPIXWebhookResponse;
begin
  if not Assigned(fWebhookRecConsultado) then
    fWebhookRecConsultado := TACBrPIXWebhookResponse.Create;
  Result := fWebhookRecConsultado;
end;

function TACBrPixEndPointWebhookRec.GetWebhookRecSolicitado: TACBrPIXWebhookRequest;
begin
  if not Assigned(fWebhookRecSolicitado) then
    fWebhookRecSolicitado := TACBrPIXWebhookRequest.Create;
  Result := fWebhookRecSolicitado;
end;

constructor TACBrPixEndPointWebhookRec.Create(AOwner: TACBrPSP);
begin
  inherited Create(AOwner);
  fpEndPoint := cEndPointWebhookRec;
end;

destructor TACBrPixEndPointWebhookRec.Destroy;
begin
  if Assigned(fWebhookRecConsultado) then
    fWebhookRecConsultado.Free;
  if Assigned(fWebhookRecSolicitado) then
    fWebhookRecSolicitado.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointWebhookRec.Clear;
begin
  inherited Clear;
  if Assigned(fWebhookRecConsultado) then
    fWebhookRecConsultado.Clear;
  if Assigned(fWebhookRecSolicitado) then
    fWebhookRecSolicitado.Clear;
end;

function TACBrPixEndPointWebhookRec.CancelarWebhookRec: Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog('CancelarWebhookRec', 2);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.AcessarEndPoint(ChttpMethodDELETE, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_NO_CONTENT);

  if not Result then
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointWebhookRec.ConsultarWebhookRec: Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog('ConsultarWebhookRec', 2);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    WebhookRecConsultado.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointWebhookRec.ConfigurarWebhookRec: Boolean;
var
  Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog('ConfigurarWebhookRec', 2);

  if (not Assigned(fWebhookRecSolicitado)) or fWebhookRecSolicitado.IsEmpty then;
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['WebhookRecSolicitado']);

  Body := Trim(WebhookRecSolicitado.AsJSON);
  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(fPSP.ChavePIX);

  fPSP.ConfigurarBody(ChttpMethodPUT, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ChttpMethodPUT, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if not Result then
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrPixEndPointWebhookCobR }

function TACBrPixEndPointWebhookCobR.GetWebhookCobRConsultado: TACBrPIXWebhookResponse;
begin
  if not Assigned(fWebhookCobRConsultado) then
    fWebhookCobRConsultado := TACBrPIXWebhookResponse.Create;
  Result := fWebhookCobRConsultado;
end;

function TACBrPixEndPointWebhookCobR.GetWebhookCobRSolicitado: TACBrPIXWebhookRequest;
begin
  if not Assigned(fWebhookCobRSolicitado) then
    fWebhookCobRSolicitado := TACBrPIXWebhookRequest.Create;
  Result := fWebhookCobRSolicitado;
end;

constructor TACBrPixEndPointWebhookCobR.Create(AOwner: TACBrPSP);
begin
  if (AOwner = nil) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));

  inherited Create(AOwner);
  fpEndPoint := cEndPointWebhookCobR;
end;

destructor TACBrPixEndPointWebhookCobR.Destroy;
begin
  if Assigned(fWebhookCobRConsultado) then
    fWebhookCobRConsultado.Free;
  if Assigned(fWebhookCobRSolicitado) then
    fWebhookCobRSolicitado.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointWebhookCobR.Clear;
begin
  inherited Clear;
  if Assigned(fWebhookCobRConsultado) then
    fWebhookCobRConsultado.Clear;
  if Assigned(fWebhookCobRSolicitado) then
    fWebhookCobRSolicitado.Clear;
end;

function TACBrPixEndPointWebhookCobR.CancelarWebhookCobR: Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog('CancelarWebhookCobR', 2);
  if EstaVazio(fPSP.ChavePIX) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPChavePIX));

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(fPSP.ChavePIX);
  fPSP.AcessarEndPoint(ChttpMethodDELETE, EndPoint, ResultCode, RespostaHttp);

  Result := (ResultCode = HTTP_NO_CONTENT);

  if not Result then
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointWebhookCobR.ConsultarWebhookCobR: Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog('ConsultarWebhookCobR', 2);
  if EstaVazio(fPSP.ChavePIX) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPChavePIX));

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(fPSP.ChavePIX);
  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);

  Result := (ResultCode = HTTP_OK);

  if Result then
    WebhookCobRConsultado.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointWebhookCobR.ConfigurarWebhookCobR: Boolean;
var
  Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  RegistrarLog('ConfigurarWebhookCobR', 2);
  if EstaVazio(fPSP.ChavePIX) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPChavePIX));

  Body := EmptyStr;
  if Assigned(fWebhookCobRSolicitado) and (not fWebhookCobRSolicitado.IsEmpty) then
    Body := Trim(WebhookCobRSolicitado.AsJSON);

  if EstaVazio(Body) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['WebhookCobRSolicitado']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(fPSP.ChavePIX);

  fPSP.ConfigurarBody(ChttpMethodPUT, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;
  fPSP.AcessarEndPoint(ChttpMethodPUT, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if not Result then
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrPSPCertificate }

function TACBrPSPCertificate.GetSenhaPFX: AnsiString;
begin
  Result := StrCrypt(fSenhaPFX, fK)  // Descritografa a Senha
end;

procedure TACBrPSPCertificate.SetArquivoCertificado(aValue: String);
begin
  fArquivoCertificado := Trim(aValue);
  fCertificado := EmptyStr;
end;

procedure TACBrPSPCertificate.SetArquivoChavePrivada(aValue: String);
begin
  fArquivoChavePrivada := Trim(aValue);
  fChavePrivada := EmptyStr;
end;

procedure TACBrPSPCertificate.SetArquivoPFX(const aValue: String);
begin
  fArquivoPFX := Trim(AValue);
  fPFX := EmptyStr;
end;

procedure TACBrPSPCertificate.SetCertificado(aValue: AnsiString);
begin
  fCertificado := aValue;
  fArquivoCertificado := EmptyStr;
end;

procedure TACBrPSPCertificate.SetChavePrivada(aValue: AnsiString);
begin
  fChavePrivada := aValue;
  fArquivoChavePrivada := EmptyStr;
end;

procedure TACBrPSPCertificate.SetPFX(aValue: AnsiString);
begin
  fPFX := aValue;
  fArquivoPFX := EmptyStr;
end;

procedure TACBrPSPCertificate.SetSenhaPFX(const aValue: AnsiString);
begin
  if NaoEstaVazio(fK) and (fSenhaPFX = StrCrypt(AValue, fK)) then
    Exit;

  fK := FormatDateTime('hhnnsszzz', Now);
  fSenhaPFX := StrCrypt(AValue, fK);  // Salva Senha de forma Criptografada, para evitar "Inspect"
end;

procedure TACBrPSPCertificate.ConfigurarHeaders(const Method, AURL: String);
begin
  inherited ConfigurarHeaders(Method, AURL);

  // Adicionando PFX
  if VerificarSeIncluiPFX(Method, AURL) then
  begin
    if NaoEstaVazio(PFX) then
      Http.Sock.SSL.PFX := PFX
    else if NaoEstaVazio(ArquivoPFX) then
      Http.Sock.SSL.PFXfile := ArquivoPFX;

    if NaoEstaVazio(SenhaPFX) then
      Http.Sock.SSL.KeyPassword := SenhaPFX;
  end
  else
  begin
    // Adicionando o Certificado
    if VerificarSeIncluiCertificado(Method, AURL) then
    begin
      if NaoEstaVazio(Certificado) then
      begin
        if StringIsPEM(Certificado) then
          Http.Sock.SSL.Certificate := ConvertPEMToASN1(Certificado)
        else
          Http.Sock.SSL.Certificate := Certificado;
      end
      else if NaoEstaVazio(ArquivoCertificado) then
        Http.Sock.SSL.CertificateFile := ArquivoCertificado;
    end;

    // Adicionando a Chave Privada
    if VerificarSeIncluiChavePrivada(Method, AURL) then
    begin
      if NaoEstaVazio(ChavePrivada) then
      begin
        if StringIsPEM(ChavePrivada) then
          Http.Sock.SSL.PrivateKey := ConvertPEMToASN1(ChavePrivada)
        else
          Http.Sock.SSL.PrivateKey := ChavePrivada;
      end
      else if NaoEstaVazio(ArquivoChavePrivada) then
        Http.Sock.SSL.PrivateKeyFile := ArquivoChavePrivada;

      if NaoEstaVazio(SenhaPFX) then
        Http.Sock.SSL.KeyPassword := SenhaPFX;
    end;
  end;
end;

function TACBrPSPCertificate.VerificarSeIncluiPFX(const Method, AURL: String): Boolean;
begin
  Result := NaoEstaVazio(fPFX) or NaoEstaVazio(fArquivoPFX);
end;

function TACBrPSPCertificate.VerificarSeIncluiCertificado(const Method,
  AURL: String): Boolean;
begin
  Result := NaoEstaVazio(fCertificado) or NaoEstaVazio(fArquivoCertificado);
end;

function TACBrPSPCertificate.VerificarSeIncluiChavePrivada(const Method,
  AURL: String): Boolean;
begin
  Result := NaoEstaVazio(fChavePrivada) or NaoEstaVazio(fArquivoChavePrivada);
end;

constructor TACBrPSPCertificate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fK := EmptyStr;
  fCertificado := EmptyStr;
  fChavePrivada := EmptyStr;
  fArquivoCertificado := EmptyStr;
  fArquivoChavePrivada := EmptyStr;
end;

{ TACBrPixEndPoint }

function TACBrPixEndPoint.GetNivelLog: Byte;
begin
  Result := fPSP.NivelLog;
end;

procedure TACBrPixEndPoint.RegistrarLog(const ALinha: String; aNivelMin: Integer);
begin
  if (NivelLog >= aNivelMin) then
    fPSP.RegistrarLog(ALinha);
end;

constructor TACBrPixEndPoint.Create(AOwner: TACBrPSP);
begin
  inherited Create;
  fPSP := AOwner;
  fHTTP := fPSP.Http;
  fpEndPoint := '';
  fProblema := TACBrPIXProblema.Create;
end;

destructor TACBrPixEndPoint.Destroy;
begin
  fProblema.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPoint.Clear;
begin
  fProblema.Clear;
end;

{ TACBrPixEndPointPix }

constructor TACBrPixEndPointPix.Create(AOwner: TACBrPSP);
begin
  if (AOwner = nil) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));

  inherited Create(AOwner);
  fpEndPoint := cEndPointPix;

  fPixConsultados := TACBrPIXConsultados.Create;
  fPix := TACBrPIX.Create('');
  fDevolucao := TACBrPIXDevolucao.Create('');
  fDevolucaoSolicitada := TACBrPIXDevolucaoSolicitada.Create('');
end;

destructor TACBrPixEndPointPix.Destroy;
begin
  fPixConsultados.Free;
  fPix.Free;
  fDevolucao.Free;
  fDevolucaoSolicitada.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointPix.Clear;
begin
  inherited Clear;
  fPixConsultados.Clear;
  fPix.Clear;
  fDevolucao.Clear;
  fDevolucaoSolicitada.Clear;
end;

function TACBrPixEndPointPix.ConsultarPix(const e2eid: String): Boolean;
var
  e: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog('ConsultarPix( '+e2eid+' )');
  if EstaVazio(Trim(e2eid)) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['e2eid']);

  e := EmptyStr;
  if fPSP.IsBacen then
    e := ValidarEndToEndId(e2eid);
  if NaoEstaVazio(e) then
    raise EACBrPixException.Create(ACBrStr(e));

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(e2eid);
  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    fPix.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointPix.ConsultarPixRecebidos(Inicio: TDateTime;
  Fim: TDateTime; const TxId: String; const CpfCnpj: String; PagAtual: Integer;
  ItensPorPagina: Integer): Boolean;
var
  s, e: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
  begin
    RegistrarLog('ConsultarPixRecebidos( '+FormatDateTimeBr(Inicio)+', '+
                                           FormatDateTimeBr(Fim)+', '+
                                           TxId+', '+CpfCnpj+', '+
                                           IntToStr(PagAtual)+', '+
                                           IntToStr(ItensPorPagina)+' )');
  end;
  Clear;
  fPSP.PrepararHTTP;

  with fPSP.URLQueryParams do
  begin
    Values['inicio'] := DateTimeToIso8601(Inicio);
    Values['fim'] := DateTimeToIso8601(Fim);

    s := Trim(TxId);
    if (s <> '') then
    begin
      Values['txid'] := s;
      Values['txIdPresente'] := 'true';
    end;

    s := OnlyNumber(CpfCnpj);
    if (s <> '') then
    begin
      e := ValidarCNPJouCPF(s);
      if (e <> '') then
        raise EACBrPixException.Create(ACBrStr(e));

      if Length(s) < 12 then
        Values['cpf'] := s
      else
        Values['cnpj'] := s;
    end;

    if (PagAtual > 0) then
      Values['paginacao.paginaAtual'] := IntToStr(PagAtual);

    if (ItensPorPagina > 0) then
      Values['paginacao.itensPorPagina'] := IntToStr(ItensPorPagina);
  end;

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    fPixConsultados.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointPix.SolicitarDevolucaoPix(const e2eid,
  idDevolucao: String): Boolean;
var
  Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog('SolicitarDevolucaoPix( '+e2eid+', '+idDevolucao+' )');
  if (Trim(e2eid) = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['e2eid']);

  if (Trim(idDevolucao) = '') and fPSP.IsBacen then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['idDevolucao']);

  Body := Trim(fDevolucaoSolicitada.AsJSON);
  if (Body = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['DevolucaoSolicitada']);

  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(e2eid);
  fPSP.URLPathParams.Add('devolucao');
  fPSP.URLPathParams.Add(idDevolucao);
  fPSP.ConfigurarBody(ChttpMethodPUT, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;

  Clear;
  fPSP.AcessarEndPoint(ChttpMethodPUT, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_CREATED);

  if Result then
    fDevolucao.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointPix.ConsultarDevolucaoPix(const e2eid,
  idDevolucao: String): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog('ConsultarDevolucaoPix( '+e2eid+', '+idDevolucao+' )');
  if (Trim(e2eid) = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['e2eid']);

  if (Trim(idDevolucao) = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['idDevolucao']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(e2eid);
  fPSP.URLPathParams.Add('devolucao');
  fPSP.URLPathParams.Add(idDevolucao);
  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    fDevolucao.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrPixEndPointCob }

procedure TACBrPixEndPointCob.VerificarCopiaECola;
begin
  try
    if EstaVazio(Trim(CobGerada.pixCopiaECola)) and Assigned(fPSP.ACBrPixCD) and
       NaoEstaVazio(CobGerada.location) and
       NaoEstaVazio(fPSP.ACBrPixCD.Recebedor.Nome) and
       NaoEstaVazio(fPSP.ACBrPixCD.Recebedor.Cidade) then
      CobGerada.pixCopiaECola := fPSP.ACBrPixCD.GerarQRCodeDinamico(CobGerada.location);
  except
  end;
end;

constructor TACBrPixEndPointCob.Create(AOwner: TACBrPSP);
begin
  if (AOwner = nil) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));

  inherited Create(AOwner);
  fpEndPoint := cEndPointCob;

  fCobsConsultadas := TACBrPIXCobsConsultadas.Create('');
  fCobSolicitada := TACBrPIXCobSolicitada.Create('');
  fCobGerada := TACBrPIXCobGerada.Create('');
  fCobRevisada := TACBrPIXCobRevisada.Create('');
  fCobCompleta := TACBrPIXCobCompleta.Create('');

  fCobsConsultadas.IsBacen := fPSP.IsBacen;
  fCobSolicitada.IsBacen := fPSP.IsBacen;
  fCobGerada.IsBacen := fPSP.IsBacen;
  fCobRevisada.IsBacen := fPSP.IsBacen;
  fCobCompleta.IsBacen := fPSP.IsBacen;
end;

destructor TACBrPixEndPointCob.Destroy;
begin
  fCobsConsultadas.Free;
  fCobSolicitada.Free;
  fCobGerada.Free;
  fCobRevisada.Free;
  fCobCompleta.Free;
  inherited Destroy;
end;

procedure TACBrPixEndPointCob.Clear;
begin
  inherited Clear;
  fCobsConsultadas.Clear;
  fCobSolicitada.Clear;
  fCobGerada.Clear;
  fCobRevisada.Clear;
  fCobCompleta.Clear;
end;

function TACBrPixEndPointCob.CriarCobrancaImediata(const TxId: String): Boolean;
var
  Body, ep: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog('CriarCobrancaImediata( '+TxId+' )');

  Body := Trim(fCobSolicitada.AsJSON);
  if (Body = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['CobSolicitada']);

  fPSP.PrepararHTTP;
  if (TxId <> '') then
  begin
    fPSP.URLPathParams.Add(TxId);
    ep := ChttpMethodPUT;
  end
  else
    ep := ChttpMethodPOST;

  fPSP.ConfigurarBody(ep, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;

  Clear;
  fPSP.AcessarEndPoint(ep, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_CREATED);

  if Result then
  begin
    fCobGerada.AsJSON := String(RespostaHttp);
    VerificarCopiaECola;
  end
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCob.RevisarCobrancaImediata(const TxId: String): Boolean;
var
  Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog('RevisarCobrancaImediata( '+TxId+' )');
  if (Trim(TxId) = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['txid']);

  Body := Trim(fCobRevisada.AsJSON);
  if (Body = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['CobRevisada']);

  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(TxId);
  fPSP.ConfigurarBody(ChttpMethodPATCH, EndPoint, Body);
  WriteStrToStream(fPSP.Http.Document, Body);
  fPSP.Http.MimeType := CContentTypeApplicationJSon;

  Clear;
  fPSP.AcessarEndPoint(ChttpMethodPATCH, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    fCobGerada.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCob.ConsultarCobrancaImediata(const TxId: String;
  Revisao: Integer): Boolean;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
    RegistrarLog('ConsultarCobrancaImediata( '+TxId+', '+IntToStr(Revisao)+' )');
  if (Trim(TxId) = '') then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['txid']);

  Clear;
  fPSP.PrepararHTTP;
  fPSP.URLPathParams.Add(TxId);
  if (Revisao <> 0) then
    fPSP.URLQueryParams.Values['revisao'] := IntToStr(Revisao);

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    fCobCompleta.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

function TACBrPixEndPointCob.ConsultarCobrancas(Inicio: TDateTime;
  Fim: TDateTime; const CpfCnpj: String; LocationPresente: Boolean;
  AStatus: TACBrPIXStatusCobranca; PagAtual: Integer; ItensPorPagina: Integer): Boolean;
var
  s, e: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (NivelLog > 1) then
  begin
    RegistrarLog('ConsultarCobrancas( '+FormatDateTimeBr(Inicio)+', '+
                                        FormatDateTimeBr(Fim)+', '+
                                        CpfCnpj+', '+
                                        BoolToStr(LocationPresente, true)+', '+
                                        PIXStatusCobrancaToString(AStatus)+', '+
                                        IntToStr(PagAtual)+', '+
                                        IntToStr(ItensPorPagina)+' )');
  end;
  Clear;
  fPSP.PrepararHTTP;

  with fPSP.URLQueryParams do
  begin
    Values['inicio'] := DateTimeToIso8601(Inicio);
    Values['fim'] := DateTimeToIso8601(Fim);

    s := OnlyNumber(CpfCnpj);
    if (s <> '') then
    begin
      e := ValidarCNPJouCPF(s);
      if (e <> '') then
        raise EACBrPixException.Create(ACBrStr(e));

      if Length(s) < 12 then
        Values['cpf'] := s
      else
        Values['cnpj'] := s;
    end;

    Values['locationPresente'] := ifthen(LocationPresente, 'true', 'false');
    if (AStatus <> stcNENHUM) then
      Values['status'] := PIXStatusCobrancaToString(AStatus);

    if (PagAtual > 0) then
      Values['paginacao.paginaAtual'] := IntToStr(PagAtual);

    if (ItensPorPagina > 0) then
      Values['paginacao.itensPorPagina'] := IntToStr(ItensPorPagina);
  end;

  fPSP.AcessarEndPoint(ChttpMethodGET, EndPoint, ResultCode, RespostaHttp);
  Result := (ResultCode = HTTP_OK);

  if Result then
    fCobsConsultadas.AsJSON := String(RespostaHttp)
  else
    fPSP.TratarRetornoComErro(ResultCode, RespostaHttp, Problema);
end;

{ TACBrQueryParams }

function TACBrQueryParams.GetAsURL: String;
var
  i: Integer;
  AName, AValue: String;
begin
  Result := '';
  if (Count = 0) then
    Exit;

  for i := 0 to Count-1 do
  begin
    AName := Names[i];
    if (AName <> '') then
    begin
      if (Result <> '') then
        Result := Result + '&';
      AValue := Values[AName];
      Result := Result + EncodeURLElement(AName)+'='+EncodeURLElement(AValue);
    end;
  end;
end;

procedure TACBrQueryParams.SetAsURL(const AValue: String);
var
  s: String;
begin
  Clear;
  s := Trim(AValue);
  if (copy(s,1,1) = '?') then
    System.Delete(s, 1, 1);

  s := DecodeURL(s);
  AddDelimitedTextToList(s, '&', Self, #0);
end;

{ TACBrPSP }

constructor TACBrPSP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fAPIVersion := ver262;
  fChavePIX := '';
  fTipoChave := tchNenhuma;
  fpAutenticado := False;
  fpValidadeToken := 0;
  fpToken := '';
  fpRefreshToken := '';
  fpIsBacen := True;

  fHttpRespStream := TMemoryStream.Create;
  fHttpSend := THTTPSend.Create;
  fHttpSend.OutputStream := fHttpRespStream;

  fepPix := TACBrPixEndPointPix.Create(Self);
  fepRec := TACBrPixEndPointRec.Create(Self);
  fepCob := TACBrPixEndPointCob.Create(Self);
  fepCobV := TACBrPixEndPointCobV.Create(Self);
  fepCobR := TACBrPixEndPointCobR.Create(Self);
  fepLocRec := TACBrPixEndPointLocRec.Create(Self);
  fepSolicRec := TACBrPixEndPointSolicRec.Create(Self);
  fepWebhook := TACBrPixEndPointWebhook.Create(Self);
  fepWebhookRec := TACBrPixEndPointWebhookRec.Create(Self);
  fepWebhookCobR := TACBrPixEndPointWebhookCobR.Create(Self);
  fURLQueryParams := TACBrQueryParams.Create;
  fURLPathParams := TStringList.Create;
  fScopes := [scCobWrite, scCobRead, scPixWrite, scPixRead];
  
  fpQuandoAcessarEndPoint := Nil;
  fpQuandoReceberRespostaEndPoint := Nil;
  fQuandoTransmitirHttp := Nil;
  fQuandoReceberRespostaHttp := Nil;
  fpOnAntesAutenticar := Nil;
  fpOnDepoisAutenticar := Nil;
end;

destructor TACBrPSP.Destroy;
begin
  fHttpSend.Free;
  fHttpRespStream.Free;
  fepPix.Free;
  fepRec.Free;
  fepCob.Free;
  fepCobV.Free;
  fepCobR.Free;
  fepLocRec.Free;
  fepSolicRec.Free;
  fepWebhook.Free;
  fepWebhookRec.Free;
  fepWebhookCobR.Free;
  fURLQueryParams.Free;
  fURLPathParams.Free;

  inherited Destroy;
end;

procedure TACBrPSP.Clear;
begin
  fHttpSend.Clear;
  fURLQueryParams.Clear;
  fepPix.Clear;
  fepRec.Clear;
  fepCob.Clear;
  fepCobV.Clear;
  fepCobR.Clear;
  fepLocRec.Clear;
  fepSolicRec.Clear;
  fepWebhook.Clear;
  fepWebhookRec.Clear;
  fepWebhookCobR.Clear;
end;

procedure TACBrPSP.SetACBrPixCD(AValue: TACBrPixCD);
var
  va: TACBrPixCD;
begin
  if (AValue = fPixCD) then
    Exit;

  if Assigned(fPixCD) then
    fPixCD.RemoveFreeNotification(Self);

  va := fPixCD;       // Usa outra variavel para evitar Loop Infinito,
  fPixCD := AValue;   // na remoção da associação dos componentes

  if Assigned(va) then
    if Assigned(va.PSP) then
      va.PSP := Nil;

  if (AValue <> Nil) then
  begin
    AValue.FreeNotification(Self);
    AValue.PSP := Self;
  end ;
end;

procedure TACBrPSP.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation) ;
  if (Operation = opRemove) and (fPixCD <> Nil) and (AComponent is TACBrPixCD) then
    fPixCD := Nil
end;

procedure TACBrPSP.VerificarPIXCDAtribuido;
begin
  if not Assigned(fPixCD) then
    raise EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido));
end;

procedure TACBrPSP.DispararExcecao(E: Exception);
begin
  if not Assigned(E) then
    Exit;

  RegistrarLog(E.ClassName + ': ' + E.Message);
  raise E;
end;

procedure TACBrPSP.RegistrarLog(const ALinha: String; aNivelLog: Integer);
begin
  if Assigned(fPixCD) then
    fPixCD.RegistrarLog(ALinha, aNivelLog);
end;

function TACBrPSP.GetClientID: String;
begin
  Result := StrCrypt(fClientID, fk1);
end;

function TACBrPSP.GetNivelLog: Byte;
begin
  if Assigned(fPixCD) then
    Result := fPixCD.NivelLog
  else
    Result := 1;
end;

procedure TACBrPSP.SetClientID(AValue: String);
begin
  if (fk1 <> '') and (fClientID = StrCrypt(AValue, fk1)) then
    Exit;

  fk1 := FormatDateTime('hhnnsszzz',Now);
  fClientID := StrCrypt(AValue, fk1);  // Salva de forma Criptografada, para evitar "Inspect"
  fpAutenticado := False;  // Força uma nova autenticação
end;

function TACBrPSP.GetClientSecret: String;
begin
  Result := StrCrypt(fClientSecret, fk2);
end;

procedure TACBrPSP.SetClientSecret(AValue: String);
begin
  if (fk2 <> '') and (fClientSecret = StrCrypt(AValue, fk2)) then
    Exit;

  fk2 := FormatDateTime('hhnnsszzz',Now);
  fClientSecret := StrCrypt(AValue, fk2);  // Salva de forma Criptografada, para evitar "Inspect"
  fpAutenticado := False;  // Força uma nova autenticação
end;

procedure TACBrPSP.SetTipoChave(AValue: TACBrPIXTipoChave);
begin
  {}
end;

procedure TACBrPSP.SetChavePIX(AValue: String);
var
  TipoChave: TACBrPIXTipoChave;
begin
  if (fChavePix = AValue) then
    Exit;

  TipoChave := DetectarTipoChave(AValue);
  if (TipoChave = tchNenhuma) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroChaveInvalida), [AValue]);

  fChavePix := Trim(AValue);
  fTipoChave := TipoChave;
end;

function TACBrPSP.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  Result := '';
  raise EACBrPixHttpException.Create(
    ACBrStr(Format(sErroMetodoNaoImplementado,['ObterURLAmbiente',ClassName])));
end;

procedure TACBrPSP.ConfigurarHTTP;
begin
  if (NivelLog > 2) then
    RegistrarLog('ConfigurarHTTP');
  ConfigurarProxy;
  ConfigurarTimeOut;
end;

procedure TACBrPSP.ConfigurarProxy;
begin
  if (NivelLog > 3) then
    RegistrarLog('ConfigurarProxy');
  VerificarPIXCDAtribuido;
  fHttpSend.ProxyHost := fPixCD.Proxy.Host;
  fHttpSend.ProxyPort := fPixCD.Proxy.Port;
  fHttpSend.ProxyUser := fPixCD.Proxy.User;
  fHttpSend.ProxyPass := fPixCD.Proxy.Pass;
end;

procedure TACBrPSP.ConfigurarTimeOut;
begin
  if (NivelLog > 3) then
    RegistrarLog('ConfigurarTimeOut');
  VerificarPIXCDAtribuido;
  if (fPixCD.TimeOut = 0) then
    Exit;

  fHttpSend.Timeout := fPixCD.TimeOut;
  with fHttpSend.Sock do
  begin
    ConnectionTimeout := fPixCD.TimeOut;
    InterPacketTimeout := False;
    NonblockSendTimeout := fPixCD.TimeOut;
    SocksTimeout := fPixCD.TimeOut;
    HTTPTunnelTimeout := fPixCD.TimeOut;
  end;
end;

procedure TACBrPSP.ConfigurarHeaders(const Method, AURL: String);
var
  i: Integer;
  ae: String;
begin
  { Sobreescrever no PSP, se necessário }
  if (NivelLog > 2) then
    RegistrarLog('ConfigurarHeaders( '+Method+', '+AURL+' )');

  // Adicionando Header de compactação
  ae := '';
  for i := Low(ChttpContentEncodingCompress) to High(ChttpContentEncodingCompress) do
    ae := ae + ', '+ChttpContentEncodingCompress[i];
  Delete(ae,1,1);
  
  fHttpSend.Headers.Add(ChhtpHeaderAcceptEncoding + ae);
end;

procedure TACBrPSP.ConfigurarAutenticacao(const Method, EndPoint: String);
begin
  { Sobreescrever no PSP, se necessário }
  if (NivelLog > 2) then
    RegistrarLog('ConfigurarAutenticacao( '+Method+', '+EndPoint+' )');
  if (fpToken <> '') then
    fHttpSend.Headers.Insert(0, ChttpHeaderAuthorization + ChttpAuthorizationBearer+' '+fpToken);
end;

procedure TACBrPSP.ConfigurarPathParameters(const Method, EndPoint: String);
begin
  { Sobreescrever no PSP, se necessário }
end;

procedure TACBrPSP.ConfigurarQueryParameters(const Method, EndPoint: String);
begin
  { Sobreescrever no PSP, se necessário }
end;

procedure TACBrPSP.ConfigurarBody(const aMethod, aEndPoint: String;
  var aBody: String);
begin
  { Sobreescreva em PSPs que possuem payload fora do padrão da API do BC }
end;

function TACBrPSP.AcessarEndPoint(const Method, EndPoint: String; out
  ResultCode: Integer; out RespostaHttp: AnsiString): Boolean;
var
  AURL, AMethod: String;
begin
  if (NivelLog > 1) then
    RegistrarLog('AcessarEndPoint( '+Method+', '+EndPoint+' )');
  ConfigurarAutenticacao(Method, EndPoint);
  AURL := CalcularURLEndPoint(Method, EndPoint);
  AMethod := Method;

  ChamarEventoQuandoAcessarEndPoint(EndPoint, AURL, AMethod);
  Result := TransmitirHttp(AMethod, AURL, ResultCode, RespostaHttp);
  ChamarEventoQuandoReceberRespostaEndPoint(EndPoint, AURL, AMethod, ResultCode, RespostaHttp);
  if (NivelLog > 1) then
    RegistrarLog('  ResultCode:'+IntToStr(ResultCode));
  if (NivelLog > 2) then
    RegistrarLog('  RespostaHttp: '+RespostaHttp);
end;

function TACBrPSP.CalcularURLEndPoint(const Method, EndPoint: String): String;
var
  AEndPointPath, p: String;
  i: Integer;
begin
  if (NivelLog > 3) then
    RegistrarLog('CalcularURLEndPoint( '+Method+', '+EndPoint+' )');
  AEndPointPath := CalcularEndPointPath(Method, EndPoint);
  Result := URLSemDelimitador(ObterURLAmbiente(fPixCD.Ambiente));
  if (AEndPointPath <> '') then
    Result := Result + AEndPointPath;

  ConfigurarPathParameters(Method, EndPoint);
  ConfigurarQueryParameters(Method, EndPoint);

  if (fURLPathParams.Count > 0) then
    for i := 0 to fURLPathParams.Count-1 do
      Result := URLComDelimitador(Result) + URLSemDelimitador(EncodeURLElement(fURLPathParams[i]));

  p := fURLQueryParams.AsURL;
  if (p <> '') then
    Result := Result + '?' + p;

  if (NivelLog > 3) then
    RegistrarLog('  '+Result);
end;

function TACBrPSP.CalcularEndPointPath(const Method, EndPoint: String): String;
begin
  { Sobreescreva em PSPs que usem Nomes de EndPoint, fora do padrão da API do BC }
  Result := Trim(EndPoint);
end;

function TACBrPSP.EfetuarAutenticacaoManual: Boolean;
var
  wToken: String;
  wValidade: TDateTime;
begin
  Result := False;
  wToken := EmptyStr;
  wValidade := 0;

  if (not Assigned(fpOnAntesAutenticar)) then
    Exit;

  fpOnAntesAutenticar(wToken, wValidade);

  Result := (wToken <> EmptyStr) and (wValidade <> 0);
  fpAutenticado := Result;

  if Result then
  begin
    fpToken := wToken;
    fpValidadeToken := wValidade;

    if (NivelLog > 1) then
      RegistrarLog(ACBrStr('Efetuada autenticação manual' + sLineBreak +
        ' - Token: ' + fpToken + sLineBreak +
        ' - Validade: ' + DateTimeToStr(fpValidadeToken)));
  end;
end;

function TACBrPSP.ScopeToString(aScope: TACBrPSPScope): String;
begin
  case aScope of
    scCobWrite: Result := 'cob.write';
    scCobRead: Result := 'cob.read';
    scCobVWrite: Result := 'cobv.write';
    scCobVRead: Result := 'cobv.read';
    scCobRWrite: Result := 'cobr.write';
    scCobRRead: Result := 'cobr.read';
    scLoteCobVWrite: Result := 'lotecobv.write';
    scLoteCobVRead: Result := 'lotecobv.read';
    scPixWrite: Result := 'pix.write';
    scPixRead: Result := 'pix.read';
    scWebhookWrite: Result := 'webhook.write';
    scWebhookRead: Result := 'webhook.read';
    scWebhookRecWrite: Result := 'webhookrec.write';
    scWebhookRecRead: Result := 'webhookrec.read';
    scWebhookCobRWrite: Result := 'webhookcobr.write';
    scWebhookCobRRead: Result := 'webhookcobr.read';
    scPayloadLocationWrite: Result := 'payloadlocation.write';
    scPayloadLocationRead: Result := 'payloadlocation.read';
    scPayloadLocationRecWrite: Result := 'payloadlocationrec.write';
    scPayloadLocationRecRead: Result := 'payloadlocationrec.read';
    scRecWrite: Result := 'rec.write';
    scRecRead: Result := 'rec.read';
    scSolicRecWrite: Result := 'solicrec.write';
    scSolicRecRead: Result := 'solicrec.read';
  end;
end;

function TACBrPSP.ScopesToString(aScopes: TACBrPSPScopes): String;
var
  i: TACBrPSPScope;
begin
  Result := EmptyStr;
  for i := Low(TACBrPSPScope) to High(TACBrPSPScope) do
    if i in aScopes then
      Result := Result + IfThen(NaoEstaVazio(Result), ' ') + ScopeToString(i);
end;

procedure TACBrPSP.ChamarEventoQuandoAcessarEndPoint(const AEndPoint: String;
  var AURL: String; var AMethod: String);
var
  vURL, vMethod: String;
  HttpBody, vBody: AnsiString;
begin
  if not Assigned(fpQuandoAcessarEndPoint) then
    Exit;

  if (NivelLog > 2) then
  begin
    RegistrarLog('ChamarEventoQuandoAcessarEndPoint( '+AEndPoint+', '+AURL+', '+AMethod+' )' );
    HttpBody := StreamToAnsiString(fHttpSend.Document);
  end;

  // Chama o Evento //
  vURL := AURL;
  vMethod := AMethod;
  fpQuandoAcessarEndPoint(AEndPoint, vURL, vMethod);

  if (AURL <> vURL) then
  begin
    if (NivelLog > 2) then
      RegistrarLog('  URL modificada: '+AURL+' -> '+vURL);
    AURL := vURL;
  end;

  if (AMethod <> vMethod) then
  begin
    if (NivelLog > 2) then
      RegistrarLog('  Method modificado: '+AMethod+' -> '+vMethod);
    AMethod := vMethod;
  end;

  if (NivelLog > 2) then
  begin
    vBody := StreamToAnsiString(fHttpSend.Document);
    if (HttpBody <> vBody) then
    begin
      if (NivelLog > 3) then
        RegistrarLog('  Body Original:'+ sLineBreak + HttpBody)
      else
        RegistrarLog('  Body Modificado');
    end;
  end;
end;

procedure TACBrPSP.ChamarEventoQuandoReceberRespostaEndPoint(const AEndPoint,
  AURL, AMethod: String; var ResultCode: Integer; var RespostaHttp: AnsiString);
var
  vRespostaHttp: AnsiString;
  vResultCode: Integer;
begin
  if not Assigned(fpQuandoReceberRespostaEndPoint) then
    Exit;

  if (NivelLog > 2) then
    RegistrarLog('ChamarEventoQuandoReceberRespostaEndPoint( '+AEndPoint+', '+AMethod+' )');

  // Chama o Evento //
  vResultCode := ResultCode;
  vRespostaHttp := RespostaHttp;
  fpQuandoReceberRespostaEndPoint(AEndPoint, AURL, AMethod, vResultCode, vRespostaHttp);

  if (ResultCode <> vResultCode) then
  begin
    if (NivelLog > 2) then
      RegistrarLog('  ResultCode modificado: '+IntToStr(ResultCode)+' -> '+IntToStr(vResultCode));
    ResultCode := vResultCode;
  end;

  if (RespostaHttp <> vRespostaHttp) then
  begin
    if (NivelLog > 3) then
      RegistrarLog('  RespostaHttp Original:'+ sLineBreak + RespostaHttp)
    else if (NivelLog > 2) then
      RegistrarLog('  RespostaHTTP Modificada:');
    RespostaHttp := vRespostaHttp;
  end;
end;

procedure TACBrPSP.LimparHTTP;
begin
  if (NivelLog > 2) then
    RegistrarLog('LimparHTTP');

  fHttpSend.Clear;
  fURLPathParams.Clear;
  fURLQueryParams.Clear;
  fHttpSend.UserName := EmptyStr;
  fHttpSend.Password := EmptyStr;
end;

procedure TACBrPSP.PrepararHTTP;
begin
  if (NivelLog > 2) then
    RegistrarLog('PrepararHTTP');
  VerificarPIXCDAtribuido;
  VerificarAutenticacao;
  LimparHTTP;
end;

function TACBrPSP.TransmitirHttp(const AMethod, AURL: String; out
  ResultCode: Integer; out RespostaHttp: AnsiString): Boolean;
var
  vMethod, vURL, vLocation, vReqHeader, wCertLog: String;
  vHttpBody, vMimeType: AnsiString;
  ContRedir: Integer;

  function DoTransmitirHTTP: Boolean;
  begin
    ConfigurarHTTP;
    ConfigurarHeaders(vMethod, vURL);
    ChamarEventoQuandoTransmitirHttp(vURL, vMethod);
    if (NivelLog > 2) then
    begin
      RegistrarLog('  Req.Headers:'+ sLineBreak + fHTTPSend.Headers.Text);
      RegistrarLog('  Req.Body:'+ sLineBreak + vHttpBody);
    end;

    wCertLog :=
      IfThen(NaoEstaVazio(Http.Sock.SSL.PFX), 'Http.Sock.SSL.PFX: ' + Http.Sock.SSL.PFX + sLineBreak) +
      IfThen(NaoEstaVazio(Http.Sock.SSL.PrivateKey), 'Http.Sock.SSL.PrivateKey: ' + Http.Sock.SSL.PrivateKey + sLineBreak) +
      IfThen(NaoEstaVazio(Http.Sock.SSL.Certificate), 'Http.Sock.SSL.Certificate: ' + Http.Sock.SSL.Certificate + sLineBreak) +
      IfThen(NaoEstaVazio(Http.Sock.SSL.PrivateKeyFile), 'Http.Sock.SSL.PrivateKeyFile: ' + Http.Sock.SSL.PrivateKeyFile + sLineBreak) +
      IfThen(NaoEstaVazio(Http.Sock.SSL.CertificateFile), 'Http.Sock.SSL.CertificateFile: ' + Http.Sock.SSL.CertificateFile + sLineBreak);

    // Registra o log apenas se uma das propriedades estiverem configuradas
    if NaoEstaVazio(wCertLog) then
      RegistrarLog(sLineBreak + wCertLog, 4);

    fHttpRespStream.Clear;
    Result := fHttpSend.HTTPMethod(vMethod, vURL);  // HTTP call
    ResultCode := fHttpSend.ResultCode;

    if (NivelLog > 1) then
      RegistrarLog('  ResultCode: '+IntToStr(ResultCode)+' - '+fHttpSend.ResultString);

    if (NivelLog > 3) then
    begin
      RegistrarLog('  Sock.LastError: '+IntToStr(fHttpSend.Sock.LastError));
      RegistrarLog('  Resp.Headers:'+ sLineBreak + fHttpSend.Headers.Text);
    end;
  end;

  function ResultCodeIsRedir(ResultCode: Integer): Boolean;
  begin
    case ResultCode of
      301, 302, 303, 307:
        Result := True;
      else
        Result := False;
    end;
  end;

  function IsAbsoluteURL(const URL: String): Boolean;
  const
    protocolos: array[0..2] of string = ('http','https', 'ftp');
  var
   i: Integer;
  begin
    Result := False;

    //Testa se é um tipo absoluto relativo ao protocolo
    if Pos('//', URL) = 1 then
    begin
      Result := True;
      Exit;
    end;

    //Testa se é um tipo relativo
    if Pos('/', URL) = 1 then
    begin
      Result := False;
      Exit;
    end;

    //Testa se inicia por protocolos...
    for I := 0 to High(protocolos) do
    begin
      if Pos(UpperCase(protocolos[i])+'://', UpperCase(URL)) = 1 then
      begin
        Result := True;
        Break;
      end;
    end;

    if Result then Exit;

    //Começa com "www."
    if Pos('www.', URL) = 1 then
    begin
      Result := True;
      Exit;
    end;
  end;

  function GetURLBasePath(const URL: String): String;
  begin
    Result := Copy(URL, 1, PosLast('/',URL) );
  end;

begin
  VerificarPIXCDAtribuido;
  if NivelLog > 1 then
    RegistrarLog('TransmitirHttp( '+AMethod+', '+AURL+' )');

  vMethod := AMethod;
  vURL := AURL;
  vReqHeader := fHttpSend.Headers.Text;
  vHttpBody := StreamToAnsiString(fHttpSend.Document);
  vMimeType := fHttpSend.MimeType;
  Result := DoTransmitirHTTP;

  ContRedir := 0;
  while Result and (ContRedir <= 10) and (ResultCodeIsRedir(ResultCode)) do
  begin
    Inc(ContRedir);
    if (NivelLog > 2) then
      RegistrarLog('  Redirect: '+IntToStr(ContRedir));

    vLocation := GetHeaderValue('Location:', fHttpSend.Headers);
    vLocation := Trim(SeparateLeft( vLocation, ';' ));

    //Location pode ser relativa ou absoluta http://stackoverflow.com/a/25643550/460775
    if IsAbsoluteURL(vLocation) then
      vURL := vLocation
    else
      vURL := GetURLBasePath( vURL ) + vLocation;

    fHTTPSend.Clear;
    fHTTPSend.Headers.Text := vReqHeader;
    WriteStrToStream(fHttpSend.Document, vHttpBody);
    fHttpSend.MimeType := vMimeType;

    // Tipo de método usado não deveria ser trocado...
    // https://tools.ietf.org/html/rfc2616#page-62
    // ... mas talvez seja necessário, pois a maioria dos browsers o fazem
    // http://blogs.msdn.com/b/ieinternals/archive/2011/08/19/understanding-the-impact-of-redirect-response-status-codes-on-http-methods-like-head-get-post-and-delete.aspx
    if (ResultCode = HTTP_SEE_OTHER) or
       ( ((ResultCode = HTTP_MOVED_PERMANENTLY) or (ResultCode = HTTP_MOVED_TEMPORARILY)) and (vMethod = ChttpMethodPOST) ) then
      vMethod := ChttpMethodGET;

    Result := DoTransmitirHTTP;
  end;

  if ContentIsCompressed(fHttpSend.Headers) then
  begin
    if (NivelLog > 2) then
      RegistrarLog('    Decompress Content');

    RespostaHttp := DecompressStream(fHttpSend.OutputStream)
  end
  else
    RespostaHttp := StreamToAnsiString(fHttpSend.OutputStream);

  if (NivelLog > 2) then
    RegistrarLog('Resp.Body:'+ sLineBreak + RespostaHttp);

  ChamarEventoQuandoReceberRespostaHttp(vURL, vMethod, ResultCode, RespostaHttp);
end;

procedure TACBrPSP.ChamarEventoQuandoTransmitirHttp(var AURL: String;
  var AMethod: String);
var
  vURL, vMethod: String;
  HttpBody, vBody: AnsiString;
begin
  if not Assigned(fQuandoTransmitirHttp) then
    Exit;

  if (NivelLog > 2) then
    RegistrarLog('ChamarEventoQuandoTransmitirHttp( '+AURL+', '+AMethod+' )');

  // Chama o Evento //
  vURL := AURL;
  vMethod := AMethod;
  HttpBody := StreamToAnsiString(fHttpSend.Document);
  vBody := HttpBody;
  fQuandoTransmitirHttp(vURL, vMethod, fHttpSend.Headers, vBody);

  if (AURL <> vURL) then
  begin
    if (NivelLog > 2) then
      RegistrarLog('  URL modificada: '+AURL+' -> '+vURL);
    AURL := vURL;
  end;

  if (AMethod <> vMethod) then
  begin
    if (NivelLog > 2) then
      RegistrarLog('  Method modificado: '+AMethod+' -> '+vMethod);
    AMethod := vMethod;
  end;

  if (HttpBody <> vBody) then
  begin
    if (NivelLog > 3) then
      RegistrarLog('  Body Original:'+ sLineBreak + HttpBody)
    else if (NivelLog > 2) then
      RegistrarLog('  Body Modificado:');
    fHttpSend.Document.Clear;
    WriteStrToStream(fHttpSend.Document, vBody);
  end;
end;

procedure TACBrPSP.ChamarEventoQuandoReceberRespostaHttp(const AURL: String;
  const AMethod: String; var ResultCode: Integer; var RespostaHttp: AnsiString);
var
  vResultCode: Integer;
  vRespostaHttp: AnsiString;
begin
  if not Assigned(fQuandoReceberRespostaHttp) then
    Exit;

  if (NivelLog > 2) then
    RegistrarLog('ChamarEventoQuandoReceberRespostaHttp( '+AURL+', '+AMethod+' )');

  vResultCode := ResultCode;
  vRespostaHttp := RespostaHttp;
  fQuandoReceberRespostaHttp(AURL, AMethod, fHttpSend.Headers, vResultCode, vRespostaHttp);

  if (ResultCode <> vResultCode) then
  begin
    if (NivelLog > 2) then
      RegistrarLog('  ResultCode modificado: '+IntToStr(ResultCode)+' -> '+IntToStr(vResultCode));
    ResultCode := vResultCode;
  end;

  if (RespostaHttp <> vRespostaHttp) then
  begin
    if (NivelLog > 3) then
      RegistrarLog('  RespostaHttp Original:'+ sLineBreak + RespostaHttp)
    else if (NivelLog > 2) then
      RegistrarLog('  RespostaHttp Modificada:');
    RespostaHttp := vRespostaHttp;
  end;
end;

procedure TACBrPSP.TratarRetornoComErro(ResultCode: Integer;
  const RespostaHttp: AnsiString; Problema: TACBrPIXProblema);
begin
  if (NivelLog > 2) then
    RegistrarLog('TratarRetornoComErro( '+IntToStr(ResultCode)+' )');

  if (ResultCode = HTTP_UNAUTHORIZED) then
    fpAutenticado := False;

  Problema.Clear;
  if (Trim(RespostaHttp) = '') then
    AtribuirErroHTTPProblema(Problema)
  else
  begin
    try
      Problema.AsJSON := UTF8ToNativeString(RespostaHttp);
    except
    end;

    if (Problema.detail = '') then
      AtribuirErroHTTPProblema(Problema);
  end;
end;

procedure TACBrPSP.AtribuirErroHTTPProblema(Problema: TACBrPIXProblema);
begin
  if (Problema.status = 0) then
    Problema.status := HTTP.ResultCode;

  if (Problema.title = '') then
    Problema.title := HTTP.ResultString;

  if (Problema.detail = '') then
    Problema.detail := StreamToAnsiString(HTTP.OutputStream);
end;

procedure TACBrPSP.Autenticar;
begin
  fpAutenticado := True;
end;

procedure TACBrPSP.VerificarValidadeToken;
begin
  if (ValidadeToken <> 0) and (ValidadeToken < Now) then
  begin
    if (NivelLog > 2) then
      RegistrarLog('RenovarToken');
    RenovarToken;
  end;
end;

procedure TACBrPSP.RenovarToken;
begin
  Autenticar;

  if Assigned(fpOnDepoisAutenticar) then
    fpOnDepoisAutenticar(fpToken, fpValidadeToken);
end;

procedure TACBrPSP.VerificarAutenticacao;
begin
  if (not (Autenticado or EfetuarAutenticacaoManual)) then
  begin
    if (NivelLog > 2) then
      RegistrarLog('Autenticar');

    Autenticar;

    if Assigned(fpOnDepoisAutenticar) then
      fpOnDepoisAutenticar(fpToken, fpValidadeToken);
  end;

  VerificarValidadeToken;
end;

{ TACBrPixRecebedor }

constructor TACBrPixRecebedor.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrPixRecebedor.Clear;
begin
  fNome := '';
  fCidade := '';
  fCEP := '';
  fCodCategoriaComerciante := 0;
end;

procedure TACBrPixRecebedor.Assign(Source: TACBrPixRecebedor);
begin
  fNome := Source.Nome;
  fCidade := Source.Cidade;
  fCEP := Source.CEP;
  fCodCategoriaComerciante := Source.CodCategoriaComerciante;
end;

procedure TACBrPixRecebedor.SetCEP(AValue: String);
begin
  if (fCEP = AValue) then
    Exit;

  fCEP := OnlyNumber(AValue);
end;

procedure TACBrPixRecebedor.SetCidade(AValue: String);
begin
  if (fCidade = AValue) then
    Exit;

  fCidade := TiraAcentos(copy(Trim(AValue),1,15));
end;

procedure TACBrPixRecebedor.SetNome(AValue: String);
begin
  if (fNome = AValue) then
    Exit;

  fNome := TiraAcentos(copy(Trim(AValue),1,25));
end;

procedure TACBrPixRecebedor.SetUF(AValue: String);
var
  s, e: String;
begin
  if fUF = AValue then
    Exit;

  s := Trim(AValue);
  if (s <> '') then
  begin
    e := ValidarUF(s);
    if (e <> '') then
      raise EACBrPixException.Create(ACBrStr(e));
  end;

  fUF := s;
end;

procedure TACBrPixRecebedor.SetCodCategoriaComerciante(AValue: Integer);
begin
  if (fCodCategoriaComerciante = AValue) then
    Exit;

  if (AValue <> 0) and (AValue < cMCCMinimo) or (AValue > cMCCMaximo) then
    raise EACBrPixException.Create(ACBrStr(sErrMCCOutOfRange));

  fCodCategoriaComerciante := AValue;
end;

{ TACBrPixDadosAutomacao }

constructor TACBrPixDadosAutomacao.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrPixDadosAutomacao.Clear;
begin
  fCNPJSoftwareHouse := '';
  fNomeAplicacao := '';
  fNomeSoftwareHouse := '';
  fVersaoAplicacao := '';
end;

procedure TACBrPixDadosAutomacao.Assign(Source: TACBrPixDadosAutomacao);
begin
  fCNPJSoftwareHouse := Source.CNPJSoftwareHouse;
  fNomeSoftwareHouse := Source.NomeSoftwareHouse;
  fNomeAplicacao := Source.NomeAplicacao;
  fVersaoAplicacao := Source.VersaoAplicacao;
end;

{ TACBrHttpProxy }

constructor TACBrHttpProxy.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrHttpProxy.Clear;
begin
  fHost := '';
  fPass := '';
  fPort := '';
  fUser := '';
end;

procedure TACBrHttpProxy.Assign(Source: TACBrHttpProxy);
begin
  fHost := Source.Host;
  fPass := Source.Pass;
  fPort := Source.Port;
  fUser := Source.User;
end;

{ TACBrPixCD }

constructor TACBrPixCD.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fRecebedor := TACBrPixRecebedor.Create;
  fDadosAutomacao := TACBrPixDadosAutomacao.Create;
  fProxy := TACBrHttpProxy.Create;

  fTimeOut := ChttpTimeOutDef;
  fArqLOG := '';
  fNivelLog := 1;
  fAmbiente := ambTeste;
  fQuandoGravarLog := Nil;
end;

destructor TACBrPixCD.Destroy;
begin
  fRecebedor.Free;
  fDadosAutomacao.Free;
  fProxy.Free;

  inherited Destroy;
end;

procedure TACBrPixCD.RegistrarLog(const ALinha: String; aNivelLog: Integer);
var
  Tratado: Boolean;
begin
  Tratado := False;
  if Assigned(fQuandoGravarLog) then
    fQuandoGravarLog(ALinha, Tratado);

  if not Tratado and (NivelLog >= aNivelLog) then
    GravarLogEmArquivo(ALinha);
end;

procedure TACBrPixCD.DispararExcecao(E: Exception);
begin
  if not Assigned(E) then
    Exit;

  RegistrarLog(E.ClassName + ': ' + E.Message);
  raise E;
end;

procedure TACBrPixCD.GravarLogEmArquivo(const ALinha: String);
begin
  if (fArqLOG = '') then
    Exit;

  WriteLog( fArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now) + ' - ' + ALinha);
end;

procedure TACBrPixCD.SetACBrPSP(AValue: TACBrPSP);
var
  va: TACBrPSP ;
  //s: String;
begin
  //DEBUG
  //s := 'Nil';
  //if AValue <> Nil then
  //  s := IfEmptyThen(AValue.Name, AValue.ClassName);
  //WriteLog('c:\temp\debug.log', 'TACBrPixCD.SetACBrPSP( '+s+' )');

  if (AValue = fPSP) then
    Exit;

  if (AValue <> Nil) then
    RegistrarLog('Atribuindo PSP: '+AValue.ClassName+', Nome: '+AValue.Name)
  else
    RegistrarLog('Atribuindo PSP: Nil');

  if Assigned(fPSP) then
    fPSP.RemoveFreeNotification(Self);

  va := fPSP;       // Usa outra variavel para evitar Loop Infinito,
  fPSP := AValue;   // na remoção da associação dos componentes

  if Assigned(va) then
    if Assigned(va.ACBrPixCD) then
      va.ACBrPixCD := Nil;

  if (AValue <> Nil) then
  begin
    AValue.FreeNotification(Self);
    AValue.ACBrPixCD := Self;
  end ;
end;

procedure TACBrPixCD.SetDadosAutomacao(AValue: TACBrPixDadosAutomacao);
begin
  if (fDadosAutomacao <> AValue) then
    fDadosAutomacao.Assign(AValue);
end;

procedure TACBrPixCD.SetProxy(AValue: TACBrHttpProxy);
begin
  if (fProxy <> AValue) then
    fProxy.Assign(AValue);
end;

procedure TACBrPixCD.SetRecebedor(AValue: TACBrPixRecebedor);
begin
  if (fRecebedor <> AValue) then
    fRecebedor.Assign(AValue);
end;

procedure TACBrPixCD.VerificarPSPAtribuido;
begin
  if not Assigned(fPSP) then
    DispararExcecao(EACBrPixException.Create(ACBrStr(sErroPSPNaoAtribuido)));
end;

procedure TACBrPixCD.Notification(AComponent: TComponent; Operation: TOperation);
begin
  //DEBUG
  //WriteLog('c:\temp\debug.log', 'TACBrPixCD.Notification('+
  //  IfEmptyThen(AComponent.Name, AComponent.ClassName)+', '+
  //  GetEnumName(TypeInfo(TOperation), integer(Operation) ) +' )');

  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (fPSP <> nil) and (AComponent is TACBrPSP) then
  begin
    RegistrarLog('Removendo PSP: '+fPSP.ClassName+', Nome: '+fPSP.Name);
    fPSP := nil ;
  end;
end;

function TACBrPixCD.GerarQRCodeEstatico(Valor: Currency;
  const infoAdicional: String; const TxId: String): String;
begin
  VerificarPSPAtribuido;
  Result := GerarQRCodeEstatico(fPSP.ChavePIX, Valor, infoAdicional, TxId);
end;

function TACBrPixCD.GerarQRCodeEstatico(const ChavePix: String;
  Valor: Currency; const infoAdicional: String; const TxId: String): String;
var
  Erros: String;
  QRCodeEstatico: TACBrPIXQRCodeEstatico;
  TipoChave: TACBrPIXTipoChave;
begin
  RegistrarLog('GerarQRCodeEstatico( '+FloatToString(Valor)+', '+
    ChavePix+', '+infoAdicional+', '+TxId+' )');

  Erros := '';
  if (fRecebedor.Nome = '') then
    Erros := Erros + sErroRecebedorNome + sLineBreak;

  if (fRecebedor.Cidade = '') then
    Erros := Erros + sErroRecebedorCidade + sLineBreak;

  if (ChavePix = '') then
    Erros := Erros + sErroPSPChavePIX + sLineBreak;

  TipoChave := DetectarTipoChave(ChavePix);
  if (TipoChave = tchNenhuma) then
    Erros := Erros + sErroPSPTipoChave + sLineBreak;

  if (Erros <> '') then
    DispararExcecao(EACBrPixException.Create(ACBrStr(Erros)));

  QRCodeEstatico := TACBrPIXQRCodeEstatico.Create;
  try
    QRCodeEstatico.Clear;
    QRCodeEstatico.MerchantName := fRecebedor.Nome;
    QRCodeEstatico.MerchantCity := fRecebedor.Cidade;
    QRCodeEstatico.PostalCode := fRecebedor.CEP;
    QRCodeEstatico.PixKey := ChavePix;
    QRCodeEstatico.TransactionAmount := Valor;
    QRCodeEstatico.AdditionalInfo := infoAdicional;
    QRCodeEstatico.TxId := TxId;

    Result := QRCodeEstatico.AsString;
    RegistrarLog('   '+Result);
  finally
    QRCodeEstatico.Free;
  end;
end;

function TACBrPixCD.GerarQRCodeDinamico(const Location: String;
  const TxID: String; const Valor: Currency): String;
var
  Erros: String;
  QRCodeDinamico: TACBrPIXQRCodeDinamico;
begin
  RegistrarLog('GerarQRCodeDinamico( ' + Location +
    IfThen(NaoEstaVazio(TxID), ', ' + TxID, EmptyStr) +
    IfThen(Valor > 0, ', ' + FloatToString(Valor), EmptyStr) + ' )');

  Erros := '';
  if (fRecebedor.Nome = '') then
    Erros := Erros + sErroRecebedorNome + sLineBreak;

  if (fRecebedor.Cidade = '') then
    Erros := Erros + sErroRecebedorCidade + sLineBreak;

  if (Erros <> '') then
    DispararExcecao(EACBrPixException.Create(ACBrStr(Erros)));

  QRCodeDinamico := TACBrPIXQRCodeDinamico.Create;
  try
    QRCodeDinamico.Clear;
    QRCodeDinamico.MerchantName := fRecebedor.Nome;
    QRCodeDinamico.MerchantCity := fRecebedor.Cidade;
    QRCodeDinamico.PostalCode := fRecebedor.CEP;
    QRCodeDinamico.URL := Location;

    if NaoEstaVazio(TxID) then
      QRCodeDinamico.TxId := TxID;
    if (Valor <> 0) then
      QRCodeDinamico.TransactionAmount := Valor;

    Result := QRCodeDinamico.AsString;
    RegistrarLog('   '+Result);
  finally
    QRCodeDinamico.Free;
  end;
end;

end.

