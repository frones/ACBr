{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Wemerson Souto                                  }
{                              André Ferreira de Moraes                        }
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

unit ACBrNFeWebServices;

interface

uses
  Classes, SysUtils, dateutils,
  ACBrDFe, ACBrDFeWebService,
  ACBrDFeUtil,
  blcksock, synacode,
  pcnNFe, pcnRetConsReciDFe,
  ACBrDFeComum.RetConsCad,
  ACBrDFeConsts,
  pcnConversao,
  ACBrNFe.AdmCSC,
  ACBrNFe.RetAdmCSC,
  ACBrNFe.RetConsSit,
  pcnNFeConsts,
  pcnConversaoNFe, pcnProcNFe, pcnEnvEventoNFe,
  ACBrNFe.RetEnvEvento,
  pcnDistDFeInt, pcnRetDistDFeInt, pcnRetEnvNFe,
  ACBrNFeNotasFiscais, ACBrNFeConfiguracoes;

type

  { TNFeWebService }

  TNFeWebService = class(TDFeWebService)
  private
    FOldSSLType: TSSLType;
    FOldHeaderElement: String;
  protected
    FPStatus: TStatusACBrNFe;
    FPLayout: TLayOut;
    FPConfiguracoesNFe: TConfiguracoesNFe;

    procedure ConfigurarSoapDEPC;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    procedure DefinirDadosIntegrador; override;
    function GerarVersaoDadosSoap: String; override;
    procedure EnviarDados; override;
    procedure FinalizarServico; override;
    function ModeloDFe(const Chave: string): TpcnModeloDF;

  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusACBrNFe read FPStatus;
    property Layout: TLayOut read FPLayout;
  end;

  { TNFeStatusServico }

  TNFeStatusServico = class(TNFeWebService)
  private
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FxMotivo: String;
    FcUF: integer;
    FdhRecbto: TDateTime;
    FTMed: integer;
    FdhRetorno: TDateTime;
    FxObs: String;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosIntegrador; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    procedure Clear; override;

    property versao: String read Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property cUF: integer read FcUF;
    property dhRecbto: TDateTime read FdhRecbto;
    property TMed: integer read FTMed;
    property dhRetorno: TDateTime read FdhRetorno;
    property xObs: String read FxObs;
  end;

  { TNFeRecepcao }

  TNFeRecepcao = class(TNFeWebService)
  private
    FLote: String;
    FRecibo: String;
    FNotasFiscais: TNotasFiscais;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;
    FxMotivo: String;
    FdhRecbto: TDateTime;
    FTMed: integer;
    FProtocolo: string;
    FSincrono: Boolean;
    FZipado: Boolean;
    FVersaoDF: TpcnVersaoDF;

    FNFeRetornoSincrono: TRetConsSitNFe;
    FNFeRetorno: TretEnvNFe;

    function GetLote: String;
    function GetRecibo: String;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosIntegrador; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property Recibo: String read GetRecibo;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;
    property xMotivo: String read FxMotivo;
    property dhRecbto: TDateTime read FdhRecbto;
    property TMed: integer read FTMed;
    property Protocolo: string read FProtocolo;
    property Lote: String read GetLote write FLote;
    property Sincrono: Boolean read FSincrono write FSincrono;
    property Zipado: Boolean read FZipado write FZipado;
  end;

  { TNFeRetRecepcao }

  TNFeRetRecepcao = class(TNFeWebService)
  private
    FRecibo: String;
    FProtocolo: String;
    FChaveNFe: String;
    FNotasFiscais: TNotasFiscais;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;
    FxMotivo: String;
    FcMsg: integer;
    FxMsg: String;
    FVersaoDF: TpcnVersaoDF;

    FNFeRetorno: TRetConsReciDFe;

    function GetRecibo: String;
    function TratarRespostaFinal: Boolean;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosIntegrador; override;
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

    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;
    property xMotivo: String read FxMotivo;
    property cMsg: integer read FcMsg;
    property xMsg: String read FxMsg;
    property Recibo: String read GetRecibo write FRecibo;
    property Protocolo: String read FProtocolo write FProtocolo;
    property ChaveNFe: String read FChaveNFe write FChaveNFe;

    property NFeRetorno: TRetConsReciDFe read FNFeRetorno;
  end;

  { TNFeRecibo }

  TNFeRecibo = class(TNFeWebService)
  private
    FNotasFiscais: TNotasFiscais;
    FRecibo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FxMotivo: String;
    FcUF: integer;
    FxMsg: String;
    FcMsg: integer;
    FVersaoDF: TpcnVersaoDF;

    FNFeRetorno: TRetConsReciDFe;
  protected
    procedure InicializarServico; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirURL; override;
    procedure DefinirDadosIntegrador; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property cUF: integer read FcUF;
    property xMsg: String read FxMsg;
    property cMsg: integer read FcMsg;
    property Recibo: String read FRecibo write FRecibo;

    property NFeRetorno: TRetConsReciDFe read FNFeRetorno;
  end;

  { TNFeConsulta }

  TNFeConsulta = class(TNFeWebService)
  private
    FOwner: TACBrDFe;
    FNFeChave: String;
    FExtrairEventos: Boolean;
    FNotasFiscais: TNotasFiscais;
    FProtocolo: String;
    FDhRecbto: TDateTime;
    FXMotivo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;
    FRetNFeDFe: String;

    FprotNFe: TProcNFe;
    FretCancNFe: TRetCancNFe;
    FprocEventoNFe: TRetEventoNFeCollection;

    procedure SetNFeChave(const AValue: String);
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosIntegrador; override;
    procedure DefinirDadosMsg; override;
    function GerarUFSoap: String; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property NFeChave: String read FNFeChave write SetNFeChave;
    property ExtrairEventos: Boolean read FExtrairEventos write FExtrairEventos;
    property Protocolo: String read FProtocolo;
    property DhRecbto: TDateTime read FDhRecbto;
    property XMotivo: String read FXMotivo;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;
    property RetNFeDFe: String read FRetNFeDFe;

    property protNFe: TProcNFe read FprotNFe;
    property retCancNFe: TRetCancNFe read FretCancNFe;
    property procEventoNFe: TRetEventoNFeCollection read FprocEventoNFe;
  end;

  { TNFeInutilizacao }

  TNFeInutilizacao = class(TNFeWebService)
  private
    FID: String;
    FProtocolo: String;
    FModelo: integer;
    FSerie: integer;
    FCNPJ: String;
    FAno: integer;
    FNumeroInicial: integer;
    FNumeroFinal: integer;
    FJustificativa: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FxMotivo: String;
    FcUF: integer;
    FdhRecbto: TDateTime;
    FNomeArquivo: String;

    FXML_ProcInutNFe: String;

    procedure SetJustificativa(const AValue: String);
    function GerarPathPorCNPJ: String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosIntegrador; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    procedure Clear; override;

    property ID: String read FID write FID;
    property Protocolo: String read FProtocolo write FProtocolo;
    property Modelo: integer read FModelo write FModelo;
    property Serie: integer read FSerie write FSerie;
    property CNPJ: String read FCNPJ write FCNPJ;
    property Ano: integer read FAno write FAno;
    property NumeroInicial: integer read FNumeroInicial write FNumeroInicial;
    property NumeroFinal: integer read FNumeroFinal write FNumeroFinal;
    property Justificativa: String read FJustificativa write SetJustificativa;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property cUF: integer read FcUF;
    property dhRecbto: TDateTime read FdhRecbto;
    property XML_ProcInutNFe: String read FXML_ProcInutNFe;
    property NomeArquivo: String read FNomeArquivo write FNomeArquivo;
  end;

  { TNFeConsultaCadastro }

  TNFeConsultaCadastro = class(TNFeWebService)
  private
    FOldBodyElement: String;

    Fversao: String;
    FverAplic: String;
    FcStat: integer;
    FxMotivo: String;
    FUF: String;
    FIE: String;
    FCNPJ: String;
    FCPF: String;
    FcUF: integer;
    FdhCons: TDateTime;

    FRetConsCad: TRetConsCad;

    procedure SetCNPJ(const Value: String);
    procedure SetCPF(const Value: String);
    procedure SetIE(const Value: String);
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosIntegrador; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;

    function GerarMsgLog: String; override;
    function GerarUFSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    property versao: String read Fversao;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property DhCons: TDateTime read FdhCons;
    property cUF: integer read FcUF;
    property UF: String read FUF write FUF;
    property IE: String read FIE write SetIE;
    property CNPJ: String read FCNPJ write SetCNPJ;
    property CPF: String read FCPF write SetCPF;

    property RetConsCad: TRetConsCad read FRetConsCad;
  end;

  { TNFeEnvEvento }

  TNFeEnvEvento = class(TNFeWebService)
  private
    FidLote: Int64;
    FEvento: TEventoNFe;
    FcStat: integer;
    FxMotivo: String;
    FTpAmb: TpcnTipoAmbiente;
    FCNPJ: String;
    FIE: String;

    FEventoRetorno: TRetEventoNFe;

    function GerarPathEvento(const ACNPJ: String = ''; const AIE: String = ''): String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosIntegrador; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; AEvento: TEventoNFe); reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property idLote: Int64 read FidLote write FidLote;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;

    property EventoRetorno: TRetEventoNFe read FEventoRetorno;
  end;

  { TAdministrarCSCNFCe }

  TAdministrarCSCNFCe = class(TNFeWebService)
  private
    FRaizCNPJ: String;
    FindOp: TpcnIndOperacao;
    FIdCSC: integer;
    FCodigoCSC: String;

    FretAdmCSCNFCe: TRetAdmCSCNFCe;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    property RaizCNPJ: String read FRaizCNPJ write FRaizCNPJ;
    property indOP: TpcnIndOperacao read FindOP write FindOP;
    property idCsc: integer read FidCsc write FidCsc;
    property codigoCsc: String read FcodigoCsc write FcodigoCsc;

    property retAdmCSCNFCe: TRetAdmCSCNFCe read FretAdmCSCNFCe;
  end;

  { TDistribuicaoDFe }

  TDistribuicaoDFe = class(TNFeWebService)
  private
    FcUFAutor: integer;
    FCNPJCPF: String;
    FultNSU: String;
    FNSU: String;
    FchNFe: String;
    FNomeArq: String;
    FlistaArqs: TStringList;

    FretDistDFeInt: TretDistDFeInt;

    function GerarPathDistribuicao(AItem :TdocZipCollectionItem): String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    property cUFAutor: integer read FcUFAutor write FcUFAutor;
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property ultNSU: String read FultNSU write FultNSU;
    property NSU: String read FNSU write FNSU;
    property chNFe: String read FchNFe write FchNFe;
    property NomeArq: String read FNomeArq;
    property ListaArqs: TStringList read FlistaArqs;

    property retDistDFeInt: TretDistDFeInt read FretDistDFeInt;
  end;

  { TNFeEnvioWebService }

  TNFeEnvioWebService = class(TNFeWebService)
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

    property Versao: String read FVersao;
    property XMLEnvio: String read FXMLEnvio write FXMLEnvio;
    property URLEnvio: String read FPURLEnvio write FPURLEnvio;
    property SoapActionEnvio: String read FSoapActionEnvio write FSoapActionEnvio;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrNFe: TACBrDFe;
    FStatusServico: TNFeStatusServico;
    FEnviar: TNFeRecepcao;
    FRetorno: TNFeRetRecepcao;
    FRecibo: TNFeRecibo;
    FConsulta: TNFeConsulta;
    FInutilizacao: TNFeInutilizacao;
    FConsultaCadastro: TNFeConsultaCadastro;
    FEnvEvento: TNFeEnvEvento;
    FAdministrarCSCNFCe: TAdministrarCSCNFCe;
    FDistribuicaoDFe: TDistribuicaoDFe;
    FEnvioWebService: TNFeEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia(ALote: Int64; const ASincrono: Boolean = False; AZipado: Boolean = False): Boolean;
      overload;
    function Envia(const ALote: String; const ASincrono: Boolean = False; AZipado: Boolean = False): Boolean;
      overload;
    procedure Inutiliza(const ACNPJ, AJustificativa: String;
      Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer);

    property ACBrNFe: TACBrDFe read FACBrNFe write FACBrNFe;
    property StatusServico: TNFeStatusServico read FStatusServico write FStatusServico;
    property Enviar: TNFeRecepcao read FEnviar write FEnviar;
    property Retorno: TNFeRetRecepcao read FRetorno write FRetorno;
    property Recibo: TNFeRecibo read FRecibo write FRecibo;
    property Consulta: TNFeConsulta read FConsulta write FConsulta;
    property Inutilizacao: TNFeInutilizacao read FInutilizacao write FInutilizacao;
    property ConsultaCadastro: TNFeConsultaCadastro
      read FConsultaCadastro write FConsultaCadastro;
    property EnvEvento: TNFeEnvEvento read FEnvEvento write FEnvEvento;
    property AdministrarCSCNFCe: TAdministrarCSCNFCe
      read FAdministrarCSCNFCe write FAdministrarCSCNFCe;
    property DistribuicaoDFe: TDistribuicaoDFe
      read FDistribuicaoDFe write FDistribuicaoDFe;
    property EnvioWebService: TNFeEnvioWebService
      read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime, ACBrUtil.XMLHTML,
  ACBrUtil.FilesIO,
  ACBrCompress, ACBrNFe, ACBrConsts,
  pcnGerador,
  ACBrDFeComum.ConsCad,
  ACBrDFeComum.ConsStatServ,
  ACBrDFeComum.RetConsStatServ,
  ACBrNFe.ConsSit,
  ACBrNFe.Inut,
  ACBrNFe.RetInut,
  pcnConsReciDFe,
  pcnLeitor, ACBrIntegrador;

{ TNFeWebService }

constructor TNFeWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesNFe := TConfiguracoesNFe(FPConfiguracoes);
  FPLayout := LayNfeStatusServico;
end;

procedure TNFeWebService.Clear;
begin
  inherited Clear;

  FPStatus := stIdle;
  if Assigned(FPDFeOwner) and Assigned(FPDFeOwner.SSL) then
    FPDFeOwner.SSL.UseCertificateHTTP := True;
end;

procedure TNFeWebService.ConfigurarSoapDEPC;
begin
  FPSoapVersion := 'soap';
  FPHeaderElement := 'sceCabecMsg';
  FPSoapEnvelopeAtributtes :=
    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
    'xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
    'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/';
  FPBodyElement := 'sceDadosMsg';
end;

procedure TNFeWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  FOldSSLType := FPDFeOwner.SSL.SSLType;
  FOldHeaderElement := FPHeaderElement;

  if FPConfiguracoesNFe.Geral.VersaoDF >= ve400 then
    FPHeaderElement := ''; //Versão 4.00 não tem o elemento <soap12:Header>

  TACBrNFe(FPDFeOwner).SetStatus(FPStatus);
end;

function TNFeWebService.ModeloDFe(const Chave: string): TpcnModeloDF;
var
  Ok: Boolean;
  xChave: string;
begin
  xChave := Trim(Chave);
  if xChave <> '' then
  begin
    result := StrToModeloDF(Ok, ExtrairModeloChaveAcesso(xChave));
    if Ok then
      Exit;
  end;
  result := FPConfiguracoesNFe.Geral.ModeloDF;
end;

procedure TNFeWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';
  FPServico := '';
  FPSoapAction := '';

  TACBrNFe(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL, FPServico, FPSoapAction);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

procedure TNFeWebService.DefinirDadosIntegrador;
begin
  inherited DefinirDadosIntegrador;

  if Assigned(FPDFeOwner.Integrador) then
    FPDFeOwner.Integrador.NomeComponente := UpperCase(ModeloDFToPrefixo(FPConfiguracoesNFe.Geral.ModeloDF));
end;


function TNFeWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrNFe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TNFeWebService.EnviarDados;
var
  UsaIntegrador: Boolean;
  Integrador: TACBrIntegrador;
begin
  UsaIntegrador := Assigned(FPDFeOwner.Integrador);

  Integrador := Nil;
  if UsaIntegrador and (FPConfiguracoesNFe.Geral.ModeloDF = moNFe) then
  begin
    Integrador := FPDFeOwner.Integrador;
    FPDFeOwner.Integrador := Nil;
  end;

  try
    inherited EnviarDados;
  finally
    if Assigned(Integrador) then
      FPDFeOwner.Integrador := Integrador;
  end;
end;

procedure TNFeWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  // Retornar configurações anteriores
  FPDFeOwner.SSL.SSLType := FOldSSLType;
  FPHeaderElement := FOldHeaderElement;

  TACBrNFe(FPDFeOwner).SetStatus(stIdle);
end;

{ TNFeStatusServico }

procedure TNFeStatusServico.Clear;
begin
  inherited Clear;

  FPStatus := stNFeStatusServico;
  FPLayout := LayNfeStatusServico;
  FPArqEnv := 'ped-sta';
  FPArqResp := 'sta';

  Fversao := '';
  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  FdhRecbto := 0;
  FTMed := 0;
  FdhRetorno := 0;
  FxObs := '';

  if Assigned(FPConfiguracoesNFe) then
  begin
    FtpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
    FcUF := FPConfiguracoesNFe.WebServices.UFCodigo;
  end
end;

procedure TNFeStatusServico.DefinirServicoEAction;
begin
  if (FPConfiguracoesNFe.Geral.VersaoDF >= ve400) then
  begin
    if EstaVazio(FPServico) then
      FPServico := GetUrlWsd + 'NFeStatusServico4';
    if EstaVazio(FPSoapAction) then
      FPSoapAction := FPServico + '/nfeStatusServicoNF';
  end
  // BA usa uma notação de Serviços diferente das demais UFs
  else if (FPConfiguracoesNFe.WebServices.UFCodigo = 29) and // 29 = BA
     (FPConfiguracoesNFe.Geral.ModeloDF = moNFe) and
     (FPConfiguracoesNFe.Geral.VersaoDF = ve310) and
     (FPConfiguracoesNFe.Geral.FormaEmissao = teNormal) then
  begin
    FPServico := GetUrlWsd + 'NfeStatusServico';
    FPSoapAction := FPServico + '/NfeStatusServicoNF';
  end
  else
  begin
    FPServico := GetUrlWsd + 'NfeStatusServico2';
    FPSoapAction := FPServico;
  end;
end;

procedure TNFeStatusServico.DefinirDadosIntegrador;
begin
  inherited DefinirDadosIntegrador;

  if Assigned(FPDFeOwner.Integrador) then
  begin
    FPDFeOwner.Integrador.Parametros.Values['versaoDados'] :=  VersaoDFToStr(FPConfiguracoesNFe.Geral.VersaoDF);
    FPDFeOwner.Integrador.SetNomeMetodo('NfeStatusServico2Soap12', (FPConfiguracoesNFe.WebServices.Ambiente = taHomologacao) );
  end;
end;

procedure TNFeStatusServico.DefinirDadosMsg;
var
  ConsStatServ: TConsStatServ;
begin
  ConsStatServ := TConsStatServ.Create(FPVersaoServico, NAME_SPACE, '', True);
  try
    ConsStatServ.TpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
    ConsStatServ.CUF := FPConfiguracoesNFe.WebServices.UFCodigo;

    FPDadosMsg := ConsStatServ.GerarXML;
  finally
    ConsStatServ.Free;
  end;
end;

function TNFeStatusServico.TratarResposta: Boolean;
var
  NFeRetorno: TRetConsStatServ;
begin
  FPRetWS := SeparaDadosArray(['nfeStatusServicoNF2Result',
                               'NfeStatusServicoNFResult',
                               'nfeResultMsg'],FPRetornoWS );

  VerificarSemResposta;

  NFeRetorno := TRetConsStatServ.Create('');
  try
    NFeRetorno.XmlRetorno := ParseText(FPRetWS);
    NFeRetorno.LerXml;

    Fversao := NFeRetorno.versao;
    FtpAmb := TpcnTipoAmbiente(NFeRetorno.tpAmb);
    FverAplic := NFeRetorno.verAplic;
    FcStat := NFeRetorno.cStat;
    FxMotivo := NFeRetorno.xMotivo;
    FcUF := NFeRetorno.cUF;
    { WebService do RS retorna horário de verão mesmo pros estados que não
      adotam esse horário, ao utilizar esta hora para basear a emissão da nota
      acontece o erro. }
    if (pos('svrs.rs.gov.br', FPURL) > 0) and
       (MinutesBetween(NFeRetorno.dhRecbto, Now) > 50) and
       (not IsHorarioDeVerao(CUFtoUF(FcUF), NFeRetorno.dhRecbto)) then
      FdhRecbto:= IncHour(NFeRetorno.dhRecbto,-1)
    else
      FdhRecbto := NFeRetorno.dhRecbto;

    FTMed := NFeRetorno.TMed;
    FdhRetorno := NFeRetorno.dhRetorno;
    FxObs := NFeRetorno.xObs;
    FPMsg := FxMotivo + LineBreak + FxObs;

    if Assigned(FPConfiguracoesNFe) and
       Assigned(FPConfiguracoesNFe.WebServices) and
       FPConfiguracoesNFe.WebServices.AjustaAguardaConsultaRet then
      FPConfiguracoesNFe.WebServices.AguardarConsultaRet := FTMed * 1000;

    Result := (FcStat = 107);

  finally
    NFeRetorno.Free;
  end;
end;

function TNFeStatusServico.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s' + LineBreak +
                           'Status Descrição: %s' + LineBreak +
                           'UF: %s' + LineBreak +
                           'Recebimento: %s' + LineBreak +
                           'Tempo Médio: %s' + LineBreak +
                           'Retorno: %s' + LineBreak +
                           'Observação: %s' + LineBreak),
                   [Fversao, TpAmbToStr(FtpAmb), FverAplic, IntToStr(FcStat),
                    FxMotivo, CodigoUFparaUF(FcUF),
                    IfThen(FdhRecbto = 0, '', FormatDateTimeBr(FdhRecbto)),
                    IntToStr(FTMed),
                    IfThen(FdhRetorno = 0, '', FormatDateTimeBr(FdhRetorno)),
                    FxObs]);
  {*)}
end;

function TNFeStatusServico.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Consulta Status serviço:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TNFeRecepcao }

constructor TNFeRecepcao.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
  FSincrono := False;
  FZipado := False;
end;

destructor TNFeRecepcao.Destroy;
begin
  FNFeRetornoSincrono.Free;
  FNFeRetorno.Free;

  inherited Destroy;
end;

procedure TNFeRecepcao.Clear;
begin
  inherited Clear;

  FPStatus := stNFeRecepcao;
  FPLayout := LayNfeRecepcao;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

  Fversao    := '';
  FTMed      := 0;
  FverAplic  := '';
  FcStat     := 0;
  FxMotivo   := '';
  FRecibo    := '';
  FdhRecbto  := 0;
  FProtocolo := '';

  if Assigned(FPConfiguracoesNFe) then
  begin
    FtpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
    FcUF := FPConfiguracoesNFe.WebServices.UFCodigo;
  end;

  if Assigned(FNFeRetornoSincrono) then
    FNFeRetornoSincrono.Free;

  if Assigned(FNFeRetorno) then
    FNFeRetorno.Free;

  FNFeRetornoSincrono := TRetConsSitNFe.Create(FPVersaoServico);
  FNFeRetorno := TretEnvNFe.Create;
end;

function TNFeRecepcao.GetLote: String;
begin
  Result := Trim(FLote);
end;

function TNFeRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

procedure TNFeRecepcao.InicializarServico;
var
  ok: Boolean;
begin
  // Alteração visando atender a NT 2023/002 que elimina o envio em Lote de NFC-e
  // a partir de 04/09/2023 - Produção
  if FPConfiguracoesNFe.Geral.ModeloDF = moNFCe then
  begin
    Sincrono := True;

    if FNotasFiscais.Count > 1 then
      GerarException(ACBrStr('ERRO: Conjunto de NFC-e transmitidas (máximo de 1 NFC-e)' +
        ' excedido. Quantidade atual: ' + IntToStr(FNotasFiscais.Count)));
  end
  else
  begin
    if FNotasFiscais.Count > 50 then
      GerarException(ACBrStr('ERRO: Conjunto de NF-e transmitidas (máximo de 50 NF-e)' +
        ' excedido. Quantidade atual: ' + IntToStr(FNotasFiscais.Count)));
  end;

  if FNotasFiscais.Count > 0 then    // Tem NFe ? Se SIM, use as informações do XML
    FVersaoDF := DblToVersaoDF(ok, FNotasFiscais.Items[0].NFe.infNFe.Versao)
  else
    FVersaoDF := FPConfiguracoesNFe.Geral.VersaoDF;

  inherited InicializarServico;

  if FVersaoDF >= ve400 then
    FPHeaderElement := ''  //Versão 4.00 não tem o elemento <soap12:Header>
  else
    FPHeaderElement := FOldHeaderElement;
end;

procedure TNFeRecepcao.DefinirURL;
var
  xUF: String;
  VerServ: Double;
  Modelo: TpcnModeloDF;
  ok: Boolean;
begin
  if FNotasFiscais.Count > 0 then    // Tem NFe ? Se SIM, use as informações do XML
  begin
    Modelo  := StrToModeloDF(ok, IntToStr(FNotasFiscais.Items[0].NFe.Ide.modelo));
    FcUF    := FNotasFiscais.Items[0].NFe.Ide.cUF;

    if FPConfiguracoesNFe.WebServices.Ambiente <> FNotasFiscais.Items[0].NFe.Ide.tpAmb then
      raise EACBrNFeException.Create( CErroAmbienteDiferente );
  end
  else
  begin                              // Se não tem NFe, use as configurações do componente
    Modelo  := FPConfiguracoesNFe.Geral.ModeloDF;
    FcUF    := FPConfiguracoesNFe.WebServices.UFCodigo;
  end;

  VerServ := VersaoDFToDbl(FVersaoDF);
  FTpAmb  := FPConfiguracoesNFe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  if TACBrNFe(FPDFeOwner).EhAutorizacao(FVersaoDF, Modelo, FcUF) then
    FPLayout := LayNfeAutorizacao
  else
    FPLayout := LayNfeRecepcao;

  // Configuração correta ao enviar para o SVC
  case FPConfiguracoesNFe.Geral.FormaEmissao of
    teSVCAN: xUF := 'SVC-AN';
    teSVCRS: xUF := 'SVC-RS';
  else
    xUF := CUFtoUF(FcUF);
  end;

  TACBrNFe(FPDFeOwner).LerServicoDeParams(
    ModeloDFToPrefixo(Modelo),
    xUF,
    FTpAmb,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFeRecepcao.DefinirServicoEAction;
begin
  if FPLayout = LayNfeAutorizacao then
  begin
    if (FVersaoDF >= ve400)  then
    begin
      if EstaVazio(FPServico) then
        FPServico := GetUrlWsd + 'NFeAutorizacao4';
      if EstaVazio(FPSoapAction) then
        FPSoapAction := FPServico + '/nfeAutorizacaoLote';
    end
    else
    begin
      FPServico := GetUrlWsd + 'NfeAutorizacao';
      FPSoapAction := FPServico;
    end;
  end
  else
  begin
    FPServico := GetUrlWsd + 'NfeRecepcao2';
    FPSoapAction := FPServico;
  end;

  if FZipado then
  begin
    if (FVersaoDF >= ve400) then
      FPSoapAction := FPSoapAction+'ZIP'
    else
      FPSoapAction := FPSoapAction+'LoteZip';

    FPBodyElement:= 'nfeDadosMsgZip';
  end
  else
    FPBodyElement := 'nfeDadosMsg';
end;

procedure TNFeRecepcao.DefinirDadosIntegrador;
begin
  inherited DefinirDadosIntegrador;

  if Assigned(FPDFeOwner.Integrador) then
  begin
    //Atualmente não é possível enviar em LOTE com o Integrador
    FPDFeOwner.Integrador.Parametros.Values['versaoDados']        :=  StringReplace(FormatFloat('0.00',FNotasFiscais.Items[0].NFe.infNFe.Versao),',','.',[rfReplaceAll]);
    FPDFeOwner.Integrador.Parametros.Values['NumeroNFCe']         := OnlyNumber(FNotasFiscais.Items[0].NFe.infNFe.ID);
    FPDFeOwner.Integrador.Parametros.Values['DataHoraNFCeGerado'] := FormatDateTime('yyyymmddhhnnss', FNotasFiscais.Items[0].NFe.Ide.dEmi);
    FPDFeOwner.Integrador.Parametros.Values['ValorNFCe']          := StringReplace(FormatFloat('0.00',FNotasFiscais.Items[0].NFe.Total.ICMSTot.vNF),',','.',[rfReplaceAll]);
    FPDFeOwner.Integrador.SetNomeMetodo('NfeAutorizacaoLote12', (FPConfiguracoesNFe.WebServices.Ambiente = taHomologacao) );
  end;
end;

procedure TNFeRecepcao.DefinirDadosMsg;
var
  I: integer;
  vNotas: String;
  indSinc: String;
begin
  if (FPLayout = LayNfeAutorizacao) or (FPConfiguracoesNFe.Geral.ModeloDF = moNFCe) or
    (FVersaoDF >= ve310) then
    indSinc := '<indSinc>' + IfThen(FSincrono, '1', '0') + '</indSinc>'
  else
    indSinc := '';

  vNotas := '';
  for I := 0 to FNotasFiscais.Count - 1 do
    vNotas := vNotas + '<NFe' + RetornarConteudoEntre(
      FNotasFiscais.Items[I].XMLAssinado, '<NFe', '</NFe>') + '</NFe>';

  FPDadosMsg := '<enviNFe xmlns="'+ACBRNFE_NAMESPACE+'" versao="' +
    FPVersaoServico + '">' + '<idLote>' + FLote + '</idLote>' + indSinc +
    vNotas + '</enviNFe>';

  if FZipado then
    FPDadosMsg := EncodeBase64(GZipCompress(CUTF8DeclaracaoXML+FPDadosMsg));

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));

  FRecibo := '';
end;

function TNFeRecepcao.TratarResposta: Boolean;
var
  I: integer;
  chNFe, AXML, NomeXMLSalvo: String;
  AProcNFe: TProcNFe;
  SalvarXML: Boolean;
begin
  FPRetWS := SeparaDadosArray(['nfeAutorizacaoLoteResult',
                               'nfeAutorizacaoResult',
                               'nfeAutorizacaoLoteZipResult',
                               'nfeResultMsg',
                               'nfeResultMsgZip',
                               'nfeRecepcaoLote2Result'],FPRetornoWS );

  VerificarSemResposta;

  if ((FPConfiguracoesNFe.Geral.ModeloDF = moNFCe) or (FVersaoDF >= ve310)) and FSincrono then
  begin
    if pos('retEnviNFe', FPRetWS) > 0 then
      AXML := StringReplace(FPRetWS, 'retEnviNFe', 'retConsSitNFe',
                                     [rfReplaceAll, rfIgnoreCase])
    else if pos('retConsReciNFe', FPRetWS) > 0 then
      AXML := StringReplace(FPRetWS, 'retConsReciNFe', 'retConsSitNFe',
                                     [rfReplaceAll, rfIgnoreCase])
    else
      AXML := FPRetWS;

    FNFeRetornoSincrono.XmlRetorno := ParseText(AXML);
    FNFeRetornoSincrono.LerXml;

    Fversao := FNFeRetornoSincrono.versao;
    FTpAmb := FNFeRetornoSincrono.TpAmb;
    FverAplic := FNFeRetornoSincrono.verAplic;

    // Consta no Retorno da NFC-e
    FRecibo := FNFeRetornoSincrono.nRec;
    FcUF := FNFeRetornoSincrono.cUF;
    chNFe := FNFeRetornoSincrono.ProtNFe.chDFe;

    if (FNFeRetornoSincrono.protNFe.cStat > 0) then
      FcStat := FNFeRetornoSincrono.protNFe.cStat
    else
      FcStat := FNFeRetornoSincrono.cStat;

    if (FNFeRetornoSincrono.protNFe.xMotivo <> '') then
    begin
      FPMsg := FNFeRetornoSincrono.protNFe.xMotivo;
      FxMotivo := FNFeRetornoSincrono.protNFe.xMotivo;
    end
    else
    begin
      FPMsg := FNFeRetornoSincrono.xMotivo;
      FxMotivo := FNFeRetornoSincrono.xMotivo;
    end;

    // Verificar se a NF-e foi autorizada com sucesso
    Result := (FNFeRetornoSincrono.cStat = 104) and
      (TACBrNFe(FPDFeOwner).CstatProcessado(FNFeRetornoSincrono.protNFe.cStat));

    if Result then
    begin
      // Pega o numero do protocolo
      FProtocolo := FNFeRetornoSincrono.protNFe.nProt;
      
      for I := 0 to TACBrNFe(FPDFeOwner).NotasFiscais.Count - 1 do
      begin
        with TACBrNFe(FPDFeOwner).NotasFiscais.Items[I] do
        begin
          if OnlyNumber(chNFe) = NumID then
          begin
            if (FPConfiguracoesNFe.Geral.ValidarDigest) and
               (FNFeRetornoSincrono.protNFe.digVal <> '') and
               (NFe.signature.DigestValue <> FNFeRetornoSincrono.protNFe.digVal) then
            begin
              raise EACBrNFeException.Create('DigestValue do documento ' + NumID + ' não confere.');
            end;

            NFe.procNFe.cStat := FNFeRetornoSincrono.protNFe.cStat;
            NFe.procNFe.tpAmb := FNFeRetornoSincrono.tpAmb;
            NFe.procNFe.verAplic := FNFeRetornoSincrono.verAplic;
            NFe.procNFe.chNFe := FNFeRetornoSincrono.ProtNFe.chDFe;
            NFe.procNFe.dhRecbto := FNFeRetornoSincrono.protNFe.dhRecbto;
            NFe.procNFe.nProt := FNFeRetornoSincrono.ProtNFe.nProt;
            NFe.procNFe.digVal := FNFeRetornoSincrono.protNFe.digVal;
            NFe.procNFe.xMotivo := FNFeRetornoSincrono.protNFe.xMotivo;
            NFe.procNFe.cMsg := FNFeRetornoSincrono.protNFe.cMsg;
            NFe.procNFe.xMsg := FNFeRetornoSincrono.protNFe.xMsg;

            AProcNFe := TProcNFe.Create;
            try
              // Processando em UTF8, para poder gravar arquivo corretamente //
              AProcNFe.XML_NFe := RemoverDeclaracaoXML(XMLAssinado);
              AProcNFe.XML_Prot := FNFeRetornoSincrono.XMLprotNFe;
              AProcNFe.Versao := FPVersaoServico;
              AjustarOpcoes( AProcNFe.Gerador.Opcoes );
              AProcNFe.GerarXML;

              XMLOriginal := AProcNFe.Gerador.ArquivoFormatoXML;

              if FPConfiguracoesNFe.Arquivos.Salvar then
              begin
                SalvarXML := (not FPConfiguracoesNFe.Arquivos.SalvarApenasNFeProcessadas) or
                             Processada;

                // Salva o XML da NF-e assinado e protocolado
                if SalvarXML then
                begin
                  NomeXMLSalvo := '';
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                  begin
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal ); // Atualiza o XML carregado
                    NomeXMLSalvo := NomeArq;
                  end;

                  if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                    GravarXML; // Salva na pasta baseado nas configurações do PathNFe
                end;
              end ;
            finally
              AProcNFe.Free;
            end;

            Break;
          end;
        end;
      end;
    end;
  end
  else
  begin
    //A função UTF8ToNativeString deve ser removida quando for refatorado para usar ACBrXMLDocument
    FNFeRetorno.Leitor.Arquivo := UTF8ToNativeString(ParseText(FPRetWS));
    FNFeRetorno.LerXml;

    Fversao := FNFeRetorno.versao;
    FTpAmb := FNFeRetorno.TpAmb;
    FverAplic := FNFeRetorno.verAplic;
    FcStat := FNFeRetorno.cStat;
    FxMotivo := FNFeRetorno.xMotivo;
    FdhRecbto := FNFeRetorno.infRec.dhRecbto;
    FTMed := FNFeRetorno.infRec.tMed;
    FcUF := FNFeRetorno.cUF;
    FPMsg := FNFeRetorno.xMotivo;
    FRecibo := FNFeRetorno.infRec.nRec;

    Result := (FNFeRetorno.CStat = 103);
  end;
end;

function TNFeRecepcao.GerarMsgLog: String;
begin
  {(*}
  if FSincrono then
    Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + sLineBreak +
                           'dhRecbto: %s ' + sLineBreak +
                           'chNFe: %s ' + LineBreak),
                     [FNFeRetornoSincrono.versao,
                      TpAmbToStr(FNFeRetornoSincrono.TpAmb),
                      FNFeRetornoSincrono.verAplic,
                      IntToStr(FNFeRetornoSincrono.protNFe.cStat),
                      FNFeRetornoSincrono.protNFe.xMotivo,
                      CodigoUFparaUF(FNFeRetornoSincrono.cUF),
                      FormatDateTimeBr(FNFeRetornoSincrono.dhRecbto),
                      FNFeRetornoSincrono.chNfe])
  else
    Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                             'Ambiente: %s ' + LineBreak +
                             'Versão Aplicativo: %s ' + LineBreak +
                             'Status Código: %s ' + LineBreak +
                             'Status Descrição: %s ' + LineBreak +
                             'UF: %s ' + sLineBreak +
                             'Recibo: %s ' + LineBreak +
                             'Recebimento: %s ' + LineBreak +
                             'Tempo Médio: %s ' + LineBreak),
                     [FNFeRetorno.versao,
                      TpAmbToStr(FNFeRetorno.TpAmb),
                      FNFeRetorno.verAplic,
                      IntToStr(FNFeRetorno.cStat),
                      FNFeRetorno.xMotivo,
                      CodigoUFparaUF(FNFeRetorno.cUF),
                      FNFeRetorno.infRec.nRec,
                      IfThen(FNFeRetorno.InfRec.dhRecbto = 0, '',
                             FormatDateTimeBr(FNFeRetorno.InfRec.dhRecbto)),
                      IntToStr(FNFeRetorno.InfRec.TMed)]);
  {*)}
end;

function TNFeRecepcao.GerarPrefixoArquivo: String;
begin
  if FSincrono then  // Esta procesando nome do Retorno Sincrono ?
  begin
    if FRecibo <> '' then
    begin
      Result := Recibo;
      FPArqResp := 'pro-rec';
    end
    else
    begin
      Result := Lote;
      FPArqResp := 'pro-lot';
    end;
  end
  else
    Result := Lote;
end;

{ TNFeRetRecepcao }

constructor TNFeRetRecepcao.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFeRetRecepcao.Destroy;
begin
  FNFeRetorno.Free;

  inherited Destroy;
end;

function TNFeRetRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

procedure TNFeRetRecepcao.InicializarServico;
var
  ok: Boolean;
begin
  if FNotasFiscais.Count > 0 then    // Tem NFe ? Se SIM, use as informações do XML
    FVersaoDF := DblToVersaoDF(ok, FNotasFiscais.Items[0].NFe.infNFe.Versao)
  else
    FVersaoDF := FPConfiguracoesNFe.Geral.VersaoDF;

  inherited InicializarServico;

  if FVersaoDF >= ve400 then
    FPHeaderElement := '' //Versão 4.00 não tem o elemento <soap12:Header>
  else
    FPHeaderElement := FOldHeaderElement;
end;

procedure TNFeRetRecepcao.Clear;
var
  i, j: Integer;
begin
  inherited Clear;

  FPStatus := stNFeRetRecepcao;
  FPLayout := LayNfeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  Fversao := '';
  FxMsg := '';
  FcMsg := 0;

  if Assigned(FPConfiguracoesNFe) then
  begin
    FtpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
    FcUF := FPConfiguracoesNFe.WebServices.UFCodigo;
  end;

  if Assigned(FNFeRetorno) and Assigned(FNotasFiscais)
		and Assigned(FNFeRetorno.ProtDFe) then
  begin
    // Limpa Dados dos retornos das notas Fiscais;
    for i := 0 to FNFeRetorno.ProtDFe.Count - 1 do
    begin
      for j := 0 to FNotasFiscais.Count - 1 do
      begin
        if OnlyNumber(FNFeRetorno.ProtDFe.Items[i].chDFe) = FNotasFiscais.Items[J].NumID then
        begin
          FNotasFiscais.Items[j].NFe.procNFe.verAplic := '';
          FNotasFiscais.Items[j].NFe.procNFe.chNFe    := '';
          FNotasFiscais.Items[j].NFe.procNFe.dhRecbto := 0;
          FNotasFiscais.Items[j].NFe.procNFe.nProt    := '';
          FNotasFiscais.Items[j].NFe.procNFe.digVal   := '';
          FNotasFiscais.Items[j].NFe.procNFe.cStat    := 0;
          FNotasFiscais.Items[j].NFe.procNFe.xMotivo  := '';
          FNotasFiscais.Items[j].NFe.procNFe.cMsg     := 0;
          FNotasFiscais.Items[j].NFe.procNFe.xMsg     := '';

        end;
      end;
    end;
  end;

  if Assigned( FNFeRetorno ) then
    FreeAndNil(FNFeRetorno);

  FNFeRetorno := TRetConsReciDFe.Create('NFe');
end;

function TNFeRetRecepcao.Executar: Boolean;
var
  IntervaloTentativas, Tentativas: integer;
begin
  Result := False;

  TACBrNFe(FPDFeOwner).SetStatus(stNfeRetRecepcao);
  try
    Sleep(FPConfiguracoesNFe.WebServices.AguardarConsultaRet);

    Tentativas := 0;
    IntervaloTentativas := max(FPConfiguracoesNFe.WebServices.IntervaloTentativas, 1000);

    while (inherited Executar) and
      (Tentativas < FPConfiguracoesNFe.WebServices.Tentativas) do
    begin
      Inc(Tentativas);
      sleep(IntervaloTentativas);
    end;
  finally
    TACBrNFe(FPDFeOwner).SetStatus(stIdle);
  end;

  if FNFeRetorno.CStat = 104 then  // Lote processado ?
    Result := TratarRespostaFinal;
end;

procedure TNFeRetRecepcao.DefinirURL;
var
  xUF: String;
  VerServ: Double;
  ok: Boolean;
  Modelo: TpcnModeloDF;
begin
  if FNotasFiscais.Count > 0 then    // Tem NFe ? Se SIM, use as informações do XML
  begin
    Modelo  := StrToModeloDF(ok, IntToStr(FNotasFiscais.Items[0].NFe.Ide.modelo));
    FcUF    := FNotasFiscais.Items[0].NFe.Ide.cUF;

    if FPConfiguracoesNFe.WebServices.Ambiente <> FNotasFiscais.Items[0].NFe.Ide.tpAmb then
      raise EACBrNFeException.Create( CErroAmbienteDiferente );
  end
  else
  begin                              // Se não tem NFe, use as configurações do componente
    Modelo  := FPConfiguracoesNFe.Geral.ModeloDF;
    FcUF    := FPConfiguracoesNFe.WebServices.UFCodigo;
  end;

  VerServ := VersaoDFToDbl(FVersaoDF);
  FTpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  if TACBrNFe(FPDFeOwner).EhAutorizacao(FVersaoDF, Modelo, FcUF) then
    FPLayout := LayNfeRetAutorizacao
  else
    FPLayout := LayNfeRetRecepcao;

  // Configuração correta ao enviar para o SVC
  case FPConfiguracoesNFe.Geral.FormaEmissao of
    teSVCAN: xUF := 'SVC-AN';
    teSVCRS: xUF := 'SVC-RS';
  else
    xUF := CUFtoUF(FcUF);
  end;

  TACBrNFe(FPDFeOwner).LerServicoDeParams(
    ModeloDFToPrefixo(Modelo),
    xUF,
    FTpAmb,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFeRetRecepcao.DefinirServicoEAction;
begin
  if FPLayout = LayNfeRetAutorizacao then
  begin
    if (FVersaoDF >= ve400) then
    begin
      if EstaVazio(FPServico) then
        FPServico := GetUrlWsd + 'NFeRetAutorizacao4';
      if EstaVazio(FPSoapAction) then
        FPSoapAction := FPServico +'/nfeRetAutorizacaoLote';
    end
    else
    begin
      FPServico := GetUrlWsd + 'NfeRetAutorizacao';
      FPSoapAction := FPServico;
    end;
  end
  else
  begin
    FPServico := GetUrlWsd + 'NfeRetRecepcao2';
    FPSoapAction := FPServico;
  end;
end;

procedure TNFeRetRecepcao.DefinirDadosIntegrador;
begin
  inherited DefinirDadosIntegrador;

  if Assigned(FPDFeOwner.Integrador) then
  begin
    FPDFeOwner.Integrador.Parametros.Values['versaoDados'] :=  VersaoDFToStr(FPConfiguracoesNFe.Geral.VersaoDF);
    FPDFeOwner.Integrador.SetNomeMetodo('NfeRetAutorizacaoLote12', (FPConfiguracoesNFe.WebServices.Ambiente = taHomologacao) );
  end;
end;

procedure TNFeRetRecepcao.DefinirDadosMsg;
var
  ConsReciNFe: TConsReciDFe;
begin
  ConsReciNFe := TConsReciDFe.Create(FPVersaoServico, NAME_SPACE, 'NFe');
  try
    ConsReciNFe.tpAmb := FTpAmb;
    ConsReciNFe.nRec := FRecibo;

    AjustarOpcoes( ConsReciNFe.Gerador.Opcoes );
    ConsReciNFe.GerarXML;

    FPDadosMsg := ConsReciNFe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciNFe.Free;
  end;
end;

function TNFeRetRecepcao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['nfeRetAutorizacaoResult',
                               'nfeRetAutorizacaoLoteResult',
                               'nfeResultMsg',
                               'nfeRetRecepcao2Result'],FPRetornoWS );

  VerificarSemResposta;

  //A função UTF8ToNativeString deve ser removida quando for refatorado para usar ACBrXMLDocument
  FNFeRetorno.Leitor.Arquivo := UTF8ToNativeString(ParseText(FPRetWS));
  FNFeRetorno.LerXML;

  Fversao := FNFeRetorno.versao;
  FTpAmb := FNFeRetorno.TpAmb;
  FverAplic := FNFeRetorno.verAplic;
  FcStat := FNFeRetorno.cStat;
  FcUF := FNFeRetorno.cUF;
  FPMsg := FNFeRetorno.xMotivo;
  FxMotivo := FNFeRetorno.xMotivo;
  FcMsg := FNFeRetorno.cMsg;
  FxMsg := FNFeRetorno.xMsg;

  Result := (FNFeRetorno.CStat = 105); // Lote em Processamento
end;

function TNFeRetRecepcao.TratarRespostaFinal: Boolean;
var
  I, J: integer;
  AProcNFe: TProcNFe;
  AInfProt: TProtDFeCollection;
  SalvarXML: Boolean;
  NomeXMLSalvo: String;
begin
  Result := False;

  AInfProt := FNFeRetorno.ProtDFe;

  if (AInfProt.Count > 0) then
  begin
    FPMsg := FNFeRetorno.ProtDFe.Items[0].xMotivo;
    FxMotivo := FNFeRetorno.ProtDFe.Items[0].xMotivo;
  end;

  //Setando os retornos das notas fiscais;
  for I := 0 to AInfProt.Count - 1 do
  begin
    for J := 0 to FNotasFiscais.Count - 1 do
    begin
      if OnlyNumber(AInfProt.Items[I].chDFe) = FNotasFiscais.Items[J].NumID then
      begin
        if (FPConfiguracoesNFe.Geral.ValidarDigest) and
           (AInfProt.Items[I].digVal <> '') and
           (FNotasFiscais.Items[J].NFe.signature.DigestValue <> AInfProt.Items[I].digVal) then
        begin
          raise EACBrNFeException.Create('DigestValue do documento ' +
            FNotasFiscais.Items[J].NumID + ' não confere.');
        end;

        with FNotasFiscais.Items[J] do
        begin
          NFe.procNFe.tpAmb := AInfProt.Items[I].tpAmb;
          NFe.procNFe.verAplic := AInfProt.Items[I].verAplic;
          NFe.procNFe.chNFe := AInfProt.Items[I].chDFe;
          NFe.procNFe.dhRecbto := AInfProt.Items[I].dhRecbto;
          NFe.procNFe.nProt := AInfProt.Items[I].nProt;
          NFe.procNFe.digVal := AInfProt.Items[I].digVal;
          NFe.procNFe.cStat := AInfProt.Items[I].cStat;
          NFe.procNFe.xMotivo := AInfProt.Items[I].xMotivo;
          NFe.ProcNFe.cMsg := AInfProt.Items[I].cMsg;
          Nfe.ProcNFe.xMsg := AInfProt.Items[I].xMsg;
        end;

        // Monta o XML da NF-e assinado e com o protocolo de Autorização ou Denegação
        if (AInfProt.Items[I].cStat = 100) or (AInfProt.Items[I].cStat = 110) or
           (AInfProt.Items[I].cStat = 150) or (AInfProt.Items[I].cStat = 301) or
           (AInfProt.Items[I].cStat = 302) or (AInfProt.Items[I].cStat = 303) then
        begin
          AProcNFe := TProcNFe.Create;
          try
            AProcNFe.XML_NFe := RemoverDeclaracaoXML(FNotasFiscais.Items[J].XMLAssinado);
            AProcNFe.XML_Prot := AInfProt.Items[I].XMLprotDFe;
            AProcNFe.Versao := FPVersaoServico;
            AjustarOpcoes( AProcNFe.Gerador.Opcoes );
            AProcNFe.GerarXML;

            with FNotasFiscais.Items[J] do
            begin
              XMLOriginal := AProcNFe.Gerador.ArquivoFormatoXML;

              if FPConfiguracoesNFe.Arquivos.Salvar then
              begin
                SalvarXML := (not FPConfiguracoesNFe.Arquivos.SalvarApenasNFeProcessadas) or
                             Processada;

                // Salva o XML da NF-e assinado e protocolado
                if SalvarXML then
                begin
                  NomeXMLSalvo := '';
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                  begin
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal );  // Atualiza o XML carregado
                    NomeXMLSalvo := NomeArq;
                  end;

                  if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                    GravarXML; // Salva na pasta baseado nas configurações do PathNFe
                end;
              end;
            end;
          finally
            AProcNFe.Free;
          end;
        end;

        break;
      end;
    end;
  end;

  //Verificando se existe alguma nota confirmada
  for I := 0 to FNotasFiscais.Count - 1 do
  begin
    if FNotasFiscais.Items[I].Confirmada then
    begin
      Result := True;
      break;
    end;
  end;

  //Verificando se existe alguma nota nao confirmada
  for I := 0 to FNotasFiscais.Count - 1 do
  begin
    if not FNotasFiscais.Items[I].Confirmada then
    begin
      FPMsg := ACBrStr('Nota(s) não confirmadas:') + LineBreak;
      break;
    end;
  end;

  //Montando a mensagem de retorno para as notas nao confirmadas
  for I := 0 to FNotasFiscais.Count - 1 do
  begin
    if not FNotasFiscais.Items[I].Confirmada then
      FPMsg := FPMsg + IntToStr(FNotasFiscais.Items[I].NFe.Ide.nNF) +
        '->' + IntToStr(FNotasFiscais.Items[I].cStat)+'-'+ FNotasFiscais.Items[I].Msg + LineBreak;
  end;

  if AInfProt.Count > 0 then
  begin
    FChaveNFe := AInfProt.Items[0].chDFe;
    FProtocolo := AInfProt.Items[0].nProt;
    FcStat := AInfProt.Items[0].cStat;
  end;
end;

procedure TNFeRetRecepcao.FinalizarServico;
begin
  // Sobrescrito, para não liberar para stIdle... não ainda...;

  // Retornar configurações anteriores
  FPDFeOwner.SSL.SSLType := FOldSSLType;
  FPHeaderElement := FOldHeaderElement;
end;

function TNFeRetRecepcao.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'cMsg: %s ' + LineBreak +
                           'xMsg: %s ' + LineBreak),
                   [FNFeRetorno.versao, TpAmbToStr(FNFeRetorno.tpAmb),
                    FNFeRetorno.verAplic, FNFeRetorno.nRec,
                    IntToStr(FNFeRetorno.cStat), FNFeRetorno.xMotivo,
                    CodigoUFparaUF(FNFeRetorno.cUF), IntToStr(FNFeRetorno.cMsg),
                    FNFeRetorno.xMsg]);
  {*)}
end;

function TNFeRetRecepcao.GerarPrefixoArquivo: String;
begin
  Result := Recibo;
end;

{ TNFeRecibo }

constructor TNFeRecibo.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFeRecibo.Destroy;
begin
  FNFeRetorno.Free;

  inherited Destroy;
end;

procedure TNFeRecibo.Clear;
begin
  inherited Clear;

  FPStatus := stNFeRecibo;
  FPLayout := LayNfeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

  Fversao := '';
  FxMsg := '';
  FcMsg := 0;
  FverAplic := '';
  FcStat    := 0;
  FxMotivo  := '';

  if Assigned(FPConfiguracoesNFe) then
  begin
    FtpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
    FcUF := FPConfiguracoesNFe.WebServices.UFCodigo;
  end;

  if Assigned(FNFeRetorno) then
    FNFeRetorno.Free;

  FNFeRetorno := TRetConsReciDFe.Create('NFe');
end;

procedure TNFeRecibo.InicializarServico;
var
  ok: Boolean;
begin
  if FNotasFiscais.Count > 0 then    // Tem NFe ? Se SIM, use as informações do XML
    FVersaoDF := DblToVersaoDF(ok, FNotasFiscais.Items[0].NFe.infNFe.Versao)
  else
    FVersaoDF := FPConfiguracoesNFe.Geral.VersaoDF;

  inherited InicializarServico;

  if FVersaoDF >= ve400 then
    FPHeaderElement := ''  //Versão 4.00 não tem o elemento <soap12:Header>
  else
    FPHeaderElement := FOldHeaderElement;
end;

procedure TNFeRecibo.DefinirServicoEAction;
begin
  if FPLayout = LayNfeRetAutorizacao then
  begin
    if (FVersaoDF >= ve400) then
    begin
      if EstaVazio(FPServico) then
        FPServico := GetUrlWsd + 'NFeRetAutorizacao4';
      if EstaVazio(FPSoapAction) then
        FPSoapAction := FPServico + '/nfeRetAutorizacaoLote';
    end
    else
    begin
      FPServico := GetUrlWsd + 'NfeRetAutorizacao';
      FPSoapAction := FPServico;
    end;
  end
  else
  begin
    FPServico := GetUrlWsd + 'NfeRetRecepcao2';
    FPSoapAction := FPServico;
  end;
end;

procedure TNFeRecibo.DefinirURL;
var
  xUF: String;
  VerServ: Double;
  ok: Boolean;
  Modelo: TpcnModeloDF;
begin
  if FNotasFiscais.Count > 0 then    // Tem NFe ? Se SIM, use as informações do XML
  begin
    Modelo  := StrToModeloDF(ok, IntToStr(FNotasFiscais.Items[0].NFe.Ide.modelo));
    FcUF    := FNotasFiscais.Items[0].NFe.Ide.cUF;

    if FPConfiguracoesNFe.WebServices.Ambiente <> FNotasFiscais.Items[0].NFe.Ide.tpAmb then
      raise EACBrNFeException.Create( CErroAmbienteDiferente );
  end
  else
  begin                              // Se não tem NFe, use as configurações do componente
    Modelo  := FPConfiguracoesNFe.Geral.ModeloDF;
    FcUF    := FPConfiguracoesNFe.WebServices.UFCodigo;
  end;

  VerServ := VersaoDFToDbl(FVersaoDF);
  FTpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  if TACBrNFe(FPDFeOwner).EhAutorizacao(FVersaoDF, Modelo, FcUF) then
    FPLayout := LayNfeRetAutorizacao
  else
    FPLayout := LayNfeRetRecepcao;

  // Configuração correta ao enviar para o SVC
  case FPConfiguracoesNFe.Geral.FormaEmissao of
    teSVCAN: xUF := 'SVC-AN';
    teSVCRS: xUF := 'SVC-RS';
  else
    xUF := CUFtoUF(FcUF);
  end;

  TACBrNFe(FPDFeOwner).LerServicoDeParams(
    ModeloDFToPrefixo(Modelo),
    xUF,
    FTpAmb,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFeRecibo.DefinirDadosIntegrador;
begin
  inherited DefinirDadosIntegrador;

  if Assigned(FPDFeOwner.Integrador) then
  begin
    FPDFeOwner.Integrador.Parametros.Values['versaoDados'] :=  VersaoDFToStr(FPConfiguracoesNFe.Geral.VersaoDF);
    FPDFeOwner.Integrador.SetNomeMetodo('NfeRetAutorizacaoLote12', (FPConfiguracoesNFe.WebServices.Ambiente = taHomologacao) );
  end;
end;

procedure TNFeRecibo.DefinirDadosMsg;
var
  ConsReciNFe: TConsReciDFe;
begin
  ConsReciNFe := TConsReciDFe.Create(FPVersaoServico, NAME_SPACE, 'NFe');
  try
    ConsReciNFe.tpAmb := FTpAmb;
    ConsReciNFe.nRec := FRecibo;

    AjustarOpcoes( ConsReciNFe.Gerador.Opcoes );
    ConsReciNFe.GerarXML;

    FPDadosMsg := ConsReciNFe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciNFe.Free;
  end;
end;

function TNFeRecibo.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['nfeRetAutorizacaoResult',
                               'nfeRetAutorizacaoLoteResult',
                               'nfeResultMsg',
                               'nfeRetRecepcao2Result'],FPRetornoWS );

  VerificarSemResposta;

  //A função UTF8ToNativeString deve ser removida quando for refatorado para usar ACBrXMLDocument
  FNFeRetorno.Leitor.Arquivo := UTF8ToNativeString(ParseText(FPRetWS));
  FNFeRetorno.LerXML;

  Fversao := FNFeRetorno.versao;
  FTpAmb := FNFeRetorno.TpAmb;
  FverAplic := FNFeRetorno.verAplic;
  FcStat := FNFeRetorno.cStat;
  FxMotivo := FNFeRetorno.xMotivo;
  FcUF := FNFeRetorno.cUF;
  FxMsg := FNFeRetorno.xMsg;
  FcMsg := FNFeRetorno.cMsg;
  FPMsg := FxMotivo;

  Result := (FNFeRetorno.CStat = 104);
end;

function TNFeRecibo.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FNFeRetorno.versao, TpAmbToStr(FNFeRetorno.TpAmb),
                   FNFeRetorno.verAplic, FNFeRetorno.nRec,
                   IntToStr(FNFeRetorno.cStat),
                   FNFeRetorno.xMotivo,
                   CodigoUFparaUF(FNFeRetorno.cUF)]);
  {*)}
end;

{ TNFeConsulta }

constructor TNFeConsulta.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FOwner := AOwner;
  FNotasFiscais := ANotasFiscais;
end;

destructor TNFeConsulta.Destroy;
begin
  FprotNFe.Free;
  FretCancNFe.Free;
  FprocEventoNFe.Free;

  inherited Destroy;
end;

procedure TNFeConsulta.Clear;
begin
  inherited Clear;

  FPStatus := stNfeConsulta;
  FPLayout := LayNfeConsulta;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';

  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  FProtocolo := '';
  FDhRecbto := 0;
  Fversao := '';
  FRetNFeDFe := '';

  if Assigned(FPConfiguracoesNFe) then
  begin
    FtpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
    FcUF := FPConfiguracoesNFe.WebServices.UFCodigo;
  end;

  if Assigned(FprotNFe) then
    FprotNFe.Free;

  if Assigned(FretCancNFe) then
    FretCancNFe.Free;

  if Assigned(FprocEventoNFe) then
    FprocEventoNFe.Free;

  FprotNFe := TProcNFe.Create;
  FretCancNFe := TRetCancNFe.Create;
  FprocEventoNFe := TRetEventoNFeCollection.Create;
end;

procedure TNFeConsulta.SetNFeChave(const AValue: String);
var
  NumChave: String;
begin
  if FNFeChave = AValue then Exit;
  NumChave := OnlyNumber(AValue);

  if not ValidarChave(NumChave) then
     raise EACBrNFeException.Create('Chave "'+AValue+'" inválida.');

  FNFeChave := NumChave;
end;

procedure TNFeConsulta.DefinirURL;
var
  VerServ: Double;
  Modelo, xUF: String;
begin
  FPVersaoServico := '';
  FPURL  := '';
  Modelo := ModeloDFToPrefixo(ModeloDFe(FNFeChave));
  FcUF   := ExtrairUFChaveAcesso(FNFeChave);
  VerServ:= VersaoDFToDbl(FPConfiguracoesNFe.Geral.VersaoDF);

  if FNotasFiscais.Count > 0 then
    FTpAmb  := FNotasFiscais.Items[0].NFe.Ide.tpAmb
  else
    FTpAmb  := FPConfiguracoesNFe.WebServices.Ambiente;

  // Se a nota foi enviada para o SVC a consulta tem que ser realizada no SVC e
  // não na SEFAZ-Autorizadora
  case FPConfiguracoesNFe.Geral.FormaEmissao of
    teSVCAN: xUF := 'SVC-AN';
    teSVCRS: xUF := 'SVC-RS';
  else
    xUF := CUFtoUF(FcUF);
  end;

  TACBrNFe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    xUF,
    FTpAmb,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFeConsulta.DefinirServicoEAction;
begin
  if (FPConfiguracoesNFe.Geral.VersaoDF >= ve400) then
  begin
    if EstaVazio(FPServico) then
      FPServico := GetUrlWsd + 'NFeConsultaProtocolo4';
    if EstaVazio(FPSoapAction) then
      FPSoapAction := FPServico + '/nfeConsultaNF';
  end
  // BA usa uma notação de Serviços diferente das demais UFs
  else
  begin
    if (FPConfiguracoesNFe.WebServices.UFCodigo = 29) and // 29 = BA
       (FPConfiguracoesNFe.Geral.ModeloDF = moNFe) and
       (FPConfiguracoesNFe.Geral.VersaoDF = ve310) and
       (FPConfiguracoesNFe.Geral.FormaEmissao = teNormal) then
      FPServico := GetUrlWsd + 'NfeConsulta'
    else
      FPServico := GetUrlWsd + 'NfeConsulta2';

    FPSoapAction := FPServico;
  end;
end;

procedure TNFeConsulta.DefinirDadosIntegrador;
begin
  inherited DefinirDadosIntegrador;

  if Assigned(FPDFeOwner.Integrador) then
  begin
    FPDFeOwner.Integrador.Parametros.Values['versaoDados'] :=  VersaoDFToStr(FPConfiguracoesNFe.Geral.VersaoDF);
    FPDFeOwner.Integrador.SetNomeMetodo('NfeConsulta2Soap12', (FPConfiguracoesNFe.WebServices.Ambiente = taHomologacao) );
  end;
end;

procedure TNFeConsulta.DefinirDadosMsg;
var
  ConsSitNFe: TConsSitNFe;
begin
  ConsSitNFe := TConsSitNFe.Create;
  try
    ConsSitNFe.TpAmb := FTpAmb;
    ConsSitNFe.chNFe := FNFeChave;
    ConsSitNFe.Versao := FPVersaoServico;

    FPDadosMsg := ConsSitNFe.GerarXML;
  finally
    ConsSitNFe.Free;
  end;
end;

function TNFeConsulta.GerarUFSoap: String;
begin
  Result := '<cUF>' + IntToStr(FcUF) + '</cUF>';
end;

function TNFeConsulta.TratarResposta: Boolean;

procedure SalvarEventos(Retorno: string);
var
  aEvento, aProcEvento, aIDEvento, sPathEvento, sCNPJ: string;
  Inicio, Fim: Integer;
  TipoEvento: TpcnTpEvento;
  Ok: Boolean;
begin
  while Retorno <> '' do
  begin
    Inicio := Pos('<procEventoNFe', Retorno);
    Fim    := Pos('</procEventoNFe>', Retorno) + 15;

    aEvento := Copy(Retorno, Inicio, Fim - Inicio + 1);

    Retorno := Copy(Retorno, Fim + 1, Length(Retorno));

    aProcEvento := '<procEventoNFe versao="' + FVersao + '" xmlns="' + ACBRNFE_NAMESPACE + '">' +
                      SeparaDados(aEvento, 'procEventoNFe') +
                   '</procEventoNFe>';

    Inicio := Pos('Id=', aProcEvento) + 6;
    Fim    := 52;

    if Inicio = 6 then
      aIDEvento := FormatDateTime('yyyymmddhhnnss', Now)
    else
      aIDEvento := Copy(aProcEvento, Inicio, Fim);

    TipoEvento  := StrToTpEventoNFe(Ok, SeparaDados(aEvento, 'tpEvento'));
    sCNPJ       := SeparaDados(aEvento, 'CNPJ');
    sPathEvento := PathWithDelim(FPConfiguracoesNFe.Arquivos.GetPathEvento(TipoEvento, sCNPJ));

    if (aProcEvento <> '') then
      FPDFeOwner.Gravar( aIDEvento + '-procEventoNFe.xml', aProcEvento, sPathEvento);
  end;
end;

var
  NFeRetorno: TRetConsSitNFe;
  SalvarXML, NFCancelada, Atualiza: Boolean;
  aEventos, sPathNFe, NomeXMLSalvo: String;
  AProcNFe: TProcNFe;
  I, J, Inicio, Fim: integer;
  dhEmissao: TDateTime;
begin
  NFeRetorno := TRetConsSitNFe.Create(FPVersaoServico);

  try
    FPRetWS := SeparaDadosArray(['NfeConsultaNF2Result',
                                 'NfeConsultaNFResult',
                                 'nfeResultMsg'],FPRetornoWS );

    VerificarSemResposta;

    NFeRetorno.XmlRetorno := ParseText(FPRetWS);
    NFeRetorno.LerXML;

    NFCancelada := False;
    aEventos := '';

    // <retConsSitNFe> - Retorno da consulta da situação da NF-e
    // Este é o status oficial da NF-e
    Fversao := NFeRetorno.versao;
    FTpAmb := NFeRetorno.tpAmb;
    FverAplic := NFeRetorno.verAplic;
    FcStat := NFeRetorno.cStat;
    FXMotivo := NFeRetorno.xMotivo;
    FcUF := NFeRetorno.cUF;
//    FNFeChave := NFeRetorno.chNfe;
    FPMsg := FXMotivo;

    // Verifica se a nota fiscal está cancelada pelo método antigo. Se estiver,
    // então NFCancelada será True e já atribui Protocolo, Data e Mensagem
    if NFeRetorno.retCancNFe.cStat > 0 then
    begin
      FRetCancNFe.versao := NFeRetorno.retCancNFe.versao;
      FretCancNFe.tpAmb := NFeRetorno.retCancNFe.tpAmb;
      FretCancNFe.verAplic := NFeRetorno.retCancNFe.verAplic;
      FretCancNFe.cStat := NFeRetorno.retCancNFe.cStat;
      FretCancNFe.xMotivo := NFeRetorno.retCancNFe.xMotivo;
      FretCancNFe.cUF := NFeRetorno.retCancNFe.cUF;
      FretCancNFe.chNFE := NFeRetorno.retCancNFe.chNFE;
      FretCancNFe.dhRecbto := NFeRetorno.retCancNFe.dhRecbto;
      FretCancNFe.nProt := NFeRetorno.retCancNFe.nProt;

      NFCancelada := True;
      FProtocolo := NFeRetorno.retCancNFe.nProt;
      FDhRecbto := NFeRetorno.retCancNFe.dhRecbto;
      FPMsg := NFeRetorno.xMotivo;
    end;

    // <protNFe> - Retorno dos dados do ENVIO da NF-e
    // Considerá-los apenas se não existir nenhum evento de cancelamento (110111)
    FprotNFe.PathNFe := NFeRetorno.protNFe.PathDFe;
    FprotNFe.PathRetConsReciNFe := NFeRetorno.protNFe.PathRetConsReciDFe;
    FprotNFe.PathRetConsSitNFe := NFeRetorno.protNFe.PathRetConsSitDFe;
    FprotNFe.tpAmb := TpcnTipoAmbiente(NFeRetorno.protNFe.tpAmb);
    FprotNFe.verAplic := NFeRetorno.protNFe.verAplic;
    FprotNFe.chNFe := NFeRetorno.protNFe.chDFe;
    FprotNFe.dhRecbto := NFeRetorno.protNFe.dhRecbto;
    FprotNFe.nProt := NFeRetorno.protNFe.nProt;
    FprotNFe.digVal := NFeRetorno.protNFe.digVal;
    FprotNFe.cStat := NFeRetorno.protNFe.cStat;
    FprotNFe.xMotivo := NFeRetorno.protNFe.xMotivo;
    FprotNFe.Versao := Fversao;
    FprotNFe.cMsg := NFeRetorno.protNFe.cMsg;
    FprotNFe.xMsg := NFeRetorno.protNFe.xMsg;

    {(*}
    if Assigned(NFeRetorno.procEventoNFe) and (NFeRetorno.procEventoNFe.Count > 0) then
    begin
      aEventos := '=====================================================' +
        LineBreak + '================== Eventos da NF-e ==================' +
        LineBreak + '=====================================================' +
        LineBreak + '' + LineBreak + 'Quantidade total de eventos: ' +
        IntToStr(NFeRetorno.procEventoNFe.Count);

      FprocEventoNFe.Clear;
      for I := 0 to NFeRetorno.procEventoNFe.Count - 1 do
      begin
        with FprocEventoNFe.New.RetEventoNFe do
        begin
          idLote := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.idLote;
          tpAmb := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.tpAmb;
          verAplic := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.verAplic;
          cOrgao := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.cOrgao;
          cStat := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.cStat;
          xMotivo := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.xMotivo;
          XML := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.XML;

          InfEvento.ID := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.ID;
          InfEvento.tpAmb := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.tpAmb;
          InfEvento.CNPJ := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.CNPJ;
          InfEvento.chNFe := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.chNFe;
          InfEvento.dhEvento := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.dhEvento;
          InfEvento.TpEvento := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.TpEvento;
          InfEvento.nSeqEvento := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.nSeqEvento;
          InfEvento.VersaoEvento := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.VersaoEvento;
          InfEvento.DetEvento.xCorrecao := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.DetEvento.xCorrecao;
          InfEvento.DetEvento.xCondUso := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.DetEvento.xCondUso;
          InfEvento.DetEvento.nProt := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.DetEvento.nProt;
          InfEvento.DetEvento.xJust := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.InfEvento.DetEvento.xJust;

          retEvento.Clear;
          for J := 0 to NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Count-1 do
          begin
            with retEvento.New.RetInfEvento do
            begin
              Id := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.Id;
              tpAmb := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.tpAmb;
              verAplic := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.verAplic;
              cOrgao := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.cOrgao;
              cStat := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.cStat;
              xMotivo := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.xMotivo;
              chNFe := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.chNFe;
              tpEvento := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.tpEvento;
              xEvento := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.xEvento;
              nSeqEvento := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.nSeqEvento;
              CNPJDest := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.CNPJDest;
              emailDest := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.emailDest;
              dhRegEvento := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.dhRegEvento;
              nProt := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.nProt;
              XML := NFeRetorno.procEventoNFe.Items[I].RetEventoNFe.retEvento.Items[J].RetInfEvento.XML;
            end;
          end;
        end;

        with NFeRetorno.procEventoNFe.Items[I].RetEventoNFe do
        begin
          for j := 0 to retEvento.Count -1 do
          begin
            aEventos := aEventos + LineBreak + LineBreak +
              Format(ACBrStr('Número de sequência: %s ' + LineBreak +
                             'Código do evento: %s ' + LineBreak +
                             'Descrição do evento: %s ' + LineBreak +
                             'Status do evento: %s ' + LineBreak +
                             'Descrição do status: %s ' + LineBreak +
                             'Protocolo: %s ' + LineBreak +
                             'Data/Hora do registro: %s '),
                     [IntToStr(InfEvento.nSeqEvento),
                      TpEventoToStr(InfEvento.TpEvento),
                      InfEvento.DescEvento,
                      IntToStr(retEvento.Items[J].RetInfEvento.cStat),
                      retEvento.Items[J].RetInfEvento.xMotivo,
                      retEvento.Items[J].RetInfEvento.nProt,
                      FormatDateTimeBr(retEvento.Items[J].RetInfEvento.dhRegEvento)]);

            if retEvento.Items[J].RetInfEvento.tpEvento in [teCancelamento, teCancSubst] then
            begin
              NFCancelada := True;
              FProtocolo := retEvento.Items[J].RetInfEvento.nProt;
              FDhRecbto := retEvento.Items[J].RetInfEvento.dhRegEvento;
              FPMsg := retEvento.Items[J].RetInfEvento.xMotivo;
            end;
          end;
        end;
      end;
    end;
    {*)}

    if (not NFCancelada) and (NaoEstaVazio(NFeRetorno.protNFe.nProt))  then
    begin
      FProtocolo := NFeRetorno.protNFe.nProt;
      FDhRecbto := NFeRetorno.protNFe.dhRecbto;
      FPMsg := NFeRetorno.protNFe.xMotivo;
    end;

    if not Assigned(FPDFeOwner) then //evita AV caso não atribua o Owner
    begin
     Result := True;
     Exit;
    end;

    with TACBrNFe(FPDFeOwner) do
    begin
      Result := CstatProcessado(NFeRetorno.CStat) or
                CstatCancelada(NFeRetorno.CStat);
    end;

    if Result then
    begin
      if TACBrNFe(FPDFeOwner).NotasFiscais.Count > 0 then
      begin
        for i := 0 to TACBrNFe(FPDFeOwner).NotasFiscais.Count - 1 do
        begin
          with TACBrNFe(FPDFeOwner).NotasFiscais.Items[i] do
          begin
            if (OnlyNumber(FNFeChave) = NumID) then
            begin
              Atualiza := (NaoEstaVazio(NFeRetorno.XMLprotNFe));
              if Atualiza and
                 TACBrNFe(FPDFeOwner).CstatCancelada(NFeRetorno.CStat) and
                 (not FPConfiguracoesNFe.Geral.AtualizarXMLCancelado) then
                Atualiza := False;

              // No retorno pode constar que a nota esta cancelada, mas traz o grupo
              // <protNFe> com as informações da sua autorização
              if not Atualiza and TACBrNFe(FPDFeOwner).cstatProcessado(NFeRetorno.protNFe.cStat) then
                Atualiza := True;

              if (FPConfiguracoesNFe.Geral.ValidarDigest) and
                (NFeRetorno.protNFe.digVal <> '') and (NFe.signature.DigestValue <> '') and
                (UpperCase(NFe.signature.DigestValue) <> UpperCase(NFeRetorno.protNFe.digVal)) then
              begin
                raise EACBrNFeException.Create('DigestValue do documento ' +
                  NumID + ' não confere.');
              end;

              // Atualiza o Status da NFe interna //
              NFe.procNFe.cStat := NFeRetorno.cStat;

              if Atualiza then
              begin
                if TACBrNFe(FPDFeOwner).CstatCancelada(NFeRetorno.CStat) and
                   FPConfiguracoesNFe.Geral.AtualizarXMLCancelado then
                begin
                  NFe.procNFe.tpAmb := NFeRetorno.tpAmb;
                  NFe.procNFe.verAplic := NFeRetorno.verAplic;
                  NFe.procNFe.chNFe := NFeRetorno.chNfe;
                  NFe.procNFe.dhRecbto := FDhRecbto;
                  NFe.procNFe.nProt := FProtocolo;
                  NFe.procNFe.digVal := NFeRetorno.protNFe.digVal;
                  NFe.procNFe.cStat := NFeRetorno.cStat;
                  NFe.procNFe.xMotivo := NFeRetorno.xMotivo;
                  NFe.procNFe.Versao := Fversao;

                  GerarXML;
                end
                else
                begin
                  NFe.procNFe.tpAmb := TpcnTipoAmbiente(NFeRetorno.protNFe.tpAmb);
                  NFe.procNFe.verAplic := NFeRetorno.protNFe.verAplic;
                  NFe.procNFe.chNFe := NFeRetorno.protNFe.chDFe;
                  NFe.procNFe.dhRecbto := NFeRetorno.protNFe.dhRecbto;
                  NFe.procNFe.nProt := NFeRetorno.protNFe.nProt;
                  NFe.procNFe.digVal := NFeRetorno.protNFe.digVal;
                  NFe.procNFe.cStat := NFeRetorno.protNFe.cStat;
                  NFe.procNFe.xMotivo := NFeRetorno.protNFe.xMotivo;
                  NFe.procNFe.Versao := Fversao;
                  NFe.procNFe.cMsg := NFeRetorno.protNFe.cMsg;
                  NFe.procNFe.xMsg := NFeRetorno.protNFe.xMsg;

                  // O código abaixo é bem mais rápido que "GerarXML" (acima)...
                  AProcNFe := TProcNFe.Create;
                  try
                    AProcNFe.XML_NFe := RemoverDeclaracaoXML(XMLOriginal);
//                    AProcNFe.XML_Prot := NFeRetorno.XMLprotNFe;

                    // A SEFAZ-PR esta retornado o XML de processamento da NFe
                    // fora do padrão, os atributos versao e Id estão sendo
                    // gerados com apostrofe em vez de aspas.
                    AProcNFe.XML_Prot := StringReplace(NFeRetorno.XMLprotNFe, '''', '"', [rfReplaceAll]);

                    AProcNFe.Versao := Fversao;
                    if AProcNFe.Versao = '' then
                      AProcNFe.Versao := FPVersaoServico;
                    AjustarOpcoes( AProcNFe.Gerador.Opcoes );
                    AProcNFe.GerarXML;

                    XMLOriginal := AProcNFe.Gerador.ArquivoFormatoXML;
                  finally
                    AProcNFe.Free;
                  end;
                end;
              end;

              { Se no retorno da consulta constar que a nota possui eventos vinculados
               será disponibilizado na propriedade FRetNFeDFe, e conforme configurado
               em "ConfiguracoesNFe.Arquivos.Salvar", também será gerado o arquivo:
               <chave>-NFeDFe.xml}

              FRetNFeDFe := '';

              if (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoNFe'))) then
              begin
                Inicio := Pos('<procEventoNFe', FPRetWS);
                Fim    := Pos('</retConsSitNFe', FPRetWS) -1;

                aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

                FRetNFeDFe := '<' + ENCODING_UTF8 + '>' +
                              '<NFeDFe>' +
                               '<procNFe versao="' + FVersao + '">' +
                                 SeparaDados(XMLOriginal, 'nfeProc') +
                               '</procNFe>' +
                               '<procEventoNFe versao="' + FVersao + '">' +
                                 aEventos +
                               '</procEventoNFe>' +
                              '</NFeDFe>';
              end;

              SalvarXML := Result and FPConfiguracoesNFe.Arquivos.Salvar and Atualiza;

              if SalvarXML then
              begin
                // Salva o XML da NF-e assinado, protocolado e com os eventos
                if FPConfiguracoesNFe.Arquivos.EmissaoPathNFe then
                  dhEmissao := NFe.Ide.dEmi
                else
                  dhEmissao := Now;

                sPathNFe := PathWithDelim(FPConfiguracoesNFe.Arquivos.GetPathNFe(dhEmissao, NFe.Emit.CNPJCPF, NFe.Emit.IE, NFe.Ide.modelo));

                if (FRetNFeDFe <> '') then
                  FPDFeOwner.Gravar( FNFeChave + '-NFeDFe.xml', FRetNFeDFe, sPathNFe);

                if Atualiza then
                begin
                  // Salva o XML da NF-e assinado e protocolado
                  NomeXMLSalvo := '';
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                  begin
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal );  // Atualiza o XML carregado
                    NomeXMLSalvo := NomeArq;
                  end;

                  // Salva na pasta baseado nas configurações do PathNFe
                  if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                    GravarXML;

                  // Salva o XML de eventos retornados ao consultar um NF-e
                  if ExtrairEventos then
                    SalvarEventos(aEventos);
                end;
              end;

              break;
            end;
          end;
        end;
      end
      else
      begin
        if ExtrairEventos and FPConfiguracoesNFe.Arquivos.Salvar and
           (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoNFe'))) then
        begin
          Inicio := Pos('<procEventoNFe', FPRetWS);
          Fim    := Pos('</retConsSitNFe', FPRetWS) -1;

          aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

          // Salva o XML de eventos retornados ao consultar um NF-e
          SalvarEventos(aEventos);
        end;
      end;
    end;
  finally
    NFeRetorno.Free;
  end;
end;

function TNFeConsulta.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Identificador: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'Chave Acesso: %s ' + LineBreak +
                           'Recebimento: %s ' + LineBreak +
                           'Protocolo: %s ' + LineBreak +
                           'Digest Value: %s ' + LineBreak),
                   [Fversao, FNFeChave, TpAmbToStr(FTpAmb), FverAplic,
                    IntToStr(FcStat), FXMotivo, CodigoUFparaUF(FcUF), FNFeChave,
                    FormatDateTimeBr(FDhRecbto), FProtocolo, FprotNFe.digVal]);
  {*)}
end;

function TNFeConsulta.GerarPrefixoArquivo: String;
begin
  Result := Trim(FNFeChave);
end;

{ TNFeInutilizacao }

procedure TNFeInutilizacao.Clear;
begin
  inherited Clear;

  FPStatus    := stNFeInutilizacao;
  FPLayout    := LayNfeInutilizacao;
  FPArqEnv    := 'ped-inu';
  FPArqResp   := 'inu';

  FverAplic := '';
  FcStat    := 0;
  FxMotivo  := '';
  Fversao   := '';
  FdhRecbto := 0;
  FProtocolo := '';
  FXML_ProcInutNFe := '';

  if Assigned(FPConfiguracoesNFe) then
  begin
    FtpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
    FcUF := FPConfiguracoesNFe.WebServices.UFCodigo;
  end
end;

procedure TNFeInutilizacao.SetJustificativa(const AValue: String);
var
  TrimValue: String;
begin
  TrimValue := Trim(AValue);

  if EstaVazio(TrimValue) then
    GerarException(ACBrStr('Informar uma Justificativa para Inutilização de ' +
      'numeração da Nota Fiscal Eletronica'));

  if Length(TrimValue) < 15 then
    GerarException(ACBrStr('A Justificativa para Inutilização de numeração da ' +
      'Nota Fiscal Eletronica deve ter no minimo 15 caracteres'));

  FJustificativa := TrimValue;
end;

function TNFeInutilizacao.GerarPathPorCNPJ: String;
var
  CNPJ_Temp: String;
begin
  if FPConfiguracoesNFe.Arquivos.SepararPorCNPJ then
    CNPJ_Temp := FCNPJ
  else
    CNPJ_Temp := '';

  Result := FPConfiguracoesNFe.Arquivos.GetPathInu(CNPJ_Temp);
end;

procedure TNFeInutilizacao.DefinirURL;
var
  ok: Boolean;
  VerServ: Double;
  ModeloDFe: String;
begin
  FPVersaoServico := '';
  FPURL  := '';

  ModeloDFe := ModeloDFToPrefixo( StrToModeloDF(ok, IntToStr(FModelo) ));
  if not ok then
    raise EACBrNFeException.Create( 'Modelo Inválido: '+IntToStr(FModelo) );

  VerServ := VersaoDFToDbl(FPConfiguracoesNFe.Geral.VersaoDF);

  TACBrNFe(FPDFeOwner).LerServicoDeParams(
    ModeloDFe,
    FPConfiguracoesNFe.WebServices.UF,
    FPConfiguracoesNFe.WebServices.Ambiente,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFeInutilizacao.DefinirServicoEAction;
var
  ok: Boolean;
begin
  if (FPConfiguracoesNFe.Geral.VersaoDF >= ve400) then
  begin
    if EstaVazio(FPServico) then
      FPServico := GetUrlWsd + 'NFeInutilizacao4';
    if EstaVazio(FPSoapAction) then
      FPSoapAction := FPServico + '/nfeInutilizacaoNF';
  end
  // BA usa uma notação de Serviços diferente das demais UFs
  else if (FPConfiguracoesNFe.WebServices.UFCodigo = 29) and // 29 = BA
     (StrToModeloDF(ok, IntToStr(FModelo)) = moNFe) and
     (FPConfiguracoesNFe.Geral.VersaoDF = ve310) and
     (FPConfiguracoesNFe.Geral.FormaEmissao = teNormal) then
  begin
    if EstaVazio(FPServico) then
      FPServico := GetUrlWsd + 'NfeInutilizacao';
    FPSoapAction := FPServico + '/NfeInutilizacao';
  end
  else
  begin
    FPServico := GetUrlWsd + 'NfeInutilizacao2';
    FPSoapAction := FPServico;
  end;
end;

procedure TNFeInutilizacao.DefinirDadosIntegrador;
begin
  inherited DefinirDadosIntegrador;

  if Assigned(FPDFeOwner.Integrador) then
  begin
    FPDFeOwner.Integrador.Parametros.Values['versaoDados'] :=  VersaoDFToStr(FPConfiguracoesNFe.Geral.VersaoDF);
    FPDFeOwner.Integrador.SetNomeMetodo('NfeInutilizacao2Soap12', (FPConfiguracoesNFe.WebServices.Ambiente = taHomologacao) );
  end;
end;

procedure TNFeInutilizacao.DefinirDadosMsg;
var
  InutNFe: TinutNFe;
begin
  InutNFe := TinutNFe.Create;
  try
//    AjustarOpcoes( InutNFe.Gerador.Opcoes );

    InutNFe.tpAmb  := FPConfiguracoesNFe.WebServices.Ambiente;
    InutNFe.cUF    := FPConfiguracoesNFe.WebServices.UFCodigo;
    InutNFe.ano    := FAno;
    InutNFe.CNPJ   := FCNPJ;
    InutNFe.modelo := FModelo;
    InutNFe.serie  := FSerie;
    InutNFe.nNFIni := FNumeroInicial;
    InutNFe.nNFFin := FNumeroFinal;
    InutNFe.xJust  := FJustificativa;
    InutNFe.Versao := FPVersaoServico;

    AssinarXML( NativeStringToUTF8( InutNFe.GerarXML ),
                'inutNFe', 'infInut',
                'Falha ao assinar Inutilização Nota Fiscal Eletrônica ');

    FID := InutNFe.ID;
  finally
    InutNFe.Free;
  end;
end;

function TNFeInutilizacao.TratarResposta: Boolean;
var
  NFeRetorno: TRetInutNFe;
begin
  NFeRetorno := TRetInutNFe.Create;
  try
    FPRetWS := SeparaDadosArray(['nfeInutilizacaoNF2Result',
                                 'nfeInutilizacaoNFResult',
                                 'nfeResultMsg'],FPRetornoWS );

    VerificarSemResposta;

    NFeRetorno.XmlRetorno := ParseText(FPRetWS);
    NFeRetorno.LerXml;

    Fversao := NFeRetorno.versao;
    FTpAmb := NFeRetorno.TpAmb;
    FverAplic := NFeRetorno.verAplic;
    FcStat := NFeRetorno.cStat;
    FxMotivo := NFeRetorno.xMotivo;
    FcUF := NFeRetorno.cUF;
    FdhRecbto := NFeRetorno.dhRecbto;
    Fprotocolo := NFeRetorno.nProt;
    FPMsg := NFeRetorno.XMotivo;

    Result := (NFeRetorno.cStat = 102);

    //gerar arquivo proc de inutilizacao
    if ((NFeRetorno.cStat = 102) or (NFeRetorno.cStat = 563)) then
    begin
      FXML_ProcInutNFe := '<' + ENCODING_UTF8 + '>' +
                          '<ProcInutNFe versao="' + FPVersaoServico +
                            '" xmlns="'+ACBRNFE_NAMESPACE+'">' +
                           FPDadosMsg +
                           FPRetWS +
                          '</ProcInutNFe>';

      FNomeArquivo := PathWithDelim(GerarPathPorCNPJ) + GerarPrefixoArquivo + '-procInutNFe.xml';
      if FPConfiguracoesNFe.Arquivos.Salvar then
        FPDFeOwner.Gravar(GerarPrefixoArquivo + '-procInutNFe.xml',
          FXML_ProcInutNFe, GerarPathPorCNPJ);
    end;
  finally
    NFeRetorno.Free;
  end;
end;

function TNFeInutilizacao.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'Recebimento: %s ' + LineBreak),
                   [Fversao, TpAmbToStr(FTpAmb), FverAplic, IntToStr(FcStat),
                    FxMotivo, CodigoUFparaUF(FcUF),
                    IfThen(FdhRecbto = 0, '', FormatDateTimeBr(FdhRecbto))]);
  {*)}
end;

function TNFeInutilizacao.GerarPrefixoArquivo: String;
begin
  Result := Trim(OnlyNumber(FID));
end;

{ TNFeConsultaCadastro }

constructor TNFeConsultaCadastro.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
end;

destructor TNFeConsultaCadastro.Destroy;
begin
  FRetConsCad.Free;

  inherited Destroy;
end;

procedure TNFeConsultaCadastro.FinalizarServico;
begin
  inherited FinalizarServico;
  FPBodyElement := FOldBodyElement;
end;

procedure TNFeConsultaCadastro.Clear;
begin
  inherited Clear;

  FPStatus := stNFeCadastro;
  FPLayout := LayNfeCadastro;
  FPArqEnv := 'ped-cad';
  FPArqResp := 'cad';

  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  Fversao := '';
  FdhCons := 0;

  if Assigned(FPConfiguracoesNFe) then
    FcUF := FPConfiguracoesNFe.WebServices.UFCodigo;

  if Assigned(FRetConsCad) then
    FRetConsCad.Free;

  FRetConsCad := TRetConsCad.Create;
end;

procedure TNFeConsultaCadastro.SetCNPJ(const Value: String);
begin
  if NaoEstaVazio(Value) then
  begin
    FIE := '';
    FCPF := '';
  end;

  FCNPJ := Value;
end;

procedure TNFeConsultaCadastro.SetCPF(const Value: String);
begin
  if NaoEstaVazio(Value) then
  begin
    FIE := '';
    FCNPJ := '';
  end;

  FCPF := Value;
end;

procedure TNFeConsultaCadastro.SetIE(const Value: String);
begin
  if NaoEstaVazio(Value) then
  begin
    FCNPJ := '';
    FCPF := '';
  end;

  FIE := Value;
end;

procedure TNFeConsultaCadastro.DefinirServicoEAction;
begin
  if (FPConfiguracoesNFe.Geral.VersaoDF >= ve400) then
  begin
    if EstaVazio(FPServico) then
      FPServico := GetUrlWsd + 'CadConsultaCadastro4';
    if EstaVazio(FPSoapAction) then
      FPSoapAction := FPServico + '/consultaCadastro';
  end
  else
  begin
    FPServico := GetUrlWsd + 'CadConsultaCadastro2';
    FPSoapAction := FPServico;
  end;
end;

procedure TNFeConsultaCadastro.DefinirDadosIntegrador;
begin
  inherited DefinirDadosIntegrador;

  if Assigned(FPDFeOwner.Integrador) then
  begin
    FPDFeOwner.Integrador.Parametros.Values['versaoDados'] :=  VersaoDFToStr(FPConfiguracoesNFe.Geral.VersaoDF);
    FPDFeOwner.Integrador.SetNomeMetodo('CadConsultaCadastro2Soap12', (FPConfiguracoesNFe.WebServices.Ambiente = taHomologacao) );
  end;
end;

procedure TNFeConsultaCadastro.DefinirURL;
var
  Versao_temp: Double;
begin
  FPVersaoServico := '';
  FPURL := '';
  Versao_temp := VersaoDFToDbl(FPConfiguracoesNFe.Geral.VersaoDF);

  if EstaVazio(FUF) then
    FUF := FPConfiguracoesNFe.WebServices.UF;

  TACBrNFe(FPDFeOwner).LerServicoDeParams(
    TACBrNFe(FPDFeOwner).GetNomeModeloDFe,
    FUF,
    FPConfiguracoesNFe.WebServices.Ambiente,
    LayOutToServico(FPLayout),
    Versao_temp,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(Versao_temp, '.', '0.00');
end;

procedure TNFeConsultaCadastro.DefinirDadosMsg;
var
  ConCadNFe: TConsCad;
begin
  ConCadNFe := TConsCad.Create;
  try
    ConCadNFe.UF := FUF;
    ConCadNFe.IE := FIE;
    ConCadNFe.CNPJ := FCNPJ;
    ConCadNFe.CPF := FCPF;
    ConCadNFe.Versao := FPVersaoServico;

    FPDadosMsg := ConCadNFe.GerarXML;

    if (FPConfiguracoesNFe.Geral.VersaoDF >= ve400) and
       (UpperCase(FUF) = 'MT') then
    begin
      FPDadosMsg := '<nfeDadosMsg>' + FPDadosMsg + '</nfeDadosMsg>';
    end;

  finally
    ConCadNFe.Free;
  end;
end;

function TNFeConsultaCadastro.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['consultaCadastroResult',
                               'consultaCadastro2Result',
                               'nfeResultMsg',
                               'consultaCadastro4Result'],FPRetornoWS );

  VerificarSemResposta;

  FRetConsCad.XmlRetorno := FPRetWS; //ParseText(FPRetWS); removido o parser por conta do ACBrDocumentXML que
                                     //não esta conseguindo ler & precisa ser &amp;
  FRetConsCad.LerXml;

  Fversao := FRetConsCad.versao;
  FverAplic := FRetConsCad.verAplic;
  FcStat := FRetConsCad.cStat;
  FxMotivo := FRetConsCad.xMotivo;
  FdhCons := FRetConsCad.dhCons;
  FcUF := FRetConsCad.cUF;
  FPMsg := FRetConsCad.XMotivo;

  Result := (FRetConsCad.cStat in [111, 112]);
end;

function TNFeConsultaCadastro.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'Consulta: %s ' + sLineBreak),
                   [FRetConsCad.versao, FRetConsCad.verAplic,
                   IntToStr(FRetConsCad.cStat), FRetConsCad.xMotivo,
                   CodigoUFparaUF(FRetConsCad.cUF),
                   FormatDateTimeBr(FRetConsCad.dhCons)]);
  {*)}
end;

function TNFeConsultaCadastro.GerarUFSoap: String;
begin
  Result := '<cUF>' + IntToStr(UFparaCodigoUF(FUF)) + '</cUF>';
end;

procedure TNFeConsultaCadastro.InicializarServico;
begin
  inherited InicializarServico;
  FOldBodyElement := FPBodyElement;
  if (FPConfiguracoesNFe.Geral.VersaoDF >= ve400) and
    (UpperCase(FUF) = 'MT') then
  begin
    FPBodyElement := 'consultaCadastro';
  end;
end;

{ TNFeEnvEvento }

constructor TNFeEnvEvento.Create(AOwner: TACBrDFe; AEvento: TEventoNFe);
begin
  inherited Create(AOwner);

  FEvento := AEvento;
end;

destructor TNFeEnvEvento.Destroy;
begin
  if Assigned(FEventoRetorno) then
    FEventoRetorno.Free;

  inherited Destroy;
end;

procedure TNFeEnvEvento.Clear;
begin
  inherited Clear;

  FPStatus := stNFeEvento;
  FPLayout := LayNFeEvento;
  FPArqEnv := 'ped-eve';
  FPArqResp := 'eve';

  FcStat   := 0;
  FxMotivo := '';
  FCNPJ := '';

  if Assigned(FPConfiguracoesNFe) then
    FtpAmb := FPConfiguracoesNFe.WebServices.Ambiente;

  if Assigned(FEventoRetorno) then
    FEventoRetorno.Free;

  FEventoRetorno := TRetEventoNFe.Create;
end;

function TNFeEnvEvento.GerarPathEvento(const ACNPJ, AIE: String): String;
begin
  with FEvento.Evento.Items[0].InfEvento do
  begin
    Result := FPConfiguracoesNFe.Arquivos.GetPathEvento(tpEvento, ACNPJ, AIE);
  end;
end;

procedure TNFeEnvEvento.DefinirURL;
var
  UF, Modelo : String;
  VerServ: Double;
begin
  { Verificação necessária pois somente os eventos de Cancelamento e CCe serão tratados pela SEFAZ do estado
    os outros eventos como manifestacao de destinatários serão tratados diretamente pela RFB }

  FPLayout := LayNFeEvento;
  VerServ  := VersaoDFToDbl(FPConfiguracoesNFe.Geral.VersaoDF);
  FCNPJ    := FEvento.Evento.Items[0].InfEvento.CNPJ;
  FTpAmb   := FEvento.Evento.Items[0].InfEvento.tpAmb;
  Modelo   := ModeloDFToPrefixo(ModeloDFe(FEvento.Evento.Items[0].InfEvento.chNFe));
  FIE      := FEvento.Evento.Items[0].InfEvento.detEvento.IE;

  // Configuração correta ao enviar para o SVC
  case FPConfiguracoesNFe.Geral.FormaEmissao of
    teSVCAN: UF := 'SVC-AN';
    teSVCRS: UF := 'SVC-RS';
  else
    UF := CUFtoUF(ExtrairUFChaveAcesso(FEvento.Evento.Items[0].InfEvento.chNFe));
  end;

  if (FEvento.Evento.Items[0].InfEvento.tpEvento = teEPECNFe) and
     (FPConfiguracoesNFe.WebServices.UFCodigo = 35) and  // Apenas SP tem EPEC para NFCe, pois não permite off-line
     (FPConfiguracoesNFe.Geral.ModeloDF = moNFCe) then
  begin
    FPLayout := LayNFCeEPEC;
  end
  else
  begin
    if not (FEvento.Evento.Items[0].InfEvento.tpEvento in [teCCe,
            teCancelamento, teCancSubst, tePedProrrog1, tePedProrrog2,
            teCanPedProrrog1, teCanPedProrrog2{, teComprEntregaNFe,
            teCancComprEntregaNFe}]) then
    begin
      FPLayout := LayNFeEventoAN;
      UF       := 'AN';
    end;

    {
      Conforme NT 2023/005 versão 1.02 os dois eventos abaixo são
      recepcionados pela SVRS - SEFAZ Virtual do RS.
    }
    if FEvento.Evento.Items[0].InfEvento.tpEvento in [teInsucessoEntregaNFe,
            teCancInsucessoEntregaNFe] then
    begin
      FPLayout := LayNFeEvento;
      UF       := 'SVRS';
    end;
  end;

  FPURL := '';

  TACBrNFe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    UF,
    FTpAmb,
    LayOutToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFeEnvEvento.DefinirServicoEAction;
begin
  if (FPConfiguracoesNFe.Geral.VersaoDF >= ve400) then
  begin
    if EstaVazio(FPServico) then
      FPServico := GetUrlWsd + 'NFeRecepcaoEvento4';
    if EstaVazio(FPSoapAction) then
      FPSoapAction := FPServico + '/nfeRecepcaoEvento';
  end
  else
  begin
    FPServico := GetUrlWsd + 'RecepcaoEvento';
    FPSoapAction := FPServico;
  end;
end;

procedure TNFeEnvEvento.DefinirDadosIntegrador;
begin
  inherited DefinirDadosIntegrador;

  if Assigned(FPDFeOwner.Integrador) then
  begin
    // Para cancelamento é necessário informar os dados da nota //
    // Verificar a necessidade de acrescentar o teCancSubst
    if (FEvento.Evento[0].InfEvento.tpEvento = teCancelamento) and
       (TACBrNFe(FPDFeOwner).NotasFiscais.Count > 0) then
    begin
      FPDFeOwner.Integrador.Parametros.Values['versaoDados'] :=  StringReplace(FormatFloat('0.00',TACBrNFe(FPDFeOwner).NotasFiscais.Items[0].NFe.infNFe.Versao),',','.',[rfReplaceAll]);
      FPDFeOwner.Integrador.Parametros.Values['NumeroNFCe'] := OnlyNumber(TACBrNFe(FPDFeOwner).NotasFiscais.Items[0].NFe.infNFe.ID);
      FPDFeOwner.Integrador.Parametros.Values['DataHoraNFCeGerado'] := FormatDateTime('yyyymmddhhnnss', TACBrNFe(FPDFeOwner).NotasFiscais.Items[0].NFe.Ide.dEmi);
      FPDFeOwner.Integrador.Parametros.Values['ValorNFCe'] := StringReplace(FormatFloat('0.00',TACBrNFe(FPDFeOwner).NotasFiscais.Items[0].NFe.Total.ICMSTot.vNF),',','.',[rfReplaceAll]);
    end;

    FPDFeOwner.Integrador.SetNomeMetodo('RecepcaoEvento', (FPConfiguracoesNFe.WebServices.Ambiente = taHomologacao) );
  end;
end;

procedure TNFeEnvEvento.DefinirDadosMsg;
var
  EventoNFe: TEventoNFe;
  I, J, F: integer;
  Lote, Evento, Eventos, EventosAssinados, AXMLEvento: AnsiString;
  FErroValidacao: string;
  MsgEventoEhValido, EventoEhValido: Boolean;
  SchemaEventoNFe: TSchemaNFe;
begin
  EventoNFe := TEventoNFe.Create;
  try
    EventoNFe.idLote := FidLote;
    SchemaEventoNFe  := schErro;
    {(*}
    for I := 0 to FEvento.Evento.Count - 1 do
    begin
      with EventoNFe.Evento.New do
      begin
        infEvento.tpAmb := FTpAmb;
        infEvento.CNPJ := FEvento.Evento[I].InfEvento.CNPJ;
        infEvento.cOrgao := FEvento.Evento[I].InfEvento.cOrgao;
        infEvento.chNFe := FEvento.Evento[I].InfEvento.chNFe;
        infEvento.dhEvento := FEvento.Evento[I].InfEvento.dhEvento;
        infEvento.tpEvento := FEvento.Evento[I].InfEvento.tpEvento;
        infEvento.nSeqEvento := FEvento.Evento[I].InfEvento.nSeqEvento;
        infEvento.versaoEvento := FEvento.Evento[I].InfEvento.versaoEvento;

        case InfEvento.tpEvento of
          teCCe:
          begin
            SchemaEventoNFe := schEnvCCe;
            infEvento.detEvento.xCorrecao := FEvento.Evento[I].InfEvento.detEvento.xCorrecao;
            infEvento.detEvento.xCondUso := FEvento.Evento[I].InfEvento.detEvento.xCondUso;
          end;

          teCancelamento:
          begin
            SchemaEventoNFe := schcancNFe;
            infEvento.detEvento.nProt := FEvento.Evento[I].InfEvento.detEvento.nProt;
            infEvento.detEvento.xJust := FEvento.Evento[I].InfEvento.detEvento.xJust;
          end;

          teCancSubst:
          begin
            SchemaEventoNFe := schCancSubst;
            infEvento.detEvento.cOrgaoAutor := FEvento.Evento[I].InfEvento.detEvento.cOrgaoAutor;
            infEvento.detEvento.tpAutor := FEvento.Evento[I].InfEvento.detEvento.tpAutor;
            infEvento.detEvento.verAplic := FEvento.Evento[I].InfEvento.detEvento.verAplic;
            infEvento.detEvento.nProt := FEvento.Evento[I].InfEvento.detEvento.nProt;
            infEvento.detEvento.xJust := FEvento.Evento[I].InfEvento.detEvento.xJust;
            infEvento.detEvento.chNFeRef := FEvento.Evento[I].InfEvento.detEvento.chNFeRef;
          end;

          teManifDestConfirmacao:
            SchemaEventoNFe := schManifDestConfirmacao;

          teManifDestCiencia:
            SchemaEventoNFe := schManifDestCiencia;

          teManifDestDesconhecimento:
            SchemaEventoNFe := schManifDestDesconhecimento;

          teManifDestOperNaoRealizada:
          begin
            SchemaEventoNFe := schManifDestOperNaoRealizada;

            infEvento.detEvento.xJust := FEvento.Evento[I].InfEvento.detEvento.xJust;
          end;

          teEPECNFe:
          begin
            SchemaEventoNFe := schEnvEPEC;
            infEvento.detEvento.cOrgaoAutor := FEvento.Evento[I].InfEvento.detEvento.cOrgaoAutor;
            infEvento.detEvento.tpAutor := FEvento.Evento[I].InfEvento.detEvento.tpAutor;
            infEvento.detEvento.verAplic := FEvento.Evento[I].InfEvento.detEvento.verAplic;
            infEvento.detEvento.dhEmi := FEvento.Evento[I].InfEvento.detEvento.dhEmi;
            infEvento.detEvento.tpNF := FEvento.Evento[I].InfEvento.detEvento.tpNF;
            infEvento.detEvento.IE := FEvento.Evento[I].InfEvento.detEvento.IE;

            infEvento.detEvento.dest.UF := FEvento.Evento[I].InfEvento.detEvento.dest.UF;
            infEvento.detEvento.dest.CNPJCPF := FEvento.Evento[I].InfEvento.detEvento.dest.CNPJCPF;
            infEvento.detEvento.dest.idEstrangeiro := FEvento.Evento[I].InfEvento.detEvento.dest.idEstrangeiro;
            infEvento.detEvento.dest.IE := FEvento.Evento[I].InfEvento.detEvento.dest.IE;

            infEvento.detEvento.vNF := FEvento.Evento[I].InfEvento.detEvento.vNF;
            infEvento.detEvento.vICMS := FEvento.Evento[I].InfEvento.detEvento.vICMS;
            infEvento.detEvento.vST := FEvento.Evento[I].InfEvento.detEvento.vST;
          end;

          tePedProrrog1,
          tePedProrrog2:
          begin
            if InfEvento.tpEvento = tePedProrrog1 then
              SchemaEventoNFe := schPedProrrog1
            else
              SchemaEventoNFe := schPedProrrog2;

            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;

            for j := 0 to FEvento.Evento.Items[i].InfEvento.detEvento.itemPedido.count - 1 do
            begin
              with infEvento.detEvento.itemPedido.New do
              begin
                numItem := FEvento.Evento[i].InfEvento.detEvento.itemPedido.Items[j].numItem;
                qtdeItem := FEvento.Evento[i].InfEvento.detEvento.itemPedido.Items[j].qtdeItem;
              end;
            end;

          end;

          teCanPedProrrog1,
          teCanPedProrrog2:
          begin
            if InfEvento.tpEvento = teCanPedProrrog1 then
              SchemaEventoNFe := schCanPedProrrog1
            else
              SchemaEventoNFe := schCanPedProrrog2;

            infEvento.detEvento.idPedidoCancelado := FEvento.Evento[i].InfEvento.detEvento.idPedidoCancelado;
            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;
          end;

          teComprEntregaNFe:
          begin
            SchemaEventoNFe := schCompEntrega;
            infEvento.detEvento.cOrgaoAutor := FEvento.Evento[i].InfEvento.detEvento.cOrgaoAutor;
            infEvento.detEvento.tpAutor := FEvento.Evento[i].InfEvento.detEvento.tpAutor;
            infEvento.detEvento.verAplic := FEvento.Evento[i].InfEvento.detEvento.verAplic;
            infEvento.detEvento.dhEntrega := FEvento.Evento[i].InfEvento.detEvento.dhEntrega;
            infEvento.detEvento.nDoc := FEvento.Evento[i].InfEvento.detEvento.nDoc;
            infEvento.detEvento.xNome := FEvento.Evento[i].InfEvento.detEvento.xNome;
            infEvento.detEvento.latGPS := FEvento.Evento[i].InfEvento.detEvento.latGPS;
            infEvento.detEvento.longGPS := FEvento.Evento[i].InfEvento.detEvento.longGPS;
            infEvento.detEvento.hashComprovante := FEvento.Evento[i].InfEvento.detEvento.hashComprovante;
            infEvento.detEvento.dhHashComprovante := FEvento.Evento[i].InfEvento.detEvento.dhHashComprovante;
          end;

          teCancComprEntregaNFe:
          begin
            SchemaEventoNFe := schCancCompEntrega;
            infEvento.detEvento.cOrgaoAutor := FEvento.Evento[i].InfEvento.detEvento.cOrgaoAutor;
            infEvento.detEvento.tpAutor := FEvento.Evento[i].InfEvento.detEvento.tpAutor;
            infEvento.detEvento.verAplic := FEvento.Evento[i].InfEvento.detEvento.verAplic;
            infEvento.detEvento.nProtEvento := FEvento.Evento[i].InfEvento.detEvento.nProtEvento;
          end;

          teAtorInteressadoNFe:
          begin
            SchemaEventoNFe := schAtorInteressadoNFe;
            infEvento.detEvento.cOrgaoAutor := FEvento.Evento[i].InfEvento.detEvento.cOrgaoAutor;
            infEvento.detEvento.tpAutor := FEvento.Evento[i].InfEvento.detEvento.tpAutor;
            infEvento.detEvento.verAplic := FEvento.Evento[i].InfEvento.detEvento.verAplic;

            for j := 0 to FEvento.Evento.Items[i].InfEvento.detEvento.autXML.count - 1 do
            begin
              with infEvento.detEvento.autXML.New do
              begin
                CNPJCPF := FEvento.Evento[i].InfEvento.detEvento.autXML[j].CNPJCPF;
              end;
            end;

            infEvento.detEvento.tpAutorizacao := FEvento.Evento[i].InfEvento.detEvento.tpAutorizacao;
            infEvento.detEvento.xCondUso := FEvento.Evento[i].InfEvento.detEvento.xCondUso;
          end;

          teInsucessoEntregaNFe:
          begin
            SchemaEventoNFe := schInsucessoEntregaNFe;
            infEvento.detEvento.cOrgaoAutor := FEvento.Evento[i].InfEvento.detEvento.cOrgaoAutor;
            infEvento.detEvento.verAplic := FEvento.Evento[i].InfEvento.detEvento.verAplic;
            infEvento.detEvento.dhTentativaEntrega := FEvento.Evento[i].InfEvento.detEvento.dhTentativaEntrega;
            infEvento.detEvento.nTentativa := FEvento.Evento[i].InfEvento.detEvento.nTentativa;
            infEvento.detEvento.tpMotivo := FEvento.Evento[i].InfEvento.detEvento.tpMotivo;
            infEvento.detEvento.xJustMotivo := FEvento.Evento[i].InfEvento.detEvento.xJustMotivo;
            infEvento.detEvento.latGPS := FEvento.Evento[i].InfEvento.detEvento.latGPS;
            infEvento.detEvento.longGPS := FEvento.Evento[i].InfEvento.detEvento.longGPS;
            infEvento.detEvento.hashTentativaEntrega := FEvento.Evento[i].InfEvento.detEvento.hashTentativaEntrega;
            infEvento.detEvento.dhHashTentativaEntrega := FEvento.Evento[i].InfEvento.detEvento.dhHashTentativaEntrega;
            infEvento.detEvento.UF := FEvento.Evento[i].InfEvento.detEvento.UF;
          end;

          teCancInsucessoEntregaNFe:
          begin
            SchemaEventoNFe := schCancInsucessoEntregaNFe;
            infEvento.detEvento.cOrgaoAutor := FEvento.Evento[i].InfEvento.detEvento.cOrgaoAutor;
            infEvento.detEvento.verAplic := FEvento.Evento[i].InfEvento.detEvento.verAplic;
            infEvento.detEvento.nProtEvento := FEvento.Evento[i].InfEvento.detEvento.nProtEvento;
          end;
        end;
      end;
    end;
    {*)}

    EventoNFe.Versao := FPVersaoServico;
    AjustarOpcoes( EventoNFe.Gerador.Opcoes );

    if SchemaEventoNFe = schAtorInteressadoNFe then
      EventoNFe.Gerador.Opcoes.RetirarAcentos := False;  // Não funciona sem acentos

    EventoNFe.GerarXML;

    // Separa os grupos <evento> e coloca na variável Eventos
    I := Pos('<evento ', EventoNFe.Gerador.ArquivoFormatoXML);
    Lote := Copy(EventoNFe.Gerador.ArquivoFormatoXML, 1, I - 1);
    Eventos := SeparaDados(EventoNFe.Gerador.ArquivoFormatoXML, 'envEvento');
    I := Pos('<evento ', Eventos);
    Eventos := NativeStringToUTF8( Copy(Eventos, I, length(Eventos)) );

    EventosAssinados := '';

    // Realiza a assinatura para cada evento
    while Eventos <> '' do
    begin
      F := Pos('</evento>', Eventos);

      if F > 0 then
      begin
        Evento := Copy(Eventos, 1, F + 8);
        Eventos := Copy(Eventos, F + 9, length(Eventos));

        AssinarXML(Evento, 'evento', 'infEvento', 'Falha ao assinar o Envio de Evento ');
        EventosAssinados := EventosAssinados + FPDadosMsg;
      end
      else
        Break;
    end;

    F := Pos('?>', EventosAssinados);
    if F <> 0 then
      FPDadosMsg := copy(EventosAssinados, 1, F + 1) + Lote +
        copy(EventosAssinados, F + 2, Length(EventosAssinados)) + '</envEvento>'
    else
      FPDadosMsg := Lote + EventosAssinados + '</envEvento>';

    with TACBrNFe(FPDFeOwner) do
    begin
      MsgEventoEhValido := SSL.Validar(FPDadosMsg,
                                       GerarNomeArqSchema(FPLayout, StringToFloatDef(FPVersaoServico,0)),
                                       FPMsg);
    end;

    if (not MsgEventoEhValido) or (SchemaEventoNFe = schErro) then
    begin
      if (SchemaEventoNFe = schErro) and (FPMsg='') then
       FPMsg := 'Schema do Evento não foi definido';

      FErroValidacao := ACBrStr('Falha na validação da Mensagem do Evento: ') +
        FPMsg;

      raise EACBrNFeException.CreateDef(FErroValidacao);
    end;

    // Realiza a validação de cada evento
    Eventos := SeparaDados(EventoNFe.Gerador.ArquivoFormatoXML, 'envEvento');
    I := Pos('<evento ', Eventos);
    Eventos := NativeStringToUTF8( Copy(Eventos, I, length(Eventos)) );

    while Eventos <> '' do
    begin
      F := Pos('</evento>', Eventos);

      if F > 0 then
      begin
        Evento := Copy(Eventos, 1, F + 8);
        Eventos := Copy(Eventos, F + 9, length(Eventos));

        // Separa o XML especifico do Evento para ser Validado.
        AXMLEvento := '<detEvento versao="' + FPVersaoServico + '" xmlns="' +
                                                      ACBRNFE_NAMESPACE + '">' +
                        SeparaDados(Evento, 'detEvento') +
                      '</detEvento>';

        with TACBrNFe(FPDFeOwner) do
        begin
          EventoEhValido := SSL.Validar(AXMLEvento,
                                        GerarNomeArqSchemaEvento(SchemaEventoNFe,
                                                             StringToFloatDef(FPVersaoServico, 0)),
                                        FPMsg);
        end;

        if not EventoEhValido then
        begin
          FErroValidacao := ACBrStr('Falha na validação dos dados do Evento: ') +
            FPMsg;

          raise EACBrNFeException.CreateDef(FErroValidacao);
        end;
      end
      else
        Break;
    end;

    for I := 0 to FEvento.Evento.Count - 1 do
      FEvento.Evento[I].InfEvento.id := EventoNFe.Evento[I].InfEvento.id;
  finally
    EventoNFe.Free;
  end;
end;

function TNFeEnvEvento.TratarResposta: Boolean;
var
  Leitor: TLeitor;
  I, J: integer;
  NomeArq, PathArq, VersaoEvento, Texto: String;
begin
  FEvento.idLote := idLote;

  FPRetWS := SeparaDadosArray(['nfeRecepcaoEventoResult',
                               'nfeRecepcaoEventoNFResult',
                               'nfeResultMsg'],FPRetornoWS );

  VerificarSemResposta;

  EventoRetorno.XmlRetorno := ParseText(FPRetWS);
  EventoRetorno.LerXml;

  FcStat := EventoRetorno.cStat;
  FxMotivo := EventoRetorno.xMotivo;
  FPMsg := EventoRetorno.xMotivo;
  FTpAmb := EventoRetorno.tpAmb;

  Result := (FcStat = 128);

  //gerar arquivo proc de evento
  if Result then
  begin
    Leitor := TLeitor.Create;
    try
      for I := 0 to FEvento.Evento.Count - 1 do
      begin
        for J := 0 to EventoRetorno.retEvento.Count - 1 do
        begin
          if FEvento.Evento.Items[I].InfEvento.chNFe =
            EventoRetorno.retEvento.Items[J].RetInfEvento.chNFe then
          begin
            FEvento.Evento.Items[I].RetInfEvento.tpAmb :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.tpAmb;
            FEvento.Evento.Items[I].RetInfEvento.nProt :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.nProt;
            FEvento.Evento.Items[I].RetInfEvento.dhRegEvento :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.dhRegEvento;
            FEvento.Evento.Items[I].RetInfEvento.cStat :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.cStat;
            FEvento.Evento.Items[I].RetInfEvento.xMotivo :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.xMotivo;

            Texto := '';

            if EventoRetorno.retEvento.Items[J].RetInfEvento.cStat in [135, 136, 155] then
            begin
              VersaoEvento := TACBrNFe(FPDFeOwner).LerVersaoDeParams(LayNfeEvento);

              Leitor.Arquivo := FPDadosMsg;
              Texto := '<procEventoNFe versao="' + VersaoEvento + '" xmlns="' + ACBRNFE_NAMESPACE + '">' +
                        '<evento versao="' + VersaoEvento + '">' +
                         Leitor.rExtrai(1, 'infEvento', '', I + 1) +
                         '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">' +
                          Leitor.rExtrai(1, 'SignedInfo', '', I + 1) +
                          Leitor.rExtrai(1, 'SignatureValue', '', I + 1) +
                          Leitor.rExtrai(1, 'KeyInfo', '', I + 1) +
                         '</Signature>'+
                        '</evento>';

              Leitor.Arquivo := FPRetWS;
              Texto := Texto +
                         '<retEvento versao="' + VersaoEvento + '">' +
                          Leitor.rExtrai(1, 'infEvento', '', J + 1) +
                         '</retEvento>' +
                        '</procEventoNFe>';

              if FPConfiguracoesNFe.Arquivos.Salvar then
              begin
                NomeArq := OnlyNumber(FEvento.Evento.Items[i].InfEvento.Id) + '-procEventoNFe.xml';
                PathArq := PathWithDelim(GerarPathEvento(FEvento.Evento.Items[I].InfEvento.CNPJ, FEvento.Evento.Items[I].InfEvento.detEvento.IE));

                FPDFeOwner.Gravar(NomeArq, Texto, PathArq);
                FEventoRetorno.retEvento.Items[J].RetInfEvento.NomeArquivo := PathArq + NomeArq;
                FEvento.Evento.Items[I].RetInfEvento.NomeArquivo := PathArq + NomeArq;
              end;

              { Converte de UTF8 para a String nativa e Decodificar caracteres HTML Entity }
              Texto := ParseText(Texto);
            end;

            // Se o evento for rejeitado a propriedade XML conterá uma string vazia
            if Texto <> '' then
              Texto := ConverteXMLtoUTF8(Texto);
            FEventoRetorno.retEvento.Items[J].RetInfEvento.XML := Texto;
            FEvento.Evento.Items[I].RetInfEvento.XML := Texto;

            break;
          end;
        end;
      end;
    finally
      Leitor.Free;
    end;
  end;
end;

function TNFeEnvEvento.GerarMsgLog: String;
var
  aMsg: String;
begin
  {(*}
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 [FEventoRetorno.versao, TpAmbToStr(FEventoRetorno.tpAmb),
                  FEventoRetorno.verAplic, IntToStr(FEventoRetorno.cStat),
                  FEventoRetorno.xMotivo]);

  if FEventoRetorno.retEvento.Count > 0 then
    aMsg := aMsg + Format(ACBrStr('Recebimento: %s ' + LineBreak),
       [IfThen(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento = 0, '',
               FormatDateTimeBr(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento))]);

  Result := aMsg;
  {*)}
end;

function TNFeEnvEvento.GerarPrefixoArquivo: String;
begin
//  Result := IntToStr(FEvento.idLote);
  Result := IntToStr(FidLote);
end;

{ TAdministrarCSCNFCe }

constructor TAdministrarCSCNFCe.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
end;

destructor TAdministrarCSCNFCe.Destroy;
begin
  FretAdmCSCNFCe.Free;

  inherited Destroy;
end;

procedure TAdministrarCSCNFCe.Clear;
begin
  inherited Clear;

  FPStatus := stAdmCSCNFCe;
  FPLayout := LayAdministrarCSCNFCe;
  FPArqEnv := 'ped-csc';
  FPArqResp := 'csc';

  if Assigned(FretAdmCSCNFCe) then
    FretAdmCSCNFCe.Free;

  FretAdmCSCNFCe := TRetAdmCSCNFCe.Create;
end;

procedure TAdministrarCSCNFCe.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'CscNFCe';
  FPSoapAction := FPServico + '/admCscNFCe';
end;

procedure TAdministrarCSCNFCe.DefinirDadosMsg;
var
  AdmCSCNFCe: TAdmCSCNFCe;
begin
  AdmCSCNFCe := TAdmCSCNFCe.Create;
  try
    AdmCSCNFCe.TpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
    AdmCSCNFCe.RaizCNPJ := FRaizCNPJ;
    AdmCSCNFCe.indOP := FindOp;
    AdmCSCNFCe.idCsc := FIdCSC;
    AdmCSCNFCe.codigoCsc := FCodigoCSC;
    AdmCSCNFCe.Versao := FPVersaoServico;

    FPDadosMsg := AdmCSCNFCe.GerarXML;
  finally
    AdmCSCNFCe.Free;
  end;
end;

function TAdministrarCSCNFCe.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['cscNFCeResult',
                               'nfeResultMsg'],FPRetornoWS );

  VerificarSemResposta;

  FretAdmCSCNFCe.XmlRetorno := ParseText(FPRetWS);
  FretAdmCSCNFCe.LerXml;

  FPMsg := FretAdmCSCNFCe.xMotivo;

  Result := (FretAdmCSCNFCe.CStat in [150..153]);
end;

function TAdministrarCSCNFCe.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak),
                   [FretAdmCSCNFCe.versao, TpAmbToStr(FretAdmCSCNFCe.tpAmb),
                    IntToStr(FretAdmCSCNFCe.cStat), FretAdmCSCNFCe.xMotivo]);
  {*)}
end;

function TAdministrarCSCNFCe.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Administrar CSC da NFC-e:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TDistribuicaoDFe }

constructor TDistribuicaoDFe.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
end;

destructor TDistribuicaoDFe.Destroy;
begin
  FretDistDFeInt.Free;
  FlistaArqs.Free;

  inherited Destroy;
end;

procedure TDistribuicaoDFe.Clear;
begin
  inherited Clear;

  FPStatus := stDistDFeInt;
  FPLayout := LayDistDFeInt;
  FPArqEnv := 'con-dist-dfe';
  FPArqResp := 'dist-dfe';
  FPBodyElement := 'nfeDistDFeInteresse';
  FPHeaderElement := '';

  if Assigned(FretDistDFeInt) then
    FretDistDFeInt.Free;

  FretDistDFeInt := TRetDistDFeInt.Create('NFe');

  if Assigned(FlistaArqs) then
    FlistaArqs.Free;

  FlistaArqs := TStringList.Create;
end;

procedure TDistribuicaoDFe.DefinirURL;
var
  UF: String;
  Versao: Double;
begin
  { Esse método é tratado diretamente pela RFB }

  UF := 'AN';

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';
  Versao := VersaoDFToDbl(FPConfiguracoesNFe.Geral.VersaoDF);

  TACBrNFe(FPDFeOwner).LerServicoDeParams(
    ModeloDFToPrefixo(ModeloDFe(FchNFe)),
    UF ,
    FPConfiguracoesNFe.WebServices.Ambiente,
    LayOutToServico(FPLayout),
    Versao,
    FPURL, FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

procedure TDistribuicaoDFe.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFeDistribuicaoDFe';
  FPSoapAction := FPServico + '/nfeDistDFeInteresse';
end;

procedure TDistribuicaoDFe.DefinirDadosMsg;
var
  DistDFeInt: TDistDFeInt;
begin
  DistDFeInt := TDistDFeInt.Create(FPVersaoServico, NAME_SPACE,
                                     'nfeDadosMsg', 'consChNFe', 'chNFe', True);
  try
    DistDFeInt.TpAmb := FPConfiguracoesNFe.WebServices.Ambiente;
    DistDFeInt.cUFAutor := FcUFAutor;
    DistDFeInt.CNPJCPF := FCNPJCPF;
    DistDFeInt.ultNSU := trim(FultNSU);
    DistDFeInt.NSU := trim(FNSU);
    DistDFeInt.Chave := trim(FchNFe);

    AjustarOpcoes( DistDFeInt.Gerador.Opcoes );
    DistDFeInt.GerarXML;

    FPDadosMsg := DistDFeInt.Gerador.ArquivoFormatoXML;
  finally
    DistDFeInt.Free;
  end;
end;

function TDistribuicaoDFe.TratarResposta: Boolean;
var
  I: integer;
  AXML, aPath: String;
begin
  FPRetWS := SeparaDadosArray(['nfeDistDFeInteresseResult',
                               'nfeResultMsg'],FPRetornoWS );

  VerificarSemResposta;

  // Processando em UTF8, para poder gravar arquivo corretamente //
  //A função UTF8ToNativeString deve ser removida quando for refatorado para usar ACBrXMLDocument
  FretDistDFeInt.Leitor.Arquivo := UTF8ToNativeString(ParseText(FPRetWS));
  FretDistDFeInt.LerXml;

  for I := 0 to FretDistDFeInt.docZip.Count - 1 do
  begin
    AXML := FretDistDFeInt.docZip.Items[I].XML;
    FNomeArq := '';
    if (AXML <> '') then
    begin
      case FretDistDFeInt.docZip.Items[I].schema of
        schresNFe:
          FNomeArq := FretDistDFeInt.docZip.Items[I].resDFe.chDFe + '-resNFe.xml';

        schresEvento:
          FNomeArq := OnlyNumber(TpEventoToStr(FretDistDFeInt.docZip.Items[I].resEvento.tpEvento) +
                      FretDistDFeInt.docZip.Items[I].resEvento.chDFe +
                      Format('%.2d', [FretDistDFeInt.docZip.Items[I].resEvento.nSeqEvento])) +
                      '-resEventoNFe.xml';

        schprocNFe:
          FNomeArq := FretDistDFeInt.docZip.Items[I].resDFe.chDFe + '-nfe.xml';

        schprocEventoNFe:
          FNomeArq := OnlyNumber(FretDistDFeInt.docZip.Items[I].procEvento.Id) +
                      '-procEventoNFe.xml';
      end;

      aPath := GerarPathDistribuicao(FretDistDFeInt.docZip.Items[I]);

      if NaoEstaVazio(NomeArq) then
        FlistaArqs.Add(aPath + PathDelim + FNomeArq);

      FretDistDFeInt.docZip.Items[I].NomeArq := aPath + PathDelim + FNomeArq;

      if (FPConfiguracoesNFe.Arquivos.Salvar) and NaoEstaVazio(NomeArq) then
      begin
        if FPConfiguracoesNFe.Arquivos.SalvarEvento then
          if (FretDistDFeInt.docZip.Items[I].schema in [schresEvento, schprocEventoNFe]) then
            FPDFeOwner.Gravar(NomeArq, AXML, aPath);

        if (FretDistDFeInt.docZip.Items[I].schema in [schresNFe, schprocNFe]) then
          FPDFeOwner.Gravar(NomeArq, AXML, aPath);
      end;
    end;
  end;

  { Processsa novamente, chamando ParseTXT, para converter de UTF8 para a String
    nativa e Decodificar caracteres HTML Entity }
  {
  FretDistDFeInt.Free;   // Limpando a lista
  FretDistDFeInt := TRetDistDFeInt.Create('NFe');

  FretDistDFeInt.Leitor.Arquivo := ParseText(FPRetWS);
  FretDistDFeInt.LerXml;
  }
  FPMsg := FretDistDFeInt.xMotivo;
  Result := (FretDistDFeInt.CStat = 137) or (FretDistDFeInt.CStat = 138);
end;

function TDistribuicaoDFe.GerarMsgLog: String;
begin
  {(*}
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'Resposta: %s ' + LineBreak +
                           'Último NSU: %s ' + LineBreak +
                           'Máximo NSU: %s ' + LineBreak),
                   [FretDistDFeInt.versao, TpAmbToStr(FretDistDFeInt.tpAmb),
                    FretDistDFeInt.verAplic, IntToStr(FretDistDFeInt.cStat),
                    FretDistDFeInt.xMotivo,
                    IfThen(FretDistDFeInt.dhResp = 0, '',
                           FormatDateTimeBr(RetDistDFeInt.dhResp)),
                    FretDistDFeInt.ultNSU, FretDistDFeInt.maxNSU]);
  {*)}
end;

function TDistribuicaoDFe.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Distribuição de DFe:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TDistribuicaoDFe.GerarPathDistribuicao(AItem: TdocZipCollectionItem): String;
var
  Data: TDateTime;
begin
  if FPConfiguracoesNFe.Arquivos.EmissaoPathNFe then
  begin
    Data := AItem.resDFe.dhEmi;
    if Data = 0 then
    begin
      Data := AItem.resEvento.dhEvento;
      if Data = 0 then
        Data := AItem.procEvento.dhEvento;
    end;
  end
  else
    Data := Now;

  case AItem.schema of
    schresEvento:
      Result := FPConfiguracoesNFe.Arquivos.GetPathDownloadEvento(AItem.resEvento.tpEvento,
                                                          AItem.resDFe.xNome,
                                                          AItem.resEvento.CNPJCPF,
                                                          AItem.resDFe.IE,
                                                          Data);

    schprocEventoNFe:
      Result := FPConfiguracoesNFe.Arquivos.GetPathDownloadEvento(AItem.procEvento.tpEvento,
                                                          AItem.resDFe.xNome,
                                                          AItem.procEvento.CNPJ,
                                                          AItem.resDFe.IE,
                                                          Data);

    schresNFe,
    schprocNFe:
      Result := FPConfiguracoesNFe.Arquivos.GetPathDownload(AItem.resDFe.xNome,
                                                        AItem.resDFe.CNPJCPF,
                                                        AItem.resDFe.IE,
                                                        Data);
  end;
end;

{ TNFeEnvioWebService }

constructor TNFeEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stEnvioWebService;
end;

destructor TNFeEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

procedure TNFeEnvioWebService.Clear;
begin
  inherited Clear;

  FVersao := '';
end;

function TNFeEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TNFeEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TNFeEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TNFeEnvioWebService.DefinirDadosMsg;
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

function TNFeEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');

  VerificarSemResposta;

  Result := True;
end;

function TNFeEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TNFeEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrNFe := TACBrNFe(AOwner);

  FStatusServico := TNFeStatusServico.Create(FACBrNFe);
  FEnviar := TNFeRecepcao.Create(FACBrNFe, TACBrNFe(FACBrNFe).NotasFiscais);
  FRetorno := TNFeRetRecepcao.Create(FACBrNFe, TACBrNFe(FACBrNFe).NotasFiscais);
  FRecibo := TNFeRecibo.Create(FACBrNFe, TACBrNFe(FACBrNFe).NotasFiscais);
  FConsulta := TNFeConsulta.Create(FACBrNFe, TACBrNFe(FACBrNFe).NotasFiscais);
  FInutilizacao := TNFeInutilizacao.Create(FACBrNFe);
  FConsultaCadastro := TNFeConsultaCadastro.Create(FACBrNFe);
  FEnvEvento := TNFeEnvEvento.Create(FACBrNFe, TACBrNFe(FACBrNFe).EventoNFe);
  FAdministrarCSCNFCe := TAdministrarCSCNFCe.Create(FACBrNFe);
  FDistribuicaoDFe := TDistribuicaoDFe.Create(FACBrNFe);
  FEnvioWebService := TNFeEnvioWebService.Create(FACBrNFe);
end;

destructor TWebServices.Destroy;
begin
  FStatusServico.Free;
  FEnviar.Free;
  FRetorno.Free;
  FRecibo.Free;
  FConsulta.Free;
  FInutilizacao.Free;
  FConsultaCadastro.Free;
  FEnvEvento.Free;
  FAdministrarCSCNFCe.Free;
  FDistribuicaoDFe.Free;
  FEnvioWebService.Free;

  inherited Destroy;
end;


function TWebServices.Envia(ALote: Int64; const ASincrono: Boolean;
  AZipado: Boolean): Boolean;
begin
  Result := Envia(IntToStr(ALote), ASincrono, AZipado );
end;

function TWebServices.Envia(const ALote: String; const ASincrono: Boolean;
  AZipado: Boolean): Boolean;
begin
  FEnviar.Clear;
  FRetorno.Clear;

  FEnviar.Lote := ALote;
  FEnviar.Sincrono := ASincrono;
  FEnviar.Zipado := AZipado;

  if not Enviar.Executar then
    Enviar.GerarException( Enviar.Msg );

  if not ASincrono or ((FEnviar.Recibo <> '') and (FEnviar.cStat = 103)) then
  begin
    FRetorno.Recibo := FEnviar.Recibo;
    if not FRetorno.Executar then
      FRetorno.GerarException( FRetorno.Msg );
  end;

  Result := True;
end;

procedure TWebServices.Inutiliza(const ACNPJ, AJustificativa: String;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer);
var
  CNPJ : string;
begin
  CNPJ := OnlyNumber(ACNPJ);

  if not ValidarCNPJouCPF(CNPJ) then
    raise EACBrNFeException.Create('CNPJ: ' + CNPJ + ACBrStr(', inválido.'));

  FInutilizacao.CNPJ := CNPJ;
  FInutilizacao.Modelo := Modelo;
  FInutilizacao.Serie := Serie;
  FInutilizacao.Ano := Ano;
  FInutilizacao.NumeroInicial := NumeroInicial;
  FInutilizacao.NumeroFinal := NumeroFinal;
  FInutilizacao.Justificativa := AJustificativa;

  if not FInutilizacao.Executar then
    FInutilizacao.GerarException( FInutilizacao.Msg );
end;

end.
