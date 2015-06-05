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
    FPStatus: TStatusACBrNFSe;
    FPLayout: TLayOutNFSe;
    FPConfiguracoesNFSe: TConfiguracoesNFSe;

    procedure ConfigurarSoapDEPC;
    function ExtrairModeloChaveAcesso(AChaveNFSe: String): String;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;

  public
    constructor Create(AOwner: TACBrDFe); override;

    property Status: TStatusACBrNFSe read FPStatus;
    property Layout: TLayOutNFSe read FPLayout;
  end;

  { TNFSeEnviarLoteRPS }

  TNFSeEnviarLoteRPS = class(TNFSeWebService)
  private
    FNumeroLote: String;
    FDataRecebimento: TDateTime;
    FProtocolo: String;
    FNFSeRetorno: TretEnvLote;
    FNotasFiscais: TNotasFiscais;

    function GetLote: String;
    function GetRecibo: String;
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

    property NumeroLote: String read FNumeroLote;
    property DataRecebimento: TDateTime read FDataRecebimento;
    property Protocolo: String read FProtocolo;
    property NFSeRetorno: TretEnvLote read FNFSeRetorno write FNFSeRetorno;
  end;

(*
  { TNFSeRetRecepcao }

  TNFSeRetRecepcao = class(TNFSeWebService)
  private
    FRecibo: String;
    FProtocolo: String;
    FChaveNFSe: String;
    FNotasFiscais: TNotasFiscais;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;
    FxMotivo: String;
    FcMsg: integer;
    FxMsg: String;

    FNFSeRetorno: TRetConsReciNFSe;

    function GetRecibo: String;
    function TratarRespostaFinal: Boolean;
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
    property ChaveNFSe: String read FChaveNFSe write FChaveNFSe;

    property NFSeRetorno: TRetConsReciNFSe read FNFSeRetorno;
  end;

  { TNFSeRecibo }

  TNFSeRecibo = class(TNFSeWebService)
  private
    FRecibo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FxMotivo: String;
    FcUF: integer;
    FxMsg: String;
    FcMsg: integer;

    FNFSeRetorno: TRetConsReciNFSe;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirURL; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property cUF: integer read FcUF;
    property xMsg: String read FxMsg;
    property cMsg: integer read FcMsg;
    property Recibo: String read FRecibo write FRecibo;

    property NFSeRetorno: TRetConsReciNFSe read FNFSeRetorno;
  end;

  { TNFSeConsulta }

  TNFSeConsulta = class(TNFSeWebService)
  private
    FNFSeChave: String;
    FProtocolo: String;
    FDhRecbto: TDateTime;
    FXMotivo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;

    FprotNFSe: TProcNFSe;
    FretCancNFSe: TRetCancNFSe;
    FprocEventoNFSe: TRetEventoNFSeCollection;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    property NFSeChave: String read FNFSeChave write FNFSeChave;
    property Protocolo: String read FProtocolo write FProtocolo;
    property DhRecbto: TDateTime read FDhRecbto write FDhRecbto;
    property XMotivo: String read FXMotivo write FXMotivo;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;

    property protNFSe: TProcNFSe read FprotNFSe;
    property retCancNFSe: TRetCancNFSe read FretCancNFSe;
    property procEventoNFSe: TRetEventoNFSeCollection read FprocEventoNFSe;
  end;

  { TNFSeInutilizacao }

  TNFSeInutilizacao = class(TNFSeWebService)
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

    FXML_ProcInutNFSe: String;

    procedure SetJustificativa(AValue: String);
    function GerarPathPorCNPJ: String;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure SalvarEnvio; override;
    function TratarResposta: Boolean; override;
    procedure SalvarResposta; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;

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

    property XML_ProcInutNFSe: String read FXML_ProcInutNFSe write FXML_ProcInutNFSe;
  end;

  { TNFSeConsultaCadastro }

  TNFSeConsultaCadastro = class(TNFSeWebService)
  private
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
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarUFSoap: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

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

  { TNFSeEnvEvento }

  TNFSeEnvEvento = class(TNFSeWebService)
  private
    FidLote: integer;
    Fversao: String;
    FEvento: TEventoNFSe;
    FcStat: integer;
    FxMotivo: String;
    FTpAmb: TpcnTipoAmbiente;

    FEventoRetorno: TRetEventoNFSe;

    function GerarPathEvento: String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    procedure SalvarEnvio; override;
    function TratarResposta: Boolean; override;
    procedure SalvarResposta; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; AEvento: TEventoNFSe); reintroduce; overload;
    destructor Destroy; override;

    property idLote: integer read FidLote write FidLote;
    property versao: String read Fversao write Fversao;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;

    property EventoRetorno: TRetEventoNFSe read FEventoRetorno;
  end;

  { TNFSeConsNFSeDest }

  TNFSeConsNFSeDest = class(TNFSeWebService)
  private
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FCNPJ: String;
    FindEmi: TpcnIndicadorEmissor;
    FindNFSe: TpcnIndicadorNFSe;
    FultNSU: String;

    FretConsNFSeDest: TretConsNFSeDest;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    property versao: String read Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property CNPJ: String read FCNPJ write FCNPJ;
    property indNFSe: TpcnIndicadorNFSe read FindNFSe write FindNFSe;
    property indEmi: TpcnIndicadorEmissor read FindEmi write FindEmi;
    property ultNSU: String read FultNSU write FultNSU;

    property retConsNFSeDest: TretConsNFSeDest read FretConsNFSeDest;
  end;

  { TNFSeDownloadNFSe }

  TNFSeDownloadNFSe = class(TNFSeWebService)
  private
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FCNPJ: String;
    FDownload: TDownLoadNFSe;

    FretDownloadNFSe: TretDownloadNFSe;

    function GerarPathDownload: String;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    constructor Create(AOwner: TACBrDFe; ADownload: TDownloadNFSe);
      reintroduce; overload;
    destructor Destroy; override;

    property versao: String read Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property CNPJ: String read FCNPJ write FCNPJ;

    property retDownloadNFSe: TretDownloadNFSe read FretDownloadNFSe;
  end;

  { TAdministrarCSCNFCe }

  TAdministrarCSCNFCe = class(TNFSeWebService)
  private
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
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

    property versao: String read Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property RaizCNPJ: String read FRaizCNPJ write FRaizCNPJ;
    property indOP: TpcnIndOperacao read FindOP write FindOP;
    property idCsc: integer read FidCsc write FidCsc;
    property codigoCsc: String read FcodigoCsc write FcodigoCsc;

    property retAdmCSCNFCe: TRetAdmCSCNFCe read FretAdmCSCNFCe;
  end;

  { TDistribuicaoDFe }

  TDistribuicaoDFe = class(TNFSeWebService)
  private
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FcUFAutor: integer;
    FCNPJCPF: String;
    FultNSU: String;
    FNSU: String;

    FretDistDFeInt: TretDistDFeInt;

    function GerarPathDistribuicao: String;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    property versao: String read Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property cUFAutor: integer read FcUFAutor write FcUFAutor;
    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property ultNSU: String read FultNSU write FultNSU;
    property NSU: String read FNSU write FNSU;

    property retDistDFeInt: TretDistDFeInt read FretDistDFeInt;
  end;
*)
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
    FEnviarLoteRPS: TNFSeEnviarLoteRPS;
    (*
    FRetorno: TNFSeRetRecepcao;
    FRecibo: TNFSeRecibo;
    FConsulta: TNFSeConsulta;
    FInutilizacao: TNFSeInutilizacao;
    FConsultaCadastro: TNFSeConsultaCadastro;
    FEnvEvento: TNFSeEnvEvento;
    FConsNFSeDest: TNFSeConsNFSeDest;
    FDownloadNFSe: TNFSeDownloadNFSe;
    FAdministrarCSCNFCe: TAdministrarCSCNFCe;
    FDistribuicaoDFe: TDistribuicaoDFe;
*)
    FEnvioWebService: TNFSeEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia(ALote: integer; const ASincrono: Boolean = False): Boolean;
      overload;
    function Envia(ALote: String; const ASincrono: Boolean = False): Boolean;
      overload;
    procedure Inutiliza(CNPJ, AJustificativa: String;
      Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer);

    property ACBrNFSe: TACBrDFe read FACBrNFSe write FACBrNFSe;
    property EnviarLoteRPS: TNFSeEnviarLoteRPS read FEnviarLoteRPS write FEnviarLoteRPS;
    (*
    property Retorno: TNFSeRetRecepcao read FRetorno write FRetorno;
    property Recibo: TNFSeRecibo read FRecibo write FRecibo;
    property Consulta: TNFSeConsulta read FConsulta write FConsulta;
    property Inutilizacao: TNFSeInutilizacao read FInutilizacao write FInutilizacao;
    property ConsultaCadastro: TNFSeConsultaCadastro
      read FConsultaCadastro write FConsultaCadastro;
    property EnvEvento: TNFSeEnvEvento read FEnvEvento write FEnvEvento;
    property ConsNFSeDest: TNFSeConsNFSeDest read FConsNFSeDest write FConsNFSeDest;
    property DownloadNFSe: TNFSeDownloadNFSe read FDownloadNFSe write FDownloadNFSe;
    property AdministrarCSCNFCe: TAdministrarCSCNFCe
      read FAdministrarCSCNFCe write FAdministrarCSCNFCe;
    property DistribuicaoDFe: TDistribuicaoDFe
      read FDistribuicaoDFe write FDistribuicaoDFe;
    *)
    property EnvioWebService: TNFSeEnvioWebService read FEnvioWebService write FEnvioWebService;
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
  FPLayout := LayNfseRecepcaoLote;
  FPStatus := stNFSeIdle;
end;

procedure TNFSeWebService.ConfigurarSoapDEPC;
begin
  FPSoapVersion := 'soap';
  FPHeaderElement := 'sceCabecMsg';
  FPSoapEnvelopeAtributtes :=
    'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
    'xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
    'xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/';
  FPBodyElement := 'sceDadosMsg';
end;

function TNFSeWebService.ExtrairModeloChaveAcesso(AChaveNFSe: String): String;
begin
  AChaveNFSe := OnlyNumber(AChaveNFSe);
  if ValidarChave('NFSe' + AChaveNFSe) then
    Result := copy(AChaveNFSe, 21, 2)
  else
    Result := '';
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

{ TNFSeEnviarLoteRPS }

constructor TNFSeEnviarLoteRPS.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
//  FSincrono := False;

  FPStatus := stNFSeRecepcao;
  FPLayout := LayNfseRecepcaoLote;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

//  FNFSeRetornoSincrono := nil;
  FNFSeRetorno := nil;
end;

destructor TNFSeEnviarLoteRPS.Destroy;
begin
//  if Assigned(FNFSeRetornoSincrono) then
//    FNFSeRetornoSincrono.Free;

  if Assigned(FNFSeRetorno) then
    FNFSeRetorno.Free;

  inherited Destroy;
end;

function TNFSeEnviarLoteRPS.GetLote: String;
begin
//  Result := Trim(FLote);
end;

function TNFSeEnviarLoteRPS.GetRecibo: String;
begin
//  Result := Trim(FRecibo);
end;

procedure TNFSeEnviarLoteRPS.DefinirURL;
begin

  inherited DefinirURL;
end;

procedure TNFSeEnviarLoteRPS.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeEnviarLoteRPS2';
  FPSoapAction := FPServico;
end;

procedure TNFSeEnviarLoteRPS.DefinirDadosMsg;
var
  I: integer;
  vNotas: String;
  indSinc: String;
begin
(*
  if (FPLayout = LayNFSeAutorizacao) or (FPConfiguracoesNFSe.Geral.ModeloDF = moNFCe) or
    (FPConfiguracoesNFSe.Geral.VersaoDF = ve310) then
    indSinc := '<indSinc>' + IfThen(FSincrono, '1', '0') + '</indSinc>'
  else
    indSinc := '';

  vNotas := '';
  for I := 0 to FNotasFiscais.Count - 1 do
    vNotas := vNotas + '<NFSe' + RetornarConteudoEntre(
      FNotasFiscais.Items[I].XMLAssinado, '<NFSe', '</NFSe>') + '</NFSe>';

  FPDadosMsg := '<enviNFSe xmlns="http://www.portalfiscal.inf.br/NFSe" versao="' +
    FPVersaoServico + '">' + '<idLote>' + FLote + '</idLote>' + indSinc +
    vNotas + '</enviNFSe>';

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));
  FRecibo := '';
*)
end;

function TNFSeEnviarLoteRPS.TratarResposta: Boolean;
var
  I: integer;
  chNFSe, NomeArquivo: String;
//  AProcNFSe: TProcNFSe;
begin
(*
  if FPLayout = LayNFSeAutorizacao then
  begin
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeAutorizacaoLoteResult');
    if FPRetWS = '' then
      FPRetWS := SeparaDados(FPRetornoWS, 'NFSeAutorizacaoResult');
  end
  else
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeEnviarLoteRPSLote2Result');

  if ((FPConfiguracoesNFSe.Geral.ModeloDF = moNFCe) or
    (FPConfiguracoesNFSe.Geral.VersaoDF = ve310)) and FSincrono then
  begin
    FNFSeRetornoSincrono := TRetConsSitNFSe.Create;

    if pos('retEnviNFSe', FPRetWS) > 0 then
      FNFSeRetornoSincrono.Leitor.Arquivo :=
        StringReplace(FPRetWS, 'retEnviNFSe', 'retConsSitNFSe',
        [rfReplaceAll, rfIgnoreCase])
    else if pos('retConsReciNFSe', FPRetWS) > 0 then
      FNFSeRetornoSincrono.Leitor.Arquivo :=
        StringReplace(FPRetWS, 'retConsReciNFSe', 'retConsSitNFSe',
        [rfReplaceAll, rfIgnoreCase])
    else
      FNFSeRetornoSincrono.Leitor.Arquivo := FPRetWS;

    FNFSeRetornoSincrono.LerXml;

    Fversao := FNFSeRetornoSincrono.versao;
    FTpAmb := FNFSeRetornoSincrono.TpAmb;
    FverAplic := FNFSeRetornoSincrono.verAplic;

    // Consta no Retorno da NFC-e
    FRecibo := FNFSeRetornoSincrono.nRec;
    FcUF := FNFSeRetornoSincrono.cUF;
    chNFSe := FNFSeRetornoSincrono.ProtNFSe.chNFSe;

    if (FNFSeRetornoSincrono.protNFSe.cStat > 0) then
      FcStat := FNFSeRetornoSincrono.protNFSe.cStat
    else
      FcStat := FNFSeRetornoSincrono.cStat;

    if (FNFSeRetornoSincrono.protNFSe.xMotivo <> '') then
    begin
      FPMsg := FNFSeRetornoSincrono.protNFSe.xMotivo;
      FxMotivo := FNFSeRetornoSincrono.protNFSe.xMotivo;
    end
    else
    begin
      FPMsg := FNFSeRetornoSincrono.xMotivo;
      FxMotivo := FNFSeRetornoSincrono.xMotivo;
    end;

    // Verificar se a NF-e foi autorizada com sucesso
    Result := (FNFSeRetornoSincrono.cStat = 104) and
      (TACBrNFSe(FPDFeOwner).CstatProcessado(FNFSeRetornoSincrono.protNFSe.cStat));

    NomeArquivo := PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) + chNFSe;

    if Result then
    begin
      for I := 0 to TACBrNFSe(FPDFeOwner).NotasFiscais.Count - 1 do
      begin
        if OnlyNumber(chNFSe) = TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NumID then
        begin
          if (TACBrNFSe(FPDFeOwner).Configuracoes.Geral.ValidarDigest) and
            (TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NFSe.signature.DigestValue <>
            FNFSeRetornoSincrono.protNFSe.digVal) and
            (FNFSeRetornoSincrono.protNFSe.digVal <> '') then
          begin
            raise EACBrNFSeException.Create('DigestValue do documento ' +
              TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NumID + ' não coNFSere.');
          end;
          with TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I] do
          begin
            NFSe.procNFSe.cStat := FNFSeRetornoSincrono.protNFSe.cStat;
            NFSe.procNFSe.tpAmb := FNFSeRetornoSincrono.tpAmb;
            NFSe.procNFSe.verAplic := FNFSeRetornoSincrono.verAplic;
            NFSe.procNFSe.chNFSe := FNFSeRetornoSincrono.ProtNFSe.chNFSe;
            NFSe.procNFSe.dhRecbto := FNFSeRetornoSincrono.protNFSe.dhRecbto;
            NFSe.procNFSe.nProt := FNFSeRetornoSincrono.ProtNFSe.nProt;
            NFSe.procNFSe.digVal := FNFSeRetornoSincrono.protNFSe.digVal;
            NFSe.procNFSe.xMotivo := FNFSeRetornoSincrono.protNFSe.xMotivo;
          end;

          if (FileExists(NomeArquivo + '-NFSe.xml')) or
            NaoEstaVazio(TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NomeArq) then
          begin
            AProcNFSe := TProcNFSe.Create;
            try
              if NaoEstaVazio(TACBrNFSe(
                FPDFeOwner).NotasFiscais.Items[I].NomeArq) then
                AProcNFSe.PathNFSe := TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].NomeArq
              else
                AProcNFSe.PathNFSe := NomeArquivo + '-NFSe.xml';

              AProcNFSe.PathRetConsSitNFSe := '';
              AProcNFSe.PathRetConsReciNFSe := '';
              AProcNFSe.tpAmb := FNFSeRetornoSincrono.protNFSe.tpAmb;
              AProcNFSe.verAplic := FNFSeRetornoSincrono.protNFSe.verAplic;
              AProcNFSe.chNFSe := FNFSeRetornoSincrono.protNFSe.chNFSe;
              AProcNFSe.dhRecbto := FNFSeRetornoSincrono.protNFSe.dhRecbto;
              AProcNFSe.nProt := FNFSeRetornoSincrono.protNFSe.nProt;
              AProcNFSe.digVal := FNFSeRetornoSincrono.protNFSe.digVal;
              AProcNFSe.cStat := FNFSeRetornoSincrono.protNFSe.cStat;
              AProcNFSe.xMotivo := FNFSeRetornoSincrono.protNFSe.xMotivo;

              AProcNFSe.Versao := FPVersaoServico;
              AProcNFSe.GerarXML;

              if NaoEstaVazio(AProcNFSe.Gerador.ArquivoFormatoXML) then
                AProcNFSe.Gerador.SalvarArquivo(AProcNFSe.PathNFSe);
            finally
              AProcNFSe.Free;
            end;
          end;

          if FPConfiguracoesNFSe.Arquivos.Salvar then
          begin
            if FPConfiguracoesNFSe.Arquivos.SalvarApenasNFSeProcessadas then
            begin
              if TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].Processada then
                TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].GravarXML;
            end
            else
              TACBrNFSe(FPDFeOwner).NotasFiscais.Items[I].GravarXML;
          end;

          Break;
        end;
      end;
    end;
  end
  else
  begin
    FNFSeRetorno := TretEnvNFSe.Create;

    FNFSeRetorno.Leitor.Arquivo := FPRetWS;
    FNFSeRetorno.LerXml;

    Fversao := FNFSeRetorno.versao;
    FTpAmb := FNFSeRetorno.TpAmb;
    FverAplic := FNFSeRetorno.verAplic;
    FcStat := FNFSeRetorno.cStat;
    FxMotivo := FNFSeRetorno.xMotivo;
    FdhRecbto := FNFSeRetorno.infRec.dhRecbto;
    FTMed := FNFSeRetorno.infRec.tMed;
    FcUF := FNFSeRetorno.cUF;
    FPMsg := FNFSeRetorno.xMotivo;
    FRecibo := FNFSeRetorno.infRec.nRec;

    Result := (FNFSeRetorno.CStat = 103);
  end;
*)
end;

procedure TNFSeEnviarLoteRPS.FinalizarServico;
begin
  inherited FinalizarServico;

//  if Assigned(FNFSeRetornoSincrono) then
//    FreeAndNil(FNFSeRetornoSincrono);

  if Assigned(FNFSeRetorno) then
    FreeAndNil(FNFSeRetorno);
end;

function TNFSeEnviarLoteRPS.GerarMsgLog: String;
begin
(*
  if Assigned(FNFSeRetorno) then
    Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                             'Ambiente: %s ' + LineBreak +
                             'Versão Aplicativo: %s ' + LineBreak +
                             'Status Código: %s ' + LineBreak +
                             'Status Descrição: %s ' + LineBreak +
                             'UF: %s ' + sLineBreak +
                             'Recibo: %s ' + LineBreak +
                             'Recebimento: %s ' + LineBreak +
                             'Tempo Médio: %s ' + LineBreak),
                     [FNFSeRetorno.versao,
                      TpAmbToStr(FNFSeRetorno.TpAmb),
                      FNFSeRetorno.verAplic,
                      IntToStr(FNFSeRetorno.cStat),
                      FNFSeRetorno.xMotivo,
                      CodigoParaUF(FNFSeRetorno.cUF),
                      FNFSeRetorno.infRec.nRec,
                      IfThen(FNFSeRetorno.InfRec.dhRecbto = 0, '',
                             FormatDateTimeBr(FNFSeRetorno.InfRec.dhRecbto)),
                      IntToStr(FNFSeRetorno.InfRec.TMed)])
  else
    Result := '';
*)
end;

function TNFSeEnviarLoteRPS.GerarPrefixoArquivo: String;
begin
(*
  if Assigned(FNFSeRetornoSincrono) then  // Esta procesando nome do Retorno Sincrono ?
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
*)
end;

(*
{ TNFSeRetRecepcao }

constructor TNFSeRetRecepcao.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
  FNFSeRetorno := TRetConsReciNFSe.Create;

  FPStatus := stNFSeRetRecepcao;
  FPLayout := LayNFSeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';
end;

destructor TNFSeRetRecepcao.Destroy;
begin
  FNFSeRetorno.Free;

  inherited Destroy;
end;

function TNFSeRetRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

function TNFSeRetRecepcao.TratarRespostaFinal: Boolean;
var
  I, J: integer;
  AProcNFSe: TProcNFSe;
  AInfProt: TProtNFSeCollection;
begin
  Result := False;

  AInfProt := FNFSeRetorno.ProtNFSe;

  if (AInfProt.Count > 0) then
  begin
    FPMsg := FNFSeRetorno.ProtNFSe.Items[0].xMotivo;
    FxMotivo := FNFSeRetorno.ProtNFSe.Items[0].xMotivo;
  end;

  //Setando os retornos das notas fiscais;
  for I := 0 to AInfProt.Count - 1 do
  begin
    for J := 0 to FNotasFiscais.Count - 1 do
    begin
      if OnlyNumber(AInfProt.Items[I].chNFSe) = FNotasFiscais.Items[J].NumID then
      begin
        if (TACBrNFSe(FPDFeOwner).Configuracoes.Geral.ValidarDigest) and
          (FNotasFiscais.Items[J].NFSe.signature.DigestValue <>
          AInfProt.Items[I].digVal) and (AInfProt.Items[I].digVal <> '') then
        begin
          raise EACBrNFSeException.Create('DigestValue do documento ' +
            FNotasFiscais.Items[J].NumID + ' não coNFSere.');
        end;

        FNotasFiscais.Items[J].NFSe.procNFSe.tpAmb := AInfProt.Items[I].tpAmb;
        FNotasFiscais.Items[J].NFSe.procNFSe.verAplic := AInfProt.Items[I].verAplic;
        FNotasFiscais.Items[J].NFSe.procNFSe.chNFSe := AInfProt.Items[I].chNFSe;
        FNotasFiscais.Items[J].NFSe.procNFSe.dhRecbto := AInfProt.Items[I].dhRecbto;
        FNotasFiscais.Items[J].NFSe.procNFSe.nProt := AInfProt.Items[I].nProt;
        FNotasFiscais.Items[J].NFSe.procNFSe.digVal := AInfProt.Items[I].digVal;
        FNotasFiscais.Items[J].NFSe.procNFSe.cStat := AInfProt.Items[I].cStat;
        FNotasFiscais.Items[J].NFSe.procNFSe.xMotivo := AInfProt.Items[I].xMotivo;

        if FPConfiguracoesNFSe.Arquivos.Salvar or NaoEstaVazio(
          FNotasFiscais.Items[J].NomeArq) then
        begin
          if FileExists(PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                        AInfProt.Items[I].chNFSe + '-NFSe.xml') and
             FileExists(PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                        FNFSeRetorno.nRec + '-pro-rec.xml') then
          begin
            AProcNFSe := TProcNFSe.Create;
            try
              AProcNFSe.PathNFSe :=
                PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                AInfProt.Items[I].chNFSe + '-NFSe.xml';
              AProcNFSe.PathRetConsReciNFSe :=
                PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                FNFSeRetorno.nRec + '-pro-rec.xml';

              AProcNFSe.Versao := FPVersaoServico;
              AProcNFSe.GerarXML;

              if NaoEstaVazio(AProcNFSe.Gerador.ArquivoFormatoXML) then
              begin
                if NaoEstaVazio(FNotasFiscais.Items[J].NomeArq) then
                  AProcNFSe.Gerador.SalvarArquivo(FNotasFiscais.Items[J].NomeArq)
                else
                  AProcNFSe.Gerador.SalvarArquivo(
                    PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) +
                    AInfProt.Items[I].chNFSe + '-NFSe.xml');
              end;
            finally
              AProcNFSe.Free;
            end;
          end;
        end;

        if FPConfiguracoesNFSe.Arquivos.Salvar then
        begin
          if FPConfiguracoesNFSe.Arquivos.SalvarApenasNFSeProcessadas then
          begin
            if FNotasFiscais.Items[J].Processada then
              FNotasFiscais.Items[J].GravarXML;
          end
          else
            FNotasFiscais.Items[J].GravarXML;
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
      FPMsg := FPMsg + IntToStr(FNotasFiscais.Items[I].NFSe.Ide.nNF) +
        '->' + FNotasFiscais.Items[I].Msg + LineBreak;
  end;

  if AInfProt.Count > 0 then
  begin
    FChaveNFSe := AInfProt.Items[0].chNFSe;
    FProtocolo := AInfProt.Items[0].nProt;
    FcStat := AInfProt.Items[0].cStat;
  end;
end;

function TNFSeRetRecepcao.Executar: Boolean;
var
  IntervaloTentativas, Tentativas: integer;
begin
  Result := False;

  TACBrNFSe(FPDFeOwner).SetStatus(stNFSeRetRecepcao);
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
    TACBrNFSe(FPDFeOwner).SetStatus(stIdle);
  end;

  if FNFSeRetorno.CStat = 104 then  // Lote processado ?
    Result := TratarRespostaFinal;
end;

procedure TNFSeRetRecepcao.DefinirURL;
begin
  if TACBrNFSe(FPDFeOwner).EhAutorizacao then
    FPLayout := LayNFSeRetAutorizacao
  else
    FPLayout := LayNFSeRetRecepcao;

  inherited DefinirURL;
end;

procedure TNFSeRetRecepcao.DefinirServicoEAction;
begin
  if FPLayout = LayNFSeRetAutorizacao then
    FPServico := GetUrlWsd + 'NFSeRetAutorizacao'
  else
    FPServico := GetUrlWsd + 'NFSeRetRecepcao2';

  FPSoapAction := FPServico;
end;

procedure TNFSeRetRecepcao.DefinirDadosMsg;
var
  ConsReciNFSe: TConsReciNFSe;
begin
  ConsReciNFSe := TConsReciNFSe.Create;
  try
    ConsReciNFSe.tpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    ConsReciNFSe.nRec := FRecibo;
    ConsReciNFSe.Versao := FPVersaoServico;
    ConsReciNFSe.GerarXML;

    FPDadosMsg := ConsReciNFSe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciNFSe.Free;
  end;
end;

function TNFSeRetRecepcao.TratarResposta: Boolean;
begin
  if FPLayout = LayNFSeRetAutorizacao then
  begin
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetAutorizacaoResult');
    if FPRetWS = '' then
      FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetAutorizacaoLoteResult');
  end
  else
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetRecepcao2Result');

  // Limpando variaveis internas
  FNFSeRetorno.Free;
  FNFSeRetorno := TRetConsReciNFSe.Create;

  FNFSeRetorno.Leitor.Arquivo := FPRetWS;
  FNFSeRetorno.LerXML;

  Fversao := FNFSeRetorno.versao;
  FTpAmb := FNFSeRetorno.TpAmb;
  FverAplic := FNFSeRetorno.verAplic;
  FcStat := FNFSeRetorno.cStat;
  FcUF := FNFSeRetorno.cUF;
  FPMsg := FNFSeRetorno.xMotivo;
  FxMotivo := FNFSeRetorno.xMotivo;
  FcMsg := FNFSeRetorno.cMsg;
  FxMsg := FNFSeRetorno.xMsg;

  Result := (FNFSeRetorno.CStat = 105); // Lote em Processamento
end;

procedure TNFSeRetRecepcao.FinalizarServico;
begin
  // Sobrescrito, para não liberar para stIdle... não ainda...;
end;

function TNFSeRetRecepcao.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'cMsg: %s ' + LineBreak +
                           'xMsg: %s ' + LineBreak),
                   [FNFSeRetorno.versao, TpAmbToStr(FNFSeRetorno.tpAmb),
                    FNFSeRetorno.verAplic, FNFSeRetorno.nRec,
                    IntToStr(FNFSeRetorno.cStat), FNFSeRetorno.xMotivo,
                    CodigoParaUF(FNFSeRetorno.cUF), IntToStr(FNFSeRetorno.cMsg),
                    FNFSeRetorno.xMsg]);
end;

function TNFSeRetRecepcao.GerarPrefixoArquivo: String;
begin
  Result := Recibo;
end;

{ TNFSeRecibo }

constructor TNFSeRecibo.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FNFSeRetorno := TRetConsReciNFSe.Create;

  FPStatus := stNFSeRecibo;
  FPLayout := LayNFSeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';
end;

destructor TNFSeRecibo.Destroy;
begin
  FNFSeRetorno.Free;

  inherited Destroy;
end;

procedure TNFSeRecibo.DefinirServicoEAction;
begin
  if FPLayout = LayNFSeRetAutorizacao then
    FPServico := GetUrlWsd + 'NFSeRetAutorizacao'
  else
    FPServico := GetUrlWsd + 'NFSeRetRecepcao2';

  FPSoapAction := FPServico;
end;

procedure TNFSeRecibo.DefinirURL;
begin
  if TACBrNFSe(FPDFeOwner).EhAutorizacao then
    FPLayout := LayNFSeRetAutorizacao
  else
    FPLayout := LayNFSeRetRecepcao;

  inherited DefinirURL;
end;

procedure TNFSeRecibo.DefinirDadosMsg;
var
  ConsReciNFSe: TConsReciNFSe;
begin
  ConsReciNFSe := TConsReciNFSe.Create;
  try
    ConsReciNFSe.tpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    ConsReciNFSe.nRec := FRecibo;
    ConsReciNFSe.Versao := FPVersaoServico;
    ConsReciNFSe.GerarXML;

    FPDadosMsg := ConsReciNFSe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciNFSe.Free;
  end;
end;

function TNFSeRecibo.TratarResposta: Boolean;
begin
  if FPLayout = LayNFSeRetAutorizacao then
  begin
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetAutorizacaoResult');
    if FPRetWS = '' then
      FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetAutorizacaoLoteResult');
  end
  else
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRetRecepcao2Result');

  // Limpando variaveis internas
  FNFSeRetorno.Free;
  FNFSeRetorno := TRetConsReciNFSe.Create;

  FNFSeRetorno.Leitor.Arquivo := FPRetWS;
  FNFSeRetorno.LerXML;

  Fversao := FNFSeRetorno.versao;
  FTpAmb := FNFSeRetorno.TpAmb;
  FverAplic := FNFSeRetorno.verAplic;
  FcStat := FNFSeRetorno.cStat;
  FxMotivo := FNFSeRetorno.xMotivo;
  FcUF := FNFSeRetorno.cUF;
  FxMsg := FNFSeRetorno.xMsg;
  FcMsg := FNFSeRetorno.cMsg;
  FPMsg := FxMotivo;

  Result := (FNFSeRetorno.CStat = 104);
end;

function TNFSeRecibo.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FNFSeRetorno.versao, TpAmbToStr(FNFSeRetorno.TpAmb),
                   FNFSeRetorno.verAplic, FNFSeRetorno.nRec,
                   IntToStr(FNFSeRetorno.cStat),
                   FNFSeRetorno.ProtNFSe.Items[0].xMotivo,
                   CodigoParaUF(FNFSeRetorno.cUF)]);
end;

{ TNFSeConsulta }

constructor TNFSeConsulta.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FprotNFSe := TProcNFSe.Create;
  FretCancNFSe := TRetCancNFSe.Create;
  FprocEventoNFSe := TRetEventoNFSeCollection.Create(AOwner);

  FPStatus := stNFSeConsulta;
  FPLayout := LayNFSeConsulta;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';
end;

destructor TNFSeConsulta.Destroy;
begin
  FprotNFSe.Free;
  FretCancNFSe.Free;
  if Assigned(FprocEventoNFSe) then
    FprocEventoNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeConsulta.DefinirServicoEAction;
begin
  if (FPConfiguracoesNFSe.Geral.ModeloDF = moNFSe) and
     (FPConfiguracoesNFSe.Geral.VersaoDF = ve310) and
     (FPConfiguracoesNFSe.WebServices.UFCodigo in [29, 41]) then // 29 = BA, 41 = PR
    FPServico := GetUrlWsd + 'NFSeConsulta'
  else
    FPServico := GetUrlWsd + 'NFSeConsulta2';

  FPSoapAction := FPServico;
end;

procedure TNFSeConsulta.DefinirDadosMsg;
var
  ConsSitNFSe: TConsSitNFSe;
  OK: Boolean;
begin
  OK := False;
  ConsSitNFSe := TConsSitNFSe.Create;
  try
    ConsSitNFSe.TpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    ConsSitNFSe.chNFSe := FNFSeChave;

    FPConfiguracoesNFSe.Geral.ModeloDF :=
      StrToModeloDF(OK, ExtrairModeloChaveAcesso(ConsSitNFSe.chNFSe));

    ConsSitNFSe.Versao := FPVersaoServico;
    ConsSitNFSe.GerarXML;

    FPDadosMsg := ConsSitNFSe.Gerador.ArquivoFormatoXML;
  finally
    ConsSitNFSe.Free;
  end;
end;

function TNFSeConsulta.TratarResposta: Boolean;
var
  NFSeRetorno: TRetConsSitNFSe;
  NFCancelada, Atualiza: Boolean;
  aEventos, aMsg, NomeArquivo: String;
  AProcNFSe: TProcNFSe;
  I, J: integer;
begin
  NFSeRetorno := TRetConsSitNFSe.Create;

  try
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeConsultaNF2Result');
    if FPRetWS = '' then
      FPRetWS := SeparaDados(FPRetornoWS, 'NFSeConsultaNFResult');

    NFSeRetorno.Leitor.Arquivo := FPRetWS;
    NFSeRetorno.LerXML;

    NFCancelada := False;
    aEventos := '';

    // <retConsSitNFSe> - Retorno da consulta da situação da NF-e
    // Este é o status oficial da NF-e
    Fversao := NFSeRetorno.versao;
    FTpAmb := NFSeRetorno.tpAmb;
    FverAplic := NFSeRetorno.verAplic;
    FcStat := NFSeRetorno.cStat;
    FXMotivo := NFSeRetorno.xMotivo;
    FcUF := NFSeRetorno.cUF;
    FNFSeChave := NFSeRetorno.chNFSe;
    FPMsg := FXMotivo;



    // Verifica se a nota fiscal está cancelada pelo método antigo. Se estiver,
    // então NFCancelada será True e já atribui Protocolo, Data e Mensagem
    if NFSeRetorno.retCancNFSe.cStat > 0 then
    begin
      FRetCancNFSe.versao := NFSeRetorno.retCancNFSe.versao;
      FretCancNFSe.tpAmb := NFSeRetorno.retCancNFSe.tpAmb;
      FretCancNFSe.verAplic := NFSeRetorno.retCancNFSe.verAplic;
      FretCancNFSe.cStat := NFSeRetorno.retCancNFSe.cStat;
      FretCancNFSe.xMotivo := NFSeRetorno.retCancNFSe.xMotivo;
      FretCancNFSe.cUF := NFSeRetorno.retCancNFSe.cUF;
      FretCancNFSe.chNFSe := NFSeRetorno.retCancNFSe.chNFSe;
      FretCancNFSe.dhRecbto := NFSeRetorno.retCancNFSe.dhRecbto;
      FretCancNFSe.nProt := NFSeRetorno.retCancNFSe.nProt;

      NFCancelada := True;
      FProtocolo := NFSeRetorno.retCancNFSe.nProt;
      FDhRecbto := NFSeRetorno.retCancNFSe.dhRecbto;
      FPMsg := NFSeRetorno.xMotivo;
    end;

    // <protNFSe> - Retorno dos dados do ENVIO da NF-e
    // Considerá-los apenas se não existir nenhum evento de cancelamento (110111)
    FprotNFSe.PathNFSe := NFSeRetorno.protNFSe.PathNFSe;
    FprotNFSe.PathRetConsReciNFSe := NFSeRetorno.protNFSe.PathRetConsReciNFSe;
    FprotNFSe.PathRetConsSitNFSe := NFSeRetorno.protNFSe.PathRetConsSitNFSe;
    FprotNFSe.PathRetConsSitNFSe := NFSeRetorno.protNFSe.PathRetConsSitNFSe;
    FprotNFSe.tpAmb := NFSeRetorno.protNFSe.tpAmb;
    FprotNFSe.verAplic := NFSeRetorno.protNFSe.verAplic;
    FprotNFSe.chNFSe := NFSeRetorno.protNFSe.chNFSe;
    FprotNFSe.dhRecbto := NFSeRetorno.protNFSe.dhRecbto;
    FprotNFSe.nProt := NFSeRetorno.protNFSe.nProt;
    FprotNFSe.digVal := NFSeRetorno.protNFSe.digVal;
    FprotNFSe.cStat := NFSeRetorno.protNFSe.cStat;
    FprotNFSe.xMotivo := NFSeRetorno.protNFSe.xMotivo;

    if Assigned(NFSeRetorno.procEventoNFSe) and (NFSeRetorno.procEventoNFSe.Count > 0) then
    begin
      aEventos := '=====================================================' +
        LineBreak + '================== Eventos da NF-e ==================' +
        LineBreak + '=====================================================' +
        LineBreak + '' + LineBreak + 'Quantidade total de eventos: ' +
        IntToStr(NFSeRetorno.procEventoNFSe.Count);

      FprocEventoNFSe.Clear;
      for I := 0 to NFSeRetorno.procEventoNFSe.Count - 1 do
      begin
        with FprocEventoNFSe.Add.RetEventoNFSe do
        begin
          idLote := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.idLote;
          tpAmb := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.tpAmb;
          verAplic := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.verAplic;
          cOrgao := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.cOrgao;
          cStat := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.cStat;
          xMotivo := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.xMotivo;
          XML := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.XML;

          INFSevento.ID := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.ID;
          INFSevento.tpAmb := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.tpAmb;
          INFSevento.CNPJ := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.CNPJ;
          INFSevento.chNFSe := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.chNFSe;
          INFSevento.dhEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.dhEvento;
          INFSevento.TpEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.TpEvento;
          INFSevento.nSeqEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.nSeqEvento;
          INFSevento.VersaoEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.VersaoEvento;
          INFSevento.DetEvento.xCorrecao := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.DetEvento.xCorrecao;
          INFSevento.DetEvento.xCondUso := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.DetEvento.xCondUso;
          INFSevento.DetEvento.nProt := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.DetEvento.nProt;
          INFSevento.DetEvento.xJust := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.INFSevento.DetEvento.xJust;

          retEvento.Clear;
          for J := 0 to NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Count-1 do
          begin
            with retEvento.Add.RetINFSevento do
            begin
              Id := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.Id;
              tpAmb := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.tpAmb;
              verAplic := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.verAplic;
              cOrgao := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.cOrgao;
              cStat := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.cStat;
              xMotivo := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.xMotivo;
              chNFSe := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.chNFSe;
              tpEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.tpEvento;
              xEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.xEvento;
              nSeqEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.nSeqEvento;
              CNPJDest := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.CNPJDest;
              emailDest := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.emailDest;
              dhRegEvento := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.dhRegEvento;
              nProt := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.nProt;
              XML := NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe.retEvento.Items[J].RetINFSevento.XML;
            end;
          end;
        end;

        with NFSeRetorno.procEventoNFSe.Items[I].RetEventoNFSe do
        begin
          aEventos := aEventos + LineBreak + LineBreak +
            Format(ACBrStr('Número de sequência: %s ' + LineBreak +
                           'Código do evento: %s ' + LineBreak +
                           'Descrição do evento: %s ' + LineBreak +
                           'Status do evento: %s ' + LineBreak +
                           'Descrição do status: %s ' + LineBreak +
                           'Protocolo: %s ' + LineBreak +
                           'Data/Hora do registro: %s '),
                   [IntToStr(INFSevento.nSeqEvento),
                    TpEventoToStr(INFSevento.TpEvento),
                    INFSevento.DescEvento,
                    IntToStr(retEvento.Items[J].RetINFSevento.cStat),
                    retEvento.Items[J].RetINFSevento.xMotivo,
                    retEvento.Items[J].RetINFSevento.nProt,
                    FormatDateTimeBr(retEvento.Items[J].RetINFSevento.dhRegEvento)]);

          if retEvento.Items[J].RetINFSevento.tpEvento = teCancelamento then
          begin
            NFCancelada := True;
            FProtocolo := retEvento.Items[J].RetINFSevento.nProt;
            FDhRecbto := retEvento.Items[J].RetINFSevento.dhRegEvento;
            FPMsg := retEvento.Items[J].RetINFSevento.xMotivo;
          end;
        end;
      end;
    end;

    if not NFCancelada and (NaoEstaVazio(NFSeRetorno.protNFSe.nProt))  then
    begin
      FProtocolo := NFSeRetorno.protNFSe.nProt;
      FDhRecbto := NFSeRetorno.protNFSe.dhRecbto;
      FPMsg := NFSeRetorno.protNFSe.xMotivo;
    end;

    //TODO: Verificar porque monta "aMsg", pois ela não está sendo usada em lugar nenhum
    aMsg := GerarMsgLog;
    if aEventos <> '' then
      aMsg := aMsg + sLineBreak + aEventos;

    Result := (NFSeRetorno.CStat in [100, 101, 110, 150, 151, 155]);

    NomeArquivo := PathWithDelim(FPConfiguracoesNFSe.Arquivos.PathSalvar) + FNFSeChave;

    for i := 0 to TACBrNFSe(FPDFeOwner).NotasFiscais.Count - 1 do
    begin
      with TACBrNFSe(FPDFeOwner).NotasFiscais.Items[i] do
      begin
        if (OnlyNumber(FNFSeChave) = NumID) then
        begin
          Atualiza := True;
          if (NFSeRetorno.CStat in [101, 151, 155]) then
            Atualiza := False;

          // Atualiza o Status da NFSe interna //
          NFSe.procNFSe.cStat := NFSeRetorno.cStat;

          if Atualiza then
          begin
            if (FPConfiguracoesNFSe.Geral.ValidarDigest) and
              (NFSeRetorno.protNFSe.digVal <> '') and
              (NFSe.signature.DigestValue <> NFSeRetorno.protNFSe.digVal) then
            begin
              raise EACBrNFSeException.Create('DigestValue do documento ' +
                NumID + ' não coNFSere.');
            end;

            NFSe.procNFSe.tpAmb := NFSeRetorno.tpAmb;
            NFSe.procNFSe.verAplic := NFSeRetorno.verAplic;
            NFSe.procNFSe.chNFSe := NFSeRetorno.chNFSe;
            NFSe.procNFSe.dhRecbto := FDhRecbto;
            NFSe.procNFSe.nProt := FProtocolo;
            NFSe.procNFSe.digVal := NFSeRetorno.protNFSe.digVal;
            NFSe.procNFSe.cStat := NFSeRetorno.cStat;
            NFSe.procNFSe.xMotivo := NFSeRetorno.xMotivo;

            if FileExists(NomeArquivo + '-NFSe.xml') or NaoEstaVazio(NomeArq) then
            begin
              AProcNFSe := TProcNFSe.Create;
              try
                if NaoEstaVazio(NomeArq) then
                  AProcNFSe.PathNFSe := NomeArq
                else
                  AProcNFSe.PathNFSe := NomeArquivo + '-NFSe.xml';

                AProcNFSe.PathRetConsSitNFSe := NomeArquivo + '-sit.xml';

                if FPConfiguracoesNFSe.Geral.VersaoDF >= ve310 then
                  AProcNFSe.Versao :=
                    TACBrNFSe(FPDFeOwner).LerVersaoDeParams(LayNFSeAutorizacao)
                else
                  AProcNFSe.Versao :=
                    TACBrNFSe(FPDFeOwner).LerVersaoDeParams(LayNFSeEnviarLoteRPS);

                AProcNFSe.GerarXML;

                if NaoEstaVazio(AProcNFSe.Gerador.ArquivoFormatoXML) then
                  AProcNFSe.Gerador.SalvarArquivo(AProcNFSe.PathNFSe);
              finally
                AProcNFSe.Free;
              end;
            end;

            if FPConfiguracoesNFSe.Arquivos.Salvar then
            begin
              if FPConfiguracoesNFSe.Arquivos.SalvarApenasNFSeProcessadas then
              begin
                if Processada then
                  GravarXML();
              end
              else
                GravarXML();
            end;
          end;

          break;
        end;
      end;
    end;

    if (TACBrNFSe(FPDFeOwner).NotasFiscais.Count <= 0) then
    begin
      if FPConfiguracoesNFSe.Arquivos.Salvar then
      begin
        if FileExists(NomeArquivo + '-NFSe.xml') then
        begin
          AProcNFSe := TProcNFSe.Create;
          try
            AProcNFSe.PathNFSe := NomeArquivo + '-NFSe.xml';
            AProcNFSe.PathRetConsSitNFSe := NomeArquivo + '-sit.xml';

            if FPConfiguracoesNFSe.Geral.VersaoDF >= ve310 then
              AProcNFSe.Versao :=
                TACBrNFSe(FPDFeOwner).LerVersaoDeParams(LayNFSeAutorizacao)
            else
              AProcNFSe.Versao := TACBrNFSe(FPDFeOwner).LerVersaoDeParams(LayNFSeEnviarLoteRPS);

            AProcNFSe.GerarXML;

            if NaoEstaVazio(AProcNFSe.Gerador.ArquivoFormatoXML) then
              AProcNFSe.Gerador.SalvarArquivo(AProcNFSe.PathNFSe);
          finally
            AProcNFSe.Free;
          end;
        end;
      end;
    end;
  finally
    NFSeRetorno.Free;
  end;
end;

function TNFSeConsulta.GerarMsgLog: String;
begin
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
                   [Fversao, FNFSeChave, TpAmbToStr(FTpAmb), FverAplic,
                    IntToStr(FcStat), FXMotivo, CodigoParaUF(FcUF), FNFSeChave,
                    FormatDateTimeBr(FDhRecbto), FProtocolo, FprotNFSe.digVal]);
end;

function TNFSeConsulta.GerarPrefixoArquivo: String;
begin
  Result := Trim(FNFSeChave);
end;

{ TNFSeInutilizacao }

constructor TNFSeInutilizacao.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stNFSeInutilizacao;
  FPLayout := LayNFSeInutilizacao;
  FPArqEnv := 'ped-inu';
  FPArqResp := 'inu';
end;

procedure TNFSeInutilizacao.SetJustificativa(AValue: String);
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

function TNFSeInutilizacao.GerarPathPorCNPJ(): String;
var
  CNPJ: String;
begin
  if FPConfiguracoesNFSe.Arquivos.SepararPorCNPJ then
    CNPJ := FCNPJ
  else
    CNPJ := '';

  Result := FPConfiguracoesNFSe.Arquivos.GetPathInu(CNPJ);
end;

procedure TNFSeInutilizacao.DefinirServicoEAction;
begin
  if (FPConfiguracoesNFSe.Geral.ModeloDF = moNFSe) and
     (FPConfiguracoesNFSe.Geral.VersaoDF = ve310) and
     (FPConfiguracoesNFSe.WebServices.UFCodigo in [29]) then // 29 = BA
  begin
    FPServico := GetUrlWsd + 'NFSeInutilizacao';
    FPSoapAction := FPServico + '/NFSeInutilizacao';
  end
  else
  begin
    FPServico := GetUrlWsd + 'NFSeInutilizacao2';
    FPSoapAction := FPServico;
  end;
//  FPServico := GetUrlWsd + 'NFSeInutilizacao2';
//  FPSoapAction := FPServico;
end;

procedure TNFSeInutilizacao.DefinirDadosMsg;
var
  InutNFSe: TinutNFSe;
  OK: Boolean;
begin
  OK := False;
  InutNFSe := TinutNFSe.Create;
  try
    InutNFSe.tpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    InutNFSe.cUF := FPConfiguracoesNFSe.WebServices.UFCodigo;
    InutNFSe.ano := FAno;
    InutNFSe.CNPJ := FCNPJ;
    InutNFSe.modelo := FModelo;
    InutNFSe.serie := FSerie;
    InutNFSe.nNFIni := FNumeroInicial;
    InutNFSe.nNFFin := FNumeroFinal;
    InutNFSe.xJust := FJustificativa;

    FPConfiguracoesNFSe.Geral.ModeloDF := StrToModeloDF(OK, IntToStr(InutNFSe.modelo));

    InutNFSe.Versao := FPVersaoServico;
    InutNFSe.GerarXML;

    AssinarXML(InutNFSe.Gerador.ArquivoFormatoXML, 'inutNFSe', 'infInut',
      'Falha ao assinar Inutilização Nota Fiscal Eletrônica ');

    FID := InutNFSe.ID;
  finally
    InutNFSe.Free;
  end;
end;

procedure TNFSeInutilizacao.SalvarEnvio;
var
  aPath: String;
begin
  inherited SalvarEnvio;

  if FPConfiguracoesNFSe.Arquivos.Salvar then
  begin
    aPath := GerarPathPorCNPJ;
    FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqEnv + '.xml', FPDadosMsg, aPath);
  end;
end;

procedure TNFSeInutilizacao.SalvarResposta;
var
  aPath: String;
begin
  inherited SalvarResposta;

  if FPConfiguracoesNFSe.Arquivos.Salvar then
  begin
    aPath := GerarPathPorCNPJ;
    FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqResp + '.xml', FPRetWS, aPath);
  end;
end;

function TNFSeInutilizacao.TratarResposta: Boolean;
var
  NFSeRetorno: TRetInutNFSe;
  wProc: TStringList;
begin
  NFSeRetorno := TRetInutNFSe.Create;
  try
    FPRetWS := SeparaDados(FPRetornoWS, 'NFSeInutilizacaoNF2Result');
    if FPRetWS = '' then
      FPRetWS := SeparaDados(FPRetornoWS, 'NFSeInutilizacaoNFResult');

    NFSeRetorno.Leitor.Arquivo := FPRetWS;
    NFSeRetorno.LerXml;

    Fversao := NFSeRetorno.versao;
    FTpAmb := NFSeRetorno.TpAmb;
    FverAplic := NFSeRetorno.verAplic;
    FcStat := NFSeRetorno.cStat;
    FxMotivo := NFSeRetorno.xMotivo;
    FcUF := NFSeRetorno.cUF;
    FdhRecbto := NFSeRetorno.dhRecbto;
    Fprotocolo := NFSeRetorno.nProt;
    FPMsg := NFSeRetorno.XMotivo;

    Result := (NFSeRetorno.cStat = 102);

    //gerar arquivo proc de inutilizacao
    if ((NFSeRetorno.cStat = 102) or (NFSeRetorno.cStat = 563)) then
    begin
      wProc := TStringList.Create;
      try
        wProc.Add('<' + ENCODING_UTF8 + '>');
        wProc.Add('<ProcInutNFSe versao="' + FPVersaoServico +
          '" xmlns="http://www.portalfiscal.inf.br/NFSe">');

        wProc.Add(FPDadosMsg);
        wProc.Add(FPRetWS);
        wProc.Add('</ProcInutNFSe>');
        FXML_ProcInutNFSe := wProc.Text;
      finally
        wProc.Free;
      end;

      if FPConfiguracoesNFSe.Geral.Salvar then
        FPDFeOwner.Gravar(GerarPrefixoArquivo + '-procInutNFSe.xml',
          FXML_ProcInutNFSe);

      if FPConfiguracoesNFSe.Arquivos.Salvar then
        FPDFeOwner.Gravar(GerarPrefixoArquivo + '-procInutNFSe.xml',
          FXML_ProcInutNFSe, GerarPathPorCNPJ);
    end;
  finally
    NFSeRetorno.Free;
  end;
end;

function TNFSeInutilizacao.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'Recebimento: %s ' + LineBreak),
                   [Fversao, TpAmbToStr(FTpAmb), FverAplic, IntToStr(FcStat),
                    FxMotivo, CodigoParaUF(FcUF),
                    IfThen(FdhRecbto = 0, '', FormatDateTimeBr(FdhRecbto))]);
end;

function TNFSeInutilizacao.GerarPrefixoArquivo: String;
begin
  Result := Trim(OnlyNumber(FID));
end;

{ TNFSeConsultaCadastro }

constructor TNFSeConsultaCadastro.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FRetConsCad := TRetConsCad.Create;

  FPStatus := stNFSeCadastro;
  FPLayout := LayNFSeCadastro;
  FPArqEnv := 'ped-cad';
  FPArqResp := 'cad';
end;

destructor TNFSeConsultaCadastro.Destroy;
begin
  FRetConsCad.Free;

  inherited Destroy;
end;

procedure TNFSeConsultaCadastro.SetCNPJ(const Value: String);
begin
  if NaoEstaVazio(Value) then
  begin
    FIE := '';
    FCPF := '';
  end;

  FCNPJ := Value;
end;

procedure TNFSeConsultaCadastro.SetCPF(const Value: String);
begin
  if NaoEstaVazio(Value) then
  begin
    FIE := '';
    FCNPJ := '';
  end;

  FCPF := Value;
end;

procedure TNFSeConsultaCadastro.SetIE(const Value: String);
begin
  if NaoEstaVazio(Value) then
  begin
    FCNPJ := '';
    FCPF := '';
  end;

  FIE := Value;
end;

procedure TNFSeConsultaCadastro.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'CadConsultaCadastro2';
  FPSoapAction := FPServico;
end;

procedure TNFSeConsultaCadastro.DefinirDadosMsg;
var
  ConCadNFSe: TConsCad;
begin
  ConCadNFSe := TConsCad.Create;
  try
    ConCadNFSe.UF := FUF;
    ConCadNFSe.IE := FIE;
    ConCadNFSe.CNPJ := FCNPJ;
    ConCadNFSe.CPF := FCPF;
    ConCadNFSe.Versao := FPVersaoServico;
    ConCadNFSe.GerarXML;

    FPDadosMsg := ConCadNFSe.Gerador.ArquivoFormatoXML;
  finally
    ConCadNFSe.Free;
  end;
end;

function TNFSeConsultaCadastro.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'consultaCadastro2Result');

  // Limpando variaveis internas
  FRetConsCad.Free;
  FRetConsCad := TRetConsCad.Create;

  FRetConsCad.Leitor.Arquivo := FPRetWS;
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

function TNFSeConsultaCadastro.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'Consulta: %s ' + sLineBreak),
                   [FRetConsCad.versao, FRetConsCad.verAplic,
                   IntToStr(FRetConsCad.cStat), FRetConsCad.xMotivo,
                   CodigoParaUF(FRetConsCad.cUF),
                   FormatDateTimeBr(FRetConsCad.dhCons)]);
end;

function TNFSeConsultaCadastro.GerarUFSoap: String;
begin
  Result := '<cUF>' + IntToStr(UFparaCodigo(FUF)) + '</cUF>';
end;

{ TNFSeEnvEvento }

constructor TNFSeEnvEvento.Create(AOwner: TACBrDFe; AEvento: TEventoNFSe);
begin
  inherited Create(AOwner);

  FEventoRetorno := TRetEventoNFSe.Create;
  FEvento := AEvento;

  FPStatus := stNFSeEvento;
  FPLayout := LayNFSeEvento;
  FPArqEnv := 'ped-eve';
  FPArqResp := 'eve';
end;

destructor TNFSeEnvEvento.Destroy;
begin
  FEventoRetorno.Free;

  inherited;
end;

function TNFSeEnvEvento.GerarPathEvento: String;
begin
  with FEvento.Evento.Items[0].INFSevento do
  begin
    if (tpEvento = teCCe) and
      (not FPConfiguracoesNFSe.Arquivos.SalvarCCeCanEvento) then
      Result := FPConfiguracoesNFSe.Arquivos.GetPathCCe
    else if (tpEvento = teCancelamento) and
      (not FPConfiguracoesNFSe.Arquivos.SalvarCCeCanEvento) then
      Result := FPConfiguracoesNFSe.Arquivos.GetPathCan
    else
      Result := FPConfiguracoesNFSe.Arquivos.GetPathEvento(tpEvento);
  end;
end;

procedure TNFSeEnvEvento.DefinirURL;
var
  UF : String;
  Versao: Double;
begin
  { Verificação necessária pois somente os eventos de Cancelamento e CCe serão tratados pela SEFAZ do estado
    os outros eventos como manifestacao de destinatários serão tratados diretamente pela RFB }

  if not (FEvento.Evento.Items[0].INFSevento.tpEvento in [teCCe, teCancelamento]) then
  begin
    FPLayout := LayNFSeEventoAN;
    UF := 'AN';
  end
  else
   begin
    FPLayout := LayNFSeEvento;
    UF := FPConfiguracoesNFSe.WebServices.UF;
   end;

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';
  Versao := VersaoDFToDbl(FPConfiguracoesNFSe.Geral.VersaoDF);

  TACBrNFSe(FPDFeOwner).LerServicoDeParams(TACBrNFSe(FPDFeOwner).GetNomeModeloDFe, UF ,
    FPConfiguracoesNFSe.WebServices.Ambiente, LayOutToServico(FPLayout),
    Versao, FPURL);

  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

procedure TNFSeEnvEvento.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'RecepcaoEvento';
  FPSoapAction := FPServico;
end;

procedure TNFSeEnvEvento.DefinirDadosMsg;
var
  EventoNFSe: TEventoNFSe;
  I, J, F: integer;
  Lote, Evento, Eventos, EventosAssinados: String;
begin
  EventoNFSe := TEventoNFSe.Create;
  try
    EventoNFSe.idLote := FidLote;

    for I := 0 to TNFSeEnvEvento(Self).FEvento.Evento.Count - 1 do
    begin
      with EventoNFSe.Evento.Add do
      begin
        iNFSevento.tpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
        iNFSevento.CNPJ := FEvento.Evento[I].INFSevento.CNPJ;
        iNFSevento.cOrgao := FEvento.Evento[I].INFSevento.cOrgao;
        iNFSevento.chNFSe := FEvento.Evento[I].INFSevento.chNFSe;
        iNFSevento.dhEvento := FEvento.Evento[I].INFSevento.dhEvento;
        iNFSevento.tpEvento := FEvento.Evento[I].INFSevento.tpEvento;
        iNFSevento.nSeqEvento := FEvento.Evento[I].INFSevento.nSeqEvento;

        case INFSevento.tpEvento of
          teCCe:
          begin
            iNFSevento.detEvento.xCorrecao := FEvento.Evento[I].INFSevento.detEvento.xCorrecao;
            iNFSevento.detEvento.xCondUso := FEvento.Evento[I].INFSevento.detEvento.xCondUso;
          end;

          teCancelamento:
          begin
            iNFSevento.detEvento.nProt := FEvento.Evento[I].INFSevento.detEvento.nProt;
            iNFSevento.detEvento.xJust := FEvento.Evento[I].INFSevento.detEvento.xJust;
          end;

          teManifDestOperNaoRealizada:
            iNFSevento.detEvento.xJust := FEvento.Evento[I].INFSevento.detEvento.xJust;

          teEPECNFSe:
          begin
            iNFSevento.detEvento.cOrgaoAutor := FEvento.Evento[I].INFSevento.detEvento.cOrgaoAutor;
            iNFSevento.detEvento.tpAutor := FEvento.Evento[I].INFSevento.detEvento.tpAutor;
            iNFSevento.detEvento.verAplic := FEvento.Evento[I].INFSevento.detEvento.verAplic;
            iNFSevento.detEvento.dhEmi := FEvento.Evento[I].INFSevento.detEvento.dhEmi;
            iNFSevento.detEvento.tpNF := FEvento.Evento[I].INFSevento.detEvento.tpNF;
            iNFSevento.detEvento.IE := FEvento.Evento[I].INFSevento.detEvento.IE;

            iNFSevento.detEvento.dest.UF := FEvento.Evento[I].INFSevento.detEvento.dest.UF;
            iNFSevento.detEvento.dest.CNPJCPF := FEvento.Evento[I].INFSevento.detEvento.dest.CNPJCPF;
            iNFSevento.detEvento.dest.idEstrangeiro := FEvento.Evento[I].INFSevento.detEvento.dest.idEstrangeiro;
            iNFSevento.detEvento.dest.IE := FEvento.Evento[I].INFSevento.detEvento.dest.IE;

            iNFSevento.detEvento.vNF := FEvento.Evento[I].INFSevento.detEvento.vNF;
            iNFSevento.detEvento.vICMS := FEvento.Evento[I].INFSevento.detEvento.vICMS;
            iNFSevento.detEvento.vST := FEvento.Evento[I].INFSevento.detEvento.vST;
          end;

          tePedProrrog1,
          tePedProrrog2:
          begin
            iNFSevento.detEvento.nProt := FEvento.Evento[I].INFSevento.detEvento.nProt;

            for j := 0 to FEvento.Evento.Items[I].INFSevento.detEvento.itemPedido.count - 1 do
            begin
              with iNFSevento.detEvento.itemPedido.Add do
              begin
                numItem := FEvento.Evento[I].INFSevento.detEvento.itemPedido.Items[J].numItem;
                qtdeItem := FEvento.Evento[I].INFSevento.detEvento.itemPedido.Items[J].qtdeItem;
              end;
            end;

          end;

          teCanPedProrrog1,
          teCanPedProrrog2:
          begin
            iNFSevento.detEvento.idPedidoCancelado := FEvento.Evento[I].INFSevento.detEvento.idPedidoCancelado;
            iNFSevento.detEvento.nProt := FEvento.Evento[I].INFSevento.detEvento.nProt;
          end;

        end;
      end;
    end;

    EventoNFSe.Versao := FPVersaoServico;
    EventoNFSe.GerarXML;

    // Separa os grupos <evento> e coloca na variável Eventos
    I := Pos('<evento ', EventoNFSe.Gerador.ArquivoFormatoXML);
    Lote := Copy(EventoNFSe.Gerador.ArquivoFormatoXML, 1, I - 1);
    Eventos := SeparaDados(EventoNFSe.Gerador.ArquivoFormatoXML, 'envEvento');
    I := Pos('<evento ', Eventos);
    Eventos := Copy(Eventos, I, length(Eventos));

    EventosAssinados := '';

    // Realiza a assinatura para cada evento
    while Eventos <> '' do
    begin
      F := Pos('</evento>', Eventos);

      if F > 0 then
      begin
        Evento := Copy(Eventos, 1, F + 8);
        Eventos := Copy(Eventos, F + 9, length(Eventos));

        AssinarXML(Evento, 'evento', 'iNFSevento', 'Falha ao assinar o Envio de Evento ');

        EventosAssinados := EventosAssinados + StringReplace(
          FPDadosMsg, '<?xml version="1.0"?>', '', []);
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

    with TACBrNFSe(FPDFeOwner) do
    begin
      SSL.Validar(FPDadosMsg, GerarNomeArqSchema(FPLayout, FPVersaoServico), FPMsg);
    end;

    for I := 0 to FEvento.Evento.Count - 1 do
      FEvento.Evento[I].INFSevento.id := EventoNFSe.Evento[I].INFSevento.id;
  finally
    EventoNFSe.Free;
  end;
end;

function TNFSeEnvEvento.TratarResposta: Boolean;
var
  Leitor: TLeitor;
  I, J: integer;
  wProc: TStringList;
  NomeArq, VersaoEvento: String;
begin
  FEvento.idLote := idLote;

  FPRetWS := SeparaDados(FPRetornoWS, 'NFSeRecepcaoEventoResult');

  // Limpando variaveis internas
  FEventoRetorno.Free;
  FEventoRetorno := TRetEventoNFSe.Create;

  EventoRetorno.Leitor.Arquivo := FPRetWS;
  EventoRetorno.LerXml;

  FcStat := EventoRetorno.cStat;
  FxMotivo := EventoRetorno.xMotivo;
  FPMsg := EventoRetorno.xMotivo;
  FTpAmb := EventoRetorno.tpAmb;

  Result := (EventoRetorno.cStat = 128) or (EventoRetorno.cStat = 135) or
    (EventoRetorno.cStat = 136) or (EventoRetorno.cStat = 155);

  //gerar arquivo proc de evento
  if Result then
  begin
    Leitor := TLeitor.Create;
    try
      for I := 0 to FEvento.Evento.Count - 1 do
      begin
        for J := 0 to EventoRetorno.retEvento.Count - 1 do
        begin
          if FEvento.Evento.Items[I].INFSevento.chNFSe =
            EventoRetorno.retEvento.Items[J].RetINFSevento.chNFSe then
          begin
            FEvento.Evento.Items[I].RetINFSevento.nProt :=
              EventoRetorno.retEvento.Items[J].RetINFSevento.nProt;
            FEvento.Evento.Items[I].RetINFSevento.dhRegEvento :=
              EventoRetorno.retEvento.Items[J].RetINFSevento.dhRegEvento;
            FEvento.Evento.Items[I].RetINFSevento.cStat :=
              EventoRetorno.retEvento.Items[J].RetINFSevento.cStat;
            FEvento.Evento.Items[I].RetINFSevento.xMotivo :=
              EventoRetorno.retEvento.Items[J].RetINFSevento.xMotivo;

            wProc := TStringList.Create;
            try
              VersaoEvento := TACBrNFSe(FPDFeOwner).LerVersaoDeParams(LayNFSeEvento);

              wProc.Add('<' + ENCODING_UTF8 + '>');
              wProc.Add('<procEventoNFSe versao="' + VersaoEvento +
                '" xmlns="http://www.portalfiscal.inf.br/NFSe">');
              wProc.Add('<evento versao="' + VersaoEvento + '">');
              Leitor.Arquivo := FPDadosMsg;
              wProc.Add(Leitor.rExtrai(1, 'iNFSevento', '', I + 1));
              wProc.Add('<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">');

              Leitor.Arquivo := FPDadosMsg;
              wProc.Add(Leitor.rExtrai(1, 'SignedInfo', '', I + 1));

              Leitor.Arquivo := FPDadosMsg;
              wProc.Add(Leitor.rExtrai(1, 'SignatureValue', '', I + 1));

              Leitor.Arquivo := FPDadosMsg;
              wProc.Add(Leitor.rExtrai(1, 'KeyInfo', '', I + 1));
              wProc.Add('</Signature>');
              wProc.Add('</evento>');
              wProc.Add('<retEvento versao="' + VersaoEvento + '">');

              Leitor.Arquivo := FPRetWS;
              wProc.Add(Leitor.rExtrai(1, 'iNFSevento', '', J + 1));
              wProc.Add('</retEvento>');
              wProc.Add('</procEventoNFSe>');

              EventoRetorno.retEvento.Items[J].RetINFSevento.XML := wProc.Text;

              FEvento.Evento.Items[I].RetINFSevento.XML := wProc.Text;

              NomeArq := OnlyNumber(FEvento.Evento.Items[i].INFSevento.Id) +
                '-procEventoNFSe.xml';

              if FPConfiguracoesNFSe.Geral.Salvar then
                FPDFeOwner.Gravar(NomeArq, wProc.Text);

              if FPConfiguracoesNFSe.Arquivos.Salvar then
                FPDFeOwner.Gravar(NomeArq, wProc.Text, GerarPathEvento);
            finally
              wProc.Free;
            end;

            break;
          end;
        end;
      end;
    finally
      Leitor.Free;
    end;
  end;
end;

procedure TNFSeEnvEvento.SalvarEnvio;
begin
  inherited SalvarEnvio;

  if FPConfiguracoesNFSe.Arquivos.Salvar then
    FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqEnv + '.xml',
      FPDadosMsg, GerarPathEvento);
end;

procedure TNFSeEnvEvento.SalvarResposta;
begin
  inherited SalvarResposta;

  if FPConfiguracoesNFSe.Arquivos.Salvar then;
  FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqEnv + '.xml',
    FPDadosMsg, GerarPathEvento);
end;

function TNFSeEnvEvento.GerarMsgLog: String;
var
  aMsg: String;
begin
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
       [IfThen(FEventoRetorno.retEvento.Items[0].RetINFSevento.dhRegEvento = 0, '',
               FormatDateTimeBr(FEventoRetorno.retEvento.Items[0].RetINFSevento.dhRegEvento))]);

  Result := aMsg;
end;

function TNFSeEnvEvento.GerarPrefixoArquivo: String;
begin
  Result := IntToStr(FEvento.idLote);
end;

{ TNFSeConsNFSeDest }

constructor TNFSeConsNFSeDest.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FretConsNFSeDest := TretConsNFSeDest.Create;

  FPStatus := stConsNFSeDest;
  FPLayout := LayNFSeConsNFSeDest;
  FPArqEnv := 'con-NFSe-dest';
  FPArqResp := 'NFSe-dest';
end;

destructor TNFSeConsNFSeDest.Destroy;
begin
  FretConsNFSeDest.Free;

  inherited Destroy;
end;

procedure TNFSeConsNFSeDest.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeConsultaDest';
  FPSoapAction := FPServico + '/NFSeConsultaNFDest';
end;

procedure TNFSeConsNFSeDest.DefinirDadosMsg;
var
  ConsNFSeDest: TConsNFSeDest;
begin
  ConsNFSeDest := TConsNFSeDest.Create;
  try
    ConsNFSeDest.TpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    ConsNFSeDest.CNPJ := FCNPJ;
    ConsNFSeDest.indNFSe := FindNFSe;
    ConsNFSeDest.indEmi := FindEmi;
    ConsNFSeDest.ultNSU := FultNSU;
    ConsNFSeDest.Versao := FPVersaoServico;
    ConsNFSeDest.GerarXML;

    FPDadosMsg := ConsNFSeDest.Gerador.ArquivoFormatoXML;
  finally
    ConsNFSeDest.Free;
  end;
end;

function TNFSeConsNFSeDest.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'NFSeConsultaNFDestResult');

  // Limpando variaveis internas
  FretConsNFSeDest.Free;
  FretConsNFSeDest := TRetConsNFSeDest.Create;

  FretConsNFSeDest.Leitor.Arquivo := FPRetWS;
  FretConsNFSeDest.LerXml;

  FPMsg := FretConsNFSeDest.xMotivo;

  Result := (FretConsNFSeDest.CStat = 137) or (FretConsNFSeDest.CStat = 138);
end;

function TNFSeConsNFSeDest.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'Recebimento: %s ' + LineBreak +
                           'Ind. Continuação: %s ' + LineBreak +
                           'Último NSU: %s ' + LineBreak),
                   [FretConsNFSeDest.versao, TpAmbToStr(FretConsNFSeDest.tpAmb),
                    FretConsNFSeDest.verAplic, IntToStr(FretConsNFSeDest.cStat),
                    FretConsNFSeDest.xMotivo,
                    IfThen(FretConsNFSeDest.dhResp = 0, '', FormatDateTimeBr(RetConsNFSeDest.dhResp)),
                    IndicadorContinuacaoToStr(FretConsNFSeDest.indCont),
                    FretConsNFSeDest.ultNSU]);
end;

function TNFSeConsNFSeDest.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Consulta NF-e Destinadas:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TNFSeDownloadNFSe }

constructor TNFSeDownloadNFSe.Create(AOwner: TACBrDFe; ADownload: TDownloadNFSe);
begin
  inherited Create(AOwner);

  FRetDownloadNFSe := TretDownloadNFSe.Create;
  FDownload := ADownload;

  FPStatus := stDownloadNFSe;
  FPLayout := LayNFSeDownloadNFSe;
  FPArqEnv := 'ped-down-NFSe';
  FPArqResp := 'down-NFSe';
end;

destructor TNFSeDownloadNFSe.Destroy;
begin
  FRetDownloadNFSe.Free;

  inherited Destroy;
end;

procedure TNFSeDownloadNFSe.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeDownloadNF';
  FPSoapAction := FPServico + '/NFSeDownloadNF';
end;

procedure TNFSeDownloadNFSe.DefinirDadosMsg;
var
  DownloadNFSe: TDownloadNFSe;
  I: integer;
begin
  DownloadNFSe := TDownloadNFSe.Create;
  try
    DownloadNFSe.TpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    DownloadNFSe.CNPJ := FDownload.CNPJ;

    for I := 0 to FDownload.Chaves.Count - 1 do
    begin
      with DownloadNFSe.Chaves.Add do
      begin
        chNFSe := FDownload.Chaves[I].chNFSe;
      end;
    end;

    DownloadNFSe.Versao := FPVersaoServico;
    DownloadNFSe.GerarXML;

    FPDadosMsg := DownloadNFSe.Gerador.ArquivoFormatoXML;
  finally
    DownloadNFSe.Free;
  end;
end;

function TNFSeDownloadNFSe.TratarResposta: Boolean;
var
  I: integer;
  NomeArq: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'NFSeDownloadNFResult');

  // Limpando variaveis internas
  FretDownloadNFSe.Free;
  FRetDownloadNFSe := TRetDownloadNFSe.Create;

  FRetDownloadNFSe.Leitor.Arquivo := FPRetWS;
  FRetDownloadNFSe.LerXml;

  FPMsg := FretDownloadNFSe.xMotivo;

  Result := (FRetDownloadNFSe.cStat = 139);

  for I := 0 to FRetDownloadNFSe.retNFSe.Count - 1 do
  begin
    if FRetDownloadNFSe.retNFSe.Items[I].cStat = 140 then
    begin
      NomeArq := FRetDownloadNFSe.retNFSe.Items[I].chNFSe + '-NFSe.xml';
      FPDFeOwner.Gravar(NomeArq, FRetDownloadNFSe.retNFSe.Items[I].procNFSe,
                        GerarPathDownload);
    end;
  end;
end;

function TNFSeDownloadNFSe.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'Recebimento: %s ' + LineBreak),
                   [FretDownloadNFSe.versao, TpAmbToStr(FretDownloadNFSe.tpAmb),
                    FretDownloadNFSe.verAplic, IntToStr(FretDownloadNFSe.cStat),
                    FretDownloadNFSe.xMotivo,
                    IfThen(FretDownloadNFSe.dhResp = 0, '',
                           FormatDateTimeBr(FRetDownloadNFSe.dhResp))]);
end;

function TNFSeDownloadNFSe.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Download de NF-e:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TNFSeDownloadNFSe.GerarPathDownload: String;
begin
  Result := FPConfiguracoesNFSe.Arquivos.GetPathDownload('');
end;

{ TAdministrarCSCNFCe }

constructor TAdministrarCSCNFCe.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FretAdmCSCNFCe := TretAdmCSCNFCe.Create;

  FPStatus := stAdmCSCNFCe;
  FPLayout := LayAdministrarCSCNFCe;
  FPArqEnv := 'ped-csc';
  FPArqResp := 'csc';
end;

destructor TAdministrarCSCNFCe.Destroy;
begin
  FretAdmCSCNFCe.Free;

  inherited Destroy;
end;

procedure TAdministrarCSCNFCe.DefinirServicoEAction;
begin
  // O Método ainda não esta definido.
  FPServico := GetUrlWsd + 'MetodoNaoDefinido';
  FPSoapAction := FPServico;
end;

procedure TAdministrarCSCNFCe.DefinirDadosMsg;
var
  AdmCSCNFCe: TAdmCSCNFCe;
begin
  AdmCSCNFCe := TAdmCSCNFCe.Create;
  try
    AdmCSCNFCe.TpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    AdmCSCNFCe.RaizCNPJ := FRaizCNPJ;
    AdmCSCNFCe.indOP := FindOp;
    AdmCSCNFCe.idCsc := FIdCSC;
    AdmCSCNFCe.codigoCsc := FCodigoCSC;

    AdmCSCNFCe.Versao := FPVersaoServico;
    AdmCSCNFCe.GerarXML;

    FPDadosMsg := AdmCSCNFCe.Gerador.ArquivoFormatoXML;
  finally
    AdmCSCNFCe.Free;
  end;
end;

function TAdministrarCSCNFCe.TratarResposta: Boolean;
begin
  // O Método ainda não esta definido.
  FPRetWS := SeparaDados(FPRetornoWS, 'MetodoNaoDefinidoResult');

  // Limpando variaveis internas
  FretAdmCSCNFCe.Free;
  FretAdmCSCNFCe := TRetAdmCSCNFCe.Create;

  FretAdmCSCNFCe.Leitor.Arquivo := FPRetWS;
  FretAdmCSCNFCe.LerXml;

  FPMsg := FretAdmCSCNFCe.xMotivo;

  Result := (FretAdmCSCNFCe.CStat in [150..153]);
end;

function TAdministrarCSCNFCe.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak),
                   [FretAdmCSCNFCe.versao, TpAmbToStr(FretAdmCSCNFCe.tpAmb),
                    IntToStr(FretAdmCSCNFCe.cStat), FretAdmCSCNFCe.xMotivo]);
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

  FretDistDFeInt := TretDistDFeInt.Create;

  FPStatus := stDistDFeInt;
  FPLayout := LayDistDFeInt;
  FPArqEnv := 'con-dist-dfe';
  FPArqResp := 'dist-dfe';
  FPBodyElement := 'NFSeDistDFeInteresse';
  FPHeaderElement := '';
end;

destructor TDistribuicaoDFe.Destroy;
begin
  FretDistDFeInt.Free;

  inherited;
end;

procedure TDistribuicaoDFe.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFSeDistribuicaoDFe';
  FPSoapAction := FPServico + '/NFSeDistDFeInteresse';
end;

procedure TDistribuicaoDFe.DefinirDadosMsg;
var
  DistDFeInt: TDistDFeInt;
begin
  DistDFeInt := TDistDFeInt.Create;
  try
    DistDFeInt.TpAmb := FPConfiguracoesNFSe.WebServices.Ambiente;
    DistDFeInt.cUFAutor := FcUFAutor;
    DistDFeInt.CNPJCPF := FCNPJCPF;
    DistDFeInt.ultNSU := FultNSU;
    DistDFeInt.NSU := FNSU;
    DistDFeInt.Versao := FPVersaoServico;
    DistDFeInt.GerarXML;

    FPDadosMsg := DistDFeInt.Gerador.ArquivoFormatoXML;
  finally
    DistDFeInt.Free;
  end;
end;

function TDistribuicaoDFe.TratarResposta: Boolean;
var
  I: integer;
  AXML, NomeArq: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'NFSeDistDFeInteresseResult');

  // Limpando variaveis internas
  FretDistDFeInt.Free;
  FretDistDFeInt := TRetDistDFeInt.Create;

  FretDistDFeInt.Leitor.Arquivo := FPRetWS;
  FretDistDFeInt.LerXml;

  FPMsg := FretDistDFeInt.xMotivo;
  Result := (FretDistDFeInt.CStat = 137) or (FretDistDFeInt.CStat = 138);

  for I := 0 to FretDistDFeInt.docZip.Count - 1 do
  begin
    AXML := FretDistDFeInt.docZip.Items[I].XML;
    NomeArq := '';
    if (AXML <> '') then
    begin
      case FretDistDFeInt.docZip.Items[I].schema of
        schresNFSe:
          NomeArq := FretDistDFeInt.docZip.Items[I].resNFSe.chNFSe + '-resNFSe.xml';
        schresEvento:
          NomeArq := OnlyNumber(TpEventoToStr(FretDistDFeInt.docZip.Items[I].resEvento.tpEvento) +
             FretDistDFeInt.docZip.Items[I].resEvento.chNFSe +
             Format('%.2d', [FretDistDFeInt.docZip.Items[I].resEvento.nSeqEvento])) +
             '-resEventoNFSe.xml';
        schprocNFSe:
          NomeArq := FretDistDFeInt.docZip.Items[I].resNFSe.chNFSe + '-NFSe.xml';
        schprocEventoNFSe:
          NomeArq := OnlyNumber(FretDistDFeInt.docZip.Items[I].procEvento.Id) +
            '-procEventoNFSe.xml';
      end;

      if NaoEstaVazio(NomeArq) then
        FPDFeOwner.Gravar(NomeArq, AXML, GerarPathDistribuicao);
    end;
  end;
end;

function TDistribuicaoDFe.GerarMsgLog: String;
begin
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
end;

function TDistribuicaoDFe.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Distribuição de DFe:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TDistribuicaoDFe.GerarPathDistribuicao: String;
begin
  Result := FPConfiguracoesNFSe.Arquivos.GetPathDownload('');
end;
*)
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

  FEnviarLoteRPS := TNFSeEnviarLoteRPS.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
(*
  FRetorno := TNFSeRetRecepcao.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).NotasFiscais);
  FRecibo := TNFSeRecibo.Create(FACBrNFSe);
  FConsulta := TNFSeConsulta.Create(FACBrNFSe);
  FInutilizacao := TNFSeInutilizacao.Create(FACBrNFSe);
  FConsultaCadastro := TNFSeConsultaCadastro.Create(FACBrNFSe);
  FEnvEvento := TNFSeEnvEvento.Create(FACBrNFSe, TACBrNFSe(FACBrNFSe).EventoNFSe);
  FConsNFSeDest := TNFSeConsNFSeDest.Create(FACBrNFSe);
  FDownloadNFSe := TNFSeDownloadNFSe.Create(FACBrNFSe, TACBrNFSe(
    FACBrNFSe).DownloadNFSe.Download);
  FAdministrarCSCNFCe := TAdministrarCSCNFCe.Create(FACBrNFSe);
  FDistribuicaoDFe := TDistribuicaoDFe.Create(FACBrNFSe);
*)
  FEnvioWebService := TNFSeEnvioWebService.Create(FACBrNFSe);
end;

destructor TWebServices.Destroy;
begin
  FEnviarLoteRPS.Free;
(*
  FRetorno.Free;
  FRecibo.Free;
  FConsulta.Free;
  FInutilizacao.Free;
  FConsultaCadastro.Free;
  FEnvEvento.Free;
  FConsNFSeDest.Free;
  FDownloadNFSe.Free;
  FAdministrarCSCNFCe.Free;
  FDistribuicaoDFe.Free;
*)
  FEnvioWebService.Free;

  inherited Destroy;
end;

function TWebServices.Envia(ALote: integer; const ASincrono: Boolean): Boolean;
begin
  Result := Envia(IntToStr(ALote), ASincrono);
end;

function TWebServices.Envia(ALote: String; const ASincrono: Boolean): Boolean;
begin
(*
  FEnviarLoteRPS.FLote := ALote;
  FEnviarLoteRPS.FSincrono := ASincrono;

  if not EnviarLoteRPS.Executar then
    EnviarLoteRPS.GerarException( EnviarLoteRPS.Msg );

  if not ASincrono then
  begin
    FRetorno.Recibo := FEnviarLoteRPS.Recibo;
    if not FRetorno.Executar then
      FRetorno.GerarException( FRetorno.Msg );
  end;
*)
  Result := True;
end;

procedure TWebServices.Inutiliza(CNPJ, AJustificativa: String;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer);
begin
(*
  CNPJ := OnlyNumber(CNPJ);

  if not ValidarCNPJ(CNPJ) then
    raise EACBrNFSeException.Create('CNPJ: ' + CNPJ + ', inválido.');

  FInutilizacao.CNPJ := CNPJ;
  FInutilizacao.Modelo := Modelo;
  FInutilizacao.Serie := Serie;
  FInutilizacao.Ano := Ano;
  FInutilizacao.NumeroInicial := NumeroInicial;
  FInutilizacao.NumeroFinal := NumeroFinal;
  FInutilizacao.Justificativa := AJustificativa;

  if not FInutilizacao.Executar then
    FInutilizacao.GerarException( FInutilizacao.Msg );
*)    
end;

end.
