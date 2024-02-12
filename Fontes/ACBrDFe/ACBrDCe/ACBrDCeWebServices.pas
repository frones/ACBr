{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrDCeWebServices;

interface

uses
  Classes, SysUtils, synacode,
  pcnConversao,
  ACBrDFe, ACBrDFeWebService,
  ACBrDCeClass,
  ACBrXmlBase,
//  pcnRetConsReciDFe,
  ACBrDCeConversao, ACBrDCeProc,
//  pmdfeEnvEventoMDFe, pmdfeRetEnvEventoMDFe,
//  pmdfeRetConsSitMDFe, pmdfeRetConsMDFeNaoEnc, pmdfeRetEnvMDFe,
  pcnDistDFeInt, pcnRetDistDFeInt,
  ACBrDCeDeclaracoes, ACBrDCeConfiguracoes;

type

  { TDCeWebService }

  TDCeWebService = class(TDFeWebService)
  private
  protected
    FPStatus: TStatusDCe;
    FPLayout: TLayOutDCe;
    FPConfiguracoesDCe: TConfiguracoesDCe;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusDCe read FPStatus;
    property Layout: TLayOutDCe read FPLayout;
  end;

  { TDCeStatusServico }

  TDCeStatusServico = class(TDCeWebService)
  private
    Fversao: String;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FdhRecbto: TDateTime;
    FTMed: Integer;
    FdhRetorno: TDateTime;
    FxObs: String;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    procedure Clear; override;

    property versao: String read Fversao;
    property tpAmb: TACBrTipoAmbiente read FtpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property xMotivo: String read FxMotivo;
    property cUF: Integer read FcUF;
    property dhRecbto: TDateTime read FdhRecbto;
    property TMed: Integer read FTMed;
    property dhRetorno: TDateTime read FdhRetorno;
    property xObs: String read FxObs;
  end;

  { TDCeRecepcao }

  TDCeRecepcao = class(TDCeWebService)
  private
    FLote: String;
    FRecibo: String;
    FDeclaracoes: TDeclaracoes;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FcUF: Integer;
    FxMotivo: String;
    FdhRecbto: TDateTime;
    FTMed: Integer;
    FProtocolo: string;
    FVersaoDF: TVersaoDCe;
    FSincrono: Boolean;
    FMsgUnZip: String;

//    FDCeRetornoSincrono: TRetConsSitDCe;
//    FDCeRetorno: TretEnvDCe;

    function GetLote: String;
    function GetRecibo: String;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property Recibo: String read GetRecibo;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property cUF: Integer read FcUF;
    property xMotivo: String read FxMotivo;
    property dhRecbto: TDateTime read FdhRecbto;
    property TMed: Integer read FTMed;
    property Protocolo: string read FProtocolo;

    property Lote: String read GetLote write FLote;
    property Sincrono: Boolean read FSincrono write FSincrono;
    property MsgUnZip: String read FMsgUnZip write FMsgUnZip;
  end;

  { TDCeRetRecepcao }

  TDCeRetRecepcao = class(TDCeWebService)
  private
    FRecibo: String;
    FProtocolo: String;
    FChaveDCe: String;
    FDeclaracoes: TDeclaracoes;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FcUF: Integer;
    FxMotivo: String;
    FcMsg: Integer;
    FxMsg: String;
    FVersaoDF: TVersaoDCe;

//    FDCeRetorno: TRetConsReciDFe;

    function GetRecibo: String;
    function TratarRespostaFinal: Boolean;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    function Executar: Boolean; override;

    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property cUF: Integer read FcUF;
    property xMotivo: String read FxMotivo;
    property cMsg: Integer read FcMsg;
    property xMsg: String read FxMsg;
    property Recibo: String read GetRecibo write FRecibo;
    property Protocolo: String read FProtocolo write FProtocolo;
    property ChaveDCe: String read FChaveDCe write FChaveDCe;

//    property DCeRetorno: TRetConsReciDFe read FDCeRetorno;
  end;

  { TDCeRecibo }

  TDCeRecibo = class(TDCeWebService)
  private
    FDeclaracoes: TDeclaracoes;
    FRecibo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FxMsg: String;
    FcMsg: Integer;
    FVersaoDF: TVersaoDCe;

//    FDCeRetorno: TRetConsReciDFe;
  protected
    procedure InicializarServico; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirURL; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property xMotivo: String read FxMotivo;
    property cUF: Integer read FcUF;
    property xMsg: String read FxMsg;
    property cMsg: Integer read FcMsg;
    property Recibo: String read FRecibo write FRecibo;

//    property DCeRetorno: TRetConsReciDFe read FDCeRetorno;
  end;

  { TDCeConsulta }

  TDCeConsulta = class(TDCeWebService)
  private
    FOwner: TACBrDFe;
    FDeclaracoes: TDeclaracoes;
    FDCeChave: String;
    FExtrairEventos: Boolean;
    FProtocolo: String;
    FDhRecbto: TDateTime;
    FXMotivo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FcUF: Integer;
    FRetDCeDFe: String;

    FprotDCe: TProcDCe;
//    FprocEventoDCe: TRetEventoDCeCollection;

    procedure SetDCeChave(const AValue: String);
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function GerarUFSoap: String; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property DCeChave: String read FDCeChave write SetDCeChave;
    property ExtrairEventos: Boolean read FExtrairEventos write FExtrairEventos;
    property Protocolo: String read FProtocolo;
    property DhRecbto: TDateTime read FDhRecbto;
    property XMotivo: String read FXMotivo;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property cUF: Integer read FcUF;
    property RetDCeDFe: String read FRetDCeDFe;

    property protDCe: TProcDCe read FprotDCe;
//    property procEventoDCe: TRetEventoDCeCollection read FprocEventoDCe;
  end;

  { TDCeEnvEvento }

  TDCeEnvEvento = class(TDCeWebService)
  private
    FidLote: Integer;
//    FEvento: TEventoDCe;
    FcStat: Integer;
    FxMotivo: String;
    FTpAmb: TpcnTipoAmbiente;
    FCNPJ: String;

//    FEventoRetorno: TRetEventoDCe;

    function GerarPathEvento(const ACNPJ: String = ''; const AIE: String = ''): String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create; //(AOwner: TACBrDFe; AEvento: TEventoDCe);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property idLote: Integer read FidLote write FidLote;
    property cStat: Integer read FcStat;
    property xMotivo: String read FxMotivo;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;

//    property EventoRetorno: TRetEventoDCe read FEventoRetorno;
  end;

  { TDCeConsultaDCeNaoEnc }

  TDCeConsultaDCeNaoEnc = Class(TDCeWebService)
  private
    FOwner: TACBrDFe;
    FCNPJCPF: String;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
//    FInfDCe: TRetInfDCeCollection;
//    FRetConsDCeNaoEnc: TRetConsDCeNaoEnc;
    FMsg: String;
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

    property CNPJCPF: String                read FCNPJCPF write FCNPJCPF;
    property versao: String                 read Fversao;
    property tpAmb: TpcnTipoAmbiente        read FtpAmb;
    property verAplic: String               read FverAplic;
    property cStat: Integer                 read FcStat;
    property xMotivo: String                read FxMotivo;
    property cUF: Integer                   read FcUF;
//    property InfDCe: TRetInfDCeCollection read FInfDCe;
    property Msg: String                    read FMsg;
  end;

  { TDistribuicaoDFe }

  TDistribuicaoDFe = class(TDCeWebService)
  private
    FCNPJCPF: String;
    FultNSU: String;
    FNSU: String;
    FchDCe: String;
    FNomeArq: String;
    FlistaArqs: TStringList;

    FretDistDFeInt: TretDistDFeInt;

    function GerarPathDistribuicao(AItem :TdocZipCollectionItem): String;
  protected
//    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    property CNPJCPF: String read FCNPJCPF write FCNPJCPF;
    property ultNSU: String read FultNSU write FultNSU;
    property NSU: String read FNSU write FNSU;
    property chDCe: String read FchDCe write FchDCe;
    property NomeArq: String read FNomeArq;
    property ListaArqs: TStringList read FlistaArqs;

    property retDistDFeInt: TretDistDFeInt read FretDistDFeInt;
  end;

 { TDCeEnvioWebService }

  TDCeEnvioWebService = class(TDCeWebService)
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
    FACBrDCe: TACBrDFe;
    FStatusServico: TDCeStatusServico;
    FEnviar: TDCeRecepcao;
    FRetorno: TDCeRetRecepcao;
    FRecibo: TDCeRecibo;
    FConsulta: TDCeConsulta;
    FEnvEvento: TDCeEnvEvento;
    FConsDCeNaoEnc: TDCeConsultaDCeNaoEnc;
    FDistribuicaoDFe: TDistribuicaoDFe;
    FEnvioWebService: TDCeEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia(ALote: Integer; ASincrono:  Boolean = False): Boolean; overload;
    function Envia(const ALote: String; ASincrono:  Boolean = False): Boolean; overload;
    function ConsultaDCeNaoEnc(const ACNPJCPF: String): Boolean;

    property ACBrDCe: TACBrDFe read FACBrDCe write FACBrDCe;
    property StatusServico: TDCeStatusServico read FStatusServico write FStatusServico;
    property Enviar: TDCeRecepcao read FEnviar write FEnviar;
    property Retorno: TDCeRetRecepcao read FRetorno write FRetorno;
    property Recibo: TDCeRecibo read FRecibo write FRecibo;
    property Consulta: TDCeConsulta read FConsulta write FConsulta;
    property EnvEvento: TDCeEnvEvento read FEnvEvento write FEnvEvento;
    property ConsDCeNaoEnc: TDCeConsultaDCeNaoEnc read FConsDCeNaoEnc write FConsDCeNaoEnc;
    property DistribuicaoDFe: TDistribuicaoDFe read FDistribuicaoDFe write FDistribuicaoDFe;
    property EnvioWebService: TDCeEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil.Base,
  ACBrUtil.XMLHTML,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrCompress, ACBrDCe, ACBrDCeConsts,
  ACBrDFeUtil,
  pcnLeitor,
  ACBrDFeComum.ConsStatServ,
  ACBrDFeComum.RetConsStatServ,
//  pmdfeConsSitDCe,
  pcnConsReciDFe;

{ TDCeWebService }

constructor TDCeWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesDCe := TConfiguracoesDCe(FPConfiguracoes);
  FPLayout := LayDCeStatusServico;

  FPHeaderElement := 'DCeCabecMsg';
  FPBodyElement := 'DCeDadosMsg';
end;

procedure TDCeWebService.Clear;
begin
  inherited Clear;

  FPStatus := stDCeIdle;
  FPDFeOwner.SSL.UseCertificateHTTP := True;
end;

procedure TDCeWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrDCe(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TDCeWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrDCe(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TDCeWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrDCe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TDCeWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrDCe(FPDFeOwner).SetStatus(stDCeIdle);
end;

{ TDCeStatusServico }

procedure TDCeStatusServico.Clear;
begin
  inherited Clear;

  FPStatus := stDCeStatusServico;
  FPLayout := LayDCeStatusServico;
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

  if Assigned(FPConfiguracoesDCe) then
  begin
    FtpAmb := TACBrTipoAmbiente(FPConfiguracoesDCe.WebServices.Ambiente);
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end
end;

procedure TDCeStatusServico.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeStatusServico';
  FPSoapAction := FPServico + '/DCeStatusServicoMDF';
end;

procedure TDCeStatusServico.DefinirDadosMsg;
var
  ConsStatServ: TConsStatServ;
begin
  ConsStatServ := TConsStatServ.Create(FPVersaoServico, NAME_SPACE_DCE, 'DCe', False);
  try
    ConsStatServ.TpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    ConsStatServ.CUF := FPConfiguracoesDCe.WebServices.UFCodigo;

    FPDadosMsg := ConsStatServ.GerarXML;
  finally
    ConsStatServ.Free;
  end;
end;

function TDCeStatusServico.TratarResposta: Boolean;
var
  DCeRetorno: TRetConsStatServ;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'DCeStatusServicoMDFResult');

  DCeRetorno := TRetConsStatServ.Create('DCe');
  try
    DCeRetorno.XmlRetorno := ParseText(FPRetWS);
    DCeRetorno.LerXml;

    Fversao := DCeRetorno.versao;
    FtpAmb := DCeRetorno.tpAmb;
    FverAplic := DCeRetorno.verAplic;
    FcStat := DCeRetorno.cStat;
    FxMotivo := DCeRetorno.xMotivo;
    FcUF := DCeRetorno.cUF;
    FdhRecbto := DCeRetorno.dhRecbto;
    FTMed := DCeRetorno.TMed;
    FdhRetorno := DCeRetorno.dhRetorno;
    FxObs := DCeRetorno.xObs;
    FPMsg := FxMotivo + LineBreak + FxObs;

    if FPConfiguracoesDCe.WebServices.AjustaAguardaConsultaRet then
      FPConfiguracoesDCe.WebServices.AguardarConsultaRet := FTMed * 1000;

    Result := (FcStat = 107);

  finally
    DCeRetorno.Free;
  end;
end;

function TDCeStatusServico.GerarMsgLog: String;
begin
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
                   [Fversao, TipoAmbienteToStr(FtpAmb), FverAplic, IntToStr(FcStat),
                    FxMotivo, CodigoUFParaUF(FcUF),
                    IfThen(FdhRecbto = 0, '', FormatDateTimeBr(FdhRecbto)),
                    IntToStr(FTMed),
                    IfThen(FdhRetorno = 0, '', FormatDateTimeBr(FdhRetorno)),
                    FxObs]);
end;

function TDCeStatusServico.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Consulta Status serviço:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TDCeRecepcao }

constructor TDCeRecepcao.Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
begin
  inherited Create(AOwner);

  FDeclaracoes := ADeclaracoes;
end;

destructor TDCeRecepcao.Destroy;
begin
//  FDCeRetornoSincrono.Free;
//  FDCeRetorno.Free;

  inherited Destroy;
end;

procedure TDCeRecepcao.Clear;
begin
  inherited Clear;

  FPStatus := stDCeRecepcao;
  FPLayout := LayDCeRecepcao;
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

  if Assigned(FPConfiguracoesDCe) then
  begin
    FtpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;
  {
  if Assigned(FDCeRetornoSincrono) then
    FDCeRetornoSincrono.Free;

  if Assigned(FDCeRetorno) then
    FDCeRetorno.Free;

  FDCeRetornoSincrono := TRetConsSitDCe.Create;
  FDCeRetorno := TretEnvDCe.Create;
  }
end;

function TDCeRecepcao.GetLote: String;
begin
  Result := Trim(FLote);
end;

function TDCeRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

procedure TDCeRecepcao.InicializarServico;
var
  ok: Boolean;
begin
  if FDeclaracoes.Count > 0 then    // Tem DCe ? Se SIM, use as informações do XML
    FVersaoDF := DblToVersaoDCe(ok, FDeclaracoes.Items[0].DCe.infDCe.Versao)
  else
    FVersaoDF := FPConfiguracoesDCe.Geral.VersaoDF;

  inherited InicializarServico;
end;

procedure TDCeRecepcao.DefinirURL;
var
  Modelo: String;
  VerServ: Double;
begin
  if FDeclaracoes.Count > 0 then    // Tem DCe ? Se SIM, use as informações do XML
  begin
    FcUF := FDeclaracoes.Items[0].DCe.Ide.cUF;

    if Integer(FPConfiguracoesDCe.WebServices.Ambiente) <> Integer(FDeclaracoes.Items[0].DCe.Ide.tpAmb) then
      raise EACBrDCeException.Create( ACBRDCe_CErroAmbDiferente );
  end
  else
  begin                              // Se não tem DCe, use as configurações do componente
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;

  if Sincrono then
    FPLayout := LayDCeRecepcaoSinc
  else
    FPLayout := LayDCeRecepcao;

  VerServ := VersaoDCeToDbl(FVersaoDF);
  Modelo := 'DCe';
  FTpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  TACBrDCe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    CUFtoUF(FcUF),
    FTpAmb,
    LayOutDCeToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TDCeRecepcao.DefinirServicoEAction;
begin
  if Sincrono then
  begin
    FPServico := GetUrlWsd + 'DCeRecepcaoSinc';
    FPSoapAction := FPServico + '/DCeRecepcao';
  end
  else
  begin
    FPServico := GetUrlWsd + 'DCeRecepcao';
    FPSoapAction := FPServico + '/DCeRecepcaoLote';
  end;
end;

procedure TDCeRecepcao.DefinirDadosMsg;
var
  I: Integer;
  vDCe: String;
begin
  if Sincrono then
  begin
    // No envio só podemos ter apena UM MDF-e, pois o seu processamento é síncrono
    if FDeclaracoes.Count > 1 then
      GerarException(ACBrStr('ERRO: Conjunto de MDF-e transmitidos (máximo de 1 MDF-e)' +
             ' excedido. Quantidade atual: ' + IntToStr(FDeclaracoes.Count)));

    if FDeclaracoes.Count > 0 then
      FPDadosMsg := '<DCe' +
        RetornarConteudoEntre(FDeclaracoes.Items[0].XMLAssinado, '<DCe', '</DCe>') +
        '</DCe>';

    FMsgUnZip := FPDadosMsg;

    FPDadosMsg := EncodeBase64(GZipCompress(FPDadosMsg));
  end
  else
  begin
    vDCe := '';

    for I := 0 to FDeclaracoes.Count - 1 do
      vDCe := vDCe + '<DCe' + RetornarConteudoEntre(
        FDeclaracoes.Items[I].XMLAssinado, '<DCe', '</DCe>') + '</DCe>';

    FPDadosMsg := '<enviDCe xmlns="' + NAME_SPACE_DCE + '" versao="' +
      FPVersaoServico + '">' + '<idLote>' + FLote + '</idLote>' +
      vDCe + '</enviDCe>';
  end;

  // Lote tem mais de 1024kb ? //
  if Length(FPDadosMsg) > (1024 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 1024 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));

  FRecibo := '';
end;

function TDCeRecepcao.TratarResposta: Boolean;
var
  I: integer;
  chDCe, AXML, NomeXMLSalvo: String;
  AProcDCe: TProcDCe;
  SalvarXML: Boolean;
begin
  FPRetWS := SeparaDadosArray(['DCeRecepcaoLoteResult',
                               'DCeRecepcaoResult'],FPRetornoWS );

  if Sincrono then
  begin
    if pos('retDCe', FPRetWS) > 0 then
      AXML := StringReplace(FPRetWS, 'retDCe', 'retConsSitDCe',
                                     [rfReplaceAll, rfIgnoreCase])
    else
      AXML := FPRetWS;
    {
    FDCeRetornoSincrono.Leitor.Arquivo := ParseText(AXML);
    FDCeRetornoSincrono.LerXml;

    Fversao := FDCeRetornoSincrono.versao;
    FTpAmb := FDCeRetornoSincrono.TpAmb;
    FverAplic := FDCeRetornoSincrono.verAplic;

    FcUF := FDCeRetornoSincrono.cUF;
    chDCe := FDCeRetornoSincrono.ProtDCe.chDCe;

    if (FDCeRetornoSincrono.protDCe.cStat > 0) then
      FcStat := FDCeRetornoSincrono.protDCe.cStat
    else
      FcStat := FDCeRetornoSincrono.cStat;

    if (FDCeRetornoSincrono.protDCe.xMotivo <> '') then
    begin
      FPMsg := FDCeRetornoSincrono.protDCe.xMotivo;
      FxMotivo := FDCeRetornoSincrono.protDCe.xMotivo;
    end
    else
    begin
      FPMsg := FDCeRetornoSincrono.xMotivo;
      FxMotivo := FDCeRetornoSincrono.xMotivo;
    end;

    // Verificar se a MDF-e foi autorizado com sucesso
    Result := (FDCeRetornoSincrono.cStat = 100) and
      (TACBrDCe(FPDFeOwner).CstatProcessado(FDCeRetornoSincrono.protDCe.cStat));
    }
    if Result then
    begin
      // Pega o numero do protocolo
//      FProtocolo := FDCeRetornoSincrono.protDCe.nProt;

      for I := 0 to TACBrDCe(FPDFeOwner).Declaracoes.Count - 1 do
      begin
        with TACBrDCe(FPDFeOwner).Declaracoes.Items[I] do
        begin
          if OnlyNumber(chDCe) = NumID then
          begin
            {
            if (FPConfiguracoesDCe.Geral.ValidarDigest) and
               (FDCeRetornoSincrono.protDCe.digVal <> '') and
               (DCe.signature.DigestValue <> FDCeRetornoSincrono.protDCe.digVal) then
            begin
              raise EACBrDCeException.Create('DigestValue do documento ' + NumID + ' não confere.');
            end;

            DCe.procDCe.cStat := FDCeRetornoSincrono.protDCe.cStat;
            DCe.procDCe.tpAmb := FDCeRetornoSincrono.tpAmb;
            DCe.procDCe.verAplic := FDCeRetornoSincrono.verAplic;
            DCe.procDCe.chDCe := FDCeRetornoSincrono.protDCe.chDCe;
            DCe.procDCe.dhRecbto := FDCeRetornoSincrono.protDCe.dhRecbto;
            DCe.procDCe.nProt := FDCeRetornoSincrono.protDCe.nProt;
            DCe.procDCe.digVal := FDCeRetornoSincrono.protDCe.digVal;
            DCe.procDCe.xMotivo := FDCeRetornoSincrono.protDCe.xMotivo;
            }
            AProcDCe := TProcDCe.Create;
            try
              // Processando em UTF8, para poder gravar arquivo corretamente //
              AProcDCe.XML_DCe := RemoverDeclaracaoXML(XMLAssinado);
//              AProcDCe.XML_Prot := FDCeRetornoSincrono.XMLprotDCe;
              AProcDCe.Versao := FPVersaoServico;
              AjustarOpcoes( AProcDCe.Gerador.Opcoes );
              AProcDCe.GerarXML;

              XMLOriginal := AProcDCe.Gerador.ArquivoFormatoXML;

              if FPConfiguracoesDCe.Arquivos.Salvar then
              begin
                SalvarXML := (not FPConfiguracoesDCe.Arquivos.SalvarApenasDCeProcessados) or
                             Processado;

                // Salva o XML do MDF-e assinado e protocolado
                if SalvarXML then
                begin
                  NomeXMLSalvo := '';
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                  begin
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal ); // Atualiza o XML carregado
                    NomeXMLSalvo := NomeArq;
                  end;

                  if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                    GravarXML; // Salva na pasta baseado nas configurações do PathDCe
                end;
              end ;
            finally
              AProcDCe.Free;
            end;

            Break;
          end;
        end;
      end;
    end;
  end
  else
  begin
  {
    FDCeRetorno.Leitor.Arquivo := ParseText(FPRetWS);
    FDCeRetorno.LerXml;

    Fversao := FDCeRetorno.versao;
    FTpAmb := FDCeRetorno.TpAmb;
    FverAplic := FDCeRetorno.verAplic;
    FcStat := FDCeRetorno.cStat;
    FxMotivo := FDCeRetorno.xMotivo;
    FdhRecbto := FDCeRetorno.infRec.dhRecbto;
    FTMed := FDCeRetorno.infRec.tMed;
    FcUF := FDCeRetorno.cUF;
    FPMsg := FDCeRetorno.xMotivo;
    FRecibo := FDCeRetorno.infRec.nRec;

    Result := (FDCeRetorno.CStat = 103);
    }
  end;
end;

function TDCeRecepcao.GerarMsgLog: String;
begin
  (*
  if Sincrono then
    Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + sLineBreak +
                           'dhRecbto: %s ' + sLineBreak +
                           'chDCe: %s ' + LineBreak),
                     [FDCeRetornoSincrono.versao,
                      TpAmbToStr(FDCeRetornoSincrono.TpAmb),
                      FDCeRetornoSincrono.verAplic,
                      IntToStr(FDCeRetornoSincrono.cStat),
                      FDCeRetornoSincrono.xMotivo,
                      CodigoParaUF(FDCeRetornoSincrono.cUF),
                      FormatDateTimeBr(FDCeRetornoSincrono.protDCe.dhRecbto),
                      FDCeRetornoSincrono.chDCe])
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
                       [FDCeRetorno.versao,
                        TpAmbToStr(FDCeRetorno.TpAmb),
                        FDCeRetorno.verAplic,
                        IntToStr(FDCeRetorno.cStat),
                        FDCeRetorno.xMotivo,
                        CodigoParaUF(FDCeRetorno.cUF),
                        FDCeRetorno.infRec.nRec,
                        IfThen(FDCeRetorno.InfRec.dhRecbto = 0, '',
                               FormatDateTimeBr(FDCeRetorno.InfRec.dhRecbto)),
                        IntToStr(FDCeRetorno.InfRec.TMed)]);
  *)
end;

function TDCeRecepcao.GerarPrefixoArquivo: String;
begin
  if Sincrono then  // Esta procesando nome do Retorno Sincrono ?
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

{ TDCeRetRecepcao }

constructor TDCeRetRecepcao.Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
begin
  inherited Create(AOwner);

  FDeclaracoes := ADeclaracoes;
end;

destructor TDCeRetRecepcao.Destroy;
begin
//  FDCeRetorno.Free;

  inherited Destroy;
end;

procedure TDCeRetRecepcao.InicializarServico;
var
  ok: Boolean;
begin
  if FDeclaracoes.Count > 0 then    // Tem DCe ? Se SIM, use as informações do XML
    FVersaoDF := DblToVersaoDCe(ok, FDeclaracoes.Items[0].DCe.infDCe.Versao)
  else
    FVersaoDF := FPConfiguracoesDCe.Geral.VersaoDF;

  inherited InicializarServico;
end;

procedure TDCeRetRecepcao.Clear;
var
  i, j: Integer;
begin
  inherited Clear;

  FPStatus := stDCeRetRecepcao;
  FPLayout := LayDCeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  Fversao := '';
  FxMsg := '';
  FcMsg := 0;

  if Assigned(FPConfiguracoesDCe) then
  begin
    FtpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;
  {
  if Assigned(FDCeRetorno) and Assigned(FDeclaracoes) then
  begin
    // Limpa Dados dos retornos dos Declaracoes
    for i := 0 to FDCeRetorno.ProtDFe.Count - 1 do
    begin
      for j := 0 to FDeclaracoes.Count - 1 do
      begin
        if OnlyNumber(FDCeRetorno.ProtDFe.Items[i].chDFe) = FDeclaracoes.Items[J].NumID then
        begin
          FDeclaracoes.Items[j].DCe.procDCe.verAplic := '';
          FDeclaracoes.Items[j].DCe.procDCe.chDCe   := '';
          FDeclaracoes.Items[j].DCe.procDCe.dhRecbto := 0;
          FDeclaracoes.Items[j].DCe.procDCe.nProt    := '';
          FDeclaracoes.Items[j].DCe.procDCe.digVal   := '';
          FDeclaracoes.Items[j].DCe.procDCe.cStat    := 0;
          FDeclaracoes.Items[j].DCe.procDCe.xMotivo  := '';
        end;
      end;
    end;

    FreeAndNil(FDCeRetorno);
  end;

  FDCeRetorno := TRetConsReciDFe.Create('DCe');
  }
end;

function TDCeRetRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

function TDCeRetRecepcao.Executar: Boolean;
var
  IntervaloTentativas, Tentativas: Integer;
begin
  Result := False;

  TACBrDCe(FPDFeOwner).SetStatus(stDCeRetRecepcao);
  try
    Sleep(FPConfiguracoesDCe.WebServices.AguardarConsultaRet);

    Tentativas := 0; // Inicializa o contador de tentativas
    IntervaloTentativas := max(FPConfiguracoesDCe.WebServices.IntervaloTentativas, 1000);

    while (inherited Executar) and
      (Tentativas < FPConfiguracoesDCe.WebServices.Tentativas) do
    begin
      Inc(Tentativas);
      sleep(IntervaloTentativas);
    end;
  finally
    TACBrDCe(FPDFeOwner).SetStatus(stDCeIdle);
  end;
  {
  if FDCeRetorno.CStat = 104 then  // Lote processado ?
    Result := TratarRespostaFinal;
  }
end;

procedure TDCeRetRecepcao.DefinirURL;
var
  Modelo: String;
  VerServ: Double;
begin
  FPLayout := LayDCeRetRecepcao;

  if FDeclaracoes.Count > 0 then    // Tem DCe ? Se SIM, use as informações do XML
  begin
    FcUF := FDeclaracoes.Items[0].DCe.Ide.cUF;

    if Integer(FPConfiguracoesDCe.WebServices.Ambiente) <> Integer(FDeclaracoes.Items[0].DCe.Ide.tpAmb) then
      raise EACBrDCeException.Create( ACBRDCe_CErroAmbDiferente );
  end
  else
  begin     // Se não tem DCe, use as configurações do componente
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;

  VerServ := VersaoDCeToDbl(FVersaoDF);
  Modelo := 'DCe';
  FTpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  TACBrDCe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    CUFtoUF(FcUF),
    FTpAmb,
    LayOutDCeToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TDCeRetRecepcao.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeRetRecepcao';
  FPSoapAction := FPServico + '/DCeRetRecepcao';
end;

procedure TDCeRetRecepcao.DefinirDadosMsg;
var
  ConsReciDCe: TConsReciDFe;
begin
  ConsReciDCe := TConsReciDFe.Create(FPVersaoServico, NAME_SPACE_DCE, 'DCe');
  try
    ConsReciDCe.tpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    ConsReciDCe.nRec := FRecibo;
//    ConsReciDCe.Versao := FPVersaoServico;

    AjustarOpcoes( ConsReciDCe.Gerador.Opcoes );

    ConsReciDCe.GerarXML;

    FPDadosMsg := ConsReciDCe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciDCe.Free;
  end;
end;

function TDCeRetRecepcao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'DCeRetRecepcaoResult');
  {
  FDCeRetorno.Leitor.Arquivo := ParseText(FPRetWS);
  FDCeRetorno.LerXML;

  Fversao := FDCeRetorno.versao;
  FTpAmb := FDCeRetorno.TpAmb;
  FverAplic := FDCeRetorno.verAplic;
  FcStat := FDCeRetorno.cStat;
  FcUF := FDCeRetorno.cUF;
  FPMsg := FDCeRetorno.xMotivo;
  FxMotivo := FDCeRetorno.xMotivo;
  FcMsg := FDCeRetorno.cMsg;
  FxMsg := FDCeRetorno.xMsg;

  Result := (FDCeRetorno.CStat = 105); // Lote em Processamento
  }
end;

function TDCeRetRecepcao.TratarRespostaFinal: Boolean;
var
  I, J: Integer;
  AProcDCe: TProcDCe;
//  AInfProt: TProtDFeCollection;
  SalvarXML: Boolean;
  NomeXMLSalvo: String;
begin
  Result := False;
  (*
  AInfProt := FDCeRetorno.ProtDFe;

  if (AInfProt.Count > 0) then
  begin
    FPMsg := FDCeRetorno.ProtDFe.Items[0].xMotivo;
    FxMotivo := FDCeRetorno.ProtDFe.Items[0].xMotivo;
  end;

  //Setando os retornos dos Declaracoes;
  for I := 0 to AInfProt.Count - 1 do
  begin
    for J := 0 to FDeclaracoes.Count - 1 do
    begin
      if OnlyNumber(AInfProt.Items[I].chDFe) = FDeclaracoes.Items[J].NumID then
      begin
        if (TACBrDCe(FPDFeOwner).Configuracoes.Geral.ValidarDigest) and
          (FDeclaracoes.Items[J].DCe.signature.DigestValue <>
          AInfProt.Items[I].digVal) and (AInfProt.Items[I].digVal <> '') then
        begin
          raise EACBrDCeException.Create('DigestValue do documento ' +
            FDeclaracoes.Items[J].NumID + ' não confere.');
        end;

        with FDeclaracoes.Items[J] do
        begin
          DCe.procDCe.tpAmb := AInfProt.Items[I].tpAmb;
          DCe.procDCe.verAplic := AInfProt.Items[I].verAplic;
          DCe.procDCe.chDCe := AInfProt.Items[I].chDFe;
          DCe.procDCe.dhRecbto := AInfProt.Items[I].dhRecbto;
          DCe.procDCe.nProt := AInfProt.Items[I].nProt;
          DCe.procDCe.digVal := AInfProt.Items[I].digVal;
          DCe.procDCe.cStat := AInfProt.Items[I].cStat;
          DCe.procDCe.xMotivo := AInfProt.Items[I].xMotivo;
        end;

        // Monta o XML do MDF-e assinado e com o protocolo de Autorização
        if (AInfProt.Items[I].cStat = 100) then
        begin
          AProcDCe := TProcDCe.Create;
          try
            AProcDCe.XML_DCe := RemoverDeclaracaoXML(FDeclaracoes.Items[J].XMLAssinado);
            AProcDCe.XML_Prot := AInfProt.Items[I].XMLprotDFe;
            AProcDCe.Versao := FPVersaoServico;
            AProcDCe.GerarXML;

            with FDeclaracoes.Items[J] do
            begin
              XMLOriginal := AProcDCe.Gerador.ArquivoFormatoXML;

              if FPConfiguracoesDCe.Arquivos.Salvar then
              begin
                SalvarXML := (not FPConfiguracoesDCe.Arquivos.SalvarApenasDCeProcessados) or
                             Processado;

                // Salva o XML do MDF-e assinado e protocolado
                if SalvarXML then
                begin
                  NomeXMLSalvo := '';
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                  begin
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal );  // Atualiza o XML carregado
                    NomeXMLSalvo := NomeArq;
                  end;

                  if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                    GravarXML; // Salva na pasta baseado nas configurações do PathDCe

//                  FPDFeOwner.Gravar(AInfProt.Items[I].chDCe + '-DCe.xml',
//                                    XMLOriginal,
//                                    PathWithDelim(FPConfiguracoesDCe.Arquivos.GetPathDCe(0)));
                end;
              end;
            end;
          finally
            AProcDCe.Free;
          end;
        end;

        break;
      end;
    end;
  end;
  *)
  //Verificando se existe algum Manifesto confirmado
  for I := 0 to FDeclaracoes.Count - 1 do
  begin
    if FDeclaracoes.Items[I].Confirmado then
    begin
      Result := True;
      break;
    end;
  end;

  //Verificando se existe algum Manifesto nao confirmado
  for I := 0 to FDeclaracoes.Count - 1 do
  begin
    if not FDeclaracoes.Items[I].Confirmado then
    begin
      FPMsg := ACBrStr('Manifesto(s) não confirmado(s):') + LineBreak;
      break;
    end;
  end;

  //Montando a mensagem de retorno para os Declaracoes nao confirmados
  for I := 0 to FDeclaracoes.Count - 1 do
  begin
    if not FDeclaracoes.Items[I].Confirmado then
      FPMsg := FPMsg + IntToStr(FDeclaracoes.Items[I].DCe.Ide.nDC) +
        '->' + IntToStr(FDeclaracoes.Items[I].cStat) + '-' + FDeclaracoes.Items[I].Msg + LineBreak;
  end;
  {
  if AInfProt.Count > 0 then
  begin
    FChaveDCe := AInfProt.Items[0].chDFe;
    FProtocolo := AInfProt.Items[0].nProt;
    FcStat := AInfProt.Items[0].cStat;
  end;
  }
end;

procedure TDCeRetRecepcao.FinalizarServico;
begin
  // Sobrescrito, para não liberar para stIdle... não ainda...;
end;

function TDCeRetRecepcao.GerarMsgLog: String;
begin
  {
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak +
                           'cMsg: %s ' + LineBreak +
                           'xMsg: %s ' + LineBreak),
                   [FDCeRetorno.versao, TpAmbToStr(FDCeRetorno.tpAmb),
                    FDCeRetorno.verAplic, FDCeRetorno.nRec,
                    IntToStr(FDCeRetorno.cStat), FDCeRetorno.xMotivo,
                    CodigoParaUF(FDCeRetorno.cUF), IntToStr(FDCeRetorno.cMsg),
                    FDCeRetorno.xMsg]);
  }
end;

function TDCeRetRecepcao.GerarPrefixoArquivo: String;
begin
  Result := Recibo;
end;

{ TDCeRecibo }

constructor TDCeRecibo.Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
begin
  inherited Create(AOwner);

  FDeclaracoes := ADeclaracoes;
end;

destructor TDCeRecibo.Destroy;
begin
//  FDCeRetorno.Free;

  inherited Destroy;
end;

procedure TDCeRecibo.Clear;
begin
  inherited Clear;

  FPStatus := stDCeRecibo;
  FPLayout := LayDCeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

  Fversao := '';
  FxMsg := '';
  FcMsg := 0;
  FverAplic := '';
  FcStat    := 0;
  FxMotivo  := '';

  if Assigned(FPConfiguracoesDCe) then
  begin
    FtpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;
  {
  if Assigned(FDCeRetorno) then
    FDCeRetorno.Free;

  FDCeRetorno := TRetConsReciDFe.Create('DCe');
  }
end;

procedure TDCeRecibo.InicializarServico;
var
  ok: Boolean;
begin
  if FDeclaracoes.Count > 0 then    // Tem DCe ? Se SIM, use as informações do XML
    FVersaoDF := DblToVersaoDCe(ok, FDeclaracoes.Items[0].DCe.infDCe.Versao)
  else
    FVersaoDF := FPConfiguracoesDCe.Geral.VersaoDF;

  inherited InicializarServico;
end;

procedure TDCeRecibo.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeRetRecepcao';
  FPSoapAction := FPServico + '/DCeRetRecepcao';
end;

procedure TDCeRecibo.DefinirURL;
var
  Modelo: String;
  VerServ: Double;
begin
  FPLayout := LayDCeRetRecepcao;

  if FDeclaracoes.Count > 0 then    // Tem DCe ? Se SIM, use as informações do XML
  begin
    FcUF := FDeclaracoes.Items[0].DCe.Ide.cUF;

    if Integer(FPConfiguracoesDCe.WebServices.Ambiente) <> Integer(FDeclaracoes.Items[0].DCe.Ide.tpAmb) then
      raise EACBrDCeException.Create( ACBRDCe_CErroAmbDiferente );
  end
  else
  begin     // Se não tem DCe, use as configurações do componente
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;

  VerServ := VersaoDCeToDbl(FVersaoDF);
  Modelo := 'DCe';
  FTpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
  FPVersaoServico := '';
  FPURL := '';

  TACBrDCe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    CUFtoUF(FcUF),
    FTpAmb,
    LayOutDCeToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TDCeRecibo.DefinirDadosMsg;
var
  ConsReciDCe: TConsReciDFe;
begin
  ConsReciDCe := TConsReciDFe.Create(FPVersaoServico, NAME_SPACE_DCE, 'DCe');
  try
    ConsReciDCe.tpAmb := FTpAmb;
    ConsReciDCe.nRec := FRecibo;
//    ConsReciDCe.Versao := FPVersaoServico;

    AjustarOpcoes( ConsReciDCe.Gerador.Opcoes );

    ConsReciDCe.GerarXML;

    FPDadosMsg := ConsReciDCe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciDCe.Free;
  end;
end;

function TDCeRecibo.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'DCeRetRecepcaoResult');
  {
  FDCeRetorno.Leitor.Arquivo := ParseText(FPRetWS);
  FDCeRetorno.LerXML;

  Fversao := FDCeRetorno.versao;
  FTpAmb := FDCeRetorno.TpAmb;
  FverAplic := FDCeRetorno.verAplic;
  FcStat := FDCeRetorno.cStat;
  FxMotivo := FDCeRetorno.xMotivo;
  FcUF := FDCeRetorno.cUF;
  FxMsg := FDCeRetorno.xMsg;
  FcMsg := FDCeRetorno.cMsg;
  FPMsg := FxMotivo;

  Result := (FDCeRetorno.CStat = 104);
  }
end;

function TDCeRecibo.GerarMsgLog: String;
begin
 {
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FDCeRetorno.versao, TpAmbToStr(FDCeRetorno.TpAmb),
                   FDCeRetorno.verAplic, FDCeRetorno.nRec,
                   IntToStr(FDCeRetorno.cStat),
                   FDCeRetorno.xMotivo,
                   CodigoParaUF(FDCeRetorno.cUF)]);
  }
end;

{ TDCeConsulta }

constructor TDCeConsulta.Create(AOwner: TACBrDFe; ADeclaracoes: TDeclaracoes);
begin
  inherited Create(AOwner);

  FOwner := AOwner;
  FDeclaracoes := ADeclaracoes;
end;

destructor TDCeConsulta.Destroy;
begin
//  FprotDCe.Free;
//  FprocEventoDCe.Free;

  inherited Destroy;
end;

procedure TDCeConsulta.Clear;
begin
  inherited Clear;

  FPStatus := stDCeConsulta;
  FPLayout := LayDCeConsulta;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';

  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  FProtocolo := '';
  FDhRecbto := 0;
  Fversao := '';
  FRetDCeDFe := '';

  if Assigned(FPConfiguracoesDCe) then
  begin
    FtpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;
  {
  if Assigned(FprotDCe) then
    FprotDCe.Free;

  if Assigned(FprocEventoDCe) then
    FprocEventoDCe.Free;

  FprotDCe       := TProcDCe.Create;
  FprocEventoDCe := TRetEventoDCeCollection.Create;
  }
end;

procedure TDCeConsulta.SetDCeChave(const AValue: String);
var
  NumChave: String;
begin
  if FDCeChave = AValue then Exit;
    NumChave := OnlyNumber(AValue);

  if not ValidarChave(NumChave) then
    raise EACBrDCeException.Create(Format('Chave "%s" inválida.',[AValue]));

  FDCeChave := NumChave;
end;

procedure TDCeConsulta.DefinirURL;
var
  VerServ: Double;
  Modelo: String;
  Ambiente: Integer;
begin
  FPVersaoServico := '';
  FPURL   := '';
  Modelo  := 'DCe';
  FcUF    := ExtrairUFChaveAcesso(FDCeChave);
  VerServ := VersaoDCeToDbl(FPConfiguracoesDCe.Geral.VersaoDF);
  {
  if FDeclaracoes.Count > 0 then
    FTpAmb := FDeclaracoes.Items[0].DCe.Ide.tpAmb
  else
    FTpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
  }
  if FDeclaracoes.Count > 0 then
    Ambiente := Integer(FDeclaracoes.Items[0].DCe.Ide.tpAmb)
  else
    Ambiente := Integer(FPConfiguracoesDCe.WebServices.Ambiente);

  TACBrDCe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    CUFtoUF(FcUF),
    TpcnTipoAmbiente(Ambiente),
    LayOutDCeToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TDCeConsulta.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeConsulta';
  FPSoapAction := FPServico + '/DCeConsultaMDF';
end;

procedure TDCeConsulta.DefinirDadosMsg;
//var
//  ConsSitDCe: TConsSitDCe;
begin
{
  ConsSitDCe := TConsSitDCe.Create;
  try
    ConsSitDCe.TpAmb := FTpAmb;
    ConsSitDCe.chDCe := FDCeChave;
    ConsSitDCe.Versao := FPVersaoServico;

    AjustarOpcoes( ConsSitDCe.Gerador.Opcoes );

    ConsSitDCe.GerarXML;

    FPDadosMsg := ConsSitDCe.Gerador.ArquivoFormatoXML;
  finally
    ConsSitDCe.Free;
  end;
  }
end;

function TDCeConsulta.GerarUFSoap: String;
begin
  Result := '<cUF>' + IntToStr(FcUF) + '</cUF>';
end;

function TDCeConsulta.TratarResposta: Boolean;

procedure SalvarEventos(Retorno: string);
var
  aEvento, aProcEvento, aIDEvento, sPathEvento, sCNPJ: string;
  Inicio, Fim: Integer;
  TipoEvento: TpcnTpEvento;
  Ok: Boolean;
begin
  while Retorno <> '' do
  begin
    Inicio := Pos('<procEventoDCe', Retorno);
    Fim    := Pos('</procEventoDCe>', Retorno) + 15;

    aEvento := Copy(Retorno, Inicio, Fim - Inicio + 1);

    Retorno := Copy(Retorno, Fim + 1, Length(Retorno));

    aProcEvento := '<procEventoDCe versao="' + FVersao + '" ' + NAME_SPACE_DCE + '>' +
                      SeparaDados(aEvento, 'procEventoDCe') +
                   '</procEventoDCe>';

    Inicio := Pos('Id=', aProcEvento) + 6;
    Fim    := 52;

    if Inicio = 6 then
      aIDEvento := FormatDateTime('yyyymmddhhnnss', Now)
    else
      aIDEvento := Copy(aProcEvento, Inicio, Fim);

//    TipoEvento  := StrToTpEventoDCe(Ok, SeparaDados(aEvento, 'tpEvento'));
    sCNPJ       := SeparaDados(aEvento, 'CNPJ');
    sPathEvento := PathWithDelim(FPConfiguracoesDCe.Arquivos.GetPathEvento(TipoEvento, sCNPJ));

    if (aProcEvento <> '') then
      FPDFeOwner.Gravar( aIDEvento + '-procEventoDCe.xml', aProcEvento, sPathEvento);
  end;
end;

var
//  DCeRetorno: TRetConsSitDCe;
  SalvarXML, MDFCancelado, DCencerrado, Atualiza: Boolean;
  aEventos, sPathDCe, NomeXMLSalvo: String;
  AProcDCe: TProcDCe;
  I, J, Inicio, Fim: Integer;
  dhEmissao: TDateTime;
begin
  (*
  DCeRetorno := TRetConsSitDCe.Create;

  try
    FPRetWS := SeparaDados(FPRetornoWS, 'DCeConsultaMDFResult');

    DCeRetorno.Leitor.Arquivo := ParseText(FPRetWS);
    DCeRetorno.LerXML;

    MDFCancelado := False;
    DCencerrado := False;
    aEventos := '';

    // <retConsSitDCe> - Retorno da consulta da situação da NF-e
    // Este é o status oficial da NF-e
    Fversao := DCeRetorno.versao;
    FTpAmb := DCeRetorno.tpAmb;
    FverAplic := DCeRetorno.verAplic;
    FcStat := DCeRetorno.cStat;
    FXMotivo := DCeRetorno.xMotivo;
    FcUF := DCeRetorno.cUF;
//    FDCeChave := DCeRetorno.chDCe;
    FPMsg := FXMotivo;

    // <protDCe> - Retorno dos dados do ENVIO da NF-e
    // Considerá-los apenas se não existir nenhum evento de cancelamento (110111)
    FprotDCe.PathDCe            := DCeRetorno.protDCe.PathDCe;
    FprotDCe.PathRetConsReciDCe := DCeRetorno.protDCe.PathRetConsReciDCe;
    FprotDCe.PathRetConsSitDCe  := DCeRetorno.protDCe.PathRetConsSitDCe;
    FprotDCe.tpAmb               := DCeRetorno.protDCe.tpAmb;
    FprotDCe.verAplic            := DCeRetorno.protDCe.verAplic;
    FprotDCe.chDCe              := DCeRetorno.protDCe.chDCe;
    FprotDCe.dhRecbto            := DCeRetorno.protDCe.dhRecbto;
    FprotDCe.nProt               := DCeRetorno.protDCe.nProt;
    FprotDCe.digVal              := DCeRetorno.protDCe.digVal;
    FprotDCe.cStat               := DCeRetorno.protDCe.cStat;
    FprotDCe.xMotivo             := DCeRetorno.protDCe.xMotivo;

    if Assigned(DCeRetorno.procEventoDCe) and (DCeRetorno.procEventoDCe.Count > 0) then
    begin
      aEventos := '=====================================================' +
        LineBreak + '================== Eventos da MDF-e ==================' +
        LineBreak + '=====================================================' +
        LineBreak + '' + LineBreak + 'Quantidade total de eventos: ' +
        IntToStr(DCeRetorno.procEventoDCe.Count);

      FprocEventoDCe.Clear;
      for I := 0 to DCeRetorno.procEventoDCe.Count - 1 do
      begin
        with FprocEventoDCe.New.RetEventoDCe do
        begin
          idLote := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.idLote;
          tpAmb := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.tpAmb;
          verAplic := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.verAplic;
          cOrgao := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.cOrgao;
          cStat := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.cStat;
          xMotivo := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.xMotivo;
          XML := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.XML;

          Infevento.ID              := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.ID;
          Infevento.tpAmb           := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.tpAmb;
          InfEvento.CNPJCPF         := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.CNPJCPF;
          InfEvento.chDCe          := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.chDCe;
          InfEvento.dhEvento        := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.dhEvento;
          InfEvento.TpEvento        := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.TpEvento;
          InfEvento.nSeqEvento      := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.nSeqEvento;
          InfEvento.VersaoEvento    := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.VersaoEvento;
          InfEvento.DetEvento.nProt := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.nProt;
          InfEvento.DetEvento.xJust := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.xJust;
          InfEvento.DetEvento.xNome := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.xNome;
          InfEvento.DetEvento.CPF   := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.CPF;
          InfEvento.DetEvento.cUF   := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.cUF;
          InfEvento.DetEvento.cMun  := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.cMun;
          InfEvento.DetEvento.dtEnc := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.InfEvento.DetEvento.dtEnc;

          retEvento.Clear;
          for J := 0 to DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Count-1 do
          begin
            with retEvento.New.RetInfEvento do
            begin
              Id          := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.Id;
              tpAmb       := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.tpAmb;
              verAplic    := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.verAplic;
              cOrgao      := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.cOrgao;
              cStat       := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.cStat;
              xMotivo     := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.xMotivo;
              chDCe      := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.chDCe;
              tpEvento    := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.tpEvento;
              xEvento     := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.xEvento;
              nSeqEvento  := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.nSeqEvento;
              CNPJDest    := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.CNPJDest;
              emailDest   := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.emailDest;
              dhRegEvento := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.dhRegEvento;
              nProt       := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.nProt;
              XML         := DCeRetorno.procEventoDCe.Items[I].RetEventoDCe.retEvento.Items[j].RetInfEvento.XML;
            end;
          end;
        end;

        with DCeRetorno.procEventoDCe.Items[I].RetEventoDCe do
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

            if retEvento.Items[J].RetInfEvento.tpEvento = teCancelamento then
            begin
              MDFCancelado := True;
              FProtocolo := retEvento.Items[J].RetInfEvento.nProt;
              FDhRecbto := retEvento.Items[J].RetInfEvento.dhRegEvento;
              FPMsg := retEvento.Items[J].RetInfEvento.xMotivo;
            end;

            if retEvento.Items[J].RetInfEvento.tpEvento = teEncerramento then
            begin
              DCencerrado := True;
              FProtocolo := retEvento.Items[J].RetInfEvento.nProt;
              FDhRecbto := retEvento.Items[J].RetInfEvento.dhRegEvento;
              FPMsg := retEvento.Items[J].RetInfEvento.xMotivo;
            end;
          end;
        end;
      end;
    end;

    if (not MDFCancelado) and (not DCencerrado) and
       (NaoEstaVazio(DCeRetorno.protDCe.nProt))  then
    begin
      FProtocolo := DCeRetorno.protDCe.nProt;
      FDhRecbto := DCeRetorno.protDCe.dhRecbto;
      FPMsg := DCeRetorno.protDCe.xMotivo;
    end;

    with TACBrDCe(FPDFeOwner) do
    begin
      // cStat = 132 indica que o MDF-e foi encerrado
      Result := cStatProcessado(DCeRetorno.cStat) or
                cStatCancelado(DCeRetorno.cStat) or
                (DCeRetorno.cStat = 132);
    end;

    if Result then
    begin
      if TACBrDCe(FPDFeOwner).Declaracoes.Count > 0 then
      begin
        for i := 0 to TACBrDCe(FPDFeOwner).Declaracoes.Count - 1 do
        begin
          with TACBrDCe(FPDFeOwner).Declaracoes.Items[i] do
          begin
            if (OnlyNumber(FDCeChave) = NumID) then
            begin
              Atualiza := (NaoEstaVazio(DCeRetorno.XMLprotDCe));

              if Atualiza and
                 TACBrDCe(FPDFeOwner).cStatCancelado(DCeRetorno.cStat) then
                Atualiza := False;

              if (FPConfiguracoesDCe.Geral.ValidarDigest) and
                 (DCeRetorno.protDCe.digVal <> '') and (DCe.signature.DigestValue <> '') and
                 (UpperCase(DCe.signature.DigestValue) <> UpperCase(DCeRetorno.protDCe.digVal)) then
              begin
                raise EACBrDCeException.Create('DigestValue do documento ' +
                    NumID + ' não confere.');
              end;

              // Atualiza o Status da DCe interna //
              DCe.procDCe.cStat := DCeRetorno.cStat;

              if Atualiza then
              begin
                DCe.procDCe.tpAmb := DCeRetorno.tpAmb;
                DCe.procDCe.verAplic := DCeRetorno.verAplic;
                DCe.procDCe.chDCe := DCeRetorno.chDCe;
                DCe.procDCe.dhRecbto := FDhRecbto;
                DCe.procDCe.nProt := FProtocolo;
                DCe.procDCe.digVal := DCeRetorno.protDCe.digVal;
                DCe.procDCe.cStat := DCeRetorno.cStat;
                DCe.procDCe.xMotivo := DCeRetorno.xMotivo;

                AProcDCe := TProcDCe.Create;
                try
                  AProcDCe.XML_DCe := RemoverDeclaracaoXML(XMLOriginal);
                  AProcDCe.XML_Prot := DCeRetorno.XMLprotDCe;
                  AProcDCe.Versao := FPVersaoServico;
                  AProcDCe.GerarXML;

                  XMLOriginal := AProcDCe.Gerador.ArquivoFormatoXML;

                  FRetDCeDFe := '';

                  if (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoDCe'))) then
                  begin
                    Inicio := Pos('<procEventoDCe', FPRetWS);
                    Fim    := Pos('</retConsSitDCe', FPRetWS) -1;

                    aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

                    FRetDCeDFe := '<DCeDFe>' +
                                    '<procDCe versao="' + FVersao + '">' +
                                      SeparaDados(XMLOriginal, 'DCeProc') +
                                    '</procDCe>' +
                                    '<procEventoDCe versao="' + FVersao + '">' +
                                      aEventos +
                                    '</procEventoDCe>' +
                                   '</DCeDFe>';

                  end;
                finally
                  AProcDCe.Free;
                end;

                SalvarXML := Result and
                           FPConfiguracoesDCe.Arquivos.Salvar and
                           ((not FPConfiguracoesDCe.Arquivos.SalvarApenasDCeProcessados) or
                             Processado);

                if SalvarXML then
                begin
                  if FPConfiguracoesDCe.Arquivos.EmissaoPathDCe then
                    dhEmissao := DCe.Ide.dhEmi
                  else
                    dhEmissao := Now;

                  sPathDCe := PathWithDelim(FPConfiguracoesDCe.Arquivos.GetPathDCe(dhEmissao, DCe.Emit.CNPJCPF, DCe.emit.IE));

                  if (FRetDCeDFe <> '') then
                    FPDFeOwner.Gravar( FDCeChave + '-DCeDFe.xml', FRetDCeDFe, sPathDCe);

                  // Salva o XML do MDF-e assinado e protocolado
                  NomeXMLSalvo := '';
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                  begin
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal );  // Atualiza o XML carregado
                    NomeXMLSalvo := NomeArq;
                  end;

                  // Salva na pasta baseado nas configurações do PathCTe
                  if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                    GravarXML;

                  // Salva o XML de eventos retornados ao consultar um MDF-e
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
        if ExtrairEventos and FPConfiguracoesDCe.Arquivos.Salvar and
           (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoDCe'))) then
        begin
          Inicio := Pos('<procEventoDCe', FPRetWS);
          Fim    := Pos('</retConsSitDCe', FPRetWS) -1;

          aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

          // Salva o XML de eventos retornados ao consultar um MDF-e
          SalvarEventos(aEventos);
        end;
      end;
    end;
  finally
    DCeRetorno.Free;
  end;
  *)
end;

function TDCeConsulta.GerarMsgLog: String;
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
                   [Fversao, FDCeChave, TpAmbToStr(FTpAmb), FverAplic,
                    IntToStr(FcStat), FXMotivo, CodigoUFParaUF(FcUF), FDCeChave,
                    FormatDateTimeBr(FDhRecbto), FProtocolo, FprotDCe.digVal]);
end;

function TDCeConsulta.GerarPrefixoArquivo: String;
begin
  Result := Trim(FDCeChave);
end;

{ TDCeEnvEvento }

constructor TDCeEnvEvento.Create; //(AOwner: TACBrDFe; AEvento: TEventoDCe);
begin
//  inherited Create(AOwner);

//  FEvento := AEvento;
end;

destructor TDCeEnvEvento.Destroy;
begin
//  if Assigned(FEventoRetorno) then
//    FEventoRetorno.Free;

  inherited;
end;

procedure TDCeEnvEvento.Clear;
begin
  inherited Clear;

  FPStatus := stDCeEvento;
  FPLayout := LayDCeEvento;
  FPArqEnv := 'ped-eve';
  FPArqResp := 'eve';

  FcStat   := 0;
  FxMotivo := '';
  FCNPJ := '';

  if Assigned(FPConfiguracoesDCe) then
    FtpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
  {
  if Assigned(FEventoRetorno) then
    FEventoRetorno.Free;

  FEventoRetorno := TRetEventoDCe.Create;
  }
end;

function TDCeEnvEvento.GerarPathEvento(const ACNPJ, AIE: String): String;
begin
 {
  with FEvento.Evento.Items[0].InfEvento do
  begin
    Result := FPConfiguracoesDCe.Arquivos.GetPathEvento(tpEvento, ACNPJ, AIE);
  end;
  }
end;

procedure TDCeEnvEvento.DefinirURL;
var
  UF, Modelo : String;
  VerServ: Double;
begin
  { Verificação necessária pois somente os eventos de Cancelamento e CCe serão tratados pela SEFAZ do estado
    os outros eventos como manifestacao de destinatários serão tratados diretamente pela RFB }
  {
  VerServ := VersaoDCeToDbl(FPConfiguracoesDCe.Geral.VersaoDF);
  FCNPJ   := FEvento.Evento.Items[0].InfEvento.CNPJCPF;
  FTpAmb  := FEvento.Evento.Items[0].InfEvento.tpAmb;
  Modelo  := 'DCe';
  UF      := CUFtoUF(ExtrairUFChaveAcesso(FEvento.Evento.Items[0].InfEvento.chDCe));
  }
  FPLayout := LayDCeEvento;

  FPURL := '';

  TACBrDCe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    UF,
    FTpAmb,
    LayOutDCeToServico(FPLayout),
    VerServ,
    FPURL
  );

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TDCeEnvEvento.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeRecepcaoEvento';
  FPSoapAction := FPServico + '/DCeRecepcaoEvento';
end;

procedure TDCeEnvEvento.DefinirDadosMsg;
var
//  EventoDCe: TEventoDCe;
  I, J, k, F: Integer;
  Evento, Eventos, EventosAssinados, AXMLEvento: AnsiString;
  FErroValidacao: String;
  EventoEhValido: Boolean;
  SchemaEventoDCe: TSchemaDCe;
begin
(*
  EventoDCe := TEventoDCe.Create;
  try
    EventoDCe.idLote := FidLote;
    SchemaEventoDCe := schErro;
    
    for I := 0 to FEvento.Evento.Count - 1 do
    begin
      with EventoDCe.Evento.New do
      begin
        infEvento.tpAmb      := FTpAmb;
        infEvento.CNPJCPF    := FEvento.Evento[i].InfEvento.CNPJCPF;
        infEvento.chDCe     := FEvento.Evento[i].InfEvento.chDCe;
        infEvento.dhEvento   := FEvento.Evento[i].InfEvento.dhEvento;
        infEvento.tpEvento   := FEvento.Evento[i].InfEvento.tpEvento;
        infEvento.nSeqEvento := FEvento.Evento[i].InfEvento.nSeqEvento;
        infEvento.versaoEvento := FEvento.Evento[i].InfEvento.versaoEvento;

        case InfEvento.tpEvento of
          teCancelamento:
          begin
            SchemaEventoDCe := schevCancDCe;
            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;
            infEvento.detEvento.xJust := FEvento.Evento[i].InfEvento.detEvento.xJust;
          end;

          teEncerramento:
          begin
            SchemaEventoDCe := schevEncDCe;
            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;
            infEvento.detEvento.dtEnc := FEvento.Evento[i].InfEvento.detEvento.dtEnc;
            infEvento.detEvento.cUF   := FEvento.Evento[i].InfEvento.detEvento.cUF;
            infEvento.detEvento.cMun  := FEvento.Evento[i].InfEvento.detEvento.cMun;
          end;

          teInclusaoCondutor:
          begin
            SchemaEventoDCe := schevIncCondutorDCe;
            infEvento.detEvento.xNome := FEvento.Evento[i].InfEvento.detEvento.xNome;
            infEvento.detEvento.CPF   := FEvento.Evento[i].InfEvento.detEvento.CPF;
          end;

          teInclusaoDFe:
          begin
            SchemaEventoDCe := schevInclusaoDFeDCe;
            infEvento.detEvento.nProt       := FEvento.Evento[i].InfEvento.detEvento.nProt;
            infEvento.detEvento.cMunCarrega := FEvento.Evento[i].InfEvento.detEvento.cMunCarrega;
            infEvento.detEvento.xMunCarrega := FEvento.Evento[i].InfEvento.detEvento.xMunCarrega;

            for j := 0 to FEvento.Evento[i].InfEvento.detEvento.infDoc.Count - 1 do
            begin
              with EventoDCe.Evento[i].InfEvento.detEvento.infDoc.New do
              begin
                cMunDescarga := FEvento.Evento[i].InfEvento.detEvento.infDoc[j].cMunDescarga;
                xMunDescarga := FEvento.Evento[i].InfEvento.detEvento.infDoc[j].xMunDescarga;
                chNFe        := FEvento.Evento[i].InfEvento.detEvento.infDoc[j].chNFe;
              end;
            end;
          end;

          tePagamentoOperacao:
          begin
            SchemaEventoDCe := schevPagtoOperDCe;
            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;

            infEvento.detEvento.infViagens.qtdViagens := FEvento.Evento[i].InfEvento.detEvento.infViagens.qtdViagens;
            infEvento.detEvento.infViagens.nroViagem  := FEvento.Evento[i].InfEvento.detEvento.infViagens.nroViagem;

            for j := 0 to FEvento.Evento[i].InfEvento.detEvento.infPag.Count - 1 do
            begin
              with EventoDCe.Evento[i].InfEvento.detEvento.infPag.New do
              begin
                xNome         := FEvento.Evento[i].InfEvento.detEvento.infPag[j].xNome;
                idEstrangeiro := FEvento.Evento[i].InfEvento.detEvento.infPag[j].idEstrangeiro;
                CNPJCPF       := FEvento.Evento[i].InfEvento.detEvento.infPag[j].CNPJCPF;

                for k := 0 to FEvento.Evento[i].InfEvento.detEvento.infPag[j].Comp.Count - 1 do
                begin
                  with EventoDCe.Evento[i].InfEvento.detEvento.infPag[j].Comp.New do
                  begin
                    tpComp := FEvento.Evento[i].InfEvento.detEvento.infPag[j].Comp[k].tpComp;
                    vComp  := FEvento.Evento[i].InfEvento.detEvento.infPag[j].Comp[k].vComp;
                    xComp  := FEvento.Evento[i].InfEvento.detEvento.infPag[j].Comp[k].xComp;
                  end;
                end;

                vContrato := FEvento.Evento[i].InfEvento.detEvento.infPag[j].vContrato;
                indPag    := FEvento.Evento[i].InfEvento.detEvento.infPag[j].indPag;
                vAdiant   := FEvento.Evento[i].InfEvento.detEvento.infPag[j].vAdiant;

                if indPag = ipPrazo then
                begin
                  for k := 0 to FEvento.Evento[i].InfEvento.detEvento.infPag[j].infPrazo.Count - 1 do
                  begin
                    with EventoDCe.Evento[i].InfEvento.detEvento.infPag[j].infPrazo.New do
                    begin
                      nParcela := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infPrazo[k].nParcela;
                      dVenc    := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infPrazo[k].dVenc;
                      vParcela := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infPrazo[k].vParcela;
                    end;
                  end;
                end;

                infBanc.PIX        := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infBanc.PIX;
                infBanc.CNPJIPEF   := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infBanc.CNPJIPEF;
                infBanc.codBanco   := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infBanc.codBanco;
                infBanc.codAgencia := FEvento.Evento[i].InfEvento.detEvento.infPag[j].infBanc.codAgencia;
              end;
            end;
          end;
        end;
      end;
    end;

    EventoDCe.Versao := FPVersaoServico;

    AjustarOpcoes( EventoDCe.Gerador.Opcoes );

    EventoDCe.GerarXML;

    Eventos := NativeStringToUTF8( EventoDCe.Gerador.ArquivoFormatoXML );
    EventosAssinados := '';

    // Realiza a assinatura para cada evento
    while Eventos <> '' do
    begin
      F := Pos('</eventoDCe>', Eventos);

      if F > 0 then
      begin
        Evento := Copy(Eventos, 1, F + 12);
        Eventos := Copy(Eventos, F + 13, length(Eventos));

        AssinarXML(Evento, 'eventoDCe', 'infEvento', 'Falha ao assinar o Envio de Evento ');
        EventosAssinados := EventosAssinados + FPDadosMsg;
      end
      else
        Break;
    end;

    // Separa o XML especifico do Evento para ser Validado.
    AXMLEvento := SeparaDados(FPDadosMsg, 'detEvento');

    case SchemaEventoDCe of
      schevCancDCe:
        begin
          AXMLEvento := '<evCancDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evCancDCe>', '</evCancDCe>')) +
                        '</evCancDCe>';
        end;

      schevEncDCe:
        begin
          AXMLEvento := '<evEncDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evEncDCe>', '</evEncDCe>')) +
                        '</evEncDCe>';
        end;

      schevIncCondutorDCe:
        begin
          AXMLEvento := '<evIncCondutorDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evIncCondutorDCe>', '</evIncCondutorDCe>')) +
                        '</evIncCondutorDCe>';
        end;

      schevInclusaoDFeDCe:
        begin
          AXMLEvento := '<evIncDFeDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evIncDFeDCe>', '</evIncDFeDCe>')) +
                        '</evIncDFeDCe>';
        end;

      schevPagtoOperDCe:
        begin
          AXMLEvento := '<evPagtoOperDCe xmlns="' + ACBRDCe_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evPagtoOperDCe>', '</evPagtoOperDCe>')) +
                        '</evPagtoOperDCe>';
        end;
    end;

    AXMLEvento := '<' + ENCODING_UTF8 + '>' + AXMLEvento;

    with TACBrDCe(FPDFeOwner) do
    begin
      EventoEhValido := SSL.Validar(FPDadosMsg,
                                    GerarNomeArqSchema(FPLayout,
                                                       StringToFloatDef(FPVersaoServico, 0)),
                                    FPMsg) and
                        SSL.Validar(AXMLEvento,
                                    GerarNomeArqSchemaEvento(SchemaEventoDCe,
                                                             StringToFloatDef(FPVersaoServico, 0)),
                                    FPMsg);
    end;

    if not EventoEhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do Evento: ') +
        FPMsg;

      raise EACBrDCeException.CreateDef(FErroValidacao);
    end;

    for I := 0 to FEvento.Evento.Count - 1 do
      FEvento.Evento[I].InfEvento.id := EventoDCe.Evento[I].InfEvento.id;
  finally
    EventoDCe.Free;
  end;
  *)
end;

function TDCeEnvEvento.TratarResposta: Boolean;
var
  Leitor: TLeitor;
  I, J: Integer;
  NomeArq, PathArq, VersaoEvento, Texto: String;
begin
 (*
  FEvento.idLote := idLote;

  FPRetWS := SeparaDados(FPRetornoWS, 'DCeRecepcaoEventoResult');

  EventoRetorno.Leitor.Arquivo := ParseText(FPRetWS);
  EventoRetorno.LerXml;

  FcStat := EventoRetorno.cStat;
  FxMotivo := EventoRetorno.xMotivo;
  FPMsg := EventoRetorno.xMotivo;
  FTpAmb := EventoRetorno.tpAmb;

  Result := (FcStat in [135, 136]);

  //gerar arquivo proc de evento
  if Result then
  begin
    Leitor := TLeitor.Create;
    try
      for I := 0 to FEvento.Evento.Count - 1 do
      begin
        for J := 0 to EventoRetorno.retEvento.Count - 1 do
        begin
          if FEvento.Evento.Items[I].InfEvento.chDCe =
            EventoRetorno.retEvento.Items[J].RetInfEvento.chDCe then
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

            VersaoEvento := TACBrDCe(FPDFeOwner).LerVersaoDeParams(LayDCeEvento);


            Leitor.Arquivo := FPDadosMsg;
            Texto := '<procEventoDCe versao="' + VersaoEvento + '" xmlns="' + ACBRDCe_NAMESPACE + '">' +
                      '<eventoDCe versao="' + VersaoEvento + '">' +
                       Leitor.rExtrai(1, 'infEvento', '', I + 1) +
                       '<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">' +
                        Leitor.rExtrai(1, 'SignedInfo', '', I + 1) +
                        Leitor.rExtrai(1, 'SignatureValue', '', I + 1) +
                        Leitor.rExtrai(1, 'KeyInfo', '', I + 1) +
                       '</Signature>' +
                      '</eventoDCe>';

            Leitor.Arquivo := FPRetWS;
            Texto := Texto +
                       '<retEventoDCe versao="' + VersaoEvento + '">' +
                        Leitor.rExtrai(1, 'infEvento', '', J + 1) +
                       '</retEventoDCe>' +
                      '</procEventoDCe>';

            if FPConfiguracoesDCe.Arquivos.Salvar then
            begin
              NomeArq := OnlyNumber(FEvento.Evento.Items[I].InfEvento.Id) + '-procEventoDCe.xml';
              PathArq := PathWithDelim(GerarPathEvento(FEvento.Evento.Items[I].InfEvento.CNPJCPF));

              FPDFeOwner.Gravar(NomeArq, Texto, PathArq);
              FEventoRetorno.retEvento.Items[J].RetInfEvento.NomeArquivo := PathArq + NomeArq;
              FEvento.Evento.Items[I].RetInfEvento.NomeArquivo := PathArq + NomeArq;
            end;

            Texto := ParseText(Texto);
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
  *)
end;

function TDCeEnvEvento.GerarMsgLog: String;
var
  aMsg: String;
begin
(*
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
  *)
  Result := aMsg;
end;

function TDCeEnvEvento.GerarPrefixoArquivo: String;
begin
//  Result := IntToStr(FEvento.idLote);
  Result := IntToStr(FidLote);
end;

{ TDCeConsultaDCeNaoEnc }

constructor TDCeConsultaDCeNaoEnc.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FOwner := AOwner;
end;

destructor TDCeConsultaDCeNaoEnc.Destroy;
begin
//  FinfDCe.Free;
//  FRetConsDCeNaoEnc.Free;

  inherited;
end;

procedure TDCeConsultaDCeNaoEnc.Clear;
begin
  inherited Clear;

  FPStatus  := stDCeConsulta;
  FPLayout  := LayDCeConsNaoEnc;
  FPArqEnv  := 'ped-cons';
  FPArqResp := 'cons';

  if Assigned(FPConfiguracoesDCe) then
  begin
    FtpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    FcUF := FPConfiguracoesDCe.WebServices.UFCodigo;
  end;
  {
  if Assigned(FinfDCe) then
    FinfDCe.Free;

  if Assigned(FRetConsDCeNaoEnc) then
    FRetConsDCeNaoEnc.Free;

  FInfDCe := TRetInfDCeCollection.Create;
  FRetConsDCeNaoEnc := TRetConsDCeNaoEnc.Create;
  }
end;

procedure TDCeConsultaDCeNaoEnc.DefinirServicoEAction;
begin
  FPServico    := GetUrlWsd + 'DCeConsNaoEnc';
  FPSoapAction := FPServico + '/DCeConsNaoEnc';
end;

procedure TDCeConsultaDCeNaoEnc.DefinirURL;
begin
  FPLayout := LayDCeConsNaoEnc;

  inherited DefinirURL;
end;

procedure TDCeConsultaDCeNaoEnc.DefinirDadosMsg;
//var
//  ConsDCeNaoEnc: TConsDCeNaoEnc;
begin
{
  ConsDCeNaoEnc := TConsDCeNaoEnc.create;
  try
    ConsDCeNaoEnc.TpAmb   := FPConfiguracoesDCe.WebServices.Ambiente;
    ConsDCeNaoEnc.CNPJCPF := OnlyNumber( FCNPJCPF );
    ConsDCeNaoEnc.Versao  := FPVersaoServico;

    AjustarOpcoes( ConsDCeNaoEnc.Gerador.Opcoes );
    ConsDCeNaoEnc.Gerador.Opcoes.RetirarAcentos := False;  // Não funciona sem acentos

    ConsDCeNaoEnc.GerarXML;

    FPDadosMsg := ConsDCeNaoEnc.Gerador.ArquivoFormatoXML;
  finally
    ConsDCeNaoEnc.Free;
  end;
  }
end;

function TDCeConsultaDCeNaoEnc.TratarResposta: Boolean;
var
  i: Integer;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'DCeConsNaoEncResult');
  {
  // Limpando variaveis internas
  FRetConsDCeNaoEnc.Free;
  FRetConsDCeNaoEnc := TRetConsDCeNaoEnc.Create;

  FRetConsDCeNaoEnc.Leitor.Arquivo := ParseText(FPRetWS);
  FRetConsDCeNaoEnc.LerXml;

  Fversao    := FRetConsDCeNaoEnc.versao;
  FtpAmb     := FRetConsDCeNaoEnc.tpAmb;
  FverAplic  := FRetConsDCeNaoEnc.verAplic;
  FcStat     := FRetConsDCeNaoEnc.cStat;
  FxMotivo   := FRetConsDCeNaoEnc.xMotivo;
  FcUF       := FRetConsDCeNaoEnc.cUF;
  FMsg       := FxMotivo;

  for i := 0 to FRetConsDCeNaoEnc.InfDCe.Count -1 do
  begin
    FinfDCe.New;
    FinfDCe.Items[i].chDCe := FRetConsDCeNaoEnc.InfDCe.Items[i].chDCe;
    FinfDCe.Items[i].nProt  := FRetConsDCeNaoEnc.InfDCe.Items[i].nProt;
  end;
  }
    // 111 = MDF-e não encerrados localizados
    // 112 = MDF-e não encerrados não localizados
  Result := (FcStat in [111, 112]);
end;

function TDCeConsultaDCeNaoEnc.GerarMsgLog: String;
begin
{
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FRetConsDCeNaoEnc.versao, TpAmbToStr(FRetConsDCeNaoEnc.tpAmb),
                    FRetConsDCeNaoEnc.verAplic, IntToStr(FRetConsDCeNaoEnc.cStat),
                    FRetConsDCeNaoEnc.xMotivo,
                    CodigoParaUF(FRetConsDCeNaoEnc.cUF)]);
  }
end;

function TDCeConsultaDCeNaoEnc.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Consulta MDF-e nao Encerradas:' + LineBreak +
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

  inherited;
end;

procedure TDistribuicaoDFe.Clear;
begin
  inherited Clear;

  FPStatus := stDCeDistDFeInt;
  FPLayout := LayDCeDistDFeInt;
  FPArqEnv := 'con-dist-dfe';
  FPArqResp := 'dist-dfe';
//  FPBodyElement := 'DCeDistDFeInteresse';
//  FPHeaderElement := '';

  if Assigned(FretDistDFeInt) then
    FretDistDFeInt.Free;

  FretDistDFeInt := TRetDistDFeInt.Create('DCe');

  if Assigned(FlistaArqs) then
    FlistaArqs.Free;

  FlistaArqs := TStringList.Create;
end;

procedure TDistribuicaoDFe.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'DCeDistribuicaoDFe';
  FPSoapAction := FPServico + '/DCeDistDFeInteresse';
end;

procedure TDistribuicaoDFe.DefinirDadosMsg;
var
  DistDFeInt: TDistDFeInt;
begin
  DistDFeInt := TDistDFeInt.Create(FPVersaoServico, NAME_SPACE_DCE,
                                   '', 'consChDCe', 'chDCe', False);
  try
    DistDFeInt.TpAmb := FPConfiguracoesDCe.WebServices.Ambiente;
    DistDFeInt.CNPJCPF := FCNPJCPF;
    DistDFeInt.ultNSU := FultNSU;
    DistDFeInt.NSU := FNSU;
    DistDFeInt.Chave := trim(FchDCe);

    AjustarOpcoes( DistDFeInt.Gerador.Opcoes );

    DistDFeInt.GerarXML;

    FPDadosMsg := DistDFeInt.Gerador.ArquivoFormatoXML;
  finally
    DistDFeInt.Free;
  end;
end;

function TDistribuicaoDFe.TratarResposta: Boolean;
var
  I: Integer;
  AXML, aPath: String;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'DCeDistDFeInteresseResult');

  // Processando em UTF8, para poder gravar arquivo corretamente //
  FretDistDFeInt.Leitor.Arquivo := FPRetWS;
  FretDistDFeInt.LerXml;

  for I := 0 to FretDistDFeInt.docZip.Count - 1 do
  begin
    AXML := FretDistDFeInt.docZip.Items[I].XML;
    FNomeArq := '';
    if (AXML <> '') then
    begin
        (*
      case FretDistDFeInt.docZip.Items[I].schema of
        schresDCe:
          FNomeArq := FretDistDFeInt.docZip.Items[I].resDCe.chDCe + '-resDCe.xml';

        schresEvento:
          FNomeArq := OnlyNumber(TpEventoToStr(FretDistDFeInt.docZip.Items[I].resEvento.tpEvento) +
                     FretDistDFeInt.docZip.Items[I].resEvento.chDCe +
                     Format('%.2d', [FretDistDFeInt.docZip.Items[I].resEvento.nSeqEvento])) +
                     '-resEventoDCe.xml';
        schprocDCe:
          FNomeArq := FretDistDFeInt.docZip.Items[I].resDFe.chDFe + '-DCe.xml';

        schprocEventoDCe:
          FNomeArq := OnlyNumber(FretDistDFeInt.docZip.Items[I].procEvento.Id) +
                     '-procEventoDCe.xml';
      end;
        *)

      if NaoEstaVazio(NomeArq) then
        FlistaArqs.Add( FNomeArq );

      aPath := GerarPathDistribuicao(FretDistDFeInt.docZip.Items[I]);
      FretDistDFeInt.docZip.Items[I].NomeArq := aPath + FNomeArq;
      {
      if (FPConfiguracoesDCe.Arquivos.Salvar) and NaoEstaVazio(FNomeArq) then
      begin
        if (FretDistDFeInt.docZip.Items[I].schema in [schprocEventoDCe]) then
          FPDFeOwner.Gravar(FNomeArq, AXML, aPath);

        if (FretDistDFeInt.docZip.Items[I].schema in [schprocDCe]) then
          FPDFeOwner.Gravar(FNomeArq, AXML, aPath);
      end;
      }
    end;
  end;

  { Processsa novamente, chamando ParseTXT, para converter de UTF8 para a String
    nativa e Decodificar caracteres HTML Entity }
  FretDistDFeInt.Free;   // Limpando a lista
  FretDistDFeInt := TRetDistDFeInt.Create('DCe');

  FretDistDFeInt.Leitor.Arquivo := ParseText(FPRetWS);
  FretDistDFeInt.LerXml;

  FPMsg := FretDistDFeInt.xMotivo;
  Result := (FretDistDFeInt.CStat = 137) or (FretDistDFeInt.CStat = 138);
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

function TDistribuicaoDFe.GerarPathDistribuicao(
  AItem: TdocZipCollectionItem): String;
var
  Data: TDateTime;
begin
  if FPConfiguracoesDCe.Arquivos.EmissaoPathDCe then
  begin
    Data := AItem.resDFe.dhEmi;
    if Data = 0 then
      Data := AItem.procEvento.dhEvento;
  end
  else
    Data := Now;
  {
  case AItem.schema of
    schprocEventoDCe:
      Result := FPConfiguracoesDCe.Arquivos.GetPathDownloadEvento(AItem.procEvento.tpEvento,
                                                           AItem.resDFe.xNome,
                                                           AItem.procEvento.CNPJ,
                                                           AItem.resDFe.IE,
                                                           Data);

    schprocDCe:
      Result := FPConfiguracoesDCe.Arquivos.GetPathDownload(AItem.resDFe.xNome,
                                                             AItem.resDFe.CNPJCPF,
                                                             AItem.resDFe.IE,
                                                             Data);
  end;
  }
end;
(*
procedure TDistribuicaoDFe.DefinirURL;
var
  UF, Modelo: String;
  Versao: Double;
begin
  { Esse método é tratado diretamente pela RFB }

  UF := 'AN';
  Modelo := 'DCe';
  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';
  Versao := VersaoDCeToDbl(FPConfiguracoesDCe.Geral.VersaoDF);

  TACBrDCe(FPDFeOwner).LerServicoDeParams(
    Modelo,
    UF ,
    FPConfiguracoesDCe.WebServices.Ambiente,
    LayOutToServico(FPLayout),
    Versao,
    FPURL);

  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;
*)
{ TDCeEnvioWebService }

constructor TDCeEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stDCeEnvioWebService;
end;

destructor TDCeEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

procedure TDCeEnvioWebService.Clear;
begin
  inherited Clear;

  FVersao := '';
end;

function TDCeEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TDCeEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TDCeEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TDCeEnvioWebService.DefinirDadosMsg;
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

function TDCeEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TDCeEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TDCeEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrDCe := TACBrDCe(AOwner);

  FStatusServico := TDCeStatusServico.Create(FACBrDCe);
  FEnviar := TDCeRecepcao.Create(FACBrDCe, TACBrDCe(FACBrDCe).Declaracoes);
  FRetorno := TDCeRetRecepcao.Create(FACBrDCe, TACBrDCe(FACBrDCe).Declaracoes);
  FRecibo := TDCeRecibo.Create(FACBrDCe, TACBrDCe(FACBrDCe).Declaracoes);
  FConsulta := TDCeConsulta.Create(FACBrDCe, TACBrDCe(FACBrDCe).Declaracoes);
//  FEnvEvento := TDCeEnvEvento.Create(FACBrDCe, TACBrDCe(FACBrDCe).EventoDCe);
  FConsDCeNaoEnc := TDCeConsultaDCeNaoEnc.Create(FACBrDCe);
  FDistribuicaoDFe := TDistribuicaoDFe.Create(FACBrDCe);
  FEnvioWebService := TDCeEnvioWebService.Create(FACBrDCe);
end;

destructor TWebServices.Destroy;
begin
  FStatusServico.Free;
  FEnviar.Free;
  FRetorno.Free;
  FRecibo.Free;
  FConsulta.Free;
  FEnvEvento.Free;
  FConsDCeNaoEnc.Free;
  FDistribuicaoDFe.Free;
  FEnvioWebService.Free;

  inherited Destroy;
end;

function TWebServices.Envia(ALote: Integer; ASincrono:  Boolean = False): Boolean;
begin
  Result := Envia(IntToStr(ALote), ASincrono);
end;

function TWebServices.Envia(const ALote: String; ASincrono:  Boolean = False): Boolean;
begin
  FEnviar.Clear;
  FRetorno.Clear;

  FEnviar.Lote := ALote;
  FEnviar.Sincrono := ASincrono;

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

function TWebServices.ConsultaDCeNaoEnc(const ACNPJCPF: String): Boolean;
begin
  FConsDCeNaoEnc.FCNPJCPF := ACNPJCPF;

  if not FConsDCeNaoEnc.Executar then
    FConsDCeNaoEnc.GerarException( FConsDCeNaoEnc.Msg );

  Result := True;
end;

end.
