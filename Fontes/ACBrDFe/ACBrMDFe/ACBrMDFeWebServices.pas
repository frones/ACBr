{******************************************************************************}
{ Projeto: Componente ACBrMDFe                                                 }
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

{*******************************************************************************
|* Historico
|*
|* 01/08/2012: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit ACBrMDFeWebServices;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeWebService,
  pmdfeMDFe,
  pmdfeRetConsReciMDFe, pcnAuxiliar, pcnConversao, pmdfeConversaoMDFe,
  pmdfeProcMDFe, pmdfeEnvEventoMDFe, pmdfeRetEnvEventoMDFe,
  pmdfeRetConsSitMDFe, pmdfeRetConsMDFeNaoEnc, pmdfeRetEnvMDFe,
  ACBrMDFeManifestos, ACBrMDFeConfiguracoes;

const
  CURL_WSDL = 'http://www.portalfiscal.inf.br/mdfe/wsdl/';
  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;

type

  { TMDFeWebService }

  TMDFeWebService = class(TDFeWebService)
  private
  protected
    FPStatus: TStatusACBrMDFe;
    FPLayout: TLayOutMDFe;
    FPConfiguracoesMDFe: TConfiguracoesMDFe;

    function ExtrairModeloChaveAcesso(AChaveMDFe: String): String;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;

  public
    constructor Create(AOwner: TACBrDFe); override;

    property Status: TStatusACBrMDFe read FPStatus;
    property Layout: TLayOutMDFe read FPLayout;
  end;

  { TMDFeStatusServico }

  TMDFeStatusServico = class(TMDFeWebService)
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
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarMsgErro(E: Exception): String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;

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

  { TMDFeRecepcao }

  TMDFeRecepcao = class(TMDFeWebService)
  private
    FLote: String;
    FRecibo: String;
    FManifestos: TManifestos;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;
    FxMotivo: String;
    FdhRecbto: TDateTime;
    FTMed: integer;

    FMDFeRetorno: TretEnvMDFe;

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
    constructor Create(AOwner: TACBrDFe; AManifestos: TManifestos);
      reintroduce; overload;
    destructor Destroy; override;

    property Recibo: String read GetRecibo;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;
    property xMotivo: String read FxMotivo;
    property dhRecbto: TDateTime read FdhRecbto;
    property TMed: integer read FTMed;
    property Lote: String read GetLote write FLote;
  end;

  { TMDFeRetRecepcao }

  TMDFeRetRecepcao = class(TMDFeWebService)
  private
    FRecibo: String;
    FProtocolo: String;
    FChaveMDFe: String;
    FManifestos: TManifestos;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;
    FxMotivo: String;
    FcMsg: integer;
    FxMsg: String;

    FMDFeRetorno: TRetConsReciMDFe;

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
    constructor Create(AOwner: TACBrDFe; AManifestos: TManifestos);
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
    property ChaveMDFe: String read FChaveMDFe write FChaveMDFe;

    property MDFeRetorno: TRetConsReciMDFe read FMDFeRetorno;
  end;

  { TMDFeRecibo }

  TMDFeRecibo = class(TMDFeWebService)
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

    FMDFeRetorno: TRetConsReciMDFe;
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

    property MDFeRetorno: TRetConsReciMDFe read FMDFeRetorno;
  end;

  { TMDFeConsulta }

  TMDFeConsulta = class(TMDFeWebService)
  private
    FMDFeChave: String;
    FProtocolo: String;
    FDhRecbto: TDateTime;
    FXMotivo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;
    FRetMDFeDFe: String;

    FprotMDFe: TProcMDFe;
    FprocEventoMDFe: TRetEventoMDFeCollection;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    property MDFeChave: String read FMDFeChave write FMDFeChave;
    property Protocolo: String read FProtocolo write FProtocolo;
    property DhRecbto: TDateTime read FDhRecbto write FDhRecbto;
    property XMotivo: String read FXMotivo write FXMotivo;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;
    property RetMDFeDFe: String read FRetMDFeDFe;

    property protMDFe: TProcMDFe read FprotMDFe;
    property procEventoMDFe: TRetEventoMDFeCollection read FprocEventoMDFe;
  end;

  { TMDFeEnvEvento }

  TMDFeEnvEvento = class(TMDFeWebService)
  private
    FidLote: integer;
    Fversao: String;
    FEvento: TEventoMDFe;
    FcStat: integer;
    FxMotivo: String;
    FTpAmb: TpcnTipoAmbiente;

    FEventoRetorno: TRetEventoMDFe;

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
    constructor Create(AOwner: TACBrDFe; AEvento: TEventoMDFe); reintroduce; overload;
    destructor Destroy; override;

    property idLote: integer read FidLote write FidLote;
    property versao: String read Fversao write Fversao;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;

    property EventoRetorno: TRetEventoMDFe read FEventoRetorno;
  end;

  { TMDFeConsultaMDFeNaoEnc }

  TMDFeConsultaMDFeNaoEnc = Class(TMDFeWebService)
  private
    FCNPJ: String;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FcUF: Integer;
    FInfMDFe: TRetInfMDFeCollection;
    FRetConsMDFeNaoEnc: TRetConsMDFeNaoEnc;
    FMsg: String;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: AnsiString; override;
    function GerarMsgErro(E: Exception): AnsiString; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    property CNPJ: String                   read FCNPJ write FCNPJ;
    property versao: String                 read Fversao;
    property tpAmb: TpcnTipoAmbiente        read FtpAmb;
    property verAplic: String               read FverAplic;
    property cStat: Integer                 read FcStat;
    property xMotivo: String                read FxMotivo;
    property cUF: Integer                   read FcUF;
    property InfMDFe: TRetInfMDFeCollection read FInfMDFe;
    property Msg: String                    read FMsg;
  end;

 { TMDFeEnvioWebService }

  TMDFeEnvioWebService = class(TMDFeWebService)
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
    FACBrMDFe: TACBrDFe;
    FStatusServico: TMDFeStatusServico;
    FEnviar: TMDFeRecepcao;
    FRetorno: TMDFeRetRecepcao;
    FRecibo: TMDFeRecibo;
    FConsulta: TMDFeConsulta;
    FEnvEvento: TMDFeEnvEvento;
    FConsMDFeNaoEnc: TMDFeConsultaMDFeNaoEnc;
    FEnvioWebService: TMDFeEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia(ALote: integer): Boolean; overload;
    function Envia(ALote: String): Boolean; overload;
    function ConsultaMDFeNaoEnc(ACNPJ: String): Boolean;

    property ACBrMDFe: TACBrDFe read FACBrMDFe write FACBrMDFe;
    property StatusServico: TMDFeStatusServico read FStatusServico write FStatusServico;
    property Enviar: TMDFeRecepcao read FEnviar write FEnviar;
    property Retorno: TMDFeRetRecepcao read FRetorno write FRetorno;
    property Recibo: TMDFeRecibo read FRecibo write FRecibo;
    property Consulta: TMDFeConsulta read FConsulta write FConsulta;
    property EnvEvento: TMDFeEnvEvento read FEnvEvento write FEnvEvento;
    property ConsMDFeNaoEnc: TMDFeConsultaMDFeNaoEnc read FConsMDFeNaoEnc write FConsMDFeNaoEnc;
    property EnvioWebService: TMDFeEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil, ACBrMDFe,
  pcnGerador, pmdfeConsStatServ, pmdfeRetConsStatServ,
  pmdfeConsSitMDFe, pmdfeConsReciMDFe, pmdfeConsMDFeNaoEnc,
  pcnLeitor, pmdfeMDFeW;

//  ACBrDFeUtil, ACBrMDFeUtil, pmdfeMDFeW,
//  pmdfeCabecalho;

{ TMDFeWebService }

constructor TMDFeWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesMDFe := TConfiguracoesMDFe(FPConfiguracoes);
  FPLayout := LayMDFeStatusServico;
  FPStatus := stMDFeIdle;
end;

function TMDFeWebService.ExtrairModeloChaveAcesso(AChaveMDFe: String): String;
begin
  AChaveMDFe := OnlyNumber(AChaveMDFe);
  if ValidarChave('MDFe' + AChaveMDFe) then
    Result := copy(AChaveMDFe, 21, 2)
  else
    Result := '';
end;

procedure TMDFeWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrMDFe(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TMDFeWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrMDFe(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);

  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;


function TMDFeWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrMDFe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TMDFeWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrMDFe(FPDFeOwner).SetStatus(stMDFeIdle);
end;

{ TMDFeStatusServico }

constructor TMDFeStatusServico.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stMDFeStatusServico;
  FPLayout := LayMDFeStatusServico;
  FPArqEnv := 'ped-sta';
  FPArqResp := 'sta';
end;

procedure TMDFeStatusServico.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'MDFeStatusServico';
  FPSoapAction := FPServico + '/mdfeStatusServicoMDF';
end;

procedure TMDFeStatusServico.DefinirDadosMsg;
var
  ConsStatServ: TConsStatServ;
begin
  ConsStatServ := TConsStatServ.Create;
  try
    ConsStatServ.TpAmb := FPConfiguracoesMDFe.WebServices.Ambiente;
    ConsStatServ.CUF := FPConfiguracoesMDFe.WebServices.UFCodigo;

    ConsStatServ.Versao := FPVersaoServico;
    ConsStatServ.GerarXML;

    // Atribuindo o XML para propriedade interna //
    FPDadosMsg := ConsStatServ.Gerador.ArquivoFormatoXML;
  finally
    ConsStatServ.Free;
  end;
end;

function TMDFeStatusServico.TratarResposta: Boolean;
var
  MDFeRetorno: TRetConsStatServ;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'mdfeStatusServicoMDFResult');

  MDFeRetorno := TRetConsStatServ.Create;
  try
    MDFeRetorno.Leitor.Arquivo := FPRetWS;
    MDFeRetorno.LerXml;

    Fversao := MDFeRetorno.versao;
    FtpAmb := MDFeRetorno.tpAmb;
    FverAplic := MDFeRetorno.verAplic;
    FcStat := MDFeRetorno.cStat;
    FxMotivo := MDFeRetorno.xMotivo;
    FcUF := MDFeRetorno.cUF;
    FdhRecbto := MDFeRetorno.dhRecbto;
    FTMed := MDFeRetorno.TMed;
    FdhRetorno := MDFeRetorno.dhRetorno;
    FxObs := MDFeRetorno.xObs;
    FPMsg := FxMotivo + LineBreak + FxObs;

    if FPConfiguracoesMDFe.WebServices.AjustaAguardaConsultaRet then
      FPConfiguracoesMDFe.WebServices.AguardarConsultaRet := FTMed * 1000;

    Result := (FcStat = 107);

  finally
    MDFeRetorno.Free;
  end;
end;

function TMDFeStatusServico.GerarMsgLog: String;
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
                   [Fversao, TpAmbToStr(FtpAmb), FverAplic, IntToStr(FcStat),
                    FxMotivo, CodigoParaUF(FcUF),
                    IfThen(FdhRecbto = 0, '', FormatDateTimeBr(FdhRecbto)),
                    IntToStr(FTMed),
                    IfThen(FdhRetorno = 0, '', FormatDateTimeBr(FdhRetorno)),
                    FxObs]);
end;

function TMDFeStatusServico.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Consulta Status serviço:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TMDFeRecepcao }

constructor TMDFeRecepcao.Create(AOwner: TACBrDFe; AManifestos: TManifestos);
begin
  inherited Create(AOwner);

  FManifestos := AManifestos;

  FPStatus := stMDFeRecepcao;
  FPLayout := LayMDFeRecepcao;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

  FMDFeRetorno := nil;
end;

destructor TMDFeRecepcao.Destroy;
begin
  if Assigned(FMDFeRetorno) then
    FMDFeRetorno.Free;

  inherited Destroy;
end;

function TMDFeRecepcao.GetLote: String;
begin
  Result := Trim(FLote);
end;

function TMDFeRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

procedure TMDFeRecepcao.DefinirURL;
begin
  FPLayout := LayMDFeRecepcao;

  inherited DefinirURL;
end;

procedure TMDFeRecepcao.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'MDFeRecepcao';
  FPSoapAction := FPServico + '/mdfeRecepcaoLote';
end;

procedure TMDFeRecepcao.DefinirDadosMsg;
var
  I: integer;
  vMDFe: String;
begin
  vMDFe := '';
  for I := 0 to FManifestos.Count - 1 do
    vMDFe := vMDFe + '<MDFe' + RetornarConteudoEntre(
      FManifestos.Items[I].XMLAssinado, '<MDFe', '</MDFe>') + '</MDFe>';

  FPDadosMsg := '<enviMDFe xmlns="http://www.portalfiscal.inf.br/mdfe" versao="' +
    FPVersaoServico + '">' + '<idLote>' + FLote + '</idLote>' +
    vMDFe + '</enviMDFe>';

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));

  FRecibo := '';
end;

function TMDFeRecepcao.TratarResposta: Boolean;
var
  I: integer;
  chMDFe, NomeArquivo: String;
  AProcMDFe: TProcMDFe;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'mdfeRecepcaoLoteResult');

  FMDFeRetorno := TretEnvMDFe.Create;

  FMDFeRetorno.Leitor.Arquivo := FPRetWS;
  FMDFeRetorno.LerXml;

  Fversao := FMDFeRetorno.versao;
  FTpAmb := FMDFeRetorno.TpAmb;
  FverAplic := FMDFeRetorno.verAplic;
  FcStat := FMDFeRetorno.cStat;
  FxMotivo := FMDFeRetorno.xMotivo;
  FdhRecbto := FMDFeRetorno.infRec.dhRecbto;
  FTMed := FMDFeRetorno.infRec.tMed;
  FcUF := FMDFeRetorno.cUF;
  FPMsg := FMDFeRetorno.xMotivo;
  FRecibo := FMDFeRetorno.infRec.nRec;

  Result := (FMDFeRetorno.CStat = 103);
end;

procedure TMDFeRecepcao.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FMDFeRetorno) then
    FreeAndNil(FMDFeRetorno);
end;

function TMDFeRecepcao.GerarMsgLog: String;
begin
  if Assigned(FMDFeRetorno) then
    Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                             'Ambiente: %s ' + LineBreak +
                             'Versão Aplicativo: %s ' + LineBreak +
                             'Status Código: %s ' + LineBreak +
                             'Status Descrição: %s ' + LineBreak +
                             'UF: %s ' + sLineBreak +
                             'Recibo: %s ' + LineBreak +
                             'Recebimento: %s ' + LineBreak +
                             'Tempo Médio: %s ' + LineBreak),
                     [FMDFeRetorno.versao,
                      TpAmbToStr(FMDFeRetorno.TpAmb),
                      FMDFeRetorno.verAplic,
                      IntToStr(FMDFeRetorno.cStat),
                      FMDFeRetorno.xMotivo,
                      CodigoParaUF(FMDFeRetorno.cUF),
                      FMDFeRetorno.infRec.nRec,
                      IfThen(FMDFeRetorno.InfRec.dhRecbto = 0, '',
                             FormatDateTimeBr(FMDFeRetorno.InfRec.dhRecbto)),
                      IntToStr(FMDFeRetorno.InfRec.TMed)])
  else
    Result := '';
end;

function TMDFeRecepcao.GerarPrefixoArquivo: String;
begin
  Result := Lote;
end;

{ TMDFeRetRecepcao }

constructor TMDFeRetRecepcao.Create(AOwner: TACBrDFe; AManifestos: TManifestos);
begin
  inherited Create(AOwner);

  FManifestos := AManifestos;
  FMDFeRetorno := TRetConsReciMDFe.Create;

  FPStatus := stMDFeRetRecepcao;
  FPLayout := LayMDFeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';
end;

destructor TMDFeRetRecepcao.Destroy;
begin
  FMDFeRetorno.Free;

  inherited Destroy;
end;

function TMDFeRetRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

function TMDFeRetRecepcao.TratarRespostaFinal: Boolean;
var
  I, J: integer;
  AProcMDFe: TProcMDFe;
  AInfProt: TProtMDFeCollection;
begin
  Result := False;

  AInfProt := FMDFeRetorno.ProtMDFe;

  if (AInfProt.Count > 0) then
  begin
    FPMsg := FMDFeRetorno.ProtMDFe.Items[0].xMotivo;
    FxMotivo := FMDFeRetorno.ProtMDFe.Items[0].xMotivo;
  end;

  //Setando os retornos dos Manifestos;
  for I := 0 to AInfProt.Count - 1 do
  begin
    for J := 0 to FManifestos.Count - 1 do
    begin
      if OnlyNumber(AInfProt.Items[I].chMDFe) = FManifestos.Items[J].NumID then
      begin
        if (TACBrMDFe(FPDFeOwner).Configuracoes.Geral.ValidarDigest) and
          (FManifestos.Items[J].MDFe.signature.DigestValue <>
          AInfProt.Items[I].digVal) and (AInfProt.Items[I].digVal <> '') then
        begin
          raise EACBrMDFeException.Create('DigestValue do documento ' +
            FManifestos.Items[J].NumID + ' não confere.');
        end;

        FManifestos.Items[J].MDFe.procMDFe.tpAmb := AInfProt.Items[I].tpAmb;
        FManifestos.Items[J].MDFe.procMDFe.verAplic := AInfProt.Items[I].verAplic;
        FManifestos.Items[J].MDFe.procMDFe.chMDFe := AInfProt.Items[I].chMDFe;
        FManifestos.Items[J].MDFe.procMDFe.dhRecbto := AInfProt.Items[I].dhRecbto;
        FManifestos.Items[J].MDFe.procMDFe.nProt := AInfProt.Items[I].nProt;
        FManifestos.Items[J].MDFe.procMDFe.digVal := AInfProt.Items[I].digVal;
        FManifestos.Items[J].MDFe.procMDFe.cStat := AInfProt.Items[I].cStat;
        FManifestos.Items[J].MDFe.procMDFe.xMotivo := AInfProt.Items[I].xMotivo;

        if FPConfiguracoesMDFe.Arquivos.Salvar or NaoEstaVazio(
          FManifestos.Items[J].NomeArq) then
        begin
          if FileExists(PathWithDelim(FPConfiguracoesMDFe.Arquivos.PathSalvar) +
                        AInfProt.Items[I].chMDFe + '-mdfe.xml') and
             FileExists(PathWithDelim(FPConfiguracoesMDFe.Arquivos.PathSalvar) +
                        FMDFeRetorno.nRec + '-pro-rec.xml') then
          begin
            AProcMDFe := TProcMDFe.Create;
            try
              AProcMDFe.PathMDFe :=
                PathWithDelim(FPConfiguracoesMDFe.Arquivos.PathSalvar) +
                AInfProt.Items[I].chMDFe + '-mdfe.xml';
              AProcMDFe.PathRetConsReciMDFe :=
                PathWithDelim(FPConfiguracoesMDFe.Arquivos.PathSalvar) +
                FMDFeRetorno.nRec + '-pro-rec.xml';

              AProcMDFe.Versao := FPVersaoServico;
              AProcMDFe.GerarXML;

              if NaoEstaVazio(AProcMDFe.Gerador.ArquivoFormatoXML) then
              begin
                if NaoEstaVazio(FManifestos.Items[J].NomeArq) then
                  AProcMDFe.Gerador.SalvarArquivo(FManifestos.Items[J].NomeArq)
                else
                  AProcMDFe.Gerador.SalvarArquivo(
                    PathWithDelim(FPConfiguracoesMDFe.Arquivos.PathSalvar) +
                    AInfProt.Items[I].chMDFe + '-mdfe.xml');
              end;
            finally
              AProcMDFe.Free;
            end;
          end;
        end;

        if FPConfiguracoesMDFe.Arquivos.Salvar then
        begin
          if FPConfiguracoesMDFe.Arquivos.SalvarApenasMDFeProcessados then
          begin
            if FManifestos.Items[J].Processado then
              FManifestos.Items[J].GravarXML;
          end
          else
            FManifestos.Items[J].GravarXML;
        end;

        break;
      end;
    end;
  end;

  //Verificando se existe algum Manifesto confirmado
  for I := 0 to FManifestos.Count - 1 do
  begin
    if FManifestos.Items[I].Confirmado then
    begin
      Result := True;
      break;
    end;
  end;

  //Verificando se existe algum Manifesto nao confirmado
  for I := 0 to FManifestos.Count - 1 do
  begin
    if not FManifestos.Items[I].Confirmado then
    begin
      FPMsg := ACBrStr('Manifesto(s) não confirmado(s):') + LineBreak;
      break;
    end;
  end;

  //Montando a mensagem de retorno para os Manifestos nao confirmados
  for I := 0 to FManifestos.Count - 1 do
  begin
    if not FManifestos.Items[I].Confirmado then
      FPMsg := FPMsg + IntToStr(FManifestos.Items[I].MDFe.Ide.nMDF) +
        '->' + FManifestos.Items[I].Msg + LineBreak;
  end;

  if AInfProt.Count > 0 then
  begin
    FChaveMDFe := AInfProt.Items[0].chMDFe;
    FProtocolo := AInfProt.Items[0].nProt;
    FcStat := AInfProt.Items[0].cStat;
  end;
end;

function TMDFeRetRecepcao.Executar: Boolean;
var
  IntervaloTentativas, Tentativas: integer;
begin
  Result := False;

  TACBrMDFe(FPDFeOwner).SetStatus(stMDFeRetRecepcao);
  try
    Sleep(FPConfiguracoesMDFe.WebServices.AguardarConsultaRet);

    Tentativas := 0; // Inicializa o contador de tentativas
    IntervaloTentativas := max(FPConfiguracoesMDFe.WebServices.IntervaloTentativas, 1000);

    while (inherited Executar) and
      (Tentativas < FPConfiguracoesMDFe.WebServices.Tentativas) do
    begin
      Inc(Tentativas);
      sleep(IntervaloTentativas);
    end;
  finally
    TACBrMDFe(FPDFeOwner).SetStatus(stMDFeIdle);
  end;

  if FMDFeRetorno.CStat = 104 then  // Lote processado ?
    Result := TratarRespostaFinal;
end;

procedure TMDFeRetRecepcao.DefinirURL;
begin
  FPLayout := LayMDFeRetRecepcao;

  inherited DefinirURL;
end;

procedure TMDFeRetRecepcao.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'MDFeRetRecepcao';
  FPSoapAction := FPServico + '/mdfeRetRecepcao';
end;

procedure TMDFeRetRecepcao.DefinirDadosMsg;
var
  ConsReciMDFe: TConsReciMDFe;
begin
  ConsReciMDFe := TConsReciMDFe.Create;
  try
    ConsReciMDFe.tpAmb := FPConfiguracoesMDFe.WebServices.Ambiente;
    ConsReciMDFe.nRec := FRecibo;
    ConsReciMDFe.Versao := FPVersaoServico;
    ConsReciMDFe.GerarXML;

    FPDadosMsg := ConsReciMDFe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciMDFe.Free;
  end;
end;

function TMDFeRetRecepcao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'mdfeRetRecepcaoResult');

  // Limpando variaveis internas
  FMDFeRetorno.Free;
  FMDFeRetorno := TRetConsReciMDFe.Create;

  FMDFeRetorno.Leitor.Arquivo := FPRetWS;
  FMDFeRetorno.LerXML;

  Fversao := FMDFeRetorno.versao;
  FTpAmb := FMDFeRetorno.TpAmb;
  FverAplic := FMDFeRetorno.verAplic;
  FcStat := FMDFeRetorno.cStat;
  FcUF := FMDFeRetorno.cUF;
  FPMsg := FMDFeRetorno.xMotivo;
  FxMotivo := FMDFeRetorno.xMotivo;
  FcMsg := FMDFeRetorno.cMsg;
  FxMsg := FMDFeRetorno.xMsg;

  Result := (FMDFeRetorno.CStat = 105); // Lote em Processamento
end;

procedure TMDFeRetRecepcao.FinalizarServico;
begin
  // Sobrescrito, para não liberar para stIdle... não ainda...;
end;

function TMDFeRetRecepcao.GerarMsgLog: String;
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
                   [FMDFeRetorno.versao, TpAmbToStr(FMDFeRetorno.tpAmb),
                    FMDFeRetorno.verAplic, FMDFeRetorno.nRec,
                    IntToStr(FMDFeRetorno.cStat), FMDFeRetorno.xMotivo,
                    CodigoParaUF(FMDFeRetorno.cUF), IntToStr(FMDFeRetorno.cMsg),
                    FMDFeRetorno.xMsg]);
end;

function TMDFeRetRecepcao.GerarPrefixoArquivo: String;
begin
  Result := Recibo;
end;

{ TMDFeRecibo }

constructor TMDFeRecibo.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FMDFeRetorno := TRetConsReciMDFe.Create;

  FPStatus := stMDFeRecibo;
  FPLayout := LayMDFeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';
end;

destructor TMDFeRecibo.Destroy;
begin
  FMDFeRetorno.Free;

  inherited Destroy;
end;

procedure TMDFeRecibo.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'MDFeRetRecepcao';
  FPSoapAction := FPServico + '/mdfeRetRecepcao';
end;

procedure TMDFeRecibo.DefinirURL;
begin
  FPLayout := LayMDFeRetRecepcao;

  inherited DefinirURL;
end;

procedure TMDFeRecibo.DefinirDadosMsg;
var
  ConsReciMDFe: TConsReciMDFe;
begin
  ConsReciMDFe := TConsReciMDFe.Create;
  try
    ConsReciMDFe.tpAmb := FPConfiguracoesMDFe.WebServices.Ambiente;
    ConsReciMDFe.nRec := FRecibo;
    ConsReciMDFe.Versao := FPVersaoServico;
    ConsReciMDFe.GerarXML;

    FPDadosMsg := ConsReciMDFe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciMDFe.Free;
  end;
end;

function TMDFeRecibo.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'mdfeRetRecepcaoResult');

  // Limpando variaveis internas
  FMDFeRetorno.Free;
  FMDFeRetorno := TRetConsReciMDFe.Create;

  FMDFeRetorno.Leitor.Arquivo := FPRetWS;
  FMDFeRetorno.LerXML;

  Fversao := FMDFeRetorno.versao;
  FTpAmb := FMDFeRetorno.TpAmb;
  FverAplic := FMDFeRetorno.verAplic;
  FcStat := FMDFeRetorno.cStat;
  FxMotivo := FMDFeRetorno.xMotivo;
  FcUF := FMDFeRetorno.cUF;
  FxMsg := FMDFeRetorno.xMsg;
  FcMsg := FMDFeRetorno.cMsg;
  FPMsg := FxMotivo;

  Result := (FMDFeRetorno.CStat = 104);
end;

function TMDFeRecibo.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FMDFeRetorno.versao, TpAmbToStr(FMDFeRetorno.TpAmb),
                   FMDFeRetorno.verAplic, FMDFeRetorno.nRec,
                   IntToStr(FMDFeRetorno.cStat),
                   FMDFeRetorno.ProtMDFe.Items[0].xMotivo,
                   CodigoParaUF(FMDFeRetorno.cUF)]);
end;

{ TMDFeConsulta }

constructor TMDFeConsulta.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FprotMDFe := TProcMDFe.Create;
  FprocEventoMDFe := TRetEventoMDFeCollection.Create(AOwner);

  FPStatus := stMDFeConsulta;
  FPLayout := LayMDFeConsulta;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';
end;

destructor TMDFeConsulta.Destroy;
begin
  FprotMDFe.Free;
  if Assigned(FprocEventoMDFe) then
    FprocEventoMDFe.Free;

  inherited Destroy;
end;

procedure TMDFeConsulta.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'MDFeConsulta';
  FPSoapAction := FPServico + '/mdfeConsultaMDF';
end;

procedure TMDFeConsulta.DefinirDadosMsg;
var
  ConsSitMDFe: TConsSitMDFe;
  OK: Boolean;
begin
  OK := False;
  ConsSitMDFe := TConsSitMDFe.Create;
  try
    ConsSitMDFe.TpAmb := FPConfiguracoesMDFe.WebServices.Ambiente;
    ConsSitMDFe.chMDFe := FMDFeChave;

    ConsSitMDFe.Versao := FPVersaoServico;
    ConsSitMDFe.GerarXML;

    FPDadosMsg := ConsSitMDFe.Gerador.ArquivoFormatoXML;
  finally
    ConsSitMDFe.Free;
  end;
end;

function TMDFeConsulta.TratarResposta: Boolean;
var
  MDFeRetorno: TRetConsSitMDFe;
  MDFCancelado, Atualiza: Boolean;
  aEventos, aMsg, NomeArquivo, aMDFe, aMDFeDFe: String;
  AProcMDFe: TProcMDFe;
  I, J, K, Inicio, Fim: Integer;
  LocMDFeW: TMDFeW;
begin
  MDFeRetorno := TRetConsSitMDFe.Create;

  try
    FPRetWS := SeparaDados(FPRetornoWS, 'mdfeConsultaMDFResult');

    MDFeRetorno.Leitor.Arquivo := FPRetWS;
    MDFeRetorno.LerXML;

    MDFCancelado := False;
    aEventos := '';

    // <retConsSitMDFe> - Retorno da consulta da situação da NF-e
    // Este é o status oficial da NF-e
    Fversao := MDFeRetorno.versao;
    FTpAmb := MDFeRetorno.tpAmb;
    FverAplic := MDFeRetorno.verAplic;
    FcStat := MDFeRetorno.cStat;
    FXMotivo := MDFeRetorno.xMotivo;
    FcUF := MDFeRetorno.cUF;
    FMDFeChave := MDFeRetorno.chMDFe;
    FPMsg := FXMotivo;

    // <protMDFe> - Retorno dos dados do ENVIO da NF-e
    // Considerá-los apenas se não existir nenhum evento de cancelamento (110111)
    FprotMDFe.PathMDFe := MDFeRetorno.protMDFe.PathMDFe;
    FprotMDFe.PathRetConsReciMDFe := MDFeRetorno.protMDFe.PathRetConsReciMDFe;
    FprotMDFe.PathRetConsSitMDFe := MDFeRetorno.protMDFe.PathRetConsSitMDFe;
    FprotMDFe.PathRetConsSitMDFe := MDFeRetorno.protMDFe.PathRetConsSitMDFe;
    FprotMDFe.tpAmb := MDFeRetorno.protMDFe.tpAmb;
    FprotMDFe.verAplic := MDFeRetorno.protMDFe.verAplic;
    FprotMDFe.chMDFe := MDFeRetorno.protMDFe.chMDFe;
    FprotMDFe.dhRecbto := MDFeRetorno.protMDFe.dhRecbto;
    FprotMDFe.nProt := MDFeRetorno.protMDFe.nProt;
    FprotMDFe.digVal := MDFeRetorno.protMDFe.digVal;
    FprotMDFe.cStat := MDFeRetorno.protMDFe.cStat;
    FprotMDFe.xMotivo := MDFeRetorno.protMDFe.xMotivo;

    if Assigned(MDFeRetorno.procEventoMDFe) and (MDFeRetorno.procEventoMDFe.Count > 0) then
    begin
      aEventos := '=====================================================' +
        LineBreak + '================== Eventos da MDF-e ==================' +
        LineBreak + '=====================================================' +
        LineBreak + '' + LineBreak + 'Quantidade total de eventos: ' +
        IntToStr(MDFeRetorno.procEventoMDFe.Count);

      FprocEventoMDFe.Clear;
      for I := 0 to MDFeRetorno.procEventoMDFe.Count - 1 do
      begin
        with FprocEventoMDFe.Add.RetEventoMDFe do
        begin
          idLote := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.idLote;
          tpAmb := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.tpAmb;
          verAplic := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.verAplic;
          cOrgao := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.cOrgao;
          cStat := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.cStat;
          xMotivo := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.xMotivo;
          XML := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.XML;

          Infevento.ID              := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.ID;
          Infevento.tpAmb           := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.tpAmb;
          InfEvento.CNPJ            := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.CNPJ;
          InfEvento.chMDFe          := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.chMDFe;
          InfEvento.dhEvento        := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.dhEvento;
          InfEvento.TpEvento        := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.TpEvento;
          InfEvento.nSeqEvento      := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.nSeqEvento;
          InfEvento.VersaoEvento    := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.VersaoEvento;
          InfEvento.DetEvento.nProt := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.DetEvento.nProt;
          InfEvento.DetEvento.xJust := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.DetEvento.xJust;
          InfEvento.DetEvento.xNome := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.DetEvento.xNome;
          InfEvento.DetEvento.CPF   := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.InfEvento.DetEvento.CPF;

          retEvento.Clear;
          for J := 0 to MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Count-1 do
          begin
            with retEvento.Add.RetInfEvento do
            begin
              Id          := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.Id;
              tpAmb       := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.tpAmb;
              verAplic    := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.verAplic;
              cOrgao      := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.cOrgao;
              cStat       := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.cStat;
              xMotivo     := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.xMotivo;
              chMDFe      := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.chMDFe;
              tpEvento    := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.tpEvento;
              xEvento     := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.xEvento;
              nSeqEvento  := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.nSeqEvento;
              CNPJDest    := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.CNPJDest;
              emailDest   := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.emailDest;
              dhRegEvento := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.dhRegEvento;
              nProt       := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.nProt;
              XML         := MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe.retEvento.Items[j].RetInfEvento.XML;
            end;
          end;
        end;

        with MDFeRetorno.procEventoMDFe.Items[I].RetEventoMDFe do
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
        end;
      end;
    end;

    if not MDFCancelado then
    begin
      FProtocolo := MDFeRetorno.protMDFe.nProt;
      FDhRecbto := MDFeRetorno.protMDFe.dhRecbto;
      FPMsg := MDFeRetorno.protMDFe.xMotivo;
    end;

    //TODO: Verificar porque monta "aMsg", pois ela não está sendo usada em lugar nenhum
    aMsg := GerarMsgLog;
    if aEventos <> '' then
      aMsg := aMsg + sLineBreak + aEventos;

    Result := (MDFeRetorno.CStat in [100, 101, 110, 150, 151, 155]);

    NomeArquivo := PathWithDelim(FPConfiguracoesMDFe.Arquivos.PathSalvar) + FMDFeChave;

    for i := 0 to TACBrMDFe(FPDFeOwner).Manifestos.Count - 1 do
    begin
      with TACBrMDFe(FPDFeOwner).Manifestos.Items[i] do
      begin
        if (OnlyNumber(FMDFeChave) = NumID) then
        begin
          Atualiza := True;
//          if ((MDFeRetorno.CStat in [101, 151, 155]) and
//            (not FPConfiguracoesMDFe.Geral.AtualizarXMLCancelado)) then
          if (MDFeRetorno.CStat in [101, 151, 155]) then
            Atualiza := False;

          // Atualiza o Status da MDFe interna //
          MDFe.procMDFe.cStat := MDFeRetorno.cStat;

          if Atualiza then
          begin
            if (FPConfiguracoesMDFe.Geral.ValidarDigest) and
              (MDFeRetorno.protMDFe.digVal <> '') and
              (MDFe.signature.DigestValue <> MDFeRetorno.protMDFe.digVal) then
            begin
              raise EACBrMDFeException.Create('DigestValue do documento ' +
                NumID + ' não confere.');
            end;

            MDFe.procMDFe.tpAmb := MDFeRetorno.tpAmb;
            MDFe.procMDFe.verAplic := MDFeRetorno.verAplic;
            MDFe.procMDFe.chMDFe := MDFeRetorno.chMDFe;
            MDFe.procMDFe.dhRecbto := FDhRecbto;
            MDFe.procMDFe.nProt := FProtocolo;
            MDFe.procMDFe.digVal := MDFeRetorno.protMDFe.digVal;
            MDFe.procMDFe.cStat := MDFeRetorno.cStat;
            MDFe.procMDFe.xMotivo := MDFeRetorno.xMotivo;

            if FileExists(NomeArquivo + '-mdfe.xml') or NaoEstaVazio(NomeArq) then
            begin
              AProcMDFe := TProcMDFe.Create;
              try
                if NaoEstaVazio(NomeArq) then
                  AProcMDFe.PathMDFe := NomeArq
                else
                  AProcMDFe.PathMDFe := NomeArquivo + '-mdfe.xml';

                AProcMDFe.PathRetConsSitMDFe := NomeArquivo + '-sit.xml';

                AProcMDFe.Versao :=
                    TACBrMDFe(FPDFeOwner).LerVersaoDeParams(LayMDFeRecepcao);

                AProcMDFe.GerarXML;

                aMDFe := AProcMDFe.Gerador.ArquivoFormatoXML;

                if NaoEstaVazio(AProcMDFe.Gerador.ArquivoFormatoXML) then
                  AProcMDFe.Gerador.SalvarArquivo(AProcMDFe.PathMDFe);

                FRetMDFeDFe := '';

                if (NaoEstaVazio(aMDFe)) and
                   (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoMDFe'))) then
                begin
                  Inicio := Pos('<procEventoMDFe', FPRetWS);
                  Fim    := Pos('</retConsSitMDFe', FPRetWS) - 1;

                  aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

                  aMDFeDFe := '<?xml version="1.0" encoding="UTF-8" ?>' +
                              '<MDFeDFe>' +
                               '<procMDFe versao="' + MDFeenviMDFe + '">' +
                                 SeparaDados(aMDFe, 'MDFeProc') +
                               '</procMDFe>' +
                               '<procEventoMDFe versao="' + MDFeEventoMDFe + '">' +
                                 aEventos +
                               '</procEventoMDFe>' +
                              '</MDFeDFe>';

                  FRetMDFeDFe := aMDFeDFe;
                end;

              finally
                AProcMDFe.Free;
              end;
            end
            else begin
             LocMDFeW := TMDFeW.Create(TACBrMDFe(FPDFeOwner).Manifestos.Items[i].MDFe);
             try
               LocMDFeW.GerarXML;

               aMDFe := LocMDFeW.Gerador.ArquivoFormatoXML;

               FRetMDFeDFe := '';

               if (NaoEstaVazio(aMDFe)) and
                  (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoMDFe'))) then
                begin
                  Inicio := Pos('<procEventoMDFe', FPRetWS);
                  Fim    := Pos('</retConsSitMDFe', FPRetWS) -1;

                  aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

                  aMDFeDFe := '<?xml version="1.0" encoding="UTF-8" ?>' +
                              '<MDFeDFe>' +
                               '<procMDFe versao="' + MDFeenviMDFe + '">' +
                                 SeparaDados(aMDFe, 'MDFeProc') +
                               '</procMDFe>' +
                               '<procEventoMDFe versao="' + MDFeEventoMDFe + '">' +
                                 aEventos +
                               '</procEventoMDFe>' +
                              '</MDFeDFe>';

                  FRetMDFeDFe := aMDFeDFe;
                end;
             finally
               LocMDFeW.Free;
             end;
            end;

            if FPConfiguracoesMDFe.Arquivos.Salvar then
            begin
              if FPConfiguracoesMDFe.Arquivos.SalvarApenasMDFeProcessados then
              begin
                if Processado then
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

    if (TACBrMDFe(FPDFeOwner).Manifestos.Count <= 0) then
    begin
      if FPConfiguracoesMDFe.Arquivos.Salvar then
      begin
        if FileExists(NomeArquivo + '-mdfe.xml') then
        begin
          AProcMDFe := TProcMDFe.Create;
          try
            AProcMDFe.PathMDFe := NomeArquivo + '-mdfe.xml';
            AProcMDFe.PathRetConsSitMDFe := NomeArquivo + '-sit.xml';

            AProcMDFe.Versao := TACBrMDFe(FPDFeOwner).LerVersaoDeParams(LayMDFeRecepcao);

            AProcMDFe.GerarXML;

            aMDFe := AProcMDFe.Gerador.ArquivoFormatoXML;

            if NaoEstaVazio(AProcMDFe.Gerador.ArquivoFormatoXML) then
              AProcMDFe.Gerador.SalvarArquivo(AProcMDFe.PathMDFe);

            if (NaoEstaVazio(aMDFe)) and
               (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoMDFe'))) then
            begin
              Inicio := Pos('<procEventoMDFe', FPRetWS);
              Fim    := Pos('</retConsSitMDFe', FPRetWS) -1;

              aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

              aMDFeDFe := '<?xml version="1.0" encoding="UTF-8" ?>' +
                          '<MDFeDFe>' +
                           '<procMDFe versao="' + MDFeenviMDFe + '">' +
                             SeparaDados(aMDFe, 'MDFeProc') +
                           '</procMDFe>' +
                           '<procEventoMDFe versao="' + MDFeEventoMDFe + '">' +
                             aEventos +
                           '</procEventoMDFe>' +
                          '</MDFeDFe>';

              FRetMDFeDFe := aMDFeDFe;
            end;

          finally
            AProcMDFe.Free;
          end;
        end;
      end;
    end;
  finally
    MDFeRetorno.Free;
  end;
end;

function TMDFeConsulta.GerarMsgLog: String;
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
                   [Fversao, FMDFeChave, TpAmbToStr(FTpAmb), FverAplic,
                    IntToStr(FcStat), FXMotivo, CodigoParaUF(FcUF), FMDFeChave,
                    FormatDateTimeBr(FDhRecbto), FProtocolo, FprotMDFe.digVal]);
end;

function TMDFeConsulta.GerarPrefixoArquivo: String;
begin
  Result := Trim(FMDFeChave);
end;

{ TMDFeEnvEvento }

constructor TMDFeEnvEvento.Create(AOwner: TACBrDFe; AEvento: TEventoMDFe);
begin
  inherited Create(AOwner);

  FEventoRetorno := TRetEventoMDFe.Create;
  FEvento := AEvento;

  FPStatus := stMDFeEvento;
  FPLayout := LayMDFeEvento;
  FPArqEnv := 'ped-eve';
  FPArqResp := 'eve';
end;

destructor TMDFeEnvEvento.Destroy;
begin
  FEventoRetorno.Free;

  inherited;
end;

function TMDFeEnvEvento.GerarPathEvento: String;
begin
  with FEvento.Evento.Items[0].InfEvento do
  begin
    Result := FPConfiguracoesMDFe.Arquivos.GetPathEvento(tpEvento);
  end;
end;

procedure TMDFeEnvEvento.DefinirURL;
begin
  FPLayout := LayMDFeEvento;

  inherited DefinirURL;
end;

procedure TMDFeEnvEvento.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'MDFeRecepcaoEvento';
  FPSoapAction := FPServico + '/mdfeRecepcaoEvento';
end;

procedure TMDFeEnvEvento.DefinirDadosMsg;
var
  EventoMDFe: TEventoMDFe;
  I, F: integer;
  Lote, Evento, Eventos, EventosAssinados: String;
begin
  EventoMDFe := TEventoMDFe.Create;
  try
    EventoMDFe.idLote := FidLote;

    for I := 0 to TMDFeEnvEvento(Self).FEvento.Evento.Count - 1 do
    begin
      with EventoMDFe.Evento.Add do
      begin
        infEvento.tpAmb      := FPConfiguracoes.WebServices.Ambiente;
        infEvento.CNPJ       := FEvento.Evento[i].InfEvento.CNPJ;
        infEvento.chMDFe     := FEvento.Evento[i].InfEvento.chMDFe;
        infEvento.dhEvento   := FEvento.Evento[i].InfEvento.dhEvento;
        infEvento.tpEvento   := FEvento.Evento[i].InfEvento.tpEvento;
        infEvento.nSeqEvento := FEvento.Evento[i].InfEvento.nSeqEvento;

        case InfEvento.tpEvento of
          teCancelamento:
          begin
            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;
            infEvento.detEvento.xJust := FEvento.Evento[i].InfEvento.detEvento.xJust;
          end;
          teEncerramento:
          begin
            infEvento.detEvento.nProt := FEvento.Evento[i].InfEvento.detEvento.nProt;
            infEvento.detEvento.dtEnc := FEvento.Evento[i].InfEvento.detEvento.dtEnc;
            infEvento.detEvento.cUF   := FEvento.Evento[i].InfEvento.detEvento.cUF;
            infEvento.detEvento.cMun  := FEvento.Evento[i].InfEvento.detEvento.cMun;
          end;
          teInclusaoCondutor:
          begin
            infEvento.detEvento.xNome := FEvento.Evento[i].InfEvento.detEvento.xNome;
            infEvento.detEvento.CPF   := FEvento.Evento[i].InfEvento.detEvento.CPF;
          end;
        end;
      end;
    end;

    EventoMDFe.Versao := FPVersaoServico;
    EventoMDFe.GerarXML;

    // Separa os grupos <evento> e coloca na variável Eventos
    I := Pos('<evento ', EventoMDFe.Gerador.ArquivoFormatoXML);
    Lote := Copy(EventoMDFe.Gerador.ArquivoFormatoXML, 1, I - 1);
    Eventos := SeparaDados(EventoMDFe.Gerador.ArquivoFormatoXML, 'envEvento');
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

        AssinarXML(Evento, 'evento', 'infEvento', 'Falha ao assinar o Envio de Evento ');

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

    with TACBrMDFe(FPDFeOwner) do
    begin
      SSL.Validar(FPDadosMsg, GerarNomeArqSchema(FPLayout, FPVersaoServico), FPMsg);
    end;

    for I := 0 to FEvento.Evento.Count - 1 do
      FEvento.Evento[I].InfEvento.id := EventoMDFe.Evento[I].InfEvento.id;
  finally
    EventoMDFe.Free;
  end;
end;

function TMDFeEnvEvento.TratarResposta: Boolean;
var
  Leitor: TLeitor;
  I, J: integer;
  wProc: TStringList;
  NomeArq, VersaoEvento: String;
begin
  FEvento.idLote := idLote;

  FPRetWS := SeparaDados(FPRetornoWS, 'mdfeRecepcaoEventoResult');

  // Limpando variaveis internas
  FEventoRetorno.Free;
  FEventoRetorno := TRetEventoMDFe.Create;

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
          if FEvento.Evento.Items[I].InfEvento.chMDFe =
            EventoRetorno.retEvento.Items[J].RetInfEvento.chMDFe then
          begin
            FEvento.Evento.Items[I].RetInfEvento.nProt :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.nProt;
            FEvento.Evento.Items[I].RetInfEvento.dhRegEvento :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.dhRegEvento;
            FEvento.Evento.Items[I].RetInfEvento.cStat :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.cStat;
            FEvento.Evento.Items[I].RetInfEvento.xMotivo :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.xMotivo;

            wProc := TStringList.Create;
            try
              VersaoEvento := TACBrMDFe(FPDFeOwner).LerVersaoDeParams(LayMDFeEvento);

              wProc.Add('<' + ENCODING_UTF8 + '>');
              wProc.Add('<procEventoMDFe versao="' + VersaoEvento +
                '" xmlns="http://www.portalfiscal.inf.br/mdfe">');
              wProc.Add('<eventoMDFe versao="' + VersaoEvento + '">');
              Leitor.Arquivo := FPDadosMsg;
              wProc.Add(Leitor.rExtrai(1, 'infEvento', '', I + 1));
              wProc.Add('<Signature xmlns="http://www.w3.org/2000/09/xmldsig#">');

              Leitor.Arquivo := FPDadosMsg;
              wProc.Add(Leitor.rExtrai(1, 'SignedInfo', '', I + 1));

              Leitor.Arquivo := FPDadosMsg;
              wProc.Add(Leitor.rExtrai(1, 'SignatureValue', '', I + 1));

              Leitor.Arquivo := FPDadosMsg;
              wProc.Add(Leitor.rExtrai(1, 'KeyInfo', '', I + 1));
              wProc.Add('</Signature>');
              wProc.Add('</eventoMDFe>');
              wProc.Add('<retEventoMDFe versao="' + VersaoEvento + '">');

              Leitor.Arquivo := FPRetWS;
              wProc.Add(Leitor.rExtrai(1, 'infEvento', '', J + 1));
              wProc.Add('</retEventoMDFe>');
              wProc.Add('</procEventoMDFe>');

              EventoRetorno.retEvento.Items[J].RetInfEvento.XML := wProc.Text;

              FEvento.Evento.Items[I].RetInfEvento.XML := wProc.Text;

              NomeArq := OnlyNumber(FEvento.Evento.Items[i].InfEvento.Id) +
                '-procEventoMDFe.xml';

              if FPConfiguracoesMDFe.Geral.Salvar then
                FPDFeOwner.Gravar(NomeArq, wProc.Text);

              if FPConfiguracoesMDFe.Arquivos.Salvar then
                FPDFeOwner.Gravar(NomeArq, wProc.Text, GerarPathEvento);

              if FPConfiguracoesMDFe.Arquivos.SalvarEvento then
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

procedure TMDFeEnvEvento.SalvarEnvio;
begin
  inherited SalvarEnvio;

  if FPConfiguracoesMDFe.Arquivos.Salvar then
    FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqEnv + '.xml',
      FPDadosMsg, GerarPathEvento);
end;

procedure TMDFeEnvEvento.SalvarResposta;
begin
  inherited SalvarResposta;

  if FPConfiguracoesMDFe.Arquivos.Salvar then;
  FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqEnv + '.xml',
    FPDadosMsg, GerarPathEvento);
end;

function TMDFeEnvEvento.GerarMsgLog: String;
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
       [IfThen(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento = 0, '',
               FormatDateTimeBr(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento))]);

  Result := aMsg;
end;

function TMDFeEnvEvento.GerarPrefixoArquivo: String;
begin
  Result := IntToStr(FEvento.idLote);
end;

{ TMDFeConsultaMDFeNaoEnc }

constructor TMDFeConsultaMDFeNaoEnc.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FInfMDFe := TRetInfMDFeCollection.Create(AOwner);

  FPStatus  := stMDFeConsulta;
  FPLayout  := LayMDFeConsNaoEnc;
  FPArqEnv  := 'ped-cons';
  FPArqResp := 'cons';
end;

destructor TMDFeConsultaMDFeNaoEnc.Destroy;
begin
  FinfMDFe.Free;

  inherited;
end;

procedure TMDFeConsultaMDFeNaoEnc.DefinirServicoEAction;
begin
  FPServico    := GetUrlWsd + 'MDFeConsNaoEnc';
  FPSoapAction := FPServico + '/mdfeConsNaoEnc';
end;

procedure TMDFeConsultaMDFeNaoEnc.DefinirURL;
begin
  FPLayout := LayMDFeConsNaoEnc;

  inherited DefinirURL;
end;

procedure TMDFeConsultaMDFeNaoEnc.DefinirDadosMsg;
var
  ConsMDFeNaoEnc: TConsMDFeNaoEnc;
begin
  ConsMDFeNaoEnc := TConsMDFeNaoEnc.create;
  try
    ConsMDFeNaoEnc.TpAmb := FPConfiguracoesMDFe.WebServices.Ambiente;
    ConsMDFeNaoEnc.CNPJ  := FCNPJ; // TMDFeConsultaMDFeNaoEnc(Self).CNPJ;

//    ConsMDFeNaoEnc.Gerador.Opcoes.RetirarAcentos := FConfiguracoes.Geral.RetirarAcentos;

    ConsMDFeNaoEnc.GerarXML;

    FPDadosMsg := ConsMDFeNaoEnc.Gerador.ArquivoFormatoXML;
  finally
    ConsMDFeNaoEnc.Free;
  end;
end;

function TMDFeConsultaMDFeNaoEnc.TratarResposta: Boolean;
var
  i: Integer;
begin
//  FRetWS := SeparaDados(FRetornoWS, 'mdfeConsNaoEncMDFResult');
  FPRetWS := SeparaDados(FPRetornoWS, 'mdfeConsNaoEncResult');

  // Limpando variaveis internas
  FRetConsMDFeNaoEnc.Free;
  FRetConsMDFeNaoEnc := TRetConsMDFeNaoEnc.Create;

  FRetConsMDFeNaoEnc.Leitor.Arquivo := FPRetWS;
  FRetConsMDFeNaoEnc.LerXml;

  Fversao    := FRetConsMDFeNaoEnc.versao;
  FtpAmb     := FRetConsMDFeNaoEnc.tpAmb;
  FverAplic  := FRetConsMDFeNaoEnc.verAplic;
  FcStat     := FRetConsMDFeNaoEnc.cStat;
  FxMotivo   := FRetConsMDFeNaoEnc.xMotivo;
  FcUF       := FRetConsMDFeNaoEnc.cUF;
  FMsg       := FxMotivo;

  for i := 0 to FRetConsMDFeNaoEnc.InfMDFe.Count -1 do
  begin
    FinfMDFe.Add;
    FinfMDFe.Items[i].chMDFe := FRetConsMDFeNaoEnc.InfMDFe.Items[i].chMDFe;
    FinfMDFe.Items[i].nProt  := FRetConsMDFeNaoEnc.InfMDFe.Items[i].nProt;
  end;

    // 111 = MDF-e não encerrados localizados
    // 112 = MDF-e não encerrados não localizados
  Result := (FcStat in [111, 112]);
end;

function TMDFeConsultaMDFeNaoEnc.GerarMsgLog: AnsiString;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FRetConsMDFeNaoEnc.versao, TpAmbToStr(FRetConsMDFeNaoEnc.tpAmb),
                    FRetConsMDFeNaoEnc.verAplic, IntToStr(FRetConsMDFeNaoEnc.cStat),
                    FRetConsMDFeNaoEnc.xMotivo,
                    FRetConsMDFeNaoEnc.cUF]);
end;

function TMDFeConsultaMDFeNaoEnc.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Consulta MDF-e nao Encerradas:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TMDFeEnvioWebService }

constructor TMDFeEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stMDFeEnvioWebService;
  FVersao := '';
end;

destructor TMDFeEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

function TMDFeEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TMDFeEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TMDFeEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TMDFeEnvioWebService.DefinirDadosMsg;
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

function TMDFeEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TMDFeEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TMDFeEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrMDFe := TACBrMDFe(AOwner);

  FStatusServico := TMDFeStatusServico.Create(FACBrMDFe);
  FEnviar := TMDFeRecepcao.Create(FACBrMDFe, TACBrMDFe(FACBrMDFe).Manifestos);
  FRetorno := TMDFeRetRecepcao.Create(FACBrMDFe, TACBrMDFe(FACBrMDFe).Manifestos);
  FRecibo := TMDFeRecibo.Create(FACBrMDFe);
  FConsulta := TMDFeConsulta.Create(FACBrMDFe);
  FEnvEvento := TMDFeEnvEvento.Create(FACBrMDFe, TACBrMDFe(FACBrMDFe).EventoMDFe);
  FConsMDFeNaoEnc := TMDFeConsultaMDFeNaoEnc.Create(FACBrMDFe);
  FEnvioWebService := TMDFeEnvioWebService.Create(FACBrMDFe);
end;

destructor TWebServices.Destroy;
begin
  FStatusServico.Free;
  FEnviar.Free;
  FRetorno.Free;
  FRecibo.Free;
  FConsulta.Free;
  FEnvEvento.Free;
  FConsMDFeNaoEnc.Free;
  FEnvioWebService.Free;

  inherited Destroy;
end;

function TWebServices.Envia(ALote: integer): Boolean;
begin
  Result := Envia(IntToStr(ALote));
end;

function TWebServices.Envia(ALote: String): Boolean;
begin
  FEnviar.FLote := ALote;

  if not Enviar.Executar then
    Enviar.GerarException( Enviar.Msg );

  FRetorno.Recibo := FEnviar.Recibo;
  if not FRetorno.Executar then
    FRetorno.GerarException( FRetorno.Msg );

  Result := True;
end;

function TWebServices.ConsultaMDFeNaoEnc(ACNPJ: String): Boolean;
begin
  FConsMDFeNaoEnc.FCNPJ := ACNPJ;

  if not FConsMDFeNaoEnc.Executar then
    FConsMDFeNaoEnc.GerarException( FConsMDFeNaoEnc.Msg );

  Result := True;
end;

end.
