{******************************************************************************}
{ Projeto: Componente ACBrCTe                                                  }
{  Biblioteca multiplataforma de componentes Delphi para emissão de Conhecimen-}
{ to de Transporte eletrônico - CTe - http://www.cte.fazenda.gov.br            }
{                                                                              }
{ Direitos Autorais Reservados (c) 2008 Wiliam Zacarias da Silva Rosa          }
{                                       Wemerson Souto                         }
{                                       Daniel Simoes de Almeida               }
{                                       André Ferreira de Moraes               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
{ Wiliam Zacarias da Silva Rosa  -  wrosa2009@yahoo.com.br -  www.motta.com.br }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrCTeWebServices;

interface

uses
  Classes, SysUtils,
  ACBrDFe, ACBrDFeWebService,
  pcteCTe,
  pcteRetConsReciCTe, pcteRetConsCad, pcnAuxiliar, pcnConversao, pcteConversaoCTe,
  pcteProcCte, pcteRetCancCTe, pcteEnvEventoCTe, pcteRetEnvEventoCTe,
  pcteRetConsSitCTe, pcteRetEnvCTe,
  ACBrCteConhecimentos, ACBrCTeConfiguracoes;

const
  CURL_WSDL = 'http://www.portalfiscal.inf.br/cte/wsdl/';
  INTERNET_OPTION_CLIENT_CERT_CONTEXT = 84;

type

  { TCTeWebService }

  TCTeWebService = class(TDFeWebService)
  private
  protected
    FPStatus: TStatusACBrCTe;
    FPLayout: TLayOut;
    FPConfiguracoesCTe: TConfiguracoesCTe;

    function ExtrairModeloChaveAcesso(AChaveCTE: String): String;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;

  public
    constructor Create(AOwner: TACBrDFe); override;

    property Status: TStatusACBrCTe read FPStatus;
    property Layout: TLayOut read FPLayout;
  end;

  { TCTeStatusServico }

  TCTeStatusServico = Class(TCTeWebService)
  private
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
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
    constructor Create(AOwner: TACBrDFe); override;

    property versao: String          read Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property verAplic: String        read FverAplic;
    property cStat: Integer          read FcStat;
    property xMotivo: String         read FxMotivo;
    property cUF: Integer            read FcUF;
    property dhRecbto: TDateTime     read FdhRecbto;
    property TMed: Integer           read FTMed;
    property dhRetorno: TDateTime    read FdhRetorno;
    property xObs: String            read FxObs;
  end;

  { TCTeRecepcao }

  TCTeRecepcao = class(TCTeWebService)
  private
    FLote: String;
    FRecibo: String;
    FConhecimentos: TConhecimentos;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;
    FxMotivo: String;
    FdhRecbto: TDateTime;
    FTMed: integer;

    FCTeRetorno: TretEnvCTe;

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
    constructor Create(AOwner: TACBrDFe; AConhecimentos: TConhecimentos);
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

  { TCTeRetRecepcao }

  TCTeRetRecepcao = class(TCTeWebService)
  private
    FRecibo: String;
    FProtocolo: String;
    FChaveCTe: String;
    FConhecimentos: TConhecimentos;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;
    FxMotivo: String;
    FcMsg: integer;
    FxMsg: String;

    FCTeRetorno: TRetConsReciCTe;

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
    constructor Create(AOwner: TACBrDFe; AConhecimentos: TConhecimentos);
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
    property ChaveCTe: String read FChaveCTe write FChaveCTe;

    property CTeRetorno: TRetConsReciCTe read FCTeRetorno;
  end;

  { TCTeRecibo }

  TCTeRecibo = class(TCTeWebService)
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

    FCTeRetorno: TRetConsReciCTe;
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

    property CTeRetorno: TRetConsReciCTe read FCTeRetorno;
  end;

  { TCTeConsulta }

  TCTeConsulta = class(TCTeWebService)
  private
    FCTeChave: String;
    FProtocolo: String;
    FDhRecbto: TDateTime;
    FXMotivo: String;
    Fversao: String;
    FTpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: integer;
    FcUF: integer;

    FprotCTe: TProcCTe;
    FretCancCTe: TRetCancCTe;
    FprocEventoCTe: TRetEventoCTeCollection;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
    function GerarPrefixoArquivo: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;

    property CTeChave: String read FCTeChave write FCTeChave;
    property Protocolo: String read FProtocolo write FProtocolo;
    property DhRecbto: TDateTime read FDhRecbto write FDhRecbto;
    property XMotivo: String read FXMotivo write FXMotivo;
    property versao: String read Fversao;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;
    property verAplic: String read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;

    property protCTe: TProcCTe read FprotCTe;
    property retCancCTe: TRetCancCTe read FretCancCTe;
    property procEventoCTe: TRetEventoCTeCollection read FprocEventoCTe;
  end;

  { TCTeInutilizacao }

  TCTeInutilizacao = class(TCTeWebService)
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

    FXML_ProcInutCTe: String;

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

    property XML_ProcInutCTe: String read FXML_ProcInutCTe write FXML_ProcInutCTe;
  end;

  { TCTeConsultaCadastro }

  TCTeConsultaCadastro = class(TCTeWebService)
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
//    procedure DefinirURL; override;
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

  { TCTeEnvEvento }

  TCTeEnvEvento = class(TCTeWebService)
  private
    FidLote: integer;
    Fversao: String;
    FEvento: TEventoCTe;
    FcStat: integer;
    FxMotivo: String;
    FTpAmb: TpcnTipoAmbiente;

    FEventoRetorno: TRetEventoCTe;

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
    constructor Create(AOwner: TACBrDFe; AEvento: TEventoCTe); reintroduce; overload;
    destructor Destroy; override;

    property idLote: integer read FidLote write FidLote;
    property versao: String read Fversao write Fversao;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property TpAmb: TpcnTipoAmbiente read FTpAmb;

    property EventoRetorno: TRetEventoCTe read FEventoRetorno;
  end;

  { TCTeEnvioWebService }

  TCTeEnvioWebService = class(TCTeWebService)
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
    FACBrCTe: TACBrDFe;
    FStatusServico: TCTeStatusServico;
    FEnviar: TCTeRecepcao;
    FRetorno: TCTeRetRecepcao;
    FRecibo: TCTeRecibo;
    FConsulta: TCTeConsulta;
    FInutilizacao: TCTeInutilizacao;
    FConsultaCadastro: TCTeConsultaCadastro;
    FEnvEvento: TCTeEnvEvento;
    FEnvioWebService: TCTeEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia(ALote: integer): Boolean; overload;
    function Envia(ALote: String): Boolean; overload;
    procedure Inutiliza(CNPJ, AJustificativa: String;
      Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer);

    property ACBrCTe: TACBrDFe read FACBrCTe write FACBrCTe;
    property StatusServico: TCTeStatusServico read FStatusServico write FStatusServico;
    property Enviar: TCTeRecepcao read FEnviar write FEnviar;
    property Retorno: TCTeRetRecepcao read FRetorno write FRetorno;
    property Recibo: TCTeRecibo read FRecibo write FRecibo;
    property Consulta: TCTeConsulta read FConsulta write FConsulta;
    property Inutilizacao: TCTeInutilizacao read FInutilizacao write FInutilizacao;
    property ConsultaCadastro: TCTeConsultaCadastro read FConsultaCadastro write FConsultaCadastro;
    property EnvEvento: TCTeEnvEvento read FEnvEvento write FEnvEvento;
    property EnvioWebService: TCTeEnvioWebService read FEnvioWebService write FEnvioWebService;
  end;

implementation

uses
  ACBrUtil, ACBrCTe,
  pcnGerador, pcteConsStatServ, pcteRetConsStatServ,
  pcteConsSitCTe, pcteInutCTe, pcteRetInutCTe, pcteConsReciCTe,
  pcteConsCad, pcnLeitor;

{ TCTeWebService }

constructor TCTeWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesCTe := TConfiguracoesCTe(FPConfiguracoes);
  FPLayout := LayCTeStatusServico;
  FPStatus := stIdle;
end;

function TCTeWebService.ExtrairModeloChaveAcesso(AChaveCTe: String): String;
begin
  AChaveCTe := OnlyNumber(AChaveCTe);
  if ValidarChave('CTe' + AChaveCTe) then
    Result := copy(AChaveCTe, 21, 2)
  else
    Result := '';
end;

procedure TCTeWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  TACBrCTe(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TCTeWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrCTe(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);

  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;


function TCTeWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrCTe(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TCTeWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  TACBrCTe(FPDFeOwner).SetStatus(stIdle);
end;

{ TCTeStatusServico }

constructor TCTeStatusServico.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stCTeStatusServico;
  FPLayout := LayCTeStatusServico;
  FPArqEnv := 'ped-sta';
  FPArqResp := 'sta';
end;

procedure TCTeStatusServico.DefinirServicoEAction;
begin
  FPServico    := GetUrlWsd + 'CteStatusServico';
  FPSoapAction := FFServico + '/cteStatusServicoCT';
end;

procedure TCTeStatusServico.DefinirDadosMsg;
var
  ConsStatServ: TConsStatServ;
begin
  ConsStatServ := TConsStatServ.Create;
  try
    ConsStatServ.TpAmb := FPConfiguracoesCTe.WebServices.Ambiente;
    ConsStatServ.CUF := FPConfiguracoesCTe.WebServices.UFCodigo;

    ConsStatServ.Versao := FPVersaoServico;
    ConsStatServ.GerarXML;

    // Atribuindo o XML para propriedade interna //
    FPDadosMsg := ConsStatServ.Gerador.ArquivoFormatoXML;
  finally
    ConsStatServ.Free;
  end;
end;

function TCTeStatusServico.TratarResposta: Boolean;
var
  CTeRetorno: TRetConsStatServ;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'cteStatusServicoCTResult');

  CTeRetorno := TRetConsStatServ.Create;
  try
    CTeRetorno.Leitor.Arquivo := FPRetWS;
    CTeRetorno.LerXml;

    Fversao := CTeRetorno.versao;
    FtpAmb := CTeRetorno.tpAmb;
    FverAplic := CTeRetorno.verAplic;
    FcStat := CTeRetorno.cStat;
    FxMotivo := CTeRetorno.xMotivo;
    FcUF := CTeRetorno.cUF;
    FdhRecbto := CTeRetorno.dhRecbto;
    FTMed := CTeRetorno.TMed;
    FdhRetorno := CTeRetorno.dhRetorno;
    FxObs := CTeRetorno.xObs;
    FPMsg := FxMotivo + LineBreak + FxObs;

    if FPConfiguracoesCTe.WebServices.AjustaAguardaConsultaRet then
      FPConfiguracoesCTe.WebServices.AguardarConsultaRet := FTMed * 1000;

    Result := (FcStat = 107);

  finally
    CTeRetorno.Free;
  end;
end;

function TCTeStatusServico.GerarMsgLog: String;
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

function TCTeStatusServico.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Consulta Status serviço:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TCTeRecepcao }

constructor TCTeRecepcao.Create(AOwner: TACBrDFe; AConhecimentos: TConhecimentos);
begin
  inherited Create(AOwner);

  FConhecimentos := AConhecimentos;

  FPStatus := stCTeRecepcao;
  FPLayout := LayCTeRecepcao;
  FPArqEnv := 'env-lot';
  FPArqResp := 'rec';

  FCTeRetorno := nil;
end;

destructor TCTeRecepcao.Destroy;
begin
  if Assigned(FCTeRetorno) then
    FCTeRetorno.Free;

  inherited Destroy;
end;

function TCTeRecepcao.GetLote: String;
begin
  Result := Trim(FLote);
end;

function TCTeRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

procedure TCTeRecepcao.DefinirURL;
begin
  FPLayout := LayCTeRecepcao;

  inherited DefinirURL;
end;

procedure TCTeRecepcao.DefinirServicoEAction;
begin
  FPServico    := GetUrlWsd + 'CteRecepcao';
  FPSoapAction := FPServico + '/cteRecepcaoLote';
end;

procedure TCTeRecepcao.DefinirDadosMsg;
var
  I: integer;
  vCTe: String;
begin
  vCTe := '';
  for I := 0 to FConhecimentos.Count - 1 do
    vCTe := vCTe + '<CTe' + RetornarConteudoEntre(
      FConhecimentos.Items[I].XMLAssinado, '<CTe', '</CTe>') + '</CTe>';

  FPDadosMsg := '<enviCTe xmlns="http://www.portalfiscal.inf.br/cte" versao="' +
    FPVersaoServico + '">' + '<idLote>' + FLote + '</idLote>' +
    vCTe + '</enviCTe>';

  // Lote tem mais de 500kb ? //
  if Length(FPDadosMsg) > (500 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 500 Kbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));

  FRecibo := '';
end;

function TCTeRecepcao.TratarResposta: Boolean;
var
  I: integer;
  chCTe, NomeArquivo: String;
  AProcCTe: TProcCTe;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'cteRecepcaoLoteResult');

  FCTeRetorno := TretEnvCTe.Create;

  FCTeRetorno.Leitor.Arquivo := FPRetWS;
  FCTeRetorno.LerXml;

  Fversao := FCTeRetorno.versao;
  FTpAmb := FCTeRetorno.TpAmb;
  FverAplic := FCTeRetorno.verAplic;
  FcStat := FCTeRetorno.cStat;
  FxMotivo := FCTeRetorno.xMotivo;
  FdhRecbto := FCTeRetorno.infRec.dhRecbto;
  FTMed := FCTeRetorno.infRec.tMed;
  FcUF := FCTeRetorno.cUF;
  FPMsg := FCTeRetorno.xMotivo;
  FRecibo := FCTeRetorno.infRec.nRec;

  Result := (FCTeRetorno.CStat = 103);
end;

procedure TCTeRecepcao.FinalizarServico;
begin
  inherited FinalizarServico;

  if Assigned(FCTeRetorno) then
    FreeAndNil(FCTeRetorno);
end;

function TCTeRecepcao.GerarMsgLog: String;
begin
  if Assigned(FCTeRetorno) then
    Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                             'Ambiente: %s ' + LineBreak +
                             'Versão Aplicativo: %s ' + LineBreak +
                             'Status Código: %s ' + LineBreak +
                             'Status Descrição: %s ' + LineBreak +
                             'UF: %s ' + sLineBreak +
                             'Recibo: %s ' + LineBreak +
                             'Recebimento: %s ' + LineBreak +
                             'Tempo Médio: %s ' + LineBreak),
                     [FCTeRetorno.versao,
                      TpAmbToStr(FCTeRetorno.TpAmb),
                      FCTeRetorno.verAplic,
                      IntToStr(FCTeRetorno.cStat),
                      FCTeRetorno.xMotivo,
                      CodigoParaUF(FCTeRetorno.cUF),
                      FCTeRetorno.infRec.nRec,
                      IfThen(FCTeRetorno.InfRec.dhRecbto = 0, '',
                             FormatDateTimeBr(FCTeRetorno.InfRec.dhRecbto)),
                      IntToStr(FCTeRetorno.InfRec.TMed)])
  else
    Result := '';
end;

function TCTeRecepcao.GerarPrefixoArquivo: String;
begin
  Result := Lote;
end;

{ TCTeRetRecepcao }

constructor TCTeRetRecepcao.Create(AOwner: TACBrDFe; AConhecimentos: TConhecimentos);
begin
  inherited Create(AOwner);

  FConhecimentos := AConhecimentos;
  FCTeRetorno := TRetConsReciCTe.Create;

  FPStatus := stCTeRetRecepcao;
  FPLayout := LayCTeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';
end;

destructor TCTeRetRecepcao.Destroy;
begin
  FCTeRetorno.Free;

  inherited Destroy;
end;

function TCTeRetRecepcao.GetRecibo: String;
begin
  Result := Trim(FRecibo);
end;

function TCTeRetRecepcao.TratarRespostaFinal: Boolean;
var
  I, J: integer;
  AProcCTe: TProcCTe;
  AInfProt: TProtCTeCollection;
  NomeXML: String;
begin
  Result := False;

  AInfProt := FCTeRetorno.ProtCTe;

  if (AInfProt.Count > 0) then
  begin
    FPMsg := FCTeRetorno.ProtCTe.Items[0].xMotivo;
    FxMotivo := FCTeRetorno.ProtCTe.Items[0].xMotivo;
  end;

  //Setando os retornos dos Conhecimentos;
  for I := 0 to AInfProt.Count - 1 do
  begin
    for J := 0 to FConhecimentos.Count - 1 do
    begin
      if OnlyNumber(AInfProt.Items[I].chCTe) = FConhecimentos.Items[J].NumID then
      begin
        if (TACBrCTe(FPDFeOwner).Configuracoes.Geral.ValidarDigest) and
          (FConhecimentos.Items[J].CTe.signature.DigestValue <>
          AInfProt.Items[I].digVal) and (AInfProt.Items[I].digVal <> '') then
        begin
          raise EACBrCTeException.Create('DigestValue do documento ' +
            FConhecimentos.Items[J].NumID + ' não confere.');
        end;

        FConhecimentos.Items[J].CTe.procCTe.tpAmb := AInfProt.Items[I].tpAmb;
        FConhecimentos.Items[J].CTe.procCTe.verAplic := AInfProt.Items[I].verAplic;
        FConhecimentos.Items[J].CTe.procCTe.chCTe := AInfProt.Items[I].chCTe;
        FConhecimentos.Items[J].CTe.procCTe.dhRecbto := AInfProt.Items[I].dhRecbto;
        FConhecimentos.Items[J].CTe.procCTe.nProt := AInfProt.Items[I].nProt;
        FConhecimentos.Items[J].CTe.procCTe.digVal := AInfProt.Items[I].digVal;
        FConhecimentos.Items[J].CTe.procCTe.cStat := AInfProt.Items[I].cStat;
        FConhecimentos.Items[J].CTe.procCTe.xMotivo := AInfProt.Items[I].xMotivo;

        NomeXML := '-cte.xml';
        if (AInfProt.Items[I].cStat = 110) or (AInfProt.Items[I].cStat = 301) then
          NomeXML := '-den.xml';

        if FPConfiguracoesCTe.Arquivos.Salvar or NaoEstaVazio(
          FConhecimentos.Items[J].NomeArq) then
        begin
          if FileExists(PathWithDelim(FPConfiguracoesCTe.Arquivos.PathSalvar) +
                        AInfProt.Items[I].chCTe + '-cte.xml') and
             FileExists(PathWithDelim(FPConfiguracoesCTe.Arquivos.PathSalvar) +
                        FCTeRetorno.nRec + '-pro-rec.xml') then
          begin
            AProcCTe := TProcCTe.Create;
            try
              AProcCTe.PathCTe :=
                PathWithDelim(FPConfiguracoesCTe.Arquivos.PathSalvar) +
                AInfProt.Items[I].chCTe + '-cte.xml';
              AProcCTe.PathRetConsReciCTe :=
                PathWithDelim(FPConfiguracoesCTe.Arquivos.PathSalvar) +
                FCTeRetorno.nRec + '-pro-rec.xml';

              AProcCTe.Versao := FPVersaoServico;
              AProcCTe.GerarXML;

              if NaoEstaVazio(AProcCTe.Gerador.ArquivoFormatoXML) then
              begin
                if NaoEstaVazio(FConhecimentos.Items[J].NomeArq) then
                  AProcCTe.Gerador.SalvarArquivo(FConhecimentos.Items[J].NomeArq)
                else
                  AProcCTe.Gerador.SalvarArquivo(
                    PathWithDelim(FPConfiguracoesCTe.Arquivos.PathSalvar) +
                    AInfProt.Items[I].chCTe + NomeXML);
              end;
            finally
              AProcCTe.Free;
            end;
          end;
        end;

        if FPConfiguracoesCTe.Arquivos.Salvar then
        begin
          if FPConfiguracoesCTe.Arquivos.SalvarApenasCTeProcessadas then
          begin
            if FConhecimentos.Items[J].Processada then
              FConhecimentos.Items[J].GravarXML;
          end
          else
            FConhecimentos.Items[J].GravarXML;
        end;

        break;
      end;
    end;
  end;

  //Verificando se existe algum Conhecimento confirmado
  for I := 0 to FConhecimentos.Count - 1 do
  begin
    if FConhecimentos.Items[I].Confirmada then
    begin
      Result := True;
      break;
    end;
  end;

  //Verificando se existe algum Conhecimento nao confirmado
  for I := 0 to FConhecimentos.Count - 1 do
  begin
    if not FConhecimentos.Items[I].Confirmada then
    begin
      FPMsg := ACBrStr('Conhecimento(s) não confirmados:') + LineBreak;
      break;
    end;
  end;

  //Montando a mensagem de retorno para os Conhecimentos nao confirmados
  for I := 0 to FConhecimentos.Count - 1 do
  begin
    if not FConhecimentos.Items[I].Confirmada then
      FPMsg := FPMsg + IntToStr(FConhecimentos.Items[I].CTe.Ide.nCT) +
        '->' + FConhecimentos.Items[I].Msg + LineBreak;
  end;

  if AInfProt.Count > 0 then
  begin
    FChaveCTe := AInfProt.Items[0].chCTe;
    FProtocolo := AInfProt.Items[0].nProt;
    FcStat := AInfProt.Items[0].cStat;
  end;
end;

function TCTeRetRecepcao.Executar: Boolean;
var
  IntervaloTentativas, vCont, qTent: integer;
begin
  Result := False;

  TACBrCTe(FPDFeOwner).SetStatus(stCTeRetRecepcao);
  try
    Sleep(FPConfiguracoesCTe.WebServices.AguardarConsultaRet);

    vCont := 1000;
    qTent := 0; // Inicializa o contador de tentativas
    IntervaloTentativas := FPConfiguracoesCTe.WebServices.IntervaloTentativas;

    while (inherited Executar) and
      (qTent < FPConfiguracoesCTe.WebServices.Tentativas) do
    begin
      Inc(qTent);

      if IntervaloTentativas > 0 then
        sleep(IntervaloTentativas)
      else
        Sleep(vCont);
    end;
  finally
    TACBrCTe(FPDFeOwner).SetStatus(stIdle);
  end;

  if FCTeRetorno.CStat = 104 then  // Lote processado ?
    Result := TratarRespostaFinal;
end;

procedure TCTeRetRecepcao.DefinirURL;
begin
  FPLayout := LayCTeRetRecepcao;

  inherited DefinirURL;
end;

procedure TCTeRetRecepcao.DefinirServicoEAction;
begin
  FPServico    := GetUrlWsd + 'CteRetRecepcao';
  FPSoapAction := FPServico + '/cteRetRecepcao';
end;

procedure TCTeRetRecepcao.DefinirDadosMsg;
var
  ConsReciCTe: TConsReciCTe;
begin
  ConsReciCTe := TConsReciCTe.Create;
  try
    ConsReciCTe.tpAmb := FPConfiguracoesCTe.WebServices.Ambiente;
    ConsReciCTe.nRec := FRecibo;
    ConsReciCTe.Versao := FPVersaoServico;
    ConsReciCTe.GerarXML;

    FPDadosMsg := ConsReciCTe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciCTe.Free;
  end;
end;

function TCTeRetRecepcao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'cteRetRecepcaoResult');

  // Limpando variaveis internas
  FCTeRetorno.Free;
  FCTeRetorno := TRetConsReciCTe.Create;

  FCTeRetorno.Leitor.Arquivo := FPRetWS;
  FCTeRetorno.LerXML;

  Fversao := FCTeRetorno.versao;
  FTpAmb := FCTeRetorno.TpAmb;
  FverAplic := FCTeRetorno.verAplic;
  FcStat := FCTeRetorno.cStat;
  FcUF := FCTeRetorno.cUF;
  FPMsg := FCTeRetorno.xMotivo;
  FxMotivo := FCTeRetorno.xMotivo;
  FcMsg := FCTeRetorno.cMsg;
  FxMsg := FCTeRetorno.xMsg;

  Result := (FCTeRetorno.CStat = 105); // Lote em Processamento
end;

procedure TCTeRetRecepcao.FinalizarServico;
begin
  // Sobrescrito, para não liberar para stIdle... não ainda...;
end;

function TCTeRetRecepcao.GerarMsgLog: String;
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
                   [FCTeRetorno.versao, TpAmbToStr(FCTeRetorno.tpAmb),
                    FCTeRetorno.verAplic, FCTeRetorno.nRec,
                    IntToStr(FCTeRetorno.cStat), FCTeRetorno.xMotivo,
                    CodigoParaUF(FCTeRetorno.cUF), IntToStr(FCTeRetorno.cMsg),
                    FCTeRetorno.xMsg]);
end;

function TCTeRetRecepcao.GerarPrefixoArquivo: String;
begin
  Result := Recibo;
end;

{ TCTeRecibo }

constructor TCTeRecibo.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FCTeRetorno := TRetConsReciCTe.Create;

  FPStatus := stCTeRecibo;
  FPLayout := LayCTeRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';
end;

destructor TCTeRecibo.Destroy;
begin
  FCTeRetorno.Free;

  inherited Destroy;
end;

procedure TCTeRecibo.DefinirServicoEAction;
begin
  FPServico    := GetUrlWsd + 'CteRetRecepcao';
  FPSoapAction := FPServico + '/cteRetRecepcao';
end;

procedure TCTeRecibo.DefinirURL;
begin
  FPLayout := LayCTeRetRecepcao;

  inherited DefinirURL;
end;

procedure TCTeRecibo.DefinirDadosMsg;
var
  ConsReciCTe: TConsReciCTe;
begin
  ConsReciCTe := TConsReciCTe.Create;
  try
    ConsReciCTe.tpAmb := FPConfiguracoesCTe.WebServices.Ambiente;
    ConsReciCTe.nRec := FRecibo;
    ConsReciCTe.Versao := FPVersaoServico;
    ConsReciCTe.GerarXML;

    FPDadosMsg := ConsReciCTe.Gerador.ArquivoFormatoXML;
  finally
    ConsReciCTe.Free;
  end;
end;

function TCTeRecibo.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'cteRetRecepcaoResult');

  // Limpando variaveis internas
  FCTeRetorno.Free;
  FCTeRetorno := TRetConsReciCTe.Create;

  FCTeRetorno.Leitor.Arquivo := FPRetWS;
  FCTeRetorno.LerXML;

  Fversao := FCTeRetorno.versao;
  FTpAmb := FCTeRetorno.TpAmb;
  FverAplic := FCTeRetorno.verAplic;
  FcStat := FCTeRetorno.cStat;
  FxMotivo := FCTeRetorno.xMotivo;
  FcUF := FCTeRetorno.cUF;
  FxMsg := FCTeRetorno.xMsg;
  FcMsg := FCTeRetorno.cMsg;
  FPMsg := FxMotivo;

  Result := (FCTeRetorno.CStat = 104);
end;

function TCTeRecibo.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Recibo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'UF: %s ' + LineBreak),
                   [FCTeRetorno.versao, TpAmbToStr(FCTeRetorno.TpAmb),
                   FCTeRetorno.verAplic, FCTeRetorno.nRec,
                   IntToStr(FCTeRetorno.cStat),
                   FCTeRetorno.ProtCTe.Items[0].xMotivo,
                   CodigoParaUF(FCTeRetorno.cUF)]);
end;

{ TCTeConsulta }

constructor TCTeConsulta.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FprotCTe := TProcCTe.Create;
  FretCancCTe := TRetCancCTe.Create;
  FprocEventoCTe := TRetEventoCTeCollection.Create(AOwner);

  FPStatus := stCTeConsulta;
  FPLayout := LayCTeConsulta;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';
end;

destructor TCTeConsulta.Destroy;
begin
  FprotCTe.Free;
  FretCancCTe.Free;
  if Assigned(FprocEventoCTe) then
    FprocEventoCTe.Free;

  inherited Destroy;
end;

procedure TCTeConsulta.DefinirServicoEAction;
begin
  FPServico    := GetUrlWsd + 'CteConsulta';
  FPSoapAction := FPServico + '/cteConsultaCT';
end;

procedure TCTeConsulta.DefinirDadosMsg;
var
  ConsSitCTe: TConsSitCTe;
begin
  ConsSitCTe := TConsSitCTe.Create;
  try
    ConsSitCTe.TpAmb := FPConfiguracoesCTe.WebServices.Ambiente;
    ConsSitCTe.chCTe := FCTeChave;

    ConsSitCTe.Versao := FPVersaoServico;
    ConsSitCTe.GerarXML;

    FPDadosMsg := ConsSitCTe.Gerador.ArquivoFormatoXML;
  finally
    ConsSitCTe.Free;
  end;
end;

function TCTeConsulta.TratarResposta: Boolean;
var
  CTeRetorno: TRetConsSitCTe;
  CTCancelado, Atualiza: Boolean;
  aEventos, aMsg, NomeArquivo, aCTe, aCTeDFe, NomeXML: String;
  AProcCTe: TProcCTe;
  I, J, K, Inicio, Fim: Integer;
  Data: TDateTime;
  LocCTeW: TCTeW;
begin
  CTeRetorno := TRetConsSitCTe.Create;

  try
    FPRetWS := SeparaDados(FPRetornoWS, 'cteConsultaCTResult');

    CTeRetorno.Leitor.Arquivo := FPRetWS;
    CTeRetorno.LerXML;

    CTCancelado := False;
    aEventos := '';

    // <retConsSitCTe> - Retorno da consulta da situação do CT-e
    // Este é o status oficial do CT-e
    Fversao := CTeRetorno.versao;
    FTpAmb := CTeRetorno.tpAmb;
    FverAplic := CTeRetorno.verAplic;
    FcStat := CTeRetorno.cStat;
    FXMotivo := CTeRetorno.xMotivo;
    FcUF := CTeRetorno.cUF;
    FCTeChave := CTeRetorno.chCTe;
    FPMsg := FXMotivo;

    // Verifica se o Conhecimento está cancelado pelo método antigo. Se estiver,
    // então CTCancelado será True e já atribui Protocolo, Data e Mensagem
    if CTeRetorno.retCancCTe.cStat > 0 then
    begin
      FRetCancCTe.versao := CTeRetorno.retCancCTe.versao;
      FretCancCTe.tpAmb := CTeRetorno.retCancCTe.tpAmb;
      FretCancCTe.verAplic := CTeRetorno.retCancCTe.verAplic;
      FretCancCTe.cStat := CTeRetorno.retCancCTe.cStat;
      FretCancCTe.xMotivo := CTeRetorno.retCancCTe.xMotivo;
      FretCancCTe.cUF := CTeRetorno.retCancCTe.cUF;
      FretCancCTe.chCTe := CTeRetorno.retCancCTe.chCTe;
      FretCancCTe.dhRecbto := CTeRetorno.retCancCTe.dhRecbto;
      FretCancCTe.nProt := CTeRetorno.retCancCTe.nProt;

      CTCancelado := True;
      FProtocolo := CTeRetorno.retCancCTe.nProt;
      FDhRecbto := CTeRetorno.retCancCTe.dhRecbto;
      FPMsg := CTeRetorno.xMotivo;
    end;

    // <protCTe> - Retorno dos dados do ENVIO da CT-e
    // Considerá-los apenas se não existir nenhum evento de cancelamento (110111)
    FprotCTe.PathCTe := CTeRetorno.protCTe.PathCTe;
    FprotCTe.PathRetConsReciCTe := CTeRetorno.protCTe.PathRetConsReciCTe;
    FprotCTe.PathRetConsSitCTe := CTeRetorno.protCTe.PathRetConsSitCTe;
    FprotCTe.tpAmb := CTeRetorno.protCTe.tpAmb;
    FprotCTe.verAplic := CTeRetorno.protCTe.verAplic;
    FprotCTe.chCTe := CTeRetorno.protCTe.chCTe;
    FprotCTe.dhRecbto := CTeRetorno.protCTe.dhRecbto;
    FprotCTe.nProt := CTeRetorno.protCTe.nProt;
    FprotCTe.digVal := CTeRetorno.protCTe.digVal;
    FprotCTe.cStat := CTeRetorno.protCTe.cStat;
    FprotCTe.xMotivo := CTeRetorno.protCTe.xMotivo;

    if Assigned(CTeRetorno.procEventoCTe) and (CTeRetorno.procEventoCTe.Count > 0) then
    begin
      aEventos := '=====================================================' +
        LineBreak + '================== Eventos da CT-e ==================' +
        LineBreak + '=====================================================' +
        LineBreak + '' + LineBreak + 'Quantidade total de eventos: ' +
        IntToStr(CTeRetorno.procEventoCTe.Count);

      FprocEventoCTe.Clear;
      for I := 0 to CTeRetorno.procEventoCTe.Count - 1 do
      begin
        with FprocEventoCTe.Add.RetEventoCTe do
        begin
          idLote := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.idLote;
          tpAmb := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.tpAmb;
          verAplic := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.verAplic;
          cOrgao := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.cOrgao;
          cStat := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.cStat;
          xMotivo := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.xMotivo;
          XML := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.XML;

          ICTevento.ID := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.ID;
          InfEvento.tpAmb := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.tpAmb;
          InfEvento.CNPJ := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.CNPJ;
          InfEvento.chCTe := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.chCTe;
          InfEvento.dhEvento := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.dhEvento;
          InfEvento.TpEvento := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.TpEvento;
          InfEvento.nSeqEvento := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.nSeqEvento;
          InfEvento.VersaoEvento := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.VersaoEvento;
          InfEvento.DetEvento.nProt := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.DetEvento.nProt;
          InfEvento.DetEvento.xJust := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.DetEvento.xJust;
          InfEvento.DetEvento.xCondUso := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.DetEvento.xCondUso;

          InfEvento.DetEvento.infCorrecao.Clear;
          for k := 0 to CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.detEvento.infCorrecao.Count -1 do
          begin
            InfEvento.DetEvento.infCorrecao.Add;
            InfEvento.DetEvento.infCorrecao.Items[k].grupoAlterado   := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.DetEvento.infCorrecao.Items[k].grupoAlterado;
            InfEvento.DetEvento.infCorrecao.Items[k].campoAlterado   := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.DetEvento.infCorrecao.Items[k].campoAlterado;
            InfEvento.DetEvento.infCorrecao.Items[k].valorAlterado   := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.DetEvento.infCorrecao.Items[k].valorAlterado;
            InfEvento.DetEvento.infCorrecao.Items[k].nroItemAlterado := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.InfEvento.DetEvento.infCorrecao.Items[k].nroItemAlterado;
          end;

          retEvento.Clear;
          for J := 0 to CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Count-1 do
          begin
            with retEvento.Add.RetInfEvento do
            begin
              Id := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.Id;
              tpAmb := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.tpAmb;
              verAplic := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.verAplic;
              cOrgao := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.cOrgao;
              cStat := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.cStat;
              xMotivo := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.xMotivo;
              chCTe := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.chCTe;
              tpEvento := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.tpEvento;
              xEvento := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.xEvento;
              nSeqEvento := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.nSeqEvento;
              CNPJDest := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.CNPJDest;
              emailDest := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.emailDest;
              dhRegEvento := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.dhRegEvento;
              nProt := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.nProt;
              XML := CTeRetorno.procEventoCTe.Items[I].RetEventoCTe.retEvento.Items[J].RetInfEvento.XML;
            end;
          end;
        end;

        with CTeRetorno.procEventoCTe.Items[I].RetEventoCTe do
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
            CTCancelado := True;
            FProtocolo := retEvento.Items[J].RetInfEvento.nProt;
            FDhRecbto := retEvento.Items[J].RetInfEvento.dhRegEvento;
            FPMsg := retEvento.Items[J].RetInfEvento.xMotivo;
          end;
        end;
      end;
    end;

    if not CTCancelado then
    begin
      FProtocolo := CTeRetorno.protCTe.nProt;
      FDhRecbto := CTeRetorno.protCTe.dhRecbto;
      FPMsg := CTeRetorno.protCTe.xMotivo;
    end;

    //TODO: Verificar porque monta "aMsg", pois ela não está sendo usada em lugar nenhum
    aMsg := GerarMsgLog;
    if aEventos <> '' then
      aMsg := aMsg + sLineBreak + aEventos;

    Result := (CTeRetorno.CStat in [100, 101, 110, 150, 151, 155]);

    NomeArquivo := PathWithDelim(FPConfiguracoesCTe.Arquivos.PathSalvar) + FCTeChave;

    for i := 0 to TACBrCTe(FPDFeOwner).Conhecimentos.Count - 1 do
    begin
      with TACBrCTe(FPDFeOwner).Conhecimentos.Items[i] do
      begin
        if (OnlyNumber(FCTeChave) = NumID) then
        begin
          Atualiza := True;
          if ((CTeRetorno.CStat in [101, 151, 155]) and
            (not FPConfiguracoesCTe.Geral.AtualizarXMLCancelado)) then
            Atualiza := False;

          // Atualiza o Status da CTe interna //
          CTe.procCTe.cStat := CTeRetorno.cStat;

          if Atualiza then
          begin
            if (FPConfiguracoesCTe.Geral.ValidarDigest) and
              (CTeRetorno.protCTe.digVal <> '') and
              (CTe.signature.DigestValue <> CTeRetorno.protCTe.digVal) then
            begin
              raise EACBrCTeException.Create('DigestValue do documento ' +
                NumID + ' não confere.');
            end;

            CTe.procCTe.Id := CTeRetorno.protCTe.Id;
            CTe.procCTe.tpAmb := CTeRetorno.tpAmb;
            CTe.procCTe.verAplic := CTeRetorno.verAplic;
            CTe.procCTe.chCTe := CTeRetorno.chCTe;
            CTe.procCTe.dhRecbto := FDhRecbto;
            CTe.procCTe.nProt := FProtocolo;
            CTe.procCTe.digVal := CTeRetorno.protCTe.digVal;
            CTe.procCTe.cStat := CTeRetorno.cStat;
            CTe.procCTe.xMotivo := CTeRetorno.xMotivo;

            NomeXML := '-cte.xml';
            if (CTeRetorno.protCTe.cStat = 110) or (CTeRetorno.protCTe.cStat = 301) then
              NomeXML := '-den.xml';

            if FileExists(NomeArquivo + '-cte.xml') or NaoEstaVazio(NomeArq) then
            begin
              AProcCTe := TProcCTe.Create;
              try
                if NaoEstaVazio(NomeArq) then
                  AProcCTe.PathCTe := NomeArq
                else
                  AProcCTe.PathCTe := NomeArquivo + '-cte.xml';

                AProcCTe.PathRetConsSitCTe := NomeArquivo + '-sit.xml';

                AProcCTe.GerarXML;

                aCTe := AProcCTe.Gerador.ArquivoFormatoXML;

                if NaoEstaVazio(AProcCTe.Gerador.ArquivoFormatoXML) then
                  AProcCTe.Gerador.SalvarArquivo(AProcCTe.PathCTe);

                FRetCTeDFe := '';

                if (NaoEstaVazio(aCTe)) and (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoCTe'))) then
                begin
                  Inicio := Pos('<procEventoCTe', FPRetWS);
                  Fim    := Pos('</retConsSitCTe', FPRetWS) -1;

                  aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

                  aCTeDFe := '<?xml version="1.0" encoding="UTF-8" ?>' +
                             '<CTeDFe>' +
                              '<procCTe versao="' + CTeenviCTe + '">' +
                                SeparaDados(aCTe, 'cteProc') +
                              '</procCTe>' +
                              '<procEventoCTe versao="' + CTeEventoCTe + '">' +
                                aEventos +
                              '</procEventoCTe>' +
                             '</CTeDFe>';

                  FRetCTeDFe := aCTeDFe;
                end;
              finally
                AProcCTe.Free;
              end;
            end
            else begin
             LocCTeW := TCTeW.Create(TACBrCTe(FPDFeOwner).Conhecimentos.Items[i].CTe);
             try
               LocCTeW.GerarXML;

               aCTe := LocCTeW.Gerador.ArquivoFormatoXML;

               FRetCTeDFe := '';

               if (NaoEstaVazio(aCTe)) and (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoCTe'))) then
                begin
                  Inicio := Pos('<procEventoCTe', FPRetWS);
                  Fim    := Pos('</retConsSitCTe', FPRetWS) -1;

                  aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

                  aCTeDFe := '<?xml version="1.0" encoding="UTF-8" ?>' +
                             '<CTeDFe>' +
                              '<procCTe versao="' + CTeenviCTe + '">' +
                                SeparaDados(aCTe, 'cteProc') +
                              '</procCTe>' +
                              '<procEventoCTe versao="' + CTeEventoCTe + '">' +
                                aEventos +
                              '</procEventoCTe>' +
                             '</CTeDFe>';

                  FRetCTeDFe := aCTeDFe;
                end;
             finally
               LocCTeW.Free;
             end;
            end;

            if FPConfiguracoesCTe.Arquivos.Salvar then
            begin
              if FPConfiguracoesCTe.Arquivos.SalvarApenasCTeProcessadas then
              begin
                if Processada then
                  GravarXML();
              end
              else
                GravarXML();
            end;


            if FPConfiguracoes.Arquivos.Salvar and (FRetCTeDFe <> '') then
            begin
              if FPConfiguracoes.Arquivos.EmissaoPathCTe then
                Data := TACBrCTe(FPDFeOwner).Conhecimentos.Items[i].CTe.Ide.dhEmi
              else
                Data := Now;

              FPConfiguracoes.Geral.Save(FCTeChave + '-CTeDFe.xml',
                                         aCTeDFe,
                                         PathWithDelim(FPConfiguracoes.Arquivos.GetPathCTe(Data)));
          end;
          end;

          break;
        end;
      end;
    end;

    if (TACBrCTe(FPDFeOwner).Conhecimentos.Count <= 0) then
    begin
      if FPConfiguracoesCTe.Arquivos.Salvar then
      begin
        if FileExists(NomeArquivo + '-cte.xml') then
        begin
          AProcCTe := TProcCTe.Create;
          try
            AProcCTe.PathCTe := NomeArquivo + '-cte.xml';
            AProcCTe.PathRetConsSitCTe := NomeArquivo + '-sit.xml';

            AProcCTe.GerarXML;

            aCTe := AProcCTe.Gerador.ArquivoFormatoXML;

            if NaoEstaVazio(AProcCTe.Gerador.ArquivoFormatoXML) then
              AProcCTe.Gerador.SalvarArquivo(AProcCTe.PathCTe);

            if (NaoEstaVazio(aCTe)) and (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoCTe'))) then
            begin
              Inicio := Pos('<procEventoCTe', FPRetWS);
              Fim    := Pos('</retConsSitCTe', FPRetWS) -1;

              aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

              aCTeDFe := '<?xml version="1.0" encoding="UTF-8" ?>' +
                         '<CTeDFe>' +
                          '<procCTe versao="' + CTeenviCTe + '">' +
                            SeparaDados(aCTe, 'cteProc') +
                          '</procCTe>' +
                          '<procEventoCTe versao="' + CTeEventoCTe + '">' +
                            aEventos +
                          '</procEventoCTe>' +
                         '</CTeDFe>';

              FRetCTeDFe := aCTeDFe;
            end;
          finally
            AProcCTe.Free;
          end;
        end;

       if (FPConfiguracoes.Arquivos.Salvar) and (FRetCTeDFe <> '') then
       begin
         if FPConfiguracoes.Arquivos.EmissaoPathCTe then
           Data := TACBrCTe(FPDFeOwner).Conhecimentos.Items[i].CTe.Ide.dhEmi
         else
           Data := Now;

         FPConfiguracoes.Geral.Save(FCTeChave + '-CTeDFe.xml',
                                    aCTeDFe,
                                    PathWithDelim(FPConfiguracoes.Arquivos.GetPathCTe(Data)));
       end;
      end;
    end;

  finally
    CTeRetorno.Free;
  end;
end;

function TCTeConsulta.GerarMsgLog: String;
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
                   [Fversao, FCTeChave, TpAmbToStr(FTpAmb), FverAplic,
                    IntToStr(FcStat), FXMotivo, CodigoParaUF(FcUF), FCTeChave,
                    FormatDateTimeBr(FDhRecbto), FProtocolo, FprotCTe.digVal]);
end;

function TCTeConsulta.GerarPrefixoArquivo: String;
begin
  Result := Trim(FCTeChave);
end;

{ TCTeInutilizacao }

constructor TCTeInutilizacao.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stCTeInutilizacao;
  FPLayout := LayCTeInutilizacao;
  FPArqEnv := 'ped-inu';
  FPArqResp := 'inu';
end;

procedure TCTeInutilizacao.SetJustificativa(AValue: String);
var
  TrimValue: String;
begin
  TrimValue := Trim(AValue);

  if EstaVazio(TrimValue) then
    GerarException(ACBrStr('Informar uma Justificativa para Inutilização de ' +
      'numeração do Conhecimento Eletronico'));

  if Length(TrimValue) < 15 then
    GerarException(ACBrStr('A Justificativa para Inutilização de numeração do ' +
      'Conhecimento Eletronico deve ter no minimo 15 caracteres'));

  FJustificativa := TrimValue;
end;

function TCTeInutilizacao.GerarPathPorCNPJ(): String;
var
  CNPJ: String;
begin
  if FPConfiguracoesCTe.Arquivos.SepararPorCNPJ then
    CNPJ := FCNPJ
  else
    CNPJ := '';

  Result := FPConfiguracoesCTe.Arquivos.GetPathInu(CNPJ);
end;

procedure TCTeInutilizacao.DefinirServicoEAction;
begin
  FPServico    := GetUrlWsd + 'CteInutilizacao';
  FPSoapAction := FPServico + '/cteInutilizacaoCT';
end;

procedure TCTeInutilizacao.DefinirDadosMsg;
var
  InutCTe: TinutCTe;
begin
  InutCTe := TinutCTe.Create;
  try
    InutCTe.tpAmb := FPConfiguracoesCTe.WebServices.Ambiente;
    InutCTe.cUF := FPConfiguracoesCTe.WebServices.UFCodigo;
    InutCTe.ano := FAno;
    InutCTe.CNPJ := FCNPJ;
    InutCTe.modelo := FModelo;
    InutCTe.serie := FSerie;
    InutCTe.nCTIni := FNumeroInicial;
    InutCTe.nCTFin := FNumeroFinal;
    InutCTe.xJust := FJustificativa;

    InutCTe.Versao := FPVersaoServico;
    InutCTe.GerarXML;

    AssinarXML(InutCTe.Gerador.ArquivoFormatoXML, 'inutCTe', 'infInut',
      'Falha ao assinar Inutilização Conhecimento Eletrônico ');

    FID := InutCTe.ID;
  finally
    InutCTe.Free;
  end;
end;

procedure TCTeInutilizacao.SalvarEnvio;
var
  aPath: String;
begin
  inherited SalvarEnvio;

  if FPConfiguracoesCTe.Geral.Salvar then
  begin
    aPath := GerarPathPorCNPJ;
    FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqEnv + '.xml', FPDadosMsg, aPath);
  end;
end;

procedure TCTeInutilizacao.SalvarResposta;
var
  aPath: String;
begin
  inherited SalvarResposta;

  if FPConfiguracoesCTe.Geral.Salvar then
  begin
    aPath := GerarPathPorCNPJ;
    FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqResp + '.xml', FPRetWS, aPath);
  end;
end;

function TCTeInutilizacao.TratarResposta: Boolean;
var
  CTeRetorno: TRetInutCTe;
  wProc: TStringList;
begin
  CTeRetorno := TRetInutCTe.Create;
  try
    FPRetWS := SeparaDados(FPRetornoWS, 'cteInutilizacaoCTResult');

    CTeRetorno.Leitor.Arquivo := FPRetWS;
    CTeRetorno.LerXml;

    Fversao := CTeRetorno.versao;
    FTpAmb := CTeRetorno.TpAmb;
    FverAplic := CTeRetorno.verAplic;
    FcStat := CTeRetorno.cStat;
    FxMotivo := CTeRetorno.xMotivo;
    FcUF := CTeRetorno.cUF;
    FdhRecbto := CTeRetorno.dhRecbto;
    Fprotocolo := CTeRetorno.nProt;
    FPMsg := CTeRetorno.XMotivo;

    Result := (CTeRetorno.cStat = 102);

    //gerar arquivo proc de inutilizacao
    if ((CTeRetorno.cStat = 102) or (CTeRetorno.cStat = 563)) then
    begin
      wProc := TStringList.Create;
      try
        wProc.Add('<' + ENCODING_UTF8 + '>');
        wProc.Add('<ProcInutCTe versao="' + FPVersaoServico +
          '" xmlns="http://www.portalfiscal.inf.br/cte">');

        wProc.Add(FPDadosMsg);
        wProc.Add(FPRetWS);
        wProc.Add('</ProcInutCTe>');
        FXML_ProcInutCTe := wProc.Text;
      finally
        wProc.Free;
      end;

      if FPConfiguracoesCTe.Geral.Salvar then
        FPDFeOwner.Gravar(GerarPrefixoArquivo + '-procInutCTe.xml',
          FXML_ProcInutCTe);

      if FPConfiguracoesCTe.Arquivos.Salvar then
        FPDFeOwner.Gravar(GerarPrefixoArquivo + '-procInutCTe.xml',
          FXML_ProcInutCTe, GerarPathPorCNPJ);
    end;
  finally
    CTeRetorno.Free;
  end;
end;

function TCTeInutilizacao.GerarMsgLog: String;
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

function TCTeInutilizacao.GerarPrefixoArquivo: String;
begin
  Result := Trim(OnlyNumber(FID));
end;

{ TCTeConsultaCadastro }

constructor TCTeConsultaCadastro.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FRetConsCad := TRetConsCad.Create;

  FPStatus := stCTeCadastro;
  FPLayout := LayCTeCadastro;
  FPArqEnv := 'ped-cad';
  FPArqResp := 'cad';
end;

destructor TCTeConsultaCadastro.Destroy;
begin
  FRetConsCad.Free;

  inherited Destroy;
end;

procedure TCTeConsultaCadastro.SetCNPJ(const Value: String);
begin
  if NaoEstaVazio(Value) then
  begin
    FIE := '';
    FCPF := '';
  end;

  FCNPJ := Value;
end;

procedure TCTeConsultaCadastro.SetCPF(const Value: String);
begin
  if NaoEstaVazio(Value) then
  begin
    FIE := '';
    FCNPJ := '';
  end;

  FCPF := Value;
end;

procedure TCTeConsultaCadastro.SetIE(const Value: String);
begin
  if NaoEstaVazio(Value) then
  begin
    FCNPJ := '';
    FCPF := '';
  end;

  FIE := Value;
end;

procedure TCTeConsultaCadastro.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'CadConsultaCadastro2';
  FPSoapAction := FPServico;
end;

procedure TCTeConsultaCadastro.DefinirDadosMsg;
var
  ConCadCTe: TConsCad;
begin
  ConCadCTe := TConsCad.Create;
  try
    ConCadCTe.UF := FUF;
    ConCadCTe.IE := FIE;
    ConCadCTe.CNPJ := FCNPJ;
    ConCadCTe.CPF := FCPF;
    ConCadCTe.Versao := FPVersaoServico;
    ConCadCTe.GerarXML;

    FPDadosMsg := ConCadCTe.Gerador.ArquivoFormatoXML;
  finally
    ConCadCTe.Free;
  end;
end;

function TCTeConsultaCadastro.TratarResposta: Boolean;
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

function TCTeConsultaCadastro.GerarMsgLog: String;
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

function TCTeConsultaCadastro.GerarUFSoap: String;
begin
  Result := '<cUF>' + IntToStr(UFparaCodigo(FUF)) + '</cUF>';
end;

{ TCTeEnvEvento }

constructor TCTeEnvEvento.Create(AOwner: TACBrDFe; AEvento: TEventoCTe);
begin
  inherited Create(AOwner);

  FEventoRetorno := TRetEventoCTe.Create;
  FEvento := AEvento;

  FPStatus := stCTeEvento;
  FPLayout := LayCTeEvento;
  FPArqEnv := 'ped-eve';
  FPArqResp := 'eve';
end;

destructor TCTeEnvEvento.Destroy;
begin
  FEventoRetorno.Free;

  inherited;
end;

function TCTeEnvEvento.GerarPathEvento: String;
begin
  with FEvento.Evento.Items[0].Infevento do
  begin
    if (tpEvento = teCCe) and
      (not FPConfiguracoesCTe.Arquivos.SalvarCCeCanEvento) then
      Result := FPConfiguracoesCTe.Arquivos.GetPathCCe
    else if (tpEvento = teCancelamento) and
      (not FPConfiguracoesCTe.Arquivos.SalvarCCeCanEvento) then
      Result := FPConfiguracoesCTe.Arquivos.GetPathCan
    else
      Result := FPConfiguracoesCTe.Arquivos.GetPathEvento(tpEvento);
  end;
end;

procedure TCTeEnvEvento.DefinirURL;
begin
  { Verificação necessária pois somente os eventos de Cancelamento e CCe serão tratados pela SEFAZ do estado
    os outros eventos como manifestacao de destinatários serão tratados diretamente pela RFB }

  if not (FEvento.Evento.Items[0].InfEvento.tpEvento in [teCCe, teCancelamento, teMultiModal]) then
    FPLayout := LayCTeEventoEPEC
  else
    FPLayout := LayCTeEvento;

  inherited DefinirURL;
end;

procedure TCTeEnvEvento.DefinirServicoEAction;
begin
  FPServico    := GetUrlWsd + 'CteRecepcaoEvento';
  FPSoapAction := FPServico + '/cteRecepcaoEvento';
end;

procedure TCTeEnvEvento.DefinirDadosMsg;
var
  EventoCTe: TEventoCTe;
  I, J, F: integer;
  Lote, Evento, Eventos, EventosAssinados, AXMLEvento: String;
  EventoEhValido: Boolean;
begin
  EventoCTe := TEventoCTe.Create;
  try
    EventoCTe.idLote := FidLote;
    FEveEPEC := False;

    for I := 0 to TCTeEnvEvento(Self).FEvento.Evento.Count - 1 do
    begin
      with EventoCTe.Evento.Add do
      begin
        infEvento.tpAmb := FPConfiguracoesCTe.WebServices.Ambiente;
        infEvento.CNPJ := FEvento.Evento[I].InfEvento.CNPJ;
        infEvento.cOrgao := FEvento.Evento[I].InfEvento.cOrgao;
        infEvento.chCTe := FEvento.Evento[I].InfEvento.chCTe;
        infEvento.dhEvento := FEvento.Evento[I].InfEvento.dhEvento;
        infEvento.tpEvento := FEvento.Evento[I].InfEvento.tpEvento;
        infEvento.nSeqEvento := FEvento.Evento[I].InfEvento.nSeqEvento;

        case InfEvento.tpEvento of
          teCCe:
          begin
            infEvento.detEvento.xCondUso := FEvento.Evento[I].InfEvento.detEvento.xCondUso;
            infEvento.detEvento.xCondUso := FEvento.Evento[i].InfEvento.detEvento.xCondUso;

            for j := 0 to FEvento.Evento[i].InfEvento.detEvento.infCorrecao.Count - 1 do
             begin
               with EventoCTe.Evento[i].InfEvento.detEvento.infCorrecao.Add do
                begin
                 grupoAlterado   := FEvento.Evento[i].InfEvento.detEvento.infCorrecao[j].grupoAlterado;
                 campoAlterado   := FEvento.Evento[i].InfEvento.detEvento.infCorrecao[j].campoAlterado;
                 valorAlterado   := FEvento.Evento[i].InfEvento.detEvento.infCorrecao[j].valorAlterado;
                 nroItemAlterado := FEvento.Evento[i].InfEvento.detEvento.infCorrecao[j].nroItemAlterado;
                end;
             end;
          end;

          teCancelamento:
          begin
            infEvento.detEvento.nProt := FEvento.Evento[I].InfEvento.detEvento.nProt;
            infEvento.detEvento.xJust := FEvento.Evento[I].InfEvento.detEvento.xJust;
          end;

          teMultiModal:
          begin
            infEvento.detEvento.xRegistro := FEvento.Evento[i].InfEvento.detEvento.xRegistro;
            infEvento.detEvento.nDoc      := FEvento.Evento[i].InfEvento.detEvento.nDoc;
          end;

          teEPEC:
          begin
            FEveEPEC := True;

            infEvento.detEvento.xJust   := FEvento.Evento[i].InfEvento.detEvento.xJust;
            infEvento.detEvento.vICMS   := FEvento.Evento[i].InfEvento.detEvento.vICMS;
            infEvento.detEvento.vTPrest := FEvento.Evento[i].InfEvento.detEvento.vTPrest;
            infEvento.detEvento.vCarga  := FEvento.Evento[i].InfEvento.detEvento.vCarga;
            infEvento.detEvento.toma    := FEvento.Evento[i].InfEvento.detEvento.toma;
            infEvento.detEvento.UF      := FEvento.Evento[i].InfEvento.detEvento.UF;
            infEvento.detEvento.CNPJCPF := FEvento.Evento[i].InfEvento.detEvento.CNPJCPF;
            infEvento.detEvento.IE      := FEvento.Evento[i].InfEvento.detEvento.IE;
            infEvento.detEvento.modal   := FEvento.Evento[i].InfEvento.detEvento.modal;
            infEvento.detEvento.UFIni   := FEvento.Evento[i].InfEvento.detEvento.UFIni;
            infEvento.detEvento.UFFim   := FEvento.Evento[i].InfEvento.detEvento.UFFim;
          end;
        end;
      end;
    end;

    EventoCTe.Versao := FPVersaoServico;
    EventoCTe.GerarXML;

    // Separa os grupos <evento> e coloca na variável Eventos
    I := Pos('<evento ', EventoCTe.Gerador.ArquivoFormatoXML);
    Lote := Copy(EventoCTe.Gerador.ArquivoFormatoXML, 1, I - 1);
    Eventos := SeparaDados(EventoCTe.Gerador.ArquivoFormatoXML, 'envEvento');
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

//    AssinarXML(EventoCTe.Gerador.ArquivoFormatoXML,
//               'Falha ao assinar o Envio de Evento ' + LineBreak + FMsg);

    // Implementar a validação do evento.
    AXMLEvento := '';

    with TACBrCTe(FPDFeOwner) do
    begin
      EventoEhValido := SSL.Validar(FPDadosMsg, GerarNomeArqSchema(FPLayout, FPVersaoServico), FPMsg) and
                        SSL.Validar(AXMLEvento, GerarNomeArqSchemaEvento(FPDadosMsg, FPVersaoServico), FPMsg);
    end;

    for I := 0 to FEvento.Evento.Count - 1 do
      FEvento.Evento[I].InfEvento.id := EventoCTe.Evento[I].InfEvento.id;
  finally
    EventoCTe.Free;
  end;
end;

procedure TCTeEnvEvento.DefinirEnvelopeSoap;
var
  Texto: AnsiString;
begin
  // UF = 51 = MT não esta aceitando SOAP 1.2
  if FPConfiguracoes.WebServices.UFCodigo <> 51 then
  begin
    Texto := '<?xml version="1.0" encoding="utf-8"?>';
    Texto := Texto + '<soap12:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                                     ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                                     ' xmlns:soap12="http://www.w3.org/2003/05/soap-envelope">';
    Texto := Texto +   '<soap12:Header>';
    Texto := Texto +     '<cteCabecMsg xmlns="' + FPServico + '">';
    Texto := Texto +       GerarUFSoap;
    Texto := Texto +       GerarVersaoDadosSoap;
    Texto := Texto +     '</cteCabecMsg>';
    Texto := Texto +   '</soap12:Header>';
    Texto := Texto +   '<soap12:Body>';
    Texto := Texto +     '<cteDadosMsg xmlns="' + FPServico + '">';
    Texto := Texto +       FPDadosMsg;
    Texto := Texto +     '</cteDadosMsg>';
    Texto := Texto +   '</soap12:Body>';
    Texto := Texto + '</soap12:Envelope>';
  end
  else begin
    Texto := '<?xml version="1.0" encoding="utf-8"?>';
    Texto := Texto + '<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
                                   ' xmlns:xsd="http://www.w3.org/2001/XMLSchema"' +
                                   ' xmlns:soap="http://www.w3.org/2003/05/soap-envelope">';
    Texto := Texto +   '<soap:Header>';
    Texto := Texto +     '<cteCabecMsg xmlns="' + FPServico + '">';
    Texto := Texto +       GerarUFSoap;
    Texto := Texto +       GerarVersaoDadosSoap;
    Texto := Texto +     '</cteCabecMsg>';
    Texto := Texto +   '</soap:Header>';
    Texto := Texto +   '<soap:Body>';
    Texto := Texto +     '<cteDadosMsg xmlns="' + FPServico + '">';
    Texto := Texto +       FPDadosMsg;
    Texto := Texto +     '</cteDadosMsg>';
    Texto := Texto +   '</soap:Body>';
    Texto := Texto + '</soap:Envelope>';
  end;

  FEnvelopeSoap := Texto;
end;

function TCTeEnvEvento.TratarResposta: Boolean;
var
  Leitor: TLeitor;
  I, J: integer;
  wProc: TStringList;
  NomeArq, VersaoEvento: String;
begin
  FEvento.idLote := idLote;

  FPRetWS := SeparaDados(FPRetornoWS, 'cteRecepcaoEventoResult');

  // Limpando variaveis internas
  FEventoRetorno.Free;
  FEventoRetorno := TRetEventoCTe.Create;

  EventoRetorno.Leitor.Arquivo := FPRetWS;
  EventoRetorno.LerXml;

  FcStat := EventoRetorno.cStat;
  FxMotivo := EventoRetorno.xMotivo;
  FPMsg := EventoRetorno.xMotivo;
  FTpAmb := EventoRetorno.tpAmb;

//  FcStat   := FEventoRetorno.retEvento.Items[0].RetInfEvento.cStat;
//  FxMotivo := FEventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
//  FPMsg    := FEventoRetorno.retEvento.Items[0].RetInfEvento.xMotivo;
//  FTpAmb   := FEventoRetorno.retEvento.Items[0].RetInfEvento.tpAmb;

  Result := (FcStat in [128, 135, 136, 155]);

  //gerar arquivo proc de evento
  if Result then
  begin
    Leitor := TLeitor.Create;
    try
      for I := 0 to FEvento.Evento.Count - 1 do
      begin
        for J := 0 to EventoRetorno.retEvento.Count - 1 do
        begin
          if FEvento.Evento.Items[I].InfEvento.chCTe =
            EventoRetorno.retEvento.Items[J].RetInfEvento.chCTe then
          begin
            FEvento.Evento.Items[I].RetInfEvento.nProt :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.nProt;
            FEvento.Evento.Items[I].RetInfEvento.dhRegEvento :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.dhRegEvento;
            FEvento.Evento.Items[I].RetInfEvento.cStat :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.cStat;
            FEvento.Evento.Items[I].RetInfEvento.xMotivo :=
              EventoRetorno.retEvento.Items[J].RetInfEvento.xMotivo;
            FEvento.Evento.Items[i].RetInfEvento.chCTe :=
              EventoRetorno.retEvento.Items[j].RetInfEvento.chCTe;

            wProc := TStringList.Create;
            try
              VersaoEvento := TACBrCTe(FPDFeOwner).LerVersaoDeParams(LayCTeEvento);

              wProc.Add('<' + ENCODING_UTF8 + '>');
              wProc.Add('<procEventoCTe versao="' + VersaoEvento +
                '" xmlns="http://www.portalfiscal.inf.br/cte">');
              wProc.Add('<eventoCTe versao="' + VersaoEvento + '">');
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
              wProc.Add('</eventoCTe>');
              wProc.Add('<retEventoCTe versao="' + VersaoEvento + '">');

              Leitor.Arquivo := FPRetWS;
              wProc.Add(Leitor.rExtrai(1, 'infEvento', '', J + 1));
              wProc.Add('</retEventoCTe>');
              wProc.Add('</procEventoCTe>');

              EventoRetorno.retEvento.Items[J].RetInfEvento.XML := wProc.Text;

              FEvento.Evento.Items[I].RetInfEvento.XML := wProc.Text;

              NomeArq := OnlyNumber(FEvento.Evento.Items[i].InfEvento.Id) +
                '-procEventoCTe.xml';

              if FPConfiguracoesCTe.Geral.Salvar then
                FPDFeOwner.Gravar(NomeArq, wProc.Text);

              if FPConfiguracoesCTe.Arquivos.Salvar then
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

procedure TCTeEnvEvento.SalvarEnvio;
begin
  inherited SalvarEnvio;

  if FPConfiguracoesCTe.Arquivos.Salvar then
    FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqEnv + '.xml',
      FPDadosMsg, GerarPathEvento);
end;

procedure TCTeEnvEvento.SalvarResposta;
begin
  inherited SalvarResposta;

  if FPConfiguracoesCTe.Arquivos.Salvar then;
  FPDFeOwner.Gravar(GerarPrefixoArquivo + '-' + ArqEnv + '.xml',
    FPDadosMsg, GerarPathEvento);
end;

function TCTeEnvEvento.GerarMsgLog: String;
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

function TCTeEnvEvento.GerarPrefixoArquivo: String;
begin
  Result := IntToStr(FEvento.idLote);
end;

{ TCTeEnvioWebService }

constructor TCTeEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stCTeEnvioWebService;
  FVersao := '';
end;

destructor TCTeEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

function TCTeEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TCTeEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TCTeEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TCTeEnvioWebService.DefinirDadosMsg;
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

function TCTeEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TCTeEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TCTeEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrCTe := TACBrCTe(AOwner);

  FStatusServico := TCTeStatusServico.Create(FACBrCTe);
  FEnviar := TCTeRecepcao.Create(FACBrCTe, TACBrCTe(FACBrCTe).Conhecimentos);
  FRetorno := TCTeRetRecepcao.Create(FACBrCTe, TACBrCTe(FACBrCTe).Conhecimentos);
  FRecibo := TCTeRecibo.Create(FACBrCTe);
  FConsulta := TCTeConsulta.Create(FACBrCTe);
  FInutilizacao := TCTeInutilizacao.Create(FACBrCTe);
  FConsultaCadastro := TCTeConsultaCadastro.Create(FACBrCTe);
  FEnvEvento := TCTeEnvEvento.Create(FACBrCTe, TACBrCTe(FACBrCTe).EventoCTe);
  FEnvioWebService := TCTeEnvioWebService.Create(FACBrCTe);
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

procedure TWebServices.Inutiliza(CNPJ, AJustificativa: String;
  Ano, Modelo, Serie, NumeroInicial, NumeroFinal: integer);
begin
  CNPJ := OnlyNumber(CNPJ);

  if not ValidarCNPJ(CNPJ) then
    raise EACBrCTeException.Create('CNPJ: ' + CNPJ + ', inválido.');

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
