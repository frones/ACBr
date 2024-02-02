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

unit ACBrNFComWebServices;

interface

uses
  Classes, SysUtils, dateutils,
  blcksock, synacode,
  ACBrDFe, ACBrDFeWebService,
//  ACBrDFeConversao,
  ACBrXmlBase,
  ACBrNFComNotasFiscais, ACBrNFComConfiguracoes,
  ACBrNFComClass, ACBrNFComConversao,
//  ACBrNFComProc,
  ACBrDFeComum.Proc,
  ACBrNFComRetConsSit,
  ACBrNFComEnvEvento, ACBrNFComRetEnvEvento,
  pcnAuxiliar,
  pcnConversao;

type

  { TNFComWebService }

  TNFComWebService = class(TDFeWebService)
  private
    FOldSSLType: TSSLType;
    FOldHeaderElement: string;
  protected
    FPStatus: TStatusNFCom;
    FPLayout: TLayOutNFCom;
    FPConfiguracoesNFCom: TConfiguracoesNFCom;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: string; override;
    procedure FinalizarServico; override;
    procedure RemoverNameSpace;

  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusNFCom read FPStatus;
    property Layout: TLayOutNFCom read FPLayout;
  end;

  { TNFComStatusServico }

  TNFComStatusServico = class(TNFComWebService)
  private
    Fversao: string;
    FtpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: integer;
    FxMotivo: string;
    FcUF: integer;
    FdhRecbto: TDateTime;
    FTMed: integer;
    FdhRetorno: TDateTime;
    FxObs: string;
  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: string; override;
    function GerarMsgErro(E: Exception): string; override;
  public
    procedure Clear; override;

    property versao: string read Fversao;
    property tpAmb: TACBrTipoAmbiente read FtpAmb;
    property verAplic: string read FverAplic;
    property cStat: integer read FcStat;
    property xMotivo: string read FxMotivo;
    property cUF: integer read FcUF;
    property dhRecbto: TDateTime read FdhRecbto;
    property TMed: integer read FTMed;
    property dhRetorno: TDateTime read FdhRetorno;
    property xObs: string read FxObs;
  end;

  { TNFComRecepcao }

  TNFComRecepcao = class(TNFComWebService)
  private
    FRecibo: string;
    FNotasFiscais: TNotasFiscais;
    Fversao: string;
    FTpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: integer;
    FcUF: integer;
    FxMotivo: string;
    FdhRecbto: TDateTime;
    FTMed: integer;
    FVersaoDF: TVersaoNFCom;

    FNFComRetornoSincrono: TRetConsSitNFCom;
//    FNFComRetorno: TretEnvNFCom;
    FMsgUnZip: string;

    function GetRecibo: string;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: string; override;
    function GerarPrefixoArquivo: string; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property Recibo: string read GetRecibo;
    property versao: string read Fversao;
    property TpAmb: TACBrTipoAmbiente read FTpAmb;
    property verAplic: string read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;
    property xMotivo: string read FxMotivo;
    property dhRecbto: TDateTime read FdhRecbto;
    property TMed: integer read FTMed;
    property MsgUnZip: string read FMsgUnZip write FMsgUnZip;

    property NFComRetornoSincrono: TRetConsSitNFCom read FNFComRetornoSincrono;
  end;

  { TNFComRetRecepcao }

  TNFComRetRecepcao = class(TNFComWebService)
  private
    FRecibo: string;
    FProtocolo: string;
    FChaveNFCom: string;
    FNotasFiscais: TNotasFiscais;
    Fversao: string;
    FTpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: integer;
    FcUF: integer;
    FxMotivo: string;
    FcMsg: integer;
    FxMsg: string;
    FVersaoDF: TVersaoNFCom;

//    FNFComRetorno: TRetConsReciDFe;

    function GetRecibo: string;
    function TratarRespostaFinal: Boolean;
  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    procedure FinalizarServico; override;

    function GerarMsgLog: string; override;
    function GerarPrefixoArquivo: string; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    function Executar: Boolean; override;

    property versao: string read Fversao;
    property TpAmb: TACBrTipoAmbiente read FTpAmb;
    property verAplic: string read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;
    property xMotivo: string read FxMotivo;
    property cMsg: integer read FcMsg;
    property xMsg: string read FxMsg;
    property Recibo: string read GetRecibo write FRecibo;
    property Protocolo: string read FProtocolo write FProtocolo;
    property ChaveNFCom: string read FChaveNFCom write FChaveNFCom;

//    property NFComRetorno: TRetConsReciDFe read FNFComRetorno;
  end;

  { TNFComConsulta }

  TNFComConsulta = class(TNFComWebService)
  private
    FOwner: TACBrDFe;
    FNFComChave: string;
    FExtrairEventos: Boolean;
    FNotasFiscais: TNotasFiscais;
    FProtocolo: string;
    FDhRecbto: TDateTime;
    FXMotivo: string;
    Fversao: string;
    FTpAmb: TACBrTipoAmbiente;
    FverAplic: string;
    FcStat: integer;
    FcUF: integer;
    FRetNFComDFe: string;

    FprotNFCom: TProcDFe;
    FprocEventoNFCom: TRetEventoNFComCollection;

    procedure SetNFComChave(const AValue: string);
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function GerarUFSoap: string; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: string; override;
    function GerarPrefixoArquivo: string; override;
  public
    constructor Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
      reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property NFComChave: string read FNFComChave write SetNFComChave;
    property ExtrairEventos: Boolean read FExtrairEventos write FExtrairEventos;
    property Protocolo: string read FProtocolo;
    property DhRecbto: TDateTime read FDhRecbto;
    property XMotivo: string read FXMotivo;
    property versao: string read Fversao;
    property TpAmb: TACBrTipoAmbiente read FTpAmb;
    property verAplic: string read FverAplic;
    property cStat: integer read FcStat;
    property cUF: integer read FcUF;
    property RetNFComDFe: string read FRetNFComDFe;

    property protNFCom: TProcDFe read FprotNFCom;
    property procEventoNFCom: TRetEventoNFComCollection read FprocEventoNFCom;
  end;

  { TNFComEnvEvento }

  TNFComEnvEvento = class(TNFComWebService)
  private
    FidLote: Int64;
    FEvento: TEventoNFCom;
    FcStat: integer;
    FxMotivo: string;
    FTpAmb: TACBrTipoAmbiente;
    FCNPJ: string;

    FEventoRetorno: TRetEventoNFCom;

    function GerarPathEvento(const ACNPJ: string = ''; const AIE: string = ''): string;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: string; override;
    function GerarPrefixoArquivo: string; override;
  public
    constructor Create(AOwner: TACBrDFe; AEvento: TEventoNFCom); reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property idLote: Int64 read FidLote write FidLote;
    property cStat: integer read FcStat;
    property xMotivo: string read FxMotivo;
    property TpAmb: TACBrTipoAmbiente read FTpAmb;

    property EventoRetorno: TRetEventoNFCom read FEventoRetorno;
  end;

  { TNFComEnvioWebService }

  TNFComEnvioWebService = class(TNFComWebService)
  private
    FXMLEnvio: string;
    FPURLEnvio: string;
    FVersao: string;
    FSoapActionEnvio: string;
  protected
    procedure DefinirURL; override;
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgErro(E: Exception): string; override;
    function GerarVersaoDadosSoap: string; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    function Executar: Boolean; override;

    property Versao: string read FVersao;
    property XMLEnvio: string read FXMLEnvio write FXMLEnvio;
    property URLEnvio: string read FPURLEnvio write FPURLEnvio;
    property SoapActionEnvio: string read FSoapActionEnvio write FSoapActionEnvio;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrNFCom: TACBrDFe;
    FStatusServico: TNFComStatusServico;
    FEnviar: TNFComRecepcao;
    FRetorno: TNFComRetRecepcao;
    FConsulta: TNFComConsulta;
    FEnvEvento: TNFComEnvEvento;
    FEnvioWebService: TNFComEnvioWebService;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function Envia: Boolean;

    property ACBrNFCom: TACBrDFe read FACBrNFCom write FACBrNFCom;
    property StatusServico: TNFComStatusServico read FStatusServico
      write FStatusServico;
    property Enviar: TNFComRecepcao read FEnviar write FEnviar;
    property Retorno: TNFComRetRecepcao read FRetorno write FRetorno;
    property Consulta: TNFComConsulta read FConsulta write FConsulta;
    property EnvEvento: TNFComEnvEvento read FEnvEvento write FEnvEvento;
    property EnvioWebService: TNFComEnvioWebService read FEnvioWebService
      write FEnvioWebService;
  end;

implementation

uses
  StrUtils, Math,
//  ACBrDFeConsts,
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Strings, ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrCompress, ACBrNFCom, ACBrIntegrador,
  ACBrNFComConsts,
  ACBrNFComConsSit,
  ACBrDFeComum.ConsStatServ, ACBrDFeComum.RetConsStatServ,
  pcnConsReciDFe;

{ TNFComWebService }

constructor TNFComWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesNFCom := TConfiguracoesNFCom(FPConfiguracoes);
  FPLayout := LayNFComStatusServico;

  FPHeaderElement := ''; //'NFComCabecMsg';
  FPBodyElement := 'nfcomDadosMsg';
end;

procedure TNFComWebService.Clear;
begin
  inherited Clear;

  FPStatus := stNFComIdle;
  if Assigned(FPDFeOwner) and Assigned(FPDFeOwner.SSL) then
    FPDFeOwner.SSL.UseCertificateHTTP := True;
end;

procedure TNFComWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  FOldSSLType := FPDFeOwner.SSL.SSLType;
  FOldHeaderElement := FPHeaderElement;

  FPHeaderElement := '';

  TACBrNFCom(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TNFComWebService.RemoverNameSpace;
begin
  FPRetWS := StringReplace(FPRetWS, ' xmlns="http://www.portalfiscal.inf.br/nfcom"',
                                    '', [rfReplaceAll, rfIgnoreCase]);
end;

procedure TNFComWebService.DefinirURL;
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

  TACBrNFCom(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL, FPServico, FPSoapAction);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;

function TNFComWebService.GerarVersaoDadosSoap: string;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrNFCom(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TNFComWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  // Retornar configurações anteriores
  FPDFeOwner.SSL.SSLType := FOldSSLType;
  FPHeaderElement := FOldHeaderElement;

  TACBrNFCom(FPDFeOwner).SetStatus(stNFComIdle);
end;

{ TNFComStatusServico }

procedure TNFComStatusServico.Clear;
begin
  inherited Clear;

  FPStatus := stNFComStatusServico;
  FPLayout := LayNFComStatusServico;
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

  if Assigned(FPConfiguracoesNFCom) then
  begin
    FtpAmb := TACBrTipoAmbiente(FPConfiguracoesNFCom.WebServices.Ambiente);
    FcUF := FPConfiguracoesNFCom.WebServices.UFCodigo;
  end
end;

procedure TNFComStatusServico.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'NFComStatusServico';
  FPSoapAction := FPServico + '/nfcomStatusServicoNF';
end;

procedure TNFComStatusServico.DefinirDadosMsg;
var
  ConsStatServ: TConsStatServ;
begin
  ConsStatServ := TConsStatServ.Create(FPVersaoServico, NAME_SPACE_NFCom,
                                                                'NFCom', False);
  try
    ConsStatServ.TpAmb := FPConfiguracoesNFCom.WebServices.Ambiente;
    ConsStatServ.CUF := FPConfiguracoesNFCom.WebServices.UFCodigo;

    FPDadosMsg := ConsStatServ.GerarXML;
  finally
    ConsStatServ.Free;
  end;
end;

function TNFComStatusServico.TratarResposta: Boolean;
var
  NFComRetorno: TRetConsStatServ;
begin
  FPRetWS := SeparaDadosArray(['nfcomResultMsg'], FPRetornoWS);

  VerificarSemResposta;

  RemoverNameSpace;

  NFComRetorno := TRetConsStatServ.Create('NFCom');
  try
    NFComRetorno.XmlRetorno := ParseText(FPRetWS, True, False);
    NFComRetorno.LerXml;

    Fversao := NFComRetorno.versao;
    FtpAmb := NFComRetorno.tpAmb;
    FverAplic := NFComRetorno.verAplic;
    FcStat := NFComRetorno.cStat;
    FxMotivo := NFComRetorno.xMotivo;
    FcUF := NFComRetorno.cUF;

    { WebService do RS retorna horário de verão mesmo pros estados que não
      adotam esse horário, ao utilizar esta hora para basear a emissão da nota
      acontece o erro. }
    if (pos('svrs.rs.gov.br', FPURL) > 0) and
       (MinutesBetween(NFComRetorno.dhRecbto, Now) > 50) and
       (not IsHorarioDeVerao(CUFtoUF(FcUF), NFComRetorno.dhRecbto)) then
      FdhRecbto:= IncHour(NFComRetorno.dhRecbto,-1)
    else
      FdhRecbto := NFComRetorno.dhRecbto;

    FTMed := NFComRetorno.TMed;
    FdhRetorno := NFComRetorno.dhRetorno;
    FxObs := NFComRetorno.xObs;
    FPMsg := FxMotivo + LineBreak + FxObs;

    if Assigned(FPConfiguracoesNFCom) and
       Assigned(FPConfiguracoesNFCom.WebServices) and
       FPConfiguracoesNFCom.WebServices.AjustaAguardaConsultaRet then
      FPConfiguracoesNFCom.WebServices.AguardarConsultaRet := FTMed * 1000;

    Result := (FcStat = 107);

  finally
    NFComRetorno.Free;
  end;
end;

function TNFComStatusServico.GerarMsgLog: string;
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
                   [Fversao, TipoAmbienteToStr(FtpAmb), FverAplic, IntToStr(FcStat),
                    FxMotivo, CodigoParaUF(FcUF),
                    IfThen(FdhRecbto = 0, '', FormatDateTimeBr(FdhRecbto)),
                    IntToStr(FTMed),
                    IfThen(FdhRetorno = 0, '', FormatDateTimeBr(FdhRetorno)),
                    FxObs]);
  {*)}
end;

function TNFComStatusServico.GerarMsgErro(E: Exception): string;
begin
  Result := ACBrStr('WebService Consulta Status serviço:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

{ TNFComRecepcao }

constructor TNFComRecepcao.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFComRecepcao.Destroy;
begin
  FNFComRetornoSincrono.Free;
//  FNFComRetorno.Free;

  inherited Destroy;
end;

procedure TNFComRecepcao.Clear;
begin
  inherited Clear;

  FPStatus := stNFComRecepcao;
  FPLayout := LayNFComRecepcao;
  FPArqEnv := 'env';
  FPArqResp := 'rec';

  Fversao := '';
  FTMed := 0;
  FverAplic := '';
  FcStat    := 0;
  FxMotivo  := '';
  FRecibo   := '';
  FdhRecbto := 0;
  FMsgUnZip := '';

  if Assigned(FPConfiguracoesNFCom) then
  begin
    FtpAmb := TACBrTipoAmbiente(FPConfiguracoesNFCom.WebServices.Ambiente);
    FcUF := FPConfiguracoesNFCom.WebServices.UFCodigo;
  end;

  if Assigned(FNFComRetornoSincrono) then
    FNFComRetornoSincrono.Free;

//  if Assigned(FNFComRetorno) then
//    FNFComRetorno.Free;

  FNFComRetornoSincrono := TRetConsSitNFCom.Create;
//  FNFComRetorno := TretEnvNFCom.Create;
end;

function TNFComRecepcao.GetRecibo: string;
begin
  Result := Trim(FRecibo);
end;

procedure TNFComRecepcao.InicializarServico;
var
  ok: Boolean;
begin
  // Tem NFCom ? Se SIM, use as informações do XML
  if FNotasFiscais.Count > 0 then
    FVersaoDF := DblToVersaoNFCom(ok, FNotasFiscais.Items[0].NFCom.infNFCom.Versao)
  else
    FVersaoDF := FPConfiguracoesNFCom.Geral.VersaoDF;

  inherited InicializarServico;

  FPHeaderElement := '';
end;

procedure TNFComRecepcao.DefinirURL;
var
  xUF: string;
  VerServ: Double;
//  ok: Boolean;
begin
  // Tem NFCom ? Se SIM, use as informações do XML
  if FNotasFiscais.Count > 0 then
  begin
    FcUF := FNotasFiscais.Items[0].NFCom.Ide.cUF;

    if Integer(FPConfiguracoesNFCom.WebServices.Ambiente) <> Integer(FNotasFiscais.Items[0].NFCom.Ide.tpAmb) then
      raise EACBrNFComException.Create( ACBrNFCom_CErroAmbienteDiferente );
  end
  else
  begin // Se não tem NFCom, use as configurações do componente
    FcUF := FPConfiguracoesNFCom.WebServices.UFCodigo;
  end;

  VerServ := VersaoNFComToDbl(FVersaoDF);
  FTpAmb  := TACBrTipoAmbiente(FPConfiguracoesNFCom.WebServices.Ambiente);
  FPVersaoServico := '';
  FPURL := '';
  FPLayout := LayNFComRecepcao;

  // Configuração correta ao enviar para o SVC
  case FPConfiguracoesNFCom.Geral.FormaEmissao of
    teSVCAN: xUF := 'SVC-AN';
    teSVCRS: xUF := 'SVC-RS';
  else
    xUF := CUFtoUF(FcUF);
  end;

  TACBrNFCom(FPDFeOwner).LerServicoDeParams(
    'NFCom',
    xUF,
    TpcnTipoAmbiente(FTpAmb),
    LayOutNFComToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFComRecepcao.DefinirServicoEAction;
begin
  if EstaVazio(FPServico) then
    FPServico := GetUrlWsd + 'NFComRecepcao';

  if EstaVazio(FPSoapAction) then
    FPSoapAction := FPServico + '/nfComRecepcao';
end;

procedure TNFComRecepcao.DefinirDadosMsg;
begin
  // No envio só podemos ter apena UM NFCom-e, pois o seu processamento é síncrono
  if FNotasFiscais.Count > 1 then
    GerarException(ACBrStr('ERRO: Conjunto de NFCom-e transmitidos (máximo de 1 NFCom-e)' +
           ' excedido. Quantidade atual: ' + IntToStr(FNotasFiscais.Count)));

  if FNotasFiscais.Count > 0 then
    FPDadosMsg := '<NFCom' +
      RetornarConteudoEntre(FNotasFiscais.Items[0].XMLAssinado, '<NFCom', '</NFCom>') +
      '</NFCom>';

  FMsgUnZip := FPDadosMsg;

  FPDadosMsg := EncodeBase64(GZipCompress(FPDadosMsg));

  // Mensagem de Dados tem mais de 1 Mb ? //
  if Length(FPDadosMsg) > (1024 * 1024) then
    GerarException(ACBrStr('Tamanho do XML de Dados superior a 1 Mbytes. Tamanho atual: ' +
      IntToStr(trunc(Length(FPDadosMsg) / 1024)) + ' Kbytes'));

  FRecibo := '';
end;

function TNFComRecepcao.TratarResposta: Boolean;
var
  I: integer;
  chNFCom, AXML, NomeXMLSalvo: string;
  AProcNFCom: TProcDFe;
  SalvarXML: Boolean;
begin
  FPRetWS := SeparaDadosArray(['nfcomResultMsg'], FPRetornoWS );

  VerificarSemResposta;

  RemoverNameSpace;

  if pos('retNFCom', FPRetWS) > 0 then
    AXML := StringReplace(FPRetWS, 'retNFCom', 'retConsSitNFCom',
                                   [rfReplaceAll, rfIgnoreCase])
  else if pos('retConsReciNFCom', FPRetWS) > 0 then
    AXML := StringReplace(FPRetWS, 'retConsReciNFCom', 'retConsSitNFCom',
                                   [rfReplaceAll, rfIgnoreCase])
  else
    AXML := FPRetWS;

  FNFComRetornoSincrono.XmlRetorno := ParseText(AXML, True, False);
  FNFComRetornoSincrono.LerXml;

  Fversao := FNFComRetornoSincrono.versao;
  FTpAmb := FNFComRetornoSincrono.TpAmb;
  FverAplic := FNFComRetornoSincrono.verAplic;

  // Consta no Retorno da NFC-e
  FRecibo := FNFComRetornoSincrono.nRec;
  FcUF := FNFComRetornoSincrono.cUF;
  chNFCom := FNFComRetornoSincrono.ProtNFCom.chNFCom;

  if (FNFComRetornoSincrono.protNFCom.cStat > 0) then
    FcStat := FNFComRetornoSincrono.protNFCom.cStat
  else
    FcStat := FNFComRetornoSincrono.cStat;

  if (FNFComRetornoSincrono.protNFCom.xMotivo <> '') then
  begin
    FPMsg := FNFComRetornoSincrono.protNFCom.xMotivo;
    FxMotivo := FNFComRetornoSincrono.protNFCom.xMotivo;
  end
  else
  begin
    FPMsg := FNFComRetornoSincrono.xMotivo;
    FxMotivo := FNFComRetornoSincrono.xMotivo;
  end;

  // Verificar se a NFCom-e foi autorizada com sucesso
  Result := (FNFComRetornoSincrono.cStat = 104) and
    (TACBrNFCom(FPDFeOwner).CstatProcessado(FNFComRetornoSincrono.protNFCom.cStat));

  if Result then
  begin
    for I := 0 to TACBrNFCom(FPDFeOwner).NotasFiscais.Count - 1 do
    begin
      with TACBrNFCom(FPDFeOwner).NotasFiscais.Items[I] do
      begin
        if OnlyNumber(chNFCom) = NumID then
        begin
          if (FPConfiguracoesNFCom.Geral.ValidarDigest) and
             (FNFComRetornoSincrono.protNFCom.digVal <> '') and
             (NFCom.signature.DigestValue <> FNFComRetornoSincrono.protNFCom.digVal) then
          begin
            raise EACBrNFComException.Create('DigestValue do documento ' + NumID + ' não coNFComre.');
          end;

          NFCom.procNFCom.cStat := FNFComRetornoSincrono.protNFCom.cStat;
          NFCom.procNFCom.tpAmb := FNFComRetornoSincrono.tpAmb;
          NFCom.procNFCom.verAplic := FNFComRetornoSincrono.verAplic;
          NFCom.procNFCom.chNFCom := FNFComRetornoSincrono.ProtNFCom.chNFCom;
          NFCom.procNFCom.dhRecbto := FNFComRetornoSincrono.protNFCom.dhRecbto;
          NFCom.procNFCom.nProt := FNFComRetornoSincrono.ProtNFCom.nProt;
          NFCom.procNFCom.digVal := FNFComRetornoSincrono.protNFCom.digVal;
          NFCom.procNFCom.xMotivo := FNFComRetornoSincrono.protNFCom.xMotivo;

          AProcNFCom := TProcDFe.Create(FPVersaoServico, NAME_SPACE_NFCom, 'NFCom');
          try
            // Processando em UTF8, para poder gravar arquivo corretamente //
            AProcNFCom.XML_DFe := RemoverDeclaracaoXML(XMLAssinado);
            AProcNFCom.XML_Prot := FNFComRetornoSincrono.XMLprotNFCom;
            XMLOriginal := AProcNFCom.GerarXML;

            if FPConfiguracoesNFCom.Arquivos.Salvar then
            begin
              SalvarXML := Processada;

              // Salva o XML da NFCom-e assinado e protocolado
              if SalvarXML then
              begin
                NomeXMLSalvo := '';
                if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                begin
                  FPDFeOwner.Gravar( NomeArq, XMLOriginal ); // Atualiza o XML carregado
                  NomeXMLSalvo := NomeArq;
                end;

                if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                  GravarXML; // Salva na pasta baseado nas configurações do PathNFCom
              end;
            end ;
          finally
            AProcNFCom.Free;
          end;
          Break;
        end;
      end;
    end;
  end;
end;

function TNFComRecepcao.GerarMsgLog: string;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak +
                         'UF: %s ' + sLineBreak +
                         'dhRecbto: %s ' + sLineBreak +
                         'chNFCom: %s ' + LineBreak),
                   [FNFComRetornoSincrono.versao,
                    TipoAmbienteToStr(FNFComRetornoSincrono.TpAmb),
                    FNFComRetornoSincrono.verAplic,
                    IntToStr(FNFComRetornoSincrono.protNFCom.cStat),
                    FNFComRetornoSincrono.protNFCom.xMotivo,
                    CodigoParaUF(FNFComRetornoSincrono.cUF),
                    FormatDateTimeBr(FNFComRetornoSincrono.dhRecbto),
                    FNFComRetornoSincrono.chNFCom]);
end;

function TNFComRecepcao.GerarPrefixoArquivo: string;
begin
  if FRecibo <> '' then
  begin
    Result := Recibo;
    FPArqResp := 'pro-rec';
  end
  else
  begin
    Result := IntToStr(FNotasFiscais.Items[0].NFCom.Ide.nNF) + '-' +
              IntToStr(FNotasFiscais.Items[0].NFCom.Ide.serie);
    FPArqResp := 'pro-env';
  end;
end;

{ TNFComRetRecepcao }

constructor TNFComRetRecepcao.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FNotasFiscais := ANotasFiscais;
end;

destructor TNFComRetRecepcao.Destroy;
begin
//  FNFComRetorno.Free;

  inherited Destroy;
end;

function TNFComRetRecepcao.GetRecibo: string;
begin
  Result := Trim(FRecibo);
end;

procedure TNFComRetRecepcao.InicializarServico;
var
  ok: Boolean;
begin
  if FNotasFiscais.Count > 0 then    // Tem NFCom ? Se SIM, use as informações do XML
    FVersaoDF := DblToVersaoNFCom(ok, FNotasFiscais.Items[0].NFCom.infNFCom.Versao)
  else
    FVersaoDF := FPConfiguracoesNFCom.Geral.VersaoDF;

  inherited InicializarServico;

  FPHeaderElement := '';
end;

procedure TNFComRetRecepcao.Clear;
//var
//  i, j: Integer;
begin
  inherited Clear;

  FPStatus := stNFComRetRecepcao;
  FPLayout := LayNFComRetRecepcao;
  FPArqEnv := 'ped-rec';
  FPArqResp := 'pro-rec';

  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  Fversao := '';
  FxMsg := '';
  FcMsg := 0;

  if Assigned(FPConfiguracoesNFCom) then
  begin
    FtpAmb := TACBrTipoAmbiente(FPConfiguracoesNFCom.WebServices.Ambiente);
    FcUF := FPConfiguracoesNFCom.WebServices.UFCodigo;
  end;
 {
  if Assigned(FNFComRetorno) and Assigned(FNotasFiscais)
		and Assigned(FNFComRetorno.ProtDFe) then
  begin
    // Limpa Dados dos retornos das notas Fiscais;
    for i := 0 to FNFComRetorno.ProtDFe.Count - 1 do
    begin
      for j := 0 to FNotasFiscais.Count - 1 do
      begin
        if OnlyNumber(FNFComRetorno.ProtDFe.Items[i].chDFe) = FNotasFiscais.Items[J].NumID then
        begin
          FNotasFiscais.Items[j].NFCom.procNFCom.verAplic := '';
          FNotasFiscais.Items[j].NFCom.procNFCom.chNFCom    := '';
          FNotasFiscais.Items[j].NFCom.procNFCom.dhRecbto := 0;
          FNotasFiscais.Items[j].NFCom.procNFCom.nProt    := '';
          FNotasFiscais.Items[j].NFCom.procNFCom.digVal   := '';
          FNotasFiscais.Items[j].NFCom.procNFCom.cStat    := 0;
          FNotasFiscais.Items[j].NFCom.procNFCom.xMotivo  := '';
        end;
      end;
    end;
  end;

  if Assigned( FNFComRetorno ) then
    FreeAndNil(FNFComRetorno);

  FNFComRetorno := TRetConsReciDFe.Create('NFCom');
  }
end;

function TNFComRetRecepcao.Executar: Boolean;
var
  IntervaloTentativas, Tentativas: integer;
begin
  Result := False;

  TACBrNFCom(FPDFeOwner).SetStatus(stNFComRetRecepcao);
  try
    Sleep(FPConfiguracoesNFCom.WebServices.AguardarConsultaRet);

    Tentativas := 0;
    IntervaloTentativas := max(FPConfiguracoesNFCom.WebServices.IntervaloTentativas, 1000);

    while (inherited Executar) and
      (Tentativas < FPConfiguracoesNFCom.WebServices.Tentativas) do
    begin
      Inc(Tentativas);
      sleep(IntervaloTentativas);
    end;
  finally
    TACBrNFCom(FPDFeOwner).SetStatus(stNFComIdle);
  end;

//  if FNFComRetorno.CStat = 104 then  // Lote processado ?
//    Result := TratarRespostaFinal;
end;

procedure TNFComRetRecepcao.DefinirURL;
var
  xUF: string;
  VerServ: Double;
//  ok: Boolean;
begin
  if FNotasFiscais.Count > 0 then    // Tem NFCom ? Se SIM, use as informações do XML
  begin
    FcUF := FNotasFiscais.Items[0].NFCom.Ide.cUF;

    if Integer(FPConfiguracoesNFCom.WebServices.Ambiente) <> Integer(FNotasFiscais.Items[0].NFCom.Ide.tpAmb) then
      raise EACBrNFComException.Create( ACBrNFCom_CErroAmbienteDiferente );
  end
  else
  begin                              // Se não tem NFCom, use as configurações do componente
    FcUF := FPConfiguracoesNFCom.WebServices.UFCodigo;
  end;

  VerServ := VersaoNFComToDbl(FVersaoDF);
  FTpAmb := TACBrTipoAmbiente(FPConfiguracoesNFCom.WebServices.Ambiente);
  FPVersaoServico := '';
  FPURL := '';

  FPLayout := LayNFComRetRecepcao;

  // Configuração correta ao enviar para o SVC
  case FPConfiguracoesNFCom.Geral.FormaEmissao of
    teSVCAN: xUF := 'SVC-AN';
    teSVCRS: xUF := 'SVC-RS';
  else
    xUF := CUFtoUF(FcUF);
  end;

  TACBrNFCom(FPDFeOwner).LerServicoDeParams(
    'NFCom',
    xUF,
    TpcnTipoAmbiente(FTpAmb),
    LayOutNFComToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFComRetRecepcao.DefinirServicoEAction;
begin
//  if FPLayout = LayNFComRetAutorizacao then
//  begin
//    if EstaVazio(FPServico) then
//      FPServico := GetUrlWsd + 'NFComRetAutorizacao4';
//    if EstaVazio(FPSoapAction) then
//      FPSoapAction := FPServico +'/NFComRetAutorizacaoLote';
//  end
//  else
//  begin
    FPServico := GetUrlWsd + 'NFComRetRecepcao';
    FPSoapAction := FPServico + '/NFComRetRecepcao';
//  end;
end;

procedure TNFComRetRecepcao.DefinirDadosMsg;
var
  ConsReciNFCom: TConsReciDFe;
begin
  ConsReciNFCom := TConsReciDFe.Create(FPVersaoServico, NAME_SPACE_NFCom, 'NFCom');
  try
    ConsReciNFCom.tpAmb := TpcnTipoAmbiente(FTpAmb);
    ConsReciNFCom.nRec := FRecibo;

    AjustarOpcoes( ConsReciNFCom.Gerador.Opcoes );
    ConsReciNFCom.GerarXML;

    FPDadosMsg := ConsReciNFCom.Gerador.ArquivoFormatoXML;
  finally
    ConsReciNFCom.Free;
  end;
end;

function TNFComRetRecepcao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['nfComResultMsg'],FPRetornoWS );

  VerificarSemResposta;

  RemoverNameSpace;
  {
  FNFComRetorno.Leitor.Arquivo := ParseText(FPRetWS, True, False);
  FNFComRetorno.LerXML;

  Fversao := FNFComRetorno.versao;
  FTpAmb := FNFComRetorno.TpAmb;
  FverAplic := FNFComRetorno.verAplic;
  FcStat := FNFComRetorno.cStat;
  FcUF := FNFComRetorno.cUF;
  FPMsg := FNFComRetorno.xMotivo;
  FxMotivo := FNFComRetorno.xMotivo;
  FcMsg := FNFComRetorno.cMsg;
  FxMsg := FNFComRetorno.xMsg;

  Result := (FNFComRetorno.CStat = 105); // Lote em Processamento
  }
end;

function TNFComRetRecepcao.TratarRespostaFinal: Boolean;
var
  I{, J}: integer;
//  AProcNFCom: TProcNFCom;
//  AInfProt: TProtDFeCollection;
//  SalvarXML: Boolean;
//  NomeXMLSalvo: string;
begin
  Result := False;
{
  AInfProt := FNFComRetorno.ProtDFe;

  if (AInfProt.Count > 0) then
  begin
    FPMsg := FNFComRetorno.ProtDFe.Items[0].xMotivo;
    FxMotivo := FNFComRetorno.ProtDFe.Items[0].xMotivo;
  end;

  //Setando os retornos das notas fiscais;
  for I := 0 to AInfProt.Count - 1 do
  begin
    for J := 0 to FNotasFiscais.Count - 1 do
    begin
      if OnlyNumber(AInfProt.Items[I].chDFe) = FNotasFiscais.Items[J].NumID then
      begin
        if (FPConfiguracoesNFCom.Geral.ValidarDigest) and
           (AInfProt.Items[I].digVal <> '') and
           (FNotasFiscais.Items[J].NFCom.signature.DigestValue <> AInfProt.Items[I].digVal) then
        begin
          raise EACBrNFComException.Create('DigestValue do documento ' +
            FNotasFiscais.Items[J].NumID + ' não coNFComre.');
        end;

        with FNotasFiscais.Items[J] do
        begin
          NFCom.procNFCom.tpAmb := TACBrTipoAmbiente(AInfProt.Items[I].tpAmb);
          NFCom.procNFCom.verAplic := AInfProt.Items[I].verAplic;
          NFCom.procNFCom.chNFCom := AInfProt.Items[I].chDFe;
          NFCom.procNFCom.dhRecbto := AInfProt.Items[I].dhRecbto;
          NFCom.procNFCom.nProt := AInfProt.Items[I].nProt;
          NFCom.procNFCom.digVal := AInfProt.Items[I].digVal;
          NFCom.procNFCom.cStat := AInfProt.Items[I].cStat;
          NFCom.procNFCom.xMotivo := AInfProt.Items[I].xMotivo;
        end;

        // Monta o XML da NFCom-e assinado e com o protocolo de Autorização ou Denegação
        if (AInfProt.Items[I].cStat = 100) or (AInfProt.Items[I].cStat = 110) or
           (AInfProt.Items[I].cStat = 150) or (AInfProt.Items[I].cStat = 301) or
           (AInfProt.Items[I].cStat = 302) or (AInfProt.Items[I].cStat = 303) then
        begin
          AProcNFCom := TProcNFCom.Create;
          try
            AProcNFCom.XML_NFCom := RemoverDeclaracaoXML(FNotasFiscais.Items[J].XMLAssinado);
            AProcNFCom.XML_Prot := AInfProt.Items[I].XMLprotDFe;
            AProcNFCom.Versao := FPVersaoServico;

            with FNotasFiscais.Items[J] do
            begin
              XMLOriginal := AProcNFCom.GerarXML;

              if FPConfiguracoesNFCom.Arquivos.Salvar then
              begin
                SalvarXML := Processada;

                // Salva o XML da NFCom-e assinado e protocolado
                if SalvarXML then
                begin
                  NomeXMLSalvo := '';
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                  begin
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal );  // Atualiza o XML carregado
                    NomeXMLSalvo := NomeArq;
                  end;

                  if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                    GravarXML; // Salva na pasta baseado nas configurações do PathNFCom
                end;
              end;
            end;
          finally
            AProcNFCom.Free;
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
}
  //Montando a mensagem de retorno para as notas nao confirmadas
  for I := 0 to FNotasFiscais.Count - 1 do
  begin
    if not FNotasFiscais.Items[I].Confirmada then
      FPMsg := FPMsg + IntToStr(FNotasFiscais.Items[I].NFCom.Ide.nNF) +
        '->' + IntToStr(FNotasFiscais.Items[I].cStat)+'-'+ FNotasFiscais.Items[I].Msg + LineBreak;
  end;
  {
  if AInfProt.Count > 0 then
  begin
    FChaveNFCom := AInfProt.Items[0].chDFe;
    FProtocolo := AInfProt.Items[0].nProt;
    FcStat := AInfProt.Items[0].cStat;
  end;
  }
end;

procedure TNFComRetRecepcao.FinalizarServico;
begin
  // Sobrescrito, para não liberar para stIdle... não ainda...;

  // Retornar configurações anteriores
  FPDFeOwner.SSL.SSLType := FOldSSLType;
  FPHeaderElement := FOldHeaderElement;
end;

function TNFComRetRecepcao.GerarMsgLog: string;
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
                   [FNFComRetorno.versao, TpAmbToStr(FNFComRetorno.tpAmb),
                    FNFComRetorno.verAplic, FNFComRetorno.nRec,
                    IntToStr(FNFComRetorno.cStat), FNFComRetorno.xMotivo,
                    CodigoParaUF(FNFComRetorno.cUF), IntToStr(FNFComRetorno.cMsg),
                    FNFComRetorno.xMsg]);
  }
end;

function TNFComRetRecepcao.GerarPrefixoArquivo: string;
begin
  Result := Recibo;
end;

{ TNFComConsulta }

constructor TNFComConsulta.Create(AOwner: TACBrDFe; ANotasFiscais: TNotasFiscais);
begin
  inherited Create(AOwner);

  FOwner := AOwner;
  FNotasFiscais := ANotasFiscais;
end;

destructor TNFComConsulta.Destroy;
begin
  FprotNFCom.Free;
  FprocEventoNFCom.Free;

  inherited Destroy;
end;

procedure TNFComConsulta.Clear;
begin
  inherited Clear;

  FPStatus := stNFComConsulta;
  FPLayout := LayNFComConsulta;
  FPArqEnv := 'ped-sit';
  FPArqResp := 'sit';

  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  FProtocolo := '';
  FDhRecbto := 0;
  Fversao := '';
  FRetNFComDFe := '';

  if Assigned(FPConfiguracoesNFCom) then
  begin
    FtpAmb := TACBrTipoAmbiente(FPConfiguracoesNFCom.WebServices.Ambiente);
    FcUF := FPConfiguracoesNFCom.WebServices.UFCodigo;
  end;

  if Assigned(FprotNFCom) then
    FprotNFCom.Free;

  if Assigned(FprocEventoNFCom) then
    FprocEventoNFCom.Free;

  FprotNFCom := TProcDFe.Create(FPVersaoServico, NAME_SPACE_NFCom, 'NFCom');
  FprocEventoNFCom := TRetEventoNFComCollection.Create;
end;

procedure TNFComConsulta.SetNFComChave(const AValue: string);
var
  NumChave: string;
begin
  if FNFComChave = AValue then Exit;
  NumChave := OnlyNumber(AValue);

  if not ValidarChave(NumChave) then
     raise EACBrNFComException.Create('Chave "' + AValue + '" inválida.');

  FNFComChave := NumChave;
end;

procedure TNFComConsulta.DefinirURL;
var
  VerServ: Double;
  xUF: string;
//  ok: Boolean;
begin
  FPVersaoServico := '';

  FPURL   := '';
  FcUF    := ExtrairUFChaveAcesso(FNFComChave);
  VerServ := VersaoNFComToDbl(FPConfiguracoesNFCom.Geral.VersaoDF);

  if FNotasFiscais.Count > 0 then
    FTpAmb := FNotasFiscais.Items[0].NFCom.Ide.tpAmb
  else
    FTpAmb := TACBrTipoAmbiente(FPConfiguracoesNFCom.WebServices.Ambiente);

  // Se a nota foi enviada para o SVC a consulta tem que ser realizada no SVC e
  // não na SEFAZ-Autorizadora
  case FPConfiguracoesNFCom.Geral.FormaEmissao of
    teSVCAN: xUF := 'SVC-AN';
    teSVCRS: xUF := 'SVC-RS';
  else
    xUF := CUFtoUF(FcUF);
  end;

  TACBrNFCom(FPDFeOwner).LerServicoDeParams(
    'NFCom',
    xUF,
    TpcnTipoAmbiente(FTpAmb),
    LayOutNFComToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFComConsulta.DefinirServicoEAction;
begin
  if EstaVazio(FPServico) then
    FPServico := GetUrlWsd + 'NFComConsulta';

  if EstaVazio(FPSoapAction) then
    FPSoapAction := FPServico + '/nfcomConsultaNF';
end;

procedure TNFComConsulta.DefinirDadosMsg;
var
  ConsSitNFCom: TConsSitNFCom;
begin
  ConsSitNFCom := TConsSitNFCom.Create;

  try
    ConsSitNFCom.TpAmb := TACBrTipoAmbiente(FTpAmb);
    ConsSitNFCom.chNFCom := FNFComChave;
    ConsSitNFCom.Versao := FPVersaoServico;
    FPDadosMsg := ConsSitNFCom.GerarXML;
  finally
    ConsSitNFCom.Free;
  end;
end;

function TNFComConsulta.GerarUFSoap: string;
begin
  Result := '<cUF>' + IntToStr(FcUF) + '</cUF>';
end;

function TNFComConsulta.TratarResposta: Boolean;

procedure SalvarEventos(Retorno: string);
var
  aEvento, aProcEvento, aIDEvento, sPathEvento, sCNPJ: string;
  Inicio, Fim: Integer;
  TipoEvento: TpcnTpEvento;
  Ok: Boolean;
begin
  while Retorno <> '' do
  begin
    Inicio := Pos('<procEventoNFCom', Retorno);
    Fim    := Pos('</procEventoNFCom>', Retorno) + 15;

    aEvento := Copy(Retorno, Inicio, Fim - Inicio + 1);

    Retorno := Copy(Retorno, Fim + 1, Length(Retorno));

    aProcEvento := '<procEventoNFCom versao="' + FVersao + '" xmlns="' + ACBRNFCom_NAMESPACE + '">' +
                      SeparaDados(aEvento, 'procEventoNFCom') +
                   '</procEventoNFCom>';

    Inicio := Pos('Id=', aProcEvento) + 6;
    Fim    := 52;

    if Inicio = 6 then
      aIDEvento := FormatDateTime('yyyymmddhhnnss', Now)
    else
      aIDEvento := Copy(aProcEvento, Inicio, Fim);

    TipoEvento  := StrToTpEventoNFCom(Ok, SeparaDados(aEvento, 'tpEvento'));
    sCNPJ       := SeparaDados(aEvento, 'CNPJ');
    sPathEvento := PathWithDelim(FPConfiguracoesNFCom.Arquivos.GetPathEvento(TipoEvento, sCNPJ));

    if (aProcEvento <> '') then
      FPDFeOwner.Gravar( aIDEvento + '-procEventoNFCom.xml', aProcEvento, sPathEvento);
  end;
end;

var
  NFComRetorno: TRetConsSitNFCom;
  SalvarXML, NFCancelada, Atualiza: Boolean;
  aEventos, sPathNFCom, NomeXMLSalvo: string;
  AProcNFCom: TProcDFe;
  I,
//  J,
  Inicio, Fim: integer;
  dhEmissao: TDateTime;
begin
  NFComRetorno := TRetConsSitNFCom.Create;

  try
    FPRetWS := SeparaDadosArray(['nfcomResultMsg'], FPRetornoWS );

    VerificarSemResposta;

    RemoverNameSpace;

    NFComRetorno.XmlRetorno := ParseText(FPRetWS, True, False);
    NFComRetorno.LerXML;

    NFCancelada := False;
    aEventos := '';

    // <retConsSitNFCom> - Retorno da consulta da situação da NFCom-e
    // Este é o status oficial da NFCom-e
    Fversao := NFComRetorno.versao;
    FTpAmb := NFComRetorno.tpAmb;
    FverAplic := NFComRetorno.verAplic;
    FcStat := NFComRetorno.cStat;
    FXMotivo := NFComRetorno.xMotivo;
    FcUF := NFComRetorno.cUF;
//    FNFComChave := NFComRetorno.chNFCom;
    FPMsg := FXMotivo;

    // <protNFCom> - Retorno dos dados do ENVIO da NFCom-e
    // Considerá-los apenas se não existir nenhum evento de cancelamento (110111)
    FprotNFCom.PathDFe := NFComRetorno.protNFCom.PathNFCom;
    FprotNFCom.PathRetConsReciDFe := NFComRetorno.protNFCom.PathRetConsReciNFCom;
    FprotNFCom.PathRetConsSitDFe := NFComRetorno.protNFCom.PathRetConsSitNFCom;
    FprotNFCom.tpAmb := NFComRetorno.protNFCom.tpAmb;
    FprotNFCom.verAplic := NFComRetorno.protNFCom.verAplic;
    FprotNFCom.chDFe := NFComRetorno.protNFCom.chNFCom;
    FprotNFCom.dhRecbto := NFComRetorno.protNFCom.dhRecbto;
    FprotNFCom.nProt := NFComRetorno.protNFCom.nProt;
    FprotNFCom.digVal := NFComRetorno.protNFCom.digVal;
    FprotNFCom.cStat := NFComRetorno.protNFCom.cStat;
    FprotNFCom.xMotivo := NFComRetorno.protNFCom.xMotivo;

    {(*}
    if Assigned(NFComRetorno.procEventoNFCom) and (NFComRetorno.procEventoNFCom.Count > 0) then
    begin
      aEventos := '=====================================================' +
        LineBreak + '================= Eventos da NFCom-e =================' +
        LineBreak + '=====================================================' +
        LineBreak + '' + LineBreak + 'Quantidade total de eventos: ' +
        IntToStr(NFComRetorno.procEventoNFCom.Count);

      FprocEventoNFCom.Clear;
      for I := 0 to NFComRetorno.procEventoNFCom.Count - 1 do
      begin
        with FprocEventoNFCom.New.RetEventoNFCom.retInfEvento do
        begin
//          idLote := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.idLote;
          tpAmb := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.tpAmb;
          verAplic := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.verAplic;
          cOrgao := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.cOrgao;
          cStat := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.cStat;
          xMotivo := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.xMotivo;
 //         XML := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.XML;

          ID := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.ID;
          tpAmb := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.tpAmb;
//          CNPJ := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.CNPJ;
          chNFCom := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.chNFCom;
//          dhEvento := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.dhEvento;
          TpEvento := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.TpEvento;
          nSeqEvento := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.nSeqEvento;
          nProt := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.nProt;
//          xJust := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retInfEvento.xJust;

          {
          retEvento.Clear;
          for J := 0 to NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Count-1 do
          begin
            with retEvento.New.RetInfEvento do
            begin
              Id := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.Id;
              tpAmb := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.tpAmb;
              verAplic := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.verAplic;
              cOrgao := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.cOrgao;
              cStat := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.cStat;
              xMotivo := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.xMotivo;
              chNFCom := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.chNFCom;
              tpEvento := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.tpEvento;
              xEvento := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.xEvento;
              nSeqEvento := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.nSeqEvento;
              CNPJDest := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.CNPJDest;
              emailDest := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.emailDest;
              dhRegEvento := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.dhRegEvento;
              nProt := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.nProt;
              XML := NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom.retEvento.Items[J].RetInfEvento.XML;
            end;
          end;
          }
        end;
        {
        with NFComRetorno.procEventoNFCom.Items[I].RetEventoNFCom do
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
                     [IntToStr(retInfEvento.nSeqEvento),
                      TpEventoToStr(retInfEvento.TpEvento),
                      retInfEvento.DescEvento,
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
        }
      end;
    end;
    {*)}

    if (not NFCancelada) and (NaoEstaVazio(NFComRetorno.protNFCom.nProt))  then
    begin
      FProtocolo := NFComRetorno.protNFCom.nProt;
      FDhRecbto := NFComRetorno.protNFCom.dhRecbto;
      FPMsg := NFComRetorno.protNFCom.xMotivo;
    end;

    if not Assigned(FPDFeOwner) then //evita AV caso não atribua o Owner
    begin
     Result := True;
     Exit;
    end;

    with TACBrNFCom(FPDFeOwner) do
    begin
      Result := CstatProcessado(NFComRetorno.CStat) or
                CstatCancelada(NFComRetorno.CStat);
    end;

    if Result then
    begin
      if TACBrNFCom(FPDFeOwner).NotasFiscais.Count > 0 then
      begin
        for i := 0 to TACBrNFCom(FPDFeOwner).NotasFiscais.Count - 1 do
        begin
          with TACBrNFCom(FPDFeOwner).NotasFiscais.Items[i] do
          begin
            if (OnlyNumber(FNFComChave) = NumID) then
            begin
              Atualiza := (NaoEstaVazio(NFComRetorno.XMLprotNFCom));
              if Atualiza and
                 TACBrNFCom(FPDFeOwner).CstatCancelada(NFComRetorno.CStat) then
                Atualiza := False;

              // No retorno pode constar que a nota esta cancelada, mas traz o grupo
              // <protNFCom> com as informações da sua autorização
              if not Atualiza and TACBrNFCom(FPDFeOwner).cstatProcessado(NFComRetorno.protNFCom.cStat) then
                Atualiza := True;

              if (FPConfiguracoesNFCom.Geral.ValidarDigest) and
                (NFComRetorno.protNFCom.digVal <> '') and (NFCom.signature.DigestValue <> '') and
                (UpperCase(NFCom.signature.DigestValue) <> UpperCase(NFComRetorno.protNFCom.digVal)) then
              begin
                raise EACBrNFComException.Create('DigestValue do documento ' +
                  NumID + ' não confere.');
              end;

              // Atualiza o Status da NFCom interna //
              NFCom.procNFCom.cStat := NFComRetorno.cStat;

              if Atualiza then
              begin
                if TACBrNFCom(FPDFeOwner).CstatCancelada(NFComRetorno.CStat) then
                begin
                  NFCom.procNFCom.tpAmb := NFComRetorno.tpAmb;
                  NFCom.procNFCom.verAplic := NFComRetorno.verAplic;
//                  NFCom.procNFCom.chNFCom := NFComRetorno.chNFCom;
                  NFCom.procNFCom.dhRecbto := FDhRecbto;
                  NFCom.procNFCom.nProt := FProtocolo;
                  NFCom.procNFCom.digVal := NFComRetorno.protNFCom.digVal;
                  NFCom.procNFCom.cStat := NFComRetorno.cStat;
                  NFCom.procNFCom.xMotivo := NFComRetorno.xMotivo;
                  NFCom.procNFCom.Versao := NFComRetorno.protNFCom.Versao;

                  GerarXML;
                end
                else
                begin
                  NFCom.procNFCom.tpAmb := NFComRetorno.protNFCom.tpAmb;
                  NFCom.procNFCom.verAplic := NFComRetorno.protNFCom.verAplic;
                  NFCom.procNFCom.chNFCom := NFComRetorno.protNFCom.chNFCom;
                  NFCom.procNFCom.dhRecbto := NFComRetorno.protNFCom.dhRecbto;
                  NFCom.procNFCom.nProt := NFComRetorno.protNFCom.nProt;
                  NFCom.procNFCom.digVal := NFComRetorno.protNFCom.digVal;
                  NFCom.procNFCom.cStat := NFComRetorno.protNFCom.cStat;
                  NFCom.procNFCom.xMotivo := NFComRetorno.protNFCom.xMotivo;
                  NFCom.procNFCom.Versao := NFComRetorno.protNFCom.Versao;

                  // O código abaixo é bem mais rápido que "GerarXML" (acima)...
                  AProcNFCom := TProcDFe.Create(FPVersaoServico, NAME_SPACE_NFCom, 'NFCom');;
                  try
                    AProcNFCom.XML_DFe := RemoverDeclaracaoXML(XMLOriginal);
                    AProcNFCom.XML_Prot := NFComRetorno.XMLprotNFCom;

                    XMLOriginal := AProcNFCom.GerarXML;
                  finally
                    AProcNFCom.Free;
                  end;
                end;
              end;

              { Se no retorno da consulta constar que a nota possui eventos vinculados
               será disponibilizado na propriedade FRetNFComDFe, e conforme configurado
               em "ConfiguracoesNFCom.Arquivos.Salvar", também será gerado o arquivo:
               <chave>-NFComDFe.xml}

              FRetNFComDFe := '';

              if (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoNFCom'))) then
              begin
                Inicio := Pos('<procEventoNFCom', FPRetWS);
                Fim    := Pos('</retConsSitNFCom', FPRetWS) -1;

                aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

                FRetNFComDFe := '<' + ENCODING_UTF8 + '>' +
                                '<NFComDFe>' +
                                 '<nfComProc versao="' + FVersao + '">' +
                                   SeparaDados(XMLOriginal, 'nfComProc') +
                                 '</nfComProc>' +
                                 '<procEventoNFCom versao="' + FVersao + '">' +
                                   aEventos +
                                 '</procEventoNFCom>' +
                                '</NFComDFe>';
              end;

              SalvarXML := Result and FPConfiguracoesNFCom.Arquivos.Salvar and Atualiza;

              if SalvarXML then
              begin
                // Salva o XML da NFCom-e assinado, protocolado e com os eventos
                if FPConfiguracoesNFCom.Arquivos.EmissaoPathNFCom then
                  dhEmissao := NFCom.Ide.dhEmi
                else
                  dhEmissao := Now;

                sPathNFCom := PathWithDelim(FPConfiguracoesNFCom.Arquivos.GetPathNFCom(dhEmissao, NFCom.Emit.CNPJ));

                if (FRetNFComDFe <> '') then
                  FPDFeOwner.Gravar( FNFComChave + '-NFComDFe.xml', FRetNFComDFe, sPathNFCom);

                if Atualiza then
                begin
                  // Salva o XML da NFCom-e assinado e protocolado
                  NomeXMLSalvo := '';
                  if NaoEstaVazio(NomeArq) and FileExists(NomeArq) then
                  begin
                    FPDFeOwner.Gravar( NomeArq, XMLOriginal );  // Atualiza o XML carregado
                    NomeXMLSalvo := NomeArq;
                  end;

                  // Salva na pasta baseado nas configurações do PathNFCom
                  if (NomeXMLSalvo <> CalcularNomeArquivoCompleto()) then
                    GravarXML;

                  // Salva o XML de eventos retornados ao consultar um NFCom-e
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
        if ExtrairEventos and FPConfiguracoesNFCom.Arquivos.Salvar and
           (NaoEstaVazio(SeparaDados(FPRetWS, 'procEventoNFCom'))) then
        begin
          Inicio := Pos('<procEventoNFCom', FPRetWS);
          Fim    := Pos('</retConsSitNFCom', FPRetWS) -1;

          aEventos := Copy(FPRetWS, Inicio, Fim - Inicio + 1);

          // Salva o XML de eventos retornados ao consultar um NFCom-e
          SalvarEventos(aEventos);
        end;
      end;
    end;
  finally
    NFComRetorno.Free;
  end;
end;

function TNFComConsulta.GerarMsgLog: string;
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
                   [Fversao, FNFComChave, TipoAmbienteToStr(FtpAmb), FverAplic,
                    IntToStr(FcStat), FXMotivo, CodigoParaUF(FcUF), FNFComChave,
                    FormatDateTimeBr(FDhRecbto), FProtocolo, FprotNFCom.digVal]);
  {*)}
end;

function TNFComConsulta.GerarPrefixoArquivo: string;
begin
  Result := Trim(FNFComChave);
end;

{ TNFComEnvEvento }

constructor TNFComEnvEvento.Create(AOwner: TACBrDFe; AEvento: TEventoNFCom);
begin
  inherited Create(AOwner);

  FEvento := AEvento;
end;

destructor TNFComEnvEvento.Destroy;
begin
  if Assigned(FEventoRetorno) then
    FEventoRetorno.Free;

  inherited Destroy;
end;

procedure TNFComEnvEvento.Clear;
begin
  inherited Clear;

  FPStatus := stNFComEvento;
  FPLayout := LayNFComEvento;
  FPArqEnv := 'ped-eve';
  FPArqResp := 'eve';

  FcStat   := 0;
  FxMotivo := '';
  FCNPJ := '';

  if Assigned(FPConfiguracoesNFCom) then
    FtpAmb := TACBrTipoAmbiente(FPConfiguracoesNFCom.WebServices.Ambiente);

  if Assigned(FEventoRetorno) then
    FEventoRetorno.Free;

  FEventoRetorno := TRetEventoNFCom.Create;
end;

function TNFComEnvEvento.GerarPathEvento(const ACNPJ: string; const AIE: string): string;
begin
  with FEvento.Evento.Items[0].InfEvento do
  begin
    Result := FPConfiguracoesNFCom.Arquivos.GetPathEvento(tpEvento, ACNPJ, AIE);
  end;
end;

procedure TNFComEnvEvento.DefinirURL;
var
  UF: string;
  VerServ: Double;
//  ok: Boolean;
begin
  { Verificação necessária pois somente os eventos de Cancelamento e CCe serão tratados pela SEFAZ do estado
    os outros eventos como manifestacao de destinatários serão tratados diretamente pela RFB }

  FPLayout := LayNFComEvento;
  VerServ  := VersaoNFComToDbl(FPConfiguracoesNFCom.Geral.VersaoDF);
  FCNPJ    := FEvento.Evento.Items[0].InfEvento.CNPJ;
  FTpAmb   := FEvento.Evento.Items[0].InfEvento.tpAmb;

  // Configuração correta ao enviar para o SVC
  case FPConfiguracoesNFCom.Geral.FormaEmissao of
    teSVCAN: UF := 'SVC-AN';
    teSVCRS: UF := 'SVC-RS';
  else
    UF := CUFtoUF(ExtrairUFChaveAcesso(FEvento.Evento.Items[0].InfEvento.chNFCom));
  end;

  FPURL := '';

  TACBrNFCom(FPDFeOwner).LerServicoDeParams(
    'NFCom',
    UF,
    TpcnTipoAmbiente(FTpAmb),
    LayOutNFComToServico(FPLayout),
    VerServ,
    FPURL,
    FPServico,
    FPSoapAction);

  FPVersaoServico := FloatToString(VerServ, '.', '0.00');
end;

procedure TNFComEnvEvento.DefinirServicoEAction;
begin
  if EstaVazio(FPServico) then
    FPServico := GetUrlWsd + 'NFComRecepcaoEvento';

  if EstaVazio(FPSoapAction) then
    FPSoapAction := FPServico + '/nfcomRecepcaoEvento';
end;

procedure TNFComEnvEvento.DefinirDadosMsg;
var
  EventoNFCom: TEventoNFCom;
  I: integer;
//  Lote, Evento, Eventos, EventosAssinados,
  AXMLEvento: AnsiString;
  FErroValidacao: string;
//  MsgEventoEhValido,
  EventoEhValido: Boolean;
  SchemaEventoNFCom: TSchemaNFCom;
begin
  EventoNFCom := TEventoNFCom.Create;
  try
    EventoNFCom.idLote := FidLote;
    SchemaEventoNFCom  := schErroNFCom;
    {(*}
    for I := 0 to FEvento.Evento.Count - 1 do
    begin
      with EventoNFCom.Evento.New do
      begin
        InfEvento.tpAmb := TACBrTipoAmbiente(FTpAmb);
        infEvento.CNPJ := FEvento.Evento[I].InfEvento.CNPJ;
        infEvento.cOrgao := FEvento.Evento[I].InfEvento.cOrgao;
        infEvento.chNFCom := FEvento.Evento[I].InfEvento.chNFCom;
        infEvento.dhEvento := FEvento.Evento[I].InfEvento.dhEvento;
        infEvento.tpEvento := FEvento.Evento[I].InfEvento.tpEvento;
        infEvento.nSeqEvento := FEvento.Evento[I].InfEvento.nSeqEvento;

        case InfEvento.tpEvento of
          teCancelamento:
            begin
              SchemaEventoNFCom := schevCancNFCom;
              infEvento.detEvento.nProt := FEvento.Evento[I].InfEvento.detEvento.nProt;
              infEvento.detEvento.xJust := FEvento.Evento[I].InfEvento.detEvento.xJust;
            end;
        end;
      end;
    end;
    {*)}

    EventoNFCom.Versao := FPVersaoServico;
    EventoNFCom.GerarXML;

    AssinarXML(EventoNFCom.Xml, 'eventoNFCom', 'infEvento', 'Falha ao assinar o Envio de Evento ');

    // Separa o XML especifico do Evento para ser Validado.
    AXMLEvento := SeparaDados(FPDadosMsg, 'detEvento');

    case SchemaEventoNFCom of
      schevCancNFCom:
        begin
          AXMLEvento := '<evCancNFCom xmlns="' + ACBRNFCom_NAMESPACE + '">' +
                          Trim(RetornarConteudoEntre(AXMLEvento, '<evCancNFCom>', '</evCancNFCom>')) +
                        '</evCancNFCom>';
        end;
    end;

    AXMLEvento := '<' + ENCODING_UTF8 + '>' + AXMLEvento;

    with TACBrNFCom(FPDFeOwner) do
    begin
      EventoEhValido := SSL.Validar(FPDadosMsg,
                                    GerarNomeArqSchema(FPLayout,
                                      StringToFloatDef(FPVersaoServico, 0)),
                                      FPMsg) and
                        SSL.Validar(AXMLEvento,
                                    GerarNomeArqSchemaEvento(SchemaEventoNFCom,
                                      StringToFloatDef(FPVersaoServico, 0)),
                                      FPMsg);
    end;

    if not EventoEhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados do Evento: ') +
        FPMsg;

      raise EACBrNFComException.CreateDef(FErroValidacao);
    end;

    for I := 0 to FEvento.Evento.Count - 1 do
      FEvento.Evento[I].InfEvento.id := EventoNFCom.Evento[I].InfEvento.id;
  finally
    EventoNFCom.Free;
  end;
end;

function TNFComEnvEvento.TratarResposta: Boolean;
//var
//  Leitor: TLeitor;
//  I, J: integer;
//  NomeArq, PathArq, VersaoEvento, Texto: string;
begin
  FEvento.idLote := idLote;

  FPRetWS := SeparaDadosArray(['nfComResultMsg'], FPRetornoWS );

  VerificarSemResposta;

  RemoverNameSpace;

  EventoRetorno.XmlRetorno := ParseText(FPRetWS, True, False);
  EventoRetorno.LerXml;

  FcStat := EventoRetorno.retInfEvento.cStat;
  FxMotivo := EventoRetorno.retInfEvento.xMotivo;
  FPMsg := EventoRetorno.retInfEvento.xMotivo;
  FTpAmb := EventoRetorno.retInfEvento.tpAmb;

  Result := (FcStat = 128);

  //gerar arquivo proc de evento
  (*
  if Result then
  begin
    Leitor := TLeitor.Create;
    try
      for I := 0 to FEvento.Evento.Count - 1 do
      begin
        for J := 0 to EventoRetorno.retEvento.Count - 1 do
        begin
          if FEvento.Evento.Items[I].InfEvento.chNFCom =
            EventoRetorno.retEvento.Items[J].RetInfEvento.chNFCom then
          begin
            FEvento.Evento.Items[I].RetInfEvento.tpAmb := EventoRetorno.retEvento.Items[J].RetInfEvento.tpAmb;
            FEvento.Evento.Items[I].RetInfEvento.nProt := EventoRetorno.retEvento.Items[J].RetInfEvento.nProt;
            FEvento.Evento.Items[I].RetInfEvento.dhRegEvento := EventoRetorno.retEvento.Items[J].RetInfEvento.dhRegEvento;
            FEvento.Evento.Items[I].RetInfEvento.cStat := EventoRetorno.retEvento.Items[J].RetInfEvento.cStat;
            FEvento.Evento.Items[I].RetInfEvento.xMotivo := EventoRetorno.retEvento.Items[J].RetInfEvento.xMotivo;

            Texto := '';

            if EventoRetorno.retEvento.Items[J].RetInfEvento.cStat in [135, 136, 155] then
            begin
              VersaoEvento := TACBrNFCom(FPDFeOwner).LerVersaoDeParams(LayNFComEvento);

              Leitor.Arquivo := FPDadosMsg;
              Texto := '<procEventoNFCom versao="' + VersaoEvento + '" xmlns="' + ACBRNFCom_NAMESPACE + '">' +
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
                        '</procEventoNFCom>';

              if FPConfiguracoesNFCom.Arquivos.Salvar then
              begin
                NomeArq := OnlyNumber(FEvento.Evento.Items[i].InfEvento.Id) + '-procEventoNFCom.xml';
                PathArq := PathWithDelim(GerarPathEvento(FEvento.Evento.Items[I].InfEvento.CNPJ));

                FPDFeOwner.Gravar(NomeArq, Texto, PathArq);
                FEventoRetorno.retEvento.Items[J].RetInfEvento.NomeArquivo := PathArq + NomeArq;
                FEvento.Evento.Items[I].RetInfEvento.NomeArquivo := PathArq + NomeArq;
              end;

              { Converte de UTF8 para a String nativa e Decodificar caracteres HTML Entity }
              Texto := ParseText(Texto, True, False);
            end;

            // Se o evento for rejeitado a propriedade XML conterá uma string vazia
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

function TNFComEnvEvento.GerarMsgLog: string;
var
  aMsg: string;
begin
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak),
                 [FEventoRetorno.versao, TipoAmbienteToStr(FEventoRetorno.retInfEvento.tpAmb),
                  FEventoRetorno.retInfEvento.verAplic, IntToStr(FEventoRetorno.retInfEvento.cStat),
                  FEventoRetorno.retInfEvento.xMotivo]);
  {
  if FEventoRetorno.retEvento.Count > 0 then
    aMsg := aMsg + Format(ACBrStr('Recebimento: %s ' + LineBreak),
       [IfThen(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento = 0, '',
               FormatDateTimeBr(FEventoRetorno.retEvento.Items[0].RetInfEvento.dhRegEvento))]);
  }
  Result := aMsg;
end;

function TNFComEnvEvento.GerarPrefixoArquivo: string;
begin
//  Result := IntToStr(FEvento.idLote);
  Result := IntToStr(FidLote);
end;

{ TNFComEnvioWebService }

constructor TNFComEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stNFComEnvioWebService;
end;

destructor TNFComEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

procedure TNFComEnvioWebService.Clear;
begin
  inherited Clear;

  FVersao := '';
end;

function TNFComEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TNFComEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TNFComEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TNFComEnvioWebService.DefinirDadosMsg;
//var
//  LeitorXML: TLeitor;
begin
{
  LeitorXML := TLeitor.Create;

  try
    LeitorXML.Arquivo := FXMLEnvio;
    LeitorXML.Grupo := FXMLEnvio;
    FVersao := LeitorXML.rAtributo('versao')
  finally
    LeitorXML.Free;
  end;
}
  FPDadosMsg := FXMLEnvio;
end;

function TNFComEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');

  VerificarSemResposta;

  RemoverNameSpace;

  Result := True;
end;

function TNFComEnvioWebService.GerarMsgErro(E: Exception): string;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TNFComEnvioWebService.GerarVersaoDadosSoap: string;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TWebServices }

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrNFCom := TACBrNFCom(AOwner);

  FStatusServico := TNFComStatusServico.Create(FACBrNFCom);
  FEnviar := TNFComRecepcao.Create(FACBrNFCom, TACBrNFCom(FACBrNFCom).NotasFiscais);
//  FRetorno := TNFComRetRecepcao.Create(FACBrNFCom, TACBrNFCom(FACBrNFCom).NotasFiscais);
  FConsulta := TNFComConsulta.Create(FACBrNFCom, TACBrNFCom(FACBrNFCom).NotasFiscais);
  FEnvEvento := TNFComEnvEvento.Create(FACBrNFCom, TACBrNFCom(FACBrNFCom).EventoNFCom);
  FEnvioWebService := TNFComEnvioWebService.Create(FACBrNFCom);
end;

destructor TWebServices.Destroy;
begin
  FStatusServico.Free;
  FEnviar.Free;
//  FRetorno.Free;
  FConsulta.Free;
  FEnvEvento.Free;
  FEnvioWebService.Free;

  inherited Destroy;
end;

function TWebServices.Envia: Boolean;
begin
  FEnviar.Clear;
//  FRetorno.Clear;

  if not Enviar.Executar then
    Enviar.GerarException( Enviar.Msg );

  Result := True;
end;

end.
