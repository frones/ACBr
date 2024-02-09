{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrONEWebServices;

interface

uses
  Classes, SysUtils, dateutils, blcksock,
  ACBrDFe, ACBrDFeUtil, ACBrDFeWebService,
  pcnConversao, pcnConversaoONE,
  pcnEnvManutencaoEQP, pcnRetManutencaoEQP,
  pcnEnvRecepcaoLeitura, pcnRetRecepcaoLeitura,
  pcnDistLeitura, pcnRetDistLeitura,
  pcnConsFoto, pcnRetConsFoto, pcnConsPlaca, pcnRetConsPlaca,
  ACBrONEConfiguracoes;

type

  { TONEWebService }

  TONEWebService = class(TDFeWebService)
  private
    FOldSSLType: TSSLType;
    FOldHeaderElement: String;
  protected
    FPStatus: TStatusACBrONE;
    FPLayout: TLayOutONE;
    FPConfiguracoesONE: TConfiguracoesONE;

  protected
    procedure InicializarServico; override;
    procedure DefinirURL; override;
    function GerarVersaoDadosSoap: String; override;
    procedure FinalizarServico; override;

  public
    constructor Create(AOwner: TACBrDFe); override;
    procedure Clear; override;

    property Status: TStatusACBrONE read FPStatus;
    property Layout: TLayOutONE read FPLayout;
  end;

  { TONEEnvioWebService }

  TONEEnvioWebService = class(TONEWebService)
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

  { TEnvManutencao }

  TEnvManutencao = class(TONEWebService)
  private
//    FManutencao: TManutencaoEQP;
    FEnvManutencaoEQP: TManutencaoEQP;
    FRetManutencaoEQP: TRetManutencaoEQP;

    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    FNSUMovto: String;
    FcUF: Integer;

  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    function GerarPrefixoArquivo: String; override;

    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
//    constructor Create(AOwner: TACBrDFe; AManutencao: TManutencaoEQP); reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property versao: String read Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property xMotivo: String read FxMotivo;
    property dhResp: TDateTime read FdhResp;
    property NSUMovto: String read FNSUMovto;
    property cUF: Integer read FcUF;

    property EnvManutencaoEQP: TManutencaoEQP read FEnvManutencaoEQP write FEnvManutencaoEQP;
    property RetManutencaoEQP: TRetManutencaoEQP read FRetManutencaoEQP;
  end;

  { TEnvLeitura }

  TEnvLeitura = class(TONEWebService)
  private
    FRecepcaoLeitura: TRecepcaoLeitura;
    FEnvRecepcaoLeitura: TRecepcaoLeitura;
    FRetRecepcaoLeitura: TRetRecepcaoLeitura;

    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    FNSU: String;
    FcUF: Integer;

  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;
    function GerarPrefixoArquivo: String; override;

    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
//    constructor Create(AOwner: TACBrDFe; ARecepcaoLeitura: TRecepcaoLeitura); reintroduce; overload;
    destructor Destroy; override;
    procedure Clear; override;

    property versao: String read Fversao;
    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property verAplic: String read FverAplic;
    property cStat: Integer read FcStat;
    property xMotivo: String read FxMotivo;
    property dhResp: TDateTime read FdhResp;
    property NSU: String read FNSU;
    property cUF: Integer read FcUF;

    property EnvRecepcaoLeitura: TRecepcaoLeitura read FEnvRecepcaoLeitura write FRecepcaoLeitura;
    property RetRecepcaoLeitura: TRetRecepcaoLeitura read FRetRecepcaoLeitura;
  end;

  { TDistLeituras }

  TDistLeituras = class(TONEWebService)
  private
    FNomeArq: String;
    FlistaArqs: TStringList;

    FverAplic: String;
    FtpDist: TtpDist;
    FultNSU: String;
    FNSUFin: String;
    FCNPJOper: String;
    FcEQP: String;
    FcUF: Integer;

    FRetDistLeitura: TRetDistLeitura;

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
    procedure Clear; override;

    property NomeArq: String read FNomeArq;
    property ListaArqs: TStringList read FlistaArqs;

    property verAplic: String read FverAplic write FverAplic;
    property tpDist: TtpDist  read FtpDist   write FtpDist;
    property ultNSU: String   read FultNSU   write FultNSU;
    property NSUFin: String   read FNSUFin   write FNSUFin;
    property CNPJOper: String read FCNPJOper write FCNPJOper;
    property cEQP: String     read FcEQP     write FcEQP;
    property cUF: Integer     read FcUF      write FcUF;

    property RetDistLeitura: TRetDistLeitura read FRetDistLeitura;
  end;

  { TONEConsultaFoto }

  TONEConsultaFoto = class(TONEWebService)
  private
    FtpAmb: TpcnTipoAmbiente;
    Fversao: String;
    FverAplic: String;
    FcStat: integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    Ffoto: String;
    FNSULeitura: String;

    FRetConsFoto: TRetConsFoto;

  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property versao: String read Fversao;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property dhResp: TDateTime read FdhResp;
    property foto: String read Ffoto;
    property verAplic: String read FverAplic;
    property NSULeitura: String read FNSULeitura;

    property RetConsFoto: TRetConsFoto read FRetConsFoto;
  end;

  { TONEConsultaPlaca }

  TONEConsultaPlaca = class(TONEWebService)
  private
    FtpAmb: TpcnTipoAmbiente;
    Fversao: String;
    FverAplic: String;
    FcStat: integer;
    FxMotivo: String;
    FdhResp: TDateTime;
    Ffoto: String;
    FPlaca: String;

    FRetConsPlaca: TRetConsPlaca;
    FDataRef: TDateTime;

  protected
    procedure DefinirServicoEAction; override;
    procedure DefinirDadosMsg; override;
    function TratarResposta: Boolean; override;

    function GerarMsgLog: String; override;
  public
    constructor Create(AOwner: TACBrDFe); override;
    destructor Destroy; override;
    procedure Clear; override;

    property tpAmb: TpcnTipoAmbiente read FtpAmb;
    property versao: String read Fversao;
    property cStat: integer read FcStat;
    property xMotivo: String read FxMotivo;
    property dhResp: TDateTime read FdhResp;
    property foto: String read Ffoto;
    property verAplic: String read FverAplic;
    property Placa: String read FPlaca;
    property DataRef: TDateTime read FDataRef;

    property RetConsPlaca: TRetConsPlaca read FRetConsPlaca;
  end;

  { TWebServices }

  TWebServices = class
  private
    FACBrONE: TACBrDFe;

    FEnvioWebService: TONEEnvioWebService;
    FEnvManutencao: TEnvManutencao;
    FEnvLeitura: TEnvLeitura;
    FDistLeituras: TDistLeituras;
    FConsultarFoto: TONEConsultaFoto;
    FConsultarPlaca: TONEConsultaPlaca;
  public
    constructor Create(AOwner: TACBrDFe); overload;
    destructor Destroy; override;

    function ConsultaFoto(const aVerAplic, aNSULeitura: string): Boolean;
    function ConsultaPlaca(const aVerAplic, aPlaca: string; aDataRef: TDateTime): Boolean;

    property ACBrONE: TACBrDFe read FACBrONE write FACBrONE;

    property EnvioWebService: TONEEnvioWebService read FEnvioWebService write FEnvioWebService;
    property EnvManutencao: TEnvManutencao read FEnvManutencao write FEnvManutencao;
    property EnvLeitura: TEnvLeitura read FEnvLeitura write FEnvLeitura;
    property DistLeituras: TDistLeituras read FDistLeituras write FDistLeituras;
    property ConsultarFoto: TONEConsultaFoto read FConsultarFoto write FConsultarFoto;
    property ConsultarPlaca: TONEConsultaPlaca read FConsultarPlaca write FConsultarPlaca;
  end;

implementation

uses
  StrUtils, Math,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.DateTime,
  ACBrONE,
  pcnGerador, pcnLeitor;

{ TONEWebService }

constructor TONEWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPConfiguracoesONE := TConfiguracoesONE(FPConfiguracoes);
  FPLayout := LayRecepcaoLeitura;

  FPBodyElement := 'oneDadosMsg';
end;

procedure TONEWebService.Clear;
begin
  inherited Clear;

  FPStatus := stIdleONE;
  FPDFeOwner.SSL.UseCertificateHTTP := True;
end;

procedure TONEWebService.InicializarServico;
begin
  { Sobrescrever apenas se necessário }
  inherited InicializarServico;

  FOldSSLType := FPDFeOwner.SSL.SSLType;
  FOldHeaderElement := FPHeaderElement;
  FPDFeOwner.SSL.SSLType := LT_TLSv1_2;
  FPHeaderElement := '';

  TACBrONE(FPDFeOwner).SetStatus(FPStatus);
end;

procedure TONEWebService.DefinirURL;
var
  Versao: Double;
begin
  { sobrescrever apenas se necessário.
    Você também pode mudar apenas o valor de "FLayoutServico" na classe
    filha e chamar: Inherited;     }

  Versao := 0;
  FPVersaoServico := '';
  FPURL := '';

  TACBrONE(FPDFeOwner).LerServicoDeParams(FPLayout, Versao, FPURL);
  FPVersaoServico := FloatToString(Versao, '.', '0.00');
end;


function TONEWebService.GerarVersaoDadosSoap: String;
begin
  { Sobrescrever apenas se necessário }

  if EstaVazio(FPVersaoServico) then
    FPVersaoServico := TACBrONE(FPDFeOwner).LerVersaoDeParams(FPLayout);

  Result := '<versaoDados>' + FPVersaoServico + '</versaoDados>';
end;

procedure TONEWebService.FinalizarServico;
begin
  { Sobrescrever apenas se necessário }

  // Retornar configurações anteriores
  FPDFeOwner.SSL.SSLType := FOldSSLType;
  FPHeaderElement := FOldHeaderElement;

  TACBrONE(FPDFeOwner).SetStatus(stIdleONE);
end;

{ TWebServices }

function TWebServices.ConsultaFoto(const aVerAplic, aNSULeitura: string): Boolean;
begin
  ConsultarFoto.Clear;

  ConsultarFoto.FverAplic   := aVerAplic;
  ConsultarFoto.FNSULeitura := aNSULeitura;

  if not ConsultarFoto.Executar then
    ConsultarFoto.GerarException( ConsultarFoto.Msg );

  Result := True;
end;

function TWebServices.ConsultaPlaca(const aVerAplic, aPlaca: string;
  aDataRef: TDateTime): Boolean;
begin
  ConsultarPlaca.Clear;

  ConsultarPlaca.FverAplic   := aVerAplic;
  ConsultarPlaca.FPlaca := aPlaca;
  ConsultarPlaca.FDataRef := aDataRef;

  if not ConsultarPlaca.Executar then
    ConsultarPlaca.GerarException( ConsultarPlaca.Msg );

  Result := True;
end;

constructor TWebServices.Create(AOwner: TACBrDFe);
begin
  FACBrONE := TACBrONE(AOwner);

//  FEnvManutencao := TEnvManutencao.Create(FACBrONE, TACBrONE(FACBrONE).ManutencaoEQP);
//  FEnvLeitura    := TEnvLeitura.Create(FACBrONE, TACBrONE(FACBrONE).RecepcaoLeitura);

  FEnvManutencao := TEnvManutencao.Create(FACBrONE);
  FEnvLeitura    := TEnvLeitura.Create(FACBrONE);
  FDistLeituras  := TDistLeituras.Create(FACBrONE);

  FEnvioWebService := TONEEnvioWebService.Create(FACBrONE);
  FConsultarFoto := TONEConsultaFoto.Create(FACBrONE);
  FConsultarPlaca := TONEConsultaPlaca.Create(FACBrONE);
end;

destructor TWebServices.Destroy;
begin
  FEnvManutencao.Free;
  FEnvLeitura.Free;
  FDistLeituras.Free;
  FEnvioWebService.Free;
  FConsultarFoto.Free;
  FConsultarPlaca.Free;

  inherited Destroy;
end;

{ TONEEnvioWebService }

constructor TONEEnvioWebService.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

  FPStatus := stRecepcaoLeitura;
end;

destructor TONEEnvioWebService.Destroy;
begin
  inherited Destroy;
end;

procedure TONEEnvioWebService.Clear;
begin
  inherited Clear;

  FVersao := '';
end;

function TONEEnvioWebService.Executar: Boolean;
begin
  Result := inherited Executar;
end;

procedure TONEEnvioWebService.DefinirURL;
begin
  FPURL := FPURLEnvio;
end;

procedure TONEEnvioWebService.DefinirServicoEAction;
begin
  FPServico := FPSoapAction;
end;

procedure TONEEnvioWebService.DefinirDadosMsg;
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

function TONEEnvioWebService.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDados(FPRetornoWS, 'soap:Body');
  Result := True;
end;

function TONEEnvioWebService.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService: '+FPServico + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TONEEnvioWebService.GerarVersaoDadosSoap: String;
begin
  Result := '<versaoDados>' + FVersao + '</versaoDados>';
end;

{ TEnvManutencao }

//constructor TEnvManutencao.Create(AOwner: TACBrDFe;
//  AManutencao: TManutencaoEQP);
constructor TEnvManutencao.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

//  FManutencao := AManutencao;

  FEnvManutencaoEQP := TManutencaoEQP.Create;
end;

destructor TEnvManutencao.Destroy;
begin
  FEnvManutencaoEQP.Free;

  if Assigned(FRetManutencaoEQP) then
    FRetManutencaoEQP.Free;

  inherited;
end;

procedure TEnvManutencao.Clear;
begin
  inherited Clear;

  FPStatus := stManutencao;
  FPLayout := LayManutencao;
  FPArqEnv := 'ped-man';
  FPArqResp := 'man';

  Fversao := '';
  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  FdhResp := 0;
  FNSUMovto := '';

  if Assigned(FRetManutencaoEQP) then
    FRetManutencaoEQP.Free;

  FRetManutencaoEQP := TRetManutencaoEQP.Create;

  if Assigned(FPConfiguracoesONE) then
  begin
    FtpAmb := FPConfiguracoesONE.WebServices.Ambiente;
    FcUF := FPConfiguracoesONE.WebServices.UFCodigo;
  end
end;

procedure TEnvManutencao.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'oneManutencaoEQP';
  FPSoapAction := FPServico + '/oneManutencaoEQP';
end;

procedure TEnvManutencao.DefinirDadosMsg;
var
  EnvManutencao: TManutencaoEQP;
  FErroValidacao: String;
  DadosEhValido: Boolean;
begin
  EnvManutencao := TManutencaoEQP.Create;

  try
    {(*}
    with EnvManutencao do
    begin
      Versao    := FPVersaoServico;
      tpAmb     := FPConfiguracoesONE.WebServices.Ambiente; //FManutencao.tpAmb;
      verAplic  := FEnvManutencaoEQP.verAplic;
      tpMan     := FEnvManutencaoEQP.tpMan;
      dhReg     := FEnvManutencaoEQP.dhReg;
      CNPJOper  := FEnvManutencaoEQP.CNPJOper;
      cEQP      := FEnvManutencaoEQP.cEQP;
      xEQP      := FEnvManutencaoEQP.xEQP;
      cUF       := FEnvManutencaoEQP.cUF;
      tpSentido := FEnvManutencaoEQP.tpSentido;
      Latitude  := FEnvManutencaoEQP.Latitude;
      Longitude := FEnvManutencaoEQP.Longitude;
      tpEQP     := FEnvManutencaoEQP.tpEQP;
{
      verAplic  := FManutencao.verAplic;
      tpMan     := FManutencao.tpMan;
      dhReg     := FManutencao.dhReg;
      CNPJOper  := FManutencao.CNPJOper;
      cEQP      := FManutencao.cEQP;
      xEQP      := FManutencao.xEQP;
      cUF       := FManutencao.cUF;
      tpSentido := FManutencao.tpSentido;
      Latitude  := FManutencao.Latitude;
      Longitude := FManutencao.Longitude;
      tpEQP     := FManutencao.tpEQP;
}
    end;
    {*)}

    AjustarOpcoes( EnvManutencao.Gerador.Opcoes );
    EnvManutencao.GerarXML;

    FPDadosMsg := EnvManutencao.Gerador.ArquivoFormatoXML;

    with TACBrONE(FPDFeOwner) do
    begin
      DadosEhValido := SSL.Validar(FPDadosMsg,
                                    GerarNomeArqSchema(FPLayout,
                                                       StringToFloatDef(FPVersaoServico, 0)),
                                    FPMsg);
    end;

    if not DadosEhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados: ') +
        FPMsg;

      raise EACBrONEException.CreateDef(FErroValidacao);
    end;

  finally
    EnvManutencao.Free;
  end;
end;

function TEnvManutencao.GerarMsgLog: String;
var
  aMsg: String;
begin
  {(*}
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak +
                         'Resposta: %s ' + LineBreak +
                         'NSU do Movimento: %s ' + LineBreak),
                 [FRetManutencaoEQP.versao, TpAmbToStr(FRetManutencaoEQP.tpAmb),
                  FRetManutencaoEQP.verAplic, IntToStr(FRetManutencaoEQP.cStat),
                  FRetManutencaoEQP.xMotivo,
                  IfThen(FRetManutencaoEQP.dhResp = 0, '', FormatDateTimeBr(FRetManutencaoEQP.dhResp)),
                  FRetManutencaoEQP.NSUMovto]);

  Result := aMsg;
  {*)}
end;

function TEnvManutencao.GerarPrefixoArquivo: String;
var
  vUF, vData, vCNPJ, vtpMan, vcEQP: String;
begin
  vUF    := Poem_Zeros(FEnvManutencaoEQP.cUF, 2);
  vData  := FormatDateTime('YYMM', FEnvManutencaoEQP.dhReg);
  vCNPJ  := PadLeft(OnlyNumber(FEnvManutencaoEQP.CNPJOper), 14, '0');
  vtpMan := Poem_Zeros(tpManToStr(FEnvManutencaoEQP.tpMan), 1);
  vcEQP  := Poem_Zeros(FEnvManutencaoEQP.cEQP, 15);

  Result := vUF + vData + vCNPJ + vtpMan + vcEQP;
end;

function TEnvManutencao.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['oneResultMsg'], FPRetornoWS );

  FRetManutencaoEQP.Leitor.Arquivo := ParseText(AnsiString(FPRetWS));
  FRetManutencaoEQP.LerXml;

  Fversao   := FRetManutencaoEQP.versao;
  FtpAmb    := FRetManutencaoEQP.tpAmb;
  FverAplic := FRetManutencaoEQP.verAplic;
  FcStat    := FRetManutencaoEQP.cStat;
  FxMotivo  := FRetManutencaoEQP.xMotivo;
  FdhResp   := FRetManutencaoEQP.dhResp;
  FNSUMovto := FRetManutencaoEQP.NSUMovto;
  FPMsg     := FxMotivo;

  Result := (FcStat = 107);
end;

{ TEnvLeitura }

//constructor TEnvLeitura.Create(AOwner: TACBrDFe;
//  ARecepcaoLeitura: TRecepcaoLeitura);
constructor TEnvLeitura.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

//  FRecepcaoLeitura := ARecepcaoLeitura;

  FEnvRecepcaoLeitura := TRecepcaoLeitura.Create;
end;

destructor TEnvLeitura.Destroy;
begin
  FEnvRecepcaoLeitura.Free;

  if Assigned(FRetRecepcaoLeitura) then
    FRetRecepcaoLeitura.Free;

  inherited;
end;

procedure TEnvLeitura.Clear;
begin
  inherited Clear;

  FPStatus := stRecepcaoLeitura;
  FPLayout := LayRecepcaoLeitura;
  FPArqEnv := 'env-lei';
  FPArqResp := 'lei';

  Fversao := '';
  FverAplic := '';
  FcStat := 0;
  FxMotivo := '';
  FdhResp := 0;
  FNSU := '';

  if Assigned(FRetRecepcaoLeitura) then
    FRetRecepcaoLeitura.Free;

  FRetRecepcaoLeitura := TRetRecepcaoLeitura.Create;

  if Assigned(FPConfiguracoesONE) then
  begin
    FtpAmb := FPConfiguracoesONE.WebServices.Ambiente;
    FcUF := FPConfiguracoesONE.WebServices.UFCodigo;
  end
end;

procedure TEnvLeitura.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'oneRecepcaoLeitura';
  FPSoapAction := FPServico + '/oneRecepcaoLeitura';
end;

procedure TEnvLeitura.DefinirDadosMsg;
var
  EnvLeitura: TRecepcaoLeitura;
  FErroValidacao: String;
  DadosEhValido: Boolean;
begin
  EnvLeitura := TRecepcaoLeitura.Create;

  try
    {(*}
    with EnvLeitura do
    begin
      Versao          := FPVersaoServico;
      tpAmb           := FPConfiguracoesONE.WebServices.Ambiente; //FManutencao.tpAmb;
      verAplic        := FEnvRecepcaoLeitura.verAplic;
      tpTransm        := FEnvRecepcaoLeitura.tpTransm;
      dhTransm        := FEnvRecepcaoLeitura.dhTransm;
      with infLeitura do
      begin
        cUF             := FEnvRecepcaoLeitura.infLeitura.cUF;
        dhPass          := FEnvRecepcaoLeitura.infLeitura.dhPass;
        CNPJOper        := FEnvRecepcaoLeitura.infLeitura.CNPJOper;
        cEQP            := FEnvRecepcaoLeitura.infLeitura.cEQP;
        latitude        := FEnvRecepcaoLeitura.infLeitura.latitude;
        longitude       := FEnvRecepcaoLeitura.infLeitura.longitude;
        tpSentido       := FEnvRecepcaoLeitura.infLeitura.tpSentido;
        placa           := FEnvRecepcaoLeitura.infLeitura.placa;
        tpVeiculo       := FEnvRecepcaoLeitura.infLeitura.tpVeiculo;
        velocidade      := FEnvRecepcaoLeitura.infLeitura.velocidade;
        foto            := FEnvRecepcaoLeitura.infLeitura.foto;
        indiceConfianca := FEnvRecepcaoLeitura.infLeitura.indiceConfianca;
        pesoBrutoTotal  := FEnvRecepcaoLeitura.infLeitura.pesoBrutoTotal;
        nroEixos        := FEnvRecepcaoLeitura.infLeitura.nroEixos;
      end;
    end;
    {*)}

    AjustarOpcoes( EnvLeitura.Gerador.Opcoes );
    EnvLeitura.GerarXML;

    FPDadosMsg := EnvLeitura.Gerador.ArquivoFormatoXML;

    with TACBrONE(FPDFeOwner) do
    begin
      DadosEhValido := SSL.Validar(FPDadosMsg,
                                    GerarNomeArqSchema(FPLayout,
                                                       StringToFloatDef(FPVersaoServico, 0)),
                                    FPMsg);
    end;

    if not DadosEhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados: ') +
        FPMsg;

      raise EACBrONEException.CreateDef(FErroValidacao);
    end;

  finally
    EnvLeitura.Free;
  end;
end;

function TEnvLeitura.GerarMsgLog: String;
var
  aMsg: String;
begin
  {(*}
  aMsg := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                         'Ambiente: %s ' + LineBreak +
                         'Versão Aplicativo: %s ' + LineBreak +
                         'Status Código: %s ' + LineBreak +
                         'Status Descrição: %s ' + LineBreak +
                         'Resposta: %s ' + LineBreak +
                         'NSU: %s ' + LineBreak),
                 [FRetRecepcaoLeitura.versao, TpAmbToStr(FRetRecepcaoLeitura.tpAmb),
                  FRetRecepcaoLeitura.verAplic, IntToStr(FRetRecepcaoLeitura.cStat),
                  FRetRecepcaoLeitura.xMotivo,
                  IfThen(FRetRecepcaoLeitura.dhResp = 0, '', FormatDateTimeBr(FRetRecepcaoLeitura.dhResp)),
                  FRetRecepcaoLeitura.NSU]);

  Result := aMsg;
  {*)}
end;

function TEnvLeitura.GerarPrefixoArquivo: String;
var
  vUF, vData, vCNPJ, vtpTra, vcEQP, vdhPass: String;
begin
  vUF     := Poem_Zeros(FEnvRecepcaoLeitura.infLeitura.cUF, 2);
  vData   := FormatDateTime('YYMM', FEnvRecepcaoLeitura.dhTransm);
  vCNPJ   := PadLeft(OnlyNumber(FEnvRecepcaoLeitura.infLeitura.CNPJOper), 14, '0');
  vtpTra  := Poem_Zeros(tpTransmToStr(FEnvRecepcaoLeitura.tpTransm), 1);
  vcEQP   := Poem_Zeros(FEnvRecepcaoLeitura.infLeitura.cEQP, 15);
  vdhPass := FormatDateTime('YYYYMMDDHHNNSS', FEnvRecepcaoLeitura.infLeitura.dhPass);

  Result := vUF + vData + vCNPJ + vtpTra + vcEQP + vdhPass;
end;

function TEnvLeitura.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['oneResultMsg'], FPRetornoWS );

  FRetRecepcaoLeitura.Leitor.Arquivo := ParseText(AnsiString(FPRetWS));
  FRetRecepcaoLeitura.LerXml;

  Fversao   := FRetRecepcaoLeitura.versao;
  FtpAmb    := FRetRecepcaoLeitura.tpAmb;
  FverAplic := FRetRecepcaoLeitura.verAplic;
  FcStat    := FRetRecepcaoLeitura.cStat;
  FxMotivo  := FRetRecepcaoLeitura.xMotivo;
  FdhResp   := FRetRecepcaoLeitura.dhResp;
  FNSU      := FRetRecepcaoLeitura.NSU;
  FPMsg     := FxMotivo;

  Result := (FcStat = 103);
end;

{ TDistLeituras }

constructor TDistLeituras.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);
end;

destructor TDistLeituras.Destroy;
begin
  FRetDistLeitura.Free;
  FlistaArqs.Free;

  inherited;
end;

procedure TDistLeituras.Clear;
begin
  inherited Clear;

  FPStatus := stDistLeitura;
  FPLayout := LayDistLeitura;
  FPArqEnv := 'con-dist-lei';
  FPArqResp := 'dist-lei';

  if Assigned(FRetDistLeitura) then
    FRetDistLeitura.Free;

  FRetDistLeitura := TRetDistLeitura.Create;

  if Assigned(FlistaArqs) then
    FlistaArqs.Free;

  FlistaArqs := TStringList.Create;
end;

procedure TDistLeituras.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'oneDistLeitura';
  FPSoapAction := FPServico + '/oneDistLeitura';
end;

procedure TDistLeituras.DefinirDadosMsg;
var
  DistLeitura: TDistLeitura;
  FErroValidacao: String;
  DadosEhValido: Boolean;
begin
  DistLeitura := TDistLeitura.Create;

  try
    DistLeitura.Versao   := FPVersaoServico;
    DistLeitura.TpAmb    := FPConfiguracoesONE.WebServices.Ambiente;
    DistLeitura.verAplic := FverAplic;
    DistLeitura.tpDist   := FtpDist;
    DistLeitura.ultNSU   := FultNSU;
    DistLeitura.NSUFin   := FNSUFin;
    DistLeitura.CNPJOper := FCNPJOper;
    DistLeitura.cEQP     := FcEQP;
    DistLeitura.cUF      := FcUF;

    AjustarOpcoes( DistLeitura.Gerador.Opcoes );
    DistLeitura.GerarXML;

    FPDadosMsg := DistLeitura.Gerador.ArquivoFormatoXML;

    with TACBrONE(FPDFeOwner) do
    begin
      DadosEhValido := SSL.Validar(FPDadosMsg,
                                    GerarNomeArqSchema(FPLayout,
                                                       StringToFloatDef(FPVersaoServico, 0)),
                                    FPMsg);
    end;

    if not DadosEhValido then
    begin
      FErroValidacao := ACBrStr('Falha na validação dos dados: ') +
        FPMsg;

      raise EACBrONEException.CreateDef(FErroValidacao);
    end;

  finally
    DistLeitura.Free;
  end;
end;

function TDistLeituras.TratarResposta: Boolean;
var
  I: Integer;
  AXML: String;

function GerarPrefixo: String;
var
  vUF, vData, vCNPJ, vtpTra, vcEQP, vdhPass: String;
begin
  with FRetDistLeitura.Leitura.Items[I].RecepcaoLeitura do
  begin
    vUF     := Poem_Zeros(cUF, 2);
    vData   := FormatDateTime('YYMM', dhTransm);
    vCNPJ   := PadLeft(OnlyNumber(CNPJOper), 14, '0');
    vtpTra  := Poem_Zeros(tpTransmToStr(tpTransm), 1);
    vcEQP   := Poem_Zeros(cEQP, 15);
    vdhPass := FormatDateTime('YYYYMMDDHHNNSS', infLeitura.dhPass);
  end;

  Result := vUF + vData + vCNPJ + vtpTra + vcEQP + vdhPass;
end;

begin
  FPRetWS := SeparaDadosArray(['oneResultMsg'], FPRetornoWS );

  // Processando em UTF8, para poder gravar arquivo corretamente //
  FRetDistLeitura.Leitor.Arquivo := FPRetWS;
  FRetDistLeitura.LerXml;

  for I := 0 to FRetDistLeitura.Leitura.Count - 1 do
  begin
    AXML := FRetDistLeitura.Leitura.Items[I].XML;
    FNomeArq := '';
    if (AXML <> '') then
    begin
      FNomeArq := GerarPrefixo + '-lei.xml';

      if NaoEstaVazio(NomeArq) then
        FlistaArqs.Add( FNomeArq );

      if (FPConfiguracoesONE.Arquivos.Salvar) and NaoEstaVazio(FNomeArq) then
        FPDFeOwner.Gravar(FNomeArq, AXML, GerarPathDistribuicao);
    end;
  end;

  { Processsa novamente, chamando ParseTXT, para converter de UTF8 para a String
    nativa e Decodificar caracteres HTML Entity }
  FRetDistLeitura.Free;   // Limpando a lista
  FRetDistLeitura := TRetDistLeitura.Create;

  FRetDistLeitura.Leitor.Arquivo := ParseText(AnsiString(FPRetWS));
  FRetDistLeitura.LerXml;

  FPMsg := FRetDistLeitura.xMotivo;
  // 104 – Consulta efetuada com sucesso
  // 105 – Nenhuma leitura localizada
  Result := (FRetDistLeitura.CStat = 104) or (FRetDistLeitura.CStat = 105);
end;

function TDistLeituras.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'Resposta: %s ' + LineBreak +
                           'Último NSU: %s ' + LineBreak +
                           'Último NSU ONE: %s ' + LineBreak),
                   [FRetDistLeitura.versao, TpAmbToStr(FRetDistLeitura.tpAmb),
                    FRetDistLeitura.verAplic, IntToStr(FRetDistLeitura.cStat),
                    FRetDistLeitura.xMotivo,
                    IfThen(FRetDistLeitura.dhResp = 0, '',
                           FormatDateTimeBr(FRetDistLeitura.dhResp)),
                    FRetDistLeitura.ultNSU, FRetDistLeitura.ultNSUONE]);
end;

function TDistLeituras.GerarMsgErro(E: Exception): String;
begin
  Result := ACBrStr('WebService Distribuição de DFe:' + LineBreak +
                    '- Inativo ou Inoperante tente novamente.');
end;

function TDistLeituras.GerarPathDistribuicao: String;
begin
  Result := FPConfiguracoesONE.Arquivos.GetPathDownload('',
                                                        FPConfiguracoesONE.Geral.CNPJOper,
                                                        '',
                                                        Now);
end;

{ TONEConsultaFoto }

procedure TONEConsultaFoto.Clear;
begin
  inherited Clear;

  FPStatus := stConsFoto;
  FPLayout := LayConsFoto;
  FPArqEnv := 'cons-foto';
  FPArqResp := 'foto';

  FcStat := 0;
  FxMotivo := '';
  Fversao := '';

  if Assigned(FPConfiguracoesONE) then
  begin
    FtpAmb := FPConfiguracoesONE.WebServices.Ambiente;
  end;

  if Assigned(FRetConsFoto) then
    FRetConsFoto.Free;

  FRetConsFoto := TRetConsFoto.Create;
end;

constructor TONEConsultaFoto.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

end;

procedure TONEConsultaFoto.DefinirDadosMsg;
var
  ConsFoto: TConsFoto;
begin
  ConsFoto := TConsFoto.Create;
  try
    ConsFoto.TpAmb      := TpAmb;
    ConsFoto.verAplic   := verAplic;
    ConsFoto.NSULeitura := NSULeitura;
    ConsFoto.Versao     := FPVersaoServico;

    AjustarOpcoes( ConsFoto.Gerador.Opcoes );

    ConsFoto.GerarXML;

    FPDadosMsg := ConsFoto.Gerador.ArquivoFormatoXML;
  finally
    ConsFoto.Free;
  end;
end;

procedure TONEConsultaFoto.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'oneConsFoto';
  FPSoapAction := FPServico + '/oneConsFoto';
end;

destructor TONEConsultaFoto.Destroy;
begin
  FRetConsFoto.Free;

  inherited Destroy;
end;

function TONEConsultaFoto.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'Resposta: %s ' + LineBreak),
                   [FRetConsFoto.versao,
                    TpAmbToStr(FRetConsFoto.TpAmb),
                    FRetConsFoto.verAplic,
                    IntToStr(FRetConsFoto.cStat),
                    FRetConsFoto.xMotivo,
                    IfThen(FRetConsFoto.dhResp = 0, '',
                           FormatDateTimeBr(FRetConsFoto.dhResp))]);
end;

function TONEConsultaFoto.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['oneResultMsg'], FPRetornoWS);

  FRetConsFoto.Leitor.Arquivo := ParseText(AnsiString(FPRetWS));
  FRetConsFoto.LerXml;

  Fversao   := FRetConsFoto.versao;
  FtpAmb    := FRetConsFoto.tpAmb;
  FverAplic := FRetConsFoto.verAplic;
  FcStat    := FRetConsFoto.cStat;
  FxMotivo  := FRetConsFoto.xMotivo;
  FdhResp   := FRetConsFoto.dhResp;
  Ffoto     := FRetConsFoto.foto;
  FPMsg     := FxMotivo;

  Result := (FcStat = 104);
end;

{ TONEConsultaPlaca }

procedure TONEConsultaPlaca.Clear;
begin
  inherited Clear;

  FPStatus := stConsPlaca;
  FPLayout := LayConsPlaca;
  FPArqEnv := 'cons-placa';
  FPArqResp := 'placa';

  FcStat := 0;
  FxMotivo := '';
  Fversao := '';

  if Assigned(FPConfiguracoesONE) then
  begin
    FtpAmb := FPConfiguracoesONE.WebServices.Ambiente;
  end;

  if Assigned(FRetConsPlaca) then
    FRetConsPlaca.Free;

  FRetConsPlaca := TRetConsPlaca.Create;
end;

constructor TONEConsultaPlaca.Create(AOwner: TACBrDFe);
begin
  inherited Create(AOwner);

end;

procedure TONEConsultaPlaca.DefinirDadosMsg;
var
  ConsPlaca: TConsPlaca;
begin
  ConsPlaca := TConsPlaca.Create;
  try
    ConsPlaca.TpAmb := TpAmb;
    ConsPlaca.verAplic := verAplic;
    ConsPlaca.Placa := Placa;
    ConsPlaca.dtRef := DataRef;
    ConsPlaca.Versao := FPVersaoServico;

    AjustarOpcoes( ConsPlaca.Gerador.Opcoes );

    ConsPlaca.GerarXML;

    FPDadosMsg := ConsPlaca.Gerador.ArquivoFormatoXML;
  finally
    ConsPlaca.Free;
  end;
end;

procedure TONEConsultaPlaca.DefinirServicoEAction;
begin
  FPServico := GetUrlWsd + 'oneConsPorPlaca';
  FPSoapAction := FPServico + '/oneConsPorPlaca';
end;

destructor TONEConsultaPlaca.Destroy;
begin
  FRetConsPlaca.Free;

  inherited Destroy;
end;

function TONEConsultaPlaca.GerarMsgLog: String;
begin
  Result := Format(ACBrStr('Versão Layout: %s ' + LineBreak +
                           'Ambiente: %s ' + LineBreak +
                           'Versão Aplicativo: %s ' + LineBreak +
                           'Status Código: %s ' + LineBreak +
                           'Status Descrição: %s ' + LineBreak +
                           'Resposta: %s ' + LineBreak),
                   [FRetConsPlaca.versao,
                    TpAmbToStr(FRetConsPlaca.TpAmb),
                    FRetConsPlaca.verAplic,
                    IntToStr(FRetConsPlaca.cStat),
                    FRetConsPlaca.xMotivo,
                    IfThen(FRetConsPlaca.dhResp = 0, '',
                           FormatDateTimeBr(FRetConsPlaca.dhResp))]);
end;

function TONEConsultaPlaca.TratarResposta: Boolean;
begin
  FPRetWS := SeparaDadosArray(['oneResultMsg'], FPRetornoWS);

  FRetConsPlaca.Leitor.Arquivo := ParseText(AnsiString(FPRetWS));
  FRetConsPlaca.LerXml;

  Fversao   := FRetConsPlaca.versao;
  FtpAmb    := FRetConsPlaca.tpAmb;
  FverAplic := FRetConsPlaca.verAplic;
  FcStat    := FRetConsPlaca.cStat;
  FxMotivo  := FRetConsPlaca.xMotivo;
  FdhResp   := FRetConsPlaca.dhResp;
  FPMsg     := FxMotivo;

  Result := (FcStat = 104);
end;

end.
