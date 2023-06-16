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

{$I ACBr.inc}

unit ACBrTEFMSitefAndroidAPI;

interface

uses
  Classes, SysUtils,
  System.Messaging,
  System.Character,
  System.Math,
  System.UITypes,
  FMX.Platform,
  FMX.Platform.Android,
  Androidapi.Helpers,
  Androidapi.JNI.App,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Net,
  Androidapi.JNI.Os,
  Androidapi.JNI.Embarcadero,
  Androidapi.JNI.GraphicsContentViewText,
  ACBrTEFComum, ACBrTEFMSitefComum, ACBrBase;

resourcestring
  sErrNOTINIT = 'TACBrTEFSIWebAndroid não foi inicializada';
  sErrTRNINIT = 'Já foi iniciada uma Transação[MSitef]';
  sErrTRNNOTINIT = 'Não foi iniciada uma Transação';
  sErrINTENTNOTFOUND = 'Ação %s não encontrada.';
  sErrCANCELED = 'Operação TEF cancelada';

const
  CACBrTEFSIWebAndroidName = 'ACBrTEFSIWebAndroid';
  CACBrTEFSIWebAndroidVersao = '0.1.0';
  Intent_Payment = 'br.com.softwareexpress.sitef.msitef.ACTIVITY_CLISITEF';

  COD_REQUEST_REPORT = 2241;
  Key_EmpresaSitef  = 'empresaSitef';
  Key_EnderecoSitef = 'enderecoSitef';
  Key_Modalidade    = 'modalidade';
  Key_Operador      = 'operador';
  Key_Data          = 'data';
  Key_Hora          = 'hora';
  Key_NumeroCupom   = 'numeroCupom';
  Key_Valor         = 'valor';
  Key_NsuSitef      = 'NSU_SITEF';
  Key_CNPJ_CPF      = 'CNPJ_CPF';
  Key_CNPJ_AUTOMACAO = 'cnpj_automacao';
  Key_pinpadMac      = 'pinpadMac';
  Key_ComExterna    = 'comExterna';
  Key_ISDoubleValidation  = 'isDoubleValidation';
  Key_Restricoes    = 'restricoes';
  Key_TransacoesHabilitadas  = 'transacoesHabilitadas';
  Key_OTP           = 'Otp';
  Key_AcessibilidadeVisual = 'acessibilidadeVisual';
  Key_TipoPinpad = 'tipoPinpad';
type
  EACBrTEFSIWebAndroid = class(EACBrTEFErro);

  TACBrTEFSIWebAndroidEstadoTransacao = procedure(AIntent: JIntent) of object;
  TACBrTEFSIWebAndroidAvaliarTransacaoPendente = procedure(pszReqNum: String; pszLocRef: String; pszExtRef: String; pszVirtMerch: String; pszAuthSyst: String) of object;

  TACBrTEFSIWebAndroidDadosAutomacao = class
  private
    fSuportaSaque: Boolean;
    fSuportaDesconto: Boolean;
    fSuportaViasDiferenciadas: Boolean;
    fUtilizaSaldoTotalVoucher: Boolean;
    fImprimeViaClienteReduzida: Boolean;
    fCNPJSoftwareHouse: String;

  public
    constructor Create;
    procedure Clear;
    property CNPJSoftwareHouse: String read fCNPJSoftwareHouse write fCNPJSoftwareHouse;
    Property SuportaSaque: Boolean read fSuportaSaque write fSuportaSaque default False;
    Property SuportaDesconto: Boolean read fSuportaDesconto write fSuportaDesconto default False;
    property ImprimeViaClienteReduzida: Boolean read fImprimeViaClienteReduzida write fImprimeViaClienteReduzida default True;
    property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas write fSuportaViasDiferenciadas default True;
    property UtilizaSaldoTotalVoucher: Boolean read fUtilizaSaldoTotalVoucher write fUtilizaSaldoTotalVoucher default False;
  end;

  TACBrTEFSIWebAndroidDadosTerminal = class
  private
    fCodEmpresa: String;
    fCodFilial: String;
    fCodTerminal: String;
    fEnderecoServidor: String;
    fOperador: String;
    fPortaPinPad: String;
  public
    constructor Create;
    procedure Clear;
  published
    property EnderecoServidor: String read fEnderecoServidor write fEnderecoServidor;
    property CodEmpresa: String read fCodEmpresa write fCodEmpresa;
    property CodFilial: String read fCodFilial write fCodFilial;
    property CodTerminal: String read fCodTerminal write fCodTerminal;
    property Operador: String read fOperador write fOperador;
    property PortaPinPad: String read fPortaPinPad write fPortaPinPad;
  end;

  TACBrTEFSIWebAndroid = class
  private
    fMessageSubscriptionID : integer;
    fValorTotalPagamento : Double;
    fInicializada: Boolean;
    fEmTransacao: Boolean;
    fCNPJEstabelecimento: String;
    fConfirmarTransacoesPendentesNoHost: Boolean;
    fDadosTransacao: TACBrTEFParametros;
    fNomeEstabelecimento: String;
    fParametrosAdicionais: TACBrTEFParametros;
    fOnGravarLog: TACBrGravarLog;
    fOnAntesIniciarTransacao: TACBrTEFSIWebAndroidEstadoTransacao;
    fOnDepoisTerminarTransacao: TACBrTEFSIWebAndroidEstadoTransacao;
    fOnAvaliarTransacaoPendente: TACBrTEFSIWebAndroidAvaliarTransacaoPendente;
    fDadosAutomacao: TACBrTEFSIWebAndroidDadosAutomacao;
    fDadosTerminal : TACBrTEFSIWebAndroidDadosTerminal;

    procedure SetCNPJEstabelecimento(AValue: String);
    procedure SetNomeEstabelecimento(AValue: String);

    procedure SetInicializada(AValue: Boolean);

    function OnActivityResult(RequestCode, ResultCode: Integer; AIntent: JIntent): Boolean;

    procedure IniciarIntent(AIntent: JIntent);

    function GerarTransactionId: String;
    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
  protected
    procedure DoException( AErrorMsg: String );
    function ObterUltimoRetorno: String;

    procedure AdicionarDadosObrigatorios(intent: JIntent);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Inicializar;
    procedure DesInicializar;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False); overload;
    procedure GravarLog(const AString: String; Traduz: Boolean = False); overload;

    procedure IniciarTransacao(iOPER: Byte; ParametrosAdicionaisTransacao: TACBrTEFParametros = Nil);

    procedure ObterDadosDaTransacao(AIntent: JIntent);
    procedure TratarTransacaoPendente(AStatus: LongWord = 0;
                                      pszReqNum: String = '';
                                      pszLocRef: String = '';
                                      pszExtRef: String = '';
                                      pszVirtMerch: String = '';
                                      pszAuthSyst: String = '');

    procedure ConfirmarTransacao(AStatus: LongWord; confirmTransactionIdentifier: String = '');
    procedure ResolverTransacaoPendente(AStatus: LongWord; pszReqNum: String; pszLocRef: String; pszExtRef: String; pszVirtMerch: String; pszAuthSyst: String);

    property Inicializada: Boolean read fInicializada write SetInicializada;

    property EmTransacao: Boolean read fEmTransacao;
    property DadosDaTransacao: TACBrTEFParametros read fDadosTransacao;

    Property NomeEstabelecimento: String read fNomeEstabelecimento write SetNomeEstabelecimento;
    property CNPJEstabelecimento: String read fCNPJEstabelecimento write SetCNPJEstabelecimento;
    property ValorTotalPagamento: Double read fValorTotalPagamento write fValorTotalPagamento;

    property DadosAutomacao: TACBrTEFSIWebAndroidDadosAutomacao read fDadosAutomacao;
    property DadosTerminal : TACBrTEFSIWebAndroidDadosTerminal read fDadosTerminal;

    property ParametrosAdicionais: TACBrTEFParametros read fParametrosAdicionais;

    property ConfirmarTransacoesPendentesNoHost: Boolean read fConfirmarTransacoesPendentesNoHost write fConfirmarTransacoesPendentesNoHost;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
    property OnAntesIniciarTransacao: TACBrTEFSIWebAndroidEstadoTransacao read fOnAntesIniciarTransacao write fOnAntesIniciarTransacao;
    property OnDepoisTerminarTransacao: TACBrTEFSIWebAndroidEstadoTransacao read fOnDepoisTerminarTransacao write fOnDepoisTerminarTransacao;
    property OnAvaliarTransacaoPendente: TACBrTEFSIWebAndroidAvaliarTransacaoPendente read fOnAvaliarTransacaoPendente write fOnAvaliarTransacaoPendente;
  end;

function OperationToPWOPER_(const AOperation: String): Byte;
function PrintReceiptsToPWINFO_RCPTPRN(const viaCliente: boolean; viaEstabelecimento : boolean): Byte;

implementation
uses
  StrUtils,
  synacode,
  synautil,
  ACBrJSON,
  ACBrUtil.Strings,
  ACBrUtil.Math,
  ACBrUtil.FilesIO,
  ACBrValidador;

function OperationToPWOPER_(const AOperation: String): Byte;
begin
  if (AOperation = 'VENDA') then
    Result := PWOPER_SALE
  else if (AOperation = 'ADMINISTRATIVA') then
    Result := PWOPER_ADMIN
  else if (AOperation = 'CANCELAMENTO') then
    Result := PWOPER_SALEVOID
  else if (AOperation = 'REIMPRESSAO') then
    Result := PWOPER_REPRINT
  else
    Result := 0;
end;

{ TACBrTEFSIWebAndroidDadosAutomacao }
constructor TACBrTEFSIWebAndroidDadosAutomacao.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFSIWebAndroidDadosAutomacao.Clear;
begin
  fCNPJSoftwareHouse := '';
  fImprimeViaClienteReduzida := True;
  fSuportaDesconto := False;
  fSuportaSaque := False;
  fSuportaViasDiferenciadas := True;
  fUtilizaSaldoTotalVoucher := False;
end;

{ TACBrTEFPGWebAndroid }
constructor TACBrTEFSIWebAndroid.Create;
begin
  inherited;

  fInicializada := False;
  fConfirmarTransacoesPendentesNoHost := True;

  fCNPJEstabelecimento := '';
  fNomeEstabelecimento := '';

  fOnGravarLog := Nil;
  fOnAntesIniciarTransacao := Nil;
  fOnDepoisTerminarTransacao := Nil;
  fOnAvaliarTransacaoPendente := Nil;

  fDadosAutomacao := TACBrTEFSIWebAndroidDadosAutomacao.Create;
  fDadosTerminal := TACBrTEFSIWebAndroidDadosTerminal.Create;

  fDadosTransacao := TACBrTEFParametros.Create;
  fParametrosAdicionais := TACBrTEFParametros.Create;
end;

destructor TACBrTEFSIWebAndroid.Destroy;
begin
  fDadosAutomacao.Free;
  fDadosTerminal.Free;

  fDadosTransacao.Free;
  fParametrosAdicionais.Free;
  inherited;
end;

procedure TACBrTEFSIWebAndroid.SetInicializada(AValue: Boolean);
begin
  if fInicializada = AValue then
    Exit;

  GravarLog('TACBrTEFPGWebAndroid.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFSIWebAndroid.Inicializar;
begin
  GravarLog('TACBrTEFSIWebAndroid.Inicializar');
  fMessageSubscriptionID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, HandleActivityMessage);
  fInicializada := True;
  fEmTransacao := False;
end;

procedure TACBrTEFSIWebAndroid.DesInicializar;
begin
  GravarLog('TACBrTEFSIWebAndroid.DesInicializar');
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, HandleActivityMessage);
  fInicializada := False;
  fEmTransacao := False;
end;

procedure TACBrTEFSIWebAndroid.HandleActivityMessage(const Sender: TObject; const M: TMessage);
begin
  if (M is TMessageResultNotification) then
  begin
    OnActivityResult( TMessageResultNotification(M).RequestCode,
                      TMessageResultNotification(M).ResultCode,
                      TMessageResultNotification(M).Value);
  end
  else if (M is TMessageReceivedNotification) then
    OnActivityResult( 0, 0, TMessageReceivedNotification(M).Value);
end;

function TACBrTEFSIWebAndroid.OnActivityResult(RequestCode, ResultCode: Integer; AIntent: JIntent): Boolean;
begin
  Result := False;
  fEmTransacao := False;
  GravarLog(Format('TACBrTEFSIWebAndroid.OnActivityResult: RequestCode: %d, ResultCode: %d', [RequestCode, ResultCode]));

  if not Assigned(AIntent) then
  begin
    GravarLog('   no Intent');
    Exit;
  end;

  try
    Result := True;

    if RequestCode = COD_REQUEST_REPORT then
      ObterDadosDaTransacao(AIntent);  // Mapeia as respostas intents para as PWINFOs

    if Assigned(fOnDepoisTerminarTransacao) then
      fOnDepoisTerminarTransacao(AIntent);
  except
    On E: Exception do
      GravarLog('   OnActivityResult - '+E.ClassName+' - '+E.Message);
  end;
end;

procedure TACBrTEFSIWebAndroid.AdicionarDadosObrigatorios(intent: JIntent);
begin
  GravarLog('TACBrTEFSIWebAndroid.AdicionarDadosObrigatorios');

  (*Para executar as funções, é obrigatório enviar 4 parâmetros ao m-SiTef: "empresaSitef", "enderecoSitef",
   "modalidade" e "CNPJ_CPF". Para a modalidade 0, também é obrigatório o parâmetro “valor”.*)

  GravarLog('  '+Key_EmpresaSitef+': '+fDadosTerminal.fCodEmpresa);
  GravarLog('  '+Key_EmpresaSitef+': '+fDadosTerminal.fCodFilial);
  if Trim(fDadosTerminal.fCodEmpresa) <> '' then
    intent.putExtra( StringToJString(Key_EmpresaSitef), StringToJString(fDadosTerminal.fCodEmpresa))
  else
    intent.putExtra( StringToJString(Key_EmpresaSitef), StringToJString(fDadosTerminal.fCodFilial));

  GravarLog('  '+Key_EnderecoSitef+': '+fDadosTerminal.fEnderecoServidor);
  intent.putExtra( StringToJString(Key_EnderecoSitef), StringToJString(fDadosTerminal.fEnderecoServidor));

  GravarLog('  '+Key_CNPJ_CPF+': '+fCNPJEstabelecimento);
  intent.putExtra( StringToJString(Key_CNPJ_CPF), StringToJString(fCNPJEstabelecimento));

  GravarLog('  '+Key_ComExterna+': '+ParametrosAdicionais.ValueInfo[PWOPER_COMEXTERNA]);
  intent.putExtra( StringToJString(Key_ComExterna), StringToJString(ParametrosAdicionais.ValueInfo[PWOPER_COMEXTERNA]));

  if (fDadosAutomacao.CNPJSoftwareHouse.Trim <> '') then
  begin
    GravarLog('  '+Key_CNPJ_AUTOMACAO+': '+fDadosAutomacao.CNPJSoftwareHouse);
    intent.putExtra( StringToJString(Key_CNPJ_AUTOMACAO), StringToJString(fDadosAutomacao.CNPJSoftwareHouse));
  end;
end;

function TACBrTEFSIWebAndroid.GerarTransactionId: String;
begin
  Result := FormatDateTime('YYYYMMDDHHNNSS', Now);
end;

procedure TACBrTEFSIWebAndroid.DoException(AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('EACBrTEFMSitefWeb: '+AErrorMsg);
  raise EACBrTEFSIWebAndroid.Create(AErrorMsg);
end;

procedure TACBrTEFSIWebAndroid.IniciarTransacao(iOPER: Byte; ParametrosAdicionaisTransacao: TACBrTEFParametros);
var
  uriDadosAutomacao, uriPersonalizacao: String;
  i: Integer;
  intent: JIntent;
begin
  if (not fInicializada) then
    DoException(ACBrStr(sErrNOTINIT));

  if fEmTransacao then
    DoException(ACBrStr(sErrTRNINIT));

  GravarLog('TACBrTEFSIWebAndroid.IniciarTransacao');

  // Criando a Intent
  intent := TJIntent.JavaClass.init(StringToJString(Intent_Payment));

  AdicionarDadosObrigatorios(intent);

  ParametrosAdicionais.Clear;  // Limpa para não usar nas próximas transações

  GravarLog('  '+Key_Modalidade+': '+ IntToStr(iOPER));
  intent.putExtra( StringToJString(Key_Modalidade), StringToJString(IntToStr(iOPER)));

  GravarLog('  '+Key_Restricoes+': '+ParametrosAdicionaisTransacao.ValueInfo[PWOPER_RESTRICOES]);
  if ParametrosAdicionaisTransacao.ValueInfo[PWOPER_RESTRICOES] <> '' then
    intent.putExtra( StringToJString(Key_Restricoes), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWOPER_RESTRICOES]));

  GravarLog('  '+Key_TransacoesHabilitadas+': '+ParametrosAdicionaisTransacao.ValueInfo[PWOPER_TRANSHABILITADA]);
  if ParametrosAdicionaisTransacao.ValueInfo[PWOPER_TRANSHABILITADA] <> '' then
    intent.putExtra( StringToJString(Key_TransacoesHabilitadas), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWOPER_TRANSHABILITADA]));

  GravarLog('  '+Key_OTP+': '+ParametrosAdicionaisTransacao.ValueInfo[PWOPER_OTP]);
  if ParametrosAdicionaisTransacao.ValueInfo[PWOPER_OTP] <> '' then
    intent.putExtra( StringToJString(Key_OTP), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWOPER_OTP]));

  GravarLog('  '+Key_AcessibilidadeVisual+': '+ParametrosAdicionaisTransacao.ValueInfo[PWOPER_ACESSIBILIDADEVISUAL]);
  if ParametrosAdicionaisTransacao.ValueInfo[PWOPER_ACESSIBILIDADEVISUAL] <> '' then
    intent.putExtra( StringToJString(Key_AcessibilidadeVisual), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWOPER_ACESSIBILIDADEVISUAL]));

  GravarLog('  '+Key_TipoPinpad+': '+ParametrosAdicionaisTransacao.ValueInfo[PWOPER_TIPOPINPAD]);
  if ParametrosAdicionaisTransacao.ValueInfo[PWOPER_TIPOPINPAD] <> '' then
    intent.putExtra( StringToJString(Key_TipoPinpad), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWOPER_TIPOPINPAD]));

  GravarLog('  '+Key_pinpadMac+': '+fDadosTerminal.PortaPinPad);
  if trim(fDadosTerminal.PortaPinPad) <> '' then
    intent.putExtra( StringToJString(Key_pinpadMac), StringToJString(fDadosTerminal.PortaPinPad));

  GravarLog('  '+Key_Operador+': '+fDadosTerminal.fOperador);
  intent.putExtra( StringToJString(Key_Operador), StringToJString(fDadosTerminal.fOperador));

  GravarLog('  '+Key_Data+': '+FormatDateTime('yyyyMMdd', Date));
  intent.putExtra( StringToJString(Key_Data), StringToJString(FormatDateTime('yyyyMMdd', Date)));

  GravarLog('  '+Key_Hora+': '+FormatDateTime('hhmmss', Time));
  intent.putExtra( StringToJString(Key_Hora), StringToJString(FormatDateTime('hhmmss', Time)));

  GravarLog('  '+Key_NumeroCupom+': '+ParametrosAdicionaisTransacao.ValueInfo[PWINFO_FISCALREF]);
  intent.putExtra( StringToJString(Key_NumeroCupom), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWINFO_FISCALREF]));

  GravarLog('  '+Key_Valor+': '+ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TOTAMNT]);
  intent.putExtra( StringToJString(Key_Valor), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TOTAMNT]));

  GravarLog('  '+Key_ISDoubleValidation+': '+ParametrosAdicionaisTransacao.ValueInfo[PWOPER_DOUBLEVALIDATION]);
  intent.putExtra( StringToJString(Key_ISDoubleValidation), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWOPER_DOUBLEVALIDATION]));

  //Disparando o Intent
  IniciarIntent(intent);
end;

procedure TACBrTEFSIWebAndroid.IniciarIntent(AIntent: JIntent);
begin
  if Assigned(fOnAntesIniciarTransacao) then
    fOnAntesIniciarTransacao(AIntent);

  fEmTransacao := True;
  fDadosTransacao.Clear;

  TAndroidHelper.Activity.startActivityForResult(AIntent, COD_REQUEST_REPORT);
end;

procedure TACBrTEFSIWebAndroid.ObterDadosDaTransacao(AIntent: JIntent);
  function buscarStringExtra(name : string) : string;
  begin
    Result := String(DecodeURL( AnsiString(JStringToString(AIntent.getStringExtra(StringToJString(name))))));
  end;

var
  jsonTipoCampos    : string;
  js                : TACBrJSONObject;
  viaCliente        : Boolean;
  viaEstabelecimento: Boolean;
begin
  if not Assigned(AIntent) then
  begin
    GravarLog('[ObterDadosDaTransacao] no Intent to read');
    Exit;
  end;

  viaCliente        := False;
  viaEstabelecimento:= False;

  fDadosTransacao.Clear;
  fDadosTransacao.ValueInfo[PWINFO_RET]          := buscarStringExtra('CODRESP');
  fDadosTransacao.ValueInfo[PWINFO_RESULTMSG]    := traduzRetorno(StrToIntDef(fDadosTransacao.ValueInfo[PWINFO_RET], 0));
  fDadosTransacao.ValueInfo[PWINFO_AUTRESPCODE]  := buscarStringExtra('COMP_DADOS_CONF');
  fDadosTransacao.ValueInfo[PWINFO_CARDTYPE]     := buscarStringExtra('CODTRANS');
  fDadosTransacao.ValueInfo[PWINFO_FINTYPE]      := buscarStringExtra('TIPO_PARC');
  fDadosTransacao.ValueInfo[PWINFO_CASHBACKAMT]  := buscarStringExtra('VLTROCO');
  fDadosTransacao.ValueInfo[PWINFO_AUTHSYST]     := buscarStringExtra('REDE_AUT');
  fDadosTransacao.ValueInfo[PWINFO_CARDNAME]     := buscarStringExtra('BANDEIRA');
  fDadosTransacao.ValueInfo[PWINFO_BANDCODE]     := buscarStringExtra('BANDEIRA');
  fDadosTransacao.ValueInfo[PWINFO_AUTLOCREF]    := buscarStringExtra('NSU_SITEF');
  fDadosTransacao.ValueInfo[PWINFO_REQNUM]       := buscarStringExtra('NSU_SITEF');
  fDadosTransacao.ValueInfo[PWINFO_AUTEXTREF]    := buscarStringExtra('NSU_HOST');
  fDadosTransacao.ValueInfo[PWINFO_AUTHCODE]     := buscarStringExtra('COD_AUTORIZACAO');
  fDadosTransacao.ValueInfo[PWINFO_INSTALLMENTS] := buscarStringExtra('NUM_PARC');
  fDadosTransacao.ValueInfo[PWINFO_CNFREQ]       := '0'; // Necessidade de confirmação: 0: não requer confirmação; 1: requer confirmação.

  viaCliente         := trim(buscarStringExtra('VIA_CLIENTE')) <> '';
  viaEstabelecimento := trim(buscarStringExtra('VIA_ESTABELECIMENTO')) <> '';

  if viaEstabelecimento then
    fDadosTransacao.ValueInfo[PWINFO_RCPTMERCH] := buscarStringExtra('VIA_ESTABELECIMENTO');

  if viaCliente then
    fDadosTransacao.ValueInfo[PWINFO_RCPTCHOLDER]  := buscarStringExtra('VIA_CLIENTE');

  //Qtd. Comprovantes
  fDadosTransacao.ValueInfo[PWINFO_RCPTPRN] := IntToStr(PrintReceiptsToPWINFO_RCPTPRN(viaCliente, viaEstabelecimento));//MSitef não retorna número de comprovantes

  if ValorTotalPagamento > 0 then //Não é retornado via intents
    fDadosTransacao.ValueInfo[PWINFO_TOTAMNT] := IntToStr(Trunc(RoundTo(ValorTotalPagamento * 100,-2)));

  fDadosTransacao.ValueInfo[PWINFO_MERCHADDDATA1]:= buscarStringExtra('TIPO_CAMPOS');

  (*[TIPO_CAMPOS]JsonObject contendo todos os campos da transação com a CliSiTef, incluindo os outros campos da tabela SITEF. Formato: { “Id TipoCampo N” : [ “valor 1”, ....] }
  Os valores são sempre passados em JsonArray, mesmo ocorrendo somente uma ocorrência do campo pela CliSiTef.*
  Para descobrir o significado de cada campo contido nessa lista favor olhar a tabela “Tabela de valores para TipoCampo” no documento SiTef - Interface Simplificada
  com a aplicação seção 5.3.2*)
  jsonTipoCampos := buscarStringExtra('TIPO_CAMPOS');
  if jsonTipoCampos <> '' then
  begin
    js := TACBrJSONObject.Parse(jsonTipoCampos);
    try

      // campo 131 retorna o Código das Redes Autorizadoras-verificar como implementar isso
      if (js.AsJSONArray['136'].Count > 0) then
        fDadosTransacao.ValueInfo[PWINFO_CARDPARCPAN]        := js.AsJSONArray['136'].Items[0]; //Contém as 6 primeiras posições do cartão (bin)

      if (js.AsJSONArray['157'].Count > 0) then
        fDadosTransacao.ValueInfo[PWINFO_VIRTMERCH]          := js.AsJSONArray['157'].Items[0]; //Código de Estabelecimento

      if (js.AsJSONArray['158'].Count > 0) then
        fDadosTransacao.ValueInfo[PWINFO_AUTMERCHID]         := js.AsJSONArray['158'].Items[0]; //Código da Rede Autorizadora

      if (js.AsJSONArray['2003'].Count > 0) then
        fDadosTransacao.ValueInfo[PWINFO_CHOLDERNAME]        := js.AsJSONArray['2003'].Items[0]; //Nome do portador

      if (js.AsJSONArray['2021'].Count > 0) then
      begin
        //Número do cartão mascarado no formato BIN + *** + 4 últimos dígitos. Ex: 543211******987
        fDadosTransacao.ValueInfo[PWINFO_DEFAULTCARDPARCPAN] := js.AsJSONArray['2021'].Items[0];
        fDadosTransacao.ValueInfo[PWINFO_CARDFULLPAN]        := js.AsJSONArray['2021'].Items[0];
      end;

      if (js.AsJSONArray['2090'].Count > 0) then
        fDadosTransacao.ValueInfo[PWINFO_CARDENTMODE]        := js.AsJSONArray['2090'].Items[0]; //Tipo do cartão Lido

      if (js.AsJSONArray['2601'].Count > 0) then
        fDadosTransacao.ValueInfo[PWINFO_PROCESSMSG]         := js.AsJSONArray['2601'].Items[0]; //Mensagem para pinpad
    finally
      js.Free;
    end;
  end;
end;

function TACBrTEFSIWebAndroid.ObterUltimoRetorno: String;
begin
  if (fDadosTransacao.Count < 1) then
    ObterDadosDaTransacao(nil);

  Result := fDadosTransacao.ValueInfo[PWINFO_RESULTMSG];
end;

procedure TACBrTEFSIWebAndroid.GravarLog(const AString: AnsiString; Traduz: Boolean);
Var
  Tratado: Boolean;
  AStringLog: AnsiString;
begin
  if not Assigned(fOnGravarLog) then
    Exit;

  if Traduz then
    AStringLog := AnsiString(TranslateUnprintable(AString))
  else
    AStringLog := AString;

  Tratado := False;
  fOnGravarLog(String(AStringLog), Tratado);
end;

procedure TACBrTEFSIWebAndroid.GravarLog(const AString: String; Traduz: Boolean);
begin
  GravarLog(AnsiString(AString), Traduz);
end;

procedure TACBrTEFSIWebAndroid.SetCNPJEstabelecimento(AValue: String);
var
  ACNPJ, ErroMsg: String;
begin
  if fCNPJEstabelecimento = AValue then
    Exit;

  ACNPJ := OnlyNumber(AValue);
  if (ACNPJ <> '') then
  begin
    ErroMsg := ACBrValidador.ValidarCNPJ(ACNPJ);
    if (ErroMsg <> '') then
      DoException('SetCNPJEstabelecimento: '+ErroMsg);
  end;

  fCNPJEstabelecimento := ACNPJ;
end;

procedure TACBrTEFSIWebAndroid.SetNomeEstabelecimento(AValue: String);
begin
  if fNomeEstabelecimento = AValue then
    Exit;

  fNomeEstabelecimento := LeftStr(Trim(AValue), 100);
end;

procedure TACBrTEFSIWebAndroid.ConfirmarTransacao(AStatus: LongWord; confirmTransactionIdentifier: String);
begin
  GravarLog('ConfirmarTransacao: Não disponível no MSitef');
end;

procedure TACBrTEFSIWebAndroid.TratarTransacaoPendente(AStatus: LongWord;
                                                       pszReqNum: String;
                                                       pszLocRef: String;
                                                       pszExtRef: String;
                                                       pszVirtMerch: String;
                                                       pszAuthSyst: String);
begin
  GravarLog('TratarTransacaoPendente: Não disponível no MSitef');
end;

procedure TACBrTEFSIWebAndroid.ResolverTransacaoPendente(AStatus: LongWord; pszReqNum, pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst: String);
begin
  GravarLog('ResolverTransacaoPendente: Não disponível no MSitef');
end;

{ TACBrTEFSIWebAndroidDadosTerminal }

procedure TACBrTEFSIWebAndroidDadosTerminal.Clear;
begin
  fCodEmpresa       := '';
  fCodFilial        := '';
  fCodTerminal      := '';
  fEnderecoServidor := '';
  fOperador         := '';
  fPortaPinPad      := '';
end;

constructor TACBrTEFSIWebAndroidDadosTerminal.Create;
begin
  inherited;
  Clear;
end;

function PrintReceiptsToPWINFO_RCPTPRN(const viaCliente: boolean; viaEstabelecimento : boolean): Byte;
begin
  Result := 0;

  // 0: não há comprovante
  // 1: imprimir somente a via do Cliente
  // 2: imprimir somente a via do Estabelecimento
  // 3: imprimir ambas as vias do Cliente e do Estabelecimento
  if (not(viaCliente) and not(viaEstabelecimento)) then
    Result := 0
  else if viaCliente and not(viaEstabelecimento) then
    Result := 1
  else if not(viaCliente) and viaEstabelecimento then
    Result := 2
  else if viaCliente and viaEstabelecimento then
    Result := 3
  else
    Result := 0;
end;

end.
