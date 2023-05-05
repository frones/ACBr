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

unit ACBrTEFElginIDHAndroidAPI;

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
  ACBrTEFComum,
  ACBrTEFElginIDHComum,
  ACBrJSON,
  ACBrBase;

resourcestring
  sErrNOTINIT = 'TACBrTEFELWebAndroid não foi inicializada';
  sErrTRNINIT = 'Já foi iniciada uma Transação[Elgin]';
  sErrTRNNOTINIT = 'Não foi iniciada uma Transação';
  sErrINTENTNOTFOUND = 'Ação %s não encontrada.';
  sErrCANCELED = 'Operação TEF cancelada';

const
  CACBrTEFELWebAndroidName = 'ACBrTEFELWebAndroid';
  CACBrTEFELWebAndroidVersao = '0.1.0';
  Intent_Payment = 'com.elgin.e1.digitalhub.TEF';

  COD_REQUEST_REPORT = 2241;
  Key_EmpresaSitef  = 'empresaSitef';
  Key_Modalidade    = 'modalidade';
  Key_Operador      = 'operador';
  Key_Data          = 'data';
  Key_Hora          = 'hora';
  Key_NumeroCupom   = 'numeroCupom';
  Key_NumParcelas   = 'numParcelas';
  Key_Valor         = 'valor';
  Key_CNPJ_CPF      = 'CNPJ_CPF';
  Key_Restricoes    = 'restricoes';
  Key_NsuSitef      = 'NSU_SITEF';
  Key_TransacoesHabilitadas  = 'transacoesHabilitadas';

type
  EACBrTEFELWebAndroid = class(EACBrTEFErro);

  TACBrTEFELWebAndroidEstadoTransacao = procedure(AIntent: JIntent) of object;
  TACBrTEFELWebAndroidAvaliarTransacaoPendente = procedure(pszReqNum: String; pszLocRef: String; pszExtRef: String; pszVirtMerch: String; pszAuthSyst: String) of object;

  TACBrTEFELWebAndroidDadosAutomacao = class
  private
    fSuportaSaque: Boolean;
    fSuportaDesconto: Boolean;
    fSuportaViasDiferenciadas: Boolean;
    fUtilizaSaldoTotalVoucher: Boolean;
    fImprimeViaClienteReduzida: Boolean;

  public
    constructor Create;
    procedure Clear;
    Property SuportaSaque: Boolean read fSuportaSaque write fSuportaSaque default False;
    Property SuportaDesconto: Boolean read fSuportaDesconto write fSuportaDesconto default False;
    property ImprimeViaClienteReduzida: Boolean read fImprimeViaClienteReduzida write fImprimeViaClienteReduzida default True;
    property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas write fSuportaViasDiferenciadas default True;
    property UtilizaSaldoTotalVoucher: Boolean read fUtilizaSaldoTotalVoucher write fUtilizaSaldoTotalVoucher default False;
  end;

  TACBrTEFELWebAndroidDadosTerminal = class
  private
    fCodEmpresa: String;
    fCodFilial: String;
    fCodTerminal: String;
    fOperador: String;
  public
    constructor Create;
    procedure Clear;
  published
    property CodEmpresa: String read fCodEmpresa write fCodEmpresa;
    property CodFilial: String read fCodFilial write fCodFilial;
    property CodTerminal: String read fCodTerminal write fCodTerminal;
    property Operador: String read fOperador write fOperador;
  end;

  TACBrTEFELWebAndroid = class
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
    fOnAntesIniciarTransacao: TACBrTEFELWebAndroidEstadoTransacao;
    fOnDepoisTerminarTransacao: TACBrTEFELWebAndroidEstadoTransacao;
    fOnAvaliarTransacaoPendente: TACBrTEFELWebAndroidAvaliarTransacaoPendente;
    fDadosAutomacao: TACBrTEFELWebAndroidDadosAutomacao;
    fDadosTerminal : TACBrTEFELWebAndroidDadosTerminal;

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

    property DadosAutomacao: TACBrTEFELWebAndroidDadosAutomacao read fDadosAutomacao;
    property DadosTerminal : TACBrTEFELWebAndroidDadosTerminal read fDadosTerminal;

    property ParametrosAdicionais: TACBrTEFParametros read fParametrosAdicionais;

    property ConfirmarTransacoesPendentesNoHost: Boolean read fConfirmarTransacoesPendentesNoHost write fConfirmarTransacoesPendentesNoHost;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
    property OnAntesIniciarTransacao: TACBrTEFELWebAndroidEstadoTransacao read fOnAntesIniciarTransacao write fOnAntesIniciarTransacao;
    property OnDepoisTerminarTransacao: TACBrTEFELWebAndroidEstadoTransacao read fOnDepoisTerminarTransacao write fOnDepoisTerminarTransacao;
    property OnAvaliarTransacaoPendente: TACBrTEFELWebAndroidAvaliarTransacaoPendente read fOnAvaliarTransacaoPendente write fOnAvaliarTransacaoPendente;
  end;

function OperationToPWOPER_(const AOperation: String): Byte;
function PrintReceiptsToPWINFO_RCPTPRN(const viaCliente: boolean; viaEstabelecimento : boolean): Byte;

implementation
uses
  StrUtils,
  synacode,
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

{ TACBrTEFELWebAndroidDadosAutomacao }
constructor TACBrTEFELWebAndroidDadosAutomacao.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFELWebAndroidDadosAutomacao.Clear;
begin
  fImprimeViaClienteReduzida := True;
  fSuportaDesconto := False;
  fSuportaSaque := False;
  fSuportaViasDiferenciadas := True;
  fUtilizaSaldoTotalVoucher := False;
end;

{ TACBrTEFPGWebAndroid }
constructor TACBrTEFELWebAndroid.Create;
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

  fDadosAutomacao := TACBrTEFELWebAndroidDadosAutomacao.Create;
  fDadosTerminal := TACBrTEFELWebAndroidDadosTerminal.Create;

  fDadosTransacao := TACBrTEFParametros.Create;
  fParametrosAdicionais := TACBrTEFParametros.Create;
end;

destructor TACBrTEFELWebAndroid.Destroy;
begin
  fDadosAutomacao.Free;
  fDadosTerminal.Free;

  fDadosTransacao.Free;
  fParametrosAdicionais.Free;
  inherited;
end;

procedure TACBrTEFELWebAndroid.SetInicializada(AValue: Boolean);
begin
  if fInicializada = AValue then
    Exit;

  GravarLog('TACBrTEFPGWebAndroid.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFELWebAndroid.Inicializar;
begin
  GravarLog('TACBrTEFELWebAndroid.Inicializar');
  fMessageSubscriptionID := TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, HandleActivityMessage);
  fInicializada := True;
  fEmTransacao := False;
end;

procedure TACBrTEFELWebAndroid.DesInicializar;
begin
  GravarLog('TACBrTEFELWebAndroid.DesInicializar');
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, HandleActivityMessage);
  fInicializada := False;
  fEmTransacao := False;
end;

procedure TACBrTEFELWebAndroid.HandleActivityMessage(const Sender: TObject; const M: TMessage);
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

function TACBrTEFELWebAndroid.OnActivityResult(RequestCode, ResultCode: Integer; AIntent: JIntent): Boolean;
begin
  Result := False;
  fEmTransacao := False;
  GravarLog(Format('TACBrTEFELWebAndroid.OnActivityResult: RequestCode: %d, ResultCode: %d', [RequestCode, ResultCode]));

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

procedure TACBrTEFELWebAndroid.AdicionarDadosObrigatorios(intent: JIntent);
begin
  GravarLog('TACBrTEFELWebAndroid.AdicionarDadosObrigatorios');

  (*Para executar as funções, é obrigatório enviar 3 parâmetros ao Elgin IDH: "empresaSitef", "enderecoSitef",
   "modalidade" e "CNPJ_CPF". Para a modalidade 0, também é obrigatório o parâmetro “valor”.*)

  GravarLog('  '+Key_EmpresaSitef+': '+fDadosTerminal.fCodEmpresa);
  GravarLog('  '+Key_EmpresaSitef+': '+fDadosTerminal.fCodFilial);
  if Trim(fDadosTerminal.fCodEmpresa) <> '' then
    intent.putExtra( StringToJString(Key_EmpresaSitef), StringToJString(fDadosTerminal.fCodEmpresa))
  else
    intent.putExtra( StringToJString(Key_EmpresaSitef), StringToJString(fDadosTerminal.fCodFilial));

  GravarLog('  '+Key_CNPJ_CPF+': '+fCNPJEstabelecimento);
  intent.putExtra( StringToJString(Key_CNPJ_CPF), StringToJString(fCNPJEstabelecimento));
end;

function TACBrTEFELWebAndroid.GerarTransactionId: String;
begin
  Result := FormatDateTime('YYYYMMDDHHNNSS', Now);
end;

procedure TACBrTEFELWebAndroid.DoException(AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('EACBrTEFElginIDHWeb: '+AErrorMsg);
  raise EACBrTEFELWebAndroid.Create(AErrorMsg);
end;

procedure TACBrTEFELWebAndroid.IniciarTransacao(iOPER: Byte; ParametrosAdicionaisTransacao: TACBrTEFParametros);
var
  uriDadosAutomacao, uriPersonalizacao: String;
  i: Integer;
  intent: JIntent;
begin
  if (not fInicializada) then
    DoException(ACBrStr(sErrNOTINIT));

  if fEmTransacao then
    DoException(ACBrStr(sErrTRNINIT));

  GravarLog('TACBrTEFELWebAndroid.IniciarTransacao');

  // Criando a Intent
  intent := TJIntent.JavaClass.init(StringToJString(Intent_Payment));

  AdicionarDadosObrigatorios(intent);

  ParametrosAdicionais.Clear;  // Limpa para não usar nas próximas transações

  GravarLog('  '+Key_Modalidade+': '+ IntToStr(iOPER));
  intent.putExtra( StringToJString(Key_Modalidade), StringToJString(IntToStr(iOPER)));

  if iOPER = PWOPER_SALEVOID {Cancelamento} then
  begin
    GravarLog('  '+Key_Valor+': '+ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TRNORIGAMNT]);
    intent.putExtra( StringToJString(Key_Valor), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TRNORIGAMNT]));

    GravarLog('  '+Key_Data+': '+ ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TRNORIGDATE]);
    intent.putExtra( StringToJString(Key_Data), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TRNORIGDATE]));

    GravarLog('  '+Key_NsuSitef+': '+ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TRNORIGNSU]);
    if ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TRNORIGNSU] <> '' then
      intent.putExtra( StringToJString(Key_NsuSitef), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TRNORIGNSU]));
  end
  else
  begin
    GravarLog('  '+Key_Restricoes+': '+ParametrosAdicionaisTransacao.ValueInfo[PWOPER_RESTRICOES]);
    if ParametrosAdicionaisTransacao.ValueInfo[PWOPER_RESTRICOES] <> '' then
      intent.putExtra( StringToJString(Key_Restricoes), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWOPER_RESTRICOES]));

    GravarLog('  '+Key_TransacoesHabilitadas+': '+ParametrosAdicionaisTransacao.ValueInfo[PWOPER_TRANSHABILITADA]);
    if ParametrosAdicionaisTransacao.ValueInfo[PWOPER_TRANSHABILITADA] <> '' then
      intent.putExtra( StringToJString(Key_TransacoesHabilitadas), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWOPER_TRANSHABILITADA]));

    GravarLog('  '+Key_Operador+': '+fDadosTerminal.fOperador);
    intent.putExtra( StringToJString(Key_Operador), StringToJString(fDadosTerminal.fOperador));

    GravarLog('  '+Key_Data+': '+FormatDateTime('yyyyMMdd', Date));
    intent.putExtra( StringToJString(Key_Data), StringToJString(FormatDateTime('yyyyMMdd', Date)));

    GravarLog('  '+Key_Hora+': '+FormatDateTime('hhmmss', Time));
    intent.putExtra( StringToJString(Key_Hora), StringToJString(FormatDateTime('hhmmss', Time)));

    GravarLog('  '+Key_NumeroCupom+': '+ParametrosAdicionaisTransacao.ValueInfo[PWINFO_FISCALREF]);
    intent.putExtra( StringToJString(Key_NumeroCupom), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWINFO_FISCALREF]));

    GravarLog('  '+Key_NumParcelas+': '+ParametrosAdicionaisTransacao.ValueInfo[PWINFO_INSTALLMENTS]);
    intent.putExtra( StringToJString(Key_NumParcelas), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWINFO_INSTALLMENTS]));

    GravarLog('  '+Key_Valor+': '+ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TOTAMNT]);
    intent.putExtra( StringToJString(Key_Valor), StringToJString(ParametrosAdicionaisTransacao.ValueInfo[PWINFO_TOTAMNT]));
  end;

  //Disparando o Intent
  IniciarIntent(intent);
end;

procedure TACBrTEFELWebAndroid.IniciarIntent(AIntent: JIntent);
begin
  if Assigned(fOnAntesIniciarTransacao) then
    fOnAntesIniciarTransacao(AIntent);

  fEmTransacao := True;
  fDadosTransacao.Clear;

  TAndroidHelper.Activity.startActivityForResult(AIntent, COD_REQUEST_REPORT);
end;

procedure TACBrTEFELWebAndroid.ObterDadosDaTransacao(AIntent: JIntent);
  function buscarStringExtra(name : string) : string;
  begin
    Result := String(DecodeURL( AnsiString(JStringToString(AIntent.getStringExtra(StringToJString(name))))));
    //Result := String(JStringToString(AIntent.getStringExtra(StringToJString(name))));
//    Result := AnsiString(JStringToString(AIntent.getStringExtra(StringToJString(name))));
  end;

var
  viaCliente        : Boolean;
  viaEstabelecimento: Boolean;
  jsonComplementoDados    : string;
  js                : TACBrJSONObject;
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
  fDadosTransacao.ValueInfo[PWINFO_MERCHADDDATA1]:= buscarStringExtra('COMP_DADOS_CONF');
  fDadosTransacao.ValueInfo[PWINFO_CARDTYPE]     := buscarStringExtra('CODTRANS');
  fDadosTransacao.ValueInfo[PWINFO_FINTYPE]      := buscarStringExtra('TIPO_PARC');
  fDadosTransacao.ValueInfo[PWINFO_CASHBACKAMT]  := buscarStringExtra('VLTROCO');
  fDadosTransacao.ValueInfo[PWINFO_AUTHSYST]     := buscarStringExtra('REDE_AUT');
  fDadosTransacao.ValueInfo[PWINFO_CARDNAME]     := buscarStringExtra('BADEIRA');
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

  jsonComplementoDados := buscarStringExtra('COMP_DADOS_CONF');
  if jsonComplementoDados <> '' then
  begin
    js := TACBrJSONObject.Parse(jsonComplementoDados);
    try
      if (js.AsString['nsuRede'] <> '') then
        fDadosTransacao.ValueInfo[PWINFO_AUTMERCHID]       := js.AsString['nsuRede']; //Código da Rede Autorizadora

      if (js.AsString['cnpjRede'] <> '') then
        fDadosTransacao.ValueInfo[PWINFO_REDECNPJ]         := js.AsString['cnpjRede']; //CNPJ da rede

      if (js.AsString['administradora'] <> '') then
        fDadosTransacao.ValueInfo[PWINFO_CARDNAMESTD]      := js.AsString['administradora']; //CNPJ da rede

//      if (js.AsString['data'] <> '') then
//        fDadosTransacao.ValueInfo[PWINFO_DATETIME]         := js.AsString['data']; //Data e hora da transação

      if (js.AsString['numeroCartao'] <> '') then
      begin
        //Número do cartão mascarado no formato BIN + *** + 4 últimos dígitos. Ex: 543211******987
        fDadosTransacao.ValueInfo[PWINFO_CARDPARCPAN]        := Copy(js.AsString['numeroCartao'], 1, 6); //Contém as 6 primeiras posições do cartão (bin)
        fDadosTransacao.ValueInfo[PWINFO_DEFAULTCARDPARCPAN] := js.AsString['numeroCartao'];
        fDadosTransacao.ValueInfo[PWINFO_CARDFULLPAN]        := js.AsString['numeroCartao'];
      end;

      if (js.AsString['tipoCartao'] <> '') then
        fDadosTransacao.ValueInfo[PWINFO_CARDENTMODE]        := js.AsString['tipoCartao']; //Tipo do cartão Lido

      if (js.AsString['mensagem'] <> '') then
        fDadosTransacao.ValueInfo[PWINFO_PROCESSMSG]         := js.AsString['mensagem']; //Mensagem para pinpad
    finally
      js.Free;
    end;
  end;
  //TIPO_CAMPOS não retorna na aplicação da Elgin
end;

function TACBrTEFELWebAndroid.ObterUltimoRetorno: String;
begin
  if (fDadosTransacao.Count < 1) then
    ObterDadosDaTransacao(nil);

  Result := fDadosTransacao.ValueInfo[PWINFO_RESULTMSG];
end;

procedure TACBrTEFELWebAndroid.GravarLog(const AString: AnsiString; Traduz: Boolean);
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

procedure TACBrTEFELWebAndroid.GravarLog(const AString: String; Traduz: Boolean);
begin
  GravarLog(AnsiString(AString), Traduz);
end;

procedure TACBrTEFELWebAndroid.SetCNPJEstabelecimento(AValue: String);
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

procedure TACBrTEFELWebAndroid.SetNomeEstabelecimento(AValue: String);
begin
  if fNomeEstabelecimento = AValue then
    Exit;

  fNomeEstabelecimento := LeftStr(Trim(AValue), 100);
end;

procedure TACBrTEFELWebAndroid.ConfirmarTransacao(AStatus: LongWord; confirmTransactionIdentifier: String);
begin
  GravarLog('ConfirmarTransacao: Não disponível na Elgin');
end;

procedure TACBrTEFELWebAndroid.TratarTransacaoPendente(AStatus: LongWord;
                                                       pszReqNum: String;
                                                       pszLocRef: String;
                                                       pszExtRef: String;
                                                       pszVirtMerch: String;
                                                       pszAuthSyst: String);
begin
  GravarLog('TratarTransacaoPendente: Não disponível no Elgin');
end;

procedure TACBrTEFELWebAndroid.ResolverTransacaoPendente(AStatus: LongWord; pszReqNum, pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst: String);
begin
  GravarLog('ResolverTransacaoPendente: Não disponível no Elgin');
end;

{ TACBrTEFELWebAndroidDadosTerminal }

procedure TACBrTEFELWebAndroidDadosTerminal.Clear;
begin
  fCodEmpresa       := '';
  fCodFilial        := '';
  fCodTerminal      := '';
  fOperador         := '';
end;

constructor TACBrTEFELWebAndroidDadosTerminal.Create;
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
