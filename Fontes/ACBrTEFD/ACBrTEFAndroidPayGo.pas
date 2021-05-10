{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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

unit ACBrTEFAndroidPayGo;

interface

uses
  Classes, SysUtils,
  Androidapi.JNI.GraphicsContentViewText,
  ACBrTEFComum, ACBrTEFPayGoComum, ACBrTEFAndroid, ACBrTEFPayGoAndroidAPI;

const
  PWRET_OK = 0;      // Operação bem sucedida

resourcestring
  sACBrTEFAndroidIdentificadorVendaVazioException = 'IdentificadorVenda não pode ser vazio';
  sACBrTEFAndroidValorPagamentoInvalidoException = 'Valor do Pagamento inválido';

  sACBrTEFAndroidTransacaoPendenteAPI = 'Transação Pendente.' + sLineBreak +
                                         'Rede: %s' + sLineBreak +
                                         'NSU: %s';

type
  TACBrTEFAndroidPayGoClass = class(TACBrTEFAndroidClass)
  private
    fTEFPayGoAPI: TACBrTEFPGWebAndroid;
    fpndReqNum: String;
    fPndLocRef: String;
    fPndExtRef: String;
    fPndVirtMerch: String;
    fPndAuthSyst: String;
    fOperacaoVenda: Byte;
    fOperacaoAdministrativa: Byte;
    fOperacaoCancelamento: Byte;
    fAutorizador: String;

    procedure QuandoIniciarTransacaoAPI(AIntent: JIntent);
    procedure QuandoFianlizarTransacaoAPI(AIntent: JIntent);
    procedure QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure QuandoAvaliarTransacaoPendenteAPI(pszReqNum: String; pszLocRef: String;
      pszExtRef: String; pszVirtMerch: String; pszAuthSyst: String);

    procedure LimparTransacaoPendente;
    procedure VerificarIdentificadorVendaInformado(IdentificadorVenda: String);

  protected
    procedure InicializarOperacaoTEF; override;
    procedure InterpretarDadosDaTransacao; override;
    procedure FinalizarOperacaoTEF; override;

  public
    constructor Create(AACBrTEFAndroid: TACBrTEFAndroid);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    procedure EfetuarPagamento(
      IdentificadorVenda: String;
      ValorPagto: Currency;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Modalidade: TACBrTEFModalidadePagamento = TACBrTEFModalidadePagamento.tefmpNaoDefinido;
      Financiamento: TACBrTEFModalidadeFinanciamento = TACBrTEFModalidadeFinanciamento.tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDate = 0); override;

    procedure EfetuarAdministrativa(
      Operacao: string = '';
      IdentificadorVenda: string = ''); override;

    procedure CancelarTransacao(
      NSU, CodigoAutorizacaoTransacao: string;
      DataHoraTransacao: TDateTime;
      Valor: Double;
      CodigoFinalizacao: string = '';
      Rede: string = ''); override;

    procedure FinalizarOperacao(
      Rede, NSU, CodigoFinalizacao: String;
      Status: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoAutomatico); override;

    procedure ResolverOperacaoPendente(
      Status: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoManual); override;

    property TEFPayGoAPI: TACBrTEFPGWebAndroid read fTEFPayGoAPI;
    property OperacaoVenda: Byte read fOperacaoVenda
      write fOperacaoVenda default PWOPER_SALE;
    property OperacaoAdministrativa: Byte read fOperacaoAdministrativa
      write fOperacaoAdministrativa default PWOPER_ADMIN;
    property OperacaoCancelamento: Byte read fOperacaoCancelamento
      write fOperacaoCancelamento default PWOPER_SALEVOID;
    property Autorizador: String read fAutorizador write fAutorizador;
  end;

function StatusTransacaoToPWCNF_( AStatus: TACBrTEFStatusTransacao): LongWord;

implementation

uses
  math;

function StatusTransacaoToPWCNF_( AStatus: TACBrTEFStatusTransacao): LongWord;
begin
  case AStatus of
    tefstsSucessoAutomatico: Result := PWCNF_CNF_AUTO;
    tefstsSucessoManual: Result := PWCNF_CNF_MANU_AUT;
    tefstsErroImpressao: Result := PWCNF_REV_PRN_AUT;
    tefstsErroDispesador: Result := PWCNF_REV_DISP_AUT;
  else
    Result := PWCNF_REV_MANU_AUT;
  end;
end;

{ TACBrTEFAndroidPayGoClass }

constructor TACBrTEFAndroidPayGoClass.Create(AACBrTEFAndroid: TACBrTEFAndroid);
begin
  inherited;
  fOperacaoVenda := PWOPER_SALE;
  fOperacaoAdministrativa := PWOPER_ADMIN;
  fOperacaoCancelamento := PWOPER_SALEVOID;
  fAutorizador := '';

  fTEFPayGoAPI := TACBrTEFPGWebAndroid.Create;
  fTEFPayGoAPI.OnGravarLog := QuandoGravarLogAPI;
  fTEFPayGoAPI.OnDepoisTerminarTransacao := QuandoFianlizarTransacaoAPI;
  fTEFPayGoAPI.OnAntesIniciarTransacao := QuandoIniciarTransacaoAPI;
end;

destructor TACBrTEFAndroidPayGoClass.Destroy;
begin
  fTEFPayGoAPI.Free;
  inherited;
end;

procedure TACBrTEFAndroidPayGoClass.Inicializar;
begin
  with fTEFPayGoAPI do
  begin
    with DadosAutomacao do
    begin
      NomeAplicacao := fpACBrTEFAndroid.DadosAutomacao.NomeAplicacao;
      VersaoAplicacao := fpACBrTEFAndroid.DadosAutomacao.VersaoAplicacao;
      SoftwareHouse := fpACBrTEFAndroid.DadosAutomacao.NomeSoftwareHouse;
      NomeEstabelecimento := fpACBrTEFAndroid.DadosEstabelecimento.RazaoSocial;
      SuportaSaque := fpACBrTEFAndroid.DadosAutomacao.SuportaDesconto;
      SuportaDesconto := fpACBrTEFAndroid.DadosAutomacao.SuportaDesconto;
      SuportaViasDiferenciadas := fpACBrTEFAndroid.DadosAutomacao.SuportaViasDiferenciadas;
      ImprimeViaClienteReduzida := fpACBrTEFAndroid.DadosAutomacao.ImprimeViaClienteReduzida;
      UtilizaSaldoTotalVoucher := fpACBrTEFAndroid.DadosAutomacao.UtilizaSaldoTotalVoucher;
    end;

    With Personalizacao do
    begin
      corFundoTela := fpACBrTEFAndroid.Personalizacao.corFundoTela;
      corFundoToolbar := fpACBrTEFAndroid.Personalizacao.corFundoToolbar;
      corFonte := fpACBrTEFAndroid.Personalizacao.corFonte;
      corSeparadorMenu := fpACBrTEFAndroid.Personalizacao.corSeparadorMenu;
      corFundoCaixaEdicao := fpACBrTEFAndroid.Personalizacao.corFundoCaixaEdicao;
      corTextoCaixaEdicao := fpACBrTEFAndroid.Personalizacao.corTextoCaixaEdicao;
      corFundoTeclado := fpACBrTEFAndroid.Personalizacao.corFundoTeclado;
      corTeclaLiberadaTeclado := fpACBrTEFAndroid.Personalizacao.corTeclaLiberadaTeclado;
      corTeclaPressionadaTeclado := fpACBrTEFAndroid.Personalizacao.corTeclaPressionadaTeclado;
      corFonteTeclado := fpACBrTEFAndroid.Personalizacao.corFonteTeclado;
      pathIconeToolbar := fpACBrTEFAndroid.Personalizacao.ArquivoIcone;
      pathFonte := fpACBrTEFAndroid.Personalizacao.ArquivoFonte;
    end;

    ConfirmarTransacoesPendentesNoHost := True;
    if fpACBrTEFAndroid.AutoConfirmarTransacoesPendente then
      OnAvaliarTransacaoPendente := Nil
    else
      OnAvaliarTransacaoPendente := QuandoAvaliarTransacaoPendenteAPI;

    Inicializar;
  end;

  LimparTransacaoPendente;

  inherited;
end;

procedure TACBrTEFAndroidPayGoClass.InicializarOperacaoTEF;
begin
  LimparTransacaoPendente;
end;

procedure TACBrTEFAndroidPayGoClass.FinalizarOperacaoTEF;
begin
  { Nada a fazer... }
end;

procedure TACBrTEFAndroidPayGoClass.DesInicializar;
begin
  fTEFPayGoAPI.DesInicializar;
  inherited;
end;

procedure TACBrTEFAndroidPayGoClass.QuandoGravarLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  fpACBrTEFAndroid.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAndroidPayGoClass.QuandoIniciarTransacaoAPI(AIntent: JIntent);
begin
  if Assigned(fpACBrTEFAndroid.QuandoIniciarTransacao) then
    fpACBrTEFAndroid.QuandoIniciarTransacao(AIntent);
end;

procedure TACBrTEFAndroidPayGoClass.ResolverOperacaoPendente(
  Status: TACBrTEFStatusTransacao);
var
  PGWebStatus: LongWord;
begin
  if fpndReqNum.IsEmpty then
    raise EACBrTEFAndroid.Create(sACBrTEFAndroidSemTransacaoPendenteException);

  PGWebStatus := StatusTransacaoToPWCNF_(Status);
  fTEFPayGoAPI.ResolverTransacaoPendente(PGWebStatus,
          fpndReqNum, fPndLocRef, fPndExtRef, fPndVirtMerch, fPndAuthSyst);
end;

procedure TACBrTEFAndroidPayGoClass.QuandoAvaliarTransacaoPendenteAPI(pszReqNum,
  pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst: String);
var
  MsgErro: String;
begin
  fpndReqNum := pszReqNum;
  fPndLocRef := pszLocRef;
  fPndExtRef := pszExtRef;
  fPndVirtMerch := pszVirtMerch;
  fPndAuthSyst := pszAuthSyst;

  MsgErro := Format(sACBrTEFAndroidTransacaoPendenteAPI, [pszAuthSyst, pszExtRef]);
  ProcessarTransacaoPendente(MsgErro);
end;

procedure TACBrTEFAndroidPayGoClass.QuandoFianlizarTransacaoAPI(AIntent: JIntent);
begin
  Self.ProcessarRespostaOperacaoTEF;
end;

procedure TACBrTEFAndroidPayGoClass.VerificarIdentificadorVendaInformado(
  IdentificadorVenda: String);
begin
  if IdentificadorVenda.Trim.IsEmpty then
    raise EACBrTEFAndroid.Create(sACBrTEFAndroidIdentificadorVendaVazioException);
end;

procedure TACBrTEFAndroidPayGoClass.EfetuarAdministrativa(
  Operacao: string = ''; IdentificadorVenda: string = '');
var
  PA: TACBrTEFPGWebAPIParametros;
  OpInt: Integer;
  OpByte: Byte;
begin
//VerificarIdentificadorVendaInformado(IdentificadorVenda);

  PA := TACBrTEFPGWebAPIParametros.Create;
  try
    OpByte := fOperacaoAdministrativa;
    if (Operacao <> '') then
    begin
      OpInt := StrToIntDef(Operacao, -1);
      if (OpInt > 0) then
      begin
       if (OpInt < High(Byte)) then
         OpByte := OpInt;
      end
      else
        OpByte := OperationToPWOPER_(Operacao);
    end;

    if (fAutorizador <> '') then
      PA.ValueInfo[PWINFO_AUTHSYST] := fAutorizador;

    PA.ValueInfo[PWINFO_FISCALREF] := IdentificadorVenda;
    fTEFPayGoAPI.IniciarTransacao(OpByte, PA);
  finally
    PA.Free;
  end;
end;

procedure TACBrTEFAndroidPayGoClass.EfetuarPagamento(IdentificadorVenda: String;
  ValorPagto: Currency; CartoesAceitos: TACBrTEFTiposCartao;
  Modalidade: TACBrTEFModalidadePagamento;
  Financiamento: TACBrTEFModalidadeFinanciamento;
  Parcelas: Byte;
  DataPreDatado: TDate);
var
  PA: TACBrTEFPGWebAPIParametros;
  Tipo: TObject;
  SomaCartoes, ModalidadeInt, FinanciamentoInt: Integer;
  TipoCartao: TACBrTEFTipoCartao;
begin
  VerificarIdentificadorVendaInformado(IdentificadorVenda);
  if (ValorPagto <= 0) then
    raise EACBrTEFAndroid.Create(sACBrTEFAndroidValorPagamentoInvalidoException);

  PA := TACBrTEFPGWebAPIParametros.Create;
  try
    PA.ValueInfo[PWINFO_FISCALREF] := IdentificadorVenda;
    PA.ValueInfo[PWINFO_CURREXP] := '2'; // centavos
    PA.ValueInfo[PWINFO_TOTAMNT] := IntToStr(Trunc(RoundTo(ValorPagto * 100,-2)));
    PA.ValueInfo[PWINFO_CURRENCY] := IntToStr(fpACBrTEFAndroid.DadosAutomacao.MoedaISO4217); // '986' ISO4217 - BRL

    SomaCartoes := 0;
    for TipoCartao in CartoesAceitos do
    begin
      case TipoCartao of
        teftcCredito: Inc(SomaCartoes, 1);
        teftcDebito: Inc(SomaCartoes, 2);
        teftcVoucher: Inc(SomaCartoes, 4);
        teftcPrivateLabel: Inc(SomaCartoes, 8);
        teftcFrota: Inc(SomaCartoes, 16);
        teftcOutros: Inc(SomaCartoes, 128);
      end;
    end;
    if (SomaCartoes > 0) then
      PA.ValueInfo[PWINFO_CARDTYPE] := IntToStr(SomaCartoes);

    case Modalidade of
      tefmpCartao: ModalidadeInt := 1;
      tefmpDinheiro: ModalidadeInt := 2;
      tefmpCheque: ModalidadeInt := 4;
      tefmpCarteiraVirtual: ModalidadeInt := 8;
    else
      ModalidadeInt := 0;
    end;
    if (ModalidadeInt > 0) then
      PA.ValueInfo[PWINFO_PAYMNTTYPE] := IntToStr(ModalidadeInt);

    case Financiamento of
      tefmfAVista: FinanciamentoInt := 1;
      tefmfParceladoEmissor: FinanciamentoInt := 2;
      tefmfParceladoEstabelecimento: FinanciamentoInt := 4;
      tefmfPredatado: FinanciamentoInt := 8;
      tefmfCreditoEmissor: FinanciamentoInt := 16;
    else
      FinanciamentoInt := 0;
    end;
    if (ModalidadeInt > 0) then
      PA.ValueInfo[PWINFO_FINTYPE] := IntToStr(FinanciamentoInt);

    if (Parcelas > 0) then
      PA.ValueInfo[PWINFO_INSTALLMENTS] := IntToStr(Parcelas);

    if (DataPreDatado <> 0) then
      PA.ValueInfo[PWINFO_INSTALLMDATE] := FormatDateTime('ddmmaa', DataPreDatado);

    if (fAutorizador <> '') then
      PA.ValueInfo[PWINFO_AUTHSYST] := fAutorizador;

    fTEFPayGoAPI.IniciarTransacao(fOperacaoVenda, PA);
  finally
    PA.Free;
  end;
end;

procedure TACBrTEFAndroidPayGoClass.CancelarTransacao(NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; CodigoFinalizacao, Rede: string);
var
  PA: TACBrTEFPGWebAPIParametros;
  i: Integer;
  Resp: TACBrTEFResp;

  procedure CopiarValorDaUltimaResposta(AInfo: Integer);
  var
    AStr: String;
  begin
    AStr := Resp.LeInformacao(AInfo).AsString;
    if (Trim(AStr) <> '') then
      PA.ValueInfo[AInfo] := AStr;
  end;

begin
  PA := TACBrTEFPGWebAPIParametros.Create;
  try
    PA.ValueInfo[PWINFO_TRNORIGNSU] := NSU;
    PA.ValueInfo[PWINFO_TRNORIGDATE] := FormatDateTime('DDMMYY', DataHoraTransacao);
    PA.ValueInfo[PWINFO_TRNORIGTIME] := FormatDateTime('hhnnss', DataHoraTransacao);
    PA.ValueInfo[PWINFO_TRNORIGDATETIME] := FormatDateTime('YYYYMMDDhhnnss', DataHoraTransacao);
    PA.ValueInfo[PWINFO_TRNORIGAMNT] :=  IntToStr(Trunc(RoundTo(Valor * 100,-2)));
    PA.ValueInfo[PWINFO_TRNORIGAUTH] := CodigoAutorizacaoTransacao;
    PA.ValueInfo[PWINFO_TRNORIGAUTHCODE] := CodigoAutorizacaoTransacao;

    if not (Rede.IsEmpty or CodigoFinalizacao.IsEmpty) then
    begin
      i := fpACBrTEFAndroid.RespostasTEF.AcharTransacao(Rede, NSU, CodigoFinalizacao);
      if (i >= 0) then
      begin
        Resp := fpACBrTEFAndroid.RespostasTEF[i];
        PA.ValueInfo[PWINFO_TRNORIGLOCREF] := Resp.Finalizacao;
        PA.ValueInfo[PWINFO_TRNORIGREQNUM] := IntToStr(Resp.NumeroLoteTransacao);
        CopiarValorDaUltimaResposta(PWINFO_MERCHCNPJCPF);
        CopiarValorDaUltimaResposta(PWINFO_CARDTYPE);
        CopiarValorDaUltimaResposta(PWINFO_VIRTMERCH);
        CopiarValorDaUltimaResposta(PWINFO_AUTMERCHID);
        CopiarValorDaUltimaResposta(PWINFO_FINTYPE);
      end;
    end;

    if (Rede <> '') then
      PA.ValueInfo[PWINFO_AUTHSYST] := Rede
    else if (fAutorizador <> '') then
      PA.ValueInfo[PWINFO_AUTHSYST] := fAutorizador;

    fTEFPayGoAPI.IniciarTransacao(fOperacaoCancelamento, PA);
  finally
    PA.Free;
  end;
end;

procedure TACBrTEFAndroidPayGoClass.FinalizarOperacao(Rede, NSU,
  CodigoFinalizacao: String; Status: TACBrTEFStatusTransacao);
var
  confirmTransactionIdentifier: String;
  PGWebStatus: LongWord;
  i: Integer;
begin
 if Rede.IsEmpty or NSU.IsEmpty then
    Exit;

  i := fpACBrTEFAndroid.RespostasTEF.AcharTransacao(Rede, NSU, CodigoFinalizacao);
  if (i >= 0) then
    confirmTransactionIdentifier := fpACBrTEFAndroid.RespostasTEF[i].Conteudo.LeInformacao(PWINFO_CONFTRANSIDENT).AsString
  else
    raise EACBrTEFAndroid.CreateFmt(sACBrTEFAndroidTransacaoNaoEncontradaException,
      [Rede+' '+ NSU+' '+CodigoFinalizacao]);

  PGWebStatus := StatusTransacaoToPWCNF_(Status);
  fTEFPayGoAPI.ConfirmarTransacao(PGWebStatus, confirmTransactionIdentifier);
end;

procedure TACBrTEFAndroidPayGoClass.InterpretarDadosDaTransacao;
var
  i, p, AInfo: Integer;
  Lin, AValue: String;
begin
  inherited;

  for i := 0 to fTEFPayGoAPI.DadosDaTransacao.Count-1 do
  begin
    Lin := fTEFPayGoAPI.DadosDaTransacao[i];
    p := pos('=', Lin);
    if (p > 0) then
    begin
      AInfo := StrToIntDef(copy(Lin, 1, p-1), -1);
      if (AInfo >= 0) then
      begin
        AValue := copy(Lin, P+1, Length(Lin));
        fpACBrTEFAndroid.UltimaRespostaTEF.Conteudo.GravaInformacao(Ainfo, 0, AValue);
      end;
    end;
  end;

  ConteudoToPropertyPayGoWeb( fpACBrTEFAndroid.UltimaRespostaTEF );

  fpTransacaoOK := (StrToIntDef(fTEFPayGoAPI.DadosDaTransacao.ValueInfo[PWINFO_RET], -1) = PWRET_OK);
  fpMensagemResultado := fTEFPayGoAPI.DadosDaTransacao.ValueInfo[PWINFO_RESULTMSG];
end;

procedure TACBrTEFAndroidPayGoClass.LimparTransacaoPendente;
begin
  fPndLocRef := '';
  fPndExtRef := '';
  fPndVirtMerch := '';
  fPndAuthSyst := '';
end;

end.
