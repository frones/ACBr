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

unit ACBrTEFAndroidPayGo;

interface

uses
  Classes, SysUtils,
  Androidapi.JNI.GraphicsContentViewText,
  ACBrTEFComum, ACBrTEFAPIComum, ACBrTEFAndroid,
  ACBrTEFPayGoComum, ACBrTEFPayGoAndroidAPI;

const
  PWRET_OK = 0;      // Operação bem sucedida

type
  TACBrTEFAndroidPayGoClass = class(TACBrTEFAPIComumClass)
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
    procedure QuandoFinalizarTransacaoAPI(AIntent: JIntent);
    procedure QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure QuandoAvaliarTransacaoPendenteAPI(pszReqNum: String; pszLocRef: String;
      pszExtRef: String; pszVirtMerch: String; pszAuthSyst: String);

    procedure LimparTransacaoPendente;

  protected
    procedure InicializarChamadaAPI(AMetodoOperacao: TACBrTEFAPIMetodo); override;
    procedure InterpretarRespostaAPI; override;

  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    function EfetuarPagamento(
      ValorPagto: Currency;
      Modalidade: TACBrTEFModalidadePagamento = TACBrTEFModalidadePagamento.tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Financiamento: TACBrTEFModalidadeFinanciamento = TACBrTEFModalidadeFinanciamento.tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDateTime = 0): Boolean; override;

    function EfetuarAdministrativa(
      OperacaoAdm: TACBrTEFOperacao = tefopAdministrativo): Boolean; overload; override;
    function EfetuarAdministrativa(
      const CodOperacaoAdm: string = ''): Boolean; overload; override;

    function CancelarTransacao(
      const NSU, CodigoAutorizacaoTransacao: string;
      DataHoraTransacao: TDateTime;
      Valor: Double;
      const CodigoFinalizacao: string = '';
      const Rede: string = ''): Boolean; override;

    procedure FinalizarTransacao(
      const Rede, NSU, CodigoFinalizacao: String;
      AStatus: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoAutomatico); override;

    procedure ResolverTransacaoPendente(
      AStatus: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoManual); override;

    property TEFPayGoAPI: TACBrTEFPGWebAndroid read fTEFPayGoAPI;
    property OperacaoVenda: Byte read fOperacaoVenda
      write fOperacaoVenda default PWOPER_SALE;
    property OperacaoAdministrativa: Byte read fOperacaoAdministrativa
      write fOperacaoAdministrativa default PWOPER_ADMIN;
    property OperacaoCancelamento: Byte read fOperacaoCancelamento
      write fOperacaoCancelamento default PWOPER_SALEVOID;
    property Autorizador: String read fAutorizador write fAutorizador;
  end;

implementation

uses
  math,
  ACBrTEFPayGoWebComum;

{ TACBrTEFAndroidPayGoClass }

constructor TACBrTEFAndroidPayGoClass.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;

  fpTEFRespClass := TACBrTEFRespPayGoWeb;

  fOperacaoVenda := PWOPER_SALE;
  fOperacaoAdministrativa := PWOPER_ADMIN;
  fOperacaoCancelamento := PWOPER_SALEVOID;
  fAutorizador := '';

  fTEFPayGoAPI := TACBrTEFPGWebAndroid.Create;
  fTEFPayGoAPI.OnGravarLog := QuandoGravarLogAPI;
  fTEFPayGoAPI.OnDepoisTerminarTransacao := QuandoFinalizarTransacaoAPI;
  fTEFPayGoAPI.OnAntesIniciarTransacao := QuandoIniciarTransacaoAPI;
end;

destructor TACBrTEFAndroidPayGoClass.Destroy;
begin
  fTEFPayGoAPI.Free;
  inherited;
end;

procedure TACBrTEFAndroidPayGoClass.Inicializar;
begin
  fTEFPayGoAPI.DadosAutomacao.NomeAplicacao := fpACBrTEFAPI.DadosAutomacao.NomeAplicacao;
  fTEFPayGoAPI.DadosAutomacao.VersaoAplicacao := fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
  fTEFPayGoAPI.DadosAutomacao.SoftwareHouse := fpACBrTEFAPI.DadosAutomacao.NomeSoftwareHouse;
  fTEFPayGoAPI.NomeEstabelecimento := fpACBrTEFAPI.DadosEstabelecimento.RazaoSocial;
  fTEFPayGoAPI.DadosAutomacao.SuportaSaque := fpACBrTEFAPI.DadosAutomacao.SuportaSaque;
  fTEFPayGoAPI.DadosAutomacao.SuportaDesconto := fpACBrTEFAPI.DadosAutomacao.SuportaDesconto;
  fTEFPayGoAPI.DadosAutomacao.SuportaViasDiferenciadas := fpACBrTEFAPI.DadosAutomacao.SuportaViasDiferenciadas;
  fTEFPayGoAPI.DadosAutomacao.ImprimeViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;
  fTEFPayGoAPI.DadosAutomacao.UtilizaSaldoTotalVoucher := fpACBrTEFAPI.DadosAutomacao.UtilizaSaldoTotalVoucher;

  With TACBrTEFAndroid(fpACBrTEFAPI).Personalizacao do
  begin
    fTEFPayGoAPI.Personalizacao.corFundoTela := corFundoTela;
    fTEFPayGoAPI.Personalizacao.corFundoToolbar := corFundoToolbar;
    fTEFPayGoAPI.Personalizacao.corFonte := corFonte;
    fTEFPayGoAPI.Personalizacao.corSeparadorMenu := corSeparadorMenu;
    fTEFPayGoAPI.Personalizacao.corFundoCaixaEdicao := corFundoCaixaEdicao;
    fTEFPayGoAPI.Personalizacao.corTextoCaixaEdicao := corTextoCaixaEdicao;
    fTEFPayGoAPI.Personalizacao.corFundoTeclado := corFundoTeclado;
    fTEFPayGoAPI.Personalizacao.corTeclaLiberadaTeclado := corTeclaLiberadaTeclado;
    fTEFPayGoAPI.Personalizacao.corTeclaPressionadaTeclado := corTeclaPressionadaTeclado;
    fTEFPayGoAPI.Personalizacao.corFonteTeclado := corFonteTeclado;
    fTEFPayGoAPI.Personalizacao.pathIconeToolbar := ArquivoIcone;
    fTEFPayGoAPI.Personalizacao.pathFonte := ArquivoFonte;
  end;

  fTEFPayGoAPI.ConfirmarTransacoesPendentesNoHost := (fpACBrTEFAPI.TratamentoTransacaoPendente = tefpenConfirmar);
  if (fpACBrTEFAPI.TratamentoTransacaoPendente = tefpenPerguntar) then
    fTEFPayGoAPI.OnAvaliarTransacaoPendente := QuandoAvaliarTransacaoPendenteAPI
  else
    fTEFPayGoAPI.OnAvaliarTransacaoPendente := Nil;

  fTEFPayGoAPI.Inicializar;

  LimparTransacaoPendente;

  inherited;
end;

procedure TACBrTEFAndroidPayGoClass.DesInicializar;
begin
  fTEFPayGoAPI.DesInicializar;
  inherited;
end;

procedure TACBrTEFAndroidPayGoClass.InicializarChamadaAPI(AMetodoOperacao: TACBrTEFAPIMetodo);
begin
  inherited;
  LimparTransacaoPendente;
end;

procedure TACBrTEFAndroidPayGoClass.InterpretarRespostaAPI;
begin
  inherited;
  fpACBrTEFAPI.UltimaRespostaTEF.ViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;
  DadosDaTransacaoToTEFResp( fTEFPayGoAPI.DadosDaTransacao,
                             fpACBrTEFAPI.UltimaRespostaTEF );
end;

procedure TACBrTEFAndroidPayGoClass.QuandoGravarLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAndroidPayGoClass.QuandoIniciarTransacaoAPI(AIntent: JIntent);
begin
  with TACBrTEFAndroid(fpACBrTEFAPI) do
  begin
    if Assigned(QuandoIniciarTransacao) then
      QuandoIniciarTransacao(AIntent);
  end;
end;

procedure TACBrTEFAndroidPayGoClass.ResolverTransacaoPendente(
  AStatus: TACBrTEFStatusTransacao);
var
  PGWebStatus: LongWord;
begin
  // Se não foi disparado por QuandoAvaliarTransacaoPendenteAPI, pegue as
  //   informações da Transação atual, na memória
  if fpndReqNum.IsEmpty then
  begin
    with fpACBrTEFAPI.UltimaRespostaTEF do
    begin
      fpndReqNum := LeInformacao(PWINFO_REQNUM,0).AsString;
      fPndLocRef := LeInformacao(PWINFO_AUTLOCREF,0).AsString;
      fPndExtRef := LeInformacao(PWINFO_AUTEXTREF,0).AsString;
      fPndVirtMerch := LeInformacao(PWINFO_VIRTMERCH,0).AsString;
      fPndAuthSyst := LeInformacao(PWINFO_AUTHSYST,0).AsString;
    end;
  end;

  if fpndReqNum.IsEmpty then
    fpACBrTEFAPI.DoException(sACBrTEFAPISemTransacaoPendenteException);

  PGWebStatus := StatusTransacaoToPWCNF_(AStatus);
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

  MsgErro := Format(sACBrTEFAPITransacaoPendente, [pszAuthSyst, pszExtRef]);
  fpACBrTEFAPI.ProcessarTransacaoPendente(MsgErro);
end;

procedure TACBrTEFAndroidPayGoClass.QuandoFinalizarTransacaoAPI(AIntent: JIntent);
begin
  Self.ProcessarRespostaOperacaoTEF;
end;

function TACBrTEFAndroidPayGoClass.EfetuarAdministrativa(
  const CodOperacaoAdm: string = ''): Boolean;
var
  PA: TACBrTEFParametros;
  OpInt: Integer;
  OpByte: Byte;
begin
//VerificarIdentificadorVendaInformado(IdentificadorVenda);

  PA := TACBrTEFParametros.Create;
  try
    OpByte := fOperacaoAdministrativa;
    if (CodOperacaoAdm <> '') then
    begin
      OpInt := StrToIntDef(CodOperacaoAdm, -1);
      if (OpInt >= 0) then
      begin
       if (OpInt <= High(Byte)) then
         OpByte := OpInt;
      end
      else
        OpByte := OperationToPWOPER_(CodOperacaoAdm);
    end;

    if (fAutorizador <> '') then
      PA.ValueInfo[PWINFO_AUTHSYST] := fAutorizador;

    if (fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao <> '') then
      PA.ValueInfo[PWINFO_FISCALREF] := fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao;

    fTEFPayGoAPI.IniciarTransacao(OpByte, PA);
    Result := True;  // TEF no Android trabalha de modo Assincrono
  finally
    PA.Free;
  end;
end;

function TACBrTEFAndroidPayGoClass.EfetuarAdministrativa(
  OperacaoAdm: TACBrTEFOperacao): Boolean;
begin
  Result := Self.EfetuarAdministrativa( IntToStr(OperacaoAdminToPWOPER_(OperacaoAdm)) );
end;

function TACBrTEFAndroidPayGoClass.EfetuarPagamento(
  ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento;
  CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento;
  Parcelas: Byte;
  DataPreDatado: TDateTime): Boolean;
var
  PA: TACBrTEFParametros;
  SomaCartoes, ModalidadeInt, FinanciamentoInt: Integer;
  TipoCartao: TACBrTEFTipoCartao;
begin
  VerificarIdentificadorVendaInformado;
  if (ValorPagto <= 0) then
    fpACBrTEFAPI.DoException(sACBrTEFAPIValorPagamentoInvalidoException);

  PA := TACBrTEFParametros.Create;
  try
    PA.ValueInfo[PWINFO_FISCALREF] := fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao;
    PA.ValueInfo[PWINFO_CURREXP] := '2'; // centavos
    PA.ValueInfo[PWINFO_TOTAMNT] := IntToStr(Trunc(RoundTo(ValorPagto * 100,-2)));
    PA.ValueInfo[PWINFO_CURRENCY] := IntToStr(fpACBrTEFAPI.DadosAutomacao.MoedaISO4217); // '986' ISO4217 - BRL

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
      PA.ValueInfo[PWINFO_INSTALLMDATE] := FormatDateTime('ddmmyy', DataPreDatado);

    if (fAutorizador <> '') then
      PA.ValueInfo[PWINFO_AUTHSYST] := fAutorizador;

    fTEFPayGoAPI.IniciarTransacao(fOperacaoVenda, PA);
    Result := True;  // TEF no Android trabalha de modo Assincrono
  finally
    PA.Free;
  end;
end;

function TACBrTEFAndroidPayGoClass.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao, Rede: string): Boolean;
var
  PA: TACBrTEFParametros;
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
  PA := TACBrTEFParametros.Create;
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
      i := fpACBrTEFAPI.RespostasTEF.AcharTransacao(Rede, NSU, CodigoFinalizacao);
      if (i >= 0) then
      begin
        Resp := fpACBrTEFAPI.RespostasTEF[i];
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
    Result := True;  // TEF no Android trabalha de modo Assincrono
  finally
    PA.Free;
  end;
end;

procedure TACBrTEFAndroidPayGoClass.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
var
  confirmTransactionIdentifier: String;
  PGWebStatus: LongWord;
  i: Integer;
begin
 if Rede.IsEmpty or NSU.IsEmpty then
    Exit;

  i := fpACBrTEFAPI.RespostasTEF.AcharTransacao(Rede, NSU, CodigoFinalizacao);
  if (i >= 0) then
    confirmTransactionIdentifier := fpACBrTEFAPI.RespostasTEF[i].Conteudo.LeInformacao(PWINFO_CONFTRANSIDENT).AsString
  else
    fpACBrTEFAPI.DoException( Format(sACBrTEFAPITransacaoNaoEncontradaException,
                              [Rede+' '+ NSU+' '+CodigoFinalizacao]));

  PGWebStatus := StatusTransacaoToPWCNF_(AStatus);
  fTEFPayGoAPI.ConfirmarTransacao(PGWebStatus, confirmTransactionIdentifier);
end;

procedure TACBrTEFAndroidPayGoClass.LimparTransacaoPendente;
begin
  fPndLocRef := '';
  fPndExtRef := '';
  fPndVirtMerch := '';
  fPndAuthSyst := '';
end;

end.
