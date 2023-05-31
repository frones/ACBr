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

unit ACBrTEFAndroidMSitef;

interface

uses
  Classes, SysUtils,
  Androidapi.JNI.GraphicsContentViewText,
  ACBrTEFComum, ACBrTEFAPIComum, ACBrTEFAndroid,
  ACBrTEFMSitefComum, ACBrTEFMSitefAndroidAPI;

type
  TTipoPinpad= (pTodos, pUsb, pBluetooth);

  TACBrTEFAndroidMSitefClass = class(TACBrTEFAPIComumClass)
  private
    fTEFMSitefAPI: TACBrTEFSIWebAndroid;
    fpndReqNum: String;
    fPndLocRef: String;
    fPndExtRef: String;
    fPndVirtMerch: String;
    fPndAuthSyst: String;
    fOperacaoVenda: Byte;
    fOperacaoAdministrativa: Byte;
    fOperacaoCancelamento: Byte;
    fAutorizador: String;
    fRestricoes: String;
    fTransacoesHabilitadas: String;
    fComExterna: String;
    fValidacaoDupla: String;
    fCodigoOTP: String;
    fModalidadeNaoGenerica: integer;
    fAcessibilidadeVisual: integer;
    fTipoPinpad: TTipoPinpad;

    procedure QuandoIniciarTransacaoAPI(AIntent: JIntent);
    procedure QuandoFinalizarTransacaoAPI(AIntent: JIntent);
    procedure QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure QuandoAvaliarTransacaoPendenteAPI(pszReqNum: String; pszLocRef: String; pszExtRef: String; pszVirtMerch: String; pszAuthSyst: String);

    procedure LimparTransacaoPendente;

  protected
    procedure InicializarChamadaAPI(AMetodoOperacao: TACBrTEFAPIMetodo); override;
    procedure InterpretarRespostaAPI; override;

  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    function EfetuarPagamento(ValorPagto: Currency;
                              Modalidade: TACBrTEFModalidadePagamento = TACBrTEFModalidadePagamento.tefmpNaoDefinido;
                              CartoesAceitos: TACBrTEFTiposCartao = [];
                              Financiamento: TACBrTEFModalidadeFinanciamento = TACBrTEFModalidadeFinanciamento.tefmfNaoDefinido;
                              Parcelas: Byte = 0;
                              DataPreDatado: TDateTime = 0): Boolean; override;

    function EfetuarAdministrativa(OperacaoAdm: TACBrTEFOperacao = tefopAdministrativo): Boolean; overload; override;
    function EfetuarAdministrativa(const CodOperacaoAdm: string = ''): Boolean; overload; override;
    function CancelarTransacao(const NSU, CodigoAutorizacaoTransacao: string;
                               DataHoraTransacao: TDateTime;
                               Valor: Double;
                               const CodigoFinalizacao: string = '';
                               const Rede: string = ''): Boolean; override;

    procedure FinalizarTransacao(const Rede, NSU, CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoAutomatico); override;

    procedure ResolverTransacaoPendente(AStatus: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoManual); override;

    property TEFMSitefAPI: TACBrTEFSIWebAndroid read fTEFMSitefAPI;
    property OperacaoVenda: Byte read fOperacaoVenda write fOperacaoVenda default PWOPER_SALE;
    property OperacaoAdministrativa: Byte read fOperacaoAdministrativa write fOperacaoAdministrativa default PWOPER_ADMIN;
    property OperacaoCancelamento: Byte read fOperacaoCancelamento write fOperacaoCancelamento default PWOPER_SALEVOID;
    property Autorizador: String read fAutorizador write fAutorizador;
    property Restricoes: String read fRestricoes write fRestricoes;
    property TransacoesHabilitadas: String read fTransacoesHabilitadas write fTransacoesHabilitadas;
    property ComExterna: String read fComExterna write fComExterna;
    property ValidacaoDupla: String read fValidacaoDupla write fValidacaoDupla;
    property ModalidadeNaoGenerica: integer read fModalidadeNaoGenerica write fModalidadeNaoGenerica;
    property CodigoOTP: string read fCodigoOTP write fCodigoOTP;
    property AcessibilidadeVisual: integer read fAcessibilidadeVisual write fAcessibilidadeVisual;
    property TipoPinpad: TTipoPinpad read fTipoPinpad write fTipoPinpad;
  end;

implementation

uses
  math;

{ TACBrTEFAndroidMSitefClass }

constructor TACBrTEFAndroidMSitefClass.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;

  fpTEFRespClass := TACBrTEFRespMSitefWeb;

  fOperacaoVenda          := PWOPER_SALE;
  fOperacaoAdministrativa := PWOPER_ADMIN;
  fOperacaoCancelamento   := PWOPER_SALEVOID;
  fRestricoes             := '';
  fTransacoesHabilitadas  := '';
  fComExterna             := '0'; // 0 – Sem (apenas para SiTef dedicado); 1 – TLS Software Express; 2 – TLS WNB Comnect; 3 – TLS Gsurf
  fValidacaoDupla         := '0'; // 0 – Para validação simples; 1 – Para validação dupla

  fTEFMSitefAPI := TACBrTEFSIWebAndroid.Create;
  fTEFMSitefAPI.OnGravarLog := QuandoGravarLogAPI;
  fTEFMSitefAPI.OnDepoisTerminarTransacao := QuandoFinalizarTransacaoAPI;
  fTEFMSitefAPI.OnAntesIniciarTransacao := QuandoIniciarTransacaoAPI;
end;

destructor TACBrTEFAndroidMSitefClass.Destroy;
begin
  fTEFMSitefAPI.Free;
  inherited;
end;

procedure TACBrTEFAndroidMSitefClass.Inicializar;
begin
  //Dados automação
  fTEFMSitefAPI.NomeEstabelecimento            := fpACBrTEFAPI.DadosEstabelecimento.RazaoSocial;
  fTEFMSitefAPI.CNPJEstabelecimento            := fpACBrTEFAPI.DadosEstabelecimento.CNPJ;
  fTEFMSitefAPI.DadosAutomacao.SuportaSaque    := fpACBrTEFAPI.DadosAutomacao.SuportaSaque;
  fTEFMSitefAPI.DadosAutomacao.SuportaDesconto := fpACBrTEFAPI.DadosAutomacao.SuportaDesconto;
  fTEFMSitefAPI.DadosAutomacao.SuportaViasDiferenciadas  := fpACBrTEFAPI.DadosAutomacao.SuportaViasDiferenciadas;
  fTEFMSitefAPI.DadosAutomacao.ImprimeViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;
  fTEFMSitefAPI.DadosAutomacao.UtilizaSaldoTotalVoucher  := fpACBrTEFAPI.DadosAutomacao.UtilizaSaldoTotalVoucher;

  //Dados terminal
  fTEFMSitefAPI.DadosTerminal.EnderecoServidor := fpACBrTEFAPI.DadosTerminal.EnderecoServidor;
  fTEFMSitefAPI.DadosTerminal.CodEmpresa       := fpACBrTEFAPI.DadosTerminal.CodEmpresa;
  fTEFMSitefAPI.DadosTerminal.CodFilial        := fpACBrTEFAPI.DadosTerminal.CodFilial;
  fTEFMSitefAPI.DadosTerminal.CodTerminal      := fpACBrTEFAPI.DadosTerminal.CodTerminal;
  fTEFMSitefAPI.DadosTerminal.Operador         := fpACBrTEFAPI.DadosTerminal.Operador;
  fTEFMSitefAPI.DadosTerminal.PortaPinPad      := fpACBrTEFAPI.DadosTerminal.PortaPinPad;

  fTEFMSitefAPI.ConfirmarTransacoesPendentesNoHost := (fpACBrTEFAPI.TratamentoTransacaoPendente = tefpenConfirmar);

  if (fpACBrTEFAPI.TratamentoTransacaoPendente = tefpenPerguntar) then
    fTEFMSitefAPI.OnAvaliarTransacaoPendente := QuandoAvaliarTransacaoPendenteAPI
  else
    fTEFMSitefAPI.OnAvaliarTransacaoPendente := Nil;

  fTEFMSitefAPI.Inicializar;

  LimparTransacaoPendente;

  inherited;
end;

procedure TACBrTEFAndroidMSitefClass.DesInicializar;
begin
  fTEFMSitefAPI.DesInicializar;
  inherited;
end;

procedure TACBrTEFAndroidMSitefClass.InicializarChamadaAPI(AMetodoOperacao: TACBrTEFAPIMetodo);
begin
  inherited;
  LimparTransacaoPendente;
end;

procedure TACBrTEFAndroidMSitefClass.InterpretarRespostaAPI;
begin
  inherited;
  fpACBrTEFAPI.UltimaRespostaTEF.ViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;
  DadosDaTransacaoToTEFResp( fTEFMSitefAPI.DadosDaTransacao, fpACBrTEFAPI.UltimaRespostaTEF );
end;

procedure TACBrTEFAndroidMSitefClass.QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAndroidMSitefClass.QuandoIniciarTransacaoAPI(AIntent: JIntent);
begin
  with TACBrTEFAndroid(fpACBrTEFAPI) do
  begin
    if Assigned(QuandoIniciarTransacao) then
      QuandoIniciarTransacao(AIntent);
  end;
end;

procedure TACBrTEFAndroidMSitefClass.ResolverTransacaoPendente( AStatus: TACBrTEFStatusTransacao);
begin
  fTEFMSitefAPI.ResolverTransacaoPendente(PWCNF_CNF_AUTO, fpndReqNum, fPndLocRef, fPndExtRef, fPndVirtMerch, fPndAuthSyst);
end;

procedure TACBrTEFAndroidMSitefClass.QuandoAvaliarTransacaoPendenteAPI(pszReqNum, pszLocRef, pszExtRef, pszVirtMerch, pszAuthSyst: String);
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

procedure TACBrTEFAndroidMSitefClass.QuandoFinalizarTransacaoAPI(AIntent: JIntent);
begin
  Self.ProcessarRespostaOperacaoTEF;
end;

function TACBrTEFAndroidMSitefClass.EfetuarAdministrativa(const CodOperacaoAdm: string = ''): Boolean;
var
  PA: TACBrTEFParametros;
  OpInt: Integer;
  OpByte: Byte;
begin
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

    fTEFMSitefAPI.IniciarTransacao(OpByte, PA);
    Result := True;  // TEF no Android trabalha de modo Assincrono
  finally
    PA.Free;
  end;
end;

function TACBrTEFAndroidMSitefClass.EfetuarAdministrativa(OperacaoAdm: TACBrTEFOperacao): Boolean;
begin
  //Result := Self.EfetuarAdministrativa( IntToStr(OperacaoAdminToPWOPER_(OperacaoAdm)) );
end;

function TACBrTEFAndroidMSitefClass.EfetuarPagamento(ValorPagto   : Currency;
                                                     Modalidade   : TACBrTEFModalidadePagamento;
                                                     CartoesAceitos: TACBrTEFTiposCartao;
                                                     Financiamento: TACBrTEFModalidadeFinanciamento;
                                                     Parcelas     : Byte;
                                                     DataPreDatado: TDateTime): Boolean;
var
  PA: TACBrTEFParametros;
  TipoCartao: TACBrTEFTipoCartao;
begin
  VerificarIdentificadorVendaInformado;
  if (ValorPagto <= 0) then
    fpACBrTEFAPI.DoException(sACBrTEFAPIValorPagamentoInvalidoException);

  PA := TACBrTEFParametros.Create;
  try
    PA.ValueInfo[PWINFO_PAYMNTTYPE] := IntToStr(fOperacaoVenda);

    (*a modalidade (fOperacaoVenda ou 0) e considerada genérica para o sitef
      ou seja ele vai montar o menu de operação contendo todas as opções disponínel no servidor sitef
      Exemplo Cartao Credito, Débito, Serasa, Carteira digitais, ITI, Recarga Celular, cobrança e etc
      caso o operador passe uma modalidade não generica ele pode passar do número 1 até 1000 para
      executar um menu\transação especifica
      **Verifica manual 5.2.2 Tabela de códigos de funções
      *)

    if fModalidadeNaoGenerica > 0 then
      PA.ValueInfo[PWINFO_PAYMNTTYPE] := IntToStr(fModalidadeNaoGenerica);

    PA.ValueInfo[PWINFO_FISCALREF]   := fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao;
    PA.ValueInfo[PWINFO_CURREXP]     := '2'; // centavos
    PA.ValueInfo[PWINFO_TOTAMNT]     := IntToStr(Trunc(RoundTo(ValorPagto * 100,-2)));
    PA.ValueInfo[PWOPER_COMEXTERNA]  := fComExterna;
    PA.ValueInfo[PWOPER_DOUBLEVALIDATION] := fValidacaoDupla;
    PA.ValueInfo[PWOPER_RESTRICOES]  := fRestricoes;
    PA.ValueInfo[PWOPER_TRANSHABILITADA] := fTransacoesHabilitadas;
    PA.ValueInfo[PWOPER_OTP]         := fCodigoOTP;
    PA.ValueInfo[PWOPER_ACESSIBILIDADEVISUAL] := '0';

    if fAcessibilidadeVisual <> 0 then
      PA.ValueInfo[PWOPER_ACESSIBILIDADEVISUAL] := IntToStr(fAcessibilidadeVisual);

    if fTipoPinpad = pUsb then
      PA.ValueInfo[PWOPER_TIPOPINPAD] := 'ANDROID_USB'
    else if fTipoPinpad = pUsb then
      PA.ValueInfo[PWOPER_TIPOPINPAD] := 'ANDROID_BT'
    else
      PA.ValueInfo[PWOPER_TIPOPINPAD] := '';

    PA.ValueInfo[PWINFO_CURRENCY]    := IntToStr(fpACBrTEFAPI.DadosAutomacao.MoedaISO4217); // '986' ISO4217 - BRL

    if (Parcelas > 0) then
      PA.ValueInfo[PWINFO_INSTALLMENTS] := IntToStr(Parcelas);

    if (DataPreDatado <> 0) then
      PA.ValueInfo[PWINFO_INSTALLMDATE] := FormatDateTime('ddmmyy', DataPreDatado);

    fTEFMSitefAPI.ValorTotalPagamento := ValorPagto; //Valor de pagamento não é retornado via intents

    fTEFMSitefAPI.IniciarTransacao(StrToIntDef(PA.ValueInfo[PWINFO_PAYMNTTYPE], fOperacaoVenda), PA);
    Result := True;  // TEF no Android trabalha de modo Assincrono
  finally
    PA.Free;
  end;
end;

function TACBrTEFAndroidMSitefClass.CancelarTransacao(const NSU,
                                                      CodigoAutorizacaoTransacao: string;
                                                      DataHoraTransacao: TDateTime;
                                                      Valor: Double;
                                                      const CodigoFinalizacao,
                                                      Rede: string): Boolean;
var
  PA : TACBrTEFParametros;
begin
  PA := TACBrTEFParametros.Create;
  try
    //Sitef não passa os valores Nsu, código Autorização e data emissão
    PA.ValueInfo[PWOPER_SALEVOID] := '-1';
    fTEFMSitefAPI.IniciarTransacao(fOperacaoCancelamento, PA);
    Result := True;  // TEF no Android trabalha de modo Assincrono
  finally
    PA.Free;
  end;
end;

procedure TACBrTEFAndroidMSitefClass.FinalizarTransacao(const Rede, NSU, CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
begin
  fTEFMSitefAPI.ConfirmarTransacao(PWCNF_CNF_AUTO, '');
end;

procedure TACBrTEFAndroidMSitefClass.LimparTransacaoPendente;
begin
  fPndLocRef := '';
  fPndExtRef := '';
  fPndVirtMerch := '';
  fPndAuthSyst := '';
end;

end.
