{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrTEFAPIPayKit;

interface

uses
  Classes, SysUtils,
  ACBrBase,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum, ACBrTEFPayKitAPI;

type

  { TACBrTEFRespPayKit }

  TACBrTEFRespPayKit = class( TACBrTEFResp )
  public
    procedure ConteudoToProperty; override;
  end;


  { TACBrTEFAPIClassPayKit }

  TACBrTEFAPIClassPayKit = class(TACBrTEFAPIClass)
  private
    fTempoMsgPinPad: Integer;
    function GetTEFPayKitAPI: TACBrTEFPayKitAPI;

    procedure QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure QuandoExibirMensagemAPI(const Mensagem: String;
      TipoMensagem: TACBrTEFPayKitTipoMensagem; MilissegundosExibicao: Integer);
    procedure QuandoPerguntarMenuAPI(const Titulo: String; Opcoes: TStringList;
      var ItemSelecionado: Integer);
    procedure QuandoPerguntarCampoAPI(DefinicaoCampo: TACBrTEFPayKitDefinicaoCampo;
      var Resposta: String; var Acao: Integer);
    procedure VerificarTransacaoEmAndamentoAPI(EstadoOperacao: TACBrTEFPayKitEstadoOperacao;
      out Cancelar: Boolean);

    procedure QuandoExibirQRCodeAPI(const DadosQRCode: String);

  protected
    procedure InterpretarRespostaAPI; override;

    function ExibirVersaoPayKit: Boolean;
    function TestarComunicacaoPayKit: Boolean;
    function PerguntarMenuAdmPayKit: TACBrTEFOperacao;
  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    function EfetuarPagamento(
      ValorPagto: Currency;
      Modalidade: TACBrTEFModalidadePagamento = tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDateTime = 0;
      DadosAdicionais: String = ''): Boolean; override;

    function EfetuarAdministrativa(
      CodOperacaoAdm: TACBrTEFOperacao = tefopAdministrativo): Boolean; overload; override;
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
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoAutomatico); override;

    procedure ResolverTransacaoPendente(AStatus: TACBrTEFStatusTransacao = tefstsSucessoManual); override;
    procedure AbortarTransacaoEmAndamento; override;
    procedure FinalizarVenda; override;

    procedure ExibirMensagemPinPad(const MsgPinPad: String); override;
    function ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad;
      TimeOut: Integer = 30000; MinLen: SmallInt = 0; MaxLen: SmallInt = 0): String; override;
    function VerificarPresencaPinPad: Byte; override;

    procedure ObterListaImagensPinPad(ALista: TStrings); override;

    procedure ExibirImagemPinPad(const NomeImagem: String); override;
    procedure ApagarImagemPinPad(const NomeImagem: String); override;
    procedure CarregarImagemPinPad(const NomeImagem: String; AStream: TStream;
      TipoImagem: TACBrTEFAPIImagemPinPad ); override;

    property TEFPayKitAPI: TACBrTEFPayKitAPI read GetTEFPayKitAPI;
    property TempoMsgPinPad: Integer read fTempoMsgPinPad write fTempoMsgPinPad default 5000;
  end;

implementation

uses
  math, StrUtils, TypInfo,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrJSON;

{ TACBrTEFRespPayKit }

procedure TACBrTEFRespPayKit.ConteudoToProperty;
var
  j: String;
  js, jsLog: TACBrJSONObject;
begin
  j := Conteudo.LeInformacao(899,300).AsString;
  js := TACBrJSONObject.Parse(j);
  try
    jsLog := js.AsJSONObject['LogTransacao'];
    if not Assigned(jsLog) then
      Exit;

    Finalizacao := jsLog.AsString['NumeroControle'];
    NSU_TEF := Finalizacao;
    NFCeSAT.CNPJCredenciadora := jsLog.AsString['CNPJRedeAdquirente'];
  finally
    js.Free;
  end;
end;


{ TACBrTEFAPIClassPayKit }

constructor TACBrTEFAPIClassPayKit.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;

  fpTEFRespClass := TACBrTEFRespPayKit;
  fTempoMsgPinPad := 5000;

  with GetTEFPayKitAPI do
  begin
    OnGravarLog := QuandoGravarLogAPI;
    OnExibeMensagem := QuandoExibirMensagemAPI;
    OnTransacaoEmAndamento := VerificarTransacaoEmAndamentoAPI;
    QuandoPerguntarMenu := QuandoPerguntarMenuAPI;
    QuandoPerguntarCampo := QuandoPerguntarCampoAPI;
    QuandoExibirQRCode := QuandoExibirQRCodeAPI;
  end;
end;

destructor TACBrTEFAPIClassPayKit.Destroy;
begin
  //fTEFPayKitAPI.Free;  // Libera em ACBrTEFPayKitAPI.finalization;
  inherited;
end;

procedure TACBrTEFAPIClassPayKit.Inicializar;
var
  s: String;
begin
  if Inicializado then
    Exit;

  with GetTEFPayKitAPI do
  begin
    PathPayKit := PathDLL;
    NomeAutomacao := fpACBrTEFAPI.DadosAutomacao.NomeAplicacao;
    VersaoAutomacao := fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
    CNPJEstabelecimento := fpACBrTEFAPI.DadosEstabelecimento.CNPJ;

    NumeroEmpresa := StrToIntDef(fpACBrTEFAPI.DadosTerminal.CodEmpresa, 0);
    NumeroLoja := StrToIntDef(fpACBrTEFAPI.DadosTerminal.CodFilial, 0);
    NumeroPDV := StrToIntDef(fpACBrTEFAPI.DadosTerminal.CodTerminal, 0);
    PortaPinPad := fpACBrTEFAPI.DadosTerminal.PortaPinPad;

    s := fpACBrTEFAPI.DadosTerminal.EnderecoServidor;
    if (s <> '') then
      if (copy(s, Length(s)-1, 1) <> ':') then  // Informou parâmetro TLS ?
        s := s + ':1';                    // Se não informou, assume como ligado
    ConfiguracaoIpPortaSsl := s;

    Inicializar;
  end;

  inherited;
end;

procedure TACBrTEFAPIClassPayKit.DesInicializar;
begin
  GetTEFPayKitAPI.DesInicializar;
  inherited;
end;

procedure TACBrTEFAPIClassPayKit.InterpretarRespostaAPI;
var
  j, NumeroControle, Cupom: String;
  js, jsLog: TACBrJSONObject;
begin
  NumeroControle := '';
  Cupom := '';
  with GetTEFPayKitAPI do
  begin
    j := ObtemLogTransacaoJson('');
    js := TACBrJSONObject.Parse(j);
    try
      jsLog := js.AsJSONObject['LogTransacao'];
      if Assigned(jsLog) then
        NumeroControle := jsLog.AsString['NumeroControle'];
    finally
      js.Free;
    end;

    if (NumeroControle <> '') then
      Cupom := ObtemComprovanteTransacao(NumeroControle, fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida);
  end;

  with fpACBrTEFAPI.UltimaRespostaTEF do
  begin
    Clear;
    Conteudo.GravaInformacao(899,300, j);
    ImagemComprovante1aVia.Text := Cupom;
    ImagemComprovante2aVia.Text := Cupom;
    DocumentoVinculado := fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao;
    AtualizarHeader;
    ConteudoToProperty;
  end;
end;

function TACBrTEFAPIClassPayKit.ExibirVersaoPayKit: Boolean;
var
  s: String;
begin
  s := Trim(GetTEFPayKitAPI.VersaoDPOS);
  Result := (s <> '');
  if Result then
    TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem(s, telaOperador, 0);
end;

function TACBrTEFAPIClassPayKit.TestarComunicacaoPayKit: Boolean;
begin
  GetTEFPayKitAPI.InicializaDPOS();
end;

function TACBrTEFAPIClassPayKit.PerguntarMenuAdmPayKit: TACBrTEFOperacao;
var
  slMenu: TStringList;
  ItemSel: Integer;
begin
  Result := tefopNenhuma;

  slMenu := TStringList.Create;
  try
    slMenu.Add(ACBrStr('Reimpressão'));
    slMenu.Add(ACBrStr('Resumo de Vendas'));
    slMenu.Add(ACBrStr('Finalizar Dia'));
    slMenu.Add(ACBrStr('Administrativo'));
    slMenu.Add(ACBrStr('Versão'));
    slMenu.Add(ACBrStr('Teste Comunicação'));
    ItemSel := -1;
    TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarMenu( 'Menu Administrativo', slMenu, ItemSel );
    case ItemSel of
      0: Result := tefopReimpressao;
      1: Result := tefopRelatResumido;
      2: Result := tefopFechamento;
      3: Result := tefopAdministrativo;
      4: Result := tefopVersao;
      5: Result := tefopTesteComunicacao;
    end;
  finally
    slMenu.Free;
  end;
end;

function TACBrTEFAPIClassPayKit.GetTEFPayKitAPI: TACBrTEFPayKitAPI;
begin
  Result := ACBrTEFPayKitAPI.GetTEFPayKitAPI;
end;

procedure TACBrTEFAPIClassPayKit.QuandoGravarLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAPIClassPayKit.QuandoExibirMensagemAPI(
  const Mensagem: String; TipoMensagem: TACBrTEFPayKitTipoMensagem;
  MilissegundosExibicao: Integer);
var
  tela: TACBrTEFAPITela;
begin
  case TipoMensagem of
    msgInfo: tela := telaCliente;
    msgAlerta: tela := telaOperador;
    msgErro: tela := telaOperador;
    msgAdicional: tela := telaCliente;
    msgTerminal: tela := telaTodas;
  else
    tela := telaTodas;
  end;

  TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem( Mensagem, tela, MilissegundosExibicao);
end;

procedure TACBrTEFAPIClassPayKit.QuandoPerguntarMenuAPI(const Titulo: String;
  Opcoes: TStringList; var ItemSelecionado: Integer);
begin
  TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarMenu( Titulo, Opcoes, ItemSelecionado);
end;

procedure TACBrTEFAPIClassPayKit.QuandoPerguntarCampoAPI(
  DefinicaoCampo: TACBrTEFPayKitDefinicaoCampo; var Resposta: String;
  var Acao: Integer);
var
  def: TACBrTEFAPIDefinicaoCampo;
  Validado, Cancelado: Boolean;
begin
  def.TituloPergunta := DefinicaoCampo.TituloPergunta;
  def.ValorInicial := DefinicaoCampo.ValorInicial;
  def.MascaraDeCaptura := DefinicaoCampo.MascaraDeCaptura;
  def.TamanhoMaximo := DefinicaoCampo.TamanhoMaximo;
  def.ValorMaximo := Trunc(DefinicaoCampo.ValorMaximo);
  def.TamanhoMinimo := DefinicaoCampo.TamanhoMinimo;
  def.ValorMinimo := Trunc(DefinicaoCampo.ValorMinimo);
  def.TipoDeEntrada := tedTodos;

  case DefinicaoCampo.TipoDeEntrada of
    teBarrasDigitado:
      begin
        def.TipoDeEntrada := tedNumerico;
        def.TipoEntradaCodigoBarras := tbDigitado;
      end;

    teBarrasLido:
      def.TipoEntradaCodigoBarras := tbLeitor;

    teValidade:
      begin
        def.TipoDeEntrada := tedNumerico;
        def.MascaraDeCaptura := '@@/@@';
        def.ValidacaoDado := valdMesAno;
      end;

    teData:
      begin
        def.TipoDeEntrada := tedNumerico;
        def.MascaraDeCaptura := '@@/@@/@@';
        def.ValidacaoDado := valdDiaMesAno;
      end;

    teCodSeguranca:
      begin
        def.TipoDeEntrada := tedNumerico;
        def.TamanhoMinimo := 3;
        def.OcultarDadosDigitados := True;
      end;

    teCartao, teValor, teNumero, teValorEspecial:
      begin
        def.TipoDeEntrada := tedNumerico;
      end;
  end;

  Validado := True;
  Cancelado := False;
  TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarCampo(def, Resposta, Validado, Cancelado);

  if Cancelado then
    Acao := -1
  else
    Acao := 0;
end;

procedure TACBrTEFAPIClassPayKit.VerificarTransacaoEmAndamentoAPI(
  EstadoOperacao: TACBrTEFPayKitEstadoOperacao; out Cancelar: Boolean);
var
  op: TACBrTEFAPIOperacaoAPI;
begin
  Cancelar := False;
  with TACBrTEFAPI(fpACBrTEFAPI) do
  begin
    op := opapiPinPad;
    if Assigned( QuandoEsperarOperacao ) then
      QuandoEsperarOperacao( op, Cancelar );
  end;
end;

procedure TACBrTEFAPIClassPayKit.QuandoExibirQRCodeAPI(const DadosQRCode: String);
begin
  with TACBrTEFAPI(fpACBrTEFAPI) do
  begin
    if Assigned( QuandoExibirQRCode ) then
      QuandoExibirQRCode( DadosQRCode );
  end;
end;

function TACBrTEFAPIClassPayKit.EfetuarAdministrativa(CodOperacaoAdm: TACBrTEFOperacao): Boolean;
begin
  Result := True;
  if (CodOperacaoAdm = tefopAdministrativo) then
    CodOperacaoAdm := PerguntarMenuAdmPayKit;

  case CodOperacaoAdm of
    tefopVersao:
      Result := ExibirVersaoPayKit();
    tefopTesteComunicacao:
      Result := TestarComunicacaoPayKit();
    tefopReimpressao:
      GetTEFPayKitAPI.TransacaoReimpressaoCupom;
    tefopRelatResumido:
      GetTEFPayKitAPI.TransacaoResumoVendas;
    tefopFechamento:
      GetTEFPayKitAPI.FinalizaDPOS(True);
  else
    GetTEFPayKitAPI.TransacaoFuncoesAdministrativas();
  end;
end;

function TACBrTEFAPIClassPayKit.EfetuarAdministrativa(const CodOperacaoAdm: string): Boolean;
begin
  Result := EfetuarAdministrativa( TACBrTEFOperacao(StrToIntDef(CodOperacaoAdm, 0)) );
end;

function TACBrTEFAPIClassPayKit.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
begin
  GetTEFPayKitAPI.CancelarTransacao(CodigoFinalizacao);
  Result := True;
end;

function TACBrTEFAPIClassPayKit.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
var
  NumeroControle, e: String;
  TipoOp: TACBrTEFPayKitTipoOperacao;
  c: TACBrTEFTipoCartao;
begin
  VerificarIdentificadorVendaInformado;
  if (ValorPagto <= 0) then
    fpACBrTEFAPI.DoException(sACBrTEFAPIValorPagamentoInvalidoException);

  NumeroControle := '';
  case Financiamento of
    tefmfParceladoEmissor, tefmfCreditoEmissor: TipoOp := opFinancAdm;
    tefmfParceladoEstabelecimento, tefmfPredatado: TipoOp := opFinancLoja;
  else
    TipoOp := opAVista;
  end;

  if (CartoesAceitos = []) then
    CartoesAceitos := [teftcCredito];

  if (Modalidade = tefmpNaoDefinido) then
    Modalidade := tefmpCartao;

  with GetTEFPayKitAPI do
  begin
    if (Modalidade = tefmpCartao) then
    begin
      if (teftcCredito in CartoesAceitos) then
      begin
        NumeroControle := TransacaoCartaoCredito( ValorPagto,
          StrToIntDef(fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao, 0));
        //TransacaoCartaoCreditoCompleta( ValorPagto,
        //  StrToIntDef(fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao, 0),
        //  NumeroControle, TipoOp,
        //  Parcelas, 0, 0, True, DadosAdicionais);
      end
      else if (teftcDebito in CartoesAceitos) then
      begin
        NumeroControle := TransacaoCartaoDebito( ValorPagto,
          StrToIntDef(fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao, 0));
      end
      else if (teftcVoucher in CartoesAceitos) then
      begin
        NumeroControle := TransacaoCartaoVoucher( ValorPagto,
          StrToIntDef(fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao, 0));
      end
      else if (teftcFrota in CartoesAceitos) then
      begin
        NumeroControle := TransacaoCartaoVoucher( ValorPagto,
          StrToIntDef(fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao, 0));
      end
      else
      begin
        e := '[ ';
        For c := Low(TACBrTEFTipoCartao) to High(TACBrTEFTipoCartao) do
        begin
          if c in CartoesAceitos then
            e := e + GetEnumName(TypeInfo(TACBrTEFTipoCartao), integer(c) )+', '
        end;
        e := e + ' ]';

        fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPICapturaNaoSuportada), [e, ClassName] ));
      end;

    end
    else if (Modalidade = tefmpCarteiraVirtual) then
    begin
      NumeroControle := TransacaoQRCode( ValorPagto,
        StrToIntDef(fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao, 0), '');
    end
    else
      fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPICapturaNaoSuportada),
        [GetEnumName(TypeInfo(TACBrTEFModalidadePagamento), integer(Modalidade) ), ClassName] ));
  end;

  Result := (NumeroControle <> '');
end;

procedure TACBrTEFAPIClassPayKit.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
begin
  with GetTEFPayKitAPI do
  begin
    if (AStatus in [tefstsSucessoAutomatico, tefstsSucessoManual]) then
      ConfirmarTransacao(CodigoFinalizacao)
    else
      DesfazerTransacao(CodigoFinalizacao);
  end;
end;

procedure TACBrTEFAPIClassPayKit.ResolverTransacaoPendente(AStatus: TACBrTEFStatusTransacao);
begin
  FinalizarTransacao( fpACBrTEFAPI.UltimaRespostaTEF.Rede,
                      fpACBrTEFAPI.UltimaRespostaTEF.NSU,
                      fpACBrTEFAPI.UltimaRespostaTEF.Finalizacao,
                      AStatus );
end;

procedure TACBrTEFAPIClassPayKit.AbortarTransacaoEmAndamento;
begin
  GetTEFPayKitAPI.AbortarTransacao;
end;

procedure TACBrTEFAPIClassPayKit.FinalizarVenda;
begin
  GetTEFPayKitAPI.FinalizarTransacao;
end;

procedure TACBrTEFAPIClassPayKit.ExibirMensagemPinPad(const MsgPinPad: String);
begin
  GetTEFPayKitAPI.ExibirMensagemPinPad(MsgPinPad, fTempoMsgPinPad);
end;

function TACBrTEFAPIClassPayKit.ObterDadoPinPad(
  TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: Integer; MinLen: SmallInt;
  MaxLen: SmallInt): String;
var
  Dados: AnsiString;
  TipoColeta: Integer;
begin
  case TipoDado of
    dpDDD: TipoColeta := 91;
    dpRedDDD: TipoColeta := 92;
    dpFone: TipoColeta := 93;
    dpRedFone: TipoColeta := 94;
    dpDDDeFone: TipoColeta := 2;
    dpRedDDDeFone: TipoColeta := 6;
    dpCPF: TipoColeta := 1;
    dpRedCPF: TipoColeta := 8;
    dpRG: TipoColeta := 9;
    dpRedRG: TipoColeta := 10;
    dp4UltDigitos: TipoColeta := 11;
    dpCodSeguranca: TipoColeta := 12;
    dpCNPJ: TipoColeta := 13;
    dpRedCNPJ: TipoColeta := 14;
    dpDataDDMMAAAA: TipoColeta := 3;
    dpDataDDMMAA: TipoColeta := 16;
    dpDataDDMM: TipoColeta := 17;
    dpDiaDD: TipoColeta := 18;
    dpMesMM: TipoColeta := 19;
    dpAnoAA: TipoColeta := 20;
    dpAnoAAAA: TipoColeta := 21;
    dpDataNascimentoDDMMAAAA: TipoColeta := 22;
    dpDataNascimentoDDMMAA: TipoColeta := 23;
    dpDataNascimentoDDMM: TipoColeta := 24;
    dpDiaNascimentoDD: TipoColeta := 25;
    dpMesNascimentoMM: TipoColeta := 26;
    dpAnoNascimentoAA: TipoColeta := 27;
    dpAnoNascimentoAAAA: TipoColeta := 28;
    dpIdentificacao: TipoColeta := 29;
    dpCodFidelidade: TipoColeta := 30;
    dpNumeroMesa: TipoColeta := 31;
    dpQtdPessoas: TipoColeta := 32;
    dpQuantidade: TipoColeta := 33;
    dpNumeroBomba: TipoColeta := 34;
    dpNumeroVaga: TipoColeta := 35;
    dpNumeroGuiche: TipoColeta := 36;
    dpCodVendedor: TipoColeta := 37;
    dpCodGarcom: TipoColeta := 38;
    dpNotaAtendimento: TipoColeta := 39;
    dpNumeroNotaFiscal: TipoColeta := 40;
    dpNumeroComanda: TipoColeta := 41;
    dpPlacaVeiculo: TipoColeta := 42;
    dpQuilometragem: TipoColeta := 43;
    dpQuilometragemInicial: TipoColeta := 44;
    dpQuilometragemFinal: TipoColeta := 45;
    dpPorcentagem: TipoColeta := 46;
    dpPesquisaSatisfacao0_10: TipoColeta := 47;
    dpAvalieAtendimento0_10: TipoColeta := 48;
    dpToken: TipoColeta := 49;
    dpNumeroCartao: TipoColeta := 50;
    dpNumeroParcelas: TipoColeta := 51;
    dpCodigoPlano: TipoColeta := 52;
    dpCodigoProduto: TipoColeta := 53;
  else
    fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPICapturaNaoSuportada),
      [GetEnumName(TypeInfo(TACBrTEFAPIDadoPinPad), integer(TipoDado) ), ClassName] ));
  end;

  if (MinLen = 0) and (MaxLen = 0) then
    CalcularTamanhosCampoDadoPinPad(TipoDado, MinLen, MaxLen);

  Dados := Format('%.2d',[MinLen]) + Format('%.2d',[MaxLen]) +
           Format('%.2d',[TipoColeta]) + StringOfChar(' ', 32);
  GetTEFPayKitAPI.TransacaoEspecial(121, Dados);
  Result := Trim(copy(Dados, 7, 32));
end;

function TACBrTEFAPIClassPayKit.VerificarPresencaPinPad: Byte;
var
  s: String;
begin
  with GetTEFPayKitAPI do
  begin
    s := LeIdentificacaoPinPad;
    if (s <> '') then
      Result := StrToIntDef( OnlyNumber(ObterPortaPinPadINI), 0);
  end;
end;

procedure TACBrTEFAPIClassPayKit.ObterListaImagensPinPad(ALista: TStrings);
var
  p, l: Integer;
  s, n: String;
  Dados: AnsiString;
begin
  ALista.Clear;
  Dados := StringOfChar(' ', 993);

  GetTEFPayKitAPI.TransacaoEspecial(126, Dados);
  s := Trim(Dados);
  p := 1;
  l := Length(s);
  while p < l do
  begin
    n := copy(s, p, 8);
    ALista.Add(n);
    Inc(p, 8);
  end;
end;

procedure TACBrTEFAPIClassPayKit.ExibirImagemPinPad(const NomeImagem: String);
var
  Dados: AnsiString;
begin
  Dados := PadRight(NomeImagem, 8);
  GetTEFPayKitAPI.TransacaoEspecial(125, Dados);
end;

procedure TACBrTEFAPIClassPayKit.ApagarImagemPinPad(const NomeImagem: String);
var
  Dados: AnsiString;
begin
  Dados := PadRight(NomeImagem, 993, #0);
  GetTEFPayKitAPI.TransacaoEspecial(127, Dados);
end;

procedure TACBrTEFAPIClassPayKit.CarregarImagemPinPad(const NomeImagem: String;
  AStream: TStream; TipoImagem: TACBrTEFAPIImagemPinPad);
var
  tmpFile, ext: String;
  Dados: AnsiString;
  ms: TMemoryStream;
begin
  ext := IfThen(TipoImagem=imgPNG, '.png', '.jpg');
  tmpFile := GetTEFPayKitAPI.CalcPayKitPath(CPayKitDirBin) +
             NomeImagem + ext;
  ms := TMemoryStream.Create;
  try
    AStream.Position := 0;
    ms.LoadFromStream(AStream);
    ms.Position := 0;
    ms.SaveToFile(tmpFile);
  finally
    ms.Free;
  end;

  if FileExists(tmpFile) then
  begin
    try
      Dados := IfThen(TipoImagem=imgPNG,'1','2')+'000' + NomeImagem + PadRight(tmpFile, 256, #0);
      GetTEFPayKitAPI.TransacaoEspecial(123, Dados);
    finally
      DeleteFile(tmpFile);
    end;
  end;
end;

end.
