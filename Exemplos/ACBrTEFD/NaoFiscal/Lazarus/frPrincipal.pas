{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit frPrincipal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Spin, Buttons, DBCtrls, ExtCtrls, Grids, ACBrTEFD, ACBrPosPrinter,
  ACBrTEFDClass, uVendaClass, ACBrTEFDCliSiTef, ACBrTEFPayGoComum, ACBrTEFPayGoWebComum,
  ACBrTEFComum, ACBrTEFDPayGoWeb;

type

  TTipoBotaoOperacao = (bopNaoExibir, bopCancelarVenda, bopLiberarCaixa, bopCancelarEsperaTEF);

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrTEFD1: TACBrTEFD;
    btAdministrativo: TBitBtn;
    btImprimir: TBitBtn;
    btIncluirPagamentos: TBitBtn;
    btExcluirPagamento: TBitBtn;
    btLerParametros: TBitBtn;
    btLimparImpressora: TBitBtn;
    btMudaPagina: TBitBtn;
    btMsgPinPad: TButton;
    btOperacao: TBitBtn;
    btEfetuarPagamentos: TBitBtn;
    btSalvarParametros: TBitBtn;
    btSerial: TSpeedButton;
    btProcuraImpressoras: TSpeedButton;
    btTestarPosPrinter: TBitBtn;
    btTestarTEF: TBitBtn;
    btObterCPF: TButton;
    cbConfirmarAntesComprovantes: TCheckBox;
    cbTestePayGo: TComboBox;
    cbIMprimirViaReduzida: TCheckBox;
    cbMultiplosCartoes: TCheckBox;
    cbSimularErroNoDoctoFiscal: TCheckBox;
    cbSuportaDesconto: TCheckBox;
    cbSuportaReajusteValor: TCheckBox;
    cbSuportaSaque: TCheckBox;
    cbxGP: TComboBox;
    cbxQRCode: TComboBox;
    cbxModeloPosPrinter: TComboBox;
    cbxPagCodigo: TComboBox;
    cbxPorta: TComboBox;
    cbEnviarImpressora: TCheckBox;
    edRazaoSocial: TEdit;
    edLog: TEdit;
    edAplicacaoNome: TEdit;
    edRegistro: TEdit;
    edAplicacaoVersao: TEdit;
    gbConfigImpressora: TGroupBox;
    gbConfigTEF: TGroupBox;
    gbPagamentos: TGroupBox;
    GroupBox1: TGroupBox;
    imgQRCode: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lMensagemCliente: TLabel;
    lMensagemOperador: TLabel;
    lNumOperacao: TLabel;
    lTituloMsgOperador: TLabel;
    lTituloMensagemCliente: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label7: TLabel;
    lSaidaImpressao: TLabel;
    lURLTEF: TLabel;
    mImpressao: TMemo;
    mLog: TMemo;
    pQRCode: TPanel;
    pImpressoraBotes: TPanel;
    pImpressao: TPanel;
    pMensagem: TPanel;
    pMensagemCliente: TPanel;
    pMensagemOperador: TPanel;
    pPrincipal: TPanel;
    pBotoesConfiguracao: TPanel;
    pConfiguracao: TPanel;
    pLogs: TPanel;
    pBotoesPagamentos: TPanel;
    pSimulador: TPanel;
    pStatus: TPanel;
    edTotalVenda: TEdit;
    edTotalPago: TEdit;
    edTroco: TEdit;
    gbTotaisVenda: TGroupBox;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ImageList1: TImageList;
    pOperacao: TPanel;
    pgPrincipal: TPageControl;
    SbArqLog: TSpeedButton;
    seColunas: TSpinEdit;
    seEspLinhas: TSpinEdit;
    seLinhasPular: TSpinEdit;
    seMaxCartoes: TSpinEdit;
    seTotalAcrescimo: TFloatSpinEdit;
    seTotalDesconto: TFloatSpinEdit;
    seTrocoMaximo: TFloatSpinEdit;
    seValorInicialVenda: TFloatSpinEdit;
    sbLimparLog: TSpeedButton;
    Splitter1: TSplitter;
    sgPagamentos: TStringGrid;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    tsConfiguracao: TTabSheet;
    tsOperacao: TTabSheet;
    procedure ACBrTEFD1AguardaResp(Arquivo: String; SegundosTimeOut: Integer;
      var Interromper: Boolean);
    procedure ACBrTEFD1AntesFinalizarRequisicao(Req: TACBrTEFDReq);
    procedure ACBrTEFD1BloqueiaMouseTeclado(Bloqueia: Boolean;
      var Tratado: Boolean);
    procedure ACBrTEFD1ComandaECF(Operacao: TACBrTEFDOperacaoECF;
      Resp: TACBrTEFDResp; var RetornoECF: Integer);
    procedure ACBrTEFD1ComandaECFAbreVinculado(COO, IndiceECF: String;
      Valor: Double; var RetornoECF: Integer);
    procedure ACBrTEFD1ComandaECFImprimeVia(
      TipoRelatorio: TACBrTEFDTipoRelatorio; Via: Integer;
      ImagemComprovante: TStringList; var RetornoECF: Integer);
    procedure ACBrTEFD1ComandaECFPagamento(IndiceECF: String; Valor: Double;
      var RetornoECF: Integer);
    procedure ACBrTEFD1ComandaECFSubtotaliza(DescAcre: Double;
      var RetornoECF: Integer);
    procedure ACBrTEFD1DepoisCancelarTransacoes(
      RespostasPendentes: TACBrTEFDRespostasPendentes);
    procedure ACBrTEFD1DepoisConfirmarTransacoes(
      RespostasPendentes: TACBrTEFDRespostasPendentes);
    procedure ACBrTEFD1ExibeMsg(Operacao: TACBrTEFDOperacaoMensagem;
      Mensagem: String; var AModalResult: TModalResult);
    procedure ACBrTEFD1ExibeQRCode(const Dados: String);
    procedure ACBrTEFD1GravarLog(const GP: TACBrTEFDTipo; ALogLine: String;
      var Tratado: Boolean);
    procedure ACBrTEFD1InfoECF(Operacao: TACBrTEFDInfoECF;
      var RetornoECF: String);
    procedure btAdministrativoClick(Sender: TObject);
    procedure btEfetuarPagamentosClick(Sender: TObject);
    procedure btExcluirPagamentoClick(Sender: TObject);
    procedure btIncluirPagamentosClick(Sender: TObject);
    procedure btMsgPinPadClick(Sender: TObject);
    procedure btOperacaoClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btMudaPaginaClick(Sender: TObject);
    procedure btProcuraImpressorasClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure btSerialClick(Sender: TObject);
    procedure btTestarPosPrinterClick(Sender: TObject);
    procedure btTestarTEFClick(Sender: TObject);
    procedure btObterCPFClick(Sender: TObject);
    procedure cbEnviarImpressoraChange(Sender: TObject);
    procedure CliSiTefExibeMenu(Titulo: String; Opcoes: TStringList;
      var ItemSelecionado: Integer; var VoltarMenu: Boolean);
    procedure CliSiTefObtemCampo(Titulo: String; TamanhoMinimo,
      TamanhoMaximo: Integer; TipoCampo: Integer;
      Operacao: TACBrTEFDCliSiTefOperacaoCampo; var Resposta: AnsiString;
      var Digitado: Boolean; var VoltarMenu: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure btImprimirClick(Sender: TObject);
    procedure btLimparImpressoraClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lURLTEFClick(Sender: TObject);
    procedure PayGoWebAguardaPinPad(
      OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad; var Cancelar: Boolean);
    procedure PayGoWebAvaliarTransacaoPendente(var Status: LongWord;
      const Mensagem: String; Resp: TACBrTEFDResp);
    procedure PayGoWebExibeMensagem(Mensagem: String;
      Terminal: TACBrTEFPGWebAPITerminalMensagem; MilissegundosExibicao: Integer);
    procedure PayGoWebExibeMenu(Titulo: String; Opcoes: TStringList;
      var ItemSelecionado: Integer; var Cancelado: Boolean);
    procedure PayGoWebObtemCampo(
      DefinicaoCampo: TACBrTEFPGWebAPIDefinicaoCampo; var Resposta: String;
      var Validado: Boolean; var Cancelado: Boolean);
    procedure SbArqLogClick(Sender: TObject);
    procedure sbLimparLogClick(Sender: TObject);
    procedure seTotalAcrescimoChange(Sender: TObject);
    procedure seTotalDescontoChange(Sender: TObject);
    procedure seValorInicialVendaChange(Sender: TObject);
  private
    FVenda: TVenda;
    FTipoBotaoOperacao: TTipoBotaoOperacao;
    FCanceladoPeloOperador: Boolean;
    FTempoDeEspera: TDateTime;
    FTestePayGo: Integer;
    FIndicePagto: String;

    function GetNomeArquivoConfiguracao: String;
    function GetNomeArquivoVenda: String;
    function GetStatusVenda: TStatusVenda;
    procedure SetTipoBotaoOperacao(AValue: TTipoBotaoOperacao);
    procedure SetStatusVenda(AValue: TStatusVenda);

    procedure TratarException(Sender : TObject; E : Exception);
  protected
    procedure LerConfiguracao;
    procedure GravarConfiguracao;

    procedure IrParaOperacaoTEF;
    procedure IrParaConfiguracao;

    procedure ConfigurarTEF;
    procedure AtivarTEF;
    procedure ConfigurarPosPrinter;
    procedure AtivarPosPrinter;
    procedure Ativar;
    procedure Desativar;

    procedure IniciarOperacao;
    procedure AdicionarPagamento(const Indice: String; AValor: Double);
    procedure ExcluirPagamento(IndicePagto: Integer);
    function AcharTransacaoTEFPendente(IndicePagto: Integer): TACBrTEFResp;
    procedure CancelarVenda;
    procedure FinalizarVenda; // Em caso de Venda, Gere e transmita seu Documento Fiscal
    procedure VerificarTestePayGo;

    procedure AtualizarCaixaLivreNaInterface;
    procedure AtualizarVendaNaInterface;
    procedure AtualizarTotaisVendaNaInterface;
    procedure AtualizarPagamentosVendaNaInterface;
    procedure MensagemTEF(const MsgOperador, MsgCliente: String);
    procedure LimparMensagensTEF;
    procedure ExibirPainelQRCode;
    procedure OcultatPainelQRCode;

    procedure AdicionarLinhaLog(AMensagem: String);
    procedure AdicionarLinhaImpressao(ALinha: String);
  public
    property NomeArquivoConfiguracao: String read GetNomeArquivoConfiguracao;
    property NomeArquivoVenda: String read GetNomeArquivoVenda;

    property StatusVenda: TStatusVenda read GetStatusVenda write SetStatusVenda;
    property TipoBotaoOperacao: TTipoBotaoOperacao read FTipoBotaoOperacao write SetTipoBotaoOperacao;

    property Venda: TVenda read FVenda;
  end;

var
  FormPrincipal: TFormPrincipal;

implementation

uses
  IniFiles, typinfo, dateutils, math, strutils, LCLType,
  frIncluirPagamento, frMenuTEF, frObtemCampo, frExibeMensagem,
  configuraserial,
  ACBrUtil, ACBrDelphiZXingQRCode;

{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.FormCreate(Sender: TObject);
var
  I: TACBrTEFDTipo;
  N: TACBrPosPrinterModelo;
  O: TACBrPosPaginaCodigo;
begin
  FVenda := TVenda.Create(NomeArquivoVenda);

  cbxGP.Items.Clear ;
  For I := Low(TACBrTEFDTipo) to High(TACBrTEFDTipo) do
     cbxGP.Items.Add( GetEnumName(TypeInfo(TACBrTEFDTipo), integer(I) ) ) ;
  cbxGP.ItemIndex := 0 ;

  cbxModeloPosPrinter.Items.Clear ;
  For N := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
     cbxModeloPosPrinter.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(N) ) ) ;

  cbxPagCodigo.Items.Clear ;
  For O := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
     cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(O) ) ) ;

  pgPrincipal.ShowTabs := False;
  pgPrincipal.ActivePageIndex := 0;

  LerConfiguracao;
  LimparMensagensTEF;
  FTipoBotaoOperacao := High(TTipoBotaoOperacao);    // Força atualizar tela
  Venda.Status := High(TStatusVenda);                // Força atualizar tela
  FCanceladoPeloOperador := False;
  FTempoDeEspera := 0;
  FTestePayGo := 0;
  FIndicePagto := '';

  Application.OnException := @TratarException;

  btProcuraImpressoras.Click;
end;

procedure TFormPrincipal.FormDestroy(Sender: TObject);
begin
  FVenda.Free;
end;

procedure TFormPrincipal.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (btOperacao.Visible and btOperacao.Enabled) then
  begin
    btOperacao.Click;
    Key := 0;
  end;
end;

procedure TFormPrincipal.lURLTEFClick(Sender: TObject);
begin
  OpenURL('https://projetoacbr.com.br/tef/');
end;

procedure TFormPrincipal.PayGoWebAguardaPinPad(
  OperacaoPinPad: TACBrTEFPGWebAPIOperacaoPinPad; var Cancelar: Boolean);
begin
   AdicionarLinhaLog( 'PayGoWebAguardaPinPad: '+
                      GetEnumName(TypeInfo(TACBrTEFPGWebAPIOperacaoPinPad), integer(OperacaoPinPad) ) );

   if FCanceladoPeloOperador then
   begin
     FCanceladoPeloOperador := False;
     Cancelar := True ;
   end
   else if (StatusVenda <> stsAguardandoTEF) then
   begin
     StatusVenda := stsAguardandoTEF;   // Liga Botão que permite cancelar
     FCanceladoPeloOperador := False;
   end;

   Application.ProcessMessages;
end;

procedure TFormPrincipal.PayGoWebAvaliarTransacaoPendente(
  var Status: LongWord; const Mensagem: String; Resp: TACBrTEFDResp);
//var
//  MR: TModalResult;
begin
  // Opção 1: Confirmando todas as transações pendentes...
  Status := PWCNF_CNF_MANU_AUT;

(*
  // Opção 2: Perguntando ao Operador, o que fazer com a transação pendente

  // Aqui você pode Confirmar ou Cancelar as transações pendentes de acordo com a sua lógica
  // Ou ainda, fazer uma pergunta ao usuário, como nesse exemplo...
  // Veja os valores possíveis, para "Status", em ACBrTEFPayGoWebComum.pas, procure por: "PWCNF_"

  MR := mrYes;
  ACBrTEFD1ExibeMsg( opmYesNo, Mensagem + sLineBreak + sLineBreak + 'Confirmar ?', MR);

  if (MR = mrNo) then
    Status := PWCNF_REV_MANU_AUT
  else
  begin
    // Imprimindo comrovante pendente //
    AdicionarLinhaImpressao( Resp.ImagemComprovante1aVia.Text );
    AdicionarLinhaImpressao('</pular_linhas>');
    AdicionarLinhaImpressao('</corte>');
    AdicionarLinhaImpressao( Resp.ImagemComprovante2aVia.Text );
    AdicionarLinhaImpressao('</pular_linhas>');
    AdicionarLinhaImpressao('</corte>');

    Status := PWCNF_CNF_MANU_AUT;
  end;
*)
end;

procedure TFormPrincipal.PayGoWebExibeMensagem(Mensagem: String;
  Terminal: TACBrTEFPGWebAPITerminalMensagem; MilissegundosExibicao: Integer);
var
  FormExibeMensagem: TFormExibeMensagem;
begin
  if (Mensagem = '') then
  begin
    if (Terminal in [tmCliente, tmTodas]) then
      MensagemTEF('',' ') ;
    if (Terminal in [tmOperador, tmTodas]) then
      MensagemTEF(' ','') ;
  end

  else if MilissegundosExibicao >= 0 then
  begin
    FormExibeMensagem := TFormExibeMensagem.Create(Self);
    try
      FormExibeMensagem.Mensagem := Mensagem;
      FormExibeMensagem.TempoEspera := MilissegundosExibicao;
      FormExibeMensagem.ShowModal;
    finally
      FormExibeMensagem.Free;
    end;
  end

  else
  begin
    if (Terminal in [tmCliente, tmTodas]) then
      MensagemTEF('',Mensagem) ;
    if (Terminal in [tmOperador, tmTodas]) then
      MensagemTEF(Mensagem,'') ;
  end;
end;

procedure TFormPrincipal.PayGoWebExibeMenu(Titulo: String; Opcoes: TStringList;
  var ItemSelecionado: Integer; var Cancelado: Boolean);
Var
  MR: TModalResult ;
  FormMenuTEF : TFormMenuTEF;
begin
  AdicionarLinhaLog( 'PayGoWebExibeMenu: '+Titulo + sLineBreak + Opcoes.Text );
  if Opcoes.Count < 1 then
  begin
    Cancelado := True;
    Exit;
  end;

  FormMenuTEF := TFormMenuTEF.Create(self);
  try
    FormMenuTEF.Titulo := Titulo;
    FormMenuTEF.Opcoes := Opcoes;
    FormMenuTEF.btVoltar.Visible := False;  // PayGoWeb não suporta Voltar
    FormMenuTEF.UsaTeclasDeAtalho := (copy(Opcoes[0],1,4) = '1 - ');
    FormMenuTEF.ItemSelecionado := ItemSelecionado;

    MR := FormMenuTEF.ShowModal ;

    if (MR = mrOK) then
      ItemSelecionado := FormMenuTEF.ItemSelecionado
    else
      Cancelado := True;
  finally
    FormMenuTEF.Free;
  end;
end;

procedure TFormPrincipal.PayGoWebObtemCampo(
  DefinicaoCampo: TACBrTEFPGWebAPIDefinicaoCampo; var Resposta: String;
  var Validado: Boolean; var Cancelado: Boolean);
Var
  MR: TModalResult ;
  FormObtemCampo: TFormObtemCampo;
begin
  AdicionarLinhaLog( 'PayGoWebObtemCampo: '+DefinicaoCampo.Titulo );

  { NOTA: Se DefinicaoCampo.ValidacaoDado = "pgvSenhaLojista"
          Você deve chamar a Tela de Senha do seu sistema, e retornar o NOME do
          Operador, NUNCA a Senha digitada

  if (DefinicaoCampo.ValidacaoDado = pgvSenhaLojista) then
  begin
    Validado := True;
    Cancelado := False;
    Resposta := fOperador;
    Exit;
  end;
  }

  FormObtemCampo := TFormObtemCampo.Create(self);
  try
    FormObtemCampo.Titulo := DefinicaoCampo.Titulo;
    FormObtemCampo.TamanhoMaximo := DefinicaoCampo.TamanhoMaximo;
    FormObtemCampo.TamanhoMinimo := DefinicaoCampo.TamanhoMinimo;
    FormObtemCampo.Resposta := DefinicaoCampo.ValorInicial;
    FormObtemCampo.Ocultar := DefinicaoCampo.OcultarDadosDigitados;
    FormObtemCampo.Mascara := DefinicaoCampo.MascaraDeCaptura;
    FormObtemCampo.btVoltar.Visible := False;  // PayGoWeb não suporta Voltar;

    if (pos('R$', DefinicaoCampo.MascaraDeCaptura) > 0) or
       (pos('@.@@@,@@', DefinicaoCampo.MascaraDeCaptura) > 0) or
       (pos('@@@@@@,@@', DefinicaoCampo.MascaraDeCaptura) > 0) then
      FormObtemCampo.TipoCampo := tcoCurrency
    else
    begin
      case DefinicaoCampo.TiposEntradaPermitidos of
        pgApenasLeitura:
          FormObtemCampo.edtResposta.ReadOnly := True;
        pgtNumerico:
          if (pos('@,@@', DefinicaoCampo.MascaraDeCaptura) > 0) then
            FormObtemCampo.TipoCampo := tcoDecimal
          else
            FormObtemCampo.TipoCampo := tcoNumeric;
        pgtAlfabetico:
          FormObtemCampo.TipoCampo := tcoAlfa;
        pgtAlfaNum:
          FormObtemCampo.TipoCampo := tcoAlfaNum;
      else
        FormObtemCampo.TipoCampo := tcoString;
      end;
    end;

    MR := FormObtemCampo.ShowModal ;

    Cancelado := (MR <> mrOK) ;
    Resposta := FormObtemCampo.Resposta;

    if (FormObtemCampo.TipoCampo = tcoCurrency) then  // PayGoWeb não precisa de ponto decimal
      Resposta := OnlyNumber(Resposta);
  finally
    FormObtemCampo.Free;
  end;
end;

procedure TFormPrincipal.SbArqLogClick(Sender: TObject);
var
  AFileLog: String;
begin
  if pos(PathDelim,edLog.Text) = 0 then
    AFileLog := ExtractFilePath( Application.ExeName ) + edLog.Text
  else
    AFileLog := edLog.Text;

  OpenURL( AFileLog );
end;

procedure TFormPrincipal.btImprimirClick(Sender: TObject);
begin
  ACBrPosPrinter1.Buffer.Assign(mImpressao.Lines);
  ACBrPosPrinter1.Imprimir;
end;

procedure TFormPrincipal.btLimparImpressoraClick(Sender: TObject);
begin
  mImpressao.Lines.Clear;
end;

procedure TFormPrincipal.sbLimparLogClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TFormPrincipal.seTotalAcrescimoChange(Sender: TObject);
begin
  Venda.TotalAcrescimo := seTotalAcrescimo.Value;
  AtualizarTotaisVendaNaInterface;
end;

procedure TFormPrincipal.seTotalDescontoChange(Sender: TObject);
begin
  Venda.TotalDesconto := seTotalDesconto.Value;
  AtualizarTotaisVendaNaInterface;
end;

procedure TFormPrincipal.btMudaPaginaClick(Sender: TObject);
begin
  if pgPrincipal.ActivePage = tsConfiguracao then
    IrParaOperacaoTEF
  else
    IrParaConfiguracao;
end;

procedure TFormPrincipal.btProcuraImpressorasClick(Sender: TObject);
begin
  cbxPorta.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasSeriais( cbxPorta.Items );
  {$IfDef MSWINDOWS}
  ACBrPosPrinter1.Device.AcharPortasUSB( cbxPorta.Items );
  {$EndIf}
  ACBrPosPrinter1.Device.AcharPortasRAW( cbxPorta.Items );

  cbxPorta.Items.Add('TCP:192.168.0.31:9100') ;

  {$IfNDef MSWINDOWS}
   cbxPorta.Items.Add('/dev/ttyS0') ;
   cbxPorta.Items.Add('/dev/ttyUSB0') ;
   cbxPorta.Items.Add('/tmp/ecf.txt') ;
  {$Else}
   cbxPorta.Items.Add('\\localhost\Epson') ;
   cbxPorta.Items.Add('c:\temp\ecf.txt') ;
  {$EndIf}
end;

procedure TFormPrincipal.btLerParametrosClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TFormPrincipal.btOperacaoClick(Sender: TObject);
begin
  AdicionarLinhaLog('- btOperacaoClick');

  case TipoBotaoOperacao of
    bopLiberarCaixa:
      StatusVenda := stsLivre;

    bopCancelarVenda:
      CancelarVenda;

    bopCancelarEsperaTEF:
    begin
      AdicionarLinhaLog( '  Operação Cancelada Pelo Operador');
      FCanceladoPeloOperador := True;
    end;
  end;
end;

procedure TFormPrincipal.btEfetuarPagamentosClick(Sender: TObject);
begin
  VerificarTestePayGo;
  StatusVenda := stsEmPagamento;
  btIncluirPagamentos.Click;
end;

procedure TFormPrincipal.btExcluirPagamentoClick(Sender: TObject);
var
  i: Integer;
begin
  i := sgPagamentos.Row-1;
  ExcluirPagamento(i);
  StatusVenda := stsEmPagamento;
end;

procedure TFormPrincipal.btIncluirPagamentosClick(Sender: TObject);
var
  FormIncluirPagamento: TFormIncluirPagamento;
begin
  FormIncluirPagamento := TFormIncluirPagamento.Create(Self);
  try
    FormIncluirPagamento.cbFormaPagamento.ItemIndex := 2;
    FormIncluirPagamento.seValorPago.Value := -Venda.Troco;
    if (FormIncluirPagamento.ShowModal = mrOK) then
    begin
      AdicionarPagamento( cPagamentos[FormIncluirPagamento.cbFormaPagamento.ItemIndex, 0],
                          FormIncluirPagamento.seValorPago.Value );
    end;
  finally
    FormIncluirPagamento.Free;
  end;
end;

procedure TFormPrincipal.btMsgPinPadClick(Sender: TObject);
var
  Msg: String;
begin
  Msg := 'PROJETO ACBR|'+FormatDateTimeBr(now,'DD/MM HH:NN:SS');
  ACBrTEFD1.ExibirMensagemPinPad(Msg);
end;

procedure TFormPrincipal.ACBrTEFD1ExibeMsg(Operacao: TACBrTEFDOperacaoMensagem;
  Mensagem: String; var AModalResult: TModalResult);
var
   Fim : TDateTime;
   OldMensagem : String;
begin
  case Operacao of
    opmOK:
      begin
        if ACBrTEFD1.GPAtual = gpPayGoWeb then
          PayGoWebExibeMensagem( Mensagem, tmOperador, CMilissegundosMensagem)
        else
          AModalResult := MessageDlg( Mensagem, mtInformation, [mbOK], 0);
      end;

    opmYesNo:
       AModalResult := MessageDlg( Mensagem, mtConfirmation, [mbYes, mbNo], 0);

    opmExibirMsgOperador:
      MensagemTEF(Mensagem,'') ;

    opmRemoverMsgOperador:
      MensagemTEF(' ','') ;

    opmExibirMsgCliente:
      MensagemTEF('', Mensagem) ;

    opmRemoverMsgCliente:
      MensagemTEF('', ' ') ;

    opmDestaqueVia:
      begin
        OldMensagem := lMensagemOperador.Caption;
        try
          { Aguardando 3 segundos }
          Fim := IncSecond(now, 3)  ;
          repeat
            MensagemTEF(Mensagem + ' ' + IntToStr(SecondsBetween(Fim,now)), '');
            Sleep(200) ;
          until (now > Fim) ;
        finally
          MensagemTEF(OldMensagem, '');
        end;
      end;
  end;
end;

procedure TFormPrincipal.ACBrTEFD1ExibeQRCode(const Dados: String);
var
  QRCode: TDelphiZXingQRCode;
  QRCodeBitmap: TBitmap;
  Row, Column: Integer;
begin
  if not (StatusVenda in [stsAguardandoTEF, stsOperacaoTEF]) then
    StatusVenda := stsAguardandoTEF;

  if (cbxQRCode.ItemIndex = 4) then  // 4 - Imprimir
  begin
    if (Dados <> '') then
      AdicionarLinhaImpressao( '</zera></ce>'+
                               '<qrcode_largura>8</qrcode_largura>'+
                               '<qrcode>'+Dados+'</qrcode>'+
                               '</lf></lf></corte_total>');

    Exit;
  end;

  if (Dados <> '') then
    ExibirPainelQRCode
  else
  begin
    OcultatPainelQRCode;
    Exit;
  end;

  QRCode := TDelphiZXingQRCode.Create;
  QRCodeBitmap := TBitmap.Create;
  try
    QRCode.Encoding  := qrUTF8BOM;
    QRCode.QuietZone := 2;
    QRCode.Data      := widestring(Dados);

    QRCodeBitmap.Width  := QRCode.Columns;
    QRCodeBitmap.Height := QRCode.Rows;

    for Row := 0 to QRCode.Rows - 1 do
    begin
      for Column := 0 to QRCode.Columns - 1 do
      begin
        if (QRCode.IsBlack[Row, Column]) then
          QRCodeBitmap.Canvas.Pixels[Column, Row] := clBlack
        else
          QRCodeBitmap.Canvas.Pixels[Column, Row] := clWhite;
      end;
    end;

    imgQRCode.Picture.Bitmap.Assign(QRCodeBitmap);
  finally
    QRCode.Free;
    QRCodeBitmap.Free;
  end;
end;

procedure TFormPrincipal.ACBrTEFD1DepoisCancelarTransacoes(
  RespostasPendentes: TACBrTEFDRespostasPendentes);
var
  i: Integer;
begin
  for i := 0 to RespostasPendentes.Count-1  do
  begin
    with RespostasPendentes[i] do
    begin
      AdicionarLinhaLog('Cancelada: '+Header+' ID: '+IntToStr( ID ) );
      AdicionarLinhaLog('- Rede: '  + Rede + ', NSU: '  + NSU );

      Venda.Pagamentos.CancelarPagamento(Rede, NSU, ValorTotal);
    end;
  end;

  AtualizarPagamentosVendaNaInterface;
end;

procedure TFormPrincipal.ACBrTEFD1DepoisConfirmarTransacoes(
  RespostasPendentes: TACBrTEFDRespostasPendentes);
var
  i: Integer;
begin
  for i := 0 to RespostasPendentes.Count-1  do
  begin
    with RespostasPendentes[i] do
    begin
      AdicionarLinhaLog('Confirmado: '+Header+' ID: '+IntToStr( ID ) );

      // Lendo os campos mapeados //
      AdicionarLinhaLog('- Rede: '  + Rede + ', NSU: '  + NSU );
      //AdicionarLinhaLog('- Parcelas: '+ IntToStr(QtdParcelas) +
      //                  ', parcelado por: '+ GetEnumName(TypeInfo(TACBrTEFRespParceladoPor), integer(ParceladoPor) ));
      AdicionarLinhaLog('- É Débito: '+BoolToStr(Debito)+
                        ', É Crédito: '+BoolToStr(Credito)+
                        ', Valor: '+ FormatFloat('###,###,##0.00',ValorTotal)) ;

      // Lendo um Campo Específico //
      //AdicionarLinhaLog('- Campo 11: ' + LeInformacao(11,0).AsString );

      Venda.Pagamentos.ConfirmarPagamento(Rede, NSU, ValorTotal);
    end;
  end;

  AtualizarPagamentosVendaNaInterface;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECF(Operacao: TACBrTEFDOperacaoECF;
  Resp: TACBrTEFDResp; var RetornoECF: Integer);

  procedure PularLinhasECortar;
  begin
    AdicionarLinhaImpressao('</pular_linhas>');
    AdicionarLinhaImpressao('</corte>');
  end;

begin
  AdicionarLinhaLog('ACBrTEFD1ComandaECF: '+GetEnumName( TypeInfo(TACBrTEFDOperacaoECF), integer(Operacao) ));

  try
    case Operacao of
      opeAbreGerencial:
        AdicionarLinhaImpressao('</zera>');

      opeSubTotalizaCupom:
      begin
        if StatusVenda = stsIniciada then
          StatusVenda := stsEmPagamento;
      end;

      opeCancelaCupom:
        CancelarVenda;

      opeFechaCupom:
        FinalizarVenda;

      opePulaLinhas:
        PularLinhasECortar;

      opeFechaGerencial, opeFechaVinculado:
      begin
        PularLinhasECortar;
        if StatusVenda in [stsOperacaoTEF] then
          StatusVenda := stsFinalizada;
      end;
    end;

    RetornoECF := 1 ;
  except
    RetornoECF := 0 ;
  end;
end;

procedure TFormPrincipal.ACBrTEFD1AntesFinalizarRequisicao(Req: TACBrTEFDReq);
begin
  AdicionarLinhaLog('Enviando: '+Req.Header+' ID: '+IntToStr( Req.ID ) );

  FCanceladoPeloOperador := False;
  FTempoDeEspera := 0;
  // Use esse evento, para inserir campos personalizados, ou modificar o arquivo
  // de requisião, que será criado e envido para o Gerenciador Padrão

  if (ACBrTEFD1.GPAtual = gpPayGo) then
  begin
    if (Req.Header = 'CRT') then
    begin
      // Instruindo CRT a apenas transações de Crédito
      if (FIndicePagto = '03') then
      begin
        Req.GravaInformacao(730,000,'1');  // 1: venda (pagamento com cartão)
        Req.GravaInformacao(731,000,'1');  // 1: crédito
        //Req.GravaInformacao(732,000,'1');  // 1: à vista
      end

      // Instruindo CRT a apenas transações de Débito
      else if (FIndicePagto = '04') then
      begin
        Req.GravaInformacao(730,000,'1');  // 1: venda (pagamento com cartão)
        Req.GravaInformacao(731,000,'2');  // 2: débito
        //Req.GravaInformacao(732,000,'1');  // 1: à vista
      end;

      FIndicePagto := '';
    end;

    if (FTestePayGo > 0) then
    begin
      if (Req.Header = 'CRT') and (FTestePayGo = 2) then // Passo 02 - Venda à vista aprovada com pré-seleção de parâmetros
      begin
        Req.GravaInformacao(010,000,'CERTIF');
        Req.GravaInformacao(730,000,'1');  // operação “VENDA”
        Req.GravaInformacao(731,000,'1');  // tipo de cartão “CRÉDITO”
        Req.GravaInformacao(732,000,'1');  // tipo de financiamento “À VISTA”
      end

      else if (Req.Header = 'CRT') and (FTestePayGo = 3) then // Passo 03 - Venda parcelada aprovada com pré-seleção de parâmetros
      begin
        Req.GravaInformacao(010,000,'CERTIF');
        Req.GravaInformacao(018,000,'3');  // número de parcelas = 3
        Req.GravaInformacao(730,000,'1');  // operação “VENDA”
        Req.GravaInformacao(731,000,'2');  // tipo de cartão “DÉBITO”
        Req.GravaInformacao(732,000,'3');  // tipo de financiamento “PARCELADO PELO ESTABELECIMENTO”
      end

      else if (Req.Header = 'CRT') and (FTestePayGo = 4) then // Passo 04 - Venda aprovada em moeda estrangeira
      begin
        Req.GravaInformacao(004,000,'1');  // Dólar americano
      end

      else if (Req.Header = 'CRT') and (FTestePayGo = 27) then // Passo 27 - Venda aprovada com pré-seleção de parâmetros de carteira digital
      begin
        Req.GravaInformacao(010,000,'CERTIF');
        Req.GravaInformacao(749,000,'8');  // Tipo de Pagamento como carteira digital
        Req.GravaInformacao(750,000,'1');  // Identificação da Carteira Digital como QR Code
      end

      else if (Req.Header = 'ADM') and (FTestePayGo = 31) then // Passo 31 - Operação bem sucedida com valor pré-definido
      begin
        Req.GravaInformacao(003,000,'1');
      end;

      FTestePayGo := 0;
    end;
  end;
end;

procedure TFormPrincipal.ACBrTEFD1AguardaResp(Arquivo: String;
  SegundosTimeOut: Integer; var Interromper: Boolean);
var
  Msg : String ;
begin
  if FCanceladoPeloOperador then
  begin
    FCanceladoPeloOperador := False;
    Interromper := True ;
    Exit;
  end;

  Msg := '' ;
  if (ACBrTEFD1.GPAtual in [gpCliSiTef, gpVeSPague, gpPayGoWeb]) then   // É TEF dedicado ?
   begin
     if (Arquivo = '23') and ACBrTEFD1.TecladoBloqueado then  // Está aguardando Pin-Pad ?
     begin
       // Desbloqueia o Teclado
       ACBrTEFD1.BloquearMouseTeclado(False);
       // Ajusta Interface para Espera do TEF, com opçao de cancelamento pelo Operador
       StatusVenda := stsAguardandoTEF;

       Msg := 'Aguardando Resposta do Pinpad.  Pressione <ESC> para Cancelar';
       FCanceladoPeloOperador := False;
     end;
   end
  else if FTempoDeEspera <> SegundosTimeOut then
  begin
     Msg := 'Aguardando: '+Arquivo+' '+IntToStr(SegundosTimeOut) ;
     FTempoDeEspera := SegundosTimeOut;
  end;

  if Msg <> '' then
    AdicionarLinhaLog(Msg);

  Application.ProcessMessages;
end;

procedure TFormPrincipal.ACBrTEFD1BloqueiaMouseTeclado(Bloqueia: Boolean; var Tratado: Boolean);
begin
  Self.Enabled := not Bloqueia ;
  AdicionarLinhaLog('BloqueiaMouseTeclado = '+IfThen(Bloqueia,'SIM', 'NAO'));
  Tratado := False ;  { Deixa executar o código de Bloqueio do ACBrTEFD }
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECFAbreVinculado(COO,
  IndiceECF: String; Valor: Double; var RetornoECF: Integer);
begin
  AdicionarLinhaLog( 'ACBrTEFD1ComandaECFAbreVinculado, COO:'+COO+
     ' IndiceECF: '+IndiceECF+' Valor: '+FormatFloatBr(Valor) ) ;
  AdicionarLinhaImpressao('</zera>');
  AdicionarLinhaImpressao('</linha_dupla>');
  RetornoECF := 1;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECFImprimeVia(
  TipoRelatorio: TACBrTEFDTipoRelatorio; Via: Integer;
  ImagemComprovante: TStringList; var RetornoECF: Integer);
begin
  AdicionarLinhaLog( 'ACBrTEFD1ComandaECFImprimeVia: '+IntToStr(Via) );
  AdicionarLinhaImpressao( ImagemComprovante.Text );
  RetornoECF := 1 ;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECFPagamento(IndiceECF: String;
  Valor: Double; var RetornoECF: Integer);
begin
  AdicionarLinhaLog('ACBrTEFD1ComandaECFPagamento, IndiceECF: '+IndiceECF+' Valor: '+FormatFloatBr(Valor));
  RetornoECF := 1;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECFSubtotaliza(DescAcre: Double;
  var RetornoECF: Integer);
begin
  AdicionarLinhaLog('ACBrTEFD1ComandaECFSubtotaliza: DescAcre: ' + FormatFloatBr(DescAcre));
  if StatusVenda = stsIniciada then
    StatusVenda := stsEmPagamento;
end;

procedure TFormPrincipal.ACBrTEFD1GravarLog(const GP: TACBrTEFDTipo;
  ALogLine: String; var Tratado: Boolean);
begin
  AdicionarLinhaLog(ALogLine);
  Tratado := False;
end;

procedure TFormPrincipal.ACBrTEFD1InfoECF(Operacao: TACBrTEFDInfoECF;
  var RetornoECF: String);
begin
   //try
   //   if not ACBrECF1.Ativo then
   //      ACBrECF1.Ativar ;
   //except
   //   { Para CliSiTEF ou V&SPague aplique o IF abaixo em sua aplicação, que
   //     permite saber se o Cupom foi concluido mesmo com o ECF desligado }
   //
   //   if (not ACBrTEFD1.TEF.Inicializado) and   { Está na inicialização ? }
   //      (Operacao = ineEstadoECF) and          { Quer Saber o estado do ECF ? (mas se chegou aqui é pq o ECF já está com problemas) }
   //      (ACBrTEFD1.GPAtual in [gpCliSiTef,gpVeSPague]) then
   //   begin
   //      { Leia o último Documento Gravado no seu Banco de Dados, e verifique
   //        se o Cupom já foi finalizado,ou se já foi aberto um CCD ou Gerencial...
   //        Exemplo:
   //
   //        Documento.Le(0);
   //
   //        if (Documento.Finalizado) or (pos(Documento.Denominacao,'CC|RG') > 0) then
   //           RetornoECF := 'R'
   //        else
   //           RetornoECF := 'O' ;
   //      }
   //
   //      //RetornoECF := 'O';    // Executará CancelarTransacoesPendentes;
   //      RetornoECF := 'R';    // Executará ConfirmarESolicitarImpressaoTransacoesPendentes;
   //      exit ;
   //   end ;
   //
   //   raise ;
   //end;

   case Operacao of
     ineSubTotal :
       RetornoECF := FloatToStr( Venda.TotalVenda ) ;

     ineTotalAPagar :
     begin
       // ACBrTEFD1.RespostasPendentes.TotalPago  deve ser subtraido, pois ACBrTEFD já subtrai o total dos pagamentos em TEF internamente
       RetornoECF := FloatToStr( RoundTo(Double(Venda.TotalPago - ACBrTEFD1.RespostasPendentes.TotalPago), -2) );
     end;

     ineEstadoECF :
       begin
         //"L" - Livre
         //"V" - Venda de Itens
         //"P" - Pagamento (ou SubTotal efetuado)
         //"C" ou "R" - CDC ou Cupom Vinculado
         //"G" ou "R" - Relatório Gerencial
         //"N" - Recebimento Não Fiscal
         //"O" - Outro
         Case StatusVenda of
           stsIniciada:
             RetornoECF := 'V' ;
           stsEmPagamento:
             RetornoECF := 'P' ;
           stsLivre, stsFinalizada, stsCancelada, stsAguardandoTEF, stsOperacaoTEF:
             RetornoECF := 'L' ;
         else
           RetornoECF := 'O' ;
         end;
       end;
   end;
end;

procedure TFormPrincipal.btAdministrativoClick(Sender: TObject);
begin
  AdicionarLinhaLog('- btAdministrativoClick');
  IniciarOperacao;
  StatusVenda := stsOperacaoTEF;
  try
    // Exemplo de como modificar a Operação Administrativa, padrão //
    //if (ACBrTEFD1.GPAtual = gpPayGoWeb) then
    //  ACBrTEFD1.TEFPayGoWeb.OperacaoADM := PWOPER_SALESUMMARY;
    ACBrTEFD1.ADM;
  finally
    StatusVenda := stsFinalizada;
    //if (ACBrTEFD1.GPAtual = gpPayGoWeb) then
    //  ACBrTEFD1.TEFPayGoWeb.OperacaoADM := PWOPER_ADMIN
  end;
end;

procedure TFormPrincipal.btSalvarParametrosClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

function TFormPrincipal.GetNomeArquivoConfiguracao: String;
begin
  Result := ChangeFileExt( Application.ExeName,'.ini' ) ;
end;

function TFormPrincipal.GetNomeArquivoVenda: String;
begin
  Result := ApplicationPath+'Venda.ini' ;
end;

function TFormPrincipal.GetStatusVenda: TStatusVenda;
begin
  Result := Venda.Status;
end;

procedure TFormPrincipal.LerConfiguracao;
Var
  INI : TIniFile ;
begin
  AdicionarLinhaLog('- LerConfiguracao');

  INI := TIniFile.Create(NomeArquivoConfiguracao);
  try
    cbxModeloPosPrinter.ItemIndex := INI.ReadInteger('PosPrinter', 'Modelo', 1);
    cbxPorta.Text := INI.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta);
    cbxPagCodigo.ItemIndex := INI.ReadInteger('PosPrinter','PaginaDeCodigo', 2);
    ACBrPosPrinter1.Device.ParamsString := INI.ReadString('PosPrinter','ParamsString','');
    seColunas.Value := INI.ReadInteger('PosPrinter','Colunas', 40);
    seEspLinhas.Value := INI.ReadInteger('PosPrinter','EspacoLinhas',ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value := INI.ReadInteger('PosPrinter','LinhasEntreCupons',ACBrPosPrinter1.LinhasEntreCupons);

    cbxGP.ItemIndex := INI.ReadInteger('TEF', 'GP', 0);
    edLog.Text := INI.ReadString('TEF', 'Log', '');
    seMaxCartoes.Value := INI.ReadInteger('TEF', 'MaxCartoes', 5);
    seTrocoMaximo.Value := INI.ReadFloat('TEF', 'TrocoMaximo', 0);
    cbImprimirViaReduzida.Checked := INI.ReadBool('TEF', 'ImprimirViaReduzida', False);
    cbMultiplosCartoes.Checked := INI.ReadBool('TEF', 'MultiplosCartoes', True);
    cbConfirmarAntesComprovantes.Checked := INI.ReadBool('TEF', 'ConfirmarAntesComprovantes', True);
    cbxQRCode.ItemIndex := INI.ReadInteger('TEF', 'QRCode', 0);
    cbSuportaDesconto.Checked := INI.ReadBool('TEF', 'SuportaDesconto', True);
    cbSuportaSaque.Checked := INI.ReadBool('TEF', 'SuportaSaque', True);
    cbSuportaReajusteValor.Checked := INI.ReadBool('TEF', 'SuportaReajusteValor', True);

    edRazaoSocial.Text := INI.ReadString('Aplicacao', 'RazaoSocial', edRazaoSocial.Text);
    edRegistro.Text := INI.ReadString('Aplicacao', 'Registro', edRegistro.Text);
    edAplicacaoNome.Text := INI.ReadString('Aplicacao', 'Nome', edAplicacaoNome.Text);
    edAplicacaoVersao.Text := INI.ReadString('Aplicacao', 'Versao', edAplicacaoVersao.Text);
  finally
     INI.Free ;
  end ;
end;

procedure TFormPrincipal.GravarConfiguracao;
Var
  INI : TIniFile ;
begin
  AdicionarLinhaLog('- GravarConfiguracao');

  INI := TIniFile.Create(NomeArquivoConfiguracao);
  try
    INI.WriteInteger('PosPrinter', 'Modelo', cbxModeloPosPrinter.ItemIndex);
    INI.WriteString('PosPrinter','Porta', cbxPorta.Text);
    INI.WriteInteger('PosPrinter','PaginaDeCodigo', cbxPagCodigo.ItemIndex);
    INI.WriteString('PosPrinter','ParamsString', ACBrPosPrinter1.Device.ParamsString);
    INI.WriteInteger('PosPrinter','Colunas', seColunas.Value);
    INI.WriteInteger('PosPrinter','EspacoLinhas', seEspLinhas.Value);
    INI.WriteInteger('PosPrinter','LinhasEntreCupons', seLinhasPular.Value);

    INI.WriteInteger('TEF', 'GP', cbxGP.ItemIndex);
    INI.WriteString('TEF', 'Log', edLog.Text);
    INI.WriteInteger('TEF', 'MaxCartoes', seMaxCartoes.Value);
    INI.WriteFloat('TEF', 'TrocoMaximo', seTrocoMaximo.Value);
    INI.WriteBool('TEF', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    INI.WriteBool('TEF', 'MultiplosCartoes', cbMultiplosCartoes.Checked);
    INI.WriteBool('TEF', 'ConfirmarAntesComprovantes', cbConfirmarAntesComprovantes.Checked);
    INI.WriteInteger('TEF', 'QRCode', cbxQRCode.ItemIndex);
    INI.WriteBool('TEF', 'SuportaDesconto', cbSuportaDesconto.Checked);
    INI.WriteBool('TEF', 'SuportaSaque', cbSuportaSaque.Checked);
    INI.WriteBool('TEF', 'SuportaReajusteValor', cbSuportaReajusteValor.Checked);

    INI.WriteString('Aplicacao', 'RazaoSocial', edRazaoSocial.Text);
    INI.WriteString('Aplicacao', 'Registro', edRegistro.Text);
    INI.WriteString('Aplicacao', 'Nome', edAplicacaoNome.Text);
    INI.WriteString('Aplicacao', 'Versao', edAplicacaoVersao.Text);
  finally
     INI.Free ;
  end ;
end;

procedure TFormPrincipal.IrParaOperacaoTEF;
begin
  AdicionarLinhaLog('- IrParaOperacaoTEF');
  Ativar;
  btMudaPagina.Caption := 'Configuração';
  btMudaPagina.ImageIndex := 0;
  pgPrincipal.ActivePage := tsOperacao;
  btImprimir.Enabled := ACBrPosPrinter1.Ativo;
  cbEnviarImpressora.Enabled := ACBrPosPrinter1.Ativo;
  cbEnviarImpressora.Checked := cbEnviarImpressora.Enabled;
  StatusVenda := stsLivre;
end;

procedure TFormPrincipal.IrParaConfiguracao;
begin
  AdicionarLinhaLog('- IrParaConfiguracao');
  Desativar;
  btMudaPagina.Caption := 'Operação TEF';
  btMudaPagina.ImageIndex := 1;
  pgPrincipal.ActivePage := tsConfiguracao;
end;

procedure TFormPrincipal.AdicionarLinhaLog(AMensagem: String);
begin
  mLog.Lines.Add(AMensagem);
end;

procedure TFormPrincipal.AdicionarLinhaImpressao(ALinha: String);
begin
  mImpressao.Lines.Add(ALinha);
  if ACBrPosPrinter1.Ativo then
    ACBrPosPrinter1.Imprimir(ALinha);
end;

procedure TFormPrincipal.SetTipoBotaoOperacao(AValue: TTipoBotaoOperacao);
var
  MsgOperacao: String;
begin
  if FTipoBotaoOperacao = AValue then Exit;

  MsgOperacao := '';

  case AValue of
    bopCancelarVenda, bopCancelarEsperaTEF:
      MsgOperacao := 'Cancelar';

    bopLiberarCaixa:
      MsgOperacao := 'Liberar';
  end;

  FTipoBotaoOperacao := AValue;

  btOperacao.Visible := (MsgOperacao <> '');
  btOperacao.Caption := 'ESC - '+MsgOperacao;
end;

procedure TFormPrincipal.SetStatusVenda(AValue: TStatusVenda);
var
  MsgStatus: String;
begin
  if StatusVenda = AValue then
    Exit;

  AdicionarLinhaLog('- StatusOperacao: '+GetEnumName(TypeInfo(TStatusVenda), integer(AValue) ));

  gbTotaisVenda.Enabled := (AValue in [stsLivre, stsIniciada]);
  gbPagamentos.Enabled := (AValue = stsEmPagamento);
  btAdministrativo.Enabled := (AValue = stsLivre);
  btObterCPF.Enabled := btAdministrativo.Enabled;
  pImpressao.Enabled := (AValue in [stsLivre, stsFinalizada, stsCancelada]);
  btEfetuarPagamentos.Enabled := (AValue = stsIniciada);
  lNumOperacao.Visible := (AValue <> stsLivre);

  case AValue of
    stsIniciada:
    begin
      MsgStatus := 'EM VENDA';
      TipoBotaoOperacao := bopCancelarVenda;
      AtualizarVendaNaInterface;
    end;

    stsEmPagamento:
    begin
      MsgStatus := 'EM PAGAMENTO';
      TipoBotaoOperacao := bopCancelarVenda;
      sgPagamentos.SetFocus;
    end;

    stsFinalizada:
    begin
      MsgStatus := 'FINALIZADA';
      TipoBotaoOperacao := bopLiberarCaixa;
    end;

    stsCancelada:
    begin
      MsgStatus := 'CANCELADA';
      TipoBotaoOperacao := bopLiberarCaixa;
    end;

    stsAguardandoTEF:
    begin
      MsgStatus := 'TRANSACAO TEF';
      TipoBotaoOperacao := bopCancelarEsperaTEF;
    end;

    stsOperacaoTEF:
    begin
      MsgStatus := 'OPERAÇÃO TEF';
      TipoBotaoOperacao := bopNaoExibir;
      AtualizarVendaNaInterface;
    end;

  else
    MsgStatus := 'CAIXA LIVRE';
    TipoBotaoOperacao := bopNaoExibir;
    AtualizarCaixaLivreNaInterface;
    if pgPrincipal.ActivePage = tsOperacao then
      seValorInicialVenda.SetFocus;
  end;

  pStatus.Caption := MsgStatus;
  Venda.Status := AValue;
  if (AValue <> stsLivre) then
    Venda.Gravar;
end;

procedure TFormPrincipal.TratarException(Sender: TObject; E: Exception);
begin
  AdicionarLinhaLog('');
  AdicionarLinhaLog('***************' + E.ClassName + '***************');
  AdicionarLinhaLog(E.Message);
  AdicionarLinhaLog('');
  //MessageDlg(E.Message, mtError, [mbOK], 0);
end;

procedure TFormPrincipal.btSerialClick(Sender: TObject);
var
  frConfiguraSerial: TfrConfiguraSerial;
begin
  AdicionarLinhaLog('- btSerialClick');
  frConfiguraSerial := TfrConfiguraSerial.Create(self);
  try
    frConfiguraSerial.Device.Porta        := ACBrPosPrinter1.Device.Porta ;
    frConfiguraSerial.cmbPortaSerial.Text := cbxPorta.Text ;
    frConfiguraSerial.Device.ParamsString := ACBrPosPrinter1.Device.ParamsString ;

    if frConfiguraSerial.ShowModal = mrOk then
    begin
      cbxPorta.Text := frConfiguraSerial.cmbPortaSerial.Text ;
      ACBrPosPrinter1.Device.ParamsString := frConfiguraSerial.Device.ParamsString ;
    end ;
  finally
    FreeAndNil( frConfiguraSerial ) ;
  end ;
end;

procedure TFormPrincipal.btTestarPosPrinterClick(Sender: TObject);
var
  SL: TStringList;
begin
  AdicionarLinhaLog('- btTestarPosPrinterClick');
  try
    AtivarPosPrinter;

    SL := TStringList.Create;
    try
      SL.Add('</zera>');
      SL.Add('</linha_dupla>');
      SL.Add('FONTE NORMAL: '+IntToStr(ACBrPosPrinter1.ColunasFonteNormal)+' Colunas');
      SL.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteNormal));
      SL.Add('<e>EXPANDIDO: '+IntToStr(ACBrPosPrinter1.ColunasFonteExpandida)+' Colunas');
      SL.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteExpandida));
      SL.Add('</e><c>CONDENSADO: '+IntToStr(ACBrPosPrinter1.ColunasFonteCondensada)+' Colunas');
      SL.Add(LeftStr('....+....1....+....2....+....3....+....4....+....5....+....6....+....7....+....8', ACBrPosPrinter1.ColunasFonteCondensada));
      SL.Add('</c><n>FONTE NEGRITO</N>');
      SL.Add('<in>FONTE INVERTIDA</in>');
      SL.Add('<S>FONTE SUBLINHADA</s>');
      SL.Add('<i>FONTE ITALICO</i>');
      SL.Add('FONTE NORMAL');
      SL.Add('');
      SL.Add('TESTE DE ACENTOS. ÁÉÍÓÚáéíóú');
      SL.Add('');
      SL.Add('</corte_total>');

      cbEnviarImpressora.Checked := True;
      AdicionarLinhaImpressao(SL.Text);
    finally
       SL.Free;
    end;
  except
    On E: Exception do
    begin
      MessageDlg('Falha ao ativar a Impressora' + sLineBreak + E.Message, mtError, [mbOK], 0);
    end;
  end
end;

procedure TFormPrincipal.btTestarTEFClick(Sender: TObject);
var
  NomeTEF: String;
begin
  GravarConfiguracao;
  NomeTEF := GetEnumName(TypeInfo(TACBrTEFDTipo), cbxGP.ItemIndex);
  AdicionarLinhaLog('- btTestarTEFClick: '+NomeTEF);
  try
    try
      AtivarTEF;
      ACBrTEFD1.ATV;
      MessageDlg(Format('TEF %S ATIVO', [NomeTEF]), mtInformation, [mbOK], 0);
    finally
      StatusVenda := stsLivre;
    end;
  except
    On E: Exception do
    begin
      MessageDlg(Format('Falha ao ativar TEF %S' + sLineBreak + E.Message, [NomeTEF]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TFormPrincipal.btObterCPFClick(Sender: TObject);
var
  Saida: String;
begin
  Saida := '';
  if ACBrTEFD1.GPAtual = gpCliSiTef then
  begin
    // SiTef precisa de parâmetros extras, vamos informar...
    ACBrTEFD1.TEFCliSiTef.PinPadIdentificador := '01.123.456/0001-07';
    ACBrTEFD1.TEFCliSiTef.PinPadChaveAcesso := 'Chave Fornecida pela Software Express, exclusiva para o Identificador acima';
  end;

  ACBrTEFD1.CDP('F', Saida);  // F=CPF
  if (Saida <> '') then
    ShowMessage(Saida)
  else
    ShowMessage('Falha ao Obter CPF no PinPad');
end;

procedure TFormPrincipal.cbEnviarImpressoraChange(Sender: TObject);
begin
  btImprimir.Enabled := ACBrPosPrinter1.Ativo and (not cbEnviarImpressora.Checked);
end;

procedure TFormPrincipal.CliSiTefExibeMenu(Titulo: String; Opcoes: TStringList;
  var ItemSelecionado: Integer; var VoltarMenu: Boolean);
Var
  MR: TModalResult ;
  FormMenuTEF: TFormMenuTEF;
begin
  FormMenuTEF := TFormMenuTEF.Create(self);
  try
    FormMenuTEF.Titulo := Titulo;
    FormMenuTEF.Opcoes := Opcoes;

    MR := FormMenuTEF.ShowModal ;

    VoltarMenu := (MR = mrRetry) ;

    if (MR = mrOK) then
      ItemSelecionado := FormMenuTEF.ItemSelecionado;
  finally
    FormMenuTEF.Free;
  end;
end;

procedure TFormPrincipal.CliSiTefObtemCampo(Titulo: String; TamanhoMinimo,
  TamanhoMaximo: Integer; TipoCampo: Integer;
  Operacao: TACBrTEFDCliSiTefOperacaoCampo; var Resposta: AnsiString;
  var Digitado: Boolean; var VoltarMenu: Boolean);
Var
  MR: TModalResult ;
  FormObtemCampo: TFormObtemCampo;
begin
  FormObtemCampo := TFormObtemCampo.Create(self);
  try
    FormObtemCampo.Titulo := Titulo;
    FormObtemCampo.TamanhoMaximo := TamanhoMaximo;
    FormObtemCampo.TamanhoMinimo := TamanhoMinimo;
    FormObtemCampo.Resposta := Resposta; { Para usar Valores Previamente informados }

    case Operacao of
      tcDouble:
        FormObtemCampo.TipoCampo := tcoCurrency;
      tcCMC7, tcBarCode:
        FormObtemCampo.TipoCampo := tcoNumeric;
    else
      FormObtemCampo.TipoCampo := tcoString;
    end;

    if (Operacao = tcStringMask) then
      FormObtemCampo.Ocultar := True;

    MR := FormObtemCampo.ShowModal ;

    Digitado := (MR = mrOK) ;
    VoltarMenu := (MR = mrRetry) ;

    if Digitado then
       Resposta := FormObtemCampo.Resposta;
  finally
    FormObtemCampo.Free;
  end;
end;

procedure TFormPrincipal.seValorInicialVendaChange(Sender: TObject);
begin
  seTotalDesconto.MaxValue := seValorInicialVenda.Value;
  if (seValorInicialVenda.Value <> 0) and (StatusVenda = stsLivre) then
  begin
    IniciarOperacao;
    Venda.ValorInicial := seValorInicialVenda.Value;
    StatusVenda := stsIniciada;
  end
  else
  begin
    Venda.ValorInicial := seValorInicialVenda.Value;
    AtualizarTotaisVendaNaInterface;
  end;
end;

procedure TFormPrincipal.AtualizarCaixaLivreNaInterface;
begin
  AdicionarLinhaLog('- AtualizarCaixaLivreNaInterface');
  LimparMensagensTEF;
  mImpressao.Clear;
  cbTestePayGo.ItemIndex := 0;
  Venda.Clear;
  AtualizarVendaNaInterface;
  FCanceladoPeloOperador := False;
  FTempoDeEspera := 0;
end;

procedure TFormPrincipal.IniciarOperacao;
var
  ProxVenda: Integer;
begin
  Venda.Ler;
  ProxVenda := Venda.NumOperacao+1;

  Venda.Clear;
  Venda.NumOperacao := ProxVenda;
  Venda.DHInicio := Now;
  FCanceladoPeloOperador := False;
  FTempoDeEspera := 0;
end;

procedure TFormPrincipal.AdicionarPagamento(const Indice: String; AValor: Double
  );
var
  Ok, TemTEF: Boolean;
  ReajusteValor: Double;
  UltResp: TACBrTEFResp;

  procedure InformarParametrosCartaoCredito;
  begin
    // Instruindo CRT a apenas transações de Crédito...
    // Isso é Opcional, e está aqui apenas para demonstração
    if (ACBrTEFD1.GPAtual = gpPayGoWeb) then
    begin
      ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_CARDTYPE]:='01'; //01: crédito;  3 = 1 crédito + 2 débito
      //ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_PAYMNTTYPE]:='1'; //01: crédito
      //ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_FINTYPE]:='1'; //01: à vista, 2: parcelado
      //ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_AUTHSYST]:='REDE';
      //ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_INSTALLMENTS]:='3';
    end
    else if (ACBrTEFD1.GPAtual = gpCliSiTef) then
      ACBrTEFD1.TEFCliSiTef.OperacaoCRT := 3;
  end;

  procedure InformarParametrosCartaoDebito;
  begin
    // Instruindo CRT a apenas transações de Débito
    if (ACBrTEFD1.GPAtual = gpPayGoWeb) then
    begin
      ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_CARDTYPE]:='02'; //02: débito
      //ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_FINTYPE]:='01'; //01: à vista
    end
    else if (ACBrTEFD1.GPAtual = gpCliSiTef) then
      ACBrTEFD1.TEFCliSiTef.OperacaoCRT := 2;
  end;

  procedure InformarParametrosCarteiraDigital;
  begin
    // Instruindo CRT a apenas transações de Débito
    if (ACBrTEFD1.GPAtual = gpPayGoWeb) then
      ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_PAYMNTTYPE]:='8' // Modalidade de pagamento:   1: cartão   2: dinheiro   4: cheque   8: carteira virtual
    else if (ACBrTEFD1.GPAtual = gpCliSiTef) then
    begin
      case cbxQRCode.ItemIndex of
        3,4:ACBrTEFD1.TEFCliSiTef.Restricoes  := '{DevolveStringQRCode=1}'; {No CheckOut}
      else
        ACBrTEFD1.TEFCliSiTef.Restricoes := '{DevolveStringQRCode=0}'; {No PinPad}
      end;
      ACBrTEFD1.TEFCliSiTef.OperacaoCRT := 122;
    end;
  end;

  procedure InformarParametrosVoucher;
  begin
    // Instruindo CRT a apenas transações de Débito
    if (ACBrTEFD1.GPAtual = gpPayGoWeb) then
    begin
      ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_PAYMNTTYPE]:='1'; // Modalidade de pagamento:   1: cartão   2: dinheiro   4: cheque   8: carteira virtual
      ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.ValueInfo[PWINFO_CARDTYPE]:='04'; // 1: crédito 2: débito 4: voucher/PAT 8: private label 16: frota 128: outros
    end;
  end;

begin
  Ok := False;
  TemTEF := False;

  FIndicePagto := Indice;
  if (ACBrTEFD1.GPAtual = gpPayGoWeb) then
    ACBrTEFD1.TEFPayGoWeb.ParametrosAdicionais.Clear
  else if (ACBrTEFD1.GPAtual = gpCliSiTef) then
    ACBrTEFD1.TEFCliSiTef.OperacaoCRT := 0;

  try
    // ** NOTA **
    // Usa '01' como Indice de Forma de Pagamento de ECF, para todas as operações TEF,
    // para evitar que o ACBrTEFD tente separar os Comprovantes por Forma de Pagamento

    if (Indice = '02') then          // 02-CHEQUE
    begin
      Ok := ACBrTEFD1.CHQ(AValor, '01');
      TemTEF := True;
    end

    else if (Indice = '03') then     // 03-CREDITO
    begin
      InformarParametrosCartaoCredito;
      Ok := ACBrTEFD1.CRT(AValor, '01');
      TemTEF := True;
    end

    else if (Indice = '04') then     // 04-DEBITO
    begin
      InformarParametrosCartaoDebito;
      Ok := ACBrTEFD1.CRT(AValor, '01');
      TemTEF := True;
    end

    else if (Indice = '05') then    // 05-CARTEIRA DIGITAL
    begin
      if (ACBrTEFD1.GPAtual = gpTefElgin) then
        Ok := ACBrTEFD1.TEFElgin.PIX(AValor, '01')
      else
      begin
        FTestePayGo := 27;
        InformarParametrosCarteiraDigital;
        Ok := ACBrTEFD1.CRT(AValor, '01');
        TemTEF := True;
      end;
    end

    else if (Indice = '06') then    // 05-VALE REFEICAO
    begin
      InformarParametrosVoucher;
      Ok := ACBrTEFD1.CRT(AValor, '01');
      TemTEF := True;
    end
    else
      Ok := True;                  // Pagamentos não TEF

  finally
    StatusVenda := stsEmPagamento;
  end;

  if Ok then
  begin
    with Venda.Pagamentos.New do
    begin
      TipoPagamento := Indice;
      ValorPago := AValor;

      if TemTEF then
      begin
        UltResp := ACBrTEFD1.RespostasPendentes[ACBrTEFD1.RespostasPendentes.Count-1];

        NSU := UltResp.NSU;
        Rede := UltResp.Rede;
        RedeCNPJ := UltResp.NFCeSAT.CNPJCredenciadora;

        // Calcula a Diferença do Valor Retornado pela Operação TEF do Valor que
        //   Informamos no CRT/CHQ
        ReajusteValor := RoundTo(Double(UltResp.ValorTotal - ValorPago), -2);

        Saque := UltResp.Saque;
        if (Saque > 0) then
        begin
          // Se houve Saque na operação TEF, devemos adicionar no ValorPago,
          //   para que o Saque conste como Troco
          ValorPago := ValorPago + Saque
        end
        else if ReajusteValor > 0 then
        begin
          // Se não é Saque, mas houve acréscimo no valor Retornado, devemos lançar
          //   o Reajuste como Acréscimo na venda
          Venda.TotalAcrescimo := Venda.TotalAcrescimo + ReajusteValor;
        end;

        Desconto := UltResp.Desconto;
        if Desconto > 0 then
        begin
          // Se houve Desconto na Operação TEF, devemos subtrair do ValorPago
          //   e lançar um Desconto no Total da Transacao
          ValorPago := ValorPago - Desconto;
          Venda.TotalDesconto := Venda.TotalDesconto + Desconto;
        end
        else if (ReajusteValor < 0) then
        begin
          // Se não é Desconto, mas houve redução no Valor Retornado, devemos
          //   considerar a redução no ValorPago, pois a Adquirente limitou o
          //   valor da Operação, a um máximo permitido... Deverá fechar o cupom,
          //   com outra forma de Pagamento
          ValorPago := ValorPago + ReajusteValor;
        end;
      end
      else
        Confirmada := True;
    end;

    AtualizarPagamentosVendaNaInterface;

    if (Venda.TotalPago >= Venda.TotalVenda) then
      FinalizarVenda;
  end;
end;

procedure TFormPrincipal.ExcluirPagamento(IndicePagto: Integer);
var
  AResp: TACBrTEFResp;
  Cancelada: Boolean;
begin
  if (IndicePagto < 0) or (IndicePagto >=  Venda.Pagamentos.Count) then
    raise Exception.CreateFmt( 'Indice de pagamento [%d] inválido', [IndicePagto]);

  if Venda.Pagamentos[IndicePagto].Cancelada then
    raise Exception.CreateFmt( 'Pagamento [%d] já foi Cancelado', [IndicePagto]);

  Cancelada := False;
  AResp := AcharTransacaoTEFPendente(IndicePagto);
  if Assigned(AResp) then
  begin
    if AResp.CNFEnviado then
      raise Exception.CreateFmt( 'Pagamento TEF [%s] já foi Confirmado.'+sLineBreak+
                                 'Para cancelar o mesmo, cancele Toda a Operação',
                                 [AResp.NSU])
    else
    begin
      ACBrTEFD1.NCN( AResp.Rede,
                     AResp.NSU,
                     AResp.Finalizacao,
                     AResp.ValorTotal,
                     AResp.DocumentoVinculado);
      Cancelada := True;
    end;

    if Cancelada then
    begin
      if (AResp.ArqBackup <> '') and FileExists(AResp.ArqBackup) then
        DeleteFile( AResp.ArqBackup );

      ACBrTEFD1.RespostasPendentes.Remove(AResp);
    end;
  end
  else
  begin
    if (Venda.Pagamentos[IndicePagto].NSU <> '') then
      raise Exception.CreateFmt( 'Transação TEF [%s] não foi Localizada.',
                                 [Venda.Pagamentos[IndicePagto].NSU])
    else
      Cancelada := True;
  end;

  if Cancelada then
  begin
    Venda.Pagamentos[IndicePagto].Cancelada := True;
    AtualizarPagamentosVendaNaInterface;
  end;
end;

function TFormPrincipal.AcharTransacaoTEFPendente(IndicePagto: Integer
  ): TACBrTEFResp;
var
  i: Integer;
begin
  Result := Nil;
  if (IndicePagto < 0) and (IndicePagto >=  Venda.Pagamentos.Count) then
    Exit;

  i := 0;
  while (i < ACBrTEFD1.RespostasPendentes.Count) and (Result = Nil) do
  begin
    if (ACBrTEFD1.RespostasPendentes[i].Rede = Venda.Pagamentos[IndicePagto].Rede) and
       (ACBrTEFD1.RespostasPendentes[i].NSU = Venda.Pagamentos[IndicePagto].NSU) and
       (ACBrTEFD1.RespostasPendentes[i].ValorTotal = Venda.Pagamentos[IndicePagto].ValorPago) then
      Result := ACBrTEFD1.RespostasPendentes[i];

    Inc(i);
  end;
end;

procedure TFormPrincipal.CancelarVenda;
begin
  AdicionarLinhaLog('- CancelarVenda');
  // AQUI você deve cancelar a sua venda no Banco de Dados, desfazendo baixa de
  // estoque ou outras operações que ocorreram durante a venda.

  StatusVenda := stsCancelada;
  ACBrTEFD1.CancelarTransacoesPendentes;
end;

procedure TFormPrincipal.FinalizarVenda;
var
  SL: TStringList;
  i: Integer;
  DoctoFiscalOk: Boolean;
  MR: TModalResult;
begin
  try
    // AQUI você deve Chamar uma Rotina para Gerar e Transmitir o Documento Fiscal (NFCe ou SAT)
    DoctoFiscalOk := not cbSimularErroNoDoctoFiscal.Checked;
    while not DoctoFiscalOk do
    begin
      MR := MessageDlg( 'Falha no Documento Fiscal',
                        'Tentar Novamente ?', mtConfirmation,
                        [mbYes, mbNo, mbIgnore], 0);
      if (MR = mrIgnore) then
        cbSimularErroNoDoctoFiscal.Checked := False
      else if (MR <> mrYes) then
        raise Exception.Create('Erro no Documento Fiscal');

      // AQUI você deve Chamar uma Rotina para Gerar e Transmitir o Documento Fiscal (NFCe ou SAT)
      DoctoFiscalOk := not cbSimularErroNoDoctoFiscal.Checked;
    end;

    SL := TStringList.Create;
    try
      // Ao invés do relatório abaixo, você deve enviar a impressão de um DANFCE ou Extrato do SAT

      SL.Add(PadCenter( ' COMPROVANTE DE OPERAÇÃO ', ACBrPosPrinter1.Colunas, '-'));
      SL.Add('Número: <n>' + FormatFloat('000000',Venda.NumOperacao) + '</n>');
      SL.Add('Data/Hora: <n>' + FormatDateTimeBr(Venda.DHInicio) + '</n>');
      SL.Add('</linha_simples>');
      SL.Add('');
      SL.Add('Valor Inicial...: <n>' + FormatFloatBr(Venda.ValorInicial) + '</n>');
      SL.Add('Total Descontos.: <n>' + FormatFloatBr(Venda.TotalDesconto) + '</n>');
      SL.Add('Total Acréscimos: <n>' + FormatFloatBr(Venda.TotalAcrescimo) + '</n>');
      SL.Add('</linha_simples>');
      SL.Add('VALOR FINAL.....: <n>' + FormatFloatBr(Venda.TotalVenda) + '</n>');
      SL.Add('');
      SL.Add(PadCenter( ' Pagamentos ', ACBrPosPrinter1.Colunas, '-'));
      for i := 0 to Venda.Pagamentos.Count-1 do
      begin
        with Venda.Pagamentos[i] do
        begin
          if not Cancelada then
            SL.Add(PadSpace( TipoPagamento+' - '+DescricaoTipoPagamento(TipoPagamento)+'|'+
                             FormatFloatBr(ValorPago)+'|'+Rede, ACBrPosPrinter1.Colunas, '|') );
        end;
      end;
      SL.Add('</linha_simples>');

      SL.Add('Total Pago......: <n>' + FormatFloatBr(Venda.TotalPago) + '</n>');
      if (Venda.Troco > 0) then
        SL.Add('Troco...........: <n>' + FormatFloatBr(Venda.Troco) + '</n>');

      SL.Add('</linha_dupla>');
      SL.Add('</corte>');
      AdicionarLinhaImpressao(SL.Text);
    finally
      SL.Free;
    end;

    StatusVenda := stsFinalizada;
    ACBrTEFD1.ImprimirTransacoesPendentes();
  except
    CancelarVenda;
  end;
end;

procedure TFormPrincipal.VerificarTestePayGo;
var
  P: Integer;
  ATeste: String;
begin
  ATeste := cbTestePayGo.Text;
  P := pos('-',ATeste);
  ATeste := Trim(copy(ATeste, 1, P-1));
  FTestePayGo := StrToIntDef(ATeste, 0);
end;

procedure TFormPrincipal.AtualizarVendaNaInterface;
begin
  lNumOperacao.Caption := FormatFloat('000000',Venda.NumOperacao);
  seValorInicialVenda.OnChange := Nil;
  try
    seValorInicialVenda.Value := Venda.ValorInicial;
  finally
    seValorInicialVenda.OnChange := @seValorInicialVendaChange;
  end;
  AtualizarPagamentosVendaNaInterface;
end;

procedure TFormPrincipal.AtualizarTotaisVendaNaInterface;
begin
  seTotalDesconto.OnChange := Nil;
  seTotalAcrescimo.OnChange := Nil;
  try
    seTotalDesconto.Value := Venda.TotalDesconto;
    seTotalAcrescimo.Value := Venda.TotalAcrescimo;
    edTotalVenda.Text := FormatFloatBr(Venda.TotalVenda);
    edTotalPago.Text := FormatFloatBr(Venda.TotalPago);
    edTroco.Text := FormatFloatBr(max(Double(Venda.Troco),0));
  finally
    seTotalDesconto.OnChange := @seTotalDescontoChange;
    seTotalAcrescimo.OnChange := @seTotalAcrescimoChange;
  end;
end;

procedure TFormPrincipal.AtualizarPagamentosVendaNaInterface;
var
  i, ARow: Integer;
  AResp: TACBrTEFResp;
begin
  sgPagamentos.RowCount := 1;
  for i := 0 to Venda.Pagamentos.Count-1 do
  begin
    ARow := sgPagamentos.RowCount;
    sgPagamentos.RowCount := sgPagamentos.RowCount + 1;

    with Venda.Pagamentos[i] do
    begin
      if not Cancelada then
      begin
        AResp := AcharTransacaoTEFPendente(i);
        if Assigned(AResp) then
          Confirmada := AResp.CNFEnviado;
      end;

      sgPagamentos.Cells[0, ARow] := FormatFloat('000', ARow);
      sgPagamentos.Cells[1, ARow] := TipoPagamento + ' - ' + DescricaoTipoPagamento(TipoPagamento);
      sgPagamentos.Cells[2, ARow] := FormatFloatBr(ValorPago);
      sgPagamentos.Cells[3, ARow] := NSU;
      sgPagamentos.Cells[4, ARow] := Rede;
      sgPagamentos.Cells[5, ARow] := ifthen(Cancelada, 'Cancelada', ifthen(Confirmada, 'Confirmada', 'Pendente'));
      sgPagamentos.Cells[6, ARow] := RedeCNPJ;
    end;
  end;

  AtualizarTotaisVendaNaInterface;
end;

procedure TFormPrincipal.MensagemTEF(const MsgOperador, MsgCliente: String);
begin
  if (MsgOperador <> '') then
    lMensagemOperador.Caption := MsgOperador;

  if (MsgCliente <> '') then
    lMensagemCliente.Caption := MsgCliente;

  pMensagemOperador.Visible := (Trim(lMensagemOperador.Caption) <> '');
  pMensagemCliente.Visible := (Trim(lMensagemCliente.Caption) <> '');
  pMensagem.Visible := pMensagemOperador.Visible or pMensagemCliente.Visible;
  Application.ProcessMessages;
end;

procedure TFormPrincipal.LimparMensagensTEF;
begin
  MensagemTEF(' ',' ');
end;

procedure TFormPrincipal.ExibirPainelQRCode;
begin
  if pQRCode.Visible then
    Exit;

  mImpressao.Visible := False;
  lSaidaImpressao.Visible := False;
  pQRCode.Visible := True;
  pQRCode.Align := alClient;
end;

procedure TFormPrincipal.OcultatPainelQRCode;
begin
  if not pQRCode.Visible then
    Exit;

  pQRCode.Visible := False;
  mImpressao.Visible := True;
  lSaidaImpressao.Visible := True;
end;

procedure TFormPrincipal.ConfigurarTEF;
begin
  AdicionarLinhaLog('- ConfigurarTEF');
  ACBrTEFD1.ArqLOG := edLog.Text;
  ACBrTEFD1.TrocoMaximo := seTrocoMaximo.Value;
  ACBrTEFD1.ImprimirViaClienteReduzida := cbImprimirViaReduzida.Checked;
  ACBrTEFD1.MultiplosCartoes := cbMultiplosCartoes.Checked;
  ACBrTEFD1.ConfirmarAntesDosComprovantes := cbConfirmarAntesComprovantes.Checked;
  ACBrTEFD1.NumeroMaximoCartoes := seMaxCartoes.Value;
  ACBrTEFD1.SuportaDesconto := cbSuportaDesconto.Checked;
  ACBrTEFD1.SuportaSaque := cbSuportaSaque.Checked;

  ACBrTEFD1.Identificacao.SoftwareHouse := edRazaoSocial.Text;
  ACBrTEFD1.Identificacao.RegistroCertificacao := edRegistro.Text;
  ACBrTEFD1.Identificacao.NomeAplicacao := edAplicacaoNome.Text;
  ACBrTEFD1.Identificacao.VersaoAplicacao := edAplicacaoVersao.Text;

  ACBrTEFD1.TEFPayGo.SuportaReajusteValor := cbSuportaReajusteValor.Checked;
  ACBrTEFD1.TEFPayGo.SuportaNSUEstendido := True;
  ACBrTEFD1.TEFPayGo.SuportaViasDiferenciadas := True;

  case cbxQRCode.ItemIndex of
    0: ACBrTEFD1.TEFPayGoWeb.ExibicaoQRCode := qreNaoSuportado;
    2: ACBrTEFD1.TEFPayGoWeb.ExibicaoQRCode := qreExibirNoPinPad;
    3, 4: ACBrTEFD1.TEFPayGoWeb.ExibicaoQRCode := qreExibirNoCheckOut;
  else
    ACBrTEFD1.TEFPayGoWeb.ExibicaoQRCode := qreAuto;
  end;

  //ACBrTEFD1.TEFPayGoWeb.DiretorioTrabalho := 'C:\PAYGOWEB';
  //ACBrTEFD1.PathBackup := 'C:\TEF\TER01';
  //ACBrTEFD1.TEFPayGoWeb.PathDLL := 'C:\DLLs';

  // Configurações abaixo são obrigatórios, para funcionamento de Não Fiscal //
  ACBrTEFD1.AutoEfetuarPagamento := False;
  ACBrTEFD1.AutoFinalizarCupom := False;
end;

procedure TFormPrincipal.AtivarTEF;
begin
  AdicionarLinhaLog('- AtivarTEF');
  ConfigurarTEF;
  ACBrTEFD1.Inicializar(TACBrTEFDTipo(cbxGP.ItemIndex));
end;

procedure TFormPrincipal.ConfigurarPosPrinter;
begin
  AdicionarLinhaLog('- ConfigurarPosPrinter');
  ACBrPosPrinter1.Desativar;
  ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo( cbxModeloPosPrinter.ItemIndex );
  ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo( cbxPagCodigo.ItemIndex );
  ACBrPosPrinter1.Porta := cbxPorta.Text;
  ACBrPosPrinter1.ColunasFonteNormal := seColunas.Value;
  ACBrPosPrinter1.LinhasEntreCupons := seLinhasPular.Value;
  ACBrPosPrinter1.EspacoEntreLinhas := seEspLinhas.Value;
end;

procedure TFormPrincipal.AtivarPosPrinter;
begin
  AdicionarLinhaLog('- AtivarPosPrinter');
  ConfigurarPosPrinter;
  if (ACBrPosPrinter1.Porta <> '') then
    ACBrPosPrinter1.Ativar
  else
    raise Exception.Create('Porta não definida');
end;

procedure TFormPrincipal.Ativar;
begin
  AdicionarLinhaLog('- Ativar');
  GravarConfiguracao;
  try
    AtivarPosPrinter;
  except
    On E: Exception do
    begin
      TratarException(nil, E);
    end;
  end;
  AtivarTEF;
end;

procedure TFormPrincipal.Desativar;
begin
  AdicionarLinhaLog('- Desativar');
  ACBrPosPrinter1.Desativar;
  ACBrTEFD1.DesInicializar(TACBrTEFDTipo(cbxGP.ItemIndex));
end;

end.

