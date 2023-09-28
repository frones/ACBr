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
  Spin, Buttons, DBCtrls, ExtCtrls, Grids,
  uVendaClass,
  ACBrPosPrinter, ACBrTEFComum, ACBrTEFAPI, ACBrBase, ACBrTEFAPIComum;

type

  TTipoBotaoOperacao = (bopNaoExibir, bopCancelarVenda, bopLiberarCaixa, bopCancelarEsperaTEF);

  { TFormPrincipal }

  TFormPrincipal = class(TForm)
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrTEFAPI1: TACBrTEFAPI;
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
    btProcuraImpressoras: TSpeedButton;
    btSalvarParametros: TBitBtn;
    btSerial: TSpeedButton;
    btTestarPosPrinter: TBitBtn;
    btTestarTEF: TBitBtn;
    btObterCPF: TButton;
    cbConfirmarAutomaticamente: TCheckBox;
    cbAutoAtendimento: TCheckBox;
    cbImprimirViaReduzida: TCheckBox;
    cbSimularErroNoDoctoFiscal: TCheckBox;
    cbSuportaDesconto: TCheckBox;
    cbSuportaSaque: TCheckBox;
    cbxGP: TComboBox;
    cbxTransacaoPendente: TComboBox;
    cbxModeloPosPrinter: TComboBox;
    cbxPagCodigo: TComboBox;
    cbxPorta: TComboBox;
    cbxQRCode: TComboBox;
    cbEnviarImpressora: TCheckBox;
    cbxImpressaoViaCliente: TComboBox;
    cbxTransacaoPendenteInicializacao: TComboBox;
    edCNPJEstabelecimento: TEdit;
    edCNPJSwHouse: TEdit;
    edLog: TEdit;
    edNomeAplicacao: TEdit;
    edRazaoSocialEstabelecimento: TEdit;
    edRazaoSocialSwHouse: TEdit;
    edVersaoAplicacao: TEdit;
    gbConfigImpressora: TGroupBox;
    gbConfigTEF: TGroupBox;
    gbPagamentos: TGroupBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    imgQRCode: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label21: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lMensagemCliente: TLabel;
    lMensagemOperador: TLabel;
    lNumOperacao: TLabel;
    lTituloMsgOperador: TLabel;
    lTituloMensagemCliente: TLabel;
    lSaidaImpressao: TLabel;
    lURLTEF: TLabel;
    mImpressao: TMemo;
    mLog: TMemo;
    pConfigImpSwHouseEstab: TPanel;
    pQRCode: TPanel;
    pImpressoraBotes: TPanel;
    pImpressao: TPanel;
    pMensagem: TPanel;
    pMensagemCliente: TPanel;
    pMensagemOperador: TPanel;
    pPrincipal: TPanel;
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
    seTotalAcrescimo: TFloatSpinEdit;
    seTotalDesconto: TFloatSpinEdit;
    seValorInicialVenda: TFloatSpinEdit;
    sbLimparLog: TSpeedButton;
    Splitter1: TSplitter;
    sgPagamentos: TStringGrid;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    tsConfiguracao: TTabSheet;
    tsOperacao: TTabSheet;
    procedure ACBrTEFAPI1QuandoDetectarTransacaoPendente(
      RespostaTEF: TACBrTEFResp; const MsgErro: String);
    procedure ACBrTEFAPI1QuandoExibirMensagem(const Mensagem: String;
      Terminal: TACBrTEFAPITela; MilissegundosExibicao: Integer);
    procedure ACBrTEFAPI1QuandoExibirQRCode(const DadosQRCode: String);
    procedure ACBrTEFAPI1QuandoFinalizarOperacao(RespostaTEF: TACBrTEFResp);
    procedure ACBrTEFAPI1QuandoFinalizarTransacao(RespostaTEF: TACBrTEFResp;
      AStatus: TACBrTEFStatusTransacao);
    procedure ACBrTEFAPI1QuandoGravarLog(const ALogLine: String;
      var Tratado: Boolean);
    procedure ACBrTEFAPI1QuandoEsperarOperacao(
      OperacaoAPI: TACBrTEFAPIOperacaoAPI; var Cancelar: Boolean);
    procedure ACBrTEFAPI1QuandoPerguntarCampo(
      DefinicaoCampo: TACBrTEFAPIDefinicaoCampo; var Resposta: String;
      var Validado: Boolean; var Cancelado: Boolean);
    procedure ACBrTEFAPI1QuandoPerguntarMenu(const Titulo: String;
      Opcoes: TStringList; var ItemSelecionado: Integer);
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
    procedure FormCreate(Sender: TObject);
    procedure btImprimirClick(Sender: TObject);
    procedure btLimparImpressoraClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lURLTEFClick(Sender: TObject);
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
    function AcharTransacaoTEF(IndicePagto: Integer): TACBrTEFResp;
    procedure CancelarVenda;
    procedure FinalizarVenda; // Em caso de Venda, Gere e transmita seu Documento Fiscal
    function ImprimirViaCliente: Boolean;

    procedure ImprimirTodosComprovantes;
    procedure ImprimirComprovantes(ATEFResp: TACBrTEFResp);
    procedure ImprimirRelatorio(ATexto: String);

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
  ACBrUtil, ACBrDelphiZXingQRCode,
  ACBrTEFPayGoComum, ACBrTEFAPIPayGoWeb;

{$R *.lfm}

{ TFormPrincipal }

procedure TFormPrincipal.FormCreate(Sender: TObject);
var
  I: TACBrTEFAPITipo;
  N: TACBrPosPrinterModelo;
  O: TACBrPosPaginaCodigo;
begin
  FVenda := TVenda.Create(NomeArquivoVenda);

  cbxGP.Items.Clear ;
  For I := Low(TACBrTEFAPITipo) to High(TACBrTEFAPITipo) do
     cbxGP.Items.Add( GetEnumName(TypeInfo(TACBrTEFAPITipo), integer(I) ) ) ;
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
    begin
      StatusVenda := stsLivre;
      ACBrTEFAPI1.LimparRespostasTEF;  // Limpa respostas TEF anteriores, e apaga Arqs Backup
    end;

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
  ACBrTEFAPI1.ExibirMensagemPinPad(Msg);
end;

procedure TFormPrincipal.btAdministrativoClick(Sender: TObject);
begin
  AdicionarLinhaLog('- btAdministrativoClick');
  IniciarOperacao;
  StatusVenda := stsOperacaoTEF;
  try
    ACBrTEFAPI1.EfetuarAdministrativa(tefopAdministrativo);
  finally
    StatusVenda := stsFinalizada;
  end;
end;

procedure TFormPrincipal.ACBrTEFAPI1QuandoExibirMensagem(
  const Mensagem: String; Terminal: TACBrTEFAPITela;
  MilissegundosExibicao: Integer);
var
  FormExibeMensagem: TFormExibeMensagem;
begin
  if (Mensagem = '') then
  begin
    if (Terminal in [telaCliente, telaTodas]) then
      MensagemTEF('',' ');
    if (Terminal in [telaOperador, telaTodas]) then
      MensagemTEF(' ','');
  end
  else if (MilissegundosExibicao >= 0) then
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
    if (Terminal in [telaCliente, telaTodas]) then
      MensagemTEF('',Mensagem);
    if (Terminal in [telaOperador, telaTodas]) then
      MensagemTEF(Mensagem,'');
  end;
end;

procedure TFormPrincipal.ACBrTEFAPI1QuandoExibirQRCode(const DadosQRCode: String
  );
var
  QRCode: TDelphiZXingQRCode;
  QRCodeBitmap: TBitmap;
  Row, Column: Integer;
begin
  if not (StatusVenda in [stsAguardandoTEF, stsOperacaoTEF]) then
    StatusVenda := stsAguardandoTEF;

  if (cbxQRCode.ItemIndex = 4) then  // 4 - Imprimir
  begin
    if (DadosQRCode <> '') then
      ImprimirRelatorio( '</ce>'+
                         '<qrcode_largura>8</qrcode_largura>'+
                         '<qrcode>'+DadosQRCode+'</qrcode>'+
                         '</lf>');

    Exit;
  end;

  if (DadosQRCode <> '') then
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
    QRCode.Data      := widestring(DadosQRCode);

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

procedure TFormPrincipal.ACBrTEFAPI1QuandoFinalizarOperacao(
  RespostaTEF: TACBrTEFResp);
var
  MsgFinal: String;
begin
  MsgFinal := RespostaTEF.TextoEspecialOperador;

  AdicionarLinhaLog('');
  AdicionarLinhaLog('');
  AdicionarLinhaLog('------ Fim da Operação ------');
  AdicionarLinhaLog('Sucesso: '+IfThen(RespostaTEF.Sucesso, 'SIM', 'NÃO'));
  AdicionarLinhaLog('Resultado: '+MsgFinal);

  // Usando as propriedades de TACBrTEFResp
  if (RespostaTEF.ValorTotal > 0) then
  begin
    AdicionarLinhaLog('');
    AdicionarLinhaLog('- Rede: '  + RespostaTEF.Rede );
    AdicionarLinhaLog('- NSU: '  + RespostaTEF.NSU );
    AdicionarLinhaLog('- Parcelas: '+ IntToStr(RespostaTEF.QtdParcelas) +
                      ', parcelado por: '+ GetEnumName(TypeInfo(TACBrTEFRespParceladoPor),
                                                       integer(RespostaTEF.ParceladoPor) ));
    AdicionarLinhaLog('- Tipo Cartão: '+IfThen(RespostaTEF.Debito, 'Debito',
                                        IfThen(RespostaTEF.Credito, 'Crédito', '')) );
    AdicionarLinhaLog(' - Valor: '+ FormatFloat(',0.00',RespostaTEF.ValorTotal)) ;
  end;

  // Lendo um Campo Específico //
  if (ACBrTEFAPI1.Modelo = tefApiPayGoWeb) then
    AdicionarLinhaLog('- PWINFO_REQNUM: ' + RespostaTEF.LeInformacao(PWINFO_REQNUM,0).AsString );

  // Exemplo de como processar a Impressão dos comprovantes
  if not RespostaTEF.Sucesso then
  begin
    if (pos('PENDENTE', UpperCase(MsgFinal)) > 0) then
    begin
      if (ACBrTEFAPI1.TratamentoTransacaoPendente = tefpenConfirmar) then
        MsgFinal := MsgFinal + sLineBreak + 'Transação será CONFIRMADA'
      else if (ACBrTEFAPI1.TratamentoTransacaoPendente = tefpenEstornar) then
          MsgFinal := MsgFinal + sLineBreak + 'Transação será ESTORNADA'
      else
        MsgFinal := '';  // Ignora esse erro, pois será tratado em QuandoDetectarTransacaoPendente
    end;

    if (MsgFinal <> '')  then
      MessageDlg( 'TEF', MsgFinal, mtError, [TMsgDlgBtn.mbOK], 0);
  end
  else
  begin
    // Para Confirmar a transação Automáticamento... use:
    //      "ConfirmarTransacoesAutomaticamente := True"

    // ----- Para Confirmar Manualmente a trasação, use o exemplo abaixo -----
    (*
    if (not cbConfirmarAutomaticamente.Checked) and RespostaTEF.Confirmar then
    begin
      MR := MessageDlg( 'TEF',
                        'Transação Autorizada'+sLineBreak+sLineBreak+
                        'Deseja Confirmar ?',
                        mtConfirmation,
                        [mbYes, mbNo], 0 );

      if (MR = mrYes) then
        AStatus := tefstsSucessoManual
      else
        AStatus := tefstsErroDiverso;

      ACBrTEFAPI1.FinalizarTransacao(AStatus);
    end;
    *)

    // Se não for Venda, já podemos Confirmar e imprimir os comprovantes
    if (RespostaTEF.Header <> CHEADER_PAGAMENTO) then
    begin
     if (not ACBrTEFAPI1.ConfirmarTransacaoAutomaticamente) and   // Não confirma de forma automática ?
       RespostaTEF.Confirmar then                                 // Requer confirmação ?
        ACBrTEFAPI1.FinalizarTransacao(tefstsSucessoAutomatico);  // ...então confirme

      ImprimirTodosComprovantes
    end;
  end;

  // --- Exemplo de como usar as Propriedades da API, fazendo TypeCast
  (*
  if (ACBrTEFAPI1.TEF is TACBrTEFAPIClassPayGoWeb) then
  begin
    AdicionarLinhaLog('');
    AdicionarLinhaLog( '-- Retornos do PayGoWeb API --');
    with TACBrTEFAPIClassPayGoWeb(ACBrTEFAPI1.TEF) do
    begin
      for i := 0 to TEFPayGoAPI.DadosDaTransacao.Count-1 do
      begin
        ParseKeyValue(TEFPayGoAPI.DadosDaTransacao[i], TheKey, TheValue);
        nINFO := StrToIntDef(TheKey,-1);
        if (nINFO >= 0) then
          AdicionarLinhaLog(PWINFOToString(nINFO) + ' = ' + TheValue );
      end;
    end;
  end;
  *)
end;

procedure TFormPrincipal.ACBrTEFAPI1QuandoFinalizarTransacao(
  RespostaTEF: TACBrTEFResp; AStatus: TACBrTEFStatusTransacao);
var
  Atualizou, Sucesso: Boolean;
begin
  Sucesso := (AStatus in [tefstsSucessoAutomatico, tefstsSucessoManual]);
  Atualizou := False;

  if (RespostaTEF.Header = CHEADER_PAGAMENTO) then
  begin
    if Sucesso then
      Atualizou := Venda.Pagamentos.ConfirmarPagamento( RespostaTEF.Rede,
                                                        RespostaTEF.NSU,
                                                        RespostaTEF.ValorTotal)
    else
      Atualizou := Venda.Pagamentos.CancelarPagamento( RespostaTEF.Rede,
                                                       RespostaTEF.NSU,
                                                       RespostaTEF.ValorTotal);
  end

  else if (RespostaTEF.Header = CHEADER_CANCELAMENTO) then
  begin
    if Sucesso then
      Atualizou := Venda.Pagamentos.CancelarPagamento( RespostaTEF.Rede,
                                                       RespostaTEF.NSUTransacaoCancelada,
                                                       RespostaTEF.ValorTotal);
  end

  else if (RespostaTEF.Header = CHEADER_ADMINISTRATIVA) then
  begin
    if Sucesso then
    begin
      // https://www.projetoacbr.com.br/forum/topic/61001-como-detectar-se-em-uma-transa%C3%A7%C3%A3o-adm-houve-um-cancelamento/
      if (RespostaTEF.NSUTransacaoCancelada <> '') and
         (RespostaTEF.ImagemComprovante1aVia.Count > 0) then
      begin
        Atualizou := Venda.Pagamentos.CancelarPagamento( RespostaTEF.Rede,
                                                         RespostaTEF.NSU,
                                                         RespostaTEF.ValorTotal);
      end;
    end;
  end;

  if Atualizou then
    AtualizarPagamentosVendaNaInterface;
end;

procedure TFormPrincipal.ACBrTEFAPI1QuandoDetectarTransacaoPendente(
  RespostaTEF: TACBrTEFResp; const MsgErro: String);
var
  AStatus: TACBrTEFStatusTransacao;
  i: Integer;
  ATEFResp: TACBrTEFResp;
  AMsgErro: String;
  MR: TModalResult;
  FormMenuTEF: TFormMenuTEF;
begin
  // Aqui você pode Confirmar ou Desfazer as transações pendentes de acordo com
  // a sua regra de negócios

  // ----------- Exemplo 0 - Deixe o ACBrTEFAndroid CONFIRMAR todas transações pendentes automaticamente
  // ACBrTEFAPI1.TratamentoTransacaoPendente := tefpenConfirmar;
  // Nesse caso... esse evento nem será disparado.

  // ----------- Exemplo 1 - Envio de confirmação automática -----------
  // AStatus := stsSucessoManual;
  // ACBrTEFAPI1.ResolverOperacaoPendente(AStatus);
  // ---------- Fim Exemplo 1 ------------


  // ----------- Exemplo 2 -  Fazer uma pergunta ao usuário ------------
  if (MsgErro = '') then
    AMsgErro := RespostaTEF.TextoEspecialOperador
  else
    AMsgErro := MsgErro;

  FormMenuTEF := TFormMenuTEF.Create(self);
  try
    FormMenuTEF.Titulo := 'Transação Pendente';
    FormMenuTEF.Opcoes.Add('1 - Confirmação Manual');
    FormMenuTEF.Opcoes.Add('2 - Estorno Manual');
    FormMenuTEF.Opcoes.Add('3 - Estorno, Falta de Energia');
    FormMenuTEF.Opcoes.Add('4 - Estorno, Erro na Impressão');
    FormMenuTEF.Opcoes.Add('5 - Estorno, Erro no Dispensador');
    FormMenuTEF.UsaTeclasDeAtalho := True;
    FormMenuTEF.ItemSelecionado := 0;
    FormMenuTEF.btVoltar.Visible := False;

    MR := FormMenuTEF.ShowModal ;
    if (MR = mrOK) then
    begin
      case FormMenuTEF.ItemSelecionado of
        0: AStatus := tefstsSucessoManual;
        1: AStatus := tefstsErroDiverso;
        2: AStatus := tefstsErroEnergia;
        3: AStatus := tefstsErroImpressao;
        4: AStatus := tefstsErroDispesador;
      else
        AStatus := tefstsSucessoManual;
      end;

      ACBrTEFAPI1.ResolverTransacaoPendente(AStatus);
    end;
  finally
    FormMenuTEF.Free;
  end;
  // ---------- Fim Exemplo 2 ------------

  // Opcional... Se confirmou, vamos re-imprimir a transação que ficou pendente
  if (AStatus in [tefstsSucessoAutomatico, tefstsSucessoManual]) then
  begin
    // Achando a transação original...
    i := ACBrTEFAPI1.RespostasTEF.AcharTransacao( RespostaTEF.Rede,
                                                  RespostaTEF.NSU,
                                                  RespostaTEF.Finalizacao);
    if (i >= 0) then
      ATEFResp := ACBrTEFAPI1.RespostasTEF[i]
    else
      ATEFResp := RespostaTEF;

    ImprimirComprovantes(ATEFResp);
  end;
end;

procedure TFormPrincipal.ACBrTEFAPI1QuandoGravarLog(const ALogLine: String;
  var Tratado: Boolean);
begin
  AdicionarLinhaLog(ALogLine);
  Tratado := False;
end;

procedure TFormPrincipal.ACBrTEFAPI1QuandoEsperarOperacao(
  OperacaoAPI: TACBrTEFAPIOperacaoAPI; var Cancelar: Boolean);
begin
  AdicionarLinhaLog( 'QuandoEsperarOperacao: '+
                     GetEnumName(TypeInfo(TACBrTEFAPIOperacaoAPI), integer(OperacaoAPI) ) );

  if FCanceladoPeloOperador then
  begin
    FCanceladoPeloOperador := False;
    Cancelar := True;  // Aborta o TEF em andamento
  end
  else if (StatusVenda <> stsAguardandoTEF) then
  begin
    StatusVenda := stsAguardandoTEF;   // Liga Botão que permite cancelar
    FCanceladoPeloOperador := False;
  end;

  Application.ProcessMessages;
end;

procedure TFormPrincipal.ACBrTEFAPI1QuandoPerguntarCampo(
  DefinicaoCampo: TACBrTEFAPIDefinicaoCampo; var Resposta: String;
  var Validado: Boolean; var Cancelado: Boolean);
Var
  MR: TModalResult ;
  FormObtemCampo: TFormObtemCampo;
begin
  AdicionarLinhaLog( 'QuandoPerguntarCampo: '+DefinicaoCampo.TituloPergunta );

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
    FormObtemCampo.Titulo := DefinicaoCampo.TituloPergunta;
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
      case DefinicaoCampo.TipoDeEntrada of
        tedApenasLeitura:
          FormObtemCampo.edtResposta.ReadOnly := True;
        tedNumerico:
          if (pos('@,@@', DefinicaoCampo.MascaraDeCaptura) > 0) then
            FormObtemCampo.TipoCampo := tcoDecimal
          else
            FormObtemCampo.TipoCampo := tcoNumeric;
        tedAlfabetico:
          FormObtemCampo.TipoCampo := tcoAlfa;
        tedAlfaNum:
          FormObtemCampo.TipoCampo := tcoAlfaNum;
      else
        FormObtemCampo.TipoCampo := tcoString;
      end;
    end;

    MR := FormObtemCampo.ShowModal ;

    Cancelado := (MR <> mrOK) ;
    Validado := False;  // Não fizemos as validações de "DefinicaoCampo.ValidacaoDado", vamos deixar o ACBrTEFAPI validar
    Resposta := FormObtemCampo.Resposta;
  finally
    FormObtemCampo.Free;
  end;
end;

procedure TFormPrincipal.ACBrTEFAPI1QuandoPerguntarMenu(const Titulo: String;
  Opcoes: TStringList; var ItemSelecionado: Integer);
Var
  MR: TModalResult ;
  FormMenuTEF : TFormMenuTEF;
begin
  AdicionarLinhaLog( 'QuandoPerguntarMenu: '+Titulo + sLineBreak + Opcoes.Text );
  if (Opcoes.Count < 1) then
  begin
    ItemSelecionado := -1;
    Exit;
  end;

  FormMenuTEF := TFormMenuTEF.Create(self);
  try
    FormMenuTEF.Titulo := Titulo;
    FormMenuTEF.Opcoes := Opcoes;
    FormMenuTEF.UsaTeclasDeAtalho := (copy(Opcoes[0],1,4) = '1 - ');
    FormMenuTEF.ItemSelecionado := ItemSelecionado;

    MR := FormMenuTEF.ShowModal ;

    case MR of
      mrOK:
        ItemSelecionado := FormMenuTEF.ItemSelecionado;
      mrRetry:
        ItemSelecionado := -2;  // Voltar
    else
      ItemSelecionado := -1;   // Cancelar
    end;
  finally
    FormMenuTEF.Free;
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
    cbxGP.ItemIndex := INI.ReadInteger('TEF', 'GP', 0);
    cbxQRCode.ItemIndex := INI.ReadInteger('TEF', 'QRCode', 1);
    edLog.Text := INI.ReadString('TEF', 'Log', '');
    cbxImpressaoViaCliente.ItemIndex := INI.ReadInteger('TEF', 'ImpressaoViaCliente', 0);
    cbxTransacaoPendente.ItemIndex := INI.ReadInteger('TEF', 'TransacaoPendente', 0);
    cbxTransacaoPendenteInicializacao.ItemIndex := INI.ReadInteger('TEF', 'TransacaoPendenteInicializacao', 1);
    cbAutoAtendimento.Checked := INI.ReadBool('TEF', 'AutoAtendimento', False);
    cbImprimirViaReduzida.Checked := INI.ReadBool('TEF', 'ImprimirViaReduzida', False);
    cbConfirmarAutomaticamente.Checked := INI.ReadBool('TEF', 'ConfirmarAutomaticamente', True);
    cbSuportaDesconto.Checked := INI.ReadBool('TEF', 'SuportaDesconto', True);
    cbSuportaSaque.Checked := INI.ReadBool('TEF', 'SuportaSaque', True);

    edRazaoSocialSwHouse.Text := INI.ReadString('SwHouse', 'RazaoSocial', edRazaoSocialSwHouse.Text);
    edCNPJSwHouse.Text := INI.ReadString('SwHouse', 'CNPJ', edCNPJSwHouse.Text);
    edNomeAplicacao.Text := INI.ReadString('Aplicacao', 'Nome', edNomeAplicacao.Text);
    edVersaoAplicacao.Text := INI.ReadString('Aplicacao', 'Versao', edVersaoAplicacao.Text);

    edRazaoSocialEstabelecimento.Text := INI.ReadString('Estabelecimento', 'RazaoSocial', edRazaoSocialEstabelecimento.Text);
    edCNPJEstabelecimento.Text := INI.ReadString('Estabelecimento', 'CNPJ', edCNPJEstabelecimento.Text);

    cbxModeloPosPrinter.ItemIndex := INI.ReadInteger('PosPrinter', 'Modelo', 1);
    cbxPorta.Text := INI.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta);
    cbxPagCodigo.ItemIndex := INI.ReadInteger('PosPrinter','PaginaDeCodigo', 2);
    ACBrPosPrinter1.Device.ParamsString := INI.ReadString('PosPrinter','ParamsString','');
    seColunas.Value := INI.ReadInteger('PosPrinter','Colunas', 40);
    seEspLinhas.Value := INI.ReadInteger('PosPrinter','EspacoLinhas',ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value := INI.ReadInteger('PosPrinter','LinhasEntreCupons',ACBrPosPrinter1.LinhasEntreCupons);
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
    INI.WriteInteger('TEF', 'GP', cbxGP.ItemIndex);
    INI.WriteInteger('TEF', 'QRCode', cbxQRCode.ItemIndex);
    INI.WriteString('TEF', 'Log', edLog.Text);
    INI.WriteInteger('TEF', 'ImpressaoViaCliente', cbxImpressaoViaCliente.ItemIndex);
    INI.WriteInteger('TEF', 'TransacaoPendente', cbxTransacaoPendente.ItemIndex);
    INI.WriteInteger('TEF', 'TransacaoPendenteInicializacao', cbxTransacaoPendenteInicializacao.ItemIndex);
    INI.WriteBool('TEF', 'AutoAtendimento', cbAutoAtendimento.Checked);
    INI.WriteBool('TEF', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    INI.WriteBool('TEF', 'ConfirmarAutomaticamente', cbConfirmarAutomaticamente.Checked);
    INI.WriteBool('TEF', 'SuportaDesconto', cbSuportaDesconto.Checked);
    INI.WriteBool('TEF', 'SuportaSaque', cbSuportaSaque.Checked);

    INI.WriteString('SwHouse', 'RazaoSocial', edRazaoSocialSwHouse.Text);
    INI.WriteString('SwHouse', 'CNPJ', edCNPJSwHouse.Text);
    INI.WriteString('Aplicacao', 'Nome', edNomeAplicacao.Text);
    INI.WriteString('Aplicacao', 'Versao', edVersaoAplicacao.Text);

    INI.WriteString('Estabelecimento', 'RazaoSocial', edRazaoSocialEstabelecimento.Text);
    INI.WriteString('Estabelecimento', 'CNPJ', edCNPJEstabelecimento.Text);

    INI.WriteInteger('PosPrinter', 'Modelo', cbxModeloPosPrinter.ItemIndex);
    INI.WriteString('PosPrinter','Porta', cbxPorta.Text);
    INI.WriteInteger('PosPrinter','PaginaDeCodigo', cbxPagCodigo.ItemIndex);
    INI.WriteString('PosPrinter','ParamsString', ACBrPosPrinter1.Device.ParamsString);
    INI.WriteInteger('PosPrinter','Colunas', seColunas.Value);
    INI.WriteInteger('PosPrinter','EspacoLinhas', seEspLinhas.Value);
    INI.WriteInteger('PosPrinter','LinhasEntreCupons', seLinhasPular.Value);
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
  NomeTEF := GetEnumName(TypeInfo(TACBrTEFAPITipo), cbxGP.ItemIndex);
  AdicionarLinhaLog('- btTestarTEFClick: '+NomeTEF);
  try
    try
      AtivarTEF;
      ACBrTEFAPI1.EfetuarAdministrativa(tefopTesteComunicacao);
      if ACBrTEFAPI1.UltimaRespostaTEF.Sucesso then
        MessageDlg(Format('TEF %S ATIVO', [NomeTEF]), mtInformation, [mbOK], 0)
      else
      begin
        if (ACBrTEFAPI1.UltimaRespostaTEF.TextoEspecialOperador <> '') then
          MessageDlg(ACBrTEFAPI1.UltimaRespostaTEF.TextoEspecialOperador, mtError, [mbOK], 0);
      end;
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
  Saida := ACBrTEFAPI1.ObterDadoPinPad(dpCPF);
  if (Saida <> '') then
    ShowMessage('CPF digitado: '+Saida)
  else
    ShowMessage('Falha ao Obter CPF no PinPad');
end;

procedure TFormPrincipal.cbEnviarImpressoraChange(Sender: TObject);
begin
  btImprimir.Enabled := ACBrPosPrinter1.Ativo and (not cbEnviarImpressora.Checked);
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

  ACBrTEFAPI1.LimparRespostasTEF;  // Limpa respostas TEF anteriores, e apaga Arqs Backup

  FCanceladoPeloOperador := False;
  FTempoDeEspera := 0;
end;

procedure TFormPrincipal.AdicionarPagamento(const Indice: String; AValor: Double
  );
var
  Ok, TemTEF: Boolean;
  Modalidade: TACBrTEFModalidadePagamento;
  CartoesAceitos: TACBrTEFTiposCartao;
  ReajusteValor: Double;
  IndicePagto: LongInt;
begin
  Ok := False;
  TemTEF := False;

  IndicePagto := StrToIntDef(Indice, 0);

  try
    // ** NOTA **
    // Usa '01' como Indice de Forma de Pagamento de ECF, para todas as operações TEF,
    // para evitar que o ACBrTEFD tente separar os Comprovantes por Forma de Pagamento

    Modalidade := tefmpNaoDefinido;
    CartoesAceitos := [];

    TemTEF := (IndicePagto >= 2) and (IndicePagto <= 5);
    case IndicePagto of
      2:
        Modalidade := tefmpCheque;
      3:
        begin
          Modalidade := tefmpCartao;
          CartoesAceitos := [teftcCredito]
        end;
      4:
        begin
          Modalidade := tefmpCartao;
          CartoesAceitos := [teftcDebito]
        end;
      5:
        Modalidade := tefmpCarteiraVirtual;
      6:
        begin
          Modalidade := tefmpCartao;
          CartoesAceitos := [teftcVoucher]
        end;
    end;

    if TemTEF then
    begin
      // -- Exemplo, usando TypeCast, para inserir Propriedades direto na Classe de TEF -- //
      (*
      if ACBrTEFAPI1.TEF is TACBrTEFAPIClassPayGoWeb then
      begin
        with TACBrTEFAPIClassPayGoWeb(ACBrTEFAPI1.TEF) do
        begin
          TEFPayGoAPI.ParametrosAdicionais.ValueInfo[PWINFO_AUTHSYST] := 'REDE';   // Autorizador
          TEFPayGoAPI.ParametrosAdicionais.ValueInfo[PWINFO_FINTYPE] := '2';       // 01: à vista, 2: parcelado
          TEFPayGoAPI.ParametrosAdicionais.ValueInfo[PWINFO_INSTALLMENTS] := '3';  // Parcelas
        end;
      end;
      *)

      // -- Exemplo, usando TypeCast, para inserir Propriedades direto na Classe de TEF -- //
      (*
      if ACBrTEFAPI1.TEF is TACBrTEFAPIClassCliSiTef then
      begin
        with TACBrTEFAPIClassCliSiTef(ACBrTEFAPI1.TEF) do
        begin
          ParamAdicConfig.Text := '[]';
          ParamAdicFinalizacao.Text := '[]';
          ParamAdicFuncao.Text := '[]';
        end;
      end;
      *)
      Ok := ACBrTEFAPI1.EfetuarPagamento( IntToStr(Venda.NumOperacao),
                                          AValor, Modalidade, CartoesAceitos,
                                          tefmfAVista );

      Ok := Ok and
            ACBrTEFAPI1.UltimaRespostaTEF.Sucesso and
            ACBrTEFAPI1.UltimaRespostaTEF.TransacaoAprovada;
    end
    else
      Ok := True;
  finally
    StatusVenda := stsEmPagamento;
  end;

  // -- Exemplo de como capturar os Erros retornados pela API do TEF PayGoWeb -- //
  (*
  if not OK then
  begin
    if (ACBrTEFAPI1.TEF is TACBrTEFAPIClassPayGoWeb) then
    begin
      CodErro := ACBrTEFAPI1.UltimaRespostaTEF.LeInformacao(PWINFO_RET).AsInteger;
      MsgErro := ACBrTEFAPI1.UltimaRespostaTEF.LeInformacao(PWINFO_RESULTMSG).AsBinary;
      ShowMessage('Erro: '+IntToStr(CodErro)+' - '+Trim(MsgErro));
    end;
  end;
  *)

  if Ok then
  begin
    with Venda.Pagamentos.New do
    begin
      TipoPagamento := Indice;
      ValorPago := AValor;

      if TemTEF then
      begin
        NSU := ACBrTEFAPI1.UltimaRespostaTEF.NSU;
        Rede := ACBrTEFAPI1.UltimaRespostaTEF.Rede;
        RedeCNPJ := ACBrTEFAPI1.UltimaRespostaTEF.NFCeSAT.CNPJCredenciadora;

        // Calcula a Diferença do Valor Retornado pela Operação TEF do Valor que
        //   Informamos no CRT/CHQ
        ReajusteValor := RoundTo(Double(ACBrTEFAPI1.UltimaRespostaTEF.ValorTotal - ValorPago), -2);

        Saque := ACBrTEFAPI1.UltimaRespostaTEF.Saque;
        if (Saque > 0) then
        begin
          // Se houve Saque na operação TEF, devemos adicionar no ValorPago,
          //   para que o Saque conste como Troco
          ValorPago := ValorPago + Saque
        end

        else if (ReajusteValor > 0) then
        begin
          // Se não é Saque, mas houve acréscimo no valor Retornado, devemos lançar
          //   o Reajuste como Acréscimo na venda
          Venda.TotalAcrescimo := Venda.TotalAcrescimo + ReajusteValor;
        end;

        Desconto := ACBrTEFAPI1.UltimaRespostaTEF.Desconto;
        if (Desconto > 0) then
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
  AResp := AcharTransacaoTEF(IndicePagto);
  if Assigned(AResp) then
  begin
    if AResp.CNFEnviado then
      raise Exception.CreateFmt( 'Pagamento TEF [%s] já foi Confirmado.'+sLineBreak+
                                 'Para cancelar o mesmo, cancele Toda a Operação',
                                 [AResp.NSU])
    else
    begin
      ACBrTEFAPI1.FinalizarTransacao( AResp.Rede,
                                      AResp.NSU,
                                      AResp.Finalizacao,
                                      tefstsErroDiverso );
      Cancelada := True;
    end;

    //if Cancelada then
    //begin
    //  if (AResp.ArqBackup <> '') and FileExists(AResp.ArqBackup) then
    //    DeleteFile( AResp.ArqBackup );
    //
    //  ACBrTEFAPI1.RespostasTEF.Remove(AResp);
    //end;
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

function TFormPrincipal.AcharTransacaoTEF(IndicePagto: Integer
  ): TACBrTEFResp;
var
  i: Integer;
begin
  Result := Nil;
  if (IndicePagto < 0) and (IndicePagto >=  Venda.Pagamentos.Count) then
    Exit;

  i := ACBrTEFAPI1.RespostasTEF.AcharTransacao( Venda.Pagamentos[IndicePagto].Rede,
                                                Venda.Pagamentos[IndicePagto].NSU );
  if (i >= 0) then
    Result := ACBrTEFAPI1.RespostasTEF[i];
end;

procedure TFormPrincipal.CancelarVenda;
begin
  AdicionarLinhaLog('- CancelarVenda');
  // AQUI você deve cancelar a sua venda no Banco de Dados, desfazendo baixa de
  // estoque ou outras operações que ocorreram durante a venda.

  ACBrTEFAPI1.CancelarOuEstornarTransacoesDiretorioTrabalho;
  AtualizarPagamentosVendaNaInterface;
  StatusVenda := stsCancelada;
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
    if not ACBrTEFAPI1.ConfirmarTransacaoAutomaticamente then
    begin
      ACBrTEFAPI1.ConfirmarTransacoesPendentes;
      AtualizarPagamentosVendaNaInterface;
    end;

    ImprimirTodosComprovantes;
  except
    CancelarVenda;
  end;
end;

procedure TFormPrincipal.ImprimirTodosComprovantes;
var
  i: Integer;
begin
  for i := 0 to ACBrTEFAPI1.RespostasTEF.Count-1 do
    ImprimirComprovantes(ACBrTEFAPI1.RespostasTEF[i]);
end;

procedure TFormPrincipal.ImprimirComprovantes(ATEFResp: TACBrTEFResp);
begin
  if not Assigned(ATEFResp) then
    Exit;

  if (ATEFResp.ImagemComprovante2aVia.Count > 0) then
    ImprimirRelatorio( ATEFResp.ImagemComprovante2aVia.Text );

  if (ATEFResp.ImagemComprovante1aVia.Count > 0) then
    if ImprimirViaCliente then
      ImprimirRelatorio( ATEFResp.ImagemComprovante1aVia.Text );
end;

procedure TFormPrincipal.ImprimirRelatorio(ATexto: String);
begin
  AdicionarLinhaImpressao('</zera>' + ATexto + '</lf></corte_total>');
end;

function TFormPrincipal.ImprimirViaCliente: Boolean;
var
  MR: TModalResult;
begin
  if (cbxImpressaoViaCliente.ItemIndex = 0) then
    Result := True
  else if (cbxImpressaoViaCliente.ItemIndex = 1) then   // Perguntar
  begin
    MR := MessageDlg( 'Impressão de TEF',
                      'Imprimir Via do Cliente ?', mtConfirmation,
                      [mbYes, mbNo], 0);
    Result := (MR = mrYes);
  end
  else
    Result := False;
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
        AResp := AcharTransacaoTEF(i);
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

  ACBrTEFAPI1.Modelo := TACBrTEFAPITipo(cbxGP.ItemIndex);
  ACBrTEFAPI1.ArqLOG := edLog.Text;
  ACBrTEFAPI1.TratamentoTransacaoPendente := TACBrTEFTratamentoTransacaoPendente(cbxTransacaoPendente.ItemIndex);
  ACBrTEFAPI1.TratamentoTransacaoInicializacao := TACBrTEFTratamentoTransacaoInicializacao(cbxTransacaoPendenteInicializacao.ItemIndex);
  ACBrTEFAPI1.DadosAutomacao.AutoAtendimento := cbAutoAtendimento.Checked;
  ACBrTEFAPI1.DadosAutomacao.ImprimeViaClienteReduzida := cbImprimirViaReduzida.Checked;
  ACBrTEFAPI1.ConfirmarTransacaoAutomaticamente := cbConfirmarAutomaticamente.Checked;

  ACBrTEFAPI1.DadosAutomacao.SuportaDesconto := cbSuportaDesconto.Checked;
  ACBrTEFAPI1.DadosAutomacao.SuportaSaque := cbSuportaSaque.Checked;
  ACBrTEFAPI1.DadosAutomacao.NomeSoftwareHouse := edRazaoSocialSwHouse.Text;
  ACBrTEFAPI1.DadosAutomacao.CNPJSoftwareHouse := edCNPJSwHouse.Text;
  ACBrTEFAPI1.DadosAutomacao.NomeAplicacao := edNomeAplicacao.Text;
  ACBrTEFAPI1.DadosAutomacao.VersaoAplicacao := edVersaoAplicacao.Text;

  ACBrTEFAPI1.DadosEstabelecimento.RazaoSocial := edRazaoSocialEstabelecimento.Text;
  ACBrTEFAPI1.DadosEstabelecimento.CNPJ := edCNPJEstabelecimento.Text;

  case cbxQRCode.ItemIndex of
    0: ACBrTEFAPI1.ExibicaoQRCode := qrapiNaoSuportado;
    2: ACBrTEFAPI1.ExibicaoQRCode := qrapiExibirPinPad;
    3, 4: ACBrTEFAPI1.ExibicaoQRCode := qrapiExibirAplicacao;
  else
    ACBrTEFAPI1.ExibicaoQRCode := qrapiAuto;
  end;

  // -- Exemplo de como ajustar o diretório de Trabalho, da PayGoWeb -- //
  (*
  if (ACBrTEFAPI1.TEF is TACBrTEFAPIClassPayGoWeb) then
    TACBrTEFAPIClassPayGoWeb(ACBrTEFAPI1.TEF).DiretorioTrabalho := 'C:\PAYGOWEB';
  *)
end;

procedure TFormPrincipal.AtivarTEF;
begin
  AdicionarLinhaLog('- AtivarTEF');
  ConfigurarTEF;
  ACBrTEFAPI1.Inicializar;
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
  ACBrTEFAPI1.DesInicializar;
end;

end.

