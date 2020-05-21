{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
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
  Spin, Buttons, DBCtrls, ExtCtrls, Grids, ACBrTEFD,
  ACBrPosPrinter, ACBrTEFDClass, uVendaClass, ACBrTEFDCliSiTef;

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
    btOperacao: TBitBtn;
    btEfetuarPagamentos: TBitBtn;
    btSalvarParametros: TBitBtn;
    btSerial: TSpeedButton;
    btTestarPosPrinter: TBitBtn;
    btTestarTEF: TBitBtn;
    cbAutoAtivar: TCheckBox;
    cbIMprimirViaReduzida: TCheckBox;
    cbMultiplosCartoes: TCheckBox;
    cbSimularErroNoDoctoFiscal: TCheckBox;
    cbSuportaDesconto: TCheckBox;
    cbSuportaReajusteValor: TCheckBox;
    cbSuportaSaque: TCheckBox;
    cbxGP: TComboBox;
    cbxModeloPosPrinter: TComboBox;
    cbxPagCodigo: TComboBox;
    cbxPorta: TComboBox;
    cbEnviarImpressora: TCheckBox;
    edLog: TEdit;
    gbConfigImpressora: TGroupBox;
    gbConfigTEF: TGroupBox;
    gbPagamentos: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    lNumOperacao: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lSaidaImpressao: TLabel;
    mImpressao: TMemo;
    mLog: TMemo;
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
    seEsperaSleep: TSpinEdit;
    seEsperaSTS: TSpinEdit;
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
    procedure ACBrTEFD1DepoisConfirmarTransacoes(
      RespostasPendentes: TACBrTEFDRespostasPendentes);
    procedure ACBrTEFD1ExibeMsg(Operacao: TACBrTEFDOperacaoMensagem;
      Mensagem: String; var AModalResult: TModalResult);
    procedure ACBrTEFD1GravarLog(const GP: TACBrTEFDTipo; ALogLine: String;
      var Tratado: Boolean);
    procedure ACBrTEFD1InfoECF(Operacao: TACBrTEFDInfoECF;
      var RetornoECF: String);
    procedure btAdministrativoClick(Sender: TObject);
    procedure btEfetuarPagamentosClick(Sender: TObject);
    procedure btIncluirPagamentosClick(Sender: TObject);
    procedure btOperacaoClick(Sender: TObject);
    procedure btLerParametrosClick(Sender: TObject);
    procedure btMudaPaginaClick(Sender: TObject);
    procedure btSalvarParametrosClick(Sender: TObject);
    procedure btSerialClick(Sender: TObject);
    procedure btTestarPosPrinterClick(Sender: TObject);
    procedure btTestarTEFClick(Sender: TObject);
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

    procedure IniciarVenda;
    procedure AdicionarPagamento(const Indice: String; AValor: Double);
    procedure CancelarVenda;
    procedure FinalizarVenda; // Em caso de Venda, Gere e transmita seu Documento Fiscal

    procedure AtualizarCaixaLivreNaInterface;
    procedure AtualizarVendaNaInterface;
    procedure AtualizarTotaisVendaNaInterface;
    procedure AtualizarPagamentosVendaNaInterface;
    procedure MensagemTEF(const MsgOperador, MsgCliente: String);
    procedure LimparMensagensTEF;

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
  frIncluirPagamento, frMenuTEF, frObtemCampo,
  configuraserial,
  ACBrUtil;

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

  pgPrincipal.ShowTabs := False;
  pgPrincipal.ActivePageIndex := 0;

  LerConfiguracao;
  LimparMensagensTEF;
  FTipoBotaoOperacao := High(TTipoBotaoOperacao);    // Força atualizar tela
  Venda.Status := High(TStatusVenda);                // Força atualizar tela
  FCanceladoPeloOperador := False;
  FTempoDeEspera := 0;

  Application.OnException := @TratarException;
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
      FCanceladoPeloOperador := True;
  end;
end;

procedure TFormPrincipal.btEfetuarPagamentosClick(Sender: TObject);
begin
  StatusVenda := stsEmPagamento;
  btIncluirPagamentos.Click;
end;

procedure TFormPrincipal.btIncluirPagamentosClick(Sender: TObject);
begin
  FormIncluirPagamento := TFormIncluirPagamento.Create(Self);

  FormIncluirPagamento.cbFormaPagamento.ItemIndex := 2;
  FormIncluirPagamento.seValorPago.Value := -Venda.Troco;
  if (FormIncluirPagamento.ShowModal = mrOK) then
  begin
    AdicionarPagamento( cPagamentos[FormIncluirPagamento.cbFormaPagamento.ItemIndex, 0],
                        FormIncluirPagamento.seValorPago.Value );
  end;
end;

procedure TFormPrincipal.ACBrTEFD1ExibeMsg(Operacao: TACBrTEFDOperacaoMensagem;
  Mensagem: String; var AModalResult: TModalResult);
var
   Fim : TDateTime;
   OldMensagem : String;
begin
  case Operacao of

    opmOK:
       AModalResult := MessageDlg( Mensagem, mtInformation, [mbOK], 0);

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
        OldMensagem := pMensagemOperador.Caption ;
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

procedure TFormPrincipal.ACBrTEFD1DepoisConfirmarTransacoes(
  RespostasPendentes: TACBrTEFDRespostasPendentes);
var
  i , j: Integer;
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

        for j := 0 to Venda.Pagamentos.Count-1 do
        begin
          if NSU = Venda.Pagamentos[j].NSU then
          begin
            Venda.Pagamentos[j].Confirmada := True;
            Break;
          end;
        end;
     end;
  end;

  AtualizarPagamentosVendaNaInterface;
end;

procedure TFormPrincipal.ACBrTEFD1ComandaECF(Operacao: TACBrTEFDOperacaoECF;
  Resp: TACBrTEFDResp; var RetornoECF: Integer);
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

      opePulaLinhas, opeFechaGerencial, opeFechaVinculado:
        begin
          AdicionarLinhaImpressao('</pular_linhas>');
          AdicionarLinhaImpressao('</corte>');
        end;
    end;

    RetornoECF := 1 ;
  except
    RetornoECF := 0 ;
  end;
end;

procedure TFormPrincipal.ACBrTEFD1AntesFinalizarRequisicao(Req: TACBrTEFDReq);
begin
  FCanceladoPeloOperador := False;
  FTempoDeEspera := 0;
  // Use esse evento, para inserir campos personalizados, ou modificar o arquivo
  // de requisião, que será criado e envido para o Gerenciador Padrão

  // Exemplo, adicionando o campo 777-777, caso seja uma transação de Cartão (CRT)
  //if Req.Header = 'CRT' then
  //   Req.GravaInformacao(777,777,'TESTE REDECARD');
  //AdicionarLinhaLog('Enviando: '+Req.Header+' ID: '+IntToStr( Req.ID ) );
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
  if (ACBrTEFD1.GPAtual in [gpCliSiTef, gpVeSPague]) then   // É TEF dedicado ?
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
       RetornoECF := FloatToStr( Venda.TotalVenda - Venda.TotalPago ) ;

     ineTotalAPagar :
       RetornoECF := FloatToStr( Venda.TotalPago ) ;

     ineEstadoECF :
       begin
         Case StatusVenda of
           stsIniciada:
             RetornoECF := 'V' ;
           stsEmPagamento:
             RetornoECF := 'P' ;
           stsLivre, stsFinalizada, stsCancelada:
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
  ACBrTEFD1.ADM;
  StatusVenda := stsFinalizada;
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
    seColunas.Value := INI.ReadInteger('PosPrinter','Colunas',ACBrPosPrinter1.ColunasFonteNormal);
    seEspLinhas.Value := INI.ReadInteger('PosPrinter','EspacoLinhas',ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value := INI.ReadInteger('PosPrinter','LinhasEntreCupons',ACBrPosPrinter1.LinhasEntreCupons);

    cbxGP.ItemIndex := INI.ReadInteger('TEF', 'GP', 0);
    edLog.Text := INI.ReadString('TEF', 'Log', '');
    seEsperaSleep.Value := INI.ReadInteger('TEF', 'Sleep', seEsperaSleep.Value);
    seEsperaSTS.Value := INI.ReadInteger('TEF', 'TimeoutSTS', seEsperaSTS.Value);
    seMaxCartoes.Value := INI.ReadInteger('TEF', 'MaxCartoes', seMaxCartoes.Value);
    seTrocoMaximo.Value := INI.ReadFloat('TEF', 'TrocoMaximo', seTrocoMaximo.Value);
    cbAutoAtivar.Checked := INI.ReadBool('TEF', 'AutoAtivar', cbAutoAtivar.Checked);
    cbImprimirViaReduzida.Checked := INI.ReadBool('TEF', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    cbMultiplosCartoes.Checked := INI.ReadBool('TEF', 'MultiplosCartoes', cbMultiplosCartoes.Checked);
    cbSuportaDesconto.Checked := INI.ReadBool('TEF', 'SuportaDesconto', cbSuportaDesconto.Checked);
    cbSuportaSaque.Checked := INI.ReadBool('TEF', 'SuportaSaque', cbSuportaSaque.Checked);
    cbSuportaReajusteValor.Checked := INI.ReadBool('TEF', 'SuportaReajusteValor', cbSuportaReajusteValor.Checked);
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
    INI.WriteInteger('TEF', 'Sleep', seEsperaSleep.Value);
    INI.WriteInteger('TEF', 'TimeoutSTS', seEsperaSTS.Value);
    INI.WriteInteger('TEF', 'MaxCartoes', seMaxCartoes.Value);
    INI.WriteFloat('TEF', 'TrocoMaximo', seTrocoMaximo.Value);
    INI.WriteBool('TEF', 'AutoAtivar', cbAutoAtivar.Checked);
    INI.WriteBool('TEF', 'ImprimirViaReduzida', cbImprimirViaReduzida.Checked);
    INI.WriteBool('TEF', 'MultiplosCartoes', cbMultiplosCartoes.Checked);
    INI.WriteBool('TEF', 'SuportaDesconto', cbSuportaDesconto.Checked);
    INI.WriteBool('TEF', 'SuportaSaque', cbSuportaSaque.Checked);
    INI.WriteBool('TEF', 'SuportaReajusteValor', cbSuportaReajusteValor.Checked);
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
  if cbEnviarImpressora.Checked then
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
  pImpressao.Enabled := not (AValue in [stsIniciada, stsEmPagamento, stsAguardandoTEF]);
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
    end

  else
    MsgStatus := 'CAIXA LIVRE';
    TipoBotaoOperacao := bopNaoExibir;
    seValorInicialVenda.SetFocus;
    AtualizarCaixaLivreNaInterface;
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
end;

procedure TFormPrincipal.btSerialClick(Sender: TObject);
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
  NomeTEF := GetEnumName(TypeInfo(TACBrTEFDTipo), cbxGP.ItemIndex);
  AdicionarLinhaLog('- btTestarTEFClick: '+NomeTEF);
  try
    AtivarTEF;
    ACBrTEFD1.ATV;
    MessageDlg(Format('TEF %S ATIVO', [NomeTEF]), mtInformation, [mbOK], 0);
  except
    On E: Exception do
    begin
      MessageDlg(Format('Falha ao ativar TEF %S' + sLineBreak + E.Message, [NomeTEF]), mtError, [mbOK], 0);
    end;
  end;
end;

procedure TFormPrincipal.cbEnviarImpressoraChange(Sender: TObject);
begin
  btImprimir.Enabled := ACBrPosPrinter1.Ativo and (not cbEnviarImpressora.Checked);
end;

procedure TFormPrincipal.CliSiTefExibeMenu(Titulo: String; Opcoes: TStringList;
  var ItemSelecionado: Integer; var VoltarMenu: Boolean);
Var
  MR: TModalResult ;
begin
  FormMenuTEF := TFormMenuTEF.Create(self);
  try
    FormMenuTEF.Panel1.Caption := Titulo;
    FormMenuTEF.ListBox1.Items.AddStrings(Opcoes);

    MR := FormMenuTEF.ShowModal ;

    VoltarMenu := (MR = mrRetry) ;

    if (MR = mrOK) then
      ItemSelecionado := FormMenuTEF.ListBox1.ItemIndex;
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
begin
  FormObtemCampo := TFormObtemCampo.Create(self);
  try
    FormObtemCampo.Panel1.Caption := Titulo;
    FormObtemCampo.TamanhoMaximo  := TamanhoMaximo;
    FormObtemCampo.TamanhoMinimo  := TamanhoMinimo;
    FormObtemCampo.Operacao       := Operacao;
    FormObtemCampo.TipoCampo      := TipoCampo;
    FormObtemCampo.Edit1.Text     := Resposta; { Para usar Valores Previamente informados }

    MR := FormObtemCampo.ShowModal ;

    Digitado   := (MR = mrOK) ;
    VoltarMenu := (MR = mrRetry) ;

    if Digitado then
       Resposta := FormObtemCampo.Edit1.Text;
  finally
    FormObtemCampo.Free;
  end;
end;

procedure TFormPrincipal.seValorInicialVendaChange(Sender: TObject);
begin
  seTotalDesconto.MaxValue := seValorInicialVenda.Value;
  if (seValorInicialVenda.Value <> 0) and (StatusVenda = stsLivre) then
  begin
    IniciarVenda;
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

procedure TFormPrincipal.IniciarVenda;
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
begin
  Ok := True;
  TemTEF := False;

  if (Indice = '02') then
  begin
    Ok := ACBrTEFD1.CHQ(AValor, Indice);
    TemTEF := True;
  end
  else if (Indice = '03') or (Indice = '04') then
  begin
    Ok := ACBrTEFD1.CRT(AValor, Indice);
    TemTEF := True;
  end;

  if Ok then
  begin
     with Venda.Pagamentos.New do
     begin
       TipoPagamento := Indice;
       Valor := AValor;
       if TemTEF then
       begin
         NSU := ACBrTEFD1.RespostasPendentes[ACBrTEFD1.RespostasPendentes.Count-1].NSU;
         Rede := ACBrTEFD1.RespostasPendentes[ACBrTEFD1.RespostasPendentes.Count-1].Rede;
       end;
     end;

     AtualizarPagamentosVendaNaInterface;

     if (Venda.TotalPago >= Venda.TotalVenda) then
       FinalizarVenda;
  end;
end;

procedure TFormPrincipal.CancelarVenda;
begin
  AdicionarLinhaLog('- CancelarVenda');
  // AQUI você deve cancelar a sua venda no Banco de Dados, desfazendo baixa de
  // estoque ou outras operações que ocorreram durante a venda.

  ACBrTEFD1.CancelarTransacoesPendentes;
  StatusVenda := stsCancelada;
end;

procedure TFormPrincipal.FinalizarVenda;
var
  SL: TStringList;
  i: Integer;
begin
  // AQUI você deve Gerar e Transmitir o Documento Fiscal (NFCe ou SAT)

  try
    if cbSimularErroNoDoctoFiscal.Checked then
      raise Exception.Create('Erro na geração do Comprovante de Venda');

    SL := TStringList.Create;
    try
      SL.Add(PadCenter( ' COMPROVANTE DE OPERAÇÃO ', ACBrPosPrinter1.Colunas, '-'));
      SL.Add('Número: <n>' + FormatFloat('000000',Venda.NumOperacao) + '</n>');
      SL.Add('Data/Hora: <n>' + FormatDateTimeBr(Venda.DHInicio) + '</n>');
      SL.Add('</linha_simples>');
      SL.Add('Valor Inicial...: <n>' + FormatFloatBr(Venda.ValorInicial) + '</n>');
      SL.Add('Total Descontos.: <n>' + FormatFloatBr(Venda.TotalDesconto) + '</n>');
      SL.Add('Total Acréscimos: <n>' + FormatFloatBr(Venda.TotalAcrescimo) + '</n>');
      SL.Add('Total Operação..: <n>' + FormatFloatBr(Venda.TotalVenda) + '</n>');
      SL.Add('');
      SL.Add(PadCenter( ' Pagamentos ', ACBrPosPrinter1.Colunas, '-'));
      for i := 0 to Venda.Pagamentos.Count-1 do
      begin
        with Venda.Pagamentos[i] do
        begin
          SL.Add(PadSpace( TipoPagamento+' - '+DescricaoTipoPagamento(TipoPagamento)+'|'+
                           FormatFloatBr(Valor)+'|'+Rede, ACBrPosPrinter1.Colunas, '|') );
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

procedure TFormPrincipal.AtualizarVendaNaInterface;
begin
  lNumOperacao.Caption := FormatFloat('000000',Venda.NumOperacao);
  seValorInicialVenda.Value := Venda.ValorInicial;
  seTotalDesconto.Value := Venda.TotalDesconto;
  seTotalAcrescimo.Value := Venda.TotalAcrescimo;
  AtualizarPagamentosVendaNaInterface;
end;

procedure TFormPrincipal.AtualizarTotaisVendaNaInterface;
begin
  edTotalVenda.Text := FormatFloatBr(Venda.TotalVenda);
  edTotalPago.Text := FormatFloatBr(Venda.TotalPago);
  edTroco.Text := FormatFloatBr(max(Venda.Troco,0));
end;

procedure TFormPrincipal.AtualizarPagamentosVendaNaInterface;
var
  i, ARow: Integer;
begin
  sgPagamentos.RowCount := 1;
  for i := 0 to Venda.Pagamentos.Count-1 do
  begin
    ARow := sgPagamentos.RowCount;
    sgPagamentos.RowCount := sgPagamentos.RowCount + 1;

    with Venda.Pagamentos[i] do
    begin
      sgPagamentos.Cells[0, ARow] := FormatFloat('000', ARow);
      sgPagamentos.Cells[1, ARow] := TipoPagamento + ' - ' + DescricaoTipoPagamento(TipoPagamento);
      sgPagamentos.Cells[2, ARow] := FormatFloatBr(Valor);
      sgPagamentos.Cells[3, ARow] := NSU;
      sgPagamentos.Cells[4, ARow] := Rede;
      sgPagamentos.Cells[5, ARow] := ifthen(Confirmada, 'Sim', 'Não');
    end;
  end;

  AtualizarTotaisVendaNaInterface;
end;

procedure TFormPrincipal.MensagemTEF(const MsgOperador, MsgCliente: String);
begin
  if (MsgOperador <> '') then
    pMensagemOperador.Caption := MsgOperador;

  if (MsgCliente <> '') then
    pMensagemCliente.Caption := MsgCliente;

  pMensagemOperador.Visible := (Trim(pMensagemOperador.Caption) <> '');
  pMensagemCliente.Visible := (Trim(pMensagemCliente.Caption) <> '');
  pMensagem.Visible := pMensagemOperador.Visible or pMensagemCliente.Visible;
  Application.ProcessMessages;
end;

procedure TFormPrincipal.LimparMensagensTEF;
begin
  MensagemTEF(' ',' ');
end;

procedure TFormPrincipal.ConfigurarTEF;
begin
  AdicionarLinhaLog('- ConfigurarTEF');
  ACBrTEFD1.ArqLOG := edLog.Text;
  ACBrTEFD1.EsperaSleep := seEsperaSleep.Value;
  ACBrTEFD1.EsperaSTS := seEsperaSTS.Value;
  ACBrTEFD1.TrocoMaximo := seTrocoMaximo.Value;
  ACBrTEFD1.AutoAtivarGP := cbAutoAtivar.Checked;
  ACBrTEFD1.ImprimirViaClienteReduzida := cbImprimirViaReduzida.Checked;
  ACBrTEFD1.MultiplosCartoes := cbMultiplosCartoes.Checked;
  ACBrTEFD1.NumeroMaximoCartoes := seMaxCartoes.Value;
  ACBrTEFD1.SuportaDesconto := cbSuportaDesconto.Checked;
  ACBrTEFD1.SuportaReajusteValor := cbSuportaReajusteValor.Checked;
  ACBrTEFD1.SuportaSaque := cbSuportaSaque.Checked;

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
  AtivarTEF;
  try
    AtivarPosPrinter;
  except
    On E: Exception do
    begin
      TratarException(nil, E);
    end;
  end;
end;

procedure TFormPrincipal.Desativar;
begin
  AdicionarLinhaLog('- Desativar');
  ACBrPosPrinter1.Desativar;
  ACBrTEFD1.DesInicializar(TACBrTEFDTipo(cbxGP.ItemIndex));
end;

end.

