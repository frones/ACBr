{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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
{                                                                              }
{  Algumas funçoes dessa Unit foram extraidas de outras Bibliotecas, veja no   }
{ cabeçalho das Funçoes no código abaixo a origem das informaçoes, e autores...}
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit FormTEFDemoAndroid;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.ImageList,
  FMX.Types,
  FMX.Platform,
  FMX.Controls,
  FMX.VirtualKeyboard,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Edit,
  FMX.SpinBox,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.Layouts,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.ImgList,
  FMX.Objects,
  FMX.ComboEdit,
  FMX.DateTimeCtrls,
  FMX.TabControl,
  FMX.EditBox,
  FMX.Controls.Presentation,
  FMX.DialogService,
  FMX.Ani,
  FMX.Memo.Types,
  Androidapi.JNI.GraphicsContentViewText,
  ACBrTEFAndroid, ACBrTEFComum,
  ACBrPosPrinterElginE1Service, ACBrPosPrinterElginE1Lib,
  ACBrPosPrinterGEDI, ACBrPosPrinterTecToySunmiLib,
  ACBrBase, ACBrPosPrinter, ACBrTEFAPIComum;

const
  CNOME_CONFIG = 'ACBrTEFDemoAndroid.ini';
  CBOTAO_VENDA = 'Venda';
  CBOTAO_PAGAR = 'Pagar';
  CBOTAO_ESTORNO = 'Estorno';
  CBOTAO_ESTORNAR = 'Estornar';

  CITENS_MODALIDADE_PAGTO: array[0..2] of String =
    ('Não Definido', 'Cartão', 'Carteira Virtual');

  CITENS_TIPO_FINANCIAMENTO: array[0..5] of String =
    ('Não Definido', 'A VISTA', 'Parcelado Emissor',
     'Parcelado Estabelecimento', 'Pré Datado', 'Crédito Emissor');

  CITENS_TIPO_CARTAO: array[0..5] of String =
    ('Não Definido', 'Cartão de Crédito', 'Cartão de Débito',
     'Cartão Voucher', 'Private Label', 'Frota');

  CITENS_PROVEDOR: array[0..22] of String =
    ('NÃO DEFINIDO', 'REDE', 'CIELO', 'STONE', 'PAGSEGURO', 'GETNET', 'SAFRA',
     'VERO', 'BIN', 'CONDUCTOR', 'CREDSYSTEM', 'CTF', 'DMCARD', 'ELAVON',
     'FDCORBAN', 'GAX', 'GLOBAL', 'INFOCARDS', 'LIBERCARD', 'NDDCARD', 'RV',
     'TICKETLOG', 'VCMAIS');

  CITENS_MOEDAS: array[0..2] of String =
    ('REAL', 'DOLAR', 'EURO');

  CITENS_MENU_ADMIN: array[0..24] of String =
    ('OPERACAO_DESCONHECIDA', 'VENDA', 'ADMINISTRATIVA', 'FECHAMENTO',
     'CANCELAMENTO', 'PREAUTORIZACAO', 'CONSULTA_SALDO', 'CONSULTA_CHEQUE',
     'GARANTIA_CHEQUE', 'CANCELAMENTO_PREAUTORIZACAO', 'SAQUE', 'DOACAO',
     'PAGAMENTO_CONTA', 'CANCELAMENTO_PAGAMENTOCONTA', 'RECARGA_CELULAR',
     'INSTALACAO', 'REIMPRESSAO', 'RELATORIO_SINTETICO', 'RELATORIO_DETALHADO',
     'TESTE_COMUNICACAO', 'RELATORIO_RESUMIDO', 'EXIBE_PDC', 'VERSAO',
     'CONFIGURACAO', 'MANUTENCAO' );

type
  TFrTEFDemoAndroid = class(TForm)
    StyleBook1: TStyleBook;
    tabsPrincipal: TTabControl;
    tabConfig: TTabItem;
    tabTeste: TTabItem;
    ToolBar2: TToolBar;
    lblTituloTestes: TLabel;
    tabsConfig: TTabControl;
    tbConfImpressora: TTabItem;
    tsConfTEF: TTabItem;
    lbConfImpressora: TListBox;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    lbImpressoras: TListBoxItem;
    cbxImpressorasBth: TComboBox;
    btnProcurarBth: TCornerButton;
    chbTodasBth: TCheckBox;
    lbModelos: TListBoxItem;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    lbLarguraEspacejamento: TListBoxItem;
    GridPanelLayout1: TGridPanelLayout;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    seColunas: TSpinBox;
    seEspLinhas: TSpinBox;
    seLinhasPular: TSpinBox;
    GridPanelLayout5: TGridPanelLayout;
    cbxModelo: TComboBox;
    Label4: TLabel;
    cbxPagCodigo: TComboBox;
    Label5: TLabel;
    cbControlePorta: TCheckBox;
    Label1: TLabel;
    gpBotoesConfig: TGridPanelLayout;
    btLerConfig: TCornerButton;
    btSalvarConfig: TCornerButton;
    lbiBotaoTestar: TListBoxItem;
    btnTestarImpressora: TCornerButton;
    lbConfTEF: TListBox;
    Image1: TImage;
    imgConfig: TImage;
    gplBtnAcoes: TGridPanelLayout;
    btVendaPagar: TButton;
    btAdmin: TButton;
    btEstorno: TButton;
    btUltTransacao: TButton;
    gplMain: TGridPanelLayout;
    layoutMain: TLayout;
    tabVenda: TTabItem;
    layoutVenda: TGridPanelLayout;
    Rectangle1: TRectangle;
    edtValorVenda: TEdit;
    b2: TCornerButton;
    b3: TCornerButton;
    bLimpar: TCornerButton;
    bApagar: TCornerButton;
    b4: TCornerButton;
    b5: TCornerButton;
    b6: TCornerButton;
    bMais: TCornerButton;
    bMenos: TCornerButton;
    b7: TCornerButton;
    b8: TCornerButton;
    b9: TCornerButton;
    b0: TCornerButton;
    b1: TCornerButton;
    lConfOper: TLayout;
    gplConfOperacao: TGridPanelLayout;
    gplbtnModalidade: TGridPanelLayout;
    lModelidade: TLabel;
    cbxModalidadePagto: TComboBox;
    gplTipoFinanciamento: TGridPanelLayout;
    lFinanciamento: TLabel;
    cbxTipoFinanciamento: TComboBox;
    gplTipoCartao: TGridPanelLayout;
    lTipoCartao: TLabel;
    cbxTipoCartao: TComboBox;
    gplProvedor: TGridPanelLayout;
    lProvedor: TLabel;
    cbxProvedor: TComboBox;
    gplMoeda: TGridPanelLayout;
    lMoeda: TLabel;
    cbxMoeda: TComboBox;
    gplPreDatado: TGridPanelLayout;
    Label6: TLabel;
    gplParcelas: TGridPanelLayout;
    Label8: TLabel;
    sbxFinancParcelas: TSpinBox;
    deFinancPreDatado: TDateEdit;
    ImageList1: TImageList;
    ToolBar1: TToolBar;
    Label15: TLabel;
    sbConfigVoltar: TSpeedButton;
    lbiDadosAutomacao: TListBoxItem;
    lbDadosAutomacao: TListBox;
    ListBoxItem2: TListBoxItem;
    edtCNPJSwHouse: TEdit;
    imgErrorCNPJSwHouse: TImage;
    ListBoxItem4: TListBoxItem;
    edtNomeSwHouse: TEdit;
    imgErrorNomeSwHouse: TImage;
    ListBoxItem6: TListBoxItem;
    edtNomeAplicacao: TEdit;
    imgErrorNomeAplicacao: TImage;
    ListBoxItem7: TListBoxItem;
    edtVersaoAplicacao: TEdit;
    imgErrorVersaoAplicacao: TImage;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    lbiDadosEstabelecimento: TListBoxItem;
    lbDadosEstabelecimento: TListBox;
    ListBoxGroupHeader6: TListBoxGroupHeader;
    ListBoxItem12: TListBoxItem;
    edtNomeFantasiaEstabelecimento: TEdit;
    imgErrorNomeFantasia: TImage;
    ListBoxItem13: TListBoxItem;
    edtCNPJEstabelecimento: TEdit;
    imgErrorCNPJEstabelecimento: TImage;
    bVezes: TCornerButton;
    bPorcent: TCornerButton;
    b00: TCornerButton;
    bIgual: TCornerButton;
    tabAdmin: TTabItem;
    lbiGeral: TListBoxItem;
    lbGeral: TListBox;
    lbghGeral: TListBoxGroupHeader;
    lbiConfirmacaoManual: TListBoxItem;
    swConfirmacaoManual: TSwitch;
    lbiInterfaceACBr: TListBoxItem;
    swInterfaceAlternativa: TSwitch;
    ListBoxItem20: TListBoxItem;
    swMenuAdministrativo: TSwitch;
    layoutAdmin: TLayout;
    lbAdmin: TListBox;
    tabEstorno: TTabItem;
    layoutEstorno: TLayout;
    lbEstorno: TListBox;
    ListBoxGroupHeader7: TListBoxGroupHeader;
    ListBoxItem11: TListBoxItem;
    edtEstornoNSU: TEdit;
    ListBoxItem14: TListBoxItem;
    edtEstornoCodAutorizacao: TEdit;
    ListBoxItem15: TListBoxItem;
    ListBoxItem16: TListBoxItem;
    edtEstornoValorTransacao: TEdit;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    tabUltTransacao: TTabItem;
    lbiFontesImpressora: TListBoxItem;
    ListBox1: TListBox;
    ListBoxGroupHeader8: TListBoxGroupHeader;
    ListBoxItem17: TListBoxItem;
    swFonteCondensada: TSwitch;
    ListBoxItem18: TListBoxItem;
    swFonteNegrito: TSwitch;
    ColorAnimation1: TColorAnimation;
    lbiCapacidadesAutomacao: TListBoxItem;
    ListBox3: TListBox;
    ListBoxGroupHeader17: TListBoxGroupHeader;
    ListBoxItem42: TListBoxItem;
    swSuportaTroco: TSwitch;
    ListBoxItem43: TListBoxItem;
    swSuportaDesconto: TSwitch;
    ListBoxItem44: TListBoxItem;
    swViasDiferenciadas: TSwitch;
    ListBoxItem45: TListBoxItem;
    swViaConsumidorReduzida: TSwitch;
    ListBoxItem46: TListBoxItem;
    swSuportaAbaterVouchere: TSwitch;
    tiStartUp: TTimer;
    lbiAutoconfirmarPendente: TListBoxItem;
    tabsUltimaTransacao: TTabControl;
    tabDadosUltimaTransacao: TTabItem;
    memoDadosUltimaTransacao: TMemo;
    tabComprovantes: TTabItem;
    tabsComprovantes: TTabControl;
    tabViaCliente: TTabItem;
    memoViaCliente: TMemo;
    tabViaEstabelecimento: TTabItem;
    memoViaEstabelecimento: TMemo;
    tabLog: TTabItem;
    memoLog: TMemo;
    ToolBar3: TToolBar;
    Label13: TLabel;
    SpeedButton1: TSpeedButton;
    CornerButton1: TCornerButton;
    ListBoxItem1: TListBoxItem;
    cbxImpressaoViaCliente: TComboBox;
    GridPanelLayout2: TGridPanelLayout;
    edtEstornoDataTransacao: TDateEdit;
    edtEstornoHoraTransacao: TEdit;
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrTEFAndroid1: TACBrTEFAndroid;
    ListBoxGroupHeader5: TListBoxGroupHeader;
    lbiClasse: TListBoxItem;
    GridPanelLayout6: TGridPanelLayout;
    rbClasseInterna: TRadioButton;
    rbClasseExterna: TRadioButton;
    cbxTransacaoPendente: TComboBox;
    lbiGapKeyboard: TListBoxItem;
    procedure FormCreate(Sender: TObject);
    procedure btVendaPagarClick(Sender: TObject);
    procedure ClickBotaoNumero(Sender: TObject);
    procedure bApagarClick(Sender: TObject);
    procedure bLimparClick(Sender: TObject);
    procedure OperacaoClick(Sender: TObject);
    procedure bIgualClick(Sender: TObject);
    procedure cbxTipoFinanciamentoChange(Sender: TObject);
    procedure bPorcentClick(Sender: TObject);
    procedure btAdminClick(Sender: TObject);
    procedure imgConfigClick(Sender: TObject);
    procedure btEstornoClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure btUltTransacaoClick(Sender: TObject);
    procedure lblTituloTestesClick(Sender: TObject);
    procedure edtApenasNumeros(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormVirtualKeyboardShown(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure FormVirtualKeyboardHidden(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure edtEnterScrollableControl(Sender: TObject);
    procedure edtCNPJSwHouseTyping(Sender: TObject);
    procedure edtCNPJEstabelecimentoTyping(Sender: TObject);
    procedure edtNomeSwHouseTyping(Sender: TObject);
    procedure edtNomeAplicacaoTyping(Sender: TObject);
    procedure edtVersaoAplicacaoTyping(Sender: TObject);
    procedure edtNomeFantasiaEstabelecimentoTyping(Sender: TObject);
    procedure tiStartUpTimer(Sender: TObject);
    procedure edtEstornoValorTransacaoTyping(Sender: TObject);
    procedure sbConfigVoltarClick(Sender: TObject);
    procedure btnTestarImpressoraClick(Sender: TObject);
    procedure btSalvarConfigClick(Sender: TObject);
    procedure btLerConfigClick(Sender: TObject);
    procedure btnProcurarBthClick(Sender: TObject);
    procedure seColunasEnter(Sender: TObject);
    procedure ACBrTEFAndroid1QuandoIniciarTransacao(AIntent: JIntent);
    procedure SpeedButton1Click(Sender: TObject);
    procedure lbAdminItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure CornerButton1Click(Sender: TObject);
    procedure ACBrTEFAndroid1QuandoGravarLog(const ALogLine: string;
      var Tratado: Boolean);
    procedure edtEstornoHoraTransacaoTyping(Sender: TObject);
    procedure rbMudaClasseImpressora(Sender: TObject);
    procedure cbxModeloChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ACBrTEFAndroid1QuandoFinalizarOperacao(RespostaTEF: TACBrTEFResp);
    procedure ACBrTEFAndroid1QuandoDetectarTransacaoPendente(
      RespostaTEF: TACBrTEFResp; const MsgErro: string);
  private
    { Private declarations }
    fE1Printer: TACBrPosPrinterElginE1Service;
    fE1Lib: TACBrPosPrinterElginE1Lib;
    fGEDIPrinter: TACBrPosPrinterGEDI;
    fSunmiPrinter: TACBrPosPrinterTecToySunmiLib;

    fValorOperacao: Double;
    fOperacao: String;
    fNovoValor: Boolean;

    fVKService: IFMXVirtualKeyboardService;
    fScrollBox: TCustomScrollBox;
    fControlToCenter: TControl;

    function GetValorVenda: Double;
    procedure SetValorVenda(const Value: Double);

    function EstaVendendo: Boolean;

    procedure AdicionarNumeroNaVenda(ANum: String);
    procedure MemorizarOperacao(Operacao: String);
    procedure LimparValorVenda;
    procedure RealizarOperacaoMemorizada;
    procedure RealizarOperacaoMemorizadaPorcentagem;

    function CalcularNomeArqConfiguracao: String;
    procedure CarregarImpressorasBth;
    procedure CarregarModelosExternos;
    procedure CarregarModelosInternos;

    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure GravarConfiguracaoTransacao;

    procedure AplicarConfiguracoes;
    procedure AplicarConfiguracaoPosPrinter;
    procedure AplicarConfiguracaoTEF;
    procedure AplicarConfiguracaoPersonalizacao;
    procedure AplicarConfiguracaoTransacao;

    procedure IniciarTransacaoTEF;
    procedure InicializarTEF;
    procedure InicializarPosPrinter;

    procedure LimparInterfacePrincipal;
    procedure AjustarScroll(AControl: TControl; AScrollBox: TCustomScrollBox = Nil);

    procedure VoltarParaTestes;
    procedure MostrarTelaVenda;
    procedure ExecutarPagamentoTEF;

    procedure MostrarMenuAdministrativoPayGo;
    procedure ExecutarAdministrativo(Operacao: String = '');

    procedure MostrarTelaEstorno;
    procedure ExecutarEstornoTEF;

    procedure MostrarTelaUltimaTransacao;

    procedure MostrarTelaConfiguracao(Aba: Integer = 0);
    procedure FecharConfiguracao;

    procedure ImprimirComprovantes(ATEFResp: TACBrTEFResp);
    procedure ImprimirRelatorio(ATexto: String);
    procedure ExibirErroImpressaoE1(const MsgErro: string);
  public
    { Public declarations }
    property ValorVenda: Double read GetValorVenda write SetValorVenda;

  end;

procedure Toast(const AMsg: string; ShortDuration: Boolean = False);

var
  FrTEFDemoAndroid: TFrTEFDemoAndroid;

implementation

uses
  Math, StrUtils, IniFiles,
  System.TypInfo,
  System.IOUtils,
  Androidapi.JNI.Widget,
  Androidapi.Helpers,
  FMX.Helpers.Android,
  ACBrTEFAndroidPayGo, ACBrTEFPayGoComum,
  ACBrUtil, ACBrValidador;

{$R *.fmx}

procedure Toast(const AMsg: string; ShortDuration: Boolean);
var
  ToastLength: Integer;
begin
  {$IfNDef ANDROID}
   TDialogService.ShowMessage(AMsg);
  {$Else}
   if ShortDuration then
     ToastLength := TJToast.JavaClass.LENGTH_SHORT
   else
     ToastLength := TJToast.JavaClass.LENGTH_LONG;

   TJToast.JavaClass.makeText(SharedActivityContext, StrToJCharSequence(AMsg), ToastLength).show;
   Application.ProcessMessages;
  {$EndIf}
end;


procedure TFrTEFDemoAndroid.FormCreate(Sender: TObject);

  procedure AddItemsToComboBox( Items: Array of string; AComboBox: TComboBox);
  var
    AItem: TListBoxItem;
    i: Integer;
  begin
    AComboBox.Items.Clear;
    for i := Low(Items) to High(Items) do
    begin
      AItem := TListBoxItem.Create(AComboBox);
      AItem.StyledSettings := AItem.StyledSettings - [TStyledSetting.Other];
      AItem.TextSettings.HorzAlign := TTextAlign.Center;
      AItem.TextSettings.VertAlign := TTextAlign.Center;
      AItem.TextSettings.WordWrap := True;
      AItem.TextSettings.FontColor := TAlphaColorRec.White;
      AItem.TextSettings.Font.Size := 8;
      AItem.Text := Items[i];
      AComboBox.AddObject(AItem);
    end;

    AComboBox.ItemIndex := 0;
  end;

var
  n: TACBrPosPrinterModelo;
  o: TACBrPosPaginaCodigo;

begin
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(fVKService));
  ACBrTEFAndroid1.QuandoIniciarTransacao := ACBrTEFAndroid1QuandoIniciarTransacao;

  // Criando Classes de Impressoras Externas //
  fE1Printer := TACBrPosPrinterElginE1Service.Create(ACBrPosPrinter1);
  fE1Printer.Modelo := TElginE1Printers.prnSmartPOS;
  fE1Printer.OnErroImpressao := ExibirErroImpressaoE1;
  fGEDIPrinter := TACBrPosPrinterGEDI.Create(ACBrPosPrinter1);
  fSunmiPrinter := TACBrPosPrinterTecToySunmiLib.Create(ACBrPosPrinter1);
  fE1Lib := TACBrPosPrinterElginE1Lib.Create(ACBrPosPrinter1);
  fE1Lib.Modelo := TElginE1LibPrinters.prnM8;

  // Zerando a Interface
  gplParcelas.Visible := False;
  gplPreDatado.Visible := False;

  tabsConfig.TabIndex := 0;
  tabsPrincipal.TabIndex := 0;
  tabsUltimaTransacao.TabIndex := 0;
  tabsComprovantes.TabIndex := 0;

  tabsPrincipal.TabPosition := TTabPosition.None;

  memoViaCliente.Lines.Clear;
  memoViaEstabelecimento.Lines.Clear;
  memoLog.Lines.Clear;

  AddItemsToComboBox(CITENS_MODALIDADE_PAGTO, cbxModalidadePagto);
  AddItemsToComboBox(CITENS_TIPO_FINANCIAMENTO, cbxTipoFinanciamento);
  AddItemsToComboBox(CITENS_TIPO_CARTAO, cbxTipoCartao);
  AddItemsToComboBox(CITENS_PROVEDOR, cbxProvedor);
  AddItemsToComboBox(CITENS_MOEDAS, cbxMoeda);

  imgErrorNomeSwHouse.Bitmap := ImageList1.Bitmap(TSizeF.Create(imgErrorNomeSwHouse.Width,imgErrorCNPJSwHouse.Height), 8);
  imgErrorCNPJSwHouse.Bitmap := imgErrorNomeSwHouse.Bitmap;
  imgErrorNomeAplicacao.Bitmap := imgErrorNomeSwHouse.Bitmap;
  imgErrorVersaoAplicacao.Bitmap := imgErrorNomeSwHouse.Bitmap;
  imgErrorNomeFantasia.Bitmap := imgErrorNomeSwHouse.Bitmap;
  imgErrorCNPJEstabelecimento.Bitmap := imgErrorNomeSwHouse.Bitmap;

  cbxPagCodigo.Items.Clear ;
  For o := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
    cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(o) ) ) ;

  lbAdmin.Items.Clear;
  LimparValorVenda;
  ValorVenda := 1;
  MostrarTelaVenda;
end;

procedure TFrTEFDemoAndroid.FormDestroy(Sender: TObject);
begin
  fE1Printer.Free;
  fGEDIPrinter.Free;
  fSunmiPrinter.Free;
  fE1Lib.Free;
end;

procedure TFrTEFDemoAndroid.CarregarModelosExternos;
begin
  cbxModelo.Items.Clear;
  cbxModelo.Items.Add('Elgin E1 Service');
  cbxModelo.Items.Add('Elgin E1 Lib');
  cbxModelo.Items.Add('Gertec GEDI');
  cbxModelo.Items.Add('TecToy Sunmi Service');
  lbImpressoras.Enabled := False;
end;

procedure TFrTEFDemoAndroid.CarregarModelosInternos;
var
  m: TACBrPosPrinterModelo;
begin
  cbxModelo.Items.Clear;
  For m := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
     cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(m) ) );

  lbImpressoras.Enabled := True;
end;

procedure TFrTEFDemoAndroid.ExibirErroImpressaoE1(
  const MsgErro: string);
begin
  TDialogService.MessageDialog( MsgErro,
                                TMsgDlgType.mtError, [TMsgDlgBtn.mbOK],
                                TMsgDlgBtn.mbOk, 0, nil, nil);
end;

procedure TFrTEFDemoAndroid.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if (Key = vkHardwareBack) then
  begin
    if (fVKService <> nil) then
    begin
      if TVirtualKeyboardState.Visible in fVKService.VirtualKeyboardState then
      begin
        fVKService.HideVirtualKeyboard;
        Key := 0;
        Exit
      end;
    end;

    if (tabsPrincipal.ActiveTab = tabConfig) then
    begin
      FecharConfiguracao;
      Key := 0;
    end

    else if (tabsPrincipal.ActiveTab = tabUltTransacao) then
    begin
      VoltarParaTestes;
      Key := 0;
    end

    else if EstaVendendo then
    begin
      LimparValorVenda;
      Key := 0;
    end;
  end

  else if EstaVendendo then
  begin
    if (Key = vkEscape) then
    begin
      LimparValorVenda;
      Key := 0;
    end

    else if (KeyChar in ['0'..'9'])  then
    begin
      AdicionarNumeroNaVenda(KeyChar);
      Key := 0;
    end

    else if (KeyChar in ['-','+','*','x','X'])  then
    begin
      MemorizarOperacao( LowerCase(KeyChar) );
      Key := 0;
    end

    else if ((KeyChar= '=') or (Key = vkReturn))  then
    begin
      RealizarOperacaoMemorizada;
      Key := 0;
    end

    else if (KeyChar = '%')  then
    begin
      RealizarOperacaoMemorizadaPorcentagem;
      Key := 0;
    end;
  end;
end;

procedure TFrTEFDemoAndroid.AjustarScroll(AControl: TControl; AScrollBox: TCustomScrollBox);
var
  AVirtualKeyboard: IVirtualKeyboardControl;
begin
  // Não chamou com parâmetros corretos
  if (not Assigned(AControl)) or (not Assigned(AScrollBox)) then
    Exit;

  // Verificando se esse controle, exibirá o Teclado
  if not Supports(AControl, IVirtualKeyboardControl, AVirtualKeyboard) then
    Exit;

  FScrollBox := AScrollBox;
  FControlToCenter := AControl;

  { Ok, agora que salvamos o Scroll e o Controle a ser centralizado, vamos
    deixar a mágica ocorrer em OnVirtualKeyboardShow }
end;

procedure TFrTEFDemoAndroid.AplicarConfiguracoes;
begin
  AplicarConfiguracaoPosPrinter;
  AplicarConfiguracaoTEF;
end;

procedure TFrTEFDemoAndroid.AplicarConfiguracaoPosPrinter;
begin
  if rbClasseExterna.IsChecked then
  begin
    case cbxModelo.ItemIndex of
      0: ACBrPosPrinter1.ModeloExterno := fE1Printer;
      1: ACBrPosPrinter1.ModeloExterno := fE1Lib;
      2: ACBrPosPrinter1.ModeloExterno := fGEDIPrinter;
    else
      ACBrPosPrinter1.ModeloExterno := fSunmiPrinter;
    end;

    cbxImpressorasBth.ItemIndex := cbxImpressorasBth.Items.IndexOf('NULL');
  end
  else
  begin
    if Assigned(cbxModelo.Selected) then
      ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo(cbxModelo.ItemIndex)
    else
      ACBrPosPrinter1.Modelo := ppTexto;

    if Assigned(cbxImpressorasBth.Selected) then
      ACBrPosPrinter1.Porta := cbxImpressorasBth.Selected.Text
    else if cbxImpressorasBth.ItemIndex = cbxImpressorasBth.Items.IndexOf('NULL') then
      cbxImpressorasBth.ItemIndex := -1;
  end;

  if Assigned(cbxPagCodigo.Selected) then
    ACBrPosPrinter1.PaginaDeCodigo := TACBrPosPaginaCodigo(cbxPagCodigo.ItemIndex);

  ACBrPosPrinter1.ColunasFonteNormal := Trunc(seColunas.Value);
  ACBrPosPrinter1.EspacoEntreLinhas := Trunc(seEspLinhas.Value);
  ACBrPosPrinter1.LinhasEntreCupons := Trunc(seLinhasPular.Value);
  ACBrPosPrinter1.ConfigLogo.KeyCode1 := 1;
  ACBrPosPrinter1.ConfigLogo.KeyCode2 := 0;
  ACBrPosPrinter1.ControlePorta := cbControlePorta.IsChecked;
end;

procedure TFrTEFDemoAndroid.AplicarConfiguracaoTEF;
begin
  ACBrTEFAndroid1.DesInicializar;
  ACBrTEFAndroid1.Modelo := tefPayGo;
  ACBrTEFAndroid1.DiretorioTrabalho := TPath.Combine(TPath.GetPublicPath, 'tef');
  ACBrTEFAndroid1.ArqLOG := TPath.Combine(ACBrTEFAndroid1.DiretorioTrabalho, 'acbrtefandroid.log');
  if not DirectoryExists(ACBrTEFAndroid1.DiretorioTrabalho) then
    ForceDirectories(ACBrTEFAndroid1.DiretorioTrabalho);

  ACBrTEFAndroid1.DadosAutomacao.NomeSoftwareHouse := edtNomeSwHouse.Text;
  ACBrTEFAndroid1.DadosAutomacao.CNPJSoftwareHouse := edtCNPJSwHouse.Text;
  ACBrTEFAndroid1.DadosAutomacao.NomeAplicacao := edtNomeAplicacao.Text;
  ACBrTEFAndroid1.DadosAutomacao.VersaoAplicacao := edtVersaoAplicacao.Text;
  ACBrTEFAndroid1.DadosAutomacao.SuportaSaque := swSuportaTroco.IsChecked;
  ACBrTEFAndroid1.DadosAutomacao.SuportaDesconto := swSuportaDesconto.IsChecked;
  ACBrTEFAndroid1.DadosAutomacao.SuportaViasDiferenciadas := swViasDiferenciadas.IsChecked;
  ACBrTEFAndroid1.DadosAutomacao.ImprimeViaClienteReduzida := swViaConsumidorReduzida.IsChecked;
  ACBrTEFAndroid1.DadosAutomacao.UtilizaSaldoTotalVoucher := swSuportaAbaterVouchere.IsChecked;

  ACBrTEFAndroid1.DadosEstabelecimento.RazaoSocial := edtNomeFantasiaEstabelecimento.Text;
  ACBrTEFAndroid1.DadosEstabelecimento.CNPJ := edtCNPJEstabelecimento.Text;

  ACBrTEFAndroid1.ConfirmarTransacaoAutomaticamente := not swConfirmacaoManual.IsChecked;
  if (cbxTransacaoPendente.ItemIndex >= 0) then
    ACBrTEFAndroid1.TratamentoTransacaoPendente := TACBrTEFTratamentoTransacaoPendente(
      cbxTransacaoPendente.ItemIndex);

  AplicarConfiguracaoPersonalizacao;
  AplicarConfiguracaoTransacao;
end;

procedure TFrTEFDemoAndroid.AplicarConfiguracaoTransacao;
var
  MoedaISO: Integer;
begin
  case cbxMoeda.ItemIndex of
    1: MoedaISO := 840;
    2: MoedaISO := 978;
  else
    MoedaISO := 986;
  end;
  ACBrTEFAndroid1.DadosAutomacao.MoedaISO4217 := MoedaISO;

  if ACBrTEFAndroid1.TEF is TACBrTEFAndroidPayGoClass then
  begin
    with TACBrTEFAndroidPayGoClass( ACBrTEFAndroid1.TEF ) do
    begin
      if (cbxProvedor.ItemIndex > 0) then
        Autorizador := cbxProvedor.Selected.Text.Trim.ToUpper
      else
        Autorizador := '';
    end;
  end;

//    cbxModalidadePagto.ItemIndex := Ini.ReadInteger('Transacao', 'ModalidadePagto', 0);
//    cbxTipoFinanciamento.ItemIndex := Ini.ReadInteger('Transacao', 'TipoFinanciamento', 0);
//    cbxTipoCartao.ItemIndex := Ini.ReadInteger('Transacao', 'TipoCartao', 0);
end;

procedure TFrTEFDemoAndroid.AplicarConfiguracaoPersonalizacao;
begin
  if swInterfaceAlternativa.IsChecked then
  begin
    With ACBrTEFAndroid1.Personalizacao do
    begin
      corFundoTela := TAlphaColorRec.Darkblue;
      corFundoToolbar := TAlphaColorRec.Gold;
      corFonte := TAlphaColorRec.White;
      corSeparadorMenu := TAlphaColorRec.White;
      corFundoCaixaEdicao := TAlphaColorRec.Azure;
      corTextoCaixaEdicao := TAlphaColorRec.Black;
      corFundoTeclado := TAlphaColorRec.Darkblue;
      corTeclaLiberadaTeclado := TAlphaColorRec.Green;
      corTeclaPressionadaTeclado := TAlphaColorRec.Greenyellow;
      corFonteTeclado := TAlphaColorRec.Black;
      ArquivoIcone := TPath.Combine(TPath.GetHomePath, 'ACBrFavIcon.png');
      ArquivoFonte := TPath.Combine(TPath.GetHomePath, 'RobotoSlab-Regular.ttf');
    end;
  end
  else
    ACBrTEFAndroid1.Personalizacao.Clear;
end;

procedure TFrTEFDemoAndroid.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
var
  KeyBoardScreenTop, ControlScreenBottom, PosicaoIdeal,
  Ajuste, NovoY: Extended;
  AParent: TControl;
begin
  if not Assigned(fControlToCenter) then
    Exit;

  if (fScrollBox = nil) then  // Se não foi atribuido um fScrollBox, tenta achar nos Pais do Controle
  begin
    AParent := TControl(fControlToCenter.Parent);
    while (AParent <> nil) do
    begin
      if (AParent is TCustomScrollBox) then  // Oba... Achei um scrollBox Pai
      begin
        fScrollBox := TCustomScrollBox(AParent);
        if (fScrollBox.ContentBounds.Height > fScrollBox.Height) then  // Essa Caixa faz Scroll ?
          Break;
      end;

      AParent := TControl(AParent.Parent);  // Ainda não achei... Vamos analisar o Pai do Pai
    end;

    if not Assigned(fScrollBox)  then  // Que pena, não achei um ScrollBox... saindo...
      Exit;
  end;

  // Verificando se precisa fazer o Scroll... Onde está posicionado o controle ?
  KeyBoardScreenTop := Bounds.Top;
  ControlScreenBottom := fControlToCenter.LocalToAbsolute(TPointF.Zero).Y +
                         (fControlToCenter.Height * 2);

  // Verificando se precisa Reposicionar o Scroll, de acordo com o tamanho do Teclado
  if (ControlScreenBottom > KeyBoardScreenTop) then
  begin
    PosicaoIdeal := FScrollBox.LocalToAbsolute(TPointF.Zero).Y +
                    Trunc(KeyBoardScreenTop / 2);  // Meio da Area Livre do Scroll
    Ajuste := ControlScreenBottom - PosicaoIdeal;

    // Reposiciona o Scroll, de acordo com o Offset calculado
    NovoY := Ajuste + FScrollBox.ViewportPosition.Y;
    FScrollBox.ViewportPosition := PointF(FScrollBox.ViewportPosition.X, NovoY);
    if (FScrollBox.ViewportPosition.Y < NovoY) then   // Se não conseguiu rolar o suficiente, vamos ativar as Margens
    begin
      fScrollBox.Margins.Bottom := NovoY - FScrollBox.ViewportPosition.Y;
      fScrollBox.ViewportPosition := PointF(FScrollBox.ViewportPosition.X, NovoY);
    end;
  end;
end;

procedure TFrTEFDemoAndroid.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  if Assigned(fScrollBox) then
  begin
    fScrollBox.Margins.Bottom := 0;
    fScrollBox := nil;
  end;
end;

function TFrTEFDemoAndroid.GetValorVenda: Double;
begin
  Result := StrToIntDef(OnlyNumber(edtValorVenda.Text), 0)/100;
end;

procedure TFrTEFDemoAndroid.SetValorVenda(const Value: Double);
var
  AValor: Double;
begin
  AValor := min( max(0, Value), 999999);
  edtValorVenda.Text := 'R$ '+FormatFloatBr(AValor);
  edtValorVenda.CaretPosition := edtValorVenda.Text.Length;
end;

procedure TFrTEFDemoAndroid.SpeedButton1Click(Sender: TObject);
begin
  VoltarParaTestes;
end;

procedure TFrTEFDemoAndroid.tiStartUpTimer(Sender: TObject);
begin
  tiStartUp.Enabled := False;  // Dispara apenas uma vez
  LerConfiguracao;
  AplicarConfiguracoes;
end;

procedure TFrTEFDemoAndroid.VoltarParaTestes;
begin
  tabsPrincipal.SetActiveTabWithTransition( tabTeste,
                                            TTabTransition.Slide,
                                            TTabTransitionDirection.Reversed ) ;
end;

procedure TFrTEFDemoAndroid.btAdminClick(Sender: TObject);
begin
  if swMenuAdministrativo.IsChecked and (ACBrTEFAndroid1.TEF is TACBrTEFAndroidPayGoClass) then
    MostrarMenuAdministrativoPayGo
  else
    ExecutarAdministrativo;
end;

procedure TFrTEFDemoAndroid.btEstornoClick(Sender: TObject);
begin
  if (btEstorno.Text = CBOTAO_ESTORNAR) then
    ExecutarEstornoTEF
  else
    MostrarTelaEstorno;
end;

procedure TFrTEFDemoAndroid.btLerConfigClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TFrTEFDemoAndroid.btnProcurarBthClick(Sender: TObject);
begin
  CarregarImpressorasBth;
  cbxImpressorasBth.DropDown;
end;

procedure TFrTEFDemoAndroid.btnTestarImpressoraClick(Sender: TObject);
var
  SL: TStringList;
begin
  AplicarConfiguracaoPosPrinter;

  SL := TStringList.Create;
  try
    SL.Add('      *** ACBR TEF - ANDROID ***');
    SL.Add('         Via Estabelecimento');
    SL.Add('              REDE');
    SL.Add('');
    SL.Add('BAR DA MARIA');
    SL.Add('50.713.076/0001-32');
    SL.Add('');
    SL.Add('COMPR:000123456  VALOR 000000000100');
    SL.Add('');
    SL.Add('ESTAB:          14/08/1971-123456');
    SL.Add('TERM:00001234');
    SL.Add('CARTAO: ************1234');
    SL.Add('AUTORIZACAO: 012345');
    SL.Add('');
    SL.Add('TRANSACAO AUTORIZADA MEDIANTE');
    SL.Add('   USO DE SENHA PESSOAL');
    SL.Add('');
    SL.Add('');
    SL.Add('--------------------------------------');
    SL.Add('12345 EC:0000000123 REF:0000001234');
    SL.Add('TRANSACAO TESTE SEM VALOR FINANCEIRO!');

    ImprimirRelatorio(SL.Text);
  finally
    SL.Free;
  end;

end;

procedure TFrTEFDemoAndroid.btSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TFrTEFDemoAndroid.btUltTransacaoClick(Sender: TObject);
begin
  MostrarTelaUltimaTransacao;
end;

procedure TFrTEFDemoAndroid.btVendaPagarClick(Sender: TObject);
begin
  if (btVendaPagar.Text = CBOTAO_PAGAR) then
    ExecutarPagamentoTEF
  else
    MostrarTelaVenda;
end;

function TFrTEFDemoAndroid.CalcularNomeArqConfiguracao: String;
begin
  Result := ApplicationPath + CNOME_CONFIG;
end;

procedure TFrTEFDemoAndroid.CarregarImpressorasBth;
begin
  {$IfDef HAS_BLUETOOTH}
   cbxImpressorasBth.Items.Clear;
   try
     ACBrPosPrinter1.Device.AcharPortasBlueTooth( cbxImpressorasBth.Items, chbTodasBth.IsChecked );
     cbxImpressorasBth.Items.Add('NULL');
   except
   end;
  {$EndIf}
end;

procedure TFrTEFDemoAndroid.cbxModeloChange(Sender: TObject);
begin
  if rbClasseInterna.IsChecked and (cbxModelo.ItemIndex = Integer(ppExterno)) then
    rbClasseExterna.IsChecked := True
  else if rbClasseExterna.IsChecked and (cbxModelo.ItemIndex = 1) then
    cbxPagCodigo.ItemIndex := Integer(pcUTF8);
end;

procedure TFrTEFDemoAndroid.cbxTipoFinanciamentoChange(Sender: TObject);
begin
  gplParcelas.Visible := (cbxTipoFinanciamento.ItemIndex = 2) or
                         (cbxTipoFinanciamento.ItemIndex = 3);
  gplPreDatado.Visible := (cbxTipoFinanciamento.ItemIndex = 4);
end;

procedure TFrTEFDemoAndroid.ClickBotaoNumero(Sender: TObject);
begin
  AdicionarNumeroNaVenda(TButton(Sender).Text);
end;

procedure TFrTEFDemoAndroid.CornerButton1Click(Sender: TObject);
var
  Relatorio: string;
begin
  case tabsUltimaTransacao.TabIndex of
    0: Relatorio := memoDadosUltimaTransacao.Lines.Text;

    1:
    begin
      if tabsComprovantes.ActiveTab = tabViaCliente then
        Relatorio := memoViaCliente.Lines.Text
      else
        Relatorio := memoViaEstabelecimento.Lines.Text;
    end;

    2: Relatorio := memoLog.Lines.Text;
  end;

  if not Relatorio.Trim.IsEmpty then
    ImprimirRelatorio( Relatorio );
end;

procedure TFrTEFDemoAndroid.edtNomeFantasiaEstabelecimentoTyping(Sender: TObject);
begin
  imgErrorNomeFantasia.Visible := (edtNomeFantasiaEstabelecimento.Text.Length < 4);
end;

procedure TFrTEFDemoAndroid.edtApenasNumeros(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if not CharIsNum(KeyChar) then
    KeyChar := #0;
end;

procedure VerificarFormatarEditCNPJ(AEdit: TEdit; AImgErr: TImage);
begin
  if (AEdit.Text.Length > 2) then
  begin
    AEdit.Text := ACBrValidador.FormatarMascaraDinamica(OnlyNumber(AEdit.Text), '**.***.***/****-**');
    AEdit.CaretPosition := AEdit.Text.Length;
  end;

  AImgErr.Visible := (AEdit.Text.Length < 18) or
                     (not ACBrValidador.ValidarCNPJ(AEdit.Text).IsEmpty);
end;

procedure TFrTEFDemoAndroid.edtCNPJEstabelecimentoTyping(Sender: TObject);
begin
  VerificarFormatarEditCNPJ(edtCNPJEstabelecimento, imgErrorCNPJEstabelecimento);
end;

procedure TFrTEFDemoAndroid.edtCNPJSwHouseTyping(Sender: TObject);
begin
  VerificarFormatarEditCNPJ(edtCNPJSwHouse, imgErrorCNPJSwHouse);
end;

procedure TFrTEFDemoAndroid.edtEnterScrollableControl(Sender: TObject);
begin
  if (Sender is TControl) then
    AjustarScroll(TControl(Sender), lbConfTEF);
end;

procedure TFrTEFDemoAndroid.edtEstornoHoraTransacaoTyping(Sender: TObject);
var
  ANumeros: string;
begin
  if (edtEstornoHoraTransacao.Text.Length > 2) then
  begin
    edtEstornoHoraTransacao.Text := ACBrValidador.FormatarMascaraDinamica(
       OnlyNumber(edtEstornoHoraTransacao.Text), '**:**:**');
    edtEstornoHoraTransacao.CaretPosition := edtEstornoHoraTransacao.Text.Length;
  end;
end;

procedure TFrTEFDemoAndroid.edtEstornoValorTransacaoTyping(Sender: TObject);
var
  AValor: Double;
begin
  AValor := StrToIntDef(OnlyNumber(edtEstornoValorTransacao.Text), 0)/100;
  AValor := min( max(0, AValor), 999999);
  edtEstornoValorTransacao.Text := 'R$ '+FormatFloatBr(AValor);
  edtEstornoValorTransacao.CaretPosition := edtEstornoValorTransacao.Text.Length;
end;

procedure TFrTEFDemoAndroid.edtNomeAplicacaoTyping(Sender: TObject);
begin
  imgErrorNomeAplicacao.Visible := (edtNomeAplicacao.Text.Length < 2);
end;

procedure TFrTEFDemoAndroid.edtNomeSwHouseTyping(Sender: TObject);
begin
  imgErrorNomeSwHouse.Visible := (edtNomeSwHouse.Text.Length < 4);
end;

procedure TFrTEFDemoAndroid.edtVersaoAplicacaoTyping(Sender: TObject);
begin
  imgErrorVersaoAplicacao.Visible := edtVersaoAplicacao.Text.IsEmpty;
end;

function TFrTEFDemoAndroid.EstaVendendo: Boolean;
begin
  Result := (tabsPrincipal.ActiveTab = tabTeste) and (layoutVenda.Parent = layoutMain);
end;

procedure TFrTEFDemoAndroid.ExecutarAdministrativo(Operacao: String = '');
begin
  IniciarTransacaoTEF;
  ACBrTEFAndroid1.EfetuarAdministrativa(Operacao);
end;

procedure TFrTEFDemoAndroid.ExecutarEstornoTEF;
var
  AValor: Double;
  ADataHora: TDateTime;
  ATime: TTime;
begin
  AValor := StrToIntDef(OnlyNumber(edtEstornoValorTransacao.Text), 0)/100;
  AValor := min( max(0, AValor), 999999);
  if (AValor = 0) then
  begin
    Toast('DEFINA VALOR DO ESTORNO');
    Exit;
  end;

  ATime := EncodeTime( StrToIntDef(copy(edtEstornoHoraTransacao.Text, 1, 2), 0),
                       StrToIntDef(copy(edtEstornoHoraTransacao.Text, 4, 2), 0),
                       StrToIntDef(copy(edtEstornoHoraTransacao.Text, 7, 2), 0),
                       0 );
  ADataHora := edtEstornoDataTransacao.Date + ATime;

  IniciarTransacaoTEF;
  ACBrTEFAndroid1.CancelarTransacao( edtEstornoNSU.Text,
                                     edtEstornoCodAutorizacao.Text,
                                     ADataHora,
                                     AValor );
end;

procedure TFrTEFDemoAndroid.ExecutarPagamentoTEF;
var
  IdentificadorTransacao: string;
  ValTransacao: Double;
  TipoCartao: TACBrTEFTiposCartao;
  ModPagto: TACBrTEFModalidadePagamento;
  ModFinanc: TACBrTEFModalidadeFinanciamento;
  Parcelas: Byte;
  DataPre: TDate;
begin
  ValTransacao := ValorVenda;
  if (ValTransacao <= 0) then
  begin
    Toast('DEFINA UM VALOR');
    Exit;
  end;

  IniciarTransacaoTEF;
  GravarConfiguracaoTransacao;
  AplicarConfiguracaoTransacao;  // Ajusta Moeda e Provedor

  IdentificadorTransacao := FormatDateTime('hhnnss',now);  // NOTA: Aqui você pode usar um Identificador da Venda no seu PDV

  case cbxModalidadePagto.ItemIndex of
    1: ModPagto := TACBrTEFModalidadePagamento.tefmpCartao;
    2: ModPagto := TACBrTEFModalidadePagamento.tefmpCarteiraVirtual;
  else
    ModPagto := TACBrTEFModalidadePagamento.tefmpNaoDefinido;
  end;

  case cbxTipoFinanciamento.ItemIndex of
    1: ModFinanc := TACBrTEFModalidadeFinanciamento.tefmfAVista;
    2: ModFinanc := TACBrTEFModalidadeFinanciamento.tefmfParceladoEmissor;
    3: ModFinanc := TACBrTEFModalidadeFinanciamento.tefmfParceladoEstabelecimento;
    4: ModFinanc := TACBrTEFModalidadeFinanciamento.tefmfPredatado;
    5: ModFinanc := TACBrTEFModalidadeFinanciamento.tefmfCreditoEmissor;
  else
    ModFinanc := TACBrTEFModalidadeFinanciamento.tefmfNaoDefinido;
  end;

  case cbxTipoCartao.ItemIndex of
    1: TipoCartao := [TACBrTEFTipoCartao.teftcCredito];
    2: TipoCartao := [TACBrTEFTipoCartao.teftcDebito];
    3: TipoCartao := [TACBrTEFTipoCartao.teftcVoucher];
    4: TipoCartao := [TACBrTEFTipoCartao.teftcPrivateLabel];
    5: TipoCartao := [TACBrTEFTipoCartao.teftcFrota];
  else
    TipoCartao := [];
  end;

  Parcelas := 0;
  if (ModFinanc in [TACBrTEFModalidadeFinanciamento.tefmfParceladoEmissor,
                    TACBrTEFModalidadeFinanciamento.tefmfParceladoEstabelecimento]) then
    Parcelas := Trunc(sbxFinancParcelas.Value);

  DataPre := 0;
  if (ModFinanc in [TACBrTEFModalidadeFinanciamento.tefmfPredatado]) then
    DataPre := deFinancPreDatado.Date;

  ACBrTEFAndroid1.EfetuarPagamento( IdentificadorTransacao,
                                    ValTransacao,
                                    ModPagto,
                                    TipoCartao,
                                    ModFinanc,
                                    Parcelas,
                                    DataPre );
end;

procedure TFrTEFDemoAndroid.bApagarClick(Sender: TObject);
var
  TextVal: string;
begin
  TextVal := OnlyNumber(edtValorVenda.Text);
  TextVal := copy(TextVal, 1, TextVal.Length-1);
  ValorVenda := StrToIntDef(TextVal, 0)/100;
end;

procedure TFrTEFDemoAndroid.bIgualClick(Sender: TObject);
begin
  RealizarOperacaoMemorizada;
end;

procedure TFrTEFDemoAndroid.bPorcentClick(Sender: TObject);
begin
  RealizarOperacaoMemorizadaPorcentagem;
end;

procedure TFrTEFDemoAndroid.bLimparClick(Sender: TObject);
begin
  LimparValorVenda;
end;

procedure TFrTEFDemoAndroid.OperacaoClick(Sender: TObject);
begin
  MemorizarOperacao(TButton(Sender).Text[1]);
end;

procedure TFrTEFDemoAndroid.rbMudaClasseImpressora(Sender: TObject);
begin
  if rbClasseInterna.IsChecked then
    CarregarModelosInternos
  else
    CarregarModelosExternos;
end;

procedure TFrTEFDemoAndroid.RealizarOperacaoMemorizada;
begin
  if not fOperacao.IsEmpty then
  begin
    case fOperacao[1] of
      '+': ValorVenda := ValorVenda + fValorOperacao;
      '-': ValorVenda := fValorOperacao - ValorVenda;
      'x', '*': ValorVenda := ValorVenda * fValorOperacao;
    end;

    fOperacao := '';
  end;

  fValorOperacao := 0;
end;

procedure TFrTEFDemoAndroid.RealizarOperacaoMemorizadaPorcentagem;
var
  Taxa: Double;
begin
  if not fOperacao.IsEmpty then
  begin
    Taxa := (ValorVenda / 100);

    case fOperacao[1] of
      '+': ValorVenda := fValorOperacao * (1 + Taxa);
      '-': ValorVenda := fValorOperacao * (1 - Taxa);
      'x', '*': ValorVenda := fValorOperacao * Taxa;
    end;

    fOperacao := '';
  end;

  fValorOperacao := 0;
end;

procedure TFrTEFDemoAndroid.sbConfigVoltarClick(Sender: TObject);
begin
  FecharConfiguracao;
end;

procedure TFrTEFDemoAndroid.seColunasEnter(Sender: TObject);
begin
  if (Sender is TControl) then
    AjustarScroll(TControl(Sender));
end;

procedure TFrTEFDemoAndroid.ACBrTEFAndroid1QuandoDetectarTransacaoPendente(
  RespostaTEF: TACBrTEFResp; const MsgErro: string);
var
  AStatus: TACBrTEFStatusTransacao;
  i: Integer;
  ATEFResp: TACBrTEFResp;
  aMsgErro: String;
begin
  // Aqui você pode Confirmar ou Desfazer as transações pendentes de acordo com
  // a sua regra de negócios

  // Exemplo 0 - Deixe o ACBrTEFAndroid CONFIRMAR todas transações pendentes automaticamente
  // ACBrTEFAndroid1.AutoConfirmarTransacoesPendente := True;
  // Nesse caso... esse evento não será disparado.

  // Exemplo 1 -  Envio de confirmação automática:
  // AStatus := stsSucessoManual;
  // ACBrTEFAndroid1.ResolverOperacaoPendente(AStatus);

  // Exemplo 2 -  Fazer uma pergunta ao usuário:
  if (MsgErro = '') then
    AMsgErro := RespostaTEF.TextoEspecialOperador
  else
    AMsgErro := MsgErro;

  TDialogService.MessageDialog( AMsgErro + sLineBreak+sLineBreak +
                                'Confirmar ?',
                                TMsgDlgType.mtConfirmation,
                                [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
                                TMsgDlgBtn.mbYes, 0,

    procedure(const AResult: TModalResult)
    begin
        if (AResult = mrYes) then
          AStatus := TACBrTEFStatusTransacao.tefstsSucessoManual
        else
          AStatus := TACBrTEFStatusTransacao.tefstsErroDiverso;

        ACBrTEFAndroid1.ResolverTransacaoPendente(AStatus);
      end
  );

  // Se confirmou, vamos re-imprimir a transação que ficou pendente
  if (AStatus in [tefstsSucessoAutomatico, tefstsSucessoManual]) then
  begin
    ATEFResp := RespostaTEF;
    // Achando a transação original...
    for i := 0 to ACBrTEFAndroid1.RespostasTEF.Count-1 do
    begin
      if (ACBrTEFAndroid1.RespostasTEF[i].NSU = RespostaTEF.NSU) and
         (ACBrTEFAndroid1.RespostasTEF[i].Rede = RespostaTEF.Rede) then
      begin
        ATEFResp := ACBrTEFAndroid1.RespostasTEF[i];
        Break;
      end;
    end;

    ImprimirComprovantes(ATEFResp);
  end;
end;

procedure TFrTEFDemoAndroid.ACBrTEFAndroid1QuandoFinalizarOperacao(
  RespostaTEF: TACBrTEFResp);
var
  i, nINFO: Integer;
  TheKey, TheValue: string;
  MsgFinal: String;
begin
  MostrarTelaUltimaTransacao;

  with memoDadosUltimaTransacao.Lines do
  begin
    Clear;

    MsgFinal := RespostaTEF.TextoEspecialOperador;

    Add('');
    Add('');
    Add('------ Fim da Transação ------');
    Add('Sucesso: '+IfThen(RespostaTEF.Sucesso, 'SIM', 'NÃO'));
    Add('Resultado: '+MsgFinal);

    // Usando as propriedades de TACBrTEFResp
    Add('');
    Add('- Rede: '  + RespostaTEF.Rede );
    Add('- NSU: '  + RespostaTEF.NSU );
    Add('- Parcelas: '+ IntToStr(RespostaTEF.QtdParcelas) +
                  ', parcelado por: '+
                  GetEnumName(TypeInfo(TACBrTEFRespParceladoPor), integer(RespostaTEF.ParceladoPor) ));
    Add('- Tipo Cartão: '+IfThen(RespostaTEF.Debito, 'Debito',
                                  IfThen(RespostaTEF.Credito, 'Crédito', '')) );
    Add(' - Valor: '+ FormatFloat(',0.00',RespostaTEF.ValorTotal)) ;

    // Lendo um Campo Específico //
    Add('- PWINFO_REQNUM: ' + RespostaTEF.LeInformacao(PWINFO_REQNUM,0).AsString );
  end;

  memoViaEstabelecimento.Lines.Text := RespostaTEF.ImagemComprovante2aVia.Text;
  memoViaCliente.Lines.Text  := RespostaTEF.ImagemComprovante1aVia.Text;

  // Exemplo de como processar a Impressão dos comprovantes
  if not RespostaTEF.Sucesso then
  begin
    if MsgFinal.ToUpper.Contains('PENDENTE') then
    begin
      if (ACBrTEFAndroid1.TratamentoTransacaoPendente = tefpenConfirmar) then
        MsgFinal := MsgFinal + sLineBreak + 'Transação será CONFIRMADA'
      else if (ACBrTEFAndroid1.TratamentoTransacaoPendente = tefpenEstornar) then
          MsgFinal := MsgFinal + sLineBreak + 'Transação será ESTORNADA'
      else
        MsgFinal := '';  // Ignora esse erro, pois será tratado em QuandoDetectarTransacaoPendente
    end;
  end
  else
  begin
    // Para Confirmar a transação Automáticamento... use:
    //      "ConfirmarTransacoesAutomaticamente := True"
    // Para Confirmar Manualmente a trasação, use o exemplo abaixo...
    if swConfirmacaoManual.IsChecked and RespostaTEF.Confirmar then
    begin
      MsgFinal := '';
      TDialogService.MessageDialog( 'Transação Autorizada'+sLineBreak+sLineBreak+
                                    'Deseja Confirmar ?',
                                    TMsgDlgType.mtConfirmation,
                                    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
                                    TMsgDlgBtn.mbYes, 0,

        procedure(const AResult: TModalResult)
        var
          AStatus: TACBrTEFStatusTransacao;
        begin
            if (AResult = mrYes) then
              AStatus := TACBrTEFStatusTransacao.tefstsSucessoManual
            else
              AStatus := TACBrTEFStatusTransacao.tefstsErroDiverso;

            ACBrTEFAndroid1.FinalizarTransacao(AStatus);

            if (AResult = mrYes) then
              ImprimirComprovantes(RespostaTEF);
        end
        );
    end
    else
      ImprimirComprovantes(RespostaTEF);
  end;

  if (MsgFinal <> '') then
  begin
    TDialogService.MessageDialog( MsgFinal,
                                  TMsgDlgType.mtError,
                                  [TMsgDlgBtn.mbOK],
                                  TMsgDlgBtn.mbOK, 0, nil);

  end;

  // Exemplo de como usar as Propriedades da API, fazendo TypeCast
  if (ACBrTEFAndroid1.TEF is TACBrTEFAndroidPayGoClass) then
  begin
    memoDadosUltimaTransacao.Lines.Add('');
    memoDadosUltimaTransacao.Lines.Add( '-- Retornos do PayGo API --');
    with TACBrTEFAndroidPayGoClass(ACBrTEFAndroid1.TEF) do
    begin
      for i := 0 to TEFPayGoAPI.DadosDaTransacao.Count-1 do
      begin
        ParseKeyValue(TEFPayGoAPI.DadosDaTransacao[i], TheKey, TheValue);
        nINFO := StrToIntDef(TheKey,-1);
        if (nINFO >= 0) then
          memoDadosUltimaTransacao.Lines.Add(PWINFOToString(nINFO) + ' = ' + TheValue );
      end;
    end;
  end;
end;

procedure TFrTEFDemoAndroid.ACBrTEFAndroid1QuandoGravarLog(const ALogLine: string;
  var Tratado: Boolean);
var
  Linha: string;
begin
  if (ALogLine.Length > 1024) then
    Linha := LeftStr(ALogLine,1024)+'...'
  else
    Linha := ALogLine;

  memoLog.Lines.Add(Linha);
end;

procedure TFrTEFDemoAndroid.ACBrTEFAndroid1QuandoIniciarTransacao(AIntent: JIntent);
begin
  memoLog.Lines.Add('');
  memoLog.Lines.Add('Iniciando Comunicação com APK');
  memoLog.Lines.Add('');
end;

procedure TFrTEFDemoAndroid.ImprimirComprovantes(ATEFResp: TACBrTEFResp);
begin
  if not Assigned(ATEFResp) then
    Exit;

  if (ATEFResp.ImagemComprovante2aVia.Count > 0) then
    ImprimirRelatorio( ATEFResp.ImagemComprovante2aVia.Text );

  if (ATEFResp.ImagemComprovante1aVia.Count > 0) then
  begin
    if (cbxImpressaoViaCliente.ItemIndex = 0) then   // Imprimir
      ImprimirRelatorio( ATEFResp.ImagemComprovante1aVia.Text )

    else if (cbxImpressaoViaCliente.ItemIndex = 1) then   // Perguntar
    begin
      TDialogService.MessageDialog( 'Imprimir Via do Cliente ?',
                                    TMsgDlgType.mtConfirmation,
                                    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
                                    TMsgDlgBtn.mbYes, 0,
        procedure(const AResult: TModalResult)
        var
          AStatus: LongWord;
        begin
          if (AResult = mrYes) then
            ImprimirRelatorio( ATEFResp.ImagemComprovante1aVia.Text );
        end);
    end;
  end;
end;

procedure TFrTEFDemoAndroid.AdicionarNumeroNaVenda(ANum: String);
var
  TextVal: string;
begin
  if fNovoValor then
  begin
    TextVal := '';
    fNovoValor := False;
  end
  else
    TextVal := OnlyNumber(edtValorVenda.Text);

  ValorVenda := StrToIntDef(TextVal+ANum, 0)/100;
end;

procedure TFrTEFDemoAndroid.LerConfiguracao;
var
  IniFile, NomePorta: string;
  Ini: TIniFile;
  cUF: Integer;
begin
  IniFile := CalcularNomeArqConfiguracao;
  Ini := TIniFile.Create(IniFile, TEncoding.UTF8);
  try
    // Configurações do ACBrPosPrinter //
    if cbxImpressorasBth.Items.Count < 1 then
      CarregarImpressorasBth;

    rbClasseInterna.IsChecked := INI.ReadBool('PosPrinter','ClasseInterna', True);
    rbClasseExterna.IsChecked := not rbClasseInterna.IsChecked;
    rbMudaClasseImpressora(nil);
    NomePorta := Ini.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta);
    cbxImpressorasBth.ItemIndex := cbxImpressorasBth.Items.IndexOf(NomePorta);
    cbxModelo.ItemIndex := Ini.ReadInteger('PosPrinter','Modelo', 1);
    cbxPagCodigo.ItemIndex := Ini.ReadInteger('PosPrinter','PaginaDeCodigo',Integer(ACBrPosPrinter1.PaginaDeCodigo));
    seColunas.Value := Ini.ReadInteger('PosPrinter','Colunas', 32);
    seEspLinhas.Value := Ini.ReadInteger('PosPrinter','EspacoEntreLinhas', ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value := Ini.ReadInteger('PosPrinter','LinhasPular', ACBrPosPrinter1.LinhasEntreCupons);

    swFonteCondensada.IsChecked := Ini.ReadBool('PosPrinter.Fonte', 'Condensada', True);
    swFonteNegrito.IsChecked := Ini.ReadBool('PosPrinter.Fonte', 'Negrito', False);

    // Configurações do ACBrTEFAndroid //                              -
    edtNomeSwHouse.Text := Ini.ReadString('SwHouse', 'NomeFantasia', '');
    edtCNPJSwHouse.Text := Ini.ReadString('SwHouse', 'CNPJ', '');

    edtNomeAplicacao.Text := Ini.ReadString('Aplicacao', 'NomeAplicacao', '');
    edtVersaoAplicacao.Text := Ini.ReadString('Aplicacao', 'VersaoAplicacao', '');

    edtNomeFantasiaEstabelecimento.Text := Ini.ReadString('Estabelecimento', 'NomeFantasia', '');
    edtCNPJEstabelecimento.Text := Ini.ReadString('Estabelecimento', 'CNPJ', '');

    swConfirmacaoManual.IsChecked := Ini.ReadBool('Geral', 'ConfirmacaoManual', False);
    swInterfaceAlternativa.IsChecked := Ini.ReadBool('Geral', 'InterfaceAlternativa', True);
    swMenuAdministrativo.IsChecked := Ini.ReadBool('Geral', 'MenuAdministrativo', False);
    cbxTransacaoPendente.ItemIndex := INI.ReadInteger('Geral', 'TransacaoPendente', 0);
    cbxImpressaoViaCliente.ItemIndex := Ini.ReadInteger('Geral', 'ImpressaoViaCliente', 1);

    swSuportaTroco.IsChecked := Ini.ReadBool('Capacidades', 'SuportaTroco', False);
    swSuportaDesconto.IsChecked := Ini.ReadBool('Capacidades', 'SuportaDesconto', True);
    swViasDiferenciadas.IsChecked := Ini.ReadBool('Capacidades', 'ViasDiferenciadas', True);
    swViaConsumidorReduzida.IsChecked := Ini.ReadBool('Capacidades', 'ViaConsumidorReduzida', False);
    swSuportaAbaterVouchere.IsChecked := Ini.ReadBool('Capacidades', 'SuportaAbaterVouchere', False);

    cbxModalidadePagto.ItemIndex := Ini.ReadInteger('Transacao', 'ModalidadePagto', 0);
    cbxTipoFinanciamento.ItemIndex := Ini.ReadInteger('Transacao', 'TipoFinanciamento', 0);
    cbxTipoCartao.ItemIndex := Ini.ReadInteger('Transacao', 'TipoCartao', 0);
    cbxProvedor.ItemIndex := Ini.ReadInteger('Transacao', 'Provedor', 0);
    cbxMoeda.ItemIndex := Ini.ReadInteger('Transacao', 'Moeda', 0);
  finally
    Ini.Free;
  end;

  // Calculando status da exibição das imagens de Alerta de configuração //
  edtNomeSwHouseTyping(nil);
  edtCNPJSwHouseTyping(nil);
  edtNomeAplicacaoTyping(nil);
  edtVersaoAplicacaoTyping(nil);
  edtNomeFantasiaEstabelecimentoTyping(nil);
  edtCNPJEstabelecimentoTyping(nil);
end;

procedure TFrTEFDemoAndroid.GravarConfiguracao;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := CalcularNomeArqConfiguracao;
  Ini := TIniFile.Create(IniFile, TEncoding.UTF8);
  try
    // configurações do ACBrPosPrinter //
    INI.WriteBool('PosPrinter','ClasseInterna', rbClasseInterna.IsChecked);
    if Assigned(cbxImpressorasBth.Selected) then
      INI.WriteString('PosPrinter','Porta', cbxImpressorasBth.Selected.Text);

    INI.WriteInteger('PosPrinter','Modelo', cbxModelo.ItemIndex);
    INI.WriteInteger('PosPrinter','PaginaDeCodigo',cbxPagCodigo.ItemIndex);
    INI.WriteInteger('PosPrinter','Colunas', Trunc(seColunas.Value) );
    INI.WriteInteger('PosPrinter','EspacoEntreLinhas', Trunc(seEspLinhas.Value) );
    INI.WriteInteger('PosPrinter','LinhasPular', Trunc(seLinhasPular.Value) );

    Ini.WriteBool('PosPrinter.Fonte', 'Condensada', swFonteCondensada.IsChecked);
    Ini.WriteBool('PosPrinter.Fonte', 'Negrito', swFonteNegrito.IsChecked);

    // Configurações do ACBrTEFAndroid //                              -
    Ini.WriteString('SwHouse', 'NomeFantasia', edtNomeSwHouse.Text);
    Ini.WriteString('SwHouse', 'CNPJ', edtCNPJSwHouse.Text);

    Ini.WriteString('Aplicacao', 'NomeAplicacao', edtNomeAplicacao.Text);
    Ini.WriteString('Aplicacao', 'VersaoAplicacao', edtVersaoAplicacao.Text);

    Ini.WriteString('Estabelecimento', 'NomeFantasia', edtNomeFantasiaEstabelecimento.Text);
    Ini.WriteString('Estabelecimento', 'CNPJ', edtCNPJEstabelecimento.Text);

    Ini.WriteBool('Geral', 'ConfirmacaoManual', swConfirmacaoManual.IsChecked);
    Ini.WriteBool('Geral', 'InterfaceAlternativa', swInterfaceAlternativa.IsChecked);
    Ini.WriteBool('Geral', 'MenuAdministrativo', swMenuAdministrativo.IsChecked);
    INI.WriteInteger('Geral', 'TransacaoPendente', cbxTransacaoPendente.ItemIndex);
    Ini.WriteInteger('Geral', 'ImpressaoViaCliente', cbxImpressaoViaCliente.ItemIndex);

    Ini.WriteBool('Capacidades', 'SuportaTroco', swSuportaTroco.IsChecked);
    Ini.WriteBool('Capacidades', 'SuportaDesconto', swSuportaDesconto.IsChecked);
    Ini.WriteBool('Capacidades', 'ViasDiferenciadas', swViasDiferenciadas.IsChecked);
    Ini.WriteBool('Capacidades', 'ViaConsumidorReduzida', swViaConsumidorReduzida.IsChecked);
    Ini.WriteBool('Capacidades', 'SuportaAbaterVouchere', swSuportaAbaterVouchere.IsChecked);
  finally
    Ini.Free;
  end;

  GravarConfiguracaoTransacao;
end;

procedure TFrTEFDemoAndroid.GravarConfiguracaoTransacao;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := CalcularNomeArqConfiguracao;
  Ini := TIniFile.Create(IniFile, TEncoding.UTF8);
  try
    Ini.WriteInteger('Transacao', 'ModalidadePagto', cbxModalidadePagto.ItemIndex);
    Ini.WriteInteger('Transacao', 'TipoFinanciamento', cbxTipoFinanciamento.ItemIndex);
    Ini.WriteInteger('Transacao', 'TipoCartao', cbxTipoCartao.ItemIndex);
    Ini.WriteInteger('Transacao', 'Provedor', cbxProvedor.ItemIndex);
    Ini.WriteInteger('Transacao', 'Moeda', cbxMoeda.ItemIndex);
  finally
    Ini.Free;
  end;
end;

procedure TFrTEFDemoAndroid.LimparInterfacePrincipal;
begin
  if (tabsPrincipal.ActiveTab <> tabTeste) then
    VoltarParaTestes;

  layoutVenda.Parent := tabVenda;
  btVendaPagar.IsPressed := False;
  btVendaPagar.Text := CBOTAO_VENDA;

  layoutAdmin.Parent := tabAdmin;
  btAdmin.IsPressed := False;

  layoutEstorno.Parent := tabEstorno;
  btEstorno.IsPressed := False;
  btEstorno.Text := CBOTAO_ESTORNO;
end;

procedure TFrTEFDemoAndroid.LimparValorVenda;
begin
  ValorVenda := 0;
  fValorOperacao := 0;
  fOperacao := '';
  fNovoValor := True;
end;

procedure TFrTEFDemoAndroid.MemorizarOperacao(Operacao: String);
begin
  RealizarOperacaoMemorizada;

  fValorOperacao := ValorVenda;
  fOperacao := Operacao;
  fNovoValor := True;
end;

procedure TFrTEFDemoAndroid.MostrarMenuAdministrativoPayGo;
var
  AItem: TListBoxItem;
  i: Integer;
begin
  if (lbAdmin.Items.Count < 1) then
  begin
    for i := Low(CITENS_MENU_ADMIN) to High(CITENS_MENU_ADMIN) do
    begin
      AItem := TListBoxItem.Create(lbAdmin);
      AItem.StyledSettings := AItem.StyledSettings - [TStyledSetting.Other];
      AItem.TextSettings.HorzAlign := TTextAlign.Leading;
      AItem.TextSettings.VertAlign := TTextAlign.Center;
      AItem.TextSettings.WordWrap := True;
      AItem.TextSettings.FontColor := TAlphaColorRec.White;
      AItem.TextSettings.Font.Size := 16;
      AItem.Text := CITENS_MENU_ADMIN[i];
      lbAdmin.AddObject(AItem);

      lbAdmin.ItemIndex := 2; // 'ADMINISTRATIVA'
    end;
  end;

  LimparInterfacePrincipal;
  layoutAdmin.Parent := layoutMain;
  lbAdmin.SetFocus;
  btAdmin.IsPressed := True;
end;

procedure TFrTEFDemoAndroid.MostrarTelaEstorno;
begin
  LimparInterfacePrincipal;
  edtEstornoNSU.Text := '';
  edtEstornoCodAutorizacao.Text := '';
  edtEstornoDataTransacao.DateTime := Now;
  edtEstornoValorTransacao.Text := '';
  edtEstornoValorTransacaoTyping(nil);

  layoutEstorno.Parent := layoutMain;
  btEstorno.Text := CBOTAO_ESTORNAR;
  btEstorno.IsPressed := True;
  edtEstornoNSU.SetFocus;
end;

procedure TFrTEFDemoAndroid.MostrarTelaUltimaTransacao;
begin
  tabsPrincipal.SetActiveTabWithTransition( tabUltTransacao,
                                            TTabTransition.Slide,
                                            TTabTransitionDirection.Normal ) ;
  tabsUltimaTransacao.TabIndex := 0;
end;

procedure TFrTEFDemoAndroid.MostrarTelaVenda;
begin
  LimparInterfacePrincipal;
  layoutVenda.Parent := layoutMain;
  btVendaPagar.Text := CBOTAO_PAGAR;
  btVendaPagar.IsPressed := True;
end;

procedure TFrTEFDemoAndroid.imgConfigClick(Sender: TObject);
begin
  MostrarTelaConfiguracao;
end;

procedure TFrTEFDemoAndroid.ImprimirRelatorio(ATexto: String);
var
  ComandoInicial, ComandoFinal: string;
begin
  InicializarPosPrinter;

  ComandoInicial := '</zera>';
  if swFonteCondensada.IsChecked then
    ComandoInicial := ComandoInicial + '<c>';
  if swFonteNegrito.IsChecked then
    ComandoInicial := ComandoInicial + '<n>';

  ComandoFinal := '</lf></corte_total>';
  ACBrPosPrinter1.Imprimir(ComandoInicial + ATexto + ComandoFinal);
end;

procedure TFrTEFDemoAndroid.InicializarPosPrinter;
begin
  if ACBrPosPrinter1.Ativo then
    Exit;

  if (cbxImpressorasBth.Selected = nil) or (cbxModelo.Selected = nil) then
  begin
    Toast('Configurar a Impressora');
    MostrarTelaConfiguracao(1);
    Abort;
  end;

  ACBrPosPrinter1.Ativar;
end;

procedure TFrTEFDemoAndroid.IniciarTransacaoTEF;
begin
  memoLog.Lines.Clear;
  InicializarTEF;
end;

procedure TFrTEFDemoAndroid.InicializarTEF;
begin
  if ACBrTEFAndroid1.Inicializado then
    Exit;

  if imgErrorNomeAplicacao.Visible or imgErrorVersaoAplicacao.Visible or
     imgErrorCNPJSwHouse.Visible or imgErrorNomeSwHouse.Visible or
     imgErrorNomeFantasia.Visible or imgErrorCNPJEstabelecimento.Visible then
  begin
    Toast('Configurar o TEF');
    MostrarTelaConfiguracao(0);
    Abort;
  end;

  AplicarConfiguracoes;
  if FileExists(ACBrTEFAndroid1.ArqLOG) then
    DeleteFile(ACBrTEFAndroid1.ArqLOG);

  ACBrTEFAndroid1.Inicializar;
end;

procedure TFrTEFDemoAndroid.lbAdminItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  if Assigned(Item) then
    ExecutarAdministrativo(Item.Text);
end;

procedure TFrTEFDemoAndroid.lblTituloTestesClick(Sender: TObject);
var
  AMsg: string;
begin
  AMsg := 'TEF é com o Projeto ACBr'+sLineBreak+sLineBreak+
          'https://projetoacbr.com.br/tef/';
  TDialogService.ShowMessage(AMsg);
end;

procedure TFrTEFDemoAndroid.MostrarTelaConfiguracao(Aba: Integer = 0);
begin
  tabsPrincipal.SetActiveTabWithTransition( tabConfig,
                                            TTabTransition.Slide,
                                            TTabTransitionDirection.Normal ) ;

  tabsConfig.TabIndex := Aba;
end;

procedure TFrTEFDemoAndroid.FecharConfiguracao;
begin
  GravarConfiguracao;
  AplicarConfiguracoes;
  VoltarParaTestes;
end;

end.


