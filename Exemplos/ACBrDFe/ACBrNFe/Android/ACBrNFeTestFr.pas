{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrNFeTestFr;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.ImageList, System.Actions, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.TabControl,
  FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.ImgList, FMX.Gestures, FMX.Objects, FMX.ScrollBox,
  FMX.Memo, FMX.ListBox, FMX.EditBox, FMX.SpinBox, FMX.Edit, FMX.Layouts,
  FMX.Ani, FMX.Effects, FMX.ActnList,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView,  FMX.VirtualKeyboard,
  FileSelectFrame,
  ACBrPosPrinterElginE1Service,
  {$IfDef ANDROID}
   ACBrPosPrinterElginE1Lib,
   ACBrPosPrinterGEDI,
  {$EndIf}
  ACBrIBGE, ACBrSocket, ACBrCEP,
  ACBrDFeReport, ACBrDFeDANFeReport, ACBrNFeDANFEClass, ACBrNFeDANFeESCPOS,
  ACBrPosPrinter, ACBrDFe, ACBrNFe, ACBrBase, ACBrMail, FMX.Memo.Types;

type
  TACBrNFCeTestOperacao = (opLivre, opErro, opGerando, opAssinando, opGravando, opTransmitindo, opImprimindo, opEmail);

  TACBrNFCeTestForm = class(TForm)
    GestureManager1: TGestureManager;
    tabsPrincipal: TTabControl;
    tabConfig: TTabItem;
    ToolBar1: TToolBar;
    lblTituloConfig: TLabel;
    tabTeste: TTabItem;
    ToolBar2: TToolBar;
    lblTituloTestes: TLabel;
    btnBack: TSpeedButton;
    ImageList1: TImageList;
    StyleBook1: TStyleBook;
    ACBrMail1: TACBrMail;
    tabLog: TTabItem;
    mLog: TMemo;
    ToolBar3: TToolBar;
    Label13: TLabel;
    SpeedButton1: TSpeedButton;
    GridPanelLayout9: TGridPanelLayout;
    btnLimpar: TCornerButton;
    btnSobre: TCornerButton;
    ACBrNFe1: TACBrNFe;
    ACBrPosPrinter1: TACBrPosPrinter;
    ACBrNFeDANFeESCPOS1: TACBrNFeDANFeESCPOS;
    tabsConfig: TTabControl;
    tabConfigNFCe: TTabItem;
    tabConfigPosPrinter: TTabItem;
    tabConfigEmail: TTabItem;
    lbConfigEmail: TListBox;
    lbiHeaderEmailRemetente: TListBoxGroupHeader;
    lbiEmailFrom: TListBoxItem;
    gpEmailFrom: TGridPanelLayout;
    Label5: TLabel;
    edtEmailFrom: TEdit;
    Label6: TLabel;
    edtEmailFromName: TEdit;
    lbiHeaderEmailSMTP: TListBoxGroupHeader;
    lbiEmailSMTP: TListBoxItem;
    gpEmailSMTP: TGridPanelLayout;
    Label8: TLabel;
    edtEmailHost: TEdit;
    Label9: TLabel;
    chkEmailTLS: TSwitch;
    Label10: TLabel;
    sbEmailPort: TSpinBox;
    Label11: TLabel;
    chkEmailSSL: TSwitch;
    lbiHeaderEmailUsuarioLogin: TListBoxGroupHeader;
    lbiEmailUsuarioLogin: TListBoxItem;
    gpEmailUsuarioLogin: TGridPanelLayout;
    Label2: TLabel;
    Label3: TLabel;
    edtEmailUser: TEdit;
    edtEmailPassword: TEdit;
    sbEmailMostrarSenha: TSpeedButton;
    lbiHeaderEmailCharset: TListBoxGroupHeader;
    lbiEmailCharSet: TListBoxItem;
    gpEmailCharset: TGridPanelLayout;
    Label1: TLabel;
    Label4: TLabel;
    cbEmailDefaultCharset: TComboBox;
    cbEmailIdeCharSet: TComboBox;
    lbConfigImpressora: TListBox;
    ListBoxGroupHeader9: TListBoxGroupHeader;
    lbiConfPrinterImpressoras: TListBoxItem;
    cbxImpressorasBth: TComboBox;
    btnProcurarBth: TCornerButton;
    chbTodasBth: TCheckBox;
    ListBoxGroupHeader10: TListBoxGroupHeader;
    lbiConfPrinterModelos: TListBoxItem;
    ListBoxGroupHeader11: TListBoxGroupHeader;
    lbiConfPrinterLarguraEspacejamento: TListBoxItem;
    gpConfPrinterLarguraEspacejamento: TGridPanelLayout;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    seColunas: TSpinBox;
    seEspLinhas: TSpinBox;
    seLinhasPular: TSpinBox;
    ListBoxGroupHeader12: TListBoxGroupHeader;
    lbiConfPrinterLogoTipo: TListBoxItem;
    gpConfPrinterLogoTipo: TGridPanelLayout;
    Label19: TLabel;
    Label20: TLabel;
    cbImprimirLogo: TCheckBox;
    seKC1: TSpinBox;
    seKC2: TSpinBox;
    gpBotoesConfig: TGridPanelLayout;
    btLerConfig: TCornerButton;
    btSalvarConfig: TCornerButton;
    lbiConfPrinterDiversos: TListBoxItem;
    GridPanelLayout12: TGridPanelLayout;
    ListBoxGroupHeader6: TListBoxGroupHeader;
    cbLogoLateral: TCheckBox;
    GridPanelLayout13: TGridPanelLayout;
    cbxModelo: TComboBox;
    GridPanelLayout14: TGridPanelLayout;
    Label21: TLabel;
    Label22: TLabel;
    cbxPagCodigo: TComboBox;
    cbImprimir1Linha: TCheckBox;
    lbConfNFCe: TListBox;
    lbiHeaderCertificado: TListBoxGroupHeader;
    lbiConfCert: TListBoxItem;
    gpConfCert: TGridPanelLayout;
    Label23: TLabel;
    edtConfCertURL: TEdit;
    lbiHeaderWebServiceNFCe: TListBoxGroupHeader;
    lbiWebServiceNFCe: TListBoxItem;
    btnCertInfo: TCornerButton;
    Label25: TLabel;
    edtConfCertSenha: TEdit;
    sbCertVerSenha: TSpeedButton;
    Label24: TLabel;
    edtConfCertPFX: TEdit;
    lbiHeaderToken: TListBoxGroupHeader;
    lbiToken: TListBoxItem;
    gpToken: TGridPanelLayout;
    edtTokenID: TEdit;
    edtTokenCSC: TEdit;
    lbiHeaderConfCertProxy: TListBoxGroupHeader;
    lbiProxy: TListBoxItem;
    gpProxy: TGridPanelLayout;
    Label28: TLabel;
    edtProxyHost: TEdit;
    Label30: TLabel;
    sbProxyPort: TSpinBox;
    Label29: TLabel;
    edtProxyUser: TEdit;
    Label31: TLabel;
    edtProxyPass: TEdit;
    seProxyVerSenha: TSpeedButton;
    gpWebServiceNFCe: TGridPanelLayout;
    Label33: TLabel;
    sbWebServiceTimeout: TSpinBox;
    Label34: TLabel;
    Label35: TLabel;
    swWebServiceAmbiente: TSwitch;
    lAmbiente: TLabel;
    cbxWebServiceUF: TComboBox;
    cbxWebServiceSSLType: TComboBox;
    lbiHeaderEmitente: TListBoxGroupHeader;
    lbiEmitente: TListBoxItem;
    lbEmitente: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    ListBoxItem5: TListBoxItem;
    ListBoxItem6: TListBoxItem;
    ListBoxItem7: TListBoxItem;
    ListBoxItem8: TListBoxItem;
    ListBoxItem9: TListBoxItem;
    ListBoxItem10: TListBoxItem;
    ListBoxItem11: TListBoxItem;
    ListBoxItem12: TListBoxItem;
    edtEmitCNPJ: TEdit;
    edtEmitIE: TEdit;
    edtEmitRazao: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitCEP: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitComp: TEdit;
    edtEmitBairro: TEdit;
    cbxEmitUF: TComboBox;
    cbxEmitCidade: TComboBox;
    sbCertConfAcharPFX: TSpeedButton;
    sbAcharCEP: TSpeedButton;
    lEmitcUF: TLabel;
    lEmitcMun: TLabel;
    ACBrCEP1: TACBrCEP;
    ACBrIBGE1: TACBrIBGE;
    lWait: TLayout;
    AniIndicator1: TAniIndicator;
    Rectangle1: TRectangle;
    RoundRect1: TRoundRect;
    lMsgAguarde: TLabel;
    ShadowEffect1: TShadowEffect;
    imgErrorCep: TImage;
    imgErrorCNPJ: TImage;
    imgErrorTelefone: TImage;
    imgErrorUF: TImage;
    imgErrorCidade: TImage;
    laIDCSC: TLayout;
    lIDCSC: TLabel;
    imgErrorIDCSC: TImage;
    laCSC: TLayout;
    lCSC: TLabel;
    imgErrorCSC: TImage;
    laWebServiceUF: TLayout;
    lWebServiceUF: TLabel;
    imgErrorWebServiceUF: TImage;
    imgErrorCert: TImage;
    imgErrorRazaoSocial: TImage;
    tiStartUp: TTimer;
    tabMenu: TTabItem;
    Image1: TImage;
    ToolBar4: TToolBar;
    Label26: TLabel;
    ShadowEffect2: TShadowEffect;
    GridPanelLayout1: TGridPanelLayout;
    laBtnTestes: TLayout;
    iTestes: TImage;
    ShadowEffect3: TShadowEffect;
    lTestes: TLabel;
    laBtnConfiguracao: TLayout;
    Image3: TImage;
    ShadowEffect4: TShadowEffect;
    Label32: TLabel;
    laBtnLogs: TLayout;
    Image4: TImage;
    ShadowEffect5: TShadowEffect;
    Label36: TLabel;
    laBtnNotasEmtidas: TLayout;
    Image5: TImage;
    ShadowEffect6: TShadowEffect;
    Label37: TLabel;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    FloatAnimation3: TFloatAnimation;
    FloatAnimation4: TFloatAnimation;
    SpeedButton2: TSpeedButton;
    tabXMLs: TTabItem;
    ToolBar5: TToolBar;
    Label38: TLabel;
    SpeedButton3: TSpeedButton;
    fraXMLs: TFrameFileSelect;
    lbTestes: TListBox;
    lbhConsultas: TListBoxGroupHeader;
    lbTestesConsultas: TListBoxItem;
    GridPanelLayout3: TGridPanelLayout;
    btnStatusServico: TButton;
    btnCadastro: TButton;
    btnChave: TButton;
    lbiTestesXML: TListBoxItem;
    GridPanelLayout2: TGridPanelLayout;
    btnGerarNFCe: TButton;
    ToolBar6: TToolBar;
    GridPanelLayout4: TGridPanelLayout;
    Button1: TButton;
    btnEnviar: TButton;
    btnImprimir: TButton;
    btnValidarAssinatura: TButton;
    btnEnviarEmail: TButton;
    btnVerificarSchema: TButton;
    libDestinatarioAssunto: TListBoxItem;
    GridPanelLayout6: TGridPanelLayout;
    Label15: TLabel;
    edtAddressEmail: TEdit;
    Label39: TLabel;
    edtAddressName: TEdit;
    Label40: TLabel;
    edtSubject: TEdit;
    ListBoxGroupHeader7: TListBoxGroupHeader;
    ListBoxGroupHeader8: TListBoxGroupHeader;
    lbiMensagem: TListBoxItem;
    mAltBody: TMemo;
    GridPanelLayout7: TGridPanelLayout;
    btnVerNotas: TButton;
    btnVerLogs: TButton;
    lbiHeaderNFCe: TListBoxGroupHeader;
    lbiConfigNFCe: TListBoxItem;
    GridPanelLayout5: TGridPanelLayout;
    Label7: TLabel;
    Label12: TLabel;
    Label14: TLabel;
    sbProximaNFCe: TSpinBox;
    sbLote: TSpinBox;
    swOnLine: TSwitch;
    lOnLine: TLabel;
    edtConsultaParametro: TEdit;
    ListBoxItem13: TListBoxItem;
    cbxTipoEmpresa: TComboBox;
    btnApagarXML: TButton;
    btnVerXML: TButton;
    btnLogs: TButton;
    lbOperacao: TListBoxGroupFooter;
    imgOperacao: TImage;
    swEnviarNFCe: TSwitch;
    lEnviarNFCe: TLabel;
    swImprimirNFCe: TSwitch;
    lImprimirNFCe: TLabel;
    swEmailNFCe: TSwitch;
    lEmailNFCe: TLabel;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    lbiClasse: TListBoxItem;
    GridPanelLayout8: TGridPanelLayout;
    rbClasseInterna: TRadioButton;
    rbClasseExterna: TRadioButton;
    cbImprimirDescAcres: TCheckBox;
    cbQRCodeLateral: TCheckBox;
    GridPanelLayout10: TGridPanelLayout;
    Label41: TLabel;
    seLarguraModulo: TSpinBox;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure btnBackClick(Sender: TObject);
    procedure btLerConfigClick(Sender: TObject);
    procedure btSalvarConfigClick(Sender: TObject);
    procedure ACBrMail1BeforeMailProcess(Sender: TObject);
    procedure ACBrMail1MailException(const AMail: TACBrMail; const E: Exception;
      var ThrowIt: Boolean);
    procedure ACBrMail1MailProcess(const AMail: TACBrMail;
      const aStatus: TMailStatus);
    procedure sbEmailMostrarSenhaClick(Sender: TObject);
    procedure btnLimparClick(Sender: TObject);
    procedure sbCertAcharPFXClick(Sender: TObject);
    procedure sbCertVerSenhaClick(Sender: TObject);
    procedure seProxyVerSenhaClick(Sender: TObject);
    procedure sbAcharCEPClick(Sender: TObject);
    procedure cbxEmitUFChange(Sender: TObject);
    procedure cbxEmitCidadeChange(Sender: TObject);
    procedure EditApenasNumeros(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
    procedure edtEmitCEPTyping(Sender: TObject);
    procedure edtEmitFoneTyping(Sender: TObject);
    procedure edtEmitCEPValidate(Sender: TObject; var Text: string);
    procedure cbxWebServiceUFChange(Sender: TObject);
    procedure btnProcurarBthClick(Sender: TObject);
    procedure FormVirtualKeyboardHidden(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure edtEnterScrollableControl(Sender: TObject);
    procedure edtExitScrollableControl(Sender: TObject);
    procedure imgErrorCNPJClick(Sender: TObject);
    procedure edtEmitRazaoTyping(Sender: TObject);
    procedure imgErrorRazaoSocialClick(Sender: TObject);
    procedure imgErrorTelefoneClick(Sender: TObject);
    procedure imgErrorUFClick(Sender: TObject);
    procedure imgErrorCidadeClick(Sender: TObject);
    procedure imgErrorIDCSCClick(Sender: TObject);
    procedure imgErrorCSCClick(Sender: TObject);
    procedure imgErrorCertClick(Sender: TObject);
    procedure edtTokenIDTyping(Sender: TObject);
    procedure edtTokenCSCTyping(Sender: TObject);
    procedure imgErrorWebServiceUFClick(Sender: TObject);
    procedure imgErrorCepClick(Sender: TObject);
    procedure tiStartUpTimer(Sender: TObject);
    procedure edtEmitCNPJTyping(Sender: TObject);
    procedure edtConfCertURLTyping(Sender: TObject);
    procedure laBtnTestesClick(Sender: TObject);
    procedure laBtnConfiguracaoClick(Sender: TObject);
    procedure laBtnLogsClick(Sender: TObject);
    procedure laBtnNotasEmtidasClick(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure btnStatusServicoClick(Sender: TObject);
    procedure ACBrMail1AfterMailProcess(Sender: TObject);
    procedure FormVirtualKeyboardShown(Sender: TObject;
      KeyboardVisible: Boolean; const Bounds: TRect);
    procedure btnCadastroClick(Sender: TObject);
    procedure edtConfCertURLExit(Sender: TObject);
    procedure btnChaveClick(Sender: TObject);
    procedure btnGerarNFCeClick(Sender: TObject);
    procedure btnVerNotasClick(Sender: TObject);
    procedure btnValidarAssinaturaClick(Sender: TObject);
    procedure btnVerificarSchemaClick(Sender: TObject);
    procedure btnCertInfoClick(Sender: TObject);
    procedure btnSobreClick(Sender: TObject);
    procedure btnVerXMLClick(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure swOnLineSwitch(Sender: TObject);
    procedure swWebServiceAmbienteSwitch(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
    procedure btnEnviarEmailClick(Sender: TObject);
    procedure edtAddressEmailEnter(Sender: TObject);
    procedure btnVerLogsClick(Sender: TObject);
    procedure seColunasEnter(Sender: TObject);
    procedure fraXMLslvFileBrowseDeletingItem(Sender: TObject; AIndex: Integer;
      var ACanDelete: Boolean);
    procedure btnApagarXMLClick(Sender: TObject);
    procedure tabsPrincipalChange(Sender: TObject);
    procedure swEnviarNFCeSwitch(Sender: TObject);
    procedure swImprimirNFCeSwitch(Sender: TObject);
    procedure swEmailNFCeSwitch(Sender: TObject);
    procedure rbMudaClasseImpressora(Sender: TObject);
    procedure cbxModeloChange(Sender: TObject);
  private
    { Private declarations }
    fE1Printer: TACBrPosPrinterElginE1Service;
    {$IfDef ANDROID}
     fGEDIPrinter: TACBrPosPrinterGEDI;
     fE1Lib: TACBrPosPrinterElginE1Lib;
    {$EndIf}

    FVKService: IFMXVirtualKeyboardService;
    FcMunList: TStringList;
    FcUF: Integer;
    FScrollBox: TCustomScrollBox;
    FControlToCenter: TControl;
    FTabList: TList<TTabItem>;
    FOperacaoNFCe: TACBrNFCeTestOperacao;

    procedure TabForward(ANewTab: TTabItem);
    procedure TabBack;

    function CalcularNomeArqConfiguracao: String;
    procedure CarregarModelosExternos;
    procedure CarregarModelosInternos;
    procedure LerConfiguracao;
    procedure GravarConfiguracao;

    procedure DescompactarSchemas;
    procedure VerificarErrosDeConfiguracao;
    procedure VerificarErrosDeConfiguracaoImpressora;
    procedure VerificarErrosDeConfiguracaoEmail;
    procedure ConfigurarACBr;
    procedure ConfigurarACBrMail;
    procedure ConfigurarACBrPosPrinter;
    procedure ConfigurarACBrDANFCe;
    procedure ConfigurarACBrNFe;

    procedure CarregarImpressorasBth;
    procedure ExibirLogs;
    procedure ExibirXMLs;

    procedure PedirPermissoes;
    procedure IniciarTelaDeEspera(const AMsg: String = '');
    procedure TerminarTelaDeEspera;
    procedure ConsultarCEP;
    procedure CarregarListaDeCidades;
    function ValidarEditsCertificado(const URL, PFX, Pass: String): Boolean;

    procedure AjustarScroll(AControl: TControl; AScrollBox: TCustomScrollBox);
    procedure CriarNFCe;
    function LerXMLNFCe(const ArquivoXML: String): Integer;
    procedure EnviarNFCe;
    procedure ImprimirNFCe;
    procedure EnviarEmailNFCe;
    function SubstituirVariaveisEmail(const AText: String): String;

    procedure AppExceptionHandle(Sender: TObject; E: Exception);
    procedure SetOperacaoNFCe(const Value: TACBrNFCeTestOperacao);
    procedure ExibirErroImpressaoE1(const MsgErro: string);
  public
    { Public declarations }
    property OperacaoNFCe: TACBrNFCeTestOperacao read FOperacaoNFCe write SetOperacaoNFCe;
  end;

var
  ACBrNFCeTestForm: TACBrNFCeTestForm;

implementation

uses
  System.typinfo, System.IniFiles, System.StrUtils, System.Permissions, System.IOUtils,
  System.DateUtils,
  {$IfDef ANDROID}
  Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.IOUtils,
  Androidapi.JNI.Widget, FMX.Helpers.Android,
  {$EndIf}
  FMX.DialogService.Async, FMX.Platform, Xml.XMLDoc, System.Zip, System.Math,
  FileSelectFr,
  ssl_openssl_lib, blcksock, ACBrLibXml2,
  pcnConversao, pcnConversaoNFe,
  ACBrConsts, ACBrValidador,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.DateTime,
  ACBrUtil.Math,
  ACBrDFeSSL, ACBrDFeUtil;

{$R *.fmx}

procedure Toast(const AMsg: string; ShortDuration: Boolean = True);
var
  ToastLength: Integer;
begin
  {$IfNDef ANDROID}
   TDialogServiceAsync.ShowMessage(AMsg);
  {$Else}
   if ShortDuration then
     ToastLength := TJToast.JavaClass.LENGTH_SHORT
   else
     ToastLength := TJToast.JavaClass.LENGTH_LONG;

   TJToast.JavaClass.makeText(SharedActivityContext, StrToJCharSequence(AMsg), ToastLength).show;
   Application.ProcessMessages;
  {$EndIf}
end;

procedure TACBrNFCeTestForm.FormCreate(Sender: TObject);
var
  m: TMailCharset;
  n: TACBrPosPrinterModelo;
  o: TACBrPosPaginaCodigo;
  p: TSSLType;
  q: TpcnCRT;
begin
  FTabList := TList<TTabItem>.Create;
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FVKService));
  FcMunList := TStringList.Create;
  FcUF := 0;
  FScrollBox := nil;
  FControlToCenter := nil;

  // Criando Classes de Impressoras Externas //
  fE1Printer := TACBrPosPrinterElginE1Service.Create(ACBrPosPrinter1);
  fE1Printer.Modelo := TElginE1Printers.prnSmartPOS;
  {$IfDef ANDROID}
  fE1Printer.OnErroImpressao := ExibirErroImpressaoE1;
  fGEDIPrinter := TACBrPosPrinterGEDI.Create(ACBrPosPrinter1);
  fE1Lib := TACBrPosPrinterElginE1Lib.Create(ACBrPosPrinter1);
  fE1Lib.Modelo := TElginE1LibPrinters.prnM8;
  {$EndIf}

  imgErrorCep.Bitmap := ImageList1.Bitmap(TSizeF.Create(imgErrorCep.Width,imgErrorCep.Height),14);
  imgErrorCNPJ.Bitmap := imgErrorCep.Bitmap;
  imgErrorTelefone.Bitmap := imgErrorCep.Bitmap;
  imgErrorCidade.Bitmap := imgErrorCep.Bitmap;
  imgErrorUF.Bitmap := imgErrorCep.Bitmap;
  imgErrorIDCSC.Bitmap := imgErrorCep.Bitmap;
  imgErrorCSC.Bitmap := imgErrorCep.Bitmap;
  imgErrorWebServiceUF.Bitmap := imgErrorCep.Bitmap;
  imgErrorCert.Bitmap := imgErrorCep.Bitmap;
  imgErrorRazaoSocial.Bitmap := imgErrorCep.Bitmap;

  cbxWebServiceUF.ItemIndex := -1;
  cbxEmitUF.ItemIndex := -1;

  // Ajustando opções de Configuração de ACBrMail //
  cbEmailDefaultCharset.Items.Clear;
  for m := Low(TMailCharset) to High(TMailCharset) do
    cbEmailDefaultCharset.Items.Add(GetEnumName(TypeInfo(TMailCharset), integer(m)));
  cbEmailDefaultCharset.ItemIndex := 0;

  cbEmailIdeCharSet.Items.Assign(cbEmailDefaultCharset.Items);
  cbEmailIdeCharSet.ItemIndex := 0;

  // Ajustando Opções de Configuração de ACBrPosPrinter //
  cbxModelo.Items.Clear ;
  For n := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
    cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(n) ) ) ;

  cbxPagCodigo.Items.Clear ;
  For o := Low(TACBrPosPaginaCodigo) to High(TACBrPosPaginaCodigo) do
    cbxPagCodigo.Items.Add( GetEnumName(TypeInfo(TACBrPosPaginaCodigo), integer(o) ) ) ;

  cbxWebServiceSSLType.Items.Clear;
  for p := Low(TSSLType) to High(TSSLType) do
    cbxWebServiceSSLType.Items.Add( GetEnumName(TypeInfo(TSSLType), integer(p) ) );
  cbxWebServiceSSLType.ItemIndex := 0;

  cbxTipoEmpresa.Items.Clear ;
  For q := Low(TpcnCRT) to High(TpcnCRT) do
    cbxTipoEmpresa.Items.Add( GetEnumName(TypeInfo(TpcnCRT), integer(q) ) ) ;

  tabsConfig.First;
  tabsPrincipal.First;
  tabsPrincipal.TabPosition := TTabPosition.None;

  Application.OnException := AppExceptionHandle;
end;

procedure TACBrNFCeTestForm.FormDestroy(Sender: TObject);
begin
  FcMunList.Free;
  FTabList.Free;
  fE1Printer.Free;
  {$IfDef ANDROID}
  fGEDIPrinter.Free;
  fE1Lib.Free;
  {$EndIf}
end;

procedure TACBrNFCeTestForm.AppExceptionHandle(Sender: TObject; E: Exception);
begin
  mLog.Lines.Add('');
  mLog.Lines.Add('----- '+E.ClassName+' -----');
  mLog.Lines.Add(E.Message);
  mLog.Lines.Add('');
  Toast(E.Message);
  ExibirLogs;
end;

procedure TACBrNFCeTestForm.PedirPermissoes;
Var
  Ok: Boolean;
begin
  Ok := True;
  {$IfDef ANDROID}
  PermissionsService.RequestPermissions( [JStringToString(TJManifest_permission.JavaClass.BLUETOOTH),
                                          JStringToString(TJManifest_permission.JavaClass.BLUETOOTH_ADMIN),
                                          JStringToString(TJManifest_permission.JavaClass.BLUETOOTH_PRIVILEGED),
                                          JStringToString(TJManifest_permission.JavaClass.INTERNET)],
      {$IfDef DELPHI28_UP}
      procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
      {$Else}
      procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
      {$EndIf}
      var
        GR: TPermissionStatus;
      begin
        for GR in AGrantResults do
          if (GR <> TPermissionStatus.Granted) then
          begin
            Ok := False;
            Break;
          end;
      end );

  if not OK then
    raise EPermissionException.Create( 'Sem permissões para acesso a Internet');
  {$EndIf}
end;

procedure TACBrNFCeTestForm.rbMudaClasseImpressora(Sender: TObject);
begin
  if rbClasseInterna.IsChecked then
    CarregarModelosInternos
  else
    CarregarModelosExternos;
end;

procedure TACBrNFCeTestForm.CarregarModelosExternos;
begin
  cbxModelo.Items.Clear;
  cbxModelo.Items.Add('Elgin E1 Service');
  cbxModelo.Items.Add('Elgin E1 Lib');
  cbxModelo.Items.Add('Gertec GEDI');
  lbiConfPrinterImpressoras.Enabled := False;
end;

procedure TACBrNFCeTestForm.CarregarModelosInternos;
var
  m: TACBrPosPrinterModelo;
begin
  cbxModelo.Items.Clear;
  For m := Low(TACBrPosPrinterModelo) to High(TACBrPosPrinterModelo) do
     cbxModelo.Items.Add( GetEnumName(TypeInfo(TACBrPosPrinterModelo), integer(m) ) );

  lbiConfPrinterImpressoras.Enabled := True;
end;

procedure TACBrNFCeTestForm.sbAcharCEPClick(Sender: TObject);
var
  Erro: String;
begin
  if imgErrorCep.Visible then
  begin
    imgErrorCepClick(Sender);
    Exit;
  end;

  IniciarTelaDeEspera;
  TThread.CreateAnonymousThread(ConsultarCEP).Start;
end;

procedure TACBrNFCeTestForm.sbCertAcharPFXClick(Sender: TObject);
var
  FrSlect: TFileSelectForm;
begin
  FrSlect := TFileSelectForm.Create(Self);
  FrSlect.InitialDir := ApplicationPath;
  FrSlect.ShowHidden := True;
  FrSlect.FileMask := '*.pfx';

  FrSlect.ShowModal(
    procedure(ModalResult : TModalResult)
    var
      AFile: string;
    begin
        if ModalResult = mrOK then
        begin
          AFile := FrSlect.FileName;
          if AFile.IndexOf(ApplicationPath) = 0 then
            AFile := ExtractFileName(AFile);

          edtConfCertPFX.Text := AFile;
        end
      end
    );
end;

procedure TACBrNFCeTestForm.sbCertVerSenhaClick(Sender: TObject);
begin
  edtConfCertSenha.Password := not sbCertVerSenha.IsPressed;
end;

procedure TACBrNFCeTestForm.ACBrMail1AfterMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('EMail ['+Trim(TACBrMail(Sender).Subject)+']');
  mLog.Lines.Add('  Enviado com sucesso');
end;

procedure TACBrNFCeTestForm.ACBrMail1BeforeMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('ACBrMail.OnBeforeMailProcess');
end;

procedure TACBrNFCeTestForm.ACBrMail1MailException(const AMail: TACBrMail;
  const E: Exception; var ThrowIt: Boolean);
begin
  mLog.Lines.Add('----- ACBrMail.Error -----');
  mLog.Lines.Add('Erro ao Enviar o EMail: ' + AMail.Subject);
  ThrowIt := True;
end;

procedure TACBrNFCeTestForm.ACBrMail1MailProcess(const AMail: TACBrMail;
  const aStatus: TMailStatus);
begin
  case aStatus of
    pmsStartProcess:
      mLog.Lines.Add('ACBrMail.pmsStartProcess');
    pmsConfigHeaders:
      mLog.Lines.Add('ACBrMail.pmsConfigHeaders');
    pmsLoginSMTP:
      mLog.Lines.Add('ACBrMail.pmsLoginSMTP');
    pmsStartSends:
      mLog.Lines.Add('ACBrMail.pmsStartSends');
    pmsSendTo:
      mLog.Lines.Add('ACBrMail.pmsSendTo');
    pmsSendCC:
      mLog.Lines.Add('ACBrMail.pmsSendCC');
    pmsSendBCC:
      mLog.Lines.Add('ACBrMail.pmsSendBCC');
    pmsSendReplyTo:
      mLog.Lines.Add('ACBrMail.pmsSendReplyTo');
    pmsSendData:
      mLog.Lines.Add('ACBrMail.pmsSendData');
    pmsLogoutSMTP:
      mLog.Lines.Add('ACBrMail.pmsLogoutSMTP');
    pmsDone:
      mLog.Lines.Add('ACBrMail.pmsDone');
  end;
end;

procedure TACBrNFCeTestForm.AjustarScroll(AControl: TControl; AScrollBox: TCustomScrollBox);
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

procedure TACBrNFCeTestForm.btLerConfigClick(Sender: TObject);
begin
  LerConfiguracao;
  Toast('Configuração carregada');
end;

procedure TACBrNFCeTestForm.btnCadastroClick(Sender: TObject);
var
  Documento: String;
begin
  Documento := Trim(OnlyNumber(edtConsultaParametro.Text));
  if Documento.IsEmpty then
  begin
    Toast('Informe um CPF ou CNPJ');
    edtConsultaParametro.SetFocus;
    Exit;
  end;

  ACBrNFe1.WebServices.ConsultaCadastro.UF := cbxEmitUF.Selected.Text;

  if Length(Documento) > 11 then
     ACBrNFe1.WebServices.ConsultaCadastro.CNPJ := Documento
  else
     ACBrNFe1.WebServices.ConsultaCadastro.CPF := Documento;

  ACBrNFe1.WebServices.ConsultaCadastro.Executar;

  mLog.Lines.Clear;
  mLog.Lines.Add('----- RetWS -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.WebServices.ConsultaCadastro.RetWS )));
  mLog.Lines.Add('');
  mLog.Lines.Add('----- RetornoWS -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.WebServices.ConsultaCadastro.RetornoWS )));
  mLog.Lines.Add('');
  mLog.Lines.Add('----- Consulta Cadastro -----');
  mLog.Lines.Add('versao: '+ACBrNFe1.WebServices.ConsultaCadastro.versao);
  mLog.Lines.Add('verAplic: '+ACBrNFe1.WebServices.ConsultaCadastro.verAplic);
  mLog.Lines.Add('cStat: '+IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.cStat));
  mLog.Lines.Add('xMotivo: '+ACBrNFe1.WebServices.ConsultaCadastro.xMotivo);
  mLog.Lines.Add('DhCons: '+DateTimeToStr(ACBrNFe1.WebServices.ConsultaCadastro.DhCons));
  mLog.Lines.Add('cUF: '+IntToStr(ACBrNFe1.WebServices.ConsultaCadastro.cUF));
  mLog.Lines.Add('UF: '+ACBrNFe1.WebServices.ConsultaCadastro.UF);
  mLog.Lines.Add('IE: '+ACBrNFe1.WebServices.ConsultaCadastro.IE);
  mLog.Lines.Add('CNPJ: '+ACBrNFe1.WebServices.ConsultaCadastro.CNPJ);
  mLog.Lines.Add('CPF: '+ACBrNFe1.WebServices.ConsultaCadastro.CPF);

  ExibirLogs;
end;

procedure TACBrNFCeTestForm.btnCertInfoClick(Sender: TObject);
var
  AMsg: string;
begin
  if imgErrorCert.Visible then
  begin
    imgErrorCertClick(Sender);
    Exit;
  end;

  ConfigurarACBrNFe;
  ACBrNFe1.SSL.CarregarCertificado;
  mLog.Lines.Clear;
  mLog.Lines.Add('---- Informações do Certificado ----');
  mLog.Lines.Add('Número de Série: '+ACBrNFe1.SSL.CertNumeroSerie);
  mLog.Lines.Add('Válido até: '+FormatDateBr(ACBrNFe1.SSL.CertDataVenc));
  mLog.Lines.Add('Subject Name: '+ACBrNFe1.SSL.CertSubjectName);
  mLog.Lines.Add('Razão Social: ' + ACBrNFe1.SSL.CertRazaoSocial);
  mLog.Lines.Add('CNPJ/CPF: ' + ACBrNFe1.SSL.CertCNPJ);
  mLog.Lines.Add('Emissor: ' + ACBrNFe1.SSL.CertIssuerName);
  mLog.Lines.Add('Certificadora: ' + ACBrNFe1.SSL.CertCertificadora);
  ExibirLogs;
end;

procedure TACBrNFCeTestForm.btnChaveClick(Sender: TObject);
var
  vChave: String;
begin
  vChave := Trim(OnlyNumber(edtConsultaParametro.Text));
  if vChave.IsEmpty then
  begin
    Toast('Informe uma Chave de NFe');
    edtConsultaParametro.SetFocus;
    Exit;
  end;

  ACBrNFe1.NotasFiscais.Clear;
  ACBrNFe1.WebServices.Consulta.NFeChave := vChave;
  ACBrNFe1.WebServices.Consulta.Executar;

  mLog.Lines.Clear;
  mLog.Lines.Add('----- RetWS -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.WebServices.Consulta.RetWS )));
  mLog.Lines.Add('');
  mLog.Lines.Add('----- RetornoWS -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.WebServices.Consulta.RetornoWS )));
  mLog.Lines.Add('');
  mLog.Lines.Add('----- Consulta Chave -----');
  mLog.Lines.Add('NFeChave: '+ACBrNFe1.WebServices.Consulta.NFeChave);
  mLog.Lines.Add('Protocolo: '+ACBrNFe1.WebServices.Consulta.Protocolo);
  mLog.Lines.Add('DhRecbto: '+DateToStr(ACBrNFe1.WebServices.Consulta.DhRecbto));
  mLog.Lines.Add('XMotivo: '+ACBrNFe1.WebServices.Consulta.XMotivo);
  mLog.Lines.Add('versao: '+ACBrNFe1.WebServices.Consulta.versao);
  mLog.Lines.Add('tpAmb: '+TpAmbToStr(ACBrNFe1.WebServices.Consulta.tpAmb));
  mLog.Lines.Add('verAplic: '+ACBrNFe1.WebServices.Consulta.verAplic);
  mLog.Lines.Add('cStat: '+IntToStr(ACBrNFe1.WebServices.Consulta.cStat));
  mLog.Lines.Add('cUF: '+IntToStr(ACBrNFe1.WebServices.Consulta.cUF));
  mLog.Lines.Add('RetNFeDFe: '+ACBrNFe1.WebServices.Consulta.RetNFeDFe);
  mLog.Lines.Add('protNFe.chNFe: '+ACBrNFe1.WebServices.Consulta.protNFe.chNFe);
  mLog.Lines.Add('protNFe.dhRecbto: '+DateToStr(ACBrNFe1.WebServices.Consulta.protNFe.dhRecbto));
  mLog.Lines.Add('protNFe.nProt: '+ACBrNFe1.WebServices.Consulta.protNFe.nProt);
  mLog.Lines.Add('protNFe.digVal: '+ACBrNFe1.WebServices.Consulta.protNFe.digVal);
  mLog.Lines.Add('protNFe.xMotivo: '+ACBrNFe1.WebServices.Consulta.protNFe.xMotivo);
  mLog.Lines.Add('protNFe.cMsg: '+IntToStr(ACBrNFe1.WebServices.Consulta.protNFe.cMsg));
  mLog.Lines.Add('protNFe.xMsg: '+ACBrNFe1.WebServices.Consulta.protNFe.xMsg);

  ExibirLogs;
end;

procedure TACBrNFCeTestForm.btnGerarNFCeClick(Sender: TObject);
var
  NomeArqXML: string;
  tini, tfim: TDateTime;
begin
  if swEmailNFCe.IsChecked then
    VerificarErrosDeConfiguracaoEmail;

  if swImprimirNFCe.IsChecked then
    VerificarErrosDeConfiguracaoImpressora;

  tini := now;
  mLog.Lines.Clear;
  mLog.Lines.Add('-- Inicio: '+FormatDateTimeBr(tini,'hh:nn:ss.zzz') + ' --');
  mLog.Lines.Add('----- Criando XML NFCe -----');
  OperacaoNFCe := opGerando;
  CriarNFCe;

  sbProximaNFCe.ValueInc;
  GravarConfiguracao;

  OperacaoNFCe := opAssinando;
  ACBrNFe1.NotasFiscais.Assinar;

  OperacaoNFCe := opGravando;
  ACBrNFe1.NotasFiscais.GravarXML;

  NomeArqXML := ACBrNFe1.NotasFiscais.Items[0].NomeArq;
  mLog.Lines.Add('----- Gerar NFCe -----');
  mLog.Lines.Add('Arquivo gerado em:');
  mLog.Lines.Add(NomeArqXML);
  mLog.Lines.Add('');
  mLog.Lines.Add('----- XML -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.NotasFiscais.Items[0].XMLAssinado )));

  if swOnLine.IsChecked and swEnviarNFCe.IsChecked then
  begin
    OperacaoNFCe := opTransmitindo;
    EnviarNFCe;
    tfim := Now;
    mLog.Lines.Add( '-- Enviado em: '+FormatFloat('##0.000', SecondSpan(tini,tfim))+' segundos --' );
  end;

  if swImprimirNFCe.IsChecked then
  begin
    tini := now;
    OperacaoNFCe := opImprimindo;
    ImprimirNFCe;
  end;

  if swEmailNFCe.IsChecked then
  begin
    OperacaoNFCe := opEmail;
    EnviarEmailNFCe;
  end;

  OperacaoNFCe := opLivre;
  ExibirLogs;
end;

procedure TACBrNFCeTestForm.btnImprimirClick(Sender: TObject);
begin
  if fraXMLs.FileName = '' then
    Exit;

  if (LerXMLNFCe(fraXMLs.FileName) > 0) then
    ImprimirNFCe;
end;

procedure TACBrNFCeTestForm.btnEnviarEmailClick(Sender: TObject);
begin
  if fraXMLs.FileName = '' then
    Exit;

  if (LerXMLNFCe(fraXMLs.FileName) > 0) then
  begin
    EnviarEmailNFCe;
    ExibirLogs;
  end;
end;

procedure TACBrNFCeTestForm.btnEnviarClick(Sender: TObject);
begin
  if fraXMLs.FileName = '' then
    Exit;

  if (LerXMLNFCe(fraXMLs.FileName) > 0) then
  begin
    EnviarNFCe;
    ExibirLogs;
  end;
end;

procedure TACBrNFCeTestForm.btnVerXMLClick(Sender: TObject);
begin
  if (fraXMLs.FileName = '') then
    Exit;

  if (LerXMLNFCe(fraXMLs.FileName) < 1) then
    Exit;

  mLog.Lines.Clear;
  mLog.Lines.Add('----- VER XML -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.NotasFiscais.Items[0].XML )));
  ExibirLogs;
end;

procedure TACBrNFCeTestForm.btnValidarAssinaturaClick(Sender: TObject);
var
  Msg, MsgRes: String;
begin
  if fraXMLs.FileName = '' then
    Exit;

  if (LerXMLNFCe(fraXMLs.FileName) < 1) then
    Exit;

  mLog.Lines.Clear;
  mLog.Lines.Add('---- ASSINATURA ----');

  if not ACBrNFe1.NotasFiscais.VerificarAssinatura(Msg) then
    MsgRes := 'ERRO: Assinatura INVÁLIDA';
  begin
    MsgRes := 'Assinatura VÁLIDA';

    ACBrNFe1.SSL.CarregarCertificadoPublico( ACBrNFe1.NotasFiscais[0].NFe.signature.X509Certificate );
    mLog.Lines.Add('Assinado por: '+ ACBrNFe1.SSL.CertRazaoSocial);
    mLog.Lines.Add('CNPJ: '+ ACBrNFe1.SSL.CertCNPJ);
    mLog.Lines.Add('Num.Série: '+ ACBrNFe1.SSL.CertNumeroSerie);
    ACBrNFe1.SSL.DescarregarCertificado;
  end;

  mLog.Lines.Add('');
  mLog.Lines.Add(MsgRes);
  Toast(MsgRes);
  ExibirLogs;
end;

procedure TACBrNFCeTestForm.btnVerificarSchemaClick(Sender: TObject);
var
  MsgRes: String;
begin
  if fraXMLs.FileName = '' then
    Exit;

  if (LerXMLNFCe(fraXMLs.FileName) < 1) then
    Exit;

  mLog.Lines.Clear;
  mLog.Lines.Add('---- VALIDAÇÃO DO XML ----');
  try
    ACBrNFe1.NotasFiscais.Validar;

    if ACBrNFe1.NotasFiscais.Items[0].Alertas <> '' then
      mLog.Lines.Add('Alertas: '+ACBrNFe1.NotasFiscais.Items[0].Alertas);

    MsgRes := 'XML da NFCe é VALIDO';
  except
    on E: Exception do
    begin
      MsgRes := 'Erro: ' + ACBrNFe1.NotasFiscais.Items[0].ErroValidacao;
      mLog.Lines.Add('Exception: ' + E.Message);
      mLog.Lines.Add('Erro Completo: ' + ACBrNFe1.NotasFiscais.Items[0].ErroValidacaoCompleto);
      ExibirLogs;
    end;
  end;

  mLog.Lines.Add(MsgRes);
  Toast(MsgRes);
end;

procedure TACBrNFCeTestForm.btnApagarXMLClick(Sender: TObject);
begin
  if fraXMLs.FileName = '' then
    Exit;

  TDialogServiceAsync.MessageDialog('Apagar arquivo ?',
    TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], TMsgDlgBtn.mbYes, 0,
    procedure(const AResult: TModalResult)
    begin
      if (AResult = mrYes) then
        if System.SysUtils.DeleteFile(fraXMLs.FileName) then
          fraXMLs.Reload;
    end);
end;

procedure TACBrNFCeTestForm.btnVerLogsClick(Sender: TObject);
begin
  ExibirLogs;
end;

procedure TACBrNFCeTestForm.btnBackClick(Sender: TObject);
begin
  TabBack;
end;

procedure TACBrNFCeTestForm.btnLimparClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TACBrNFCeTestForm.btnProcurarBthClick(Sender: TObject);
begin
  CarregarImpressorasBth;
  cbxImpressorasBth.DropDown;
end;

procedure TACBrNFCeTestForm.btnStatusServicoClick(Sender: TObject);
begin
  ACBrNFe1.WebServices.StatusServico.Executar;

  mLog.Lines.Clear;
  mLog.Lines.Add('----- RetWS -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.WebServices.StatusServico.RetWS )));
  mLog.Lines.Add('');
  mLog.Lines.Add('----- RetornoWS -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.WebServices.StatusServico.RetornoWS )));
  mLog.Lines.Add('');
  mLog.Lines.Add('----- Status Serviço -----');
  mLog.Lines.Add('versao: ' + ACBrNFe1.WebServices.StatusServico.versao);
  mLog.Lines.Add('tpAmb: '    +TpAmbToStr(ACBrNFe1.WebServices.StatusServico.tpAmb));
  mLog.Lines.Add('verAplic: ' +ACBrNFe1.WebServices.StatusServico.verAplic);
  mLog.Lines.Add('cStat: '    +IntToStr(ACBrNFe1.WebServices.StatusServico.cStat));
  mLog.Lines.Add('xMotivo: '  +ACBrNFe1.WebServices.StatusServico.xMotivo);
  mLog.Lines.Add('cUF: '      +IntToStr(ACBrNFe1.WebServices.StatusServico.cUF));
  mLog.Lines.Add('dhRecbto: ' +DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRecbto));
  mLog.Lines.Add('tMed: '     +IntToStr(ACBrNFe1.WebServices.StatusServico.TMed));
  mLog.Lines.Add('dhRetorno: '+DateTimeToStr(ACBrNFe1.WebServices.StatusServico.dhRetorno));
  mLog.Lines.Add('xObs: '     +ACBrNFe1.WebServices.StatusServico.xObs);

  ExibirLogs;
end;

procedure TACBrNFCeTestForm.btnVerNotasClick(Sender: TObject);
begin
  ExibirXMLs;
end;

procedure TACBrNFCeTestForm.btnSobreClick(Sender: TObject);
begin
  mLog.Lines.Clear;
  mLog.Lines.Add('Projeto ACBr - DSA');
  mLog.Lines.Add('https://projetoacbr.com.br/');
  mLog.Lines.Add('');
  mLog.Lines.Add('Versão do OpenSSL:');
  mLog.Lines.Add(ssl_openssl_lib.OpenSSLVersion(0));
end;

procedure TACBrNFCeTestForm.btSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
  Toast('Configuração Salva com sucesso');
  TabBack;
end;

function TACBrNFCeTestForm.CalcularNomeArqConfiguracao: String;
begin
  Result := ApplicationPath + 'ACBrNFeTeste.ini';
end;

procedure TACBrNFCeTestForm.CarregarImpressorasBth;
begin
  {$IfDef HAS_BLUETOOTH}
   PedirPermissoes;
   cbxImpressorasBth.Items.Clear;
   try
     ACBrPosPrinter1.Device.AcharPortasBlueTooth( cbxImpressorasBth.Items, chbTodasBth.IsChecked );
     cbxImpressorasBth.Items.Add('NULL');
   except
   end;
  {$EndIf}
end;

procedure TACBrNFCeTestForm.CarregarListaDeCidades;
var
  cUF: Integer;
begin
  cUF := StrToIntDef(lEmitcUF.Text,0);
  if (cUF = 0) or (FcUF = cUF) then
    Exit;

  FcUF := cUF;
  try
    try
      ACBrIBGE1.BuscarPorcUF(FcUF);
    except
      On E: Exception do
      begin
        TThread.Synchronize(nil, procedure
        begin
          lMsgAguarde.Text := 'Erro ao carregar cidades';
          mLog.Lines.Add(E.ClassName);
          mLog.Lines.Add(E.Message);
        end);
        sleep(1500);
      end;
    end;
  finally
    TThread.Synchronize(nil, procedure

    var
      i: Integer;
      Cidade: TACBrIBGECidade;
    begin
      cbxEmitCidade.BeginUpdate;
      try
        cbxEmitCidade.Items.Clear;
        FcMunList.Clear;
        for i := 0 to ACBrIBGE1.Cidades.Count-1 do
        begin
          Cidade := ACBrIBGE1.Cidades[i];
          cbxEmitCidade.Items.Add(Cidade.Municipio);
          FcMunList.Add(IntToStr(Cidade.CodMunicipio));
        end;
      finally
        cbxEmitCidade.EndUpdate;
        if (cbxEmitCidade.Items.Count > 0) then
          cbxEmitCidade.ItemIndex := 0;

        TerminarTelaDeEspera;
      end;
    end);
  end;
end;

procedure TACBrNFCeTestForm.ExibirXMLs;
begin
  fraXMLs.ActualDir := ACBrNFe1.Configuracoes.Arquivos.PathNFe;
  fraXMLs.FileMask := '*.xml';
  fraXMLs.CanChangeDir := False;
  fraXMLs.SortType := flsDate;
  fraXMLs.Execute;
  TabForward(tabXMLs);
  // Vai para o último XML emitido
  fraXMLs.lvFileBrowse.ScrollTo(fraXMLs.lvFileBrowse.ItemCount-1);
  fraXMLs.SelectItem(fraXMLs.lvFileBrowse.ItemCount-1);
end;

procedure TACBrNFCeTestForm.cbxEmitCidadeChange(Sender: TObject);
var
  Ok: Boolean;
begin
  Ok := Assigned(cbxEmitCidade.Selected);
  imgErrorCidade.Visible := not Ok;
  if Ok then
    lEmitcMun.Text := FcMunList[cbxEmitCidade.Selected.Index];
end;

procedure TACBrNFCeTestForm.cbxEmitUFChange(Sender: TObject);
var
  cUF: Integer;
  Ok: Boolean;
begin
  Ok := Assigned(cbxEmitUF.Selected);
  imgErrorUF.Visible := not Ok;

  if Ok then
  begin
    cUF := UFtoCUF(cbxEmitUF.Selected.Text);
    if (cUF <> FcUF) then
    begin
      lEmitcUF.Text := IntToStrZero(cUF, 2);
      IniciarTelaDeEspera('Carregando Cidades de '+cbxEmitUF.Selected.Text);
      TThread.CreateAnonymousThread(CarregarListaDeCidades).Start;
    end;
  end;
end;

procedure TACBrNFCeTestForm.cbxModeloChange(Sender: TObject);
begin
  if rbClasseInterna.IsChecked and (cbxModelo.ItemIndex = Integer(ppExterno)) then
    rbClasseExterna.IsChecked := True
  else if rbClasseExterna.IsChecked and (cbxModelo.ItemIndex = 1) then
    cbxPagCodigo.ItemIndex := Integer(pcUTF8);
end;

procedure TACBrNFCeTestForm.cbxWebServiceUFChange(Sender: TObject);
begin
  imgErrorWebServiceUF.Visible := (cbxWebServiceUF.ItemIndex < 0);
end;

procedure TACBrNFCeTestForm.sbEmailMostrarSenhaClick(Sender: TObject);
begin
  edtEmailPassword.Password := not sbEmailMostrarSenha.IsPressed;
end;

procedure TACBrNFCeTestForm.seColunasEnter(Sender: TObject);
begin
  if (Sender is TControl) then
    AjustarScroll(TControl(Sender), lbConfigImpressora);
end;

procedure TACBrNFCeTestForm.seProxyVerSenhaClick(Sender: TObject);
begin
  edtProxyPass.Password := not seProxyVerSenha.IsPressed;
end;

procedure TACBrNFCeTestForm.SetOperacaoNFCe(const Value: TACBrNFCeTestOperacao);
var
  AMsg: String;
  AImg: Integer;
begin
  FOperacaoNFCe := Value;
  case FOperacaoNFCe of
    opErro:
      begin
        AMsg := 'ERRO';
        AImg := 14;
      end ;
    opGerando:
      begin
        AMsg := 'Gerando XML da NFCe';
        AImg := 18;
      end ;
    opAssinando:
      begin
        AMsg := 'Assinando XML da NFCe';
        AImg := 9;
      end ;
    opGravando:
      begin
        AMsg := 'Gravando XML da NFCe';
        AImg := 0;
      end ;
    opTransmitindo:
      begin
        AMsg := 'Transmitindo NFCe';
        AImg := 11;
      end ;
    opImprimindo:
      begin
        AMsg := 'Imprimindo DANFCe';
        AImg := 10;
      end ;
    opEmail:
      begin
        AMsg := 'Enviando XML por E-mail';
        AImg := 4;
      end ;
  else
    AMsg := '';
    AImg := -1;
  end;

  lbOperacao.Text := AMsg;
  imgOperacao.Visible := (AImg >= 0);
  if (AImg >= 0) then
    imgOperacao.Bitmap := ImageList1.Bitmap(TSizeF.Create(imgOperacao.Width,imgOperacao.Height),AImg);

  Application.ProcessMessages;
end;

procedure TACBrNFCeTestForm.SpeedButton4Click(Sender: TObject);
begin
  ExibirXMLs;
end;

function TACBrNFCeTestForm.SubstituirVariaveisEmail(const AText: String): String;
begin
  Result := AText;
  Result := StringReplace(Result, '[EmitFantasia]',
                         ACBrNFe1.NotasFiscais[0].NFe.Emit.xFant, [rfReplaceAll]);
  Result := StringReplace(Result, '[dEmissao]',
                         DateToStr(ACBrNFe1.NotasFiscais[0].NFe.Ide.dEmi), [rfReplaceAll]);
  Result := StringReplace(Result, '[ChaveDFe]',
                         ACBrNFe1.NotasFiscais[0].NFe.infNFe.ID, [rfReplaceAll]);
end;

procedure TACBrNFCeTestForm.swEmailNFCeSwitch(Sender: TObject);
begin
  if swEmailNFCe.IsChecked then
    lEmailNFCe.Text := 'E-mail'
  else
    lEmailNFCe.Text := 'Sem e-mail';
end;

procedure TACBrNFCeTestForm.swEnviarNFCeSwitch(Sender: TObject);
begin
  if swEnviarNFCe.IsChecked then
    lEnviarNFCe.Text := 'Enviar'
  else
    lEnviarNFCe.Text := 'Não enviar';
end;

procedure TACBrNFCeTestForm.swImprimirNFCeSwitch(Sender: TObject);
begin
  if swImprimirNFCe.IsChecked then
    lImprimirNFCe.Text := 'Imprimir'
  else
    lImprimirNFCe.Text := 'Não imprimir';
end;

procedure TACBrNFCeTestForm.swOnLineSwitch(Sender: TObject);
begin
  if swOnLine.IsChecked then
    lOnLine.Text := 'On-Line'
  else
    lOnLine.Text := 'Off-Line';

  swEnviarNFCe.Enabled := swOnLine.IsChecked;
  if not swEnviarNFCe.Enabled then
    swEnviarNFCe.IsChecked := False
  else
    swEnviarNFCe.IsChecked := True;
end;

procedure TACBrNFCeTestForm.swWebServiceAmbienteSwitch(Sender: TObject);
begin
  if swWebServiceAmbiente.IsChecked then
    lAmbiente.Text := 'Produção'
  else
    lAmbiente.Text := 'Testes';
end;

procedure TACBrNFCeTestForm.ConfigurarACBr;
begin
  ConfigurarACBrMail;
  ConfigurarACBrPosPrinter;
  ConfigurarACBrDANFCe;
  ConfigurarACBrNFe;
end;

procedure TACBrNFCeTestForm.ConfigurarACBrMail;
begin
  // Configurando o ACBrMail //
  ACBrMail1.From := edtEmailFrom.text;
  ACBrMail1.FromName := edtEmailFromName.text;
  ACBrMail1.Host := edtEmailHost.text; // troque pelo seu servidor smtp
  ACBrMail1.Username := edtEmailUser.text;
  ACBrMail1.Password := edtEmailPassword.text;
  ACBrMail1.Port := sbEmailPort.Text; // troque pela porta do seu servidor smtp
  ACBrMail1.SetTLS := chkEmailTLS.IsChecked;
  ACBrMail1.SetSSL := chkEmailSSL.IsChecked;  // Verifique se o seu servidor necessita SSL
  ACBrMail1.DefaultCharset := TMailCharset(cbEmailDefaultCharset.ItemIndex);
  ACBrMail1.IDECharset := TMailCharset(cbEmailIdeCharSet.ItemIndex);
end;

procedure TACBrNFCeTestForm.ConfigurarACBrNFe;
var
  CertPFX: string;
begin
  CertPFX := edtConfCertPFX.Text;
  if (ExtractFilePath(CertPFX) = '') then
    CertPFX := ApplicationPath + CertPFX;

  ACBrNFe1.Configuracoes.Certificados.URLPFX := edtConfCertURL.Text;
  ACBrNFe1.Configuracoes.Certificados.ArquivoPFX := CertPFX;
  ACBrNFe1.Configuracoes.Certificados.Senha := edtConfCertSenha.Text;
  ACBrNFe1.SSL.URLPFX := ACBrNFe1.Configuracoes.Certificados.URLPFX;
  ACBrNFe1.SSL.ArquivoPFX := ACBrNFe1.Configuracoes.Certificados.ArquivoPFX;
  ACBrNFe1.SSL.Senha := ACBrNFe1.Configuracoes.Certificados.Senha;

  if Assigned(cbxWebServiceUF.Selected) then
    ACBrNFe1.Configuracoes.WebServices.UF := cbxWebServiceUF.Selected.Text;

  ACBrNFe1.Configuracoes.WebServices.Ambiente := TpcnTipoAmbiente(IfThen(swWebServiceAmbiente.IsChecked, 0, 1));
  ACBrNFe1.Configuracoes.WebServices.TimeOut := Trunc(sbWebServiceTimeout.Value);

  ACBrNFe1.Configuracoes.Geral.SSLLib := TSSLLib.libOpenSSL;
  if Assigned(cbxWebServiceSSLType.Selected) then
    ACBrNFe1.Configuracoes.WebServices.SSLType := TSSLType(cbxWebServiceSSLType.ItemIndex);

  ACBrNFe1.Configuracoes.Geral.IdCSC := edtTokenID.Text;
  ACBrNFe1.Configuracoes.Geral.CSC := edtTokenCSC.Text;
  // Sugestão de configuração para apresentação de mensagem mais amigável ao usuário final
  ACBrNFe1.Configuracoes.Geral.ExibirErroSchema := True;
  ACBrNFe1.Configuracoes.Geral.FormatoAlerta := 'Campo:%DESCRICAO% - %MSG%';

  ACBrNFe1.Configuracoes.WebServices.ProxyHost := edtProxyHost.Text;
  ACBrNFe1.Configuracoes.WebServices.ProxyPort := Trunc(sbProxyPort.Value).ToString;
  ACBrNFe1.Configuracoes.WebServices.ProxyUser := edtProxyUser.Text;
  ACBrNFe1.Configuracoes.WebServices.ProxyPass := edtProxyPass.Text;
  ACBrNFe1.Configuracoes.WebServices.Salvar := False;

  ACBrNFe1.Configuracoes.Arquivos.PathNFe := TPath.Combine(ApplicationPath, 'xml');
  ACBrNFe1.Configuracoes.Arquivos.PathEvento := ACBrNFe1.Configuracoes.Arquivos.PathNFe;
  ACBrNFe1.Configuracoes.Arquivos.PathInu := ACBrNFe1.Configuracoes.Arquivos.PathNFe;
  ACBrNFe1.Configuracoes.Arquivos.PathSalvar := TPath.Combine(TPath.Combine(ApplicationPath, 'xml'), 'soap');

  DescompactarSchemas;
end;

procedure TACBrNFCeTestForm.ConfigurarACBrPosPrinter;
begin
  PedirPermissoes;
  ACBrPosPrinter1.Desativar;

  if rbClasseExterna.IsChecked then
  begin
    {$IfDef ANDROID}
    case cbxModelo.ItemIndex of
      0: ACBrPosPrinter1.ModeloExterno := fE1Printer;
      1: ACBrPosPrinter1.ModeloExterno := fE1Lib;
    else
      ACBrPosPrinter1.ModeloExterno := fGEDIPrinter;
    end;
    {$Else}
     ACBrPosPrinter1.ModeloExterno := fE1Printer;
    {$EndIf}

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
  ACBrPosPrinter1.ConfigQRCode.LarguraModulo := Trunc(seLarguraModulo.Value);
  ACBrPosPrinter1.ConfigLogo.KeyCode1 := Trunc(seKC1.Value);
  ACBrPosPrinter1.ConfigLogo.KeyCode2 := Trunc(seKC2.Value);
  ACBrPosPrinter1.ConfigLogo.IgnorarLogo := not cbImprimirLogo.IsChecked;
  ACBrPosPrinter1.CortaPapel := True;
end;

procedure TACBrNFCeTestForm.ConfigurarACBrDANFCe;
begin
  // Configurando ACBrNFeDANFeESCPOS //
  ACBrNFeDANFeESCPOS1.PosPrinter := ACBrPosPrinter1;
  ACBrNFeDANFeESCPOS1.ImprimeLogoLateral := cbLogoLateral.IsChecked;
  ACBrNFeDANFeESCPOS1.ImprimeQRCodeLateral := cbQRCodeLateral.IsChecked;
  ACBrNFeDANFeESCPOS1.ImprimeEmUmaLinha := cbImprimir1Linha.IsChecked;
  ACBrNFeDANFeESCPOS1.ImprimeDescAcrescItem := cbImprimirDescAcres.IsChecked;
  ACBrNFeDANFeESCPOS1.SuportaCondensado := False;
end;

procedure TACBrNFCeTestForm.ConsultarCEP;
var
  Erro: string;
begin
  Erro := '';
  try
    try
      ACBrCEP1.BuscarPorCEP(OnlyNumber(edtEmitCEP.Text));
    except
      On E: Exception do
      begin
        TThread.Synchronize(nil, procedure
        begin
          lMsgAguarde.Text := 'Erro ao consultar o CEP: '+edtEmitCEP.Text;
          mLog.Lines.Add(E.ClassName);
          mLog.Lines.Add(E.Message);
        end);
        Sleep(1500);
      end;
    end;
  finally
    TThread.Synchronize(nil, procedure
    var
      EndAchado: TACBrCEPEndereco;
    begin  
      if (ACBrCEP1.Enderecos.Count > 0) then
      begin
        EndAchado := ACBrCEP1.Enderecos[0];
        edtEmitLogradouro.Text := EndAchado.Tipo_Logradouro + ' ' + EndAchado.Logradouro;
        edtEmitBairro.Text := EndAchado.Bairro;
        edtEmitCEP.Text := ACBrValidador.FormatarCEP(EndAchado.CEP);
        edtEmitComp.Text := EndAchado.Complemento;
        lEmitcUF.Text := IntToStr(UFtoCUF(EndAchado.UF));
        CarregarListaDeCidades;
        cbxEmitUF.ItemIndex := cbxEmitUF.Items.IndexOf(EndAchado.UF);
        cbxEmitCidade.ItemIndex := cbxEmitCidade.Items.IndexOf(EndAchado.Municipio);
        edtEmitNumero.SetFocus;
      end;

      TerminarTelaDeEspera;
    end);
  end;
end;

procedure TACBrNFCeTestForm.DescompactarSchemas;
var
  BasePathSchemas, SchemasZip, ArquivoControle: string;
  ZipFile: TZipFile;
  Descompactar: Boolean;
  dtSchema, dtControle: TDateTime;
begin
  BasePathSchemas := ApplicationPath + 'Schemas';
  ArquivoControle := TPath.Combine(BasePathSchemas, 'leiame.txt');
  ACBrNFe1.Configuracoes.Arquivos.PathSchemas := TPath.Combine(BasePathSchemas, 'NFe');
  {$IfDef MSWINDOWS}
   SchemasZip := '..\..\Schemas.zip';
  {$Else}
   SchemasZip := TPath.Combine( TPath.GetDocumentsPath, 'Schemas.zip' );
  {$EndIf}
  if not FileExists(SchemasZip) then
    Exit;

  Descompactar := not FileExists( ArquivoControle );
  if not (Descompactar) then
  begin
    FileAge(SchemasZIP, dtSchema);
    FileAge(ArquivoControle, dtControle);
    Descompactar := (dtSchema > dtControle);
  end;

  if Descompactar then
  begin
    ZipFile := TZipFile.Create;
    try
      ZipFile.Open(SchemasZip, zmReadWrite);
      ZipFile.ExtractAll(BasePathSchemas);
    finally
      ZipFile.Free;
    end;

    // Criando arquivo de controle
    System.SysUtils.DeleteFile(ArquivoControle);
    WriteToFile(ArquivoControle,'https://www.projetoacbr.com.br/');
  end;
end;

procedure TACBrNFCeTestForm.EditApenasNumeros(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if not CharIsNum(KeyChar) then
    KeyChar := #0;
end;

procedure TACBrNFCeTestForm.edtAddressEmailEnter(Sender: TObject);
begin
  if (Sender is TControl) then
    AjustarScroll(TControl(Sender), lbConfigEmail);
end;

procedure TACBrNFCeTestForm.edtConfCertURLExit(Sender: TObject);
begin
  if (edtConfCertURL.Text <> '') and (edtConfCertPFX.Text = '') then
    edtConfCertPFX.Text := 'CertA1.pfx';  // Usa para Cache local
end;

procedure TACBrNFCeTestForm.edtConfCertURLTyping(Sender: TObject);
begin
  imgErrorCert.Visible := not ValidarEditsCertificado( edtConfCertURL.Text,
                                                       edtConfCertPFX.Text,
                                                       edtConfCertSenha.Text);
end;

procedure TACBrNFCeTestForm.edtEmitCEPTyping(Sender: TObject);
begin
  if (edtEmitCEP.Text.Length > 5) then
  begin
    edtEmitCEP.Text := FormatarMascaraDinamica(OnlyNumber(edtEmitCEP.Text), '*****-***');
    edtEmitCEP.CaretPosition := edtEmitCEP.Text.Length;
  end;

  imgErrorCep.Visible := (edtEmitCEP.Text.Length < 9);
end;

procedure TACBrNFCeTestForm.edtEmitCEPValidate(Sender: TObject;
  var Text: string);
begin
  if not imgErrorCep.Visible then
    sbAcharCEPClick(Sender);
end;

procedure TACBrNFCeTestForm.edtEmitCNPJTyping(Sender: TObject);
begin
  if (edtEmitCNPJ.Text.Length > 2) then
  begin
    TThread.Queue(nil, procedure
    begin
      edtEmitCNPJ.Text := ACBrValidador.FormatarMascaraDinamica(OnlyNumber(edtEmitCNPJ.Text), '**.***.***/****-**');
      edtEmitCNPJ.CaretPosition := edtEmitCNPJ.Text.Length;
    end);
  end;

  imgErrorCNPJ.Visible := (edtEmitCNPJ.Text.Length < 18) or
                          (not ACBrValidador.ValidarCNPJ(edtEmitCNPJ.Text).IsEmpty);
end;

procedure TACBrNFCeTestForm.edtExitScrollableControl(Sender: TObject);
begin
  {$IfDef MSWINDOWS}
   FormVirtualKeyboardHidden(Sender,False, TRect.Create(0,0,0,0));
  {$EndIf}
end;

procedure TACBrNFCeTestForm.edtEmitFoneTyping(Sender: TObject);
var
  AStr, Mascara: String;
begin
  if (edtEmitFone.Text.Length > 2) then
  begin
    AStr := OnlyNumber(edtEmitFone.Text);
    Mascara := '(**)****-****';
    case AStr.Length of
      10:
      begin
        if (copy(AStr,1,1) = '0') and (copy(AStr,3,2) = '00') then  // 0300,0500,0800,0900
          Mascara := '****-***-****';
      end;
      11: Mascara := '(**)*****-****';
      12: Mascara := '+**(**)****-****';
    end;

    edtEmitFone.Text := ACBrValidador.FormatarMascaraDinamica(AStr, Mascara);
    edtEmitFone.CaretPosition := edtEmitFone.Text.Length;
  end;

  imgErrorTelefone.Visible := (OnlyNumber(edtEmitFone.Text).Length < 10);
end;

procedure TACBrNFCeTestForm.edtEmitRazaoTyping(Sender: TObject);
begin
  imgErrorRazaoSocial.Visible := (edtEmitRazao.Text.Length < 4);
end;

procedure TACBrNFCeTestForm.edtEnterScrollableControl(Sender: TObject);
begin
  if (Sender is TControl) then
    AjustarScroll(TControl(Sender), lbConfNFCe);
end;

procedure TACBrNFCeTestForm.edtTokenCSCTyping(Sender: TObject);
begin
  imgErrorCSC.Visible := edtTokenCSC.Text.IsEmpty;
end;

procedure TACBrNFCeTestForm.edtTokenIDTyping(Sender: TObject);
begin
  imgErrorIDCSC.Visible := edtTokenID.Text.IsEmpty;
end;

procedure TACBrNFCeTestForm.EnviarNFCe;
begin
  if (ACBrNFe1.NotasFiscais.Count < 1) then
    Exit;

  ACBrNFe1.Enviar(IntToStr(Trunc(sbLote.Value)), False, True, False);  // NãoImprimir, Sincrono

  //mLog.Lines.Clear;
  mLog.Lines.Add('----- RetWS -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.WebServices.Enviar.RetWS )));
  mLog.Lines.Add('');
  mLog.Lines.Add('----- RetornoWS -----');
  mLog.Lines.Add( XML.XMLDoc.FormatXMLData(UTF8ToNativeString(
    ACBrNFe1.WebServices.Enviar.RetornoWS )));
  mLog.Lines.Add('');
  mLog.Lines.Add('---- ENVIAR XML ----');
  mLog.Lines.Add('Recibo: '+ ACBrNFe1.WebServices.Enviar.Recibo);
  mLog.Lines.Add('versao: '+ACBrNFe1.WebServices.Enviar.versao);
  mLog.Lines.Add('tpAmb: ' + TpAmbToStr(ACBrNFe1.WebServices.Enviar.TpAmb));
  mLog.Lines.Add('verAplic: ' + ACBrNFe1.WebServices.Enviar.verAplic);
  mLog.Lines.Add('cStat: ' + IntToStr(ACBrNFe1.WebServices.Enviar.cStat));
  mLog.Lines.Add('cUF: ' + IntToStr(ACBrNFe1.WebServices.Enviar.cUF));
  mLog.Lines.Add('xMotivo: ' + ACBrNFe1.WebServices.Enviar.xMotivo);
  mLog.Lines.Add('DhRecbto: '+DateToStr(ACBrNFe1.WebServices.Enviar.DhRecbto));
  mLog.Lines.Add('TMed: ' + IntToStr(ACBrNFe1.WebServices.Enviar.TMed));
  mLog.Lines.Add('Protocolo: '+ACBrNFe1.WebServices.Enviar.Protocolo);
  mLog.Lines.Add('Lote: ' + ACBrNFe1.WebServices.Enviar.Lote);
end;

procedure TACBrNFCeTestForm.ImprimirNFCe;
begin
  if (ACBrNFe1.NotasFiscais.Count < 1) then
    Exit;

  VerificarErrosDeConfiguracaoImpressora;
  ACBrPosPrinter1.Ativar;
  ACBrNFe1.NotasFiscais.Imprimir;
end;

procedure TACBrNFCeTestForm.EnviarEmailNFCe;
var
  SL: TStringList;
  ASubject: String;
begin
  VerificarErrosDeConfiguracaoEmail;
  if (ACBrNFe1.NotasFiscais.Count < 1) then
    Exit;

  SL := TStringList.Create;
  try
    SL.Text := SubstituirVariaveisEmail(mAltBody.Lines.Text);
    ASubject := SubstituirVariaveisEmail(edtSubject.Text);

    {$IfDef DELPHI28_UP}
     ACBrMail1.AltBody.DefaultEncoding := TEncoding.ANSI;
     ACBrMail1.Body.DefaultEncoding := TEncoding.ANSI;
    {$EndIf}

    ACBrNFe1.NotasFiscais.Items[0].EnviarEmail(
      edtAddressEmail.Text,
      ASubject,
      SL,
      False,  // PDF ainda  não suportdo
      nil,    // Lista com emails copias - TStrings
      nil     // Lista de anexos - TStrings
    );
  finally
    SL.Free;
  end;
end;

procedure TACBrNFCeTestForm.ExibirErroImpressaoE1(const MsgErro: string);
begin
  Toast(MsgErro, False);
end;

procedure TACBrNFCeTestForm.ExibirLogs;
begin
  TabForward(tabLog) ;
  mLog.ScrollToTop(False);
  mLog.GoToTextEnd;
end;

procedure TACBrNFCeTestForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if Key = vkHardwareBack then
  begin
    if (FVKService <> nil) then
    begin
      if TVirtualKeyboardState.Visible in FVKService.VirtualKeyboardState then
      begin
        FVKService.HideVirtualKeyboard;
        Key := 0;
        Exit
      end;
    end;

    if (FTabList.Count > 0) then
    begin
      TabBack;
      Key := 0;
    end;
  end;
end;

procedure TACBrNFCeTestForm.FormVirtualKeyboardHidden(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
begin
  if Assigned(FScrollBox) then
  begin
    FScrollBox.Margins.Bottom := 0;
    FScrollBox := nil;
  end;
end;

procedure TACBrNFCeTestForm.FormVirtualKeyboardShown(Sender: TObject;
  KeyboardVisible: Boolean; const Bounds: TRect);
var
  KeyBoardScreenTop, ControlScreenBottom, PosicaoIdeal,
  OffSet, NovoY: Extended;
  AParent: TControl;
begin
  if (Sender = mLog) then
  begin
    KeyboardVisible := False;
    Exit;
  end;

  if not Assigned(FControlToCenter) then
    Exit;

  if (FScrollBox = nil) then  // Se não tem o FScrollBox, tenta deduzir
  begin
    AParent := TControl(FControlToCenter.Parent);
    while (AParent <> nil) do
    begin
      if (AParent is TCustomScrollBox) then  // Achei um scrollBox Pai
      begin
        FScrollBox := TCustomScrollBox(AParent);
        if (FScrollBox.ContentBounds.Height > FScrollBox.Height) then  // Essa Caixa faz Scroll ?
          Break;
      end;

      AParent := TControl(AParent.Parent);
    end;

    if not Assigned(FScrollBox)  then
      Exit;
  end;

  // Verificando se precisa fazer o Scroll
  KeyBoardScreenTop := Bounds.Top;
  ControlScreenBottom := FControlToCenter.LocalToAbsolute(TPointF.Zero).Y +
                         (FControlToCenter.Height*2);

  // Verificando se precisa Reposicionar o Scroll...
  if (ControlScreenBottom > KeyBoardScreenTop) then
  begin
    PosicaoIdeal := FScrollBox.LocalToAbsolute(TPointF.Zero).Y +
                    Trunc(KeyBoardScreenTop/2);  // Meio da Area Livre do Scroll
    OffSet := ControlScreenBottom - PosicaoIdeal;

    // Reposiciona o Scroll, de acordo com o Offset calculado
    NovoY := OffSet + FScrollBox.ViewportPosition.Y;
    FScrollBox.ViewportPosition := PointF(FScrollBox.ViewportPosition.X, NovoY);
    if (FScrollBox.ViewportPosition.Y < NovoY) then
    begin
      // Não conseguiu rolar o suficiente, vamos ativar as Margens
      FScrollBox.Margins.Bottom := NovoY - FScrollBox.ViewportPosition.Y;
      FScrollBox.ViewportPosition := PointF(FScrollBox.ViewportPosition.X, NovoY);
    end;
  end;
end;

procedure TACBrNFCeTestForm.fraXMLslvFileBrowseDeletingItem(Sender: TObject;
  AIndex: Integer; var ACanDelete: Boolean);
begin
  fraXMLs.SelectItem(AIndex);
  if (fraXMLs.FileName = '') then
  begin
    ACanDelete := False;
    Exit;
  end;

  if System.SysUtils.DeleteFile(fraXMLs.FileName) then
  begin
    ACanDelete := True;
    fraXMLs.Reload;
  end;
end;

procedure TACBrNFCeTestForm.GravarConfiguracao;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := CalcularNomeArqConfiguracao;
  Ini := TIniFile.Create(IniFile {$IfDef POSIX}, TEncoding.ANSI{$EndIf});
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
    INI.WriteInteger('PosPrinter','LarguraModulo', Trunc(seLarguraModulo.Value) );
    Ini.WriteBool('PosPrinter', 'Logo', cbImprimirLogo.IsChecked);
    INI.WriteInteger('PosPrinter.Logo','KC1',Trunc(seKC1.Value));
    INI.WriteInteger('PosPrinter.Logo','KC2',Trunc(seKC2.Value));

    // Configurações de ACBrNFeDANFeESCPOS //
    Ini.WriteBool('DANFCE', 'QrCodeLateral', cbQRCodeLateral.IsChecked);
    Ini.WriteBool('DANFCE', 'ItemUmaLinha', cbImprimir1Linha.IsChecked);
    Ini.WriteBool('DANFCE', 'ItemDescAcres', cbImprimirDescAcres.IsChecked);
    Ini.WriteBool('DANFCE', 'LogoLateral', cbLogoLateral.IsChecked);

    // Configurações do ACBrMail //
    Ini.WriteString('Email', 'From', edtEmailFrom.Text);
    Ini.WriteString('Email', 'FromName', edtEmailFromName.Text);
    Ini.WriteString('Email', 'Host', edtEmailHost.Text);
    Ini.WriteInteger('Email', 'Port', Trunc(sbEmailPort.Value));
    Ini.WriteString('Email', 'User', edtEmailUser.text);
    Ini.WriteString('Email', 'Pass', edtEmailPassword.Text);
    Ini.WriteBool('Email', 'TLS', chkEmailTLS.IsChecked);
    Ini.WriteBool('Email', 'SSL', chkEmailSSL.IsChecked);
    Ini.WriteInteger('Email', 'DefaultCharset', cbEmailDefaultCharset.ItemIndex);
    Ini.WriteInteger('Email', 'IdeCharset', cbEmailIdeCharSet.ItemIndex);
    Ini.WriteString('Email', 'AddressEmail', edtAddressEmail.Text);
    Ini.WriteString('Email', 'AddressName', edtAddressName.Text);
    Ini.WriteString('Email', 'Subject', edtSubject.Text);
    Ini.WriteString('Email', 'Mensagem', BinaryStringToString(mAltBody.Text));

    // Configurações de ACBrNFe //
    Ini.WriteString( 'Certificado', 'URL', edtConfCertURL.Text);
    Ini.WriteString( 'Certificado', 'Caminho', edtConfCertPFX.Text);
    Ini.WriteString('Certificado', 'URL', edtConfCertURL.Text);
    Ini.WriteString( 'Certificado', 'Senha', edtConfCertSenha.Text);

    if cbxWebServiceUF.ItemIndex >= 0  then
      Ini.WriteString('WebService', 'UF', cbxWebServiceUF.Selected.Text);

    Ini.WriteInteger('WebService', 'Ambiente', ifthen( swWebServiceAmbiente.IsChecked, 0, 1) );  // segue TpcnTipoAmbiente
    Ini.WriteInteger('WebService', 'TimeOut', Trunc(sbWebServiceTimeout.Value));
    Ini.WriteInteger('WebService', 'SSLType',    cbxWebServiceSSLType.ItemIndex);

    Ini.WriteString( 'Token', 'IdCSCn', edtTokenID.Text);
    Ini.WriteString( 'Token', 'CSC', edtTokenCSC.Text);

    // Configurações do Emitente //
    Ini.WriteString('Emitente', 'CNPJ', edtEmitCNPJ.Text);
    Ini.WriteString('Emitente', 'IE', edtEmitIE.Text);
    Ini.WriteString('Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString('Emitente', 'Fantasia', edtEmitFantasia.Text);
    Ini.WriteString('Emitente', 'Fone', edtEmitFone.Text);
    Ini.WriteString('Emitente', 'CEP', edtEmitCEP.Text);
    Ini.WriteString('Emitente', 'Logradouro', edtEmitLogradouro.Text);
    Ini.WriteString('Emitente', 'Numero', edtEmitNumero.Text);
    Ini.WriteString('Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString('Emitente', 'Bairro', edtEmitBairro.Text);
    Ini.WriteInteger('Emitente', 'cUF', StrToIntDef(lEmitcUF.Text,0));
    Ini.WriteInteger('Emitente', 'cMun', StrToIntDef(lEmitcMun.Text,0));
    INI.WriteInteger('Emitente','CRT', cbxTipoEmpresa.ItemIndex);

    // Configurações do Proxy //
    Ini.WriteString('Proxy', 'Host', edtProxyHost.Text);
    Ini.WriteInteger('Proxy', 'Porta', Trunc(sbProxyPort.Value));
    Ini.WriteString('Proxy', 'User', edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass', edtProxyPass.Text);

    Ini.WriteInteger('NFCe', 'TipoEmissao', ifthen( swOnLine.IsChecked, 0, 8) );  // segue TpcnTipoEmissao
    Ini.WriteInteger('NFCe', 'ProximoNumero', Trunc(sbProximaNFCe.Value));
    Ini.WriteInteger('NFCe', 'Lote', Trunc(sbLote.Value));
  finally
    Ini.Free;
  end;
end;

procedure TACBrNFCeTestForm.imgErrorCepClick(Sender: TObject);
begin
  Toast('CEP incompleto');
end;

procedure TACBrNFCeTestForm.imgErrorCertClick(Sender: TObject);
var
  Pass, URL, PFX: string;
begin
  Pass := edtConfCertSenha.Text;
  URL := edtConfCertURL.Text;
  PFX := edtConfCertPFX.Text;

  if Pass.IsEmpty then
    Toast('Senha do Certificado não informada')
  else
  begin
    if (not URL.IsEmpty) then
    begin
      if PFX.IsEmpty then
        Toast('Informe Arquivo, para Cache Local');
    end
    else
    begin
      if PFX.IsEmpty then
        Toast('Informe Arquivo ou URL')
      else if not FileExists(PFX) then
        Toast('Arquivo: '+ExtractFileName(PFX)+' não existe');
    end;
  end;
end;

procedure TACBrNFCeTestForm.imgErrorCidadeClick(Sender: TObject);
begin
  Toast('Selecione uma Cidade');
end;

procedure TACBrNFCeTestForm.imgErrorCNPJClick(Sender: TObject);
var
  Erro: string;
begin
  if (edtEmitCNPJ.Text.Length < 18) then
    Erro := 'CNPJ incompleto'
  else
    Erro:= ACBrValidador.ValidarCNPJ(edtEmitCNPJ.Text);

  if not Erro.IsEmpty then
    Toast(Erro);
end;

procedure TACBrNFCeTestForm.imgErrorCSCClick(Sender: TObject);
begin
  Toast('Informe o CSC do Emissor');
end;

procedure TACBrNFCeTestForm.imgErrorIDCSCClick(Sender: TObject);
begin
  Toast('Informe a ID do CSC');
end;

procedure TACBrNFCeTestForm.imgErrorRazaoSocialClick(Sender: TObject);
begin
  Toast('Informe uma Razão Social');
end;

procedure TACBrNFCeTestForm.imgErrorTelefoneClick(Sender: TObject);
begin
 Toast('Telefone inválido');
end;

procedure TACBrNFCeTestForm.imgErrorUFClick(Sender: TObject);
begin
  Toast('Selecione uma UF');
end;

procedure TACBrNFCeTestForm.imgErrorWebServiceUFClick(Sender: TObject);
begin
  Toast('Informe a UF do WebService');
end;

procedure TACBrNFCeTestForm.IniciarTelaDeEspera(const AMsg: String);
begin
  if not AMsg.IsEmpty then
    lMsgAguarde.Text := AMsg
  else
    lMsgAguarde.Text := 'Aguarde Processando...';

  lWait.Visible := True;
  AniIndicator1.Enabled := True;
  lWait.BringToFront;
end;

procedure TACBrNFCeTestForm.laBtnConfiguracaoClick(Sender: TObject);
begin
  TabForward(tabConfig) ;
end;

procedure TACBrNFCeTestForm.laBtnLogsClick(Sender: TObject);
begin
  ExibirLogs;
end;

procedure TACBrNFCeTestForm.laBtnNotasEmtidasClick(Sender: TObject);
begin
  ConfigurarACBr;
  ExibirXMLs;
end;

procedure TACBrNFCeTestForm.laBtnTestesClick(Sender: TObject);
begin
  VerificarErrosDeConfiguracao;
  ConfigurarACBr;
  TabForward(tabTeste);
end;

procedure TACBrNFCeTestForm.TabForward(ANewTab: TTabItem);
begin
  FTabList.Add(tabsPrincipal.ActiveTab);
  tabsPrincipal.SetActiveTabWithTransition( ANewTab,
                                            TTabTransition.Slide) ;
end;

procedure TACBrNFCeTestForm.tabsPrincipalChange(Sender: TObject);
begin
  if tabsPrincipal.ActiveTab = tabTeste then
    OperacaoNFCe := opLivre;
end;

procedure TACBrNFCeTestForm.TabBack;
var
  OldTab: TTabItem;
begin
  if (FTabList.Count > 0) then
  begin
    OldTab := FTabList.Last;
    FTabList.Delete(FTabList.Count-1);
  end
  else
    OldTab := tabMenu;

  tabsPrincipal.SetActiveTabWithTransition( OldTab,
                                            TTabTransition.Slide,
                                            TTabTransitionDirection.Reversed);
end;

procedure TACBrNFCeTestForm.TerminarTelaDeEspera;
begin
  lWait.Visible := False;
  AniIndicator1.Enabled := False;
end;

procedure TACBrNFCeTestForm.tiStartUpTimer(Sender: TObject);
begin
  tiStartUp.Enabled := False;  // Dispara apenas uma vez
  LerConfiguracao;
end;

function TACBrNFCeTestForm.ValidarEditsCertificado(const URL, PFX, Pass: String): Boolean;
var
 sArqPfx: String;
begin
  Result := not Pass.IsEmpty;
  if Result then
  begin
    if (not URL.IsEmpty) then
      Result := (not PFX.IsEmpty)   // Precisa do PFX, para Cache Local
    else
    begin
      if (ExtractFilePath(PFX) = '') then
        sArqPfx := ApplicationPath + PFX
      else
        sArqPfx := PFX;

      Result := (not PFX.IsEmpty) and FileExists(sArqPfx);
    end;
  end;
end;

procedure TACBrNFCeTestForm.VerificarErrosDeConfiguracao;

  function VerificarErrosDeConfiguracaoEmComponente(AControl: TFmxObject): Boolean;
  var
    i: Integer;
    AChild: TFmxObject;
  begin
    Result := True;
    i := 0;

    while Result and (i < AControl.ChildrenCount) do
    begin
      AChild := AControl.Children[i];
      if (AChild is TImage) and (LowerCase(LeftStr(AChild.Name,6)) = 'imgerr') then
      begin
        with (AChild as TImage) do
        begin
          if Visible then
          begin
            Result := False;
            OnClick(nil);
            Break;
          end;
        end;
      end;

      if Result then
        Result := VerificarErrosDeConfiguracaoEmComponente(AChild);

      inc(i);
    end;
  end;
begin
  if not VerificarErrosDeConfiguracaoEmComponente(tabConfigNFCe) then
  begin
    TabForward(tabConfig);
    tabsConfig.ActiveTab := tabConfigNFCe;
    Abort;
  end;
end;

procedure TACBrNFCeTestForm.VerificarErrosDeConfiguracaoEmail;
var
  MsgErro: string;
begin
  MsgErro := '';
  if (edtEmailFrom.Text = '') then
    MsgErro := 'Remetente não configurado'
  else if ( (edtEmailHost.Text = '') or (sbEmailPort.Value < 1)) then
    MsgErro := 'SMTP não configurado'
  else if ( (edtEmailUser.Text = '') or (edtEmailPassword.Text = '')) then
    MsgErro := 'Login não configurado'
  else if (edtAddressEmail.Text = '') then
    MsgErro := 'Destinatário não configurada'
  else if (edtSubject.Text = '') then
    MsgErro := 'Assunto não configurada';

  if (MsgErro <> '') then
  begin
    Toast(MsgErro);
    TabForward(tabConfig);
    tabsConfig.ActiveTab := tabConfigEmail;
    Abort;
  end;
end;

procedure TACBrNFCeTestForm.VerificarErrosDeConfiguracaoImpressora;
var
  MsgErro: string;
begin
  MsgErro := '';
  if (cbxImpressorasBth.Selected = nil) then
    MsgErro := 'Impressora não configurada'
  else if (cbxModelo.Selected = nil) then
    MsgErro := 'Modelo de Impressora não configurada';

  if (MsgErro <> '') then
  begin
    Toast(MsgErro);
    TabForward(tabConfig);
    tabsConfig.ActiveTab := tabConfigPosPrinter;
    Abort;
  end;
end;

procedure TACBrNFCeTestForm.LerConfiguracao;
var
  IniFile: string;
  Ini: TIniFile;
  cUF: Integer;
begin
  IniFile := CalcularNomeArqConfiguracao;
  Ini := TIniFile.Create(IniFile{$IfDef POSIX}, TEncoding.ANSI{$EndIf});
  try
    // configurações do ACBrPosPrinter //
    if cbxImpressorasBth.Items.Count < 1 then
      CarregarImpressorasBth;

    rbClasseInterna.IsChecked := INI.ReadBool('PosPrinter','ClasseInterna', True);
    rbClasseExterna.IsChecked := not rbClasseInterna.IsChecked;
    rbMudaClasseImpressora(nil);
    cbxImpressorasBth.ItemIndex := cbxImpressorasBth.Items.IndexOf(Ini.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta));
    cbxModelo.ItemIndex := Ini.ReadInteger('PosPrinter','Modelo', 1);
    cbxPagCodigo.ItemIndex := Ini.ReadInteger('PosPrinter','PaginaDeCodigo',Integer(ACBrPosPrinter1.PaginaDeCodigo));
    seColunas.Value := Ini.ReadInteger('PosPrinter','Colunas', 32);
    seEspLinhas.Value := Ini.ReadInteger('PosPrinter','EspacoEntreLinhas', ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value := Ini.ReadInteger('PosPrinter','LinhasPular', ACBrPosPrinter1.LinhasEntreCupons);
    seLarguraModulo.Value := INI.ReadInteger('PosPrinter','LarguraModulo', 4);
    cbImprimirLogo.IsChecked := Ini.ReadBool('PosPrinter', 'Logo', False);
    seKC1.Value := Ini.ReadInteger('PosPrinter.Logo','KC1', 1);
    seKC2.Value := Ini.ReadInteger('PosPrinter.Logo','KC2', 0);

    // Configurações de ACBrNFeDANFeESCPOS //
    cbQRCodeLateral.IsChecked := Ini.ReadBool('DANFCE', 'QrCodeLateral', False);
    cbImprimir1Linha.IsChecked := Ini.ReadBool('DANFCE', 'ItemUmaLinha', True);
    cbImprimirDescAcres.IsChecked := Ini.ReadBool('DANFCE', 'ItemDescAcres', False);
    cbLogoLateral.IsChecked := Ini.ReadBool('DANFCE', 'LogoLateral', False);

    // Configurações do ACBrMail //
    edtEmailFrom.text := Ini.ReadString('Email', 'From', ACBrMail1.From);
    edtEmailFromName.text := Ini.ReadString('Email', 'FromName', ACBrMail1.FromName);
    edtEmailHost.text := Ini.ReadString('Email', 'Host', ACBrMail1.Host);
    sbEmailPort.Value := Ini.ReadInteger('Email', 'Port', StrToIntDef(ACBrMail1.Port,0));
    edtEmailUser.text := Ini.ReadString('Email', 'User', ACBrMail1.Username);
    edtEmailPassword.text := Ini.ReadString('Email', 'Pass', ACBrMail1.Password);
    chkEmailTLS.IsChecked := Ini.ReadBool('Email', 'TLS', ACBrMail1.SetTLS);
    chkEmailSSL.IsChecked := Ini.ReadBool('Email', 'SSL', ACBrMail1.SetSSL);
    cbEmailDefaultCharset.ItemIndex := Ini.ReadInteger('Email', 'DefaultCharset', Integer(ACBrMail1.DefaultCharset));
    cbEmailIdeCharSet.ItemIndex := Ini.ReadInteger('Email', 'IdeCharset', Integer(ACBrMail1.IDECharset));
    edtAddressEmail.Text := Ini.ReadString('Email', 'AddressEmail', edtAddressEmail.Text);
    edtAddressName.Text := Ini.ReadString('Email', 'AddressName', edtAddressName.Text);
    edtSubject.Text := Ini.ReadString('Email', 'Subject', edtSubject.Text);
    mAltBody.Text := StringToBinaryString(
       Ini.ReadString('Email', 'Mensagem', BinaryStringToString(mAltBody.Text)) );

    // Configurações de ACBrNFe //
    edtConfCertURL.Text := Ini.ReadString('Certificado', 'URL', ACBrNFe1.SSL.URLPFX);
    edtConfCertPFX.Text  := Ini.ReadString('Certificado', 'Caminho', ACBrNFe1.SSL.ArquivoPFX);
    edtConfCertURL.Text  := Ini.ReadString('Certificado', 'URL', ACBrNFe1.SSL.URLPFX);
    edtConfCertSenha.Text := Ini.ReadString('Certificado', 'Senha', ACBrNFe1.SSL.Senha);

    cbxWebServiceUF.ItemIndex := cbxWebServiceUF.Items.IndexOf(Ini.ReadString('WebService', 'UF', ACBrNFe1.Configuracoes.WebServices.UF));
    swWebServiceAmbiente.IsChecked := (Ini.ReadInteger('WebService', 'Ambiente', Integer(ACBrNFe1.Configuracoes.WebServices.Ambiente)) = 0);
    sbWebServiceTimeout.Value := Ini.ReadInteger('WebService', 'TimeOut', ACBrNFe1.Configuracoes.WebServices.TimeOut);
    cbxWebServiceSSLType.ItemIndex := Ini.ReadInteger('WebService', 'SSLType', Integer(ACBrNFe1.Configuracoes.WebServices.SSLType));

    edtTokenID.Text := Ini.ReadString( 'Token', 'IdCSCn', ACBrNFe1.Configuracoes.Geral.IdCSC);
    edtTokenCSC.Text := Ini.ReadString( 'Token', 'CSC', ACBrNFe1.Configuracoes.Geral.CSC);

    // Configurações do Emitente //
    edtEmitCNPJ.Text := Ini.ReadString('Emitente', 'CNPJ', '');
    edtEmitIE.Text := Ini.ReadString('Emitente', 'IE', '');
    edtEmitRazao.Text := Ini.ReadString('Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text := Ini.ReadString('Emitente', 'Fantasia', '');
    edtEmitFone.Text := Ini.ReadString('Emitente', 'Fone', '');
    edtEmitCEP.Text := Ini.ReadString('Emitente', 'CEP', '');
    edtEmitLogradouro.Text := Ini.ReadString('Emitente', 'Logradouro', '');
    edtEmitNumero.Text := Ini.ReadString('Emitente', 'Numero', '');
    edtEmitComp.Text := Ini.ReadString('Emitente', 'Complemento', '');
    edtEmitBairro.Text := Ini.ReadString('Emitente', 'Bairro', '');
    cUF := Ini.ReadInteger('Emitente', 'cUF', 0);
    cbxTipoEmpresa.ItemIndex := INI.ReadInteger('Emitente','CRT', 0);
    lEmitcUF.Text := IntToStr(cUF);
    CarregarListaDeCidades;
    cbxEmitUF.ItemIndex := cbxEmitUF.Items.IndexOf(CUFtoUF(cUF));
    cbxEmitCidade.ItemIndex := FcMunList.IndexOf(IntToStr(Ini.ReadInteger('Emitente', 'cMun', 0)));

    // Configurações do Proxy //
    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', ACBrNFe1.Configuracoes.WebServices.ProxyHost);
    sbProxyPort.Value := Ini.ReadInteger('Proxy', 'Porta', StrToIntDef(ACBrNFe1.Configuracoes.WebServices.ProxyPort,0));
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', ACBrNFe1.Configuracoes.WebServices.ProxyUser);
    edtProxyPass.Text := Ini.ReadString('Proxy', 'Pass', ACBrNFe1.Configuracoes.WebServices.ProxyPass);

    swOnLine.IsChecked := (Ini.ReadInteger('NFCe', 'TipoEmissao', 0) = 0);
    sbProximaNFCe.Value := Ini.ReadInteger('NFCe', 'ProximoNumero', 1);
    sbLote.Value := Ini.ReadInteger('NFCe', 'Lote', 1);
  finally
    Ini.Free;
  end;

  // Calculando Alertas dos Campos //
  edtEmitCNPJTyping(nil);
  edtEmitRazaoTyping(nil);
  edtEmitCEPTyping(nil);
  edtEmitFoneTyping(nil);
  edtConfCertURLTyping(nil);
  edtTokenIDTyping(nil);
  edtTokenCSCTyping(nil);
  swWebServiceAmbienteSwitch(nil);
  swOnLineSwitch(nil);
end;

function TACBrNFCeTestForm.LerXMLNFCe(const ArquivoXML: String): Integer;
begin
  ACBrNFe1.NotasFiscais.Clear;
  ACBrNFe1.NotasFiscais.LoadFromFile(ArquivoXML, True);
  Result := ACBrNFe1.NotasFiscais.Count;
end;

procedure TACBrNFCeTestForm.CriarNFCe;
var
  Ok: Boolean;
  BaseCalculo,
  ValorICMS: Double;
begin
  ACBrNFe1.NotasFiscais.Clear;
  with ACBrNFe1.NotasFiscais.Add.NFe do
  begin
    Ide.natOp     := 'VENDA';
    Ide.indPag    := ipVista;
    Ide.modelo    := 65;
    Ide.serie     := 1;
    Ide.nNF       := Trunc(sbProximaNFCe.Value);
    Ide.cNF       := GerarCodigoDFe(Ide.nNF);
    Ide.dEmi      := now;
    Ide.dSaiEnt   := now;
    Ide.hSaiEnt   := now;
    Ide.tpNF      := tnSaida;
    Ide.tpEmis    := TpcnTipoEmissao( IfThen(swOnLine.IsChecked, 0, 8) );
    Ide.tpAmb     := TpcnTipoAmbiente( IfThen(swWebServiceAmbiente.IsChecked, 0, 1) );
    Ide.cUF       := StrToInt(lEmitcUF.Text);
    Ide.cMunFG    := StrToInt(lEmitcMun.Text);
    Ide.finNFe    := fnNormal;
    Ide.tpImp     := tiNFCe;
    Ide.indFinal  := cfConsumidorFinal;
    Ide.indPres   := pcPresencial;

    if not swOnLine.IsChecked then
    begin
      Ide.dhCont := date;
      Ide.xJust  := 'Justificativa Contingencia';
    end;

    Emit.CNPJCPF           := OnlyNumber(edtEmitCNPJ.Text);
    Emit.IE                := OnlyNumber(edtEmitIE.Text);
    Emit.xNome             := edtEmitRazao.Text;
    Emit.xFant             := edtEmitFantasia.Text;

    Emit.EnderEmit.fone    := edtEmitFone.Text;
    Emit.EnderEmit.CEP     := StrToInt(OnlyNumber(edtEmitCEP.Text));
    Emit.EnderEmit.xLgr    := edtEmitLogradouro.Text;
    Emit.EnderEmit.nro     := edtEmitNumero.Text;
    Emit.EnderEmit.xCpl    := edtEmitComp.Text;
    Emit.EnderEmit.xBairro := edtEmitBairro.Text;
    Emit.EnderEmit.cMun    := StrToInt(lEmitcMun.Text);
    Emit.EnderEmit.xMun    := cbxEmitCidade.Selected.Text;
    Emit.EnderEmit.UF      := cbxEmitUF.Selected.Text;
    Emit.enderEmit.cPais   := 1058;
    Emit.enderEmit.xPais   := 'BRASIL';

    Emit.IEST := '';
    // esta sendo somando 1 uma vez que o ItemIndex inicia do zero e devemos
    // passar os valores 1, 2 ou 3
    // (1-crtSimplesNacional, 2-crtSimplesExcessoReceita, 3-crtRegimeNormal)
    Emit.CRT  := StrToCRT(Ok, IntToStr(cbxTipoEmpresa.ItemIndex + 1));

    // Na NFC-e o Destinatário é opcional
    {
    Dest.CNPJCPF           := 'informar o CPF do destinatário';
    Dest.ISUF              := '';
    Dest.xNome             := 'nome do destinatário';

    Dest.indIEDest         := inNaoContribuinte;

    Dest.EnderDest.Fone    := '1533243333';
    Dest.EnderDest.CEP     := 18270170;
    Dest.EnderDest.xLgr    := 'Rua Coronel Aureliano de Camargo';
    Dest.EnderDest.nro     := '973';
    Dest.EnderDest.xCpl    := '';
    Dest.EnderDest.xBairro := 'Centro';
    Dest.EnderDest.cMun    := 3554003;
    Dest.EnderDest.xMun    := 'Tatuí';
    Dest.EnderDest.UF      := 'SP';
    Dest.EnderDest.cPais   := 1058;
    Dest.EnderDest.xPais   := 'BRASIL';
    }

//Use os campos abaixo para informar o endereço de retirada quando for diferente do Remetente/Destinatário
    Retirada.CNPJCPF := '';
    Retirada.xLgr    := '';
    Retirada.nro     := '';
    Retirada.xCpl    := '';
    Retirada.xBairro := '';
    Retirada.cMun    := 0;
    Retirada.xMun    := '';
    Retirada.UF      := '';

//Use os campos abaixo para informar o endereço de entrega quando for diferente do Remetente/Destinatário
    Entrega.CNPJCPF := '';
    Entrega.xLgr    := '';
    Entrega.nro     := '';
    Entrega.xCpl    := '';
    Entrega.xBairro := '';
    Entrega.cMun    := 0;
    Entrega.xMun    := '';
    Entrega.UF      := '';

//Adicionando Produtos
    with Det.New do
    begin
      Prod.nItem    := 1; // Número sequencial, para cada item deve ser incrementado
      Prod.cProd    := '123456';
      Prod.cEAN     := '7896523206646';
      Prod.xProd    := 'Camisa Polo ACBr';
      Prod.NCM      := '61051000'; // Tabela NCM disponível em  http://www.receita.fazenda.gov.br/Aliquotas/DownloadArqTIPI.htm
      Prod.EXTIPI   := '';
      Prod.CFOP     := '5101';
      Prod.uCom     := 'UN';
      Prod.qCom     := 1;
      Prod.vUnCom   := 100;
      Prod.vProd    := 100;

      Prod.cEANTrib  := '7896523206646';
      Prod.uTrib     := 'UN';
      Prod.qTrib     := 1;
      Prod.vUnTrib   := 100;

      Prod.vOutro    := 0;
      Prod.vFrete    := 0;
      Prod.vSeg      := 0;
      Prod.vDesc     := 0;

      Prod.CEST := '1111111';

//         infAdProd      := 'Informação Adicional do Produto';

      with Imposto do
      begin
        // lei da transparencia nos impostos
        vTotTrib := 0;

        with ICMS do
        begin
          // caso o CRT seja:
          // 1=Simples Nacional
          // Os valores aceitos para CSOSN são:
          // csosn101, csosn102, csosn103, csosn201, csosn202, csosn203,
          // csosn300, csosn400, csosn500,csosn900

          // 2=Simples Nacional, excesso sublimite de receita bruta;
          // ou 3=Regime Normal.
          // Os valores aceitos para CST são:
          // cst00, cst10, cst20, cst30, cst40, cst41, cst45, cst50, cst51,
          // cst60, cst70, cst80, cst81, cst90, cstPart10, cstPart90,
          // cstRep41, cstVazio, cstICMSOutraUF, cstICMSSN, cstRep60

          // (consulte o contador do seu cliente para saber qual deve ser utilizado)
          // Pode variar de um produto para outro.

          if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
            CST := cst00
          else
            CSOSN := csosn102;

          orig    := oeNacional;
          modBC   := dbiValorOperacao;

          if Emit.CRT in [crtSimplesExcessoReceita, crtRegimeNormal] then
            BaseCalculo := 100
          else
            BaseCalculo := 0;

          vBC     := BaseCalculo;
          pICMS   := 18;

          ValorICMS := vBC * pICMS;

          vICMS   := ValorICMS;
          modBCST := dbisMargemValorAgregado;
          pMVAST  := 0;
          pRedBCST:= 0;
          vBCST   := 0;
          pICMSST := 0;
          vICMSST := 0;
          pRedBC  := 0;

          pCredSN := 5;
          vCredICMSSN := 50;
          vBCFCPST := 100;
          pFCPST := 2;
          vFCPST := 2;
          vBCSTRet := 0;
          pST := 0;
          vICMSSubstituto := 0;
          vICMSSTRet := 0;
          vBCFCPSTRet := 0;
          pFCPSTRet := 0;
          vFCPSTRet := 0;
          pRedBCEfet := 0;
          vBCEfet := 0;
          pICMSEfet := 0;
          vICMSEfet := 0;

          // partilha do ICMS e fundo de probreza
          with ICMSUFDest do
          begin
            vBCUFDest      := 0.00;
            pFCPUFDest     := 0.00;
            pICMSUFDest    := 0.00;
            pICMSInter     := 0.00;
            pICMSInterPart := 0.00;
            vFCPUFDest     := 0.00;
            vICMSUFDest    := 0.00;
            vICMSUFRemet   := 0.00;
          end;
        end;

        with PIS do
        begin
          CST      := pis99;
          PIS.vBC  := 0;
          PIS.pPIS := 0;
          PIS.vPIS := 0;

          PIS.qBCProd   := 0;
          PIS.vAliqProd := 0;
          PIS.vPIS      := 0;
        end;

        with PISST do
        begin
          vBc       := 0;
          pPis      := 0;
          qBCProd   := 0;
          vAliqProd := 0;
          vPIS      := 0;
        end;

        with COFINS do
        begin
          CST            := cof99;
          COFINS.vBC     := 0;
          COFINS.pCOFINS := 0;
          COFINS.vCOFINS := 0;

          COFINS.qBCProd   := 0;
          COFINS.vAliqProd := 0;
        end;

        with COFINSST do
        begin
          vBC       := 0;
          pCOFINS   := 0;
          qBCProd   := 0;
          vAliqProd := 0;
          vCOFINS   := 0;
        end;
      end;
    end;

    Total.ICMSTot.vBC     := BaseCalculo;
    Total.ICMSTot.vICMS   := ValorICMS;
    Total.ICMSTot.vBCST   := 0;
    Total.ICMSTot.vST     := 0;
    Total.ICMSTot.vProd   := 100;
    Total.ICMSTot.vFrete  := 0;
    Total.ICMSTot.vSeg    := 0;
    Total.ICMSTot.vDesc   := 0;
    Total.ICMSTot.vII     := 0;
    Total.ICMSTot.vIPI    := 0;
    Total.ICMSTot.vPIS    := 0;
    Total.ICMSTot.vCOFINS := 0;
    Total.ICMSTot.vOutro  := 0;
    Total.ICMSTot.vNF     := 100;

    // partilha do icms e fundo de probreza
    Total.ICMSTot.vFCPUFDest   := 0.00;
    Total.ICMSTot.vICMSUFDest  := 0.00;
    Total.ICMSTot.vICMSUFRemet := 0.00;

    Total.ISSQNtot.vServ   := 0;
    Total.ISSQNTot.vBC     := 0;
    Total.ISSQNTot.vISS    := 0;
    Total.ISSQNTot.vPIS    := 0;
    Total.ISSQNTot.vCOFINS := 0;

    Total.retTrib.vRetPIS    := 0;
    Total.retTrib.vRetCOFINS := 0;
    Total.retTrib.vRetCSLL   := 0;
    Total.retTrib.vBCIRRF    := 0;
    Total.retTrib.vIRRF      := 0;
    Total.retTrib.vBCRetPrev := 0;
    Total.retTrib.vRetPrev   := 0;

    Transp.modFrete := mfSemFrete; // NFC-e não pode ter FRETE

    with pag.New do
    begin
      tPag := fpDinheiro;
      vPag := 100;
    end;

    InfAdic.infCpl     :=  '';
    InfAdic.infAdFisco :=  '';

    with InfAdic.obsCont.New do
    begin
      xCampo := 'ObsCont';
      xTexto := 'Texto';
    end;

    with InfAdic.obsFisco.New do
    begin
      xCampo := 'ObsFisco';
      xTexto := 'Texto';
    end;
  end;

  ACBrNFe1.NotasFiscais.GerarNFe;
end;

end.

