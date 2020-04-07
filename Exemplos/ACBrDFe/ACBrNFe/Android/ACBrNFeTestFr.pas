unit ACBrNFeTestFr;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.ListBox, FMX.Layouts, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.ScrollBox, FMX.Memo, System.ImageList, FMX.ImgList, FMX.VirtualKeyboard,
  ACBrMail, ACBrBase, ACBrDFeReport, ACBrDFeDANFeReport, ACBrNFeDANFEClass,
  ACBrNFeDANFeESCPOS, ACBrPosPrinter, ACBrDFe, ACBrNFe, ACBrIBGE, ACBrSocket,
  ACBrCEP, FMX.Objects, FMX.Effects;

type
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
    lbTestes: TListBox;
    ListBoxGroupHeader5: TListBoxGroupHeader;
    lbiOpcoes: TListBoxItem;
    GridPanelLayout3: TGridPanelLayout;
    cbUsarTXT: TCheckBox;
    cbUsarHTML: TCheckBox;
    cbAddImgHTML: TCheckBox;
    cbAddImgAtt: TCheckBox;
    cbAddPDF: TCheckBox;
    cbAddXML: TCheckBox;
    cbUsarThread: TCheckBox;
    ListBoxGroupHeader7: TListBoxGroupHeader;
    libDestinatarioAssunto: TListBoxItem;
    GridPanelLayout4: TGridPanelLayout;
    Label7: TLabel;
    edtAddressEmail: TEdit;
    Label12: TLabel;
    edtAddressName: TEdit;
    tabLog: TTabItem;
    mLog: TMemo;
    ToolBar3: TToolBar;
    Label13: TLabel;
    SpeedButton1: TSpeedButton;
    ListBoxGroupHeader8: TListBoxGroupHeader;
    Label14: TLabel;
    edSubject: TEdit;
    libMensagens: TListBoxItem;
    lbiBotoesTestes: TListBoxItem;
    GridPanelLayout8: TGridPanelLayout;
    bEnviar: TCornerButton;
    bEnviarLote: TCornerButton;
    tabsMensagen: TTabControl;
    tabTexto: TTabItem;
    tabHTML: TTabItem;
    mAltBody: TMemo;
    mBody: TMemo;
    swHTML: TSwitch;
    Label15: TLabel;
    ProgressBar1: TProgressBar;
    GridPanelLayout9: TGridPanelLayout;
    btnLimpar: TCornerButton;
    btnVersaoOpenSSL: TCornerButton;
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
    cbQRCodeLateral: TCheckBox;
    cbLogoLateral: TCheckBox;
    GridPanelLayout13: TGridPanelLayout;
    cbxModelo: TComboBox;
    GridPanelLayout14: TGridPanelLayout;
    Label21: TLabel;
    Label22: TLabel;
    cbxPagCodigo: TComboBox;
    cbImprimir1Linha: TCheckBox;
    cbImprimirDescAcres: TCheckBox;
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
    procedure GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure btnBackClick(Sender: TObject);
    procedure btLerConfigClick(Sender: TObject);
    procedure btSalvarConfigClick(Sender: TObject);
    procedure swHTMLSwitch(Sender: TObject);
    procedure ACBrMail1BeforeMailProcess(Sender: TObject);
    procedure ACBrMail1MailException(const AMail: TACBrMail; const E: Exception;
      var ThrowIt: Boolean);
    procedure ACBrMail1MailProcess(const AMail: TACBrMail;
      const aStatus: TMailStatus);
    procedure sbEmailMostrarSenhaClick(Sender: TObject);
    procedure btnLimparClick(Sender: TObject);
    procedure btnVersaoOpenSSLClick(Sender: TObject);
    procedure ACBrMail1AfterMailProcess(Sender: TObject);
    procedure sbCertAcharPFXClick(Sender: TObject);
    procedure sbCertVerSenhaClick(Sender: TObject);
    procedure seProxyVerSenhaClick(Sender: TObject);
    procedure swWebServiceAmbienteSwitch(Sender: TObject);
    procedure sbAcharCEPClick(Sender: TObject);
    procedure cbxEmitUFChange(Sender: TObject);
    procedure cbxEmitCidadeChange(Sender: TObject);
    procedure EditApenasNumeros(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure edtEmitCEPValidating(Sender: TObject; var Text: string);
    procedure FormDestroy(Sender: TObject);
    procedure edtEmitCNPJValidating(Sender: TObject; var Text: string);
    procedure edtEmitCNPJTyping(Sender: TObject);
    procedure edtEmitCEPTyping(Sender: TObject);
    procedure edtEmitFoneTyping(Sender: TObject);
    procedure edtEmitFoneValidating(Sender: TObject; var Text: string);
    procedure edtEmitCEPValidate(Sender: TObject; var Text: string);
    procedure cbxWebServiceUFChange(Sender: TObject);
    procedure edtTokenCSCValidating(Sender: TObject; var Text: string);
    procedure edtTokenIDValidating(Sender: TObject; var Text: string);
    procedure edtConfCertURLValidating(Sender: TObject; var Text: string);
    procedure edtConfCertPFXValidating(Sender: TObject; var Text: string);
    procedure edtConfCertSenhaValidating(Sender: TObject; var Text: string);
    procedure btnProcurarBthClick(Sender: TObject);
  private
    { Private declarations }
    FVKService: IFMXVirtualKeyboardService;
    FcMunList: TStringList;
    FcUF: Integer;

    function CalcularNomeArqConfiguracao: String;
    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    procedure ConfigurarACBr;

    procedure CarregarImpressorasBth;

    procedure PedirPermissoesInternet;
    procedure IniciarTelaDeEspera(const AMsg: String = '');
    procedure TerminarTelaDeEspera;
    procedure ConsultarCEP;
    procedure CarregarListaDeCidades;
    function ValidarEditsCertificado(const URL, PFX, Pass: String): Boolean;
  public
    { Public declarations }
  end;

var
  ACBrNFCeTestForm: TACBrNFCeTestForm;

implementation

uses
  System.typinfo, System.IniFiles, System.StrUtils, System.Permissions,
  {$IfDef ANDROID}
  Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes, Androidapi.IOUtils,
  Androidapi.JNI.Widget,
  {$EndIf}
  FMX.DialogService, FMX.Platform,
  ssl_openssl_lib, blcksock,
  ACBrUtil, ACBrConsts, ACBrValidador, pcnConversao,
  FileSelectFr, System.Math;

{$R *.fmx}

procedure TACBrNFCeTestForm.FormCreate(Sender: TObject);
var
  m: TMailCharset;
  n: TACBrPosPrinterModelo;
  o: TACBrPosPaginaCodigo;
  p: TSSLType;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FVKService));
  FcMunList := TStringList.Create;
  FcUF := 0;

  imgErrorCep.Bitmap := ImageList1.Bitmap(TSizeF.Create(imgErrorCep.Width,imgErrorCep.Height),14);
  imgErrorCNPJ.Bitmap := imgErrorCep.Bitmap;
  imgErrorTelefone.Bitmap := imgErrorCep.Bitmap;
  imgErrorCidade.Bitmap := imgErrorCep.Bitmap;
  imgErrorUF.Bitmap := imgErrorCep.Bitmap;
  imgErrorIDCSC.Bitmap := imgErrorCep.Bitmap;
  imgErrorCSC.Bitmap := imgErrorCep.Bitmap;
  imgErrorWebServiceUF.Bitmap := imgErrorCep.Bitmap;
  imgErrorCert.Bitmap := imgErrorCep.Bitmap;

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

  tabsPrincipal.First;
  ProgressBar1.Visible := False;
  LerConfiguracao;
end;

procedure TACBrNFCeTestForm.FormDestroy(Sender: TObject);
begin
  FcMunList.Free;
end;

procedure TACBrNFCeTestForm.PedirPermissoesInternet;
Var
  Ok: Boolean;
begin
  Ok := True;
  {$IfDef ANDROID}
  PermissionsService.RequestPermissions( [JStringToString(TJManifest_permission.JavaClass.INTERNET)],
      procedure(const APermissions: TArray<string>; const AGrantResults: TArray<TPermissionStatus>)
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

procedure TACBrNFCeTestForm.sbAcharCEPClick(Sender: TObject);
var
  Erro: String;
begin
  if imgErrorCep.Visible then
  begin
    TDialogService.ShowMessage('CEP inválido');
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
  FrSlect.FileMask := '*.pfx';

  FrSlect.ShowModal(
    procedure(ModalResult : TModalResult)
    begin
      if ModalResult = mrOK then
        edtConfCertPFX.Text := FrSlect.FileName;
      end
  );
end;

procedure TACBrNFCeTestForm.sbCertVerSenhaClick(Sender: TObject);
begin
  edtConfCertSenha.Password := not sbCertVerSenha.IsPressed;
end;

procedure TACBrNFCeTestForm.swHTMLSwitch(Sender: TObject);
begin
  if swHTML.IsChecked then
    tabsMensagen.ActiveTab := tabHTML
  else
    tabsMensagen.ActiveTab := tabTexto;
end;

procedure TACBrNFCeTestForm.swWebServiceAmbienteSwitch(Sender: TObject);
begin
  if swWebServiceAmbiente.IsChecked then
    lAmbiente.Text := 'Produção'
  else
    lAmbiente.Text := 'Homologação'
end;

procedure TACBrNFCeTestForm.ACBrMail1AfterMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('Depois de Enviar o email: ' + TACBrMail(Sender).Subject);
  ProgressBar1.Visible := False;
end;

procedure TACBrNFCeTestForm.ACBrMail1BeforeMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('Antes de Enviar o email: ' + TACBrMail(Sender).Subject);
  ProgressBar1.Visible := True;
end;

procedure TACBrNFCeTestForm.ACBrMail1MailException(const AMail: TACBrMail;
  const E: Exception; var ThrowIt: Boolean);
begin
  ShowMessage(E.Message);
  ThrowIt := False;
  mLog.Lines.Add('*** Erro ao Enviar o email: ' + AMail.Subject);
  ProgressBar1.Visible := False;
end;

procedure TACBrNFCeTestForm.ACBrMail1MailProcess(const AMail: TACBrMail;
  const aStatus: TMailStatus);
begin
  ProgressBar1.Value := Integer(aStatus);
  ProgressBar1.Visible := True;

  case aStatus of
    pmsStartProcess:
      mLog.Lines.Add('Iniciando processo de envio.');
    pmsConfigHeaders:
      mLog.Lines.Add('Configurando o cabeçalho do e-mail.');
    pmsLoginSMTP:
      mLog.Lines.Add('Logando no servidor de e-mail.');
    pmsStartSends:
      mLog.Lines.Add('Iniciando os envios.');
    pmsSendTo:
      mLog.Lines.Add('Processando lista de destinatários.');
    pmsSendCC:
      mLog.Lines.Add('Processando lista CC.');
    pmsSendBCC:
      mLog.Lines.Add('Processando lista BCC.');
    pmsSendReplyTo:
      mLog.Lines.Add('Processando lista ReplyTo.');
    pmsSendData:
      mLog.Lines.Add('Enviando dados.');
    pmsLogoutSMTP:
      mLog.Lines.Add('Fazendo Logout no servidor de e-mail.');
    pmsDone:
      begin
        mLog.Lines.Add('Terminando e limpando.');
        ProgressBar1.Value := ProgressBar1.Max;
      end;
  end;

  mLog.Lines.Add('   ' + AMail.Subject);
end;

procedure TACBrNFCeTestForm.btLerConfigClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TACBrNFCeTestForm.btnBackClick(Sender: TObject);
begin
  tabsPrincipal.Previous;
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

procedure TACBrNFCeTestForm.btnVersaoOpenSSLClick(Sender: TObject);
var
  AMsg: string;
begin
  ACBrNFe1.SSL.CarregarCertificado;
  mLog.Lines.Add('');
  mLog.Lines.Add('---- Informações do Certificado ----');
  mLog.Lines.Add('Número de Série: '+ACBrNFe1.SSL.CertNumeroSerie);
  mLog.Lines.Add('Válido até: '+FormatDateBr(ACBrNFe1.SSL.CertDataVenc));
  mLog.Lines.Add('Subject Name: '+ACBrNFe1.SSL.CertSubjectName);
  mLog.Lines.Add('Razão Social: ' + ACBrNFe1.SSL.CertRazaoSocial);
  mLog.Lines.Add('CNPJ/CPF: ' + ACBrNFe1.SSL.CertCNPJ);
  mLog.Lines.Add('Emissor: ' + ACBrNFe1.SSL.CertIssuerName);
  mLog.Lines.Add('Certificadora: ' + ACBrNFe1.SSL.CertCertificadora);
  tabsPrincipal.ActiveTab := tabLog;
end;

procedure TACBrNFCeTestForm.btSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

function TACBrNFCeTestForm.CalcularNomeArqConfiguracao: String;
begin
  Result := ApplicationPath + 'ACBrNFeTeste.ini';
end;

procedure TACBrNFCeTestForm.CarregarImpressorasBth;
begin
  PedirPermissoesInternet;
  cbxImpressorasBth.Items.Clear;
  ACBrPosPrinter1.Device.AcharPortasBlueTooth( cbxImpressorasBth.Items, chbTodasBth.IsChecked );
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
      TThread.Synchronize(nil, procedure
      begin
        lMsgAguarde.Text := 'Erro ao carregar cidades';
      end);
      sleep(1500);
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
        TerminarTelaDeEspera;
      end;
    end);
  end;
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
      IniciarTelaDeEspera('Carregando Cidades');
      TThread.CreateAnonymousThread(CarregarListaDeCidades).Start;
    end;
  end;
end;

procedure TACBrNFCeTestForm.cbxWebServiceUFChange(Sender: TObject);
begin
  imgErrorWebServiceUF.Visible := (cbxWebServiceUF.ItemIndex < 0);
end;

procedure TACBrNFCeTestForm.sbEmailMostrarSenhaClick(Sender: TObject);
begin
  edtEmailPassword.Password := not sbEmailMostrarSenha.IsPressed;
end;

procedure TACBrNFCeTestForm.seProxyVerSenhaClick(Sender: TObject);
begin
  edtProxyPass.Password := not seProxyVerSenha.IsPressed;
end;

procedure TACBrNFCeTestForm.ConfigurarACBr;
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

  // Configurando o ACBrPosPrinter //
  if Assigned(cbxImpressorasBth.Selected) then
    ACBrPosPrinter1.Porta := cbxImpressorasBth.Selected.Text;

  if Assigned(cbxModelo.Selected) then
    ACBrPosPrinter1.Modelo := TACBrPosPrinterModelo(cbxModelo.ItemIndex);

  ACBrPosPrinter1.ColunasFonteNormal := Trunc(seColunas.Value);
  ACBrPosPrinter1.EspacoEntreLinhas := Trunc(seEspLinhas.Value);
  ACBrPosPrinter1.LinhasEntreCupons := Trunc(seLinhasPular.Value);
  ACBrPosPrinter1.ConfigLogo.KeyCode1 := Trunc(seKC1.Value);
  ACBrPosPrinter1.ConfigLogo.KeyCode2 := Trunc(seKC2.Value);
  ACBrPosPrinter1.ConfigLogo.IgnorarLogo := not cbImprimirLogo.IsChecked;

  // Configurando ACBrNFeDANFeESCPOS //
  ACBrNFeDANFeESCPOS1.ImprimeLogoLateral := cbLogoLateral.IsChecked;
  ACBrNFeDANFeESCPOS1.ImprimeQRCodeLateral := cbQRCodeLateral.IsChecked;
  ACBrNFeDANFeESCPOS1.ImprimeEmUmaLinha := cbImprimir1Linha.IsChecked;
  ACBrNFeDANFeESCPOS1.ImprimeDescAcrescItem := cbImprimirDescAcres.IsChecked;

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
      TThread.Synchronize(nil, procedure
      begin
        lMsgAguarde.Text := 'Erro ao consultar o CEP: '+edtEmitCEP.Text;
      end);
      Sleep(1500);
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

procedure TACBrNFCeTestForm.EditApenasNumeros(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if not CharIsNum(KeyChar) then
    KeyChar := #0;
end;

procedure TACBrNFCeTestForm.edtConfCertPFXValidating(Sender: TObject;
  var Text: string);
begin
  imgErrorCert.Visible := not ValidarEditsCertificado(edtConfCertURL.Text, Text, edtConfCertSenha.Text);
end;

procedure TACBrNFCeTestForm.edtConfCertSenhaValidating(Sender: TObject;
  var Text: string);
begin
  imgErrorCert.Visible := not ValidarEditsCertificado(edtConfCertURL.Text, edtConfCertPFX.Text, Text);
end;

procedure TACBrNFCeTestForm.edtConfCertURLValidating(Sender: TObject;
  var Text: string);
begin
  imgErrorCert.Visible := not ValidarEditsCertificado(Text, edtConfCertPFX.Text, edtConfCertSenha.Text);
end;

procedure TACBrNFCeTestForm.edtEmitCEPTyping(Sender: TObject);
begin
  if (edtEmitCEP.Text.Length > 5) then
  begin
    edtEmitCEP.Text := FormatarMascaraDinamica(OnlyNumber(edtEmitCEP.Text), '*****-***');
    edtEmitCEP.CaretPosition := edtEmitCEP.Text.Length;
  end;
end;

procedure TACBrNFCeTestForm.edtEmitCEPValidate(Sender: TObject;
  var Text: string);
begin
  if not imgErrorCep.Visible then
    sbAcharCEPClick(Sender);
end;

procedure TACBrNFCeTestForm.edtEmitCEPValidating(Sender: TObject;
  var Text: string);
begin
  imgErrorCep.Visible := (Text.Length < 9);
end;

procedure TACBrNFCeTestForm.edtEmitCNPJTyping(Sender: TObject);
begin
  if (edtEmitCNPJ.Text.Length > 2) then
  begin
    edtEmitCNPJ.Text := ACBrValidador.FormatarMascaraDinamica(OnlyNumber(edtEmitCNPJ.Text), '**.***.***/****-**');
    edtEmitCNPJ.CaretPosition := edtEmitCNPJ.Text.Length;
  end;
end;

procedure TACBrNFCeTestForm.edtEmitCNPJValidating(Sender: TObject;
  var Text: string);
begin
  imgErrorCNPJ.Visible := (Text.Length < 18) or
                          (not ACBrValidador.ValidarCNPJ(Text).IsEmpty);
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
end;

procedure TACBrNFCeTestForm.edtEmitFoneValidating(Sender: TObject;
  var Text: string);
var
  AStr: string;
begin
  AStr := OnlyNumber(Text);
  imgErrorTelefone.Visible := (AStr.Length < 10);
end;

procedure TACBrNFCeTestForm.edtTokenCSCValidating(Sender: TObject;
  var Text: string);
begin
  imgErrorCSC.Visible := Text.IsEmpty
end;

procedure TACBrNFCeTestForm.edtTokenIDValidating(Sender: TObject;
  var Text: string);
begin
  imgErrorIDCSC.Visible := Text.IsEmpty;
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

    if (tabsPrincipal.ActiveTab = tabTeste) then
    begin
      tabsPrincipal.Previous;
      Key := 0;
    end;
  end;
end;

procedure TACBrNFCeTestForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  case EventInfo.GestureID of
    sgiLeft:
      begin
        if tabsPrincipal.ActiveTab <> tabsPrincipal.Tabs[tabsPrincipal.TabCount - 1] then
          tabsPrincipal.Next;
        Handled := True;
      end;

    sgiRight:
      begin
        if tabsPrincipal.ActiveTab <> tabsPrincipal.Tabs[0] then
          tabsPrincipal.Previous;
        Handled := True;
      end;
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
    if Assigned(cbxImpressorasBth.Selected) then
      INI.WriteString('PosPrinter','Porta', cbxImpressorasBth.Selected.Text);
    INI.WriteInteger('PosPrinter','Modelo', cbxModelo.ItemIndex);
    INI.WriteInteger('PosPrinter','PaginaDeCodigo',cbxPagCodigo.ItemIndex);
    INI.WriteInteger('PosPrinter','Colunas', Trunc(seColunas.Value) );
    INI.WriteInteger('PosPrinter','EspacoEntreLinhas', Trunc(seEspLinhas.Value) );
    INI.WriteInteger('PosPrinter','LinhasPular', Trunc(seLinhasPular.Value) );
    Ini.WriteBool('PosPrinter', 'Logo', cbImprimirLogo.IsChecked);
    INI.WriteInteger('PosPrinter.Logo','KC1',Trunc(seKC1.Value));
    INI.WriteInteger('PosPrinter.Logo','KC2',Trunc(seKC2.Value));

    // Configurações de ACBrNFeDANFeESCPOS //
    Ini.WriteBool('DANFCE', 'QrCodeLateral', cbQRCodeLateral.IsChecked);
    Ini.WriteBool('DANFCE', 'ItemUmaLinha', cbImprimir1Linha.IsChecked);
    Ini.WriteBool('DANFCE', 'ItemDescAcres', cbImprimirDescAcres.IsChecked);
    Ini.WriteBool('DANFCE', 'LogoLateral', cbLogoLateral.IsChecked);

    // Configurações do ACBrMail //
    Ini.WriteString('Email', 'From', edtEmailFrom.text);
    Ini.WriteString('Email', 'FromName', edtEmailFromName.text);
    Ini.WriteString('Email', 'Host', edtEmailHost.text);
    Ini.WriteInteger('Email', 'Port', Trunc(sbEmailPort.Value));
    Ini.WriteString('Email', 'User', edtEmailUser.text);
    Ini.WriteString('Email', 'Pass', edtEmailPassword.text);
    Ini.WriteBool('Email', 'TLS', chkEmailTLS.IsChecked);
    Ini.WriteBool('Email', 'SSL', chkEmailSSL.IsChecked);
    Ini.WriteInteger('Email', 'DefaultCharset', cbEmailDefaultCharset.ItemIndex);
    Ini.WriteInteger('Email', 'IdeCharset', cbEmailIdeCharSet.ItemIndex);

    // Configurações de ACBrNFe //
    Ini.WriteString( 'Certificado', 'URL', edtConfCertURL.Text);
    Ini.WriteString( 'Certificado', 'Caminho', edtConfCertPFX.Text);
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

    // Configurações do Proxy //
    Ini.WriteString('Proxy', 'Host', edtProxyHost.Text);
    Ini.WriteInteger('Proxy', 'Porta', Trunc(sbProxyPort.Value));
    Ini.WriteString('Proxy', 'User', edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass', edtProxyPass.Text);
  finally
    Ini.Free;
  end;
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

procedure TACBrNFCeTestForm.TerminarTelaDeEspera;
begin
  lWait.Visible := False;
  AniIndicator1.Enabled := False;
end;

function TACBrNFCeTestForm.ValidarEditsCertificado(const URL, PFX, Pass: String): Boolean;
var
  Ok: Boolean;
begin
  Ok := not Pass.IsEmpty;
  if Ok then
    Ok := (not URL.IsEmpty) or (not PFX.IsEmpty);

  if Ok and (not PFX.IsEmpty) then
    Ok := FileExists(PFX);

  Result := Ok;
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

    cbxImpressorasBth.ItemIndex := cbxImpressorasBth.Items.IndexOf(Ini.ReadString('PosPrinter','Porta',ACBrPosPrinter1.Porta));
    cbxModelo.ItemIndex := Ini.ReadInteger('PosPrinter','Modelo', Integer(ACBrPosPrinter1.Modelo));
    cbxPagCodigo.ItemIndex := Ini.ReadInteger('PosPrinter','PaginaDeCodigo',Integer(ACBrPosPrinter1.PaginaDeCodigo));
    seColunas.Value := Ini.ReadInteger('PosPrinter','Colunas', ACBrPosPrinter1.Colunas);
    seEspLinhas.Value := Ini.ReadInteger('PosPrinter','EspacoEntreLinhas', ACBrPosPrinter1.EspacoEntreLinhas);
    seLinhasPular.Value := Ini.ReadInteger('PosPrinter','LinhasPular', ACBrPosPrinter1.LinhasEntreCupons);
    cbImprimirLogo.IsChecked := Ini.ReadBool('PosPrinter', 'Logo', not ACBrPosPrinter1.ConfigLogo.IgnorarLogo);
    seKC1.Value := Ini.ReadInteger('PosPrinter.Logo','KC1', ACBrPosPrinter1.ConfigLogo.KeyCode1);
    seKC2.Value := Ini.ReadInteger('PosPrinter.Logo','KC2', ACBrPosPrinter1.ConfigLogo.KeyCode2);

    // Configurações de ACBrNFeDANFeESCPOS //
    cbQRCodeLateral.IsChecked := Ini.ReadBool('DANFCE', 'QrCodeLateral', ACBrNFeDANFeESCPOS1.ImprimeQRCodeLateral);
    cbImprimir1Linha.IsChecked := Ini.ReadBool('DANFCE', 'ItemUmaLinha', ACBrNFeDANFeESCPOS1.ImprimeEmUmaLinha);
    cbImprimirDescAcres.IsChecked := Ini.ReadBool('DANFCE', 'ItemDescAcres', ACBrNFeDANFeESCPOS1.ImprimeDescAcrescItem);
    cbLogoLateral.IsChecked := Ini.ReadBool('DANFCE', 'LogoLateral', ACBrNFeDANFeESCPOS1.ImprimeLogoLateral);

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

    // Configurações de ACBrNFe //
    edtConfCertURL.Text := Ini.ReadString('Certificado', 'URL', ACBrNFe1.SSL.URLPFX);
    edtConfCertPFX.Text  := Ini.ReadString('Certificado', 'Caminho', ACBrNFe1.SSL.ArquivoPFX);
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
    lEmitcUF.Text := IntToStr(cUF);
    CarregarListaDeCidades;
    cbxEmitUF.ItemIndex := cbxEmitUF.Items.IndexOf(CUFtoUF(cUF));
    cbxEmitCidade.ItemIndex := FcMunList.IndexOf(IntToStr(Ini.ReadInteger('Emitente', 'cMun', 0)));

    // Configurações do Proxy //
    edtProxyHost.Text := Ini.ReadString('Proxy', 'Host', ACBrNFe1.Configuracoes.WebServices.ProxyHost);
    sbProxyPort.Value := Ini.ReadInteger('Proxy', 'Porta', StrToIntDef(ACBrNFe1.Configuracoes.WebServices.ProxyPort,0));
    edtProxyUser.Text := Ini.ReadString('Proxy', 'User', ACBrNFe1.Configuracoes.WebServices.ProxyUser);
    edtProxyPass.Text := Ini.ReadString('Proxy', 'Pass', ACBrNFe1.Configuracoes.WebServices.ProxyPass);
  finally
    Ini.Free;
  end;
end;

end.

