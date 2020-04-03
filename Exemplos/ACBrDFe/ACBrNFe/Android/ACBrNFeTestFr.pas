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
  ACBrNFeDANFeESCPOS, ACBrPosPrinter, ACBrDFe, ACBrNFe;

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
    Label26: TLabel;
    edtTokenID: TEdit;
    Label27: TLabel;
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
    Label32: TLabel;
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
  private
    { Private declarations }
    FVKService: IFMXVirtualKeyboardService;

    function CalcularNomeArqINI: String;
    procedure LerINI;
    procedure GravarINI;
    procedure ConfigurarACBr;
    function PedirPermissoes: Boolean;
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
  {$EndIf}
  FMX.DialogService, FMX.Platform,
  ssl_openssl_lib, blcksock,
  ACBrUtil, ACBrConsts,
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
  LerINI;
end;

function TACBrNFCeTestForm.PedirPermissoes: Boolean;
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
  begin
    TDialogService.MessageDialog( 'Sem permissões para acesso a Internet',
                                  TMsgDlgType.mtError, [TMsgDlgBtn.mbOK],
                                  TMsgDlgBtn.mbOk, 0, nil, nil);
  end;
  {$EndIf}

  Result := Ok;
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
  LerINI;
end;

procedure TACBrNFCeTestForm.btnBackClick(Sender: TObject);
begin
  tabsPrincipal.Previous;
end;

procedure TACBrNFCeTestForm.btnLimparClick(Sender: TObject);
begin
  mLog.Lines.Clear;
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
  GravarINI;
end;

function TACBrNFCeTestForm.CalcularNomeArqINI: String;
begin
  Result := ApplicationPath + 'ACBrNFeTeste.ini';
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

procedure TACBrNFCeTestForm.GravarINI;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := CalcularNomeArqINI;
  Ini := TIniFile.Create(IniFile {$IfDef POSIX}, TEncoding.ANSI{$EndIf});
  try
    // configurações do ACBrPosPrinter //
    INI.WriteInteger('PosPrinter','Modelo', cbxModelo.ItemIndex);
    INI.WriteInteger('PosPrinter','PaginaDeCodigo',cbxPagCodigo.ItemIndex);
    if Assigned(cbxImpressorasBth.Selected) then
      INI.WriteString('PosPrinter','Porta', cbxImpressorasBth.Selected.Text);
    INI.WriteInteger('PosPrinter','Colunas', Trunc(seColunas.Value) );
    INI.WriteInteger('PosPrinter','EspacoEntreLinhas', Trunc(seEspLinhas.Value) );
    INI.WriteInteger('PosPrinter','LinhasPular', Trunc(seLinhasPular.Value) );
    INI.WriteInteger('PosPrinter.Logo','KC1',Trunc(seKC1.Value));
    INI.WriteInteger('PosPrinter.Logo','KC2',Trunc(seKC2.Value));

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

    // Configurações de ACBrNFeDANFeESCPOS //
    Ini.WriteBool('DANFCE', 'QrCodeLateral', cbQRCodeLateral.IsChecked);
    Ini.WriteBool('DANFCE', 'ItemUmaLinha', cbImprimir1Linha.IsChecked);
    Ini.WriteBool('DANFCE', 'ItemDescAcres', cbImprimirDescAcres.IsChecked);
    Ini.WriteBool('DANFCE', 'Logo', cbImprimirLogo.IsChecked);
    Ini.WriteBool('DANFCE', 'LogoLateral', cbLogoLateral.IsChecked);

    // Configurações de ACBrNFe //
    Ini.WriteString( 'Certificado', 'URL', edtConfCertURL.Text);
    Ini.WriteString( 'Certificado', 'Caminho', edtConfCertPFX.Text);
    Ini.WriteString( 'Certificado', 'Senha', edtConfCertSenha.Text);
    Ini.WriteString('Proxy', 'Host', edtProxyHost.Text);
    Ini.WriteInteger('Proxy', 'Porta', Trunc(sbProxyPort.Value));
    Ini.WriteString('Proxy', 'User', edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass', edtProxyPass.Text);
    Ini.WriteString( 'Token', 'IdCSCn', edtTokenID.Text);
    Ini.WriteString( 'Token', 'CSC', edtTokenCSC.Text);
    if cbxWebServiceUF.ItemIndex >= 0  then
      Ini.WriteString('WebService', 'UF', cbxWebServiceUF.Selected.Text);
    Ini.WriteInteger('WebService', 'Ambiente', ifthen( swWebServiceAmbiente.IsChecked, 0, 1) );  // segue TpcnTipoAmbiente
    Ini.WriteInteger('WebService', 'TimeOut', Trunc(sbWebServiceTimeout.Value));
    Ini.WriteInteger('WebService', 'SSLType',    cbxWebServiceSSLType.ItemIndex);

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
    Ini.WriteString('Emitente', 'cMun', lEmitcMun.Text);
    Ini.WriteString('Emitente', 'cUF', lEmitcUF.Text);
  finally
    Ini.Free;
  end;
end;

procedure TACBrNFCeTestForm.LerINI;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := CalcularNomeArqINI;
  Ini := TIniFile.Create(IniFile{$IfDef POSIX}, TEncoding.ANSI{$EndIf});
  try
    edtEmailFrom.text := Ini.ReadString('Email', 'From', 'fulano@empresa.com.br');
    edtEmailFromName.text := Ini.ReadString('Email', 'FromName', 'Fulano de Tal');
    edtEmailHost.text := Ini.ReadString('Email', 'Host', 'smtp.empresa.com.br');
    sbEmailPort.Value := Ini.ReadInteger('Email', 'Port', 587);
    edtEmailUser.text := Ini.ReadString('Email', 'User', 'fulano@empresa.com.br');
    edtEmailPassword.text := Ini.ReadString('Email', 'Pass', 'Sua_Senha_123');
    chkEmailTLS.IsChecked  := Ini.ReadBool('Email', 'TLS', False);
    chkEmailSSL.IsChecked  := Ini.ReadBool('Email', 'SSL', False);
    cbEmailDefaultCharset.ItemIndex := Ini.ReadInteger('Email', 'DefaultCharset', 27);
    cbEmailIdeCharSet.ItemIndex := Ini.ReadInteger('Email', 'IdeCharset', {$IfDef MSWINDOWS}15{$Else}27{$EndIf});
  finally
    Ini.Free;
  end;
end;

end.


procedure TACBrNFCeTestForm.AjustaParametrosDeEnvio;
begin
  ACBrMail1.From := edtFrom.text;
  ACBrMail1.FromName := edtFromName.text;
  ACBrMail1.Host := edtHost.text; // troque pelo seu servidor smtp
  ACBrMail1.Username := edtUser.text;
  ACBrMail1.Password := edtPassword.text;
  ACBrMail1.Port := edtPort.text; // troque pela porta do seu servidor smtp
  ACBrMail1.SetTLS := chkTLS.IsChecked;
  ACBrMail1.SetSSL := chkSSL.IsChecked;  // Verifique se o seu servidor necessita SSL
  ACBrMail1.DefaultCharset := TMailCharset(cbbDefaultCharset.ItemIndex);
  ACBrMail1.IDECharset := TMailCharset(cbbIdeCharSet.ItemIndex);
  ACBrMail1.AddAddress(edtAddressEmail.text, edtAddressName.text);
  //ACBrMail1.AddCC('outro_email@gmail.com'); // opcional
  //ACBrMail1.AddReplyTo('um_email'); // opcional
  //ACBrMail1.AddBCC('um_email'); // opcional
  //ACBrMail1.Priority := MP_high;
  //ACBrMail1.ReadingConfirmation := True; // solicita confirmação de leitura
end;

  ACBrMail1.AddAddress(edtAddressEmail.text, edtAddressName.text);

procedure TACBrNFCeTestForm.bEnviarClick(Sender: TObject);
var
  Dir, ArqXML: string;
  MS: TMemoryStream;
  P, N: Integer;
begin
  if not PedirPermissoes then
    Exit;

  mLog.Lines.Clear;
  ProgressBar1.Value := 1;

  Dir := ApplicationPath;

  P := pos(' - ', edSubject.Text);
  if P > 0 then
  begin
    N := StrToIntDef(copy(edSubject.Text, P + 3, 5), 0) + 1;
    edSubject.Text := copy(edSubject.Text, 1, P + 2) + IntToStr(N);
  end;

  ACBrMail1.Clear;
  ACBrMail1.IsHTML := cbUsarHTML.IsChecked;
  ACBrMail1.Subject := edSubject.Text;

  AjustaParametrosDeEnvio;

  // mensagem principal do e-mail. pode ser html ou texto puro
  if cbUsarTXT.IsChecked  then
    ACBrMail1.AltBody.Assign(mAltBody.Lines);

  if cbUsarHTML.IsChecked  then
    ACBrMail1.Body.Assign(mBody.Lines);

  if cbUsarHTML.IsChecked  and cbAddImgHTML.IsChecked  then
  begin
    // Depende de: "<img src='cid:LogoACBr'>" em ACBrMail1.Body;
    if Pos('cid:LogoACBr', ACBrMail1.Body.Text) > 0 then
      ACBrMail1.AddAttachment(Dir + 'acbr_logo2.png', 'LogoACBr');
  end;

  if cbAddImgAtt.IsChecked  then
    ACBrMail1.AddAttachment(Dir + 'acbr_logo.jpg');

  if cbAddPDF.IsChecked  then
    ACBrMail1.AddAttachment(Dir + '35150905481336000137550010000111291000111298-nfe.pdf', 'DANFE');

  if cbAddXML.IsChecked  then
  begin
    MS := TMemoryStream.Create;
    try
      ArqXML := '35150905481336000137550010000111291000111298-nfe.xml';
      MS.LoadFromFile(Dir + ArqXML);
      ACBrMail1.AddAttachment(MS, ArqXML, adAttachment);
    finally
      MS.Free;
    end;
  end;

  ACBrMail1.Send(cbUsarThread.IsChecked);
end;


    Ini.WriteString('Email', 'AddressEmail', edtAddressEmail.text);
    Ini.WriteString('Email', 'AddressName', edtEmailAddressName.text);
    edtAddressEmail.text := Ini.ReadString('Email', 'AddressEmail', 'fulano@empresa.com.br');
    edtAddressName.text := Ini.ReadString('Email', 'AddressName', 'Fulano de Tal');

