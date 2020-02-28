unit ACBrMailTestFr;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Gestures, System.Actions, FMX.ActnList,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.ListBox, FMX.Layouts, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.ScrollBox, FMX.Memo, System.ImageList, FMX.ImgList, FMX.VirtualKeyboard,
  ACBrMail, ACBrBase;

type
  TACBrMailTestForm = class(TForm)
    GestureManager1: TGestureManager;
    tabsPrincipal: TTabControl;
    tabConfig: TTabItem;
    ToolBar1: TToolBar;
    lblTituloConfig: TLabel;
    tabTeste: TTabItem;
    ToolBar2: TToolBar;
    lblTituloTestes: TLabel;
    btnBack: TSpeedButton;
    lbConfig: TListBox;
    lbiFrom: TListBoxItem;
    lbiSMTP: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    lbiUsuarioLogin: TListBoxItem;
    lbiBotoesParametros: TListBoxItem;
    GridPanelLayout2: TGridPanelLayout;
    btLerConfig: TCornerButton;
    btSalvarConfig: TCornerButton;
    ListBoxGroupHeader4: TListBoxGroupHeader;
    lbiCharSet: TListBoxItem;
    ListBoxGroupHeader6: TListBoxGroupHeader;
    ImageList1: TImageList;
    StyleBook1: TStyleBook;
    ACBrMail1: TACBrMail;
    GridPanelLayout6: TGridPanelLayout;
    Label5: TLabel;
    edtFrom: TEdit;
    Label6: TLabel;
    edtFromName: TEdit;
    GridPanelLayout7: TGridPanelLayout;
    Label8: TLabel;
    edtHost: TEdit;
    Label9: TLabel;
    chkTLS: TSwitch;
    Label10: TLabel;
    edtPort: TSpinBox;
    Label11: TLabel;
    chkSSL: TSwitch;
    GridPanelLayout1: TGridPanelLayout;
    Label2: TLabel;
    Label3: TLabel;
    edtUser: TEdit;
    edtPassword: TEdit;
    chkMostraSenha: TSpeedButton;
    GridPanelLayout5: TGridPanelLayout;
    Label1: TLabel;
    Label4: TLabel;
    cbbDefaultCharset: TComboBox;
    cbbIdeCharSet: TComboBox;
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
    procedure bEnviarClick(Sender: TObject);
    procedure chkMostraSenhaClick(Sender: TObject);
    procedure btnLimparClick(Sender: TObject);
    procedure btnVersaoOpenSSLClick(Sender: TObject);
    procedure ACBrMail1AfterMailProcess(Sender: TObject);
    procedure bEnviarLoteClick(Sender: TObject);
  private
    { Private declarations }
    FVKService: IFMXVirtualKeyboardService;

    function CalcularNomeArqINI: String;
    procedure LerINI;
    procedure GravarINI;
    function PedirPermissoes: Boolean;
    procedure AjustaParametrosDeEnvio;
  public
    { Public declarations }
  end;

var
  ACBrMailTestForm: TACBrMailTestForm;

implementation

uses
  System.typinfo, System.IniFiles, System.StrUtils, System.Permissions,
  {$IfDef ANDROID}
  Androidapi.Helpers, Androidapi.JNI.Os, Androidapi.JNI.JavaTypes,
  {$EndIf}
  FMX.DialogService, FMX.Platform,
  ssl_openssl_lib,
  ACBrUtil, ACBrConsts;

{$R *.fmx}

procedure TACBrMailTestForm.FormCreate(Sender: TObject);
var
  m: TMailCharset;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService, IInterface(FVKService));

  cbbDefaultCharset.Items.Clear;
  for m := Low(TMailCharset) to High(TMailCharset) do
    cbbDefaultCharset.Items.Add(GetEnumName(TypeInfo(TMailCharset), integer(m)));

  cbbDefaultCharset.ItemIndex := 0;
  cbbIdeCharSet.Items.Assign(cbbDefaultCharset.Items);
  cbbIdeCharSet.ItemIndex := 0;

  tabsPrincipal.First;
  ProgressBar1.Visible := False;
  LerINI;
end;

function TACBrMailTestForm.PedirPermissoes: Boolean;
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

procedure TACBrMailTestForm.swHTMLSwitch(Sender: TObject);
begin
  if swHTML.IsChecked then
    tabsMensagen.ActiveTab := tabHTML
  else
    tabsMensagen.ActiveTab := tabTexto;
end;

procedure TACBrMailTestForm.ACBrMail1AfterMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('Depois de Enviar o email: ' + TACBrMail(Sender).Subject);
  ProgressBar1.Visible := False;
end;

procedure TACBrMailTestForm.ACBrMail1BeforeMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('Antes de Enviar o email: ' + TACBrMail(Sender).Subject);
  ProgressBar1.Visible := True;
end;

procedure TACBrMailTestForm.ACBrMail1MailException(const AMail: TACBrMail;
  const E: Exception; var ThrowIt: Boolean);
begin
  ShowMessage(E.Message);
  ThrowIt := False;
  mLog.Lines.Add('*** Erro ao Enviar o email: ' + AMail.Subject);
  ProgressBar1.Visible := False;
end;

procedure TACBrMailTestForm.ACBrMail1MailProcess(const AMail: TACBrMail;
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

procedure TACBrMailTestForm.bEnviarClick(Sender: TObject);
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

procedure TACBrMailTestForm.bEnviarLoteClick(Sender: TObject);
var
  A: Integer;
begin
  cbUsarThread.IsChecked  := True;

  AjustaParametrosDeEnvio;

  mLog.Lines.Add('***** Iniciando envio de 5 emails por Thread *****');
  for A := 1 to 5 do
  begin
    mLog.Lines.Add('***** Enviando email: ' + IntToStr(A));
    edSubject.Text := 'Teste de email: ' + IntToStr(A);
    bEnviarClick(Sender);
  end;
  mLog.Lines.Add('***** 5 emails enviados ***** ');
end;

procedure TACBrMailTestForm.btLerConfigClick(Sender: TObject);
begin
  LerINI;
end;

procedure TACBrMailTestForm.btnBackClick(Sender: TObject);
begin
  tabsPrincipal.Previous;
end;

procedure TACBrMailTestForm.btnLimparClick(Sender: TObject);
begin
  mLog.Lines.Clear;
end;

procedure TACBrMailTestForm.btnVersaoOpenSSLClick(Sender: TObject);
begin
  mLog.Lines.Add('Versão do OpenSSL:');
  mLog.Lines.Add(ssl_openssl_lib.OpenSSLVersion(0));
end;

procedure TACBrMailTestForm.btSalvarConfigClick(Sender: TObject);
begin
  GravarINI;
end;

function TACBrMailTestForm.CalcularNomeArqINI: String;
begin
  Result := ApplicationPath + 'ACBrMailTeste.ini';
end;

procedure TACBrMailTestForm.chkMostraSenhaClick(Sender: TObject);
begin
  edtPassword.Password := not chkMostraSenha.IsPressed;
end;

procedure TACBrMailTestForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
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

procedure TACBrMailTestForm.GestureDone(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
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

procedure TACBrMailTestForm.GravarINI;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := CalcularNomeArqINI;
  Ini := TIniFile.Create(IniFile {$IfDef POSIX}, TEncoding.ANSI{$EndIf});
  try
    Ini.WriteString('Email', 'From', edtFrom.text);
    Ini.WriteString('Email', 'FromName', edtFromName.text);
    Ini.WriteString('Email', 'AddressEmail', edtAddressEmail.text);
    Ini.WriteString('Email', 'AddressName', edtAddressName.text);
    Ini.WriteString('Email', 'Host', edtHost.text);
    Ini.WriteString('Email', 'Port', edtPort.text);
    Ini.WriteString('Email', 'User', edtUser.text);
    Ini.WriteString('Email', 'Pass', edtPassword.text);
    Ini.WriteBool('Email', 'TLS', chkTLS.IsChecked);
    Ini.WriteBool('Email', 'SSL', chkSSL.IsChecked);
    Ini.WriteInteger('Email', 'DefaultCharset', cbbDefaultCharset.ItemIndex);
    Ini.WriteInteger('Email', 'IdeCharset', cbbIdeCharSet.ItemIndex);
  finally
    Ini.Free;
  end;
end;

procedure TACBrMailTestForm.LerINI;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := CalcularNomeArqINI;
  Ini := TIniFile.Create(IniFile{$IfDef POSIX}, TEncoding.ANSI{$EndIf});
  try
    edtFrom.text := Ini.ReadString('Email', 'From', 'fulano@empresa.com.br');
    edtFromName.text := Ini.ReadString('Email', 'FromName', 'Fulano de Tal');
    edtAddressEmail.text := Ini.ReadString('Email', 'AddressEmail', 'fulano@empresa.com.br');
    edtAddressName.text := Ini.ReadString('Email', 'AddressName', 'Fulano de Tal');
    edtHost.text := Ini.ReadString('Email', 'Host', 'smtp.empresa.com.br');
    edtPort.text := Ini.ReadString('Email', 'Port', '587');
    edtUser.text := Ini.ReadString('Email', 'User', 'fulano@empresa.com.br');
    edtPassword.text := Ini.ReadString('Email', 'Pass', 'Sua_Senha_123');
    chkTLS.IsChecked  := Ini.ReadBool('Email', 'TLS', False);
    chkSSL.IsChecked  := Ini.ReadBool('Email', 'SSL', False);
    cbbDefaultCharset.ItemIndex := Ini.ReadInteger('Email', 'DefaultCharset', 27);
    cbbIdeCharSet.ItemIndex := Ini.ReadInteger('Email', 'IdeCharset', {$IfDef MSWINDOWS}15{$Else}27{$EndIf});
  finally
    Ini.Free;
  end;
end;

procedure TACBrMailTestForm.AjustaParametrosDeEnvio;
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

end.

