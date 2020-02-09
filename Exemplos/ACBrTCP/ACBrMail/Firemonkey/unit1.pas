unit Unit1;

interface

//** Converted with Mida 600     http://www.midaconverter.com - PROJETO.ACBR



uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IniFiles,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Menus,
  FMX.Grid,
  FMX.ExtCtrls,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Memo,
  FMX.TabControl,
  FMX.Layouts,
  FMX.Edit,
  FMX.Platform,
  FMX.Bind.DBEngExt,
  FMX.Bind.Editors,
  FMX.Bind.DBLinks,
  FMX.Bind.Navigator,
  Data.Bind.EngExt,
  Data.Bind.Components,
  Data.Bind.DBScope,
  Data.Bind.DBLinks,
  Datasnap.DBClient,
  Fmx.Bind.Grid,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Grid,
  Fmx.StdCtrls,
  FMX.Header,
  FMX.Graphics, ACBrBase, ACBrMail, FMX.ScrollBox, FMX.Controls.Presentation;

//**   Original VCL Uses section : 


//**   Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ACBrMail, types, ACBrBase, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrMail1: TACBrMail;
    pnlTopo: TPanel;
    imgLogo: TImage;
    lblDescricao: TLabel;
    pgc: TTabControl;
    tsMensagem: TTabItem;
    tsConfigConta: TTabItem;
    grpOpcoes: TGroupBox;
    cbUsarTXT: TCheckBox;
    cbUsarHTML: TCheckBox;
    cbAddImgHTML: TCheckBox;
    cbAddImgAtt: TCheckBox;
    cbAddPDF: TCheckBox;
    cbAddXML: TCheckBox;
    cbUsarThread: TCheckBox;
    edSubject: TEdit;
    Label2: TLabel;
    edtAddressEmail: TEdit;
    Label6: TLabel;
    mLog: TMemo;
    Label5: TLabel;
    mAltBody: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    mBody: TMemo;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    bEnviar: TButton;
    bEnviarLote: TButton;
    edtHost: TEdit;
    lblHost: TLabel;
    lblFrom: TLabel;
    edtFrom: TEdit;
    edtFromName: TEdit;
    lblFromName: TLabel;
    lblUser: TLabel;
    edtUser: TEdit;
    lblPassword: TLabel;
    edtPassword: TEdit;
    chkMostraSenha: TCheckBox;
    edtPort: TEdit;
    lblPort: TLabel;
    btnSalvar: TButton;
    chkTLS: TCheckBox;
    chkSSL: TCheckBox;
    lblTipoAutenticacao: TLabel;
    lblAdressName: TLabel;
    edtAddressName: TEdit;
    lblDefaultCharset: TLabel;
    cbbDefaultCharset: TComboBox;
    cbbIdeCharSet: TComboBox;
    lbl1: TLabel;
    btLer: TButton;
    procedure ACBrMail1AfterMailProcess(Sender: TObject);
    procedure ACBrMail1BeforeMailProcess(Sender: TObject);
    procedure ACBrMail1MailException(const AMail: TACBrMail; const E: Exception; var ThrowIt: Boolean);
    procedure ACBrMail1MailProcess(const AMail: TACBrMail; const aStatus: TMailStatus);
    procedure bEnviarClick(Sender: TObject);
    procedure bEnviarLoteClick(Sender: TObject);
    procedure chkMostraSenhaClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btLerClick(Sender: TObject);
  private
    procedure AjustaParametrosDeEnvio;
    procedure LerConfiguracao;
    procedure GravarConfiguracao;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  mimemess, TypInfo;

{$R *.FMX}

{ TForm1 }
procedure TForm1.GravarConfiguracao;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt(ParamStr(0), '.ini');
  Ini := TIniFile.Create(IniFile {$IfDef POSIX}, TEncoding.ANSI{$EndIf});
  try
    Ini.WriteString('Email', 'From', edtFrom.text);
    Ini.WriteString('Email', 'FromName', edtFromName.text);
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

procedure TForm1.FormCreate(Sender: TObject);
var
  m: TMailCharset;
begin
  cbbDefaultCharset.Items.Clear;
  for m := Low(TMailCharset) to High(TMailCharset) do
    cbbDefaultCharset.Items.Add(GetEnumName(TypeInfo(TMailCharset), integer(m)));
  cbbDefaultCharset.ItemIndex := 0;
  cbbIdeCharSet.Items.Assign(cbbDefaultCharset.Items);
  cbbIdeCharSet.ItemIndex := 0;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TForm1.LerConfiguracao;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt(ParamStr(0), '.ini');
  Ini := TIniFile.Create(IniFile{$IfDef POSIX}, TEncoding.ANSI{$EndIf});
  try
    edtFrom.text := Ini.readString('Email', 'From', 'fulano@empresa.com.br');
    edtFromName.text := Ini.readString('Email', 'FromName', 'Fulano de Tal');
    edtHost.text := Ini.readString('Email', 'Host', 'smtp.empresa.com.br');
    edtPort.text := Ini.readString('Email', 'Port', '587');
    edtUser.text := Ini.readString('Email', 'User', 'fulano@empresa.com.br');
    edtPassword.text := Ini.readString('Email', 'Pass', 'Sua_Senha_123');
    chkTLS.IsChecked  := Ini.ReadBool('Email', 'TLS', False);
    chkSSL.IsChecked  := Ini.ReadBool('Email', 'SSL', False);
    cbbDefaultCharset.ItemIndex := Ini.ReadInteger('Email', 'DefaultCharset', 27);
    cbbIdeCharSet.ItemIndex := Ini.ReadInteger('Email', 'IdeCharset', 15);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.bEnviarClick(Sender: TObject);
var
  Dir, ArqXML: string;
  MS: TMemoryStream;
  P, N: Integer;
begin
  mLog.Lines.Clear;
  ProgressBar1.Value := 1;

  Dir := ExtractFilePath(ParamStr(0));

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

procedure TForm1.ACBrMail1BeforeMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('Antes de Enviar o email: ' + TACBrMail(Sender).Subject);
end;

procedure TForm1.ACBrMail1MailException(const AMail: TACBrMail; const E: Exception; var ThrowIt: Boolean);
begin
  ShowMessage(E.Message);
  ThrowIt := False;
  mLog.Lines.Add('*** Erro ao Enviar o email: ' + AMail.Subject);
end;

procedure TForm1.ACBrMail1MailProcess(const AMail: TACBrMail; const aStatus: TMailStatus);
begin
  ProgressBar1.Value := Integer(aStatus);

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

  Application.ProcessMessages;
end;

procedure TForm1.ACBrMail1AfterMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('Depois de Enviar o email: ' + TACBrMail(Sender).Subject);
end;

procedure TForm1.bEnviarLoteClick(Sender: TObject);
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

procedure TForm1.btLerClick(Sender: TObject);
begin
  LerConfiguracao;
end;

procedure TForm1.btnSalvarClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TForm1.chkMostraSenhaClick(Sender: TObject);
begin
  edtPassword.Password := chkMostraSenha.IsChecked;
end;

procedure TForm1.AjustaParametrosDeEnvio;
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




