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

unit Unit1;

interface

uses
  Classes, 
  SysUtils, 
  Forms, 
  Controls, 
  Graphics, 
  Dialogs, 
  StdCtrls, 
  ComCtrls, 
  ACBrMail, 
  types, 
  ACBrBase, 
  ExtCtrls,
  blcksock;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrMail1: TACBrMail;
    pnlTopo: TPanel;
    imgLogo: TImage;
    lblDescricao: TLabel;
    pgc: TPageControl;
    tsMensagem: TTabSheet;
    tsConfigConta: TTabSheet;
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
    btLerConfig: TButton;
    Label7: TLabel;
    cbxSSLTYPE: TComboBox;
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
    procedure btLerConfigClick(Sender: TObject);
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
  mimemess, IniFiles, TypInfo;

{$R *.dfm}

{ TForm1 }
procedure TForm1.GravarConfiguracao;
var
  IniFile: string;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');
  Ini := TIniFile.Create(IniFile);
  try
    Ini.WriteString('Email', 'From', edtFrom.text);
    Ini.WriteString('Email', 'FromName', edtFromName.text);
    Ini.WriteString('Email', 'Host', edtHost.text);
    Ini.WriteString('Email', 'Port', edtPort.text);
    Ini.WriteString('Email', 'User', edtUser.text);
    Ini.WriteString('Email', 'Pass', edtPassword.text);
    Ini.WriteBool('Email', 'TLS', chkTLS.Checked);
    Ini.WriteBool('Email', 'SSL', chkSSL.Checked);
    Ini.WriteInteger('Email', 'SSLType', cbxSSLTYPE.ItemIndex);
    Ini.WriteInteger('Email', 'DefaultCharset', cbbDefaultCharset.ItemIndex);
    Ini.WriteInteger('Email', 'IdeCharset', cbbIdeCharSet.ItemIndex);
  finally
    Ini.Free;
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  m: TMailCharset;
  LTLS : TSSLType;
begin
  cbbDefaultCharset.Items.Clear;
  for m := Low(TMailCharset) to High(TMailCharset) do
    cbbDefaultCharset.Items.Add(GetEnumName(TypeInfo(TMailCharset), integer(m)));
  cbbDefaultCharset.ItemIndex := 0;
  cbbIdeCharSet.Items.Assign(cbbDefaultCharset.Items);
  cbbIdeCharSet.ItemIndex := 0;

  for LTLS := Low(TSSLType) to High(TSSLType) do
    cbxSSLTYPE.Items.Add(GetEnumName(TypeInfo(TSSLType), integer(LTLS)));

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
  IniFile := ChangeFileExt(Application.ExeName, '.ini');
  Ini := TIniFile.Create(IniFile);
  try
    edtFrom.text := Ini.readString('Email', 'From', 'fulano@empresa.com.br');
    edtFromName.text := Ini.readString('Email', 'FromName', 'Fulano de Tal');
    edtHost.text := Ini.readString('Email', 'Host', 'smtp.empresa.com.br');
    edtPort.text := Ini.readString('Email', 'Port', '587');
    edtUser.text := Ini.readString('Email', 'User', 'fulano@empresa.com.br');
    edtPassword.text := Ini.readString('Email', 'Pass', 'Sua_Senha_123');
    chkTLS.Checked := Ini.ReadBool('Email', 'TLS', False);
    chkSSL.Checked := Ini.ReadBool('Email', 'SSL', False);
    cbxSSLTYPE.ItemIndex := Ini.ReadInteger('Email', 'SSLType', 0);
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
  ProgressBar1.Position := 1;

  Dir := ExtractFilePath(ParamStr(0));

  P := pos(' - ', edSubject.Text);
  if P > 0 then
  begin
    N := StrToIntDef(copy(edSubject.Text, P + 3, 5), 0) + 1;
    edSubject.Text := copy(edSubject.Text, 1, P + 2) + IntToStr(N);
  end;

  ACBrMail1.Clear;
  ACBrMail1.IsHTML := cbUsarHTML.Checked;
  ACBrMail1.Subject := edSubject.Text;

  AjustaParametrosDeEnvio;

  // mensagem principal do e-mail. pode ser html ou texto puro
  if cbUsarTXT.Checked then
    ACBrMail1.AltBody.Assign(mAltBody.Lines);

  if cbUsarHTML.Checked then
    ACBrMail1.Body.Assign(mBody.Lines);

  if cbUsarHTML.Checked and cbAddImgHTML.Checked then
  begin
    // Depende de: "<img src='cid:LogoACBr'>" em ACBrMail1.Body;
    if Pos('cid:LogoACBr', ACBrMail1.Body.Text) > 0 then
      ACBrMail1.AddAttachment(Dir + 'acbr_logo2.png', 'LogoACBr', adInline);
  end;

  if cbAddImgAtt.Checked then
    ACBrMail1.AddAttachment(Dir + 'acbr_logo.jpg', '', adAttachment);

  if cbAddPDF.Checked then
    ACBrMail1.AddAttachment(Dir + '35150905481336000137550010000111291000111298-nfe.pdf', 'DANFE', adAttachment);

  if cbAddXML.Checked then
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


  try
    ACBrMail1.Send(cbUsarThread.Checked);

  except on E: Exception do
    ShowMessage(E.Message);
  end;
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
  ProgressBar1.Position := Integer(aStatus);

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
        ProgressBar1.Position := ProgressBar1.Max;
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
  cbUsarThread.Checked := True;

  AjustaParametrosDeEnvio;

  mLog.Lines.Add('***** Iniciando envio de 5 emails por Thread *****');
  for A := 1 to 5 do
  begin
    mLog.Lines.Add('***** Enviando email: ' + IntToStr(A));
    edSubject.Text := 'Teste de email: ' + IntToStr(A);
    bEnviar.Click;
  end;
  mLog.Lines.Add('***** 5 emails enviados ***** ');
end;

procedure TForm1.btnSalvarClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TForm1.chkMostraSenhaClick(Sender: TObject);
begin
  if chkMostraSenha.Checked then
    edtPassword.PasswordChar := #0
  else
    edtPassword.PasswordChar := '@';
end;

procedure TForm1.AjustaParametrosDeEnvio;
begin
  ACBrMail1.From := edtFrom.text;
  ACBrMail1.FromName := edtFromName.text;
  ACBrMail1.Host := edtHost.text; // troque pelo seu servidor smtp
  ACBrMail1.Username := edtUser.text;
  ACBrMail1.Password := edtPassword.text;
  ACBrMail1.Port := edtPort.text; // troque pela porta do seu servidor smtp
  ACBrMail1.SetTLS := chkTLS.Checked;
  ACBrMail1.SetSSL := chkSSL.Checked;  // Verifique se o seu servidor necessita SSL
  ACBrMail1.DefaultCharset := TMailCharset(cbbDefaultCharset.ItemIndex);
  ACBrMail1.IDECharset := TMailCharset(cbbIdeCharSet.ItemIndex);
  ACBrMail1.AddAddress(edtAddressEmail.text, edtAddressName.text);
  ACBrMail1.SSLType := TSSLType(cbxSSLTYPE.ItemIndex);
  //ACBrMail1.AddCC('outro_email@gmail.com'); // opcional
  //ACBrMail1.AddReplyTo('um_email'); // opcional
  //ACBrMail1.AddBCC('um_email'); // opcional
  //ACBrMail1.Priority := MP_high;
  //ACBrMail1.ReadingConfirmation := True; // solicita confirmação de leitura
end;

procedure TForm1.btLerConfigClick(Sender: TObject);
begin
  LerConfiguracao;
end;

end.




