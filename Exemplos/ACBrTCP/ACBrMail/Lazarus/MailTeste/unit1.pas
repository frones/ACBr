unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ACBrMail, types;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrMail1: TACBrMail;
    bEnviar: TButton;
    bEnviarLote: TButton;
    cbAddImgAtt: TCheckBox;
    cbAddXML: TCheckBox;
    cbUsarThread: TCheckBox;
    cbUsarTXT: TCheckBox;
    cbUsarHTML: TCheckBox;
    cbAddImgHTML: TCheckBox;
    cbAddPDF: TCheckBox;
    edSubject: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    mLog: TMemo;
    mAltBody: TMemo;
    mBody: TMemo;
    ProgressBar1: TProgressBar;
    procedure ACBrMail1AfterMailProcess(Sender: TObject);
    procedure ACBrMail1BeforeMailProcess(Sender: TObject);
    procedure ACBrMail1MailException(const AMail: TACBrMail;
      const E: Exception; var ThrowIt: Boolean);
    procedure ACBrMail1MailProcess(const AMail: TACBrMail;
      const aStatus: TMailStatus);
    procedure bEnviarClick(Sender: TObject);
    procedure bEnviarLoteClick(Sender: TObject);
  private
    procedure AjustaParametrosDeEnvio;
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

Uses mimemess;

{$R *.lfm}

{ TForm1 }

procedure TForm1.bEnviarClick(Sender: TObject);
var
  Dir, ArqXML: String;
  MS: TMemoryStream;
  P, N: Integer;
begin
  mLog.Lines.Clear;
  ProgressBar1.Position := 1;

  Dir := ExtractFilePath(ParamStr(0));

  P := pos(' - ', edSubject.Text);
  if P > 0 then
  begin
    N := StrToIntDef( copy(edSubject.Text, P+3, 5), 0) + 1;
    edSubject.Text := copy(edSubject.Text, 1, P+2) + IntToStr(N);
  end;

  ACBrMail1.Clear;
  ACBrMail1.IsHTML := cbUsarHTML.Checked;
  ACBrMail1.Subject := edSubject.Text;

  AjustaParametrosDeEnvio;

  // mensagem principal do e-mail. pode ser html ou texto puro
  if cbUsarTXT.Checked then
    ACBrMail1.AltBody.Assign( mAltBody.Lines );

  if cbUsarHTML.Checked then
    ACBrMail1.Body.Assign( mBody.Lines );

  if cbUsarHTML.Checked and cbAddImgHTML.Checked then
  begin
    // Depende de: "<img src='cid:LogoACBr'>" em ACBrMail1.Body;
    if Pos( 'cid:LogoACBr', ACBrMail1.Body.Text ) > 0 then
      ACBrMail1.AddAttachment(Dir+'acbr_logo2.png', 'LogoACBr');
  end;

  if cbAddImgAtt.Checked then
    ACBrMail1.AddAttachment(Dir+'acbr_logo.jpg');

  if cbAddPDF.Checked then
    ACBrMail1.AddAttachment(Dir+'35150905481336000137550010000111291000111298-nfe.pdf', 'DANFE');

  if cbAddXML.Checked then
  begin
    MS := TMemoryStream.Create;
    try
      ArqXML := '35150905481336000137550010000111291000111298-nfe.xml';
      MS.LoadFromFile(Dir + ArqXML);
      ACBrMail1.AddAttachment(MS, ArqXML);
    finally
      MS.Free;
    end;
  end;

  ACBrMail1.Send( cbUsarThread.Checked );
end;

procedure TForm1.ACBrMail1BeforeMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('Antes de Enviar o email: '+ TACBrMail(Sender).Subject);
end;

procedure TForm1.ACBrMail1MailException(const AMail: TACBrMail;
  const E: Exception; var ThrowIt: Boolean);
begin
  ShowMessage(E.Message);
  ThrowIt := False;
  mLog.Lines.Add('*** Erro ao Enviar o email: '+ AMail.Subject);
end;

procedure TForm1.ACBrMail1MailProcess(const AMail: TACBrMail;
  const aStatus: TMailStatus);
begin
  ProgressBar1.Position := Integer( aStatus );

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

  mLog.Lines.Add('   '+AMail.Subject);

  Application.ProcessMessages;
end;

procedure TForm1.ACBrMail1AfterMailProcess(Sender: TObject);
begin
  mLog.Lines.Add('Depois de Enviar o email: '+ TACBrMail(Sender).Subject);
end;

procedure TForm1.bEnviarLoteClick(Sender: TObject);
var
  A: Integer;
begin
  cbUsarThread.Checked := True;

  AjustaParametrosDeEnvio;

  mLog.Lines.Add('***** Iniciando envio de 5 emails por Thread *****');
  For A := 1 to 5 do
  begin
    mLog.Lines.Add('***** Enviando email: '+IntToStr(A));
    edSubject.Text := 'Teste de email: '+IntToStr(A);
    bEnviar.Click;
  end;
  mLog.Lines.Add('***** 5 emails enviados ***** ');
end;

procedure TForm1.AjustaParametrosDeEnvio;
begin
  ACBrMail1.From := 'fulano@empresa.com.br';
  ACBrMail1.FromName := 'Fula do Tal';
  ACBrMail1.Host := 'smtp.empresa.com.br'; // troque pelo seu servidor smtp
  ACBrMail1.Username := 'fulano@empresa.com.br';
  ACBrMail1.Password := 'Super_Senha_123';
  ACBrMail1.Port := '587'; // troque pela porta do seu servidor smtp
  ACBrMail1.SetTLS := True;  // Verifique se o seu servidor necessita SSL
  ACBrMail1.AddAddress('destinatario@gmail.com', 'Outro Fulano de Tal');
  ACBrMail1.AddCC('outro_email@gmail.com'); // opcional
  //ACBrMail1.AddReplyTo('um_email'); // opcional
  //ACBrMail1.AddBCC('um_email'); // opcional
  //ACBrMail1.Priority := MP_high;
  //ACBrMail1.ReadingConfirmation := True; // solicita confirmação de leitura
end;

end.




