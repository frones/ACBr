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
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    ProgressBar1: TProgressBar;
    procedure ACBrMail1MailProcess(const aStatus: TMailStatus);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure EnviarEmail;
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  ProgressBar1.Position := 1;
  ACBrMail1.IsHTML := True; // define que a mensagem é html
  // mensagem principal do e-mail. pode ser html ou texto puro
  ACBrMail1.Body.Text :=
  '<html>' +
  '<head>' +
  '<meta http-equiv="content-type" content="text/html; charset=ISO-8859-1">' +
  '</head>' +
  '<body text="#000000" bgcolor="#FFFFFF">' +
  '<h1>Texto em HTML.</h1><br>' +
  '<p>Teste de Envio ÁÉÍÓÚÇçáéíúó ÝÍÃãõÕ</p><br>' +
  '</body>' +
  '</html>';

  EnviarEmail;
end;

procedure TForm1.ACBrMail1MailProcess(const aStatus: TMailStatus);
begin
  ProgressBar1.Position := Integer( aStatus );

  case aStatus of
    pmsStartProcess:
    begin
      Memo1.Lines.Clear;
      Memo1.Lines.Add('Iniciando processo de envio.');
    end;
    pmsConfigHeaders:
      Memo1.Lines.Add('Configurando o cabeçalho do e-mail.');
    pmsLoginSMTP:
      Memo1.Lines.Add('Logando no servidor de e-mail.');
    pmsStartSends:
      Memo1.Lines.Add('Iniciando os envios.');
    pmsSendTo:
      Memo1.Lines.Add('Processando lista de destinatários.');
    pmsSendCC:
      Memo1.Lines.Add('Processando lista CC.');
    pmsSendBCC:
      Memo1.Lines.Add('Processando lista BCC.');
    pmsSendReplyTo:
      Memo1.Lines.Add('Processando lista ReplyTo.');
    pmsSendData:
      Memo1.Lines.Add('Enviando dados.');
    pmsLogoutSMTP:
      Memo1.Lines.Add('Fazendo Logout no servidor de e-mail.');
    pmsDone:
    begin
      Memo1.Lines.Add('Terminando e limpando.');
      ProgressBar1.Position := ProgressBar1.Max;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  ProgressBar1.Position := 1;
  //ACBrMail1.Priority := MP_high;
  ACBrMail1.IsHTML := False; // define que a mensagem é texto puro
  // mensagem principal do e-mail. pode ser html ou texto puro
  //ACBrMail1.ReadingConfirmation := True; // solicita confirmação de leitura
  EnviarEmail;
end;

procedure TForm1.EnviarEmail;
begin
  ACBrMail1.From := 'seu_email@gmail.com';
  ACBrMail1.FromName := 'Fula de Tal';
  ACBrMail1.Host := 'smtp.gmail.com'; // troque pelo seu servidor smtp
  ACBrMail1.Username := 'seu_login_ou_email';
  ACBrMail1.Password := 'sua_senha';
  ACBrMail1.Port := '465'; // troque pela porta do seu servidor smtp
  ACBrMail1.SetSSL := True;  // Verifique se o seu servidor necessita SSL
  ACBrMail1.AddAddress('destinatario@gmail.com', 'Nome do destinatário');
  //ACBrMail1.AddCC('um_email'); // opcional
  //ACBrMail1.AddReplyTo('um_email'); // opcional
  //ACBrMail1.AddBCC('um_email'); // opcional
  ACBrMail1.Subject := Edit1.Text; // assunto
  ACBrMail1.AltBody.Text := Memo2.Lines.Text;
  //ACBrMail1.AddAttachment('c:\temp\imagem.png', 'imagem.png');
  ACBrMail1.Send;
end;

end.
