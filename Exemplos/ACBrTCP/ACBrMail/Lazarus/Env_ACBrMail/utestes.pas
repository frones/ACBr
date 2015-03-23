unit uTestes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ACBrMail;

type

  { TForm2 }

  TForm2 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    procedure ACBrMailTesteMailProcess(const aStatus: TMailStatus);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    ACBrMailTeste: TACBrMail;
    MensagemErro: string;
    { public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  ACBrMailTeste := TACBrMail.Create(nil);
  ACBrMailTeste.OnMailProcess := @ACBrMailTesteMailProcess;
  MensagemErro := '';
end;

procedure TForm2.ACBrMailTesteMailProcess(const aStatus: TMailStatus);
begin
  case aStatus of
    pmsStartProcess:
      Label2.Caption := 'Teste: Iniciando processo de envio.';
    pmsConfigHeaders:
      Label2.Caption := 'Teste: Configurando o cabeçalho do e-mail.';
    pmsLoginSMTP:
      Label2.Caption := 'Teste: Logando no servidor de e-mail.';
    pmsStartSends:
      Label2.Caption := 'Teste: Iniciando os envios.';
    pmsSendTo:
      Label2.Caption := 'Teste: Processando lista de destinatários.';
    pmsSendData:
      Label2.Caption := 'Teste: Enviando dados.';
    pmsLogoutSMTP:
      Label2.Caption := 'Teste: Fazendo Logout no servidor de e-mail.';
  end;
  Application.ProcessMessages;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  try
    try
      Screen.Cursor := crHourGlass;
      Application.ProcessMessages;
      ACBrMailTeste.Send;
    finally
      ACBrMailTeste.Free;
    end;
    ModalResult := mrOK;
  except
    on e: Exception do
    begin
      MensagemErro := e.Message;
      ModalResult := mrCancel;
    end;
  end;
end;

end.

