unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, blcksock;

type

  { TForm1 }

  TForm1 = class(TForm)
    bEnviar: TButton;
    bConectar: TButton;
    edIP: TEdit;
    Label1: TLabel;
    mEnviar: TMemo;
    mResposta: TMemo;
    Panel1: TPanel;
    procedure bConectarClick(Sender: TObject);
    procedure bEnviarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    fSocket : TBlockSocket;

    procedure AguardaReposta;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  fSocket := TBlockSocket.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  fSocket.Free;
end;

procedure TForm1.bConectarClick(Sender: TObject);
begin
  if bConectar.Caption = 'Conectar' then
  begin
    fSocket.Connect(edIP.Text, '3434');
    AguardaReposta;
    bConectar.Caption := 'Desconectar';
  end
  else
  begin
    fSocket.CloseSocket;
    bConectar.Caption := 'Conectar';
    AguardaReposta;
  end;
end;

procedure TForm1.bEnviarClick(Sender: TObject);
begin
  fSocket.SendString(mEnviar.Lines.Text + #13+#10+'.'+#13+#10);
  AguardaReposta;
end;

procedure TForm1.AguardaReposta;
var
  Resposta: String;
begin
  Resposta := fSocket.RecvTerminated(5000, #3);
  mResposta.Lines.Add('===================');
  mResposta.Lines.Add(Resposta);
end;


end.

