unit uContasCad;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ExtCtrls, Buttons, StdCtrls, Spin, sysutils;

type

  { TfrmContasCad }

  TfrmContasCad = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    cbSsl: TCheckBox;
    cbTls: TCheckBox;
    Dados: TGroupBox;
    edNome: TEdit;
    edEmail: TEdit;
    edUsuario: TEdit;
    edSenha: TEdit;
    edHost: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    edPorta: TSpinEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmContasCad: TfrmContasCad;

implementation

uses uDados, uTestes, variants, LCLType, db;

{$R *.lfm}

{ TfrmContasCad }

procedure TfrmContasCad.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 27) then
  begin
    key := 0;
    ModalResult := mrCancel;
  end;
end;

procedure TfrmContasCad.BitBtn1Click(Sender: TObject);
var
  vErro: string;
  vCampo: Integer;
begin
  vErro := '';
  vCampo := 1;
  if Trim(edEmail.Text) = '' then
    vErro := 'O campo e-mail é obrigatório!'
  else
  if not(dm.ValidaEmail(edEmail.Text)) then
    vErro := 'O e-mail digitado não está em um formato válido!'
  else
  if Trim(edHost.Text) = '' then
  begin
    vErro := 'O campo Host é obrigatório!';
    vCampo := 2;
  end
  else
  if Trim(edPorta.Text) = '' then
  begin
    vErro := 'O campo Porta é obrigatório!';
    vCampo := 3;
  end;
  if vErro <> '' then
  begin
    Application.MessageBox(PChar(vErro),'Atenção',MB_ICONWARNING);
    case vCampo of
      1: edEmail.SetFocus;
      2: edHost.SetFocus;
      3: edPorta.SetFocus;
    end;
    ModalResult := mrNone;
  end;
end;

procedure TfrmContasCad.BitBtn3Click(Sender: TObject);
begin
  Form2 := TForm2.Create(nil);
  try
    with Form2.ACBrMailTeste do
    begin
      Attempts := 1;
      FromName := edNome.Text;
      From := edEmail.Text;
      Username := edUsuario.Text;
      Password := edSenha.Text;
      Host := edHost.Text;
      Port := edPorta.Text;
      SetSSL := cbSsl.Checked;
      SetTLS := cbTls.Checked;
      AddAddress(edEmail.Text);
      Subject := 'Teste de Configurações de SMTP - ACBrMail';
      Body.Add('Se você consegue ler esta mensagem, significa que suas configurações');
      Body.Add('de SMTP estão corretas.');
      Body.Add('');
      Body.Add('Enviador de E-mails ACBrMail - v1.0');
      if Form2.ShowModal = mrOK then
        Application.MessageBox(PChar('Uma mensagem de teste foi enviada para o e-mail: '+
          edEmail.Text),'Informação',MB_ICONASTERISK)
      else
      begin
        Application.MessageBox(PChar('O teste falhou!' + LineEnding + LineEnding +
          Form2.MensagemErro),'Atenção',MB_ICONWARNING);
        Application.ProcessMessages;
      end;
    end;
  finally
    Screen.Cursor := crDefault;
    Form2.Release;
    FreeAndNil(Form2);
  end;
end;

procedure TfrmContasCad.FormShow(Sender: TObject);
begin
  edNome.SetFocus;
end;

end.

