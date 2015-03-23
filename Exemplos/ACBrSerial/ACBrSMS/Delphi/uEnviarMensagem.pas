unit uEnviarMensagem;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmEnviarMensagem = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    lblContador: TLabel;
    edtTelefone: TEdit;
    btnEnviar: TButton;
    memMensagem: TMemo;
    btnCancelar: TButton;
    ckbQuebrarMensagem: TCheckBox;
    rdgBandeja: TRadioGroup;
    procedure btnCancelarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnEnviarClick(Sender: TObject);
    procedure memMensagemChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmEnviarMensagem: TfrmEnviarMensagem;

implementation

uses
  uPrincipal, ACBrSMSClass;

{$R *.dfm}

procedure TfrmEnviarMensagem.FormCreate(Sender: TObject);
begin
  edtTelefone.Clear;
  memMensagem.Clear;

  rdgBandeja.Enabled := frmPrincipal.ACBrSMS1.BandejasSimCard > 1;
  if rdgBandeja.Enabled then
    rdgBandeja.ItemIndex := Integer(frmPrincipal.ACBrSMS1.SimCard);
end;

procedure TfrmEnviarMensagem.memMensagemChange(Sender: TObject);
begin
  lblContador.Caption := Format('%d caracter(es)', [Length(memMensagem.Text)]);
end;

procedure TfrmEnviarMensagem.btnCancelarClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmEnviarMensagem.btnEnviarClick(Sender: TObject);
var
  IndiceMsgEnviada: String;
begin
  if Trim(edtTelefone.Text) = EmptyStr then
    raise Exception.Create('Informe o número do telefone.');

  if Trim(memMensagem.Text) = EmptyStr then
    raise Exception.Create('Informe a mensagem a ser enviada.');

  // definir a bandeja de chip quando possuir mais de uma
  if frmPrincipal.ACBrSMS1.BandejasSimCard > 1 then
  begin
    if rdgBandeja.ItemIndex = 0 then
      frmPrincipal.ACBrSMS1.TrocarBandeja(simCard1)
    else
      frmPrincipal.ACBrSMS1.TrocarBandeja(simCard2);
  end;

  // quebra de mensagens maiores que 160 caracteres
  frmPrincipal.ACBrSMS1.QuebraMensagens := ckbQuebrarMensagem.Checked;

  // enviar o sms
  frmPrincipal.ACBrSMS1.EnviarSMS(
    edtTelefone.Text,
    memMensagem.Text,
    IndiceMsgEnviada
  );

  ShowMessage(
    'Mensagem envida com sucesso. Indice: ' + IndiceMsgEnviada +
    sLineBreak +
    sLineBreak +
    'Ultima resposta: ' +
    sLineBreak +
    frmPrincipal.ACBrSMS1.UltimaResposta
  );
end;

end.
