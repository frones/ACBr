unit FSinaleiraDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, 
  ACBrBase, ACBrSIN, ACBrSINClass, ACBrDevice, ACBrDeviceSerial;

type
  TFormSinaleiraDemo = class(TForm)
    ACBrSIN1: TACBrSIN;
    Panel1: TPanel;
    Label1: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label11: TLabel;
    cmbSinaleira: TComboBox;
    cmbPortaSerial: TComboBox;
    cmbBaudRate: TComboBox;
    cmbDataBits: TComboBox;
    cmbHandShaking: TComboBox;
    cmbParity: TComboBox;
    cmbStopBits: TComboBox;
    ButtonAtivar: TButton;
    ButtonDesativar: TButton;
    GroupBox1: TGroupBox;
    ButtonRed: TButton;
    ButtonYellow: TButton;
    ButtonGreen: TButton;
    procedure ButtonAtivarClick(Sender: TObject);
    procedure ButtonGreenClick(Sender: TObject);
    procedure ButtonYellowClick(Sender: TObject);
    procedure ButtonRedClick(Sender: TObject);
    procedure ButtonDesativarClick(Sender: TObject);
  private
    procedure InicializarSinaleira(Ativar: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormSinaleiraDemo: TFormSinaleiraDemo;

implementation

{$R *.dfm}

procedure TFormSinaleiraDemo.ButtonAtivarClick(Sender: TObject);
begin
  InicializarSinaleira(True)
end;

procedure TFormSinaleiraDemo.ButtonYellowClick(Sender: TObject);
begin
  ACBrSIN1.defineLed(corAmarelo);
end;

procedure TFormSinaleiraDemo.ButtonDesativarClick(Sender: TObject);
begin
  InicializarSinaleira(False);
end;

procedure TFormSinaleiraDemo.ButtonGreenClick(Sender: TObject);
begin
  ACBrSIN1.defineLed(corVerde);
end;

procedure TFormSinaleiraDemo.ButtonRedClick(Sender: TObject);
begin
  ACBrSIN1.defineLed(corVermelho);
end;

procedure TFormSinaleiraDemo.InicializarSinaleira(Ativar: Boolean);
begin
  ACBrSIN1.Desativar;

  if Ativar then
  begin
    ACBrSIN1.Modelo           := TACBrSINModelo(cmbSinaleira.ItemIndex);
    ACBrSIN1.Device.HandShake := TACBrHandShake( cmbHandShaking.ItemIndex );
    ACBrSIN1.Device.Parity    := TACBrSerialParity( cmbParity.ItemIndex );
    ACBrSIN1.Device.Stop      := TACBrSerialStop( cmbStopBits.ItemIndex );
    ACBrSIN1.Device.Data      := StrToInt( cmbDataBits.text );
    ACBrSIN1.Device.Baud      := StrToInt( cmbBaudRate.Text );
    ACBrSIN1.Device.Porta     := cmbPortaSerial.Text;

    // Conecta com a balança
    ACBrSIN1.Ativar;
  end;

  Panel1.Enabled := (not Ativar);

  ButtonAtivar.Enabled    := (not Ativar);
  ButtonAtivar.Visible    := (not Ativar);

  ButtonDesativar.Enabled := Ativar;
  ButtonDesativar.Visible := Ativar;
end;

end.
