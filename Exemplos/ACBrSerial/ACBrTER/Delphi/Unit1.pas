unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrBase, ACBrTER , ACBrDevice, ExtCtrls, Spin, jpeg ;

type
  TForm1 = class(TForm)
    ACBrTER1: TACBrTER;
    btnEnviaMensagem: TButton;
    btnLimpaDisplay: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    cbxPorta: TComboBox;
    Label2: TLabel;
    cbxBaudRate: TComboBox;
    btnAtivar: TButton;
    ckbComutadora: TCheckBox;
    Label3: TLabel;
    edtIntervalo: TEdit;
    ckbRotacao: TCheckBox;
    lblPassoRotacao: TLabel;
    edtPassoRotacao: TEdit;
    Label4: TLabel;
    cbxModelo: TComboBox;
    Panel2: TPanel;
    Label6: TLabel;
    edtMensagem: TEdit;
    Image1: TImage;
    Panel1: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Button1: TButton;
    Label9: TLabel;
    cbxLinha: TSpinEdit;
    Label10: TLabel;
    cbxColuna: TSpinEdit;
    Panel3: TPanel;
    Label5: TLabel;
    cbxTerminal: TSpinEdit;
    procedure btnEnviaMensagemClick(Sender: TObject);
    procedure btnLimpaDisplayClick(Sender: TObject);
    procedure ACBrTER1RecebeChar(Terminal: Word; Char: Char);
    procedure btnAtivarClick(Sender: TObject);
    procedure ckbRotacaoClick(Sender: TObject);
    procedure ckbComutadoraClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}



procedure TForm1.btnEnviaMensagemClick(Sender: TObject);
begin
  ACBrTER1.EnviaString( edtMensagem.Text , cbxTerminal.Value );
end;

procedure TForm1.btnLimpaDisplayClick(Sender: TObject);
begin
  ACBrTER1.LimpaTela( cbxTerminal.Value );
end;


procedure TForm1.ACBrTER1RecebeChar(Terminal: Word; Char: Char);
  var TeclaRecebida : integer  ;
begin
  TeclaRecebida := Ord(char);
  if TeclaRecebida = 13 then
     begin
     ACBrTER1.LimpaTela( cbxTerminal.Value );
     memo1.Lines.add( 'Terminal:'+vartostr(Terminal) + ' = Recebeu:ENTER' ) ;
     exit ;
     end ;
  if TeclaRecebida = 8 then
     begin
     ACBrTER1.LimpaTela( cbxTerminal.Value );
     memo1.Lines.add( 'Terminal:'+vartostr(Terminal) + ' = Recebeu:BACKSPACE' ) ;
     exit ;
     end ;
  if TeclaRecebida = 127 then
     begin
     ACBrTER1.LimpaTela( cbxTerminal.Value );
     memo1.Lines.add( 'Terminal:'+vartostr(Terminal) + ' = Recebeu:DELETE' ) ;
     exit ;
     end ;
  memo1.Lines.add( 'Terminal:'+vartostr(Terminal) + ' = Recebeu:' + char ) ;
  ACBrTER1.EnviaString(char, cbxTerminal.Value );
end;


procedure TForm1.btnAtivarClick(Sender: TObject);
begin
   if btnAtivar.Caption = 'Desativar'  then
      begin
      btnAtivar.Caption := 'Ativar' ;
      ACBrTER1.Desativar ;
      end
       else
        begin
        btnAtivar.Caption := 'Desativar' ;
        ACBrTER1.Porta       := cbxPorta.Text ;
        ACBrTER1.Device.Baud := strtoint(cbxBaudRate.Text);
        ACBrTER1.Intervalo   := strtoint(edtIntervalo.Text) ;
        if cbxModelo.Text = 'Wilbor' then
           ACBrTER1.Modelo := terWilbor ; 
        ACBrTER1.ativar ;
        end ;
end;


procedure TForm1.ckbRotacaoClick(Sender: TObject);
begin
     ACBrTER1.Rotacao.Ativo  := ckbRotacao.Checked ;
     lblPassoRotacao.Enabled := ckbRotacao.Checked ;
     edtPassoRotacao.Enabled := ckbRotacao.Checked ;
end;

procedure TForm1.ckbComutadoraClick(Sender: TObject);
begin
   ACBrTER1.Comutadora := ckbComutadora.Checked ;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
   ckbComutadoraClick(Sender);
   ckbRotacaoClick(Sender);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
   ACBrTER1.PosicionaCursor( cbxLinha.Value , cbxColuna.Value , cbxTerminal.Value );
end;

end.
