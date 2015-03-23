unit CMC7Teste1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ACBrLCB, ACBrCMC7, StdCtrls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
     ACBrCMC7_1: TACBrCMC7;
    ACBrLCB1: TACBrLCB;
    edtcmc7: TEdit;
    btnAtivar: TButton;
    ACBrCMC71: TACBrCMC7;
    Memo1: TMemo;
    edtcomp: TEdit;
    edtbanco: TEdit;
    edtagencia: TEdit;
    edtconta: TEdit;
    edtnumcheque: TEdit;
    edttipificacao: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    Label5: TLabel;
    Label6: TLabel;
    procedure ACBrLCB1LeCodigo(Sender: TObject);
    procedure btnAtivarClick(Sender: TObject);
    procedure edtcmc7KeyPress(Sender: TObject; var Key: Char);
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    procedure MostraCMC7(Codigo : String);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

procedure TForm1.MostraCMC7(Codigo : String);
begin
  ACBrCMC71.CMC7 := Codigo;
// Memo1.Lines.Clear;
  Memo1.Lines.Add('=======================================');
  Memo1.Lines.Add('CMC7:'+ACBrCMC71.CMC7);
  Memo1.Lines.Add('Comp:'+ACBrCMC71.Comp);
  Memo1.Lines.Add('Banco:'+ACBrCMC71.Banco);
  Memo1.Lines.Add('Agencia:'+ACBrCMC71.Agencia);
  Memo1.Lines.Add('Conta:'+ACBrCMC71.Conta);
  Memo1.Lines.Add('Numero:'+ACBrCMC71.Numero);
end;

procedure TForm1.ACBrLCB1LeCodigo(Sender: TObject);
begin
  edtcmc7.Text := ACBrLCB1.UltimoCodigo;
  MostraCMC7(edtcmc7.Text);
end;

procedure TForm1.btnAtivarClick(Sender: TObject);
begin
 if ACBrLCB1.Ativo then
  begin
    btnAtivar.Caption := 'Ativar Leitor Serial';
    ACBrLCB1.Desativar ;
  end
 else
  begin
    btnAtivar.Caption := 'Desativar Leitor Serial';
    ACBrLCB1.Ativar ;
  end;
end;

procedure TForm1.edtcmc7KeyPress(Sender: TObject; var Key: Char);
begin
  if key = #13 then
     MostraCMC7(edtcmc7.Text) ;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ACBrCMC71.MontaCMC7(edtbanco.Text,edtagencia.Text,edtconta.Text,edtnumcheque.Text,edtcomp.Text,edttipificacao.Text);
  edtcmc7.Text := ACBrCMC71.CMC7;
end;

initialization
  {$I cmc7teste1.lrs}

end.

