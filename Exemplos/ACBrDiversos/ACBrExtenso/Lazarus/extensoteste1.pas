unit ExtensoTeste1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ACBrExtenso, StdCtrls, Buttons, SynEdit;

type

  { TfrExtenso }

  TfrExtenso = class(TForm)
    ACBrExtenso1: TACBrExtenso;
    ComboBox1: TComboBox;
    edValor: TEdit;
    Label1: TLabel;
    bExtenso: TButton;
    Label2: TLabel;
    mExtenso: TMemo;
    cbZeroAEsquerda: TCheckBox;
    procedure bExtensoClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure edValorKeyPress(Sender: TObject; var Key: Char);
    procedure cbZeroAEsquerdaClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frExtenso: TfrExtenso;

implementation
Uses ACBrUtil ;


procedure TfrExtenso.bExtensoClick(Sender: TObject);
begin
  ACBrExtenso1.Valor := StringToFloat( edValor.Text ) ;
  mExtenso.Text := ACBrExtenso1.Texto ;
end;

procedure TfrExtenso.ComboBox1Change(Sender: TObject);
begin
  ACBrExtenso1.Formato := TACBrExtensoFormato( ComboBox1.ItemIndex ) ;

  if ACBrExtenso1.Formato = extDolar then
   begin
     ACBrExtenso1.StrMoeda  := 'Dolar Americano' ;
     ACBrExtenso1.StrMoedas := 'Dolares Americanos' ;
   end
  else
   begin
     ACBrExtenso1.StrMoeda  := 'Real' ;
     ACBrExtenso1.StrMoedas := 'Reais' ;
   end ;
end;

procedure TfrExtenso.edValorKeyPress(Sender: TObject; var Key: Char);
begin
  if not ( Key in ['0'..'9',',','.',#13,#8] ) then
     Key := #0
  else
     if Key in [',','.'] then
        Key := DecimalSeparator ;
end;

procedure TfrExtenso.cbZeroAEsquerdaClick(Sender: TObject);
begin
  ACBrExtenso1.ZeroAEsquerda := cbZeroAEsquerda.Checked ;
end;

initialization
  {$I extensoteste1.lrs}

end.

