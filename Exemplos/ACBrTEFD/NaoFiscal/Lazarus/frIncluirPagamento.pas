unit frIncluirPagamento;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  Buttons, uVendaClass;

type

  { TFormIncluirPagamento }

  TFormIncluirPagamento = class(TForm)
    btGravar: TBitBtn;
    btCancelar: TBitBtn;
    cbFormaPagamento: TComboBox;
    Label14: TLabel;
    Label16: TLabel;
    seValorPago: TFloatSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure seValorPagoChange(Sender: TObject);
  private

  public

  end;

var
  FormIncluirPagamento: TFormIncluirPagamento;

implementation

{$R *.lfm}

{ TFormIncluirPagamento }

procedure TFormIncluirPagamento.FormCreate(Sender: TObject);
var
  l, i: Integer;
begin
  cbFormaPagamento.Clear;
  l := Length(cPagamentos)-1;
  for i := 0 to l do
    cbFormaPagamento.Items.Add(cPagamentos[i,0] + ' - ' + cPagamentos[i,1]);

  cbFormaPagamento.ItemIndex := 0;
end;

procedure TFormIncluirPagamento.seValorPagoChange(Sender: TObject);
begin
  btGravar.Enabled := (seValorPago.Value > 0) and (cbFormaPagamento.ItemIndex >= 0);
end;

end.

