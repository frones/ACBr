unit RelatorioGerencialFormatado;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmGerencialFormatado = class(TForm)
    memRelatorio: TMemo;
    btnImprimir: TButton;
    btnCancelar: TButton;
    procedure btnCancelarClick(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmGerencialFormatado: TfrmGerencialFormatado;

implementation

uses
  ECFTeste1;

{$R *.dfm}

procedure TfrmGerencialFormatado.btnCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmGerencialFormatado.btnImprimirClick(Sender: TObject);
begin
  Form1.ACBrECF1.RelatorioGerencial(memRelatorio.Lines);
end;

end.
