unit RelatorioGerencialFormatado;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo;

type
  TfrmGerencialFormatado = class(TForm)
    memRelatorio: TMemo;
    btnImprimir: TButton;
    btnCancelar: TButton;
    procedure btnImprimirClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
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

{$R *.fmx}

procedure TfrmGerencialFormatado.btnCancelarClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmGerencialFormatado.btnImprimirClick(Sender: TObject);
begin
  Form1.ACBrECF1.RelatorioGerencial(memRelatorio.Lines);
end;

end.
