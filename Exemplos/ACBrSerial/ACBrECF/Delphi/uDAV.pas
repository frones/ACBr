unit uDAV;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TfrmDAV = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label11: TLabel;
    btnAbrirDAV: TButton;
    edtTipoDocumento: TComboBox;
    edtNumero: TEdit;
    edtData: TDateTimePicker;
    edtSituacao: TEdit;
    edtVendedor: TEdit;
    edtNomeCliente: TEdit;
    edtEndereco: TEdit;
    edtCNPJCPF: TEdit;
    TabSheet2: TTabSheet;
    Label14: TLabel;
    Label15: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    btnRegistrarItem: TButton;
    edtProdCodigo: TEdit;
    edtProdQuantidade: TEdit;
    edtProdUnidade: TEdit;
    edtProdDescricao: TEdit;
    edtProdVlUnitario: TEdit;
    edtProdVlDesconto: TEdit;
    edtProdVlAcrescimo: TEdit;
    ckbProdCancelado: TCheckBox;
    TabSheet3: TTabSheet;
    btnFecharRelatorio: TButton;
    memObervacao: TMemo;
    pnlRodape: TPanel;
    btnCancelar: TButton;
    procedure edtProdQuantidadeKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure btnAbrirDAVClick(Sender: TObject);
    procedure btnRegistrarItemClick(Sender: TObject);
    procedure btnFecharRelatorioClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  private

  public

  end;

var
  frmDAV: TfrmDAV;

implementation

uses
  ECFTeste1, ACBrUtil;

{$R *.dfm}

procedure TfrmDAV.FormCreate(Sender: TObject);
begin
  inherited;

  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmDAV.edtProdQuantidadeKeyPress(Sender: TObject; var Key: Char);
begin
  if not(Key in ['0'..'9', ',', #8]) then
    Key := #0;
end;

procedure TfrmDAV.btnAbrirDAVClick(Sender: TObject);
begin
  Form1.ACBrECF1.DAV_Abrir(
    edtData.DateTime,
    edtTipoDocumento.Text,
    edtNumero.Text,
    edtSituacao.Text,
    edtVendedor.Text,
    '',
    edtCNPJCPF.Text,
    edtNomeCliente.Text,
    edtEndereco.Text
  );

  PageControl1.ActivePageIndex := 1;
end;

procedure TfrmDAV.btnRegistrarItemClick(Sender: TObject);
begin
  Form1.ACBrECF1.DAV_RegistrarItem(
    edtProdCodigo.Text,
    edtProdDescricao.Text,
    edtProdUnidade.Text,
    StringToFloat(edtProdQuantidade.Text),
    StringToFloat(edtProdVlUnitario.Text),
    StringToFloat(edtProdVlDesconto.Text),
    StringToFloat(edtProdVlAcrescimo.Text),
    ckbProdCancelado.Checked
  );
end;

procedure TfrmDAV.btnFecharRelatorioClick(Sender: TObject);
begin
  Form1.ACBrECF1.DAV_Fechar(memObervacao.Text);
  Self.Close;
end;

procedure TfrmDAV.btnCancelarClick(Sender: TObject);
begin
  Self.Close;
end;

end.

