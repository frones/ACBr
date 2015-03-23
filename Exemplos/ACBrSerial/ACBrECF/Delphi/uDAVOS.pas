unit uDAVOS;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type
  TfrmDAVOS = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure btnAbrirDAVClick(Sender: TObject);
    procedure edtProdQuantidadeKeyPress(Sender: TObject; var Key: Char);
    procedure btnRegistrarItemClick(Sender: TObject);
    procedure btnFecharRelatorioClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDAVOS: TfrmDAVOS;

implementation

uses
  ECFTeste1;

{$R *.dfm}

procedure TfrmDAVOS.FormCreate(Sender: TObject);
begin
  inherited;

  PageControl1.ActivePageIndex := 0;
end;

procedure TfrmDAVOS.edtProdQuantidadeKeyPress(Sender: TObject; var Key: Char);
begin
  if not(Key in ['0'..'9', ',', #8]) then
    Key := #0;
end;

procedure TfrmDAVOS.btnAbrirDAVClick(Sender: TObject);
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

procedure TfrmDAVOS.btnRegistrarItemClick(Sender: TObject);
begin
  Form1.ACBrECF1.DAV_RegistrarItem(
    edtProdCodigo.Text,
    edtProdDescricao.Text,
    edtProdUnidade.Text,
    StrToFloat(edtProdQuantidade.Text),
    StrToFloat(edtProdVlUnitario.Text),
    StrToFloat(edtProdVlDesconto.Text),
    StrToFloat(edtProdVlAcrescimo.Text),
    ckbProdCancelado.Checked
  );
end;

procedure TfrmDAVOS.btnFecharRelatorioClick(Sender: TObject);
begin
  Form1.ACBrECF1.DAV_Fechar(memObervacao.Text);
  Self.Close;
end;

procedure TfrmDAVOS.btnCancelarClick(Sender: TObject);
begin
  Self.Close;
end;

end.
