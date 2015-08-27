unit uDAVOS;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl, FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.DateTimeCtrls, FMX.Edit, FMX.ComboEdit, FMX.ScrollBox, FMX.Memo;

type
  TfrmDAVOS = class(TForm)
    Panel1: TPanel;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    TabItem3: TTabItem;
    Label1: TLabel;
    Label2: TLabel;
    edtTipoDocumento: TComboEdit;
    edtNumero: TEdit;
    Label4: TLabel;
    edtSituacao: TEdit;
    Label3: TLabel;
    Label5: TLabel;
    edtVendedor: TEdit;
    Label11: TLabel;
    edtCNPJCPF: TEdit;
    Label6: TLabel;
    edtNomeCliente: TEdit;
    Label7: TLabel;
    edtEndereco: TEdit;
    btnAbrirDAV: TButton;
    edtData: TDateEdit;
    btnCancelar: TButton;
    Label14: TLabel;
    edtProdCodigo: TEdit;
    Label19: TLabel;
    edtProdDescricao: TEdit;
    Label15: TLabel;
    edtProdQuantidade: TEdit;
    ckbProdCancelado: TCheckBox;
    edtProdUnidade: TEdit;
    Label18: TLabel;
    Label22: TLabel;
    edtProdVlUnitario: TEdit;
    edtProdVlDesconto: TEdit;
    Label23: TLabel;
    Label24: TLabel;
    edtProdVlAcrescimo: TEdit;
    btnRegistrarItem: TButton;
    memObervacao: TMemo;
    btnFecharRelatorio: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnAbrirDAVClick(Sender: TObject);
    procedure btnRegistrarItemClick(Sender: TObject);
    procedure btnFecharRelatorioClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure edtProdQuantidadeKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDAVOS: TfrmDAVOS;

implementation

uses
  ECFTeste1, ACBrUtil;

{$R *.fmx}


procedure TfrmDAVOS.FormCreate(Sender: TObject);
begin
  inherited;

  TabControl1.ActiveTab := TabItem1;
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

  TabControl1.ActiveTab := TabItem2;
end;

procedure TfrmDAVOS.btnRegistrarItemClick(Sender: TObject);
begin
  Form1.ACBrECF1.DAV_RegistrarItem(
    edtProdCodigo.Text,
    edtProdDescricao.Text,
    edtProdUnidade.Text,
    StringToFloat(edtProdQuantidade.Text),
    StringToFloat(edtProdVlUnitario.Text),
    StringToFloat(edtProdVlDesconto.Text),
    StringToFloat(edtProdVlAcrescimo.Text),
    ckbProdCancelado.isChecked
    );
end;

procedure TfrmDAVOS.edtProdQuantidadeKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  if not(KeyChar in ['0' .. '9', ',', #8]) then
    KeyChar := #0;
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
