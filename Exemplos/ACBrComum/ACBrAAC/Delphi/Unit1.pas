unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrBase, ACBrAAC, jpeg, ExtCtrls;

type
  TForm2 = class(TForm)
    Image1: TImage;
    ACBrAAC1: TACBrAAC;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    edtSH_RazaoSocial: TEdit;
    Label2: TLabel;
    edtSH_CNPJ: TEdit;
    Label3: TLabel;
    edtSH_IE: TEdit;
    Label4: TLabel;
    edtSH_IM: TEdit;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    edtPAF_Nome: TEdit;
    Label6: TLabel;
    edtPAF_Versao: TEdit;
    Label7: TLabel;
    edtPAF_MD5: TEdit;
    Label8: TLabel;
    edtNomeArquivoAuxiliar: TEdit;
    Label9: TLabel;
    edtParams: TMemo;
    Salvar: TButton;
    Button1: TButton;
    Memo1: TMemo;
    Label10: TLabel;
    procedure SalvarClick(Sender: TObject);
    procedure ACBrAAC1GetChave(var Chave: AnsiString);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.ACBrAAC1GetChave(var Chave: AnsiString);
begin
   // Adicione aqui sua chave privada, que será usada para criptografar e
   // descriptogravar o arquivo auxiliar.
   Chave := '1234';
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
   // Nome do arquivo auxiliar criptografado
   ACBrAAC1.NomeArquivoAux := edtNomeArquivoAuxiliar.Text;
   ACBrAAC1.AbrirArquivo;

   // Dados da software house
   edtSH_RazaoSocial.Text := ACBrAAC1.IdentPAF.Empresa.RazaoSocial;
   edtSH_CNPJ.Text := ACBrAAC1.IdentPAF.Empresa.CNPJ;
   edtSH_IE.Text := ACBrAAC1.IdentPAF.Empresa.IE;
   edtSH_IM.Text := ACBrAAC1.IdentPAF.Empresa.IM;

   // Dados do Aplicativo PAF
   edtPAF_Nome.Text := ACBrAAC1.IdentPAF.Paf.Nome;
   edtPAF_Versao.Text := ACBrAAC1.IdentPAF.Paf.Versao;

   // Outras informações extras
   edtParams.Text := ACBrAAC1.Params.Text;

   // Mostra como pegar valores dos parametros extras
   ShowMessage('PAF_PVPendentes=' + ACBrAAC1.Params.Values['PAF_PVPendentes']);
end;

procedure TForm2.SalvarClick(Sender: TObject);
begin
   // Nome do arquivo auxiliar criptografado
   ACBrAAC1.NomeArquivoAux := edtNomeArquivoAuxiliar.Text;

   // Dados da software house
   ACBrAAC1.IdentPAF.Empresa.RazaoSocial := edtSH_RazaoSocial.Text;
   ACBrAAC1.IdentPAF.Empresa.CNPJ := edtSH_CNPJ.Text;
   ACBrAAC1.IdentPAF.Empresa.IE := edtSH_IE.Text;
   ACBrAAC1.IdentPAF.Empresa.IM := edtSH_IM.Text;

   // Dados do Aplicativo PAF
   ACBrAAC1.IdentPAF.Paf.Nome := edtPAF_Nome.Text;
   ACBrAAC1.IdentPAF.Paf.Versao := edtPAF_Versao.Text;
   ACBrAAC1.IdentPAF.Paf.PrincipalExe.MD5 := edtPAF_MD5.Text;

   // Outras informações extras
   ACBrAAC1.Params.Text := edtParams.Text;

   // Salvar arquivo criptografado
   ACBrAAC1.SalvarArquivo;
end;

end.
