{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{																			   }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrBase, ACBrAAC, ExtCtrls;

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

{$R *.lfm}

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
