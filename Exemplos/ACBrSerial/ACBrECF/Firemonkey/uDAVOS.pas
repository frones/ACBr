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
