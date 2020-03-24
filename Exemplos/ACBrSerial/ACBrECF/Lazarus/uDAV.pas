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

unit uDAV;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, EditBtn;

type

  { TfrmDAV }

  TfrmDAV = class(TForm)
    edtData: TDateEdit;
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
  ECFTeste1;

{$R *.lfm}

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
    edtData.Date,
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
    StrToFloat(edtProdQuantidade.Text),
    StrToFloat(edtProdVlUnitario.Text),
    StrToFloat(edtProdVlDesconto.Text),
    StrToFloat(edtProdVlAcrescimo.Text),
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

