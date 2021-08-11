{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
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

unit frIncluirPagamento;

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
    seValorPago: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure seValorPagoChange(Sender: TObject);
    procedure seValorPagoKeyPress(Sender: TObject; var Key: Char);
  private

  public

  end;

implementation

uses
  frPrincipal,
  ACBrUtil;

{$R *.dfm}

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
  FormPrincipal.ImageList1.GetBitmap(11, btCancelar.Glyph);
  FormPrincipal.ImageList1.GetBitmap(5, btGravar.Glyph);
end;

procedure TFormIncluirPagamento.seValorPagoChange(Sender: TObject);
var
  AValor: Double;
begin
  AValor := StrToIntDef(OnlyNumber(seValorPago.Text), 0)/100;
  seValorPago.Text := FormatFloatBr(AValor);
  seValorPago.SelStart := Length(seValorPago.Text);

  btGravar.Enabled := (AValor > 0) and (cbFormaPagamento.ItemIndex >= 0);
end;

procedure TFormIncluirPagamento.seValorPagoKeyPress(Sender: TObject;
  var Key: Char);
begin
  if not CharInSet(Key, ['0'..'9',#8,#13,#27])  then
    Key := #0;
end;

end.

