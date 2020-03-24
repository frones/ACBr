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

unit UPrincipal;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACBrCotacao, ACBrBase, ACBrSocket, ComCtrls;

type

  TfrmPrincipal = class(TForm)
    GroupBox1: TGroupBox;
    btnAtualizarMostrar: TButton;
    ACBrCotacao1: TACBrCotacao;
    btnProcurarSimbolo: TButton;
    ListBox1: TListBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    DateTimePicker1: TDateTimePicker;
    Label3: TLabel;
    procedure btnAtualizarMostrarClick(Sender: TObject);
    procedure btnProcurarSimboloClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  ShellAPI, ACBrUtil;

{$R *.dfm}

procedure TfrmPrincipal.btnAtualizarMostrarClick(Sender: TObject);
var
  I: Integer;
begin
  ListBox1.Items.BeginUpdate;
  try
    ListBox1.Clear;

    ACBrCotacao1.AtualizarTabela(DateTimePicker1.Date);
    for I := 0 to ACBrCotacao1.Tabela.Count - 1 do
    begin
      ListBox1.Items.Add(
        Format('%s  %3.3d  %s  %s  %s  %5.5d  %s  %10s  %10s  %10s  %10s', [
          DateToStr(ACBrCotacao1.Tabela[I].DataCotacao),
          ACBrCotacao1.Tabela[I].CodigoMoeda,
          ACBrCotacao1.Tabela[I].Tipo,
          ACBrCotacao1.Tabela[I].Moeda,
          PadRight(ACBrCotacao1.Tabela[I].Nome, 20, ' '),
          ACBrCotacao1.Tabela[I].CodPais,
          PadRight(ACBrCotacao1.Tabela[I].Pais, 30, ' '),
          FloatTostr(ACBrCotacao1.Tabela[I].TaxaCompra),
          FloatTostr(ACBrCotacao1.Tabela[I].TaxaVenda),
          FloatTostr(ACBrCotacao1.Tabela[I].ParidadeCompra),
          FloatTostr(ACBrCotacao1.Tabela[I].ParidadeVenda)
        ])
      );
    end;
  finally
    ListBox1.Items.EndUpdate;
  end;
end;

procedure TfrmPrincipal.btnProcurarSimboloClick(Sender: TObject);
var
  Simbolo: String;
  Item: TACBrCotacaoItem;
begin
  if ACBrCotacao1.Tabela.Count = 0 then
  begin
    if Application.MessageBox('Deseja atualizar a tabela de cotações?', 'Atualizar', MB_ICONQUESTION + MB_YESNO) = ID_YES then
      ACBrCotacao1.AtualizarTabela(DateTimePicker1.Date)
    else
      raise Exception.Create('Antes de continuar atualize a tabela de cotações!');
  end;

  Simbolo := AnsiUpperCase(Trim(InputBox('Procurar', 'Informe o código da moeda:', '')));
  if Simbolo <> '' then
  begin
    Item := ACBrCotacao1.Procurar(Simbolo);

    if Item <> nil then
    begin
      ShowMessage(
        'Data Cotacao: ' + DateToStr(Item.DataCotacao) + sLineBreak +
        'Codigo Moeda: ' + IntToStr(Item.CodigoMoeda) + sLineBreak +
        'Tipo: ' + Item.Tipo + sLineBreak +
        'Moeda: ' + Item.Moeda + sLineBreak +
        'Nome: ' + Item.Nome + sLineBreak +
        'Cod Pais: ' + IntToStr(Item.CodPais) + sLineBreak +
        'Pais: ' + Item.Pais + sLineBreak +
        'Taxa Compra: ' + FloatToStr(Item.TaxaCompra) + sLineBreak +
        'Taxa Venda: ' + FloatToStr(Item.TaxaVenda) + sLineBreak +
        'Paridade Compra: ' + FloatToStr(Item.ParidadeCompra) + sLineBreak +
        'Paridade Venda: ' + FloatToStr(Item.ParidadeVenda)
      );
    end
    else
      raise Exception.Create('Não foi encontrado nenhuma cotação para a moeda informada!');
  end;
  
end;

procedure TfrmPrincipal.Label2Click(Sender: TObject);
begin
  OpenURL( Label2.Caption )
end;

procedure TfrmPrincipal.FormCreate(Sender: TObject);
begin
  DateTimePicker1.DateTime := Date;
end;

end.
