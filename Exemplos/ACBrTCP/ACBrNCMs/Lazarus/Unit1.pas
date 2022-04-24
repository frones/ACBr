{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:  Elias Cesar Vieira                             }
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


unit Unit1;

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ACBrNCMs, DB, BufDataset, StdCtrls, Buttons, DBGrids, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrNcms1: TACBrNcms;
    btFiltrarPorCodigo: TButton;
    btFiltrarPorDescricao: TButton;
    btListarNCMs: TBitBtn;
    btSalvar: TBitBtn;
    btValidar: TBitBtn;
    BufDataset1: TBufDataset;
    BufDataset1ANOATO: TLongintField;
    BufDataset1CODNCM: TStringField;
    BufDataset1DATAFIM: TDateField;
    BufDataset1DATAINICIO: TDateField;
    BufDataset1DESCRICAO: TStringField;
    BufDataset1NUMEROATO: TStringField;
    BufDataset1TIPOATO: TStringField;
    cbTipoFiltro: TComboBox;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    edFiltroCodigo: TEdit;
    edFiltroDescricao: TEdit;
    edValidar: TEdit;
    gbFiltrarPorDescricao: TGroupBox;
    gbValidar: TGroupBox;
    gbFiltrarPorCodigo: TGroupBox;
    lbNumRegistros: TLabel;
    lbUltAtualizacao: TLabel;
    pCarregando: TPanel;
    pnMenu: TPanel;
    procedure ACBrNcms1BuscaEfetuada(Sender: TObject);
    procedure btFiltrarPorCodigoClick(Sender: TObject);
    procedure btFiltrarPorDescricaoClick(Sender: TObject);
    procedure btListarNCMsClick(Sender: TObject);
    procedure btValidarClick(Sender: TObject);
    procedure btSalvarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AtualizarInterface(aListaNCM: TACBrNCMsList);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ACBrUtil.DateTime, ACBrUtil.Base;

{$R *.lfm}

procedure TForm1.btListarNCMsClick(Sender: TObject);
begin
  ACBrNcms1.ObterNCMs;
  AtualizarInterface(ACBrNcms1.NCMs);
end;

procedure TForm1.btFiltrarPorCodigoClick(Sender: TObject);
begin
  edFiltroDescricao.Text := '';
  if EstaVazio(edFiltroCodigo.Text) then
  begin
    AtualizarInterface(ACBrNcms1.NCMs);
    Exit;
  end;

  ACBrNcms1.BuscarPorCodigo(edFiltroCodigo.Text, False);
  AtualizarInterface(ACBrNcms1.NCMsFiltrados);
end;

procedure TForm1.btFiltrarPorDescricaoClick(Sender: TObject);
begin
  edFiltroCodigo.Text := '';
  if EstaVazio(edFiltroDescricao.Text) then
  begin
    AtualizarInterface(ACBrNcms1.NCMs);
    Exit;
  end;

  case cbTipoFiltro.ItemIndex of
    0: ACBrNcms1.BuscarPorDescricao(edFiltroDescricao.Text, ntfIniciaCom);
    1: ACBrNcms1.BuscarPorDescricao(edFiltroDescricao.Text, ntfContem);
    2: ACBrNcms1.BuscarPorDescricao(edFiltroDescricao.Text, ntfFinalizaCom);
  end;

  AtualizarInterface(ACBrNcms1.NCMsFiltrados);
end;

procedure TForm1.ACBrNcms1BuscaEfetuada(Sender: TObject);
begin
  AtualizarInterface(ACBrNcms1.NCMsFiltrados);
  MessageDlg('Fim do Processo!', mtInformation, [mbOK], 0);
end;

procedure TForm1.btValidarClick(Sender: TObject);
begin
  if not ACBrNcms1.Validar(edValidar.Text) then
    MessageDlg('Codigo NCM Inválido', mtError, [mbOK], 0)
  else
    MessageDlg('Codigo NCM Válido OK!', mtInformation, [mbOK], 0);
end;

procedure TForm1.btSalvarClick(Sender: TObject);
var
  wArq: String;
begin
  if (ACBrNcms1.NCMS.Count <= 0) then
  begin
    MessageDlg('Nenhum NCM encontrado. Utilize "Listar NCM''s', mtWarning, [mbOK], 0);
    Exit;
  end;

  wArq := Application.GetNamePath + 'NCMs.txt';
  ACBrNcms1.NCMS.SaveToFile(wArq);
  MessageDlg('Lista de NCMS gravada no arquivo : ' + sLineBreak + wArq, mtInformation, [mbOK], 0);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  BufDataset1.CreateDataSet;
end;

procedure TForm1.AtualizarInterface(aListaNCM: TACBrNCMsList);
var
  I: Integer;
begin
  pCarregando.Visible := True;
  DBGrid1.Visible := False;
  try
    lbNumRegistros.Caption := 'Numero de Registros: ' + IntToStr(aListaNCM.Count);
    lbUltAtualizacao.Caption := 'Última Atualização: ' + FormatDateBr(ACBrNcms1.UltimaAtualizacao);
    Application.ProcessMessages;

    BufDataset1.Close;
    BufDataset1.Open;

    for I := 0 to aListaNCM.Count - 1 do
    begin
      BufDataset1.Append;
      BufDataset1CODNCM.Value := aListaNCM[I].CodigoNcm;
      BufDataset1DESCRICAO.Value := aListaNCM[I].DescricaoNcm;
      BufDataset1DATAINICIO.Value := aListaNCM[I].DataInicio;
      BufDataset1DATAFIM.Value := aListaNCM[I].DataFim;
      BufDataset1TIPOATO.Value := aListaNCM[I].TipoAto;
      BufDataset1NUMEROATO.Value := aListaNCM[I].NumeroAto;
      BufDataset1ANOATO.Value := aListaNCM[I].AnoAto;
      BufDataset1.Post;
    end;

    BufDataset1.First;
  finally
    DBGrid1.Visible := True;
    pCarregando.Visible := False;
    Application.ProcessMessages;
  end;
end;

end.


