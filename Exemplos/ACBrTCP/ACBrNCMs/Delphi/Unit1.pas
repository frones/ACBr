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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, DB, DBClient, StdCtrls, ExtCtrls, ACBrBase,
  ACBrSocket, ACBrNCMs,
  MidasLib;

type
  TForm1 = class(TForm)
    pnMenu: TPanel;
    btListarNCMs: TButton;
    btSalvarEmArquivo: TButton;
    gbFiltrarPorCodigo: TGroupBox;
    edFiltroCodigo: TEdit;
    gbFiltrarPorDescricao: TGroupBox;
    edFiltroDescricao: TEdit;
    cbTipoFiltro: TComboBox;
    btFiltrarPorCodigo: TButton;
    btFiltrarPorDescricao: TButton;
    gbValidarNCM: TGroupBox;
    edValidarNCM: TEdit;
    btValidarNCM: TButton;
    lbNumRegistros: TLabel;
    lbUltAtualizacao: TLabel;
    ClientDataSet1: TClientDataSet;
    ClientDataSet1CODNCM: TStringField;
    ClientDataSet1DESCRICAO: TStringField;
    ClientDataSet1DATAINICIO: TDateField;
    ClientDataSet1DATAFIM: TDateField;
    ClientDataSet1TIPOATO: TStringField;
    ClientDataSet1NUMEROATO: TStringField;
    ClientDataSet1ANOATO: TIntegerField;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    ACBrNCMs1: TACBrNCMs;
    pCarregando: TPanel;
    procedure btListarNCMsClick(Sender: TObject);
    procedure btSalvarEmArquivoClick(Sender: TObject);
    procedure btFiltrarPorCodigoClick(Sender: TObject);
    procedure btFiltrarPorDescricaoClick(Sender: TObject);
    procedure btValidarNCMClick(Sender: TObject);
  private
    procedure AtualizarInterface(aListaNCM: TACBrNCMsList);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ACBrUtil.Base, ACBrUtil.DateTime;

{$R *.dfm}

procedure TForm1.AtualizarInterface(aListaNCM: TACBrNCMsList);
var
  I: Integer;
begin
  pCarregando.Visible := True;
  DBGrid1.Visible := False;
  try
    lbNumRegistros.Caption := 'Numero de Registros: ' + IntToStr(aListaNCM.Count);
    lbUltAtualizacao.Caption := 'Ultima Atualizacao: ' + FormatDateBr(ACBrNcms1.UltimaAtualizacao);
    Application.ProcessMessages;

    ClientDataSet1.Close;
    ClientDataSet1.CreateDataSet;

    for I := 0 to aListaNCM.Count - 1 do
    begin
      ClientDataSet1.Append;

      ClientDataSet1CODNCM.Value := aListaNCM[I].CodigoNcm;
      ClientDataSet1DESCRICAO.Value := aListaNCM[I].DescricaoNcm;
      ClientDataSet1DATAINICIO.Value := aListaNCM[I].DataInicio;
      ClientDataSet1DATAFIM.Value := aListaNCM[I].DataFim;
      ClientDataSet1TIPOATO.Value := aListaNCM[I].TipoAto;
      ClientDataSet1NUMEROATO.Value := aListaNCM[I].NumeroAto;
      ClientDataSet1ANOATO.Value := aListaNCM[I].AnoAto;

      ClientDataSet1.Post;
    end;

    ClientDataSet1.First;
    Application.ProcessMessages;
    lbNumRegistros.Caption := 'Numero de Registros: '+ IntToStr(ClientDataSet1.RecordCount);
  finally
    DBGrid1.Visible := True;
    pCarregando.Visible := False;
    Application.ProcessMessages;
  end;
end;

procedure TForm1.btListarNCMsClick(Sender: TObject);
begin
  pCarregando.Visible := True;
  DBGrid1.Visible := False;
  try
    Application.ProcessMessages;
    ACBrNcms1.ObterNCMs;
    AtualizarInterface(ACBrNcms1.NCMs);
  finally
    DBGrid1.Visible := True;
    pCarregando.Visible := False;
    Application.ProcessMessages;
  end;
end;

procedure TForm1.btSalvarEmArquivoClick(Sender: TObject);
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

procedure TForm1.btValidarNCMClick(Sender: TObject);
begin
  if not ACBrNcms1.Validar(edValidarNCM.Text) then
    MessageDlg('Codigo NCM Invalido', mtError, [mbOK], 0)
  else
    MessageDlg('Codigo NCM Valido OK!', mtInformation, [mbOK], 0);
end;

end.
