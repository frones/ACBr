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

unit uContatosList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, DBGrids,
  Buttons, uDados;

type

  { TfrmContatosList }

  TfrmContatosList = class(TForm)
    btDica: TSpeedButton;
    ComboBox1: TComboBox;
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    Image1: TImage;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure btDicaClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure Edit1Change(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    procedure InserirDestino;
  public
    { public declarations }
  end;

var
  frmContatosList: TfrmContatosList;

implementation

uses uContatosCad, db, LCLType;

{$R *.lfm}

{ TfrmContatosList }

procedure TfrmContatosList.ToolButton1Click(Sender: TObject);
begin
  frmContatosCad := TfrmContatosCad.Create(nil);
  try
    frmContatosCad.Caption := 'Cadastrar Novo Contato';
    frmContatosCad.Tag := 1;
    with frmContatosCad do
      if ShowModal = mrOK then
      begin
        dm.tbContatos.Append;
        dm.tbContatos.Fields[1].AsString := edNome.Text;
        dm.tbContatos.Fields[2].AsString := edEmail.Text;
        dm.tbContatos.Post;
        if DeleteFile('contatos.dat') then
          dm.tbContatos.SaveToFile('contatos.dat');
      end;
  finally
    frmContatosCad.Release;
    FreeAndNil(frmContatosCad);
  end;
end;

procedure TfrmContatosList.Edit1Change(Sender: TObject);
begin
  if Edit1.Text = '' then
    dm.tbContatos.Filter := ''
  else
    dm.tbContatos.Filter := '(nome = ' + QuotedStr(Edit1.Text + '*') + ') or ' +
      '(email = ' + QuotedStr(Edit1.Text + '*') + ')';
end;

procedure TfrmContatosList.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  x: Integer;
begin
  if (key = 27) then
  begin
    key := 0;
    ModalResult := mrCancel;
  end;
  if (key = 13) and (Tag = 1) then
  begin
    with DBGrid1.SelectedRows do
      if Count <> 0 then
      begin
        for x := 0 to Count - 1 do
        begin
          if IndexOf(Items[x]) > -1 then
          begin
            DBGrid1.DataSource.Dataset.Bookmark := Items[x];
            InserirDestino;
          end;
        end;
        Clear;
      end
      else
        InserirDestino;
    ModalResult := mrOK;
    Abort;
  end;
end;

procedure TfrmContatosList.FormShow(Sender: TObject);
begin
  if Tag = 1 then
  begin
    ToolBar1.Visible := False;
    Caption := 'Selecione um Contato';
    btDica.Visible := True;
    ComboBox1.Visible := True;
    DBGrid1.Options := DBGrid1.Options + [dgMultiselect];
    DBGrid1.SetFocus;
  end;
end;

procedure TfrmContatosList.DBGrid1TitleClick(Column: TColumn);
begin
  dm.Ordenacao(dm.tbContatos, Column.FieldName);
end;

procedure TfrmContatosList.DBGrid1DblClick(Sender: TObject);
begin
  if Tag = 1 then
  begin
    InserirDestino;
    ModalResult := mrOK
  end
  else
    ToolButton2.Click;
end;

procedure TfrmContatosList.btDicaClick(Sender: TObject);
begin
  Application.MessageBox(PChar(btDica.Hint),'Ajuda',MB_ICONINFORMATION);
end;

procedure TfrmContatosList.ToolButton2Click(Sender: TObject);
begin
  frmContatosCad := TfrmContatosCad.Create(nil);
  try
    frmContatosCad.Caption := 'Editar Contato';
    with frmContatosCad do
    begin
      edNome.Text := dm.tbContatos.Fields[1].AsString;
      edEmail.Text := dm.tbContatos.Fields[2].AsString;
      if ShowModal = mrOK then
      begin
        dm.tbContatos.Edit;
        dm.tbContatos.Fields[1].AsString := edNome.Text;
        dm.tbContatos.Fields[2].AsString := edEmail.Text;
        dm.tbContatos.Post;
        if DeleteFile('contatos.dat') then
          dm.tbContatos.SaveToFile('contatos.dat');
      end;
    end;
  finally
    frmContatosCad.Release;
    FreeAndNil(frmContatosCad);
  end;
end;

procedure TfrmContatosList.ToolButton3Click(Sender: TObject);
begin
  if Application.MessageBox('Exluir o contato?','Confirmação',6) <> 6 then Exit;
  dm.tbContatos.Delete;
  if DeleteFile('contatos.dat') then
    dm.tbContatos.SaveToFile('contatos.dat');
end;

procedure TfrmContatosList.InserirDestino;
begin
  if not(dm.tbDestinos.State in [dsInsert,dsEdit]) then
    dm.tbDestinos.Append;
  dm.tbDestinos.Fields[1].AsString := ComboBox1.Text;
  dm.tbDestinos.Fields[2].AsString := dm.tbContatos.Fields[2].AsString;
  dm.tbDestinos.Fields[3].AsString := dm.tbContatos.Fields[1].AsString;
  dm.tbDestinos.Post;
  if dm.tbDestinos.Fields[2].AsString <> '' then
    dm.tbDestinos.Append;
end;

end.

