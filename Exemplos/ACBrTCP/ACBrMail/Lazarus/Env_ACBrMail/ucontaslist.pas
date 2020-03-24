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

unit uContasList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls, DBGrids,
  uDados, uProvedores;

type

  { TfrmContasList }

  TfrmContasList = class(TForm)
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    Image1: TImage;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure Edit1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ToolButton1Click(Sender: TObject);
    procedure ToolButton2Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmContasList: TfrmContasList;

implementation

uses uContasCad;

{$R *.lfm}

{ TfrmContasList }

procedure TfrmContasList.ToolButton1Click(Sender: TObject);
begin
  frmProvedores := TfrmProvedores.Create(nil);
  try
    if frmProvedores.ShowModal = mrOK then
    begin
      frmContasCad := TfrmContasCad.Create(nil);
      try
        frmContasCad.Caption := 'Cadastrar Nova Conta';
        case frmProvedores.Tag of
          1: with frmContasCad do
             begin
               edHost.Text := 'smtp.gmail.com';
               edPorta.Value := 465;
               cbSsl.Checked := True;
             end;
          2: with frmContasCad do
             begin
               edHost.Text := 'smtp.live.com';
               edPorta.Value := 587;
               cbTls.Checked := True;
             end;
          3: with frmContasCad do
             begin
               edHost.Text := 'smtp.mail.yahoo.com.br';
               edPorta.Value := 465;
               cbSsl.Checked := True;
             end;
        end;
        with frmContasCad do
          if ShowModal = mrOK then
          begin
            dm.tbContas.Append;
            dm.tbContas.Fields[1].AsString := edNome.Text + ' <' + edEmail.Text + '>';
            dm.tbContas.Fields[2].AsString := edNome.Text;
            dm.tbContas.Fields[3].AsString := edEmail.Text;
            dm.tbContas.Fields[4].AsString := edUsuario.Text;
            dm.tbContas.Fields[5].AsString := edSenha.Text;
            dm.tbContas.Fields[6].AsString := edHost.Text;
            dm.tbContas.Fields[7].AsInteger := edPorta.Value;
            dm.tbContas.Fields[8].AsBoolean := cbSsl.Checked;
            dm.tbContas.Fields[9].AsBoolean := cbTls.Checked;
            dm.tbContas.Post;
            if DeleteFile('contas.dat') then
              dm.tbContas.SaveToFile('contas.dat');
          end;
      finally
        frmContasCad.Release;
        FreeAndNil(frmContasCad);
      end;
    end;
  finally
    frmProvedores.Release;
    FreeAndNil(frmProvedores);
  end;
end;

procedure TfrmContasList.ToolButton2Click(Sender: TObject);
begin
  frmContasCad := TfrmContasCad.Create(nil);
  try
    frmContasCad.Caption := 'Editar Conta';
    with frmContasCad do
    begin
      edNome.Text := dm.tbContas.Fields[2].AsString;
      edEmail.Text := dm.tbContas.Fields[3].AsString;
      edUsuario.Text := dm.tbContas.Fields[4].AsString;
      edSenha.Text := dm.tbContas.Fields[5].AsString;
      edHost.Text := dm.tbContas.Fields[6].AsString;
      edPorta.Value := dm.tbContas.Fields[7].AsInteger;
      cbSsl.Checked := dm.tbContas.Fields[8].AsBoolean;
      cbTls.Checked := dm.tbContas.Fields[9].AsBoolean;
      if ShowModal = mrOK then
      begin
        dm.tbContas.Edit;
        dm.tbContas.Fields[1].AsString := edNome.Text + ' <' + edEmail.Text + '>';
        dm.tbContas.Fields[2].AsString := edNome.Text;
        dm.tbContas.Fields[3].AsString := edEmail.Text;
        dm.tbContas.Fields[4].AsString := edUsuario.Text;
        dm.tbContas.Fields[5].AsString := edSenha.Text;
        dm.tbContas.Fields[6].AsString := edHost.Text;
        dm.tbContas.Fields[7].AsInteger := edPorta.Value;
        dm.tbContas.Fields[8].AsBoolean := cbSsl.Checked;
        dm.tbContas.Fields[9].AsBoolean := cbTls.Checked;
        dm.tbContas.Post;
        if DeleteFile('contas.dat') then
          dm.tbContas.SaveToFile('contas.dat');
      end;
    end;
  finally
    frmContasCad.Release;
    FreeAndNil(frmContasCad);
  end;
end;

procedure TfrmContasList.ToolButton3Click(Sender: TObject);
begin
  if Application.MessageBox('Exluir a conta?','Confirmação',6) <> 6 then Exit;
  dm.tbContas.Delete;
  if DeleteFile('contas.dat') then
    dm.tbContas.SaveToFile('contas.dat');
end;

procedure TfrmContasList.FormActivate(Sender: TObject);
begin
  if dm.tbContas.IsEmpty then ToolButton1.Click;
end;

procedure TfrmContasList.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 27) then
  begin
    key := 0;
    ModalResult := mrCancel;
  end;
end;

procedure TfrmContasList.Edit1Change(Sender: TObject);
begin
  if Edit1.Text = '' then
    dm.tbContas.Filter := ''
  else
    dm.tbContas.Filter := '(nome = ' + QuotedStr(Edit1.Text + '*') + ') or ' +
      '(email = ' + QuotedStr(Edit1.Text + '*') + ')';
end;

procedure TfrmContasList.DBGrid1TitleClick(Column: TColumn);
begin
  dm.Ordenacao(dm.tbContas, Column.FieldName);
end;

end.

