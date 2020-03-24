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

unit uContatosCad;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, ExtCtrls, Buttons, StdCtrls, sysutils;

type

  { TfrmContatosCad }

  TfrmContatosCad = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Dados: TGroupBox;
    edNome: TLabeledEdit;
    edEmail: TLabeledEdit;
    Panel1: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmContatosCad: TfrmContatosCad;

implementation

uses uDados, variants, LCLType, db;

{$R *.lfm}

{ TfrmContatosCad }

procedure TfrmContatosCad.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 27) then
  begin
    key := 0;
    ModalResult := mrCancel;
  end;
end;

procedure TfrmContatosCad.BitBtn1Click(Sender: TObject);
var
  vErro: string;
  vCampo, vReg: Integer;
begin
  vErro := '';
  vCampo := 2;
  vReg := 0;
  dm.tbContatos.DisableControls;
  if not(dm.tbContatos.IsEmpty) then vReg := dm.tbContatos.RecNo;
  if Trim(edEmail.Text) = '' then
    vErro := 'O campo e-mail é obrigatório!'
  else
  if not(dm.ValidaEmail(edEmail.Text)) then
    vErro := 'O e-mail digitado não está em um formato válido!'
  else
  if (dm.tbContatos.Locate('nome;email',VarArrayOf([edNome.Text,edEmail.Text]),
    [loCaseInsensitive])) then
  begin
    if (dm.tbContatos.RecNo <> vReg) or (Tag = 1) then
      vErro := 'Esta combinação de nome e e-mail já está cadastrada!';
    vCampo := 1;
  end;
  if not(dm.tbContatos.IsEmpty) then dm.tbContatos.RecNo := vReg;
  dm.tbContatos.EnableControls;
  if vErro <> '' then
  begin
    Application.MessageBox(PChar(vErro),'Atenção',MB_ICONWARNING);
    if vCampo = 2 then edEmail.SetFocus else edNome.SetFocus;
    ModalResult := mrNone;
  end;
end;

procedure TfrmContatosCad.FormShow(Sender: TObject);
begin
  edNome.SetFocus;
end;

end.

