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

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ACBrBase, ACBrSocket, ACBrNcms, DB, StdCtrls, Buttons, Grids,
  DBGrids, DBClient;

type
  TForm1 = class(TForm)
    ACBrNcms1: TACBrNcms;
    ClientDataSet1: TClientDataSet;
    DBGrid1: TDBGrid;
    BitBtn1: TBitBtn;
    ClientDataSet1CODNCM: TStringField;
    ClientDataSet1DESCRICAO: TStringField;
    BitBtn2: TBitBtn;
    Edit1: TEdit;
    DataSource1: TDataSource;
    Label1: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
Var
I:Integer;
begin
ACBrNcms1.ListarNcms();

Try
ClientDataSet1.CreateDataSet;
Except
End;


for I := 0 to ACBrNcms1.Ncms.Count -1 do
  Begin
  ClientDataSet1.Append;

  ClientDataSet1CODNCM.Value := ACBrNcms1.Ncms[i].CodigoNcm;
  ClientDataSet1DESCRICAO.Value := ACBrNcms1.Ncms[i].DescricaoNcm;

  ClientDataSet1.Post;

  Application.ProcessMessages;
  End;

Label1.Caption := 'Numero de Registros: '+ IntToStr(ClientDataSet1.RecordCount);
MessageDlg('Fim do Processo!', mtInformation, [mbOK], 0);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
if Length(Edit1.Text) <> 8 then
 Begin
 MessageDlg('O codigo do NCM deve conter 8 Caracteres', mtWarning, [mbOK], 0);
 Exit;
 End;


if Not ACBrNcms1.validar(Edit1.Text) then
 MessageDlg('Codigo NCM Invalido', mtWarning, [mbOK], 0)
Else
 MessageDlg('Codigo NCM Valido OK!', mtInformation, [mbOK], 0);
end;

end.


