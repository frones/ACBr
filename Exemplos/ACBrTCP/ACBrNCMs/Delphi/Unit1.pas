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


