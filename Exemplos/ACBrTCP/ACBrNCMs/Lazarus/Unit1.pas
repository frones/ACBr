unit Unit1;

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ACBrBase, ACBrSocket, ACBrNCMs, DB, BufDataset,
  StdCtrls, Buttons, Grids, DBGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    ACBrNcms1: TACBrNcms;
    BufDataset1: TBufDataset;
    BufDataset1CODNCM1: TStringField;
    BufDataset1DESCRICAO1: TStringField;
    DBGrid1: TDBGrid;
    BitBtn1: TBitBtn;
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

{$R *.lfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
var
  I: integer;
begin
  ACBrNcms1.ListarNcms();

  try
    BufDataset1.CreateDataSet;
  except
  end;


  for I := 0 to ACBrNcms1.Ncms.Count - 1 do
  begin
    BufDataset1.Append;

    BufDataset1CODNCM1.Value := ACBrNcms1.Ncms[i].CodigoNcm;
    BufDataset1DESCRICAO1.Value := ACBrNcms1.Ncms[i].DescricaoNcm;

    BufDataset1.Post;

    Application.ProcessMessages;
  end;

  Label1.Caption := 'Numero de Registros: ' + IntToStr(BufDataset1.RecordCount);
  MessageDlg('Fim do Processo!', mtInformation, [mbOK], 0);
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  if Length(Edit1.Text) <> 8 then
  begin
    MessageDlg('O codigo do NCM deve conter 8 Caracteres', mtWarning, [mbOK], 0);
    Exit;
  end;


  if not ACBrNcms1.validar(Edit1.Text) then
    MessageDlg('Codigo NCM Invalido', mtWarning, [mbOK], 0)
  else
    MessageDlg('Codigo NCM Valido OK!', mtInformation, [mbOK], 0);
end;

end.


