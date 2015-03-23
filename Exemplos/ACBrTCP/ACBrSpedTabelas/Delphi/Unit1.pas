unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, DB, DBClient, StdCtrls, Buttons, ExtCtrls, ACBrBase,
  ACBrSocket, ACBrSpedTabelas;

type
  TForm1 = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    ClientDataSet1Id: TStringField;
    ClientDataSet1Pacote: TStringField;
    ClientDataSet1Tipo: TStringField;
    ClientDataSet1Desc: TStringField;
    ClientDataSet1Versao: TStringField;
    ClientDataSet1DtCriacao: TDateField;
    ClientDataSet1DtVersao: TDateField;
    ClientDataSet1Hash: TStringField;
    Panel1: TPanel;
    BtnListar: TBitBtn;
    BtnDow: TBitBtn;
    BtnDowT: TBitBtn;
    ACBrSpedTabelas1: TACBrSpedTabelas;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure FormShow(Sender: TObject);
    procedure BtnListarClick(Sender: TObject);
    procedure BtnDowClick(Sender: TObject);
    procedure BtnDowTClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
bId,bversao:String;
implementation

{$R *.dfm}

procedure TForm1.BtnDowClick(Sender: TObject);
begin
bId := ClientDataSet1.FieldByName('Id').AsString;
bVersao := ClientDataSet1.FieldByName('Versao').AsString;


if ACBrSpedTabelas1.Download(bId,bVersao,bId+bVersao+'.txt') then
 MessageDlg('Download comcluido com sucesso', mtInformation, [mbOK], 0)
Else
 MessageDlg('Falha ao efetuar o download', mtError, [mbOK], 0);
end;

procedure TForm1.BtnDowTClick(Sender: TObject);
var
Ok,Ok1 : boolean;
begin
Ok := True;
ClientDataSet1.First;
while Not ClientDataSet1.Eof do
 Begin
 bId := ClientDataSet1.FieldByName('Id').AsString;
 bVersao := ClientDataSet1.FieldByName('Versao').AsString;
 Ok1 := ACBrSpedTabelas1.Download(bId,bVersao,bId+bVersao+'.txt');

 if Ok1 = False then
  Ok := False;

 ClientDataSet1.Next;
 Application.ProcessMessages;
 End;
if Ok = True then
 MessageDlg('Download comcluido com sucesso', mtInformation, [mbOK], 0)
Else
 MessageDlg('Falha ao efetuar o download', mtError, [mbOK], 0);
end;

procedure TForm1.BtnListarClick(Sender: TObject);
Var
I:Integer;
begin
ClientDataSet1.EmptyDataSet;

ACBrSpedTabelas1.CodSistema := TACBrCodSistema( ComboBox1.ItemIndex ) ;
ACBrSpedTabelas1.ListarTabelas;
for I := 0 to ACBrSpedTabelas1.Tabelas.Count -1 do
 Begin
 ClientDataSet1.Append;

 ClientDataSet1Id.Value := ACBrSpedTabelas1.Tabelas[I].Id;
 ClientDataSet1Pacote.Value := ACBrSpedTabelas1.Tabelas[I].Pacote;
 ClientDataSet1Tipo.Value := ACBrSpedTabelas1.Tabelas[I].Tipo;
 ClientDataSet1Desc.Value := ACBrSpedTabelas1.Tabelas[I].Desc;
 ClientDataSet1Versao.Value := ACBrSpedTabelas1.Tabelas[I].Versao;
 ClientDataSet1DtCriacao.Value := ACBrSpedTabelas1.Tabelas[I].DtCriacao;
 ClientDataSet1DtVersao.Value := ACBrSpedTabelas1.Tabelas[I].DtVersao;
 ClientDataSet1Hash.Value := ACBrSpedTabelas1.Tabelas[I].Hash;


 ClientDataSet1.Post;
 Application.ProcessMessages;
 End;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ComboBox1.ItemIndex := 0 ;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
ClientDataSet1.CreateDataSet;
end;

end.
