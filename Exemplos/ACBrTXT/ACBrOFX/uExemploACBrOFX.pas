unit uExemploACBrOFX;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, DB, Grids,
  DBGrids, StdCtrls, DBClient, ExtCtrls, ACBrOFX;

type
  TFExemploACBrOFX = class(TForm)
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    Button1: TButton;
    cdsOfx: TClientDataSet;
    dsOFX: TDataSource;
    cboTipos: TComboBox;
    Button2: TButton;
    lblCredito: TLabel;
    lblDebito: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    DBGrid2: TDBGrid;
    cdsTipos: TClientDataSet;
    dsTipos: TDataSource;
    cdsOfxINDEX: TIntegerField;
    cdsOfxID: TStringField;
    cdsOfxDOCUMENT: TStringField;
    cdsOfxMOVDATE: TDateField;
    cdsOfxMOVTYPE: TStringField;
    cdsOfxVALUE: TFloatField;
    cdsOfxDESCRIPTION: TStringField;
    Button3: TButton;
    cdsTiposTIPO: TStringField;
    cdsTiposMOVTYPE: TStringField;
    cdsTiposVALOR: TFloatField;
    Memo1: TMemo;
    ACBrOFX1: TACBrOFX;
    btnPesquisar: TButton;
    dlgOpen: TOpenDialog;
    procedure btnPesquisarClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
    procedure DBGrid2DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FExemploACBrOFX: TFExemploACBrOFX;

implementation

{$R *.dfm}

procedure TFExemploACBrOFX.btnPesquisarClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    Edit1.Text := dlgOpen.FileName;
end;

procedure TFExemploACBrOFX.Button1Click(Sender: TObject);
var
  i: Integer;
  Index: Integer;
  Creditos, Debitos: Currency;
begin
  Creditos := 0;
  Debitos  := 0;

  if cdsOfx.Active then
  begin
    cdsOfx.EmptyDataSet;
    cdsTipos.EmptyDataSet;
  end
  else
  begin
    cdsOfx.CreateDataSet;
    cdsTipos.CreateDataSet;
  end;
  cboTipos.Items.Clear;

  ACBrOFX1.FileOFX := Edit1.Text;
  ACBrOFX1.Import;

  for i := 0 to Pred(ACBrOFX1.Count) do
  begin
    if not cdsTipos.Locate('TIPO', ACBrOFX1.Get(i).Description, [loCaseInsensitive, loPartialKey]) then
    begin
      cdsTipos.Append;
      cdsTipos.FieldByName('TIPO').AsString := ACBrOFX1.Get(i).Description;
      cdsTipos.FieldByName('MOVTYPE').AsString := ACBrOFX1.Get(i).MovType;
      cdsTipos.FieldByName('VALOR').AsFloat := 0;
      cdsTipos.Post;
    end;
  end;

  for i := 0 to Pred(ACBrOFX1.Count) do
  begin
    Index := cboTipos.Items.IndexOf(ACBrOFX1.Get(i).Description);

    if Index < 0 then
    begin
      cboTipos.Items.Add(ACBrOFX1.Get(i).Description);
    end;

    if ACBrOFX1.Get(i).Value > 0 then
    begin
      Creditos := Creditos + ACBrOFX1.Get(i).Value;
    END
    ELSE
    begin
      Debitos := Debitos + ACBrOFX1.Get(i).Value;
    end;

    if cdsTipos.Locate('TIPO', ACBrOFX1.Get(i).Description, [loCaseInsensitive, loPartialKey]) then
    begin
      cdsTipos.Edit;
      cdsTipos.FieldByName('VALOR').AsFloat := cdsTipos.FieldByName('VALOR').AsFloat + ACBrOFX1.Get(i).Value;
      cdsTipos.Post;
    end;

    cdsOfx.InsertRecord([i, ACBrOFX1.Get(i).ID, ACBrOFX1.Get(i).Document, ACBrOFX1.Get(i).MovDate, ACBrOFX1.Get(i).MovType, ACBrOFX1.Get(i).Value,
      ACBrOFX1.Get(i).Description]);
  end;

  lblCredito.Caption := FormatFloat('#,##0.00', Creditos);
  lblDebito.Caption := FormatFloat('#,##0.00', Debitos);
  Memo1.Lines.Clear;
  Memo1.Lines.Add('Banco: ' +ACBrOFX1.BankID+' - ' + ACBrOFX1.BankName + #13#10 + 'Agência: ' + ACBrOFX1.BranchID + ' - CC: ' + ACBrOFX1.AccountID + #13#10 + 'Data Inicial: ' +
    ACBrOFX1.DateStart + ' Data Final: ' + ACBrOFX1.DateEnd);
end;

procedure TFExemploACBrOFX.Button2Click(Sender: TObject);
begin
  cdsOfx.Filter := 'DESCRIPTION LIKE ' + QuotedStr('%' + cboTipos.Text);
  cdsOfx.Filtered := TRUE;
end;

procedure TFExemploACBrOFX.Button3Click(Sender: TObject);
begin
  cdsOfx.Filtered := False;
end;

procedure TFExemploACBrOFX.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  ShowScrollBar(DBGrid1.Handle, SB_HORZ, False);
  if odd(cdsOfx.RecNo) then
    DBGrid1.Canvas.Brush.Color := $00F8F5E6
  else
    DBGrid1.Canvas.Brush.Color := clWhite;
  TDBGrid(Sender).Canvas.font.Color := clBlack;

  if cdsOfxMOVTYPE.AsString = 'D' then
  begin
    TDBGrid(Sender).Canvas.font.Color := clRed;
  END
  ELSE
  begin
    TDBGrid(Sender).Canvas.font.Color := clBlack;
  end;

  if gdSelected in State then
    with (Sender as TDBGrid).Canvas do
    begin
      Brush.Color := $00ECE3BD;
      FillRect(Rect);
      font.Style := [fsbold]
    end;
  TDBGrid(Sender).DefaultDrawDataCell(Rect, TDBGrid(Sender).columns[DataCol].field, State);
end;

procedure TFExemploACBrOFX.DBGrid2DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  ShowScrollBar(DBGrid2.Handle, SB_HORZ, False);
  if odd(cdsTipos.RecNo) then
    DBGrid2.Canvas.Brush.Color := $00F8F5E6
  else
    DBGrid2.Canvas.Brush.Color := clWhite;
  TDBGrid(Sender).Canvas.font.Color := clBlack;

  if cdsTiposMOVTYPE.AsString = 'D' then
  begin
    TDBGrid(Sender).Canvas.font.Color := clRed;
  END
  ELSE
  begin
    TDBGrid(Sender).Canvas.font.Color := clBlack;
  end;

  if gdSelected in State then
    with (Sender as TDBGrid).Canvas do
    begin
      Brush.Color := $00ECE3BD;
      FillRect(Rect);
      font.Style := [fsbold]
    end;
  TDBGrid(Sender).DefaultDrawDataCell(Rect, TDBGrid(Sender).columns[DataCol].field, State);
end;

end.
