unit UdmVenda;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, memds, Forms, db;

type

  { TdmVenda }

  TdmVenda = class(TDataModule)
    tblVenda: TMemDataset;
    procedure DataModuleCreate(Sender: TObject);
    procedure tblVendaNewRecord(DataSet: TDataSet);
  private
    procedure CalculaSubtotal;
  public
    Subtotal: Real;
    procedure CancelaItem;
    procedure VendeItem(const cod:integer);
  end;

var
  dmVenda: TdmVenda;

implementation

{$R *.lfm}

{ TdmVenda }

procedure TdmVenda.DataModuleCreate(Sender: TObject);
var
  i: Integer;
begin
  Subtotal := 0;
  tblVenda.CreateTable;
  tblVenda.Open;
  //for i := 1 to 10 do
  //begin
  //  VendeItem(i);
  //  if i = 6 then
  //    CancelaItem;
  //end;

end;

procedure TdmVenda.tblVendaNewRecord(DataSet: TDataSet);
begin
  tblVenda.Fields[0].Value := tblVenda.RecordCount +1;
  tblVenda.Fields[6].AsBoolean := False;
end;

procedure TdmVenda.CalculaSubtotal;
var
  i: LongInt;
begin
  tblVenda.DisableControls;
  try
    i := tblVenda.RecNo;
    Subtotal := 0;
    while not tblVenda.eof do
    begin
      if not tblVenda.Fields[6].AsBoolean then
        Subtotal:= Subtotal + tblVenda.Fields[5].AsFloat;
      tblVenda.Next;
    end;
    tblVenda.First;
    while not (tblVenda.RecNo >= i) do
    begin
      if not tblVenda.Fields[6].AsBoolean then
        Subtotal:= Subtotal + tblVenda.Fields[5].AsFloat;
      tblVenda.Next;
    end;
  finally
    tblVenda.EnableControls;
  end;
end;

procedure TdmVenda.CancelaItem;
begin
  tblVenda.Edit;
  tblVenda.Fields[6].Value := True;
  tblVenda.Post;
  CalculaSubtotal;
end;

procedure TdmVenda.VendeItem(const cod: integer);
begin
  tblVenda.Insert;
  tblVenda.Fields[1].Value:= cod;
  tblVenda.Fields[2].Value:= 'Produto '+ IntToStr(cod);
  tblVenda.Fields[3].Value:= cod;
  tblVenda.Fields[4].Value:= cod;
  tblVenda.Fields[5].Value:= tblVenda.Fields[3].Value * tblVenda.Fields[4].Value;
  tblVenda.Post;
  CalculaSubtotal;
end;

end.

