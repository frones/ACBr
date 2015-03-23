unit uDados;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BufDataset, db, SynHighlighterMulti, SynHighlighterHTML,
  SynHighlighterCss, SynHighlighterJScript, Controls, Dialogs, RegExpr;

type

  { Tdm }

  Tdm = class(TDataModule)
    tbAnexos: TBufDataset;
    dsAnexos: TDatasource;
    tbContatos: TBufDataset;
    dsContatos: TDatasource;
    tbContas: TBufDataset;
    dsContas: TDatasource;
    ImagesContatos: TImageList;
    ImagesContas: TImageList;
    tbDestinos: TBufDataset;
    ColorDialog1: TColorDialog;
    dsDestinos: TDatasource;
    FontDialog1: TFontDialog;
    ImagesFiles: TImageList;
    ImagesTools: TImageList;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SynCssSyn1: TSynCssSyn;
    SynHTMLSyn1: TSynHTMLSyn;
    SynJScriptSyn1: TSynJScriptSyn;
    SynMultiSyn1: TSynMultiSyn;
    procedure tbDestinosAfterInsert(DataSet: TDataSet);
    procedure tbDestinosBeforePost(DataSet: TDataSet);
    function CarregaContas: string;
    function Ordenacao(DataSet: TBufDataSet;const FieldName: String): Boolean;
    function ValidaEmail(aEmail: string): Boolean;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  dm: Tdm;

implementation

uses Forms, LCLType;

{$R *.lfm}

{ Tdm }

procedure Tdm.tbDestinosAfterInsert(DataSet: TDataSet);
begin
  if tbDestinos.Fields[1].IsNull then
    tbDestinos.Fields[1].AsString := 'Para:';
end;

procedure Tdm.tbDestinosBeforePost(DataSet: TDataSet);
var
  vErro: string;
begin
  vErro := '';
  if (Trim(tbDestinos.Fields[2].AsString) = '') then
    vErro := 'O e-mail é obrigatório!'
  else
  if not(ValidaEmail(tbDestinos.Fields[2].AsString)) then
    vErro := 'O e-mail digitado não está em um formato válido!';
  if vErro <> '' then
  begin
    Application.MessageBox(PChar(vErro),'Atenção',MB_ICONWARNING);
    Abort;
  end;
end;

function Tdm.CarregaContas: string;
begin
  Result := '';
  tbContas.Filter := '';
  tbContas.First;
  while not(tbContas.EOF) do
  begin
    Result += '"' + tbContas.Fields[1].AsString;
    tbContas.Next;
    if not(tbContas.EOF) then Result += '",' else Result += '"';
  end;
end;

function Tdm.Ordenacao(DataSet: TBufDataSet;const FieldName: String): Boolean;
var
  i: Integer;
  IndexDefs: TIndexDefs;
  IndexName: String;
  IndexOptions: TIndexOptions;
  Field: TField;
begin
  Result := False;
  if DataSet.IsEmpty then Exit;
  Field := DataSet.Fields.FindField(FieldName);
  if Field = nil then Exit;
  if (Field is TBlobField) or (Field is TVariantField) or (Field is TBinaryField) then Exit;
  IndexDefs := DataSet.IndexDefs;
  IndexName := DataSet.IndexName;
  IndexDefs.Updated:=false;
  IndexDefs.Update;
  if IndexName = FieldName + '__IdxA'
  then
    begin
      IndexName := FieldName + '__IdxD';
      IndexOptions := [ixDescending];
    end
  else
    begin
      IndexName := FieldName + '__IdxA';
      IndexOptions := [];
    end;
  for i := 0 to Pred(IndexDefs.Count) do
  begin
    if IndexDefs[i].Name = IndexName then
      begin
        Result := True;
        Break
      end;
  end;
  if not Result then
      begin
        if IndexName=FieldName + '__IdxD' then
          DataSet.AddIndex(IndexName, FieldName, IndexOptions, FieldName)
        else
          DataSet.AddIndex(IndexName, FieldName, IndexOptions);
        Result := True;
      end;
  DataSet.IndexName := IndexName;
end;

function Tdm.ValidaEmail(aEmail: string): Boolean;
var
  vRegex: TRegExpr;
begin
  Result := False;
  vRegex := TRegExpr.Create;
  try
    vRegex.Expression := '^([a-zA-Z0-9_\-\.]+)@((\[[0-9]{1,3}' +
                         '\.[0-9]{1,3}\.[0-9]{1,3}\.)|(([a-zA-Z0-9\-]+\' +
                         '.)+))([a-zA-Z]{2,4}|[0-9]{1,3})(\]?)$';
    if vRegex.Exec(aEmail) then Result := True;
  finally
    vRegex.Free;
  end;
end;

end.

