unit uProvedores;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, Buttons, ExtCtrls;

type

  { TfrmProvedores }

  TfrmProvedores = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Panel1: TPanel;
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmProvedores: TfrmProvedores;

implementation

{$R *.lfm}

{ TfrmProvedores }

procedure TfrmProvedores.BitBtn4Click(Sender: TObject);
begin
  Tag := 1;
end;

procedure TfrmProvedores.BitBtn5Click(Sender: TObject);
begin
  Tag := 2;
end;

procedure TfrmProvedores.BitBtn6Click(Sender: TObject);
begin
  Tag := 3;
end;

procedure TfrmProvedores.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if (key = 27) then
  begin
    key := 0;
    ModalResult := mrCancel;
  end;
end;

end.

