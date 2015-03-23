unit Unit7; 

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls;

type

  { TForm6 }

  { TForm7 }

  TForm7 = class(TForm)
    BitBtn1 : TBitBtn;
    BitBtn2 : TBitBtn;
    ListBox1 : TListBox;
    Panel1 : TPanel;
    pnlInformacao : TPanel;
    procedure FormShow(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form7 : TForm7; 

implementation

{$IFNDEF FPC}
 {$R *.dfm}
{$ELSE}
 {$R *.lfm}
{$ENDIF}

{ TForm7 }

procedure TForm7.FormShow(Sender : TObject);
begin
  ListBox1.SetFocus;
  if ListBox1.Items.Count > 0 then
     ListBox1.ItemIndex := 0 ;
end;

end.

