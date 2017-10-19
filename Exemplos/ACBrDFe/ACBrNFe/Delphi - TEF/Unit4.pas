unit Unit4; 

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons;

type

{$IFNDEF FPC}
 {$R *.dfm}
{$ELSE}
 {$R *.lfm}
{$ENDIF}

  { TForm4 }

  TForm4 = class(TForm)
     BitBtn1 : TBitBtn;
     BitBtn2 : TBitBtn;
     BitBtn3: TBitBtn;
     ListBox1 : TListBox;
     Memo1 : TMemo ;
     Panel1 : TPanel;
     Panel2 : TPanel;
     Splitter1 : TSplitter ;
     procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form4 : TForm4; 

implementation

{ TForm4 }

procedure TForm4.FormShow(Sender: TObject);
begin
   if Memo1.Lines.Count > 0 then
   begin
     Memo1.Width   := Trunc(Width/2)-10;
     Memo1.Visible := True ;
     Splitter1.Visible := True ;
   end ;

   ListBox1.SetFocus;
   if ListBox1.Items.Count > 0 then
      ListBox1.ItemIndex := 0 ;


end;

end.

