unit Unit6;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TForm6 }

  TForm6 = class(TForm)
    BitBtn1 : TBitBtn;
    RadioButton1 : TRadioButton;
    RadioButton2 : TRadioButton;
    RadioButton3 : TRadioButton;
    procedure BitBtn1Click(Sender : TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form6 : TForm6; 

implementation

{$IFNDEF FPC}
 {$R *.dfm}
{$ELSE}
 {$R *.lfm}
{$ENDIF}

{ TForm6 }

procedure TForm6.BitBtn1Click(Sender : TObject);
begin
  ModalResult := mrOK;
end;

end.
