unit uSecondDisplay;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.Layouts,
  FMX.Effects,
  ACBr.Android.SecondDisplay;

type
  TFormSecondDisplay = class(TForm)
    Tela02: TACBrSecondDisplayLayout;
  private
    { Private declarations }
  public
    Procedure ShowLayout(aLayout: TControl);
    Procedure HideLayout(aLayout: TControl);
  end;

var
  FormSecondDisplay: TFormSecondDisplay;

implementation

{$R *.fmx}

{ TForm3 }

procedure TFormSecondDisplay.ShowLayout(aLayout: TControl);
begin
  aLayout.Width  := Self.Tela02.Width;
  aLayout.Height := Self.Tela02.Height;
  aLayout.Parent := Self.Tela02;
  aLayout.Align  := TAlignLayout.Contents;
  aLayout.BringToFront;
  Tela02 .Show;
end;

procedure TFormSecondDisplay.HideLayout(aLayout: TControl);
begin
  aLayout.Parent := Nil;
  Self.Tela02.Clear;
end;

end.
