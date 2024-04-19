program ACBrLibAbecsPinpadTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibConsts, GuiTestRunner, ACBrLibAbecsPinpadTestCase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

