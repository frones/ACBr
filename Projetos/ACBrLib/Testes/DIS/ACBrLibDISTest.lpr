program ACBrLibDISTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ACBrLibDISTestCase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

