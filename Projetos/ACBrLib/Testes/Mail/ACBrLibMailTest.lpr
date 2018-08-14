program ACBrLibMailTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ACBrLibMailTestCase, ACBrLibMailStaticImport;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

