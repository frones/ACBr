program ACBrLibSATTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, printer4lazarus, ACBrLibSATTestCase,
  ACBrLibSATStaticImport;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

