program ACBrLibCTeTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ACBrLibCTeTestCase, ACBrLibCTeStaticImport;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

