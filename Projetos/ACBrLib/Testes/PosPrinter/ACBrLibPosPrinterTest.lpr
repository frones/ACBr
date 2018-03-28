program ACBrLibPosPrinterTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ACBrLibPosPrinterTestCase, ACBrLibPosPrinterStaticImport;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

