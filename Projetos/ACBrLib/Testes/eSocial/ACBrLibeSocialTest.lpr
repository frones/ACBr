program ACBrLibeSocialTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibeSocialStaticImportST, ACBrLibConsts, GuiTestRunner,
  ACBrLibeSocialTestCase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

