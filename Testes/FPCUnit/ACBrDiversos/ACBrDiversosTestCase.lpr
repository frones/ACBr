program ACBrDiversosTestCase;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, ACBrValidadorTest, ACBrTests.Util;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

