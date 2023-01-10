program ACBreSocialTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, fpcunittestrunner, ACBreSocialTests, 
ACBreSocialEventosNaoPeriodicosTests, ACBreSocialEventosPeriodicosTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

