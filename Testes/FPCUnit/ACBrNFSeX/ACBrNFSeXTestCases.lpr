program ACBrNFSeXTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrNFSeXTests, ACBrTests.Util, GuiTestRunner,
  ACBrNFSeXProvedorISSNETTests, ACBrNFSeXProvedorAbrasfTests,
  ACBrNFSeXProvedorGinfesTests;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

