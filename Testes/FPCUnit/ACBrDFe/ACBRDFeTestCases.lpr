program ACBrDiversosTestCase;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, fpcunittestrunner, ACBrDFeUtilTest,
  LibXml2TestCases, ACBrTests.Util, ACBrLibXml2TestClass,
  ACBrLibXml2TestAnsiConsts, ACBrLibXml2TestUTF8Consts,
  ACBrLibXml2UTF8TestClass, ACBrLibXml2ANSITestClass;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

