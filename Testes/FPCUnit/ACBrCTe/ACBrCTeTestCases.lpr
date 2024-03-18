program ACBrCTeTestCases;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, fpcunittestrunner, ACBrTests.Util,
  ACBrCTeTestConsts, ACBrCTeXmlReaderOtherTests,
  ACBrCTeXmlReaderReadingTests_CTe_Ver400,
  ACBrCTeXmlReaderReadingTests_GTVe_Ver400,
  ACBrCTeXmlReaderReadingTests_CTeOS_Ver400,
  ACBrCTeXmlReaderReadingTests_CTe_Ver300;

{$R *.res}

begin
  Application.Initialize;
  SetHeapTraceOutput('heaptrace.log');
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

