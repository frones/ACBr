program ACBrLibNFSeTest;

{$mode objfpc}{$H+}

uses
    Interfaces, Forms, ACBrLibConsts, ACBrLibNFSeStaticImportMT, GuiTestRunner,
    ACBrLibNFSeTestCase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

