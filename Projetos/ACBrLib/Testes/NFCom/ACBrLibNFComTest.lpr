program ACBrLibNFComTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibNFComStaticImportMT, GuiTestRunner,
  ACBrLibConsts, ACBrLibNFComTestCase;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

