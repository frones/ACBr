program ProjACBrLibComumTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, ACBrLibResposta, GuiTestRunner, ACBrLibComumTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

