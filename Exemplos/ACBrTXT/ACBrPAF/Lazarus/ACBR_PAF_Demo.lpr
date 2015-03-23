program ACBR_PAF_Demo;

{$MODE Delphi}

uses
  Forms, Interfaces,
  uFormPrincipal in 'uFormPrincipal.pas' {Form6};

{$R *.res}

begin
//  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
