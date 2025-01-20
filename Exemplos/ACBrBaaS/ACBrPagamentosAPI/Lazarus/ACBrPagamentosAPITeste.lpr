program ACBrPagamentosAPITeste;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, uDemoBaaS, uPagamentos;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrPagamentosAPITeste, frPagamentosAPITeste);
  Application.CreateForm(TfrmPagamentos, frmPagamentos);
  Application.Run;
end.

