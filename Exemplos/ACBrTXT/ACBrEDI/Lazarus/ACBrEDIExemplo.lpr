program ACBrEDIExemplo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils,
  Forms, Frm_ACBrEDI;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmACBrEDI, frmACBrEDI);
  Application.Run;

end.
