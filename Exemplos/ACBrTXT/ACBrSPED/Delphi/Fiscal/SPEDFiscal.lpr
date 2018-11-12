program SPEDFiscal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Frm_SPEDFiscal, ACBr_SPED, LResources;

begin
  Application.Initialize;
  Application.CreateForm(TFrmSPEDFiscal, FrmSPEDFiscal);
  Application.Run;
end.

