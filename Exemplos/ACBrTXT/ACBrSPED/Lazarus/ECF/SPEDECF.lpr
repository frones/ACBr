program SPEDECF;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Frm_SPEDECF in 'Frm_SPEDECF.pas' {FrmSPEDECF};

begin
  Application.Initialize;
  Application.CreateForm(TFrmSPEDECF, FrmSPEDECF);
  Application.Run;
end.
