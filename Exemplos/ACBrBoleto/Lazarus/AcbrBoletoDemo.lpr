program AcbrBoletoDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazreport, ACBr_Boleto, uDemo, ACBr_BoletoFC_LazReport,
  ACBr_BoletoFC_Fortes;

{$R *.res}

begin
  Application.Initialize;
   Application.CreateForm ( TfrmDemo, frmDemo ) ;
  Application.Run;
end.

