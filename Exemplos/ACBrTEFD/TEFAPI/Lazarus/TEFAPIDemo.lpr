program TEFAPIDemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils,
  Forms, frPrincipal, uVendaClass, frIncluirPagamento, frExibeMensagem;

{$R *.res}

begin
  {$IFDEF DEBUG}
   DeleteFile( 'heaptrclog.trc');
   SetHeapTraceOutput( 'heaptrclog.trc');
  {$ENDIF}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormPrincipal, FormPrincipal);
  Application.Run;
end.

