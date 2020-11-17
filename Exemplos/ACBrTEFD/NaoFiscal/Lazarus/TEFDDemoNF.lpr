program TEFDDemoNF;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  sysutils,
  Forms, frPrincipal, uVendaClass, frIncluirPagamento, frExibeMensagem
  { you can add units after this };

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

