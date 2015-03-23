program EADTeste;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, EADTeste1;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1) ;
  Application.Run;
end.

