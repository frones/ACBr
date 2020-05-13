program TEFDDemo;

{$mode objfpc}{$H+}

uses
  {$DEFINE UseCThreads}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, ConfiguraSerial, ACBr_TEFD, Unit2, Unit3, Unit4, Unit5, Unit6,
  Unit7, ACBrComum, ACBrSerial;

{$R TEFDDemo.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

