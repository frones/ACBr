program Server;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Unit1 in 'Unit1.pas', ACBrTCP {Form1};

{$R *.res}

begin
  Application.Title := 'ACBrTCPServer Demo';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
