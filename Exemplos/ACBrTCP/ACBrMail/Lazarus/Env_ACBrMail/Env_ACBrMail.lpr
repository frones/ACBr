program Env_ACBrMail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, uProvedores, uContasList, uContasCad, uContatosCad,
  uContatosList, uDados, sdflaz, uTestes;

{$R *.res}

begin
  Application.Title := 'Enviador de E-mails ACBrMail - v1.0';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(Tdm, dm);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

