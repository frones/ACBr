program Download;

{$MODE Delphi}

uses
  Forms, Interfaces,
  Frm_Download in 'Frm_Download.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
