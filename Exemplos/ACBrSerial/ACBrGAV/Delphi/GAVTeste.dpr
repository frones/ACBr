program GAVTeste;

uses
  Forms,
  GAVTeste1 in 'GAVTeste1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
