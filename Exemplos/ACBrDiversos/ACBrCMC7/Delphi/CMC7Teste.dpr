program CMC7Teste;

uses
  Forms,
  CMC7Teste1 in 'CMC7Teste1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
