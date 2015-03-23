program LCBTeste;

uses
  Forms,
  LCBTeste1 in 'LCBTeste1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
